// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import Node._
import uncore._

case object NBTBEntries extends Field[Int]
case object NRAS extends Field[Int]

abstract trait BTBParameters extends UsesParameters {
  val vaddrBits = params(VAddrBits)
  val matchBits = params(PgIdxBits)
  val entries = params(NBTBEntries)
  val nRAS = params(NRAS)
  val nPages = ((1 max(log2Up(entries)))+1)/2*2 // control logic assumes 2 divides pages
  val opaqueBits = log2Up(entries)
  val nBHT = 1 << log2Up(entries*2)
}

class RAS(nras: Int) {
  def push(addr: UInt): Unit = {
    when (count < nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(nras)) || pos > 0, pos+1, UInt(0))
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop: Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(nras)) || pos > 0, pos-1, UInt(nras-1))
  }
  def clear: Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  private val count = Reg(init=UInt(0,log2Up(nras+1)))
  private val pos = Reg(init=UInt(0,log2Up(nras)))
  private val stack = Vec.fill(nras){Reg(UInt())}
}

class BHTResp extends Bundle with BTBParameters {
  val history = UInt(width = log2Up(nBHT).max(1))
  val value = UInt(width = 2)
}

// BHT contains table of 2-bit counters and a global history register.
// The BHT only predicts and updates when there is a BTB hit.
// The global history:
//    - updated speculatively in fetch (if there's a BTB hit).
//    - on a mispredict, the history register is reset (again, only if BTB hit).
// The counter table:
//    - each PC has its own counter, updated when a branch resolves (and BTB hit).
//    - the BTB provides the predicted branch PC, allowing us to properly index
//      the counter table and provide the prediction for that specific branch.
//      Critical path concerns may require only providing a single counter for
//      the entire fetch packet, but that complicates how multiple branches
//      update that line.
class BHT(nbht: Int, fetchwidth: Int) {
  val nbhtbits = log2Up(nbht)
  private val logfw = if (fetchwidth == 1) 0 else log2Up(fetchwidth)

  def get(fetch_addr: UInt, bridx: UInt, update: Bool): BHTResp = {
    val res = new BHTResp
    val aligned_addr = fetch_addr >> UInt(logfw + 2)
    val index = aligned_addr ^ history
    val counters = table(index)
    res.value := (counters >> (bridx<<1)) & Bits(0x3)
    res.history := history
    val taken = res.value(0)
    when (update) { history := Cat(taken, history(nbhtbits-1,1)) }
    res
  }
  def update(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val aligned_addr = addr >> UInt(logfw + 2)
    val index = aligned_addr ^ d.history
    val new_cntr = Cat(taken, (d.value(1) & d.value(0)) | ((d.value(1) | d.value(0)) & taken))
    var bridx: UInt = null
    if (logfw == 0) bridx = UInt(0) else bridx = addr(logfw+1,2)
    val mask = Bits(0x3) << (bridx<<1)
    table.write(index, new_cntr, mask) 
    when (mispredict) { history := Cat(taken, d.history(nbhtbits-1,1)) }
  }

  private val table = Mem(UInt(width = 2*fetchwidth), nbht)
  val history = Reg(UInt(width = nbhtbits))
}

// BTB update occurs during branch resolution.
//  - "pc" is what future fetch PCs will tag match against.
//  - "br_pc" is the PC of the branch instruction.
//  - "resp.mask" provides a mask of valid instructions (instructions are
//      masked off by the predicted taken branch).
class BTBUpdate extends Bundle with BTBParameters {
  val prediction = Valid(new BTBResp)
  val pc = UInt(width = vaddrBits)
  val target = UInt(width = vaddrBits)
  val returnAddr = UInt(width = vaddrBits)
  val taken = Bool()
  val isJump = Bool()
  val isCall = Bool()
  val isReturn = Bool()
  val br_pc = UInt(width = vaddrBits)
  val incorrectTarget = Bool()
}

class BTBResp extends Bundle with BTBParameters {
  val taken = Bool()
  val mask = Bits(width = log2Up(params(FetchWidth)))
  val target = UInt(width = vaddrBits)
  val entry = UInt(width = opaqueBits)
  val bht = new BHTResp
}

class BTBReq extends Bundle with BTBParameters {
   val addr = UInt(width = vaddrBits)
}

// fully-associative branch target buffer
class BTB extends Module with BTBParameters {
  val io = new Bundle {
    val req = Valid(new BTBReq).flip
    val resp = Valid(new BTBResp)
    val update = Valid(new BTBUpdate).flip
    val invalidate = Bool(INPUT)
  }

  val idxValid = Reg(init=UInt(0, entries))
  val idxs = Mem(UInt(width=matchBits), entries)
  val idxPages = Mem(UInt(width=log2Up(nPages)), entries)
  val tgts = Mem(UInt(width=matchBits), entries)
  val tgtPages = Mem(UInt(width=log2Up(nPages)), entries)
  val pages = Mem(UInt(width=vaddrBits-matchBits), nPages)
  val pageValid = Reg(init=UInt(0, nPages))
  val idxPagesOH = idxPages.map(UIntToOH(_)(nPages-1,0))
  val tgtPagesOH = tgtPages.map(UIntToOH(_)(nPages-1,0))

  val useRAS = Reg(UInt(width = entries))
  val isJump = Reg(UInt(width = entries))
  val brIdx  = Mem(UInt(width=log2Up(params(FetchWidth))), entries)

  private def page(addr: UInt) = addr >> matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    Vec(pages.map(_ === p)).toBits & pageValid
  }
  private def tagMatch(addr: UInt, pgMatch: UInt): UInt = {
    val idx = addr(matchBits-1,0)
    val idxMatch = idxs.map(_ === idx).toBits
    val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).toBits
    idxValid & idxMatch & idxPageMatch
  }

  val r_update = Pipe(io.update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(io.req.bits.addr)
  val hits = tagMatch(io.req.bits.addr, pageHit)
  val updatePageHit = pageMatch(r_update.bits.pc)
  val updateHits = tagMatch(r_update.bits.pc, updatePageHit)

  private var lfsr = LFSR16(r_update.valid)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    Random.oneHot(width, lfsr)
  }

  val updateHit = r_update.bits.prediction.valid
  val updateValid = r_update.bits.incorrectTarget || updateHit && Bool(nBHT > 0)
  val updateTarget = updateValid && r_update.bits.incorrectTarget

  val useUpdatePageHit = updatePageHit.orR
  val doIdxPageRepl = updateTarget && !useUpdatePageHit
  val idxPageRepl = UInt()
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(r_update.bits.pc) === page(update_target)
  val usePageHit = (pageHit & ~idxPageReplEn).orR
  val doTgtPageRepl = updateTarget && !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, idxPageUpdateOH(nPages-2,0) << 1 | idxPageUpdateOH(nPages-1))
  val tgtPageUpdate = OHToUInt(Mux(usePageHit, pageHit, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))
  val doPageRepl = doIdxPageRepl || doTgtPageRepl

  val pageReplEn = idxPageReplEn | tgtPageReplEn
  idxPageRepl := UIntToOH(Counter(r_update.valid && doPageRepl, nPages)._1)

  when (r_update.valid && !(updateValid && !updateTarget)) {
    val nextRepl = Counter(!updateHit && updateValid, entries)._1
    val waddr = Mux(updateHit, r_update.bits.prediction.bits.entry, nextRepl)

    // invalidate entries if we stomp on pages they depend upon
    idxValid := idxValid & ~Vec.tabulate(entries)(i => (pageReplEn & (idxPagesOH(i) | tgtPagesOH(i))).orR).toBits

    idxValid(waddr) := updateValid
    when (updateTarget) {
      assert(io.req.bits.addr === r_update.bits.target, "BTB request != I$ target")
      idxs(waddr) := r_update.bits.pc
      tgts(waddr) := update_target
      idxPages(waddr) := idxPageUpdate
      tgtPages(waddr) := tgtPageUpdate
      useRAS(waddr) := r_update.bits.isReturn
      isJump(waddr) := r_update.bits.isJump
      brIdx(waddr)  := r_update.bits.br_pc
    }

    require(nPages % 2 == 0)
    val idxWritesEven = (idxPageUpdateOH & Fill(nPages/2, UInt(1,2))).orR

    def writeBank(i: Int, mod: Int, en: Bool, data: UInt) =
      for (i <- i until nPages by mod)
        when (en && pageReplEn(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, doIdxPageRepl, doTgtPageRepl),
      Mux(idxWritesEven, page(r_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, doTgtPageRepl, doIdxPageRepl),
      Mux(idxWritesEven, page(update_target), page(r_update.bits.pc)))

    when (doPageRepl) { pageValid := pageValid | pageReplEn }
  }

  when (io.invalidate) {
    idxValid := 0
    pageValid := 0
  }

  io.resp.valid := hits.orR
  io.resp.bits.taken := io.resp.valid
  io.resp.bits.target := Cat(Mux1H(Mux1H(hits, tgtPagesOH), pages), Mux1H(hits, tgts))
  io.resp.bits.entry := OHToUInt(hits)
  io.resp.bits.mask := Cat((UInt(1) << brIdx(io.resp.bits.entry))-1, UInt(1)) 

  if (nBHT > 0) {
    val bht = new BHT(nBHT, params(FetchWidth))
    val res = bht.get(io.req.bits.addr, brIdx(io.resp.bits.entry), io.req.valid && hits.orR && !Mux1H(hits, isJump))
    val update_btb_hit = io.update.bits.prediction.valid
    when (io.update.valid && update_btb_hit && !io.update.bits.isJump) {
      bht.update(io.update.bits.br_pc, io.update.bits.prediction.bits.bht,
                 io.update.bits.taken, io.update.bits.incorrectTarget)
    }
    when (!res.value(0) && !Mux1H(hits, isJump)) { io.resp.bits.taken := false }
    io.resp.bits.bht := res
  }

  if (nRAS > 0) {
    val ras = new RAS(nRAS)
    val doPeek = Mux1H(hits, useRAS)
    when (!ras.isEmpty && doPeek) {
      io.resp.bits.target := ras.peek
    }
    when (io.update.valid) {
      when (io.update.bits.isCall) {
        ras.push(io.update.bits.returnAddr)
        when (doPeek) {
          io.resp.bits.target := io.update.bits.returnAddr
        }
      }.elsewhen (io.update.bits.isReturn && io.update.bits.prediction.valid) {
        ras.pop
      }
    }
    when (io.invalidate) { ras.clear }
  }
}
