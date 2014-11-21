// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._

case object CoreName extends Field[String]
case object NDCachePorts extends Field[Int]
case object NTilePorts extends Field[Int]
case object NPTWPorts extends Field[Int]

abstract class Tile(resetSignal: Bool = null) extends Module(_reset = resetSignal) {
  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO
    val temac = new TEMACIO
    val rocc = new RoCCInterface().flip
  }
}

class RocketTile(resetSignal: Bool = null) extends Tile(resetSignal) {

  val icache = Module(new Frontend, { case CacheName => "L1I"; case CoreName => "Rocket" })
  val dcache = Module(new HellaCache, { case CacheName => "L1D" })
  val ptw = Module(new PTW(params(NPTWPorts)))
  val core = Module(new Core, { case CoreName => "Rocket" })

  val dcArb = Module(new HellaCacheArbiter(params(NDCachePorts)))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcArb.io.mem <> dcache.io.cpu

  ptw.io.requestor(0) <> icache.io.cpu.ptw
  ptw.io.requestor(1) <> dcache.io.cpu.ptw

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.ptw <> ptw.io.dpath

  io.temac <> core.io.temac

  val memArb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(params(NTilePorts)))
  val dcPortId = 0
  memArb.io.in(dcPortId) <> dcache.io.mem
  memArb.io.in(1) <> icache.io.mem

  // wire RoCC connections to port
  val dcIF = Module(new SimpleHellaCacheIF)
  core.io.rocc <> io.rocc
  dcIF.io.requestor <> io.rocc.mem
  dcArb.io.requestor(2) <> dcIF.io.cache
  memArb.io.in(2) <> io.rocc.imem
  ptw.io.requestor(2) <> io.rocc.iptw
  ptw.io.requestor(3) <> io.rocc.dptw
  ptw.io.requestor(4) <> io.rocc.pptw

  io.tilelink.acquire <> memArb.io.out.acquire
  io.tilelink.grant <> memArb.io.out.grant
  io.tilelink.finish <> memArb.io.out.finish
  // Probes and releases routed directly to coherent dcache
  io.tilelink.probe <> dcache.io.mem.probe
  // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)
  io.tilelink.release.valid   := dcache.io.mem.release.valid
  dcache.io.mem.release.ready := io.tilelink.release.ready
  io.tilelink.release.bits := dcache.io.mem.release.bits
  io.tilelink.release.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.bits.payload.client_xact_id, UInt(dcPortId, log2Up(params(NTilePorts))))

}
