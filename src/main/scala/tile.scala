// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import dma._
import Util._

case object CoreName extends Field[String]
case object NDCachePorts extends Field[Int]
case object NTilePorts extends Field[Int]
case object NPTWPorts extends Field[Int]
case object UseRoCC extends Field[Boolean]

abstract class Tile(resetSignal: Bool = null) extends Module(_reset = resetSignal) {
  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO
    val temac = new TEMACIO
    val dma = new DMAControlIO().flip
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
  io.dma <> core.io.dma

  val memArb = Module(new TileLinkIOArbiterThatAppendsArbiterId(params(NTilePorts)))
  io.tilelink <> memArb.io.out
  memArb.io.in(0) <> dcache.io.mem
  memArb.io.in(1) <> TileLinkIOWrapper(icache.io.mem)

  // wire RoCC connections to port
  if (params(UseRoCC)) {
    val dcIF = Module(new SimpleHellaCacheIF)
    core.io.rocc <> io.rocc
    dcIF.io.requestor <> io.rocc.mem
    dcArb.io.requestor(2) <> dcIF.io.cache
    memArb.io.in(2) <> io.rocc.imem
    ptw.io.requestor(2) <> io.rocc.iptw
    ptw.io.requestor(3) <> io.rocc.dptw
    ptw.io.requestor(4) <> io.rocc.pptw
  }
}
