// TEMAC state machines for rocket

package rocket

import Chisel._
import Util._
import Node._
import uncore._
import scala.math._

class TransmitMachine extends Module
{
  val io = new TEMACIOTransmit

  val idle :: issue_write :: Nil = Enum(UInt(), 2)

  val state = Reg(init=idle)

  io.tx_axis_fifo_tdata := io.txd_in(7, 0)
  io.tx_axis_fifo_tlast := io.txd_in(9)
  io.tx_axis_fifo_tvalid := (state === issue_write)
  
  // default
  state := state

  when (state === idle) {
    when (io.write_to_txd) {
      state := issue_write
    }
  } .elsewhen (state === issue_write) {
    when (io.tx_axis_fifo_tready) {
      state := idle
    }
  }
}

class ReceiveMachine extends Module
{
  // RXD
  // _ _ _ _ _ _ _ _ _ _ 
  // 9 8 7 6 5 4 3 2 1 0
  // ^ ^ ^-------------^---- data
  // | |------ 0 = ready, 1 = valid
  // |----- last signal
  val io = new TEMACIOReceive

  val rxd_reg = Reg(init=UInt(0, 64))
  io.rxd_val := rxd_reg

  io.rx_axis_fifo_tready := (rxd_reg(8) === UInt(0))

  when (io.rxd_val_in_valid) {
    rxd_reg := io.rxd_val_in
  } .elsewhen (rxd_reg(8) === UInt(0) && io.rx_axis_fifo_tvalid) {
    rxd_reg := Cat(io.rx_axis_fifo_tlast, UInt(1), io.rx_axis_fifo_tdata)
  }

}

class ManagementMachine extends Module 
{
  val io = new TEMACIOManage


  // io.write_to_cfga - this is the "start" signal
  // io.cfgd
  // io.stall_out - indicates whether or not to stall


  val idle :: issue_addr :: issue_data :: wait_for_data_in :: confirm :: Nil = Enum(UInt(), 5)

  val state = Reg(init=idle)

  val handled = Reg(init=Bits(0))

  val bringup_sfp_tx_disable = Reg(init=UInt(0, 32))

  io.s_axi_awvalid := (state === issue_addr) && io.cfgd_in(32)
  io.s_axi_arvalid := (state === issue_addr) && (io.cfgd_in(32) === UInt(0))

  io.s_axi_wvalid := (state === issue_data) 
  io.s_axi_bready := (state === confirm)

  io.s_axi_rready := (state === wait_for_data_in)

  io.stall_out := ((state != idle) || (io.write_to_cfga)) && (!handled) // stall when not in idle, or when transitioning out of idle

  io.sfp_tx_disable := (bringup_sfp_tx_disable < UInt(50000000))

  // default:
  state := state

  // counter idles at 0
  when (bringup_sfp_tx_disable === UInt(0)) {
    bringup_sfp_tx_disable := bringup_sfp_tx_disable
  } .elsewhen (bringup_sfp_tx_disable === UInt(100000000)) {
    bringup_sfp_tx_disable := UInt(0)
  } .otherwise {
    bringup_sfp_tx_disable := bringup_sfp_tx_disable + UInt(1)
  }

  when (state === idle) {
    handled := Bits(0)
    when (io.write_to_cfga) {
      state := issue_addr
    }
  } .elsewhen (state === issue_addr) {
    when (io.cfgd_in(32) && io.s_axi_awready) {
      state := issue_data
    } .elsewhen ((io.cfgd_in(32) === UInt(0)) && io.s_axi_arready) {
      state := wait_for_data_in
    }
  } .elsewhen (state === issue_data) {
    when (io.s_axi_wready) {
      state := confirm
    }
  } .elsewhen (state === wait_for_data_in) {
    when (io.s_axi_rvalid) {
      // actually capturing happens in csr.scala
      state := idle
      handled := Bits(1) // "software stall" makes this irrelevant
    }
  } .elsewhen (state === confirm) {
    when (io.s_axi_bvalid) {
      state := idle
      handled := Bits(1) // "software stall" makes this irrelevant
      when (io.cfgd_in(33)) { //cfgd bit 33 indicates launch sfp_tx_disable
        // kickoff the sfp_tx_disable reset cycle after turning off isolate
        bringup_sfp_tx_disable := UInt(1)
      }
    }
  } .otherwise {
    // should never happen
    state := idle
  }

}

class TEMACIOTransmit extends Bundle {
  val tx_axis_fifo_tdata = UInt(OUTPUT, 8)
  val tx_axis_fifo_tvalid = Bool(OUTPUT)
  val tx_axis_fifo_tready = Bool(INPUT)
  val tx_axis_fifo_tlast = Bool(OUTPUT)

  val txd_in = UInt(INPUT, 64)
  val write_to_txd = Bool(INPUT)
}

class TEMACIOReceive extends Bundle {
  // recv
  val rx_axis_fifo_tdata = UInt(INPUT, 8)
  val rx_axis_fifo_tvalid = Bool(INPUT)
  val rx_axis_fifo_tready = Bool(OUTPUT)
  val rx_axis_fifo_tlast = Bool(INPUT)

  val rxd_val = UInt(OUTPUT, 64)
  val rxd_val_in = UInt(INPUT, 64)
  val rxd_val_in_valid = Bool(INPUT)
}


class TEMACIO extends Bundle {
  // recv
  val rx_axis_fifo_tdata = UInt(INPUT, 8)
  val rx_axis_fifo_tvalid = Bool(INPUT)
  val rx_axis_fifo_tready = Bool(OUTPUT)
  val rx_axis_fifo_tlast = Bool(INPUT)

  // transmit
  val tx_axis_fifo_tdata = UInt(OUTPUT, 8)
  val tx_axis_fifo_tvalid = Bool(OUTPUT)
  val tx_axis_fifo_tready = Bool(INPUT)
  val tx_axis_fifo_tlast = Bool(OUTPUT)

  // config
  val s_axi_awaddr = UInt(OUTPUT, 12)
  val s_axi_awvalid = Bool(OUTPUT)
  val s_axi_awready = Bool(INPUT)

  val s_axi_wdata = UInt(OUTPUT, 32)
  val s_axi_wvalid = Bool(OUTPUT)
  val s_axi_wready = Bool(INPUT)

  val s_axi_bresp = UInt(INPUT, 2)
  val s_axi_bvalid = Bool(INPUT)
  val s_axi_bready = Bool(OUTPUT)

  val s_axi_araddr = UInt(OUTPUT, 12)
  val s_axi_arvalid = Bool(OUTPUT)
  val s_axi_arready = Bool(INPUT)

  val s_axi_rdata = UInt(INPUT, 32)
  val s_axi_rresp = UInt(INPUT, 2)
  val s_axi_rvalid = Bool(INPUT)
  val s_axi_rready = Bool(OUTPUT)

  val sfp_tx_disable = Bool(OUTPUT)
}

class TEMACIOManage extends Bundle {
  // config
  val stall_out = Bool(OUTPUT)
  val write_to_cfga = Bool(INPUT)
  val cfgd_in = UInt(INPUT, 64)

  val s_axi_awvalid = Bool(OUTPUT) // in mgt
  val s_axi_awready = Bool(INPUT) // used mgt

  val s_axi_wvalid = Bool(OUTPUT) // in mgt
  val s_axi_wready = Bool(INPUT) // used mgt

  val s_axi_bresp = UInt(INPUT, 2) // unused?
  val s_axi_bvalid = Bool(INPUT) // used mgt
  val s_axi_bready = Bool(OUTPUT) // in mgt

  val s_axi_arvalid = Bool(OUTPUT)
  val s_axi_arready = Bool(INPUT)

  val s_axi_rdata = UInt(INPUT, 32)
  val s_axi_rresp = UInt(INPUT, 2)
  val s_axi_rvalid = Bool(INPUT)
  val s_axi_rready = Bool(OUTPUT)

  val sfp_tx_disable = Bool(OUTPUT)
}
