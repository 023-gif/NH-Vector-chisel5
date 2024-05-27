package xiangshan.backend.issue.MemRs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.SelectResp
import xiangshan.{FuType, MicroOp, Redirect, SrcType, XSModule}
import xs.utils.{GTimer, LogicShiftRight}

sealed class MemPipelineEnqBundle(chosenNum:Int, bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val selectResp = new SelectResp(bankIdxWidth, entryIdxWidth)
  val uop = new MicroOp
  val chosen = UInt(chosenNum.W)
  val stdSel = UInt(bankIdxWidth.W)
  val staSel = UInt(bankIdxWidth.W)
  val loadSel = UInt(bankIdxWidth.W)
}

sealed class MemPipelineDeqBundle(chosenNum:Int, bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop = new MicroOp
  val bankIdxOH: UInt = UInt(bankIdxWidth.W)
  val entryIdxOH: UInt = UInt(entryIdxWidth.W)
  val chosen: UInt = UInt(chosenNum.W)
  val stdSel: UInt = UInt(bankIdxWidth.W)
  val staSel: UInt = UInt(bankIdxWidth.W)
  val loadSel: UInt = UInt(bankIdxWidth.W)
}
//
//class MemoryIssuePipeline(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
//  val io = IO(new Bundle{
//    val redirect = Input(Valid(new Redirect))
//    val enq = Flipped(DecoupledIO(new MemPipelineEnqBundle(3, bankIdxWidth, entryIdxWidth)))
//    val deq = DecoupledIO(new MemPipelineDeqBundle(bankIdxWidth, entryIdxWidth)
//    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
//    val issueFire = Output(Bool())
//    val hold = Output(Bool())
//    val isLoad = Output(Bool())
//    val ldStop = Input(Bool())
//  })
//  private val hold = RegInit(false.B)
//  private val enqInfo = io.enq.bits.selectResp.info
//  when(io.enq.fire && enqInfo.isVector && enqInfo.isSgOrStride && enqInfo.fuType =/= FuType.std){
//    hold := true.B
//  }.elsewhen(hold){
//    hold := false.B
//  }
//  io.hold := hold
//
//  io.enq.ready := io.deq.ready && !hold && !io.ldStop
//  io.issueFire := !hold && io.deq.fire
//
//  private val deqValidDriverReg = RegInit(false.B)
//  private val deqDataDriverReg = Reg(new SelectResp(bankIdxWidth, entryIdxWidth))
//  private val shouldBeFlushed = deqDataDriverReg.info.robPtr.needFlush(io.redirect)
//  private val shouldBeCanceled = deqDataDriverReg.info.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
//  io.isLoad := deqDataDriverReg.info.fuType === FuType.ldu
//  when(!hold | shouldBeFlushed | shouldBeCanceled){
//    deqValidDriverReg := io.enq.fire
//  }
//  when(hold){
//    deqDataDriverReg.info.lpv.foreach(l => l := LogicShiftRight(l,1))
//  }.elsewhen(io.enq.fire){
//    deqDataDriverReg := io.enq.bits.selectResp
//  }
//  private val timer = GTimer()
//  io.deq.valid := deqValidDriverReg && !shouldBeCanceled && !io.ldStop
//  io.deq.bits.uop := io.enq.bits.uop
//  io.deq.bits.uop.debugInfo.selectTime := timer
//  io.deq.bits.uop.debugInfo.issueTime := timer + 1.U
//  io.deq.bits.uop.robIdx := deqDataDriverReg.info.robPtr
//  io.deq.bits.uop.ctrl.rfWen := deqDataDriverReg.info.rfWen
//  io.deq.bits.uop.ctrl.fpWen := deqDataDriverReg.info.fpWen
//  io.deq.bits.uop.pdest := deqDataDriverReg.info.pdest
//  io.deq.bits.uop.ctrl.fuType := deqDataDriverReg.info.fuType
//  io.deq.bits.uop.psrc := deqDataDriverReg.info.psrc
//  io.deq.bits.uop.vm := deqDataDriverReg.info.vm
//  io.deq.bits.uop.lpv.zip(deqDataDriverReg.info.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
//  io.deq.bits.entryIdxOH := deqDataDriverReg.entryIdxOH
//  io.deq.bits.bankIdxOH := deqDataDriverReg.bankIdxOH
//  io.deq.bits.uop.cf.ftqPtr := deqDataDriverReg.info.ftqPtr
//  io.deq.bits.uop.cf.ftqOffset := deqDataDriverReg.info.ftqOffset
//
//  private val isVec = deqDataDriverReg.info.isVector
//  private val isStd = deqDataDriverReg.info.fuType === FuType.std
//  when(!isVec && isStd) {
//    io.deq.bits.uop.ctrl.srcType(0) := io.enq.bits.uop.ctrl.srcType(1)
//    io.deq.bits.uop.psrc(0) := deqDataDriverReg.info.psrc(1)
//  }
//}

class MemoryIssuePipelineBlock(chosenNum:Int, bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new MemPipelineEnqBundle(chosenNum, bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new MemPipelineDeqBundle(chosenNum, bankIdxWidth, entryIdxWidth))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val issueFire = Output(Bool())
    val hold = Output(Bool())
    val isLoad = Output(Bool())
    val ldStop = Input(Bool())
  })
  private val enqInfo = io.enq.bits.selectResp.info
  io.hold := false.B

  io.enq.ready := io.deq.ready && !io.ldStop
  io.issueFire := io.deq.fire

  private val deqValidDriverReg = RegInit(false.B)
  private val deqDataDriverReg = Reg(new SelectResp(bankIdxWidth, entryIdxWidth))
  private val deqChosenNumReg = Reg(UInt(chosenNum.W))
  private val deqStaSel = Reg(UInt(bankIdxWidth.W))
  private val deqStdSel = Reg(UInt(bankIdxWidth.W))
  private val deqLoadSel = Reg(UInt(bankIdxWidth.W))


  private val shouldBeFlushed = deqDataDriverReg.info.robPtr.needFlush(io.redirect)
  private val shouldBeCanceled = deqDataDriverReg.info.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
  io.isLoad := deqDataDriverReg.info.fuType === FuType.ldu

  val enqFire = io.enq.valid && io.deq.ready && !io.ldStop
  when(io.deq.fire){ deqValidDriverReg := false.B }
  when(enqFire){ deqValidDriverReg := true.B }
  when(shouldBeFlushed | shouldBeCanceled) {
    deqValidDriverReg := false.B
  }
  when(enqFire){
    deqDataDriverReg := io.enq.bits.selectResp
    deqChosenNumReg := io.enq.bits.chosen
    deqStaSel := io.enq.bits.staSel
    deqStdSel := io.enq.bits.stdSel
    deqLoadSel := io.enq.bits.loadSel
  }

  private val timer = GTimer()
  io.deq.valid := deqValidDriverReg && !shouldBeCanceled && !io.ldStop
  io.deq.bits.uop := io.enq.bits.uop
  io.deq.bits.uop.debugInfo.selectTime := timer
  io.deq.bits.uop.debugInfo.issueTime := timer + 1.U
  io.deq.bits.uop.robIdx := deqDataDriverReg.info.robPtr
  io.deq.bits.uop.ctrl.rfWen := deqDataDriverReg.info.rfWen
  io.deq.bits.uop.ctrl.fpWen := deqDataDriverReg.info.fpWen
  io.deq.bits.uop.pdest := deqDataDriverReg.info.pdest
  io.deq.bits.uop.ctrl.fuType := deqDataDriverReg.info.fuType
  io.deq.bits.uop.psrc := deqDataDriverReg.info.psrc
  io.deq.bits.uop.vm := deqDataDriverReg.info.vm
  io.deq.bits.uop.lpv.zip(deqDataDriverReg.info.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
  io.deq.bits.entryIdxOH := deqDataDriverReg.entryIdxOH
  io.deq.bits.bankIdxOH := deqDataDriverReg.bankIdxOH
  io.deq.bits.uop.cf.ftqPtr := deqDataDriverReg.info.ftqPtr
  io.deq.bits.uop.cf.ftqOffset := deqDataDriverReg.info.ftqOffset
  io.deq.bits.chosen  := DontCare
  io.deq.bits.loadSel := DontCare
  io.deq.bits.staSel  := DontCare
  io.deq.bits.stdSel  := DontCare

  private val isVec = deqDataDriverReg.info.isVector
  private val isStd = deqDataDriverReg.info.fuType === FuType.std
  when(!isVec && isStd) {
    io.deq.bits.uop.ctrl.srcType(0) := io.enq.bits.uop.ctrl.srcType(1)
    io.deq.bits.uop.psrc(0) := deqDataDriverReg.info.psrc(1)
  }
}
