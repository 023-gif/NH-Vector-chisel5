package xiangshan.vector.vbackend.vissue.vprs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{FuType, MicroOp, Redirect, SrcState, SrcType, XSBundle, XSModule}
import xiangshan.backend.issue._
import xiangshan.backend.rob.RobPtr
import xs.utils.PickOneLow
class VprsStatusArrayEntry(implicit p: Parameters) extends XSBundle{
  val robPtr: RobPtr = new RobPtr
  val prs: UInt = UInt(PhyRegIdxWidth.W)
  val pvs1: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvs2: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pov: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvm: UInt = UInt(PhyRegIdxWidth.W)
  val prsType: UInt = SrcType()
  val prsState: UInt = SrcState()
  val pvs1States: Vec[UInt] = Vec(8, SrcState())
  val pvs2States: Vec[UInt] = Vec(8, SrcState())
  val povStates: Vec[UInt] = Vec(8, SrcState())
  val pvmState: UInt = SrcState()
  val allMerged: Bool = Bool()
}

class VprsStatusArrayEntryUpdateNetwork(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val enq = Input(Valid(new MicroOp))
    val enqIsMerge = Input(Bool())
    val entry = Input(Valid(new VprsStatusArrayEntry))
    val issued = Input(Bool())
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))

    val entryNext = Output(Valid(new VprsStatusArrayEntry))
    val updateEnable = Output(Bool())
  })

  private val enqEntryNext = WireInit(io.entry)
  when(io.enq.valid){
    val agnostic = (io.enq.bits.vCsrInfo.vta(0) && io.enq.bits.isTail) || (io.enq.bits.vCsrInfo.vma(0) && io.enq.bits.ctrl.vm)
    when(io.enqIsMerge){
      assert(io.entry.valid)
      enqEntryNext.bits.pvs1(io.enq.bits.uopIdx) := io.enq.bits.psrc(0)
      enqEntryNext.bits.pvs1States(io.enq.bits.uopIdx) := Mux(SrcType.isVec(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      enqEntryNext.bits.pvs2(io.enq.bits.uopIdx) := io.enq.bits.psrc(1)
      enqEntryNext.bits.pvs2States(io.enq.bits.uopIdx) := io.enq.bits.srcState(1)
      enqEntryNext.bits.pov(io.enq.bits.uopIdx) := io.enq.bits.psrc(2)
      enqEntryNext.bits.povStates(io.enq.bits.uopIdx) := Mux(agnostic, SrcState.rdy, io.enq.bits.psrc(2))
      enqEntryNext.bits.allMerged := io.enq.bits.uopNum === io.enq.bits.uopIdx
    }.otherwise{
      assert(!io.entry.valid)
      val src1IsVec = SrcType.isVec(io.enq.bits.ctrl.srcType(0))
      enqEntryNext.bits.robPtr := io.enq.bits.robIdx
      enqEntryNext.bits.prs := io.enq.bits.psrc(0)
      enqEntryNext.bits.prsType := io.enq.bits.ctrl.srcType(0)
      enqEntryNext.bits.prsState := Mux(SrcType.isRegOrFp(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      enqEntryNext.bits.pvs1(0) := io.enq.bits.psrc(0)
      enqEntryNext.bits.pvs1States(0) := Mux(SrcType.isVec(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      enqEntryNext.bits.pvs2(0) := io.enq.bits.psrc(1)
      enqEntryNext.bits.pvs2States(0) := io.enq.bits.srcState(1)
      enqEntryNext.bits.pov(0) := io.enq.bits.psrc(2)
      enqEntryNext.bits.povStates(0) := Mux(agnostic, SrcState.rdy, io.enq.bits.psrc(2))
      enqEntryNext.bits.pvm := io.enq.bits.vm
      enqEntryNext.bits.pvmState(0) := Mux(io.enq.bits.ctrl.vm, io.enq.bits.vmState, SrcState.rdy)
      enqEntryNext.bits.allMerged := io.enq.bits.uopNum === 0.U
      enqEntryNext.valid := true.B

      enqEntryNext.bits.pvs1States.zipWithIndex.drop(1).foreach({ case (s,i) =>
        s := Mux(src1IsVec, Mux(i.U <= io.enq.bits.uopNum, SrcState.busy, SrcState.rdy), SrcState.rdy)
      })
      enqEntryNext.bits.pvs2States.zipWithIndex.drop(1).foreach({ case (s, i) =>
        s := Mux(i.U <= io.enq.bits.uopNum, SrcState.busy, SrcState.rdy)
      })
      enqEntryNext.bits.povStates.zipWithIndex.drop(1).foreach({ case (s, i) =>
        s := Mux(i.U <= io.enq.bits.uopNum, Mux(agnostic, SrcState.rdy, SrcState.busy), SrcState.rdy)
      })
    }
  }.elsewhen(io.entry.valid && io.entry.bits.robPtr.needFlush(io.redirect) || io.issued) {
    assert(enqEntryNext.valid)
    enqEntryNext.valid := false.B
  }

  private val enqOrRedirectOrDeqUpdateEn = io.entry.valid && io.entry.bits.robPtr.needFlush(io.redirect) || io.enq.valid || io.issued

  private val wkpEntryNext = WireInit(io.entry)
  private val rsWakeupValid = io.scalarWakeUps.map(wkp=> {
    io.entry.valid && wkp.valid && wkp.bits.destType === io.entry.bits.prsType &&
      io.entry.bits.prs === wkp.bits.pdest &&
      !(SrcType.isReg(io.entry.bits.prsType) && io.entry.bits.prs === 0.U)
  }).reduce(_|_)
  when(rsWakeupValid){
    assert(wkpEntryNext.bits.prs === SrcState.busy)
    wkpEntryNext.bits.prs := SrcState.rdy
  }

  private val pvsSeq = io.entry.bits.pvs1 ++ io.entry.bits.pvs2 ++ io.entry.bits.pov :+ io.entry.bits.pvm
  private val pvsStateSeq = wkpEntryNext.bits.pvs1States ++ wkpEntryNext.bits.pvs2States ++ wkpEntryNext.bits.povStates :+ wkpEntryNext.bits.pvmState
  private val pvsWkpHitsSeq = pvsSeq.map(pv => {
    io.vectorWakeUps.map(wkp => {io.entry.valid && wkp.valid && pv === wkp.bits.pdest}).reduce(_|_)
  })
  pvsStateSeq.zip(pvsWkpHitsSeq).foreach({case(s, w) => when(w){s := SrcState.rdy}})
  private val wkpUpdateEn = pvsWkpHitsSeq.reduce(_|_) || rsWakeupValid

  io.entryNext := Mux(enqOrRedirectOrDeqUpdateEn, enqEntryNext, wkpEntryNext)
  io.updateEnable := enqOrRedirectOrDeqUpdateEn || wkpUpdateEn
}

class VprsStatusArray(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends XSModule{
  private val size = vectorParameters.vPRsDepth
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled(new MicroOp()))
    val selectInfo = Output(Vec(size, Valid(new VprsStatusArrayEntry)))
    val issueOH = Input(Valid(UInt(size.W)))
    val enqToPayload = Output(Valid(UInt(size.W)))
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))
  })

  private val array = Reg(Vec(size, new VprsStatusArrayEntry))
  private val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val validsAux = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val robIdxAux = Reg(Vec(size, new RobPtr))

  for(((port, entry), v) <- io.selectInfo.zip(array).zip(valids)){
    val selCond0 = entry.prsState === SrcState.rdy
    val selCond1 = entry.pvs1States.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond2 = entry.pvs2States.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond3 = entry.povStates.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond4 = entry.pvmState === SrcState.rdy
    port.valid := v && selCond0 && selCond1 && selCond2 && selCond3 && selCond4 && entry.allMerged
    port.bits := entry
  }

  private val mergeVec = valids.zip(array).map({case(v, e) =>
    v && e.robPtr === io.enq.bits.robIdx
  })
  private val mergeAuxVec = validsAux.zip(robIdxAux).map({ case (v, e) =>
    v && e === io.enq.bits
  })

  when(io.enq.valid){assert(PopCount(mergeVec) <= 1.U)}

  private val emptyEntry = PickOneLow(validsAux)

  private val needMerge = mergeVec.reduce(_|_)
  private val needMergeAux = mergeAuxVec.reduce(_|_)
  private val needAlloc = !needMergeAux && emptyEntry.valid && io.enq.valid

  io.enq.ready := needMergeAux | emptyEntry.valid

  io.enqToPayload.valid := needAlloc
  io.enqToPayload.bits := emptyEntry.bits

  for(((v, e), i) <- valids.zip(array).zipWithIndex){
    val updateNetwork = Module(new VprsStatusArrayEntryUpdateNetwork(sWkpWidth, vWkpWidth))
    updateNetwork.io.enq.valid := io.enq.fire
    updateNetwork.io.enq.bits := io.enq.bits
    updateNetwork.io.enqIsMerge := needMerge
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := e
    updateNetwork.io.issued := io.issueOH.valid && io.issueOH.bits(i)
    updateNetwork.io.scalarWakeUps := io.scalarWakeUps
    updateNetwork.io.vectorWakeUps := io.vectorWakeUps
    updateNetwork.io.redirect := io.redirect
    when(updateNetwork.io.updateEnable){
      v := updateNetwork.io.entryNext.valid
      validsAux(i) := updateNetwork.io.entryNext.valid
      e := updateNetwork.io.entryNext.bits
      robIdxAux(i) := updateNetwork.io.entryNext.bits.robPtr
    }
  }
  assert(valids === validsAux)
  for(i <- 0 until size){
    when(valids(i)){assert(robIdxAux(i) === array(i).robPtr)}
  }
  when(io.issueOH.valid){assert((valids.asUInt & io.issueOH.bits).orR)}
}