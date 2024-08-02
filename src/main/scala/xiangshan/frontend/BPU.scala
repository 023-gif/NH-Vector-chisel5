/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xs.utils._
import xs.utils.perf.HasPerfLogging

import scala.math.min
import scala.{Tuple2 => &}

trait HasBPUConst extends HasXSParameter {
  val MaxMetaLength = if (!env.FPGAPlatform) 512 else 150 // TODO: Reduce meta length

  val useBPD = true
  val numBrSlot = numBr-1
  val totalSlot = numBrSlot + 1

  val numDup = 5
  def dupForFtq = numDup - 1
  def dupForFtb = 0
  def dupForTageSC = 1
  def dupForUbtb = 2
  def dupForIttage = 3
  def dupForRas = 2

  def BP_STAGES = (0 until 3).map(_.U(2.W))
  def BP_S1 = BP_STAGES(0)
  def BP_S2 = BP_STAGES(1)
  def BP_S3 = BP_STAGES(2)

  def dup_seq[T](src: T, num: Int = numDup) = Seq.fill(num)(src)
  def dup[T <: Data](src: T, num: Int = numDup) = VecInit(Seq.fill(num)(src))
  def dup_wire[T <: Data](src: T, num: Int = numDup) = Wire(Vec(num, src.cloneType))
  def dup_idx = Seq.tabulate(numDup)(n => n.toString())
  val numBpStages = BP_STAGES.length

  val debug = true
  // TODO: Replace log2Up by log2Ceil
}

trait HasBPUParameter extends HasXSParameter with HasBPUConst {
  val BPUDebug = true && !env.FPGAPlatform && env.EnablePerfDebug
  val EnableCFICommitLog = true
  val EnbaleCFIPredLog = true
  val EnableBPUTimeRecord = (EnableCFICommitLog || EnbaleCFIPredLog) && !env.FPGAPlatform
  val EnableCommit = false
}

class BPUCtrl(implicit p: Parameters) extends XSBundle {
  val ubtb_enable = Bool()
  val btb_enable  = Bool()
  val bim_enable  = Bool()
  val tage_enable = Bool()
  val sc_enable   = Bool()
  val ras_enable  = Bool()
  val loop_enable = Bool()
}

trait BPUUtils extends HasXSParameter {

  // To be verified
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  def signedSatUpdate(old: SInt, len: Int, taken: Bool): SInt = {
    val oldSatTaken = old === ((1 << (len-1))-1).S
    val oldSatNotTaken = old === (-(1 << (len-1))).S
    Mux(oldSatTaken && taken, ((1 << (len-1))-1).S,
      Mux(oldSatNotTaken && !taken, (-(1 << (len-1))).S,
        Mux(taken, old + 1.S, old - 1.S)))
  }
}

class BasePredictorInput (implicit p: Parameters) extends XSBundle with HasBPUConst {
  def nInputs = 1

  val s0_pc = Vec(numDup, UInt(VAddrBits.W))

  val foldedHist = Vec(numDup, new AllFoldedHistories(foldedGHistInfos))
  // val s1_folded_hist = Vec(numDup, new AllFoldedHistories(foldedGHistInfos))
  val ghist = UInt(HistoryLength.W)

  val resp_in = Vec(nInputs, new BranchPredictionResp)

}

class BasePredictorOutput (implicit p: Parameters) extends BranchPredictionResp {}

class BasePredictorIO (implicit p: Parameters) extends XSBundle with HasBPUConst {
  val reset_vector = Input(UInt(PAddrBits.W))
  val in  = Flipped(DecoupledIO(new BasePredictorInput)) // TODO: Remove DecoupledIO
  // val out = DecoupledIO(new BasePredictorOutput)
  val out = Output(new BasePredictorOutput)
  // val flush_out = Valid(UInt(VAddrBits.W))

  val fauftb_entry_in = Input(new FTBEntry)
  val fauftb_entry_hit_in = Input(Bool())
  val fauftb_entry_out = Output(new FTBEntry)
  val fauftb_entry_hit_out = Output(Bool())
  val ctrl = Input(new BPUCtrl)

  val s0_fire = Input(Vec(numDup, Bool()))
  val s1_fire = Input(Vec(numDup, Bool()))
  val s2_fire = Input(Vec(numDup, Bool()))
  val s3_fire = Input(Vec(numDup, Bool()))

  val s2_redirect = Input(Vec(numDup, Bool()))
  val s3_redirect = Input(Vec(numDup, Bool()))

  val s1_ready = Output(Bool())
  val s2_ready = Output(Bool())
  val s3_ready = Output(Bool())

  val update = Vec(numDup, Flipped(Valid(new BranchPredictionUpdate)))
  val redirect = Flipped(Valid(new BranchPredictionRedirect))
  val redirectFromIFU = Input(Bool())
}

abstract class BasePredictor(implicit p: Parameters) extends XSModule
  with HasBPUConst with BPUUtils with HasPerfEvents with HasPerfLogging {
  val meta_size = 0
  val spec_meta_size = 0
  val is_fast_pred = false
  val io = IO(new BasePredictorIO())

  io.out := io.in.bits.resp_in(0)

  io.fauftb_entry_out := io.fauftb_entry_in
  io.fauftb_entry_hit_out := io.fauftb_entry_hit_in

  io.out.lastStageMeta := 0.U

  io.in.ready := !io.redirect.valid

  io.s1_ready := true.B
  io.s2_ready := true.B
  io.s3_ready := true.B

  val s0_pc_dup   = WireInit(io.in.bits.s0_pc) // fetchIdx(io.f0_pc)
  val s1_pc_dup   = s0_pc_dup.indices.map(i => Reg(UInt(VAddrBits.W)))
  for(((s1, s0), s0f) <- s1_pc_dup.zip(s0_pc_dup).zip(io.s0_fire)){
    when(reset.asBool) {
      s1 := io.reset_vector.asTypeOf(UInt(VAddrBits.W))
    }.elsewhen(s0f) {
      s1 := s0
    }
  }
  val s2_pc_dup   = s1_pc_dup.zip(io.s1_fire).map {case (s1_pc, s1_fire) => RegEnable(s1_pc, s1_fire)}
  val s3_pc_dup   = s2_pc_dup.zip(io.s2_fire).map {case (s2_pc, s2_fire) => RegEnable(s2_pc, s2_fire)}

  io.out.s1.pc := s1_pc_dup
  io.out.s2.pc := s2_pc_dup
  io.out.s3.pc := s3_pc_dup

  val perfEvents: Seq[(String, UInt)] = Seq()


  def getFoldedHistoryInfo: Option[Set[FoldedHistoryInfo]] = None
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.in.ready                 := true.B
  io.out.lastStageMeta      := 0.U
  io.out := io.in.bits.resp_in(0)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val resp = DecoupledIO(new BpuToFtqBundle())
}

class PredictorIO(implicit p: Parameters) extends XSBundle {
  val bpu_to_ftq = new BpuToFtqIO()
  val ftq_to_bpu = Flipped(new FtqToBpuIO())
  val ctrl = Input(new BPUCtrl)
  val reset_vector = Input(UInt(PAddrBits.W))
}


class Predictor(parentName:String = "Unknown")(implicit p: Parameters) extends XSModule
  with HasBPUConst with HasPerfEvents with HasCircularQueuePtrHelper with HasPerfLogging {
  val io = IO(new PredictorIO)

  val ctrl = DelayN(io.ctrl, 1)
  val predictors = Module(if (useBPD) new Composer(parentName = parentName) else new FakePredictor)

  // ctrl signal
  predictors.io.ctrl := ctrl
  predictors.io.reset_vector := io.reset_vector


  val s0_fire_dup, s1_fire_dup, s2_fire_dup, s3_fire_dup = dup_wire(Bool())
  val s1_valid_dup, s2_valid_dup, s3_valid_dup = RegInit(dup(false.B))
  val s1_ready_dup, s2_ready_dup, s3_ready_dup = dup_wire(Bool())
  val s1_components_ready_dup, s2_components_ready_dup, s3_components_ready_dup = dup_wire(Bool())

  val s0_pc_dup = Wire(Vec(numDup, UInt(VAddrBits.W)))
  val s0_pc_reg_dup = s0_pc_dup.indices.map(_ => Reg(UInt(VAddrBits.W)))
  s0_pc_reg_dup.zip(s0_pc_dup).foreach({case(a, b) =>
    when(reset.asBool){
      a := io.reset_vector.asTypeOf(UInt(VAddrBits.W))
    }.otherwise{
      a := b
    }
  })
  // for debug, would not appear in real RTL
  val s1_pc = RegEnable(s0_pc_dup(0), s0_fire_dup(0))
  val s2_pc = RegEnable(s1_pc, s1_fire_dup(0))
  val s3_pc = RegEnable(s2_pc, s2_fire_dup(0))

  val s0_folded_gh_dup = dup_wire(new AllFoldedHistories(foldedGHistInfos))
  val s0_folded_gh_reg_dup = s0_folded_gh_dup.map(x => RegNext(x, init=0.U.asTypeOf(s0_folded_gh_dup(0))))
  val s1_folded_gh_dup = RegEnable(s0_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s0_fire_dup(1))
  val s2_folded_gh_dup = RegEnable(s1_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s1_fire_dup(1))
  val s3_folded_gh_dup = RegEnable(s2_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s2_fire_dup(1))

  val s0_last_br_num_oh_dup = dup_wire(UInt((numBr+1).W))
  val s0_last_br_num_oh_reg_dup = s0_last_br_num_oh_dup.map(x => RegNext(x, init=0.U))
  val s1_last_br_num_oh_dup = RegEnable(s0_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s0_fire_dup(1))
  val s2_last_br_num_oh_dup = RegEnable(s1_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s1_fire_dup(1))
  val s3_last_br_num_oh_dup = RegEnable(s2_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s2_fire_dup(1))

  val s0_ahead_fh_oldest_bits_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  val s0_ahead_fh_oldest_bits_reg_dup = s0_ahead_fh_oldest_bits_dup.map(x => RegNext(x, init=0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup(0))))
  val s1_ahead_fh_oldest_bits_dup = RegEnable(s0_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s0_fire_dup(1))
  val s2_ahead_fh_oldest_bits_dup = RegEnable(s1_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s1_fire_dup(1))
  val s3_ahead_fh_oldest_bits_dup = RegEnable(s2_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s2_fire_dup(1))

  val npcGen_dup         = Seq.fill(numDup)(new PhyPriorityMuxGenerator[UInt])
  val foldedGhGen_dup    = Seq.fill(numDup)(new PhyPriorityMuxGenerator[AllFoldedHistories])
  val ghistPtrGen_dup    = Seq.fill(numDup)(new PhyPriorityMuxGenerator[CGHPtr])
  val lastBrNumOHGen_dup = Seq.fill(numDup)(new PhyPriorityMuxGenerator[UInt])
  val aheadFhObGen_dup   = Seq.fill(numDup)(new PhyPriorityMuxGenerator[AllAheadFoldedHistoryOldestBits])

  val ghvBitWriteGens = Seq.fill(HistoryLength)(new PhyPriorityMuxGenerator[Bool])
  // val ghistGen = new PhyPriorityMuxGenerator[UInt]

  val ghv = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
  val ghv_wire = WireInit(ghv)

  val s0_ghist = WireInit(0.U.asTypeOf(UInt(HistoryLength.W)))


  println(f"history buffer length ${HistoryLength}")
  val ghv_write_datas = Wire(Vec(HistoryLength, Bool()))
  val ghv_wens = Wire(Vec(HistoryLength, Bool()))

  val s0_ghist_ptr_dup = dup_wire(new CGHPtr)
  val s0_ghist_ptr_reg_dup = s0_ghist_ptr_dup.map(x => RegNext(x, init=0.U.asTypeOf(new CGHPtr)))
  val s1_ghist_ptr_dup = RegEnable(s0_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s0_fire_dup(1))
  val s2_ghist_ptr_dup = RegEnable(s1_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s1_fire_dup(1))
  val s3_ghist_ptr_dup = RegEnable(s2_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s2_fire_dup(1))

  def getHist(ptr: CGHPtr): UInt = (Cat(ghv_wire.asUInt, ghv_wire.asUInt) >> (ptr.value + 1.U))(HistoryLength-1, 0)
  s0_ghist := getHist(s0_ghist_ptr_dup(0))

  val resp = predictors.io.out


  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  val s1_flush_dup, s2_flush_dup, s3_flush_dup = dup_wire(Bool())
  val s2_redirect_dup, s3_redirect_dup = dup_wire(Bool())

  // predictors.io := DontCare
  predictors.io.in.valid := s0_fire_dup(0)
  predictors.io.in.bits.s0_pc := s0_pc_dup
  predictors.io.in.bits.ghist := s0_ghist
  predictors.io.in.bits.foldedHist := s0_folded_gh_dup
  // predictors.io.in.bits.s1_folded_hist := s1_folded_gh_dup
  predictors.io.in.bits.resp_in(0) := (0.U).asTypeOf(new BranchPredictionResp)
  predictors.io.fauftb_entry_in := (0.U).asTypeOf(new FTBEntry)
  predictors.io.fauftb_entry_hit_in := false.B
  predictors.io.redirectFromIFU := RegNext(io.ftq_to_bpu.redirctFromIFU, init=false.B)
  // predictors.io.in.bits.resp_in(0).s1.pc := s0_pc
  // predictors.io.in.bits.toFtq_fire := toFtq_fire

  // predictors.io.out.ready := io.bpu_to_ftq.resp.ready

  val redirect_req = io.ftq_to_bpu.redirect
  val do_redirect_dup = RegNext(dup(redirect_req), init=0.U.asTypeOf(dup(redirect_req)))
  do_redirect_dup.foreach(dontTouch(_))

  // Pipeline logic
  s2_redirect_dup.foreach(_ := false.B)
  s3_redirect_dup.foreach(_ := false.B)

  s3_flush_dup.foreach(_ := redirect_req.valid) // flush when redirect comes
  for (s2_flush & s3_flush & s3_redirect <- s2_flush_dup zip s3_flush_dup zip s3_redirect_dup)
    s2_flush := s3_flush || s3_redirect
  for (s1_flush & s2_flush & s2_redirect <- s1_flush_dup zip s2_flush_dup zip s2_redirect_dup)
    s1_flush := s2_flush || s2_redirect


  s1_components_ready_dup.foreach(_ := predictors.io.s1_ready)
  for (s1_ready & s1_fire & s1_valid <- s1_ready_dup zip s1_fire_dup zip s1_valid_dup)
    s1_ready := s1_fire || !s1_valid
  for (s0_fire & s1_components_ready & s1_ready <- s0_fire_dup zip s1_components_ready_dup zip s1_ready_dup)
    s0_fire := s1_components_ready && s1_ready
  predictors.io.s0_fire := s0_fire_dup

  s2_components_ready_dup.foreach(_ := predictors.io.s2_ready)
  for (s2_ready & s2_fire & s2_valid <- s2_ready_dup zip s2_fire_dup zip s2_valid_dup)
    s2_ready := s2_fire || !s2_valid
  for (s1_fire & s2_components_ready & s2_ready & s1_valid <- s1_fire_dup zip s2_components_ready_dup zip s2_ready_dup zip s1_valid_dup)
    s1_fire := s1_valid && s2_components_ready && s2_ready && io.bpu_to_ftq.resp.ready

  s3_components_ready_dup.foreach(_ := predictors.io.s3_ready)
  for (s3_ready & s3_fire & s3_valid <- s3_ready_dup zip s3_fire_dup zip s3_valid_dup)
    s3_ready := s3_fire || !s3_valid
  for (s2_fire & s3_components_ready & s3_ready & s2_valid <- s2_fire_dup zip s3_components_ready_dup zip s3_ready_dup zip s2_valid_dup)
    s2_fire := s2_valid && s3_components_ready && s3_ready

  for (s0_fire & s1_flush & s1_fire & s1_valid <- s0_fire_dup zip s1_flush_dup zip s1_fire_dup zip s1_valid_dup) {
    when (redirect_req.valid) { s1_valid := false.B }
      .elsewhen(s0_fire)      { s1_valid := true.B  }
      .elsewhen(s1_flush)     { s1_valid := false.B }
      .elsewhen(s1_fire)      { s1_valid := false.B }
    dontTouch(s1_valid)
  }
  predictors.io.s1_fire := s1_fire_dup

  s2_fire_dup := s2_valid_dup

  for (s1_fire & s2_flush & s2_fire & s2_valid & s1_flush <-
    s1_fire_dup zip s2_flush_dup zip s2_fire_dup zip s2_valid_dup zip s1_flush_dup) {
      
    when (s2_flush)      { s2_valid := false.B   }
      .elsewhen(s1_fire) { s2_valid := !s1_flush }
      .elsewhen(s2_fire) { s2_valid := false.B   }
    dontTouch(s2_valid)
  }

  predictors.io.s2_fire := s2_fire_dup
  predictors.io.s2_redirect := s2_redirect_dup

  s3_fire_dup := s3_valid_dup

  for (s2_fire & s3_flush & s3_fire & s3_valid & s2_flush <-
    s2_fire_dup zip s3_flush_dup zip s3_fire_dup zip s3_valid_dup zip s2_flush_dup) {
      
    when (s3_flush)      { s3_valid := false.B   }
      .elsewhen(s2_fire) { s3_valid := !s2_flush }
      .elsewhen(s3_fire) { s3_valid := false.B   }
    dontTouch(s3_valid)
  }

  predictors.io.s3_fire := s3_fire_dup
  predictors.io.s3_redirect := s3_redirect_dup


  io.bpu_to_ftq.resp.valid :=
    s1_valid_dup(2) && s2_components_ready_dup(2) && s2_ready_dup(2) ||
    s2_fire_dup(2) && s2_redirect_dup(2) ||
    s3_fire_dup(2) && s3_redirect_dup(2)
  io.bpu_to_ftq.resp.bits  := predictors.io.out
  io.bpu_to_ftq.resp.bits.lastStageSpecInfo.histPtr     := s3_ghist_ptr_dup(2)

  npcGen_dup.zip(s0_pc_reg_dup).foreach{ case (gen, reg) =>
    gen.register(true.B, reg, Some("stallPC"), 0)}
  foldedGhGen_dup.zip(s0_folded_gh_reg_dup).foreach{ case (gen, reg) =>
    gen.register(true.B, reg, Some("stallFGH"), 0)}
  ghistPtrGen_dup.zip(s0_ghist_ptr_reg_dup).foreach{ case (gen, reg) =>
    gen.register(true.B, reg, Some("stallGHPtr"), 0)}
  lastBrNumOHGen_dup.zip(s0_last_br_num_oh_reg_dup).foreach{ case (gen, reg) =>
    gen.register(true.B, reg, Some("stallBrNumOH"), 0)}
  aheadFhObGen_dup.zip(s0_ahead_fh_oldest_bits_reg_dup).foreach{ case (gen, reg) =>
    gen.register(true.B, reg, Some("stallAFHOB"), 0)}

  // History manage
  // s1
  val s1_possible_predicted_ghist_ptrs_dup = s1_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s1_predicted_ghist_ptr_dup = s1_possible_predicted_ghist_ptrs_dup.zip(resp.s1.lastBrPosOH).map{ case (ptr, oh) => Mux1H(oh, ptr)}
  val s1_possible_predicted_fhs_dup =
    for (fgh & afh & br_num_oh & t & br_pos_oh <-
      s1_folded_gh_dup zip s1_ahead_fh_oldest_bits_dup zip s1_last_br_num_oh_dup zip resp.s1.brTaken zip resp.s1.lastBrPosOH)
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, t & br_pos_oh(i))
      )
  val s1_predicted_fh_dup = resp.s1.lastBrPosOH.zip(s1_possible_predicted_fhs_dup).map{ case (oh, fh) => Mux1H(oh, fh)}

  val s1_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s1_ahead_fh_ob_src_dup.zip(s1_ghist_ptr_dup).foreach { case (src, ptr) => src.read(ghv, ptr)}

  if (EnableGHistDiff) {
    val s1_predicted_ghist = WireInit(getHist(s1_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    when (resp.s1.shouldShiftVec(0)) {
      s1_predicted_ghist(0) := resp.s1.brTaken(0)
    }

    when (s1_valid_dup(0)) {
      s0_ghist := s1_predicted_ghist.asUInt
    }
  }

  val s1_ghv_wens = (0 until HistoryLength).map(n =>
    s1_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s1.shouldShiftVec(0) && s1_valid_dup(0)
  )
  val s1_ghv_wdatas = (0 until HistoryLength).map(n =>
    s1_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s1.shouldShiftVec(0) &&
    resp.s1.brTaken(0) && resp.s1.lastBrPosOH(0)(1)
  )

  for (npcGen & s1_valid & s1_target <- npcGen_dup zip s1_valid_dup zip resp.s1.target)
    npcGen.register(s1_valid, s1_target, Some("s1_target"), 4)
  for (foldedGhGen & s1_valid & s1_predicted_fh <- foldedGhGen_dup zip s1_valid_dup zip s1_predicted_fh_dup)
    foldedGhGen.register(s1_valid, s1_predicted_fh, Some("s1_FGH"), 4)
  for (ghistPtrGen & s1_valid & s1_predicted_ghist_ptr <- ghistPtrGen_dup zip s1_valid_dup zip s1_predicted_ghist_ptr_dup)
    ghistPtrGen.register(s1_valid, s1_predicted_ghist_ptr, Some("s1_GHPtr"), 4)
  for (lastBrNumOHGen & s1_valid & s1_brPosOH <- lastBrNumOHGen_dup zip s1_valid_dup zip resp.s1.lastBrPosOH.map(_.asUInt))
    lastBrNumOHGen.register(s1_valid, s1_brPosOH, Some("s1_BrNumOH"), 4)
  for (aheadFhObGen & s1_valid & s1_ahead_fh_ob_src <- aheadFhObGen_dup zip s1_valid_dup zip s1_ahead_fh_ob_src_dup)
    aheadFhObGen.register(s1_valid, s1_ahead_fh_ob_src, Some("s1_AFHOB"), 4)
  ghvBitWriteGens.zip(s1_ghv_wens).zipWithIndex.foreach{case ((b, w), i) =>
    b.register(w, s1_ghv_wdatas(i), Some(s"s1_new_bit_$i"), 4)
  }

  class PreviousPredInfo extends Bundle {
    val target = UInt(VAddrBits.W)
    val lastBrPosOH = UInt((numBr+1).W)
    val taken = Bool()
    val cfiIndex = UInt(log2Ceil(PredictWidth).W)
  }

  def preds_needs_redirect_vec_dup(x: Seq[PreviousPredInfo], y: BranchPredictionBundle) = {
    val target_diff = x.zip(y.target).map {case (t1, t2) => t1.target =/= t2 }
    val lastBrPosOH_diff = x.zip(y.lastBrPosOH).map {case (oh1, oh2) => oh1.lastBrPosOH.asUInt =/= oh2.asUInt}
    val taken_diff = x.zip(y.taken).map {case (t1, t2) => t1.taken =/= t2}
    val takenOffset_diff = x.zip(y.cfiIndex).map {case (i1, i2) => i1.cfiIndex =/= i2.bits}
    VecInit(
      for (tgtd & lbpohd & tkd & tod <-
        target_diff zip lastBrPosOH_diff zip taken_diff zip takenOffset_diff)
        yield VecInit(tgtd, lbpohd, tkd, tod)
    )
  }

  // s2
  val s2_possible_predicted_ghist_ptrs_dup = s2_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s2_predicted_ghist_ptr_dup = s2_possible_predicted_ghist_ptrs_dup.zip(resp.s2.lastBrPosOH).map{ case (ptr, oh) => Mux1H(oh, ptr)}

  val s2_possible_predicted_fhs_dup = 
    for (fgh & afh & br_num_oh & fullPred <-
      s2_folded_gh_dup zip s2_ahead_fh_oldest_bits_dup zip s2_last_br_num_oh_dup zip resp.s2.fullPred)
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, if (i > 0) fullPred.br_taken else false.B)
      )
  val s2_predicted_fh_dup = resp.s2.lastBrPosOH.zip(s2_possible_predicted_fhs_dup).map{ case (oh, fh) => Mux1H(oh, fh)}

  val s2_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s2_ahead_fh_ob_src_dup.zip(s2_ghist_ptr_dup).foreach{ case (src, ptr) => src.read(ghv, ptr)}

  if (EnableGHistDiff) {
    val s2_predicted_ghist = WireInit(getHist(s2_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    when (resp.s2.shouldShiftVec(0)) {
      s2_predicted_ghist(0) := resp.s2.brTaken(0)
    }

    when(s2_redirect_dup(0)) {
      s0_ghist := s2_predicted_ghist.asUInt
    }
  }

  val s2_ghv_wens = (0 until HistoryLength).map(n =>
    s2_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s2.shouldShiftVec(0) && s2_redirect_dup(0))
  val s2_ghv_wdatas = (0 until HistoryLength).map(n =>
    s2_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s2.shouldShiftVec(0) &&
    resp.s2.fullPred(0).realBrTaken
  )

  val s1_pred_info = dup_wire(new PreviousPredInfo)
  s1_pred_info.zip(resp.s1.target).foreach(tp => tp._1.target := tp._2)
  s1_pred_info.zip(resp.s1.lastBrPosOH).foreach(tp => tp._1.lastBrPosOH := tp._2.asUInt)
  s1_pred_info.zip(resp.s1.taken).foreach(tp => tp._1.taken := tp._2)
  s1_pred_info.zip(resp.s1.cfiIndex).foreach(tp => tp._1.cfiIndex := tp._2.bits)

  val previous_s1_pred_info = RegEnable(s1_pred_info, 0.U.asTypeOf(s1_pred_info), s1_fire_dup(0))

  val s2_redirect_s1_last_pred_vec_dup = preds_needs_redirect_vec_dup(previous_s1_pred_info, resp.s2)

  for (s2_redirect & s2_fire & s2_redirect_s1_last_pred_vec <- s2_redirect_dup zip s2_fire_dup zip s2_redirect_s1_last_pred_vec_dup)
    s2_redirect := s2_fire && s2_redirect_s1_last_pred_vec.reduce(_||_)

  for (npcGen & s2_redirect & s2_target <- npcGen_dup zip s2_redirect_dup zip resp.s2.target)
    npcGen.register(s2_redirect, s2_target, Some("s2_target"), 5)
  for (foldedGhGen & s2_redirect & s2_predicted_fh <- foldedGhGen_dup zip s2_redirect_dup zip s2_predicted_fh_dup)
    foldedGhGen.register(s2_redirect, s2_predicted_fh, Some("s2_FGH"), 5)
  for (ghistPtrGen & s2_redirect & s2_predicted_ghist_ptr <- ghistPtrGen_dup zip s2_redirect_dup zip s2_predicted_ghist_ptr_dup)
    ghistPtrGen.register(s2_redirect, s2_predicted_ghist_ptr, Some("s2_GHPtr"), 5)
  for (lastBrNumOHGen & s2_redirect & s2_brPosOH <- lastBrNumOHGen_dup zip s2_redirect_dup zip resp.s2.lastBrPosOH.map(_.asUInt))
    lastBrNumOHGen.register(s2_redirect, s2_brPosOH, Some("s2_BrNumOH"), 5)
  for (aheadFhObGen & s2_redirect & s2_ahead_fh_ob_src <- aheadFhObGen_dup zip s2_redirect_dup zip s2_ahead_fh_ob_src_dup)
    aheadFhObGen.register(s2_redirect, s2_ahead_fh_ob_src, Some("s2_AFHOB"), 5)
  ghvBitWriteGens.zip(s2_ghv_wens).zipWithIndex.foreach {case ((b, w), i) =>
    b.register(w, s2_ghv_wdatas(i), Some(s"s2_new_bit_$i"), 5)
  }

  XSPerfAccumulate("s2_redirect_because_target_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(0))
  XSPerfAccumulate("s2_redirect_because_branch_num_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(1))
  XSPerfAccumulate("s2_redirect_because_direction_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(2))
  XSPerfAccumulate("s2_redirect_because_cfi_idx_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(3))
  // XSPerfAccumulate("s2_redirect_because_shouldShiftVec_diff", s2_fire && s2_redirect_s1_last_pred_vec(4))
  // XSPerfAccumulate("s2_redirect_because_brTaken_diff", s2_fire && s2_redirect_s1_last_pred_vec(5))
  XSPerfAccumulate("s2_redirect_because_fallThroughError", s2_fire_dup(0) && resp.s2.fallThruError(0))

  XSPerfAccumulate("s2_redirect_when_taken", s2_redirect_dup(0) && resp.s2.taken(0) && resp.s2.fullPred(0).hit)
  XSPerfAccumulate("s2_redirect_when_not_taken", s2_redirect_dup(0) && !resp.s2.taken(0) && resp.s2.fullPred(0).hit)
  XSPerfAccumulate("s2_redirect_when_not_hit", s2_redirect_dup(0) && !resp.s2.fullPred(0).hit)


  // s3
  val s3_possible_predicted_ghist_ptrs_dup = s3_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s3_predicted_ghist_ptr_dup = s3_possible_predicted_ghist_ptrs_dup.zip(resp.s3.lastBrPosOH).map{ case (ptr, oh) => Mux1H(oh, ptr)}

  val s3_possible_predicted_fhs_dup =
    for (fgh & afh & br_num_oh & fullPred <-
      s3_folded_gh_dup zip s3_ahead_fh_oldest_bits_dup zip s3_last_br_num_oh_dup zip resp.s3.fullPred)
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, if (i > 0) fullPred.br_taken else false.B)
      )
  val s3_predicted_fh_dup = resp.s3.lastBrPosOH.zip(s3_possible_predicted_fhs_dup).map{ case (oh, fh) => Mux1H(oh, fh)}

  val s3_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s3_ahead_fh_ob_src_dup.zip(s3_ghist_ptr_dup).foreach{ case (src, ptr) => src.read(ghv, ptr)}

  if (EnableGHistDiff) {
    val s3_predicted_ghist = WireInit(getHist(s3_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))

    when (resp.s3.shouldShiftVec(0)) {
      s3_predicted_ghist(0) := resp.s3.brTaken(0)
    }

    when(s3_redirect_dup(0)) {
      s0_ghist := s3_predicted_ghist.asUInt
    }
  }

  val s3_ghv_wens = (0 until HistoryLength).map(n =>
    s3_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s3.shouldShiftVec(0) && s3_redirect_dup(0))
  val s3_ghv_wdatas = (0 until HistoryLength).map(n =>
    s3_ghist_ptr_dup(0).value === CGHPtr(false.B, n.U).value && resp.s3.shouldShiftVec(0) &&
    resp.s3.fullPred(0).realBrTaken
  )

  val previous_s2_pred = RegEnable(resp.s2, 0.U.asTypeOf(resp.s2), s2_fire_dup(0))

  val s3_redirect_on_br_taken_dup = resp.s3.fullPred.zip(previous_s2_pred.fullPred).map {case (fp1, fp2) => fp1.realBrTaken =/= fp2.realBrTaken}
  val s3_redirect_on_target_dup = resp.s3.target.zip(previous_s2_pred.target).map {case (t1, t2) => t1 =/= t2}
  val s3_redirect_on_jalr_target_dup = resp.s3.fullPred.zip(previous_s2_pred.fullPred).map {case (fp1, fp2) => fp1.hitTakenOnJalr && fp1.jalrTarget =/= fp2.jalrTarget}
  val s3_redirect_on_fall_thru_error_dup = resp.s3.fallThruError
  val s3_redirect_on_ftb_multi_hit_dup = resp.s3.ftbMultiHit

  for (s3_redirect & s3_fire & s3_redirect_on_br_taken & s3_redirect_on_target & s3_redirect_on_fall_thru_error & s3_redirect_on_ftb_multi_hit <-
    s3_redirect_dup zip s3_fire_dup zip s3_redirect_on_br_taken_dup zip s3_redirect_on_target_dup zip s3_redirect_on_fall_thru_error_dup zip s3_redirect_on_ftb_multi_hit_dup) {

    s3_redirect := s3_fire && (
      s3_redirect_on_br_taken || s3_redirect_on_target || s3_redirect_on_fall_thru_error || s3_redirect_on_ftb_multi_hit
    )
  }

  XSPerfAccumulate(f"s3_redirect_on_br_taken", s3_fire_dup(0) && s3_redirect_on_br_taken_dup(0))
  XSPerfAccumulate(f"s3_redirect_on_jalr_target", s3_fire_dup(0) && s3_redirect_on_jalr_target_dup(0))
  XSPerfAccumulate(f"s3_redirect_on_others", s3_redirect_dup(0) && !(s3_redirect_on_br_taken_dup(0) || s3_redirect_on_jalr_target_dup(0)))

  for (npcGen & s3_redirect & s3_target <- npcGen_dup zip s3_redirect_dup zip resp.s3.target)
    npcGen.register(s3_redirect, s3_target, Some("s3_target"), 3)
  for (foldedGhGen & s3_redirect & s3_predicted_fh <- foldedGhGen_dup zip s3_redirect_dup zip s3_predicted_fh_dup)
    foldedGhGen.register(s3_redirect, s3_predicted_fh, Some("s3_FGH"), 3)
  for (ghistPtrGen & s3_redirect & s3_predicted_ghist_ptr <- ghistPtrGen_dup zip s3_redirect_dup zip s3_predicted_ghist_ptr_dup)
    ghistPtrGen.register(s3_redirect, s3_predicted_ghist_ptr, Some("s3_GHPtr"), 3)
  for (lastBrNumOHGen & s3_redirect & s3_brPosOH <- lastBrNumOHGen_dup zip s3_redirect_dup zip resp.s3.lastBrPosOH.map(_.asUInt))
    lastBrNumOHGen.register(s3_redirect, s3_brPosOH, Some("s3_BrNumOH"), 3)
  for (aheadFhObGen & s3_redirect & s3_ahead_fh_ob_src <- aheadFhObGen_dup zip s3_redirect_dup zip s3_ahead_fh_ob_src_dup)
    aheadFhObGen.register(s3_redirect, s3_ahead_fh_ob_src, Some("s3_AFHOB"), 3)
  ghvBitWriteGens.zip(s3_ghv_wens).zipWithIndex.foreach {case ((b, w), i) =>
    b.register(w, s3_ghv_wdatas(i), Some(s"s3_new_bit_$i"), 3)
  }

  // Send signal tell Ftq override
  val s2_ftq_idx = RegEnable(io.ftq_to_bpu.enq_ptr, s1_fire_dup(0))
  val s3_ftq_idx = RegEnable(s2_ftq_idx, s2_fire_dup(0))

  for (to_ftq_s1_valid & s1_fire & s1_flush <- io.bpu_to_ftq.resp.bits.s1.valid zip s1_fire_dup zip s1_flush_dup) {
    to_ftq_s1_valid := s1_fire && !s1_flush
  }
  io.bpu_to_ftq.resp.bits.s1.hasRedirect.foreach(_ := false.B)
  io.bpu_to_ftq.resp.bits.s1.ftqIdx := DontCare
  for (to_ftq_s2_valid & s2_fire & s2_flush <- io.bpu_to_ftq.resp.bits.s2.valid zip s2_fire_dup zip s2_flush_dup) {
    to_ftq_s2_valid := s2_fire && !s2_flush
  }
  io.bpu_to_ftq.resp.bits.s2.hasRedirect.zip(s2_redirect_dup).foreach {case (hr, r) => hr := r}
  io.bpu_to_ftq.resp.bits.s2.ftqIdx := s2_ftq_idx
  for (to_ftq_s3_valid & s3_fire & s3_flush <- io.bpu_to_ftq.resp.bits.s3.valid zip s3_fire_dup zip s3_flush_dup) {
    to_ftq_s3_valid := s3_fire && !s3_flush
  }
  io.bpu_to_ftq.resp.bits.s3.hasRedirect.zip(s3_redirect_dup).foreach {case (hr, r) => hr := r}
  io.bpu_to_ftq.resp.bits.s3.ftqIdx := s3_ftq_idx

  predictors.io.update := RegNext(dup(io.ftq_to_bpu.update))
  predictors.io.update.foreach(_.bits.ghist := RegNext(getHist(io.ftq_to_bpu.update.bits.specInfo.histPtr)))
  
  val redirect_dup = do_redirect_dup.map(_.bits)
  predictors.io.redirect := do_redirect_dup(0)

  // Redirect logic
  val shift_dup = redirect_dup.map(_.cfiUpdate.shift)
  val addIntoHist_dup = redirect_dup.map(_.cfiUpdate.addIntoHist)
  val taken_dup = redirect_dup.map(_.cfiUpdate.taken)
  val real_br_taken_mask_dup =
    for (shift & taken & addIntoHist <- shift_dup zip taken_dup zip addIntoHist_dup)
      yield shift && taken && addIntoHist

  val oldPtr_dup = redirect_dup.map(_.cfiUpdate.histPtr)
  def computeFoldedHist(hist: UInt, compLen: Int)(histLen: Int): UInt = {
    if (histLen > 0) {
      val nChunks = (histLen + compLen - 1) / compLen
      val hist_chunks = (0 until nChunks) map { i =>
        hist(min((i + 1) * compLen, histLen) - 1, i * compLen)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }

  val oldFh_dup = dup_seq(WireInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos))))
  oldFh_dup.zip(oldPtr_dup).foreach { case (oldFh, oldPtr) =>
    foldedGHistInfos.foreach { case (histLen, compLen) =>
      oldFh.getHistWithInfo((histLen, compLen)).foldedHist := computeFoldedHist(getHist(oldPtr), compLen)(histLen)
    }
  }
  val updated_ptr_dup = oldPtr_dup.zip(shift_dup).map {case (oldPtr, shift) => oldPtr - shift}
  val updated_fh_dup =
    for (oldFh & oldPtr & taken & addIntoHist & shift <- oldFh_dup zip oldPtr_dup zip taken_dup zip addIntoHist_dup zip shift_dup)
      yield VecInit((0 to numBr).map(i => oldFh.update(ghv, oldPtr, i, taken && addIntoHist)))(shift)


  val thisBrNumOH_dup = shift_dup.map(shift => UIntToOH(shift, numBr+1))
  val thisAheadFhOb_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  thisAheadFhOb_dup.zip(oldPtr_dup).foreach {case (afhob, oldPtr) => afhob.read(ghv, oldPtr)}
  val redirect_ghv_wens = (0 until HistoryLength).map(n =>
    oldPtr_dup(0).value === CGHPtr(false.B, n.U).value && shift_dup(0) && do_redirect_dup(0).valid)
  val redirect_ghv_wdatas = (0 until HistoryLength).map(n =>
    oldPtr_dup(0).value === CGHPtr(false.B, n.U).value && shift_dup(0) && real_br_taken_mask_dup(0)
  )

  if (EnableGHistDiff) {
    val updated_ghist = WireInit(getHist(updated_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))

    when (shift_dup(0) >= 1.U) {
      updated_ghist(0) := taken_dup(0) && addIntoHist_dup(0)
    }

    when(do_redirect_dup(0).valid) {
      s0_ghist := updated_ghist.asUInt
    }
  }


  // val updatedGh = oldGh.update(shift, taken && addIntoHist)
  for (npcGen & do_redirect <- npcGen_dup zip do_redirect_dup)
    npcGen.register(do_redirect.valid, do_redirect.bits.cfiUpdate.target, Some("redirect_target"), 2)
  for (foldedGhGen & do_redirect & updated_fh <- foldedGhGen_dup zip do_redirect_dup zip updated_fh_dup)
    foldedGhGen.register(do_redirect.valid, updated_fh, Some("redirect_FGHT"), 2)
  for (ghistPtrGen & do_redirect & updated_ptr <- ghistPtrGen_dup zip do_redirect_dup zip updated_ptr_dup)
    ghistPtrGen.register(do_redirect.valid, updated_ptr, Some("redirect_GHPtr"), 2)
  for (lastBrNumOHGen & do_redirect & thisBrNumOH <- lastBrNumOHGen_dup zip do_redirect_dup zip thisBrNumOH_dup)
    lastBrNumOHGen.register(do_redirect.valid, thisBrNumOH, Some("redirect_BrNumOH"), 2)
  for (aheadFhObGen & do_redirect & thisAheadFhOb <- aheadFhObGen_dup zip do_redirect_dup zip thisAheadFhOb_dup)
    aheadFhObGen.register(do_redirect.valid, thisAheadFhOb, Some("redirect_AFHOB"), 2)
  ghvBitWriteGens.zip(redirect_ghv_wens).zipWithIndex.foreach{case ((b, w), i) =>
    b.register(w, redirect_ghv_wdatas(i), Some(s"redirect_new_bit_$i"), 2)
  }
  // no need to assign s0_last_pred

  // val need_reset = RegNext(reset.asBool) && !reset.asBool

  // Reset
  // npcGen.register(need_reset, resetVector.U, Some("reset_pc"), 1)
  // foldedGhGen.register(need_reset, 0.U.asTypeOf(s0_folded_gh), Some("reset_FGH"), 1)
  // ghistPtrGen.register(need_reset, 0.U.asTypeOf(new CGHPtr), Some("reset_GHPtr"), 1)

  s0_pc_dup.zip(npcGen_dup).foreach {case (s0_pc, npcGen) => s0_pc := npcGen()}
  s0_folded_gh_dup.zip(foldedGhGen_dup).foreach {case (s0_folded_gh, foldedGhGen) => s0_folded_gh := foldedGhGen()}
  s0_ghist_ptr_dup.zip(ghistPtrGen_dup).foreach {case (s0_ghist_ptr, ghistPtrGen) => s0_ghist_ptr := ghistPtrGen()}
  s0_ahead_fh_oldest_bits_dup.zip(aheadFhObGen_dup).foreach {case (s0_ahead_fh_oldest_bits, aheadFhObGen) =>
    s0_ahead_fh_oldest_bits := aheadFhObGen()}
  s0_last_br_num_oh_dup.zip(lastBrNumOHGen_dup).foreach {case (s0_last_br_num_oh, lastBrNumOHGen) =>
    s0_last_br_num_oh := lastBrNumOHGen()}
  (ghv_write_datas zip ghvBitWriteGens).foreach{case (wd, d) => wd := d()}
  for (i <- 0 until HistoryLength) {
    ghv_wens(i) := Seq(s1_ghv_wens, s2_ghv_wens, s3_ghv_wens, redirect_ghv_wens).map(_(i)).reduce(_||_)
    when (ghv_wens(i)) {
      ghv(i) := ghv_write_datas(i)
    }
  }

  XSError(isBefore(redirect_dup(0).cfiUpdate.histPtr, s3_ghist_ptr_dup(0)) && do_redirect_dup(0).valid,
    p"s3_ghist_ptr ${s3_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_dup(0).cfiUpdate.histPtr}\n")
  XSError(isBefore(redirect_dup(0).cfiUpdate.histPtr, s2_ghist_ptr_dup(0)) && do_redirect_dup(0).valid,
    p"s2_ghist_ptr ${s2_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_dup(0).cfiUpdate.histPtr}\n")
  XSError(isBefore(redirect_dup(0).cfiUpdate.histPtr, s1_ghist_ptr_dup(0)) && do_redirect_dup(0).valid,
    p"s1_ghist_ptr ${s1_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_dup(0).cfiUpdate.histPtr}\n")

  XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
  XSDebug(io.ftq_to_bpu.update.valid, p"Update from ftq\n")
  XSDebug(io.ftq_to_bpu.redirect.valid, p"Redirect from ftq\n")

  XSDebug("[BP0]                 fire=%d                      pc=%x\n", s0_fire_dup(0), s0_pc_dup(0))
  XSDebug("[BP1] v=%d r=%d cr=%d fire=%d             flush=%d pc=%x\n",
    s1_valid_dup(0), s1_ready_dup(0), s1_components_ready_dup(0), s1_fire_dup(0), s1_flush_dup(0), s1_pc)
  XSDebug("[BP2] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s2_valid_dup(0), s2_ready_dup(0), s2_components_ready_dup(0), s2_fire_dup(0), s2_redirect_dup(0), s2_flush_dup(0), s2_pc)
  XSDebug("[BP3] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s3_valid_dup(0), s3_ready_dup(0), s3_components_ready_dup(0), s3_fire_dup(0), s3_redirect_dup(0), s3_flush_dup(0), s3_pc)
  XSDebug("[FTQ] ready=%d\n", io.bpu_to_ftq.resp.ready)
  XSDebug("resp.s1.target=%x\n", resp.s1.target(0))
  XSDebug("resp.s2.target=%x\n", resp.s2.target(0))
  // XSDebug("s0_ghist: %b\n", s0_ghist.predHist)
  // XSDebug("s1_ghist: %b\n", s1_ghist.predHist)
  // XSDebug("s2_ghist: %b\n", s2_ghist.predHist)
  // XSDebug("s2_predicted_ghist: %b\n", s2_predicted_ghist.predHist)
  XSDebug(p"s0_ghist_ptr: ${s0_ghist_ptr_dup(0)}\n")
  XSDebug(p"s1_ghist_ptr: ${s1_ghist_ptr_dup(0)}\n")
  XSDebug(p"s2_ghist_ptr: ${s2_ghist_ptr_dup(0)}\n")
  XSDebug(p"s3_ghist_ptr: ${s3_ghist_ptr_dup(0)}\n")

  XSPerfAccumulate("s2_redirect", s2_redirect_dup(0))
  XSPerfAccumulate("s3_redirect", s3_redirect_dup(0))
  XSPerfAccumulate("s1_not_valid", !s1_valid_dup(0))

  val perfEvents = predictors.asInstanceOf[Composer].getPerfEvents
  generatePerfEvent()
}
