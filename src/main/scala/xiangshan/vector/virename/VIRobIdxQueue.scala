// /***************************************************************************************
//  * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
//  *
//  * XiangShan is licensed under Mulan PSL v2.
//  * You can use this software according to the terms and conditions of the Mulan PSL v2.
//  * You may obtain a copy of Mulan PSL v2 at:
//  *          http://license.coscl.org.cn/MulanPSL2
//  *
//  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
//  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
//  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//  *
//  * See the Mulan PSL v2 for more details.
//  ***************************************************************************************/

// /*--------------------------------------------------------------------------------------
//     Author: GMX
//     Date: 2023-06-28
//     email: guanmingxing@bosc.ac.cn

// ---------------------------------------------------------------------------------------*/

// package xiangshan.vector.virename

// import chisel3._
// import chisel3.util._
// import chipsalliance.rocketchip.config.Parameters

// import xiangshan._
// import utils._
// import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, CircularShift}

// import xiangshan.vector._
// import xiangshan.backend.rob.RobPtr

// class VIRobIdxQueueDeqIO(implicit p: Parameters) extends VectorBaseBundle {
//   val hasPendingRobIdx = Input(Bool())
//   val pendingType = Input(Bool()) //0-commit, 1-walk

//   val canCommitRobIdxNum = Input(UInt(log2Up(VICommitWidth + 1).W))
//   val canRollBackRobIdxNum = Input(UInt(log2Up(VICommitWidth + 1).W))

//   val doCommit = Output(Bool())
//   val doWalk = Output(Bool())
//   val robIdx = Output(Wire(new RobPtr))
// }

// class VIRobIdxQueueEnqIO(implicit p: Parameters) extends VectorBaseBundle {
//   val doCommit = Input(Bool())
//   val doWalk = Input(Bool())
//   val canEnq = Output(Bool())
//   val robIdx = Vec(CommitWidth, Flipped(ValidIO(UInt(log2Up(RobSize).W))))
// }

// class VICommitReqEntry(implicit p: Parameters) extends VectorBaseBundle {
//   val robIdx = UInt(log2Up(RobSize).W)
//   val commitType = Bool() //0-commit, 1-rollback
// }

// // class VIRobIdxQueue(size: Int)(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
// //   val io = IO(new Bundle {
// //     val out = new VIRobIdxQueueDeqIO
// //     val in = new VIRobIdxQueueEnqIO
// //     val hasWalk = Output(Bool())
// //   })

// //   class robIdxQueuePtr extends CircularQueuePtr[robIdxQueuePtr](size)
// //   object robIdxQueuePtr {
// //     def apply(f: Boolean, v: Int): robIdxQueuePtr = {
// //       val ptr = Wire(new robIdxQueuePtr)
// //       ptr.flag    := f.B
// //       ptr.value   := v.U
// //       ptr
// //     }
// //   }

// //   val robIdxQueue_ds = Vec(size, new VICommitReqEntry)
// //   val robIdxQueue = Reg(robIdxQueue_ds)

// //   val walkNum = RegInit(UInt(log2Up(VIPhyRegsNum + 1).W), 0.U)

// //   val headPtr = RegInit(robIdxQueuePtr(false, 0))
// //   val tailPtr = RegInit(robIdxQueuePtr(false, 0))

// //   val freeEntryNum = distanceBetween(tailPtr, headPtr)

// //   //enq
// //   io.in.canEnq := (freeEntryNum >= VICommitWidth.U)
// //   val enqNum = PopCount(io.in.robIdx.map(e => e.valid))

// //   val entryEnq = (io.in.doCommit || io.in.doWalk)

// //   val headPtrNext = WireInit(headPtr)
// //   headPtrNext := headPtr + enqNum
// //   headPtr := Mux(entryEnq, headPtrNext, headPtr)

// //   walkNum := Mux(io.in.doWalk, walkNum + enqNum, walkNum)
// //   io.hasWalk := (walkNum =/= 0.U)

// //   val enqType = Mux(io.in.doCommit, false.B, true.B)
// //   for((e, i) <- io.in.robIdx.zipWithIndex) {
// //     val wPtr = headPtr + i.U
// //     when(e.valid && entryEnq) {
// //       robIdxQueue(wPtr.value).robIdx := e.bits
// //       robIdxQueue(wPtr.value).commitType  := enqType
// //     }
// //   }

// //   //deq
// //   val deqTypeEqualVec = Wire(Vec(VICommitWidth, Bool()))
// //   for((e, i) <- deqTypeEqualVec.zipWithIndex) {
// //     if(i == 0) {
// //       e := true.B
// //     } else {
// //       val typeMatch = (robIdxQueue(headPtr.value).commitType === robIdxQueue((headPtr + i.U).value).commitType)
// //       e := deqTypeEqualVec(i-1) && (freeEntryNum >= i.U) && typeMatch
// //     }
// //   }

// //   val deqTypeEqualNum = PopCount(deqTypeEqualVec)

// //   val hasPendingIdx   = io.out.hasPendingRobIdx
// //   val hasPendingType  = io.out.pendingType

// //   val deqType = robIdxQueue(tailPtr.value).commitType
// //   val deqNum  = Wire(UInt(log2Up(VICommitWidth + 1).W))
// //   deqNum  := Mux(deqType === false.B, Mux(io.out.canRollBackRobIdxNum > deqTypeEqualNum, deqTypeEqualNum, io.out.canRollBackRobIdxNum), 
// //                                       Mux(io.out.canCommitRobIdxNum > deqTypeEqualNum, deqTypeEqualNum, io.out.canCommitRobIdxNum))
// //   val deqNumReal = Mux(io.out.hasPendingRobIdx && (io.out.pendingType =/= robIdxQueue(tailPtr.value).commitType), 0.U, deqNum)
// //   for(i <- 0 until VICommitWidth) {
// //     io.out.robIdx(i).valid := (i.U < deqNum)
// //     io.out.robIdx(i).bits := robIdxQueue((tailPtr + i.U).value).robIdx
// //   }
// //   io.out.doCommit := Mux((deqNum =/= 0.U) && (deqType === false.B), true.B, false.B)
// //   io.out.doWalk   := Mux((deqNum =/= 0.U) && (deqType === true.B), true.B, false.B)

// //   val tailPtrNext = RegNext(tailPtr + deqNum)
// //   tailPtr := Mux(deqNum =/= 0.U, tailPtrNext, tailPtr)
// // }
