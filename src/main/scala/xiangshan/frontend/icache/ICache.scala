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

package  xiangshan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, _}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import xs.utils.mbist.MBISTPipeline
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._
import xs.utils._
import xs.utils.sram.SRAMTemplate
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.backend.execute.fu.fence.{FenceIBundle, SfenceBundle}
import xs.utils.perf.HasPerfLogging
import xs.utils.sram._

case class ICacheParameters(
    nSets: Int = 256,//128,//256,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),

    PortNumber: Int = 2,
    nFetchMshr: Int = 4,
    nPrefetchMshr: Int = 10,
    nWayLookupSize: Int = 32,
    DataCodeUnit: Int = 64,
    ICacheDataBanks: Int = 8,
    ICacheDataSRAMWidth: Int = 66,

    partWayNum: Int = 4,

    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq()
  val echoFields: Seq[BundleFieldBase] = Seq()
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst with HasIFUConst{
  val cacheParams = icacheParameters

  def ICacheSets            = cacheParams.nSets
  def ICacheWays            = cacheParams.nWays
  def PortNumber            = cacheParams.PortNumber
  def nFetchMshr            = cacheParams.nFetchMshr
  def nPrefetchMshr         = cacheParams.nPrefetchMshr
  def nWayLookupSize        = cacheParams.nWayLookupSize
  def DataCodeUnit          = cacheParams.DataCodeUnit
  def ICacheDataBanks       = cacheParams.ICacheDataBanks
  def ICacheDataSRAMWidth   = cacheParams.ICacheDataSRAMWidth
  def partWayNum            = cacheParams.partWayNum

  def ICacheDataBits        = blockBits / ICacheDataBanks                       // 64
  def ICacheCodeBits        = math.ceil(ICacheDataBits / DataCodeUnit).toInt    // 1
  def ICacheEntryBits       = ICacheDataBits + ICacheCodeBits                   // 65
  def ICacheBankVisitNum    = PredictWidth * 2 * 8 / ICacheDataBits + 1         // Since 6 fetch width and RVC, we need 24 + 2 Btyes in 4 Bank.
  def highestIdxBit         = log2Ceil(nSets) - 1                               // 7

  require((ICacheDataBanks >= 2) && isPow2(ICacheDataBanks))
  require(ICacheDataSRAMWidth >= ICacheEntryBits)
  require(isPow2(ICacheSets), s"nSets($ICacheSets) must be pow2")
  require(isPow2(ICacheWays), s"nWays($ICacheWays) must be pow2")

  def getBits(num: Int) = log2Ceil(num).W

  def generatePipeControl(lastFire: Bool, thisFire: Bool, thisFlush: Bool, lastFlush: Bool): Bool = {
    val valid  = RegInit(false.B)
    when(thisFlush)                    {valid  := false.B}
      .elsewhen(lastFire && !lastFlush)  {valid  := true.B}
      .elsewhen(thisFire)                 {valid  := false.B}
    valid
  }

  def ResultHoldBypass[T<:Data](data: T, valid: Bool): T = {
    Mux(valid, data, RegEnable(data, valid))
  }

  def ResultHoldBypass[T <: Data](data: T, init: T, valid: Bool): T = {
    Mux(valid, data, RegEnable(data, init, valid))
  }

  def encode(data: UInt): UInt = {
    val datas = data.asTypeOf(Vec(ICacheCodeBits, UInt((ICacheDataBits / ICacheCodeBits).W)))
    val codes = VecInit(datas.map(cacheParams.dataCode.encode(_) >> (ICacheDataBits / ICacheCodeBits)))
    codes.asTypeOf(UInt(ICacheCodeBits.W))
  }
  
  // Return banks read mask, For Fetchwidth = 6, blkOffset=110000,
  // the return mask is
  // 00000111
  // 10000000
  def getBankSel(blkOffset: UInt, valid: Bool = true.B): Vec[UInt] = {
    val fetchByte = PredictWidth * 2
    val bankIdxLow  = Cat(0.U(1.W), blkOffset) >> log2Ceil(blockBytes/ICacheDataBanks)
    val bankIdxHigh = (Cat(0.U(1.W), blkOffset) + fetchByte.U) >> log2Ceil(blockBytes/ICacheDataBanks)
    val bankSel = VecInit((0 until ICacheDataBanks * 2).map(i => (i.U >= bankIdxLow) && (i.U <= bankIdxHigh)))
    assert(!valid || PopCount(bankSel) === ICacheBankVisitNum.U, "The number of bank visits must be %d, but bankSel=0x%x", ICacheBankVisitNum.U, bankSel.asUInt)
    bankSel.asTypeOf(UInt((ICacheDataBanks * 2).W)).asTypeOf(Vec(2, UInt(ICacheDataBanks.W)))
  }

  // Indicate which line read from
  // For example above, return 11111000
  def getLineSel(blkOffset: UInt)(implicit p: Parameters): Vec[Bool] = {
    val bankIdxLow  = blkOffset >> log2Ceil(blockBytes/ICacheDataBanks)
    val lineSel = VecInit((0 until ICacheDataBanks).map(i => i.U < bankIdxLow))
    lineSel
  }

  def getBlkAddr(addr: UInt) = addr >> blockOffBits
  def getPhyTagFromBlk(addr: UInt) = addr >> (pgUntagBits - blockOffBits)
  def getIdxFromBlk(addr: UInt) = addr(idxBits - 1, 0)
  def get_paddr_from_ptag(vaddr: UInt, ptag: UInt) = Cat(ptag, vaddr(pgUntagBits - 1, 0))

}

abstract class ICacheBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters

abstract class ICacheArray(implicit p: Parameters) extends XSModule
  with HasICacheParameters

class ICacheMetadata(implicit p: Parameters) extends ICacheBundle {
  val tag = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits)(implicit p: Parameters) = {
    val meta = Wire(new ICacheMetadata)
    meta.tag := tag
    meta.tag
  }
}

class ICacheMetaArray(parentName:String = "Unknown")(implicit p: Parameters) extends ICacheArray with HasPerfLogging {
  def onReset = ICacheMetadata(0.U)
  val metaBits = onReset.getWidth
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io = IO (new Bundle {
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
    val fencei   = Flipped(new FenceIBundle)
  })

  io.read.ready := !io.write.valid

  val port_0_read_0 = io.read.valid && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1 = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0 = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(port_0_read_0, 0.U.asTypeOf(port_0_read_0) ,io.read.fire)
  val port_0_read_1_reg = RegEnable(port_0_read_1, 0.U.asTypeOf(port_0_read_1) ,io.read.fire)
  val port_1_read_1_reg = RegEnable(port_1_read_1, 0.U.asTypeOf(port_1_read_1) ,io.read.fire)
  val port_1_read_0_reg = RegEnable(port_1_read_0, 0.U.asTypeOf(port_1_read_0) ,io.read.fire)

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))

  val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  val write_bank_1 = io.write.valid &&  io.write.bits.bankIdx


  // Parity Encode
  val write_meta_bits = Wire(UInt(metaEntryBits.W))

  // Tag Array
  val tagArrays = (0 until PortNumber) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      UInt(metaEntryBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true,
      hasMbist = coreParams.hasMbist,
      hasShareBus = coreParams.hasShareBus,
      parentName = parentName + s"tagArrays${bank}_"
    ))

    if(bank == 0) {
      tagArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    tagArray
  }

  val mbistTagPipeline = Option.when(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(1, s"${parentName}_mbistTagPipe")
  }

  // Valid Array
  val read_set_idx_next = RegEnable(io.read.bits.vSetIdx, 0.U.asTypeOf(io.read.bits.vSetIdx), io.read.fire)
  val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))

  io.readResp.entryValid.zip(read_set_idx_next).foreach{ case(valid, set) =>
    for(way <- 0 until nWays){
      valid(way) := valid_array(way)(set)
    }
  }

  io.read.ready := !io.write.valid && !io.fencei.start && tagArrays.map(_.io.r.req.ready).reduce(_&&_)

  // Parity Decode
  val read_fire_delay1 = RegNext(io.read.fire, init = false.B)
  val read_fire_delay2 = RegNext(read_fire_delay1, init = false.B)
  val read_metas = Wire(Vec(2,Vec(nWays,new ICacheMetadata())))
  for((tagArray,i) <- tagArrays.zipWithIndex){
    val read_meta_bits = tagArray.io.r.resp.asTypeOf(Vec(nWays,UInt(metaEntryBits.W)))
    val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits)}
    val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
    val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
    read_metas(i) := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
    (0 until nWays).foreach{ w => io.readResp.errors(i)(w) := RegEnable(read_meta_wrong(w), 0.U.asTypeOf(read_meta_wrong(w)), read_fire_delay1) && read_fire_delay2}
  }

  // Parity Encode
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = io.write.bits.phyTag).asUInt)

  val way_num = OHToUInt(io.write.bits.waymask)
  when (io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }

  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metaData <> DontCare
  when(port_0_read_0_reg){
    io.readResp.metaData(0) := read_metas(0)
  }.elsewhen(port_0_read_1_reg){
    io.readResp.metaData(0) := read_metas(1)
  }

  when(port_1_read_0_reg){
    io.readResp.metaData(1) := read_metas(0)
  }.elsewhen(port_1_read_1_reg){
    io.readResp.metaData(1) := read_metas(1)
  }

  io.write.ready := true.B

  // Fence logic
  when(io.fencei.start) {
    valid_array.foreach(v => v :=0.U)
    io.fencei.done := true.B
  }.otherwise {
     io.fencei.done := false.B
  }
}

class ICacheDataArray(parentName:String = "Unknown")(implicit p: Parameters) extends ICacheArray {

  class ICacheDataEntry(implicit p: Parameters) extends ICacheBundle {
    val data = UInt(ICacheDataBits.W)
    val code = UInt(ICacheCodeBits.W)
  }

  object ICacheDataEntry {
    def apply(data: UInt)(implicit p: Parameters) = {
      require(data.getWidth == ICacheDataBits)
      val entry = Wire(new ICacheDataEntry)
      entry.data := data
      entry.code := encode(data)
      entry
    }
  }

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(Vec(partWayNum, DecoupledIO(new ICacheReadBundle)))
    val readResp = Output(new ICacheDataRespBundle)
  }}

  /**
    ******************************************************************************
    * data array
    ******************************************************************************
    */

  val writeDatas   = io.write.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt(ICacheDataBits.W)))
  val writeEntries = writeDatas.map(ICacheDataEntry(_).asUInt)

  val bankSel = getBankSel(io.read(0).bits.blkOffset, io.read(0).valid)
  val lineSel = getLineSel(io.read(0).bits.blkOffset)
  val waymasks = io.read(0).bits.wayMask

  // this mask mean the final read data location
  val masks = Wire(Vec(nWays, Vec(ICacheDataBanks, Bool())))
  for (way <- 0 until nWays){
    for(bank <- 0 until ICacheDataBanks){
      masks(way)(bank) := Mux(lineSel(bank), waymasks(1)(way) && bankSel(1)(bank).asBool,
                                             waymasks(0)(way) && bankSel(0)(bank).asBool)
    }
  }

  val dataArrays = (0 until nWays).map{ way =>
    (0 until ICacheDataBanks).map { bank =>
      val sramBank = Module(new SRAMTemplateWithFixedWidth(
        UInt(ICacheEntryBits.W),
        set=nSets,
        width=ICacheDataSRAMWidth,
        shouldReset = true,
        holdRead = true,
        singlePort = true
      ))

      // read
      sramBank.io.r.req.valid := io.read(bank % 4).valid && masks(way)(bank)
      sramBank.io.r.req.bits.apply(setIdx=Mux(lineSel(bank),
                                              io.read(bank % 4).bits.vSetIdx(1),
                                              io.read(bank % 4).bits.vSetIdx(0)))
      // write
      sramBank.io.w.req.valid := io.write.valid && io.write.bits.waymask(way).asBool
      sramBank.io.w.req.bits.apply(
        data    = writeEntries(bank),
        setIdx  = io.write.bits.virIdx,
        // waymask is invalid when way of SRAMTemplate <= 1
        waymask = 0.U
      )
      sramBank
    }
  }

  /**
    ******************************************************************************
    * read logic
    ******************************************************************************
    */

  val masksReg          = RegEnable(masks, 0.U.asTypeOf(masks), io.read(0).valid)
  val readDataWithCode  = (0 until ICacheDataBanks).map(bank =>
                            Mux1H(VecInit(masksReg.map(_(bank))).asTypeOf(UInt(nWays.W)),
                                  dataArrays.map(_(bank).io.r.resp.asUInt)))

  val readEntries       = readDataWithCode.map(_.asTypeOf(new ICacheDataEntry()))
  val readDatas         = VecInit(readEntries.map(_.data))
  val readCodes         = VecInit(readEntries.map(_.code))
  

  /**
    ******************************************************************************
    * IO
    ******************************************************************************
    */
  io.readResp.datas   := readDatas
  io.readResp.codes   := readCodes
  io.write.ready      := true.B
  io.read.foreach( _.ready := !io.write.valid)

}

class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val touch   = Vec(PortNumber, Flipped(ValidIO(new ReplacerTouch)))
    val victim  = Flipped(new ReplacerVictim)
  })

  val replacers = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/PortNumber))

  // touch
  val touch_sets = Seq.fill(PortNumber)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(PortNumber)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))))
  (0 until PortNumber).foreach {i =>
    touch_sets(i)(0)        := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).bits.vSetIdx(highestIdxBit, 1), io.touch(0).bits.vSetIdx(highestIdxBit, 1))
    touch_ways(i)(0).bits   := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).bits.way, io.touch(0).bits.way)
    touch_ways(i)(0).valid  := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).valid, io.touch(0).valid)
  }

  // victim
  io.victim.way := Mux(io.victim.vSetIdx.bits(0),
                       replacers(1).way(io.victim.vSetIdx.bits(highestIdxBit, 1)),
                       replacers(0).way(io.victim.vSetIdx.bits(highestIdxBit, 1)))

  // touch the victim in next cycle
  val victim_vSetIdx_reg = RegEnable(io.victim.vSetIdx.bits, 0.U.asTypeOf(io.victim.vSetIdx.bits), io.victim.vSetIdx.valid)
  val victim_way_reg     = RegEnable(io.victim.way,          0.U.asTypeOf(io.victim.way),          io.victim.vSetIdx.valid)
  (0 until PortNumber).foreach {i =>
    touch_sets(i)(1)        := victim_vSetIdx_reg(highestIdxBit, 1)
    touch_ways(i)(1).bits   := victim_way_reg
    touch_ways(i)(1).valid  := RegNext(io.victim.vSetIdx.valid) && (victim_vSetIdx_reg(0) === i.U)
  }

  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
}


class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val hartId = Input(UInt(8.W))
  val fencei = Flipped(new FenceIBundle)
  val prefetch    = Flipped(new FtqToPrefetchIO)
  val stop        = Input(Bool())
  val fetch       = new ICacheMainPipeBundle
  val toIFU       = Output(Bool())
  val pmp         = Vec(2 * PortNumber, new ICachePMPBundle)
  val itlb        = Vec(PortNumber, new TlbRequestIO)
  val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())

  val flush       = Input(Bool())
}

class ICache(val parentName:String = "Unknown")(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nFetchMshr + cacheParams.nPrefetchMshr),
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new ICacheImp(this)
}

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters with HasPerfEvents {
  val io = IO(new ICacheIO)

  println("ICache:")
  println("  TagECC: "                + cacheParams.tagECC)
  println("  DataECC: "               + cacheParams.dataECC)
  println("  ICacheSets: "            + cacheParams.nSets)
  println("  ICacheWays: "            + cacheParams.nWays)
  println("  PortNumber: "            + cacheParams.PortNumber)
  println("  nFetchMshr: "            + cacheParams.nFetchMshr)
  println("  nPrefetchMshr: "         + cacheParams.nPrefetchMshr)
  println("  nWayLookupSize: "        + cacheParams.nWayLookupSize)
  println("  DataCodeUnit: "          + cacheParams.DataCodeUnit)
  println("  ICacheDataBanks: "       + cacheParams.ICacheDataBanks)
  println("  ICacheDataSRAMWidth: "   + cacheParams.ICacheDataSRAMWidth)

  val (bus, edge) = outer.clientNode.out.head

  val metaArray      = Module(new ICacheMetaArray(parentName = s"${outer.parentName}metaArray_"))
  val dataArray      = Module(new ICacheDataArray(parentName = s"${outer.parentName}dataArray_"))
  val mainPipe       = Module(new ICacheMainPipe)
  val missUnit       = Module(new ICacheMissUnit(edge))
  val replacer       = Module(new ICacheReplacer)
  val prefetcher     = Module(new IPrefetchPipe)
  val wayLookup      = Module(new WayLookup)

  val mbistTagPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(2, s"${outer.parentName}_mbistPipe")
  } else {
    None
  }

  dataArray.io.write    <> missUnit.io.data_write
  dataArray.io.read     <> mainPipe.io.dataArray.toIData
  dataArray.io.readResp <> mainPipe.io.dataArray.fromIData

  metaArray.io.fencei   <> io.fencei
  metaArray.io.write    <> missUnit.io.meta_write
  metaArray.io.read     <> prefetcher.io.metaRead.toIMeta
  metaArray.io.readResp <> prefetcher.io.metaRead.fromIMeta

  prefetcher.io.flush             := io.flush
  prefetcher.io.csr_pf_enable     := io.csr_pf_enable
  prefetcher.io.ftqReq            <> io.prefetch
  prefetcher.io.MSHRResp          := missUnit.io.fetch_resp

  missUnit.io.hartId            := io.hartId
  missUnit.io.fencei            := io.fencei.start
  missUnit.io.flush             := io.flush
  missUnit.io.fetch_req         <> mainPipe.io.mshr.req
  missUnit.io.prefetch_req      <> prefetcher.io.MSHRReq
  missUnit.io.mem_grant.valid   := false.B
  missUnit.io.mem_grant.bits    := DontCare
  missUnit.io.mem_grant         <> bus.d

  mainPipe.io.flush             := io.flush
  mainPipe.io.respStall         := io.stop
  mainPipe.io.csr_parity_enable := io.csr_parity_enable
  mainPipe.io.hartId            := io.hartId
  mainPipe.io.mshr.resp         := missUnit.io.fetch_resp
  mainPipe.io.fetch.req         <> io.fetch.req
  mainPipe.io.wayLookupRead     <> wayLookup.io.read

  wayLookup.io.flush            := io.flush
  wayLookup.io.write            <> prefetcher.io.wayLookupWrite
  wayLookup.io.update           := missUnit.io.fetch_resp

  replacer.io.touch   <> mainPipe.io.touch
  replacer.io.victim  <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)

  //notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready
  io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp     <>    mainPipe.io.fetch.resp

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire

  private val errors = mainPipe.io.errors
  val errors_valid = errors.map(e => e.valid).reduce(_ | _)
  io.error := RegEnable(Mux1H(errors.map(e => e.valid -> e)), 0.U.asTypeOf(errors(0)), errors_valid)

  val perfEvents = Seq(
    ("icache_miss_cnt  ", bus.a.fire),
    ("icache_miss_penalty", BoolStopWatch(start = bus.a.fire, stop = bus.d.fire, startHighPriority = true)),
  )
  generatePerfEvent()
}

// Automatically partition the SRAM based on the width of the data and the desired width.
// final SRAM width = width * way
class SRAMTemplateWithFixedWidth[T <: Data]
(
  gen: T, set: Int, width: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  hasMbist: Boolean = false, hasShareBus: Boolean = false,
  parentName: String = s"Unknown"
)(implicit p: Parameters) extends XSModule {

  val dataBits  = gen.getWidth
  val bankNum = math.ceil(dataBits.toDouble / width.toDouble).toInt
  val totalBits = bankNum * width

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType   = UInt(width.W)
  val writeDatas = (0 until bankNum).map(bank =>
    VecInit((0 until way).map(i =>
      io.w.req.bits.data(i).asTypeOf(UInt(totalBits.W)).asTypeOf(Vec(bankNum, wordType))(bank)
    ))
  )

  val srams = (0 until bankNum) map { bank =>
    val sramBank = Module(new SRAMTemplate(
      wordType,
      set=set,
      way=way,
      shouldReset = shouldReset,
      holdRead = holdRead,
      singlePort = singlePort,
      bypassWrite = bypassWrite,
      hasMbist = coreParams.hasMbist,
      hasShareBus = coreParams.hasShareBus,
      parentName = parentName + s"bank${bank}_"
    ))
    // read req
    sramBank.io.r.req.valid       := io.r.req.valid
    sramBank.io.r.req.bits.setIdx := io.r.req.bits.setIdx

    // write req
    sramBank.io.w.req.valid       := io.w.req.valid
    sramBank.io.w.req.bits.setIdx := io.w.req.bits.setIdx
    sramBank.io.w.req.bits.data   := writeDatas(bank)
    sramBank.io.w.req.bits.waymask.foreach (_ := io.w.req.bits.waymask.get)

    sramBank
  }

  val mbistTagPipeline = Option.when(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(1, s"${parentName}_mbistDataPipe")
  }

  io.r.req.ready := !io.w.req.valid
  (0 until way).foreach{i =>
    io.r.resp.data(i) := VecInit((0 until bankNum).map(bank =>
                           srams(bank).io.r.resp.data(i)
                         )).asTypeOf(UInt(totalBits.W))(dataBits-1, 0).asTypeOf(gen.cloneType)
  }

  io.r.req.ready := srams.head.io.r.req.ready
  io.w.req.ready := srams.head.io.w.req.ready
}