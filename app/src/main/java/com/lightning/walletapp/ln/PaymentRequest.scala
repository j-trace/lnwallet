package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import fr.acinq.bitcoin.Bech32._
import fr.acinq.bitcoin.Crypto._
import fr.acinq.bitcoin.Protocol._
import fr.acinq.eclair.crypto.BitStream._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.PaymentRequest._
import com.lightning.walletapp.ln.RoutingInfoTag._
import com.lightning.walletapp.ln.crypto.MultiStreamUtils._
import com.lightning.walletapp.ln.wire.Hop
import fr.acinq.eclair.crypto.BitStream
import java.nio.ByteOrder.BIG_ENDIAN
import scodec.bits.ByteVector
import java.math.BigInteger
import scala.util.Try


sealed trait Tag {
  def toInt5s: Bytes
  def encode(ints: Bytes, v: Char): Bytes =
    Array(Bech32 map v, (ints.length / 32).toByte,
      (ints.length % 32).toByte) ++ ints
}

case class PaymentHashTag(hash: ByteVector) extends Tag {
  def toInt5s = encode(Bech32 eight2five hash.toArray, 'p')
}

case class DescriptionTag(description: String) extends Tag {
  def toInt5s = encode(Bech32 eight2five description.getBytes, 'd')
}

object LNUrl {
  def fromBech32(bech32url: String) = {
    val _ \ data = Bech32.decode(bech32url)
    val request = Bech32.five2eight(data)
    LNUrl(Tools bin2readable request)
  }
}

case class LNUrl(request: String) {
  val uri = android.net.Uri parse request
  require(uri.toString contains "https://", "First level uri is not an HTTPS endpoint")
  lazy val isLogin: Boolean = Try(uri getQueryParameter "tag" contains "login") getOrElse false
  lazy val k1: Try[Bytes] = Try(uri.getQueryParameter("k1").getBytes)
}

case class FallbackAddressTag(version: Byte, hash: ByteVector) extends Tag {
  def toInt5s = encode(version +: Bech32.eight2five(hash.toArray), 'f')
}

object FallbackAddressTag { me =>
  def apply(address: String): FallbackAddressTag = {
    val base58Try = Try(me fromBase58Address address)
    val bech32Try = Try(me fromBech32Address address)
    bech32Try.orElse(base58Try).get
  }

  def fromBase58Address(address: String) = Base58Check decode address match {
    case Base58.Prefix.PubkeyAddressTestnet \ hash => FallbackAddressTag(17, hash)
    case Base58.Prefix.ScriptAddressTestnet \ hash => FallbackAddressTag(18, hash)
    case Base58.Prefix.PubkeyAddress \ hash => FallbackAddressTag(17, hash)
    case Base58.Prefix.ScriptAddress \ hash => FallbackAddressTag(18, hash)
  }

  def fromBech32Address(address: String): FallbackAddressTag = {
    val (_, version, hash) = Bech32 decodeWitnessAddressMainChain address
    FallbackAddressTag(version, hash)
  }
}

case class RoutingInfoTag(route: PaymentRoute) extends Tag {
  def toInt5s = encode(Bech32.eight2five(route.flatMap(pack).toArray), 'r')
  def pack(hop: Hop) = aconcat(hop.nodeId.toBin.toArray, writeUInt64Array(hop.shortChannelId, BIG_ENDIAN),
    writeUInt32Array(hop.feeBaseMsat, BIG_ENDIAN), writeUInt32Array(hop.feeProportionalMillionths, BIG_ENDIAN),
    writeUInt16Array(hop.cltvExpiryDelta, BIG_ENDIAN))
}

object RoutingInfoTag {
  def parse(data: Bytes) = {
    val pubkey = ByteVector apply data.slice(0, 33)
    val shortChanId = uint64(data.slice(33, 33 + 8), BIG_ENDIAN)
    val feeBaseMsat = uint32(data.slice(33 + 8, 33 + 8 + 4), BIG_ENDIAN)
    val cltvExpiryDelta = uint16(data.slice(33 + 8 + 4 + 4, chunkLength), BIG_ENDIAN)
    val feeProportionalMillionths = uint32(data.slice(33 + 8 + 4, 33 + 8 + 4 + 4), BIG_ENDIAN)
    Hop(PublicKey(pubkey), shortChanId, cltvExpiryDelta, 0L, feeBaseMsat, feeProportionalMillionths)
  }

  type PaymentRoute = Vector[Hop]
  type PaymentRouteVec = Vector[PaymentRoute]

  val chunkLength = 33 + 8 + 4 + 4 + 2
  def parseAll(data: Bytes): PaymentRoute =
    data.grouped(chunkLength).map(parse).toVector
}

case class ExpiryTag(seconds: Long) extends Tag {
  def toInt5s = Bech32.map('x') +: (writeSize(ints.length) ++ ints)
  lazy val ints = writeUnsignedLong(seconds)
}

case class MinFinalCltvExpiryTag(expiryDelta: Long) extends Tag {
  def toInt5s = Bech32.map('c') +: (writeSize(ints.length) ++ ints)
  lazy val ints = writeUnsignedLong(expiryDelta)
}

case class UnknownTag(tag: Int5, int5s: Bytes) extends Tag {
  def toInt5s = tag +: (writeSize(int5s.length) ++ int5s)
}

case class PaymentRequest(prefix: String, amount: Option[MilliSatoshi], timestamp: Long,
                          nodeId: PublicKey, tags: Vector[Tag], signature: ByteVector) {

  lazy val msatOrMin = amount getOrElse LNParams.minHtlcValue
  lazy val adjustedMinFinalCltvExpiry = minFinalCltvExpiry.getOrElse(0L) + 10L
  lazy val description = tags.collectFirst { case DescriptionTag(info) => info }.getOrElse(new String)
  lazy val minFinalCltvExpiry = tags.collectFirst { case m: MinFinalCltvExpiryTag => m.expiryDelta }
  lazy val paymentHash = tags.collectFirst { case payHash: PaymentHashTag => payHash.hash }.get
  lazy val routingInfo = tags.collect { case r: RoutingInfoTag => r }

  lazy val fallbackAddress = tags.collectFirst {
    case FallbackAddressTag(17, hash) if prefix == "lnbc" => Base58Check.encode(Base58.Prefix.PubkeyAddress, hash)
    case FallbackAddressTag(18, hash) if prefix == "lnbc" => Base58Check.encode(Base58.Prefix.ScriptAddress, hash)
    case FallbackAddressTag(17, hash) if prefix == "lntb" || prefix == "lnbcrt" => Base58Check.encode(Base58.Prefix.PubkeyAddressTestnet, hash)
    case FallbackAddressTag(18, hash) if prefix == "lntb" || prefix == "lnbcrt" => Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, hash)
    case FallbackAddressTag(version, hash) if prefix == "lntb" || prefix == "lnbcrt" => Bech32.encodeWitnessAddress("tb", version, hash)
    case FallbackAddressTag(version, hash) if prefix == "lnbc" => Bech32.encodeWitnessAddress("bc", version, hash)
  }

  def isFresh: Boolean = {
    val expiry = tags.collectFirst { case ex: ExpiryTag => ex.seconds }
    timestamp + expiry.getOrElse(3600L) > System.currentTimeMillis / 1000L
  }

  def stream: BitStream = {
    val int5s = Timestamp.encode(timestamp) ++ tags.flatMap(_.toInt5s)
    val stream1 = int5s.foldLeft(BitStream.empty)(PaymentRequest.write5)
    stream1
  }

  def hash: ByteVector = {
    val base = prefix + Amount.encode(amount)
    val ready = base.getBytes("UTF-8") ++ stream.bytes
    Crypto sha256 ByteVector.view(ready)
  }

  def sign(priv: PrivateKey) = {
    val (r, s) = Crypto.sign(hash, priv)
    val (pub1, _) = Crypto.recoverPublicKey(r -> s, hash)
    val recid = if (nodeId == pub1) 0.toByte else 1.toByte
    val signature1 = Signature.encode(r, s, recid)
    copy(signature = signature1)
  }
}

object PaymentRequest {
  type AmountOption = Option[MilliSatoshi]
  val expiryTag = ExpiryTag(3600 * 24 + 1)

  val prefixes =
    Map(Block.RegtestGenesisBlock.hash -> "lnbcrt",
      Block.TestnetGenesisBlock.hash -> "lntb",
      Block.LivenetGenesisBlock.hash -> "lnbc")

  def apply(chain: ByteVector, amount: Option[MilliSatoshi], paymentHash: ByteVector,
            privKey: PrivateKey, description: String, fallbackAddress: Option[String],
            routes: PaymentRouteVec): PaymentRequest = {

    val baseTags = Vector(DescriptionTag(description), MinFinalCltvExpiryTag(72), PaymentHashTag(paymentHash), expiryTag)
    val completeTags = routes.map(RoutingInfoTag.apply) ++ fallbackAddress.map(FallbackAddressTag.apply).toVector ++ baseTags
    PaymentRequest(prefixes(chain), amount, System.currentTimeMillis / 1000L, privKey.publicKey, completeTags, ByteVector.empty) sign privKey
  }

  object Amount {
    // Shortest representation possible
    def unit(sum: MilliSatoshi): Char = sum match {
      case MilliSatoshi(pico) if pico * 10 % 1000 > 0 => 'p'
      case MilliSatoshi(pico) if pico * 10 % 1000000 > 0 => 'n'
      case MilliSatoshi(pico) if pico * 10 % 1000000000 > 0 => 'u'
      case _ => 'm'
    }

    def decode(input: String): AmountOption = input.lastOption match {
      case Some('p') => Some(MilliSatoshi apply input.dropRight(1).toLong / 10L)
      case Some('n') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100L)
      case Some('u') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100000L)
      case Some('m') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100000000L)
      case _ if input.nonEmpty => Some(MilliSatoshi apply input.toLong * 100000000000L)
      case _ => None
    }

    def encode(amt: AmountOption): String = amt match {
      case Some(sum) if unit(sum) == 'p' => s"${sum.amount * 10}p"
      case Some(sum) if unit(sum) == 'n' => s"${sum.amount / 100}n"
      case Some(sum) if unit(sum) == 'u' => s"${sum.amount / 100000}u"
      case Some(sum) if unit(sum) == 'm' => s"${sum.amount / 100000000}m"
      case _ => ""
    }
  }

  object Timestamp {
    def decode(data: Bytes): Long = data.take(7).foldLeft(0L) { case (a, b) => a * 32 + b }
    def encode(timestamp: Long, acc: Bytes = Array.emptyByteArray): Bytes = if (acc.length == 7) acc
    else encode(timestamp / 32, (timestamp % 32).toByte +: acc)
  }

  object Signature {
    def decode(signature: ByteVector) = {
      require(signature.length == 65, "Invalid signature length")
      val s = new BigInteger(1, signature.slice(32, 64).toArray)
      val r = new BigInteger(1, signature.take(32).toArray)
      val recid = signature.last
      (r, s, recid)
    }

    def encode(r: BigInteger, s: BigInteger, recid: Byte) = {
      val rEncoded = Crypto fixSize ByteVector.view(r.toByteArray dropWhile 0.==)
      val sEncoded = Crypto fixSize ByteVector.view(s.toByteArray dropWhile 0.==)
      rEncoded ++ sEncoded :+ recid
    }
  }

  object Tag {
    def parse(input: Bytes): Tag = {
      val len = input(1) * 32 + input(2)

      input.head match {
        case pTag if pTag == Bech32.map('p') =>
          val hash = Bech32 five2eight input.slice(3, 52 + 3)
          PaymentHashTag(ByteVector apply hash)

        case dTag if dTag == Bech32.map('d') =>
          val description = Bech32 five2eight input.slice(3, len + 3)
          DescriptionTag(Tools bin2readable description)

        case fTag if fTag == Bech32.map('f') =>
          val fallbackAddress = input.slice(4, len + 4 - 1)
          if (input(3) < 0 || input(3) > 18) UnknownTag(input.head, fallbackAddress) else {
            val fallbackAddressHash = ByteVector.view(Bech32 five2eight fallbackAddress)
            FallbackAddressTag(input(3), fallbackAddressHash)
          }

        case rTag if rTag == Bech32.map('r') =>
          val data = Bech32 five2eight input.slice(3, len + 3)
          val path = RoutingInfoTag parseAll data
          RoutingInfoTag(path)

        case xTag if xTag == Bech32.map('x') =>
          val ints: Bytes = input.slice(3, len + 3)
          val expiry = readUnsignedLong(len, ints)
          ExpiryTag(expiry)

        case cTag if cTag == Bech32.map('c') =>
          val ints: Bytes = input.slice(3, len + 3)
          val expiry = readUnsignedLong(len, ints)
          MinFinalCltvExpiryTag(expiry)

        case _ =>
          val unknown = input.slice(3, len + 3)
          UnknownTag(input.head, unknown)
      }
    }
  }

  def toBits(value: Int5): Seq[Bit] =
    Seq(elems = (value & 16) != 0, (value & 8) != 0,
      (value & 4) != 0, (value & 2) != 0, (value & 1) != 0)

  def write5(stream: BitStream, value: Int5): BitStream =
    stream writeBits toBits(value)

  def read5(stream: BitStream) = {
    val (stream1, bits) = stream readBits 5

    val b0 = if (bits.head) 1 << 4 else 0
    val b1 = if (bits apply 1) 1 << 3 else 0
    val b2 = if (bits apply 2) 1 << 2 else 0
    val b3 = if (bits apply 3) 1 << 1 else 0
    val b4 = if (bits apply 4) 1 << 0 else 0
    val value = b0 + b1 + b2 + b3 + b4
    (stream1, (value & 0xff).toByte)
  }

  def toInt5s(stream: BitStream, acc: Bytes = Array.emptyByteArray): Bytes =
    if (stream.bitCount == 0) acc else {
      val stream1 \ value = read5(stream)
      toInt5s(stream1, acc :+ value)
    }

  def writeSize(size: Long): Bytes = {
    val outputData = writeUnsignedLong(size)
    require(outputData.length <= 2)

    outputData.length match {
      case 0 => Array(0.toByte, 0.toByte)
      case 1 => 0.toByte +: outputData
      case _ => outputData
    }
  }

  def writeUnsignedLong(value: Long, acc: Bytes = Array.emptyByteArray): Bytes =
    if (value == 0) acc else writeUnsignedLong(value / 32, (value % 32).toByte +: acc)

  def readUnsignedLong(length: Int, ints: Bytes): Long =
    ints.take(length).foldLeft(0L) { case acc \ i => acc * 32 + i }

  def read(input: String): PaymentRequest = {
    def loop(data: Bytes, tags: Seq[Bytes] = Nil): Seq[Bytes] =

      if (data.isEmpty) tags else {
        // 104 is the size of a signature
        val len = 1 + 2 + 32 * data(1) + data(2)
        val tags1 = tags :+ data.take(len)
        loop(data drop len, tags1)
      }

    val (hrp, data) = Bech32 decode input
    val stream = data.foldLeft(BitStream.empty)(write5)
    require(stream.bitCount >= 65 * 8, "Data is too short")

    val (stream1, sig) = stream.popBytes(65)
    val data0 = toInt5s(stream1)

    val rawtags = loop(data0 drop 7)
    val tags = rawtags map Tag.parse

    val signature = ByteVector(sig.reverse)
    val (r, s, recid) = Signature decode signature
    val messageHash = Crypto sha256 ByteVector.view(hrp.getBytes ++ stream1.bytes)
    val (pub1, pub2) = Crypto.recoverPublicKey(r -> s, messageHash)
    val pub = if (recid % 2 != 0) pub2 else pub1

    // Will throw on unknown prefix, this is fine
    val prefix = prefixes.values.find(hrp.startsWith).get
    val amountOpt = Amount.decode(hrp drop prefix.length)

    val pr = PaymentRequest(prefix, amountOpt, Timestamp decode data0, pub, tags.toVector, signature)
    require(Crypto.verifySignature(messageHash, r -> s, pub), "Invalid payment request signature")
    pr
  }

  def write(pr: PaymentRequest): String = {
    val hrp = pr.prefix + Amount.encode(pr.amount)
    val int5s = toInt5s(pr.stream writeBytes pr.signature.toSeq)
    Bech32.encode(hrp, int5s)
  }
}