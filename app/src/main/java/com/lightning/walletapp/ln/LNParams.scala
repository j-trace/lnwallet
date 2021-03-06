package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.walletapp.Utils.{app, dbFileName}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.ChannelManager
import com.lightning.walletapp.ln.Tools.Bytes
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector


object LNParams { me =>
  type DepthAndDead = (Int, Boolean)
  val localFeatures = ByteVector.fromValidHex("02")
  val globalFeatures = ByteVector.fromValidHex("")
  val chainHash = Block.LivenetGenesisBlock.hash

  val minDepth = 1
  val minCapacityMsat = 100000000L
  val channelReserveToFundingRatio = 100

  final val dust = Satoshi(2730L)
  final val maxToSelfDelay = 2016
  final val minFeeratePerKw = 253
  final val maxCltvDelta = 7 * 144L
  final val minHtlcValue = MilliSatoshi(1000L)
  final val maxCapacity = MilliSatoshi(16777215000L)

  var db: LNOpenHelper = _
  private[this] var master: ExtendedPrivateKey = _
  lazy val extendedNodeKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
  lazy val extendedCloudKey = derivePrivateKey(master, hardened(92L) :: hardened(0L) :: Nil)
  // HashingKey is used for creating domain-specific identifiers when using "linkable payment" LNUrl
  lazy val hashingKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey.toBin
  // Cloud secret is used to encrypt Olympus and GDrive data, cloud ID is used as identifier
  lazy val cloudSecret = sha256(extendedCloudKey.privateKey.toBin)
  lazy val cloudId = sha256(cloudSecret)

  lazy val bag = PaymentInfoWrap
  lazy val broadcaster: Broadcaster = ChannelManager
  lazy val nodePrivateKey: PrivateKey = extendedNodeKey.privateKey
  lazy val nodePublicKey: PublicKey = nodePrivateKey.publicKey

  def setup(seed: Bytes) = {
    master = generate(ByteVector view seed)
    db = new LNOpenHelper(app, dbFileName)
    app.olympus = new OlympusWrap
  }

  def hopFee(msat: Long, base: Long, proportional: Long) = base + (proportional * msat) / 1000000L
  def maxAcceptableFee(msat: Long, hops: Int, percent: Long = 100L) = 25000 * (hops + 1) + msat / percent

  def isFeeBreach(route: PaymentRoute, msat: Long, divider: Long = 100L) =
    getCompundFee(route, msat) > maxAcceptableFee(msat, route.size, divider)

  def getCompundFee(route: PaymentRoute, msat: Long) = route.reverse.foldLeft(msat) {
    case sum \ hop => sum + hopFee(sum, hop.feeBaseMsat, hop.feeProportionalMillionths)
  } - msat

  def shouldUpdateFee(network: Long, commit: Long) = {
    val mismatch = 2.0 * (network - commit) / (commit + network)
    mismatch < -0.25 || mismatch > 0.25
  }

  def getLinkingKey(domainName: String) = {
    val prefix = crypto.Digests.hmacSha256(hashingKey.toArray, domainName getBytes "UTF-8") take 8
    derivePrivateKey(master, hardened(138L) :: 0L :: BigInt(prefix).toLong :: Nil).privateKey
  }

  def backupFileName = s"blw${chainHash.toHex}-${cloudId.toHex}.bkup"
  def updateFeerate = for (chan <- ChannelManager.notClosing) chan process CMDFeerate(broadcaster.perKwThreeSat)
  def makeLocalParams(ann: NodeAnnouncement, theirReserve: Long, finalScriptPubKey: ByteVector, idx: Long, isFunder: Boolean) = {
    val Seq(fund, rev, pay, delay, htlc, sha) = for (order <- 0L to 5L) yield derivePrivateKey(extendedNodeKey, idx :: order :: Nil)
    LocalParams(UInt64(maxCapacity.amount), theirReserve, toSelfDelay = 2016, maxAcceptedHtlcs = 25, fund.privateKey, rev.privateKey,
      pay.privateKey, delay.privateKey, htlc.privateKey, finalScriptPubKey, dust, sha256(sha.privateKey.toBin), isFunder)
  }
}

object AddErrorCodes {
  import com.lightning.walletapp.R.string._
  val ERR_REMOTE_AMOUNT_HIGH = err_ln_remote_amount_high
  val ERR_REMOTE_AMOUNT_LOW = err_ln_remote_amount_low
  val ERR_TOO_MANY_HTLC = err_ln_too_many
}

trait PublishStatus {
  val txn: Transaction
  def isPublishable = true
}

trait DelayedPublishStatus extends PublishStatus {
  // Is publishable if parent depth > 0 AND parent is not dead AND no CLTV or CSV delays
  override def isPublishable = parent match { case pd \ false \ 0L => pd > 0L case _ => false }
  def delay = parent match { case pd \ false \ blocksLeft => blocksLeft case _ => Long.MinValue }
  val parent: (DepthAndDead, Long)
}

case class HideReady(txn: Transaction) extends PublishStatus
case class ShowReady(txn: Transaction, fee: Satoshi, amount: Satoshi) extends PublishStatus
case class HideDelayed(parent: (DepthAndDead, Long), txn: Transaction) extends DelayedPublishStatus
case class ShowDelayed(parent: (DepthAndDead, Long), txn: Transaction, commitTx: Transaction,
                       fee: Satoshi, amount: Satoshi) extends DelayedPublishStatus

trait Broadcaster extends ChannelListener {
  def getTx(txid: ByteVector): Option[org.bitcoinj.core.Transaction]
  def getStatus(txid: ByteVector): DepthAndDead
  def perKwThreeSat: Long
  def currentHeight: Int
  def perKwSixSat: Long

  // Parent state and next tier cltv delay
  // actual negative delay will be represented as 0L
  def cltv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = getStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(child) - currentHeight, 0L)
    parentDepth -> parentIsDead -> cltvDelay
  }

  // Parent state and cltv + next tier csv delay
  // actual negative delay will be represented as 0L
  def csv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = getStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(parent) - currentHeight, 0L)
    val csvDelay = math.max(csvTimeout(child) - parentDepth, 0L)
    parentDepth -> parentIsDead -> (cltvDelay + csvDelay)
  }

  val blocksPerDay: Int = 144
  def csvShowDelayed(t1: TransactionWithInputInfo, t2: TransactionWithInputInfo, commitTx: Transaction) =
    ShowDelayed(parent = csv(t1.tx, t2.tx), t2.tx, commitTx, fee = t1 -- t2, t2.tx.allOutputsAmount)
}