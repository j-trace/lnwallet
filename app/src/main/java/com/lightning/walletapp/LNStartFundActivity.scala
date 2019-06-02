package com.lightning.walletapp

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import android.widget.{ImageButton, TextView, Toast}
import scala.util.{Success, Try}

import com.lightning.walletapp.lnutils.olympus.ChannelUploadAct
import com.lightning.walletapp.ln.Scripts.pubKeyScript
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.MilliSatoshi
import android.app.AlertDialog
import org.bitcoinj.core.Batch
import scodec.bits.ByteVector
import android.os.Bundle


class LNStartFundActivity extends TimerActivity { me =>
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  lazy val fundNodeView = app getString ln_ops_start_fund_node_view
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  override def onBackPressed = whenBackPressed.run

  def INIT(s: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)
    defineFurtherActionBasedOnTransDataValue
  } else me exitTo classOf[MainActivity]

  def defineFurtherActionBasedOnTransDataValue = app.TransData checkAndMaybeErase {
    case remoteNodeView @ RemoteNodeView(ann \ _) => proceed(None, remoteNodeView.asString(fundNodeView), ann)
    case hardcodedNodeView @ HardcodedNodeView(ann, _) => proceed(None, hardcodedNodeView.asString(fundNodeView), ann)
    case ann: NodeAnnouncement => proceed(None, HardcodedNodeView(ann, tip = "( ͡° ͜ʖ ͡°)").asString(fundNodeView), ann)
    case icp: IncomingChannelParams => proceed(Some(icp.open), icp.nodeView.asString(fundNodeView), icp.nodeView.ann)
    case _ => finish
  }

  def proceed(openOpt: Option[OpenChannel], asString: String, ann: NodeAnnouncement): Unit = {
    val freshChannel = ChannelManager.createChannel(bootstrap = InitData(ann), initialListeners = Set.empty)
    val peerIncompatible = new LightningException(me getString err_ln_peer_incompatible format ann.alias)
    val peerOffline = new LightningException(me getString err_ln_peer_offline format ann.alias)
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText asString.html

    class OpenListener extends ConnectionListener with ChannelListener { self =>
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChannel -> peerOffline)

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case open: OpenChannel if nodeId == ann.nodeId && !open.channelFlags.isPublic => onOpenOffer(nodeId, open)
        case remoteError: Error if nodeId == ann.nodeId => onException(freshChannel -> remoteError.exception)
        case _: ChannelSetupMessage if nodeId == ann.nodeId => freshChannel process msg
        case _ => // We only listen to setup messages here to avoid conflicts
      }

      override def onOpenOffer(nodeId: PublicKey, open: OpenChannel) = if (openOpt.isEmpty) {
        val hnv = HardcodedNodeView(ann, tip = app getString ln_ops_start_fund_incoming_channel)

        // This one is useless now
        freshChannel.listeners -= self
        ConnectionManager.listeners -= self
        // Replace outgoing listener with an incoming one
        app.TransData.value = IncomingChannelParams(hnv, open)
        UITask(defineFurtherActionBasedOnTransDataValue).run
      }

      override def onException = {
        case _ \ errorWhileOpening =>
          // Inform user about error, disconnect this channel, go back to channel list
          UITask(Toast.makeText(me, errorWhileOpening.getMessage, Toast.LENGTH_LONG).show).run
          whenBackPressed.run
      }
    }

    abstract class LocalOpenListener extends OpenListener {
      override def onOperational(nodeId: PublicKey, isCompat: Boolean) = if (nodeId == ann.nodeId) {
        // Peer has sent us their Init so we ask user to provide a funding amount if peer is compatible
        if (isCompat) askLocalFundingConfirm.run else onException(freshChannel -> peerIncompatible)
      }

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we create a real funding transaction
          // We create a funding transaction by replacing an output with a real one in a saved dummy funding transaction
          val req = cmd.batch replaceDummy pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          freshChannel process CMDFunding(app.kit.sign(req).tx)

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and broadcast a fund tx
          saveChan(wait)

          // Broadcast a funding transaction
          // Tell wallet activity to redirect to ops
          LNParams.broadcaster nullOnBecome freshChannel
          app.TransData.value = FragWallet.REDIRECT
          me exitTo MainActivity.wallet
      }

      // Provide manual or batched amount
      def askLocalFundingConfirm: Runnable
    }

    def saveChan(some: HasCommitments) = {
      // First of all we should store this chan
      // error here will halt all further progress
      freshChannel STORE some

      app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
      // Start watching a channel funding script and save a channel, order an encrypted backup upload
      val encrypted = AES.encReadable(RefundingData(some.announce, None, some.commitments).toJson.toString, LNParams.cloudSecret.toArray)
      val chanUpload = ChannelUploadAct(encrypted.toByteVector, Seq("key" -> LNParams.cloudId.toHex), "data/put", some.announce.alias)
      app.olympus.tellClouds(chanUpload)

      // Make this channel able to receive ordinary events
      freshChannel.listeners = ChannelManager.operationalListeners
      ChannelManager.all +:= freshChannel
    }

    def localWalletFunderListener = new LocalOpenListener {
      // Asks user to provide a funding amount manually

      def askLocalFundingConfirm = UITask {
        val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
        val maxCap = MilliSatoshi(LNParams.maxCapacity.amount min app.kit.conf0Balance.value * 1000L)
        val minCap = MilliSatoshi(LNParams.minCapacityMsat max LNParams.broadcaster.perKwThreeSat * 3 * 1000L)
        val rateManager = new RateManager(content) hint getString(amount_hint_newchan).format(denom parsedWithSign minCap,
          denom parsedWithSign LNParams.maxCapacity, denom parsedWithSign app.kit.conf0Balance)

        def askAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if ms < minCap => app toast dialog_sum_small
          case Success(ms) if ms > maxCap => app toast dialog_sum_big

          case Success(ms) =>
            val txProcessor = new TxProcessor {
              val dummyKey = randomPrivKey.publicKey
              val dummyScript = pubKeyScript(dummyKey, dummyKey)
              val pay = P2WSHData(ms, pay2wsh = dummyScript)

              def onTxFail(error: Throwable): Unit =
                mkCheckForm(alert => rm(alert)(finish), none,
                  txMakeErrorBuilder(error), dialog_ok, -1)

              def futureProcess(unsignedRequest: SendRequest) = {
                val batch = Batch(unsignedRequest, dummyScript, null)
                val theirReserveSat = batch.fundingAmountSat / LNParams.channelReserveToFundingRatio
                val finalPubKeyScript = ByteVector(ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram)
                val localParams = LNParams.makeLocalParams(ann, theirReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
                val cmd = CMDOpenChannel(localParams, ByteVector(random getBytes 32), LNParams.broadcaster.perKwThreeSat, batch, batch.fundingAmountSat)
                freshChannel process cmd
              }
            }

            val coloredAmount = denom.coloredP2WSH(txProcessor.pay.cn, denom.sign)
            val coloredExplanation = txProcessor.pay destination coloredAmount
            rm(alert)(txProcessor start coloredExplanation)

          case _ =>
            app toast dialog_sum_small
        }

        def useMax(alert: AlertDialog) = rateManager setSum Try(maxCap)
        val bld = baseBuilder(getString(ln_ops_start_fund_local_title).html, content)
        mkCheckFormNeutral(askAttempt, none, useMax, bld, dialog_next, dialog_cancel, dialog_max)
      }
    }

    def remoteFundeeListener(open: OpenChannel) = new OpenListener {
      val theirReserve = open.fundingSatoshis / LNParams.channelReserveToFundingRatio
      val finalPubKeyScript = ByteVector(ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram)
      val params = LNParams.makeLocalParams(ann, theirReserve, finalPubKeyScript, System.currentTimeMillis, isFunder = false)
      freshChannel process Tuple2(params, open)

      override def onBecome = {
        case (_, wait: WaitBroadcastRemoteData, WAIT_FOR_FUNDING, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and wait for their tx
          saveChan(wait)

          // Tell wallet activity to redirect to ops
          app.TransData.value = FragWallet.REDIRECT
          me exitTo MainActivity.wallet
      }
    }

    val openListener = openOpt match {
      case Some(open) => remoteFundeeListener(open)
      case None => localWalletFunderListener
    }

    whenBackPressed = UITask {
      freshChannel.listeners -= openListener
      ConnectionManager.listeners -= openListener
      // Worker may have already been removed on some connection failure
      ConnectionManager.connections.get(ann.nodeId).foreach(_.disconnect)
      finish
    }

    freshChannel.listeners += openListener
    ConnectionManager.listeners += openListener
    ConnectionManager.connectTo(ann, notify = true)
  }
}