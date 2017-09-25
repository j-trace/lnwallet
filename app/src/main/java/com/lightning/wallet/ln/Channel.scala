package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.AddErrorCodes._

import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import com.lightning.wallet.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.{Satoshi, Transaction}

import concurrent.ExecutionContext.Implicits.global
import fr.acinq.bitcoin.Crypto.PrivateKey
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Success


abstract class Channel extends StateMachine[ChannelData] { me =>
  def pull[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any) = Future apply synchronized(me doProcess change) onFailure events.onError
  val listeners = mutable.Set.empty[ChannelListener]

  private[this] val events = new ChannelListener {
    override def onError = { case error => for (lst <- listeners if lst.onError isDefinedAt error) lst onError error }
    override def onBecome = { case trans => for (lst <- listeners if lst.onBecome isDefinedAt trans) lst onBecome trans }
    override def onProcess = { case some => for (lst <- listeners if lst.onProcess isDefinedAt some) lst onProcess some }
  }

  def SEND(msg: LightningMessage): Unit
  def STORE(content: HasCommitments): HasCommitments
  def UPDATE(d1: ChannelData): Channel = BECOME(d1, state)
  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition should always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd @ CMDOpenChannel(localParams, tempId,
        initialFeeratePerKw, pushMsat, _, fundingSat), WAIT_FOR_INIT) =>

        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash,
          tempId, fundingSat, pushMsat, LNParams.dustLimit.amount, localParams.maxHtlcValueInFlightMsat,
          localParams.channelReserveSat, LNParams.htlcMinimumMsat, initialFeeratePerKw, localParams.toSelfDelay,
          localParams.maxAcceptedHtlcs, localParams.fundingPrivKey.publicKey, localParams.revocationSecret.toPoint,
          localParams.paymentKey.toPoint, localParams.delayedPaymentKey.toPoint,
          Generators.perCommitPoint(localParams.shaSeed, index = 0),
          channelFlags = 1.toByte) // TODO: remove this


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
        if accept.temporaryChannelId == cmd.temporaryChannelId =>

        // If remote requires us to keep too much in local reserve we should close
        // otherwise we should wait for user to create a funding transaction in wallet UI
        val exceedsReserve = LNParams.exceedsReserve(accept.channelReserveSatoshis, cmd.fundingAmountSat)
        if (exceedsReserve) BECOME(wait, CLOSING) else BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


      // They have accepted our proposal, now let them sign a first commit tx
      case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(cmd, accept,
          fundTx.hash, outIndex, accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.temporaryChannelId, fundTx.hash, outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(index = 0, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)

        BECOME(WaitFundingSignedData(announce, cmd.localParams,
          Tools.toLongId(fundTx.hash, outIndex), accept, fundTx, localSpec,
          localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit tx, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED)
        if remote.channelId == wait.channelId =>

        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
          wait.remoteParams.fundingPubkey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

        if (Scripts.checkSpendable(signedLocalCommitTx).isEmpty) BECOME(wait, CLOSING) else {
          val localCommit = LocalCommit(index = 0, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
          val localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
          val remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty)
          val dummy = PrivateKey(data = Tools.random getBytes 32, compressed = true).toPoint

          val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit,
            remoteCommit = wait.remoteCommit, localChanges, remoteChanges, localNextHtlcId = 0,
            remoteNextHtlcId = 0, remoteNextCommitInfo = Right(dummy), wait.localCommitTx.input,
            remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), wait.channelId)

          BECOME(me STORE WaitFundingDoneData(wait.announce, our = None,
            their = None, wait.fundingTx, commitments), WAIT_FUNDING_DONE)
        }


      // FUNDING TX IS BROADCASTED AT THIS POINT


      // We have not yet sent a FundingLocked but just got one from them so we save it and keep waiting
      case (wait @ WaitFundingDoneData(_, None, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>
        me UPDATE wait.copy(their = Some apply their)


      // We have already sent them a FundingLocked and now we got one from them so we can enter normal state now
      case (wait @ WaitFundingDoneData(_, Some(our), _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>
        startNormalChannelMode(wait, their)


      // We got our lock but their is not yet present so we save ours and just keep waiting for their
      case (wait @ WaitFundingDoneData(_, _, None, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val d1 = me STORE wait.copy(our = Some apply our)
        me UPDATE d1 SEND our


      // We got our lock when their is already present so we can safely enter normal state now
      case (wait @ WaitFundingDoneData(_, _, Some(their), _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        startNormalChannelMode(wait, their) SEND our


      // NORMAL MODE


      // TODO: remove this
      case (norm @ NormalData(_, commitments, None, None), remote: AnnouncementSignatures, NORMAL) =>
        val (localNodeSig, localBitcoinSig) = Announcements.signChannelAnnouncement(LNParams.chainHash, remote.shortChannelId,
          LNParams.nodePrivateKey, norm.announce.nodeId, commitments.localParams.fundingPrivKey, commitments.remoteParams.fundingPubkey,
          LNParams.globalFeatures)

        me SEND AnnouncementSignatures(commitments.channelId,
          remote.shortChannelId, localNodeSig, localBitcoinSig)


      case (norm: NormalData, add: UpdateAddHtlc, NORMAL)
        if add.channelId == norm.commitments.channelId =>

        // Should check if we have enough blocktime left to fulfill this HTLC
        val c1 = Commitments.receiveAdd(norm.commitments, add, LNParams.expiry)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
        if fulfill.channelId == norm.commitments.channelId =>

        // Got a fulfill for an outgoing HTLC we sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailHtlc, NORMAL)
        if fail.channelId == norm.commitments.channelId =>

        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, NORMAL)
        if fail.channelId == norm.commitments.channelId =>

        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATE norm.copy(commitments = c1)


      // We only can add new HTLCs when mutual shutdown process is not active
      case (norm @ NormalData(_, commitments, None, None), cmd: CMDAddHtlc, NORMAL) =>
        val c1 \ updateAddHtlc = Commitments.sendAdd(commitments, cmd)

        LNParams.bag getPaymentInfo updateAddHtlc.paymentHash match {
          case Success(out: OutgoingPayment) if out.actualStatus == WAITING => throw AddException(cmd, ERR_IN_FLIGHT)
          case Success(out: OutgoingPayment) if out.actualStatus == SUCCESS => throw AddException(cmd, ERR_FULFILLED)
          case Success(out: OutgoingPayment) if out.actualStatus == TEMP => throw AddException(cmd, ERR_IN_FLIGHT)
          case Success(out: OutgoingPayment) if out.actualStatus == REFUND => throw AddException(cmd, ERR_FAILED)
          case Success(_: IncomingPayment) => throw AddException(cmd, ERR_FAILED)

          case _ =>
            // This may be a failed outgoing payment
            // which probably means we try to re-use a failed request
            // this is fine, but such a request should not be stored twice
            me UPDATE norm.copy(commitments = c1) SEND updateAddHtlc
            doProcess(CMDProceed)
        }

      // We're fulfilling an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFulfillHtlc, NORMAL) =>
        val c1 \ updateFulfillHtlc = Commitments.sendFulfill(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailHtlc, NORMAL) =>
        val c1 \ updateFailHtlc = Commitments.sendFail(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailMalformedHtlc, NORMAL) =>
        val c1 \ updateFailMalformedHtlс = Commitments.sendFailMalformed(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, NORMAL) =>
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag)

        // And sign once done
        doProcess(CMDProceed)


      // GUARD: We can send a commit sig
      case (norm: NormalData, CMDProceed, NORMAL)
        if Commitments.localHasChanges(norm.commitments) &&
          norm.commitments.remoteNextCommitInfo.isRight =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND commitSig


      case (norm: NormalData, sig: CommitSig, NORMAL)
        if sig.channelId == norm.commitments.channelId =>

        // We received a commit sig from them, now we can update our local commit
        val c1 \ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND revokeAndAck
        doProcess(CMDProceed)


      case (norm: NormalData, rev: RevokeAndAck, NORMAL)
        if rev.channelId == norm.commitments.channelId =>

        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        me UPDATE norm.copy(commitments = c1)
        doProcess(CMDHTLCProcess)


      case (norm: NormalData, CMDFeerate(rate), NORMAL)
        // GUARD: only send fee updates if the fee gap between nodes is large enough
        if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>

        // Periodic fee updates to ensure commit txs could be confirmed
        val c1 \ updateFeeMessage = Commitments.sendFee(norm.commitments, rate)
        me UPDATE norm.copy(commitments = c1) SEND updateFeeMessage
        doProcess(CMDProceed)


      // NORMAL: SHUTDOWN


      // Sending CMDShutdown when mutual shutdown is already in progress means we want uncooperative close
      case (norm: NormalData, CMDShutdown, NORMAL) if norm.localShutdown.isDefined => startLocalCurrentClose(norm)
      case (norm: NormalData, CMDShutdown, NORMAL) if norm.remoteShutdown.isDefined => startLocalCurrentClose(norm)
      case (norm: NormalData, CMDShutdown, NORMAL) => me startShutdown norm


      // They try to shutdown with uncommited changes
      case (norm: NormalData, remote: Shutdown, NORMAL)
        if remote.channelId == norm.commitments.channelId &&
          Commitments.remoteHasChanges(norm.commitments) =>

        // Can't start mutual shutdown
        startLocalCurrentClose(norm)


      // They initiate shutdown or respond to ours
      case (norm: NormalData, remote: Shutdown, NORMAL)
        if remote.channelId == norm.commitments.channelId &&
          norm.remoteShutdown.isEmpty =>

        // We got their first shutdown so save it and proceed
        me UPDATE norm.copy(remoteShutdown = Some apply remote)
        doProcess(CMDProceed)


      // GUARD: we can't send our shutdown untill all HTLCs are resolved
      case (norm @ NormalData(_, commitments, None, their), CMDProceed, NORMAL)
        if Commitments.pendingHtlcs(commitments).isEmpty && their.isDefined =>

        me startShutdown norm
        doProcess(CMDProceed)


      // This is the final stage: both Shutdown messages are present and no pending HTLCs are left
      case (NormalData(announce, commitments, Some(local), Some(remote) /* both present */), CMDProceed, NORMAL)
        if Commitments.pendingHtlcs(commitments).isEmpty =>

        val feerate = commitments.localCommit.spec.feeratePerKw
        val sig = Closing.makeFirstClosing(commitments, local.scriptPubKey, remote.scriptPubKey, feerate)
        val neg = NegotiationsData(announce, commitments, localClosingSigned = sig, local, remote)
        BECOME(me STORE neg, NEGOTIATIONS) SEND sig


      case (norm: NormalData, CMDBestHeight(height), NORMAL | SYNC)
        if norm.commitments.localCommit.spec.htlcs.exists(htlc => !htlc.incoming && height >= htlc.add.expiry) ||
          norm.commitments.remoteCommit.spec.htlcs.exists(htlc => htlc.incoming && height >= htlc.add.expiry) =>

        // Outdated HTLCs are present
        startLocalCurrentClose(norm)


      // SYNC MODE


      // We may get this message any time so just save it here
      case (wait: WaitFundingDoneData, CMDConfirmed(tx), SYNC)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val wait1 = me STORE wait.copy(our = Some apply our)
        me UPDATE wait1


      // We're exiting a sync state but don't have enough locks so we keep waiting
      case (wait: WaitFundingDoneData, ChannelReestablish(channelId, 1, 0), SYNC)
        if channelId == wait.commitments.channelId =>

        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      case (neg: NegotiationsData, channelReestablish: ChannelReestablish, SYNC)
        if channelReestablish.channelId == neg.commitments.channelId =>
        BECOME(neg, NEGOTIATIONS) SEND neg.localClosingSigned


      case (norm: NormalData, cr: ChannelReestablish, SYNC)
        if cr.channelId == norm.commitments.channelId =>

        // If next_local_commitment_number is 1 in both the channel_reestablish it sent
        // and received, then the node MUST retransmit funding_locked, otherwise it MUST NOT
        if (cr.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0)
          me SEND makeFundingLocked(norm.commitments)

        // First we clean up unacknowledged updates
        val localProposedIdDelta = norm.commitments.localChanges.proposed count { case u: UpdateAddHtlc => true }
        val remoteProposedIdDelta = norm.commitments.remoteChanges.proposed count { case u: UpdateAddHtlc => true }
        val c1 = norm.commitments.modifyAll(_.localChanges.proposed, _.remoteChanges.proposed).setTo(Vector.empty)
          .modify(_.remoteNextHtlcId).using(currentRemoteCount => currentRemoteCount - remoteProposedIdDelta)
          .modify(_.localNextHtlcId).using(currentLocalCount => currentLocalCount - localProposedIdDelta)

        def maybeResendRevocation = if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
          val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
          val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
          me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
        } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw new LightningException

        c1.remoteNextCommitInfo match {
          // We had sent a new sig and were waiting for their revocation
          // they didn't receive the new sig because disconnection happened
          // we resend the same updates and sig, also be careful about revocation
          case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>
            val revocationWasSentLast = c1.localCommit.index > wait.localCommitIndexSnapshot

            if (!revocationWasSentLast) maybeResendRevocation
            c1.localChanges.signed :+ wait.sent foreach SEND
            if (revocationWasSentLast) maybeResendRevocation

          // We had sent a new sig and were waiting for their revocation, they had received
          // the new sig but their revocation was lost during the disconnection, they'll resend us the revocation
          case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case _ => throw new LightningException
        }

        BECOME(norm.copy(commitments = c1), NORMAL)
        norm.localShutdown foreach SEND
        doProcess(CMDHTLCProcess)


      // We just close a channel in any kind of irregular state
      case (some: HasCommitments, cr: ChannelReestablish, SYNC)
        if cr.channelId == some.commitments.channelId =>
        startLocalCurrentClose(some)


      case (recovery: RefundingData, cr: ChannelReestablish, REFUNDING)
        if cr.channelId == recovery.commitments.channelId =>

        val d1 = recovery.modify(_.commitments.remoteCommit.index) setTo cr.nextRemoteRevocationNumber
        val d2 = d1.modify(_.commitments.remoteCommit.remotePerCommitmentPoint) setTo null // TODO: use real value
        me UPDATE d2 SEND Error(cr.channelId, "Balance recovery")


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, SYNC) =>
        me SEND ChannelReestablish(channelId = some.commitments.channelId,
          nextLocalCommitmentNumber = some.commitments.localCommit.index + 1,
          nextRemoteRevocationNumber = some.commitments.remoteCommit.index)


      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, SYNC)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, SYNC)
      case (norm: NormalData, CMDOffline, NORMAL) => BECOME(norm, SYNC)


      // NEGOTIATIONS MODE


      case (neg: NegotiationsData, ClosingSigned(channelId, feeSatoshis, remoteSig), NEGOTIATIONS)
        if channelId == neg.commitments.channelId =>

        val Seq(closeFeeSat, remoteFeeSat) = Seq(neg.localClosingSigned.feeSatoshis, feeSatoshis) map Satoshi
        val Seq(localScript, remoteScript) = Seq(neg.localShutdown.scriptPubKey, neg.remoteShutdown.scriptPubKey)
        val closeOpt = Closing.checkClosingSignature(neg.commitments, localScript, remoteScript, remoteFeeSat, remoteSig)
        lazy val (_, nextClosingSigned) = Closing.makeClosing(neg.commitments, localScript, remoteScript, nextCloseFee)
        lazy val nextCloseFee = Closing.nextClosingFee(closeFeeSat, remoteFeeSat)

        closeOpt match {
          case Some(close) if closeFeeSat == remoteFeeSat => startMutualClose(neg, close.tx)
          case Some(close) if nextCloseFee == remoteFeeSat => startMutualClose(neg, close.tx)
          case Some(_) => me UPDATE neg.copy(localClosingSigned = nextClosingSigned) SEND nextClosingSigned
          case _ => throw new LightningException
        }


      // HANDLE FUNDING SPENT


      case (some: HasCommitments, CMDSpent(spendTx), _)
        // GUARD: something which spends our funding is broadcasted, must react
        if spendTx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>
        some.commitments.remoteNextCommitInfo.left.map(wait => wait.nextRemoteCommit) match {
          case Left(remoteCommit) if remoteCommit.txid == spendTx.txid => startRemoteNextClose(some, remoteCommit)
          case _ if some.commitments.remoteCommit.txid == spendTx.txid => startRemoteCurrentClose(some)
          case _ => startOtherClose(some, spendTx)
        }


      // HANDLE INITIALIZATION


      case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
      case (null, closing: ClosingData, null) => BECOME(closing, CLOSING)
      case (null, recovery: RefundingData, null) => BECOME(recovery, REFUNDING)
      case (null, wait: WaitFundingDoneData, null) => BECOME(wait, SYNC)
      case (null, negs: NegotiationsData, null) => BECOME(negs, SYNC)
      case (null, norm: NormalData, null) => BECOME(norm, SYNC)


      // MISC


      case (some, CMDShutdown, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) => BECOME(some, CLOSING)
      case (some: HasCommitments, CMDShutdown, WAIT_FUNDING_DONE | NEGOTIATIONS | SYNC) => startLocalCurrentClose(some)
      case (_: NormalData, add: CMDAddHtlc, SYNC) => throw AddException(add, ERR_OFFLINE)

      case _ =>
        // Let know if received an unhandled message
        Tools log s"Channel: unhandled $state : $change"
    }

    // Change has been successfully processed
    events onProcess Tuple3(me, data, change)
  }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, index = 1)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = point)
  }

  private def startNormalChannelMode(wait: HasCommitments, their: FundingLocked) = {
    val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), NORMAL)
  }

  private def startShutdown(norm: NormalData) = {
    val localShutdown = Shutdown(norm.commitments.channelId, norm.commitments.localParams.defaultFinalScriptPubKey)
    me UPDATE norm.copy(localShutdown = Some apply localShutdown) SEND localShutdown
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction) =
    BECOME(me STORE ClosingData(neg.announce, neg.commitments, closeTx :: Nil), CLOSING)

  private def startLocalCurrentClose(some: HasCommitments) =
    // Something went wrong and we decided to spend our current commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, some.commitments.localCommit.commitTx.tx, LNParams.bag) -> some match {
      case (claim: LocalCommitPublished, closing: ClosingData) => BECOME(data1 = me STORE closing.copy(localCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(data1 = me STORE ClosingData(some.announce, some.commitments, localCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteCurrentClose(some: HasCommitments) =
    // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me STORE closing.copy(remoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, remoteCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteNextClose(some: HasCommitments, nextRemoteCommit: RemoteCommit) =
    // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me STORE closing.copy(nextRemoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, nextRemoteCommit = claim :: Nil), CLOSING)
    }

  private def startOtherClose(some: HasCommitments, tx: Transaction) =
    // They may have spent a revoked transaction so we can maybe take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, tx) -> some match {
      case (Some(claim), close: ClosingData) => BECOME(me STORE close.modify(_.revokedCommit).using(claim +: _), CLOSING)
      case (Some(claim), _) => BECOME(me STORE ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil), CLOSING)
      case (None, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log "Disregarding mutual closing tx"
      case (None, neg: NegotiationsData) => startMutualClose(neg, tx)
      case _ => startLocalCurrentClose(some)
    }
}

object Channel {
  // Ephemeral (chan not saved)
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FOR_FUNDING = "WaitForFunding"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"

  // These states are saved
  val WAIT_FUNDING_DONE = "WaitFundingDone"
  val NEGOTIATIONS = "Negotiations"
  val REFUNDING = "Refunding"
  val NORMAL = "Normal"
  val SYNC = "Sync"

  // Makes chan inactive
  val CLOSING = "Closing"
}

trait ChannelListener {
  def reloadOnBecome(chan: Channel): Unit = {
    val trans = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt trans) onBecome(trans)
  }

  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onBecome: PartialFunction[Transition, Unit] = none
  def onProcess: PartialFunction[Incoming, Unit] = none
  def onError: PartialFunction[Throwable, Unit] = none
}