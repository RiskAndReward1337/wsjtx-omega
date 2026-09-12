#include <QtTest>
#include "widgets/MultiResponse.hpp"

class TestMultiResponse : public QObject
{
  Q_OBJECT

  static QDateTime utc(int second)
  {
    return QDateTime(QDate(2026, 9, 12), QTime(12, 0), Qt::UTC).addSecs(second);
  }

  static void hear(MultiResponse::Caller& caller, int second)
  {
    caller.heard(MultiResponse::decodePeriod(12 * 3600 + second, utc(second + 12), 15000));
    caller.heardAt = utc(second + 12);
  }

private slots:
  void onlyEnabledFt8AutoPotaUsesMultiResponse()
  {
    QVERIFY(MultiResponse::enabled(true, "FT8", true, false, false, false));
    QVERIFY(!MultiResponse::enabled(false, "FT8", true, false, false, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", false, false, false, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", false, true, false, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", false, false, true, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", false, false, true, true));
    for (QString const& mode : {QString("FT4"), QString("FT2"), QString("Q65"), QString("MSK144")}) {
      QVERIFY(!MultiResponse::enabled(true, mode, true, false, false, false));
    }
    // Overlapping switches during a mode transition must not enable the queue.
    QVERIFY(!MultiResponse::enabled(true, "FT8", true, true, false, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", true, false, true, false));
    QVERIFY(!MultiResponse::enabled(true, "FT8", true, false, false, true));
  }

  void continuingCallerBeatsSilentCaller()
  {
    MultiResponse::Caller second, third;
    second.call = "N2ABC";
    third.call = "N3ABC";
    hear(second, 0);
    hear(third, 0);
    hear(third, 30);
    QQueue<MultiResponse::Caller> callers;
    callers << second << third;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), 1);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::QueueOrder, utc(45), 15000, false), 0);
  }

  void duplicatePassesAreNotRepeatCalls()
  {
    MultiResponse::Caller caller;
    hear(caller, 0);
    hear(caller, 0);
    hear(caller, 0);
    QCOMPARE(caller.heardPeriods, 1);
    hear(caller, 30);
    QCOMPARE(caller.heardPeriods, 2);
    hear(caller, 0); // Delayed output from an older decode cannot advance activity.
    QCOMPARE(caller.heardPeriods, 2);
    QCOMPARE(caller.lastPeriod, MultiResponse::receivePeriod(utc(45), 15000, false));
  }

  void staleEntriesLeaveTheQueueAtTx()
  {
    MultiResponse::Caller second, third;
    second.call = "N2ABC";
    third.call = "N3ABC";
    hear(second, 0);
    hear(third, 0);
    hear(third, 30);
    QQueue<MultiResponse::Caller> callers;
    callers << second << third;
    auto discarded = MultiResponse::discardStaleCallers(callers, utc(45), 15000, false);
    QCOMPARE(discarded.size(), 1);
    QCOMPARE(discarded.first().call, second.call);
    QCOMPARE(callers.size(), 1);
    QCOMPARE(callers.first().call, third.call);
    QCOMPARE(callers.first().heardPeriods, 2);
    discarded = MultiResponse::discardStaleCallers(callers, utc(75), 15000, false);
    QCOMPARE(discarded.size(), 1);
    QVERIFY(callers.isEmpty());
  }

  void repeatCallerBeatsNewCallerInSamePeriod()
  {
    MultiResponse::Caller newcomer, repeat;
    hear(newcomer, 30);
    hear(repeat, 0);
    hear(repeat, 30);
    QQueue<MultiResponse::Caller> callers;
    callers << newcomer << repeat;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), 1);
  }

  void tiesKeepArrivalOrder()
  {
    MultiResponse::Caller first, second;
    hear(first, 30);
    hear(second, 30);
    QQueue<MultiResponse::Caller> callers;
    callers << first << second;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), 0);
  }

  void silentQueueDoesNotStartAnotherQso()
  {
    MultiResponse::Caller caller;
    hear(caller, 0);
    QQueue<MultiResponse::Caller> callers;
    callers << caller;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), -1);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(75), 15000, false), -1);
  }

  void callerCanReturnAfterMissingAPeriod()
  {
    MultiResponse::Caller caller;
    hear(caller, 0);
    QQueue<MultiResponse::Caller> callers;
    callers << caller;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), -1);
    hear(callers[0], 60);
    QCOMPARE(callers[0].heardPeriods, 1);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(75), 15000, false), 0);
  }

  void allDecodePassesCanContributeBeforeTx()
  {
    MultiResponse::Caller caller;
    hear(caller, 0);
    QQueue<MultiResponse::Caller> callers;
    callers << caller;
    // Cleanup after our signoff at :33 must not seize a caller heard at :00.
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(33), 15000, false), -1);
    // A late decode from :30 is still eligible at the following TX boundary.
    hear(callers[0], 30);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(44), 15000, false), 0);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), 0);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(46), 15000, false), 0);
  }

  void txFirstUsesOppositeReceivePeriod()
  {
    MultiResponse::Caller caller;
    caller.txFirst = true;
    hear(caller, 15);
    QQueue<MultiResponse::Caller> callers;
    callers << caller;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(30), 15000, true), 0);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(30), 15000, false), -1);
  }

  void midnightDecodeKeepsItsOriginalPeriod()
  {
    QDateTime const midnight(QDate(2026, 9, 13), QTime(0, 0), Qt::UTC);
    auto const before = MultiResponse::decodePeriod(86385, midnight.addSecs(-1), 15000);
    auto const after = MultiResponse::decodePeriod(86385, midnight.addSecs(1), 15000);
    QCOMPARE(before, after);
    QCOMPARE(after, MultiResponse::receivePeriod(midnight, 15000, true));
  }

  void emptyQueueHasNoCandidate()
  {
    QQueue<MultiResponse::Caller> callers;
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::RecentCallers, utc(45), 15000, false), -1);
    QCOMPARE(MultiResponse::nextCaller(callers, MultiResponse::QueueOrder, utc(45), 15000, false), -1);
  }

  void aQuietActiveQsoIsNotReplacedByWaitingCallers()
  {
    MultiResponse::Caller silent, continuing;
    silent.call = "N2ABC";
    continuing.call = "N3ABC";
    hear(silent, 0);
    hear(continuing, 30);
    QQueue<MultiResponse::Caller> callers;
    callers << silent << continuing;
    MultiResponse::discardStaleCallers(callers, utc(45), 15000, false);
    QCOMPARE(callers.size(), 1);
    QCOMPARE(callers.first().call, QString("N3ABC"));
    // Station 1 remains our partner even if it missed this receive period.
    QVERIFY(MultiResponse::keepActiveCaller(false, true, false));
    // Its later report/RR73 still progresses normally; a manual click can switch.
    QVERIFY(!MultiResponse::keepActiveCaller(false, true, true));
    QVERIFY(!MultiResponse::keepActiveCaller(true, true, false));
    QVERIFY(!MultiResponse::keepActiveCaller(false, false, false));
  }
};

QTEST_GUILESS_MAIN(TestMultiResponse)
#include "test_multi_response.moc"
