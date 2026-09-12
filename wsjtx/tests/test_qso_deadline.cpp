#include <QtTest>
#include "widgets/QsoDeadline.hpp"

class TestQsoDeadline : public QObject
{
  Q_OBJECT

private slots:
  void idleAndDisabledLimitsDoNotExpire()
  {
    QsoDeadline deadline;
    QVERIFY(!deadline.active());
    QVERIFY(!deadline.expired(360000, 3, QsoDeadline::Minutes));
    QVERIFY(!deadline.begin("", 0, 15000));
    deadline.begin("N1ABC|FT8|20m", 0, 15000);
    QVERIFY(!deadline.expired(360000, 0, QsoDeadline::Minutes));
    QVERIFY(!deadline.expired(360000, 0, QsoDeadline::Cycles));
  }

  void receiveTimeAndProgressDoNotRestartClock()
  {
    QsoDeadline deadline;
    QVERIFY(deadline.begin("N1ABC|FT8|20m", 1000, 15000));
    // Subsequent TX2/TX3/TX4 choices, reports and missed replies use the same ID.
    for (qint64 now = 31000; now < 181000; now += 30000) {
      QVERIFY(!deadline.begin("N1ABC|FT8|20m", now, 15000));
      QCOMPARE(deadline.elapsed(now), now - 1000);
    }
    QVERIFY(!deadline.expired(180999, 3, QsoDeadline::Minutes));
    QVERIFY(deadline.expired(181000, 3, QsoDeadline::Minutes));
    // This deadline is independent of the secondary TX watchdog's resets.
    QVERIFY(deadline.expired(200000, 3, QsoDeadline::Minutes));
  }

  void cyclesIncludeTxAndRx_data()
  {
    QTest::addColumn<qint64>("periodMs");
    QTest::newRow("FT8") << qint64(15000);
    QTest::newRow("FT4") << qint64(7500);
    QTest::newRow("FT2") << qint64(3750);
    QTest::newRow("Q65-60") << qint64(60000);
  }

  void cyclesIncludeTxAndRx()
  {
    QFETCH(qint64, periodMs);
    QsoDeadline deadline;
    deadline.begin("N1ABC", 0, periodMs);
    QCOMPARE(deadline.cycleMs(), 2 * periodMs);
    QVERIFY(!deadline.expired(6 * periodMs - 1, 3, QsoDeadline::Cycles));
    QVERIFY(deadline.expired(6 * periodMs, 3, QsoDeadline::Cycles));
  }

  void cycleExpiryUsesSlotBoundaryNotGuiJitter()
  {
    QsoDeadline deadline;
    deadline.begin("N1ABC", 10080, 15000, 80);
    QVERIFY(!deadline.expired(39999, 1, QsoDeadline::Cycles));
    QVERIFY(deadline.expired(40000, 1, QsoDeadline::Cycles));
    // Minute mode still measures actual elapsed time from the first TX.
    QVERIFY(!deadline.expired(70000, 1, QsoDeadline::Minutes));
    QVERIFY(deadline.expired(70080, 1, QsoDeadline::Minutes));
  }

  void eachMultiResponseHandoffHasItsOwnLimit()
  {
    QsoDeadline deadline;
    deadline.begin("N1ABC|FT8|20m", 0, 15000);
    // RR73 for N1ABC and the first report to N2ABC share this transmission.
    QVERIFY(deadline.begin("N2ABC|FT8|20m", 60000, 15000));
    QCOMPARE(deadline.elapsed(60000), qint64(0));
    QVERIFY(!deadline.expired(180000, 3, QsoDeadline::Minutes));
    QVERIFY(deadline.expired(240000, 3, QsoDeadline::Minutes));
    QCOMPARE(deadline.identity(), QString("N2ABC|FT8|20m"));
  }

  void finishHaltBandChangeAndManualRetry()
  {
    QsoDeadline deadline;
    deadline.begin("N1ABC|FT8|20m", 0, 15000);
    deadline.clear();
    QVERIFY(!deadline.active());
    QVERIFY(!deadline.expired(180000, 3, QsoDeadline::Minutes));
    QVERIFY(deadline.begin("N1ABC|FT8|20m", 200000, 15000));
    QCOMPARE(deadline.elapsed(210000), qint64(10000));
    QVERIFY(deadline.begin("N1ABC|FT8|40m", 220000, 15000));
    QCOMPARE(deadline.elapsed(220000), qint64(0));
    QVERIFY(deadline.begin("N1ABC|FT4|40m", 230000, 7500));
    QCOMPARE(deadline.cycleMs(), qint64(15000));
  }
};

QTEST_GUILESS_MAIN(TestQsoDeadline)
#include "test_qso_deadline.moc"
