#include <QtTest>
#include "logbook/LogQsoStatistics.hpp"

class TestLogQsoStatistics : public QObject
{
  Q_OBJECT

private slots:
  void everyRepeatAfterFirstCounts()
  {
    LogQsoStatistics stats;
    stats.add("N1ABC", "20m", "FT8");
    QCOMPARE(stats.total(), 1);
    QCOMPARE(stats.duplicates(), 0);
    stats.add("N1ABC", "20m", "FT8");
    stats.add("N1ABC", "20m", "FT8");
    stats.add("N2ABC", "20m", "FT8");
    QCOMPARE(stats.total(), 4);
    QCOMPARE(stats.duplicates(), 2);
  }

  void bandModeAndFullCallArePartOfKey()
  {
    LogQsoStatistics stats;
    stats.add("WB9X/P", "20m", "FT8");
    stats.add("WB9X/P", "40m", "FT8");
    stats.add("WB9X/P", "20m", "FT4");
    stats.add("WB9X", "20m", "FT8");
    stats.add("WB9X/QRP", "20m", "FT8");
    QCOMPARE(stats.duplicates(), 0);
    // The same full callsign, band and mode counts regardless of date or grid.
    stats.add("wb9x/p", "20M", "ft8");
    QCOMPARE(stats.total(), 6);
    QCOMPARE(stats.duplicates(), 1);
  }

  void caseAndWhitespaceDoNotCreateUniqueContacts()
  {
    LogQsoStatistics stats;
    stats.add(" YS3/PY3XX ", " 20m ", " FT8 ");
    stats.add("ys3/py3xx", "20M", "ft8");
    QCOMPARE(stats.total(), 2);
    QCOMPARE(stats.duplicates(), 1);
  }

  void incompleteRecordsDoNotInventDuplicates()
  {
    LogQsoStatistics stats;
    stats.add("", "20m", "FT8");
    stats.add("N1ABC", "", "FT8");
    stats.add("N1ABC", "", "FT8");
    stats.add("N1ABC", "20m", "");
    QCOMPARE(stats.total(), 3);
    QCOMPARE(stats.duplicates(), 0);
  }

  void snapshotReloadAndAppendUseTheSameCount()
  {
    LogQsoStatistics loaded;
    loaded.add("N1ABC", "20m", "FT8");
    loaded.add("N1ABC", "20m", "FT8");
    LogQsoStatistics live = loaded;
    live.add("N1ABC", "20m", "FT8");
    live.add("N2ABC", "40m", "FT4");
    QCOMPARE(loaded.total(), 2);
    QCOMPARE(loaded.duplicates(), 1);
    QCOMPARE(live.total(), 4);
    QCOMPARE(live.duplicates(), 2);
  }
};

QTEST_GUILESS_MAIN(TestLogQsoStatistics)
#include "test_log_qso_statistics.moc"
