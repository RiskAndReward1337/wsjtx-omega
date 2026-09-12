#ifndef MULTI_RESPONSE_HPP
#define MULTI_RESPONSE_HPP

#include <QDateTime>
#include <QQueue>
#include <QString>

namespace MultiResponse
{
enum Selection { QueueOrder, RecentCallers };

inline bool keepActiveCaller(bool manualSelection, bool hasActiveCaller, bool fromActiveCaller)
{
  // Recency applies only to waiting callers, never to the QSO already underway.
  return hasActiveCaller && !manualSelection && !fromActiveCaller;
}

inline bool enabled(bool requested, QString const& mode, bool autoPota,
                    bool autoCq, bool autoCall, bool autoHunt)
{
  return requested && mode == "FT8" && autoPota && !autoCq && !autoCall && !autoHunt;
}

struct Caller
{
  QString call;
  QString grid;
  QString report;
  int freq = 0;
  bool txFirst = false;
  QDateTime heardAt;
  qint64 lastPeriod = -1;
  int heardPeriods = 0;

  void heard(qint64 period)
  {
    // Decoder passes can report the same transmission more than once.
    if (period <= lastPeriod) return;
    heardPeriods = (period == lastPeriod + 2) ? heardPeriods + 1 : 1;
    lastPeriod = period;
  }
};

inline qint64 decodePeriod(int secondsSinceMidnight, QDateTime const& now, qint64 periodMs)
{
  QDateTime decoded(now.date(), QTime(0, 0), Qt::UTC);
  decoded = decoded.addSecs(secondsSinceMidnight);
  // A late decode of the last transmission of the day arrives after midnight.
  if (now.secsTo(decoded) > 43200) decoded = decoded.addDays(-1);
  if (decoded.secsTo(now) > 43200) decoded = decoded.addDays(1);
  return decoded.toMSecsSinceEpoch() / periodMs;
}

inline qint64 receivePeriod(QDateTime const& now, qint64 periodMs, bool txFirst)
{
  qint64 period = now.toMSecsSinceEpoch() / periodMs;
  if ((period % 2 == 0) == txFirst) --period;
  return period;
}

inline QQueue<Caller> discardStaleCallers(QQueue<Caller>& callers, QDateTime const& now,
                                         qint64 periodMs, bool txFirst)
{
  qint64 const latestReceive = receivePeriod(now, periodMs, txFirst);
  QQueue<Caller> discarded;
  for (int i = callers.size() - 1; i >= 0; --i) {
    if (callers.at(i).lastPeriod != latestReceive || callers.at(i).txFirst != txFirst) {
      discarded.enqueue(callers.takeAt(i));
    }
  }
  return discarded;
}

inline int nextCaller(QQueue<Caller> const& callers, Selection selection,
                      QDateTime const& now, qint64 periodMs, bool txFirst)
{
  if (selection == QueueOrder) return callers.isEmpty() ? -1 : 0;

  qint64 const latestReceive = receivePeriod(now, periodMs, txFirst);
  int best = -1;
  for (int i = 0; i < callers.size(); ++i) {
    auto const& caller = callers.at(i);
    if (caller.txFirst != txFirst || caller.lastPeriod != latestReceive) continue;
    if (best < 0 || caller.heardPeriods > callers.at(best).heardPeriods) best = i;
  }
  return best;
}
}

#endif
