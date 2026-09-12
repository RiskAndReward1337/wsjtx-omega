#ifndef QSO_DEADLINE_HPP
#define QSO_DEADLINE_HPP

#include <QString>
#include <QtGlobal>

// Uses a monotonic clock supplied by the caller, independent of TX state,
// decoded reports, message selection, and changes to the computer's UTC clock.
class QsoDeadline
{
public:
  enum Unit { Minutes, Cycles };

  bool begin(QString const& identity, qint64 now, qint64 periodMs, qint64 phaseMs = 0)
  {
    if (identity.isEmpty() || identity == identity_) return false;
    identity_ = identity;
    started_ = now;
    cycleMs_ = qMax(qint64(1), 2 * periodMs);
    cycleStarted_ = now - qBound(qint64(0), phaseMs, qMax(qint64(0), periodMs - 1));
    return true;
  }

  void clear() { identity_.clear(); }
  bool active() const { return !identity_.isEmpty(); }
  QString const& identity() const { return identity_; }
  qint64 elapsed(qint64 now, Unit unit = Minutes) const
  {
    return active() ? qMax(qint64(0), now - (unit == Cycles ? cycleStarted_ : started_)) : 0;
  }
  qint64 cycleMs() const { return cycleMs_; }
  qint64 limitMs(int limit, Unit unit) const { return qint64(limit) * (unit == Cycles ? cycleMs_ : 60000); }
  bool expired(qint64 now, int limit, Unit unit) const
  {
    return active() && limit > 0 && elapsed(now, unit) >= limitMs(limit, unit);
  }

private:
  QString identity_;
  qint64 started_ = 0;
  qint64 cycleStarted_ = 0;
  qint64 cycleMs_ = 30000;
};

#endif
