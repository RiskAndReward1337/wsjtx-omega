#ifndef LOG_QSO_STATISTICS_HPP
#define LOG_QSO_STATISTICS_HPP

#include <QSet>
#include <QString>

class LogQsoStatistics
{
public:
  void add(QString call, QString band, QString mode)
  {
    call = call.trimmed().toUpper();
    band = band.trimmed().toUpper();
    mode = mode.trimmed().toUpper();
    if (call.isEmpty()) return;
    ++total_;
    // Count incomplete records in the total without guessing duplicate keys.
    if (band.isEmpty() || mode.isEmpty()) return;
    QString const key = call + '\t' + band + '\t' + mode;
    if (keys_.contains(key)) ++duplicates_;
    else keys_.insert(key);
  }

  int total() const { return total_; }
  int duplicates() const { return duplicates_; }

private:
  QSet<QString> keys_;
  int total_ = 0;
  int duplicates_ = 0;
};

#endif
