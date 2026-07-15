  integer :: level, onlevel(0:10)
  common/timer_private/ level, onlevel
#ifndef WSJT_NO_TIMER_THREADPRIVATE
  !$omp threadprivate(/timer_private/)
#endif
