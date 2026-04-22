module ftnquit_mod
  use filbig_mod
  use four2a_mod
  implicit none
contains

subroutine ftnquit
  implicit none

  integer, parameter :: MAXFFT2 = 77175

  real*4        :: dd_dummy(1,1)
  integer       :: nmax_dummy = -1
  real*8        :: f0_dummy = 0.0d0
  integer       :: newdat_dummy = 0
  integer       :: nfsample_dummy = 0
  logical       :: xpol_dummy = .false.
  complex       :: c4a_dummy(MAXFFT2), c4b_dummy(MAXFFT2)
  integer       :: n4_dummy = 0

  complex :: a_dummy(1)

  call four2a(a_dummy, -1, 1, 1, 1)

  call filbig(dd_dummy, nmax_dummy, f0_dummy, newdat_dummy, &
              nfsample_dummy, xpol_dummy, c4a_dummy, c4b_dummy, n4_dummy)

end subroutine ftnquit

end module ftnquit_mod


