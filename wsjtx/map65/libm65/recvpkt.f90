module recvpkt_mod
  use datcom_ptrs_mod          ! for dd
  implicit none
contains

  subroutine recvpkt(nsam, nblock2, userx_no, k, buf4, buf8, buf16) bind(C, name="recvpkt_")
    implicit none

    ! ===== Arguments (with safe intents) =====
    integer,        intent(in)    :: nsam
    integer*2,      intent(inout) :: nblock2     ! legacy quirk: may be modified to silence warning
    integer*1,      intent(in)    :: userx_no
    integer,        intent(inout) :: k
    real*4,         intent(in)    :: buf4(*)
    real*8,         intent(in)    :: buf8(*)
    complex*16,     intent(in)    :: buf16(*)

    ! ===== Locals =====
    integer :: i
    integer, parameter :: NSMAX = 60*96000

    ! Silence unused warning
    if (nblock2 .eq. -9999) nblock2 = -9998

    if (nsam .eq. -1) then
       ! Move data from UDP/PortAudio buffer into dd()

       select case (userx_no)

       case (-1)
          do i = 1, 174
             k = k + 1
             call unpack_r8_to_r4(buf8(i), dd(1,k), dd(2,k))
          end do

       case (1)
          do i = 1, 348
             k = k + 1
             call unpack_r4_to_i2_as_r4(buf4(i), dd(1,k), dd(2,k))
          end do

       case (-2)
          do i = 1, 87
             k = k + 1
             call unpack_c16_to_r4(buf16(i), dd(1,k), dd(2,k), dd(3,k), dd(4,k))
          end do

       case (2)
          do i = 1, 174
             k = k + 1
             call unpack_r8_to_i2_as_r4(buf8(i), dd(1,k), dd(2,k), dd(3,k), dd(4,k))
          end do

       end select

    else
       ! nsam >= 0: special case for one RF channel, r*4 data
       if (userx_no .eq. 1) then
          do i = 1, nsam
             k = k + 1
             call unpack_r4_to_i2_as_r4(buf4(i), dd(1,k), dd(2,k))

             k = k + 1
             dd(1,k) = dd(1,k-1)
             dd(2,k) = dd(2,k-1)
          end do
       end if
    end if

  end subroutine recvpkt


  ! ===== Helper routines (unchanged ABI, now module-scoped) =====

  subroutine unpack_r8_to_r4(x, a, b)
    real*8,  intent(in)  :: x
    real*4,  intent(out) :: a, b
    real*4               :: tmp(2)
    tmp = transfer(x, tmp)
    a = tmp(1)
    b = tmp(2)
  end subroutine unpack_r8_to_r4

  subroutine unpack_r4_to_i2_as_r4(x, a, b)
    real*4,    intent(in)  :: x
    real*4,    intent(out) :: a, b
    integer*2              :: tmp(2)
    tmp = transfer(x, tmp)
    a = real(tmp(1))
    b = real(tmp(2))
  end subroutine unpack_r4_to_i2_as_r4

  subroutine unpack_c16_to_r4(x, a1, a2, a3, a4)
    complex*16, intent(in)  :: x
    real*4,     intent(out) :: a1, a2, a3, a4
    real*4                  :: tmp(4)
    tmp = transfer(x, tmp)
    a1 = tmp(1)
    a2 = tmp(2)
    a3 = tmp(3)
    a4 = tmp(4)
  end subroutine unpack_c16_to_r4

  subroutine unpack_r8_to_i2_as_r4(x, a1, a2, a3, a4)
    real*8,    intent(in)  :: x
    real*4,    intent(out) :: a1, a2, a3, a4
    integer*2              :: tmp(4)
    tmp = transfer(x, tmp)
    a1 = real(tmp(1))
    a2 = real(tmp(2))
    a3 = real(tmp(3))
    a4 = real(tmp(4))
  end subroutine unpack_r8_to_i2_as_r4

end module recvpkt_mod
