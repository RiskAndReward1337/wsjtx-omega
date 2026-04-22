module rs_mod
  use iso_c_binding
  implicit none
  interface
    subroutine rs_encode(dgen, sent) bind(C, name='rs_encode_')
      import :: c_int
      integer(c_int), intent(in) :: dgen(12)
      integer(c_int), intent(out) :: sent(63)
    end subroutine rs_encode

    subroutine rs_decode(recd0, era0, numera0, decoded, nerr) bind(C, name='rs_decode_')
      import :: c_int
      integer(c_int), intent(in) :: recd0(63)
      integer(c_int), intent(in) :: era0(*)
      integer(c_int), intent(in) :: numera0
      integer(c_int), intent(out) :: decoded(12)
      integer(c_int), intent(out) :: nerr
    end subroutine rs_decode
  end interface
end module rs_mod
