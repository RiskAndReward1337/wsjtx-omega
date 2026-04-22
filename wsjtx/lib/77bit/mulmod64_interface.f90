module mulmod64_interface
  use iso_c_binding
  implicit none

  interface
    function mulmod64(a, b) bind(C, name="mulmod64")
      import :: c_int64_t
      implicit none
      integer(c_int64_t), value :: a, b
      integer(c_int64_t) :: mulmod64
    end function mulmod64
  end interface

end module mulmod64_interface
