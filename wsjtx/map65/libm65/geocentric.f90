module geocentric_mod
  implicit none
contains
  subroutine geocentric(alat,elev,hlt,erad)
    implicit none
    real*8, intent(in)  :: alat, elev
    real*8, intent(out) :: hlt, erad
  
    real*8 :: f, a_radius, c, arcf, arsf
  
  ! IAU 1976 flattening f, equatorial radius a
    f = 1.d0/298.257d0
    a_radius = 6378140.d0
    c = 1.d0/sqrt(1.d0 + (-2.d0 + f)*f*sin(alat)*sin(alat))
    arcf = (a_radius*c + elev)*cos(alat)
    arsf = (a_radius*(1.d0 - f)*(1.d0 - f)*c + elev)*sin(alat)
    hlt = atan2(arsf,arcf)
    erad = sqrt(arcf*arcf + arsf*arsf)
    erad = 0.001d0*erad
  
  end subroutine geocentric
end module geocentric_mod

