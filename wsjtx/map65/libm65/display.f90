module display_mod
  implicit none
contains

subroutine display(nkeep, ftol)
  use stdout_channel_mod, only: write_stdout
  implicit none

  ! Arguments
  integer, intent(in) :: nkeep
  real,    intent(in) :: ftol

  ! Parameters
  integer, parameter :: MAXLINES = 400
  integer, parameter :: MX       = 400
  integer, parameter :: MAXCALLS = 500

  ! Locals
  integer         :: indx(MAXLINES), indx2(MX)
  character*83    :: line(MAXLINES), line2(MX), line3(MAXLINES)
  character*63    :: out, out0
  character*3     :: cfreq0
  character*6     :: callsign, callsign0
  character*12    :: freqcall(MAXCALLS)

  real            :: freqkHz(MAXLINES)
  integer         :: utc(MAXLINES), utc2(MX), utcz
  real*8          :: f0

  character(len=83)  :: livecq2, livecq3
  character(len=128) :: linenew
  character(len=64)  :: linenew2

  integer :: io_status
  integer :: ndf, nh, nm
  integer :: i, j, j0, k, k3, kz
  integer :: nz, nage, iage, nquad
  integer :: i0, i1, i2, len, nc, m, nstart

  ! Initialize some scalars/arrays defensively
  out0      = ' '
  cfreq0    = ' '
  callsign0 = ' '
  freqcall  = '            '

  !------------------ Read and filter valid lines ---------------------
  rewind(26)
  nz = 0

  do i = 1, MAXLINES
     read(26,1010,end=10) line(i)
1010 format(a83)

     read(line(i),1020,iostat=io_status) f0, ndf, nh, nm
1020 format(f8.3,i5,25x,i3,i2)

     if (io_status /= 0) cycle

     nz = nz + 1
     utc(nz)     = 60*nh + nm
     freqkHz(nz) = 1000.d0*(f0 - 144.d0) + 0.001d0*ndf
  enddo

10 continue
  backspace(26)
  if (nz < 1) return

  utcz = utc(nz)

  nquad = max(nkeep/4, 3)

  do i = 1, nz
     nage = utcz - utc(i)
     if (nage < 0) nage = nage + 1440
     iage = nage / nquad
     write(line(i)(79:80),1021) iage
1021 format(i2)
  enddo

  nage = utcz - utc(1)
  if (nage < 0) nage = nage + 1440

  if (nage > nkeep) then
     do i = 1, nz
        nage = utcz - utc(i)
        if (nage < 0) nage = nage + 1440
        if (nage <= nkeep) exit
     enddo
     i0 = i
     nz = nz - i0 + 1
     rewind(26)
     if (nz < 1) return
     do i = 1, nz
        j = i + i0 - 1
        line(i)    = line(j)
        utc(i)     = utc(j)
        freqkHz(i) = freqkHz(j)
        write(26,1022) line(i)
1022    format(a83)
     enddo
  endif

  call flush(26)
  call indexx(freqkHz, nz, indx)

  nstart = 1
  k3     = 0
  k      = 1
  m      = indx(1)

  if (m < 1 .or. m > nz) then
     write(linenew,'(A,1X,I0,1X,I0)') 'Error in display.f90:', nz, m
     call write_stdout(trim(linenew)//new_line('a'))
     m = 1
  endif

  line2(1) = line(m)
  utc2(1)  = utc(m)

  do i = 2, nz
     j0 = indx(i-1)
     j  = indx(i)

     if (freqkHz(j) - freqkHz(j0) > 2.0*ftol) then
        if (nstart == 0) then
           k = k + 1
           line2(k) = ""
           utc2(k)  = -1
        endif

        kz = k
        call indexx(float(utc2(1:kz)), kz, indx2)

        do k = 1, kz
           k3 = min(k3+1, MAXLINES)
           line3(k3) = line2(indx2(k))
        enddo

        nstart = 0
        k      = 0
     endif

     if (i == nz) then
        k = k + 1
        line2(k) = ""
        utc2(k)  = -1
     endif

     k = k + 1
     line2(k) = line(j)
     utc2(k)  = utc(j)
  enddo

  kz = k
  call indexx(float(utc2(1:kz)), kz, indx2)

  do k = 1, kz
     k3 = min(k3+1, MAXLINES)
     line3(k3) = line2(indx2(k))
  enddo

  rewind 19
  rewind 20
  nc = 0

  do k = 1, k3
     out = line3(k)(1:13)//line3(k)(28:31)//line3(k)(39:45)// &
           line3(k)(35:38)//line3(k)(46:80)

     livecq2 = line3(k)

     if (out(6:8) /= '   ') then
        cfreq0 = out(6:8)

        if (out(19:22) /= out0(19:22) .or. out(31:55) /= out0(31:55) .or. &
            out(6:8) /= out0(6:8)) then

           livecq3 = out(1:61)//' '//livecq2(23:27)//' '//livecq2(79:83)

           write(linenew,'("@",A)') trim(livecq3)
           call write_stdout(trim(linenew)//new_line('a'))

           out0 = out
        endif

        i1 = index(out(31:), ' ')
        callsign = out(i1+31:)
        i2 = index(callsign, ' ')
        if (i2 > 1) callsign(i2:) = '      '

        if (callsign /= '      ' .and. callsign /= callsign0) then
           len = i2 - 1
           if (len < 0) len = 6
           if (len >= 3) then
              if (nc < MAXCALLS) then
                 nc = nc + 1
                 freqcall(nc) = cfreq0//' '//callsign//line3(k)(79:80)
                 callsign0    = callsign
              endif
           endif
        endif

        if (callsign /= '      ' .and. callsign == callsign0) then
           if (nc > 0 .and. nc <= MAXCALLS) then
              freqcall(nc) = cfreq0//' '//callsign//line3(k)(79:80)
           endif
        endif
     endif
  enddo

  ! Safely append terminators without overrunning MAXCALLS
  if (nc < MAXCALLS) then
     nc = nc + 1
     freqcall(nc) = '            '
  endif
  if (nc < MAXCALLS) then
     nc = nc + 1
     freqcall(nc) = '            '
  endif
  if (nc < MAXCALLS) then
     nc = nc + 1
     freqcall(nc) = '            '
  endif
  if (nc < MAXCALLS) then
     nc = nc + 1
     freqcall(nc) = '            '
  endif

  do i = 1, nc
     write(linenew2,'("&",A)') trim(freqcall(i))
     call write_stdout(trim(linenew2)//new_line('a'))
  enddo

end subroutine display

end module display_mod
