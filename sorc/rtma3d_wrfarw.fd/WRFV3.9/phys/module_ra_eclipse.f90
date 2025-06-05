module module_ra_eclipse
contains
subroutine solar_eclipse(ims,ime,jms,jme,its,ite,jts,jte, &
                          julian,gmt,year,xtime,radt, &
                          degrad,xlon,xlat,Obscur,mask, &
                          elat_track,elon_track,sw_eclipse )
  implicit none
  integer, intent(in) :: ims,ime,jms,jme, &
                                                        its,ite,jts,jte
  real, intent(in) :: gmt,degrad,julian,xtime,radt
  integer, intent(in) :: year
  real, dimension(ims:ime,jms:jme), intent(in) :: xlat,xlon
  real, dimension(ims:ime,jms:jme), intent(inout) :: Obscur
  integer, dimension(ims:ime,jms:jme), intent(inout) :: mask
  real, intent(inout) :: elat_track,elon_track
  integer, intent(in) :: sw_eclipse
  real, parameter :: pi = 3.1415926535897932384626433
  real, parameter :: epsil = 0.0818192
  integer, parameter :: NBessel = 4
  real, dimension(NBessel) :: X, Y, d, l1, l2, mu
  real :: tanf1, tanf2, t0, tmin, tmax, Dt
  real :: rmu, rd
  real :: cx, cy, cd, cl1, cl2, cmu
  real :: t1, &
                           H
  real :: rlat, rlon
  real :: cos_lat1, sin_lat1
  real :: xi, eta1, zeta1
  real :: DELTA, DELTA2
  real :: LL1, LL2
  logical :: eclipse_exist
  integer :: i, j
  real :: A, B
  real :: da,eot,xt24
  integer :: julday
  real :: E, E_1, tan_rd1, rd1, rho1, cy1, tan_gamma, cgamma, sin_beta, &
          tan_C, C, cm, tan_lat1, tan_lat, sin_H, &
          cos_H, tan_H, beta, lon, tan_rd2, rd2, rho2
  real :: MISSING
  MISSING = 1E30
  if(sw_eclipse .eq. 0) then
     Obscur(:,:) = 0.
     mask(:,:) = 0
     elat_track = MISSING
     elon_track = MISSING
     return
  endif
  xt24 = mod(xtime+radt,1440.)
  t1 = gmt+xt24/60
  julday = int(julian)+1
  if(t1 .ge. 24.) then
 t1 = t1 - 24.
  endif
  eclipse_exist = .false.
  call load_besselian_elements(t1, julday, year, X, Y, d, l1, l2, mu, &
                               tanf1, tanf2, t0, tmin, tmax, NBessel, &
                               eclipse_exist,Dt)
  if(.not. eclipse_exist) then
     Obscur(:,:) = 0.
     mask(:,:) = 0
     elat_track = MISSING
     elon_track = MISSING
     return
  endif
  call compute_besselian_t(t1, NBessel, X, Y, d, l1, l2, mu, t0, Dt, &
                               cx, cy, cd, cl1, cl2, cmu)
  rmu = cmu * degrad
  rd = cd * degrad
  elat_track=MISSING
  elon_track=MISSING
  E = sqrt(1 - epsil**2)
  E_1 = 1/E
  tan_rd1 = E_1 * tan(rd)
  rd1 = atan(tan_rd1)
  rho1 = sin(rd)/sin(rd1)
  cy1 = cy / rho1
  tan_rd2 = E * tan(rd)
  rd2 = atan(tan_rd2)
  rho2 = cos(rd)/cos(rd2)
  tan_gamma = cx / cy1
  cgamma = atan(tan_gamma)
  sin_beta = cx/sin(cgamma)
  if(abs(sin_beta) .lt. 1) then
 beta = asin(sin_beta)
 tan_C = cy1 / cos(beta)
 C = atan(tan_C)
 cm = cy1 / sin(C)
        sin_lat1 = cm * sin(C + rd1)
        cos_lat1 = sqrt(1 - sin_lat1**2)
        tan_lat1 = sin_lat1/cos_lat1
        tan_lat = E_1 * tan_lat1
        elat_track= atan(tan_lat)/degrad
        sin_H = cx / cos_lat1
        cos_H = cm * cos(C+rd1)/cos_lat1
        tan_H = sin_H/cos_H
        H = atan(tan_H)
 if(cos_H .lt. 0) then
  if(sin_H .ge. 0) then
   H = pi - abs(H)
  else
   H = pi + abs(H)
  end if
 end if
        elon_track= (rmu - H)/degrad
        if(elon_track .gt. 360) then
                elon_track = elon_track -360
        endif
        if(elon_track .gt. 180) then
                elon_track = elon_track -360
        endif
        if(elon_track .le. -180) then
                elon_track = elon_track +360
        endif
        elon_track = -elon_track + 1.002738 * 15*Dt/3600
  endif
  do j=jts,jte
     do i=its,ite
 lon = -xlon(i,j) - 1.002738 * 15*Dt/3600
   if(lon .lt. 0) then
           lon = lon + 360
        endif
        if(lon .gt. 360) then
            lon = lon -360
        endif
 rlat = xlat(i,j)*degrad
        rlon = lon*degrad
        H = rmu - rlon
 A = sqrt( 1 - ( epsil * sin(rlat) )**2 )
        cos_lat1 = cos(rlat) / A
        sin_lat1 = sin(rlat) * E / A
 xi = cos_lat1 * sin(H)
 eta1 = sin_lat1 * cos(rd1) * E - cos_lat1 * sin(rd1) * cos(H)
 zeta1 = sin_lat1 * sin(rd2) * E + cos_lat1 * cos(rd2) * cos(H)
 cy1 = cy/rho1
        DELTA2 = (xi - cx)**2 + (eta1 - cy1)**2;
 DELTA = sqrt(DELTA2)
 LL1 = (cl1 - zeta1 * tanf1)
 LL2 = (cl2 - zeta1 * tanf2)
        if(DELTA .gt. LL1) then
              Obscur(i,j) = 0.
              mask(i,j) = 0
        elseif(DELTA .le. LL1 .and. DELTA .gt. abs(LL2)) then
              Obscur(i,j) = (LL1 - DELTA) / (LL1 + LL2)
              mask(i,j) = 1
        elseif(DELTA .le. abs(LL2)) then
  if(LL2 .lt. 0) then
                      Obscur(i,j) = 1.
               mask(i,j) = 2
                else
                      Obscur(i,j) = (LL1 - DELTA) / (LL1 + LL2)
               mask(i,j) = 3
                endif
        endif
        if(Obscur(i,j) .gt. 1) then
              Obscur(i,j) = 1.
        endif
     enddo
  enddo
end subroutine
subroutine load_besselian_elements(t, julday, year, X, Y, d, l1, l2, mu, &
                                   tanf1, tanf2, t0, tmin, tmax, NBessel, &
                                   eclipse_exist,Dt)
  IMPLICIT NONE
  integer, intent(in) :: julday, year, NBessel
  real, intent(in) :: t
  logical, intent(inout) :: eclipse_exist
  real, dimension(NBessel), intent(inout) :: X, Y, d, l1, l2, mu
  real, intent(inout) :: tanf1, tanf2, t0, tmin, tmax, Dt
  integer :: ryear, rjulday
  logical :: file_exist
  eclipse_exist = .false.
  inquire( file="eclipse_besselian_elements.dat", exist=file_exist)
  if (file_exist) then
  open (20,FILE='eclipse_besselian_elements.dat',status="old",action="read")
  else
 call wrf_error_fatal3("<stdin>",348,&
'load_besselian_elements: eclipse_besselian_elements.dat not found')
 return
  end if
  do
     read(20,*,end=1)ryear,rjulday,t0,tmin,tmax, &
                         X(1), X(2), X(3), X(4), &
                         Y(1), Y(2), Y(3), Y(4), &
                         d(1), d(2), d(3), d(4), &
                        l1(1),l1(2),l1(3),l1(4), &
                        l2(1),l2(2),l2(3),l2(4), &
                        mu(1),mu(2),mu(3),mu(4), &
                        tanf1,tanf2,Dt
     if(ryear .eq. year .and. rjulday .eq. julday .and. tmin .le. t .and. t .le. tmax) then
             eclipse_exist = .true.
             close(20)
             return
     else
     endif
  end do
  1 close(20)
  return
end subroutine
subroutine compute_besselian_t(t1, NBessel, X, Y, d, l1, l2, mu, t0, Dt, &
                               cx, cy, cd, cl1, cl2, cmu)
  IMPLICIT NONE
  integer, intent(in) :: NBessel
  real, intent(in) :: t1
  real, dimension(NBessel), intent(in) :: X, Y, d, l1, l2, mu
  real, intent(in) :: t0, Dt
  real, intent(inout) :: cx, cy, cd, cl1, cl2, cmu
  real :: t
  integer :: i
  cx = 0.
  cy = 0.
  cd = 0.
  cl1 = 0.
  cl2 = 0.
  cmu = 0.
  t = (t1 + DT/3600) - t0
  do i=1,NBessel
      cx = cx + X(i) * t**(i-1)
      cy = cy + Y(i) * t**(i-1)
      cd = cd + d(i) * t**(i-1)
      cl1 = cl1 + l1(i) * t**(i-1)
      cl2 = cl2 + l2(i) * t**(i-1)
      cmu = cmu + mu(i) * t**(i-1)
  enddo
end subroutine
end module module_ra_eclipse
