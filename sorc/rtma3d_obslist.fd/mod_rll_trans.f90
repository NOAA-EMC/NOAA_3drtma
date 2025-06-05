module mod_rll_trans

! use pkind, only : spi, dpi, sp, dp

  implicit none
  private
  public rll_trans, rll2xy
  public rll_grid

  interface rll_trans; module procedure rll_trans_s, rll_trans_v; end interface
  interface rll2xy;    module procedure rll2xy_s,    rll2xy_v;    end interface

  type :: rotatedlatlon_grid
       real(8)  :: sp_lon               ! earth longitude of south pole (unit: deg)
       real(8)  :: sp_lat               ! earth latitude  of south pole (unit: deg)
       real(8)  :: origin_rlon          ! rotated longitude of origin point of x-y coordinates (unit: deg)
       real(8)  :: origin_rlat          ! rotated longitude of origin point of x-y coordinates (unit: deg)
       real(8)  :: dlon                 ! grid spacing along x-direction/rotated-longitude (deg)
       real(8)  :: dlat                 ! grid spacing along y-direction/rotated-latitude  (deg)
       real(8)  :: offset_x             ! x-coordiante of origin point (default 0)
       real(8)  :: offset_y             ! y-coordiante of origin point (default 0)
       real(8)  :: llcnr(2)             ! rotated lon/lat of lower-left  corner of domain (deg)
       real(8)  :: urcnr(2)             ! rotated lon/lat of upper-right corner of domain (deg)
  end type rotatedlatlon_grid

  type(rotatedlatlon_grid)  :: rll_grid

!---- parameters
  real(8) , parameter :: pi = dacos(-1.0_8)
  real(8) , parameter :: zero = 0.0_8
  real(8) , parameter :: r90 = 90.0_8
  real(8) , parameter :: r180 = 180.0_8
  real(8) , parameter :: r360 = 360.0_8

contains

!=============================================================================!

  subroutine rll_trans_s(lon_in, lat_in,           opt, lon_out, lat_out)
! for input lon_in/lat_in are variables (as scalars), not arrays
! this is used for transforming the lat/lon of single point

  implicit none

  real(8),    intent( in) :: lon_in        ! longitude of input (unit: deg)
  real(8),    intent( in) :: lat_in        ! latitude  of input (unit: deg)
  integer, intent( in) :: opt           ! option to control the directon of transform
                                                               ! = 1 : regular lat/lon to rotated lat/lon
                                                               ! =-1 : rotated lat/lon to regular lat/lon

  real(8),    intent(out) :: lon_out       ! longitude of output (unit: deg)
  real(8),    intent(out) :: lat_out       ! latitude  of output (unit: deg)

!---- local variables
  real(8) :: theta                ! rotation around y-axis
  real(8) :: phi                  ! rotation around x-axis
  real(8) :: lon_in_rad           ! input longitude in radiance
  real(8) :: lat_in_rad           ! input latitude  in radiance
  real(8) :: x, y, z              ! cartesian coordinates (original)
  real(8) :: x_n, y_n, z_n        ! cartesian coordinates (transformed)
!-----------------------------------------------------------------------------!

! write(6,*) ' (sub rll_trans_s) pi = ', pi
! lon_in_rad = -999.0 ; lat_in_rad = -999.0
! x = -999.0 ; y = -999.0 ; z = -999.0

  lat_in_rad = lat_in
  lon_in_rad = lon_in
! reset longitude be in range of [0, 360.0)
  if (lon_in_rad >= r360) lon_in_rad = lon_in_rad - r360
  if (lon_in_rad <  zero) lon_in_rad = lon_in_rad + r360

  lon_in_rad = lon_in_rad * pi / r180           ! deg --> rad
  lat_in_rad = lat_in_rad * pi / r180           ! deg --> rad

  theta = rll_grid%sp_lat + r90                  ! rotation around y-axis
  phi   = rll_grid%sp_lon                        ! rotation around z-axis
  theta = theta * pi / r180           ! deg --> rad
  phi   = phi   * pi / r180           ! deg --> rad

  x = cos(lon_in_rad) * cos(lat_in_rad)
  y = sin(lon_in_rad) * cos(lat_in_rad)
  z = sin(lat_in_rad)

  if ( opt == 1 ) then                      ! 1: regular --> rotated
     x_n =  cos(theta) * cos(phi) * x + cos(theta) * sin(phi) * y + sin(theta) * z
     y_n =              -sin(phi) * x +              cos(phi) * y
     z_n = -sin(theta) * cos(phi) * x - sin(theta) * sin(phi) * y + cos(theta) * z
  else if ( opt == -1 ) then                !-1: rotated --> regular
     phi   = -phi
     theta = -theta
     x_n =  cos(theta) * cos(phi) * x +              sin(phi) * y + sin(theta) * cos(phi) * z
     y_n = -cos(theta) * sin(phi) * x +              cos(phi) * y - sin(theta) * sin(phi) * z
     z_n = -sin(theta) *            x                             + cos(theta) *            z
  else
     write(6,*) ' unrecognized option for opt, which must be either for regular to rotatedi (1) or vice versa (-1) '
     stop 999
  end if

  lon_out = atan2(y_n, x_n)
  lat_out = asin(z_n)

  lon_out = lon_out *  r180 / pi     ! radiance --> degreee
  lat_out = lat_out *  r180 / pi     ! radiance --> degreee

!-----------------------------------------------------------------------------!
  return

  end subroutine rll_trans_s

!=============================================================================!

  subroutine rll_trans_v(lon_in, lat_in,                 opt, lon_out, lat_out)
! for input lon_in/lat_in are arrays (as vectors)
! this is used for transforming the lats/lons of multiple points.

  implicit none

  real,    dimension(:),  intent( in) :: lon_in        ! longitude of input (unit: deg)
  real,    dimension(:),  intent( in) :: lat_in        ! latitude  of input (unit: deg)
  integer,                intent( in) :: opt           ! option to control the directon of transform
                                                               ! = 1 : regular lat/lon to rotated lat/lon
                                                               ! =-1 : rotated lat/lon to regular lat/lon

  real,    dimension(:),  intent(out) :: lon_out       ! longitude of output (unit: deg)
  real,    dimension(:),  intent(out) :: lat_out       ! latitude  of output (unit: deg)

!---- local variables
  real(8) :: theta                                           ! rotation around y-axis
  real(8) :: phi                                             ! rotation around x-axis
  real(8), allocatable, dimension(:) :: lon_in_rad           ! input longitude in radiance
  real(8), allocatable, dimension(:) :: lat_in_rad           ! input latitude  in radiance
  real(8), allocatable, dimension(:) :: x, y, z              ! cartesian coordinates (original)
  real(8), allocatable, dimension(:) :: x_n, y_n, z_n        ! cartesian coordinates (transformed)
  integer  :: N
!-----------------------------------------------------------------------------!

! write(6,*) ' (sub rll_trans_v) pi = ', pi
  N =size(lon_in)
  write(6,*) ' length of input array longitude N = ',N
  allocate(lon_in_rad(1:N), lat_in_rad(1:N))
  allocate(x(1:N), y(1:N), z(1:N))
  allocate(x_n(1:N), y_n(1:N), z_n(1:N))
! lon_in_rad = -999.0 ; lat_in_rad = -999.0
! x = -999.0 ; y = -999.0 ; z = -999.0

  lat_in_rad(:) = lat_in(:)
  lon_in_rad(:) = lon_in(:)
! reset longitude be in range of [0, 360.0)
  where(lon_in_rad >= r360)  lon_in_rad = lon_in_rad - r360
  where(lon_in_rad <   zero) lon_in_rad = lon_in_rad + r360

  lon_in_rad(:) = lon_in_rad(:) * pi / r180           ! deg --> rad
  lat_in_rad(:) = lat_in_rad(:) * pi / r180           ! deg --> rad
  
  theta = rll_grid%sp_lat + r90                        ! rotation around y-axis
  phi   = rll_grid%sp_lon                              ! rotation around z-axis
  theta = theta * pi / r180           ! deg --> rad
  phi   = phi   * pi / r180           ! deg --> rad

  x(:) = cos(lon_in_rad(:)) * cos(lat_in_rad(:))
  y(:) = sin(lon_in_rad(:)) * cos(lat_in_rad(:))
  z(:) = sin(lat_in_rad(:))

  if ( opt == 1 ) then                      ! 1: regular --> rotated
     x_n(:) =  cos(theta) * cos(phi) * x(:) + cos(theta) * sin(phi) * y(:) + sin(theta) * z(:)
     y_n(:) =              -sin(phi) * x(:) +              cos(phi) * y(:)
     z_n(:) = -sin(theta) * cos(phi) * x(:) - sin(theta) * sin(phi) * y(:) + cos(theta) * z(:)
  else if ( opt == -1 ) then                !-1: rotated --> regular
     phi   = -phi
     theta = -theta
     x_n(:) =  cos(theta) * cos(phi) * x(:) +              sin(phi) * y(:) + sin(theta) * cos(phi) * z(:)
     y_n(:) = -cos(theta) * sin(phi) * x(:) +              cos(phi) * y(:) - sin(theta) * sin(phi) * z(:)
     z_n(:) = -sin(theta) *            x(:)                                + cos(theta) *            z(:)
  else
     write(6,*) ' unrecognized option for opt, which must be either for regular to rotatedi (1) or vice versa (-1) '
     stop 999
  end if

  lon_out(:) = atan2(y_n(:), x_n(:))
  lat_out(:) = asin(z_n(:))

  lon_out(:) = lon_out(:) *  r180 / pi     ! radiance --> degreee
  lat_out(:) = lat_out(:) *  r180 / pi     ! radiance --> degreee

  deallocate(lon_in_rad, lat_in_rad)
  deallocate(x,y,z)
  deallocate(x_n,y_n,z_n)

!-----------------------------------------------------------------------------!
  return

  end subroutine rll_trans_v

!=============================================================================!

  subroutine rll2xy_s(rlon, rlat,           x, y)
!-----------------------------------------------------------------------------!
! for input rlon/rlatn are variables (as scalars)
! this is used for transforming the lats/lons of single point.

  implicit none

  real(8),    intent( in) :: rlon        ! rotated longitude of input (unit: deg)
  real(8),    intent( in) :: rlat        ! rotated latitude  of input (unit: deg)

  real(8),    intent(out) :: x           ! x coordiante
  real(8),    intent(out) :: y           ! y coordinate

!---- local variables
  real(8) :: rlon1, rlat1
  real(8) :: olon        ! longitude of origin point of x-y coordinates (unit: deg)
  real(8) :: olat        ! latitude  of origin point of x-y coordinates (unit: deg)
  real(8) :: llcnr_rlon, llcnr_rlat
  real(8) :: urcnr_rlon, urcnr_rlat
!-----------------------------------------------------------------------------!
! write(6,*) '(running sub rll2xy_s ...)'

  olat = rll_grid%origin_rlat
  olon = rll_grid%origin_rlon
  llcnr_rlon = rll_grid%llcnr(1)
  llcnr_rlat = rll_grid%llcnr(2)
  urcnr_rlon = rll_grid%urcnr(1)
  urcnr_rlat = rll_grid%urcnr(2)

! reset longitude to be in the range of (-180.0, 180.0)
  if ( olon >=r360 ) then
     olon = olon - r360
  else if ( olon > r180 .and. olon < r360 ) then
     olon = olon - r360
  end if
  if ( llcnr_rlon >=r360 ) then
     llcnr_rlon = llcnr_rlon - r360
  else if ( llcnr_rlon > r180 .and. llcnr_rlon < r360 ) then
     llcnr_rlon = llcnr_rlon - r360
  end if
  if ( urcnr_rlon >=r360 ) then
     urcnr_rlon = urcnr_rlon - r360
  else if ( urcnr_rlon > r180 .and. urcnr_rlon < r360 ) then
     urcnr_rlon = urcnr_rlon - r360
  end if

  rlat1 = rlat
  rlon1 = rlon
! reset longitude to be in the range of (-180.0, 180.0)
  if (rlon1 >=r360 ) then
     rlon1 = rlon1 - r360
  else if (rlon1 > r180 .and. rlon1 < r360 ) then
     rlon1 = rlon1 - r360
  end if

  y = rll_grid%offset_y + (rlat1 - olat) / rll_grid%dlat
  x = rll_grid%offset_x + (rlon1 - olon) / rll_grid%dlon

! if the point is outside of domain
  if (    rlat1 < llcnr_rlat .or. rlat1 > urcnr_rlat          &
     .or. rlon1 < llcnr_rlon .or. rlon1 > urcnr_rlon ) then
     x = -9.9999999E+7_8
     y = -9.9999999E+7_8
  end if

!-----------------------------------------------------------------------------!
  return

  end subroutine rll2xy_s

!=============================================================================!

  subroutine rll2xy_v(rlon, rlat,           x, y)
!-----------------------------------------------------------------------------!
! for input rlon/rlatn are arrays (as vectors)
! this is used for transforming the lats/lons of multiple points.

  implicit none

  real(8),    dimension(:),  intent( in) :: rlon        ! rotated longitude of input (unit: deg)
  real(8),    dimension(:),  intent( in) :: rlat        ! rotated latitude  of input (unit: deg)

  real(8),    dimension(:),  intent(out) :: x           ! x coordiante
  real(8),    dimension(:),  intent(out) :: y           ! y coordinate

!---- local variables
  real(8), allocatable, dimension(:) :: rlon1, rlat1
  real(8) :: olon        ! longitude of origin point of x-y coordinates (unit: deg)
  real(8) :: olat        ! latitude  of origin point of x-y coordinates (unit: deg)
  real(8) :: llcnr_rlon, llcnr_rlat
  real(8) :: urcnr_rlon, urcnr_rlat
  integer  :: N
!-----------------------------------------------------------------------------!
  write(6,*) '(running sub rll2xy_v ...)'

  olat = rll_grid%origin_rlat
  olon = rll_grid%origin_rlon
  llcnr_rlon = rll_grid%llcnr(1)
  llcnr_rlat = rll_grid%llcnr(2)
  urcnr_rlon = rll_grid%urcnr(1)
  urcnr_rlat = rll_grid%urcnr(2)

! reset longitude to be in the range of (-180.0, 180.0)
  if ( olon >=r360 ) then
     olon = olon - r360
  else if ( olon > r180 .and. olon < r360 ) then
     olon = olon - r360
  end if
  if ( llcnr_rlon >=r360 ) then
     llcnr_rlon = llcnr_rlon - r360
  else if ( llcnr_rlon > r180 .and. llcnr_rlon < r360 ) then
     llcnr_rlon = llcnr_rlon - r360
  end if
  if ( urcnr_rlon >=r360 ) then
     urcnr_rlon = urcnr_rlon - r360
  else if ( urcnr_rlon > r180 .and. urcnr_rlon < r360 ) then
     urcnr_rlon = urcnr_rlon - r360
  end if

  N =size(rlon)
  allocate(rlon1(1:N), rlat1(1:N))
  rlat1(:) = rlat(:)
  rlon1(:) = rlon(:)
! reset longitude to be in the range of (-180.0, 180.0)
  where (rlon >=r360 ) 
     rlon1 = rlon1 - r360
  else where (rlon > r180 .and. rlon < r360 )
     rlon1 = rlon1 - r360
  end where

  y(:) = rll_grid%offset_y + (rlat1(:) - olat) / rll_grid%dlat
  x(:) = rll_grid%offset_x + (rlon1(:) - olon) / rll_grid%dlon

! if the point is outside of domain
  where (    rlat1 < llcnr_rlat .or. rlat1 > urcnr_rlat          &
        .or. rlon1 < llcnr_rlon .or. rlon1 > urcnr_rlon )
     x = -9.9999999E+7_8
     y = -9.9999999E+7_8
  end where

!-----------------------------------------------------------------------------!
  return

  end subroutine rll2xy_v

!=============================================================================!

end module mod_rll_trans
