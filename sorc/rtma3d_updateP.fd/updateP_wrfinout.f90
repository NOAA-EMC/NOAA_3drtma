program updateP_wrfinout
!===============================================================================!
  use omp_lib
  use netcdf
! use kinds, only: r_kind, r_single, i_kind

  implicit none

  real(8), parameter       :: kappa = 0.286      ! (Rd/Cp)
  real(4), parameter       :: fill_r4 = -9999.0
  real(8), parameter       :: fill_r8 = -9999.0

  character(len=256)       :: input_file
  character(len=256)       :: outfile_bin
  character(len=32)        :: varname
  character(len=32)        :: dimname

  integer(4)               :: ncid, varid, dimid
  integer(4)               :: ndims, nvars, natts, recdim
  integer(4)               :: nx, ny, nz
  integer(4)               :: nz_stag

  integer(4)               :: i, j, k
  integer(4)               :: i2, j2

!-- variables used to read from /write to netcdf file
  real(4)                  :: dummy
  real(4), allocatable     :: dummy_1d(:), dummy2_1d(:)
  real(4), allocatable     :: dummy_2d(:,:)
  real(4), allocatable     :: dummy_3d(:,:,:)

!-- variables read from netcdf file
  real(8)                  :: ptop
  real(8), allocatable     :: xlat_2d(:,:), xlon_2d(:,:)
  real(8), allocatable     :: c3h_1d(:), c4h_1d(:)
  real(8), allocatable     :: c3f_1d(:), c4f_1d(:)
  real(8), allocatable     :: th2_2d(:,:)
  real(8), allocatable     :: mu_2d(:,:), mub_2d(:,:)
  real(8), allocatable     :: th_3d(:,:,:), qv_3d(:,:,:)
  real(8), allocatable     :: p_3d(:,:,:), pb_3d(:,:,:)
  real(8), allocatable     :: p_hyd_3d(:,:,:), psfc_2d(:,:)

!-- variables used in calculation
  real(8), allocatable     :: q_integral(:,:), q_integralc4h(:,:)
  real(8), allocatable     :: psfc_2d_new_dry(:,:), psfc_2d_new_wet(:,:)
  real(8), allocatable     :: psfc_2d_new_diff(:,:), psfc_2d_new_diffperc(:,:)
  real(8), allocatable     :: p_3d_new_dry(:,:,:), p_3d_new_wet(:,:,:)
  real(8), allocatable     :: p_inc_3d_dry(:,:,:), p_inc_3d_wet(:,:,:)
  real(8), allocatable     :: t_3d(:,:,:)
  real(8), allocatable     :: t_3d_new_dry(:,:,:), t_3d_new_wet(:,:,:)
  real(8), allocatable     :: t_inc_3d_dry(:,:,:), t_inc_3d_wet(:,:,:)
  real(8)                  :: deltasigma,deltasigmac4h

!-- variables dumped out to netcdf file
  real(8), allocatable     :: p_hyd_3d_new(:,:,:), p_3d_new(:,:,:)

!-------------------------------------------------------------------------------!
  input_file = "./wrf_inout"
! call check( nf90_open(trim(adjustl(input_file)), nf90_nowrite, ncid) )
  call check( nf90_open(trim(adjustl(input_file)), nf90_write,   ncid) ) ! updating P fields in file
!-- get information of netcdf file
  call check( nf90_inquire(ncid, ndims, nvars, natts, recdim) )
  write(6,*) 'Number of dimensions in file ', trim(input_file), ': ', ndims
  write(6,*) 'Number of variables  in file ', trim(input_file), ': ', nvars
!-- get dimensions of x/y/z
!-- dimension in x-direction
  dimname = 'west_east'
  call check ( nf90_inq_dimid(ncid, trim(adjustl(dimname)), dimid)  )
  call check ( nf90_inquire_dimension(ncid, dimid, len = nx)  )
  write(6,*) 'Dimension ', trim(adjustl(dimname)), ': ', nx
!-- dimension in y-direction
  dimname = 'south_north'
  call check ( nf90_inq_dimid(ncid, trim(adjustl(dimname)), dimid)  )
  call check ( nf90_inquire_dimension(ncid, dimid, len = ny)  )
  write(6,*) 'Dimension ', trim(adjustl(dimname)), ': ', ny
!-- dimension in z-direction
  dimname = 'bottom_top'
  call check ( nf90_inq_dimid(ncid, trim(adjustl(dimname)), dimid)  )
  call check ( nf90_inquire_dimension(ncid, dimid, len = nz)  )
  write(6,*) 'Dimension ', trim(adjustl(dimname)), ': ', nz
  dimname = 'bottom_top_stag'
  call check ( nf90_inq_dimid(ncid, trim(adjustl(dimname)), dimid)  )
  call check ( nf90_inquire_dimension(ncid, dimid, len = nz_stag)  )
  write(6,*) 'Dimension ', trim(adjustl(dimname)), ': ', nz_stag
  write(6,'(1x,A,4(3x,A,I6))') 'Dimensions of x/y/z in wrf_inout ==> ',         &
       ' xdim:', nx, ' ydim:', ny, ' zdim:',nz, ' zdim_stag:',nz_stag 

  allocate(dummy_2d(nx, ny))                 ! 2-D array used in netcdf-4 I/O
  allocate(dummy_3d(nx, ny, nz))             ! 3-D array used in netcdf-4 I/O
  allocate(dummy_1d(nz))                     ! 1-D array used in netcdf-4 I/O
  allocate(dummy2_1d(nz_stag))               ! 1-D array used in netcdf-4 I/O

  allocate(xlat_2d(nx, ny))                  ! latitudes
  allocate(xlon_2d(nx, ny))                  ! longitudes
  allocate(c3h_1d(nz))                       ! hvc coefficent (half levels)
  allocate(c4h_1d(nz))                       ! hvc coefficent (half levels)
  allocate(c3f_1d(nz_stag))                  ! hvc coefficent (full levels)
  allocate(c4f_1d(nz_stag))                  ! hvc coefficent (full levels)
  allocate(th2_2d(nx, ny))                   ! potential temperature at 2-m
  allocate(th_3d(nx, ny, nz))                ! potential temperature
  allocate(mu_2d(nx, ny))                    ! air mass
  allocate(mub_2d(nx, ny))                   ! air mass
  allocate(p_3d(nx, ny, nz))                 ! pressure
  allocate(pb_3d(nx, ny, nz))                ! base-state pressure
  allocate(p_hyd_3d(nx, ny, nz))             ! hydrostatic pressure
  allocate(psfc_2d(nx, ny))                  ! surface pressure
  allocate(qv_3d(nx, ny, nz))                ! mixing ratio of water vapor

  allocate(q_integral(nx, ny))               !
  allocate(q_integralc4h(nx, ny))            ! 
  allocate(psfc_2d_new_dry(nx, ny))          ! surface pressure (dry air)
  allocate(psfc_2d_new_wet(nx, ny))          ! surface pressure (wet air)
  allocate(psfc_2d_new_diff(nx, ny))         ! surface pressure differences (wet minus dry)
  allocate(psfc_2d_new_diffperc(nx, ny))     ! surface pressure difference percentage [ (wet-air)/wet ] 

  allocate(p_3d_new_dry(nx, ny, nz))         ! pressure (updated with dry air)
  allocate(p_3d_new_wet(nx, ny, nz))         ! pressure (updated with wet air)
  allocate(p_inc_3d_dry(nx, ny, nz))         ! increments of pressure
  allocate(p_inc_3d_wet(nx, ny, nz))         ! increments of pressure

  allocate(t_3d(nx, ny, nz))                 ! temp (th-->t with not-updated p)
  allocate(t_3d_new_dry(nx, ny, nz))         ! temp (th-->t with updated p)
  allocate(t_3d_new_wet(nx, ny, nz))         ! temp (th-->t with updated p)
  allocate(t_inc_3d_dry(nx, ny, nz))         ! increments of temperature
  allocate(t_inc_3d_wet(nx, ny, nz))         ! increments of temperature

  allocate(p_hyd_3d_new(nx, ny, nz))         ! hydrostatic pressure (updated with dry air)
  allocate(p_3d_new(nx, ny, nz))             ! pressure (updated with wet air)

  ptop = 0.0                                 ! pressure at model top (initial)

  i2 = NINT(REAL(nx)/2.0)
  j2 = NINT(REAL(ny)/2.0)
  print *, 'center grid index i/j = ', i2, j2
!-- latitudes and Longitudes
  varname = "XLAT"
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  xlat_2d(:,:) = dummy_2d(:,:)
  varname = "XLONG"
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  xlon_2d(:,:) = dummy_2d(:,:)
  write(6,'(1x,A,2(1x,F13.6))') '(lat,lon) at lower-left  corner: ',xlat_2d( 1, 1), xlon_2d( 1, 1)
  write(6,'(1x,A,2(1x,F13.6))') '(lat,lon) at lower-right corner: ',xlat_2d(nx, 1), xlon_2d(nx, 1)
  write(6,'(1x,A,2(1x,F13.6))') '(lat,lon) at upper-left  corner: ',xlat_2d( 1,ny), xlon_2d( 1,ny)
  write(6,'(1x,A,2(1x,F13.6))') '(lat,lon) at upper-right corner: ',xlat_2d(nx,ny), xlon_2d(nx,ny)
  write(6,'(1x,A,2(1x,F13.6))') '(lat,lon) at center grid point : ',xlat_2d(i2,j2), xlon_2d(i2,j2)

!-- wrf mass vertical hybrid cooirdinate  (aeta coefficients on half-levels)
!---- 1. read subroutine convert_netcdf_mass_wrf in cplr_wrf_netcdf_interface.f90
!        to see how C3H/C3F/C4H/C4F are read in and dumped out into binary sigf03;
!---- 2. read subroutine init_reg_glob_ll in gridmod.F90 (for wrf_mass_regional)
!        to see how C3H/C3F/C4H/C4F are read from binary sigf03 and, then
!        saved into aeta1/aeta2/eta1/eta2;
!        C3H/C4H -- for half levelsi(mid-layers);
!        C3F/C4F -- for full levels (layer interfaces)
  varname = "C3H"      ! aeta1
  dummy_1d(:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_1d ))
  c3h_1d(:) = dummy_1d(:)

  varname = "C4H"      ! aeta2 (Pa)
  dummy_1d(:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_1d ))
  c4h_1d(:) = dummy_1d(:)

  varname = "C3F"      ! eta1
  dummy2_1d(:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy2_1d ))
  c3f_1d(:) = dummy2_1d(:)

  varname = "C4F"      ! eta2 (Pa)
  dummy2_1d(:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy2_1d ))
  c4f_1d(:) = dummy2_1d(:)

!-- ptop PRESSURE at TOP OF THE MODEL (Pa)
  varname = "P_TOP"
  dummy = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy ))
  ptop = dummy

!-- Potenetial temperature at 2-meters (K)
  varname = "TH2"
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  th2_2d(:,:) = dummy_2d(:,:)
  write(6,'(1x,A,A,A,5(1x,F13.6))') 'checking values at corners and center for ',&
       trim(adjustl(varname)),'=',th2_2d(1,1),th2_2d(1,ny),th2_2d(nx,ny),       &
       th2_2d(ny,1),th2_2d(i2,j2)

!-- Potenetial temperature (K)
  varname = "T"       ! perturbation
  dummy_3d(:,:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_3d ))
  th_3d(:,:,:) = dummy_3d(:,:,:)
  th_3d(:,:,:) = th_3d(:,:,:) + 300.0            ! pertb + 300 ==> full

!-- Dry Air Mass in column (Pa)
  varname = "MU"       ! perturbation dry air mass in column
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  mu_2d(:,:) = dummy_2d(:,:)

  varname = "MUB"      ! base state dry air mass in column
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  mub_2d(:,:) = dummy_2d(:,:)
  mu_2d(:,:) = mu_2d(:,:) + mub_2d(:,:)          ! pertb + base ==> full

!-- pressure (Pa)
  varname = "P"       ! perturbation
  dummy_3d(:,:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_3d ))
  p_3d(:,:,:) = dummy_3d(:,:,:)

  varname = "PB"      ! base-state
  dummy_3d(:,:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_3d ))
  pb_3d(:,:,:) = dummy_3d(:,:,:)
  p_3d(:,:,:) = p_3d(:,:,:) + pb_3d(:,:,:)       ! pertb + base ==> full

  varname = "P_HYD"   ! hydrostatic pressure
  dummy_3d(:,:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_3d ))
  p_hyd_3d(:,:,:) = dummy_3d(:,:,:)

  varname = "PSFC"   ! surface pressure
  dummy_2d(:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_2d ))
  psfc_2d(:,:) = dummy_2d(:,:)

!-- mixing ratio of water vapor (kg/kg) -- moisture
  varname = "QVAPOR"
  dummy_3d(:,:,:) = fill_r4
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_get_var(ncid, varid, dummy_3d ))
  qv_3d(:,:,:) = dummy_3d(:,:,:)

!-- updating surface pressure with MU
!---- 1. surface pressure of dry atmosphere
  psfc_2d_new_dry(:,:) = mu_2d(:,:) + ptop                ! Pa

!---- 2. surface pressure of dry atmosphere + moisture contribution
!     2.1 estimate the moisture contribution (mixing ratio is used)
!         following the calculation in 
!           subroutine read_wrf_mass_netcdf_guess_wrf in cplr_read_wrf_mass_guess.f90
  q_integral(:,:) = 1.0
  q_integralc4h(:,:) = 0.0
  do k = 1, nz
      deltasigma = c3f_1d(k) - c3f_1d(k+1)                                  ! dimensionless
      deltasigmac4h = c4f_1d(k) - c4f_1d(k+1)                               ! Pa
      q_integral(:,:) = q_integral(:,:) + deltasigma*qv_3d(:,:,k)           ! kg/kg
      q_integralc4h(:,:) = q_integralc4h(:,:) + deltasigmac4h*qv_3d(:,:,k)  ! Pa*kg/kg
  end do
!     2.2 for psfc_dry --> psfc
!           see subroutine read_wrf_mass_netcdf_guess_wrf in cplr_read_wrf_mass_guess.f90
!         for psfc --> psfc_dry
!           see subroutine wrwrfmassa_netcdf_wrf in cplr_wrwrfmassa.f90
  psfc_2d_new_wet(:,:) = (psfc_2d_new_dry(:,:)-ptop)*q_integral(:,:)+ptop+q_integralc4h(:,:)

  psfc_2d_new_diff(:,:) = psfc_2d_new_wet(:,:) - psfc_2d_new_dry(:,:)
  psfc_2d_new_diffperc(:,:) = psfc_2d_new_diff(:,:) / psfc_2d_new_wet(:,:) * 100.0

  write(6, *) 'ptop (hPa) = ',ptop/100.0
  write(6,90) 'max/min of increments of P_sfc (hPa) for Dry Air : ',&
             maxval(psfc_2d_new_dry - psfc_2d)/100.0, minval(psfc_2d_new_dry - psfc_2d)/100.0
  write(6,90) 'max/min of increments of P_sfc (hPa) for Wet Air : ',&
             maxval(psfc_2d_new_wet - psfc_2d)/100.0, minval(psfc_2d_new_wet - psfc_2d)/100.0
  write(6,90) 'max/min of P_sfc (hPa) Differences between Wet Air-vs-Dry Air: ',&
             maxval(psfc_2d_new_diff)/100.0, minval(psfc_2d_new_diff)/100.0 
  write(6,91) 'max/min of P_sfc (hPa) Differences (Percentage) between Wet Air-vs-Dry Air: ',&
             maxval(psfc_2d_new_diffperc), minval(psfc_2d_new_diffperc) 
90   format(1x,A,2(1x,F12.3))
91   format(1x,A,2(1x,F12.6))

!-- updating 3-d pressure fields with updated psfc (dry air or wet air)
!     (following code in subroutine load_prsges in guess_grids.F90 of GSI for wrf_mass_regional)
!     This 3-d pressure field is built up with definition of HVC of WRF-ARW, it is 
!     more like hydrostatic pressure (p_hyd)
  do k = 1, nz
      p_3d_new_dry(:,:,k) = c3h_1d(k)*(psfc_2d_new_dry(:,:) - ptop) + c4h_1d(k) + ptop
      p_3d_new_wet(:,:,k) = c3h_1d(k)*(psfc_2d_new_wet(:,:) - ptop) + c4h_1d(k) + ptop
  end do

!  p_inc_3d_dry(:,:,:) = p_3d_new_dry(:,:,:) - p_3d(:,:,:)
!  p_inc_3d_wet(:,:,:) = p_3d_new_wet(:,:,:) - p_3d(:,:,:)
  p_inc_3d_dry(:,:,:) = p_3d_new_dry(:,:,:) - p_hyd_3d(:,:,:)
  p_inc_3d_wet(:,:,:) = p_3d_new_wet(:,:,:) - p_hyd_3d(:,:,:)
  
!-- convert potential temperature to temeprature
!-------------------------------------------------------------------------------!
! Note:                                                                         !
!     old (not-updated) pressure is read from 3-D pressure fields in file.      !
!     new (updated) pressure is calculated with updated MU (and Ps)             !
!                                                                               !
!-------------------------------------------------------------------------------!
!   with not-updated pressure (same as in firstguess)
!  t_3d(:,:,:)     = th_3d(:,:,:) * (    p_3d(:,:,:) / 100000.0) ** kappa
! --> in UPP, P_HYD is read in and used as 3-D pressure fields, is also consistent with GSI.
  t_3d(:,:,:)     = th_3d(:,:,:) * (    p_hyd_3d(:,:,:) / 100000.0) ** kappa
!   updated pressure (by updated Psfc)
  t_3d_new_dry(:,:,:) = th_3d(:,:,:) * (p_3d_new_dry(:,:,:) / 100000.0) ** kappa
  t_3d_new_wet(:,:,:) = th_3d(:,:,:) * (p_3d_new_wet(:,:,:) / 100000.0) ** kappa

  t_inc_3d_dry(:,:,:) = t_3d_new_dry(:,:,:) - t_3d(:,:,:)
  t_inc_3d_wet(:,:,:) = t_3d_new_wet(:,:,:) - t_3d(:,:,:)

  do k = 1, nz
      write(6,100) ' On level ', k, ' at domain center grid point,',            &
           ' Pot-Temp (K)=', th_3d(i2,j2,k),                                    &
           ' P(hPa)=', p_3d(i2,j2,k)/100.0, p_hyd_3d(i2,j2,k)/100.0,            &
           p_3d_new_dry(i2,j2,k)/100.0, p_3d_new_wet(i2,j2,k)/100.0,            &
           ' Temp (K)=', t_3d(i2,j2,k), t_3d_new_dry(i2,j2,k),                  &
           t_3d_new_wet(i2,j2,k)
      write(6,101) ' max/min increment of P(hPa) = (Dry Air updating)',         &
           maxval(p_inc_3d_dry(:,:,k))/100.0, minval(p_inc_3d_dry(:,:,k))/100.0,&
           ' max/min difference of Temp(K)=',                                    &
           maxval(t_inc_3d_dry(:,:,k)), minval(t_inc_3d_dry(:,:,k))
      write(6,101) ' max/min increment of P(hPa) = (wet Air updating)',         &
           maxval(p_inc_3d_wet(:,:,k))/100.0, minval(p_inc_3d_wet(:,:,k))/100.0,&
           ' max/min difference of Temp(K)=',                                    &
           maxval(t_inc_3d_wet(:,:,k)), minval(t_inc_3d_wet(:,:,k))
  end do

100  format(1x,A,I04,A,5x,A,F13.6,A,4F13.6,A,3F13.6)
101  format(1x,20X,2(A,2(1x,F13.6)))

!-- hydrostatic pressure (3D) is updated with wet air (psfc of atmosphere with moisture)
!    following definition of hybrid vertical coordinate used in WRF-ARW.
! p_hyd_3d_new(:,:,:) = p_3d_new_dry(:,:,:)
  p_hyd_3d_new(:,:,:) = p_3d_new_wet(:,:,:)

!-- saving the 3D pressure perturbation with "wet" atmosphere (psfc of atmosphere with moisture)
  p_3d_new(:,:,:) = p_3d_new_wet(:,:,:) - pb_3d(:,:,:)
  
!-- dumping out the updated variables back to original netcdf-4 file (replace original values)
!----    hydrostatic pressure
  varname = "P_HYD"   ! hydrostatic pressure
  dummy_3d(:,:,:) = p_hyd_3d_new(:,:,:)
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_put_var(ncid, varid, dummy_3d ))

!----    surface pressure
  varname = "PSFC"   ! surface pressure
  dummy_2d(:,:) = psfc_2d_new_wet(:,:)
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_put_var(ncid, varid, dummy_2d ))

!----    pressure perturbation
  varname = "P"      ! pressure perturbation
  dummy_3d(:,:,:) = p_3d_new(:,:,:)
  call check( nf90_inq_varid(ncid, trim(adjustl(varname)), varid) )
  call check( nf90_put_var(ncid, varid, dummy_3d ))

!- Close the dataset
  call check( nf90_close(ncid) )

  deallocate(xlat_2d, xlon_2d, c3h_1d, c4h_1d, c3f_1d, c4f_1d, th2_2d, mu_2d, mub_2d)
  deallocate(p_3d, pb_3d, p_hyd_3d, psfc_2d, qv_3d, th_3d)
  deallocate(q_integral, q_integralc4h, psfc_2d_new_dry, psfc_2d_new_wet)
  deallocate(psfc_2d_new_diff, psfc_2d_new_diffperc)
  deallocate(p_3d_new_dry, p_3d_new_wet, p_inc_3d_dry, p_inc_3d_wet)
  deallocate(t_3d_new_dry, t_3d_new_wet, t_inc_3d_dry, t_inc_3d_wet, t_3d)
  deallocate(p_hyd_3d_new, p_3d_new)

!===============================================================================!
      contains
!-----------------------------------------------------------------------
!
      subroutine check(status)
      integer,intent(in) :: status
!
      if(status /= nf90_noerr) then
        print *, 'NetCDF error: ', trim(nf90_strerror(status))
        stop "Stopped"
      end if
      end subroutine check
!
!-----------------------------------------------------------------------
!
!===============================================================================!
end program updateP_wrfinout
