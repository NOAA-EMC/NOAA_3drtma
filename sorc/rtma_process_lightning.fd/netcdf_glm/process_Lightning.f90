program process_Lightning
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2008-01-02
!
! ABSTRACT: 
!     This routine read in lightning data and 
!     map them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  NLDN and Alasks lightning data
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use mpi
  use map_utils
  use misc_definitions_module , only : PROJ_LC
  use constants_module ,only : EARTH_RADIUS_M
  use constants ,only : init_constants_derived, deg2rad
  use gridmod_gsimap ,only : nlon,nlat,init_general_transform,tll2xy,txy2ll
  use kinds, only: r_kind,i_kind

  implicit none
  INCLUDE 'netcdf.inc'
!
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid and map
  CHARACTER*180   geofile

  real ::  userDX, userDY, CEN_LAT, CEN_LON
  real ::  userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON
  integer :: MAP_PROJ

  type (proj_info) :: proj_stack
  REAL :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
  REAL :: knowni, knownj, dx

!
!  For lightning data
!
  INTEGER ::    numStrike
  CHARACTER*180   lightsngle
  real,allocatable:: llon(:)    !
  real,allocatable:: llat(:)    !
  real,allocatable:: ltime(:)   !
  real,allocatable:: lflashqc(:) !
  character*21,allocatable:: ctime(:)   !
  real :: rtmp
  integer,allocatable:: lquality(:) !

  REAL, allocatable :: lightning(:,:)   ! lightning  strakes
  REAL(r_kind), allocatable :: lightning_out(:,:)   ! lightning  strakes

  integer :: numNLDN_all, numNLDN_used
  integer :: numAlaska_all, numAlask_used
!
!! Declare namelists 
!
! SETUP (general control namelist) :
!
  character*10 :: analysis_time
  integer      :: NLDN_filenum
  logical      :: IfAlaska
  namelist/setup/analysis_time, NLDN_filenum, IfAlaska
!
!  ** misc
      
  CHARACTER*180   workpath

  real :: LAT_LL_P,LON_LL_P
  real :: user_known_x, user_known_y
  real(r_kind) :: XC,YC
  real(r_kind)        :: rlon  ! earth longitude (radians)
  real(r_kind)        :: rlat  ! earth latitude  (radians)

  integer i,j,igrid,jgrid,nt

  integer :: NCID, istatus
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  real(r_kind),allocatable:: rxlon(:,:)    !
  real(r_kind),allocatable:: rylat(:,:)    !

  integer :: numlightning,idate,iii
  integer,parameter :: maxsave=100000
  real,allocatable:: savelon(:)    !
  real,allocatable:: savelat(:)    !


!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!  call MPI_INIT(ierror) 
!  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
!  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  allocate(savelon(maxsave))
  allocate(savelat(maxsave))

  numNLDN_all=0
  numNLDN_used=0
  numAlaska_all=0
  numAlask_used=0
  iii=0

  read(5,setup)

  call init_constants_derived
!
! set geogrid fle name
!
  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  call GET_MAP_ATT_geo(geofile, userDX, userDY, CEN_LAT, CEN_LON, &
                userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ)
  write(*,*) userDX, userDY, CEN_LAT, CEN_LON
  write(*,*) userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ
!
!   setup  map
!
  allocate(rxlon(nlon,nlat))
  allocate(rylat(nlon,nlat))

!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)

!  user_known_x = (NLON+1)/2.0
!  user_known_y = (NLAT+1)/2.0
!  call map_init(proj_stack)
!
!  if (MAP_PROJ == PROJ_LC) then
!     call map_set(MAP_PROJ, proj_stack, &
!                  truelat1=userTRUELAT1, &
!                  truelat2=userTRUELAT2, &
!                  stdlon=STAND_LON, &
!                  lat1=CEN_LAT, &
!                  lon1=CEN_LON, &
!                  knowni=user_known_x, &
!                  knownj=user_known_y, &
!                  dx=userDX, &
!                  r_earth=earth_radius_m)
!  else
     mype=0
     rylat=ylat*deg2rad
     rxlon=xlon*deg2rad
     call init_general_transform(rylat,rxlon,mype)
!  endif

  allocate(lightning(nlon,nlat))
  lightning=0

  open(14,file='filelist_lightning')
!
!  process NLDN data
!
!  DO nt=1,NLDN_filenum
   do while (.true.)
    read(14,'(a)',end=200) lightsngle
    write(*,'(a)') trim(lightsngle)
    call ifexist_file(trim(lightsngle),istatus)
    if (ISTATUS .NE. NF_NOERR) CYCLE

    call GET_DIM_ATT_NLDN(lightsngle,numStrike)
    write(*,*) 'number of strikes=', nt, numStrike

    numNLDN_all=numNLDN_all+numStrike
    if(numNLDN_all > maxsave) then
      write(*,*) 'too many raw obs'
      cycle
    endif

    allocate(llon(numStrike))
    allocate(llat(numStrike))
    allocate(ltime(numStrike))
    allocate(lflashqc(numStrike))

    call GET_lightning_NLDN(lightsngle,numStrike,llon,llat,ltime,lflashqc)
    do i=1,numStrike
!        write(*,*) i, llon(i),llat(i),ltime(i),lflashqc(i)
       iii=iii+1
       savelon(iii)=llon(i)
       savelat(iii)=llat(i)
    enddo
!
!  check quality
!
    allocate(lquality(numStrike))
    lquality = 0    ! 0 good data,  > 0 bad data
    call Check_NLDN(numStrike,llon,llat,ltime,lflashqc,lquality)

    do i=1,numStrike

      if(lquality(i) == 0 ) then
        rlon=llon(i)*deg2rad
        rlat=llat(i)*deg2rad
        call tll2xy(rlon,rlat,xc,yc)

        igrid = int(XC+0.5)
        jgrid = int(YC+0.5)
        if( (igrid > 0 .and. igrid< nlon).and.  &
            (jgrid > 0 .and. jgrid< nlat)) then 
            lightning(igrid,jgrid) = lightning(igrid,jgrid) + 1
            numNLDN_used=numNLDN_used+1
        endif
      endif

    enddo

    deallocate(llon)
    deallocate(llat)
    deallocate(ltime)
    deallocate(lflashqc)
    deallocate(lquality)
  enddo ! nt
200 continue
!
!  statistic
!
  write(*,*) ' The total number of NLDN data is:', numNLDN_all
  write(*,*) ' The number of NLDN data used is:', numNLDN_used
  write(*,*) ' The total number of Alasks data is:', numAlaska_all
  write(*,*) ' The number of Alasks data used is:', numAlask_used

!
!     Find max reflectivity in each column
!
   allocate(lightning_out(3,nlon*nlat))
   numlightning=0
   DO j=1,nlat
   DO i=1,nlon
     if(lightning(i,j) > 0 ) then
       numlightning=numlightning+1
       lightning_out(1,numlightning)=float(i)
       lightning_out(2,numlightning)=float(j)
       lightning_out(3,numlightning)=lightning(i,j)
       if(lightning_out(3,numlightning) > 1000.0 ) then
          lightning_out(3,numlightning)=1000.0
          write(6,*) 'high lightning strokes=',lightning(i,j),i,j
       endif
     endif
   ENDDO
   ENDDO
     write(*,*) 'Dump out results',numlightning,'out of',nlon*nlat
     OPEN(10,file=trim(workPath)//'LightningInGSI.dat',form='unformatted')
      write(10) 3,nlon,nlat,numlightning,1,2
      write(10) ((real(lightning_out(i,j)),i=1,3),j=1,numlightning)
      write(10) lightning
      write(10) numNLDN_all
      write(10) savelon(1:numNLDN_all)
      write(10) savelat(1:numNLDN_all)
     close(10)
!
!  
   read(analysis_time,'(I10)') idate
   write(6,*) 'cycle time is :', idate

   write(6,*) ' write lightning in BUFR'
   call write_bufr_lightning(1,nlon,nlat,numlightning,lightning_out,idate)

!  call MPI_FINALIZE(ierror)
!
end program process_Lightning 
!