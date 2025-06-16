    MODULE module_dep_simple
      IMPLICIT NONE
      INTEGER, PARAMETER :: dep_seasons = 5
      INTEGER, PARAMETER :: nlu = 25
      REAL, parameter :: small_value = 1.e-36
      REAL, parameter :: large_value = 1.e36
      integer, parameter :: isice_temp = 24
      integer, parameter :: iswater_temp = 16
      integer, parameter :: wrf2mz_lt_map(nlu) = (/ 1, 2, 2, 2, 2, &
                                                    4, 3, 3, 3, 3, &
                                                    4, 5, 4, 5, 6, &
                                                    7, 9, 6, 8, 9, &
                                                    6, 6, 8, 0, 0 /)
      real, parameter :: wh2o = 18.0153
      real, parameter :: wpan = 121.04793
      character(len=4), parameter :: mminlu = 'USGS'
      INTEGER :: month = 0
      INTEGER :: ixxxlu(nlu)
      INTEGER, allocatable :: luse2usgs(:)
      REAL :: kpart(nlu)
      REAL :: rac(nlu,dep_seasons), rclo(nlu,dep_seasons), rcls(nlu,dep_seasons)
      REAL :: rgso(nlu,dep_seasons), rgss(nlu,dep_seasons)
      REAL :: ri(nlu,dep_seasons), rlu(nlu,dep_seasons)
      REAL :: ri_pan(5,11)
      real :: c0_pan(11) = (/ 0.000, 0.006, 0.002, 0.009, 0.015, &
                                 0.006, 0.000, 0.000, 0.000, 0.002, 0.002 /)
      real :: k_pan (11) = (/ 0.000, 0.010, 0.005, 0.004, 0.003, &
                                 0.005, 0.000, 0.000, 0.000, 0.075, 0.002 /)
      REAL :: dratio(1000), hstar(1000), hstar4(1000)
      REAL :: f0(1000), dhr(1000), scpr23(1000)
    PUBLIC
    logical, allocatable :: is_aerosol(:)
    CONTAINS
SUBROUTINE wesely_driver( id, config_flags, current_month, &
                          julday, rh,moist, p8w, t8w, raincv, &
                          ddvel, aer_res_def, &
                          ivgtyp, tsk, gsw, vegfra, pbl, &
                          rmol, ust, znt, &
                          z, z_at_w, snowh, numgas, &
                          ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte )
  USE module_model_constants
  USE module_configure
  USE module_state_description
  USE module_state_description, only: param_first_scalar
   INTEGER, INTENT(IN ) :: id,julday, &
                                  numgas, current_month, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), INTENT(IN ) :: &
                                                      moist
   REAL, DIMENSION( its:ite, jts:jte, num_chem ), INTENT(INOUT ) :: &
                                                      ddvel
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: &
                                                      z, &
                                                      t8w, &
                                                      p8w, &
                                                      z_at_w, rh
   INTEGER,DIMENSION (ims:ime , jms:jme ), INTENT(IN ) :: &
                                                     ivgtyp
   REAL, DIMENSION (ims:ime , jms:jme ), INTENT(INOUT ) :: &
                                                     tsk, &
                                                     gsw, &
                                                     vegfra, &
                                                     pbl, &
                                                     rmol, &
                                                     ust, &
                                                     raincv, &
                                                     znt
   REAL, DIMENSION (ims:ime,jms:jme), INTENT(INOUT) :: aer_res_def
   REAL, optional, INTENT(IN) :: snowh(ims:ime,jms:jme)
   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags
      REAL :: dvpart, pa, rad
      REAL :: rhchem, ta, ustar, vegfrac, z1, zntt
      INTEGER :: i, iland, iseason, j, jce, jcs, n, nr, ipr,jpr,nvr
      LOGICAL :: rainflag, vegflag, wetflag
      REAL :: p(kts:kte)
      REAL :: srfres(numgas)
      REAL :: ddvel0d(numgas)
      real :: rcx(numgas)
      INTRINSIC max, min
      CALL wrf_debug(15,'in dry_dep_wesely')
         if( julday < 90 .or. julday > 270 ) then
            iseason = 2
            CALL wrf_debug(15,'setting iseason to 2')
         else
            iseason = 1
         endif
tile_lat_loop : &
      do j = jts,jte
tile_lon_loop : &
         do i = its,ite
            iland = luse2usgs( ivgtyp(i,j) )
            ta = tsk(i,j)
            rad = gsw(i,j)
            vegfrac = vegfra(i,j)
            ustar = ust(i,j)
            zntt = znt(i,j)
            z1 = z_at_w(i,kts+1,j) - z_at_w(i,kts,j)
            rainflag = .FALSE.
            wetflag = .FALSE.
            if(p_qr > 1) then
               if(moist(i,kts,j,p_qr) > 1.e-18 .or. raincv(i,j) > 0.) then
                  rainflag = .true.
               endif
            endif
            rhchem= rh(i,kts,j)*100.
            if (rhchem >= 95.) wetflag = .true.
            CALL rc( rcx, ta, rad, rhchem, iland, &
                     iseason, numgas, wetflag, rainflag )
                srfres(:)= 0.
            CALL deppart( rmol(i,j), ustar, rhchem, iland, dvpart )
            ddvel0d(1:numgas) = 0.
            aer_res_def(i,j) = 0.
            CALL landusevg( ddvel0d, ustar, rmol(i,j), zntt, z1, dvpart, iland, &
                            numgas, srfres, aer_res_def(i,j) )
            ddvel(i,j,1:numgas) = ddvel0d(1:numgas)
         end do tile_lon_loop
      end do tile_lat_loop
END SUBROUTINE wesely_driver
      SUBROUTINE rc( rcx, t, rad, rh, iland, &
                     iseason, numgas, wetflag, rainflag )
  USE module_state_description
        INTEGER, intent(in) :: iland, iseason, numgas
        REAL, intent(in) :: rad, rh
        REAL, intent(in) :: t
        real, intent(out) :: rcx(numgas)
        LOGICAL, intent(in) :: rainflag, wetflag
        REAL, parameter :: t0 = 298.
        REAL, parameter :: tmelt = 273.16
        INTEGER :: lt, n
        REAL :: rclx, rdc, resice, rgsx, rluo1, rluo2
        REAL :: rlux, rmx, rs, rsmx, rdtheta, z, wrk
        REAL :: qs, es, ws, dewm, dv_pan, drat
        REAL :: crs, tc
        REAL :: rs_pan, tc_pan
        LOGICAL :: has_dew
        REAL :: hstary(numgas)
        INTRINSIC exp
        rcx(1:numgas) = 1.
        tc = t - 273.15
        rdtheta = 0.
        z = 200./(rad+0.1)
        IF ( tc<=0. .OR. tc>=40. ) THEN
          rs = 9999.
        ELSE
          rs = ri(iland,iseason)*(1+z*z)*(400./(tc*(40.-tc)))
        END IF
        rdc = 100.*(1. + 1000./(rad + 10.))/(1. + 1000.*rdtheta)
        rluo1 = 1./(1./3000. + 3./rlu(iland,iseason))
        rluo2 = 1./(1./1000. + 3./rlu(iland,iseason))
        resice = 1000.*exp( -(tc + 4.) )
        wrk = (t0 - t)/(t0*t)
        rgsx = 1000.
        IF ((wetflag) .OR. (rainflag)) THEN
            rgsx = 500.
        END IF
        IF (iland==iswater_temp) THEN
          rgsx = 0.
        END IF
        IF( iseason==4 .OR. iland==isice_temp ) THEN
          IF( tc > 2. ) THEN
            rgsx = 0.
          else IF ( tc >= -1. .AND. tc <= 2. ) THEN
            rgsx = 70.*(2. - tc)
          else IF ( tc < -1. ) THEN
            rgsx = 500.
          END IF
        END IF
      END SUBROUTINE rc
      SUBROUTINE deppart( rmol, ustar, rh, iland, dvpart )
        INTEGER, intent(in) :: iland
        REAL, intent(in) :: rh, rmol, ustar
        REAL, intent(out) :: dvpart
        INTRINSIC exp
        dvpart = ustar/kpart(iland)
        IF (rmol<0.) THEN
          dvpart = dvpart*(1.+(-300.*rmol)**0.66667)
        END IF
        IF (rh>80.) THEN
          dvpart = dvpart*(1.+0.37*exp((rh-80.)/20.))
        END IF
        IF (ixxxlu(iland)==5) THEN
        END IF
      END SUBROUTINE deppart
      SUBROUTINE landusevg( vgs, ustar, rmol, z0, zz, &
                            dvparx, iland, numgas, srfres, aer_res_def)
        USE module_model_constants, only: karman
        INTEGER, intent(in) :: iland, numgas
        REAL, intent(in) :: dvparx, ustar, z0, zz
        REAL, intent(inout) :: rmol
        REAL, intent(inout) :: aer_res_def
        REAL, intent(in) :: srfres(numgas)
        REAL, intent(out) :: vgs(numgas)
        INTEGER :: jspec
        REAL :: vgp, vgpart, zr
        REAL :: rmol_tmp
        REAL :: vgspec(numgas)
        zr = zz*.5
        rmol_tmp = rmol
        CALL depvel( numgas, rmol_tmp, zr, z0, ustar, vgspec, vgpart, aer_res_def )
        zr = 2.0
        CALL depvel( numgas, rmol, zr, z0, ustar, vgspec, vgpart, aer_res_def )
        vgp = 1.0/((1.0/vgpart)+(1.0/dvparx))
        DO jspec = 1, numgas
          vgs(jspec) = 1.0/(1.0/vgspec(jspec) + srfres(jspec))
        END DO
        CALL cellvg( vgs, ustar, zz, zr, rmol, numgas )
      END SUBROUTINE landusevg
      SUBROUTINE cellvg( vgtemp, ustar, dz, zr, rmol, nspec )
        USE module_model_constants, only: karman
        INTEGER, intent(in) :: nspec
        REAL, intent(in) :: dz, rmol, ustar, zr
        REAL, intent(out) :: vgtemp(nspec)
        INTEGER :: nss
        REAL :: a, fac, pdz, pzr, vk
        INTRINSIC alog, sqrt
        vk = karman
        DO nss = 1, nspec
          IF (rmol < 0.) THEN
            pdz = sqrt(1.0 - 9.0*dz*rmol)
            pzr = sqrt(1.0 - 9.0*zr*rmol)
            fac = ((pdz - 1.0)/(pzr - 1.0))*((pzr + 1.0)/(pdz + 1.0))
            a = 0.74*dz*alog(fac) + (0.164/rmol)*(pdz-pzr)
          ELSE IF (rmol == 0.) THEN
            a = 0.74*(dz*alog(dz/zr) - dz + zr)
          ELSE
            a = 0.74*(dz*alog(dz/zr) - dz + zr) + (2.35*rmol)*(dz - zr)**2
          END IF
          vgtemp(nss) = vgtemp(nss)/(1.0 + vgtemp(nss)*a/(vk*ustar*(dz - zr)))
        END DO
      END SUBROUTINE cellvg
      SUBROUTINE depvel( numgas, rmol, zr, z0, ustar, depv, vgpart, aer_res )
        USE module_model_constants, only: karman
        INTEGER, intent(in) :: numgas
        REAL, intent(in) :: ustar, z0, zr
        REAL, intent(out) :: vgpart, aer_res
        REAL, intent(inout) :: rmol
        REAL, intent(out) :: depv(numgas)
        INTEGER :: l
        REAL :: ao, ar, polint, vk
        INTRINSIC alog
        vk = karman
        if(abs(rmol) < 1.E-6 ) rmol = 0.
        IF (rmol<0) THEN
          ar = ((1.0-9.0*zr*rmol)**(0.25)+0.001)**2
          ao = ((1.0-9.0*z0*rmol)**(0.25)+0.001)**2
          polint = 0.74*(alog((ar-1.0)/(ar+1.0))-alog((ao-1.0)/(ao+1.0)))
        ELSE IF (rmol==0.) THEN
          polint = 0.74*alog(zr/z0)
        ELSE
          polint = 0.74*alog(zr/z0) + 4.7*rmol*(zr-z0)
        END IF
        DO l = 1, numgas
          depv(l) = ustar*vk/(2.0*scpr23(l)+polint)
        END DO
        vgpart = ustar*vk/polint
        aer_res = polint/(karman*max(ustar,1.0e-4))
      END SUBROUTINE depvel
      SUBROUTINE dep_init( id, config_flags, numgas, mminlu_loc, &
                           ips, ipe, jps, jpe, ide, jde )
  USE module_model_constants
  USE module_configure
  USE module_state_description
   TYPE (grid_config_rec_type) , INTENT (in) :: config_flags
        character(len=*), intent(in) :: mminlu_loc
        integer, intent(in) :: id, numgas
        integer, intent(in) :: ips, ipe, jps, jpe
        integer, intent(in) :: ide, jde
        INTEGER :: iland, iseason, l
        integer :: astat
        integer :: ncid
        integer :: dimid
        integer :: varid
        integer :: max_dom
        integer :: lon_e, lat_e
        integer :: iend, jend
        REAL :: sc
        character(len=128) :: err_msg
        character(len=128) :: filename
        character(len=3) :: id_num
        REAL :: dat1(nlu,dep_seasons), dat2(nlu,dep_seasons), &
                dat3(nlu,dep_seasons), dat4(nlu,dep_seasons), &
                dat5(nlu,dep_seasons), dat6(nlu,dep_seasons), &
                dat7(nlu,dep_seasons), dvj(numgas)
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
include 'netcdf.inc'
        call nl_get_sf_surface_physics(id,l)
        if( l == 0 ) &
             call wrf_error_fatal3("<stdin>",818,&
"ERROR: Cannot use dry deposition without using a soil model.")
        DATA ((dat1(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.60E+02, 0.60E+02, 0.60E+02, 0.60E+02, 0.70E+02, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.12E+03, 0.70E+02, 0.13E+03, 0.70E+02, &
          0.13E+03, 0.10E+03, 0.10E+11, 0.80E+02, 0.10E+03, 0.10E+11, &
          0.80E+02, 0.10E+03, 0.10E+03, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, 0.10E+11, &
          0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, &
          0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, &
          0.10E+11, 0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.70E+02, 0.40E+03, 0.80E+03, 0.10E+11, &
          0.10E+11, 0.80E+03, 0.10E+11, 0.10E+11, 0.80E+03, 0.80E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.14E+03, 0.24E+03, 0.24E+03, 0.24E+03, &
          0.12E+03, 0.14E+03, 0.25E+03, 0.70E+02, 0.25E+03, 0.19E+03, &
          0.10E+11, 0.16E+03, 0.19E+03, 0.10E+11, 0.16E+03, 0.19E+03, &
          0.19E+03, 0.10E+11, 0.10E+11, 0.10E+11/
        IF (nlu/=25) THEN
          call wrf_debug(0, 'number of land use classifications not correct ')
          CALL wrf_error_fatal3("<stdin>",849,&
"LAND USE CLASSIFICATIONS NOT 25")
        END IF
        IF (dep_seasons/=5) THEN
          call wrf_debug(0, 'number of dep_seasons not correct ')
          CALL wrf_error_fatal3("<stdin>",854,&
"DEP_SEASONS NOT 5")
        END IF
        DO iseason = 1, dep_seasons
           ri(1:nlu,iseason) = dat1(1:nlu,iseason)
        END DO
        DATA ((dat2(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, &
          0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, &
          0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.20E+04, 0.60E+04, 0.90E+04, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rlu(1:nlu,iseason) = dat2(1:nlu,iseason)
        END DO
        DATA ((dat3(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+04, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.00E+00, 0.30E+03, 0.20E+04, 0.00E+00, &
          0.30E+03, 0.20E+04, 0.20E+04, 0.00E+00, 0.00E+00, 0.00E+00, &
          0.10E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+04, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.15E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.17E+04, 0.00E+00, 0.20E+03, 0.17E+04, &
          0.00E+00, 0.20E+03, 0.17E+04, 0.17E+04, 0.00E+00, 0.00E+00, &
          0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, 0.10E+03, &
          0.15E+04, 0.00E+00, 0.10E+03, 0.15E+04, 0.15E+04, 0.00E+00, &
          0.00E+00, 0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+02, 0.10E+04, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, &
          0.50E+02, 0.15E+04, 0.00E+00, 0.50E+02, 0.15E+04, 0.15E+04, &
          0.00E+00, 0.00E+00, 0.00E+00, 0.10E+03, 0.50E+02, 0.50E+02, &
          0.50E+02, 0.50E+02, 0.12E+04, 0.80E+02, 0.80E+02, 0.80E+02, &
          0.10E+03, 0.12E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, &
          0.00E+00, 0.20E+03, 0.15E+04, 0.00E+00, 0.20E+03, 0.15E+04, &
          0.15E+04, 0.00E+00, 0.00E+00, 0.00E+00/
        DO iseason = 1, dep_seasons
           rac(1:nlu,iseason) = dat3(1:nlu,iseason)
        END DO
        DATA ((dat4(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.40E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, 0.10E+04, &
          0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+04, &
          0.40E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.50E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, &
          0.10E+04, 0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, &
          0.10E+04, 0.40E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, 0.10E+01, 0.10E+01, &
          0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, 0.20E+03, 0.10E+04, &
          0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.50E+03, 0.10E+03, 0.10E+03, 0.10E+01, &
          0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+04, 0.10E+03, 0.10E+04, 0.50E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, &
          0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, &
          0.10E+01, 0.10E+01, 0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, &
          0.20E+03, 0.10E+04, 0.10E+03, 0.10E+04/
        DO iseason = 1, dep_seasons
           rgss(1:nlu,iseason) = dat4(1:nlu,iseason)
        END DO
        DATA ((dat5(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.30E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, &
          0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03, &
          0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.80E+03, 0.30E+03, &
          0.40E+03, 0.80E+03, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, &
          0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, &
          0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, &
          0.35E+04, 0.40E+03, 0.60E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.20E+03, 0.35E+04, 0.35E+04, 0.20E+04, &
          0.35E+04, 0.35E+04, 0.40E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.40E+03, 0.35E+04, 0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, &
          0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, &
          0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03/
        DO iseason = 1, dep_seasons
           rgso(1:nlu,iseason) = dat5(1:nlu,iseason)
        END DO
        DATA ((dat6(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.20E+04, 0.40E+04, 0.10E+11, 0.90E+04, 0.40E+04, &
          0.10E+11, 0.90E+04, 0.40E+04, 0.40E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.30E+04, 0.60E+04, 0.10E+11, 0.90E+04, &
          0.60E+04, 0.10E+11, 0.90E+04, 0.60E+04, 0.60E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.20E+04, 0.20E+03, 0.40E+03, 0.10E+11, &
          0.90E+04, 0.40E+03, 0.10E+11, 0.90E+04, 0.40E+03, 0.40E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rcls(1:nlu,iseason) = dat6(1:nlu,iseason)
        END DO
        DATA ((dat7(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, 0.40E+03, &
          0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.40E+03, 0.60E+03, &
          0.10E+11, 0.40E+03, 0.60E+03, 0.60E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, &
          0.40E+03, 0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.80E+03, &
          0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.40E+03, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.10E+04, 0.15E+04, 0.60E+03, 0.10E+11, &
          0.80E+03, 0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.10E+04, 0.50E+03, 0.15E+04, 0.10E+04, 0.15E+04, 0.70E+03, &
          0.10E+11, 0.60E+03, 0.70E+03, 0.10E+11, 0.60E+03, 0.70E+03, &
          0.70E+03, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rclo(1:nlu,iseason) = dat7(1:nlu,iseason)
        END DO
        hstar(1:numgas) = 0.
        hstar4(1:numgas) = 0.
        dhr(1:numgas) = 0.
        f0(1:numgas) = 0.
        dvj(1:numgas) = 99.
        if( id == 1 .or. id == 2 ) then
        if( allocated (luse2usgs) ) deallocate (luse2usgs)
        allocate( luse2usgs(config_flags%num_land_cat),stat=astat )
        if( astat /= 0 ) then
           CALL wrf_message( 'dep_init: failed to allocate luse2usgs array' )
           CALL wrf_abort
        end if
        if( trim(mminlu_loc) == 'USGS' ) then
           luse2usgs(:) = (/ (iland,iland=1,config_flags%num_land_cat) /)
         elseif( trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' ) then
           luse2usgs(:) = (/ 14,13,12,11,15,8,9,10,10,7, &
                              17,4,1,5,24,19,16,21,22,23 /)
         endif
        endif
        kpart(1) = 500.
        kpart(2) = 500.
        kpart(3) = 500.
        kpart(4) = 500.
        kpart(5) = 500.
        kpart(6) = 100.
        kpart(7) = 500.
        kpart(8) = 500.
        kpart(9) = 500.
        kpart(10) = 500.
        kpart(11) = 100.
        kpart(12) = 100.
        kpart(13) = 100.
        kpart(14) = 100.
        kpart(15) = 100.
        kpart(16) = 500.
        kpart(17) = 500.
        kpart(18) = 500.
        kpart(19) = 500.
        kpart(20) = 500.
        kpart(21) = 100.
        kpart(22) = 500.
        kpart(23) = 500.
        kpart(24) = 500.
        kpart(25) = 500.
        IF (mminlu=='OLD ') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 3
          ixxxlu(4) = 4
          ixxxlu(5) = 5
          ixxxlu(6) = 5
          ixxxlu(7) = 0
          ixxxlu(8) = 6
          ixxxlu(9) = 1
          ixxxlu(10) = 6
          ixxxlu(11) = 0
          ixxxlu(12) = 4
          ixxxlu(13) = 6
        END IF
        IF (mminlu=='USGS') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 2
          ixxxlu(4) = 2
          ixxxlu(5) = 2
          ixxxlu(6) = 4
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 3
          ixxxlu(10) = 6
          ixxxlu(11) = 4
          ixxxlu(12) = 5
          ixxxlu(13) = 4
          ixxxlu(14) = 5
          ixxxlu(15) = 5
          ixxxlu(16) = 0
          ixxxlu(17) = 6
          ixxxlu(18) = 4
          ixxxlu(19) = 1
          ixxxlu(20) = 6
          ixxxlu(21) = 4
          ixxxlu(22) = 6
          ixxxlu(23) = 1
          ixxxlu(24) = 0
          ixxxlu(25) = 1
        END IF
        IF (mminlu=='SiB ') THEN
          ixxxlu(1) = 4
          ixxxlu(2) = 4
          ixxxlu(3) = 4
          ixxxlu(4) = 5
          ixxxlu(5) = 5
          ixxxlu(6) = 6
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 6
          ixxxlu(10) = 6
          ixxxlu(11) = 1
          ixxxlu(12) = 2
          ixxxlu(13) = 6
          ixxxlu(14) = 1
          ixxxlu(15) = 0
          ixxxlu(16) = 0
          ixxxlu(17) = 1
        END IF
      END SUBROUTINE dep_init
    END MODULE module_dep_simple
