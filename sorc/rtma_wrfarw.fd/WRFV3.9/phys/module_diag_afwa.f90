MODULE diag_functions
CONTAINS
  FUNCTION calc_rh ( p, t, qv ) result ( rh )
    IMPLICIT NONE
    REAL, INTENT(IN) :: p, t, qv
    REAL :: rh
    REAL, PARAMETER :: pq0=379.90516
    REAL, PARAMETER :: a2=17.2693882
    REAL, PARAMETER :: a3=273.16
    REAL, PARAMETER :: a4=35.86
    REAL, PARAMETER :: rhmin=1.
    REAL :: q, qs
    INTEGER :: i,j,k
      q=qv/(1.0+qv)
      qs=pq0/p*exp(a2*(t-a3)/(t-a4))
      rh=100.*q/qs
      IF (rh .gt. 100.) THEN
        rh=100.
      ELSE IF (rh .lt. rhmin) THEN
        rh=rhmin
      ENDIF
  END FUNCTION calc_rh
  FUNCTION uv_wind ( u, v ) result ( wind_speed )
    IMPLICIT NONE
    REAL, INTENT(IN) :: u, v
    REAL :: wind_speed
    wind_speed = sqrt( u*u + v*v )
  END FUNCTION uv_wind
  FUNCTION Theta ( t, p )
  IMPLICIT NONE
     REAL, INTENT ( IN ) :: t
     REAL, INTENT ( IN ) :: p
     REAL :: theta
     REAL :: Rd
     REAL :: Cp
     REAL :: p0
     Rd = 287.04
     Cp = 1004.67
     p0 = 1000.00
     theta = t * ( (p0/p)**(Rd/Cp) )
  END FUNCTION Theta
  FUNCTION Thetae ( tK, p, rh, mixr )
  IMPLICIT NONE
     REAL :: tK
     REAL :: p
     REAL :: rh
     REAL :: mixr
     REAL :: te
     REAL :: thetae
     REAL, PARAMETER :: R = 287.04
     REAL, PARAMETER :: P0 = 1000.0
     REAL, PARAMETER :: lv = 2.54*(10**6)
     REAL, PARAMETER :: cp = 1004.67
     REAL :: tlc
     tlc = TLCL ( tK, rh )
     thetae = (tK * (p0/p)**( (R/Cp)*(1.- ( (.28E-3)*mixr*1000.) ) ) )* &
                 exp( (((3.376/tlc)-.00254))*&
                    (mixr*1000.*(1.+(.81E-3)*mixr*1000.)) )
  END FUNCTION Thetae
  FUNCTION The2T ( thetaeK, pres, flag ) result ( tparcel )
  IMPLICIT NONE
     REAL, INTENT ( IN ) :: thetaeK
     REAL, INTENT ( IN ) :: pres
     LOGICAL, INTENT ( INOUT ) :: flag
     REAL :: tparcel
     REAL :: thetaK
     REAL :: tovtheta
     REAL :: tcheck
     REAL :: svpr, svpr2
     REAL :: smixr, smixr2
     REAL :: thetae_check, thetae_check2
     REAL :: tguess_2, correction
     LOGICAL :: found
     INTEGER :: iter
     REAL :: R
     REAL :: Cp
     REAL :: kappa
     REAL :: Lv
     R = 287.04
     Cp = 1004.67
     Kappa = R/Cp
     Lv = 2.500E+6
     tovtheta = (pres/100000.0)**(r/cp)
     tparcel = thetaeK/exp(lv*.012/(cp*295.))*tovtheta
     iter = 1
     found = .false.
     flag = .false.
     DO
        IF ( iter > 105 ) EXIT
        tguess_2 = tparcel + REAL ( 1 )
        svpr = 6.122 * exp ( (17.67*(tparcel-273.15)) / (tparcel-29.66) )
        smixr = ( 0.622*svpr ) / ( (pres/100.0)-svpr )
        svpr2 = 6.122 * exp ( (17.67*(tguess_2-273.15)) / (tguess_2-29.66) )
        smixr2 = ( 0.622*svpr2 ) / ( (pres/100.0)-svpr2 )
        thetae_check = Thetae ( tparcel, pres/100., 100., smixr )
        thetae_check2 = Thetae ( tguess_2, pres/100., 100., smixr2 )
        IF ( ABS (thetaeK-thetae_check) < .001) THEN
           found = .true.
           flag = .true.
           EXIT
        END IF
        correction = ( thetaeK-thetae_check ) / ( thetae_check2-thetae_check )
        tparcel = tparcel + correction
        iter = iter + 1
     END DO
  END FUNCTION The2T
  FUNCTION VirtualTemperature ( tK, w ) result ( Tv )
  IMPLICIT NONE
     real, intent ( in ) :: tK
     real, intent ( in ) :: w
     real :: Tv
     Tv = tK * ( 1.0 + (w/0.622) ) / ( 1.0 + w )
  END FUNCTION VirtualTemperature
  FUNCTION SaturationMixingRatio ( tK, p ) result ( ws )
    IMPLICIT NONE
    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: p
    REAL :: ws
    REAL :: es
    es = 6.122 * exp ( (17.67*(tK-273.15))/ (tK-29.66) )
    ws = ( 0.622*es ) / ( (p/100.0)-es )
  END FUNCTION SaturationMixingRatio
  FUNCTION TLCL ( tk, rh )
    IMPLICIT NONE
    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: rh
    REAL :: tlcl
    REAL :: denom, term1, term2
    term1 = 1.0 / ( tK - 55.0 )
    IF ( rh > REAL (0) ) THEN
      term2 = ( LOG (rh/100.0) / 2840.0 )
    ELSE
      term2 = ( LOG (0.001/1.0) / 2840.0 )
    END IF
    denom = term1 - term2
    tlcl = ( 1.0 / denom ) + REAL ( 55 )
  END FUNCTION TLCL
  FUNCTION Pwat ( nz, qv, qc, dz8w, rho )
    IMPLICIT NONE
     INTEGER, INTENT ( IN ) :: nz
     REAL, INTENT ( IN ) :: qv ( nz )
     REAL, INTENT ( IN ) :: qc ( nz )
     REAL, INTENT ( IN ) :: dz8w ( nz )
     REAL, INTENT ( IN ) :: rho ( nz )
     REAL :: Pwat
     INTEGER :: k
     Pwat=0
     DO k = 1, nz
       Pwat = Pwat + (qv(k) + qc(k)) * dz8w(k) * rho(k)
     ENDDO
  END FUNCTION Pwat
    FUNCTION Buoyancy ( nz, tk, rh, p, hgt, sfc, cape, cin, zlfc, plfc, lidx, &
                        parcel ) result (ostat)
      IMPLICIT NONE
      INTEGER, INTENT ( IN ) :: nz
      INTEGER, INTENT ( IN ) :: sfc
      REAL, INTENT ( IN ) :: tk ( nz )
      REAL, INTENT ( IN ) :: rh ( nz )
      REAL, INTENT ( IN ) :: p ( nz )
      REAL, INTENT ( IN ) :: hgt ( nz )
      REAL, INTENT ( OUT ) :: cape
      REAL, INTENT ( OUT ) :: cin
      REAL, INTENT ( OUT ) :: zlfc
      REAL, INTENT ( OUT ) :: plfc
      REAL, INTENT ( OUT ) :: lidx
      INTEGER :: ostat
      INTEGER, INTENT ( IN ) :: parcel
      REAL :: ws ( nz )
      REAL :: w ( nz )
      REAL :: dTvK ( nz )
      REAL :: buoy ( nz )
      REAL :: tlclK
      REAL :: plcl
      REAL :: nbuoy
      REAL :: pbuoy
      REAL :: srctK
      REAL :: srcrh
      REAL :: srcws
      REAL :: srcw
      REAL :: srcp
      REAL :: srctheta
      REAL :: srcthetaeK
      INTEGER :: srclev
      REAL :: spdiff
      REAL :: ptK
      REAL :: ptvK
      REAL :: tvK
      REAL :: pw
      INTEGER :: i, j, k
      INTEGER :: lfclev
      INTEGER :: prcl
      INTEGER :: mlev
      INTEGER :: lyrcnt
      LOGICAL :: flag
      LOGICAL :: wflag
      REAL :: freeze
      REAL :: pdiff
      REAL :: pm, pu, pd
      REAL :: lidxu
      REAL :: lidxd
      REAL :: Rd
         PARAMETER ( Rd = 287.058 )
      REAL :: Cp
         PARAMETER ( Cp = 1004.67 )
      REAL :: g
         PARAMETER ( g = 9.80665 )
      REAL :: RUNDEF
         PARAMETER ( RUNDEF = -9.999E30 )
      ostat = 0
      CAPE = REAL ( 0 )
      CIN = REAL ( 0 )
      ZLFC = RUNDEF
      PLFC = RUNDEF
      IF ( parcel > 3 .or. parcel < 1 ) THEN
         prcl = 1
      ELSE
         prcl = parcel
      END IF
      DO k = sfc, nz
        ws ( k ) = SaturationMixingRatio ( tK(k), p(k) )
        w ( k ) = ( rh(k)/100.0 ) * ws ( k )
      END DO
      srclev = sfc
      srctK = tK ( sfc )
      srcrh = rh ( sfc )
      srcp = p ( sfc )
      srcws = ws ( sfc )
      srcw = w ( sfc )
      srctheta = Theta ( tK(sfc), p(sfc)/100.0 )
      mlev = sfc + 1
      DO k = sfc + 1, nz
         pdiff = ( p (sfc) - p (k) ) / REAL ( 100 )
         IF ( pdiff <= REAL (100) ) mlev = k
         IF ( pdiff >= REAL (180) ) EXIT
         IF ( prcl == 1 ) THEN
            IF ( (w(k) > srcw) ) THEN
               srctheta = Theta ( tK(k), p(k)/100.0 )
               srcw = w ( k )
               srclev = k
               srctK = tK ( k )
               srcrh = rh ( k )
               srcp = p ( k )
            END IF
         END IF
      END DO
      lyrcnt = mlev - sfc + 1
      IF ( prcl == 2 ) THEN
         srclev = sfc
         srctK = SUM ( tK (sfc:mlev) ) / REAL ( lyrcnt )
         srcw = SUM ( w (sfc:mlev) ) / REAL ( lyrcnt )
         srcrh = SUM ( rh (sfc:mlev) ) / REAL ( lyrcnt )
         srcp = SUM ( p (sfc:mlev) ) / REAL ( lyrcnt )
         srctheta = Theta ( srctK, srcp/100. )
      END IF
      srcthetaeK = Thetae ( srctK, srcp/100.0, srcrh, srcw )
      tlclK = TLCL ( tK(srclev), rh(srclev) )
      plcl = p(srclev) * ( (tlclK/tK(srclev))**(Cp/Rd) )
      buoy = REAL ( 0 )
      pw = srcw
      wflag = .false.
      DO k = srclev, nz
         IF ( p (k) <= plcl ) THEN
            IF ( wflag ) THEN
               flag = .false.
               ptK = The2T ( srcthetaeK, p(k), flag )
               pw = SaturationMixingRatio ( ptK, p(k) )
               ptvK = VirtualTemperature ( ptK, pw )
               tvK = VirtualTemperature ( tK (k), w (k) )
               freeze = 0.033 * ( 263.15 - pTvK )
               IF ( freeze > 1.0 ) freeze = 1.0
               IF ( freeze < 0.0 ) freeze = 0.0
               freeze = freeze * 333700.0 * ( srcw - pw ) / 1005.7
               pTvK = pTvK - pTvK * ( srcw - pw ) + freeze
               dTvK ( k ) = ptvK - tvK
               buoy ( k ) = g * ( dTvK ( k ) / tvK )
            ELSE
               ptK = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
               ptvK = VirtualTemperature ( ptK, srcw )
               tvK = VirtualTemperature ( tK (k), w (k) )
               dTvK ( k ) = ptvK - tvK
               buoy ( k ) = g * ( dtvK ( k ) / tvK )
               wflag = .true.
            END IF
         ELSE
            ptK = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
            ptvK = VirtualTemperature ( ptK, srcw )
            tvK = VirtualTemperature ( tK (k), w (k) )
            dTvK ( k ) = ptvK - tvK
            buoy ( k ) = g * ( dtvK ( k ) / tvK )
         END IF
      END DO
      flag = .false.
      lfclev = -1
      nbuoy = REAL ( 0 )
      pbuoy = REAL ( 0 )
      DO k = sfc + 1, nz
         IF ( tK (k) < 253.15 ) EXIT
         CAPE = CAPE + MAX ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
         CIN = CIN + MIN ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
         IF ( flag .and. buoy (k) > REAL (0) ) THEN
            pbuoy = pbuoy + buoy (k)
         END IF
         IF ( .not. flag .and. buoy (k) > REAL (0) .and. p (k) < plcl ) THEN
            flag = .true.
            pbuoy = pbuoy + buoy (k)
            lfclev = k
         END IF
         IF ( flag .and. buoy (k) < REAL (0) ) THEN
            nbuoy = nbuoy + buoy (k)
            IF ( ABS (nbuoy) > pbuoy ) THEN
               flag = .false.
               nbuoy = REAL ( 0 )
               pbuoy = REAL ( 0 )
               lfclev = -1
            END IF
         END IF
      END DO
      DO k = sfc + 1, nz
         pm = 50000.
         pu = p ( k )
         pd = p ( k - 1 )
         IF ( pd .le. pm ) THEN
            lidx = 0.
            EXIT
         ELSEIF ( pu .le. pm .and. pd .gt. pm) THEN
            lidxu = -dTvK ( k ) * ( pu / 100000. ) ** (Rd/Cp)
            lidxd = -dTvK ( k-1 ) * ( pd / 100000. ) ** (Rd/Cp)
            lidx = ( lidxu * (pm-pd) + lidxd * (pu-pm) ) / (pu-pd)
            EXIT
         ENDIF
      END DO
      IF ( lfclev > 0 ) THEN
         PLFC = p ( lfclev )
         ZLFC = hgt ( lfclev )
      END IF
      IF ( PLFC /= PLFC .OR. PLFC < REAL (0) ) THEN
         PLFC = REAL ( -1 )
         ZLFC = REAL ( -1 )
      END IF
      IF ( CAPE /= CAPE ) cape = REAL ( 0 )
      IF ( CIN /= CIN ) cin = REAL ( 0 )
  END FUNCTION Buoyancy
  FUNCTION MSLP ( zsfc, psfc, zlev1, qlev1, tlev1 )
      implicit none
      REAL, INTENT ( IN ) :: zsfc
      REAL, INTENT ( IN ) :: psfc
      REAL, INTENT ( IN ) :: zlev1
      REAL, INTENT ( IN ) :: qlev1
      REAL, INTENT ( IN ) :: tlev1
      real,PARAMETER :: G=9.81
      real,PARAMETER :: GI=1./G
      real,PARAMETER :: RD=287.0
      real,PARAMETER :: ZSL=0.0
      real,PARAMETER :: TAUCR=RD*GI*290.66,CONST=0.005*G/RD
      real,PARAMETER :: GORD=G/RD,DP=60.E2
      real,PARAMETER :: GAMMA=6.5E-3
      real MSLP,TVRT,TVRSFC,TAUSFC,TVRSL,TAUSL,TAUAVG
         MSLP = PSFC
         TVRT = TLEV1*(1.0+0.608*QLEV1)
         TVRSFC = TVRT + (ZLEV1 - ZSFC)*GAMMA
         TAUSFC = TVRSFC*RD*GI
         TVRSL = TVRT + (ZLEV1 - ZSL)*GAMMA
         TAUSL = TVRSL*RD*GI
         IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
            TAUSL=TAUCR
         ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
            TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
         ENDIF
         TAUAVG = 0.5*(TAUSL+TAUSFC)
         MSLP = PSFC*EXP(ZSFC/TAUAVG)
  END FUNCTION MSLP
  FUNCTION calc_fits ( p, tK, rh ) RESULT ( fits )
    implicit none
    real, intent ( in ) :: p
    real, intent ( in ) :: tK
    real, intent ( in ) :: rh
    real :: fits
    real :: twb
    real :: wbt
    fits = REAL ( 0 )
    twb = WetBulbTemp ( p, tK, rh ) - 273.15
    fits = 0.8281*twb + 0.3549*( tK - 273.15 ) + 5.08
    fits = fits + 273.15
  END FUNCTION calc_fits
  FUNCTION calc_wc ( tK, wspd ) RESULT ( wc )
    implicit none
    real, intent ( in ) :: tK
    real, intent ( in ) :: wspd
    real :: tF, wc, wspd_mph
    wspd_mph = wspd * 2.23693629
    tF = (( tK - 273.15 ) * ( REAL (9) / REAL (5) ) ) + REAL ( 32 )
    wc = 35.74 &
       + ( 0.6215 * tF ) &
       - ( 35.75 * ( wspd_mph**0.16 ) ) &
       + ( 0.4275 * tF * ( wspd_mph**0.16 ) )
    wc = (( wc - REAL (32) ) * ( REAL (5) / REAL (9) ) ) + 273.15
  END FUNCTION calc_wc
  FUNCTION calc_hi ( Tk, RH ) result ( HI )
    implicit none
    real, intent ( in ) :: Tk
    real, intent ( in ) :: RH
    real :: tF, tF2, rh2, HI
    IF ( Tk > 294.26111 ) THEN
      tF = ( (Tk - 273.15) * (REAL (9)/REAL (5)) ) + REAL ( 32 )
      tF2 = tF ** 2
      rh2 = RH ** 2
      HI = -42.379 &
         + ( 2.04901523 * tF ) &
         + ( 10.14333127 * RH ) &
         - ( 0.22475541 * tF * RH ) &
         - ( 6.83783E-03 * tF2 ) &
         - ( 5.481717E-02 * rh2 ) &
         + ( 1.22874E-03 * tF2 * RH ) &
         + ( 8.5282E-04 * tF * rh2 ) &
         - ( 1.99E-06 * tF2 * rh2 )
      HI = ((HI - REAL (32)) * (REAL (5)/REAL (9))) + 273.15
    ELSE
      HI = Tk
    END IF
  END FUNCTION calc_hi
  FUNCTION WetBulbTemp ( p, tK, rh) result( wbt )
    implicit none
    real, intent ( in ) :: p
    real, intent ( in ) :: tK
    real, intent ( in ) :: rh
    real :: wbt
    real :: tdK
    real :: tC
    real :: tdC
    real :: svapr
    real :: vapr
    real :: gamma
    real :: delta
    wbt = REAL ( 0 )
    tC = tK - 273.15
    svapr = calc_es ( tK ) * REAL ( 100 )
    vapr = svapr * ( rh / REAL (100) )
    tdC = calc_Dewpoint ( tC, rh )
    tdK = tdC + 273.15
    gamma = 0.00066 * ( p / REAL (1000) )
    delta = REAL ( 4098 ) * ( vapr / REAL(1000) ) / ( (tC+237.3)**2 )
    wbt = ( ((gamma * tC) + (delta * tdC)) / (gamma + delta) ) + 273.15
  END FUNCTION WetBulbTemp
  FUNCTION calc_Dewpoint ( tC, rh) result( Dewpoint )
    implicit none
    real, intent ( in ) :: tC
    real, intent ( in ) :: rh
    real :: Dewpoint
    real :: term, es, e1, e, logs, expon
    expon = ( 7.5*tC ) / ( 237.7+tC )
    es = 6.112 * ( 10**expon )
    e = es * ( rh/100.0 )
    logs = LOG10 ( e/6.112 )
    Dewpoint = ( 237.7*logs ) / ( 7.5-logs )
  END FUNCTION calc_Dewpoint
  FUNCTION calc_es ( tK ) result ( es )
    implicit none
    real, intent ( in ) :: tK
    real :: es
    real :: p1, p2, c1
    p1 = 11.344 - 0.0303998 * tK
    p2 = 3.49149 - 1302.8844 / tK
    c1 = 23.832241 - 5.02808 * ALOG10 ( tK )
    es = 10.**(c1-1.3816E-7*10.**p1+8.1328E-3*10.**p2-2949.076/tK)
  END FUNCTION calc_es
  FUNCTION CATTurbulence ( ugrdbot, ugrdtop, vgrdbot, vgrdtop &
                           ,defor11bot, defor11top, defor12bot, defor12top &
                           ,defor22bot, defor22top, zbot, ztop ) result ( ti )
    IMPLICIT NONE
    REAL, INTENT ( IN ) :: ugrdbot
    REAL, INTENT ( IN ) :: ugrdtop
    REAL, INTENT ( IN ) :: vgrdbot
    REAL, INTENT ( IN ) :: vgrdtop
    REAL, INTENT ( IN ) :: defor11bot
    REAL, INTENT ( IN ) :: defor11top
    REAL, INTENT ( IN ) :: defor12bot
    REAL, INTENT ( IN ) :: defor12top
    REAL, INTENT ( IN ) :: defor22bot
    REAL, INTENT ( IN ) :: defor22top
    REAL, INTENT ( IN ) :: zbot
    REAL, INTENT ( IN ) :: ztop
    REAL :: ti
    REAL :: dudx, dudx1, dudx2
    REAL :: dvdy, dvdy1, dvdy2
    REAL :: dudz, dvdz
    REAL :: depth, vws, conv
    REAL :: def, shear, stretch
    ti = REAL ( 0 )
    depth = ABS ( ztop - zbot )
    dudz = ( ugrdbot - ugrdtop ) / depth
    dvdz = ( vgrdbot - vgrdtop ) / depth
    vws = SQRT ( dudz**2 + dvdz**2 )
    dudx1 = defor11top / 2.
    dudx2 = defor11bot / 2.
    dudx = ( dudx1 + dudx2 ) / REAL ( 2 )
    dvdy1 = defor22top / 2.
    dvdy2 = defor22bot / 2.
    dvdy = ( dvdy1 + dvdy2 ) / REAL ( 2 )
    stretch = dudx - dvdy
    shear = ( defor12top + defor12bot ) / REAL ( 2 )
    def = SQRT ( stretch**2 + shear**2 )
    conv = - ( dudx + dvdy )
    ti = vws * ( def + conv ) * 1.0E+07
    IF ( ti /= ti ) ti = REAL ( 0 )
    IF ( ti < 0 ) ti = REAL ( 0 )
  END FUNCTION CATTurbulence
  FUNCTION lin_interp ( x, f, y ) result ( g )
    implicit none
    real, intent(in), dimension(:) :: x
    real, intent(in), dimension(:) :: f
    real, intent(in) :: y
    real :: g
    integer :: k
    integer :: n
    real :: a
    n = size(x)
    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if
    a = ( f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))
  END FUNCTION lin_interp
  FUNCTION LLT_Windspeed ( nlayer, u, v ) RESULT ( dynamic )
    IMPLICIT NONE
    INTEGER, INTENT ( IN ) :: nlayer
    REAL, INTENT ( IN ) :: u ( nlayer )
    REAL, INTENT ( IN ) :: v ( nlayer )
    REAL :: dynamic
    INTEGER :: i
    REAL :: this_windspeed ( nlayer )
    REAL :: PI
       PARAMETER ( PI = 3.14159265359 )
    dynamic = REAL ( 0 )
    DO i = 1, nlayer
       this_windspeed ( i ) = SQRT ( u(i)**2 + v(i)**2 )
    END DO
    dynamic = ( this_windspeed(1)+this_windspeed(nlayer) ) / REAL (20)
    IF ( dynamic > REAL (2) ) dynamic = REAL ( 2 )
    dynamic = ( dynamic + REAL (3) ) / REAL ( 2 )
    dynamic = SIN ( dynamic*PI )
    dynamic = ( dynamic + REAL (1) ) / REAL ( 2 )
  END FUNCTION LLT_Windspeed
  FUNCTION LLT_Thermodynamic ( nlayer, tK, hgt ) RESULT ( thermodynamic )
  IMPLICIT NONE
    INTEGER, INTENT ( IN ) :: nlayer
    REAL, INTENT ( IN ) :: tK ( nlayer )
    REAL, INTENT ( IN ) :: hgt ( nlayer )
    REAL :: thermodynamic
    INTEGER :: i
    REAL :: lapse
    REAL :: PI
       PARAMETER ( PI = 3.14159265359 )
    thermodynamic = REAL ( 0 )
    lapse = ( tk(1) - tk(nlayer) ) * REAL ( 1000 )
    lapse = lapse / ( hgt(nlayer) - hgt(1) )
    thermodynamic = lapse / REAL ( 10 )
    thermodynamic = ( thermodynamic + REAL (3) ) / REAL ( 2 )
    thermodynamic = SIN ( thermodynamic * PI )
    thermodynamic = ( thermodynamic + REAL (1) ) / REAL ( 2 )
  END FUNCTION LLT_Thermodynamic
  FUNCTION LLT_MountainWave ( nlayer, tdx, tdy, u, v, tK, hgt) &
                                                         RESULT ( MountainWave )
    IMPLICIT NONE
    INTEGER, INTENT ( IN ) :: nlayer
    REAL, INTENT ( IN ) :: tdx
    REAL, INTENT ( IN ) :: tdy
    REAL, INTENT ( IN ) :: u ( nlayer )
    REAL, INTENT ( IN ) :: v ( nlayer )
    REAL, INTENT ( IN ) :: tK ( nlayer )
    REAL, INTENT ( IN ) :: hgt ( nlayer )
    REAL :: MountainWave
    REAL :: u_term
    REAL :: v_term
    REAL :: uv_term
    REAL :: lapse
    REAL :: total_mw, this_total_mw
    REAL :: this_uv_term
    REAL :: min_uv_term, cross_terrain, max_total_mw
    INTEGER :: i, j, k
    REAL :: PI
       PARAMETER ( PI = 3.14159265359 )
    MountainWave = REAL ( 0 )
    DO i = 2, nlayer
      u_term = ( (u(i-1) + u(i) ) / REAL(2) ) * tdx
      v_term = ( (v(i-1) + v(i) ) / REAL(2) ) * tdy
      this_uv_term = ( u_term + v_term ) * REAL ( -1 )
      IF ( min_uv_term < REAL (0) ) min_uv_term = REAL ( 0 )
      IF ( i == 2 ) THEN
        min_uv_term = this_uv_term
      ELSE
        IF ( this_uv_term < min_uv_term ) min_uv_term = this_uv_term
      END IF
      lapse = ( tK (i-1) - tK (i) ) * REAL ( 1000 )
      lapse = lapse / ABS ( hgt(i)-hgt(i-1) )
      IF ( lapse > REAL (0) ) lapse = REAL ( 0 )
      lapse = lapse * REAL ( -1 )
      this_total_mw = this_uv_term * lapse * REAL ( 40000 )
      IF ( i == 2 ) THEN
        total_mw = this_total_mw
      ELSE
        IF ( this_total_mw > total_mw ) total_mw = this_total_mw
      END IF
    END DO
    cross_terrain = min_uv_term * REAL ( 500 )
    IF ( min_uv_term < 0.03 ) THEN
      cross_terrain = REAL ( 0 )
    END IF
    IF ( cross_terrain > REAL (50) ) cross_terrain = REAL ( 50 )
    IF ( total_mw > REAL (50) ) total_mw = REAL ( 50 )
    MountainWave = ( total_mw*(cross_terrain/50.) ) + cross_terrain
    MountainWave = MountainWave / REAL ( 100 )
  END FUNCTION LLT_MountainWave
  FUNCTION LLT_TrappedWave ( nlayer, u, v, p ) RESULT ( trapped )
     IMPLICIT NONE
     INTEGER, INTENT ( IN ) :: nlayer
     REAL, INTENT ( IN ) :: u ( nlayer )
     REAL, INTENT ( IN ) :: v ( nlayer )
     REAL, INTENT ( IN ) :: p ( nlayer )
     REAL :: trapped
     INTEGER :: i
     REAL :: du, dv
     REAL :: scale_fact, this_p
     REAL :: dudv, this_dudv
     REAL :: PI
        PARAMETER ( PI = 3.14159265359 )
     REAL, PARAMETER :: scale_950 = 0.050000
     REAL, PARAMETER :: scale_925 = 0.040000
     REAL, PARAMETER :: scale_900 = 0.025000
     REAL, PARAMETER :: scale_850 = 0.010000
     REAL, PARAMETER :: scale_800 = 0.005000
     REAL, PARAMETER :: scale_750 = 0.002941
     REAL, PARAMETER :: scale_700 = 0.001923
     REAL, PARAMETER :: scale_650 = 0.001351
     REAL, PARAMETER :: scale_600 = 0.001000
     REAL, PARAMETER :: scale_550 = 0.000800
     trapped = REAL ( 0 )
     dudv = REAL ( 0 )
     DO i = 2, nlayer
       du = u ( i-1 ) - u ( i )
       dv = v ( i-1 ) - v ( i )
       this_p = p ( i ) / REAL ( 100 )
       IF ( this_p > REAL (950) ) THEN
         scale_fact = scale_950
       ELSE IF ( this_p <= REAL (950) .AND. this_p > REAL (925) ) THEN
         scale_fact = scale_925
       ELSE IF ( this_p <= REAL (925) .AND. this_p > REAL (900) ) THEN
         scale_fact = scale_900
       ELSE IF ( this_p <= REAL (900) .AND. this_p > REAL (850) ) THEN
         scale_fact = scale_850
       ELSE IF ( this_p <= REAL (850) .AND. this_p > REAL (800) ) THEN
         scale_fact = scale_800
       ELSE IF ( this_p <= REAL (800) .AND. this_p > REAL (750) ) THEN
         scale_fact = scale_750
       ELSE IF ( this_p <= REAL (750) .AND. this_p > REAL (700) ) THEN
         scale_fact = scale_700
       ELSE IF ( this_p <= REAL (700) .AND. this_p > REAL (650) ) THEN
         scale_fact = scale_650
       ELSE IF ( this_p <= REAL (650) .AND. this_p > REAL (600) ) THEN
         scale_fact = scale_600
       ELSE IF ( this_p <= REAL (600) ) THEN
         scale_fact = scale_550
       END IF
       this_dudv = ( (du**2)*(dv**2) ) * scale_fact
       IF ( this_dudv > dudv ) dudv = this_dudv
    END DO
    trapped = dudv
    IF ( trapped > REAL ( 1 ) ) trapped = REAL ( 1 )
    trapped = trapped / REAL ( 4 )
  END FUNCTION LLT_TrappedWave
END MODULE diag_functions
MODULE module_diag_afwa
    USE diag_functions
CONTAINS
  SUBROUTINE afwa_diagnostics_driver ( grid , config_flags &
                             , moist &
                             , scalar &
                             , chem &
                             , th_phy , pi_phy , p_phy &
                             , u_phy , v_phy &
                             , dz8w , p8w , t8w , rho_phy , rho &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe &
                             , its, ite, jts, jte &
                             , k_start, k_end )
    USE module_domain, ONLY : domain , domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
    USE module_utility
    USE module_streams, ONLY: history_alarm, auxhist2_alarm
    USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
    IMPLICIT NONE
    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER :: k_start , k_end, its, ite, jts, jte
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist), &
         INTENT(IN ) :: moist
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_scalar), &
         INTENT(IN ) :: scalar
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_chem), &
         INTENT(IN ) :: chem
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: th_phy &
                                              , pi_phy &
                                              , p_phy &
                                              , u_phy &
                                              , v_phy &
                                              , dz8w &
                                              , p8w &
                                              , t8w &
                                              , rho_phy &
                                              , rho
    CHARACTER*512 :: message
    CHARACTER*256 :: timestr
    INTEGER :: i,j,k,nz,ostat
    INTEGER :: icing_opt
    REAL :: bdump
    INTEGER :: i_start, i_end, j_start, j_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qrain &
                                              , qsnow &
                                              , qgrpl &
                                              , qvapr &
                                              , qcloud &
                                              , qice &
                                              , ncloud &
                                              , ngraup &
                                              , rh &
                                              , rh_cld &
                                              , ptot &
                                              , z_e &
                                              , zagl
    REAL, DIMENSION( ims:ime, jms:jme, 5 ) :: dustc
    REAL, DIMENSION( ims:ime, jms:jme ) :: rh2m &
                                              , rh20m &
                                              , tv2m &
                                              , tv20m &
                                              , wind10m &
                                              , wup_mask &
                                              , wind125m &
                                              , llws &
                                              , pwater
    LOGICAL :: do_buoy_calc
    REAL :: zlfc_msl, dum1, dum2, dum3, wind_vel, wind_blend
    REAL :: prate_mm_per_hr, factor
    REAL :: u1km, v1km, ublend, vblend, u2000, v2000, us, vs
    LOGICAL :: is_target_level
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime, StartTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep, is_first_timestep
    write ( message, * ) 'inside afwa_diagnostics_driver'
    CALL wrf_debug( 100 , message )
    CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time, &
         ringinterval=histint)
    CALL WRFU_ALARMGET( grid%alarms( AUXHIST2_ALARM ), prevringtime=aux2_time, &
         ringinterval=aux2int)
    CALL domain_clock_get ( grid, current_time=CurrTime, &
         simulationStartTime=StartTime, &
         current_timestr=timestr, time_step=dtint )
    is_after_history_dump = ( Currtime .lt. hist_time + dtint )
    is_output_timestep = (Currtime .ge. hist_time + histint - dtint .or. &
                         Currtime .ge. aux2_time + aux2int - dtint )
    write ( message, * ) 'is output timestep? ', is_output_timestep
    CALL wrf_debug( 100 , message )
    is_first_timestep = ( Currtime .eq. StartTime + dtint )
    IF ( config_flags%afwa_bad_data_check .GT. 0 ) THEN
      IF ( ( is_output_timestep ) .AND. ( .NOT. is_first_timestep ) ) THEN
        DO i=its, MIN( ide-1, ite )
          DO k=k_start, k_end
            DO j=jts, MIN( jde-1, jte )
              IF ( ( u_phy(i,k,j) .GT. 300. ) .OR. &
                   ( u_phy(i,k,j) .LT. -300. ) .OR. &
                   ( v_phy(i,k,j) .GT. 300. ) .OR. &
                   ( v_phy(i,k,j) .LT. -300. ) .OR. &
                   ( th_phy(i,k,j) .GT. 9999. ) .OR. &
                   ( th_phy(i,k,j) .LT. 99. ) ) THEN
                write ( message, * ) "AFWA Diagnostics: ERROR - Model winds and/or " // &
                "potential temperature appear to be bad. If you do not want this check, " // &
                "set afwa_bad_data_check=0. i=",i,", j=",j,", k=",k,", u_phy=",u_phy(i,k,j), &
                ", v_phy=", v_phy(i,k,j),", th_phy=",th_phy(i,k,j)
                CALL wrf_error_fatal3("<stdin>",1550,&
message )
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          qvapr(i,k,j) = moist(i,k,j,P_QV)
          qrain(i,k,j) = moist(i,k,j,P_QR)
          qsnow(i,k,j) = moist(i,k,j,P_QS)
          qgrpl(i,k,j) = moist(i,k,j,P_QG)
          qcloud(i,k,j) = moist(i,k,j,P_QC)
          qice(i,k,j) = moist(i,k,j,P_QI)
          ncloud(i,k,j) = scalar(i,k,j,P_QNC)
        ENDDO
      ENDDO
    ENDDO
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          zagl(i,k,j)=grid%z(i,k,j)-grid%ht(i,j)
        ENDDO
      ENDDO
    ENDDO
    DO i=ims,ime
      DO k=kms,kme
        DO j=jms,jme
          rh(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j))
          rh_cld(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j)+qcloud(i,k,j)+qice(i,k,j))
        ENDDO
      ENDDO
    ENDDO
    DO i=ims,ime
      DO j=jms,jme
        grid % afwa_precip(i,j) = grid%raincv(i,j) + grid%rainncv(i,j)
        grid % afwa_totprecip(i,j) = grid%rainc(i,j) + grid%rainnc(i,j)
      ENDDO
    ENDDO
    nz=kme-kms+1
    DO i=ims,ime
      DO j=jms,jme
        grid % afwa_pwat ( i, j ) = Pwat( nz, &
                                          qvapr(i,kms:kme,j), &
                                          qcloud(i,kms:kme,j), &
                                          dz8w(i,kms:kme,j), &
                                          rho(i,kms:kme,j) )
      ENDDO
    ENDDO
    IF ( is_after_history_dump ) THEN
      DO j = jms, jme
        DO i = ims, ime
          grid % wspd10max(i,j) = 0.
          grid % afwa_llws(i,j) = 0.
        ENDDO
      ENDDO
    ENDIF
    DO j = jms, jme
      DO i = ims, ime
        wind_vel = uv_wind ( grid % u10(i,j) , grid % v10(i,j) )
        prate_mm_per_hr = ( grid % afwa_precip(i,j) / grid % dt ) * 3600.
        IF ( prate_mm_per_hr .GT. 50. ) THEN
          is_target_level=.false.
          DO k=kms,kme
            IF ( ( zagl(i,k,j) >= 1000. ) .and. &
                 ( .NOT. is_target_level ) .and. &
                 ( k .ne. kms ) ) THEN
              is_target_level = .true.
              u1km = u_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                     * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
              v1km = v_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                     * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
              EXIT
            ENDIF
          ENDDO
          factor = MAX ( ( ( 150. - prate_mm_per_hr ) / 100. ), 0. )
          ublend = grid % u10(i,j) * factor + u1km * (1. - factor)
          vblend = grid % v10(i,j) * factor + v1km * (1. - factor)
          wind_blend = uv_wind ( ublend, vblend )
          IF ( wind_blend .GT. wind_vel ) THEN
            wind_vel = wind_blend
          ENDIF
        ENDIF
        IF ( wind_vel .GT. grid % wspd10max(i,j) ) THEN
          grid % wspd10max(i,j) = wind_vel
        ENDIF
      ENDDO
    ENDDO
    DO j = jts, jte
      DO i = its, ite
        is_target_level=.false.
        DO k=kms,kme
          IF ( ( zagl(i,k,j) >= 609.6 ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            u2000 = u_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
            v2000 = v_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
            us = u2000 - grid % u10(i,j)
            vs = v2000 - grid % v10(i,j)
            llws(i,j) = uv_wind ( us , vs )
            IF ( llws(i,j) .gt. grid % afwa_llws(i,j) ) THEN
              grid % afwa_llws(i,j) = llws(i,j)
            ENDIF
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    dustc(ims:ime,jms:jme,:)=0.
    IF ( config_flags % afwa_severe_opt == 1 ) THEN
      IF ( is_after_history_dump ) THEN
        DO j = jms, jme
          DO i = ims, ime
            grid%w_up_max(i,j) = 0.
            grid%w_dn_max(i,j) = 0.
            grid%tcoli_max(i,j) = 0.
            grid%grpl_flx_max(i,j) = 0.
            grid%up_heli_max(i,j) = 0.
            grid%afwa_tornado(i,j) = 0.
            grid%midrh_min_old(i,j) = grid%midrh_min(i,j)
            grid%midrh_min(i,j) = 999.
            grid%afwa_hail(i,j) = 0.
          ENDDO
        ENDDO
      ENDIF
      IF ( ( is_first_timestep ) .OR. ( is_output_timestep ) ) THEN
        do_buoy_calc = .true.
      ELSE
        do_buoy_calc = .false.
      ENDIF
      i_start = its
      i_end = ite
      j_start = jts
      j_end = jte
      IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
           config_flags%nested) i_start = MAX( ids+1, its )
      IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
           config_flags%nested) i_end = MIN( ide-1, ite )
      IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
           config_flags%nested) j_start = MAX( jds+1, jts )
      IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
           config_flags%nested) j_end = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
      CALL severe_wx_diagnostics ( grid % wspd10max &
                             , grid % w_up_max &
                             , grid % w_dn_max &
                             , grid % up_heli_max &
                             , grid % tcoli_max &
                             , grid % midrh_min_old &
                             , grid % midrh_min &
                             , grid % afwa_hail &
                             , grid % afwa_cape &
                             , grid % afwa_cin &
                             , grid % afwa_zlfc &
                             , grid % afwa_plfc &
                             , grid % afwa_lidx &
                             , llws &
                             , grid % afwa_tornado &
                             , grid % grpl_flx_max &
                             , grid % u10 &
                             , grid % v10 &
                             , grid % w_2 &
                             , grid % uh &
                             , grid % t_phy &
                             , grid % t2 &
                             , grid % z &
                             , grid % ht &
                             , grid % tornado_mask &
                             , grid % tornado_dur &
                             , grid % dt &
                             , grid % afwa_pwat &
                             , u_phy &
                             , v_phy &
                             , ptot &
                             , qice &
                             , qsnow &
                             , qgrpl &
                             , ngraup &
                             , qvapr, qrain, qcloud &
                             , rho &
                             , dz8w &
                             , rh &
                             , do_buoy_calc &
                             , ims, ime, jms, jme, kms, kme &
                             , its, ite, jts, jte &
                             , k_start, k_end &
                             , j_start, j_end, i_start, i_end )
    ENDIF
    IF ( config_flags % afwa_ptype_opt == 1 ) THEN
      IF ( grid % itimestep .eq. 1) THEN
        DO i=ims,ime
          DO j=jms,jme
            grid % afwa_rain(i,j)=0.
            grid % afwa_snow(i,j)=0.
            grid % afwa_ice(i,j)=0.
            grid % afwa_fzra(i,j)=0.
            grid % afwa_snowfall(i,j)=0.
          ENDDO
        ENDDO
      ENDIF
      CALL precip_type_diagnostics ( grid % t_phy &
                             , grid % t2 &
                             , rh &
                             , grid % z &
                             , grid % ht &
                             , grid % afwa_precip &
                             , grid % swdown &
                             , grid % afwa_rain &
                             , grid % afwa_snow &
                             , grid % afwa_ice &
                             , grid % afwa_fzra &
                             , grid % afwa_snowfall &
                             , grid % afwa_ptype_ccn_tmp &
                             , grid % afwa_ptype_tot_melt &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    ENDIF
    IF ( is_output_timestep ) THEN
      DO j = jms, jme
        DO i = ims, ime
          grid % afwa_mslp ( i, j ) = MSLP ( grid % ht ( i, j ) &
                                              , grid % psfc ( i, j ) &
                                              , grid % z ( i, kms, j ) &
                                              , qvapr ( i, kms, j ) &
                                              , grid % t_phy ( i, kms, j ) )
        ENDDO
      ENDDO
      DO i=ims,ime
        DO j=jms,jme
          wind10m(i,j)=uv_wind(grid%u10(i,j),grid%v10(i,j))
        ENDDO
      ENDDO
      DO i=ims,ime
        DO j=jms,jme
          rh2m(i,j)=calc_rh(grid%psfc(i,j), grid%t2(i,j), grid%q2(i,j))
          tv2m(i,j)=grid%t2(i,j) * (1 + 0.61 * grid%q2(i,j))
        ENDDO
      ENDDO
      IF ( config_flags % afwa_buoy_opt == 1 ) THEN
        nz = k_end - k_start + 1
        DO j = jts, jte
          DO i = its, ite
            ostat = Buoyancy ( nz &
                                     , grid%t_phy(i,kms:kme,j) &
                                     , rh(i,kms:kme,j) &
                                     , ptot(i,kms:kme,j) &
                                     , grid % z(i,kms:kme,j) &
                                     , 1 &
                                     , grid % afwa_cape(i,j) &
                                     , grid % afwa_cin(i,j) &
                                     , zlfc_msl &
                                     , grid % afwa_plfc(i,j) &
                                     , grid % afwa_lidx(i,j) &
                                     , 3 )
            IF ( zlfc_msl .ge. grid % ht ( i, j ) ) THEN
              grid % afwa_zlfc ( i, j ) = zlfc_msl - grid % ht ( i, j )
            ELSE
              grid % afwa_zlfc( i, j ) = -1.
            ENDIF
            IF ( grid % afwa_lidx ( i, j ) .ne. 999. ) THEN
              grid % afwa_lidx ( i, j ) = grid % afwa_lidx ( i, j ) + 273.15
            ENDIF
            ostat = Buoyancy ( nz &
                                     , grid%t_phy(i,kms:kme,j) &
                                     , rh(i,kms:kme,j) &
                                     , ptot(i,kms:kme,j) &
                                     , grid % z(i,kms:kme,j) &
                                     , 1 &
                                     , grid % afwa_cape_mu(i,j) &
                                     , grid % afwa_cin_mu(i,j) &
                                     , dum1 &
                                     , dum2 &
                                     , dum3 &
                                     , 1 )
          ENDDO
        ENDDO
      ENDIF
      IF ( config_flags % afwa_therm_opt == 1 ) THEN
        write ( message, * ) 'Calculating thermal indices'
        CALL wrf_debug( 100 , message )
        CALL thermal_diagnostics ( grid % t2 &
                                 , grid % psfc &
                                 , rh2m &
                                 , wind10m &
                                 , grid % afwa_heatidx &
                                 , grid % afwa_wchill &
                                 , grid % afwa_fits &
                                 , ids, ide, jds, jde, kds, kde &
                                 , ims, ime, jms, jme, kms, kme &
                                 , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_turb_opt == 1 ) THEN
        write ( message, * ) 'Calculating turbulence indices'
        grid % afwa_tlyrbot = (/ 1500., 3000., 4600., 6100., 7600., 9100., &
                                  10700. /)
        grid % afwa_tlyrtop = (/ 3000., 4600., 6100., 7600., 9100., 10700., &
                                  12700. /)
        call turbulence_diagnostics ( u_phy &
                             , v_phy &
                             , grid % t_phy &
                             , ptot &
                             , zagl &
                             , grid % defor11 &
                             , grid % defor12 &
                             , grid % defor22 &
                             , grid % afwa_turb &
                             , grid % afwa_llturb &
                             , grid % afwa_llturblgt &
                             , grid % afwa_llturbmdt &
                             , grid % afwa_llturbsvr &
                             , 7 &
                             , grid % afwa_tlyrbot &
                             , grid % afwa_tlyrtop &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_radar_opt == 1 .or. &
         config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating Radar'
        CALL wrf_debug( 100 , message )
        CALL wrf_dbzcalc ( rho &
                             , grid%t_phy &
                             , qrain &
                             , qsnow &
                             , qgrpl &
                             , z_e &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_radar_opt == 1 ) THEN
        write ( message, * ) 'Calculating derived radar variables'
        CALL wrf_debug( 100 , message )
        CALL radar_diagnostics ( grid % refd &
                             , grid % refd_com &
                             , grid % echotop &
                             , grid % z &
                             , z_e &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating VIL'
        CALL wrf_debug( 100 , message )
        CALL vert_int_liquid_diagnostics ( grid % vil &
                             , grid % radarvil &
                             , grid % t_phy &
                             , qrain &
                             , qsnow &
                             , qgrpl &
                             , z_e &
                             , dz8w &
                             , rho &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_icing_opt == 1 ) THEN
        IF ( config_flags % mp_physics == GSFCGCESCHEME ) THEN
          icing_opt=1
        ELSEIF ( config_flags % mp_physics == ETAMPNEW ) THEN
          icing_opt=2
        ELSEIF ( config_flags % mp_physics == THOMPSON ) THEN
          icing_opt=3
        ELSEIF ( config_flags % mp_physics == WSM5SCHEME .OR. &
                 config_flags % mp_physics == WSM6SCHEME ) THEN
          icing_opt=4
        ELSEIF ( config_flags % mp_physics == MORR_TWO_MOMENT ) THEN
          IF (config_flags % progn > 0) THEN
             icing_opt=6
          ELSE
             icing_opt=5
          ENDIF
        ELSEIF ( config_flags % mp_physics == WDM6SCHEME ) THEN
          icing_opt=7
        ELSE
          icing_opt=0
        ENDIF
        write ( message, * ) 'Calculating Icing with icing opt ',icing_opt
        CALL wrf_debug( 100 , message )
        CALL icing_diagnostics ( icing_opt &
                             , grid % fzlev &
                             , grid % icing_lg &
                             , grid % icing_sm &
                             , grid % qicing_lg_max &
                             , grid % qicing_sm_max &
                             , grid % qicing_lg &
                             , grid % qicing_sm &
                             , grid % icingtop &
                             , grid % icingbot &
                             , grid % t_phy &
                             , grid % z &
                             , dz8w &
                             , rho &
                             , qrain &
                             , qcloud &
                             , ncloud &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_vis_opt == 1 ) THEN
        DO i=ims,ime
          DO j=jms,jme
            tv20m(i,j) = -999.
            rh20m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (tv20m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 20.) THEN
                IF (k .eq. kps) THEN
                  tv20m(i,j) = tv2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - tv2m(i,j))
                  rh20m(i,j) = rh2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (rh(i,k,j) - rh2m(i,j))
                ELSE
                  tv20m(i,j) = grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)) + &
                               ((20. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - &
                               grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)))
                  rh20m(i,j) = rh (i,k-1,j) + &
                               ((20. - zagl (i,k-1,j)) / &
                               (zagl (i,k,j) - zagl (i,k-1,j))) * &
                               (rh (i,k,j) - rh (i,k-1,j))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        DO i=ims,ime
          DO j=jms,jme
            wind125m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (wind125m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 125.) THEN
                IF (k .eq. kps) THEN
                  wind125m(i,j) = uv_wind(u_phy(i,k,j),v_phy(i,k,j))
                ELSE
                  wind125m(i,j) = uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)) + &
                               ((125. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (uv_wind(u_phy(i,k,j),v_phy(i,k,j)) - &
                               uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        write ( message, * ) 'Calculating visibility'
        CALL wrf_debug( 100 , message )
        CALL vis_diagnostics ( qcloud(ims:ime,k_start,jms:jme) &
                             , qrain(ims:ime,k_start,jms:jme) &
                             , qice(ims:ime,k_start,jms:jme) &
                             , qsnow(ims:ime,k_start,jms:jme) &
                             , qgrpl(ims:ime,k_start,jms:jme) &
                             , rho(ims:ime,k_start,jms:jme) &
                             , wind10m &
                             , wind125m &
                             , grid % afwa_pwat &
                             , grid % q2 &
                             , rh2m &
                             , rh20m &
                             , tv2m &
                             , tv20m &
                             , dustc &
                             , grid % afwa_vis &
                             , grid % afwa_vis_dust &
                             , grid % afwa_vis_alpha &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
      IF ( config_flags % afwa_cloud_opt == 1 ) THEN
        write ( message, * ) 'Calculating cloud'
        CALL wrf_debug( 100 , message )
        CALL cloud_diagnostics (qcloud &
                             , qice &
                             , qsnow &
                             , rh_cld &
                             , dz8w &
                             , rho &
                             , grid % z &
                             , grid % ht &
                             , grid % afwa_cloud &
                             , grid % afwa_cloud_ceil &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF
    ENDIF
  END SUBROUTINE afwa_diagnostics_driver
  SUBROUTINE severe_wx_diagnostics ( wspd10max &
                             , w_up_max &
                             , w_dn_max &
                             , up_heli_max &
                             , tcoli_max &
                             , midrh_min_old &
                             , midrh_min &
                             , afwa_hail &
                             , cape &
                             , cin &
                             , zlfc &
                             , plfc &
                             , lidx &
                             , llws &
                             , afwa_tornado &
                             , grpl_flx_max &
                             , u10 &
                             , v10 &
                             , w_2 &
                             , uh &
                             , t_phy &
                             , t2 &
                             , z &
                             , ht &
                             , tornado_mask &
                             , tornado_dur &
                             , dt &
                             , pwat &
                             , u_phy &
                             , v_phy &
                             , p &
                             , qi &
                             , qs &
                             , qg &
                             , ng &
                             , qv, qr, qc &
                             , rho &
                             , dz8w &
                             , rh &
                             , do_buoy_calc &
                             , ims, ime, jms, jme, kms, kme &
                             , its, ite, jts, jte &
                             , k_start, k_end &
                             , j_start, j_end, i_start, i_end )
    INTEGER, INTENT(IN) :: its, ite, jts, jte, k_start, k_end &
                         , ims, ime, jms, jme, kms, kme &
                         , j_start, j_end, i_start, i_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: p &
                                              , w_2 &
                                              , t_phy &
                                              , u_phy &
                                              , v_phy &
                                              , qi &
                                              , qs &
                                              , qg &
                                              , ng &
                                              , qv, qr, qc &
                                              , rho &
                                              , z &
                                              , dz8w &
                                              , rh
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN ) :: u10 &
                                              , v10 &
                                              , wspd10max &
                                              , uh &
                                              , t2 &
                                              , ht &
                                              , midrh_min_old &
                                              , up_heli_max &
                                              , llws &
                                              , pwat
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(INOUT) :: w_up_max &
                                              , w_dn_max &
                                              , tcoli_max &
                                              , midrh_min &
                                              , afwa_hail &
                                              , afwa_tornado &
                                              , grpl_flx_max &
                                              , tornado_mask &
                                              , tornado_dur
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(INOUT) :: cape &
                                              , cin &
                                              , zlfc &
                                              , plfc &
                                              , lidx
    REAL, INTENT(IN) :: dt
    LOGICAL, INTENT(IN) :: do_buoy_calc
    INTEGER :: i,j,k
    INTEGER :: kts,kte
    REAL :: zagl, zlfc_msl, melt_term, midrh_term, hail, midrh
    REAL :: dum1, dum2, dum3
    REAL :: tornado, lfc_term, shr_term, midrh2_term, uh_term
    REAL :: wind_vel, p_tot, tcoli, grpl_flx, w_n15, qg_n15
    INTEGER :: nz, ostat
    REAL, DIMENSION( ims:ime, jms:jme ) :: w_up &
                                              , w_dn &
                                           , tornado_mask_prev &
                                           , tornado_dur_prev
    REAL :: time_factor, time_factor_prev
    LOGICAL :: is_target_level
    DO i=ims,ime
      DO j=jms,jme
        is_target_level=.false.
        DO k=kms,kme
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 3500. ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            midrh = rh(i,k-1,j) + (3500. - (z(i,k-1,j) - ht(i,j))) &
                    * ((rh(i,k,j) - rh(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            IF ( midrh .lt. midrh_min(i,j) ) THEN
              midrh_min(i,j) = midrh
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    w_up=0.
    w_dn=0.
    DO j = jts, jte
      DO k = k_start, k_end
        DO i = its, ite
          p_tot = p(i,k,j) / 100.
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up(i,j) ) THEN
            w_up(i,j) = w_2(i,k,j)
            IF ( w_up(i,j) .GT. w_up_max(i,j) ) THEN
              w_up_max(i,j) = w_up(i,j)
            ENDIF
          ENDIF
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn(i,j) ) THEN
            w_dn(i,j) = w_2(i,k,j)
            IF ( w_dn(i,j) .LT. w_dn_max(i,j) ) THEN
              w_dn_max(i,j) = w_dn(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    DO j = jts, jte
      DO i = its, ite
        melt_term=max(t2(i,j)-288.15,0.)
        midrh_term=max(2*(min(midrh_min(i,j),midrh_min_old(i,j))-70.),0.)
        hail=max((w_up(i,j)/1.4)**1.1-melt_term-midrh_term,0.)
        hail=hail*((uh(i,j)/100)+0.25)
        IF ( hail .gt. afwa_hail(i,j) ) THEN
          afwa_hail(i,j)=hail
        ENDIF
      ENDDO
    ENDDO
    DO j = jts, jte
      DO i = its, ite
        tcoli=0.
        DO k = k_start, k_end
          tcoli = tcoli + &
          (qi (i,k,j) + &
           qs (i,k,j) + &
           qg (i,k,j)) &
           *dz8w (i,k,j) * rho(i,k,j)
        ENDDO
        IF ( tcoli .GT. tcoli_max(i,j) ) THEN
          tcoli_max(i,j) = tcoli
        ENDIF
      ENDDO
    ENDDO
    DO j = jts, jte
      DO i = its, ite
        grpl_flx=0
        w_n15=-999.
        DO k = k_start, k_end
          IF ( t_phy (i,k,j) .LE. 258.15 .AND. w_n15 .EQ. -999. .AND. &
               k .GT. k_start .AND. qg (i,k,j) .GT. 1.E-20 ) THEN
            w_n15 = w_2 (i,k,j)
            qg_n15 = 1000. * qg (i,k,j)
            grpl_flx = qg_n15 * w_n15
          ENDIF
        ENDDO
        IF ( grpl_flx .GT. grpl_flx_max(i,j) ) THEN
          grpl_flx_max(i,j) = grpl_flx
        ENDIF
      ENDDO
    ENDDO
    IF ( do_buoy_calc ) THEN
      nz = k_end - k_start + 1
      DO j = jts, jte
        DO i = its, ite
          ostat = Buoyancy ( nz &
                                       , t_phy(i,kms:kme ,j) &
                                       , rh(i,kms:kme ,j) &
                                       , p(i,kms:kme ,j) &
                                       , z(i,kms:kme ,j) &
                                       , 1 &
                                       , cape(i,j) &
                                       , cin(i,j) &
                                       , zlfc_msl &
                                       , plfc(i,j) &
                                       , lidx(i,j) &
                                       , 3 )
          IF ( lidx ( i, j ) .ne. 999. ) lidx ( i, j ) = lidx ( i, j ) + 273.15
          IF ( zlfc_msl .ge. 0. ) THEN
            zlfc ( i, j ) = zlfc_msl - ht ( i, j )
          ELSE
            zlfc( i, j ) = -1.
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    tornado_dur_prev(:,:)=tornado_dur(:,:)
    tornado_mask_prev(:,:)=tornado_mask(:,:)
    tornado_mask(:,:)=0.
    tornado_dur(:,:)=0.
    DO j = j_start, j_end
      DO i = i_start, i_end
        tornado = 0.
        IF ( zlfc(i,j) .ge. 0. ) THEN
          uh_term = min(max((uh(i,j) - 25.) / 50., 0.), 1.)
          shr_term = min(max((llws(i,j) - 2.) / 10., 0.), 1.)
          lfc_term = min(max((3000. - zlfc(i,j)) / 1500., 0.), 1.)
          midrh2_term = min(max((90. - &
                        min(midrh_min(i,j),midrh_min_old(i,j))) / 30., 0.), 1.)
          tornado = 50. * uh_term * shr_term * lfc_term * midrh2_term
        ENDIF
        IF (tornado .GT. 0.) THEN
          tornado_mask(i,j) = 1.
        ENDIF
        IF ( ( tornado_mask(i,j) .GT. 0.5 ) .OR. &
           ( MAXVAL(tornado_mask_prev(i-1:i+1,j-1:j+1)) .GT. 0.5 ) ) THEN
          tornado_dur(i,j) = MAXVAL(tornado_dur_prev(i-1:i+1,j-1:j+1)) + dt
        ENDIF
        time_factor = MIN(tornado_dur(i,j)/900.,1.)
        tornado = tornado * time_factor
        IF ( tornado .GT. afwa_tornado(i,j) ) THEN
          afwa_tornado(i,j) = tornado
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE severe_wx_diagnostics
  SUBROUTINE vert_int_liquid_diagnostics ( vil &
                             , radarvil &
                             , t_phy &
                             , qrain &
                             , qsnow &
                             , qgrpl &
                             , z_e &
                             , dz8w &
                             , rho &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN) :: rho &
                                              , qrain &
                                              , qsnow &
                                              , qgrpl &
                                              , t_phy &
                                              , z_e &
                                              , dz8w
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(INOUT) :: vil &
                                              , radarvil
    INTEGER :: i,j,k,ktime
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      vil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        vil (i,j) = vil (i,j) + &
         (qrain (i,k,j) + &
          qsnow (i,k,j) + &
          qgrpl (i,k,j)) &
          *dz8w (i,k,j) * rho(i,k,j)
      ENDDO
    ENDDO
    ENDDO
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      radarvil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        radarvil (i,j) = radarvil (i,j) + &
        0.00344 * z_e(i,k,j)**0.57143 &
        *dz8w (i,k,j)/1000.0
      END DO
    END DO
    END DO
  END SUBROUTINE vert_int_liquid_diagnostics
  SUBROUTINE icing_diagnostics ( icing_opt &
                             , fzlev &
                             , icing_lg &
                             , icing_sm &
                             , qicing_lg_max &
                             , qicing_sm_max &
                             , qicing_lg &
                             , qicing_sm &
                             , icingtop &
                             , icingbot &
                             , t_phy &
                             , z &
                             , dz8w &
                             , rho &
                             , qrain &
                             , qcloud &
                             , ncloud &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(IN) :: icing_opt
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN) :: z &
                                              , qrain &
                                              , qcloud &
                                              , ncloud &
                                              , rho &
                                              , dz8w &
                                              , t_phy
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT( OUT) :: fzlev &
                                              , icing_lg &
                                              , icing_sm &
                                              , qicing_lg_max &
                                              , qicing_sm_max &
                                              , icingtop &
                                              , icingbot
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT( OUT) :: qicing_lg &
                                              , qicing_sm
    INTEGER :: i,j,k,ktime,ktop,kbot
    REAL :: qcfrac_lg, qcfrac_sm, qc, qr, small, all
    fzlev (ips:ipe,jps:jpe) = -999.
    icingtop (ips:ipe,jps:jpe) = -999.
    icingbot (ips:ipe,jps:jpe) = -999.
    icing_lg (ips:ipe,jps:jpe) = 0.
    icing_sm (ips:ipe,jps:jpe) = 0.
    qicing_lg_max (ips:ipe,jps:jpe) = 0.
    qicing_sm_max (ips:ipe,jps:jpe) = 0.
    qicing_sm(ips:ipe,kps:kpe,jps:jpe)=0.
    qicing_lg(ips:ipe,kps:kpe,jps:jpe)=0.
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      ktop=-1
      kbot=-1
      DO k = kps, MIN(kpe,kde-1)
        IF (t_phy(i,k,j) .lt. 273.15) THEN
          qc = qcloud (i,k,j)
          qr = qrain (i,k,j)
          nc = ncloud(i,k,j)
          den = rho(i,k,j)
          qcfrac_lg = 0.
          qcfrac_sm = 0.
          IF (icing_opt .eq. 2) THEN
            IF (qc .lt. 2.5E-4) THEN
              qcfrac_lg = 395000. * qc**2. + 102.9 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 276.1 * qc - 0.01861
            ELSE
              qcfrac_lg = 0.3 * log(641.789 * qc) + 0.4
            ENDIF
          ELSEIF ((icing_opt .eq. 3) .OR. (icing_opt .eq. 4)) THEN
            IF (qc .lt. 5.E-4) THEN
              qcfrac_lg = 50420.0 * qc**2. + 29.39 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 97.65 * qc - 0.02152
            ELSE
              qcfrac_lg = 0.2 * log(646.908 * qc) + 0.135
            ENDIF
          ELSEIF (icing_opt .eq. 5) THEN
            IF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 28000. * qc**2. + 0.1 * qc
            ELSEIF (qc .lt. 2.6E-3) THEN
              qcfrac_lg = 112.351 * qc - 0.102272
            ELSE
              qcfrac_lg = 0.3 * log(654.92 * qc) * 0.301607
            ENDIF
          ELSEIF ((icing_opt .eq. 6) .OR. (icing_opt .eq. 7)) THEN
            IF ((qc .gt. 1.0E-12) .and. (nc .gt. 1.0E-12)) THEN
               small = -nc * exp(-nc*3141.59265*(5.E-5)**3./(6000.*den*qc))+nc
               all = -nc * exp(-nc*3141.59265*(2.)**3./(6000.*den*qc))+nc
               qcfrac_lg = 1. - (small / all)
            ELSE
               qcfac_lg = 0.
            ENDIF
          ENDIF
          qcfrac_lg = max(qcfrac_lg, 0.)
          IF (icing_opt .ne. 0 ) THEN
            qcfrac_sm = 1 - qcfrac_lg
          ENDIF
          qicing_lg (i,k,j) = max(qr + qcfrac_lg * qc, 0.)
          qicing_sm (i,k,j) = max(qcfrac_sm * qc, 0.)
          icing_lg (i,j) = icing_lg (i,j) + qicing_lg (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)
          icing_sm (i,j) = icing_sm (i,j) + qicing_sm (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)
          IF ( qicing_lg(i,k,j) .gt. qicing_lg_max(i,j) ) THEN
            qicing_lg_max (i,j) = qicing_lg(i,k,j)
          ENDIF
          IF ( qicing_sm(i,k,j) .gt. qicing_sm_max(i,j) ) THEN
            qicing_sm_max (i,j) = qicing_sm(i,k,j)
          ENDIF
          IF (fzlev (i,j) .eq. -999.) THEN
            IF (k .ne. kps) THEN
              fzlev (i,j) = z (i,k-1,j) + &
                             ((273.15 - t_phy (i,k-1,j)) &
                            /(t_phy (i,k,j) - t_phy (i,k-1,j))) &
                            *(z (i,k,j) - z (i,k-1,j))
            ELSE
              fzlev(i,j) = z (i,k,j)
            ENDIF
          ENDIF
          IF ((qicing_lg (i,k,j) + qicing_sm (i,k,j)) .ge. 1.E-5) THEN
            IF (kbot .eq. -1) kbot = k
            ktop=k
          ENDIF
        ENDIF
      END DO
      IF (kbot .ne. -1) THEN
        IF (kbot .ne. kps) THEN
          icingbot (i,j) = z (i,kbot-1,j) + ((1.E-5 - &
                   (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j))) &
                  / ((qicing_lg (i,kbot,j) + qicing_sm (i,kbot,j)) &
                  - (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j)))) &
                  * (z (i,kbot,j) - z (i,kbot-1,j))
          icingbot (i,j) = MAX(icingbot (i,j), fzlev (i,j))
        ELSE
          icingbot (i,j) = z(i,kbot,j)
        ENDIF
      ENDIF
      IF (ktop .ne. -1 .and. ktop .ne. kpe) THEN
        icingtop (i,j) = z (i,ktop,j) + ((1.E-5 - &
                 (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j))) &
                 / ((qicing_lg (i,ktop+1,j) + qicing_sm (i,ktop+1,j)) &
                 - (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j)))) &
                 * (z (i,ktop+1,j) - z (i,ktop,j))
        icingtop (i,j) = MAX(icingtop (i,j), icingbot (i,j))
      ENDIF
    END DO
    END DO
  END SUBROUTINE icing_diagnostics
  SUBROUTINE radar_diagnostics ( refd &
                             , refd_com &
                             , echotop &
                             , z &
                             , z_e &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN) :: z &
                                              , z_e
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(INOUT) :: refd &
                                              , refd_com &
                                              , echotop
    INTEGER :: i,j,k,ktime
    DO j = jps, MIN(jpe,jde-1)
    DO i = ips, MIN(ipe,ide-1)
      ktop = -1
      echotop (i,j) = 0.
      refd_com (i,j) = 0.
      refd (i,j) = 0.
      DO k = kps, MIN(kpe,kde-1)
        IF (z_e(i,k,j) .gt. 1.e-20) THEN
          IF (k == kps) refd(i,j) = MAX(10.0 * log10(z_e(i,k,j)),0.)
          IF (10.0 * log10(z_e(i,k,j)) .gt. refd_com(i,j)) THEN
            refd_com(i,j) = 10.0 * log10(z_e(i,k,j))
          ENDIF
        ENDIF
        IF ( z_e(i,k,j) .gt. 63.0957) THEN
          ktop = k
        ENDIF
      END DO
      IF ( ktop .ne. -1 ) THEN
        echotop (i,j) = z (i,ktop,j) + &
                          ((63.0957 - z_e (i,ktop,j)) &
                         /(z_e (i,ktop+1,j) - z_e (i,ktop,j))) &
                         *(z (i,ktop+1,j) - z (i,ktop,j))
      ENDIF
    END DO
    END DO
  END SUBROUTINE radar_diagnostics
  SUBROUTINE wrf_dbzcalc( rho &
                             , t_phy &
                             , qr &
                             , qs &
                             , qg &
                             , z_e &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: rho &
                                              , t_phy &
                                              , qr &
                                              , qs &
                                              , qg
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT( OUT) :: z_e
    REAL :: factor_r, factor_s, factor_g, factorb_s, factorb_g, ronv, sonv, gonv
    REAL :: temp_c, rhoair, qgr, qra, qsn
    INTEGER :: i, j, k
    INTEGER, PARAMETER :: iBrightBand = 1
    REAL, PARAMETER :: T_0 = 273.15
    REAL, PARAMETER :: PI = 3.1415926536
    REAL, PARAMETER :: rgas=287.04, gamma_seven = 720.0, alpha2 = 0.224
    REAL, PARAMETER :: rho_w = 1000.0, rho_r = 1000.0, rho_s = 100.0
    REAL, PARAMETER :: rho_g = 400.0, rho_i = 890.0
    REAL, PARAMETER :: ron=8.e6, son=2.e7, gon=5.e7, r1=1.e-15
    REAL, PARAMETER :: ron_min = 8.e6, ron2=1.e10
    REAL, PARAMETER :: ron_qr0 = 0.0001, ron_delqr0 = 0.25*ron_qr0
    REAL, PARAMETER :: ron_const1r = (ron2-ron_min)*0.5
    REAL, PARAMETER :: ron_const2r = (ron2+ron_min)*0.5
    ronv = 8.e6
    sonv = 2.e7
    gonv = 4.e6
    factor_r = gamma_seven * 1.e18 * (1./(pi*rho_r))**1.75
    factor_s = gamma_seven * 1.e18 * (1./(pi*rho_s))**1.75 &
              * (rho_s/rho_w)**2 * alpha2
    factor_g = gamma_seven * 1.e18 * (1./(pi*rho_g))**1.75 &
              * (rho_g/rho_w)**2 * alpha2
    DO j = jps, jpe
    DO k = kps, kpe
    DO i = ips, ipe
      factorb_s = factor_s
      factorb_g = factor_g
      IF( iBrightBand == 1 ) THEN
        IF (t_phy(i,k,j) > T_0) THEN
          factorb_s = factor_s /alpha2
          factorb_g = factor_g /alpha2
        ENDIF
      ENDIF
      temp_c = amin1(-0.001, t_phy(i,k,j)- T_0)
      sonv = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
      gonv = gon
      qgr = QG(i,k,j)
      qra = QR(i,k,j)
      qsn = QS(i,k,j)
      IF (qgr.gt.r1) THEN
        gonv = 2.38*(pi*rho_g/(rho(i,k,j)*qgr))**0.92
        gonv = max(1.e4, min(gonv,gon))
      ENDIF
      ronv = ron2
      IF (qra.gt. r1) THEN
        ronv = ron_const1r*tanh((ron_qr0-qra)/ron_delqr0) + ron_const2r
      ENDIF
      IF (qra < 0.0 ) qra = 0.0
      IF (qsn < 0.0 ) qsn = 0.0
      IF (qgr < 0.0 ) qgr = 0.0
      z_e(i,k,j) = factor_r * (rho(i,k,j) * qra)**1.75 / ronv**.75 + &
                     factorb_s * (rho(i,k,j) * qsn)**1.75 / sonv**.75 + &
                     factorb_g * (rho(i,k,j) * qgr)**1.75 / gonv**.75
      IF ( z_e(i,k,j) < 0.0 ) z_e(i,k,j) = 0.0
    END DO
    END DO
    END DO
  END SUBROUTINE wrf_dbzcalc
  SUBROUTINE precip_type_diagnostics ( t_phy &
                             , t2 &
                             , rh &
                             , z &
                             , ht &
                             , precip &
                             , swdown &
                             , rain &
                             , snow &
                             , ice &
                             , frz_rain &
                             , snowfall &
                             , ccn_tmp &
                             , total_melt &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: t_phy &
                                              , rh &
                                              , z
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN ) :: t2 &
                                              , ht &
                                              , precip &
                                              , swdown
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(INOUT) :: snowfall &
                                              , rain &
                                              , frz_rain &
                                              , snow &
                                              , ice
    REAL, INTENT(IN) :: ccn_tmp
    REAL, INTENT(IN) :: total_melt
    REAL, DIMENSION( ims:ime, jms:jme ) :: &
                                     melt &
                                   , mod_2m_tmp &
                                   , cloud_top_tmp &
                                   , maxtmp
    INTEGER, DIMENSION( ims:ime, jms:jme ) :: &
                                     cloud_top_k_index &
                                   , precip_type
    LOGICAL, DIMENSION (ims:ime, jms:jme ) :: &
                                     saturation
    REAL, PARAMETER :: snow_ratio=5.0
    DO i=ips,ipe
    DO j=jps,jpe
      saturation(i,j)=.false.
      melt(i,j)=0.0
      precip_type(i,j)=0
      mod_2m_tmp(i,j)=t2(i,j)+(swdown(i,j)/100.0)
      maxtmp(i,j)=mod_2m_tmp(i,j)
      IF (precip(i,j) .gt. 0.0) THEN
        IF (mod_2m_tmp(i,j) .gt. 275.15) THEN
          precip_type(i,j)=1
        ELSE
          cloud_top_k_index(i,j)=kpe
          DO k=kpe,kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
              IF (t_phy(i,k,j) .gt. maxtmp(i,j)) THEN
                maxtmp(i,j)=t_phy(i,k,j)
              ENDIF
              IF (rh(i,k,j) .gt. 80 .and. saturation(i,j) .eqv. .false.) THEN
                cloud_top_tmp(i,j)=t_phy(i,k,j)
                cloud_top_k_index(i,j)=k
                saturation(i,j)=.true.
                precip_type(i,j)=2
              ENDIF
              IF (rh(i,k,j) .le. 70 .and. saturation(i,j) .eqv. .true.) THEN
                saturation(i,j)=.false.
              ENDIF
            ENDIF
          ENDDO
          IF (cloud_top_tmp(i,j) .le. ccn_tmp .and. &
          maxtmp(i,j) .le. 273.15) THEN
            precip_type(i,j)=2
          ENDIF
          DO k=cloud_top_k_index(i,j),kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
              IF (cloud_top_tmp(i,j) .eq. t_phy(i,k,j) .and. &
              cloud_top_tmp(i,j) .gt. ccn_tmp) THEN
                 precip_type(i,j)=1
              ENDIF
              IF ((precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3) .and. &
              t_phy(i,k,j) .gt. 273.15) THEN
                melt(i,j)=melt(i,j)+9.8*(((t_phy(i,k,j)-273.15)/273.15)* &
                          (z(i,k,j)-z(i,k-1,j)))
                IF (melt(i,j) .gt. total_melt) THEN
                  precip_type(i,j)=1
                  melt(i,j)=0.0
                ENDIF
              ENDIF
              IF (t_phy(i,k,j) .le. 273.15 .and. &
              melt(i,j) .gt. total_melt/4.0 .and. &
              (precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3)) THEN
                precip_type(i,j)=3
                melt(i,j)=0.0
              ENDIF
              IF (precip_type(i,j) .eq. 1) THEN
                IF (t_phy(i,k,j) .le. ccn_tmp) THEN
                  precip_type(i,j)=3
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        IF (precip_type(i,j) .eq. 3) THEN
          ice(i,j)=ice(i,j)+precip(i,j)
        ENDIF
        IF (precip_type(i,j) .eq. 2) THEN
          snow(i,j)=snow(i,j)+precip(i,j)
          snowfall(i,j)=snowfall(i,j)+snow_ratio*precip(i,j) &
                        *(5.-mod_2m_tmp(i,j)+273.15)**0.4
        ENDIF
        IF (precip_type(i,j) .eq. 1) THEN
          IF (mod_2m_tmp(i,j) .gt. 273.15) THEN
            rain(i,j)=rain(i,j)+precip(i,j)
          ELSE
            frz_rain(i,j)=frz_rain(i,j)+precip(i,j)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    ENDDO
  END SUBROUTINE precip_type_diagnostics
  SUBROUTINE vis_diagnostics ( qcloud &
                             , qrain &
                             , qice &
                             , qsnow &
                             , qgrpl &
                             , rho &
                             , wind10m &
                             , wind125m &
                             , pwater &
                             , q2m &
                             , rh2m &
                             , rh20m &
                             , tv2m &
                             , tv20m &
                             , dustc &
                             , vis &
                             , vis_dust &
                             , vis_alpha &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER, PARAMETER :: ndust=5
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN ) :: qcloud &
                                              , qrain &
                                              , qice &
                                              , qsnow &
                                              , qgrpl &
                                              , rho &
                                              , wind10m &
                                              , wind125m &
                                              , pwater &
                                              , rh2m &
                                              , q2m &
                                              , rh20m &
                                              , tv2m &
                                              , tv20m
    REAL, DIMENSION( ims:ime, jms:jme, ndust ), &
         INTENT(IN ) :: dustc
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT( OUT) :: vis &
                                              , vis_dust &
                                              , vis_alpha
    INTEGER :: i,j,k,d
    REAL, PARAMETER :: visfactor=3.912
    REAL, DIMENSION (ndust) :: dustfact
    REAL :: bc, br, bi, bs, dust_extcoeff, hydro_extcoeff, extcoeff, vis_haze
    REAL :: tvd, rh, prob_ext_coeff_gt_p29, haze_ext_coeff
    REAL :: vis_hydlith, alpha_haze
    dustfact=(/1.470E-6,7.877E-7,4.623E-7,2.429E-7,1.387E-7/)
    DO i=ims,ime
      DO j=jms,jme
        br=1.1*(1000.*rho(i,j)*(qrain(i,j)+qgrpl(i,j)))**0.75
        bs=10.36*(1000.*rho(i,j)*qsnow(i,j))**0.78
        hydro_extcoeff=(br+bs)/1000.
        dust_extcoeff=0.
        DO d=1,ndust
          dust_extcoeff=dust_extcoeff+dustfact(d)*dustc(i,j,d)
        ENDDO
        vis_haze=999999.
        IF (q2m(i,j) .gt. 0.) THEN
          vis_haze=1500.*(105.-rh2m(i,j))*(5./min(1000.*q2m(i,j),5.))
        ENDIF
        alpha_haze=3.6
        IF (q2m(i,j) .gt. 0.) THEN
          alpha_haze=0.1 + pwater(i,j)/25. + wind125m(i,j)/3. + &
                          (100.-rh2m(i,j))/10. + 1./(1000.*q2m(i,j))
          alpha_haze=min(alpha_haze,3.6)
        ENDIF
        extcoeff=hydro_extcoeff+dust_extcoeff
        IF (extcoeff .gt. 0.) THEN
          vis_hydlith=min(visfactor/extcoeff, 999999.)
        ELSE
          vis_hydlith=999999.
        ENDIF
        IF (vis_hydlith < vis_haze) THEN
           vis(i,j)=vis_hydlith
           vis_alpha(i,j)=3.6
        ELSE
           vis(i,j)=vis_haze
           vis_alpha(i,j)=alpha_haze
        ENDIF
        IF (dust_extcoeff .gt. 0.) THEN
          vis_dust(i,j)=MIN(visfactor/dust_extcoeff,999999.)
        ELSE
          vis_dust(i,j)=999999.
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE vis_diagnostics
  SUBROUTINE cloud_diagnostics (qcloud &
                             , qice &
                             , qsnow &
                             , rh &
                             , dz8w &
                             , rho &
                             , z &
                             , ht &
                             , cloud &
                             , cloud_ceil &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: qcloud &
                                              , qice &
                                              , qsnow &
                                              , rh &
                                              , dz8w &
                                              , rho &
                                              , z
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN ) :: ht
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT( OUT) :: cloud &
                                              , cloud_ceil
    INTEGER :: i, j, k
    REAL :: tot_cld_cond, maxrh, cld_frm_cnd, cld_frm_rh, z_maxrh
    REAL :: snow_extcoeff, vis_snow, cloud_lo, zagl_up, zagl_lo
    REAL, PARAMETER :: min_ceil = 125.
    DO i=ims,ime
      DO j=jms,jme
        tot_cld_cond = 0.
        cloud(i,j) = 0.
        maxrh = -9999.
        cloud_ceil(i,j) = -9999.
        cloud_lo = 0.
        DO k=kms,kme
          IF ( z(i,k,j) - ht (i,j) .gt. min_ceil ) THEN
            IF (rh (i,k,j) .gt. maxrh) THEN
              maxrh = rh (i,k,j)
              z_maxrh = z(i,k,j)
            ENDIF
            cld_frm_rh = MAX(((rh (i,k,j) - 90.) / 10.),0.)
            cloud (i,j) = cloud (i,j) + ( cld_frm_rh * dz8w (i,k,j) ) / 250.
            IF ( cloud_ceil (i,j) .eq. -9999. .and. cloud (i,j) .gt. 0.8 ) THEN
              zagl_up = z (i,k,j) - ht (i,j)
              IF ( k .EQ. kps ) THEN
                cloud_ceil (i,j) = zagl_up
              ELSE
                zagl_lo = z (i,k-1,j) - ht (i,j)
                cloud_ceil (i,j) = zagl_lo + &
                             ((0.8 - cloud_lo) / &
                             (cloud (i,j) - cloud_lo)) * &
                             (zagl_up - zagl_lo)
                cloud_ceil (i,j) = MAX(cloud_ceil (i,j),ceil_min)
              ENDIF
            ENDIF
            cloud_lo=cloud(i,j)
          ENDIF
        ENDDO
        IF (cloud_ceil (i,j) .eq. -9999.) THEN
          cloud_ceil (i,j) = z_maxrh - ht (i,j)
        ENDIF
        IF (qsnow (i,1,j) .GT. 0. .AND. rho (i,1,j) .GT. 0.) THEN
          snow_extcoeff = 25. * (1000. * rho(i,1,j) * qsnow (i,1,j))**0.78
          snow_extcoeff = snow_extcoeff / 1000.
          vis_snow = 3.912 / snow_extcoeff
          IF (vis_snow .LT. cloud_ceil (i,j)) cloud_ceil (i,j) = vis_snow
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE cloud_diagnostics
  SUBROUTINE thermal_diagnostics ( t2 &
                             , psfc &
                             , rh2m &
                             , wind10m &
                             , heatidx &
                             , wchill &
                             , fits &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN ) :: t2 &
                                              , psfc &
                                              , rh2m &
                                              , wind10m
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT( OUT) :: heatidx &
                                              , wchill &
                                              , fits
    INTEGER :: i, j
    DO i=ims,ime
      DO j=jms,jme
        heatidx ( i, j ) = calc_hi ( t2 ( i, j ) &
                                       , rh2m ( i, j ) )
        wchill ( i, j ) = calc_wc ( t2 ( i, j ) &
                                       , wind10m ( i, j ) )
        fits ( i, j ) = calc_fits ( psfc ( i, j ) &
                                       , t2 ( i, j ) &
                                       , rh2m ( i, j ) )
      ENDDO
    ENDDO
  END SUBROUTINE thermal_diagnostics
  SUBROUTINE turbulence_diagnostics ( u_phy &
                             , v_phy &
                             , t_phy &
                             , p &
                             , zagl &
                             , defor11 &
                             , defor12 &
                             , defor22 &
                             , turb &
                             , llturb &
                             , llturblgt &
                             , llturbmdt &
                             , llturbsvr &
                             , nlyrs &
                             , lyrbot &
                             , lyrtop &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe )
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(IN) :: nlyrs
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: u_phy &
                                              , v_phy &
                                              , t_phy &
                                              , p &
                                              , zagl &
                                              , defor11 &
                                              , defor12 &
                                              , defor22
    REAL, DIMENSION( nlyrs ), &
         INTENT(IN ) :: lyrtop &
                                              , lyrbot
    REAL, DIMENSION( ims:ime, nlyrs, jms:jme ), &
         INTENT( OUT) :: turb
    REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT( OUT) :: llturb &
                                              , llturblgt &
                                              , llturbmdt &
                                              , llturbsvr
    INTEGER :: i, j, k, n, bot, top, nlayer
    REAL :: ugrdtop &
                                              , ugrdbot &
                                              , vgrdtop &
                                              , vgrdbot &
                                              , defor11top &
                                              , defor11bot &
                                              , defor12top &
                                              , defor12bot &
                                              , defor22top &
                                              , defor22bot
    REAL, DIMENSION( kms:kme ) :: this_zagl &
                                              , this_tK &
                                              , this_p &
                                              , this_u &
                                              , this_v
    REAL :: wind, therm, mtn_wave, tpd_wave
    turb = REAL ( 0 )
    llturb = REAL ( 0 )
    llturblgt = REAL ( 0 )
    llturbsvr = REAL ( 0 )
    DO i=ims,ime
      DO j=jms,jme
        DO n = 1, nlyrs
          ugrdtop = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , u_phy ( i, kms:kme-1, j ) &
                                   , lyrtop ( n ) )
          ugrdbot = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , u_phy ( i, kms:kme-1, j ) &
                                   , lyrbot ( n ) )
          vgrdtop = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , v_phy ( i, kms:kme-1, j ) &
                                   , lyrtop ( n ) )
          vgrdbot = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , v_phy ( i, kms:kme-1, j ) &
                                   , lyrbot ( n ) )
          defor11top = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor11 ( i, kms:kme-1, j ) &
                                   , lyrtop ( n ) )
          defor11bot = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor11 ( i, kms:kme-1, j ) &
                                   , lyrbot ( n ) )
          defor12top = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor12 ( i, kms:kme-1, j ) &
                                   , lyrtop ( n ) )
          defor12bot = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor12 ( i, kms:kme-1, j ) &
                                   , lyrbot ( n ) )
          defor22top = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor22 ( i, kms:kme-1, j ) &
                                   , lyrtop ( n ) )
          defor22bot = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                   , defor22 ( i, kms:kme-1, j ) &
                                   , lyrbot ( n ) )
          turb ( i, n, j ) = CATTurbulence ( ugrdbot &
                                               , ugrdtop &
                                               , vgrdbot &
                                               , vgrdtop &
                                               , defor11bot &
                                               , defor11top &
                                               , defor12bot &
                                               , defor12top &
                                               , defor22bot &
                                               , defor22top &
                                               , lyrbot (n) &
                                               , lyrtop (n) )
        ENDDO
        bot = kms
        top = kms
        DO k=kms+1,kme
          IF ( zagl ( i, k, j ) .gt. 1500. ) THEN
            top = k
            EXIT
          ENDIF
        ENDDO
        nlayer = top - bot + 1
        this_zagl = zagl ( i, kms:kme, j )
        this_tK = t_phy ( i, kms:kme, j )
        this_p = p ( i, kms:kme, j )
        this_u = u_phy ( i, kms:kme, j )
        this_v = v_phy ( i, kms:kme, j )
        this_zagl ( top ) = 1500.
        this_tK ( top ) = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                     , t_phy ( i, kms:kme-1, j ) &
                                     , this_zagl (top) )
        this_p ( top ) = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                     , p ( i, kms:kme-1, j ) &
                                     , this_zagl (top) )
        this_u ( top ) = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                     , u_phy ( i, kms:kme-1, j ) &
                                     , this_zagl (top) )
        this_v ( top ) = lin_interp ( zagl ( i, kms:kme-1, j ) &
                                     , v_phy ( i, kms:kme-1, j ) &
                                     , this_zagl (top) )
        wind = LLT_WindSpeed ( nlayer, this_u (bot:top) &
                                , this_v (bot:top) )
        therm = LLT_Thermodynamic ( nlayer, this_tK(bot:top) &
                                , this_zagl(bot:top) )
        mtn_wave = LLT_MountainWave ( nlayer, terrain_dx, terrain_dy &
                                , this_u(bot:top), this_v(bot:top) &
                                , this_tK(bot:top), this_zagl(bot:top) )
        tpd_wave = LLT_TrappedWave ( nlayer, this_u(bot:top) &
                                , this_v(bot:top), this_p(bot:top) )
        llturb ( i,j ) = 1.-((1.-wind)*(1.-therm)*(1.-mtn_wave)*(1.-tpd_wave))
        llturblgt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (30)) &
                                        *2.5)*.01)**2)*0.75)*REAL(100))
        IF ( llturb (i,j) < 0.3 ) llturblgt ( i,j ) = REAL ( 0 )
        IF ( llturblgt (i,j) > REAL (90) ) llturblgt ( i,j ) = REAL ( 90 )
        llturbmdt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (35)) &
                                     *2.22222)*.01)*0.75)**2)*88.88888)
        IF ( llturb (i,j) < 0.35 ) llturbmdt ( i,j ) = REAL ( 0 )
        IF ( llturbmdt (i,j) > REAL (70) ) llturbmdt ( i,j ) = REAL ( 70 )
        llturbsvr ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (40)) &
                                     *REAL(2))*.01)*0.5)**2)*REAL(100))
        IF ( llturb (i,j) < 0.40 ) llturbsvr ( i,j ) = REAL ( 0 )
        IF ( llturbsvr (i,j) > REAL (35) ) llturbsvr ( i,j ) = REAL ( 35 )
      ENDDO
    ENDDO
  END SUBROUTINE turbulence_diagnostics
END MODULE module_diag_afwa
