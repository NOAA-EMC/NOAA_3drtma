module module_smoke_plumerise
  use module_model_constants
  use module_zero_plumegen_coms
  real,parameter :: rgas=r_d
  real,parameter :: cpor=1./rcp
  real,parameter :: p00=p1000mb
CONTAINS
    subroutine plumerise(m1,m2,m3,ia,iz,ja,jz, &
                         up,vp,wp,theta,pp,dn0,rv,zt_rams,zm_rams, &
                         plume_fre,k1,k2, ktau, dbg_opt )
  implicit none
  LOGICAL, INTENT (IN) :: dbg_opt
  INTEGER, PARAMETER :: imean_frp=1, istd_frp=2, imean_fsize=3, istd_fsize=4
  real, dimension(4) :: plume_fre
  integer :: ng,m1,m2,m3,ia,iz,ja,jz,ibcon,mynum,i,j,k,imm,ixx,ispc
  INTEGER, INTENT (IN) :: ktau
  INTEGER, INTENT (OUT) :: k1,k2
  integer, save :: ncall = 0
  integer :: kmt
  real, dimension(m1,m2,m3) :: up, vp, wp,theta,pp,dn0,rv
  real, dimension(m1) :: zt_rams,zm_rams
  real :: burnt_area,dzi,FRP
  real, dimension(2) :: ztopmax
  real :: q_smold_kgm2
  integer, parameter :: nveg_agreg = 4, iveg_ag=1
  integer, parameter :: tropical_forest = 1
  integer, parameter :: boreal_forest = 2
  integer, parameter :: savannah = 3
  integer, parameter :: grassland = 4
  INTEGER, PARAMETER :: wind_eff = 1
  INTEGER, SAVE :: icall
  if (ncall==0) then
    ncall=1
    call zero_plumegen_coms
  endif
  j=1
  i=1
  do k = 1,m1
    ucon (k)=up(k,i,j)
    vcon (k)=vp(k,i,j)
    thtcon(k)=theta(k,i,j)
    picon (k)=pp(k,i,j)
    rvcon (k)=rv(k,i,j)
    zcon (k)=zt_rams(k)
    zzcon (k)=zm_rams(k)
  enddo
         call get_env_condition(1,m1,kmt,wind_eff,ktau)
    lp_minmax: do imm=1,2
            if(imm==1 ) then
               burnt_area = max(1.0e4,plume_fre(imean_fsize))
               FRP = max(1000.,0.75*plume_fre(imean_frp))
            elseif(imm==2 ) then
               burnt_area = max(1.0e4,plume_fre(imean_fsize))
               FRP = max(1000.,1.25*plume_fre(imean_frp))
            endif
        IF (dbg_opt .AND. icall<20) THEN
            WRITE(*,*) 'plumerise: m1,ktau ', m1,ktau
            WRITE(*,*) 'plumerise:  zcon ', zcon
            WRITE(*,*) 'plumerise: zzcon ', zzcon
         END IF
         IF (dbg_opt .AND. icall<2000) then
             WRITE(*,*) 'plumerise: imm ', imm
             WRITE(*,*) 'plumerise: plume_fre(imean_fsize),plume_fre(istd_fsize) ', plume_fre(imean_fsize),plume_fre(istd_fsize)
             WRITE(*,*) 'plumerise: plume_fre(imean_frp),plume_fre(istd_frp),FRP ', plume_fre(imean_frp),plume_fre(istd_frp),FRP
             WRITE(*,*) 'plumerise: burnt_area ',burnt_area
         END IF
       call get_fire_properties(imm,iveg_ag,burnt_area,FRP)
       call makeplume (kmt,ztopmax(imm),ixx,imm)
       IF (dbg_opt .AND. icall<2000) then
            WRITE(*,*) 'plumerise after makeplume: imm,kmt,ztopmax(imm) ',imm,kmt,ztopmax(imm)
       END IF
    enddo lp_minmax
        call set_flam_vert(ztopmax,k1,k2,nkp,zzcon)
    IF (dbg_opt .AND. icall<4000) then
        WRITE(*,*) 'plumerise after set_flam_vert: nkp,k1,k2, ', nkp,k1,k2
        icall=icall+1
    END IF
end subroutine plumerise
subroutine get_env_condition(k1,k2,kmt,wind_eff,ktau)
implicit none
integer :: k1,k2,k,kcon,klcl,kmt,nk,nkmid,i
real :: znz,themax,tlll,plll,rlll,zlll,dzdd,dzlll,tlcl,plcl,dzlcl,dummy
integer, save :: n_setgrid = 0
integer :: wind_eff,ktau
if(n_setgrid==0) then
  n_setgrid = 1
  call set_grid
endif
znz=zcon(k2)
do k=nkp,1,-1
  if(zt(k).lt.znz)go to 13
enddo
stop ' envir stop 12'
13 continue
kmt=min(k,nkp-1)
nk=k2-k1+1
 call htint(nk, ucon,zcon,kmt,upe,zt)
 call htint(nk, vcon,zcon,kmt,vpe,zt)
 call htint(nk,thtcon,zcon,kmt,the ,zt)
 call htint(nk, rvcon,zcon,kmt,qvenv,zt)
do k=1,kmt
  qvenv(k)=max(qvenv(k),1e-8)
enddo
pke(1)=picon(1)
do k=1,kmt
  thve(k)=the(k)*(1.+.61*qvenv(k))
enddo
do k=2,kmt
  pke(k)=pke(k-1)-g*2.*(zt(k)-zt(k-1)) &
        /(thve(k)+thve(k-1))
enddo
do k=1,kmt
  te(k) = the(k)*pke(k)/cp
  pe(k) = (pke(k)/cp)**cpor*p00
  dne(k)= pe(k)/(rgas*te(k)*(1.+.61*qvenv(k)))
  vel_e(k) = sqrt(upe(k)**2+vpe(k)**2)
enddo
if(wind_eff < 1) vel_e(1:kmt) = 0.
do k=1,kmt
 pe(k) = pe(k)*1.e-3
enddo
return
end subroutine get_env_condition
subroutine set_grid()
implicit none
integer :: k,mzp
dz=100.
mzp=nkp
zt(1) = zsurf
zm(1) = zsurf
zt(2) = zt(1) + 0.5*dz
zm(2) = zm(1) + dz
do k=3,mzp
 zt(k) = zt(k-1) + dz
 zm(k) = zm(k-1) + dz
enddo
do k = 1,mzp-1
   dzm(k) = 1. / (zt(k+1) - zt(k))
enddo
dzm(mzp)=dzm(mzp-1)
do k = 2,mzp
   dzt(k) = 1. / (zm(k) - zm(k-1))
enddo
dzt(1) = dzt(2) * dzt(2) / dzt(3)
return
end subroutine set_grid
  SUBROUTINE set_flam_vert(ztopmax,k1,k2,nkp,zzcon)
    REAL , INTENT(IN) :: ztopmax(2)
    INTEGER , INTENT(OUT) :: k1,k2
    INTEGER , INTENT(IN) :: nkp
    REAL , INTENT(IN) :: zzcon(nkp)
    INTEGER imm,k
    INTEGER, DIMENSION(2) :: k_lim
    DO imm=1,2
       DO k=1,nkp-1
          IF(zzcon(k) > ztopmax(imm)) EXIT
       ENDDO
       k_lim(imm) = k
    ENDDO
    k1= MIN(MAX(4,k_lim(1)),31)
    k2= MIN(31,k_lim(2))
    IF (k2 <= k1) THEN
       k2= k1+1
    ENDIF
  END SUBROUTINE set_flam_vert
subroutine get_fire_properties(imm,iveg_ag,burnt_area,FRP)
implicit none
integer :: moist, i, icount,imm,iveg_ag
real:: bfract, effload, heat, hinc ,burnt_area,heat_fluxW,FRP
real, dimension(2,4) :: heat_flux
INTEGER, parameter :: use_last = 0
REAL, parameter :: beta = 0.88
data heat_flux/ &
30.0, 80.0, &
30.0, 80.0, &
4.4, 23.0, &
3.3, 3.3 /
area = burnt_area
    heat_fluxW = beta*(FRP/area)/0.55
mdur = 53
bload = 10.
moist = 10
maxtime =mdur+2
heat = 19.3e6
alpha = 0.05
MAXTIME = MAXTIME * 60
RSURF = SQRT (AREA / 3.14159)
FMOIST = MOIST / 100.
  DO I = 1, ntime
    HEATING (I) = 0.0001
  enddo
  TDUR = MDUR * 60.
  bfract = 1.
  EFFLOAD = BLOAD * BFRACT
  ICOUNT = 1
  if(MDUR > NTIME) STOP 'Increase time duration (ntime) in min - see file "plumerise_mod.f90"'
  DO WHILE (ICOUNT.LE.MDUR)
   HEATING (ICOUNT) = heat_fluxW * 0.55
   ICOUNT = ICOUNT + 1
  ENDDO
 IF(use_last /= 1) THEN
    HINC = HEATING (1) / 4.
    HEATING (1) = 0.1
    HEATING (2) = HINC
    HEATING (3) = 2. * HINC
    HEATING (4) = 3. * HINC
 ELSE
    IF(imm==1) THEN
       HINC = HEATING (1) / 4.
       HEATING (1) = 0.1
       HEATING (2) = HINC
       HEATING (3) = 2. * HINC
       HEATING (4) = 3. * HINC
    ELSE
       HINC = (HEATING (1) - heat_flux(imm-1,iveg_ag) * 1000. *0.55)/ 4.
       HEATING (1) = heat_flux(imm-1,iveg_ag) * 1000. *0.55 + 0.1
       HEATING (2) = HEATING (1)+ HINC
       HEATING (3) = HEATING (2)+ HINC
       HEATING (4) = HEATING (3)+ HINC
    ENDIF
 ENDIF
return
end subroutine get_fire_properties
SUBROUTINE MAKEPLUME ( kmt,ztopmax,ixx,imm)
implicit none
character (len=10) :: varn
integer :: izprint, iconv, itime, k, kk, kkmax, deltak,ilastprint,kmt &
           ,ixx,nrectotal,i_micro,n_sub_step
real :: vc, g, r, cp, eps, &
         tmelt, heatsubl, heatfus, heatcond, tfreeze, &
         ztopmax, wmax, rmaxtime, es, esat, heat,dt_save
character (len=2) :: cixx
    REAL :: DELZ_THRESOLD = 100.
    INTEGER :: imm
parameter (vc = 5.107387)
parameter (g = 9.80796, r = 287.04, cp = 1004., eps = 0.622, tmelt = 273.3)
parameter (heatsubl = 2.834e6, heatfus = 3.34e5, heatcond = 2.501e6)
parameter (tfreeze = 269.3)
tstpf = 2.0
viscosity = 500.
nrectotal=150
mintime = 1
ztopmax = 0.
ztop = 0.
   time = 0.
     dt = 1.
   wmax = 1.
kkmax = 10
deltaK = 20
ilastprint=0
L = 1
CALL INITIAL(kmt)
izprint = 0
rmaxtime = float(maxtime)
 DO WHILE (TIME.LE.RMAXTIME)
    nm1 = min(kmt, kkmax + deltak)
    dt = min(5.,(zm(2)-zm(1)) / (tstpf * wmax))
    time = time+dt
    mintime = 1 + int (time) / 60
    wmax = 1.
    call tend0_plumerise
    L=1
    call lbound()
    call vel_advectc_plumerise(NM1,WC,WT,RHO,DZM)
    call scl_advectc_plumerise('SC',NM1)
    call scl_misc(NM1)
     call scl_dyn_entrain(NM1,nkp,wbar,w,adiabat,alpha,radius,tt,t,te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,&
                    vel_e,vel_p,vel_t,rad_p,rad_t)
    call damp_grav_wave(1,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)
    dt_save=dt
    n_sub_step=3
    dt=dt/float(n_sub_step)
    do i_micro=1,n_sub_step
     call fallpart(NM1)
     do L=2,nm1-1
        WBAR = 0.5*(W(L)+W(L-1))
        ES = ESAT_PR (T(L))
        QSAT(L) = (EPS * ES) / (PE(L) - ES)
        EST (L) = ES
        RHO (L) = 3483.8 * PE (L) / T (L)
        IF (W(L) .ge. 0.) then
           DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1 )-ZT(L-1))
        ELSE
           DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1) -ZT(L-1))
        ENDIF
        call waterbal
     enddo
    enddo
    dt=dt_save
    101 continue
    call visc_W(nm1,deltak,kmt)
    call update_plumerise(nm1,'S')
    call hadvance_plumerise(1,nm1,dt,WC,WT,W,mintime)
    call buoyancy_plumerise(NM1, T, TE, QV, QVENV, QH, QI, QC, WT, SCR1)
    call entrainment(NM1,W,WT,RADIUS,ALPHA)
    call update_plumerise(nm1,'W')
    call hadvance_plumerise(2,nm1,dt,WC,WT,W,mintime)
    do k=2,nm1
     es = esat_pr (t(k))
     qsat(k) = (eps * es) / (pe(k) - es)
     est (k) = es
     txs (k) = t(k) - te(k)
     rho (k) = 3483.8 * pe (k) / t (k)
     if((abs(wc(k))).gt.wmax) wmax = abs(wc(k))
    enddo
    call damp_grav_wave(2,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)
       do k=2,nm1
        radius(k) = rad_p(k)
       enddo
       kk = 1
       DO WHILE (w (kk) .GT. 1.)
          kk = kk + 1
          ztop = zm(kk)
       ENDDO
       ztop_(mintime) = ztop
       ztopmax = MAX (ztop, ztopmax)
       kkmax = MAX (kk , kkmax )
       IF(mintime > 10) THEN
          IF( ABS(ztop_(mintime)-ztop_(mintime-10)) < DELZ_THRESOLD) then
          EXIT
         ENDIF
       ENDIF
ENDDO
RETURN
END SUBROUTINE MAKEPLUME
SUBROUTINE BURN(EFLUX, WATER)
real, parameter :: HEAT = 19.3E6
real :: eflux,water
IF (TIME.GT.TDUR) THEN
   EFLUX = 0.000001
   WATER = 0.
   RETURN
ELSE
   EFLUX = HEATING (MINTIME)
   WATER = EFLUX * (DT / HEAT) * (0.5 + FMOIST) /0.55
   WATER = WATER * 1000.
ENDIF
RETURN
END SUBROUTINE BURN
SUBROUTINE LBOUND ()
implicit none
real, parameter :: g = 9.80796, r = 287.04, cp = 1004.6, eps = 0.622,tmelt = 273.3
real, parameter :: tfreeze = 269.3, pi = 3.14159, e1 = 1./3., e2 = 5./3.
real :: es, esat, eflux, water, pres, c1, c2, f, zv, denscor, xwater
QH (1) = QH (2)
QI (1) = QI (2)
QC (1) = 0.
   CALL BURN (EFLUX, WATER)
   PRES = PE (1) * 1000.
   C1 = 5. / (6. * ALPHA)
   C2 = 0.9 * ALPHA
   F = EFLUX / (PRES * CP * PI)
   F = G * R * F * AREA
   ZV = C1 * RSURF
   W (1) = C1 * ( (C2 * F) **E1) / ZV**E1
   DENSCOR = C1 * F / G / (C2 * F) **E1 / ZV**E2
   T (1) = TE (1) / (1. - DENSCOR)
   WC(1) = W(1)
    VEL_P(1) = 0.
    rad_p(1) = rsurf
   VTH (1) = - 4.
   VTI (1) = - 3.
   TXS (1) = T (1) - TE (1)
   VISC (1) = VISCOSITY
   RHO (1) = 3483.8 * PE (1) / T (1)
   XWATER = WATER / (W (1) * DT * RHO (1) )
   QV (1) = XWATER + QVENV (1)
   ES = ESAT_PR (T(1))
   EST (1) = ES
   QSAT (1) = (EPS * ES) / (PE (1) - ES)
   IF (QV (1) .gt. QSAT (1) ) THEN
       QC (1) = QV (1) - QSAT (1) + QC (1)
       QV (1) = QSAT (1)
   ENDIF
   CALL WATERBAL
RETURN
END SUBROUTINE LBOUND
SUBROUTINE INITIAL ( kmt)
implicit none
real, parameter :: tfreeze = 269.3
integer :: isub, k, n1, n2, n3, lbuoy, itmp, isubm1 ,kmt
real :: xn1, xi, es, esat
N=kmt
  do k = 1, N
  TXS (k) = 0.0
    W (k) = 0.0
    T (k) = TE(k)
    WC(k) = 0.0
    WT(k) = 0.0
    QV(k) = QVENV (k)
   VTH(k) = 0.
   VTI(k) = 0.
    QH(k) = 0.
    QI(k) = 0.
    QC(k) = 0.
   ES = ESAT_PR (T(k))
   EST (k) = ES
   QSAT (k) = (.622 * ES) / (PE (k) - ES)
   RHO (k) = 3483.8 * PE (k) / T (k)
       VEL_P(k) = 0.
       rad_p(k) = 0.
  enddo
  radius(1) = rsurf
  do k=2,N
     radius(k) = radius(k-1)+(6./5.)*alpha*(zt(k)-zt(k-1))
  enddo
    radius(1) = rsurf
    rad_p(1) = rsurf
    DO k=2,N
       radius(k) = radius(k-1)+(6./5.)*alpha*(zt(k)-zt(k-1))
       rad_p(k) = radius(k)
   ENDDO
   VISC (1) = VISCOSITY
   do k=2,N
     VISC (k) = max(1.e-3,visc(k-1) - 1.* VISCOSITY/float(nkp))
   enddo
   CALL LBOUND()
RETURN
END SUBROUTINE INITIAL
subroutine damp_grav_wave(ifrom,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)
implicit none
integer nm1,ifrom,deltak
real dt
real, dimension(nm1) :: w,t,tt,qv,qh,qi,qc,te,pe,qvenv,dummy,zt,zm
if(ifrom==1) then
 call friction(ifrom,nm1,deltak,dt,zt,zm,t,tt ,te)
 return
endif
dummy(:) = 0.
if(ifrom==2) call friction(ifrom,nm1,deltak,dt,zt,zm,w,dummy ,dummy)
return
end subroutine damp_grav_wave
subroutine friction(ifrom,nm1,deltak,dt,zt,zm,var1,vart,var2)
implicit none
real, dimension(nm1) :: var1,var2,vart,zt,zm
integer k,nfpt,kf,nm1,ifrom,deltak
real zmkf,ztop,distim,c1,c2,dt
 kf = nm1 - int(deltak)
zmkf = zm(kf)
ztop = zm(nm1)
 distim = min(3.*dt,60.)
c1 = 1. / (distim * (ztop - zmkf))
c2 = dt * c1
if(ifrom == 1) then
  do k = nm1,2,-1
   if (zt(k) .le. zmkf) cycle
   vart(k) = vart(k) + c1 * (zt(k) - zmkf)*(var2(k) - var1(k))
  enddo
elseif(ifrom == 2) then
  do k = nm1,2,-1
   if (zt(k) .le. zmkf) cycle
   var1(k) = var1(k) + c2 * (zt(k) - zmkf)*(var2(k) - var1(k))
  enddo
endif
return
end subroutine friction
subroutine vel_advectc_plumerise(m1,wc,wt,rho,dzm)
implicit none
integer :: k,m1
real, dimension(m1) :: wc,wt,flxw,dzm,rho
real, dimension(m1) :: dn0
real :: c1z
dn0(1:m1)=rho(1:m1)*1.e-3
flxw(1) = wc(1) * dn0(1)
do k = 2,m1-1
   flxw(k) = wc(k) * .5 * (dn0(k) + dn0(k+1))
enddo
c1z = .5
do k = 2,m1-2
   wt(k) = wt(k) &
      + c1z * dzm(k) / (dn0(k) + dn0(k+1)) * ( &
 (flxw(k) + flxw(k-1)) * (wc(k) + wc(k-1)) &
      - (flxw(k) + flxw(k+1)) * (wc(k) + wc(k+1)) &
      + (flxw(k+1) - flxw(k-1)) * 2.* wc(k) )
enddo
return
end subroutine vel_advectc_plumerise
subroutine hadvance_plumerise(iac,m1,dt,wc,wt,wp,mintime)
implicit none
integer :: k,iac
integer :: m1,mintime
real, dimension(m1) :: dummy, wc,wt,wp
real eps,dt
eps = .2
if(mintime == 1) eps=0.5
call predict_plumerise(m1,wc,wp,wt,dummy,iac,2.*dt,eps)
return
end subroutine hadvance_plumerise
subroutine predict_plumerise(npts,ac,ap,fa,af,iac,dtlp,epsu)
implicit none
integer :: npts,iac,m
real :: epsu,dtlp
real, dimension(*) :: ac,ap,fa,af
if (iac .eq. 1) then
   do m = 1,npts
      ac(m) = ac(m) + epsu * (ap(m) - 2. * ac(m))
   enddo
   return
elseif (iac .eq. 2) then
   do m = 1,npts
      af(m) = ap(m)
      ap(m) = ac(m) + epsu * af(m)
   enddo
endif
do m = 1,npts
  ac(m) = af(m)
enddo
return
end subroutine predict_plumerise
subroutine buoyancy_plumerise(m1, T, TE, QV, QVENV, QH, QI, QC, WT, scr1)
implicit none
integer :: k,m1
real, parameter :: g = 9.8, eps = 0.622, gama = 0.5
real, dimension(m1) :: T, TE, QV, QVENV, QH, QI, QC, WT, scr1
real :: TV,TVE,QWTOTL,umgamai
real, parameter :: mu = 0.15
umgamai = 1./(1.+gama)
do k = 2,m1-1
    TV = T(k) * (1. + (QV(k) /EPS))/(1. + QV(k) )
    TVE = TE(k) * (1. + (QVENV(k)/EPS))/(1. + QVENV(k))
    QWTOTL = QH(k) + QI(k) + QC(k)
    scr1(k)= G* umgamai*( (TV - TVE) / TVE - QWTOTL)
enddo
do k = 2,m1-2
    wt(k) = wt(k)+0.5*(scr1(k)+scr1(k+1))
enddo
end subroutine buoyancy_plumerise
subroutine ENTRAINMENT(m1,w,wt,radius,ALPHA)
implicit none
integer :: k,m1
real, dimension(m1) :: w,wt,radius
REAL DMDTM,WBAR,RADIUS_BAR,umgamai,DYN_ENTR,ALPHA
real, parameter :: mu = 0.15 ,gama = 0.5
umgamai = 1./(1.+gama)
  do k=2,m1-1
      WBAR=W(k)
      RADIUS_BAR = 0.5*(RADIUS(k) + RADIUS(k-1))
      DMDTM = umgamai * 2. * ALPHA * ABS (WBAR) / RADIUS_BAR
      wt(k) = wt(k) - DMDTM*ABS (WBAR)
       DYN_ENTR = (2./3.1416)*0.5*ABS (VEL_P(k)-VEL_E(k)+VEL_P(k-1)-VEL_E(k-1)) /RADIUS_BAR
       wt(k) = wt(k) - DYN_ENTR*ABS (WBAR)
  enddo
end subroutine ENTRAINMENT
subroutine scl_advectc_plumerise(varn,mzp)
implicit none
integer :: mzp
character(len=*) :: varn
real :: dtlto2
integer :: k
   dtlto2 = .5 * dt
   vt3dc(1) = (w(1) + wc(1)) * dtlto2 * rho(1)*1.e-3
   vt3df(1) = .5 * (w(1) + wc(1)) * dtlto2 * dzm(1)
   do k = 2,mzp
      vt3dc(k) = (w(k) + wc(k)) * dtlto2 *.5 * (rho(k) + rho(k+1))*1.e-3
      vt3df(k) = (w(k) + wc(k)) * dtlto2 *.5 * dzm(k)
   enddo
  do k = 1,mzp
     vctr1(k) = (zt(k+1) - zm(k)) * dzm(k)
     vctr2(k) = (zm(k) - zt(k)) * dzm(k)
     vt3dk(k) = dzt(k) /(rho(k)*1.e-3)
  enddo
   scr1=T
   call fa_zc_plumerise(mzp &
                     ,T ,scr1 (1) &
                     ,vt3dc (1) ,vt3df (1) &
                     ,vt3dg (1) ,vt3dk (1) &
                     ,vctr1,vctr2 )
   call advtndc_plumerise(mzp,T,scr1(1),TT,dt)
   scr1=QV
   call fa_zc_plumerise(mzp &
                     ,QV ,scr1 (1) &
                     ,vt3dc (1) ,vt3df (1) &
                     ,vt3dg (1) ,vt3dk (1) &
                     ,vctr1,vctr2 )
   call advtndc_plumerise(mzp,QV,scr1(1),QVT,dt)
   scr1=QC
   call fa_zc_plumerise(mzp &
                     ,QC ,scr1 (1) &
                     ,vt3dc (1) ,vt3df (1) &
                     ,vt3dg (1) ,vt3dk (1) &
                     ,vctr1,vctr2 )
   call advtndc_plumerise(mzp,QC,scr1(1),QCT,dt)
   scr1=QI
   call fa_zc_plumerise(mzp &
                     ,QI ,scr1 (1) &
                     ,vt3dc (1) ,vt3df (1) &
                     ,vt3dg (1) ,vt3dk (1) &
                     ,vctr1,vctr2 )
   call advtndc_plumerise(mzp,QI,scr1(1),QIT,dt)
      scr1=QH
      call fa_zc_plumerise(mzp &
                        ,QH ,scr1 (1) &
                        ,vt3dc (1) ,vt3df (1) &
                        ,vt3dg (1) ,vt3dk (1) &
                        ,vctr1,vctr2 )
      call advtndc_plumerise(mzp,QH,scr1(1),QHT,dt)
    scr1=VEL_P
    call fa_zc_plumerise(mzp &
       ,VEL_P ,scr1 (1) &
       ,vt3dc (1) ,vt3df (1) &
       ,vt3dg (1) ,vt3dk (1) &
       ,vctr1,vctr2 )
    call advtndc_plumerise(mzp,VEL_P,scr1(1),VEL_T,dt)
    scr1=rad_p
    call fa_zc_plumerise(mzp &
                      ,rad_p ,scr1 (1) &
                      ,vt3dc (1) ,vt3df (1) &
                      ,vt3dg (1) ,vt3dk (1) &
                      ,vctr1,vctr2 )
    call advtndc_plumerise(mzp,rad_p,scr1(1),rad_t,dt)
   return
   scr1=SC
   call fa_zc_plumerise(mzp &
                 ,SC ,scr1 (1) &
                 ,vt3dc (1) ,vt3df (1) &
                 ,vt3dg (1) ,vt3dk (1) &
                 ,vctr1,vctr2 )
   call advtndc_plumerise(mzp,SC,scr1(1),SCT,dt)
return
end subroutine scl_advectc_plumerise
subroutine fa_zc_plumerise(m1,scp,scr1,vt3dc,vt3df,vt3dg,vt3dk,vctr1,vctr2)
implicit none
integer :: m1,k
real :: dfact
real, dimension(m1) :: scp,scr1,vt3dc,vt3df,vt3dg,vt3dk
real, dimension(m1) :: vctr1,vctr2
dfact = .5
      do k = 1,m1-1
         vt3dg(k) = vt3dc(k) &
                  * (vctr1(k) * scr1(k) &
                  + vctr2(k) * scr1(k+1) &
                  + vt3df(k) * (scr1(k) - scr1(k+1)))
      enddo
do k = 1,m1-1
 if (vt3dc(k) .gt. 0.) then
   if (vt3dg(k) * vt3dk(k) .gt. dfact * scr1(k)) then
  vt3dg(k) = vt3dc(k) * scr1(k)
   endif
 elseif (vt3dc(k) .lt. 0.) then
   if (-vt3dg(k) * vt3dk(k+1) .gt. dfact * scr1(k+1)) then
  vt3dg(k) = vt3dc(k) * scr1(k+1)
   endif
 endif
enddo
do k = 2,m1-1
    scr1(k) = scr1(k) &
            + vt3dk(k) * ( vt3dg(k-1) - vt3dg(k) &
            + scp (k) * ( vt3dc(k) - vt3dc(k-1)))
enddo
return
end subroutine fa_zc_plumerise
subroutine advtndc_plumerise(m1,scp,sca,sct,dtl)
implicit none
integer :: m1,k
real :: dtl,dtli
real, dimension(m1) :: scp,sca,sct
dtli = 1. / dtl
do k = 2,m1-1
   sct(k) = sct(k) + (sca(k)-scp(k)) * dtli
enddo
return
end subroutine advtndc_plumerise
subroutine tend0_plumerise
 wt(1:nm1) = 0.
 tt(1:nm1) = 0.
qvt(1:nm1) = 0.
qct(1:nm1) = 0.
qht(1:nm1) = 0.
qit(1:nm1) = 0.
vel_t(1:nm1) = 0.
rad_t(1:nm1) = 0.
end subroutine tend0_plumerise
subroutine scl_misc(m1)
implicit none
real, parameter :: g = 9.81, cp=1004.
integer m1,k
real dmdtm
 do k=2,m1-1
      WBAR = 0.5*(W(k)+W(k-1))
      ADIABAT = - WBAR * G / CP
      DMDTM = 2. * ALPHA * ABS (WBAR) / RADIUS (k)
      TT(k) = TT(K) + ADIABAT - DMDTM * ( T (k) - TE (k) )
      QVT(K) = QVT(K) - DMDTM * ( QV (k) - QVENV (k) )
      QCT(K) = QCT(K) - DMDTM * ( QC (k) )
      QHT(K) = QHT(K) - DMDTM * ( QH (k) )
      QIT(K) = QIT(K) - DMDTM * ( QI (k) )
      VEL_T(K) = VEL_T(K) - DMDTM * ( VEL_P (k) - VEL_E (k) )
      rad_t(K) = rad_t(K) + 0.5*DMDTM*(6./5.)*RADIUS (k)
enddo
end subroutine scl_misc
  SUBROUTINE scl_dyn_entrain(m1,nkp,wbar,w,adiabat,alpha,radius,tt,t,te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,&
                    vel_e,vel_p,vel_t,rad_p,rad_t)
    implicit none
    INTEGER , INTENT(IN) :: m1
    INTEGER , INTENT(IN) :: nkp
    REAL , INTENT(INOUT) :: wbar
    REAL , INTENT(IN) :: w(nkp)
    REAL , INTENT(INOUT) :: adiabat
    REAL , INTENT(IN) :: alpha
    REAL , INTENT(IN) :: radius(nkp)
    REAL , INTENT(INOUT) :: tt(nkp)
    REAL , INTENT(IN) :: t(nkp)
    REAL , INTENT(IN) :: te(nkp)
    REAL , INTENT(INOUT) :: qvt(nkp)
    REAL , INTENT(IN) :: qv(nkp)
    REAL , INTENT(IN) :: qvenv(nkp)
    REAL , INTENT(INOUT) :: qct(nkp)
    REAL , INTENT(IN) :: qc(nkp)
    REAL , INTENT(INOUT) :: qht(nkp)
    REAL , INTENT(IN) :: qh(nkp)
    REAL , INTENT(INOUT) :: qit(nkp)
    REAL , INTENT(IN) :: qi(nkp)
    REAL , INTENT(IN) :: vel_e(nkp)
    REAL , INTENT(IN) :: vel_p(nkp)
    REAL , INTENT(INOUT) :: vel_t(nkp)
    REAL , INTENT(INOUT) :: rad_T(nkp)
    REAL , INTENT(IN) :: rad_p(nkp)
    real, parameter :: g = 9.81, cp=1004., pi=3.1416
    integer k
    real dmdtm
    DO k=2,m1-1
          rad_t(K) = rad_t(K) + ABS((vel_e(k)-vel_p(k)))/pi
          DMDTM = (2./3.1416) * ABS(VEL_E (k) - VEL_P (k)) / RADIUS (k)
          VEL_T(K) = VEL_T(K) - DMDTM * ( VEL_P (k) - VEL_E (k) )
          TT(k) = TT(K) - DMDTM * ( T (k) - TE (k) )
        QVT(K) = QVT(K) - DMDTM * ( QV (k) - QVENV (k) )
          QCT(K) = QCT(K) - DMDTM * ( QC (k) )
          QHT(K) = QHT(K) - DMDTM * ( QH (k) )
          QIT(K) = QIT(K) - DMDTM * ( QI (k) )
    ENDDO
   END SUBROUTINE scl_dyn_entrain
subroutine visc_W(m1,deltak,kmt)
implicit none
integer m1,k,deltak,kmt,m2
real dz1t,dz1m,dz2t,dz2m,d2wdz,d2tdz ,d2qvdz ,d2qhdz ,d2qcdz ,d2qidz ,d2scdz, &
 d2vel_pdz,d2rad_dz
m2=min(m1,kmt)
do k=2,m2-1
 DZ1T = 0.5*(ZT(K+1)-ZT(K-1))
 DZ2T = VISC (k) / (DZ1T * DZ1T)
 DZ1M = 0.5*(ZM(K+1)-ZM(K-1))
 DZ2M = VISC (k) / (DZ1M * DZ1M)
 D2WDZ = (W (k + 1) - 2 * W (k) + W (k - 1) ) * DZ2M
 D2TDZ = (T (k + 1) - 2 * T (k) + T (k - 1) ) * DZ2T
 D2QVDZ = (QV (k + 1) - 2 * QV (k) + QV (k - 1) ) * DZ2T
 D2QHDZ = (QH (k + 1) - 2 * QH (k) + QH (k - 1) ) * DZ2T
 D2QCDZ = (QC (k + 1) - 2 * QC (k) + QC (k - 1) ) * DZ2T
 D2QIDZ = (QI (k + 1) - 2 * QI (k) + QI (k - 1) ) * DZ2T
 d2vel_pdz=(vel_P (k + 1) - 2 * vel_P (k) + vel_P (k - 1) ) * DZ2T
 d2rad_dz =(rad_p (k + 1) - 2 * rad_p (k) + rad_p (k - 1) ) * DZ2T
  WT(k) = WT(k) + D2WDZ
  TT(k) = TT(k) + D2TDZ
 QVT(k) = QVT(k) + D2QVDZ
 QCT(k) = QCT(k) + D2QCDZ
 QHT(k) = QHT(k) + D2QHDZ
 QIT(k) = QIT(k) + D2QIDZ
 vel_t(k) = vel_t(k) + d2vel_pdz
 rad_t(k) = rad_t(k) + d2rad_dz
enddo
end subroutine visc_W
subroutine update_plumerise(m1,varn)
integer m1,k
character(len=*) :: varn
if(varn == 'W') then
 do k=2,m1-1
   W(k) = W(k) + WT(k) * DT
 enddo
 return
else
do k=2,m1-1
   T(k) = T(k) + TT(k) * DT
  QV(k) = QV(k) + QVT(k) * DT
  QC(k) = QC(k) + QCT(k) * DT
  QH(k) = QH(k) + QHT(k) * DT
  QI(k) = QI(k) + QIT(k) * DT
  QV(k) = max(0., QV(k))
  QC(k) = max(0., QC(k))
  QH(k) = max(0., QH(k))
  QI(k) = max(0., QI(k))
  VEL_P(k) = VEL_P(k) + VEL_T(k) * DT
  rad_p(k) = rad_p(k) + rad_t(k) * DT
 enddo
endif
end subroutine update_plumerise
subroutine fallpart(m1)
integer m1,k
real vtc, dfhz,dfiz,dz1
real, PARAMETER :: VCONST = 5.107387, EPS = 0.622, F0 = 0.75
real, PARAMETER :: G = 9.81, CP = 1004.
do k=2,m1-1
   VTC = VCONST * RHO (k) **.125
   VTH (k) = - 4.
   VHREL = W (k) + VTH (k)
   CVH(k) = 1.6 + 0.57E-3 * (ABS (VHREL) ) **1.5
   VTI (k) = - 3.
   VIREL = W (k) + VTI (k)
   CVI(k) = 1.6 + 0.57E-3 * (ABS (VIREL) ) **1.5 / F0
   IF (VHREL.GE.0.0) THEN
    DFHZ=QH(k)*(RHO(k )*VTH(k )-RHO(k-1)*VTH(k-1))/RHO(k-1)
   ELSE
    DFHZ=QH(k)*(RHO(k+1)*VTH(k+1)-RHO(k )*VTH(k ))/RHO(k)
   ENDIF
   IF (VIREL.GE.0.0) THEN
    DFIZ=QI(k)*(RHO(k )*VTI(k )-RHO(k-1)*VTI(k-1))/RHO(k-1)
   ELSE
    DFIZ=QI(k)*(RHO(k+1)*VTI(k+1)-RHO(k )*VTI(k ))/RHO(k)
   ENDIF
   DZ1=ZM(K)-ZM(K-1)
   qht(k) = qht(k) - DFHZ / DZ1
   qit(k) = qit(k) - DFIZ / DZ1
enddo
end subroutine fallpart
SUBROUTINE WATERBAL
IF (QC (L) .LE.1.0E-10) QC (L) = 0.
IF (QH (L) .LE.1.0E-10) QH (L) = 0.
IF (QI (L) .LE.1.0E-10) QI (L) = 0.
CALL EVAPORATE
CALL SUBLIMATE
CALL GLACIATE
CALL MELT
CALL CONVERT ()
RETURN
END SUBROUTINE WATERBAL
SUBROUTINE EVAPORATE
implicit none
real, PARAMETER :: HERC = 5.44E-4, CP = 1.004, HEATCOND = 2.5E3
real, PARAMETER :: HEATSUBL = 2834., TMELT = 273., TFREEZE = 269.3
real, PARAMETER :: FRC = HEATCOND / CP, SRC = HEATSUBL / CP
real :: evhdt, evidt, evrate, evap, sd, quant, dividend, divisor, devidt
SD = QSAT (L) - QV (L)
IF (SD.EQ.0.0) RETURN
EVHDT = 0.
EVIDT = 0.
EVRATE = ABS (WBAR * DQSDZ)
EVAP = EVRATE * DT
IF (SD.LE.0.0) THEN
   IF (EVAP.GE.ABS (SD) ) THEN
      QC (L) = QC (L) - SD
      QV (L) = QSAT(L)
      T (L) = T (L) - SD * FRC
      RETURN
   ELSE
      QC (L) = QC (L) + EVAP
      QV (L) = QV (L) - EVAP
      T (L) = T (L) + EVAP * FRC
      RETURN
   ENDIF
ELSE
   IF (EVAP.LE.QC (L) ) THEN
      IF (SD.LE.EVAP) THEN
         QC (L) = QC (L) - SD
         QV (L) = QSAT (L)
         T (L) = T (L) - SD * FRC
         RETURN
      ELSE
         SD = SD-EVAP
         QV (L) = QV (L) + EVAP
         T (L) = T (L) - EVAP * FRC
         QC (L) = QC (L) - EVAP
      ENDIF
   ELSE
      IF (SD.LE.QC (L) ) THEN
         QV (L) = QSAT (L)
         QC (L) = QC (L) - SD
         T (L) = T (L) - SD * FRC
         RETURN
      ELSE
         SD = SD-QC (L)
         QV (L) = QV (L) + QC (L)
         T (L) = T (L) - QC (L) * FRC
         QC (L) = 0.0
      ENDIF
   ENDIF
   IF (QH (L) .LE.1.E-10) GOTO 33
   QUANT = ( QSAT (L)- QC (L) - QV (L) ) * RHO (L)
   EVHDT = (DT * HERC * (QUANT) * (QH (L) * RHO (L) ) **.65) / RHO (L)
   IF (EVHDT.LE.QH (L) ) THEN
      IF (SD.LE.EVHDT) THEN
         QH (L) = QH (L) - SD
         QV (L) = QSAT (L)
         T (L) = T (L) - SD * FRC
  RETURN
      ELSE
         SD = SD-EVHDT
         QV (L) = QV (L) + EVHDT
         T (L) = T (L) - EVHDT * FRC
         QH (L) = QH (L) - EVHDT
      ENDIF
   ELSE
      IF (SD.LE.QH (L) ) THEN
         QV (L) = QSAT (L)
         QH (L) = QH (L) - SD
         T (L) = T (L) - SD * FRC
         RETURN
      ELSE
         SD = SD-QH (L)
         QV (L) = QV (L) + QH (L)
         T (L) = T (L) - QH (L) * FRC
         QH (L) = 0.0
      ENDIF
   ENDIF
   33 continue
   IF (QI (L) .LE.1.E-10) RETURN
   DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (SD / QSAT (L) &
            - 1) * (QI (L) **0.525) * 1.13
   DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )
   DEVIDT = - CVI(L) * DIVIDEND / DIVISOR
   EVIDT = DEVIDT * DT
   IF (EVIDT.LE.QI (L) ) THEN
      IF (SD.LE.EVIDT) THEN
         QI (L) = QI (L) - SD
         QV (L) = QSAT (L)
         T (L) = T (L) - SD * SRC
         RETURN
      ELSE
         SD = SD-EVIDT
         QV (L) = QV (L) + EVIDT
          T (L) = T (L) - EVIDT * SRC
         QI (L) = QI (L) - EVIDT
      ENDIF
   ELSE
      IF (SD.LE.QI (L) ) THEN
         QV (L) = QSAT (L)
         QI (L) = QI (L) - SD
          T (L) = T (L) - SD * SRC
         RETURN
      ELSE
         SD = SD-QI (L)
         QV (L) = QV (L) + QI (L)
         T (L) = T (L) - QI (L) * SRC
         QI (L) = 0.0
      ENDIF
   ENDIF
ENDIF
RETURN
END SUBROUTINE EVAPORATE
SUBROUTINE CONVERT ()
real, PARAMETER :: AK1 = 0.001
real, PARAMETER :: AK2 = 0.0052
real, PARAMETER :: TH = 0.5
integer, PARAMETER :: iconv = 1
 real, parameter :: ANBASE =100000.
 real, parameter :: BDISP = 0.146
real, parameter :: TFREEZE = 269.3
real :: accrete, con, q, h, bc1, bc2, total
IF (T (L) .LE. TFREEZE) RETURN
IF (QC (L) .EQ. 0. ) RETURN
ACCRETE = 0.
CON = 0.
Q = RHO (L) * QC (L)
H = RHO (L) * QH (L)
IF (QH (L) .GT. 0. ) ACCRETE = AK2 * Q * (H**.875)
IF (ICONV.NE.0) THEN
   CON = Q*Q*Q*BDISP/(60.*(5.*Q*BDISP+0.0366*ANBASE))
ELSE
   CON = max(0.,AK1 * (Q - TH))
ENDIF
TOTAL = (CON + ACCRETE) * DT / RHO (L)
IF (TOTAL.LT.QC (L) ) THEN
   QC (L) = QC (L) - TOTAL
   QH (L) = QH (L) + TOTAL
   RETURN
ELSE
   QH (L) = QH (L) + QC (L)
   QC (L) = 0.0
ENDIF
RETURN
END SUBROUTINE CONVERT
SUBROUTINE SUBLIMATE
real, PARAMETER :: EPS = 0.622, HEATFUS = 334., HEATSUBL = 2834., CP = 1.004
real, PARAMETER :: SRC = HEATSUBL / CP, FRC = HEATFUS / CP, TMELT = 273.3
real, PARAMETER :: TFREEZE = 269.3
real ::dtsubh, dividend,divisor, subl
DTSUBH = 0.
IF (T (L) .GT. TFREEZE ) RETURN
IF (QV (L) .LE. QSAT (L) ) RETURN
 DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (QV (L) / QSAT (L) &
            - 1) * (QI (L) **0.525) * 1.13
 DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )
 DTSUBH = ABS (DIVIDEND / DIVISOR)
 SUBL = DTSUBH * DT
IF (SUBL.LT.QV (L) ) THEN
   QV (L) = QV (L) - SUBL
   QI (L) = QI (L) + SUBL
   T (L) = T (L) + SUBL * SRC
   RETURN
ELSE
   QI (L) = QV (L)
   T (L) = T (L) + QV (L) * SRC
   QV (L) = 0.0
ENDIF
RETURN
END SUBROUTINE SUBLIMATE
SUBROUTINE GLACIATE
real, PARAMETER :: HEATFUS = 334., CP = 1.004, EPS = 0.622, HEATSUBL = 2834.
real, PARAMETER :: FRC = HEATFUS / CP, FRS = HEATSUBL / CP, TFREEZE = 269.3
real, PARAMETER :: GLCONST = 0.025
real dfrzh
 DFRZH = 0.
IF (QH (L) .LE. 0. ) RETURN
IF (QV (L) .LT. QSAT (L) ) RETURN
IF (T (L) .GT. TFREEZE ) RETURN
 DFRZH = DT * GLCONST * QH (L)
IF (DFRZH.LT.QH (L) ) THEN
   QI (L) = QI (L) + DFRZH
   QH (L) = QH (L) - DFRZH
   T (L) = T (L) + FRC * DFRZH
   RETURN
ELSE
   QI (L) = QI (L) + QH (L)
   T (L) = T (L) + FRC * QH (L)
   QH (L) = 0.0
ENDIF
RETURN
END SUBROUTINE GLACIATE
SUBROUTINE MELT
real, PARAMETER :: FRC = 332.27, TMELT = 273., F0 = 0.75
real DTMELT
 DTMELT = 0.
IF (QI (L) .LE. 0.0 ) RETURN
IF (T (L) .LT. TMELT) RETURN
 DTMELT = DT * (2.27 / RHO (L) ) * CVI(L) * (T (L) - TMELT) * ( (RHO(L) &
         * QI (L) * 1.E-6) **0.525) * (F0** ( - 0.42) )
IF (DTMELT.LT.QI (L) ) THEN
   QH (L) = QH (L) + DTMELT
   QI (L) = QI (L) - DTMELT
   T (L) = T (L) - FRC * DTMELT
   RETURN
ELSE
   QH (L) = QH (L) + QI (L)
   T (L) = T (L) - FRC * QI (L)
   QI (L) = 0.0
ENDIF
RETURN
END SUBROUTINE MELT
SUBROUTINE htint (nzz1, vctra, eleva, nzz2, vctrb, elevb)
  IMPLICIT NONE
  INTEGER, INTENT(IN ) :: nzz1
  INTEGER, INTENT(IN ) :: nzz2
  REAL, INTENT(IN ) :: vctra(nzz1)
  REAL, INTENT(OUT) :: vctrb(nzz2)
  REAL, INTENT(IN ) :: eleva(nzz1)
  REAL, INTENT(IN ) :: elevb(nzz2)
  INTEGER :: l
  INTEGER :: k
  INTEGER :: kk
  REAL :: wt
  l=1
  DO k=1,nzz2
     DO
        IF ( (elevb(k) < eleva(1)) .OR. &
             ((elevb(k) >= eleva(l)) .AND. (elevb(k) <= eleva(l+1))) ) THEN
           wt = (elevb(k)-eleva(l))/(eleva(l+1)-eleva(l))
           vctrb(k) = vctra(l)+(vctra(l+1)-vctra(l))*wt
           EXIT
        ELSE IF ( elevb(k) > eleva(nzz1)) THEN
           wt = (elevb(k)-eleva(nzz1))/(eleva(nzz1-1)-eleva(nzz1))
           vctrb(k) = vctra(nzz1)+(vctra(nzz1-1)-vctra(nzz1))*wt
           EXIT
        END IF
        l=l+1
        IF(l == nzz1) THEN
           PRINT *,'htint:nzz1',nzz1
           DO kk=1,l
              PRINT*,'kk,eleva(kk),elevb(kk)',eleva(kk),elevb(kk)
           END DO
           STOP 'htint'
        END IF
     END DO
  END DO
END SUBROUTINE htint
FUNCTION ESAT_PR (TEM)
real, PARAMETER :: CI1 = 6.1115, CI2 = 22.542, CI3 = 273.48
real, PARAMETER :: CW1 = 6.1121, CW2 = 18.729, CW3 = 257.87, CW4 = 227.3
real, PARAMETER :: TMELT = 273.3
real ESAT_PR
real temc , tem,esatm
TEMC = TEM - TMELT
IF (TEMC.GT. - 40.0) GOTO 230
ESATM = CI1 * EXP (CI2 * TEMC / (TEMC + CI3) )
ESAT_PR = ESATM / 10.
RETURN
230 ESATM = CW1 * EXP ( ( (CW2 - (TEMC / CW4) ) * TEMC) / (TEMC + CW3))
ESAT_PR = ESATM / 10.
RETURN
END function ESAT_PR
END Module module_smoke_plumerise
