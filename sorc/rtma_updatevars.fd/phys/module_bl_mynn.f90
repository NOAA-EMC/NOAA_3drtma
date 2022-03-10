MODULE module_bl_mynn
  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, r_v, rcp, xlv, xlf, xls, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2, rvovrd, &
       &cpv, cliq, cice
  USE module_state_description, only: param_first_scalar, &
       &p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni
  IMPLICIT NONE
  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0
  REAL, PARAMETER :: xlvcp=xlv/cp, xlscp=(xlv+xlf)/cp, ev=xlv, rd=r_d, &
       &rk=cp/rd, svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2
  REAL, PARAMETER :: tref=300.0
  REAL, PARAMETER :: TKmin=253.0
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref
  REAL, PARAMETER :: &
       &vk = karman, &
       &pr = 0.74, &
       &g1 = 0.235, &
       &b1 = 24.0, &
       &b2 = 15.0, &
       &c2 = 0.729, &
       &c3 = 0.340, &
       &c4 = 0.0, &
       &c5 = 0.2, &
       &a1 = b1*( 1.0-3.0*g1 )/6.0, &
       &c1 = g1 -1.0/( 3.0*a1*2.88449914061481660), &
       &a2 = a1*( g1-c1 )/( g1*pr ), &
       &g2 = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )
  REAL, PARAMETER :: &
       &cc2 = 1.0-c2, &
       &cc3 = 1.0-c3, &
       &e1c = 3.0*a2*b2*cc3, &
       &e2c = 9.0*a1*a2*cc2, &
       &e3c = 9.0*a2*a2*cc2*( 1.0-c5 ), &
       &e4c = 12.0*a1*a2*cc2, &
       &e5c = 6.0*a1*a1
  REAL, PARAMETER :: qmin=0.0, zmax=1.0, Sqfac=3.0
  REAL, PARAMETER :: gno=1.0
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12
  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423
  REAL, PARAMETER :: zero = 0.0, half = 0.5, one = 1.0, two = 2.0
  REAL, PARAMETER :: CKmod=1.
  REAL, PARAMETER :: scaleaware=1.
  INTEGER, PARAMETER :: bl_mynn_mixchem = 0
  INTEGER, PARAMETER :: bl_mynn_topdown = 0
  REAL, PARAMETER :: dheat_opt = 1.
  LOGICAL, PARAMETER :: debug_code = .false.
  REAL, PARAMETER:: J0= .611583699E03
  REAL, PARAMETER:: J1= .444606896E02
  REAL, PARAMETER:: J2= .143177157E01
  REAL, PARAMETER:: J3= .264224321E-1
  REAL, PARAMETER:: J4= .299291081E-3
  REAL, PARAMETER:: J5= .203154182E-5
  REAL, PARAMETER:: J6= .702620698E-8
  REAL, PARAMETER:: J7= .379534310E-11
  REAL, PARAMETER:: J8=-.321582393E-13
  REAL, PARAMETER:: K0= .609868993E03
  REAL, PARAMETER:: K1= .499320233E02
  REAL, PARAMETER:: K2= .184672631E01
  REAL, PARAMETER:: K3= .402737184E-1
  REAL, PARAMETER:: K4= .565392987E-3
  REAL, PARAMETER:: K5= .521693933E-5
  REAL, PARAMETER:: K6= .307839583E-7
  REAL, PARAMETER:: K7= .105785160E-9
  REAL, PARAMETER:: K8= .161444444E-12
  INTEGER :: mynn_level
  CHARACTER*128 :: mynn_message
  INTEGER, PARAMETER :: kdebug=27
CONTAINS
  SUBROUTINE mym_initialize ( &
       & kts,kte, &
       & dz, zw, &
       & u, v, thl, qw, &
       & zi, theta, sh, &
       & ust, rmo, el, &
       & Qke, Tsq, Qsq, Cov, Psig_bl, cldfra_bl1D, &
       & bl_mynn_mixlength, &
       & edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf, &
       & INITIALIZE_QKE, &
       & spp_pbl,rstoch_col)
    INTEGER, INTENT(IN) :: kts,kte
    INTEGER, INTENT(IN) :: bl_mynn_mixlength,bl_mynn_edmf
    LOGICAL, INTENT(IN) :: INITIALIZE_QKE
    REAL, INTENT(IN) :: ust, rmo, Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,cldfra_bl1D,&
                                          edmf_w1,edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke
    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta
    REAL, DIMENSION(kts:kte) :: rstoch_col
    INTEGER ::spp_pbl
    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO
    CALL mym_level2 ( kts,kte,&
         & dz, &
         & u, v, thl, qw, &
         & ql, vt, vq, &
         & dtl, dqw, dtv, gm, gh, sm, sh )
    el (kts) = 0.0
    IF (INITIALIZE_QKE) THEN
       qke(kts) = 1.5 * ust**2 * ( b1*pmz )**(2.0/3.0)
       DO k = kts+1,kte
          qke(k)=qke(kts)*MAX((ust*700. - zw(k))/(MAX(ust,0.01)*700.), 0.01)
       ENDDO
    ENDIF
    phm = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )
    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
       tsq(k) = 0.0
       qsq(k) = 0.0
       cov(k) = 0.0
    END DO
    lmax = 5
    DO l = 1,lmax
       CALL mym_length ( &
            & kts,kte, &
            & dz, zw, &
            & rmo, flt, flq, &
            & vt, vq, &
            & qke, &
            & dtv, &
            & el, &
            & zi,theta, &
            & qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
            & edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf)
       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm (k)+&
               &sh(k)*gh (k) )
          pdt(k) = elq* sh(k)*dtl(k)**2
          pdq(k) = elq* sh(k)*dqw(k)**2
          pdc(k) = elq* sh(k)*dtl(k)*dqw(k)
       END DO
       vkz = vk*0.5*dz(kts)
       elv = 0.5*( el(kts+1)+el(kts) ) / vkz
       IF (INITIALIZE_QKE)THEN
          qke(kts) = 1.0 * MAX(ust,0.02)**2 * ( b1*pmz*elv )**(2.0/3.0)
       ENDIF
       phm = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )
       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          tmpq=MIN(MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin),125.)
          IF (INITIALIZE_QKE)THEN
             qke(k) = tmpq**0.666666666
          ENDIF
          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF
          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
    END DO
    IF (INITIALIZE_QKE)THEN
       qke(kts)=0.5*(qke(kts)+qke(kts+1))
       qke(kte)=qke(kte-1)
    ENDIF
    tsq(kte)=tsq(kte-1)
    qsq(kte)=qsq(kte-1)
    cov(kte)=cov(kte-1)
  END SUBROUTINE mym_initialize
  SUBROUTINE mym_level2 (kts,kte,&
       & dz, &
       & u, v, thl, qw, &
       & ql, vt, vq, &
       & dtl, dqw, dtv, gm, gh, sm, sh )
    INTEGER, INTENT(IN) :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq
    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh
    INTEGER :: k
    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf
    REAL :: a2den
    rfc = g1/( g1+g2 )
    f1 = b1*( g1-c1 ) +3.0*a2*( 1.0 -c2 )*( 1.0-c5 ) &
    & +2.0*a1*( 3.0-2.0*c2 )
    f2 = b1*( g1+g2 ) -3.0*a1*( 1.0 -c2 )
    rf1 = b1*( g1-c1 )/f1
    rf2 = b1* g1 /f2
    smc = a1 /a2* f1/f2
    shc = 3.0*a2*( g1+g2 )
    ri1 = 0.5/smc
    ri2 = rf1*smc
    ri3 = 4.0*rf2*smc -2.0*ri2
    ri4 = ri2**2
    DO k = kts+1,kte
       dzk = 0.5 *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz = duz /dzk**2
       dtz = ( thl(k)-thl(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )
       vtt = 1.0 +vt(k)*abk +vt(k-1)*afk
       vqq = tv0 +vq(k)*abk +vq(k-1)*afk
       dtq = vtt*dtz +vqq*dqz
       dtl(k) = dtz
       dqw(k) = dqz
       dtv(k) = dtq
       gm (k) = duz
       gh (k) = -dtq*gtr
       ri = -gh(k)/MAX( duz, 1.0e-10 )
    IF (CKmod .eq. 1) THEN
       a2den = 1. + MAX(ri,0.0)
    ELSE
       a2den = 1. + 0.0
    ENDIF
       rfc = g1/( g1+g2 )
       f1 = b1*( g1-c1 ) +3.0*(a2/a2den)*( 1.0 -c2 )*( 1.0-c5 ) &
    & +2.0*a1*( 3.0-2.0*c2 )
       f2 = b1*( g1+g2 ) -3.0*a1*( 1.0 -c2 )
       rf1 = b1*( g1-c1 )/f1
       rf2 = b1* g1 /f2
       smc = a1 /(a2/a2den)* f1/f2
       shc = 3.0*(a2/a2den)*( g1+g2 )
       ri1 = 0.5/smc
       ri2 = rf1*smc
       ri3 = 4.0*rf2*smc -2.0*ri2
       ri4 = ri2**2
       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )
       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO
  END SUBROUTINE mym_level2
  SUBROUTINE mym_length ( &
    & kts,kte, &
    & dz, zw, &
    & rmo, flt, flq, &
    & vt, vq, &
    & qke, &
    & dtv, &
    & el, &
    & zi,theta, &
    & qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
    & edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf)
    INTEGER, INTENT(IN) :: kts,kte
    INTEGER, INTENT(IN) :: bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(IN) :: qke,vt,vq,cldfra_bl1D,&
                                          edmf_w1,edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out) :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in) :: dtv
    REAL :: elt,vsc
    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,wt2,zi,zi2,h1,h2,hs,elBLmin0,elBLavg0,cldavg
    REAL :: cns, &
            alp1, &
            alp2, &
            alp3, &
            alp4, &
            alp5, &
            alp6
    REAL, PARAMETER :: minzi = 300.
    REAL, PARAMETER :: maxdz = 750.
    REAL, PARAMETER :: mindz = 300.
    REAL, PARAMETER :: ZSLH = 100.
    REAL, PARAMETER :: CSL = 2.
    REAL :: z_m
    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,zwk1,dzk,qdz,vflx,bv,tau_cloud,elb,els,els1,elf, &
            & el_stab,el_unstab,el_mf,el_stab_mf,elb_mf,PBLH_PLUS_ENT,el_les
    SELECT CASE(bl_mynn_mixlength)
      CASE (0)
        cns = 2.7
        alp1 = 0.23
        alp2 = 1.0
        alp3 = 5.0
        alp4 = 100.
        alp5 = 0.4
        zi2 = MIN(10000.,zw(kte-2))
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)
        h2=h1/2.0
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))
        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
        END DO
        elt = 1.0e-5
        vsc = 1.0e-5
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. zi2+h1)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k = k+1
           zwk = zw(k)
        END DO
        elt = alp1*elt/vsc
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)
        el(kts) = 0.0
        zwk1 = zw(kts+1)
        DO k = kts+1,kte
           zwk = zw(k)
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv = SQRT( gtr*dtv(k) )
              elb = alp2*qkw(k) / bv &
                  & *( 1.0 + alp3/alp2*&
                  &SQRT( vsc/( bv*elt ) ) )
              elf = alp2 * qkw(k)/bv
           ELSE
              elb = 1.0e10
              elf = elb
           ENDIF
           z_m = MAX(0.,zwk - 4.)
           IF ( rmo .GT. 0.0 ) THEN
              els = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
              els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els = vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
              els1 = vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
           END IF
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
           el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
        END DO
      CASE (1)
        cns = 2.3
        alp1 = 0.23
        alp2 = 0.65
        alp3 = 3.0
        alp4 = 20.
        alp5 = 0.4
        zi2=MAX(zi,minzi)
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)
        h2=h1/2.0
        qtke(kts)=MAX(qke(kts)/2.,0.01)
        thetaw(kts)=theta(kts)
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))
        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = (qkw(k)**2.)/2.
           thetaw(k)= theta(k)*abk + theta(k-1)*afk
        END DO
        elt = 1.0e-5
        vsc = 1.0e-5
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. zi2+h1)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k = k+1
           zwk = zw(k)
        END DO
        elt = alp1*elt/vsc
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)
        el(kts) = 0.0
        zwk1 = zw(kts+1)
        CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)
        DO k = kts+1,kte
           zwk = zw(k)
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv = SQRT( gtr*dtv(k) )
              elb = alp2*qkw(k) / bv &
                  & *( 1.0 + alp3/alp2*&
                  &SQRT( vsc/( bv*elt ) ) )
              elb = MIN(elb, zwk)
              elf = alp2 * qkw(k)/bv
           ELSE
              elb = 1.0e10
              elf = elb
           ENDIF
           z_m = MAX(0.,zwk - 4.)
           IF ( rmo .GT. 0.0 ) THEN
              els = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
              els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els = vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
              els1 = vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
           END IF
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
           el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
           el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt
           el(k) = el(k)*Psig_bl
         END DO
      CASE (2)
        cns = 3.5
        alp1 = 0.25 + 0.02*MIN(MAX(zi-200.,0.),1000.)/1000.
        alp2 = 0.3
        alp3 = 2.0
        alp4 = 20.
        alp5 = 0.3
        alp6 = 50.0
        zi2=MAX(zi, 100.)
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)
        h2=h1*0.5
        qtke(kts)=MAX(0.5*qke(kts),0.01)
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))
        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = 0.5*qkw(k)
        END DO
        elt = 1.0e-5
        vsc = 1.0e-5
        PBLH_PLUS_ENT = MAX(zi+h1, 100.)
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. PBLH_PLUS_ENT)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k = k+1
           zwk = zw(k)
        END DO
        elt = MAX(alp1*elt/vsc, 10.)
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(0.33333)
        el(kts) = 0.0
        zwk1 = zw(kts+1)
        DO k = kts+1,kte
           zwk = zw(k)
           cldavg = 0.5*(cldfra_bl1D(k-1)+cldfra_bl1D(k))
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv = SQRT( gtr*dtv(k) )
              elb_mf = MAX(alp2*qkw(k), &
                  & alp6*edmf_a1(k)*edmf_w1(k)) / bv &
                  & *( 1.0 + alp3*SQRT( vsc/( bv*elt ) ) )
              elb = MIN(alp5*qkw(k)/bv, zwk)
              elf = elb/(1. + (elb/600.))
           ELSE
              tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(flt,1.0e-4))**(0.3333)),50.),150.)
              wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
              tau_cloud = tau_cloud*(1.-wt) + 50.*wt
              elb = MIN(tau_cloud*SQRT(MIN(qtke(k),30.)), zwk)
              elf = elb
              elb_mf = elb
         END IF
         z_m = MAX(0.,zwk - 4.)
         IF ( rmo .GT. 0.0 ) THEN
            els = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
            els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
         ELSE
            els = vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
            els1 = vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
         END IF
         wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
         el_unstab = els/(1. + (els1/elt))
         el(k) = MIN(el_unstab, elb_mf)
         el(k) = el(k)*(1.-wt) + elf*wt
         el_les= MIN(els/(1. + (els1/12.)), elb_mf)
         el(k) = el(k)*Psig_bl + (1.-Psig_bl)*el_les
       END DO
    END SELECT
  END SUBROUTINE mym_length
  SUBROUTINE boulac_length0(k,kts,kte,zw,dz,qtke,theta,lb1,lb2)
     INTEGER, INTENT(IN) :: k,kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw
     INTEGER :: izz, found
     REAL :: dlu,dld
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz
     zup=0.
     dlu=zw(kte+1)-zw(k)-dz(k)/2.
     zzz=0.
     zup_inf=0.
     beta=g/theta(k)
     if (k .lt. kte) then
        found = 0
        izz=k
        DO WHILE (found .EQ. 0)
           if (izz .lt. kte) then
              dzt=dz(izz)
              zup=zup-beta*theta(k)*dzt
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              if (qtke(k).lt.zup .and. qtke(k).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    tl=(-beta*(theta(izz)-theta(k)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      & 2.*bbb*beta*(qtke(k)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k))then
                       tl=(qtke(k)-zup_inf)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dlu=zzz-dzt+tl
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
           ELSE
              found = 1
           ENDIF
        ENDDO
     endif
     zdo=0.
     zdo_sup=0.
     dld=zw(k)
     zzz=0.
     if (k .gt. kts) then
        found = 0
        izz=k
        DO WHILE (found .EQ. 0)
           if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(k)*dzt
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              if (qtke(k).lt.zdo .and. qtke(k).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(k))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      & 2.*bbb*beta*(qtke(k)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k)) then
                       tl=(qtke(k)-zdo_sup)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dld=zzz-dzt+tl
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
           ELSE
              found = 1
           ENDIF
        ENDDO
     endif
     dld = min(dld,zw(k+1))
     lb1 = min(dlu,dld)
     dlu=MAX(0.1,MIN(dlu,1000.))
     dld=MAX(0.1,MIN(dld,1000.))
     lb2 = sqrt(dlu*dld)
     if (k .eq. kte) then
        lb1 = 0.
        lb2 = 0.
     endif
  END SUBROUTINE boulac_length0
  SUBROUTINE boulac_length(kts,kte,zw,dz,qtke,theta,lb1,lb2)
     INTEGER, INTENT(IN) :: kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, DIMENSION(kts:kte), INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw
     INTEGER :: iz, izz, found
     REAL, DIMENSION(kts:kte) :: dlu,dld
     REAL, PARAMETER :: Lmax=2000.
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz
     do iz=kts,kte
        zup=0.
        dlu(iz)=zw(kte+1)-zw(iz)-dz(iz)/2.
        zzz=0.
        zup_inf=0.
        beta=g/theta(iz)
        if (iz .lt. kte) then
          found = 0
          izz=iz
          DO WHILE (found .EQ. 0)
            if (izz .lt. kte) then
              dzt=dz(izz)
              zup=zup-beta*theta(iz)*dzt
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              if (qtke(iz).lt.zup .and. qtke(iz).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    tl=(-beta*(theta(izz)-theta(iz)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      & 2.*bbb*beta*(qtke(iz)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz))then
                       tl=(qtke(iz)-zup_inf)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif
                 dlu(iz)=zzz-dzt+tl
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
             ELSE
              found = 1
            ENDIF
          ENDDO
        endif
        zdo=0.
        zdo_sup=0.
        dld(iz)=zw(iz)
        zzz=0.
        if (iz .gt. kts) then
          found = 0
          izz=iz
          DO WHILE (found .EQ. 0)
            if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(iz)*dzt
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              if (qtke(iz).lt.zdo .and. qtke(iz).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(iz))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      & 2.*bbb*beta*(qtke(iz)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz)) then
                       tl=(qtke(iz)-zdo_sup)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif
                 dld(iz)=zzz-dzt+tl
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
            ELSE
              found = 1
            ENDIF
          ENDDO
        endif
        dld(iz) = min(dld(iz),zw(iz+1))
        lb1(iz) = min(dlu(iz),dld(iz))
        dlu(iz)=MAX(0.1,MIN(dlu(iz),1000.))
        dld(iz)=MAX(0.1,MIN(dld(iz),1000.))
        lb2(iz) = sqrt(dlu(iz)*dld(iz))
        lb1(iz) = lb1(iz)/(1. + (lb1(iz)/Lmax))
        lb2(iz) = lb2(iz)/(1. + (lb2(iz)/Lmax))
        if (iz .eq. kte) then
           lb1(kte) = lb1(kte-1)
           lb2(kte) = lb2(kte-1)
        endif
     ENDDO
  END SUBROUTINE boulac_length
  SUBROUTINE mym_turbulence ( &
    & kts,kte, &
    & levflag, &
    & dz, zw, &
    & u, v, thl, ql, qw, &
    & qke, tsq, qsq, cov, &
    & vt, vq, &
    & rmo, flt, flq, &
    & zi,theta, &
    & sh, &
    & El, &
    & Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
    & qWT1D,qSHEAR1D,qBUOY1D,qDISS1D, &
    & bl_mynn_tkebudget, &
    & Psig_bl,Psig_shcu,cldfra_bl1D,bl_mynn_mixlength,&
    & edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf, &
    & TKEprodTD, &
    & spp_pbl,rstoch_col)
    INTEGER, INTENT(IN) :: kts,kte
    INTEGER, INTENT(IN) :: levflag,bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,Psig_shcu
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,&
         &ql,vt,vq,qke,tsq,qsq,cov,cldfra_bl1D,edmf_w1,edmf_a1,edmf_qc1,&
         &TKEprodTD
    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el
    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh
    INTEGER :: k
    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh
    REAL :: zi, cldavg
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta
    REAL :: a2den, duz, ri, HLmod
    REAL:: auh,aum,adh,adm,aeh,aem,Req,Rsl,Rsl2
    DOUBLE PRECISION q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION e1, e2, e3, e4, enum, eden, wden
    INTEGER, INTENT(IN) :: spp_pbl
    REAL, DIMENSION(KTS:KTE) :: rstoch_col
    REAL :: prlimit
    CALL mym_level2 (kts,kte,&
    & dz, &
    & u, v, thl, qw, &
    & ql, vt, vq, &
    & dtl, dqw, dtv, gm, gh, sm, sh )
    CALL mym_length ( &
    & kts,kte, &
    & dz, zw, &
    & rmo, flt, flq, &
    & vt, vq, &
    & qke, &
    & dtv, &
    & el, &
    & zi,theta, &
    & qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength, &
    & edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf )
    DO k = kts+1,kte
       dzk = 0.5 *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )
       q3sq = qkw(k)**2
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz = duz /dzk**2
       ri = -gh(k)/MAX( duz, 1.0e-10 )
       IF (CKmod .eq. 1) THEN
          a2den = 1. + MAX(ri,0.0)
       ELSE
          a2den = 1. + 0.0
       ENDIF
       gmel = gm (k)*elsq
       ghel = gh (k)*elsq
       IF ( debug_code ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
           print*,"MYNN; mym_turbulence2.0; sh=",sh(k)," k=",k
           print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)," ri=",ri
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
         ENDIF
       ENDIF
       IF (CKmod .eq. 1) THEN
          HLmod = q2sq -1.
       ELSE
          HLmod = q3sq
       ENDIF
          dlsq = elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)
       IF ( q3sq .LT. q2sq ) THEN
          qdiv = SQRT( q3sq/q2sq )
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv
          e1 = q3sq - e1c*ghel/a2den * qdiv**2
          e2 = q3sq - e2c*ghel/a2den * qdiv**2
          e3 = e1 + e3c*ghel/(a2den**2) * qdiv**2
          e4 = e1 - e4c*ghel/a2den * qdiv**2
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE
          e1 = q3sq - e1c*ghel/a2den
          e2 = q3sq - e2c*ghel/a2den
          e3 = e1 + e3c*ghel/(a2den**2)
          e4 = e1 - e4c*ghel/a2den
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )
          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4 )/eden
          sh(k) = q3sq*(a2/a2den)*( e2+3.0*c1*e5c*gmel )/eden
       END IF
       IF ( debug_code ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
           print*,"MYNN; mym_turbulence2.5; sh=",sh(k)," k=",k
           print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)," ri=",ri
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
         ENDIF
       ENDIF
       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq = cov(k)*abk+cov(k-1)*afk
          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
          vtt = 1.0 +vt(k)*abk +vt(k-1)*afk
          vqq = tv0 +vq(k)*abk +vq(k-1)*afk
          t2sq = vtt*t2sq +vqq*c2sq
          r2sq = vtt*c2sq +vqq*r2sq
          c2sq = MAX( vtt*t2sq+vqq*r2sq, 0.0d0 )
          t3sq = vtt*t3sq +vqq*c3sq
          r3sq = vtt*c3sq +vqq*r3sq
          c3sq = MAX( vtt*t3sq+vqq*r3sq, 0.0d0 )
          cw25 = e1*( e2 + 3.0*c1*e5c*gmel*qdiv**2 )/( 3.0*eden )
          dlsq = elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)
          auh = 27.*a1*((a2/a2den)**2)*b2*(g/tref)**2
          aum = 54.*(a1**2)*(a2/a2den)*b2*c1*(g/tref)
          adh = 9.*a1*((a2/a2den)**2)*(12.*a1 + 3.*b2)*(g/tref)**2
          adm = 18.*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))*(g/tref)
          aeh = (9.*a1*((a2/a2den)**2)*b1 +9.*a1*((a2/a2den)**2)* &
                (12.*a1 + 3.*b2))*(g/tref)
          aem = 3.*a1*(a2/a2den)*b1*(3.*(a2/a2den) + 3.*b2*c1 + &
                (18.*a1*c1 - b2)) + &
                (18.)*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))
          Req = -aeh/aem
          Rsl = (auh + aum*Req)/(3.*adh + 3.*adm*Req)
          Rsl = .12
          Rsl2= 1.0 - 2.*Rsl
          e2 = q3sq - e2c*ghel/a2den * qdiv**2
          e3 = q3sq + e3c*ghel/(a2den**2) * qdiv**2
          e4 = q3sq - e4c*ghel/a2den * qdiv**2
          eden = e2*e4 + e3 *e5c*gmel * qdiv**2
          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               & *( e2*e4c/a2den - e3c*e5c*gmel/(a2den**2) * qdiv**2 )
          IF ( wden .NE. 0.0 ) THEN
             clow = q3sq*( Rsl -cw25 )*eden/wden
             cupp = q3sq*( Rsl2-cw25 )*eden/wden
             IF ( wden .GT. 0.0 ) THEN
                c3sq = MIN( MAX( c3sq, c2sq+clow ), c2sq+cupp )
             ELSE
                c3sq = MAX( MIN( c3sq, c2sq+clow ), c2sq+cupp )
             END IF
          END IF
          e1 = e2 + e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
          e6c = 3.0*(a2/a2den)*cc3*gtr * dlsq/elsq
          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF
          gamt =-e1 *enum /eden
          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF
          gamq =-e1 *enum /eden
          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)
          smd = dlsq*enum*gtr/eden * qdiv**2 * (e3c/(a2den**2) + &
               & e4c/a2den)*a1/(a2/a2den)
          gamv = e1 *enum*gtr/eden
          sm(k) = sm(k) +smd
          qdiv = 1.0
          IF ( debug_code ) THEN
            IF (sh(k)<-0.3 .OR. sm(k)<-0.3 .OR. &
              qke(k) < -0.1 .or. ABS(smd) .gt. 2.0) THEN
              print*," MYNN; mym_turbulence3.0; sh=",sh(k)," k=",k
              print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
              print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
              print*," qke=",qke(k)," el=",el(k)," ri=",ri
              print*," PBLH=",zi," u=",u(k)," v=",v(k)
            ENDIF
          ENDIF
       ELSE
          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF
       if (spp_pbl==1) then
          prlimit = MIN(MAX(1.,2.5 + 5.0*rstoch_col(k)), 10.)
          IF(sm(k) > sh(k)*Prlimit) THEN
             sm(k) = sh(k)*Prlimit
          ENDIF
       ENDIF
       cldavg = 0.5*(cldfra_bl1D(k-1) + cldfra_bl1D(k))
       IF (edmf_a1(k) > 0.001 .OR. cldavg > 0.02) THEN
           cldavg = 0.5*(cldfra_bl1D(k-1) + cldfra_bl1D(k))
           sm(k) = MAX(sm(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           sh(k) = MAX(sh(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           sm(k) = MAX(sm(k), 0.03*MIN(cldavg,1.0) )
           sh(k) = MAX(sh(k), 0.03*MIN(cldavg,1.0) )
       ENDIF
       elq = el(k)*qkw(k)
       elh = elq*qdiv
       pdk(k) = elq*( sm(k)*gm(k) &
            & +sh(k)*gh(k)+gamv ) + &
            & TKEprodTD(k)
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5
       tcd(k) = elq*gamt
       qcd(k) = elq*gamq
       dfm(k) = elq*sm(k) / dzk
       dfh(k) = elq*sh(k) / dzk
       dfq(k) = dfm(k)
   IF ( bl_mynn_tkebudget == 1) THEN
       dudz = ( u(k)-u(k-1) )/dzk
       dvdz = ( v(k)-v(k-1) )/dzk
       dTdz = ( thl(k)-thl(k-1) )/dzk
       upwp = -elq*sm(k)*dudz
       vpwp = -elq*sm(k)*dvdz
       Tpwp = -elq*sh(k)*dTdz
       Tpwp = SIGN(MAX(ABS(Tpwp),1.E-6),Tpwp)
       IF ( k .EQ. kts+1 ) THEN
          qWT1D(kts)=0.
          q3sq_old =0.
          qWTP_old =0.
          dlsq1 = MAX(el(kts)**2,1.0)
          IF ( q3sq_old/dlsq1 .LT. -gh(k) ) q3sq_old = -dlsq1*gh(k)
       ENDIF
       qWTP_new = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       qWT1D(k) = 0.5*(qWTP_new - qWTP_old)/dzk
       qWTP_old = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       q3sq_old = q3sq
       qSHEAR1D(k) = elq*sm(k)*gm(k)
       qBUOY1D(k) = elq*(sh(k)*(-dTdz*g/thl(k)) + gamv)
       qDISS1D(k) = (q3sq**(3./2.))/(b1*MAX(el(k),1.))
    ENDIF
    END DO
    dfm(kts) = 0.0
    dfh(kts) = 0.0
    dfq(kts) = 0.0
    tcd(kts) = 0.0
    qcd(kts) = 0.0
    tcd(kte) = 0.0
    qcd(kte) = 0.0
    DO k = kts,kte-1
       dzk = dz(k)
       tcd(k) = ( tcd(k+1)-tcd(k) )/( dzk )
       qcd(k) = ( qcd(k+1)-qcd(k) )/( dzk )
    END DO
   IF ( bl_mynn_tkebudget == 1) THEN
      qWT1D(kts)=0.
      qSHEAR1D(kts)=qSHEAR1D(kts+1)
      qBUOY1D(kts)=qBUOY1D(kts+1)
      qDISS1D(kts)=qDISS1D(kts+1)
   ENDIF
    if (spp_pbl==1) then
       DO k = kts,kte
          dfm(k)= dfm(k) + dfm(k)* rstoch_col(k) * 1.5 * MAX(exp(-MAX(zw(k)-8000.,0.0)/2000.),0.001)
          dfh(k)= dfh(k) + dfh(k)* rstoch_col(k) * 1.5 * MAX(exp(-MAX(zw(k)-8000.,0.0)/2000.),0.001)
       END DO
    endif
  END SUBROUTINE mym_turbulence
  SUBROUTINE mym_predict (kts,kte,&
       & levflag, &
       & delt,&
       & dz, &
       & ust, flt, flq, pmz, phh, &
       & el, dfq, &
       & pdk, pdt, pdq, pdc,&
       & qke, tsq, qsq, cov, &
       & s_aw,s_awqke,bl_mynn_edmf_tke &
       &)
    INTEGER, INTENT(IN) :: kts,kte
    INTEGER, INTENT(IN) :: levflag
    INTEGER, INTENT(IN) :: bl_mynn_edmf_tke
    REAL, INTENT(IN) :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN) :: flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov
    REAL, DIMENSION(kts:kte+1), INTENT(INOUT) :: s_awqke,s_aw
    INTEGER :: k
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l,onoff
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x
    IF (bl_mynn_edmf_tke == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF
    vkz = vk*0.5*dz(kts)
    DO k = kts,kte
       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       df3q(k)=Sqfac*dfq(k)
       dtz(k)=delt/dz(k)
    END DO
    pdk1 = 2.0*ust**3*pmz/( vkz )
    phm = 2.0/ust *phh/( vkz )
    pdt1 = phm*flt**2
    pdq1 = phm*flq**2
    pdc1 = phm*flt*flq
    pdk(kts) = pdk1 -pdk(kts+1)
    pdt(kts) = pdt(kts+1)
    pdq(kts) = pdq(kts+1)
    pdc(kts) = pdc(kts+1)
    DO k = kts,kte-1
       b1l = b1*0.5*( el(k+1)+el(k) )
       bp(k) = 2.*qkw(k) / b1l
       rp(k) = pdk(k+1) + pdk(k)
    END DO
    DO k=kts,kte-1
       a(k-kts+1)=-dtz(k)*df3q(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(k-kts+1)=1. + dtz(k)*(df3q(k)+df3q(k+1)) &
                     + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff + bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k-kts+1)=rp(k)*delt + qke(k) + dtz(k)*(s_awqke(k)-s_awqke(k+1))*onoff
    ENDDO
    a(kte)=-1.
    b(kte)=1.
    c(kte)=0.
    d(kte)=0.
    CALL tridiag2(kte,a,b,c,d,x)
    DO k=kts,kte
       qke(k)=max(x(k), 1.e-4)
    ENDDO
    IF ( levflag .EQ. 3 ) THEN
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdt(k+1) + pdt(k)
       END DO
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + tsq(k)
       ENDDO
       a(kte)=-1.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.
    CALL tridiag2(kte,a,b,c,d,x)
       DO k=kts,kte
           tsq(k)=x(k)
       ENDDO
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) +pdq(k)
       END DO
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + qsq(k)
       ENDDO
       a(kte)=-1.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.
       CALL tridiag2(kte,a,b,c,d,x)
       DO k=kts,kte
           qsq(k)=x(k)
       ENDDO
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdc(k+1) + pdc(k)
       END DO
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + cov(k)
       ENDDO
       a(kte)=-1.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.
    CALL tridiag2(kte,a,b,c,d,x)
       DO k=kts,kte
          cov(k)=x(k)
       ENDDO
    ELSE
       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF
          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       tsq(kte)=tsq(kte-1)
       qsq(kte)=qsq(kte-1)
       cov(kte)=cov(kte-1)
    END IF
  END SUBROUTINE mym_predict
  SUBROUTINE mym_condensation (kts,kte, &
    & dx, dz, zw, &
    & thl, qw, &
    & p,exner, &
    & tsq, qsq, cov, &
    & Sh, el, bl_mynn_cloudpdf,&
    & qc_bl1D, cldfra_bl1D, &
    & PBLH1,HFX1, &
    & Vt, Vq, th, sgm, rmo, &
    & spp_pbl,rstoch_col )
    INTEGER, INTENT(IN) :: kts,kte, bl_mynn_cloudpdf
    REAL, INTENT(IN) :: dx,PBLH1,HFX1,rmo
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov, th
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: vt,vq,sgm
    REAL, DIMENSION(kts:kte) :: qmq,alp,a,bet,b,ql,q1,cld,RH
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qc_bl1D,cldfra_bl1D
    DOUBLE PRECISION :: t3sq, r3sq, c3sq
    REAL :: qsl,esat,qsat,tlk,qsat_tl,dqsl,cld0,q1k,eq1,qll,&
         &q2p,pt,rac,qt,t,xl,rsl,cpm,cdhdz,Fng,qww,alpha,beta,bb,&
         &ls_min,ls,wt,cld_factor,fac_damp
    INTEGER :: i,j,k
    REAL :: erf
    REAL::dth,dtl,dqw,dzk,els
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el
    REAL::zagl,cld9,damp,edown,RHcrit,RHmean,RHsum,RHnum,Hshcu,PBLH2,ql_limit
    REAL, PARAMETER :: Hfac = 3.0
    REAL, PARAMETER :: HFXmin = 50.0
    REAL :: RH_00L, RH_00O, phi_dz, lfac
    REAL, PARAMETER :: cdz = 2.0
    REAL, PARAMETER :: mdz = 1.5
    REAL :: theta1, theta2, ht1, ht2
    INTEGER :: k_tropo
    INTEGER, INTENT(IN) :: spp_pbl
    REAL, DIMENSION(KTS:KTE) :: rstoch_col
    REAL :: qw_pert
    DO k = kte-3, kts, -1
       theta1 = th(k)
       theta2 = th(k+2)
       ht1 = 44307.692 * (1.0 - (p(k)/101325.)**0.190)
       ht2 = 44307.692 * (1.0 - (p(k+2)/101325.)**0.190)
       if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND. &
     & (ht1.lt.19000.) .and. (ht1.gt.4000.) ) then
          goto 86
       endif
    ENDDO
 86 continue
    k_tropo = MAX(kts+2, k+2)
    zagl = 0.
    SELECT CASE(bl_mynn_cloudpdf)
      CASE (0)
        DO k = kts,kte-1
           t = th(k)*exner(k)
           esat = esat_blend(t)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           dqsl = qsl*ep_2*ev/( rd*t**2 )
           RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)
           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)
           t3sq = MAX( tsq(k), 0.0 )
           r3sq = MAX( qsq(k), 0.0 )
           c3sq = cov(k)
           c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
           r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
           qmq(k) = qw(k) -qsl
           sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
           q1(k) = qmq(k) / sgm(k)
           cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )
        END DO
      CASE (1, -1)
        DO k = kts,kte-1
           t = th(k)*exner(k)
           esat = esat_blend(t)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           dqsl = qsl*ep_2*ev/( rd*t**2 )
           RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)
           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)
           if (k .eq. kts) then
             dzk = 0.5*dz(k)
           else
             dzk = 0.5*( dz(k) + dz(k-1) )
           end if
           dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
           dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
           sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,0.1) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
           qmq(k) = qw(k) -qsl
           q1(k) = qmq(k) / sgm(k)
           cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )
        END DO
      CASE (2, -2)
        DO k = kts,kte-1
           t = th(k)*exner(k)
           esat = esat_blend(t)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           dqsl = qsl*ep_2*ev/( rd*t**2 )
           RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)
           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)
           xl = xl_blend(t)
           tlk = thl(k)*(p(k)/p1000mb)**rcp
           qsat_tl = qsat_blend(tlk,p(k))
           rsl = xl*qsat_tl / (r_v*tlk**2)
           cpm = cp + qw(k)*cpv
           a(k) = 1./(1. + xl*rsl/cpm)
           qw_pert = qw(k) + qw(k)*0.5*rstoch_col(k)*real(spp_pbl)
           qmq(k) = a(k) * (qw_pert - qsat_tl)
           b(k) = a(k)*rsl
           dtl = 0.5*(thl(k+1)*(p(k+1)/p1000mb)**rcp + tlk) &
               & - 0.5*(tlk + thl(MAX(k-1,kts))*(p(MAX(k-1,kts))/p1000mb)**rcp)
           dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
           if (k .eq. kts) then
             dzk = 0.5*dz(k)
           else
             dzk = 0.5*( dz(k) + dz(k-1) )
           end if
           cdhdz = dtl/dzk + (g/cpm)*(1.+qw(k))
           zagl = zagl + dz(k)
           els = zagl
           ls_min = 300. + MIN(3.*MAX(HFX1,0.),300.)
           ls_min = MIN(MAX(els,25.),ls_min)
           if (zagl > PBLH1+2000.) ls_min = MAX(ls_min + 0.5*(PBLH1+2000.-zagl),300.)
           lfac=MIN(4.25+dx/4000.,6.)
           ls = MAX(MIN(lfac*el(k),600.),ls_min)
           sgm(k) = MAX(1.e-10, 0.225*ls*SQRT(MAX(0., &
                   & (a(k)*dqw/dzk)**2 &
                   & -2*a(k)*b(k)*cdhdz*dqw/dzk &
                   & +b(k)**2 * cdhdz**2)))
           q1(k) = qmq(k) / sgm(k)
           cld(k) = MAX(0., MIN(1., 0.5+0.36*ATAN(1.55*q1(k))))
         END DO
    END SELECT
    zagl = 0.
    RHsum=0.
    RHnum=0.
    RHmean=0.1
    damp =0
    PBLH2=MAX(10.,PBLH1)
    SELECT CASE(bl_mynn_cloudpdf)
      CASE (-1 : 1)
        DO k = kts,kte-1
           t = th(k)*exner(k)
           q1k = q1(k)
           zagl = zagl + dz(k)
           IF (zagl < PBLH2 .AND. PBLH2 > 400.) THEN
              RHsum=RHsum+RH(k)
              RHnum=RHnum+1.0
              RHmean=RHsum/RHnum
           ENDIF
           RHcrit = 1. - 0.35*(1.0 - (MAX(250.- MAX(HFX1,HFXmin),0.0)/200.)**2)
           if (HFX1 > HFXmin) then
              cld9=MIN(MAX(0., (rh(k)-RHcrit)/(1.1-RHcrit)), 1.)**2
           else
              cld9=0.0
           endif
           edown=PBLH2*.1
           Hshcu=200. + (RHmean+0.5)**1.5*MAX(HFX1,0.)*Hfac
           if (zagl < PBLH2-edown) then
              damp=MIN(1.0,exp(-ABS(((PBLH2-edown)-zagl)/edown)))
           elseif(zagl >= PBLH2-edown .AND. zagl < PBLH2+Hshcu)then
              damp=1.
           elseif (zagl >= PBLH2+Hshcu)then
              damp=MIN(1.0,exp(-ABS((zagl-(PBLH2+Hshcu))/500.)))
           endif
           cldfra_bl1D(k)=cld9*damp
           eq1 = rrp*EXP( -0.5*q1k*q1k )
           qll = MAX( cldfra_bl1D(k)*q1k + eq1, 0.0 )
           ql (k) = alp(k)*sgm(k)*qll
           if(cldfra_bl1D(k)>0.01 .and. ql(k)<1.E-6)ql(k)=1.E-6
           qc_bl1D(k)=ql(k)*damp
           eq1 = rrp*EXP( -0.5*q1k*q1k )
           qll = MAX( cld(k)*q1k + eq1, 0.0 )
           ql (k) = alp(k)*sgm(k)*qll
           q2p = xlvcp/exner(k)
           pt = thl(k) +q2p*ql(k)
           qt = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
           rac = alp(k)*( cld(k)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )
           vt(k) = qt-1.0 -rac*bet(k)
           vq(k) = p608*pt-tv0 +rac
        END DO
      CASE ( 2, -2)
        DO k = kts,kte-1
           t = th(k)*exner(k)
           q1k = q1(k)
           zagl = zagl + dz(k)
           IF (q1k < 0.) THEN
              ql (k) = sgm(k)*EXP(1.2*q1k-1)
           ELSE IF (q1k > 2.) THEN
              ql (k) = sgm(k)*q1k
           ELSE
              ql (k) = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
           ENDIF
           if (k .ge. k_tropo-1) then
              cld(k) = 0.
               ql(k) = 0.
           endif
           Q1(k)=MAX(Q1(k),-5.0)
           IF (Q1(k) .GE. 1.0) THEN
              Fng = 1.0
           ELSEIF (Q1(k) .GE. -1.7 .AND. Q1(k) < 1.0) THEN
              Fng = EXP(-0.4*(Q1(k)-1.0))
           ELSEIF (Q1(k) .GE. -2.5 .AND. Q1(k) .LE. -1.7) THEN
              Fng = 3.0 + EXP(-3.8*(Q1(k)+1.7))
           ELSE
              Fng = MIN(23.9 + EXP(-1.6*(Q1(k)+2.5)), 60.)
           ENDIF
           Fng = MIN(Fng, 20.)
           xl = xl_blend(t)
           bb = b(k)*t/th(k)
           qww = 1.+0.61*qw(k)
           alpha = 0.61*th(k)
           beta = (th(k)/t)*(xl/cp) - 1.61*th(k)
           vt(k) = qww - MIN(cld(k),0.99)*beta*bb*Fng - 1.
           vq(k) = alpha + MIN(cld(k),0.99)*beta*a(k)*Fng - tv0
           fac_damp = 1. -MIN(MAX( zagl-(PBLH2+1000.),0.0)/ &
                              MAX((zw(k_tropo)-(PBLH2+1000.)),500.), 1.)
           cld_factor = 1.0 + fac_damp*MAX(0.0, ( RH(k) - 0.75 ) / 0.26 )**1.9
           cld(k) = MIN( 1., cld_factor*cld(k) )
           cldfra_bl1D(k) = cld(k)
           qc_bl1D(k) = ql(k)
         END DO
      END SELECT
      IF (bl_mynn_cloudpdf .LT. 0) THEN
         DO k = kts,kte-1
              cldfra_bl1D(k) = 0.0
              qc_bl1D(k) = 0.0
         END DO
      ENDIF
      cld(kte) = cld(kte-1)
      ql(kte) = ql(kte-1)
      vt(kte) = vt(kte-1)
      vq(kte) = vq(kte-1)
      qc_bl1D(kte)=0.
      cldfra_bl1D(kte)=0.
    RETURN
  END SUBROUTINE mym_condensation
  SUBROUTINE mynn_tendencies(kts,kte, &
       &levflag,grav_settling, &
       &delt,dz,rho, &
       &u,v,th,tk,qv,qc,qi,qnc,qni, &
       &p,exner, &
       &thl,sqv,sqc,sqi,sqw, &
       &qnwfa,qnifa, &
       &ust,flt,flq,flqv,flqc,wspd,qcg, &
       &uoce,voce, &
       &tsq,qsq,cov, &
       &tcd,qcd, &
       &dfm,dfh,dfq, &
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqnc,Dqni, &
       &Dqnwfa,Dqnifa, &
       &vdfg1,diss_heat, &
       &s_aw,s_awthl,s_awqt,s_awqv,s_awqc, &
       &s_awu,s_awv, &
       &s_awqnc,s_awqni, &
       &s_awqnwfa,s_awqnifa, &
       &FLAG_QC,FLAG_QI,FLAG_QNC,FLAG_QNI, &
       &FLAG_QNWFA,FLAG_QNIFA, &
       &cldfra_bl1d, &
       &bl_mynn_cloudmix, &
       &bl_mynn_mixqt, &
       &bl_mynn_edmf, &
       &bl_mynn_edmf_mom, &
       &bl_mynn_mixscalars )
    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag
    INTEGER, INTENT(in) :: bl_mynn_cloudmix,bl_mynn_mixqt,&
                           bl_mynn_edmf,bl_mynn_edmf_mom, &
                           bl_mynn_mixscalars
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: s_aw,s_awthl,s_awqt,&
         &s_awqnc,s_awqni,s_awqv,s_awqc,s_awu,s_awv,s_awqnwfa,s_awqnifa
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,tk,qv,qc,qi,qni,qnc,&
         &rho,p,exner,dfq,dz,tsq,qsq,cov,tcd,qcd,cldfra_bl1d,diss_heat
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi,&
         &qnwfa,qnifa,dfm,dfh
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni,dqnc,dqnwfa,dqnifa
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg
    REAL, DIMENSION(kts:kte) :: dtz,vt,vq,dfhc,dfmc
    REAL, DIMENSION(kts:kte) :: sqv2,sqc2,sqi2,sqw2,qni2,qnc2, &
                                qnwfa2,qnifa2
    REAL, DIMENSION(kts:kte) :: zfac,plumeKh
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x
    REAL, DIMENSION(kts:kte+1) :: rhoz, &
          & khdz, kmdz
    REAL :: rhs,gfluxm,gfluxp,dztop,maxdfh,mindfh,maxcf,maxKh,zw
    REAL :: grav_settling2,vdfg1
    REAL :: t,esat,qsl,onoff,kh,km,dzk
    INTEGER :: k,kk
    REAL, PARAMETER :: nonloc = 0.0
    dztop=.5*(dz(kte)+dz(kte-1))
    IF (bl_mynn_edmf_mom == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF
    dtz(kts)=delt/dz(kts)
    kh=dfh(kts)*dz(kts)
    km=dfm(kts)*dz(kts)
    rhoz(kts)=rho(kts)
    khdz(kts)=rhoz(kts)*kh/dz(kts)
    kmdz(kts)=rhoz(kts)*km/dz(kts)
    DO k=kts+1,kte
       dtz(k)=delt/dz(k)
       rhoz(k)=(rho(k)*dz(k-1) + rho(k-1)*dz(k))/(dz(k-1)+dz(k))
       dzk = 0.5 *( dz(k)+dz(k-1) )
       kh = dfh(k)*dzk
       km = dfm(k)*dzk
       khdz(k)= rhoz(k)*kh/dzk
       kmdz(k)= rhoz(k)*km/dzk
    ENDDO
    rhoz(kte+1)=rho(kte)
    kh=dfh(kte)*dz(kte)
    km=dfm(kte)*dz(kte)
    khdz(kte+1)=rhoz(kte+1)*kh/dz(kte)
    kmdz(kte+1)=rhoz(kte+1)*km/dz(kte)
    k=kts
    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
    d(1)=u(k) + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff
    DO k=kts+1,kte-1
       a(k)= - dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(k)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(k)= - dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k)=u(k) + dtz(k)*(s_awu(k)-s_awu(k+1))*onoff
    ENDDO
    a(kte)=0
    b(kte)=1.
    c(kte)=0.
    d(kte)=u(kte)
    CALL tridiag2(kte,a,b,c,d,x)
    DO k=kts,kte
       du(k)=(x(k)-u(k))/delt
    ENDDO
    k=kts
    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)= - dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
    d(1)=v(k) + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff
    DO k=kts+1,kte-1
       a(k)= - dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(k)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(k)= - dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k)=v(k) + dtz(k)*(s_awv(k)-s_awv(k+1))*onoff
    ENDDO
    a(kte)=0
    b(kte)=1.
    c(kte)=0.
    d(kte)=v(kte)
    CALL tridiag2(kte,a,b,c,d,x)
    DO k=kts,kte
       dv(k)=(x(k)-v(k))/delt
    ENDDO
    k=kts
    a(k)=0.
    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    d(k)=thl(k) + dtz(k)*flt + tcd(k)*delt &
        & -dtz(k)*s_awthl(kts+1) + diss_heat(k)*delt*dheat_opt
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=thl(k) + tcd(k)*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1)) &
           & + diss_heat(k)*delt*dheat_opt
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=thl(kte)
    CALL tridiag2(kte,a,b,c,d,x)
    DO k=kts,kte
       thl(k)=x(k)
    ENDDO
IF (bl_mynn_mixqt > 0) THEN
    k=kts
    a(k)=0.
    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqw(k) + dtz(k)*flq + qcd(k)*delt - dtz(k)*s_awqt(k+1)
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqw(k) + qcd(k)*delt + dtz(k)*(s_awqt(k)-s_awqt(k+1))
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqw(kte)
    CALL tridiag2(kte,a,b,c,d,sqw2)
ELSE
    sqw2=sqw
ENDIF
IF (bl_mynn_mixqt == 0) THEN
  IF (bl_mynn_cloudmix > 0 .AND. FLAG_QC) THEN
    k=kts
    a(k)=0.
    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqc(k) + dtz(k)*flqc + qcd(k)*delt -dtz(k)*s_awqc(k+1)
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqc(k) + qcd(k)*delt + dtz(k)*(s_awqc(k)-s_awqc(k+1))
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqc(kte)
    CALL tridiag2(kte,a,b,c,d,sqc2)
  ELSE
    sqc2=sqc
  ENDIF
ENDIF
IF (bl_mynn_mixqt == 0) THEN
    k=kts
    a(k)=0.
    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqv(k) + dtz(k)*flqv + qcd(k)*delt - dtz(k)*s_awqv(k+1)
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)= -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqv(k) + qcd(k)*delt + dtz(k)*(s_awqv(k)-s_awqv(k+1))
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqv(kte)
    CALL tridiag2(kte,a,b,c,d,sqv2)
ELSE
    sqv2=sqv
ENDIF
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN
    k=kts
    a(k)=0.
    b(k)=1.+dtz(k)*dfh(k+1)
    c(k)= -dtz(k)*dfh(k+1)
    d(k)=sqi(k)
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*dfh(k)
       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(k)= -dtz(k)*dfh(k+1)
       d(k)=sqi(k)
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqi(kte)
    CALL tridiag2(kte,a,b,c,d,sqi2)
ELSE
   sqi2=sqi
ENDIF
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNI .AND. &
      bl_mynn_mixscalars > 0) THEN
    k=kts
    a(k)= -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qni(k) - dtz(k)*s_awqni(k+1)*nonloc
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*khdz(k)/rho(k) + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           & 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qni(k) + dtz(k)*(s_awqni(k)-s_awqni(k+1))*nonloc
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qni(kte)
    CALL tridiag3(kte,a,b,c,d,x)
    DO k=kts,kte
       qni2(k)=x(k)
    ENDDO
ELSE
    qni2=qni
ENDIF
  IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNC .AND. &
      bl_mynn_mixscalars > 0) THEN
    k=kts
    a(k)= -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnc(k) - dtz(k)*s_awqnc(k+1)*nonloc
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*khdz(k)/rho(k) + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           & 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnc(k) + dtz(k)*(s_awqnc(k)-s_awqnc(k+1))*nonloc
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnc(kte)
    CALL tridiag3(kte,a,b,c,d,x)
    DO k=kts,kte
       qnc2(k)=x(k)
    ENDDO
ELSE
    qnc2=qnc
ENDIF
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNWFA .AND. &
      bl_mynn_mixscalars > 0) THEN
    k=kts
    a(k)= -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) - &
           & 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnwfa(k) - dtz(k)*s_awqnwfa(k+1)*nonloc
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*khdz(k)/rho(k) + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) + &
           & 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnwfa(k) + dtz(k)*(s_awqnwfa(k)-s_awqnwfa(k+1))*nonloc
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnwfa(kte)
    CALL tridiag3(kte,a,b,c,d,x)
    DO k=kts,kte
       qnwfa2(k)=x(k)
    ENDDO
ELSE
    qnwfa2=qnwfa
ENDIF
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNIFA .AND. &
      bl_mynn_mixscalars > 0) THEN
   k=kts
    a(k)= -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) - &
           & 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnifa(k) - dtz(k)*s_awqnifa(k+1)*nonloc
    DO k=kts+1,kte-1
       a(k)= -dtz(k)*khdz(k)/rho(k) + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) + &
           & 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)= -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnifa(k) + dtz(k)*(s_awqnifa(k)-s_awqnifa(k+1))*nonloc
    ENDDO
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnifa(kte)
    CALL tridiag3(kte,a,b,c,d,x)
    DO k=kts,kte
       qnifa2(k)=x(k)
    ENDDO
ELSE
    qnifa2=qnifa
ENDIF
    IF (bl_mynn_mixqt > 0) THEN
      DO k=kts,kte
         t = th(k)*exner(k)
         esat=esat_blend(t)
         qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
            IF (FLAG_QI) THEN
              sqi2(k) = MAX(0., sqi2(k))
              sqc2(k) = MAX(0., sqw2(k) - sqi2(k) - qsl)
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k) - sqi2(k))
            ELSE
              sqi2(k) = 0.0
              sqc2(k) = MAX(0., sqw2(k) - qsl)
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k))
            ENDIF
      ENDDO
    ENDIF
    DO k=kts,kte
       Dqv(k)=(sqv2(k)/(1.-sqv2(k)) - qv(k))/delt
    ENDDO
    IF (bl_mynn_cloudmix > 0) THEN
      IF (FLAG_QC) THEN
         DO k=kts,kte
            Dqc(k)=(sqc2(k)/(1.-sqv2(k)) - qc(k))/delt
            IF(Dqc(k)*delt + qc(k) < 0.) THEN
              Dqc(k)=-qc(k)/delt
            ENDIF
         ENDDO
      ELSE
         DO k=kts,kte
           Dqc(k) = 0.
         ENDDO
      ENDIF
      IF (FLAG_QNC .AND. bl_mynn_mixscalars > 0) THEN
         DO k=kts,kte
           Dqnc(k) = (qnc2(k)-qnc(k))/delt
         ENDDO
      ELSE
         DO k=kts,kte
           Dqnc(k) = 0.
         ENDDO
      ENDIF
      IF (FLAG_QI) THEN
         DO k=kts,kte
           Dqi(k)=(sqi2(k)/(1.-sqv2(k)) - qi(k))/delt
           IF(Dqi(k)*delt + qi(k) < 0.) THEN
              Dqi(k)=-qi(k)/delt
           ENDIF
         ENDDO
      ELSE
         DO k=kts,kte
           Dqi(k) = 0.
         ENDDO
      ENDIF
      IF (FLAG_QNI .AND. bl_mynn_mixscalars > 0) THEN
         DO k=kts,kte
           Dqni(k)=(qni2(k)-qni(k))/delt
         ENDDO
      ELSE
         DO k=kts,kte
           Dqni(k)=0.
         ENDDO
      ENDIF
    ELSE
      DO k=kts,kte
         Dqc(k)=0.
         Dqnc(k)=0.
         Dqi(k)=0.
         Dqni(k)=0.
      ENDDO
    ENDIF
    IF (FLAG_QI) THEN
      DO k=kts,kte
         Dth(k)=(thl(k) + xlvcp/exner(k)*sqc(k) &
           & + xlscp/exner(k)*sqi(k) &
           & - th(k))/delt
      ENDDO
    ELSE
      DO k=kts,kte
         Dth(k)=(thl(k)+xlvcp/exner(k)*sqc2(k) - th(k))/delt
      ENDDO
    ENDIF
    IF (FLAG_QNWFA .AND. FLAG_QNIFA .AND. &
        bl_mynn_mixscalars > 0) THEN
       DO k=kts,kte
          Dqnwfa(k)=(qnwfa2(k) - qnwfa(k))/delt
          Dqnifa(k)=(qnifa2(k) - qnifa(k))/delt
       ENDDO
    ELSE
       DO k=kts,kte
          Dqnwfa(k)=0.
          Dqnifa(k)=0.
       ENDDO
    ENDIF
  END SUBROUTINE mynn_tendencies
  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dz,K_m,K_h)
    INTEGER , INTENT(in) :: kts,kte
    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh
    REAL, DIMENSION(KtS:KtE), INTENT(out) :: K_m, K_h
    INTEGER :: k
    REAL :: dzk
    K_m(kts)=0.
    K_h(kts)=0.
    DO k=kts+1,kte
       dzk = 0.5 *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
    ENDDO
  END SUBROUTINE retrieve_exchange_coeffs
  SUBROUTINE tridiag(n,a,b,c,d)
    INTEGER, INTENT(in):: n
    REAL, DIMENSION(n), INTENT(in) :: a,b
    REAL, DIMENSION(n), INTENT(inout) :: c,d
    INTEGER :: i
    REAL :: p
    REAL, DIMENSION(n) :: q
    c(n)=0.
    q(1)=-c(1)/b(1)
    d(1)=d(1)/b(1)
    DO i=2,n
       p=1./(b(i)+a(i)*q(i-1))
       q(i)=-c(i)*p
       d(i)=(d(i)-a(i)*d(i-1))*p
    ENDDO
    DO i=n-1,1,-1
       d(i)=d(i)+q(i)*d(i+1)
    ENDDO
  END SUBROUTINE tridiag
      subroutine tridiag2(n,a,b,c,d,x)
      implicit none
        integer,intent(in) :: n
        real, dimension(n),intent(in) :: a,b,c,d
        real ,dimension(n),intent(out) :: x
        real ,dimension(n) :: cp,dp
        real :: m
        integer :: i
        cp(1) = c(1)/b(1)
        dp(1) = d(1)/b(1)
        do i = 2,n
           m = b(i)-cp(i-1)*a(i)
           cp(i) = c(i)/m
           dp(i) = (d(i)-dp(i-1)*a(i))/m
        enddo
        x(n) = dp(n)
        do i = n-1, 1, -1
           x(i) = dp(i)-cp(i)*x(i+1)
        end do
    end subroutine tridiag2
       subroutine tridiag3(kte,a,b,c,d,x)
       implicit none
        integer,intent(in) :: kte
        integer, parameter :: kts=1
        real, dimension(kte) :: a,b,c,d
        real ,dimension(kte),intent(out) :: x
        integer :: in
        do in=kte-1,kts,-1
         d(in)=d(in)-c(in)*d(in+1)/b(in+1)
         b(in)=b(in)-c(in)*a(in+1)/b(in+1)
        enddo
        do in=kts+1,kte
         d(in)=d(in)-a(in)*d(in-1)/b(in-1)
        enddo
        do in=kts,kte
         x(in)=d(in)/b(in)
        enddo
        return
        end subroutine tridiag3
  SUBROUTINE mynn_bl_driver( &
       &initflag,restart,cycling, &
       &grav_settling, &
       &delt,dz,dx,znt, &
       &u,v,w,th,qv,qc,qi,qnc,qni, &
       &qnwfa,qnifa, &
       &p,exner,rho,T3D, &
       &xland,ts,qsfc,qcg,ps, &
       &ust,ch,hfx,qfx,rmol,wspd, &
       &uoce,voce, &
       &vdfg, &
       &Qke, &
       &qke_adv,bl_mynn_tkeadvect, &
       &Tsq,Qsq,Cov, &
       &RUBLTEN,RVBLTEN,RTHBLTEN, &
       &RQVBLTEN,RQCBLTEN,RQIBLTEN, &
       &RQNCBLTEN,RQNIBLTEN, &
       &RQNWFABLTEN,RQNIFABLTEN, &
       &exch_h,exch_m, &
       &Pblh,kpbl, &
       &el_pbl, &
       &dqke,qWT,qSHEAR,qBUOY,qDISS, &
       &wstar,delta, &
       &bl_mynn_tkebudget, &
       &bl_mynn_cloudpdf,Sh3D, &
       &bl_mynn_mixlength, &
       &icloud_bl,qc_bl,cldfra_bl, &
       &bl_mynn_edmf, &
       &bl_mynn_edmf_mom,bl_mynn_edmf_tke, &
       &bl_mynn_mixscalars, &
       &bl_mynn_cloudmix,bl_mynn_mixqt, &
       &edmf_a,edmf_w,edmf_qt, &
       &edmf_thl,edmf_ent,edmf_qc, &
       &nupdraft,maxMF,ktop_plume, &
       &kbot_shallow,mf_at_base, &
       &spp_pbl,pattern_spp_pbl, &
       &RTHRATEN, &
       &FLAG_QC,FLAG_QI,FLAG_QNC, &
       &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA &
       &,IDS,IDE,JDS,JDE,KDS,KDE &
       &,IMS,IME,JMS,JME,KMS,KME &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    INTEGER, INTENT(in) :: initflag
    LOGICAL, INTENT(IN) :: restart,cycling
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    INTEGER, INTENT(in) :: bl_mynn_mixlength
    INTEGER, INTENT(in) :: bl_mynn_edmf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
    INTEGER, INTENT(in) :: bl_mynn_edmf_mom
    INTEGER, INTENT(in) :: bl_mynn_edmf_tke
    INTEGER, INTENT(in) :: bl_mynn_mixscalars
    INTEGER, INTENT(in) :: bl_mynn_cloudmix
    INTEGER, INTENT(in) :: bl_mynn_mixqt
    INTEGER, INTENT(in) :: icloud_bl
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE
    REAL, INTENT(in) :: delt
    REAL, INTENT(in) :: dx
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,w,th,qv,p,exner,rho,T3D
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(in)::&
         &qc,qi,qni,qnc,qnwfa,qnifa
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce, vdfg,znt
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
         &qke_adv
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
         &RQIBLTEN,RQNIBLTEN,RTHRATEN,RQNCBLTEN, &
         &RQNWFABLTEN,RQNIFABLTEN
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &exch_h,exch_m
   REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(inout) :: &
         & edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh,wstar,delta
    REAL, DIMENSION(IMS:IME,JMS:JME) :: &
         &Psig_bl,Psig_shcu
    INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: &
         &KPBL,nupdraft,ktop_plume,kbot_shallow
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT) :: &
         &maxmf,mf_at_base
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1,diss_heat
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: Sh3D
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &qc_bl,cldfra_bl
    REAL, DIMENSION(KTS:KTE) :: qc_bl1D,cldfra_bl1D,&
                            qc_bl1D_old,cldfra_bl1D_old
    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,thvl,tl,sqv,sqc,sqi,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
         &Vt, Vq, sgm, thlsg
    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,w1,p1,ex1,dz1,th1,tk1,rho1,&
           & qke1,tsq1,qsq1,cov1,qv1,qi1,qc1,du1,dv1,dth1,dqv1,dqc1,dqi1, &
           & k_m1,k_h1,qni1,dqni1,qnc1,dqnc1,qnwfa1,qnifa1,dqnwfa1,dqnifa1
    REAL, DIMENSION(KTS:KTE) :: dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf
    REAL, DIMENSION(KTS:KTE) :: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,&
                                edmf_ent1,edmf_qc1
    REAL,DIMENSION(KTS:KTE+1) :: s_aw1,s_awthl1,s_awqt1,&
                  s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1,&
                  s_awqnc1,s_awqni1,s_awqnwfa1,s_awqnifa1
    REAL, DIMENSION(KTS:KTE+1) :: zw
    REAL :: cpm,sqcg,flt,flq,flqv,flqc,pmz,phh,exnerg,zet,&
          & afk,abk,ts_decay,th_sfc,ztop_plume,sqc9,sqi9
   real,parameter :: d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter :: h1 = 0.33333335, h2 = 0.6666667
   REAL :: govrth, sflux, bfx0, wstar3, wm2, wm3, delb
   REAL, DIMENSION(ITS:ITE,JTS:JTE) :: maxKHtopdown
   REAL,DIMENSION(KTS:KTE) :: KHtopdown,zfac,wscalek2,&
                             zfacent,TKEprodTD
   REAL :: bfxpbl,dthvx,tmp1,temps,templ,zl1,wstar3_2
   real :: ent_eff,radsum,radflux,we,rcldb,rvls,&
           minrad,zminrad
   real, parameter :: pfac =2.0, zfmin = 0.01, phifac=8.0
   integer :: kk,kminrad
   logical :: cloudflg
    INTEGER, SAVE :: levflag
    LOGICAL :: INITIALIZE_QKE
     INTEGER, INTENT(IN) ::spp_pbl
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN),OPTIONAL ::pattern_spp_pbl
     REAL, DIMENSION(KTS:KTE) :: rstoch_col
    IF ( debug_code ) THEN
       print*,'in MYNN driver; at beginning'
    ENDIF
    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2
    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)
    levflag=mynn_level
    IF (bl_mynn_edmf > 0) THEN
      IF (bl_mynn_edmf > 1) THEN
         edmf_a(its:ite,kts:kte,jts:jte)=0.
         edmf_w(its:ite,kts:kte,jts:jte)=0.
         edmf_qt(its:ite,kts:kte,jts:jte)=0.
         edmf_thl(its:ite,kts:kte,jts:jte)=0.
         edmf_ent(its:ite,kts:kte,jts:jte)=0.
         edmf_qc(its:ite,kts:kte,jts:jte)=0.
      ENDIF
      ktop_plume(its:ite,jts:jte)=0
      kbot_shallow(its:ite,jts:jte)=0
      nupdraft(its:ite,jts:jte)=0
      maxmf(its:ite,jts:jte)=0.
      mf_at_base(its:ite,jts:jte)=0.
    ENDIF
    maxKHtopdown(its:ite,jts:jte)=0.
    IF (initflag > 0) THEN
       IF ( (restart .or. cycling)) THEN
          IF (MAXVAL(QKE(its:ite,kts,jts:jte)) < 0.0002) THEN
             INITIALIZE_QKE = .TRUE.
          ELSE
             INITIALIZE_QKE = .FALSE.
          ENDIF
       ELSE
          INITIALIZE_QKE = .TRUE.
       ENDIF
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.
       dqc1(kts:kte)=0.0
       dqi1(kts:kte)=0.0
       dqni1(kts:kte)=0.0
       dqnc1(kts:kte)=0.0
       dqnwfa1(kts:kte)=0.0
       dqnifa1(kts:kte)=0.0
       qc_bl1D(kts:kte)=0.0
       cldfra_bl1D(kts:kte)=0.0
       qc_bl1D_old(kts:kte)=0.0
       cldfra_bl1D_old(kts:kte)=0.0
       edmf_a1(kts:kte)=0.0
       edmf_w1(kts:kte)=0.0
       edmf_qc1(kts:kte)=0.0
       sgm(kts:kte)=0.0
       vt(kts:kte)=0.0
       vq(kts:kte)=0.0
       DO j=JTS,JTF
          DO k=KTS,KTE
             DO i=ITS,ITF
                exch_m(i,k,j)=0.
                exch_h(i,k,j)=0.
            ENDDO
         ENDDO
       ENDDO
       IF ( bl_mynn_tkebudget == 1) THEN
         DO j=JTS,JTF
            DO k=KTS,KTE
               DO i=ITS,ITF
                  qWT(i,k,j)=0.
                  qSHEAR(i,k,j)=0.
                  qBUOY(i,k,j)=0.
                  qDISS(i,k,j)=0.
                  dqke(i,k,j)=0.
               ENDDO
            ENDDO
         ENDDO
       ENDIF
       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTE
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                w1(k) = w(i,k,j)
                th1(k)=th(i,k,j)
                tk1(k)=T3D(i,k,j)
                rho1(k)=rho(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qv(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                IF (PRESENT(qi) .AND. FLAG_QI ) THEN
                   sqi(k)=qi(i,k,j)/(1.+qv(i,k,j))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc(k) &
                       & - xlscp/exner(i,k,j)*sqi(k)
                   IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL(i,k,j)>0.001)THEN
                      sqc9=QC_BL(i,k,j)*(MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                      sqi9=QC_BL(i,k,j)*(1. - MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                   ELSE
                      sqc9=sqc(k)
                      sqi9=sqi(k)
                   ENDIF
                   thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                         & - xlscp/exner(i,k,j)*sqi9
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                   IF(sqc(k)<1e-6 .and. CLDFRA_BL(i,k,j)>0.001)THEN
        sqc9=QC_BL(i,k,j)*(MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                      sqi9=QC_BL(i,k,j)*(1. - MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                   ELSE
                      sqc9=sqc(k)
                      sqi9=0.0
                   ENDIF
                   thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                         & - xlscp/exner(i,k,j)*sqi9
                ENDIF
                thvl(k)=thlsg(k)*(1.+0.61*sqv(k))
                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF
                IF (INITIALIZE_QKE) THEN
                   qke1(k)=5.*ust(i,j) * MAX((ust(i,j)*700. - zw(k))/(MAX(ust(i,j),0.01)*700.), 0.01)
                ELSE
                   qke1(k)=qke(i,k,j)
                ENDIF
                el(k)=el_pbl(i,k,j)
                sh(k)=Sh3D(i,k,j)
                tsq1(k)=tsq(i,k,j)
                qsq1(k)=qsq(i,k,j)
                cov1(k)=cov(i,k,j)
                if (spp_pbl==1) then
                    rstoch_col(k)=pattern_spp_pbl(i,k,j)
                else
                    rstoch_col(k)=0.0
                endif
             ENDDO
             zw(kte+1)=zw(kte)+dz(i,kte,j)
             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thvl,&
               & Qke1,zw,dz1,xland(i,j),KPBL(i,j))
             IF (scaleaware > 0.) THEN
                CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
             ELSE
                Psig_bl(i,j)=1.0
                Psig_shcu(i,j)=1.0
             ENDIF
             CALL mym_initialize ( &
                  &kts,kte, &
                  &dz1, zw, u1, v1, thl, sqv, &
                  &PBLH(i,j), th1, sh, &
                  &ust(i,j), rmol(i,j), &
                  &el, Qke1, Tsq1, Qsq1, Cov1, &
                  &Psig_bl(i,j), cldfra_bl1D, &
                  &bl_mynn_mixlength, &
                  &edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf,&
                  &INITIALIZE_QKE, &
                  &spp_pbl,rstoch_col )
             DO k=KTS,KTE
                el_pbl(i,k,j)=el(k)
                sh3d(i,k,j)=sh(k)
                qke(i,k,j)=qke1(k)
                tsq(i,k,j)=tsq1(k)
                qsq(i,k,j)=qsq1(k)
                cov(i,k,j)=cov1(k)
                IF (bl_mynn_tkeadvect) THEN
                   qke_adv(i,k,j)=qke1(k)
                ENDIF
             ENDDO
          ENDDO
       ENDDO
    ENDIF
    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF
    DO j=JTS,JTF
       DO i=ITS,ITF
          DO k=KTS,KTE
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)=qke(i,k,j)
             END IF
             dz1(k)= dz(i,k,j)
             u1(k) = u(i,k,j)
             v1(k) = v(i,k,j)
             w1(k) = w(i,k,j)
             th1(k)= th(i,k,j)
             tk1(k)=T3D(i,k,j)
             rho1(k)=rho(i,k,j)
             qv1(k)= qv(i,k,j)
             qc1(k)= qc(i,k,j)
             sqv(k)= qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)= qc(i,k,j)/(1.+qv(i,k,j))
             IF(icloud_bl > 0)cldfra_bl1D_old(k)=cldfra_bl(i,k,j)
             IF(icloud_bl > 0)qc_bl1D_old(k)=qc_bl(i,k,j)
             dqc1(k)=0.0
             dqi1(k)=0.0
             dqni1(k)=0.0
             dqnc1(k)=0.0
             dqnwfa1(k)=0.0
             dqnifa1(k)=0.0
             IF(PRESENT(qi) .AND. FLAG_QI)THEN
                qi1(k)= qi(i,k,j)
                sqi(k)= qi(i,k,j)/(1.+qv(i,k,j))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th(i,k,j) - xlvcp/exner(i,k,j)*sqc(k) &
                     & - xlscp/exner(i,k,j)*sqi(k)
                IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL(i,k,j)>0.001)THEN
                   sqc9=QC_BL(i,k,j)*(MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                   sqi9=QC_BL(i,k,j)*(1. - MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                ELSE
                   sqc9=sqc(k)
                   sqi9=sqi(k)
                ENDIF
                thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                      & - xlscp/exner(i,k,j)*sqi9
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                IF(sqc(k)<1e-6 .and. CLDFRA_BL(i,k,j)>0.001)THEN
                   sqc9=QC_BL(i,k,j)*(MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                   sqi9=QC_BL(i,k,j)*(1. - MIN(1., MAX(0., (tk1(k)-254.)/15.)))*CLDFRA_BL(i,k,j)
                ELSE
                   sqc9=sqc(k)
                   sqi9=0.0
                ENDIF
                thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                      & - xlscp/exner(i,k,j)*sqi9
            ENDIF
            thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
            thvl(k)=thlsg(k)*(1.+0.61*sqv(k))
             IF (PRESENT(qni) .AND. FLAG_QNI ) THEN
                qni1(k)=qni(i,k,j)
             ELSE
                qni1(k)=0.0
             ENDIF
             IF (PRESENT(qnc) .AND. FLAG_QNC ) THEN
                qnc1(k)=qnc(i,k,j)
             ELSE
                qnc1(k)=0.0
             ENDIF
             IF (PRESENT(qnwfa) .AND. FLAG_QNWFA ) THEN
                qnwfa1(k)=qnwfa(i,k,j)
             ELSE
                qnwfa1(k)=0.0
             ENDIF
             IF (PRESENT(qnifa) .AND. FLAG_QNIFA ) THEN
                qnifa1(k)=qnifa(i,k,j)
             ELSE
                qnifa1(k)=0.0
             ENDIF
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)
             if (spp_pbl==1) then
                rstoch_col(k)=pattern_spp_pbl(i,k,j)
             else
                rstoch_col(k)=0.0
             endif
             edmf_a1(k)=0.0
             edmf_w1(k)=0.0
             edmf_qc1(k)=0.0
             s_aw1(k)=0.
             s_awthl1(k)=0.
             s_awqt1(k)=0.
             s_awqv1(k)=0.
             s_awqc1(k)=0.
             s_awu1(k)=0.
             s_awv1(k)=0.
             s_awqke1(k)=0.
             s_awqnc1(k)=0.
             s_awqni1(k)=0.
             s_awqnwfa1(k)=0.
             s_awqnifa1(k)=0.
             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO
          zw(kte+1)=zw(kte)+dz(i,kte,j)
          s_aw1(kte+1)=0.
          s_awthl1(kte+1)=0.
          s_awqt1(kte+1)=0.
          s_awqv1(kte+1)=0.
          s_awqc1(kte+1)=0.
          s_awu1(kte+1)=0.
          s_awv1(kte+1)=0.
          s_awqke1(kte+1)=0.
          s_awqnc1(kte+1)=0.
          s_awqni1(kte+1)=0.
          s_awqnwfa1(kte+1)=0.
          s_awqnifa1(kte+1)=0.
          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thvl,&
          & Qke1,zw,dz1,xland(i,j),KPBL(i,j))
          IF (scaleaware > 0.) THEN
             CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
          ELSE
             Psig_bl(i,j)=1.0
             Psig_shcu(i,j)=1.0
          ENDIF
          sqcg= 0.0
          cpm=cp*(1.+0.84*qv(i,kts,j))
          exnerg=(ps(i,j)/p1000mb)**rcp
          flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
            & +xlvcp*vdfg(i,j)*(sqc(kts)/exner(i,kts,j)- sqcg/exnerg)
          flq = qfx(i,j)/ rho(i,kts,j) &
            & -vdfg(i,j)*(sqc(kts) - sqcg )
          flqv = qfx(i,j)/rho(i,kts,j)
          flqc = -vdfg(i,j)*(sqc(kts) - sqcg )
          th_sfc = ts(i,j)/ex1(kts)
          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 + cphh_st * zet
          else
            pmz = 1.0/ (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if
          govrth = g/th1(kts)
          sflux = hfx(i,j)/rho(i,kts,j)/cpm + &
                  qfx(i,j)/rho(i,kts,j)*ep_1*th1(kts)
          bfx0 = max(sflux,0.)
          wstar3 = (govrth*bfx0*pblh(i,j))
          wstar(i,j) = wstar3**h1
          wm3 = wstar3 + 5.*ust(i,j)**3.
          wm2 = wm3**h2
          delb = govrth*d3*pblh(i,j)
          delta(i,j) = min(d1*pblh(i,j) + d2*wm2/delb, 100.)
          CALL mym_condensation ( kts,kte, &
               &dx,dz1,zw,thl,sqw,p1,ex1, &
               &tsq1, qsq1, cov1, &
               &Sh,el,bl_mynn_cloudpdf, &
               &qc_bl1D,cldfra_bl1D, &
               &PBLH(i,j),HFX(i,j), &
               &Vt, Vq, th1, sgm, rmol(i,j), &
               &spp_pbl, rstoch_col )
          IF (bl_mynn_topdown.eq.1)then
             cloudflg=.false.
             minrad=100.
             kminrad=kpbl(i,j)
             zminrad=PBLH(i,j)
             KHtopdown(kts:kte)=0.0
             TKEprodTD(kts:kte)=0.0
             maxKHtopdown(i,j)=0.0
             DO kk = MAX(1,kpbl(i,j)-2),kpbl(i,j)+3
                if(sqc(kk).gt. 1.e-6 .OR. sqi(kk).gt. 1.e-6 .OR. &
                   cldfra_bl1D(kk).gt.0.5) then
                   cloudflg=.true.
                endif
                if(rthraten(i,kk,j) < minrad)then
                   minrad=rthraten(i,kk,j)
                   kminrad=kk
                   zminrad=zw(kk) + 0.5*dz1(kk)
                endif
             ENDDO
             IF (MAX(kminrad,kpbl(i,j)) < 2)cloudflg = .false.
             IF (cloudflg) THEN
                zl1 = dz1(kts)
                k = MAX(kpbl(i,j)-1, kminrad-1)
                templ=thl(k)*ex1(k)
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep_2/p1(k+1))
                temps=templ + (sqw(k)-rvls)/(cp/xlv + ep_2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep_2/p1(k+1))
                rcldb=max(sqw(k)-rvls,0.)
                dthvx = (thl(k+2) + th1(k+2)*ep_1*sqw(k+2)) &
                          - (thl(k) + th1(k) *ep_1*sqw(k))
                dthvx = max(dthvx,0.1)
                tmp1 = xlvcp * rcldb/(ex1(k)*dthvx)
                ent_eff = 0.2 + 0.2*8.*tmp1
                radsum=0.
                DO kk = MAX(1,kpbl(i,j)-3),kpbl(i,j)+3
                   radflux=rthraten(i,kk,j)*ex1(kk)
                   radflux=radflux*cp/g*(p1(kk)-p1(kk+1))
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                ENDDO
                radsum=MIN(radsum,60.0)
                bfx0 = max(radsum/rho1(k)/cp - max(sflux,0.0),0.)
                wm3 = g/thetav(k)*bfx0*MIN(pblh(i,j),1500.)
                wm2 = wm2 + wm3**h2
                bfxpbl = - ent_eff * bfx0
                dthvx = max(thetav(k+1)-thetav(k),0.1)
                we = max(bfxpbl/dthvx,-sqrt(wm3**h2))
                DO kk = kts,kpbl(i,j)+3
                   zfac(kk) = min(max((1.-(zw(kk+1)-zl1)/(zminrad-zl1)),zfmin),1.)
                   zfacent(kk) = 10.*MAX((zminrad-zw(kk+1))/zminrad,0.0)*(1.-zfac(kk))**3
                   wscalek2(kk) = (phifac*karman*wm3*(zfac(kk)))**h1
                   KHtopdown(kk) = wscalek2(kk)*karman*(zminrad-zw(kk+1))*(1.-zfac(kk))**3
                   KHtopdown(kk) = MAX(KHtopdown(kk),0.0)
                   TKEprodTD(kk)=2.*ent_eff*wm3/MAX(pblh(i,j),100.)*zfacent(kk)
                   TKEprodTD(kk)= MAX(TKEprodTD(kk),0.0)
                ENDDO
             ENDIF
             maxKHtopdown(i,j)=MAXVAL(KHtopdown(:))
          ELSE
             maxKHtopdown(i,j)=0.0
             KHtopdown(kts:kte) = 0.0
             TKEprodTD(kts:kte)=0.0
          ENDIF
          IF (bl_mynn_edmf > 0) THEN
            CALL DMP_mf( &
               &kts,kte,delt,zw,dz1,p1, &
               &bl_mynn_edmf_mom, &
               &bl_mynn_edmf_tke, &
               &bl_mynn_mixscalars, &
               &u1,v1,w1,th1,thl,thetav,tk1, &
               &sqw,sqv,sqc,qke1, &
               &qnc1,qni1,qnwfa1,qnifa1, &
               &ex1,Vt,Vq,sgm, &
               &ust(i,j),flt,flq,flqv,flqc, &
               &PBLH(i,j),KPBL(i,j),DX, &
               &xland(i,j),th_sfc, &
               & edmf_a1,edmf_w1,edmf_qt1, &
               & edmf_thl1,edmf_ent1,edmf_qc1, &
               & s_aw1,s_awthl1,s_awqt1, &
               & s_awqv1,s_awqc1, &
               & s_awu1,s_awv1,s_awqke1, &
               & s_awqnc1,s_awqni1, &
               & s_awqnwfa1,s_awqnifa1, &
               & qc_bl1D,cldfra_bl1D, &
               & FLAG_QC,FLAG_QI, &
               & FLAG_QNC,FLAG_QNI, &
               & FLAG_QNWFA,FLAG_QNIFA, &
               & Psig_shcu(i,j), &
               & nupdraft(i,j),ktop_plume(i,j), &
               & maxmf(i,j),ztop_plume, &
               & kbot_shallow(i,j),mf_at_base(i,j),&
               & spp_pbl,rstoch_col &
            )
          ENDIF
          CALL mym_turbulence ( &
               &kts,kte,levflag, &
               &dz1, zw, u1, v1, thl, sqc, sqw, &
               &qke1, tsq1, qsq1, cov1, &
               &vt, vq, &
               &rmol(i,j), flt, flq, &
               &PBLH(i,j),th1, &
               &Sh,el, &
               &Dfm,Dfh,Dfq, &
               &Tcd,Qcd,Pdk, &
               &Pdt,Pdq,Pdc, &
               &qWT1,qSHEAR1,qBUOY1,qDISS1, &
               &bl_mynn_tkebudget, &
               &Psig_bl(i,j),Psig_shcu(i,j), &
               &cldfra_bl1D,bl_mynn_mixlength, &
               &edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf, &
               &TKEprodTD, &
               &spp_pbl,rstoch_col)
          CALL mym_predict (kts,kte,levflag, &
               &delt, dz1, &
               &ust(i,j), flt, flq, pmz, phh, &
               &el, dfq, pdk, pdt, pdq, pdc, &
               &Qke1, Tsq1, Qsq1, Cov1, &
               &s_aw1, s_awqke1, bl_mynn_edmf_tke)
          DO k=kts,kte-1
             diss_heat(k) = MIN(MAX(0.5*(qke1(k)**1.5)/(b1*MAX(0.5*(el(k)+el(k+1)),1.))/cp, 0.0),0.00002)
          ENDDO
          diss_heat(kte) = 0.
          CALL mynn_tendencies(kts,kte, &
               &levflag,grav_settling, &
               &delt, dz1, rho1, &
               &u1, v1, th1, tk1, qv1, &
               &qc1, qi1, qnc1, qni1, &
               &p1, ex1, thl, sqv, sqc, sqi, sqw,&
               &qnwfa1, qnifa1, &
               &ust(i,j),flt,flq,flqv,flqc, &
               &wspd(i,j),qcg(i,j), &
               &uoce(i,j),voce(i,j), &
               &tsq1, qsq1, cov1, &
               &tcd, qcd, &
               &dfm, dfh, dfq, &
               &Du1, Dv1, Dth1, Dqv1, &
               &Dqc1, Dqi1, Dqnc1, Dqni1, &
               &Dqnwfa1, Dqnifa1, &
               &vdfg(i,j), diss_heat, &
               &s_aw1,s_awthl1,s_awqt1, &
               &s_awqv1,s_awqc1,s_awu1,s_awv1, &
               &s_awqnc1,s_awqni1, &
               &s_awqnwfa1,s_awqnifa1, &
               &FLAG_QC,FLAG_QI,FLAG_QNC, &
               &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA, &
               &cldfra_bl1d, &
               &bl_mynn_cloudmix, &
               &bl_mynn_mixqt, &
               &bl_mynn_edmf, &
               &bl_mynn_edmf_mom, &
               &bl_mynn_mixscalars )
          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dz1, K_m1, K_h1)
          DO k=KTS,KTE
             exch_m(i,k,j)=K_m1(k)
             exch_h(i,k,j)=K_h1(k)
             RUBLTEN(i,k,j)=du1(k)
             RVBLTEN(i,k,j)=dv1(k)
             RTHBLTEN(i,k,j)=dth1(k)
             RQVBLTEN(i,k,j)=dqv1(k)
             IF(bl_mynn_cloudmix > 0)THEN
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=dqc1(k)
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=dqi1(k)
             ELSE
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=0.
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=0.
             ENDIF
             IF(bl_mynn_cloudmix > 0 .AND. bl_mynn_mixscalars > 0)THEN
               IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=dqnc1(k)
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=dqni1(k)
               IF (PRESENT(qnwfa) .AND. FLAG_QNWFA) RQNWFABLTEN(i,k,j)=dqnwfa1(k)
               IF (PRESENT(qnifa) .AND. FLAG_QNIFA) RQNIFABLTEN(i,k,j)=dqnifa1(k)
             ELSE
               IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=0.
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=0.
               IF (PRESENT(qnwfa) .AND. FLAG_QNWFA) RQNWFABLTEN(i,k,j)=0.
               IF (PRESENT(qnifa) .AND. FLAG_QNIFA) RQNIFABLTEN(i,k,j)=0.
             ENDIF
             IF(icloud_bl > 0)THEN
               qc_bl(i,k,j)=qc_bl1D(k)
               cldfra_bl(i,k,j)=cldfra_bl1D(k)
               IF (CLDFRA_BL(i,k,j) < cldfra_bl1D_old(k)) THEN
                  ts_decay = MIN( 1800., 3.*dx/MAX(SQRT(u1(k)**2 + v1(k)**2),1.0) )
                  cldfra_bl(i,k,j)= MAX(cldfra_bl1D(k),cldfra_bl1D_old(k)-(0.25*delt/ts_decay))
                  IF (cldfra_bl(i,k,j) < 0.005) THEN
                    CLDFRA_BL(i,k,j)= 0.
                    QC_BL(i,k,j) = 0.
                  ENDIF
               ENDIF
               IF (QC_BL(i,k,j) < 1E-8 .AND. CLDFRA_BL(i,k,j) > 0.005) QC_BL(i,k,j)= 1E-8
             ENDIF
             el_pbl(i,k,j)=el(k)
             qke(i,k,j)=qke1(k)
             tsq(i,k,j)=tsq1(k)
             qsq(i,k,j)=qsq1(k)
             cov(i,k,j)=cov1(k)
             sh3d(i,k,j)=sh(k)
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j) = (qke1(k)-dqke(i,k,j))*0.5
                qWT(i,k,j) = qWT1(k)*delt
                qSHEAR(i,k,j)= qSHEAR1(k)*delt
                qBUOY(i,k,j) = qBUOY1(k)*delt
                qDISS(i,k,j) = qDISS1(k)*delt
             ENDIF
             IF (bl_mynn_edmf > 1) THEN
                edmf_a(i,k,j)=edmf_a1(k)
                edmf_w(i,k,j)=edmf_w1(k)
                edmf_qt(i,k,j)=edmf_qt1(k)
                edmf_thl(i,k,j)=edmf_thl1(k)
                edmf_ent(i,k,j)=edmf_ent1(k)
                edmf_qc(i,k,j)=edmf_qc1(k)
             ENDIF
             IF ( debug_code ) THEN
               IF ( sh(k) < 0. .OR. sh(k)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," sh=",sh(k)
               IF ( qke(i,k,j) < -1. .OR. qke(i,k,j)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," qke=",qke(i,k,j)
               IF ( el_pbl(i,k,j) < 0. .OR. el_pbl(i,k,j)> 2000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," el_pbl=",el_pbl(i,k,j)
               IF ( ABS(vt(k)) > 0.8 )print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vt=",vt(k)
               IF ( ABS(vq(k)) > 6000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vq=",vq(k)
               IF ( exch_m(i,k,j) < 0. .OR. exch_m(i,k,j)> 2000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," exxch_m=",exch_m(i,k,j)
               IF ( vdfg(i,j) < 0. .OR. vdfg(i,j)>5. )print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vdfg=",vdfg(i,j)
               IF ( ABS(QFX(i,j))>.001)print*,&
                  "SUSPICIOUS VALUES AT: i,j=",i,j," QFX=",QFX(i,j)
               IF ( ABS(HFX(i,j))>1000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j=",i,j," HFX=",HFX(i,j)
               IF (icloud_bl > 0) then
                  IF( cldfra_bl(i,k,j) < 0.0 .OR. cldfra_bl(i,k,j)> 1.)THEN
                  PRINT*,"SUSPICIOUS VALUES: CLDFRA_BL=",cldfra_bl(i,k,j)," qc_bl=",QC_BL(i,k,j)
                  ENDIF
               ENDIF
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    IF (bl_mynn_tkeadvect) THEN
       qke_adv=qke
    ENDIF
  END SUBROUTINE mynn_bl_driver
  SUBROUTINE mynn_bl_init_driver( &
       &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
       &RQCBLTEN,RQIBLTEN &
       &,QKE, &
       &EXCH_H &
       &,RESTART,ALLOWED_TO_READ,LEVEL &
       &,IDS,IDE,JDS,JDE,KDS,KDE &
       &,IMS,IME,JMS,JME,KMS,KME &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL
    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE, &
         & IMS,IME,JMS,JME,KMS,KME, &
         & ITS,ITE,JTS,JTE,KTS,KTE
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
         &RQCBLTEN,RQIBLTEN,&
         &QKE, &
         &EXCH_H
    INTEGER :: I,J,K,ITF,JTF,KTF
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                RUBLTEN(i,k,j)=0.
                RVBLTEN(i,k,j)=0.
                RTHBLTEN(i,k,j)=0.
                RQVBLTEN(i,k,j)=0.
                if( p_qc >= param_first_scalar ) RQCBLTEN(i,k,j)=0.
                if( p_qi >= param_first_scalar ) RQIBLTEN(i,k,j)=0.
                EXCH_H(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF
    mynn_level=level
  END SUBROUTINE mynn_bl_init_driver
  SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea,kzi)
    INTEGER,INTENT(IN) :: KTS,KTE
    REAL, INTENT(OUT) :: zi
    REAL, INTENT(IN) :: landsea
    REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
    REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
    REAL :: PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
    REAL :: delt_thv
    REAL, PARAMETER :: sbl_lim = 200.
    REAL, PARAMETER :: sbl_damp = 400.
    INTEGER :: I,J,K,kthv,ktke,kzi,kzi2
    kzi = 2
    kzi2= 2
    k = kts+1
    kthv = 1
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. 200.)
       IF (minthv > thetav1D(k)) then
           minthv = thetav1D(k)
           kthv = k
       ENDIF
       k = k+1
    ENDDO
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN
        delt_thv = 1.0
    ELSE
        delt_thv = 1.25
    ENDIF
    zi=0.
    k = kthv+1
    DO k=kts+1,kte-1
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
          zi = zw1D(k) - dz1D(k-1)* &
             & MIN((thetav1D(k)-(minthv + delt_thv))/ &
             & MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
          kzi= MAX(k-1,1) + NINT((zi-zw1D(k-1))/dz1D(k-1))
       ENDIF
       IF (k .EQ. kte-1) zi = zw1D(kts+1)
       IF (zi .NE. 0.0) exit
    ENDDO
    ktke = 1
    maxqke = MAX(Qke1D(kts),0.)
    TKEeps = maxqke/40.
    TKEeps = MAX(TKEeps,0.02)
    PBLH_TKE=0.
    k = ktke+1
    DO k=kts+1,kte-1
       qtke =MAX(Qke1D(k)/2.,0.)
       qtkem1=MAX(Qke1D(k-1)/2.,0.)
       IF (qtke .LE. TKEeps) THEN
           PBLH_TKE = zw1D(k) - dz1D(k-1)* &
             & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
           PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
           kzi2 = MAX(k-1,1) + NINT((PBLH_TKE-zw1D(k-1))/dz1D(k-1))
       ENDIF
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1)
       IF (PBLH_TKE .NE. 0.) exit
    ENDDO
    PBLH_TKE = MIN(PBLH_TKE,zi+350.)
    PBLH_TKE = MAX(PBLH_TKE,MAX(zi-350.,10.))
    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    IF (maxqke <= 0.05) THEN
    ELSE
       zi=PBLH_TKE*(1.-wt) + zi*wt
    ENDIF
     kzi = MAX(INT(kzi2*(1.-wt) + kzi*wt),1)
  END SUBROUTINE GET_PBLH
  SUBROUTINE DMP_mf( &
                 & kts,kte,dt,zw,dz,p, &
                 & momentum_opt, &
                 & tke_opt, &
                 & scalar_opt, &
                 & u,v,w,th,thl,thv,tk, &
                 & qt,qv,qc,qke, &
                 qnc,qni,qnwfa,qnifa, &
                 & exner,vt,vq,sgm, &
                 & ust,flt,flq,flqv,flqc, &
                 & pblh,kpbl,DX,landsea,ts, &
                 & edmf_a,edmf_w, &
                 & edmf_qt,edmf_thl, &
                 & edmf_ent,edmf_qc, &
                 & s_aw,s_awthl,s_awqt, &
                 & s_awqv,s_awqc, &
                 & s_awu,s_awv,s_awqke, &
                 & s_awqnc,s_awqni, &
                 & s_awqnwfa,s_awqnifa, &
                 & qc_bl1d,cldfra_bl1d, &
                 & F_QC,F_QI, &
                 F_QNC,F_QNI, &
                 & F_QNWFA,F_QNIFA, &
                 & Psig_shcu, &
                 &nup2,ktop,maxmf,ztop, &
                 &kbot,mf_at_base, &
                 &spp_pbl,rstoch_col)
     INTEGER, INTENT(IN) :: KTS,KTE,KPBL,momentum_opt,tke_opt,scalar_opt
     INTEGER, INTENT(IN) :: spp_pbl
     REAL, DIMENSION(KTS:KTE) :: rstoch_col
     REAL,DIMENSION(KTS:KTE), INTENT(IN) :: U,V,W,TH,THL,TK,QT,QV,QC,&
                      exner,dz,THV,P,qke,qnc,qni,qnwfa,qnifa
     REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: ZW
     REAL, INTENT(IN) :: DT,UST,FLT,FLQ,FLQV,FLQC,PBLH,&
                         DX,Psig_shcu,landsea,ts
     LOGICAL, OPTIONAL :: F_QC,F_QI,F_QNC,F_QNI,F_QNWFA,F_QNIFA
     REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a,edmf_w, &
                      & edmf_qt,edmf_thl, edmf_ent,edmf_qc
     REAL,DIMENSION(KTS:KTE) :: edmf_th
     INTEGER, INTENT(OUT) :: nup2,ktop,kbot
     REAL, INTENT(OUT) :: maxmf,ztop,mf_at_base
     REAL,DIMENSION(KTS:KTE+1) :: s_aw, &
                               s_awthl, &
                                s_awqt, &
                                s_awqv, &
                                s_awqc, &
                               s_awqnc, &
                               s_awqni, &
                             s_awqnwfa, &
                             s_awqnifa, &
                                 s_awu, &
                                 s_awv, &
                               s_awqke, s_aw2
     REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d
    INTEGER, PARAMETER :: NUP=10, debug_mf=0
     REAL,DIMENSION(KTS:KTE+1,1:NUP) :: UPW,UPTHL,UPQT,UPQC,UPQV, &
                                        UPA,UPU,UPV,UPTHV,UPQKE,UPQNC, &
                                        UPQNI,UPQNWFA,UPQNIFA
     REAL,DIMENSION(KTS:KTE,1:NUP) :: ENT,ENTf
     INTEGER,DIMENSION(KTS:KTE,1:NUP) :: ENTi
     INTEGER :: K,I,k50
     REAL :: fltv,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0, &
             pwmin,pwmax,wmin,wmax,wlv,wtv,Psig_w,maxw,maxqc,wpbl
     REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,QNCn,QNIn,QNWFAn,QNIFAn, &
             Wn2,Wn,EntEXP,EntW,BCOEFF,THVkm1,THVk,Pk
     REAL,PARAMETER :: &
          &Wa=2./3., &
          &Wb=0.002,&
          &Wc=1.5
     REAL,PARAMETER :: &
         & L0=100.,&
         & ENT0=0.1
     REAL, PARAMETER :: Atot = 0.10
     REAL, PARAMETER :: lmax = 1000.
     REAL, PARAMETER :: dl = 100.
     REAL, PARAMETER :: dcut = 1.2
     REAL :: d
     REAL :: cn,c,l,n,an2,hux,maxwidth,wspd_pbl,cloud_base,width_flx
   REAL :: ERF
   LOGICAL :: superadiabatic
   REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: vt, vq, sgm
   REAL :: sigq,xl,tlk,qsat_tl,rsl,cpm,a,qmq,mf_cf,Q1,diffqt,&
           Fng,qww,alpha,beta,bb,f,pt,t,q2p,b9,satvp,rhgrid
   REAL,DIMENSION(KTS:KTE) :: exneri,dzi
   REAL :: THp, QTp, QCp, esat, qsl
   REAL :: csigma,acfac,EntThrottle
   INTEGER :: overshoot
   REAL :: bvf, Frz
   REAL :: adjustment, flx1
   REAL, PARAMETER :: fluxportion=0.75
  UPW=0.
  UPTHL=0.
  UPTHV=0.
  UPQT=0.
  UPA=0.
  UPU=0.
  UPV=0.
  UPQC=0.
  UPQV=0.
  UPQKE=0.
  UPQNC=0.
  UPQNI=0.
  UPQNWFA=0.
  UPQNIFA=0.
  ENT=0.001
  edmf_a =0.
  edmf_w =0.
  edmf_qt =0.
  edmf_thl=0.
  edmf_ent=0.
  edmf_qc =0.
  s_aw=0.
  s_awthl=0.
  s_awqt=0.
  s_awqv=0.
  s_awqc=0.
  s_awu=0.
  s_awv=0.
  s_awqke=0.
  s_awqnc=0.
  s_awqni=0.
  s_awqnwfa=0.
  s_awqnifa=0.
  k = 1
  maxw = 0.0
  cloud_base = 9000.0
  DO k=1,kte-1
     IF(ZW(k) > pblh + 500.) exit
     wpbl = w(k)
     IF(w(k) < 0.)wpbl = 2.*w(k)
     maxw = MAX(maxw,ABS(wpbl))
     IF(ZW(k)<=50.)k50=k
     IF(qc(k)>1E-5 .AND. cloud_base == 9000.0)THEN
       cloud_base = 0.5*(ZW(k)+ZW(k+1))
     ENDIF
  ENDDO
  maxw = MAX(0.,maxw - 0.5)
  Psig_w = MAX(0.0, 1.0 - maxw/0.5)
  Psig_w = MIN(Psig_w, Psig_shcu)
  fltv = flt + svp1*flq
  IF(Psig_w == 0.0 .and. fltv > 0.0) fltv = -1.*fltv
      superadiabatic = .false.
  IF((landsea-1.5).GE.0)THEN
     hux = -0.002
  ELSE
     hux = -0.005
  ENDIF
  DO k=1,MAX(1,k50-1)
    IF (k == 1) then
      IF ((th(k)-ts)/(0.5*dz(k)) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ELSE
      IF ((th(k)-th(k-1))/(0.5*(dz(k)+dz(k-1))) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ENDIF
  ENDDO
    NUP2 = max(1,min(NUP,INT(dx*dcut/dl)))
    maxwidth = 1.2*PBLH
    maxwidth = MIN(maxwidth,cloud_base)
    IF((landsea-1.5).LT.0)THEN
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.050)/0.03) + .5),1000.), 0.)
      maxwidth = MIN(maxwidth,width_flx)
    ENDIF
    NUP2 = MIN(MAX(INT((maxwidth - MOD(maxwidth,100.))/100), 0), NUP2)
  ktop = 0
  kbot = 0
  ztop = 0.0
  maxmf= 0.0
  mf_at_base=0.0
  IF ( fltv > 0.002 .AND. NUP2 .GE. 1 .AND. superadiabatic) then
    cn = 0.
    d=-1.9
    do I=1,NUP
       IF(I > NUP2) exit
       l = dl*I
       cn = cn + l**d * (l*l)/(dx*dx) * dl
    enddo
    C = Atot/cn
    An2 = 0.
    do I=1,NUP
       IF(I > NUP2) exit
       l = dl*I
       N = C*l**d
       UPA(1,I) = N*l*l/(dx*dx) * dl
       acfac = .5*tanh((fltv - 0.02)/0.09) + .5
       UPA(1,I)=UPA(1,I)*acfac
       An2 = An2 + UPA(1,I)
    end do
    z0=50.
    pwmin=0.1
    pwmax=0.4
    wstar=max(1.E-2,(g/thv(1)*fltv*pblh)**(1./3.))
    qstar=max(flq,1.0E-5)/wstar
    thstar=flt/wstar
    IF((landsea-1.5).GE.0)THEN
       csigma = 1.34
    ELSE
       csigma = 1.34
    ENDIF
    sigmaW =1.34*wstar*(z0/pblh)**(1./3.)*(1 - 0.8*z0/pblh)
    sigmaQT=csigma*qstar*(z0/pblh)**(-1./3.)
    sigmaTH=csigma*thstar*(z0/pblh)**(-1./3.)
    wmin=MIN(sigmaW*pwmin,0.1)
    wmax=MIN(sigmaW*pwmax,0.3)
    acfac = .5*tanh((fltv - 0.03)/0.07) + .5
    DO I=1,NUP
       IF(I > NUP2) exit
       wlv=wmin+(wmax-wmin)/NUP2*(i-1)
       wtv=wmin+(wmax-wmin)/NUP2*i
       UPW(1,I)=wmin + REAL(i)/REAL(NUP)*(wmax-wmin)
       UPU(1,I)=(U(KTS)*DZ(KTS+1)+U(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPV(1,I)=(V(KTS)*DZ(KTS+1)+V(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQC(1,I)=0
       UPQT(1,I)=(QT(KTS)*DZ(KTS+1)+QT(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))&
           & +0.58*UPW(1,I)*sigmaQT/sigmaW
       UPTHV(1,I)=(THV(KTS)*DZ(KTS+1)+THV(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           & +0.58*UPW(1,I)*sigmaTH/sigmaW
       UPTHL(1,I)=(THL(KTS)*DZ(KTS+1)+THL(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           & +0.58*UPW(1,I)*sigmaTH/sigmaW
       UPQKE(1,I)=(QKE(KTS)*DZ(KTS+1)+QKE(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNC(1,I)=(QNC(KTS)*DZ(KTS+1)+QNC(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNI(1,I)=(QNI(KTS)*DZ(KTS+1)+QNI(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNWFA(1,I)=(QNWFA(KTS)*DZ(KTS+1)+QNWFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNIFA(1,I)=(QNIFA(KTS)*DZ(KTS+1)+QNIFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
    ENDDO
  EntThrottle = 0.001
    DO I=1,NUP
       IF(I > NUP2) exit
       QCn = 0.
       overshoot = 0
       l = dl*I
       DO k=KTS+1,KTE-1
          ENT(k,i) = 0.35/(MIN(MAX(UPW(K-1,I),0.75),1.9)*l)
          ENT(k,i) = max(ENT(k,i),0.0003)
          IF(ZW(k) >= MIN(pblh+1500., 3500.))THEN
            ENT(k,i)=ENT(k,i) + (ZW(k)-MIN(pblh+1500.,3500.))*5.0E-6
          ENDIF
          ENT(k,i) = ENT(k,i) * (1.0 - rstoch_col(k))
          ENT(k,i) = min(ENT(k,i),0.9/(ZW(k+1)-ZW(k)))
          EntExp= ENT(K,I)*(ZW(k+1)-ZW(k))
          QTn =UPQT(k-1,I) *(1.-EntExp) + QT(k)*EntExp
          THLn=UPTHL(k-1,I)*(1.-EntExp) + THL(k)*EntExp
          Un =UPU(k-1,I) *(1.-EntExp) + U(k)*EntExp
          Vn =UPV(k-1,I) *(1.-EntExp) + V(k)*EntExp
          QKEn=UPQKE(k-1,I)*(1.-EntExp) + QKE(k)*EntExp
          QNCn=UPQNC(k-1,I)*(1.-EntExp) + QNC(k)*EntExp
          QNIn=UPQNI(k-1,I)*(1.-EntExp) + QNI(k)*EntExp
          QNWFAn=UPQNWFA(k-1,I)*(1.-EntExp) + QNWFA(k)*EntExp
          QNIFAn=UPQNIFA(k-1,I)*(1.-EntExp) + QNIFA(k)*EntExp
          Pk =(P(k)*DZ(k+1)+P(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          call condensation_edmf(QTn,THLn,Pk,ZW(k+1),THVn,QCn)
          THVk =(THV(k)*DZ(k+1)+THV(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          THVkm1=(THV(k-1)*DZ(k)+THV(k)*DZ(k-1))/(DZ(k-1)+DZ(k))
          B=g*(THVn/THVk - 1.0)
          IF(B>0.)THEN
            BCOEFF = 0.15
          ELSE
            BCOEFF = 0.2
          ENDIF
          IF (UPW(K-1,I) < 0.2 ) THEN
             Wn = UPW(K-1,I) + (-2. * ENT(K,I) * UPW(K-1,I) + BCOEFF*B / MAX(UPW(K-1,I),0.2)) * MIN(ZW(k)-ZW(k-1), 250.)
          ELSE
             Wn = UPW(K-1,I) + (-2. * ENT(K,I) * UPW(K-1,I) + BCOEFF*B / UPW(K-1,I)) * MIN(ZW(k)-ZW(k-1), 250.)
          ENDIF
          IF(Wn > UPW(K-1,I) + MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0) ) THEN
             Wn = UPW(K-1,I) + MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0)
          ENDIF
          IF(Wn < UPW(K-1,I) - MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0) ) THEN
             Wn = UPW(K-1,I) - MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0)
          ENDIF
          Wn = MIN(MAX(Wn,0.0), 3.0)
          IF (debug_mf == 1) THEN
            IF (Wn .GE. 3.0) THEN
              print *," **** SUSPICIOUSLY LARGE W:"
              print *,' QCn:',QCn,' ENT=',ENT(k,i),' Nup2=',Nup2
              print *,'pblh:',pblh,' Wn:',Wn,' UPW(k-1)=',UPW(K-1,I)
              print *,'K=',k,' B=',B,' dz=',ZW(k)-ZW(k-1)
            ENDIF
          ENDIF
          IF (fltv > 0.05 .AND. Wn <= 0 .AND. overshoot == 0) THEN
             overshoot = 1
             IF ( THVk-THVkm1 .GT. 0.0 ) THEN
                bvf = SQRT( gtr*(THVk-THVkm1)/dz(k) )
                Frz = UPW(K-1,I)/(bvf*dz(k))
                IF ( Frz >= 0.5 ) Wn = MIN(Frz,1.0)*UPW(K-1,I)
             ENDIF
          ELSEIF (fltv > 0.05 .AND. overshoot == 1) THEN
             Wn = 0.0
          ENDIF
          Wn=Wn*EXP(-MAX(ZW(k+1)-MIN(pblh+2000.,3000.),0.0)/1000.)
          IF(ZW(k+1) >= MIN(pblh+3000.,4500.))Wn=0.
          IF (Wn > 0.) THEN
             UPW(K,I)=Wn
             UPTHV(K,I)=THVn
             UPTHL(K,I)=THLn
             UPQT(K,I)=QTn
             UPQC(K,I)=QCn
             UPU(K,I)=Un
             UPV(K,I)=Vn
             UPQKE(K,I)=QKEn
             UPQNC(K,I)=QNCn
             UPQNI(K,I)=QNIn
             UPQNWFA(K,I)=QNWFAn
             UPQNIFA(K,I)=QNIFAn
             UPA(K,I)=UPA(K-1,I)
             ktop = MAX(ktop,k)
          ELSE
             exit
          END IF
       ENDDO
       IF (debug_mf == 1) THEN
          IF (MAXVAL(UPW(:,I)) > 10.0 .OR. MINVAL(UPA(:,I)) < 0.0 .OR. &
              MAXVAL(UPA(:,I)) > Atot .OR. NUP2 > 10) THEN
             print *,'flq:',flq,' fltv:',fltv,' Nup2=',Nup2
             print *,'pblh:',pblh,' wstar:',wstar,' ktop=',ktop
             print *,'sigmaW=',sigmaW,' sigmaTH=',sigmaTH,' sigmaQT=',sigmaQT
             print *,'u:',u
             print *,'v:',v
             print *,'thl:',thl
             print *,'UPA:',UPA(:,I)
             print *,'UPW:',UPW(:,I)
             print *,'UPTHL:',UPTHL(:,I)
             print *,'UPQT:',UPQT(:,I)
             print *,'ENT:',ENT(:,I)
          ENDIF
       ENDIF
    ENDDO
  ELSE
    NUP2=0.
  END IF
  ktop=MIN(ktop,KTE-1)
  IF (ktop == 0) THEN
     ztop = 0.0
  ELSE
     ztop=zw(ktop)
  ENDIF
  IF(nup2 > 0) THEN
    DO k=KTS,KTE
      IF(k > KTOP) exit
      DO i=1,NUP
        IF(I > NUP2) exit
        s_aw(k+1) = s_aw(k+1) + UPA(K,i)*UPW(K,i)*Psig_w
        s_awthl(k+1)= s_awthl(k+1) + UPA(K,i)*UPW(K,i)*UPTHL(K,i)*Psig_w
        s_awqt(k+1) = s_awqt(k+1) + UPA(K,i)*UPW(K,i)*UPQT(K,i)*Psig_w
        s_awqc(k+1) = s_awqc(k+1) + UPA(K,i)*UPW(K,i)*UPQC(K,i)*Psig_w
        IF (momentum_opt > 0) THEN
          s_awu(k+1) = s_awu(k+1) + UPA(K,i)*UPW(K,i)*UPU(K,i)*Psig_w
          s_awv(k+1) = s_awv(k+1) + UPA(K,i)*UPW(K,i)*UPV(K,i)*Psig_w
        ENDIF
        IF (tke_opt > 0) THEN
          s_awqke(k+1)= s_awqke(k+1) + UPA(K,i)*UPW(K,i)*UPQKE(K,i)*Psig_w
        ENDIF
      ENDDO
      s_awqv(k+1) = s_awqt(k+1) - s_awqc(k+1)
    ENDDO
    IF (scalar_opt > 0) THEN
      DO k=KTS,KTE
        IF(k > KTOP) exit
        DO I=1,NUP
          IF (I > NUP2) exit
          s_awqnc(k+1)= s_awqnc(K+1) + UPA(K,i)*UPW(K,i)*UPQNC(K,i)*Psig_w
          s_awqni(k+1)= s_awqni(K+1) + UPA(K,i)*UPW(K,i)*UPQNI(K,i)*Psig_w
          s_awqnwfa(k+1)= s_awqnwfa(K+1) + UPA(K,i)*UPW(K,i)*UPQNWFA(K,i)*Psig_w
          s_awqnifa(k+1)= s_awqnifa(K+1) + UPA(K,i)*UPW(K,i)*UPQNIFA(K,i)*Psig_w
        ENDDO
      ENDDO
    ENDIF
    THVk = (THL(kts)*DZ(kts+1)+THL(kts+1)*DZ(kts))/(DZ(kts+1)+DZ(kts))
    flx1 = MAX(s_aw(kts+1)*(s_awthl(kts+1)/s_aw(kts+1) - THVk),0.0)
    adjustment=1.0
    IF (flx1 > fluxportion*flt .AND. flx1>0.0) THEN
       adjustment= fluxportion*flt/flx1
       s_aw = s_aw*adjustment
       s_awthl= s_awthl*adjustment
       s_awqt = s_awqt*adjustment
       s_awqc = s_awqc*adjustment
       s_awqv = s_awqv*adjustment
       s_awqnc= s_awqnc*adjustment
       s_awqni= s_awqni*adjustment
       s_awqnwfa= s_awqnwfa*adjustment
       s_awqnifa= s_awqnifa*adjustment
       IF (momentum_opt > 0) THEN
          s_awu = s_awu*adjustment
          s_awv = s_awv*adjustment
       ENDIF
       IF (tke_opt > 0) THEN
          s_awqke= s_awqke*adjustment
       ENDIF
       UPA = UPA*adjustment
    ENDIF
    DO k=KTS,KTE-1
      IF(k > KTOP) exit
      DO I=1,NUP
        IF(I > NUP2) exit
        edmf_a(K) =edmf_a(K) +UPA(K,i)
        edmf_w(K) =edmf_w(K) +UPA(K,i)*UPW(K,i)
        edmf_qt(K) =edmf_qt(K) +UPA(K,i)*UPQT(K,i)
        edmf_thl(K)=edmf_thl(K)+UPA(K,i)*UPTHL(K,i)
        edmf_ent(K)=edmf_ent(K)+UPA(K,i)*ENT(K,i)
        edmf_qc(K) =edmf_qc(K) +UPA(K,i)*UPQC(K,i)
      ENDDO
      IF (edmf_a(k)>0.) THEN
        edmf_w(k)=edmf_w(k)/edmf_a(k)
        edmf_qt(k)=edmf_qt(k)/edmf_a(k)
        edmf_thl(k)=edmf_thl(k)/edmf_a(k)
        edmf_ent(k)=edmf_ent(k)/edmf_a(k)
        edmf_qc(k)=edmf_qc(k)/edmf_a(k)
        edmf_a(k)=edmf_a(k)*Psig_w
        IF(edmf_a(k)*edmf_w(k) > maxmf) maxmf = edmf_a(k)*edmf_w(k)
        IF (k>1) THEN
          IF (edmf_qc(K)>0. .AND. edmf_qc(K-1)==0.) THEN
            kbot=k
            mf_at_base=edmf_a(kbot)*edmf_w(kbot)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    DO K=KTS,KTE-1
       exneri(k) = (exner(k)*DZ(k+1)+exner(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
       edmf_th(k)= edmf_thl(k) + xlvcp/exneri(k)*edmf_qc(K)
       dzi(k) = 0.5*(DZ(k)+DZ(k+1))
    ENDDO
    DO K=KTS+1,KTE-2
        IF(k > KTOP) exit
        IF(0.5*(edmf_qc(k)+edmf_qc(k-1))>0.0)THEN
            satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
            rhgrid = max(.01,MIN( 1., qv(k) /satvp))
            THp = (edmf_th(k)*dzi(k-1)+edmf_th(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            QTp = (edmf_qt(k)*dzi(k-1)+edmf_qt(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            t = THp*exner(k)
            esat = esat_blend(t)
            qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
            IF (edmf_qc(k)>0.0 .AND. edmf_qc(k-1)>0.0)THEN
              QCp = 0.5*(edmf_qc(k)+edmf_qc(k-1))
            ELSE
              QCp = MAX(0.0, QTp-qsl)
            ENDIF
            xl = xl_blend(tk(k))
            tlk = thl(k)*(p(k)/p1000mb)**rcp
            qsat_tl = qsat_blend(tlk,p(k))
            rsl = xl*qsat_tl / (r_v*tlk**2)
            cpm = cp + qt(k)*cpv
            a = 1./(1. + xl*rsl/cpm)
            b9 = a*rsl
            q2p = xlvcp/exner(k)
            pt = thl(k) +q2p*QCp*0.5*(edmf_a(k)+edmf_a(k-1))
            bb = b9*tk(k)/pt
            qww = 1.+0.61*qt(k)
            alpha = 0.61*pt
            t = TH(k)*exner(k)
            beta = pt*xl/(t*cp) - 1.61*pt
            if (a > 0.0) then
               f = MIN(1.0/a, 4.0)
            else
               f = 1.0
            endif
            sigq = 9.E-3 * 0.5*(edmf_a(k)+edmf_a(k-1)) * &
               & 0.5*(edmf_w(k)+edmf_w(k-1)) * f
            sigq = SQRT(sigq**2 + sgm(k)**2)
            qmq = a * (qt(k) - qsat_tl)
            mf_cf = min(max(0.5 + 0.36 * atan(1.55*(qmq/sigq)),0.01),0.6)
            IF ( debug_code ) THEN
               print*,"In MYNN, StEM edmf"
               print*,"  CB: env qt=",qt(k)," qsat=",qsat_tl
               print*,"      satdef=",QTp - qsat_tl
               print*,"  CB: sigq=",sigq," qmq=",qmq," tlk=",tlk
               print*,"  CB: mf_cf=",mf_cf," cldfra_bl=",cldfra_bl1d(k)," edmf_a=",edmf_a(k)
            ENDIF
            IF (cldfra_bl1d(k) < 0.5) THEN
               IF (mf_cf > 0.5*(edmf_a(k)+edmf_a(k-1))) THEN
                  cldfra_bl1d(k) = mf_cf
                  qc_bl1d(k) = QCp*0.5*(edmf_a(k)+edmf_a(k-1))/mf_cf
               ELSE
                  cldfra_bl1d(k)=0.5*(edmf_a(k)+edmf_a(k-1))
                  qc_bl1d(k) = QCp
               ENDIF
            ENDIF
            Q1 = qmq/MAX(sigq,1E-10)
            Q1=MAX(Q1,-5.0)
            IF (Q1 .GE. 1.0) THEN
               Fng = 1.0
            ELSEIF (Q1 .GE. -1.7 .AND. Q1 < 1.0) THEN
               Fng = EXP(-0.4*(Q1-1.0))
            ELSEIF (Q1 .GE. -2.5 .AND. Q1 .LE. -1.7) THEN
               Fng = 3.0 + EXP(-3.8*(Q1+1.7))
            ELSE
               Fng = MIN(23.9 + EXP(-1.6*(Q1+2.5)), 60.)
            ENDIF
            vt(k) = qww - MIN(0.4,cldfra_bl1D(k))*beta*bb*Fng - 1.
            vq(k) = alpha + MIN(0.4,cldfra_bl1D(k))*beta*a*Fng - tv0
         ENDIF
      ENDDO
    ENDIF
    IF (ktop > 0) THEN
      maxqc = maxval(edmf_qc(1:ktop))
      IF ( maxqc < 1.E-8) maxmf = -1.0*maxmf
    ENDIF
IF (edmf_w(1) > 4.0) THEN
    print *,'flq:',flq,' fltv:',fltv
    print *,'pblh:',pblh,' wstar:',wstar
    print *,'sigmaW=',sigmaW,' sigmaTH=',sigmaTH,' sigmaQT=',sigmaQT
   print *,' edmf_a',edmf_a(1:14)
   print *,' edmf_w',edmf_w(1:14)
   print *,' edmf_qt:',edmf_qt(1:14)
   print *,' edmf_thl:',edmf_thl(1:14)
ENDIF
END SUBROUTINE DMP_MF
subroutine condensation_edmf(QT,THL,P,zagl,THV,QC)
real,intent(in) :: QT,THL,P,zagl
real,intent(out) :: THV
real,intent(inout):: QC
integer :: niter,i
real :: diff,exn,t,th,qs,qcold
  niter=50
  diff=2.e-5
  EXN=(P/p1000mb)**rcp
  do i=1,NITER
     T=EXN*THL + xlv/cp*QC
     QS=qsat_blend(T,P)
     QCOLD=QC
     QC=0.5*QC + 0.5*MAX((QT-QS),0.)
     if (abs(QC-QCOLD)<Diff) exit
  enddo
  T=EXN*THL + xlv/cp*QC
  QS=qsat_blend(T,P)
  QC=max(QT-QS,0.)
  if(zagl < 100.)QC=0.
  THV=(THL+xlv/cp*QC)*(1.+QT*(rvovrd-1.)-rvovrd*QC)
end subroutine condensation_edmf
SUBROUTINE SCALE_AWARE(dx,PBL1,Psig_bl,Psig_shcu)
    REAL,INTENT(IN) :: dx,PBL1
    REAL, INTENT(OUT) :: Psig_bl,Psig_shcu
    REAL :: dxdh
    Psig_bl=1.0
    Psig_shcu=1.0
    dxdh=MAX(dx,10.)/MIN(PBL1,3000.)
     Psig_bl= ((dxdh**2) + 0.106*(dxdh**0.667))/((dxdh**2) +0.066*(dxdh**0.667) + 0.071)
    dxdh=MAX(dx,10.)/MIN(PBL1+500.,3500.)
    Psig_shcu= ((dxdh**2) + 0.145*(dxdh**0.667))/((dxdh**2) +0.172*(dxdh**0.667) + 0.170)
    If(Psig_bl > 1.0) Psig_bl=1.0
    If(Psig_bl < 0.0) Psig_bl=0.0
    If(Psig_shcu > 1.0) Psig_shcu=1.0
    If(Psig_shcu < 0.0) Psig_shcu=0.0
  END SUBROUTINE SCALE_AWARE
  FUNCTION esat_blend(t)
      IMPLICIT NONE
      REAL, INTENT(IN):: t
      REAL :: esat_blend,XC,ESL,ESI,chi
      XC=MAX(-80.,t-273.16)
      IF (t .GE. 273.16) THEN
          esat_blend = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
      ELSE IF (t .LE. 253.) THEN
          esat_blend = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
      ELSE
          ESL = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          chi = (273.16-t)/20.16
          esat_blend = (1.-chi)*ESL + chi*ESI
      END IF
  END FUNCTION esat_blend
  FUNCTION qsat_blend(t, P, waterice)
      IMPLICIT NONE
      REAL, INTENT(IN):: t, P
      CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: waterice
      CHARACTER(LEN=1) :: wrt
      REAL :: qsat_blend,XC,ESL,ESI,RSLF,RSIF,chi
      IF ( .NOT. PRESENT(waterice) ) THEN
          wrt = 'b'
      ELSE
          wrt = waterice
      ENDIF
      XC=MAX(-80.,t-273.16)
      IF ((t .GE. 273.16) .OR. (wrt .EQ. 'w')) THEN
          ESL = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          qsat_blend = 0.622*ESL/(P-ESL)
      ELSE IF (t .LE. 253.) THEN
          ESI = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          qsat_blend = 0.622*ESI/(P-ESI)
      ELSE
          ESL = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          RSLF = 0.622*ESL/(P-ESL)
          RSIF = 0.622*ESI/(P-ESI)
          chi = (273.16-t)/20.16
          qsat_blend = (1.-chi)*RSLF + chi*RSIF
      END IF
  END FUNCTION qsat_blend
  FUNCTION xl_blend(t)
      IMPLICIT NONE
      REAL, INTENT(IN):: t
      REAL :: xl_blend,xlvt,xlst,chi
      IF (t .GE. 273.16) THEN
          xl_blend = xlv + (cpv-cliq)*(t-273.16)
      ELSE IF (t .LE. 253.) THEN
          xl_blend = xls + (cpv-cice)*(t-273.16)
      ELSE
          xlvt = xlv + (cpv-cliq)*(t-273.16)
          xlst = xls + (cpv-cice)*(t-273.16)
          chi = (273.16-t)/20.16
          xl_blend = (1.-chi)*xlvt + chi*xlst
      END IF
  END FUNCTION xl_blend
END MODULE module_bl_mynn
