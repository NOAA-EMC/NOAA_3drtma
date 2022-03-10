MODULE module_sf_sfclay
 REAL , PARAMETER :: VCONVC=1.
 REAL , PARAMETER :: CZO=0.0185
 REAL , PARAMETER :: OZO=1.59E-5
 REAL, DIMENSION(0:1000 ),SAVE :: PSIMTB,PSIHTB
CONTAINS
   SUBROUTINE SFCLAY(U3D,V3D,T3D,QV3D,P3D,dz8w, &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM, &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     FM,FH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2, &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX, &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2, &
                     KARMAN,EOMEG,STBOLT, &
                     P1000mb, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte, &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,scm_force_flux )
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN ) :: ISFFLX
      REAL, INTENT(IN ) :: SVP1,SVP2,SVP3,SVPT0
      REAL, INTENT(IN ) :: EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL, INTENT(IN ) :: P1000mb
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: dz8w
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: QV3D, &
                                                              P3D, &
                                                              T3D
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(IN ) :: MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(OUT ) :: U10, &
                                                              V10, &
                                                              TH2, &
                                                               T2, &
                                                               Q2, &
                                                             QSFC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                          MOL,RMOL
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: GZ1OZ0,WSPD,BR, &
                                                  PSIM,PSIH,FM,FH
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: U3D, &
                                                              V3D
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(IN ) :: PSFC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: FLHC,FLQC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: &
                                                              QGH
      REAL, INTENT(IN ) :: CP,G,ROVCP,R,XLV,DX
      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(OUT) :: ck,cka,cd,cda
      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: USTM
      INTEGER, OPTIONAL, INTENT(IN ) :: ISFTCFLX, IZ0TLND
      INTEGER, OPTIONAL, INTENT(IN ) :: SCM_FORCE_FLUX
      REAL, DIMENSION( its:ite ) :: U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D
      REAL, DIMENSION( its:ite ) :: dz8w1d
      INTEGER :: I,J
      DO J=jts,jte
        DO i=its,ite
          dz8w1d(I) = dz8w(i,1,j)
        ENDDO
        DO i=its,ite
           U1D(i) =U3D(i,1,j)
           V1D(i) =V3D(i,1,j)
           QV1D(i)=QV3D(i,1,j)
           P1D(i) =P3D(i,1,j)
           T1D(i) =T3D(i,1,j)
        ENDDO
        CALL SFCLAY1D(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d, &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j), &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j), &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j), &
                FM(ims,j),FH(ims,j), &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j), &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j), &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j), &
                QSFC(ims,j),LH(ims,j), &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX, &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT, &
                P1000mb, &
                ids,ide, jds,jde, kds,kde, &
                ims,ime, jms,jme, kms,kme, &
                its,ite, jts,jte, kts,kte &
                ,isftcflx,iz0tlnd,scm_force_flux, &
                USTM(ims,j),CK(ims,j),CKA(ims,j), &
                CD(ims,j),CDA(ims,j) &
                                                                   )
      ENDDO
   END SUBROUTINE SFCLAY
   SUBROUTINE SFCLAY1D(J,UX,VX,T1D,QV1D,P1D,dz8w1d, &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,FM,FH,&
                     XLAND,HFX,QFX,TSK, &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH, &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX, &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2, &
                     KARMAN,EOMEG,STBOLT, &
                     P1000mb, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte, &
                     isftcflx, iz0tlnd, scm_force_flux, &
                     ustm,ck,cka,cd,cda )
      IMPLICIT NONE
      REAL, PARAMETER :: XKA=2.4E-5
      REAL, PARAMETER :: PRT=1.
      INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J
      INTEGER, INTENT(IN ) :: ISFFLX
      REAL, INTENT(IN ) :: SVP1,SVP2,SVP3,SVPT0
      REAL, INTENT(IN ) :: EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL, INTENT(IN ) :: P1000mb
      REAL, DIMENSION( ims:ime ) , &
                INTENT(IN ) :: MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL, DIMENSION( ims:ime ) , &
                INTENT(IN ) :: PSFCPA
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: GZ1OZ0,WSPD,BR, &
                                                  PSIM,PSIH,FM,FH
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: FLHC,FLQC
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: &
                                                              QGH
      REAL, DIMENSION( ims:ime ) , &
                INTENT(OUT) :: U10,V10, &
                                                TH2,T2,Q2,QSFC,LH
      REAL, INTENT(IN ) :: CP,G,ROVCP,R,XLV,DX
      REAL, DIMENSION( its:ite ), INTENT(IN ) :: dz8w1d
      REAL, DIMENSION( its:ite ), INTENT(IN ) :: UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D
      REAL, OPTIONAL, DIMENSION( ims:ime ) , &
                INTENT(OUT) :: ck,cka,cd,cda
      REAL, OPTIONAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: USTM
      INTEGER, OPTIONAL, INTENT(IN ) :: ISFTCFLX, IZ0TLND
      INTEGER, OPTIONAL, INTENT(IN ) :: SCM_FORCE_FLUX
      REAL, DIMENSION( its:ite ) :: ZA, &
                                                        THVX,ZQKL, &
                                                           ZQKLP1, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                           PSIH10, &
                                                           PSIM10, &
                                                           DENOMQ, &
                                                          DENOMQ2, &
                                                          DENOMT2, &
                                                            WSPDI, &
                                                           GZ2OZ0, &
                                                           GZ10OZ0
      REAL, DIMENSION( its:ite ) :: &
                                                      RHOX,GOVRTH, &
                                                            TGDSA
      REAL, DIMENSION( its:ite) :: SCR3,SCR4
      REAL, DIMENSION( its:ite ) :: THGB, PSFC
      INTEGER :: KL
      INTEGER :: N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10
      REAL :: PL,THCON,TVCON,E1
      REAL :: ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL :: DTG,PSIX,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2,PSIQ10
      REAL :: FLUXC,VSGD,Z0Q,VISC,RESTAR,CZIL,GZ0OZQ,GZ0OZT
      REAL :: ZW, ZN1, ZN2
      REAL :: Z0T, CZC
      KL=kte
      DO i=its,ite
         PSFC(I)=PSFCPA(I)/1000.
      ENDDO
      DO 5 I=its,ite
        TGDSA(I)=TSK(I)
        THGB(I)=TSK(I)*(P1000mb/PSFCPA(I))**ROVCP
    5 CONTINUE
   10 CONTINUE
   26 CONTINUE
      DO 30 I=its,ite
         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)
         THCON=(P1000mb*0.001/PL)**ROVCP
         THX(I)=SCR3(I)*THCON
         SCR4(I)=SCR3(I)
         THVX(I)=THX(I)
         QX(I)=0.
   30 CONTINUE
      DO I=its,ite
         QGH(I)=0.
         FLHC(I)=0.
         FLQC(I)=0.
         CPM(I)=CP
      ENDDO
      DO 50 I=its,ite
         QX(I)=QV1D(I)
         TVCON=(1.+EP1*QX(I))
         THVX(I)=THX(I)*TVCON
         SCR4(I)=SCR3(I)*TVCON
   50 CONTINUE
      DO 60 I=its,ite
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))
        if(xland(i).gt.1.5.or.qsfc(i).le.0.0)QSFC(I)=EP2*E1/(PSFC(I)-E1)
        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
        PL=P1D(I)/1000.
        QGH(I)=EP2*E1/(PL-E1)
        CPM(I)=CP*(1.+0.8*QX(I))
   60 CONTINUE
   80 CONTINUE
      DO 90 I=its,ite
        ZQKLP1(I)=0.
        RHOX(I)=PSFC(I)*1000./(R*SCR4(I))
   90 CONTINUE
      DO 110 I=its,ite
           ZQKL(I)=dz8w1d(I)+ZQKLP1(I)
  110 CONTINUE
      DO 120 I=its,ite
         ZA(I)=0.5*(ZQKL(I)+ZQKLP1(I))
  120 CONTINUE
      DO 160 I=its,ite
        GOVRTH(I)=G/THX(I)
  160 CONTINUE
      DO 260 I=its,ite
        GZ1OZ0(I)=ALOG(ZA(I)/ZNT(I))
        GZ2OZ0(I)=ALOG(2./ZNT(I))
        GZ10OZ0(I)=ALOG(10./ZNT(I))
        IF((XLAND(I)-1.5).GE.0)THEN
          ZL=ZNT(I)
        ELSE
          ZL=0.01
        ENDIF
        WSPD(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))
        TSKV=THGB(I)*(1.+EP1*QSFC(I))
        DTHVDZ=(THVX(I)-TSKV)
        if (xland(i).lt.1.5) then
        fluxc = max(hfx(i)/rhox(i)/cp &
              + ep1*tskv*qfx(i)/rhox(i),0.)
        VCONV = vconvc*(g/tgdsa(i)*pblh(i)*fluxc)**.33
        else
        IF(-DTHVDZ.GE.0)THEN
          DTHVM=-DTHVDZ
        ELSE
          DTHVM=0.
        ENDIF
        VCONV = SQRT(DTHVM)
        endif
        VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV+vsgd*vsgd)
        WSPD(I)=AMAX1(WSPD(I),0.1)
        BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))
        IF(MOL(I).LT.0.)BR(I)=AMIN1(BR(I),0.0)
        RMOL(I)=-GOVRTH(I)*DTHVDZ*ZA(I)*KARMAN
  260 CONTINUE
      DO 320 I=its,ite
        IF(BR(I).LT.0.)GOTO 310
        IF(BR(I).LT.0.2)GOTO 270
        REGIME(I)=1.
        PSIM(I)=-10.*GZ1OZ0(I)
        PSIM(I)=AMAX1(PSIM(I),-10.)
        PSIH(I)=PSIM(I)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)
        PSIH2(I)=PSIM2(I)
        IF(UST(I).LT.0.01)THEN
           RMOL(I)=BR(I)*GZ1OZ0(I)
        ELSE
           RMOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF
        RMOL(I)=AMIN1(RMOL(I),9.999)
        RMOL(I) = RMOL(I)/ZA(I)
        GOTO 320
  270 IF(BR(I).EQ.0.0)GOTO 280
        REGIME(I)=2.
        PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))
        PSIM(I)=AMAX1(PSIM(I),-10.)
        PSIH(I)=PSIM(I)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)
        PSIH2(I)=PSIM2(I)
        ZOL(I) = BR(I)*GZ1OZ0(I)/(1.00001-5.0*BR(I))
        if ( ZOL(I) .GT. 0.5 ) then
           ZOL(I) = ( 1.89*GZ1OZ0(I) + 44.2 ) * BR(I)*BR(I) &
                + ( 1.18*GZ1OZ0(I) - 1.37 ) * BR(I)
           ZOL(I)=AMIN1(ZOL(I),9.999)
        end if
        RMOL(I)= ZOL(I)/ZA(I)
        GOTO 320
  280 REGIME(I)=3.
        PSIM(I)=0.0
        PSIH(I)=PSIM(I)
        PSIM10(I)=0.
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=0.
        PSIH2(I)=PSIM2(I)
        IF(UST(I).LT.0.01)THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
        ELSE
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF
        RMOL(I) = ZOL(I)/ZA(I)
        GOTO 320
  310 CONTINUE
        REGIME(I)=4.
        IF(UST(I).LT.0.01)THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
        ELSE
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF
        ZOL10=10./ZA(I)*ZOL(I)
        ZOL2=2./ZA(I)*ZOL(I)
        ZOL(I)=AMIN1(ZOL(I),0.)
        ZOL(I)=AMAX1(ZOL(I),-9.9999)
        ZOL10=AMIN1(ZOL10,0.)
        ZOL10=AMAX1(ZOL10,-9.9999)
        ZOL2=AMIN1(ZOL2,0.)
        ZOL2=AMAX1(ZOL2,-9.9999)
        NZOL=INT(-ZOL(I)*100.)
        RZOL=-ZOL(I)*100.-NZOL
        NZOL10=INT(-ZOL10*100.)
        RZOL10=-ZOL10*100.-NZOL10
        NZOL2=INT(-ZOL2*100.)
        RZOL2=-ZOL2*100.-NZOL2
        PSIM(I)=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))
        PSIH(I)=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))
        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))
        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))
        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))
        PSIH2(I)=AMIN1(PSIH2(I),0.9*GZ2OZ0(I))
        PSIM10(I)=AMIN1(PSIM10(I),0.9*GZ10OZ0(I))
        PSIH10(I)=AMIN1(PSIH10(I),0.9*GZ10OZ0(I))
        RMOL(I) = ZOL(I)/ZA(I)
  320 CONTINUE
      DO 330 I=its,ite
        DTG=THX(I)-THGB(I)
        PSIX=GZ1OZ0(I)-PSIM(I)
        PSIX10=GZ10OZ0(I)-PSIM10(I)
        PSIT=AMAX1(GZ1OZ0(I)-PSIH(I),2.)
        IF((XLAND(I)-1.5).GE.0)THEN
          ZL=ZNT(I)
        ELSE
          ZL=0.01
        ENDIF
        PSIQ=ALOG(KARMAN*UST(I)*ZA(I)/XKA+ZA(I)/ZL)-PSIH(I)
        PSIT2=GZ2OZ0(I)-PSIH2(I)
        PSIQ2=ALOG(KARMAN*UST(I)*2./XKA+2./ZL)-PSIH2(I)
        PSIQ10=ALOG(KARMAN*UST(I)*10./XKA+10./ZL)-PSIH10(I)
        IF ( (XLAND(I)-1.5).GE.0. ) THEN
              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5
              RESTAR=UST(I)*ZNT(I)/VISC
              Z0T = (5.5e-5)*(RESTAR**(-0.60))
              Z0T = MIN(Z0T,1.0e-4)
              Z0T = MAX(Z0T,2.0e-9)
              Z0Q = Z0T
              PSIQ=max(ALOG((ZA(I)+Z0Q)/Z0Q)-PSIH(I), 2.)
              PSIT=max(ALOG((ZA(I)+Z0T)/Z0T)-PSIH(I), 2.)
              PSIQ2=max(ALOG((2.+Z0Q)/Z0Q)-PSIH2(I), 2.)
              PSIT2=max(ALOG((2.+Z0T)/Z0T)-PSIH2(I), 2.)
              PSIQ10=max(ALOG((10.+Z0Q)/Z0Q)-PSIH10(I), 2.)
        ENDIF
        IF ( PRESENT(ISFTCFLX) ) THEN
           IF ( ISFTCFLX.EQ.1 .AND. (XLAND(I)-1.5).GE.0. ) THEN
              Z0Q = 1.e-4
              PSIQ=ALOG(ZA(I)/Z0Q)-PSIH(I)
              PSIT=PSIQ
              PSIQ2=ALOG(2./Z0Q)-PSIH2(I)
              PSIQ10=ALOG(10./Z0Q)-PSIH10(I)
              PSIT2=PSIQ2
           ENDIF
           IF ( ISFTCFLX.EQ.2 .AND. (XLAND(I)-1.5).GE.0. ) THEN
              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5
              RESTAR=UST(I)*ZNT(I)/VISC
              GZ0OZT=0.40*(7.3*SQRT(SQRT(RESTAR))*SQRT(0.71)-5.)
              GZ0OZQ=0.40*(7.3*SQRT(SQRT(RESTAR))*SQRT(0.60)-5.)
              PSIT=GZ1OZ0(I)-PSIH(I)+GZ0OZT
              PSIQ=GZ1OZ0(I)-PSIH(I)+GZ0OZQ
              PSIT2=GZ2OZ0(I)-PSIH2(I)+GZ0OZT
              PSIQ2=GZ2OZ0(I)-PSIH2(I)+GZ0OZQ
              PSIQ10=GZ10OZ0(I)-PSIH(I)+GZ0OZQ
           ENDIF
        ENDIF
        IF(PRESENT(ck) .and. PRESENT(cd) .and. PRESENT(cka) .and. PRESENT(cda)) THEN
           Ck(I)=(karman/psix10)*(karman/psiq10)
           Cd(I)=(karman/psix10)*(karman/psix10)
           Cka(I)=(karman/psix)*(karman/psiq)
           Cda(I)=(karman/psix)*(karman/psix)
        ENDIF
        IF ( PRESENT(IZ0TLND) ) THEN
           IF ( IZ0TLND.EQ.1 .AND. (XLAND(I)-1.5).LE.0. ) THEN
              ZL=ZNT(I)
              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5
              RESTAR=UST(I)*ZL/VISC
              CZIL = 10.0 ** ( -0.40 * ( ZL / 0.07 ) )
              PSIT=GZ1OZ0(I)-PSIH(I)+CZIL*KARMAN*SQRT(RESTAR)
              PSIQ=GZ1OZ0(I)-PSIH(I)+CZIL*KARMAN*SQRT(RESTAR)
              PSIT2=GZ2OZ0(I)-PSIH2(I)+CZIL*KARMAN*SQRT(RESTAR)
              PSIQ2=GZ2OZ0(I)-PSIH2(I)+CZIL*KARMAN*SQRT(RESTAR)
           ENDIF
        ENDIF
        UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX
        WSPDI(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))
        IF ( PRESENT(USTM) ) THEN
        USTM(I)=0.5*USTM(I)+0.5*KARMAN*WSPDI(I)/PSIX
        ENDIF
        U10(I)=UX(I)*PSIX10/PSIX
        V10(I)=VX(I)*PSIX10/PSIX
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT
        Q2(I)=QSFC(I)+(QX(I)-QSFC(I))*PSIQ2/PSIQ
        T2(I) = TH2(I)*(PSFCPA(I)/P1000mb)**ROVCP
        IF((XLAND(I)-1.5).LT.0.)THEN
          UST(I)=AMAX1(UST(I),0.1)
        ENDIF
        MOL(I)=KARMAN*DTG/PSIT/PRT
        DENOMQ(I)=PSIQ
        DENOMQ2(I)=PSIQ2
        DENOMT2(I)=PSIT2
        FM(I)=PSIX
        FH(I)=PSIT
  330 CONTINUE
  335 CONTINUE
      IF ( PRESENT(SCM_FORCE_FLUX) ) THEN
         IF (SCM_FORCE_FLUX.EQ.1) GOTO 350
      ENDIF
      DO i=its,ite
        QFX(i)=0.
        HFX(i)=0.
      ENDDO
  350 CONTINUE
      IF (ISFFLX.EQ.0) GOTO 410
      DO 360 I=its,ite
        IF((XLAND(I)-1.5).GE.0)THEN
          ZNT(I)=CZO*UST(I)*UST(I)/G+0.11*1.5E-5/UST(I)
          ZNT(I)=MIN(ZNT(I),2.85e-3)
          IF ( PRESENT(ISFTCFLX) ) THEN
             IF ( ISFTCFLX.NE.0 ) THEN
                ZW = MIN((UST(I)/1.06)**(0.3),1.0)
                ZN1 = 0.011*UST(I)*UST(I)/G + OZO
                ZN2 = 10.*exp(-9.5*UST(I)**(-.3333)) + &
                       0.11*1.5E-5/AMAX1(UST(I),0.01)
                ZNT(I)=(1.0-ZW) * ZN1 + ZW * ZN2
                ZNT(I)=MIN(ZNT(I),2.85e-3)
                ZNT(I)=MAX(ZNT(I),1.27e-7)
             ENDIF
          ENDIF
          ZL = ZNT(I)
        ELSE
          ZL = 0.01
        ENDIF
        FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/DENOMQ(I)
        DTTHX=ABS(THX(I)-THGB(I))
        IF(DTTHX.GT.1.E-5)THEN
          FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))
 1001 format(f8.5,2x,f12.7,2x,f12.10,2x,f12.10,2x,f13.10,2x,f12.8,f12.8,2x,i3)
        ELSE
          FLHC(I)=0.
        ENDIF
  360 CONTINUE
     IF ( PRESENT(SCM_FORCE_FLUX) ) THEN
        IF (SCM_FORCE_FLUX.EQ.1) GOTO 405
     ENDIF
      DO 370 I=its,ite
        QFX(I)=FLQC(I)*(QSFC(I)-QX(I))
        QFX(I)=AMAX1(QFX(I),0.)
        LH(I)=XLV*QFX(I)
  370 CONTINUE
  390 CONTINUE
      DO 400 I=its,ite
        IF(XLAND(I)-1.5.GT.0.)THEN
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))
        ELSEIF(XLAND(I)-1.5.LT.0.)THEN
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))
          HFX(I)=AMAX1(HFX(I),-250.)
        ENDIF
  400 CONTINUE
  405 CONTINUE
      DO I=its,ite
         IF((XLAND(I)-1.5).GE.0)THEN
           ZL=ZNT(I)
         ELSE
           ZL=0.01
         ENDIF
         CHS(I)=UST(I)*KARMAN/DENOMQ(I)
         CQS2(I)=UST(I)*KARMAN/DENOMQ2(I)
         CHS2(I)=UST(I)*KARMAN/DENOMT2(I)
      ENDDO
  410 CONTINUE
   END SUBROUTINE SFCLAY1D
   SUBROUTINE sfclayinit( allowed_to_read )
   LOGICAL , INTENT(IN) :: allowed_to_read
   INTEGER :: N
   REAL :: ZOLN,X,Y
   DO N=0,1000
      ZOLN=-FLOAT(N)*0.01
      X=(1-16.*ZOLN)**0.25
      PSIMTB(N)=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))- &
                2.*ATAN(X)+2.*ATAN(1.)
      Y=(1-16*ZOLN)**0.5
      PSIHTB(N)=2*ALOG(0.5*(1+Y))
   ENDDO
   END SUBROUTINE sfclayinit
END MODULE module_sf_sfclay
