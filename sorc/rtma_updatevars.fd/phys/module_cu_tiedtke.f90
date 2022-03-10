module module_cu_tiedtke
     use module_model_constants, only:rd=>r_d, rv=>r_v, &
   & cpd=>cp, alv=>xlv, als=>xls, alf=>xlf, g
     implicit none
     real :: rcpd,vtmpc1,t000,hgfr,rhoh2o,tmelt, &
             c1es,c2es,c3les,c3ies,c4les,c4ies,c5les,c5ies,zrg
     real :: entrpen,entrscv,entrmid,entrdd,cmfctop,rhm,rhc, &
             cmfcmax,cmfcmin,cmfdeps,cprcon,crirh,zbuo0, &
             fdbk,ztau
     real :: cevapcu1, cevapcu2, zdnoprc
     parameter( &
      rcpd=1.0/cpd, &
      rhoh2o=1.0e03, &
      tmelt=273.16, &
      t000= 273.15, &
      hgfr= 233.15, &
      zrg=1.0/g, &
      c1es=610.78, &
      c2es=c1es*rd/rv, &
      c3les=17.269, &
      c4les=35.86, &
      c5les=c3les*(tmelt-c4les), &
      c3ies=21.875, &
      c4ies=7.66, &
      c5ies=c3ies*(tmelt-c4ies), &
      vtmpc1=rv/rd-1.0, &
      cevapcu1=1.93e-6*261.0*0.5/g, &
      cevapcu2=1.e3/(38.3*0.293) )
      parameter(entrpen=1.0e-4)
      parameter(entrscv=1.2e-3)
      parameter(entrmid=1.0e-4)
      parameter(entrdd =2.0e-4)
      parameter(cmfctop=0.30)
      parameter(cmfcmax=1.0)
      parameter(cmfcmin=1.e-10)
      parameter(cmfdeps=0.30)
      parameter(cprcon = 1.1e-3/g)
      parameter(zdnoprc = 1.5e4)
      parameter(rhc=0.80,rhm=1.0,zbuo0=0.50)
      parameter(crirh=0.70,fdbk = 1.0,ztau = 2400.0)
      logical :: lmfpen,lmfmid,lmfscv,lmfdd,lmfdudv
      parameter(lmfpen=.true.,lmfmid=.true.,lmfscv=.true.,lmfdd=.true.,lmfdudv=.true.)
contains
      subroutine cu_tiedtke( &
                 dt,itimestep,stepcu &
                ,raincv,pratec,qfx,znu &
                ,u3d,v3d,w,t3d,qv3d,qc3d,qi3d,pi3d,rho3d &
                ,qvften,qvpblten &
                ,dz8w,pcps,p8w,xland,cu_act_flag &
                ,ids,ide, jds,jde, kds,kde &
                ,ims,ime, jms,jme, kms,kme &
                ,its,ite, jts,jte, kts,kte &
                ,rthcuten,rqvcuten,rqccuten,rqicuten &
                ,rucuten, rvcuten &
                ,f_qv ,f_qc ,f_qr ,f_qi ,f_qs &
                                                                )
      implicit none
      integer, intent(in) :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        itimestep, &
                                        stepcu
      real, intent(in) :: &
                                        dt
      real, dimension(ims:ime, jms:jme), intent(in) :: &
                                        xland
      real, dimension(ims:ime, jms:jme), intent(inout) :: &
                                        raincv, pratec
      logical, dimension(ims:ime,jms:jme), intent(inout) :: &
                                        cu_act_flag
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: &
                                        dz8w, &
                                        p8w, &
                                        pcps, &
                                        pi3d, &
                                        qc3d, &
                                        qvften, &
                                        qvpblten, &
                                        qi3d, &
                                        qv3d, &
                                        rho3d, &
                                        t3d, &
                                        u3d, &
                                        v3d, &
                                        w
      real, dimension(ims:ime, kms:kme, jms:jme), &
               optional, intent(inout) :: &
                                        rqccuten, &
                                        rqicuten, &
                                        rqvcuten, &
                                        rthcuten, &
                                        rucuten, &
                                        rvcuten
     logical, optional :: &
                                                   f_qv &
                                                  ,f_qc &
                                                  ,f_qr &
                                                  ,f_qi &
                                                  ,f_qs
      real, dimension(ims:ime, jms:jme) :: &
                                        qfx
      real :: &
                                        delt, &
                                        rdelt
      real , dimension(its:ite) :: &
                                        rcs, &
                                        rn, &
                                        evap
      integer , dimension(its:ite) :: slimsk
      real , dimension(its:ite, kts:kte+1) :: &
                                        prsi
      real , dimension(its:ite, kts:kte) :: &
                                        del, &
                                        dot, &
                                        phil, &
                                        prsl, &
                                        q1, &
                                        q2, &
                                        q3, &
                                        q1b, &
                                        q1bl, &
                                        q11, &
                                        q12, &
                                        t1, &
                                        u1, &
                                        v1, &
                                        zi, &
                                        zl, &
                                        omg, &
                                        ght
      integer, dimension(its:ite) :: &
                                        kbot, &
                                        ktop
      integer :: &
                                        i, &
                                        im, &
                                        j, &
                                        k, &
                                        km, &
                                        kp, &
                                        kx
      logical :: run_param , doing_adapt_dt , decided
      integer,dimension( its:ite ) :: ktype
      real, dimension( kts:kte ) :: sig1
      real, dimension( kms:kme ) :: znu
      integer :: zz
      do j=jts,jte
         do i=its,ite
            cu_act_flag(i,j)=.true.
         enddo
      enddo
      im=ite-its+1
      kx=kte-kts+1
      delt=dt*stepcu
      rdelt=1./delt
   do j=jts,jte
      do i=its,ite
        zi(i,kts)=0.0
      enddo
      do k=kts+1,kte
        km=k-1
        do i=its,ite
          zi(i,k)=zi(i,km)+dz8w(i,km,j)
        enddo
      enddo
      do k=kts+1,kte
        km=k-1
        do i=its,ite
          zl(i,km)=(zi(i,k)+zi(i,km))*0.5
        enddo
      enddo
      do i=its,ite
        zl(i,kte)=2.*zi(i,kte)-zl(i,kte-1)
      enddo
      do i=its,ite
        slimsk(i)=int(abs(xland(i,j)-2.))
      enddo
      do k=kts,kte
        kp=k+1
        do i=its,ite
          dot(i,k)=-0.5*g*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j))
        enddo
      enddo
      do k=kts,kte
        zz = kte+1-k
        do i=its,ite
          u1(i,zz)=u3d(i,k,j)
          v1(i,zz)=v3d(i,k,j)
          t1(i,zz)=t3d(i,k,j)
          q1(i,zz)= qv3d(i,k,j)
          if(itimestep == 1) then
             q1b(i,zz)=0.
             q1bl(i,zz)=0.
          else
             q1b(i,zz)=qvften(i,k,j)
             q1bl(i,zz)=qvpblten(i,k,j)
          endif
          q2(i,zz)=qc3d(i,k,j)
          q3(i,zz)=qi3d(i,k,j)
          omg(i,zz)=dot(i,k)
          ght(i,zz)=zl(i,k)
          prsl(i,zz) = pcps(i,k,j)
        enddo
      enddo
      do k=kts,kte+1
        zz = kte+2-k
        do i=its,ite
          prsi(i,zz) = p8w(i,k,j)
        enddo
      enddo
      do k=kts,kte
         zz = kte+1-k
         sig1(zz) = znu(k)
      enddo
      do i=its,ite
        evap(i) = qfx(i,j)
      enddo
      call tiecnv(u1,v1,t1,q1,q2,q3,q1b,q1bl,ght,omg,prsl,prsi,evap, &
                  rn,slimsk,ktype,im,kx,kx+1,sig1,delt)
      do i=its,ite
         raincv(i,j)=rn(i)/stepcu
         pratec(i,j)=rn(i)/(stepcu * dt)
      enddo
      do k=kts,kte
        zz = kte+1-k
        do i=its,ite
          rthcuten(i,k,j)=(t1(i,zz)-t3d(i,k,j))/pi3d(i,k,j)*rdelt
          rqvcuten(i,k,j)=(q1(i,zz)-qv3d(i,k,j))*rdelt
          rucuten(i,k,j) =(u1(i,zz)-u3d(i,k,j))*rdelt
          rvcuten(i,k,j) =(v1(i,zz)-v3d(i,k,j))*rdelt
        enddo
      enddo
      if(present(rqccuten))then
        if ( f_qc ) then
          do k=kts,kte
            zz = kte+1-k
            do i=its,ite
              rqccuten(i,k,j)=(q2(i,zz)-qc3d(i,k,j))*rdelt
            enddo
          enddo
        endif
      endif
      if(present(rqicuten))then
        if ( f_qi ) then
          do k=kts,kte
            zz = kte+1-k
            do i=its,ite
              rqicuten(i,k,j)=(q3(i,zz)-qi3d(i,k,j))*rdelt
            enddo
          enddo
        endif
      endif
   enddo
   end subroutine cu_tiedtke
   subroutine tiedtkeinit(rthcuten,rqvcuten,rqccuten,rqicuten, &
                     rucuten,rvcuten, &
                     restart,p_qc,p_qi,p_first_scalar, &
                     allowed_to_read, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte)
   implicit none
   logical , intent(in) :: allowed_to_read,restart
   integer , intent(in) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   integer , intent(in) :: p_first_scalar, p_qi, p_qc
   real, dimension( ims:ime , kms:kme , jms:jme ) , intent(out) :: &
                                                              rthcuten, &
                                                              rqvcuten, &
                                                              rqccuten, &
                                                              rqicuten, &
                                                              rucuten,rvcuten
   integer :: i, j, k, itf, jtf, ktf
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
   if(.not.restart)then
     do j=jts,jtf
     do k=kts,ktf
     do i=its,itf
       rthcuten(i,k,j)=0.
       rqvcuten(i,k,j)=0.
       rucuten(i,k,j)=0.
       rvcuten(i,k,j)=0.
     enddo
     enddo
     enddo
     if (p_qc .ge. p_first_scalar) then
        do j=jts,jtf
        do k=kts,ktf
        do i=its,itf
           rqccuten(i,k,j)=0.
        enddo
        enddo
        enddo
     endif
     if (p_qi .ge. p_first_scalar) then
        do j=jts,jtf
        do k=kts,ktf
        do i=its,itf
           rqicuten(i,k,j)=0.
        enddo
        enddo
        enddo
     endif
   endif
      end subroutine tiedtkeinit
      subroutine tiecnv(pu,pv,pt,pqv,pqc,pqi,pqvf,pqvbl,poz,pomg, &
               pap,paph,evap,zprecc,lndj,ktype,lq,km,km1,sig1,dt)
      implicit none
      real pu(lq,km),pv(lq,km),pt(lq,km),pqv(lq,km),pqvf(lq,km)
      real poz(lq,km),pomg(lq,km),evap(lq),zprecc(lq),pqvbl(lq,km)
      real pum1(lq,km), pvm1(lq,km), &
          ptte(lq,km), pqte(lq,km), pvom(lq,km), pvol(lq,km), &
          pverv(lq,km), pgeo(lq,km), pap(lq,km), paph(lq,km1)
      real pqhfl(lq), zqq(lq,km), paprc(lq), paprs(lq), &
          prsfc(lq), pssfc(lq), paprsm(lq), pcte(lq,km)
      real ztp1(lq,km), zqp1(lq,km), ztu(lq,km), zqu(lq,km), &
          zlu(lq,km), zlude(lq,km), zmfu(lq,km), zmfd(lq,km), &
          zqsat(lq,km), pqc(lq,km), pqi(lq,km), zrain(lq)
      real sig(km1),sig1(km)
      integer icbot(lq), ictop(lq), ktype(lq), lndj(lq)
      real dt
      logical locum(lq)
      real psheat,psrain,psevap,psmelt,psdiss,tt
      real ztmst,ztpp1,fliq,fice,ztc,zalf
      integer i,j,k,lq,lp,km,km1
      ztmst=dt
      psheat=0.0
      psrain=0.0
      psevap=0.0
      psmelt=0.0
      psdiss=0.0
      do j=1,lq
        zrain(j)=0.0
        locum(j)=.false.
        prsfc(j)=0.0
        pssfc(j)=0.0
        paprc(j)=0.0
        paprs(j)=0.0
        paprsm(j)=0.0
        pqhfl(j)=evap(j)
     end do
      do k=1,km
        do j=1,lq
          ptte(j,k)=0.0
          pcte(j,k)=0.0
          pvom(j,k)=0.0
          pvol(j,k)=0.0
          ztp1(j,k)=pt(j,k)
          zqp1(j,k)=pqv(j,k)/(1.0+pqv(j,k))
          pum1(j,k)=pu(j,k)
          pvm1(j,k)=pv(j,k)
          pverv(j,k)=pomg(j,k)
          pgeo(j,k)=g*poz(j,k)
          tt=ztp1(j,k)
          zqsat(j,k)=tlucua(tt)/pap(j,k)
          zqsat(j,k)=min(0.5,zqsat(j,k))
          zqsat(j,k)=zqsat(j,k)/(1.-vtmpc1*zqsat(j,k))
          pqte(j,k)=pqvf(j,k)+pqvbl(j,k)
          zqq(j,k)=pqte(j,k)
        end do
      end do
      call cumastr_new &
         (lq, km, km1, km-1, ztp1, &
          zqp1, pum1, pvm1, pverv, zqsat, &
          pqhfl, ztmst, pap, paph, pgeo, &
          ptte, pqte, pvom, pvol, prsfc, &
          pssfc, paprc, paprsm, paprs, locum, &
          ktype, icbot, ictop, ztu, zqu, &
          zlu, zlude, zmfu, zmfd, zrain, &
          psrain, psevap, psheat, psdiss, psmelt, &
          pcte, sig1, lndj)
      if(fdbk.ge.1.0e-9) then
      do k=1,km
      do j=1,lq
      if(pcte(j,k).gt.0.0) then
        ztpp1=pt(j,k)+ptte(j,k)*ztmst
        if(ztpp1.ge.t000) then
           fliq=1.0
           zalf=0.0
        else if(ztpp1.le.hgfr) then
           fliq=0.0
           zalf=alf
        else
           ztc=ztpp1-t000
           fliq=0.0059+0.9941*exp(-0.003102*ztc*ztc)
           zalf=alf
        endif
        fice=1.0-fliq
        pqc(j,k)=pqc(j,k)+fliq*pcte(j,k)*ztmst
        pqi(j,k)=pqi(j,k)+fice*pcte(j,k)*ztmst
        ptte(j,k)=ptte(j,k)-zalf*rcpd*fliq*pcte(j,k)
      endif
      end do
      end do
      endif
      do k=1,km
        do j=1,lq
          pt(j,k)=ztp1(j,k)+ptte(j,k)*ztmst
          zqp1(j,k)=zqp1(j,k)+(pqte(j,k)-zqq(j,k))*ztmst
          pqv(j,k)=zqp1(j,k)/(1.0-zqp1(j,k))
        end do
      end do
      do j=1,lq
        zprecc(j)=amax1(0.0,(prsfc(j)+pssfc(j))*ztmst)
      end do
      if (lmfdudv) then
        do k=1,km
          do j=1,lq
            pu(j,k)=pu(j,k)+pvom(j,k)*ztmst
            pv(j,k)=pv(j,k)+pvol(j,k)*ztmst
          end do
        end do
      endif
      return
      end subroutine tiecnv
      subroutine cumastr_new &
         (klon, klev, klevp1, klevm1, pten, &
          pqen, puen, pven, pverv, pqsen, &
          pqhfl, ztmst, pap, paph, pgeo, &
          ptte, pqte, pvom, pvol, prsfc, &
          pssfc, paprc, paprsm, paprs, ldcum, &
          ktype, kcbot, kctop, ptu, pqu, &
          plu, plude, pmfu, pmfd, prain, &
          psrain, psevap, psheat, psdiss, psmelt,&
          pcte, sig1, lndj)
      implicit none
      integer klon, klev, klevp1
      integer klevm1
      real ztmst
      real psrain, psevap, psheat, psdiss, psmelt, zcons2
      integer jk,jl,ikb
      real zqumqe, zdqmin, zmfmax, zalvdcp, zqalv
      real zhsat, zgam, zzz, zhhat, zbi, zro, zdz, zdhdz, zdepth
      real zfac, zrh, zpbmpt, dept, zht, zeps
      integer icum, itopm2
      real pten(klon,klev), pqen(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              ptte(klon,klev), pqte(klon,klev), &
              pvom(klon,klev), pvol(klon,klev), &
              pqsen(klon,klev), pgeo(klon,klev), &
              pap(klon,klev), paph(klon,klevp1),&
              pverv(klon,klev), pqhfl(klon)
      real ptu(klon,klev), pqu(klon,klev), &
              plu(klon,klev), plude(klon,klev), &
              pmfu(klon,klev), pmfd(klon,klev), &
              paprc(klon), paprs(klon), &
              paprsm(klon), prain(klon), &
              prsfc(klon), pssfc(klon)
      real ztenh(klon,klev), zqenh(klon,klev),&
              zgeoh(klon,klev), zqsenh(klon,klev),&
              ztd(klon,klev), zqd(klon,klev), &
              zmfus(klon,klev), zmfds(klon,klev), &
              zmfuq(klon,klev), zmfdq(klon,klev), &
              zdmfup(klon,klev), zdmfdp(klon,klev),&
              zmful(klon,klev), zrfl(klon), &
              zuu(klon,klev), zvu(klon,klev), &
              zud(klon,klev), zvd(klon,klev)
      real zentr(klon), zhcbase(klon), &
              zmfub(klon), zmfub1(klon), &
              zdqpbl(klon), zdqcv(klon)
      real zsfl(klon), zdpmel(klon,klev), &
              pcte(klon,klev), zcape(klon), &
              zheat(klon), zhhatt(klon,klev), &
              zhmin(klon), zrelh(klon)
      real sig1(klev)
      integer ilab(klon,klev), idtop(klon), &
              ictop0(klon), ilwmin(klon)
      integer kcbot(klon), kctop(klon), &
              ktype(klon), ihmin(klon), &
              ktop0, lndj(klon)
      logical ldcum(klon)
      logical loddraf(klon), llo1
      zcons2=1./(g*ztmst)
      call cuini &
         (klon, klev, klevp1, klevm1, pten, &
          pqen, pqsen, puen, pven, pverv, &
          pgeo, paph, zgeoh, ztenh, zqenh, &
          zqsenh, ilwmin, ptu, pqu, ztd, &
          zqd, zuu, zvu, zud, zvd, &
          pmfu, pmfd, zmfus, zmfds, zmfuq, &
          zmfdq, zdmfup, zdmfdp, zdpmel, plu, &
          plude, ilab)
      call cubase &
         (klon, klev, klevp1, klevm1, ztenh, &
          zqenh, zgeoh, paph, ptu, pqu, &
          plu, puen, pven, zuu, zvu, &
          ldcum, kcbot, ilab)
       jk=1
       do jl=1,klon
       zdqcv(jl) =pqte(jl,jk)*(paph(jl,jk+1)-paph(jl,jk))
       zdqpbl(jl)=0.0
       idtop(jl)=0
       end do
       do jk=2,klev
       do jl=1,klon
       zdqcv(jl)=zdqcv(jl)+pqte(jl,jk)*(paph(jl,jk+1)-paph(jl,jk))
       if(jk.ge.kcbot(jl)) zdqpbl(jl)=zdqpbl(jl)+pqte(jl,jk) &
                                    *(paph(jl,jk+1)-paph(jl,jk))
       end do
       end do
      do jl=1,klon
         ktype(jl)=0
      if(zdqcv(jl).gt.max(0.,1.1*pqhfl(jl)*g)) then
         ktype(jl)=1
      else
         ktype(jl)=2
      endif
      ikb=kcbot(jl)
      zqumqe=pqu(jl,ikb)+plu(jl,ikb)-zqenh(jl,ikb)
      zdqmin=max(0.01*zqenh(jl,ikb),1.e-10)
      if(zdqpbl(jl).gt.0..and.zqumqe.gt.zdqmin.and.ldcum(jl)) then
         zmfub(jl)=zdqpbl(jl)/(g*max(zqumqe,zdqmin))
      else
         zmfub(jl)=0.01
         ldcum(jl)=.false.
      endif
      zmfmax=(paph(jl,ikb)-paph(jl,ikb-1))*zcons2
      zmfub(jl)=min(zmfub(jl),zmfmax)
      ikb=kcbot(jl)
      zhcbase(jl)=cpd*ptu(jl,ikb)+zgeoh(jl,ikb)+alv*pqu(jl,ikb)
      ictop0(jl)=kcbot(jl)-1
      end do
      zalvdcp=alv/cpd
      zqalv=1./alv
      do jk=klevm1,3,-1
      do jl=1,klon
      zhsat=cpd*ztenh(jl,jk)+zgeoh(jl,jk)+alv*zqsenh(jl,jk)
      zgam=c5les*zalvdcp*zqsenh(jl,jk)/ &
          ((1.-vtmpc1*zqsenh(jl,jk))*(ztenh(jl,jk)-c4les)**2)
      zzz=cpd*ztenh(jl,jk)*0.608
      zhhat=zhsat-(zzz+zgam*zzz)/(1.+zgam*zzz*zqalv)* &
                 max(zqsenh(jl,jk)-zqenh(jl,jk),0.)
      zhhatt(jl,jk)=zhhat
      if(jk.lt.ictop0(jl).and.zhcbase(jl).gt.zhhat) ictop0(jl)=jk
      end do
      end do
      do jl=1,klon
      jk=kcbot(jl)
      zhsat=cpd*ztenh(jl,jk)+zgeoh(jl,jk)+alv*zqsenh(jl,jk)
      zgam=c5les*zalvdcp*zqsenh(jl,jk)/ &
          ((1.-vtmpc1*zqsenh(jl,jk))*(ztenh(jl,jk)-c4les)**2)
      zzz=cpd*ztenh(jl,jk)*0.608
      zhhat=zhsat-(zzz+zgam*zzz)/(1.+zgam*zzz*zqalv)* &
                 max(zqsenh(jl,jk)-zqenh(jl,jk),0.)
      zhhatt(jl,jk)=zhhat
      end do
      do jl = 1, klon
         zhmin(jl) = 0.
         if( ldcum(jl).and.ktype(jl).eq.1 ) then
            ihmin(jl) = kcbot(jl)
         else
            ihmin(jl) = -1
         end if
      end do
      zbi = 1./(25.*g)
      do jk = klev, 1, -1
      do jl = 1, klon
      llo1 = ldcum(jl).and.ktype(jl).eq.1.and.ihmin(jl).eq.kcbot(jl)
      if (llo1.and.jk.lt.kcbot(jl).and.jk.ge.ictop0(jl)) then
        ikb = kcbot(jl)
        zro = rd*ztenh(jl,jk)/(g*paph(jl,jk))
        zdz = (paph(jl,jk)-paph(jl,jk-1))*zro
        zdhdz=(cpd*(pten(jl,jk-1)-pten(jl,jk))+alv*(pqen(jl,jk-1)- &
          pqen(jl,jk))+(pgeo(jl,jk-1)-pgeo(jl,jk)))*g/(pgeo(jl, &
          jk-1)-pgeo(jl,jk))
        zdepth = zgeoh(jl,jk) - zgeoh(jl,ikb)
        zfac = sqrt(1.+zdepth*zbi)
        zhmin(jl) = zhmin(jl) + zdhdz*zfac*zdz
        zrh = -alv*(zqsenh(jl,jk)-zqenh(jl,jk))*zfac
        if (zhmin(jl).gt.zrh) ihmin(jl) = jk
      end if
      end do
      end do
      do jl = 1, klon
      if (ldcum(jl).and.ktype(jl).eq.1) then
        if (ihmin(jl).lt.ictop0(jl)) ihmin(jl) = ictop0(jl)
      end if
      if(ktype(jl).eq.1) then
        zentr(jl)=entrpen
      else
        zentr(jl)=entrscv
      endif
      if(lndj(jl).eq.1) zentr(jl)=zentr(jl)*1.1
      end do
      call cuasc_new &
         (klon, klev, klevp1, klevm1, ztenh, &
          zqenh, puen, pven, pten, pqen, &
          pqsen, pgeo, zgeoh, pap, paph, &
          pqte, pverv, ilwmin, ldcum, zhcbase, &
          ktype, ilab, ptu, pqu, plu, &
          zuu, zvu, pmfu, zmfub, zentr, &
          zmfus, zmfuq, zmful, plude, zdmfup, &
          kcbot, kctop, ictop0, icum, ztmst, &
          ihmin, zhhatt, zqsenh)
      if(icum.eq.0) return
      do jl=1,klon
      zpbmpt=paph(jl,kcbot(jl))-paph(jl,kctop(jl))
      if(ldcum(jl)) ictop0(jl)=kctop(jl)
      if(ldcum(jl).and.ktype(jl).eq.1.and.zpbmpt.lt.zdnoprc) ktype(jl)=2
      if(ktype(jl).eq.2) then
        zentr(jl)=entrscv
        if(lndj(jl).eq.1) zentr(jl)=zentr(jl)*1.1
      endif
      zrfl(jl)=zdmfup(jl,1)
      end do
      do jk=2,klev
      do jl=1,klon
          zrfl(jl)=zrfl(jl)+zdmfup(jl,jk)
      end do
      end do
      if(lmfdd) then
         call cudlfs &
         (klon, klev, klevp1, ztenh, zqenh, &
          puen, pven, zgeoh, paph, ptu, &
          pqu, zuu, zvu, ldcum, kcbot, &
          kctop, zmfub, zrfl, ztd, zqd, &
          zud, zvd, pmfd, zmfds, zmfdq, &
          zdmfdp, idtop, loddraf)
         call cuddraf &
         (klon, klev, klevp1, ztenh, zqenh, &
          puen, pven, zgeoh, paph, zrfl, &
          loddraf, ztd, zqd, zud, zvd, &
          pmfd, zmfds, zmfdq, zdmfdp)
      end if
      do jl=1,klon
        zheat(jl)=0.0
        zcape(jl)=0.0
        zrelh(jl)=0.0
        zmfub1(jl)=zmfub(jl)
      end do
      do jl=1,klon
      if(ldcum(jl).and.ktype(jl).eq.1) then
       ktop0=max(12,kctop(jl))
       ikb = kcbot(jl)
       do jk=2,klev
       if(jk.le.kcbot(jl).and.jk.gt.kctop(jl)) then
         zro=paph(jl,jk)/(rd*ztenh(jl,jk))
         zdz=(paph(jl,jk)-paph(jl,jk-1))/(g*zro)
         zheat(jl)=zheat(jl)+((pten(jl,jk-1)-pten(jl,jk) &
           +g*zdz/cpd)/ztenh(jl,jk)+0.608*(pqen(jl,jk-1)- &
           pqen(jl,jk)))*(pmfu(jl,jk)+pmfd(jl,jk))*g/zro
         zcape(jl)=zcape(jl)+g*((ptu(jl,jk)*(1.+.608*pqu(jl,jk) &
           -plu(jl,jk)))/(ztenh(jl,jk)*(1.+.608*zqenh(jl,jk))) &
           -1.0)*zdz
       endif
       if(jk.le.kcbot(jl).and.jk.gt.ktop0) then
         dept=(paph(jl,jk+1)-paph(jl,jk))/(paph(jl,ikb+1)- &
            paph(jl,ktop0+1))
         zrelh(jl)=zrelh(jl)+dept*pqen(jl,jk)/pqsen(jl,jk)
       endif
       enddo
       if(zrelh(jl).ge.crirh) then
         zht=max(0.0,(zcape(jl)-0.0))/(ztau*zheat(jl))
         zmfub1(jl)=max(zmfub(jl)*zht,0.01)
         zmfmax=(paph(jl,ikb)-paph(jl,ikb-1))*zcons2
         zmfub1(jl)=min(zmfub1(jl),zmfmax)
       else
         zmfub1(jl)=0.01
         zmfub(jl)=0.01
         ldcum(jl)=.false.
        endif
       endif
       end do
       do jl=1,klon
        if(ktype(jl).ne.1) then
           ikb=kcbot(jl)
           if(pmfd(jl,ikb).lt.0.0.and.loddraf(jl)) then
              zeps=cmfdeps
           else
              zeps=0.
           endif
           zqumqe=pqu(jl,ikb)+plu(jl,ikb)- &
                 zeps*zqd(jl,ikb)-(1.-zeps)*zqenh(jl,ikb)
           zdqmin=max(0.01*zqenh(jl,ikb),1.e-10)
           zmfmax=(paph(jl,ikb)-paph(jl,ikb-1))*zcons2
           if(zdqpbl(jl).gt.0..and.zqumqe.gt.zdqmin.and.ldcum(jl) &
             .and.zmfub(jl).lt.zmfmax) then
              zmfub1(jl)=zdqpbl(jl)/(g*max(zqumqe,zdqmin))
           else
              zmfub1(jl)=zmfub(jl)
           endif
           llo1=(ktype(jl).eq.2).and.abs(zmfub1(jl) &
                -zmfub(jl)).lt.0.2*zmfub(jl)
           if(.not.llo1) zmfub1(jl)=zmfub(jl)
           zmfub1(jl)=min(zmfub1(jl),zmfmax)
        end if
        end do
        do jk=1,klev
        do jl=1,klon
        if(ldcum(jl)) then
           zfac=zmfub1(jl)/max(zmfub(jl),1.e-10)
           pmfd(jl,jk)=pmfd(jl,jk)*zfac
           zmfds(jl,jk)=zmfds(jl,jk)*zfac
           zmfdq(jl,jk)=zmfdq(jl,jk)*zfac
           zdmfdp(jl,jk)=zdmfdp(jl,jk)*zfac
        else
           pmfd(jl,jk)=0.0
           zmfds(jl,jk)=0.0
           zmfdq(jl,jk)=0.0
           zdmfdp(jl,jk)=0.0
        endif
        end do
        end do
        do jl=1,klon
           if(ldcum(jl)) then
              zmfub(jl)=zmfub1(jl)
           else
              zmfub(jl)=0.0
           endif
        end do
      call cuasc_new &
         (klon, klev, klevp1, klevm1, ztenh, &
          zqenh, puen, pven, pten, pqen, &
          pqsen, pgeo, zgeoh, pap, paph, &
          pqte, pverv, ilwmin, ldcum, zhcbase,&
          ktype, ilab, ptu, pqu, plu, &
          zuu, zvu, pmfu, zmfub, zentr, &
          zmfus, zmfuq, zmful, plude, zdmfup, &
          kcbot, kctop, ictop0, icum, ztmst, &
          ihmin, zhhatt, zqsenh)
      call cuflx &
         (klon, klev, klevp1, pqen, pqsen, &
          ztenh, zqenh, paph, zgeoh, kcbot, &
          kctop, idtop, ktype, loddraf, ldcum, &
          pmfu, pmfd, zmfus, zmfds, zmfuq, &
          zmfdq, zmful, plude, zdmfup, zdmfdp, &
          zrfl, prain, pten, zsfl, zdpmel, &
          itopm2, ztmst, sig1)
      call cudtdq &
         (klon, klev, klevp1, itopm2, paph, &
          ldcum, pten, ptte, pqte, zmfus, &
          zmfds, zmfuq, zmfdq, zmful, zdmfup, &
          zdmfdp, ztmst, zdpmel, prain, zrfl, &
          zsfl, psrain, psevap, psheat, psmelt, &
          prsfc, pssfc, paprc, paprsm, paprs, &
          pqen, pqsen, plude, pcte)
      if(lmfdudv) then
      call cududv &
         (klon, klev, klevp1, itopm2, ktype, &
          kcbot, paph, ldcum, puen, pven, &
          pvom, pvol, zuu, zud, zvu, &
          zvd, pmfu, pmfd, psdiss)
      end if
      return
      end subroutine cumastr_new
      subroutine cuini &
         (klon, klev, klevp1, klevm1, pten, &
          pqen, pqsen, puen, pven, pverv, &
          pgeo, paph, pgeoh, ptenh, pqenh, &
          pqsenh, klwmin, ptu, pqu, ptd, &
          pqd, puu, pvu, pud, pvd, &
          pmfu, pmfd, pmfus, pmfds, pmfuq, &
          pmfdq, pdmfup, pdmfdp, pdpmel, plu, &
          plude, klab)
      implicit none
      integer klon, klev, klevp1
      integer klevm1
      integer jk,jl,ik, icall
      real zdp, zzs
      real pten(klon,klev), pqen(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              pqsen(klon,klev), pverv(klon,klev), &
              pgeo(klon,klev), pgeoh(klon,klev), &
              paph(klon,klevp1), ptenh(klon,klev), &
              pqenh(klon,klev), pqsenh(klon,klev)
      real ptu(klon,klev), pqu(klon,klev), &
              ptd(klon,klev), pqd(klon,klev), &
              puu(klon,klev), pud(klon,klev), &
              pvu(klon,klev), pvd(klon,klev), &
              pmfu(klon,klev), pmfd(klon,klev), &
              pmfus(klon,klev), pmfds(klon,klev), &
              pmfuq(klon,klev), pmfdq(klon,klev), &
              pdmfup(klon,klev), pdmfdp(klon,klev), &
              plu(klon,klev), plude(klon,klev)
      real zwmax(klon), zph(klon), &
              pdpmel(klon,klev)
      integer klab(klon,klev), klwmin(klon)
      logical loflag(klon)
      zdp=0.5
      do jk=2,klev
      do jl=1,klon
      pgeoh(jl,jk)=pgeo(jl,jk)+(pgeo(jl,jk-1)-pgeo(jl,jk))*zdp
      ptenh(jl,jk)=(max(cpd*pten(jl,jk-1)+pgeo(jl,jk-1), &
                  cpd*pten(jl,jk)+pgeo(jl,jk))-pgeoh(jl,jk))*rcpd
      pqsenh(jl,jk)=pqsen(jl,jk-1)
      zph(jl)=paph(jl,jk)
      loflag(jl)=.true.
      end do
      ik=jk
      icall=0
      call cuadjtq(klon,klev,ik,zph,ptenh,pqsenh,loflag,icall)
      do jl=1,klon
      pqenh(jl,jk)=min(pqen(jl,jk-1),pqsen(jl,jk-1)) &
                 +(pqsenh(jl,jk)-pqsen(jl,jk-1))
      pqenh(jl,jk)=max(pqenh(jl,jk),0.)
      end do
      end do
      do jl=1,klon
      ptenh(jl,klev)=(cpd*pten(jl,klev)+pgeo(jl,klev)- &
                     pgeoh(jl,klev))*rcpd
      pqenh(jl,klev)=pqen(jl,klev)
      ptenh(jl,1)=pten(jl,1)
      pqenh(jl,1)=pqen(jl,1)
      pgeoh(jl,1)=pgeo(jl,1)
      klwmin(jl)=klev
      zwmax(jl)=0.
      end do
      do jk=klevm1,2,-1
      do jl=1,klon
      zzs=max(cpd*ptenh(jl,jk)+pgeoh(jl,jk), &
             cpd*ptenh(jl,jk+1)+pgeoh(jl,jk+1))
      ptenh(jl,jk)=(zzs-pgeoh(jl,jk))*rcpd
      end do
      end do
      do jk=klev,3,-1
      do jl=1,klon
      if(pverv(jl,jk).lt.zwmax(jl)) then
         zwmax(jl)=pverv(jl,jk)
         klwmin(jl)=jk
      end if
      end do
      end do
      do jk=1,klev
      ik=jk-1
      if(jk.eq.1) ik=1
      do jl=1,klon
      ptu(jl,jk)=ptenh(jl,jk)
      ptd(jl,jk)=ptenh(jl,jk)
      pqu(jl,jk)=pqenh(jl,jk)
      pqd(jl,jk)=pqenh(jl,jk)
      plu(jl,jk)=0.
      puu(jl,jk)=puen(jl,ik)
      pud(jl,jk)=puen(jl,ik)
      pvu(jl,jk)=pven(jl,ik)
      pvd(jl,jk)=pven(jl,ik)
      pmfu(jl,jk)=0.
      pmfd(jl,jk)=0.
      pmfus(jl,jk)=0.
      pmfds(jl,jk)=0.
      pmfuq(jl,jk)=0.
      pmfdq(jl,jk)=0.
      pdmfup(jl,jk)=0.
      pdmfdp(jl,jk)=0.
      pdpmel(jl,jk)=0.
      plude(jl,jk)=0.
      klab(jl,jk)=0
      end do
      end do
      return
      end subroutine cuini
      subroutine cubase &
         (klon, klev, klevp1, klevm1, ptenh, &
          pqenh, pgeoh, paph, ptu, pqu, &
          plu, puen, pven, puu, pvu, &
          ldcum, kcbot, klab)
      implicit none
      integer klon, klev, klevp1
      integer klevm1
      integer jl,jk,is,ik,icall,ikb
      real zbuo,zz
      real ptenh(klon,klev), pqenh(klon,klev), &
              pgeoh(klon,klev), paph(klon,klevp1)
      real ptu(klon,klev), pqu(klon,klev), &
              plu(klon,klev)
      real puen(klon,klev), pven(klon,klev), &
              puu(klon,klev), pvu(klon,klev)
      real zqold(klon,klev), zph(klon)
      integer klab(klon,klev), kcbot(klon)
      logical ldcum(klon), loflag(klon)
      logical ldbase(klon)
      logical llo1
      do jl=1,klon
        klab(jl,klev)=1
        kcbot(jl)=klevm1
        ldcum(jl)=.false.
        ldbase(jl)=.false.
        puu(jl,klev)=puen(jl,klev)*(paph(jl,klevp1)-paph(jl,klev))
        pvu(jl,klev)=pven(jl,klev)*(paph(jl,klevp1)-paph(jl,klev))
      end do
      do jk=1,klev
      do jl=1,klon
        zqold(jl,jk)=0.0
      end do
      end do
      do jk=klevm1,2,-1
        is=0
        do jl=1,klon
          if(klab(jl,jk+1).eq.1 .or.(ldcum(jl).and.kcbot(jl).eq.jk+1)) then
             is=is+1
             loflag(jl)=.true.
          else
             loflag(jl)=.false.
          endif
          zph(jl)=paph(jl,jk)
        end do
        if(is.eq.0) cycle
        if(lmfdudv) then
          do jl=1,klon
            if(.not.ldbase(jl)) then
              puu(jl,klev)=puu(jl,klev)+ &
                  puen(jl,jk)*(paph(jl,jk+1)-paph(jl,jk))
              pvu(jl,klev)=pvu(jl,klev)+ &
                  pven(jl,jk)*(paph(jl,jk+1)-paph(jl,jk))
            endif
          enddo
        endif
        do jl=1,klon
          if(loflag(jl)) then
             pqu(jl,jk)=pqu(jl,jk+1)
             ptu(jl,jk)=(cpd*ptu(jl,jk+1)+pgeoh(jl,jk+1) &
                       -pgeoh(jl,jk))*rcpd
             zqold(jl,jk)=pqu(jl,jk)
          end if
        end do
        ik=jk
        icall=1
        call cuadjtq(klon,klev,ik,zph,ptu,pqu,loflag,icall)
        do jl=1,klon
          if(loflag(jl)) then
           if(pqu(jl,jk).eq.zqold(jl,jk)) then
             zbuo=ptu(jl,jk)*(1.+vtmpc1*pqu(jl,jk))- &
                 ptenh(jl,jk)*(1.+vtmpc1*pqenh(jl,jk))+zbuo0
             if(zbuo.gt.0.) klab(jl,jk)=1
           else
             klab(jl,jk)=2
             plu(jl,jk)=plu(jl,jk)+zqold(jl,jk)-pqu(jl,jk)
             zbuo=ptu(jl,jk)*(1.+vtmpc1*pqu(jl,jk))- &
                 ptenh(jl,jk)*(1.+vtmpc1*pqenh(jl,jk))+zbuo0
             llo1=zbuo.gt.0..and.klab(jl,jk+1).eq.1
             if(llo1) then
                kcbot(jl)=jk
                ldcum(jl)=.true.
                ldbase(jl)=.true.
             end if
           end if
          end if
        end do
      end do
      if(lmfdudv) then
         do jl=1,klon
         if(ldcum(jl)) then
            ikb=kcbot(jl)
            zz=1./(paph(jl,klevp1)-paph(jl,ikb))
            puu(jl,klev)=puu(jl,klev)*zz
            pvu(jl,klev)=pvu(jl,klev)*zz
         else
            puu(jl,klev)=puen(jl,klevm1)
            pvu(jl,klev)=pven(jl,klevm1)
         end if
         end do
      end if
      return
      end subroutine cubase
      subroutine cuasc_new &
         (klon, klev, klevp1, klevm1, ptenh, &
          pqenh, puen, pven, pten, pqen, &
          pqsen, pgeo, pgeoh, pap, paph, &
          pqte, pverv, klwmin, ldcum, phcbase,&
          ktype, klab, ptu, pqu, plu, &
          puu, pvu, pmfu, pmfub, pentr, &
          pmfus, pmfuq, pmful, plude, pdmfup, &
          kcbot, kctop, kctop0, kcum, ztmst, &
          khmin, phhatt, pqsenh)
      implicit none
      integer klon, klev, klevp1
      integer klevm1,kcum
      real ztmst,zcons2,zdz,zdrodz
      integer jl,jk,ikb,ik,is,ikt,icall
      real zmfmax,zfac,zmftest,zdprho,zmse,znevn,zodmax
      real zqeen,zseen,zscde,zga,zdt,zscod
      real zqude,zqcod, zmfusk, zmfuqk,zmfulk
      real zbuo, zprcon, zlnew, zz, zdmfeu, zdmfdu
      real zbuoyz,zzdmf
      real ptenh(klon,klev), pqenh(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              pten(klon,klev), pqen(klon,klev), &
              pgeo(klon,klev), pgeoh(klon,klev), &
              pap(klon,klev), paph(klon,klevp1), &
              pqsen(klon,klev), pqte(klon,klev), &
              pverv(klon,klev), pqsenh(klon,klev)
      real ptu(klon,klev), pqu(klon,klev), &
              puu(klon,klev), pvu(klon,klev), &
              pmfu(klon,klev), zph(klon), &
              pmfub(klon), pentr(klon), &
              pmfus(klon,klev), pmfuq(klon,klev), &
              plu(klon,klev), plude(klon,klev), &
              pmful(klon,klev), pdmfup(klon,klev)
      real zdmfen(klon), zdmfde(klon), &
              zmfuu(klon), zmfuv(klon), &
              zpbase(klon), zqold(klon), &
              phhatt(klon,klev), zodetr(klon,klev), &
              zoentr(klon,klev), zbuoy(klon)
      real phcbase(klon)
      integer klwmin(klon), ktype(klon), &
              klab(klon,klev), kcbot(klon), &
              kctop(klon), kctop0(klon), &
              khmin(klon)
      logical ldcum(klon), loflag(klon)
      zcons2=1./(g*ztmst)
      do jl=1,klon
        zmfuu(jl)=0.
        zmfuv(jl)=0.
        zbuoy(jl)=0.
        if(.not.ldcum(jl)) ktype(jl)=0
      end do
      do jk=1,klev
      do jl=1,klon
          plu(jl,jk)=0.
          pmfu(jl,jk)=0.
          pmfus(jl,jk)=0.
          pmfuq(jl,jk)=0.
          pmful(jl,jk)=0.
          plude(jl,jk)=0.
          pdmfup(jl,jk)=0.
          zoentr(jl,jk)=0.
          zodetr(jl,jk)=0.
          if(.not.ldcum(jl).or.ktype(jl).eq.3) klab(jl,jk)=0
          if(.not.ldcum(jl).and.paph(jl,jk).lt.4.e4) kctop0(jl)=jk
      end do
      end do
      do jl=1,klon
        kctop(jl)=klevm1
        if(.not.ldcum(jl)) then
           kcbot(jl)=klevm1
           pmfub(jl)=0.
           pqu(jl,klev)=0.
        end if
        pmfu(jl,klev)=pmfub(jl)
        pmfus(jl,klev)=pmfub(jl)*(cpd*ptu(jl,klev)+pgeoh(jl,klev))
        pmfuq(jl,klev)=pmfub(jl)*pqu(jl,klev)
        if(lmfdudv) then
           zmfuu(jl)=pmfub(jl)*puu(jl,klev)
           zmfuv(jl)=pmfub(jl)*pvu(jl,klev)
        end if
      end do
      do jl=1,klon
      ldcum(jl)=.false.
      if (ktype(jl).eq.1) then
      ikb = kcbot(jl)
      zbuoy(jl)=g*((ptu(jl,ikb)-ptenh(jl,ikb))/ptenh(jl,ikb)+ &
               0.608*(pqu(jl,ikb)-pqenh(jl,ikb)))
       if (zbuoy(jl).gt.0.) then
        zdz = (pgeo(jl,ikb-1)-pgeo(jl,ikb))*zrg
        zdrodz = -log(pten(jl,ikb-1)/pten(jl,ikb))/zdz - &
                 g/(rd*ptenh(jl,ikb))
        zoentr(jl,ikb-1)=zbuoy(jl)*0.5/(1.+zbuoy(jl)*zdz) &
                +zdrodz
        zoentr(jl,ikb-1) = min(zoentr(jl,ikb-1),1.e-3)
        zoentr(jl,ikb-1) = max(zoentr(jl,ikb-1),0.)
       end if
      end if
      end do
      do jk=klevm1,2,-1
      ik=jk
      if(lmfmid.and.ik.lt.klevm1.and.ik.gt.klev-13) then
      call cubasmc &
         (klon, klev, klevm1, ik, pten, &
          pqen, pqsen, puen, pven, pverv, &
          pgeo, pgeoh, ldcum, ktype, klab, &
          pmfu, pmfub, pentr, kcbot, ptu, &
          pqu, plu, puu, pvu, pmfus, &
          pmfuq, pmful, pdmfup, zmfuu, zmfuv)
      endif
      is=0
      do jl=1,klon
        zqold(jl)=0.0
        is=is+klab(jl,jk+1)
        if(klab(jl,jk+1).eq.0) klab(jl,jk)=0
        loflag(jl)=klab(jl,jk+1).gt.0
        zph(jl)=paph(jl,jk)
        if(ktype(jl).eq.3.and.jk.eq.kcbot(jl)) then
           zmfmax=(paph(jl,jk)-paph(jl,jk-1))*zcons2
           if(pmfub(jl).gt.zmfmax) then
              zfac=zmfmax/pmfub(jl)
              pmfu(jl,jk+1)=pmfu(jl,jk+1)*zfac
              pmfus(jl,jk+1)=pmfus(jl,jk+1)*zfac
              pmfuq(jl,jk+1)=pmfuq(jl,jk+1)*zfac
              zmfuu(jl)=zmfuu(jl)*zfac
              zmfuv(jl)=zmfuv(jl)*zfac
              pmfub(jl)=zmfmax
           end if
        end if
      end do
      if(is.eq.0) cycle
      ik=jk
      call cuentr_new &
         (klon, klev, klevp1, ik, ptenh,&
          paph, pap, pgeoh, klwmin, ldcum,&
          ktype, kcbot, kctop0, zpbase, pmfu, &
          pentr, zdmfen, zdmfde, zodetr, khmin)
      do jl=1,klon
      if(loflag(jl)) then
        if(jk.lt.kcbot(jl)) then
         zmftest=pmfu(jl,jk+1)+zdmfen(jl)-zdmfde(jl)
         zmfmax=min(zmftest,(paph(jl,jk)-paph(jl,jk-1))*zcons2)
         zdmfen(jl)=max(zdmfen(jl)-max(zmftest-zmfmax,0.),0.)
        end if
        zdmfde(jl)=min(zdmfde(jl),0.75*pmfu(jl,jk+1))
        pmfu(jl,jk)=pmfu(jl,jk+1)+zdmfen(jl)-zdmfde(jl)
        if (jk.lt.kcbot(jl)) then
          zdprho = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
          zoentr(jl,jk) = zoentr(jl,jk)*zdprho*pmfu(jl,jk+1)
          zmftest = pmfu(jl,jk) + zoentr(jl,jk)-zodetr(jl,jk)
          zmfmax = min(zmftest,(paph(jl,jk)-paph(jl,jk-1))*zcons2)
          zoentr(jl,jk) = max(zoentr(jl,jk)-max(zmftest-zmfmax,0.),0.)
        end if
        if (ktype(jl).eq.1.and.jk.lt.kcbot(jl).and.jk.le.khmin(jl)) then
          zmse = cpd*ptu(jl,jk+1) + alv*pqu(jl,jk+1) + pgeoh(jl,jk+1)
          ikt = kctop0(jl)
          znevn=(pgeoh(jl,ikt)-pgeoh(jl,jk+1))*(zmse-phhatt(jl, &
               jk+1))*zrg
          if (znevn.le.0.) znevn = 1.
          zdprho = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
          zodmax = ((phcbase(jl)-zmse)/znevn)*zdprho*pmfu(jl,jk+1)
          zodmax = max(zodmax,0.)
          zodetr(jl,jk) = min(zodetr(jl,jk),zodmax)
        end if
        zodetr(jl,jk) = min(zodetr(jl,jk),0.75*pmfu(jl,jk))
        pmfu(jl,jk) = pmfu(jl,jk) + zoentr(jl,jk) - zodetr(jl,jk)
        zqeen=pqenh(jl,jk+1)*zdmfen(jl)
        zqeen=zqeen + pqenh(jl,jk+1)*zoentr(jl,jk)
        zseen=(cpd*ptenh(jl,jk+1)+pgeoh(jl,jk+1))*zdmfen(jl)
        zseen=zseen+(cpd*ptenh(jl,jk+1)+pgeoh(jl,jk+1))* &
             zoentr(jl,jk)
        zscde=(cpd*ptu(jl,jk+1)+pgeoh(jl,jk+1))*zdmfde(jl)
        zga = alv*pqsenh(jl,jk+1)/(rv*(ptenh(jl,jk+1)**2))
        zdt = (plu(jl,jk+1)-0.608*(pqsenh(jl,jk+1)-pqenh(jl, &
               jk+1)))/(1./ptenh(jl,jk+1)+0.608*zga)
        zscod = cpd*ptenh(jl,jk+1) + pgeoh(jl,jk+1) + cpd*zdt
        zscde = zscde + zodetr(jl,jk)*zscod
        zqude = pqu(jl,jk+1)*zdmfde(jl)
        zqcod = pqsenh(jl,jk+1) + zga*zdt
        zqude = zqude + zodetr(jl,jk)*zqcod
        plude(jl,jk) = plu(jl,jk+1)*zdmfde(jl)
        plude(jl,jk) = plude(jl,jk)+plu(jl,jk+1)*zodetr(jl,jk)
        zmfusk = pmfus(jl,jk+1) + zseen - zscde
        zmfuqk = pmfuq(jl,jk+1) + zqeen - zqude
        zmfulk = pmful(jl,jk+1) - plude(jl,jk)
        plu(jl,jk) = zmfulk*(1./max(cmfcmin,pmfu(jl,jk)))
        pqu(jl,jk) = zmfuqk*(1./max(cmfcmin,pmfu(jl,jk)))
        ptu(jl,jk)=(zmfusk*(1./max(cmfcmin,pmfu(jl,jk)))- &
            pgeoh(jl,jk))*rcpd
        ptu(jl,jk) = max(100.,ptu(jl,jk))
        ptu(jl,jk) = min(400.,ptu(jl,jk))
        zqold(jl) = pqu(jl,jk)
      end if
      end do
      ik=jk
      icall=1
      call cuadjtq(klon,klev,ik,zph,ptu,pqu,loflag,icall)
      do jl=1,klon
      if(loflag(jl).and.pqu(jl,jk).ne.zqold(jl)) then
         klab(jl,jk)=2
         plu(jl,jk)=plu(jl,jk)+zqold(jl)-pqu(jl,jk)
         zbuo=ptu(jl,jk)*(1.+vtmpc1*pqu(jl,jk)-plu(jl,jk))- &
        ptenh(jl,jk)*(1.+vtmpc1*pqenh(jl,jk))
         if(klab(jl,jk+1).eq.1) zbuo=zbuo+zbuo0
         if(zbuo.gt.0..and.pmfu(jl,jk).gt.0.01*pmfub(jl).and. &
                            jk.ge.kctop0(jl)) then
            kctop(jl)=jk
            ldcum(jl)=.true.
            if(zpbase(jl)-paph(jl,jk).ge.zdnoprc) then
               zprcon=cprcon
            else
               zprcon=0.
            endif
            zlnew=plu(jl,jk)/(1.+zprcon*(pgeoh(jl,jk)-pgeoh(jl,jk+1)))
            pdmfup(jl,jk)=max(0.,(plu(jl,jk)-zlnew)*pmfu(jl,jk))
            plu(jl,jk)=zlnew
         else
            klab(jl,jk)=0
            pmfu(jl,jk)=0.
         end if
      end if
      if(loflag(jl)) then
         pmful(jl,jk)=plu(jl,jk)*pmfu(jl,jk)
         pmfus(jl,jk)=(cpd*ptu(jl,jk)+pgeoh(jl,jk))*pmfu(jl,jk)
         pmfuq(jl,jk)=pqu(jl,jk)*pmfu(jl,jk)
      end if
     end do
      if(lmfdudv) then
        do jl=1,klon
        zdmfen(jl) = zdmfen(jl) + zoentr(jl,jk)
        zdmfde(jl) = zdmfde(jl) + zodetr(jl,jk)
           if(loflag(jl)) then
              if(ktype(jl).eq.1.or.ktype(jl).eq.3) then
                 if(zdmfen(jl).le.1.e-20) then
                    zz=3.
                 else
                    zz=2.
                 endif
              else
                 if(zdmfen(jl).le.1.0e-20) then
                    zz=1.
                 else
                    zz=0.
                 endif
              end if
              zdmfeu=zdmfen(jl)+zz*zdmfde(jl)
              zdmfdu=zdmfde(jl)+zz*zdmfde(jl)
              zdmfdu=min(zdmfdu,0.75*pmfu(jl,jk+1))
              zmfuu(jl)=zmfuu(jl)+ &
                       zdmfeu*puen(jl,jk)-zdmfdu*puu(jl,jk+1)
              zmfuv(jl)=zmfuv(jl)+ &
                       zdmfeu*pven(jl,jk)-zdmfdu*pvu(jl,jk+1)
              if(pmfu(jl,jk).gt.0.) then
                 puu(jl,jk)=zmfuu(jl)*(1./pmfu(jl,jk))
                 pvu(jl,jk)=zmfuv(jl)*(1./pmfu(jl,jk))
              end if
           end if
        end do
        end if
      do jl = 1, klon
       if (loflag(jl).and.ktype(jl).eq.1) then
        zbuoyz=g*((ptu(jl,jk)-ptenh(jl,jk))/ptenh(jl,jk)+ &
              0.608*(pqu(jl,jk)-pqenh(jl,jk))-plu(jl,jk))
        zbuoyz = max(zbuoyz,0.0)
        zdz = (pgeo(jl,jk-1)-pgeo(jl,jk))*zrg
        zdrodz = -log(pten(jl,jk-1)/pten(jl,jk))/zdz - &
                 g/(rd*ptenh(jl,jk))
        zbuoy(jl) = zbuoy(jl) + zbuoyz*zdz
        zoentr(jl,jk-1) = zbuoyz*0.5/(1.+zbuoy(jl))+zdrodz
        zoentr(jl,jk-1) = min(zoentr(jl,jk-1),1.e-3)
        zoentr(jl,jk-1) = max(zoentr(jl,jk-1),0.)
       end if
      end do
    end do
      do jl=1,klon
      if(kctop(jl).eq.klevm1) ldcum(jl)=.false.
      kcbot(jl)=max(kcbot(jl),kctop(jl))
      end do
      is=0
      do jl=1,klon
      if(ldcum(jl)) then
         is=is+1
      endif
      end do
      kcum=is
      if(is.eq.0) return
      do jl=1,klon
      if(ldcum(jl)) then
         jk=kctop(jl)-1
         zzdmf=cmfctop
         zdmfde(jl)=(1.-zzdmf)*pmfu(jl,jk+1)
         plude(jl,jk)=zdmfde(jl)*plu(jl,jk+1)
         pmfu(jl,jk)=pmfu(jl,jk+1)-zdmfde(jl)
         pmfus(jl,jk)=(cpd*ptu(jl,jk)+pgeoh(jl,jk))*pmfu(jl,jk)
         pmfuq(jl,jk)=pqu(jl,jk)*pmfu(jl,jk)
         pmful(jl,jk)=plu(jl,jk)*pmfu(jl,jk)
         plude(jl,jk-1)=pmful(jl,jk)
         pdmfup(jl,jk)=0.
      end if
      end do
      if(lmfdudv) then
         do jl=1,klon
         if(ldcum(jl)) then
            jk=kctop(jl)-1
            puu(jl,jk)=puu(jl,jk+1)
            pvu(jl,jk)=pvu(jl,jk+1)
         end if
         end do
      end if
      return
      end subroutine cuasc_new
      subroutine cudlfs &
         (klon, klev, klevp1, ptenh, pqenh, &
          puen, pven, pgeoh, paph, ptu, &
          pqu, puu, pvu, ldcum, kcbot, &
          kctop, pmfub, prfl, ptd, pqd, &
          pud, pvd, pmfd, pmfds, pmfdq, &
          pdmfdp, kdtop, lddraf)
      implicit none
      integer klon, klev, klevp1
      integer jl,ke,jk,is,ik,icall
      real zttest, zqtest, zbuo, zmftop
      real ptenh(klon,klev), pqenh(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              pgeoh(klon,klev), paph(klon,klevp1), &
              ptu(klon,klev), pqu(klon,klev), &
              puu(klon,klev), pvu(klon,klev), &
              pmfub(klon), prfl(klon)
      real ptd(klon,klev), pqd(klon,klev), &
              pud(klon,klev), pvd(klon,klev), &
              pmfd(klon,klev), pmfds(klon,klev), &
              pmfdq(klon,klev), pdmfdp(klon,klev)
      real ztenwb(klon,klev), zqenwb(klon,klev), &
              zcond(klon), zph(klon)
      integer kcbot(klon), kctop(klon), &
              kdtop(klon)
      logical ldcum(klon), llo2(klon), &
              lddraf(klon)
      do jl=1,klon
      lddraf(jl)=.false.
      kdtop(jl)=klevp1
      end do
      if(.not.lmfdd) return
      ke=klev-3
      do jk=3,ke
      is=0
      do jl=1,klon
      ztenwb(jl,jk)=ptenh(jl,jk)
      zqenwb(jl,jk)=pqenh(jl,jk)
      zph(jl)=paph(jl,jk)
      llo2(jl)=ldcum(jl).and.prfl(jl).gt.0..and..not.lddraf(jl).and. &
              (jk.lt.kcbot(jl).and.jk.gt.kctop(jl))
      if(llo2(jl))then
         is=is+1
      endif
      end do
      if(is.eq.0) cycle
      ik=jk
      icall=2
      call cuadjtq(klon,klev,ik,zph,ztenwb,zqenwb,llo2,icall)
      do jl=1,klon
      if(llo2(jl)) then
         zttest=0.5*(ptu(jl,jk)+ztenwb(jl,jk))
         zqtest=0.5*(pqu(jl,jk)+zqenwb(jl,jk))
         zbuo=zttest*(1.+vtmpc1*zqtest)- &
             ptenh(jl,jk)*(1.+vtmpc1*pqenh(jl,jk))
         zcond(jl)=pqenh(jl,jk)-zqenwb(jl,jk)
         zmftop=-cmfdeps*pmfub(jl)
         if(zbuo.lt.0..and.prfl(jl).gt.10.*zmftop*zcond(jl)) then
            kdtop(jl)=jk
            lddraf(jl)=.true.
            ptd(jl,jk)=zttest
            pqd(jl,jk)=zqtest
            pmfd(jl,jk)=zmftop
            pmfds(jl,jk)=pmfd(jl,jk)*(cpd*ptd(jl,jk)+pgeoh(jl,jk))
            pmfdq(jl,jk)=pmfd(jl,jk)*pqd(jl,jk)
            pdmfdp(jl,jk-1)=-0.5*pmfd(jl,jk)*zcond(jl)
            prfl(jl)=prfl(jl)+pdmfdp(jl,jk-1)
         end if
      end if
      end do
      if(lmfdudv) then
          do jl=1,klon
          if(pmfd(jl,jk).lt.0.) then
             pud(jl,jk)=0.5*(puu(jl,jk)+puen(jl,jk-1))
             pvd(jl,jk)=0.5*(pvu(jl,jk)+pven(jl,jk-1))
          end if
          end do
      end if
      end do
      return
      end subroutine cudlfs
      subroutine cuddraf &
         (klon, klev, klevp1, ptenh, pqenh, &
          puen, pven, pgeoh, paph, prfl, &
          lddraf, ptd, pqd, pud, pvd, &
          pmfd, pmfds, pmfdq, pdmfdp)
      implicit none
      integer klon, klev, klevp1
      integer jk,is,jl,itopde, ik, icall
      real zentr,zseen, zqeen, zsdde, zqdde,zmfdsk, zmfdqk
      real zbuo, zdmfdp, zmfduk, zmfdvk
      real ptenh(klon,klev), pqenh(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              pgeoh(klon,klev), paph(klon,klevp1)
      real ptd(klon,klev), pqd(klon,klev), &
              pud(klon,klev), pvd(klon,klev), &
              pmfd(klon,klev), pmfds(klon,klev), &
              pmfdq(klon,klev), pdmfdp(klon,klev), &
              prfl(klon)
      real zdmfen(klon), zdmfde(klon), &
              zcond(klon), zph(klon)
      logical lddraf(klon), llo2(klon)
      do jk=3,klev
      is=0
      do jl=1,klon
      zph(jl)=paph(jl,jk)
      llo2(jl)=lddraf(jl).and.pmfd(jl,jk-1).lt.0.
      if(llo2(jl)) then
         is=is+1
      endif
      end do
      if(is.eq.0) cycle
      do jl=1,klon
      if(llo2(jl)) then
         zentr=entrdd*pmfd(jl,jk-1)*rd*ptenh(jl,jk-1)/ &
              (g*paph(jl,jk-1))*(paph(jl,jk)-paph(jl,jk-1))
         zdmfen(jl)=zentr
         zdmfde(jl)=zentr
      end if
      end do
      itopde=klev-2
      if(jk.gt.itopde) then
         do jl=1,klon
         if(llo2(jl)) then
            zdmfen(jl)=0.
            zdmfde(jl)=pmfd(jl,itopde)* &
            (paph(jl,jk)-paph(jl,jk-1))/ &
            (paph(jl,klevp1)-paph(jl,itopde))
         end if
         end do
      end if
      do jl=1,klon
         if(llo2(jl)) then
            pmfd(jl,jk)=pmfd(jl,jk-1)+zdmfen(jl)-zdmfde(jl)
            zseen=(cpd*ptenh(jl,jk-1)+pgeoh(jl,jk-1))*zdmfen(jl)
            zqeen=pqenh(jl,jk-1)*zdmfen(jl)
            zsdde=(cpd*ptd(jl,jk-1)+pgeoh(jl,jk-1))*zdmfde(jl)
            zqdde=pqd(jl,jk-1)*zdmfde(jl)
            zmfdsk=pmfds(jl,jk-1)+zseen-zsdde
            zmfdqk=pmfdq(jl,jk-1)+zqeen-zqdde
            pqd(jl,jk)=zmfdqk*(1./min(-cmfcmin,pmfd(jl,jk)))
            ptd(jl,jk)=(zmfdsk*(1./min(-cmfcmin,pmfd(jl,jk)))- &
                       pgeoh(jl,jk))*rcpd
            ptd(jl,jk)=min(400.,ptd(jl,jk))
            ptd(jl,jk)=max(100.,ptd(jl,jk))
            zcond(jl)=pqd(jl,jk)
         end if
      end do
      ik=jk
      icall=2
      call cuadjtq(klon,klev,ik,zph,ptd,pqd,llo2,icall)
      do jl=1,klon
         if(llo2(jl)) then
            zcond(jl)=zcond(jl)-pqd(jl,jk)
            zbuo=ptd(jl,jk)*(1.+vtmpc1*pqd(jl,jk))- &
           ptenh(jl,jk)*(1.+vtmpc1*pqenh(jl,jk))
            if(zbuo.ge.0..or.prfl(jl).le.(pmfd(jl,jk)*zcond(jl))) then
               pmfd(jl,jk)=0.
            endif
            pmfds(jl,jk)=(cpd*ptd(jl,jk)+pgeoh(jl,jk))*pmfd(jl,jk)
            pmfdq(jl,jk)=pqd(jl,jk)*pmfd(jl,jk)
            zdmfdp=-pmfd(jl,jk)*zcond(jl)
            pdmfdp(jl,jk-1)=zdmfdp
            prfl(jl)=prfl(jl)+zdmfdp
         end if
      end do
      if(lmfdudv) then
          do jl=1,klon
             if(llo2(jl).and.pmfd(jl,jk).lt.0.) then
                zmfduk=pmfd(jl,jk-1)*pud(jl,jk-1)+ &
               zdmfen(jl)*puen(jl,jk-1)-zdmfde(jl)*pud(jl,jk-1)
                zmfdvk=pmfd(jl,jk-1)*pvd(jl,jk-1)+ &
               zdmfen(jl)*pven(jl,jk-1)-zdmfde(jl)*pvd(jl,jk-1)
                pud(jl,jk)=zmfduk*(1./min(-cmfcmin,pmfd(jl,jk)))
                pvd(jl,jk)=zmfdvk*(1./min(-cmfcmin,pmfd(jl,jk)))
             end if
          end do
       end if
      end do
      return
      end subroutine cuddraf
      subroutine cuflx &
         (klon, klev, klevp1, pqen, pqsen, &
          ptenh, pqenh, paph, pgeoh, kcbot, &
          kctop, kdtop, ktype, lddraf, ldcum, &
          pmfu, pmfd, pmfus, pmfds, pmfuq, &
          pmfdq, pmful, plude, pdmfup, pdmfdp, &
          prfl, prain, pten, psfl, pdpmel, &
          ktopm2, ztmst, sig1)
      implicit none
      integer klon, klev, klevp1
      integer ktopm2, itop, jl, jk, ikb
      real ztmst, zcons1, zcons2, zcucov, ztmelp2
      real zzp, zfac, zsnmlt, zrfl, cevapcu, zrnew
      real zrmin, zrfln, zdrfl, zdpevap
      real pqen(klon,klev), pqsen(klon,klev), &
              ptenh(klon,klev), pqenh(klon,klev), &
              paph(klon,klevp1), pgeoh(klon,klev)
      real pmfu(klon,klev), pmfd(klon,klev), &
              pmfus(klon,klev), pmfds(klon,klev), &
              pmfuq(klon,klev), pmfdq(klon,klev), &
              pdmfup(klon,klev), pdmfdp(klon,klev), &
              pmful(klon,klev), plude(klon,klev), &
              prfl(klon), prain(klon)
      real pten(klon,klev), pdpmel(klon,klev), &
              psfl(klon), zpsubcl(klon)
      real sig1(klev)
      integer kcbot(klon), kctop(klon), &
              kdtop(klon), ktype(klon)
      logical lddraf(klon), ldcum(klon)
      zcons1=cpd/(alf*g*ztmst)
      zcons2=1./(g*ztmst)
      zcucov=0.05
      ztmelp2=tmelt+2.
      itop=klev
      do jl=1,klon
      prfl(jl)=0.
      psfl(jl)=0.
      prain(jl)=0.
      if(.not.lmfscv.and.ktype(jl).eq.2)then
        ldcum(jl)=.false.
        lddraf(jl)=.false.
      endif
      itop=min(itop,kctop(jl))
      if(.not.ldcum(jl).or.kdtop(jl).lt.kctop(jl)) lddraf(jl)=.false.
      if(.not.ldcum(jl)) ktype(jl)=0
      end do
      ktopm2=itop-2
      do jk=ktopm2,klev
      do jl=1,klon
      if(ldcum(jl).and.jk.ge.kctop(jl)-1) then
         pmfus(jl,jk)=pmfus(jl,jk)-pmfu(jl,jk)* &
                     (cpd*ptenh(jl,jk)+pgeoh(jl,jk))
         pmfuq(jl,jk)=pmfuq(jl,jk)-pmfu(jl,jk)*pqenh(jl,jk)
         if(lddraf(jl).and.jk.ge.kdtop(jl)) then
            pmfds(jl,jk)=pmfds(jl,jk)-pmfd(jl,jk)* &
                        (cpd*ptenh(jl,jk)+pgeoh(jl,jk))
            pmfdq(jl,jk)=pmfdq(jl,jk)-pmfd(jl,jk)*pqenh(jl,jk)
         else
            pmfd(jl,jk)=0.
            pmfds(jl,jk)=0.
            pmfdq(jl,jk)=0.
            pdmfdp(jl,jk-1)=0.
         end if
      else
         pmfu(jl,jk)=0.
         pmfd(jl,jk)=0.
         pmfus(jl,jk)=0.
         pmfds(jl,jk)=0.
         pmfuq(jl,jk)=0.
         pmfdq(jl,jk)=0.
         pmful(jl,jk)=0.
         pdmfup(jl,jk-1)=0.
         pdmfdp(jl,jk-1)=0.
         plude(jl,jk-1)=0.
      end if
      end do
      end do
      do jk=ktopm2,klev
      do jl=1,klon
      if(ldcum(jl).and.jk.gt.kcbot(jl)) then
         ikb=kcbot(jl)
         zzp=((paph(jl,klevp1)-paph(jl,jk))/ &
             (paph(jl,klevp1)-paph(jl,ikb)))
         if(ktype(jl).eq.3) then
            zzp=zzp**2
         endif
         pmfu(jl,jk)=pmfu(jl,ikb)*zzp
         pmfus(jl,jk)=pmfus(jl,ikb)*zzp
         pmfuq(jl,jk)=pmfuq(jl,ikb)*zzp
         pmful(jl,jk)=pmful(jl,ikb)*zzp
      end if
      if(ldcum(jl)) then
         prain(jl)=prain(jl)+pdmfup(jl,jk)
         if(pten(jl,jk).gt.tmelt) then
            prfl(jl)=prfl(jl)+pdmfup(jl,jk)+pdmfdp(jl,jk)
            if(psfl(jl).gt.0..and.pten(jl,jk).gt.ztmelp2) then
               zfac=zcons1*(paph(jl,jk+1)-paph(jl,jk))
               zsnmlt=min(psfl(jl),zfac*(pten(jl,jk)-ztmelp2))
               pdpmel(jl,jk)=zsnmlt
               psfl(jl)=psfl(jl)-zsnmlt
               prfl(jl)=prfl(jl)+zsnmlt
            end if
         else
            psfl(jl)=psfl(jl)+pdmfup(jl,jk)+pdmfdp(jl,jk)
         end if
      end if
      end do
      end do
      do jl=1,klon
        prfl(jl)=max(prfl(jl),0.)
        psfl(jl)=max(psfl(jl),0.)
        zpsubcl(jl)=prfl(jl)+psfl(jl)
      end do
      do jk=ktopm2,klev
      do jl=1,klon
      if(ldcum(jl).and.jk.ge.kcbot(jl).and. &
             zpsubcl(jl).gt.1.e-20) then
          zrfl=zpsubcl(jl)
          cevapcu=cevapcu1*sqrt(cevapcu2*sqrt(sig1(jk)))
          zrnew=(max(0.,sqrt(zrfl/zcucov)- &
                  cevapcu*(paph(jl,jk+1)-paph(jl,jk))* &
                max(0.,pqsen(jl,jk)-pqen(jl,jk))))**2*zcucov
          zrmin=zrfl-zcucov*max(0.,0.8*pqsen(jl,jk)-pqen(jl,jk)) &
               *zcons2*(paph(jl,jk+1)-paph(jl,jk))
          zrnew=max(zrnew,zrmin)
          zrfln=max(zrnew,0.)
          zdrfl=min(0.,zrfln-zrfl)
          pdmfup(jl,jk)=pdmfup(jl,jk)+zdrfl
          zpsubcl(jl)=zrfln
      end if
      end do
      end do
      do jl=1,klon
        zdpevap=zpsubcl(jl)-(prfl(jl)+psfl(jl))
        prfl(jl)=prfl(jl)+zdpevap*prfl(jl)* &
                  (1./max(1.e-20,prfl(jl)+psfl(jl)))
        psfl(jl)=psfl(jl)+zdpevap*psfl(jl)* &
                  (1./max(1.e-20,prfl(jl)+psfl(jl)))
      end do
      return
      end subroutine cuflx
      subroutine cudtdq &
         (klon, klev, klevp1, ktopm2, paph, &
          ldcum, pten, ptte, pqte, pmfus, &
          pmfds, pmfuq, pmfdq, pmful, pdmfup, &
          pdmfdp, ztmst, pdpmel, prain, prfl, &
          psfl, psrain, psevap, psheat, psmelt, &
          prsfc, pssfc, paprc, paprsm, paprs, &
          pqen, pqsen, plude, pcte)
      implicit none
      integer klon, klev, klevp1
      integer ktopm2,jl, jk
      real ztmst, psrain, psevap, psheat, psmelt, zdiagt, zdiagw
      real zalv, rhk, rhcoe, pldfd, zdtdt, zdqdt
      real ptte(klon,klev), pqte(klon,klev), &
              pten(klon,klev), plude(klon,klev), &
              pgeo(klon,klev), paph(klon,klevp1), &
              paprc(klon), paprs(klon), &
              paprsm(klon), pcte(klon,klev), &
              prsfc(klon), pssfc(klon)
      real pmfus(klon,klev), pmfds(klon,klev), &
              pmfuq(klon,klev), pmfdq(klon,klev), &
              pmful(klon,klev), pqsen(klon,klev), &
              pdmfup(klon,klev), pdmfdp(klon,klev),&
              prfl(klon), prain(klon), &
              pqen(klon,klev)
      real pdpmel(klon,klev), psfl(klon)
      real zsheat(klon), zmelt(klon)
      logical ldcum(klon)
      zdiagt=ztmst
      zdiagw=zdiagt/rhoh2o
      do jl=1,klon
        zmelt(jl)=0.
        zsheat(jl)=0.
      end do
      do jk=ktopm2,klev
      if(jk.lt.klev) then
         do jl=1,klon
         if(ldcum(jl)) then
            if(pten(jl,jk).gt.tmelt) then
               zalv=alv
            else
               zalv=als
            endif
            rhk=min(1.0,pqen(jl,jk)/pqsen(jl,jk))
            rhcoe=max(0.0,(rhk-rhc)/(rhm-rhc))
            pldfd=max(0.0,rhcoe*fdbk*plude(jl,jk))
            zdtdt=(g/(paph(jl,jk+1)-paph(jl,jk)))*rcpd* &
              (pmfus(jl,jk+1)-pmfus(jl,jk)+ &
              pmfds(jl,jk+1)-pmfds(jl,jk)-alf*pdpmel(jl,jk) &
              -zalv*(pmful(jl,jk+1)-pmful(jl,jk)-pldfd- &
              (pdmfup(jl,jk)+pdmfdp(jl,jk))))
            ptte(jl,jk)=ptte(jl,jk)+zdtdt
            zdqdt=(g/(paph(jl,jk+1)-paph(jl,jk)))*&
              (pmfuq(jl,jk+1)-pmfuq(jl,jk)+ &
              pmfdq(jl,jk+1)-pmfdq(jl,jk)+ &
              pmful(jl,jk+1)-pmful(jl,jk)-pldfd- &
              (pdmfup(jl,jk)+pdmfdp(jl,jk)))
            pqte(jl,jk)=pqte(jl,jk)+zdqdt
            pcte(jl,jk)=(g/(paph(jl,jk+1)-paph(jl,jk)))*pldfd
            zsheat(jl)=zsheat(jl)+zalv*(pdmfup(jl,jk)+pdmfdp(jl,jk))
            zmelt(jl)=zmelt(jl)+pdpmel(jl,jk)
         end if
         end do
      else
         do jl=1,klon
         if(ldcum(jl)) then
            if(pten(jl,jk).gt.tmelt) then
               zalv=alv
            else
               zalv=als
            endif
            rhk=min(1.0,pqen(jl,jk)/pqsen(jl,jk))
            rhcoe=max(0.0,(rhk-rhc)/(rhm-rhc))
            pldfd=max(0.0,rhcoe*fdbk*plude(jl,jk))
            zdtdt=-(g/(paph(jl,jk+1)-paph(jl,jk)))*rcpd* &
                (pmfus(jl,jk)+pmfds(jl,jk)+alf*pdpmel(jl,jk)-zalv* &
                (pmful(jl,jk)+pdmfup(jl,jk)+pdmfdp(jl,jk)+pldfd))
            ptte(jl,jk)=ptte(jl,jk)+zdtdt
            zdqdt=-(g/(paph(jl,jk+1)-paph(jl,jk)))* &
                     (pmfuq(jl,jk)+pmfdq(jl,jk)+pldfd+ &
                     (pmful(jl,jk)+pdmfup(jl,jk)+pdmfdp(jl,jk)))
            pqte(jl,jk)=pqte(jl,jk)+zdqdt
            pcte(jl,jk)=(g/(paph(jl,jk+1)-paph(jl,jk)))*pldfd
            zsheat(jl)=zsheat(jl)+zalv*(pdmfup(jl,jk)+pdmfdp(jl,jk))
            zmelt(jl)=zmelt(jl)+pdpmel(jl,jk)
         end if
         end do
      end if
      end do
      do jl=1,klon
      prsfc(jl)=prfl(jl)
      pssfc(jl)=psfl(jl)
      paprc(jl)=paprc(jl)+zdiagw*(prfl(jl)+psfl(jl))
      paprs(jl)=paprsm(jl)+zdiagw*psfl(jl)
      psheat=psheat+zsheat(jl)
      psrain=psrain+prain(jl)
      psevap=psevap-(prfl(jl)+psfl(jl))
      psmelt=psmelt+zmelt(jl)
      end do
      psevap=psevap+psrain
      return
      end subroutine cudtdq
      subroutine cududv &
         (klon, klev, klevp1, ktopm2, ktype, &
          kcbot, paph, ldcum, puen, pven, &
          pvom, pvol, puu, pud, pvu, &
          pvd, pmfu, pmfd, psdiss)
      implicit none
      integer klon, klev, klevp1
      integer ktopm2, jk, ik, jl, ikb
      real psdiss,zzp, zdudt ,zdvdt, zsum
      real puen(klon,klev), pven(klon,klev), &
              pvol(klon,klev), pvom(klon,klev), &
              paph(klon,klevp1)
      real puu(klon,klev), pud(klon,klev), &
              pvu(klon,klev), pvd(klon,klev), &
              pmfu(klon,klev), pmfd(klon,klev)
      real zmfuu(klon,klev), zmfdu(klon,klev), &
              zmfuv(klon,klev), zmfdv(klon,klev), &
              zdiss(klon)
      integer ktype(klon), kcbot(klon)
      logical ldcum(klon)
      do jk=ktopm2,klev
      ik=jk-1
      do jl=1,klon
      if(ldcum(jl)) then
        zmfuu(jl,jk)=pmfu(jl,jk)*(puu(jl,jk)-puen(jl,ik))
        zmfuv(jl,jk)=pmfu(jl,jk)*(pvu(jl,jk)-pven(jl,ik))
        zmfdu(jl,jk)=pmfd(jl,jk)*(pud(jl,jk)-puen(jl,ik))
        zmfdv(jl,jk)=pmfd(jl,jk)*(pvd(jl,jk)-pven(jl,ik))
      end if
      end do
      end do
      do jk=ktopm2,klev
      do jl=1,klon
      if(ldcum(jl).and.jk.gt.kcbot(jl)) then
         ikb=kcbot(jl)
         zzp=((paph(jl,klevp1)-paph(jl,jk))/ &
             (paph(jl,klevp1)-paph(jl,ikb)))
         if(ktype(jl).eq.3) then
            zzp=zzp**2
         endif
         zmfuu(jl,jk)=zmfuu(jl,ikb)*zzp
         zmfuv(jl,jk)=zmfuv(jl,ikb)*zzp
         zmfdu(jl,jk)=zmfdu(jl,ikb)*zzp
         zmfdv(jl,jk)=zmfdv(jl,ikb)*zzp
      end if
      end do
      end do
      do jl=1,klon
      zdiss(jl)=0.
      end do
      do jk=ktopm2,klev
      if(jk.lt.klev) then
         do jl=1,klon
            if(ldcum(jl)) then
               zdudt=(g/(paph(jl,jk+1)-paph(jl,jk)))* &
                    (zmfuu(jl,jk+1)-zmfuu(jl,jk)+ &
                     zmfdu(jl,jk+1)-zmfdu(jl,jk))
               zdvdt=(g/(paph(jl,jk+1)-paph(jl,jk)))* &
                    (zmfuv(jl,jk+1)-zmfuv(jl,jk)+ &
                     zmfdv(jl,jk+1)-zmfdv(jl,jk))
               zdiss(jl)=zdiss(jl)+ &
                        puen(jl,jk)*(zmfuu(jl,jk+1)-zmfuu(jl,jk)+ &
                                     zmfdu(jl,jk+1)-zmfdu(jl,jk))+ &
                        pven(jl,jk)*(zmfuv(jl,jk+1)-zmfuv(jl,jk)+ &
                                     zmfdv(jl,jk+1)-zmfdv(jl,jk))
               pvom(jl,jk)=pvom(jl,jk)+zdudt
               pvol(jl,jk)=pvol(jl,jk)+zdvdt
            end if
         end do
      else
         do jl=1,klon
            if(ldcum(jl)) then
               zdudt=-(g/(paph(jl,jk+1)-paph(jl,jk)))* &
                        (zmfuu(jl,jk)+zmfdu(jl,jk))
               zdvdt=-(g/(paph(jl,jk+1)-paph(jl,jk)))* &
                        (zmfuv(jl,jk)+zmfdv(jl,jk))
               zdiss(jl)=zdiss(jl)- &
                         (puen(jl,jk)*(zmfuu(jl,jk)+zmfdu(jl,jk))+ &
                          pven(jl,jk)*(zmfuv(jl,jk)+zmfdv(jl,jk)))
               pvom(jl,jk)=pvom(jl,jk)+zdudt
               pvol(jl,jk)=pvol(jl,jk)+zdvdt
            end if
         end do
       end if
      end do
      zsum=ssum(klon,zdiss(1),1)
      psdiss=psdiss+zsum
      return
      end subroutine cududv
      subroutine cubasmc &
         (klon, klev, klevm1, kk, pten, &
          pqen, pqsen, puen, pven, pverv, &
          pgeo, pgeoh, ldcum, ktype, klab, &
          pmfu, pmfub, pentr, kcbot, ptu, &
          pqu, plu, puu, pvu, pmfus, &
          pmfuq, pmful, pdmfup, pmfuu, pmfuv)
      implicit none
      integer klon, klev, klevp1
      integer klevm1,kk, jl
      real zzzmb
      real pten(klon,klev), pqen(klon,klev), &
              puen(klon,klev), pven(klon,klev), &
              pqsen(klon,klev), pverv(klon,klev), &
              pgeo(klon,klev), pgeoh(klon,klev)
      real ptu(klon,klev), pqu(klon,klev), &
              puu(klon,klev), pvu(klon,klev), &
              plu(klon,klev), pmfu(klon,klev), &
              pmfub(klon), pentr(klon), &
              pmfus(klon,klev), pmfuq(klon,klev), &
              pmful(klon,klev), pdmfup(klon,klev), &
              pmfuu(klon), pmfuv(klon)
      integer ktype(klon), kcbot(klon), &
              klab(klon,klev)
      logical ldcum(klon)
         do jl=1,klon
          if( .not. ldcum(jl).and.klab(jl,kk+1).eq.0.0.and. &
             pqen(jl,kk).gt.0.80*pqsen(jl,kk)) then
            ptu(jl,kk+1)=(cpd*pten(jl,kk)+pgeo(jl,kk)-pgeoh(jl,kk+1)) &
                               *rcpd
            pqu(jl,kk+1)=pqen(jl,kk)
            plu(jl,kk+1)=0.
            zzzmb=max(cmfcmin,-pverv(jl,kk)/g)
            zzzmb=min(zzzmb,cmfcmax)
            pmfub(jl)=zzzmb
            pmfu(jl,kk+1)=pmfub(jl)
            pmfus(jl,kk+1)=pmfub(jl)*(cpd*ptu(jl,kk+1)+pgeoh(jl,kk+1))
            pmfuq(jl,kk+1)=pmfub(jl)*pqu(jl,kk+1)
            pmful(jl,kk+1)=0.
            pdmfup(jl,kk+1)=0.
            kcbot(jl)=kk
            klab(jl,kk+1)=1
            ktype(jl)=3
            pentr(jl)=entrmid
               if(lmfdudv) then
                  puu(jl,kk+1)=puen(jl,kk)
                  pvu(jl,kk+1)=pven(jl,kk)
                  pmfuu(jl)=pmfub(jl)*puu(jl,kk+1)
                  pmfuv(jl)=pmfub(jl)*pvu(jl,kk+1)
               end if
         end if
        end do
      return
      end subroutine cubasmc
      subroutine cuadjtq(klon,klev,kk,pp,pt,pq,ldflag,kcall)
      implicit none
      integer klon, klev
      integer kk, kcall, isum, jl
      real zqsat, zcor, zcond1, tt
      real pt(klon,klev), pq(klon,klev), &
              zcond(klon), zqp(klon), &
              pp(klon)
      logical ldflag(klon)
      if (kcall.eq.1 ) then
         isum=0
         do jl=1,klon
         zcond(jl)=0.
         if(ldflag(jl)) then
            zqp(jl)=1./pp(jl)
            tt=pt(jl,kk)
            zqsat=tlucua(tt)*zqp(jl)
            zqsat=min(0.5,zqsat)
            zcor=1./(1.-vtmpc1*zqsat)
            zqsat=zqsat*zcor
            zcond(jl)=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
            zcond(jl)=max(zcond(jl),0.)
            pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond(jl)
            pq(jl,kk)=pq(jl,kk)-zcond(jl)
            if(zcond(jl).ne.0.0) isum=isum+1
         end if
         end do
         if(isum.eq.0) return
         do jl=1,klon
         if(ldflag(jl).and.zcond(jl).ne.0.) then
            tt=pt(jl,kk)
            zqsat=tlucua(tt)*zqp(jl)
            zqsat=min(0.5,zqsat)
            zcor=1./(1.-vtmpc1*zqsat)
            zqsat=zqsat*zcor
            zcond1=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
            pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond1
            pq(jl,kk)=pq(jl,kk)-zcond1
         end if
         end do
      end if
      if(kcall.eq.2) then
         isum=0
         do jl=1,klon
         zcond(jl)=0.
         if(ldflag(jl)) then
            tt=pt(jl,kk)
            zqp(jl)=1./pp(jl)
            zqsat=tlucua(tt)*zqp(jl)
            zqsat=min(0.5,zqsat)
            zcor=1./(1.-vtmpc1*zqsat)
            zqsat=zqsat*zcor
            zcond(jl)=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
            zcond(jl)=min(zcond(jl),0.)
            pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond(jl)
            pq(jl,kk)=pq(jl,kk)-zcond(jl)
            if(zcond(jl).ne.0.0) isum=isum+1
         end if
         end do
         if(isum.eq.0) return
         do jl=1,klon
         if(ldflag(jl).and.zcond(jl).ne.0.) then
            tt=pt(jl,kk)
            zqsat=tlucua(tt)*zqp(jl)
            zqsat=min(0.5,zqsat)
            zcor=1./(1.-vtmpc1*zqsat)
            zqsat=zqsat*zcor
            zcond1=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
            pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond1
            pq(jl,kk)=pq(jl,kk)-zcond1
         end if
         end do
      end if
      if(kcall.eq.0) then
         isum=0
         do jl=1,klon
           tt=pt(jl,kk)
           zqp(jl)=1./pp(jl)
           zqsat=tlucua(tt)*zqp(jl)
           zqsat=min(0.5,zqsat)
           zcor=1./(1.-vtmpc1*zqsat)
           zqsat=zqsat*zcor
           zcond(jl)=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
           pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond(jl)
           pq(jl,kk)=pq(jl,kk)-zcond(jl)
           if(zcond(jl).ne.0.0) isum=isum+1
         end do
         if(isum.eq.0) return
         do jl=1,klon
           tt=pt(jl,kk)
           zqsat=tlucua(tt)*zqp(jl)
           zqsat=min(0.5,zqsat)
           zcor=1./(1.-vtmpc1*zqsat)
           zqsat=zqsat*zcor
           zcond1=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
           pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond1
           pq(jl,kk)=pq(jl,kk)-zcond1
         end do
      end if
      if(kcall.eq.4) then
         do jl=1,klon
           tt=pt(jl,kk)
           zqp(jl)=1./pp(jl)
           zqsat=tlucua(tt)*zqp(jl)
           zqsat=min(0.5,zqsat)
           zcor=1./(1.-vtmpc1*zqsat)
           zqsat=zqsat*zcor
           zcond(jl)=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
           pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond(jl)
           pq(jl,kk)=pq(jl,kk)-zcond(jl)
         end do
         do jl=1,klon
           tt=pt(jl,kk)
           zqsat=tlucua(tt)*zqp(jl)
           zqsat=min(0.5,zqsat)
           zcor=1./(1.-vtmpc1*zqsat)
           zqsat=zqsat*zcor
           zcond1=(pq(jl,kk)-zqsat)/(1.+zqsat*zcor*tlucub(tt))
           pt(jl,kk)=pt(jl,kk)+tlucuc(tt)*zcond1
           pq(jl,kk)=pq(jl,kk)-zcond1
         end do
      end if
      return
      end subroutine cuadjtq
      subroutine cuentr_new &
         (klon, klev, klevp1, kk, ptenh, &
          paph, pap, pgeoh, klwmin, ldcum, &
          ktype, kcbot, kctop0, zpbase, pmfu, &
          pentr, zdmfen, zdmfde, zodetr, khmin)
      implicit none
      integer klon, klev, klevp1
      integer kk, jl, iklwmin,ikb, ikt, ikh
      real zrrho, zdprho, zpmid, zentr, zzmzk, ztmzk, arg, zorgde
      real ptenh(klon,klev), &
              pap(klon,klev), paph(klon,klevp1), &
              pmfu(klon,klev), pgeoh(klon,klev), &
              pentr(klon), zpbase(klon), &
              zdmfen(klon), zdmfde(klon), &
              zodetr(klon,klev)
      integer klwmin(klon), ktype(klon), &
              kcbot(klon), kctop0(klon), &
              khmin(klon)
      logical ldcum(klon),llo1,llo2
      do jl = 1, klon
        zpbase(jl) = paph(jl,kcbot(jl))
        zrrho = (rd*ptenh(jl,kk+1))/paph(jl,kk+1)
        zdprho = (paph(jl,kk+1)-paph(jl,kk))*zrg
        zpmid = 0.5*(zpbase(jl)+paph(jl,kctop0(jl)))
        zentr = pentr(jl)*pmfu(jl,kk+1)*zdprho*zrrho
        llo1 = kk.lt.kcbot(jl).and.ldcum(jl)
        if(llo1) then
           zdmfde(jl) = zentr
        else
           zdmfde(jl) = 0.0
        endif
        llo2 = llo1.and.ktype(jl).eq.2.and.((zpbase(jl)-paph(jl,kk)) &
             .lt.zdnoprc.or.paph(jl,kk).gt.zpmid)
        if(llo2) then
            zdmfen(jl) = zentr
        else
            zdmfen(jl) = 0.0
        endif
        iklwmin = max(klwmin(jl),kctop0(jl)+2)
        llo2 = llo1.and.ktype(jl).eq.3.and.(kk.ge.iklwmin.or.pap(jl,kk) &
             .gt.zpmid)
        if (llo2) zdmfen(jl) = zentr
        llo2 = llo1.and.ktype(jl).eq.1
        if (llo2) zdmfen(jl) = zentr
        ikb = kcbot(jl)
        zodetr(jl,kk) = 0.
        if (llo2.and.kk.le.khmin(jl).and.kk.ge.kctop0(jl)) then
          ikt = kctop0(jl)
          ikh = khmin(jl)
          if (ikh.gt.ikt) then
            zzmzk = -(pgeoh(jl,ikh)-pgeoh(jl,kk))*zrg
            ztmzk = -(pgeoh(jl,ikh)-pgeoh(jl,ikt))*zrg
            arg = 3.1415*(zzmzk/ztmzk)*0.5
            zorgde = tan(arg)*3.1415*0.5/ztmzk
            zdprho = (paph(jl,kk+1)-paph(jl,kk))*(zrg*zrrho)
            zodetr(jl,kk) = min(zorgde,1.e-3)*pmfu(jl,kk+1)*zdprho
          end if
        end if
      enddo
      return
      end subroutine cuentr_new
      real function ssum ( n, x, ix )
      implicit none
      real x(*)
      real zsum
      integer n, ix, jx, jl
      jx = 1
      zsum = 0.0
      do jl = 1, n
        zsum = zsum + x(jx)
        jx = jx + ix
      enddo
      ssum=zsum
      return
      end function ssum
      real function tlucua(tt)
      implicit none
      real zcvm3,zcvm4,tt
      if(tt-tmelt.gt.0.) then
         zcvm3=c3les
         zcvm4=c4les
      else
         zcvm3=c3ies
         zcvm4=c4ies
      end if
      tlucua=c2es*exp(zcvm3*(tt-tmelt)*(1./(tt-zcvm4)))
      return
      end function tlucua
      real function tlucub(tt)
      implicit none
      real z5alvcp,z5alscp,zcvm4,zcvm5,tt
      z5alvcp=c5les*alv/cpd
      z5alscp=c5ies*als/cpd
      if(tt-tmelt.gt.0.) then
         zcvm4=c4les
         zcvm5=z5alvcp
      else
         zcvm4=c4ies
         zcvm5=z5alscp
      end if
      tlucub=zcvm5*(1./(tt-zcvm4))**2
      return
      end function tlucub
      real function tlucuc(tt)
      implicit none
      real zalvdcp,zalsdcp,tt,zldcp
      zalvdcp=alv/cpd
      zalsdcp=als/cpd
      if(tt-tmelt.gt.0.) then
         zldcp=zalvdcp
      else
         zldcp=zalsdcp
      end if
      tlucuc=zldcp
      return
      end function tlucuc
end module module_cu_tiedtke
