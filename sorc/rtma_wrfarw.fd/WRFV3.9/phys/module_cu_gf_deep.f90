MODULE module_cu_gf_deep
     real, parameter::g=9.81
     real, parameter:: cp=1004.
     real, parameter:: xlv=2.5e6
     real, parameter::r_v=461.
     real, parameter :: tcrit=258.
     real, parameter:: c1=0.002
     integer, parameter :: irainevap=0
     real, parameter::frh_thresh = .9
     real, parameter::rh_thresh = .97
     real, parameter::betajb=1.2
     integer, parameter:: use_excess=0
     real, parameter :: fluxtune=1.5
     real, parameter :: pgcd = 1.
     integer, parameter :: autoconv=1
     integer, parameter :: aeroevap=1
     real, parameter :: ccnclean=250.
     integer, parameter:: maxens3=16
 LOGICAL, PARAMETER :: MELT_GLAC = .True.
 real, parameter :: &
  T_0 = 273.16, &
  T_ice = 250.16, &
  xlf = 0.333e6
 real, parameter :: qrc_crit= 2.e-4
contains
   SUBROUTINE CUP_gf( &
               itf,ktf,its,ite, kts,kte &
              ,dicycle &
              ,ichoice &
              ,ipr &
              ,ccn &
              ,DTIME &
              ,imid &
              ,kpbl &
              ,dhdt &
              ,xland &
              ,zo &
              ,forcing &
              ,T &
              ,Q &
              ,Z1 &
              ,Tn &
              ,QO &
              ,PO &
              ,PSUR &
              ,US &
              ,VS &
              ,rho &
              ,hfx &
              ,qfx &
              ,dx &
              ,mconv &
              ,omeg &
              ,csum &
              ,cnvwt &
              ,zuo &
              ,zdo &
              ,zdm &
              ,edto &
              ,edtm &
              ,xmb_out &
              ,xmbm_in &
              ,xmbs_in &
              ,pre &
              ,outu &
              ,outv &
              ,outt &
              ,outq &
              ,outqc &
              ,kbcon &
              ,ktop &
              ,cupclw &
              ,ierr &
              ,ierrc &
              ,rand_mom &
              ,rand_vmas &
              ,rand_clos &
              ,nranflag &
              ,do_capsuppress,cap_suppress_j &
              ,alpha_sc,chem,outc,numc &
              ,k22 &
              ,jmin)
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        numc,nranflag,itf,ktf,its,ite, kts,kte,ipr,imid
     integer, intent (in ) :: &
        ichoice
     real, dimension (its:ite,4) &
        ,intent (in ) :: rand_clos
     real, dimension (its:ite) &
        ,intent (in ) :: rand_mom,rand_vmas
   INTEGER, INTENT(IN ) ,OPTIONAL :: do_capsuppress
   REAL, DIMENSION( its:ite ) :: cap_suppress_j
      real, dimension (its:ite,1:maxens3) :: xf_ens,pr_ens
     real, dimension (its:ite,kts:kte) &
        ,intent (inout ) :: &
        cnvwt,outu,outv,OUTT,OUTQ,OUTQC,cupclw
     real, dimension (its:ite,kts:kte,max(1,numc)) &
        ,intent (inout ) :: &
         chem,outc
     real, dimension (its:ite,kts:kte,max(1,numc)) :: &
         chem_cup,chem_up,chem_down,dellac,chem_c,chem_pw,chem_pwd
     real, dimension (its:ite,max(1,numc)) :: &
         chem_pwav,chem_psum
     real, dimension (max(1,numc)),intent (in) :: alpha_sc
     real, dimension (its:ite) &
        ,intent (inout ) :: &
        pre,xmb_out
     real, dimension (its:ite) &
        ,intent (in ) :: &
        hfx,qfx,xmbm_in,xmbs_in
     integer, dimension (its:ite) &
        ,intent (inout ) :: &
        kbcon,ktop
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kpbl
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        dhdt,rho,T,PO,US,VS,tn
     real, dimension (its:ite,kts:kte) &
        ,intent (inout ) :: &
        omeg
     real, dimension (its:ite,kts:kte) &
        ,intent (inout) :: &
         Q,QO,zuo,zdo,zdm
     real, dimension (its:ite) &
        ,intent (in ) :: &
        dx,ccn,Z1,PSUR,xland
     real, dimension (its:ite) &
        ,intent (inout ) :: &
        mconv
       real &
        ,intent (in ) :: &
        dtime
     real, dimension (its:ite,1) :: &
        xaa0_ens
     real, dimension (its:ite,1) :: &
        edtc
     real, dimension (its:ite,kts:kte,1) :: &
        dellat_ens,dellaqc_ens,dellaq_ens,pwo_ens
     real, dimension (its:ite,kts:kte) :: &
        entr_rate_2d,mentrd_rate_2d,he,hes,qes,z, heo,heso,qeso,zo, &
        xhe,xhes,xqes,xz,xt,xq,qes_cup,q_cup,he_cup,hes_cup,z_cup, &
        p_cup,gamma_cup,t_cup, qeso_cup,qo_cup,heo_cup,heso_cup, &
        zo_cup,po_cup,gammao_cup,tn_cup, &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup, &
        xt_cup, dby,hc,zu,clw_all, &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco, &
        dbyt,xdby,xhc,xzu,pwdper,c0, &
        cd,cdd,DELLAH,DELLAQ,DELLAT,DELLAQC, &
        u_cup,v_cup,uc,vc,ucd,vcd,dellu,dellv
     real, dimension (its:ite) :: &
       edt,edto,edtm,AA1,AA0,XAA0,HKB, &
       HKBO,XHKB, &
       XMB,PWAVO, &
       PWEVO,BU,BUD,cap_max, &
       cap_max_increment,closure_n,psum,psumh,sig,sigd
     real, dimension (its:ite) :: &
        axx,edtmax,edtmin,entr_rate
     integer, dimension (its:ite) :: &
       kzdown,KDET,K22,JMIN,kstabi,kstabm,K22x,xland1, &
       ktopdby,KBCONx,ierr2,ierr3,KBMAX
     integer, dimension (its:ite), intent(inout) :: ierr
     integer, dimension (its:ite), intent(in) :: csum
     integer :: &
       nv,iloop,nens3,ki,kk,I,K
     real :: &
      dz,dzo,mbdt,radius,trcc,alpha, &
      zcutdown,depth_min,zkbmax,z_detr,zktop, &
      dh,cap_maxs,trash,trash2,frh,sig_thresh
     real entdo,dp,subin,detdo,entup, &
      detup,subdown,entdoj,entupk,detupk,totmas
     real, dimension (its:ite) :: lambau,flux_tun,zws,ztexec,zqexec
     integer :: jprnt,jmini,start_k22
     logical :: keep_going,flg(its:ite)
     character*50 :: ierrc(its:ite)
     character*4 :: cumulus
     real, dimension (its:ite,kts:kte) :: &
       up_massentr,up_massdetr,c1d &
      ,up_massentro,up_massdetro,dd_massentro,dd_massdetro
     real, dimension (its:ite,kts:kte) :: &
       up_massentru,up_massdetru,dd_massentru,dd_massdetru
     real c1_max,buo_flux,pgcon,pgc,blqe
     real :: xff_mid(its:ite,2)
     integer :: iversion=1
     real :: denom,h_entr,umean,t_star,dq
     integer, intent(IN) :: DICYCLE
     real, dimension (its:ite) :: aa1_bl,hkbo_bl,tau_bl,tau_ecmwf,wmean
     real, dimension (its:ite,kts:kte) :: tn_bl, qo_bl, qeso_bl, heo_bl, heso_bl &
                                              ,qeso_cup_bl,qo_cup_bl, heo_cup_bl,heso_cup_bl &
                                              ,gammao_cup_bl,tn_cup_bl,hco_bl,DBYo_bl
     real, dimension(its:ite) :: xf_dicycle
     real, intent(inout), dimension(its:ite,10) :: forcing
     integer :: pmin_lev(its:ite),start_level(its:ite),ktopkeep(its:ite)
     real, dimension (its:ite,kts:kte) :: dtempdz
     integer, dimension (its:ite,kts:kte) :: k_inv_layers
     real zuh2(40)
     real, dimension (its:ite) :: rntot,delqev,delq2,qevap,rn,qcond
     real :: rain,t1,q1,elocp,evef,el2orc,evfact,evfactl,g_rain,e_dn,c_up
     real :: pgeoh,dts,fp,fpi,pmin,x_add,beta,beta_u
     real :: cbeg,cmid,cend,const_a,const_b,const_c
     real, dimension (its:ite,kts:kte) :: p_liq_ice,melting_layer,melting
      melting_layer(:,:)=0.
      melting(:,:)=0.
      flux_tun(:)=fluxtune
      cumulus='deep'
      if(imid.eq.1)cumulus='mid'
      pmin=150.
      if(imid.eq.1)pmin=75.
      ktopdby(:)=0
      c1_max=c1
      elocp=xlv/cp
      el2orc=xlv*xlv/(r_v*cp)
      evfact=.3
      evfactl=.3
       pgcon=0.
       lambau(:)=2.
       if(imid.eq.1)lambau(:)=2.
       if(nranflag == 1)then
           lambau(:)=1.5+rand_mom(:)
       endif
      ztexec(:) = 0.
      zqexec(:) = 0.
      zws(:) = 0.
      do i=its,itf
         buo_flux= (hfx(i)/cp+0.608*t(i,1)*qfx(i)/xlv)/rho(i,1)
         pgeoh = zo(i,2)*g
         zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,2)*g/t(i,1))
         if(zws(i) > TINY(pgeoh)) then
            zws(i) = 1.2*zws(i)**.3333
            ztexec(i) = MAX(flux_tun(i)*hfx(i)/(rho(i,1)*zws(i)*cp),0.0)
            zqexec(i) = MAX(flux_tun(i)*qfx(i)/xlv/(rho(i,1)*zws(i)),0.)
         endif
         zws(i) = max(0.,.001-flux_tun(i)*0.41*buo_flux*zo(i,kpbl(i))*g/t(i,kpbl(i)))
         zws(i) = 1.2*zws(i)**.3333
         zws(i) = zws(i)*rho(i,kpbl(i))
      enddo
      cap_maxs=75.
      do i=its,itf
        edto(i)=0.
        closure_n(i)=16.
        xmb_out(i)=0.
        cap_max(i)=cap_maxs
        cap_max_increment(i)=20.
        xland1(i)=int(xland(i)+.0001)
        if(xland(i).gt.1.5 .or. xland(i).lt.0.5)then
            xland1(i)=0
            cap_max_increment(i)=20.
        else
            if(ztexec(i).gt.0.)cap_max(i)=cap_max(i)+25.
            if(ztexec(i).lt.0.)cap_max(i)=cap_max(i)-25.
        endif
        ierrc(i)=" "
      enddo
      if(use_excess == 0 )then
       ztexec(:)=0
       zqexec(:)=0
      endif
  if(do_capsuppress == 1) then
      do i=its,itf
          cap_max(i)=cap_maxs
          if (abs(cap_suppress_j(i) - 1.0 ) < 0.1 ) then
             cap_max(i)=cap_maxs+75.
          elseif (abs(cap_suppress_j(i) - 0.0 ) < 0.1 ) then
             cap_max(i)=10.0
          endif
      enddo
  endif
      start_level(:)=kte
      do i=its,ite
         c1d(i,:)= 0.
         entr_rate(i)=7.e-5 - min(20.,float(csum(i))) * 3.e-6
         if(xland1(i) == 0)entr_rate(i)=7.e-5
         if(imid.eq.1)entr_rate(i)=3.e-4
         if(imid.eq.1)c1d(i,:)=c1
         radius=.2/entr_rate(i)
         frh=min(1.,3.14*radius*radius/dx(i)/dx(i))
         if(frh > frh_thresh)then
            frh=frh_thresh
            radius=sqrt(frh*dx(i)*dx(i)/3.14)
            entr_rate(i)=.2/radius
         endif
         sig(i)=(1.-frh)**2
      enddo
      sig_thresh = (1.-frh_thresh)**2
      do k=kts,ktf
      do i=its,itf
        cnvwt(i,k)=0.
        zuo(i,k)=0.
        zdo(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        cupclw(i,k)=0.
        cd(i,k)=.1*entr_rate(i)
        if(imid.eq.1)cd(i,k)=.5*entr_rate(i)
        cdd(i,k)=1.e-9
        hcdo(i,k)=0.
        qrcdo(i,k)=0.
        dellaqc(i,k)=0.
      enddo
      enddo
      edtmax(:)=1.
      if(imid.eq.1)edtmax(:)=.15
      edtmin(:)=.1
      if(imid.eq.1)edtmin(:)=.05
      depth_min=1000.
      if(imid.eq.1)depth_min=500.
      DO i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
        edt(i)=0.
        kstabm(i)=ktf-1
        IERR2(i)=0
        IERR3(i)=0
        x_add=0.
      enddo
      zkbmax=4000.
      if(imid.eq.1)zkbmax=2000.
      zcutdown=4000.
      z_detr=500.
      do i=its,itf
            do k=1,maxens3
               xf_ens(i,k)=0.
               pr_ens(i,k)=0.
            enddo
      enddo
      call cup_env(z,qes,he,hes,t,q,po,z1, &
           psur,ierr,tcrit,-1, &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,-1, &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env_clev(t,qes,q,he,hes,z,po,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1, &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur, &
           ierr,z1, &
           itf,ktf, &
           its,ite, kts,kte)
      call get_partition_liq_ice(ierr,tn,po_cup,p_liq_ice,melting_layer,&
                                 itf,ktf,its,ite,kts,kte,cumulus)
      do i=its,itf
        if(ierr(i).eq.0)then
          if(kpbl(i).gt.5 .and. imid.eq.1)cap_max(i)=po_cup(i,kpbl(i))
          u_cup(i,kts)=us(i,kts)
          v_cup(i,kts)=vs(i,kts)
          do k=kts+1,ktf
           u_cup(i,k)=.5*(us(i,k-1)+us(i,k))
           v_cup(i,k)=.5*(vs(i,k-1)+vs(i,k))
          enddo
        endif
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        do k=kts,ktf
          if(zo_cup(i,k).gt.zkbmax+z1(i))then
            kbmax(i)=k
            go to 25
          endif
        enddo
 25 continue
        do k=kts,ktf
          if(zo_cup(i,k).gt.z_detr+z1(i))then
            kdet(i)=k
            go to 26
          endif
        enddo
 26 continue
        endif
      enddo
      start_k22=2
       DO 36 i=its,itf
         IF(ierr(I).eq.0)THEN
            k22(i)=maxloc(HEO_CUP(i,start_k22:kbmax(i)+2),1)+start_k22-1
            if(K22(I).GE.KBMAX(i))then
             ierr(i)=2
             ierrc(i)="could not find k22"
             ktop(i)=0
             k22(i)=0
             kbcon(i)=0
           endif
         endif
 36 CONTINUE
      do i=its,itf
       IF(ierr(I).eq.0)THEN
         x_add = xlv*zqexec(i)+cp*ztexec(i)
         call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
         call get_cloud_bc(kte,heo_cup (i,1:kte),hkbo (i),k22(i),x_add)
       endif
      enddo
      jprnt=0
      iloop=1
      if(imid.eq.1)iloop=5
      call cup_kbcon(ierrc,cap_max_increment,iloop,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           ztexec,zqexec, &
           jprnt,itf,ktf, &
           its,ite, kts,kte, &
           z_cup,entr_rate,heo,imid)
      CALL cup_minimi(HEso_cup,Kbcon,kstabm,kstabi,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      DO i=its,itf
         IF(ierr(I) == 0)THEN
           frh = min(qo_cup(i,kbcon(i))/qeso_cup(i,kbcon(i)),1.)
           if(frh >= rh_thresh .and. sig(i) <= sig_thresh )then
             ierr(i)=231
             cycle
           endif
           x_add=0.
           do k=kbcon(i)+1,ktf
             if(po(i,kbcon(i))-po(i,k) > pmin+x_add)then
                pmin_lev(i)=k
                exit
             endif
           enddo
            start_level(i)=k22(i)
            x_add = xlv*zqexec(i)+cp*ztexec(i)
            call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
         endif
      enddo
      if(imid.eq.1)then
      call get_inversion_layers(ierr,p_cup,t_cup,z_cup,q_cup,qes_cup,k_inv_layers, &
                               kbcon,kstabi,dtempdz,itf,ktf,its,ite, kts,kte)
      endif
      DO i=its,itf
         if(kstabi(i).lt.kbcon(i))then
           kbcon(i)=1
           ierr(i)=42
         endif
         do k=kts,ktf
            entr_rate_2d(i,k)=entr_rate(i)
         enddo
         IF(ierr(I).eq.0)THEN
            kbcon(i)=max(2,kbcon(i))
            do k=kts,ktf
               frh = min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate(i) *(1.3-frh)
            enddo
            if(imid.eq.1)then
                if(k_inv_layers(i,2).gt.0 .and. &
                  (po_cup(i,k22(i))-po_cup(i,k_inv_layers(i,2))).lt.500.)then
                 ktop(i)=min(kstabi(i),k_inv_layers(i,2))
                 ktopdby(i)=ktop(i)
               else
                 do k=kbcon(i)+1,ktf
                  if((po_cup(i,k22(i))-po_cup(i,k)).gt.500.)then
                    ktop(i)=k
                    ktopdby(i)=ktop(i)
                    exit
                  endif
                 enddo
               endif
            endif
          endif
      ENDDO
      i=0
      if(imid.eq.1)then
          call rates_up_pdf(rand_vmas,ipr,'mid',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
                            xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopdby,csum,pmin_lev)
      else
          call rates_up_pdf(rand_vmas,ipr,'deep',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
                            xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kbcon,ktopdby,csum,pmin_lev)
      endif
      do i=its,itf
       if(ierr(i).eq.0)then
         if(k22(i).gt.1)then
            do k=1,k22(i) -1
              zuo(i,k)=0.
              zu (i,k)=0.
              xzu(i,k)=0.
            enddo
         endif
         do k=k22(i),ktop(i)
          xzu(i,k)= zuo(i,k)
          zu (i,k)= zuo(i,k)
         enddo
         do k=ktop(i)+1,kte
           zuo(i,k)=0.
           zu (i,k)=0.
           xzu(i,k)=0.
         enddo
        endif
      enddo
      CALL get_lateral_massflux(itf,ktf, its,ite, kts,kte &
                                ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d &
                                ,up_massentro, up_massdetro ,up_massentr, up_massdetr &
                                ,'deep',kbcon,k22,up_massentru,up_massdetru,lambau)
      do k=kts,ktf
       do i=its,itf
         uc (i,k)=0.
         vc (i,k)=0.
         hc (i,k)=0.
         dby (i,k)=0.
         hco (i,k)=0.
         dbyo(i,k)=0.
       enddo
      enddo
      do i=its,itf
       IF(ierr(I).eq.0)THEN
         do k=1,start_level(i)
            uc(i,k)=u_cup(i,k)
            vc(i,k)=v_cup(i,k)
         enddo
         do k=1,start_level(i)-1
            hc (i,k)=he_cup(i,k)
            hco(i,k)=heo_cup(i,k)
         enddo
         k=start_level(i)
         hc (i,k)=hkb(i)
         hco(i,k)=hkbo(i)
       ENDIF
      enddo
      DO i=its,itf
       ktopkeep(i)=0
       dbyt(i,:)=0.
       if(ierr(i) /= 0) cycle
       ktopkeep(i)=ktop(i)
       DO k=start_level(i) +1,ktop(i)
          denom=zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1)
          if(denom.lt.1.e-8)then
           ierr(i)=51
           exit
          endif
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                                             up_massentro(i,k-1)*heo(i,k-1)) / &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
       ENDDO
       kk=maxloc(dbyt(i,:),1)
       ki=maxloc(zuo(i,:),1)
       do k=ktop(i)-1,kbcon(i),-1
           if(dbyo(i,k).gt.0.)then
              ktopkeep(i)=k+1
              exit
           endif
        enddo
        ktop(I)=ktopkeep(i)
        if(ierr(i).eq.0)ktop(I)=ktopkeep(i)
      ENDDO
      DO i=its,itf
       if(ierr(i) /= 0) cycle
         do k=kbcon(i)+1,ktop(i)-1
           c1d(i,k)=c1
         enddo
       do k=ktop(i)+1,ktf
           HCo(i,K)=heso_cup(i,k)
           DBYo(I,K)=0.
       enddo
      ENDDO
      if(imid.eq.1)then
        call cup_up_moisture('mid',ierr,zo_cup,qco,qrco,pwo,pwavo, &
             p_cup,kbcon,ktop,dbyo,clw_all,xland1, &
             qo,GAMMAo_cup,zuo,qeso_cup,k22,qo_cup,c0, &
             ZQEXEC,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh, &
             1,itf,ktf, &
             its,ite, kts,kte)
      else
         call cup_up_moisture('deep',ierr,zo_cup,qco,qrco,pwo,pwavo, &
             p_cup,kbcon,ktop,dbyo,clw_all,xland1, &
             qo,GAMMAo_cup,zuo,qeso_cup,k22,qo_cup,c0, &
             ZQEXEC,ccn,rho,c1d,tn_cup,up_massentr,up_massdetr,psum,psumh, &
             1,itf,ktf, &
             its,ite, kts,kte)
     endif
      DO i=its,itf
       ktopkeep(i)=0
       dbyt(i,:)=0.
       if(ierr(i) /= 0) cycle
       ktopkeep(i)=ktop(i)
       DO k=start_level(i) +1,ktop(i)
          denom=zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1)
          if(denom.lt.1.e-8)then
           ierr(i)=51
           exit
          endif
          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                                          up_massentr(i,k-1)*he(i,k-1)) / &
                            (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          uc(i,k)=(uc(i,k-1)*zu(i,k-1)-.5*up_massdetru(i,k-1)*uc(i,k-1)+ &
                                          up_massentru(i,k-1)*us(i,k-1) &
                            -pgcon*.5*(zu(i,k)+zu(i,k-1))*(u_cup(i,k)-u_cup(i,k-1))) / &
                           (zu(i,k-1)-.5*up_massdetru(i,k-1)+up_massentru(i,k-1))
          vc(i,k)=(vc(i,k-1)*zu(i,k-1)-.5*up_massdetru(i,k-1)*vc(i,k-1)+ &
                                          up_massentru(i,k-1)*vs(i,k-1) &
                         -pgcon*.5*(zu(i,k)+zu(i,k-1))*(v_cup(i,k)-v_cup(i,k-1))) / &
                         (zu(i,k-1)-.5*up_massdetru(i,k-1)+up_massentru(i,k-1))
          dby(i,k)=hc(i,k)-hes_cup(i,k)
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                                             up_massentro(i,k-1)*heo(i,k-1)) / &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          hc (i,k)= hc (i,k)+(1.-p_liq_ice(i,k))*qrco(i,k)*xlf
          hco(i,k)= hco(i,k)+(1.-p_liq_ice(i,k))*qrco(i,k)*xlf
          dby(i,k)=hc(i,k)-hes_cup(i,k)
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
          DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
          dbyt(i,k)=dbyt(i,k-1)+dbyo(i,k)*dz
       ENDDO
       kk=maxloc(dbyt(i,:),1)
       ki=maxloc(zuo(i,:),1)
        do k=ktop(i)-1,kbcon(i),-1
           if(dbyo(i,k).gt.0.)then
              ktopkeep(i)=k+1
              exit
           endif
        enddo
        ktop(I)=ktopkeep(i)
        if(ierr(i).eq.0)ktop(I)=ktopkeep(i)
      ENDDO
41 continue
      DO i=its,itf
       if(ierr(i) /= 0) cycle
       do k=ktop(i)+1,ktf
           HC(i,K)=hes_cup(i,k)
           UC(i,K)=u_cup(i,k)
           VC(i,K)=v_cup(i,k)
           HCo(i,K)=heso_cup(i,k)
           DBY(I,K)=0.
           DBYo(I,K)=0.
           zu(i,k)=0.
           zuo(i,k)=0.
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentr(i,k)=0.
           up_massdetr(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
       enddo
      ENDDO
      DO i=its,itf
        if(ierr(i)/=0)cycle
        if(ktop(i).lt.kbcon(i)+2)then
              ierr(i)=5
              ierrc(i)='ktop too small deep'
              ktop(i)=0
        endif
      ENDDO
      DO 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(zo_cup(i,ktop(i))-z1(i))*.6
            if(imid.eq.1)zktop=(zo_cup(i,ktop(i))-z1(i))*.4
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,ktf
              if(zo_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 kzdown(i)=min(kzdown(i),kstabi(i)-1)
                 go to 37
              endif
              enddo
         endif
 37 CONTINUE
      call cup_minimi(HEso_cup,K22,kzdown,JMIN,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      DO 100 i=its,itf
         IF(ierr(I).eq.0)THEN
         jmini = jmin(i)
         keep_going = .TRUE.
         do while ( keep_going )
           keep_going = .FALSE.
           if ( jmini - 1 .lt. kdet(i) ) kdet(i) = jmini-1
           if ( jmini .ge. ktop(i)-1 ) jmini = ktop(i) - 2
           ki = jmini
           hcdo(i,ki)=heso_cup(i,ki)
           DZ=Zo_cup(i,Ki+1)-Zo_cup(i,Ki)
           dh=0.
           do k=ki-1,1,-1
             hcdo(i,k)=heso_cup(i,jmini)
             DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
             dh=dh+dz*(HCDo(i,K)-heso_cup(i,k))
             if(dh.gt.0.)then
               jmini=jmini-1
               if ( jmini .gt. 5 ) then
                 keep_going = .TRUE.
               else
                 ierr(i) = 9
                 ierrc(i) = "could not find jmini9"
                 exit
               endif
             endif
           enddo
         enddo
         jmin(i) = jmini
         if ( jmini .le. 5 ) then
           ierr(i)=4
           ierrc(i) = "could not find jmini4"
         endif
       ENDIF
100 continue
      do i=its,itf
         IF(ierr(I).eq.0)THEN
            if ( jmin(i) - 1 .lt. kdet(i) ) kdet(i) = jmin(i)-1
            IF(-zo_cup(I,KBCON(I))+zo_cup(I,KTOP(I)).LT.depth_min)then
               ierr(i)=6
               ierrc(i)="cloud depth very shallow"
            endif
         endif
      enddo
      do k=kts,ktf
      do i=its,itf
       zdo(i,k)=0.
       cdd(i,k)=0.
       dd_massentro(i,k)=0.
       dd_massdetro(i,k)=0.
       dd_massentru(i,k)=0.
       dd_massdetru(i,k)=0.
       hcdo(i,k)=heso_cup(i,k)
       ucd(i,k)=u_cup(i,k)
       vcd(i,k)=v_cup(i,k)
       dbydo(i,k)=0.
       mentrd_rate_2d(i,k)=entr_rate(i)
      enddo
      enddo
      do i=its,itf
        if(ierr(i)/=0)cycle
        beta=max(.025,.055-float(csum(i))*.0015)
        if(imid.eq.0 .and. xland1(i) == 0)then
              edtmax(i)=max(0.1,.4-float(csum(i))*.015)
        endif
        if(imid.eq.1)beta=.025
        bud(i)=0.
        cdd(i,1:jmin(i))=.1*entr_rate(i)
        cdd(i,jmin(i))=0.
        dd_massdetro(i,:)=0.
        dd_massentro(i,:)=0.
        call get_zu_zd_pdf_fim(0,po_cup(i,:),rand_vmas(i),0.,ipr,xland1(i),zuh2,"DOWN",ierr(i),kdet(i), &
         jmin(i)+1,zdo(i,:),kts,kte,ktf,beta,kpbl(i),csum(i),pmin_lev(i))
        if(zdo(i,jmin(i)) .lt.1.e-8)then
          zdo(i,jmin(i))=0.
          jmin(i)=jmin(i)-1
          cdd(i,jmin(i):ktf)=0.
          zdo(i,jmin(i)+1:ktf)=0.
          if(zdo(i,jmin(i)) .lt.1.e-8)then
             ierr(i)=876
             cycle
          endif
        endif
        do ki=jmin(i) ,maxloc(zdo(i,:),1),-1
          dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
          dd_massdetro(i,ki)=cdd(i,ki)*dzo*zdo(i,ki+1)
          dd_massentro(i,ki)=zdo(i,ki)-zdo(i,ki+1)+dd_massdetro(i,ki)
          if(dd_massentro(i,ki).lt.0.)then
             dd_massentro(i,ki)=0.
             dd_massdetro(i,ki)=zdo(i,ki+1)-zdo(i,ki)
             if(zdo(i,ki+1).gt.0.)cdd(i,ki)=dd_massdetro(i,ki)/(dzo*zdo(i,ki+1))
          endif
          if(zdo(i,ki+1).gt.0.)mentrd_rate_2d(i,ki)=dd_massentro(i,ki)/(dzo*zdo(i,ki+1))
        enddo
        mentrd_rate_2d(i,1)=0.
        do ki=maxloc(zdo(i,:),1)-1,1,-1
          dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
          dd_massentro(i,ki)=mentrd_rate_2d(i,ki)*dzo*zdo(i,ki+1)
          dd_massdetro(i,ki) = zdo(i,ki+1)+dd_massentro(i,ki)-zdo(i,ki)
          if(dd_massdetro(i,ki).lt.0.)then
            dd_massdetro(i,ki)=0.
            dd_massentro(i,ki)=zdo(i,ki)-zdo(i,ki+1)
            if(zdo(i,ki+1).gt.0.)mentrd_rate_2d(i,ki)=dd_massentro(i,ki)/(dzo*zdo(i,ki+1))
          endif
          if(zdo(i,ki+1).gt.0.)cdd(i,ki)= dd_massdetro(i,ki)/(dzo*zdo(i,ki+1))
        enddo
          do k=2,jmin(i)+1
           dd_massentru(i,k-1)=dd_massentro(i,k-1)+lambau(i)*dd_massdetro(i,k-1)
           dd_massdetru(i,k-1)=dd_massdetro(i,k-1)+lambau(i)*dd_massdetro(i,k-1)
          enddo
            dbydo(i,jmin(i))=hcdo(i,jmin(i))-heso_cup(i,jmin(i))
            bud(i)=dbydo(i,jmin(i))*(zo_cup(i,jmin(i)+1)-zo_cup(i,jmin(i)))
            ucd(i,jmin(i)+1)=.5*(uc(i,jmin(i)+1)+u_cup(i,jmin(i)+1))
            do ki=jmin(i) ,1,-1
             dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
             h_entr=.5*(heo(i,ki)+.5*(hco(i,ki)+hco(i,ki+1)))
             ucd(i,ki)=(ucd(i,ki+1)*zdo(i,ki+1) &
                         -.5*dd_massdetru(i,ki)*ucd(i,ki+1)+ &
                        dd_massentru(i,ki)*us(i,ki) &
                        -pgcon*zdo(i,ki+1)*(us(i,ki+1)-us(i,ki))) / &
                        (zdo(i,ki+1)-.5*dd_massdetru(i,ki)+dd_massentru(i,ki))
             vcd(i,ki)=(vcd(i,ki+1)*zdo(i,ki+1) &
                         -.5*dd_massdetru(i,ki)*vcd(i,ki+1)+ &
                        dd_massentru(i,ki)*vs(i,ki) &
                        -pgcon*zdo(i,ki+1)*(vs(i,ki+1)-vs(i,ki))) / &
                        (zdo(i,ki+1)-.5*dd_massdetru(i,ki)+dd_massentru(i,ki))
             hcdo(i,ki)=(hcdo(i,ki+1)*zdo(i,ki+1) &
                         -.5*dd_massdetro(i,ki)*hcdo(i,ki+1)+ &
                        dd_massentro(i,ki)*h_entr) / &
                        (zdo(i,ki+1)-.5*dd_massdetro(i,ki)+dd_massentro(i,ki))
             dbydo(i,ki)=hcdo(i,ki)-heso_cup(i,ki)
             bud(i)=bud(i)+dbydo(i,ki)*dzo
            enddo
        if(bud(i).gt.0)then
          ierr(i)=7
          ierrc(i)='downdraft is not negatively buoyant '
        endif
      enddo
      call cup_dd_moisture(ierrc,zdo,hcdo,heso_cup,qcdo,qeso_cup, &
           pwdo,qo_cup,zo_cup,dd_massentro,dd_massdetro,jmin,ierr,gammao_cup, &
           pwevo,bu,qrcdo,qo,heo,1, &
           itf,ktf, &
           its,ite, kts,kte)
      do i=its,itf
        if(ierr(i)/=0)cycle
        do k=kts+1,ktop(i)
          dp=100.*(po_cup(i,1)-po_cup(i,2))
          cupclw(i,k)=qrco(i,k)
          cnvwt(i,k)=zuo(i,k)*cupclw(i,k)*g/dp
        enddo
      enddo
      call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
           kbcon,ktop,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
           kbcon,ktop,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      do i=its,itf
        if(ierr(i)/=0)cycle
           if(aa1(i).eq.0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
      enddo
      aa1_bl (:) = 0.0
      xf_dicycle (:) = 0.0
      tau_ecmwf (:) = 0.
      iversion=1
      DO i=its,itf
            if(ierr(i).eq.0)then
                wmean(i) = 3.0
                if(imid.eq.1)wmean(i) = 3.0
                tau_ecmwf(i)=( zo_cup(i,ktopdby(i))- zo_cup(i,kbcon(i)) ) / wmean(i)
                tau_ecmwf(i)=max(tau_ecmwf(i),720.)
                tau_ecmwf(i)= tau_ecmwf(i) * (1.0061 + 1.23E-2 * (dx(i)/1000.))
            endif
      enddo
      tau_bl(:) = 0.
      IF(dicycle == 1) then
        DO i=its,itf
            if(ierr(i).eq.0)then
                if(xland1(i) == 0 ) then
                  umean= 2.0+sqrt(0.5*(US(i,1)**2+VS(i,1)**2+US(i,kbcon(i))**2+VS(i,kbcon(i))**2))
                  tau_bl(i) = (zo_cup(i,kbcon(i))- z1(i)) /umean
                else
                  tau_bl(i) =( zo_cup(i,ktopdby(i))- zo_cup(i,kbcon(i)) ) / wmean(i)
                endif
            endif
        ENDDO
        if(iversion == 1) then
        t_star=40.
            call cup_up_aa1bl(aa1_bl,t,tn,q,qo,dtime, &
                              zo_cup,zuo,dbyo_bl,GAMMAo_CUP_bl,tn_cup_bl, &
                              kbcon,ktop,ierr, &
                              itf,ktf,its,ite, kts,kte)
            DO i=its,itf
            if(ierr(i).eq.0)then
                  aa1_bl(i) = ( aa1_bl(i)/t_star)* tau_bl(i)
            endif
            ENDDO
        else
          DO i=its,itf
           tn_bl(i,:)=0.;qo_bl(i,:)=0.
           if ( ierr(i) == 0 )then
            tn_bl(i,1:kbcon(i)) = tn(i,1:kbcon(i))
            qo_bl(i,1:kbcon(i)) = qo(i,1:kbcon(i))
            tn_bl(i,kbcon(i)+1:ktf) = t(i,kbcon(i)+1:ktf)
            qo_bl(i,kbcon(i)+1:ktf) = q(i,kbcon(i)+1:ktf)
           endif
          ENDDO
          call cup_env(zo,qeso_bl,heo_bl,heso_bl,tn_bl,qo_bl,po,z1, &
                     psur,ierr,tcrit,-1, &
                     itf,ktf, its,ite, kts,kte)
          call cup_env_clev(tn_bl,qeso_bl,qo_bl,heo_bl,heso_bl,zo,po,qeso_cup_bl,qo_cup_bl, &
                              heo_cup_bl,heso_cup_bl,zo_cup,po_cup,gammao_cup_bl,tn_cup_bl,psur, &
                              ierr,z1, &
                              itf,ktf,its,ite, kts,kte)
          DO i=its,itf
            IF(ierr(I).eq.0)THEN
               hkbo_bl(i)=heo_cup_bl(i,k22(i))
            endif
          ENDDO
          DO k=kts,ktf
           do i=its,itf
             hco_bl (i,k)=0.
             DBYo_bl(i,k)=0.
           enddo
          ENDDO
          DO i=its,itf
            IF(ierr(I).eq.0)THEN
             do k=1,kbcon(i)-1
              hco_bl(i,k)=hkbo_bl(i)
             enddo
             k=kbcon(i)
             hco_bl (i,k)=hkbo_bl(i)
             DBYo_bl(i,k)=Hkbo_bl(i) - HESo_cup_bl(i,k)
            ENDIF
          ENDDO
          DO i=its,itf
            if(ierr(i).eq.0)then
               do k=kbcon(i)+1,ktop(i)
                    hco_bl(i,k)=(hco_bl(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco_bl(i,k-1)+ &
                               up_massentro(i,k-1)*heo_bl(i,k-1)) / &
                               (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
                    dbyo_bl(i,k)=hco_bl(i,k)-heso_cup_bl(i,k)
               enddo
               do k=ktop(i)+1,ktf
                  hco_bl (i,k)=heso_cup_bl(i,k)
                  dbyo_bl(i,k)=0.0
               enddo
            endif
          ENDDO
          call cup_up_aa0(aa1_bl,zo,zuo,dbyo_bl,GAMMAo_CUP_bl,tn_cup_bl, &
                        kbcon,ktop,ierr, &
                        itf,ktf,its,ite, kts,kte)
          DO i=its,itf
            if(ierr(i).eq.0)then
                aa1_bl(i) = aa1_bl(i) - aa0(i)
                   aa1_bl(i) = aa1_bl(i)* tau_bl(i)/ dtime
                print*,'aa0,aa1bl=',aa0(i),aa1_bl(i),aa0(i)-aa1_bl(i),tau_bl(i)
            endif
           ENDDO
        ENDIF
     ENDIF
       axx(:)=aa1(:)
      call cup_dd_edt(ierr,us,vs,zo,ktop,kbcon,edt,po,pwavo, &
           pwo,ccn,pwevo,edtmax,edtmin,edtc,psum,psumh, &
           rho,aeroevap,itf,ktf, &
           its,ite, kts,kte)
           chem_pwav(:,:)=0.
           chem_pw(:,:,:)=0.
           chem_pwd(:,:,:)=0.
           pwdper(:,:)=0.
           chem_down(:,:,:)=0.
           chem_up(:,:,:)=0.
           if(numc.gt.0)then
             do i=its,itf
               if(ierr(i).eq.0)then
                do k=kts,jmin(i)
                 pwdper(i,k)=-edtc(i,1)*pwdo(i,k)/pwavo(i)
                enddo
              do nv=1,numc
                 alpha=alpha_sc(nv)
                do k=kts+1,ktf
                   chem_cup(i,k,nv)=.5*(chem(i,k-1,nv)+chem(i,k,nv))
                enddo
                chem_cup(i,kts,nv)=chem(i,kts,nv)
                do k=1,k22(i)
                   chem_up(i,k,nv)=chem_cup(i,k,nv)
                enddo
                do k=k22(i)+1,ktop(i)
                   chem_up(i,k,nv)=(chem_up(i,k-1,nv)*zu(i,k-1) &
                             -.5*up_massdetr(i,k-1)*chem_up(i,k-1,nv)+ &
                         up_massentr(i,k-1)*chem(i,k-1,nv)) / &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
                   chem_c(i,k,nv)=alpha*chem_up(i,k,nv)
                   dz=zo_cup(i,K)-zo_cup(i,K-1)
                   trcc=chem_c(i,k,nv)/(1.+c0(i,k)*dz)
                   chem_pw(i,k,nv)=c0(i,k)*dz*trcc*zu(i,k)
                   chem_pwav(i,nv)=chem_pwav(i,nv)+chem_pw(i,k,nv)
                   chem_up(i,k,nv)=trcc+chem_up(i,k,nv)-chem_c(i,k,nv)
                enddo
                do k=ktop(i)+1,ktf
                 chem_up(i,k,nv)=chem_cup(i,k,nv)
                enddo
                chem_down(i,jmin(i)+1,nv)=chem_cup(i,jmin(i)+1,nv)
                do ki=jmin(i) ,1,-1
                  chem_down(i,ki,nv)=(chem_down(i,ki+1,nv)*zdo(i,ki+1) &
                         -.5*dd_massdetro(i,ki)*chem_down(i,ki+1,nv)+ &
                        dd_massentro(i,ki)*chem(i,ki,nv)) / &
                        (zdo(i,ki+1)-.5*dd_massdetro(i,ki)+dd_massentro(i,ki))
                  chem_down(i,ki,nv)=chem_down(i,ki,nv)+pwdper(i,ki)*chem_pwav(i,nv)
                  chem_pwd(i,ki,nv)=max(0.,pwdper(i,ki)*chem_pwav(i,nv))
                enddo
              enddo
            endif
          enddo
        endif
        do i=its,itf
        if(ierr(i)/=0)cycle
            edto(i)=edtc(i,1)
        enddo
      call get_melting_profile(ierr,tn_cup,po_cup, p_liq_ice,melting_layer,qrco &
                              ,pwo,edto,pwdo,melting &
                              ,itf,ktf,its,ite, kts,kte, cumulus )
        do k=kts,ktf
        do i=its,itf
           dellat_ens (i,k,1)=0.
           dellaq_ens (i,k,1)=0.
           dellaqc_ens(i,k,1)=0.
           pwo_ens (i,k,1)=0.
        enddo
        enddo
      do k=kts,kte
      do i=its,itf
        dellu (i,k)=0.
        dellv (i,k)=0.
        dellah (i,k)=0.
        dellat (i,k)=0.
        dellaq (i,k)=0.
        dellaqc(i,k)=0.
      enddo
      enddo
      do i=its,itf
        if(ierr(i)/=0)cycle
         dp=100.*(po_cup(i,1)-po_cup(i,2))
         dellu(i,1)=pgcd*(edto(i)*zdo(i,2)*ucd(i,2) &
                         -edto(i)*zdo(i,2)*u_cup(i,2))*g/dp
         dellv(i,1)=pgcd*(edto(i)*zdo(i,2)*vcd(i,2) &
                         -edto(i)*zdo(i,2)*v_cup(i,2))*g/dp
         do k=kts+1,ktop(i)
            pgc=pgcon
            entupk=0.
            if(k == k22(i)-1) entupk=zuo(i,k+1)
            detupk=0.
            entdoj=0.
            detdo=edto(i)*dd_massdetro(i,k)
            entdo=edto(i)*dd_massentro(i,k)
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
            subin=-zdo(i,k+1)*edto(i)
            subdown=-zdo(i,k)*edto(i)
            if(k.eq.ktop(i))then
               detupk=zuo(i,ktop(i))
               subin=0.
               subdown=0.
               detdo=0.
               entdo=0.
               entup=0.
               detup=0.
            endif
            totmas=subin-subdown+detup-entup-entdo+ &
                   detdo-entupk-entdoj+detupk+zuo(i,k+1)-zuo(i,k)
            if(abs(totmas).gt.1.e-6)then
123 formAT(a7,1X,3i3,2E12.4,1(1x,f5.2),3e12.4)
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
             pgc=pgcon
            if(k.ge.ktop(i))pgc=0.
             dellu(i,k) =-(zuo(i,k+1)*(uc (i,k+1)-u_cup(i,k+1) ) - &
                            zuo(i,k )*(uc (i,k )-u_cup(i,k ) ) )*g/dp &
                          +(zdo(i,k+1)*(ucd(i,k+1)-u_cup(i,k+1) ) - &
                            zdo(i,k )*(ucd(i,k )-u_cup(i,k ) ) )*g/dp*edto(i)*pgcd
             dellv(i,k) =-(zuo(i,k+1)*(vc (i,k+1)-v_cup(i,k+1) ) - &
                            zuo(i,k )*(vc (i,k )-v_cup(i,k ) ) )*g/dp &
                         +(zdo(i,k+1)*(vcd(i,k+1)-v_cup(i,k+1) ) - &
                            zdo(i,k )*(vcd(i,k )-v_cup(i,k ) ) )*g/dp*edto(i)*pgcd
       enddo
    enddo
    do i=its,itf
        if(ierr(i).eq.0)then
         dp=100.*(po_cup(i,1)-po_cup(i,2))
         dellah(i,1)=(edto(i)*zdo(i,2)*hcdo(i,2) &
                     -edto(i)*zdo(i,2)*heo_cup(i,2))*g/dp
         dellaq (i,1)=(edto(i)*zdo(i,2)*qcdo(i,2) &
                      -edto(i)*zdo(i,2)*qo_cup(i,2))*g/dp
         G_rain= 0.5*(pwo (i,1)+pwo (i,2))*g/dp
         E_dn = -0.5*(pwdo(i,1)+pwdo(i,2))*g/dp*edto(i)
         dellaq(i,1) = dellaq(i,1)+ E_dn-G_rain
         do k=kts+1,ktop(i)
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k) =-(zuo(i,k+1)*(hco (i,k+1)-heo_cup(i,k+1) ) - &
                           zuo(i,k )*(hco (i,k )-heo_cup(i,k ) ) )*g/dp &
                         +(zdo(i,k+1)*(hcdo(i,k+1)-heo_cup(i,k+1) ) - &
                           zdo(i,k )*(hcdo(i,k )-heo_cup(i,k ) ) )*g/dp*edto(i)
           dellah(i,k) = dellah(i,k) + xlf*((1.-p_liq_ice(i,k))*0.5*(qrco(i,k+1)+qrco(i,k)) &
                                     - melting(i,k))*g/dp
            detup=up_massdetro(i,k)
            dz=zo_cup(i,k)-zo_cup(i,k-1)
            if(k.lt.ktop(i) .and. c1d(i,k).gt.0) then
                dellaqc(i,k) = zuo(i,k)*c1d(i,k)*qrco(i,k)*dz/dp*g
            else
                dellaqc(i,k)= detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
            endif
            if(k.eq.ktop(i))dellaqc(i,k)= detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
            G_rain= 0.5*(pwo (i,k)+pwo (i,k+1))*g/dp
            E_dn = -0.5*(pwdo(i,k)+pwdo(i,k+1))*g/dp*edto(i)
            C_up = dellaqc(i,k)+(zuo(i,k+1)* qrco(i,k+1) - &
                                zuo(i,k )* qrco(i,k ) )*g/dp + G_rain
            dellaq(i,k) =-(zuo(i,k+1)*(qco (i,k+1)-qo_cup(i,k+1) ) - &
                           zuo(i,k )*(qco (i,k )-qo_cup(i,k ) ) )*g/dp &
                         +(zdo(i,k+1)*(qcdo(i,k+1)-qo_cup(i,k+1) ) - &
                           zdo(i,k )*(qcdo(i,k )-qo_cup(i,k ) ) )*g/dp*edto(i) &
                         - C_up + E_dn
         enddo
        endif
      enddo
444 format(1x,i2,1x,7e12.4)
     if (numc.gt.0)then
      dellac(:,:,:)=0.
      do nv=1,numc
      do i=its,itf
        if(ierr(i).eq.0)then
            dp=100.*(po_cup(i,1)-po_cup(i,2))
            G_rain= 0.5*(chem_pw (i,1,nv)+chem_pw (i,2,nv))*g/dp
            E_dn = 0.5*(chem_pwd(i,1,nv)+chem_pwd(i,2,nv))*g/dp
            dellac(i,1,nv)=(edto(i)*zdo(i,2)*chem_down(i,2,nv) &
                     -edto(i)*zdo(i,2)*chem_cup(i,2,nv))*g/dp &
                         - g_rain + E_dn
            do k=kts+1,ktop(i)
               dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            G_rain= 0.5*(chem_pw (i,k,nv)+chem_pw (i,k+1,nv))*g/dp
            E_dn = 0.5*(chem_pwd(i,k,nv)+chem_pwd(i,k+1,nv))*g/dp
               dellac(i,k,nv) =-(zuo(i,k+1)*(chem_up(i,k+1,nv)-chem_cup(i,k+1,nv) ) - &
                    zuo(i,k )*(chem_up (i,k,nv )-chem_cup(i,k,nv ) ) )*g/dp &
                    +(zdo(i,k+1)*(chem_down(i,k+1,nv)-chem_cup(i,k+1,nv) ) - &
                    zdo(i,k )*(chem_down(i,k,nv)-chem_cup(i,k,nv)))*g/dp*edto(i) &
                         - g_rain + E_dn
            enddo
         endif
       enddo
       enddo
131 format(1x,i3,5f13.5)
     endif
      mbdt=.1
      do i=its,itf
      xaa0_ens(i,1)=0.
      enddo
      do i=its,itf
         if(ierr(i).eq.0)then
           do k=kts,ktf
            XHE(I,K)=DELLAH(I,K)*MBDT+HEO(I,K)
            XQ(I,K)=max(1.e-16,DELLAQ(I,K)*MBDT+QO(I,K))
            DELLAT(I,K)=(1./cp)*(DELLAH(I,K)-xlv*DELLAQ(I,K))
            XT(I,K)= DELLAT(I,K)*MBDT+TN(I,K)
            xt(i,k)=max(190.,xt(i,k))
           enddo
         ENDIF
      enddo
      do i=its,itf
      if(ierr(i).eq.0)then
      XHE(I,ktf)=HEO(I,ktf)
      XQ(I,ktf)=QO(I,ktf)
      XT(I,ktf)=TN(I,ktf)
      endif
      enddo
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,-1, &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur, &
           ierr,z1, &
           itf,ktf, &
           its,ite, kts,kte)
      do k=kts,ktf
      do i=its,itf
         xhc(i,k)=0.
         xDBY(I,K)=0.
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
         x_add = xlv*zqexec(i)+cp*ztexec(i)
         call get_cloud_bc(kte,xhe_cup (i,1:kte),xhkb (i),k22(i),x_add)
         do k=1,start_level(i)-1
            xhc(i,k)=xhe_cup(i,k)
         enddo
         k=start_level(i)
         xhc(i,k)=xhkb(i)
        endif
      enddo
      do i=its,itf
       if(ierr(i).eq.0)then
        do k=start_level(i) +1,ktop(i)
         xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1) + &
                                            up_massentro(i,k-1)*xhe(i,k-1)) / &
                             (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          xhc (i,k)= xhc (i,k)+ xlf*(1.-p_liq_ice(i,k))*qrco(i,k)
         xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
        enddo
        do k=ktop(i)+1,ktf
           xHC (i,K)=xhes_cup(i,k)
           xDBY(I,K)=0.
        enddo
       endif
      enddo
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      do i=its,itf
         if(ierr(i).eq.0)then
           xaa0_ens(i,1)=xaa0(i)
           do k=kts,ktop(i)
                 do nens3=1,maxens3
                 if(nens3.eq.7)then
                 pr_ens(i,nens3)=pr_ens(i,nens3) &
                                    +pwo(i,k)+edto(i)*pwdo(i,k)
                 else if(nens3.eq.8)then
                 pr_ens(i,nens3)=pr_ens(i,nens3)+ &
                                    pwo(i,k)+edto(i)*pwdo(i,k)
                 else if(nens3.eq.9)then
                 pr_ens(i,nens3)=pr_ens(i,nens3) &
                                 + pwo(i,k)+edto(i)*pwdo(i,k)
                 else
                 pr_ens(i,nens3)=pr_ens(i,nens3)+ &
                                    pwo(i,k) +edto(i)*pwdo(i,k)
                 endif
                 enddo
           enddo
         if(pr_ens(i,7).lt.1.e-6)then
            ierr(i)=18
            ierrc(i)="total normalized condensate too small"
            do nens3=1,maxens3
               pr_ens(i,nens3)=0.
            enddo
         endif
         do nens3=1,maxens3
           if(pr_ens(i,nens3).lt.1.e-5)then
            pr_ens(i,nens3)=0.
           endif
         enddo
         endif
      enddo
 200 continue
      do i=its,itf
         ierr2(i)=ierr(i)
         ierr3(i)=ierr(i)
         k22x(i)=k22(i)
      enddo
        CALL cup_MAXIMI(HEO_CUP,2,KBMAX,K22x,ierr, &
             itf,ktf, &
             its,ite, kts,kte)
        iloop=2
        call cup_kbcon(ierrc,cap_max_increment,iloop,k22x,kbconx,heo_cup, &
             heso_cup,hkbo,ierr2,kbmax,po_cup,cap_max, &
             ztexec,zqexec, &
             0,itf,ktf, &
             its,ite, kts,kte, &
             z_cup,entr_rate,heo,imid)
        iloop=3
        call cup_kbcon(ierrc,cap_max_increment,iloop,k22x,kbconx,heo_cup, &
             heso_cup,hkbo,ierr3,kbmax,po_cup,cap_max, &
             ztexec,zqexec, &
             0,itf,ktf, &
             its,ite, kts,kte, &
             z_cup,entr_rate,heo,imid)
      DO I = its,itf
        mconv(i) = 0
        if(ierr(i)/=0)cycle
        DO K=1,ktop(i)
          dq=(qo_cup(i,k+1)-qo_cup(i,k))
          mconv(i)=mconv(i)+omeg(i,k)*dq/g
        ENDDO
      ENDDO
      call cup_forcing_ens_3d(closure_n,xland1,aa0,aa1,xaa0_ens,mbdt,dtime, &
           ierr,ierr2,ierr3,xf_ens,axx,forcing, &
           maxens3,mconv,rand_clos, &
           po_cup,ktop,omeg,zdo,zdm,k22,zuo,pr_ens,edto,edtm,kbcon, &
           ichoice, &
           imid,ipr,itf,ktf, &
           its,ite, kts,kte, &
           dicycle,tau_ecmwf,aa1_bl,xf_dicycle)
      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
           dellat_ens (i,k,1)=dellat(i,k)
           dellaq_ens (i,k,1)=dellaq(i,k)
           dellaqc_ens(i,k,1)=dellaqc(i,k)
           pwo_ens (i,k,1)=pwo(i,k)
        else
           dellat_ens (i,k,1)=0.
           dellaq_ens (i,k,1)=0.
           dellaqc_ens(i,k,1)=0.
           pwo_ens (i,k,1)=0.
        endif
      enddo
      enddo
 250 continue
       if(imid.eq.1 .and. ichoice .le.2)then
         do i=its,itf
          xff_mid(i,1)=0.
          xff_mid(i,2)=0.
          if(ierr(i).eq.0)then
            blqe=0.
            trash=0.
            if(k22(i).lt.kpbl(i)+1)then
               do k=1,kpbl(i)
                  blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
               enddo
               trash=max((hco(i,kbcon(i))-heo_cup(i,kbcon(i))),1.e1)
               xff_mid(i,1)=max(0.,blqe/trash)
               xff_mid(i,1)=min(0.1,xff_mid(i,1))
             endif
             xff_mid(i,2)=min(0.1,.03*zws(i))
          endif
         enddo
       endif
       call cup_output_ens_3d(xff_mid,xf_ens,ierr,dellat_ens,dellaq_ens, &
            dellaqc_ens,outt, &
            outq,outqc,zuo,pre,pwo_ens,xmb,ktop, &
            edto,pwdo,'deep',ierr2,ierr3, &
            po_cup,pr_ens,maxens3, &
            sig,closure_n,xland1,xmbm_in,xmbs_in, &
            ichoice,imid,ipr,itf,ktf, &
            its,ite, kts,kte, &
            dicycle,xf_dicycle )
      k=1
      do i=its,itf
          if(ierr(i).eq.0 .and.pre(i).gt.0.) then
             PRE(I)=MAX(PRE(I),0.)
             xmb_out(i)=xmb(i)
             do k=kts,ktop(i)
               outu(i,k)=dellu(i,k)*xmb(i)
               outv(i,k)=dellv(i,k)*xmb(i)
             enddo
          elseif(ierr(i).ne.0 .or. pre(i).eq.0.)then
             ktop(i)=0
             do k=kts,kte
               outt(i,k)=0.
               outq(i,k)=0.
               outqc(i,k)=0.
               outu(i,k)=0.
               outv(i,k)=0.
             enddo
          endif
      enddo
      if(irainevap.eq.1)then
      do i = its,itf
       rntot(i) = 0.
       delqev(i) = 0.
       delq2(i) = 0.
       rn(i) = 0.
       rntot(i) = 0.
       rain=0.
       if(ierr(i).eq.0)then
         do k = ktop(i), 1, -1
              rain = pwo(i,k) + edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i)* .001 * dtime
         enddo
       endif
      enddo
      do i = its,itf
         qevap(i) = 0.
         flg(i) = .true.
         if(ierr(i).eq.0)then
         evef = edt(i) * evfact
         if(xland(i).gt.0.5 .and. xland(i).lt.1.5) evef=edt(i) * evfactl
         do k = ktop(i), 1, -1
              rain = pwo(i,k) + edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * .001 * dtime
              if(flg(i))then
              q1=qo(i,k)+(outq(i,k))*dtime
              t1=tn(i,k)+(outt(i,k))*dtime
              qcond(i) = evef * (q1 - qeso(i,k)) &
     & / (1. + el2orc * qeso(i,k) / t1**2)
              dp = -100.*(p_cup(i,k+1)-p_cup(i,k))
              if(rn(i).gt.0. .and. qcond(i).lt.0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dtime*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i).gt.0..and.qcond(i).lt.0..and. &
     & delq2(i).gt.rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i).gt.0..and.qevap(i).gt.0.) then
                outq(i,k) = outq(i,k) + qevap(i)/dtime
                outt(i,k) = outt(i,k) - elocp * qevap(i)/dtime
                rn(i) = max(0.,rn(i) - .001 * qevap(i) * dp / g)
                pre(i) = pre(i) - qevap(i) * dp /g/dtime
                PRE(I)=MAX(PRE(I),0.)
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
          endif
        enddo
      endif
      enddo
      endif
      do i=its,itf
          if(ierr(i).eq.0) then
             dts=0.
             fpi=0.
             do k=kts,ktop(i)
                dp=(po_cup(i,k)-po_cup(i,k+1))*100.
                dts= dts -(outu(i,k)*us(i,k)+outv(i,k)*vs(i,k))*dp/g
                fpi = fpi +sqrt(outu(i,k)*outu(i,k) + outv(i,k)*outv(i,k))*dp
             enddo
             if(fpi.gt.0.)then
                do k=kts,ktop(i)
                   fp= sqrt((outu(i,k)*outu(i,k)+outv(i,k)*outv(i,k)))/fpi
                   outt(i,k)=outt(i,k)+fp*dts*g/cp
                enddo
             endif
             if(numc.gt.0)then
               do nv=1,numc
                 do k=2,ktop(i)
                   outc(i,k,nv)=dellac(i,k,nv)*xmb(i)
                 enddo
               enddo
             endif
          endif
      enddo
   END SUBROUTINE CUP_gf
   SUBROUTINE cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
              pw,ccn,pwev,edtmax,edtmin,edtc,psum2,psumh, &
              rho,aeroevap,itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        aeroevap,itf,ktf, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        rho,us,vs,z,p,pw
     real, dimension (its:ite,1) &
        ,intent (out ) :: &
        edtc
     real, dimension (its:ite) &
        ,intent (out ) :: &
        edt
     real, dimension (its:ite) &
        ,intent (in ) :: &
        pwav,pwev,ccn,psum2,psumh,edtmax,edtmin
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        ktop,kbcon
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     integer i,k,kk
     real einc,pef,pefb,prezk,zkbc
     real, dimension (its:ite) :: &
      vshear,sdp,vws
     real :: prop_c,pefc,aeroadd,alpha3,beta3
     prop_c=8.
     alpha3 = 1.9
     beta3 = -1.13
     pefc=0.
       do i=its,itf
        edt(i)=0.
        vws(i)=0.
        sdp(i)=0.
        vshear(i)=0.
       enddo
       do i=its,itf
        edtc(i,1)=0.
       enddo
       do kk = kts,ktf-1
         do 62 i=its,itf
          IF(ierr(i).ne.0)GO TO 62
          if (kk .le. min0(ktop(i),ktf) .and. kk .ge. kbcon(i)) then
             vws(i) = vws(i)+ &
              (abs((us(i,kk+1)-us(i,kk))/(z(i,kk+1)-z(i,kk))) &
          + abs((vs(i,kk+1)-vs(i,kk))/(z(i,kk+1)-z(i,kk)))) * &
              (p(i,kk) - p(i,kk+1))
            sdp(i) = sdp(i) + p(i,kk) - p(i,kk+1)
          endif
          if (kk .eq. ktf-1)vshear(i) = 1.e3 * vws(i) / sdp(i)
   62 continue
       end do
      do i=its,itf
         IF(ierr(i).eq.0)then
            pef=(1.591-.639*VSHEAR(I)+.0953*(VSHEAR(I)**2) &
               -.00496*(VSHEAR(I)**3))
            if(pef.gt.0.9)pef=0.9
            if(pef.lt.0.1)pef=0.1
            zkbc=z(i,kbcon(i))*3.281e-3
            prezk=.02
            if(zkbc.gt.3.)then
               prezk=.96729352+zkbc*(-.70034167+zkbc*(.162179896+zkbc &
               *(- 1.2569798E-2+zkbc*(4.2772E-4-zkbc*5.44E-6))))
            endif
            if(zkbc.gt.25)then
               prezk=2.4
            endif
            pefb=1./(1.+prezk)
            if(pefb.gt.0.9)pefb=0.9
            if(pefb.lt.0.1)pefb=0.1
            EDT(I)=1.-.5*(pefb+pef)
            if(aeroevap.gt.1)then
               aeroadd=(ccnclean**beta3)*((psumh(i))**(alpha3-1))
               prop_c=.5*(pefb+pef)/aeroadd
               aeroadd=(ccn(i)**beta3)*((psum2(i))**(alpha3-1))
               aeroadd=prop_c*aeroadd
               pefc=aeroadd
               if(pefc.gt.0.9)pefc=0.9
               if(pefc.lt.0.1)pefc=0.1
               EDT(I)=1.-pefc
               if(aeroevap.eq.2)EDT(I)=1.-.25*(pefb+pef+2.*pefc)
            endif
            einc=.2*edt(i)
            edtc(i,1)=edt(i)-einc
         endif
      enddo
      do i=its,itf
         IF(ierr(i).eq.0)then
               EDTC(I,1)=-EDTC(I,1)*pwav(I)/PWEV(I)
               IF(EDTC(I,1).GT.edtmax(i))EDTC(I,1)=edtmax(i)
               IF(EDTC(I,1).LT.edtmin(i))EDTC(I,1)=edtmin(i)
         endif
      enddo
   END SUBROUTINE cup_dd_edt
   SUBROUTINE cup_dd_moisture(ierrc,zd,hcd,hes_cup,qcd,qes_cup, &
              pwd,q_cup,z_cup,dd_massentr,dd_massdetr,jmin,ierr, &
              gamma_cup,pwev,bu,qrcd, &
              q,he,iloop, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
                                  itf,ktf, &
                                  its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        zd,hes_cup,hcd,qes_cup,q_cup,z_cup, &
        dd_massentr,dd_massdetr,gamma_cup,q,he
     integer &
        ,intent (in ) :: &
        iloop
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        jmin
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     real, dimension (its:ite,kts:kte)&
        ,intent (out ) :: &
        qcd,qrcd,pwd
     real, dimension (its:ite)&
        ,intent (out ) :: &
        pwev,bu
     character*50 :: ierrc(its:ite)
     integer :: &
        i,k,ki
     real :: &
        denom,dh,dz,dqeva
      do i=its,itf
         bu(i)=0.
         pwev(i)=0.
      enddo
      do k=kts,ktf
      do i=its,itf
         qcd(i,k)=0.
         qrcd(i,k)=0.
         pwd(i,k)=0.
      enddo
      enddo
      do 100 i=its,itf
      IF(ierr(I).eq.0)then
      k=jmin(i)
      DZ=Z_cup(i,K+1)-Z_cup(i,K)
      qcd(i,k)=q_cup(i,k)
      DH=HCD(I,k)-HES_cup(I,K)
      if(dh.lt.0)then
        QRCD(I,K)=(qes_cup(i,k)+(1./XLV)*(GAMMA_cup(i,k) &
                  /(1.+GAMMA_cup(i,k)))*DH)
        else
          qrcd(i,k)=qes_cup(i,k)
        endif
      pwd(i,jmin(i))=zd(i,jmin(i))*min(0.,qcd(i,k)-qrcd(i,k))
      qcd(i,k)=qrcd(i,k)
      pwev(i)=pwev(i)+pwd(i,jmin(i))
      bu(i)=dz*dh
      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         denom=zd(i,ki+1)-.5*dd_massdetr(i,ki)+dd_massentr(i,ki)
         if(denom.lt.1.e-16)then
            ierr(i)=51
            exit
         endif
         qcd(i,ki)=(qcd(i,ki+1)*zd(i,ki+1) &
                  -.5*dd_massdetr(i,ki)*qcd(i,ki+1)+ &
                  dd_massentr(i,ki)*q(i,ki)) / &
                  (zd(i,ki+1)-.5*dd_massdetr(i,ki)+dd_massentr(i,ki))
         DH=HCD(I,ki)-HES_cup(I,Ki)
         bu(i)=bu(i)+dz*dh
         QRCD(I,Ki)=qes_cup(i,ki)+(1./XLV)*(GAMMA_cup(i,ki) &
                  /(1.+GAMMA_cup(i,ki)))*DH
         dqeva=qcd(i,ki)-qrcd(i,ki)
         if(dqeva.gt.0.)then
          dqeva=0.
          qrcd(i,ki)=qcd(i,ki)
         endif
         pwd(i,ki)=zd(i,ki)*dqeva
         qcd(i,ki)=qrcd(i,ki)
         pwev(i)=pwev(i)+pwd(i,ki)
      enddo
       if( (pwev(i).eq.0.) .and. (iloop.eq.1))then
         ierr(i)=7
         ierrc(i)="problem with buoy in cup_dd_moisture"
       endif
       if(BU(I).GE.0.and.iloop.eq.1)then
         ierr(i)=7
         ierrc(i)="problem2 with buoy in cup_dd_moisture"
       endif
      endif
100 continue
   END SUBROUTINE cup_dd_moisture
   SUBROUTINE cup_env(z,qes,he,hes,t,q,p,z1, &
              psur,ierr,tcrit,itest, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        itf,ktf, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        p,t,q
     real, dimension (its:ite,kts:kte) &
        ,intent (out ) :: &
        he,hes,qes
     real, dimension (its:ite,kts:kte) &
        ,intent (inout) :: &
        z
     real, dimension (its:ite) &
        ,intent (in ) :: &
        psur,z1
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     integer &
        ,intent (in ) :: &
        itest
     integer :: &
       i,k
      real, dimension (its:ite,kts:kte) :: tv
      real :: tcrit,e,tvbar
      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
        e=satvap(t(i,k))
        qes(i,k)=0.622*e/max(1.e-8,(p(i,k)-e))
        IF(QES(I,K).LE.1.E-16)QES(I,K)=1.E-16
        IF(QES(I,K).LT.Q(I,K))QES(I,K)=Q(I,K)
        TV(I,K)=T(I,K)+.608*Q(I,K)*T(I,K)
        endif
      enddo
      enddo
      if(itest.eq.1 .or. itest.eq.0)then
         do i=its,itf
           if(ierr(i).eq.0)then
             Z(I,1)=max(0.,Z1(I))-(ALOG(P(I,1))- &
                 ALOG(PSUR(I)))*287.*TV(I,1)/9.81
           endif
         enddo
         DO K=kts+1,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
              TVBAR=.5*TV(I,K)+.5*TV(I,K-1)
              Z(I,K)=Z(I,K-1)-(ALOG(P(I,K))- &
               ALOG(P(I,K-1)))*287.*TVBAR/9.81
           endif
         enddo
         enddo
      else if(itest.eq.2)then
         do k=kts,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
             z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
             z(i,k)=max(1.e-3,z(i,k))
           endif
         enddo
         enddo
      else if(itest.eq.-1)then
      endif
       DO k=kts,ktf
       do i=its,itf
         if(ierr(i).eq.0)then
         if(itest.le.0)HE(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*Q(I,K)
         HES(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*QES(I,K)
         IF(HE(I,K).GE.HES(I,K))HE(I,K)=HES(I,K)
         endif
      enddo
      enddo
   END SUBROUTINE cup_env
   SUBROUTINE cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup, &
              he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
              ierr,z1, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        itf,ktf, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        qes,q,he,hes,z,p,t
     real, dimension (its:ite,kts:kte) &
        ,intent (out ) :: &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup
     real, dimension (its:ite) &
        ,intent (in ) :: &
        psur,z1
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     integer :: &
       i,k
      do k=kts,ktf
      do i=its,itf
        qes_cup(i,k)=0.
        q_cup(i,k)=0.
        hes_cup(i,k)=0.
        he_cup(i,k)=0.
        z_cup(i,k)=0.
        p_cup(i,k)=0.
        t_cup(i,k)=0.
        gamma_cup(i,k)=0.
      enddo
      enddo
      do k=kts+1,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,k)=.5*(qes(i,k-1)+qes(i,k))
        q_cup(i,k)=.5*(q(i,k-1)+q(i,k))
        hes_cup(i,k)=.5*(hes(i,k-1)+hes(i,k))
        he_cup(i,k)=.5*(he(i,k-1)+he(i,k))
        if(he_cup(i,k).gt.hes_cup(i,k))he_cup(i,k)=hes_cup(i,k)
        z_cup(i,k)=.5*(z(i,k-1)+z(i,k))
        p_cup(i,k)=.5*(p(i,k-1)+p(i,k))
        t_cup(i,k)=.5*(t(i,k-1)+t(i,k))
        gamma_cup(i,k)=(xlv/cp)*(xlv/(r_v*t_cup(i,k) &
                       *t_cup(i,k)))*qes_cup(i,k)
        endif
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,1)=qes(i,1)
        q_cup(i,1)=q(i,1)
        hes_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*qes(i,1)
        he_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*q(i,1)
        z_cup(i,1)=.5*(z(i,1)+z1(i))
        p_cup(i,1)=.5*(p(i,1)+psur(i))
        z_cup(i,1)=z1(i)
        p_cup(i,1)=psur(i)
        t_cup(i,1)=t(i,1)
        gamma_cup(i,1)=xlv/cp*(xlv/(r_v*t_cup(i,1) &
                       *t_cup(i,1)))*qes_cup(i,1)
        endif
      enddo
   END SUBROUTINE cup_env_clev
   SUBROUTINE cup_forcing_ens_3d(closure_n,xland,aa0,aa1,xaa0,mbdt,dtime,ierr,ierr2,ierr3,&
              xf_ens,axx,forcing,maxens3,mconv,rand_clos, &
              p_cup,ktop,omeg,zd,zdm,k22,zu,pr_ens,edt,edtm,kbcon, &
              ichoice, &
              imid,ipr,itf,ktf, &
              its,ite, kts,kte, &
              dicycle,tau_ecmwf,aa1_bl,xf_dicycle )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        imid,ipr,itf,ktf, &
        its,ite, kts,kte
     integer, intent (in ) :: &
        maxens3
     real, dimension (its:ite,1:maxens3) &
        ,intent (inout) :: &
        pr_ens
     real, dimension (its:ite,1:maxens3) &
        ,intent (inout ) :: &
        xf_ens
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        zd,zu,p_cup,zdm
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        omeg
     real, dimension (its:ite,1) &
        ,intent (in ) :: &
        xaa0
     real, dimension (its:ite,4) &
        ,intent (in ) :: &
       rand_clos
     real, dimension (its:ite) &
        ,intent (in ) :: &
        aa1,edt,edtm
     real, dimension (its:ite) &
        ,intent (in ) :: &
        mconv,axx
     real, dimension (its:ite) &
        ,intent (inout) :: &
        aa0,closure_n
     real &
        ,intent (in ) :: &
        mbdt
     real &
        ,intent (in ) :: &
        dtime
     integer, dimension (its:ite) &
        ,intent (inout ) :: &
        k22,kbcon,ktop
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        xland
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr,ierr2,ierr3
     integer &
        ,intent (in ) :: &
        ichoice
      integer, intent(IN) :: DICYCLE
      real, intent(IN) , dimension (its:ite) :: aa1_bl,tau_ecmwf
      real, intent(INOUT), dimension (its:ite) :: xf_dicycle
      real, intent(INOUT), dimension (its:ite,10) :: forcing
      real :: xff_dicycle
     real, dimension (1:maxens3) :: &
       xff_ens3
     real, dimension (1) :: &
       xk
     integer :: &
       kk,i,k,n,ne
     integer, dimension (its:ite) :: kloc
     real :: &
       a1,a_ave,xff0,xomg
     real, dimension (its:ite) :: ens_adj
       ens_adj(:)=1.
       xff_dicycle = 0.
       DO 100 i=its,itf
          kloc(i)=1
          IF(ierr(i).eq.0)then
           kloc(i)=kbcon(i)
           ens_adj(i)=1.
             a_ave=0.
             a_ave=axx(i)
             a_ave=max(0.,a_ave)
             a_ave=min(a_ave,aa1(i))
             a_ave=max(0.,a_ave)
             xff_ens3(:)=0.
             xff0= (AA1(I)-AA0(I))/DTIME
             xff_ens3(1)=max(0.,(AA1(I)-AA0(I))/dtime)
             xff_ens3(2)=max(0.,(AA1(I)-AA0(I))/dtime)
             xff_ens3(3)=max(0.,(AA1(I)-AA0(I))/dtime)
             xff_ens3(16)=max(0.,(AA1(I)-AA0(I))/dtime)
             forcing(i,1)=xff_ens3(2)
             xomg=0.
             kk=0
             xff_ens3(4)=0.
             xff_ens3(5)=0.
             xff_ens3(6)=0.
             do k=kbcon(i)-1,kbcon(i)+1
                     if(zu(i,k).gt.0.)then
                       xomg=xomg-omeg(i,k)/9.81/max(0.3,(1.-(edt(i)*zd(i,k)-edtm(i)*zdm(i,k))/zu(i,k)))
                       kk=kk+1
                     endif
             enddo
             if(kk.gt.0)xff_ens3(4)=xomg/float(kk)
             xff_ens3(4)=betajb*xff_ens3(4)
             xff_ens3(5)=xff_ens3(4)
             xff_ens3(6)=xff_ens3(4)
             if(xff_ens3(4).lt.0.)xff_ens3(4)=0.
             if(xff_ens3(5).lt.0.)xff_ens3(5)=0.
             if(xff_ens3(6).lt.0.)xff_ens3(6)=0.
             xff_ens3(14)=xff_ens3(4)
             forcing(i,2)=xff_ens3(4)
              xff_ens3(7)= mconv(i)
              xff_ens3(8)= mconv(i)
              xff_ens3(9)= mconv(i)
              xff_ens3(15)=mconv(i)
             forcing(i,3)=xff_ens3(8)
             xff_ens3(10)=AA1(i)/tau_ecmwf(i)
             xff_ens3(11)=AA1(I)/tau_ecmwf(i)
             xff_ens3(12)=AA1(I)/tau_ecmwf(i)
             xff_ens3(13)=(AA1(i))/tau_ecmwf(i)
             if(ichoice.eq.0)then
                if(xff0.lt.0.)then
                     xff_ens3(1)=0.
                     xff_ens3(2)=0.
                     xff_ens3(3)=0.
                     xff_ens3(10)=0.
                     xff_ens3(11)=0.
                     xff_ens3(12)=0.
                     xff_ens3(13)= 0.
                     xff_ens3(16)= 0.
                endif
             endif
             XK(1)=(XAA0(I,1)-AA1(I))/MBDT
             forcing(i,4)=aa0(i)
             forcing(i,5)=aa1(i)
             forcing(i,6)=xaa0(i,1)
             forcing(i,7)=xk(1)
             if(xk(1).le.0.and.xk(1).gt.-.01*mbdt) &
                           xk(1)=-.01*mbdt
             if(xk(1).gt.0.and.xk(1).lt.1.e-2) &
                           xk(1)=1.e-2
              if(xland(i).lt.0.1)then
                 if(ierr2(i).gt.0.or.ierr3(i).gt.0)then
                      xff_ens3(1) =ens_adj(i)*xff_ens3(1)
                      xff_ens3(2) =ens_adj(i)*xff_ens3(2)
                      xff_ens3(3) =ens_adj(i)*xff_ens3(3)
                      xff_ens3(4) =ens_adj(i)*xff_ens3(4)
                      xff_ens3(5) =ens_adj(i)*xff_ens3(5)
                      xff_ens3(6) =ens_adj(i)*xff_ens3(6)
                      xff_ens3(7) =ens_adj(i)*xff_ens3(7)
                      xff_ens3(8) =ens_adj(i)*xff_ens3(8)
                      xff_ens3(9) =ens_adj(i)*xff_ens3(9)
                      xff_ens3(10) =ens_adj(i)*xff_ens3(10)
                      xff_ens3(11) =ens_adj(i)*xff_ens3(11)
                      xff_ens3(12) =ens_adj(i)*xff_ens3(12)
                      xff_ens3(13) =ens_adj(i)*xff_ens3(13)
                      xff_ens3(14) =ens_adj(i)*xff_ens3(14)
                      xff_ens3(15) =ens_adj(i)*xff_ens3(15)
                      xff_ens3(16) =ens_adj(i)*xff_ens3(16)
                 endif
              endif
              if(XK(1).lt.0.)then
                 if(xff_ens3(1).gt.0)xf_ens(i,1)=max(0.,-xff_ens3(1)/xk(1))
                 if(xff_ens3(2).gt.0)xf_ens(i,2)=max(0.,-xff_ens3(2)/xk(1))
                 if(xff_ens3(3).gt.0)xf_ens(i,3)=max(0.,-xff_ens3(3)/xk(1))
                 if(xff_ens3(16).gt.0)xf_ens(i,16)=max(0.,-xff_ens3(16)/xk(1))
                 xf_ens(i,1)= xf_ens(i,1)+xf_ens(i,1)*rand_clos(i,1)
                 xf_ens(i,2)= xf_ens(i,2)+xf_ens(i,2)*rand_clos(i,1)
                 xf_ens(i,3)= xf_ens(i,3)+xf_ens(i,3)*rand_clos(i,1)
                 xf_ens(i,16)=xf_ens(i,16)+xf_ens(i,16)*rand_clos(i,1)
              else
                 xff_ens3(1)= 0
                 xff_ens3(2)= 0
                 xff_ens3(3)= 0
                 xff_ens3(16)=0
              endif
              xf_ens(i,4)=max(0.,xff_ens3(4))
              xf_ens(i,5)=max(0.,xff_ens3(5))
              xf_ens(i,6)=max(0.,xff_ens3(6))
              xf_ens(i,14)=max(0.,xff_ens3(14))
              a1=max(1.e-5,pr_ens(i,7))
              xf_ens(i,7)=max(0.,xff_ens3(7)/a1)
              a1=max(1.e-5,pr_ens(i,8))
              xf_ens(i,8)=max(0.,xff_ens3(8)/a1)
              a1=max(1.e-5,pr_ens(i,9))
              xf_ens(i,9)=max(0.,xff_ens3(9)/a1)
              a1=max(1.e-3,pr_ens(i,15))
              xf_ens(i,15)=max(0.,xff_ens3(15)/a1)
              xf_ens(i,4)=xf_ens(i,4)+xf_ens(i,4)*rand_clos(i,2)
              xf_ens(i,5)=xf_ens(i,5)+xf_ens(i,5)*rand_clos(i,2)
              xf_ens(i,6)=xf_ens(i,6)+xf_ens(i,6)*rand_clos(i,2)
              xf_ens(i,14)=xf_ens(i,14)+xf_ens(i,14)*rand_clos(i,2)
              xf_ens(i,7)=xf_ens(i,7)+xf_ens(i,7)*rand_clos(i,3)
              xf_ens(i,8)=xf_ens(i,8)+xf_ens(i,8)*rand_clos(i,3)
              xf_ens(i,9)=xf_ens(i,9)+xf_ens(i,9)*rand_clos(i,3)
              xf_ens(i,15)=xf_ens(i,15)+xf_ens(i,15)*rand_clos(i,3)
              if(XK(1).lt.0.)then
                 xf_ens(i,10)=max(0.,-xff_ens3(10)/xk(1))
                 xf_ens(i,11)=max(0.,-xff_ens3(11)/xk(1))
                 xf_ens(i,12)=max(0.,-xff_ens3(12)/xk(1))
                 xf_ens(i,13)=max(0.,-xff_ens3(13)/xk(1))
                 xf_ens(i,10)=xf_ens(i,10)+xf_ens(i,10)*rand_clos(i,4)
                 xf_ens(i,11)=xf_ens(i,11)+xf_ens(i,11)*rand_clos(i,4)
                 xf_ens(i,12)=xf_ens(i,12)+xf_ens(i,12)*rand_clos(i,4)
                 xf_ens(i,13)=xf_ens(i,13)+xf_ens(i,13)*rand_clos(i,4)
                 forcing(i,8)=xf_ens(i,11)
              else
                 xf_ens(i,10)=0.
                 xf_ens(i,11)=0.
                 xf_ens(i,12)=0.
                 xf_ens(i,13)=0.
                 forcing(i,8)=0.
              endif
              if(ichoice.ge.1)then
                 xf_ens(i,1)=xf_ens(i,ichoice)
                 xf_ens(i,2)=xf_ens(i,ichoice)
                 xf_ens(i,3)=xf_ens(i,ichoice)
                 xf_ens(i,4)=xf_ens(i,ichoice)
                 xf_ens(i,5)=xf_ens(i,ichoice)
                 xf_ens(i,6)=xf_ens(i,ichoice)
                 xf_ens(i,7)=xf_ens(i,ichoice)
                 xf_ens(i,8)=xf_ens(i,ichoice)
                 xf_ens(i,9)=xf_ens(i,ichoice)
                 xf_ens(i,10)=xf_ens(i,ichoice)
                 xf_ens(i,11)=xf_ens(i,ichoice)
                 xf_ens(i,12)=xf_ens(i,ichoice)
                 xf_ens(i,13)=xf_ens(i,ichoice)
                 xf_ens(i,14)=xf_ens(i,ichoice)
                 xf_ens(i,15)=xf_ens(i,ichoice)
                 xf_ens(i,16)=xf_ens(i,ichoice)
              endif
          elseif(ierr(i).ne.20.and.ierr(i).ne.0)then
              do n=1,maxens3
                 xf_ens(i,n)=0.
             enddo
          endif
 100 continue
IF(DICYCLE == 1 )THEN
       DO i=its,itf
          xf_dicycle(i) = 0.
          IF(ierr(i) /= 0)cycle
            XK(1)=(XAA0(I,1)-AA1(I))/MBDT
            if(xk(1).le.0.and.xk(1).gt.-.01*mbdt) xk(1)=-.01*mbdt
            if(xk(1).gt.0.and.xk(1).lt.1.e-2) xk(1)=1.e-2
            xff_dicycle = (AA1(i)-AA1_BL(i))/tau_ecmwf(i)
            if(xk(1).lt.0) xf_dicycle(i)= max(0.,-xff_dicycle/xk(1))
            xf_dicycle(i)= xf_ens(i,10)-xf_dicycle(i)
       ENDDO
ELSE
       xf_dicycle(:) = 0.
ENDIF
   END SUBROUTINE cup_forcing_ens_3d
   SUBROUTINE cup_kbcon(ierrc,cap_inc,iloop_in,k22,kbcon,he_cup,hes_cup, &
              hkb,ierr,kbmax,p_cup,cap_max, &
              ztexec,zqexec, &
              jprnt,itf,ktf, &
              its,ite, kts,kte, &
              z_cup,entr_rate,heo,imid )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        jprnt,itf,ktf,imid, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        he_cup,hes_cup,p_cup
     real, dimension (its:ite) &
        ,intent (in ) :: &
        entr_rate,ztexec,zqexec,cap_inc,cap_max
     real, dimension (its:ite) &
        ,intent (inout ) :: &
        hkb
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kbmax
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        kbcon,k22,ierr
     integer &
        ,intent (in ) :: &
        iloop_in
     character*50 :: ierrc(its:ite)
     real, dimension (its:ite,kts:kte),intent (in) :: z_cup,heo
     integer, dimension (its:ite) :: iloop,start_level
     integer :: &
        i,k
     real :: &
        x_add,pbcdif,plus,hetest,dz
     real, dimension (its:ite,kts:kte) ::hcot
      iloop(:)=iloop_in
       DO 27 i=its,itf
      kbcon(i)=1
      if(cap_max(i).gt.200 .and. imid.eq.1)iloop(i)=5
      IF(ierr(I).ne.0)GO TO 27
      start_level(i)=k22(i)
      KBCON(I)=K22(I)+1
      if(iloop(i).eq.5)KBCON(I)=K22(I)
        hcot(i,1:start_level(i)) = HKB(I)
        do k=start_level(i)+1,KBMAX(i)+3
           dz=z_cup(i,k)-z_cup(i,k-1)
           hcot(i,k)= ( (1.-0.5*entr_rate(i)*dz)*hcot(i,k-1) &
                         + entr_rate(i)*dz*heo(i,k-1) )/ &
                      (1.+0.5*entr_rate(i)*dz)
        enddo
      GO TO 32
 31 CONTINUE
      KBCON(I)=KBCON(I)+1
      IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop(i).ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        GO TO 27
      ENDIF
 32 CONTINUE
      hetest=hcot(i,kbcon(i))
      IF(HETEST.LT.HES_cup(I,KBCON(I)))then
        GO TO 31
      endif
      if(KBCON(I)-K22(I).eq.1)go to 27
      if(iloop(i).eq.5 .and. (KBCON(I)-K22(I)).le.2)go to 27
      PBCDIF=-P_cup(I,KBCON(I))+P_cup(I,K22(I))
      plus=max(25.,cap_max(i)-float(iloop(i)-1)*cap_inc(i))
      if(iloop(i).eq.4)plus=cap_max(i)
      if(iloop(i).eq.5)plus=150.
        if(iloop(i).eq.5.and.cap_max(i).gt.200)pbcdif=-P_cup(I,KBCON(I))+cap_max(i)
      IF(PBCDIF.le.plus)THEN
        Go To 27
      ElseIF(PBCDIF.GT.plus)THEN
        K22(I)=K22(I)+1
        KBCON(I)=K22(I)+1
        x_add = xlv*zqexec(i)+cp*ztexec(i)
        call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
        start_level(i)=k22(i)
        hcot(i,1:start_level(i)) = hkb(I)
        do k=start_level(i)+1,KBMAX(i)+3
           dz=z_cup(i,k)-z_cup(i,k-1)
           hcot(i,k)= ( (1.-0.5*entr_rate(i)*dz)*hcot(i,k-1) &
                         + entr_rate(i)*dz*heo(i,k-1) )/ &
                      (1.+0.5*entr_rate(i)*dz)
        enddo
        if(iloop(i).eq.5)KBCON(I)=K22(I)
        IF(KBCON(I).GT.KBMAX(i)+2)THEN
            if(iloop(i).ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
            endif
            GO TO 27
        ENDIF
        GO TO 32
      ENDIF
 27 CONTINUE
   END SUBROUTINE cup_kbcon
   SUBROUTINE cup_MAXIMI(ARRAY,KS,KE,MAXX,ierr, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
         itf,ktf, &
         its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
         array
     integer, dimension (its:ite) &
        ,intent (in ) :: &
         ierr,ke
     integer &
        ,intent (in ) :: &
         ks
     integer, dimension (its:ite) &
        ,intent (out ) :: &
         maxx
     real, dimension (its:ite) :: &
         x
     real :: &
         xar
     integer :: &
         i,k
       DO 200 i=its,itf
       MAXX(I)=KS
       if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS)
       DO 100 K=KS,KE(i)
         XAR=ARRAY(I,K)
         IF(XAR.GE.X(I)) THEN
            X(I)=XAR
            MAXX(I)=K
         ENDIF
 100 CONTINUE
      endif
 200 CONTINUE
   END SUBROUTINE cup_MAXIMI
   SUBROUTINE cup_minimi(ARRAY,KS,KEND,KT,ierr, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
         itf,ktf, &
         its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
         array
     integer, dimension (its:ite) &
        ,intent (in ) :: &
         ierr,ks,kend
     integer, dimension (its:ite) &
        ,intent (out ) :: &
         kt
     real, dimension (its:ite) :: &
         x
     integer :: &
         i,k,kstop
       DO 200 i=its,itf
      KT(I)=KS(I)
      if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS(I))
       KSTOP=MAX(KS(I)+1,KEND(I))
       DO 100 K=KS(I)+1,KSTOP
         IF(ARRAY(I,K).LT.X(I)) THEN
              X(I)=ARRAY(I,K)
              KT(I)=K
         ENDIF
 100 CONTINUE
      endif
 200 CONTINUE
   END SUBROUTINE cup_MINIMI
   SUBROUTINE cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
              kbcon,ktop,ierr, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        itf,ktf, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        z,zu,gamma_cup,t_cup,dby
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kbcon,ktop
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     real, dimension (its:ite) &
        ,intent (out ) :: &
        aa0
     integer :: &
        i,k
     real :: &
        dz,da
        do i=its,itf
         aa0(i)=0.
        enddo
        DO 100 k=kts+1,ktf
        DO 100 i=its,itf
         IF(ierr(i).ne.0)GO TO 100
         IF(K.LT.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         DZ=Z(I,K)-Z(I,K-1)
         da=zu(i,k)*DZ*(9.81/(1004.*( &
                (T_cup(I,K)))))*DBY(I,K-1)/ &
             (1.+GAMMA_CUP(I,K))
         AA0(I)=AA0(I)+max(0.,da)
         if(aa0(i).lt.0.)aa0(i)=0.
100 continue
   END SUBROUTINE cup_up_aa0
   SUBROUTINE neg_check(name,j,dt,q,outq,outt,outu,outv, &
                        outqc,outc,pret,its,ite,kts,kte,itf,ktf,numc,ktop)
   INTEGER, INTENT(IN ) :: j,its,ite,kts,kte,itf,ktf,numc
     real, dimension (its:ite,kts:kte ) , &
      intent(inout ) :: &
       outq,outt,outqc,outu,outv
     real, dimension (its:ite,kts:kte,1 ) , &
      intent(inout ) :: &
        outc
     real, dimension (its:ite,kts:kte ) , &
      intent(inout ) :: &
       q
     real, dimension (its:ite ) , &
      intent(inout ) :: &
       pret
      character *(*), intent (in) :: &
       name
     real &
        ,intent (in ) :: &
        dt
     real :: names,scalef,thresh,qmem,qmemf,qmem2,qtest,qmem1
     integer :: icheck
     integer, dimension (its:ite ) , &
        intent (in ) :: &
         ktop
      thresh=300.01
      names=1.
      if(name == 'shallow' .or. name == 'mid')then
        thresh=148.01
        names=1.
      endif
      scalef=86400.
      do i=its,itf
      if(ktop(i) <= 2)cycle
      icheck=0
      qmemf=1.
      qmem=0.
      do k=kts,ktop(i)
         qmem=(outt(i,k))*86400.
         if(qmem.gt.thresh)then
           qmem2=thresh/qmem
           qmemf=min(qmemf,qmem2)
      icheck=1
         endif
         if(qmem.lt.-.5*thresh*names)then
           qmem2=-.5*names*thresh/qmem
           qmemf=min(qmemf,qmem2)
      icheck=2
         endif
      enddo
      do k=kts,ktop(i)
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outu(i,k)=outu(i,k)*qmemf
         outv(i,k)=outv(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      if(numc.gt.0)then
         do nv=1,numc
            do k=kts,ktop(i)
              outc(i,k,nv)=outc(i,k,nv)*qmemf
            enddo
         enddo
endif
      pret(i)=pret(i)*qmemf
      enddo
      thresh=1.e-32
      do i=its,itf
      if(ktop(i) <= 2)cycle
      qmemf=1.
      do k=kts,ktop(i)
         qmem=outq(i,k)
         if(abs(qmem).gt.0. .and. q(i,k).gt.1.e-6)then
         qtest=q(i,k)+(outq(i,k))*dt
         if(qtest.lt.thresh)then
           qmem1=abs(outq(i,k))
           qmem2=abs((thresh-q(i,k))/dt)
           qmemf=min(qmemf,qmem2/qmem1)
           qmemf=max(0.,qmemf)
         endif
         endif
      enddo
      do k=kts,ktop(i)
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outu(i,k)=outu(i,k)*qmemf
         outv(i,k)=outv(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      if(numc.gt.0)then
         do nv=1,numc
            do k=kts,ktop(i)
              outc(i,k,nv)=outc(i,k,nv)*qmemf
            enddo
         enddo
      endif
      pret(i)=pret(i)*qmemf
      enddo
   END SUBROUTINE neg_check
   SUBROUTINE cup_output_ens_3d(xff_mid,xf_ens,ierr,dellat,dellaq,dellaqc, &
              outtem,outq,outqc, &
              zu,pre,pw,xmb,ktop, &
              edt,pwd,name,ierr2,ierr3,p_cup,pr_ens, &
              maxens3, &
              sig,closure_n,xland1,xmbm_in,xmbs_in, &
              ichoice,imid,ipr,itf,ktf, &
              its,ite, kts,kte, &
              dicycle,xf_dicycle )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        ichoice,imid,ipr,itf,ktf, &
        its,ite, kts,kte
     integer, intent (in ) :: &
        maxens3
     real, dimension (its:ite,1:maxens3) &
        ,intent (inout) :: &
       xf_ens,pr_ens
     real, dimension (its:ite,kts:kte) &
        ,intent (inout ) :: &
        outtem,outq,outqc
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        zu,pwd,p_cup
     real, dimension (its:ite) &
         ,intent (in ) :: &
        sig,xmbm_in,xmbs_in,edt
     real, dimension (its:ite,2) &
         ,intent (in ) :: &
        xff_mid
     real, dimension (its:ite) &
        ,intent (inout ) :: &
        pre,xmb
     real, dimension (its:ite) &
        ,intent (inout ) :: &
        closure_n
     real, dimension (its:ite,kts:kte,1) &
        ,intent (in ) :: &
       dellat,dellaqc,dellaq,pw
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        ktop,xland1
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr,ierr2,ierr3
     integer, intent(IN) :: DICYCLE
     real, intent(IN), dimension (its:ite) :: xf_dicycle
     integer :: &
        i,k,n
     real :: &
        clos_wei,dtt,dp,dtq,dtqc,dtpw,dtpwd
     real, dimension (its:ite) :: &
       pre2,xmb_ave,pwtot
      character *(*), intent (in) :: &
       name
      DO k=kts,kte
      do i=its,ite
        outtem (i,k)=0.
        outq (i,k)=0.
        outqc (i,k)=0.
      enddo
      enddo
      do i=its,itf
        pre(i)=0.
        xmb(i)=0.
      enddo
      do i=its,itf
        IF(ierr(i).eq.0)then
        do n=1,maxens3
           if(pr_ens(i,n).le.0.)then
             xf_ens(i,n)=0.
           endif
        enddo
        endif
      enddo
      if(imid.eq.0)then
      do i=its,itf
        if(ierr(i).eq.0)then
         k=0
         xmb_ave(i)=0.
         do n=1,maxens3
          k=k+1
          xmb_ave(i)=xmb_ave(i)+xf_ens(i,n)
         enddo
         xmb_ave(i)=xmb_ave(i)/float(k)
         if(dicycle == 2 )then
            xmb_ave(i)=xmb_ave(i)-max(0.,xmbs_in(i))
            xmb_ave(i)=max(0.,xmb_ave(i))
         else if (dicycle == 1) then
            xmb_ave(i)=xmb_ave(i) - xf_dicycle(i)
            xmb_ave(i)=max(0.,xmb_ave(i))
         endif
           clos_wei=16./max(1.,closure_n(i))
         xmb_ave(i)=min(xmb_ave(i),100.)
         xmb(i)=clos_wei*sig(i)*xmb_ave(i)
           if(xmb(i) < 1.e-16)then
              ierr(i)=19
           endif
        endif
      ENDDO
      else
         do i=its,itf
         xmb_ave(i)=0.
         IF(ierr(i).eq.0)then
           if(ichoice.eq.1 .or. ichoice.eq.2)then
              xmb_ave(i)=sig(i)*xff_mid(i,ichoice)
           else if(ichoice.gt.2)then
              k=0
              do n=1,maxens3
                    k=k+1
                    xmb_ave(i)=xmb_ave(i)+xf_ens(i,n)
              enddo
              xmb_ave(i)=xmb_ave(i)/float(k)
           else if(ichoice == 0)then
              xmb_ave(i)=.5*sig(i)*(xff_mid(i,1)+xff_mid(i,2))
           endif
           if(dicycle == 2 )then
              xmb(i)=max(0.,xmb_ave(i)-xmbs_in(i))
           else if (dicycle == 1) then
              xmb(i)=xmb_ave(i) - xf_dicycle(i)
              xmb(i)=max(0.,xmb_ave(i))
           else if (dicycle == 0) then
              xmb(i)=max(0.,xmb_ave(i))
           endif
         endif
         enddo
      endif
      do i=its,itf
        pwtot(i)=0.
        pre2(i)=0.
        IF(ierr(i).eq.0)then
            DO k=kts,ktop(i)
              pwtot(i)=pwtot(i)+pw(i,k,1)
            enddo
            DO k=kts,ktop(i)
            dp=100.*(p_cup(i,k)-p_cup(i,k+1))/g
            dtt =dellat (i,k,1)
            dtq =dellaq (i,k,1)
            dtpwd=-pwd(i,k)*edt(i)
            dtqc=dellaqc (i,k,1)*dp - dtpwd
           if(dtqc < 0.)then
             dtpwd=dtpwd-dellaqc(i,k,1)*dp
             dtqc=0.
           else
             dtqc=dtqc/dp
             dtpwd=0.
           endif
           OUTTEM(I,K)= XMB(I)* dtt
           OUTQ (I,K)= XMB(I)* dtq
           OUTQC (I,K)= XMB(I)* dtqc
           xf_ens(i,:)=sig(i)*xf_ens(i,:)
           PRE(I)=PRE(I)-XMB(I)*dtpwd
           pre2(i)=pre2(i)+xmb(i)*(pw(i,k,1)+edt(i)*pwd(i,k))
          enddo
          PRE(I)=-PRE(I)+XMB(I)*pwtot(i)
        endif
124 format(1x,i3,4e13.4)
125 format(1x,2e13.4)
      enddo
   END SUBROUTINE cup_output_ens_3d
   SUBROUTINE cup_up_moisture(name,ierr,z_cup,qc,qrc,pw,pwav, &
              p_cup,kbcon,ktop,dby,clw_all,xland1, &
              q,GAMMA_cup,zu,qes_cup,k22,qe_cup, &
              c0,ZQEXEC,ccn,rho,c1d,t, &
              up_massentr,up_massdetr,psum,psumh, &
              itest,itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
  real, parameter :: BDISPM = 0.366
  REAL, PARAMETER :: BDISPC = 0.146
     integer &
        ,intent (in ) :: &
                                  itest,itf,ktf, &
                                  its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        p_cup,rho,q,zu,gamma_cup,qe_cup, &
        up_massentr,up_massdetr,dby,qes_cup,z_cup
     real, dimension (its:ite) &
        ,intent (in ) :: &
        zqexec
     real, dimension (its:ite,kts:kte) &
        ,intent (inout ) :: c0
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kbcon,ktop,k22,xland1
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
      character *(*), intent (in) :: &
       name
     real, dimension (its:ite,kts:kte) &
        ,intent (out ) :: &
        qc,qrc,pw,clw_all
     real, dimension (its:ite,kts:kte) :: &
        qch,qrcb,pwh,clw_allh,c1d,t
     real, dimension (its:ite) :: &
        pwavh
     real, dimension (its:ite) &
        ,intent (out ) :: &
        pwav,psum,psumh
     real, dimension (its:ite) &
        ,intent (in ) :: &
        ccn
     integer :: &
        iprop,iall,i,k
     integer :: start_level(its:ite)
     real :: &
        prop_ave,qrcb_h,bdsp,dp,rhoc,qrch,qaver, &
        dz,berryc0,q1,berryc,c1c0max
     real :: &
        denom, c0t
     real, dimension (kts:kte) :: &
        prop_b
        prop_b(kts:kte)=0
        iall=0
        c0(:,:)=.002
        bdsp=BDISPM
        do i=its,itf
          pwav(i)=0.
          pwavh(i)=0.
          psum(i)=0.
          psumh(i)=0.
        enddo
        do k=kts,ktf
        do i=its,itf
          pw(i,k)=0.
          pwh(i,k)=0.
          qc(i,k)=0.
          if(ierr(i).eq.0)qc(i,k)=qe_cup(i,k)
          if(ierr(i).eq.0)qch(i,k)=qe_cup(i,k)
          clw_all(i,k)=0.
          clw_allh(i,k)=0.
          qrc(i,k)=0.
          qrcb(i,k)=0.
        enddo
        enddo
      do i=its,itf
      if(ierr(i).eq.0)then
         start_level=k22(i)
         call get_cloud_bc(kte,qe_cup (i,1:kte),qaver,k22(i))
         qaver = qaver
         k=start_level(i)
         qc (i,k)= qaver
         qch (i,k)= qaver
         do k=1,start_level(i)-1
           qc (i,k)= qe_cup(i,k)
           qch (i,k)= qe_cup(i,k)
         enddo
      endif
      enddo
       DO 100 i=its,itf
         IF(ierr(i).eq.0)then
            DO k=k22(i)+1,kbcon(i)
               c0(i,k)=.002
               c1c0max=c0(i,k)+c1d(i,k)
              if(t(i,k) > 273.16) then
               c0t = c0(i,k)
              else
               c0t = c0(i,k) * exp(0.07 * (t(i,k) - 273.16))
              endif
              c0(i,k)=c0t
              c1d(i,k)=c1c0max-c0(i,k)
              qc(i,k)= (qc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)* qc(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1)) / &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
               QRCH=QES_cup(I,K)+(1./XLV)*(GAMMA_cup(i,k) &
                 /(1.+GAMMA_cup(i,k)))*DBY(I,K)
              if(k.lt.kbcon(i))qrch=qc(i,k)
              if(qc(i,k).gt.qrch)then
                DZ=Z_cup(i,K)-Z_cup(i,K-1)
                QRC(I,K)=(QC(I,K)-QRCH)/(1.+c0t*DZ)
                PW(i,k)=c0t*dz*QRC(I,K)*zu(i,k)
                qc(i,k)=qrch+qrc(i,k)
                clw_all(i,k)=qrc(i,k)
              endif
            enddo
            DO k=kbcon(i)+1,ktop(i)
               c0(i,k)=.006
               c1c0max=c0(i,k)+c1d(i,k)
               if(t(i,k).lt.270.)c0(i,k)=.002
               if(t(i,k) > 273.16) then
                  c0t = c0(i,k)
               else
                  c0t = c0(i,k) * exp(0.07 * (t(i,k) - 273.16))
               endif
               c0(i,k)=c0t
               c1d(i,k)=c1c0max-c0(i,k)
               denom=zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1)
               if(denom.lt.1.e-16)then
                     ierr(i)=51
                exit
               endif
               rhoc=.5*(rho(i,k)+rho(i,k-1))
               DZ=Z_cup(i,K)-Z_cup(i,K-1)
               DP=p_cup(i,K)-p_cup(i,K-1)
               QRCH=QES_cup(I,K)+(1./XLV)*(GAMMA_cup(i,k) &
                 /(1.+GAMMA_cup(i,k)))*DBY(I,K)
               qc(i,k)= (qc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)* qc(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1)) / &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
               qch(i,k)= (qch(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*qch(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1)) / &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
               if(qc(i,k).le.qrch)then
                 qc(i,k)=qrch
               endif
               if(qch(i,k).le.qrch)then
                 qch(i,k)=qrch
               endif
               clw_all(i,k)=max(0.,QC(I,K)-QRCH)
               QRC(I,K)=max(0.,(QC(I,K)-QRCH))
               clw_allh(i,k)=max(0.,QCH(I,K)-QRCH)
               QRCB(I,K)=max(0.,(QCH(I,K)-QRCH))
               IF(autoconv.eq.2) then
                 q1=1.e3*rhoc*qrcb(i,k)
                 berryc0=q1*q1/(60.0*(5.0 + 0.0366*CCNclean/ &
                    ( q1 * BDSP) ) )
                 qrcb_h=((QCH(I,K)-QRCH)*zu(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1)+c0t*dz*zu(i,k))
                 prop_b(k)=c0t*qrcb_h*zu(i,k)/(1.e-3*berryc0)
                 pwh(i,k)=zu(i,k)*1.e-3*berryc0*dz*prop_b(k)
                 berryc=qrcb(i,k)
                 qrcb(i,k)=((QCh(I,K)-QRCH)*zu(i,k)-pwh(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                       (zu(i,k)+.5*up_massdetr(i,k-1))
                 if(qrcb(i,k).lt.0.)then
                   berryc0=(qrcb(i,k-1)*(.5*up_massdetr(i,k-1))-(QCh(I,K)-QRCH)*zu(i,k))/zu(i,k)*1.e-3*dz*prop_b(k)
                   pwh(i,k)=zu(i,k)*1.e-3*berryc0*dz*prop_b(k)
                   qrcb(i,k)=0.
                 endif
                 QCh(I,K)=QRCb(I,K)+qrch
                 PWAVH(I)=PWAVH(I)+pwh(I,K)
                 Psumh(I)=Psumh(I)+clw_allh(I,K)*zu(i,k) *dz
                 q1=1.e3*rhoc*qrc(i,k)
                 berryc0=q1*q1/(60.0*(5.0 + 0.0366*CCN(i)/ &
                    ( q1 * BDSP) ) )
                 berryc0=1.e-3*berryc0*dz*prop_b(k)
                 berryc=qrc(i,k)
                 qrc(i,k)=((QC(I,K)-QRCH)*zu(i,k)-zu(i,k)*berryc0-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                       (zu(i,k)+.5*up_massdetr(i,k-1))
                 if(qrc(i,k).lt.0.)then
                    berryc0=((QC(I,K)-QRCH)*zu(i,k)-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/zu(i,k)
                    qrc(i,k)=0.
                 endif
                 pw(i,k)=berryc0*zu(i,k)
                 QC(I,K)=QRC(I,K)+qrch
               ELSE
                 if(iall.eq.1)then
                   qrc(i,k)=0.
                   pw(i,k)=(QC(I,K)-QRCH)*zu(i,k)
                   if(pw(i,k).lt.0.)pw(i,k)=0.
                 else
                   QRC(I,K)=(QC(I,K)-QRCH)/(1.+(c1d(i,k)+c0t)*DZ)
                   PW(i,k)=c0t*dz*QRC(I,K)*zu(i,k)
                   if(qrc(i,k).lt.0)then
                     qrc(i,k)=0.
                     pw(i,k)=0.
                   endif
                 endif
                 QC(I,K)=QRC(I,K)+qrch
               endif
               PWAV(I)=PWAV(I)+PW(I,K)
               Psum(I)=Psum(I)+clw_all(I,K)*zu(i,k) *dz
            enddo
       do k=k22(i)+1,ktop(i)
           qc(i,k)=qc(i,k)-qrc(i,k)
       enddo
      endif
 100 CONTINUE
       prop_ave=0.
       iprop=0
       do k=kts,kte
        prop_ave=prop_ave+prop_b(k)
        if(prop_b(k).gt.0)iprop=iprop+1
       enddo
       iprop=max(iprop,1)
 END SUBROUTINE cup_up_moisture
 REAL FUNCTION satvap(temp2)
      implicit none
      real :: temp2, temp, toot, toto, eilog, tsot, &
     & ewlog, ewlog2, ewlog3, ewlog4
      temp = temp2-273.155
      if (temp.lt.-20.) then
        toot = 273.16 / temp2
        toto = 1 / toot
        eilog = -9.09718 * (toot - 1) - 3.56654 * (log(toot) / &
     & log(10.)) + .876793 * (1 - toto) + (log(6.1071) / log(10.))
        satvap = 10 ** eilog
      else
        tsot = 373.16 / temp2
        ewlog = -7.90298 * (tsot - 1) + 5.02808 * &
     & (log(tsot) / log(10.))
        ewlog2 = ewlog - 1.3816e-07 * &
     & (10 ** (11.344 * (1 - (1 / tsot))) - 1)
        ewlog3 = ewlog2 + .0081328 * &
     & (10 ** (-3.49149 * (tsot - 1)) - 1)
        ewlog4 = ewlog3 + (log(1013.246) / log(10.))
        satvap = 10 ** ewlog4
      end if
 END FUNCTION
 SUBROUTINE get_cloud_bc(mzp,array,x_aver,k22,add)
    implicit none
    integer, intent(in) :: mzp,k22
    real , intent(in) :: array(mzp)
    real , optional , intent(in) :: add
    real , intent(out) :: x_aver
    integer :: i,local_order_aver,order_aver
    order_aver = 3
    local_order_aver=min(k22,order_aver)
    x_aver=0.
    do i = 1,local_order_aver
      x_aver = x_aver + array(k22-i+1)
    enddo
      x_aver = x_aver/float(local_order_aver)
    if(present(add)) x_aver = x_aver + add
 end SUBROUTINE get_cloud_bc
 SUBROUTINE rates_up_pdf(rand_vmas,ipr,name,ktop,ierr,p_cup,entr_rate_2d,hkbo,heo,heso_cup,z_cup, &
               xland,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopdby,csum,pmin_lev)
     implicit none
     character *(*), intent (in) :: name
     integer, intent(in) :: ipr,its,ite,itf,kts,kte,ktf
     real, dimension (its:ite,kts:kte),intent (inout) :: entr_rate_2d,zuo
     real, dimension (its:ite,kts:kte),intent (in) ::p_cup, heo,heso_cup,z_cup
     real, dimension (its:ite),intent (in) :: hkbo,rand_vmas
     integer, dimension (its:ite),intent (in) :: kstabi,k22,kpbl,csum,xland,pmin_lev
     integer, dimension (its:ite),intent (inout) :: kbcon,ierr,ktop,ktopdby
     real, dimension (its:ite,kts:kte) :: hcot
     real :: beta_u,dz,dbythresh,dzh2,zustart,zubeg,massent,massdetr
     real :: dby(kts:kte),dbm(kts:kte),zux(kts:kte)
     real zuh2(40),zh2(40)
     integer :: kklev,i,kk,kbegin,k,kfinalzu
     integer, dimension (its:ite) :: start_level
     zustart=.1
     dbythresh= 1.
     if(name == 'shallow' .or. name == 'mid') dbythresh=1.
     dby(:)=0.
     DO i=its,itf
      if(ierr(i) > 0 )cycle
      zux(:)=0.
      beta_u=max(.1,.2-float(csum(i))*.01)
      zuo(i,:)=0.
      dby(:)=0.
      dbm(:)=0.
      kbcon(i)=max(kbcon(i),2)
       start_level(i)=k22(i)
       zuo(i,start_level(i))=zustart
        zux(start_level(i))=zustart
        do k=start_level(i)+1,kbcon(i)
          dz=z_cup(i,k)-z_cup(i,k-1)
          massent=dz*entr_rate_2d(i,k-1)*zuo(i,k-1)
          massdetr=dz*1.e-9*zuo(i,k-1)
          zuo(i,k)=zuo(i,k-1)+massent-massdetr
          zux(k)=zuo(i,k)
        enddo
       zubeg=zustart
       if(name .eq. 'deep')then
        ktop(i)=0
        hcot(i,start_level(i))=hkbo(i)
        dz=z_cup(i,start_level(i))-z_cup(i,start_level(i)-1)
        do k=start_level(i)+1,ktf-2
           dz=z_cup(i,k)-z_cup(i,k-1)
           hcot(i,k)=( (1.-0.5*entr_rate_2d(i,k-1)*dz)*hcot(i,k-1) &
                      + entr_rate_2d(i,k-1)*dz*heo(i,k-1))/ &
                      (1.+0.5*entr_rate_2d(i,k-1)*dz)
           if(k >= kbcon(i)) dby(k)=dby(k-1)+(hcot(i,k)-heso_cup(i,k))*dz
           if(k >= kbcon(i)) dbm(k)=hcot(i,k)-heso_cup(i,k)
        enddo
        ktopdby(i)=maxloc(dby(:),1)
        kklev=maxloc(dbm(:),1)
        do k=maxloc(dby(:),1)+1,ktf-2
          if(dby(k).lt.dbythresh*maxval(dby))then
              kfinalzu=k - 1
              ktop(i)=kfinalzu
              go to 412
          endif
        enddo
        kfinalzu=ktf-2
        ktop(i)=kfinalzu
412 continue
        if(kfinalzu.le.kbcon(i)+2)then
              ierr(i)=41
              ktop(i)= 0
        else
           call get_zu_zd_pdf_fim(kklev,p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"UP",ierr(i),k22(i), &
            kfinalzu+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kstabi(i),csum(i),pmin_lev(i))
        endif
      endif
      if ( name == 'mid' ) then
       if(ktop(i) <= kbcon(i)+2)then
              ierr(i)=41
              ktop(i)= 0
       else
           kfinalzu=ktop(i)
           ktopdby(i)=ktop(i)+1
          call get_zu_zd_pdf_fim(kklev,p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"MID",ierr(i),k22(i), &
           ktopdby(i)+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
       endif
      endif
      if ( name == 'shallow' ) then
       if(ktop(i) <= kbcon(i)+2)then
           ierr(i)=41
           ktop(i)= 0
       else
           kfinalzu=ktop(i)
           ktopdby(i)=ktop(i)+1
           call get_zu_zd_pdf_fim(kbcon(i),p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"SH2",ierr(i),k22(i), &
             ktopdby(i)+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kpbl(i),csum(i),pmin_lev(i))
         endif
         endif
     ENDDO
  END SUBROUTINE rates_up_pdf
 SUBROUTINE get_zu_zd_pdf_fim(kklev,p,rand_vmas,zubeg,ipr,xland,zuh2,draft,ierr,kb,kt,zu,kts,kte,ktf,max_mass,kpbli,csum,pmin_lev)
 implicit none
 real, parameter :: beta_deep=1.3,g_beta_deep=0.8974707
 real, parameter :: beta_sh=2.5,g_beta_sh=1.329340
 real, parameter :: beta_mid=1.3,g_beta_mid=0.8974707
 real, parameter :: beta_dd=4.0,g_beta_dd=6.
 integer, intent(in) ::ipr,xland,kb,kklev,kt,kts,kte,ktf,kpbli,csum,pmin_lev
 real, intent(in) ::max_mass,zubeg
 real, intent(inout) :: zu(kts:kte)
 real, intent(in) :: p(kts:kte)
 real :: zuh(kts:kte),zuh2(1:40)
 integer, intent(inout) :: ierr
 character*(*), intent(in) ::draft
 integer :: k1,kk,k,kb_adj,kpbli_adj
 real :: krmax,kratio,tunning,FZU,rand_vmas,lev_start
 real :: a,b,x1,y1,g_a,g_b,alpha2,g_alpha2
        real, dimension(30) :: alpha,g_alpha
        data (alpha(k),k=4,27)/3.699999, &
                      3.024999,2.559999,2.249999,2.028571,1.862500, &
                      1.733333,1.630000,1.545454,1.475000,1.415385, &
                      1.364286,1.320000,1.281250,1.247059,1.216667, &
                      1.189474,1.165000,1.142857,1.122727,1.104348, &
                      1.087500,1.075000,1.075000/
        data (g_alpha(k),k=4,27)/4.170645, &
                      2.046925 , 1.387837, 1.133003, 1.012418,0.9494680, &
                      0.9153771,0.8972442,0.8885444,0.8856795,0.8865333, &
                      0.8897996,0.8946404,0.9005030,0.9070138,0.9139161, &
                      0.9210315,0.9282347,0.9354376,0.9425780,0.9496124, &
                      0.9565111,0.9619183,0.9619183/
        alpha(1:3)=alpha(4)
        g_alpha(1:3)=g_alpha(4)
        alpha(28:30)=alpha(27)
        g_alpha(28:30)=g_alpha(27)
 zu(:)=0.0
 zuh(:)=0.0
   kb_adj=max(kb,2)
 IF(draft == "UP") then
   lev_start=min(.9,.4+csum*.013)
   kb_adj=max(kb,2)
   tunning=p(kt)+(p(kpbli)-p(kt))*lev_start
   tunning =min(0.95, (tunning-p(kb_adj))/(p(kt)-p(kb_adj)))
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_deep -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif
    fzu = gamma(alpha2 + beta_deep)/(gamma(alpha2)*gamma(beta_deep))
      zu(kb_adj)=zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj))
      zu(k) = zubeg+FZU*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_deep-1.0)
   enddo
   if(zu(kpbli).gt.0.) &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
     kb_adj=max(2,kb_adj)
     do k=kts,kb_adj-1
       zu(k)=0.
     enddo
122 format(1x,i4,1x,f8.1,1x,f6.2)
 ELSEIF(draft == "SH2") then
   k=kklev
   if(kpbli.gt.5)k=kpbli
   tunning =min(0.95, (p(k)-p(kb_adj))/(p(kt)-p(kb_adj)))
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_sh -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif
    fzu = gamma(alpha2 + beta_sh)/(g_alpha2*g_beta_sh)
      zu(kb_adj) = zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj))
      zu(k) = zubeg+FZU*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_sh-1.0)
   enddo
   if(zu(kpbli).gt.0.) &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
 ELSEIF(draft == "MID") then
   kb_adj=max(kb,2)
   tunning=.5*(p(kt)+p(kpbli))
   tunning =min(0.95, (tunning-p(kb_adj))/(p(kt)-p(kb_adj)))
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_mid -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif
    fzu = gamma(alpha2 + beta_mid)/(g_alpha2*g_beta_mid)
      zu(kb_adj) = zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj))
      zu(k) = zubeg+FZU*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_mid-1.0)
   enddo
   if(zu(kpbli).gt.0.) &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
     kb_adj=max(2,kb_adj)
     do k=kts,kb_adj-1
       zu(k)=0.
     enddo
 ELSEIF(draft == "DOWN" .or. draft == "DOWNM") then
   tunning=p(kb)
   tunning =min(0.95, (tunning-p(1))/(p(kt)-p(1)))
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_dd -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif
   fzu = gamma(alpha2 + beta_dd)/(g_alpha2*g_beta_dd)
  zu(:)=0.
  do k=2,min(kte,kt-1)
      kratio= (p(k)-p(1))/(p(kt)-p(1))
      zu(k) = fzu*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_dd-1.0)
   enddo
    fzu=maxval(zu(kts:min(ktf,kt-1)))
   if(fzu.gt.0.) &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/fzu
     zu(1)=0.
   do k=1,kb-2
     zu(kb-k)=zu(kb-k+1)-zu(kb)*(p(kb-k)-p(kb-k+1))/(p(1)-p(kb))
   enddo
     zu(1)=0.
  ENDIF
  END SUBROUTINE get_zu_zd_pdf_fim
  SUBROUTINE cup_up_aa1bl(aa0,t,tn,q,qo,dtime, &
              z_cup,zu,dby,gamma_cup,t_cup, &
              kbcon,ktop,ierr, &
              itf,ktf, &
              its,ite, kts,kte )
   IMPLICIT NONE
     integer &
        ,intent (in ) :: &
        itf,ktf, &
        its,ite, kts,kte
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        z_cup,zu,gamma_cup,t_cup,dby,t,tn,q,qo
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kbcon,ktop
     real, intent(in) :: dtime
     integer, dimension (its:ite) &
        ,intent (inout) :: &
        ierr
     real, dimension (its:ite) &
        ,intent (out ) :: &
        aa0
     integer :: &
        i,k
     real :: &
        dz,dA
        DO i=its,itf
         AA0(I)=0.
        ENDDO
        DO 100 i=its,itf
        DO 100 k=kts,kbcon(i)
        IF(ierr(i).ne.0 )GO TO 100
          dz = (z_cup (i,k+1)-z_cup (i,k))*g
          da = dz*(tn(i,k)*(1.+0.608*qo(i,k))-t(i,k)*(1.+0.608*q(i,k)))/dtime
         AA0(I)=AA0(I)+dA
100 CONTINUE
 END SUBROUTINE cup_up_aa1bl
 SUBROUTINE get_inversion_layers(ierr,p_cup,t_cup,z_cup,qo_cup,qeso_cup,k_inv_layers,&
                     kstart,kend,dtempdz,itf,ktf,its,ite, kts,kte)
        IMPLICIT NONE
        integer ,intent (in ) :: itf,ktf,its,ite,kts,kte
        integer, dimension (its:ite) ,intent (in ) :: ierr,kstart,kend
        integer, dimension (its:ite) :: kend_p3
        real, dimension (its:ite,kts:kte), intent (in ) :: p_cup,t_cup,z_cup,qo_cup,qeso_cup
        real, dimension (its:ite,kts:kte), intent (out) :: dtempdz
        integer, dimension (its:ite,kts:kte), intent (out) :: k_inv_layers
        real :: dp,l_mid,l_shal,first_deriv(kts:kte),sec_deriv(kts:kte)
        integer:: ken,kadd,kj,i,k,ilev,kk,ix,k800,k550,mid,shal
        l_mid=300.
        l_shal=100.
        k_inv_layers(:,:) = 1
         do i = its,itf
           if(ierr(i) == 0)then
           sec_deriv(:)=0.
           kend_p3(i)=kend(i)+3
           DO k = kts+1,kend_p3(i)+4
            first_deriv(k)= (t_cup(i,k+1)-t_cup(i,k-1))/(z_cup(i,k+1)-z_cup(i,k-1))
            dtempdz(i,k)=first_deriv(k)
               enddo
           DO k = kts+2,kend_p3(i)+3
            sec_deriv(k)= (first_deriv(k+1)-first_deriv(k-1))/(z_cup(i,k+1)-z_cup(i,k-1))
            sec_deriv(k)= abs(sec_deriv(k))
           enddo
         ilev=max(kts+3,kstart(i)+1)
         ix=1
         k=ilev
         DO WHILE (ilev < kend_p3(i))
           do kk=k,kend_p3(i)+2
             if(sec_deriv(kk) < sec_deriv(kk+1) .and. &
                sec_deriv(kk) < sec_deriv(kk-1) ) then
              k_inv_layers(i,ix)=kk
              ix=min(5,ix+1)
              ilev=kk+1
              exit
             endif
              ilev=kk+1
               enddo
           k=ilev
         ENDDO
         kadd=0
         ken=maxloc(k_inv_layers(i,:),1)
         do k=1,ken
           kk=k_inv_layers(i,k+kadd)
           if(kk.eq.1)exit
           if( dtempdz(i,kk) < dtempdz(i,kk-1) .and. &
               dtempdz(i,kk) < dtempdz(i,kk+1) ) then
               kadd=kadd+1
                do kj = k,ken
               if(k_inv_layers(i,kj+kadd).gt.1)k_inv_layers(i,kj) = k_inv_layers(i,kj+kadd)
               if(k_inv_layers(i,kj+kadd).eq.1)k_inv_layers(i,kj) = 1
                enddo
           endif
         ENDDO
        endif
        ENDDO
100 format(1x,16i3)
        do i = its,itf
         if(ierr(i) /= 0) cycle
         sec_deriv(:)=1.e9
         do k=1,maxloc(k_inv_layers(i,:),1)
           dp=p_cup(i,k_inv_layers(i,k))-p_cup(i,kstart(i))
           sec_deriv(k)=abs(dp)-l_shal
         enddo
         k800=minloc(abs(sec_deriv),1)
        sec_deriv(:)=1.e9
         do k=1,maxloc(k_inv_layers(i,:),1)
           dp=p_cup(i,k_inv_layers(i,k))-p_cup(i,kstart(i))
           sec_deriv(k)=abs(dp)-l_mid
         enddo
         k550=minloc(abs(sec_deriv),1)
         shal=1
         mid=2
         k_inv_layers(i,shal)=k_inv_layers(i,k800)
         k_inv_layers(i,mid )=k_inv_layers(i,k550)
         k_inv_layers(i,mid+1:kte)=-1
        ENDDO
 END SUBROUTINE get_inversion_layers
 FUNCTION DERIV3(xx, xi, yi, ni, m)
    implicit none
    integer, parameter :: n=3
    integer ni, m,i, j, k, ix
    real:: deriv3, xx
    real:: xi(ni), yi(ni), x(n), f(n)
    if (m > 2) then
      deriv3 = 0.0
      return
    end if
    if (xx < xi(1) .or. xx > xi(ni)) then
      deriv3 = 0.0
      stop "problems with finding the 2nd derivative"
    end if
    i = 1
    j = ni
    do while (j > i+1)
      k = (i+j)/2
      if (xx < xi(k)) then
        j = k
      else
        i = k
      end if
    end do
      i = i + 1 - n/2
    if (i < 1) i=1
    if (i + n > ni) i=ni-n+1
    ix = i
    do i=1,n
      f(i) = yi(ix+i-1)
      x(i) = xi(ix+i-1)
    end do
    if (m == 1) then
        deriv3 = (2.0*xx - (x(2)+x(3)))*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + (2.0*xx - (x(1)+x(3)))*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + (2.0*xx - (x(1)+x(2)))*f(3)/((x(3)-x(1))*(x(3)-x(2)))
      else
        deriv3 = 2.0*f(1)/((x(1)-x(2))*(x(1)-x(3)))
        deriv3 = deriv3 + 2.0*f(2)/((x(2)-x(1))*(x(2)-x(3)))
        deriv3 = deriv3 + 2.0*f(3)/((x(3)-x(1))*(x(3)-x(2)))
    end if
 END FUNCTION DERIV3
  SUBROUTINE get_lateral_massflux(itf,ktf, its,ite, kts,kte &
                                  ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d &
                                  ,up_massentro, up_massdetro ,up_massentr, up_massdetr &
                                  ,draft,kbcon,k22,up_massentru,up_massdetru,lambau)
     Implicit none
     character *(*), intent (in) :: draft
     integer, intent(in):: itf,ktf, its,ite, kts,kte
     integer, intent(in) , dimension(its:ite) :: ierr,ktop,kbcon,k22
     real, intent(in), OPTIONAL , dimension(its:ite):: lambau
     real, intent(in) , dimension(its:ite,kts:kte) :: zo_cup,zuo
     real, intent(inout), dimension(its:ite,kts:kte) :: cd,entr_rate_2d
     real, intent( out), dimension(its:ite,kts:kte) :: up_massentro, up_massdetro &
                                                          ,up_massentr, up_massdetr
     real, intent( out), dimension(its:ite,kts:kte), OPTIONAL :: &
                                                          up_massentru,up_massdetru
     Integer :: i,k, incr1,incr2,turn
     REAL :: dz,trash,trash2
     do k=kts,kte
      do i=its,ite
         up_massentro(i,k)=0.
         up_massdetro(i,k)=0.
         up_massentr (i,k)=0.
         up_massdetr (i,k)=0.
      enddo
     enddo
     if(present(up_massentru) .and. present(up_massdetru))then
       do k=kts,kte
        do i=its,ite
          up_massentru(i,k)=0.
          up_massdetru(i,k)=0.
        enddo
       enddo
     endif
     DO i=its,itf
       if(ierr(i).eq.0)then
          do k=max(2,k22(i)+1),maxloc(zuo(i,:),1)
           dz=zo_cup(i,k)-zo_cup(i,k-1)
           up_massdetro(i,k-1)=cd(i,k-1)*dz*zuo(i,k-1)
           up_massentro(i,k-1)=zuo(i,k)-zuo(i,k-1)+up_massdetro(i,k-1)
           if(up_massentro(i,k-1).lt.0.)then
              up_massentro(i,k-1)=0.
              up_massdetro(i,k-1)=zuo(i,k-1)-zuo(i,k)
              if(zuo(i,k-1).gt.0.)cd(i,k-1)=up_massdetro(i,k-1)/(dz*zuo(i,k-1))
           endif
           if(zuo(i,k-1).gt.0.)entr_rate_2d(i,k-1)=(up_massentro(i,k-1))/(dz*zuo(i,k-1))
         enddo
         do k=maxloc(zuo(i,:),1)+1,ktop(i)
           dz=zo_cup(i,k)-zo_cup(i,k-1)
           up_massentro(i,k-1)=entr_rate_2d(i,k-1)*dz*zuo(i,k-1)
           up_massdetro(i,k-1)=zuo(i,k-1)+up_massentro(i,k-1)-zuo(i,k)
           if(up_massdetro(i,k-1).lt.0.)then
              up_massdetro(i,k-1)=0.
              up_massentro(i,k-1)=zuo(i,k)-zuo(i,k-1)
              if(zuo(i,k-1).gt.0.)entr_rate_2d(i,k-1)=(up_massentro(i,k-1))/(dz*zuo(i,k-1))
           endif
           if(zuo(i,k-1).gt.0.)cd(i,k-1)=up_massdetro(i,k-1)/(dz*zuo(i,k-1))
         enddo
         up_massdetro(i,ktop(i))=zuo(i,ktop(i))
         up_massentro(i,ktop(i))=0.
         do k=ktop(i)+1,ktf
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
         enddo
         do k=2,ktf-1
           up_massentr (i,k-1)=up_massentro(i,k-1)
           up_massdetr (i,k-1)=up_massdetro(i,k-1)
         enddo
         if(present(up_massentru) .and. present(up_massdetru))then
          turn=maxloc(zuo(i,:),1)
          do k=2,turn
           up_massentru(i,k-1)=up_massentro(i,k-1)+lambau(i)*up_massentro(i,k-1)
           up_massdetru(i,k-1)=up_massdetro(i,k-1)+lambau(i)*up_massentro(i,k-1)
          enddo
          do k=turn+1,ktf-1
           up_massentru(i,k-1)=up_massentro(i,k-1)+lambau(i)*up_massdetro(i,k-1)
           up_massdetru(i,k-1)=up_massdetro(i,k-1)+lambau(i)*up_massdetro(i,k-1)
          enddo
         endif
         trash=0.
         trash2=0.
         do k=k22(i)+1,ktop(i)
             trash2=trash2+entr_rate_2d(i,k)
         enddo
         do k=k22(i)+1,kbcon(i)
            trash=trash+entr_rate_2d(i,k)
         enddo
       endif
    ENDDO
 END SUBROUTINE get_lateral_massflux
   SUBROUTINE get_partition_liq_ice(ierr,tn,po_cup, p_liq_ice,melting_layer &
                                   ,itf,ktf,its,ite, kts,kte, cumulus )
   IMPLICIT NONE
     CHARACTER *(*), INTENT (IN) :: cumulus
     INTEGER ,INTENT (IN ) :: itf,ktf, its,ite, kts,kte
     REAL, INTENT (IN ), DIMENSION(its:ite,kts:kte) :: tn,po_cup
     INTEGER, INTENT (IN ), DIMENSION(its:ite) :: ierr
     REAL, INTENT (INOUT), DIMENSION(its:ite,kts:kte) :: p_liq_ice,melting_layer
     INTEGER :: i,k
     REAL :: dp
     REAL, DIMENSION(its:ite) :: norm
     REAL, PARAMETER :: T1=276.16
        p_liq_ice (:,:) = 1.
        melting_layer(:,:) = 0.
     IF(MELT_GLAC .and. cumulus == 'deep') then
          DO i=its,itf
           if(ierr(i).eq.0)then
           DO k=kts,ktf
             if (tn(i,k) <= T_ice) then
                p_liq_ice(i,k) = 0.
             elseif( tn(i,k) > T_ice .and. tn(i,k) < T_0) then
                p_liq_ice(i,k) = ((tn(i,k)-T_ice)/(T_0-T_ice))**2
             else
                p_liq_ice(i,k) = 1.
             endif
          ENDDO
          ENDIF
        ENDDO
          DO i=its,itf
           if(ierr(i).eq.0)then
            DO k=kts,ktf
             if (tn(i,k) <= T_0+1) then
                melting_layer(i,k) = 0.
             elseif( tn(i,k) > T_0+1 .and. tn(i,k) < T1) then
                melting_layer(i,k) = ((tn(i,k)-T_0+1)/(T1-T_0+1))**2
             else
                melting_layer(i,k) = 1.
             endif
             melting_layer(i,k) = melting_layer(i,k)*(1-melting_layer(i,k))
          ENDDO
          ENDIF
        ENDDO
        655 continue
        norm(:)=0.
          DO i=its,itf
           if(ierr(i).eq.0)then
           DO k=kts,ktf-1
             dp = 100.*(po_cup(i,k)-po_cup(i,k+1))
             norm(i) = norm(i) + melting_layer(i,k)*dp/g
          ENDDO
          endif
        ENDDO
        DO i=its,itf
           if(ierr(i).eq.0)then
         melting_layer(i,:)=melting_layer(i,:)/(norm(i)+1.e-6)*(100*(po_cup(i,kts)-po_cup(i,ktf))/g)
          endif
        ENDDO
     ELSE
        p_liq_ice (:,:) = 1.
        melting_layer(:,:) = 0.
     ENDIF
   END SUBROUTINE get_partition_liq_ice
   SUBROUTINE get_melting_profile(ierr,tn_cup,po_cup, p_liq_ice,melting_layer,qrco &
                                 ,pwo,edto,pwdo,melting &
                                 ,itf,ktf,its,ite, kts,kte, cumulus )
     IMPLICIT NONE
     CHARACTER *(*), INTENT (IN) :: cumulus
     INTEGER ,INTENT (IN ) :: itf,ktf, its,ite, kts,kte
     INTEGER ,INTENT (IN ), DIMENSION(its:ite) :: ierr
     REAL ,INTENT (IN ), DIMENSION(its:ite) :: edto
     REAL ,INTENT (IN ), DIMENSION(its:ite,kts:kte) :: tn_cup,po_cup,qrco,pwo &
                                                            ,pwdo,p_liq_ice,melting_layer
     REAL ,INTENT (INOUT), DIMENSION(its:ite,kts:kte) :: melting
     INTEGER :: i,k
     REAL :: dp
     REAL, DIMENSION(its:ite) :: norm,total_pwo_solid_phase
     REAL, DIMENSION(its:ite,kts:kte) :: pwo_solid_phase,pwo_eff
     IF(MELT_GLAC .and. cumulus == 'deep') then
        DO i=its,itf
            if(ierr(i) > 0) melting(i,:) = 0.
        ENDDO
       total_pwo_solid_phase(:)=0.
       DO k=kts,ktf-1
          DO i=its,itf
           if(ierr(i) /= 0) cycle
             dp = 100.*(po_cup(i,k)-po_cup(i,k+1))
             pwo_eff(i,k) = 0.5*(pwo(i,k)+pwo(i,k+1) + edto(i)*(pwdo(i,k)+pwdo(i,k+1)))
             pwo_solid_phase(i,k) = (1.-p_liq_ice(i,k))*pwo_eff(i,k)
             total_pwo_solid_phase(i) = total_pwo_solid_phase(i)+pwo_solid_phase(i,k)*dp/g
          ENDDO
        ENDDO
        DO k=kts,ktf
          DO i=its,itf
           if(ierr(i) /= 0) cycle
             melting(i,k) = melting_layer(i,k)*(total_pwo_solid_phase(i)/(100*(po_cup(i,kts)-po_cup(i,ktf))/g))
          ENDDO
        ENDDO
     ELSE
        melting (:,:) = 0.
     ENDIF
   END SUBROUTINE get_melting_profile
 SUBROUTINE get_cloud_top(name,ktop,ierr,p_cup,entr_rate_2d,hkbo,heo,heso_cup,z_cup, &
                         kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,klcl,hcot)
     implicit none
     integer, intent(in) :: its,ite,itf,kts,kte,ktf
     real, dimension (its:ite,kts:kte),intent (inout) :: entr_rate_2d,zuo
     real, dimension (its:ite,kts:kte),intent (in) ::p_cup, heo,heso_cup,z_cup
     real, dimension (its:ite),intent (in) :: hkbo
     integer, dimension (its:ite),intent (in) :: kstabi,k22,kbcon,kpbl,klcl
     integer, dimension (its:ite),intent (inout) :: ierr,ktop
     real, dimension (its:ite,kts:kte) :: hcot
     character *(*), intent (in) :: name
     real :: dz,dh, dbythresh
     real :: dby(kts:kte)
     integer :: i,k,ipr,kdefi,kstart,kbegzu,kfinalzu
     integer, dimension (its:ite) :: start_level
     integer,Parameter :: FIND_KTOP_OPTION = 1
     dbythresh=1.
     if(name == 'shallow'.or. name == 'mid')then
         dbythresh=1.0
     endif
     DO i=its,itf
       kfinalzu=ktf-2
       ktop(i)=kfinalzu
       if(ierr(i).eq.0)then
       dby (kts:kte)=0.0
       start_level(i)=kbcon(i)
       hcot(i,kts:start_level(i))=hkbo(i)
       dz=z_cup(i,start_level(i))-z_cup(i,start_level(i)-1)
       dby(start_level(i))=(hcot(i,start_level(i))-heso_cup(i,start_level(i)))*dz
       do k=start_level(i)+1,ktf-2
           dz=z_cup(i,k)-z_cup(i,k-1)
           hcot(i,k)=( (1.-0.5*entr_rate_2d(i,k-1)*dz)*hcot(i,k-1) &
                              +entr_rate_2d(i,k-1)*dz *heo (i,k-1) )/ &
                       (1.+0.5*entr_rate_2d(i,k-1)*dz)
           dby(k)=dby(k-1)+(hcot(i,k)-heso_cup(i,k))*dz
       enddo
       if(FIND_KTOP_OPTION==0) then
        do k=maxloc(dby(:),1),ktf-2
          if(dby(k).lt.dbythresh*maxval(dby))then
              kfinalzu = k - 1
              ktop(i) = kfinalzu
              go to 412
          endif
        enddo
        412 continue
       else
         do k=start_level(i)+1,ktf-2
          if(hcot(i,k) < heso_cup(i,k) )then
              kfinalzu = k - 1
              ktop(i) = kfinalzu
              exit
          endif
        enddo
       endif
       if(kfinalzu.le.kbcon(i)+1) ierr(i)=41
      ENDIF
     ENDDO
  END SUBROUTINE get_cloud_top
END MODULE module_cu_gf_deep
