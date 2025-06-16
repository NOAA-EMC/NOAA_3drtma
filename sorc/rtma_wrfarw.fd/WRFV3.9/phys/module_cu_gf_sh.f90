MODULE module_cu_gf_sh
    real, parameter:: c1_shal=0.
    real, parameter:: g =9.81
    real, parameter:: cp =1004.
    real, parameter:: xlv=2.5e6
    real, parameter:: r_v=461.
    real, parameter:: c0_shal=.001
    real, parameter:: fluxtune=1.5
contains
  SUBROUTINE CUP_gf_sh ( &
                         zo,T,Q,Z1,TN,QO,PO,PSUR,dhdt,kpbl,rho, &
                         hfx,qfx,xland,ichoice,tcrit,dtime, &
                         zuo,xmb_out,kbcon,ktop,k22,ierr,ierrc, &
                         OUTT,OUTQ,OUTQC,cnvwt,pre,cupclw, &
                         itf,ktf,its,ite, kts,kte,ipr)
  use module_cu_gf_deep,only:cup_env,cup_env_clev,get_cloud_bc,cup_minimi, &
                      get_inversion_layers,rates_up_pdf,get_cloud_bc, &
                      cup_up_aa0,cup_kbcon,get_lateral_massflux
     implicit none
     integer &
        ,intent (in ) :: &
        itf,ktf, &
        its,ite, kts,kte,ipr
     logical :: MAKE_CALC_FOR_XK = .true.
     integer, intent (in ) :: &
        ichoice
     real, dimension (its:ite,kts:kte) &
        ,intent (inout ) :: &
        cnvwt,OUTT,OUTQ,OUTQC,cupclw,zuo
     real, dimension (its:ite) &
        ,intent (out ) :: &
        xmb_out
     integer, dimension (its:ite) &
        ,intent (inout ) :: &
        ierr
     integer, dimension (its:ite) &
        ,intent (out ) :: &
        kbcon,ktop,k22
     integer, dimension (its:ite) &
        ,intent (in ) :: &
        kpbl
     real, dimension (its:ite,kts:kte) &
        ,intent (in ) :: &
        T,PO,tn,dhdt,rho
     real, dimension (its:ite,kts:kte) &
        ,intent (inout) :: &
         Q,QO
     real, dimension (its:ite) &
        ,intent (in ) :: &
        xland,Z1,PSUR,hfx,qfx
       real &
        ,intent (in ) :: &
        dtime,tcrit
     real, dimension (its:ite,kts:kte) :: &
        entr_rate_2d,he,hes,qes,z, &
        heo,heso,qeso,zo, &
        xhe,xhes,xqes,xz,xt,xq, &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup, &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup, &
        tn_cup, &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup, &
        xt_cup,dby,hc,zu, &
        dbyo,qco,pwo,hco,qrco, &
        dbyt,xdby,xhc,xzu, &
        cd,DELLAH,DELLAQ,DELLAT,DELLAQC
     real, dimension (its:ite) :: &
       zws,ztexec,zqexec,pre,AA1,AA0,XAA0,HKB, &
       flux_tun,HKBO,XHKB, &
       rand_vmas,xmbmax,XMB, &
       cap_max,entr_rate, &
       cap_max_increment
     integer, dimension (its:ite) :: &
       kstabi,xland1,KBMAX,ktopx
     integer :: &
       I,K,ki
     real :: &
      dz,mbdt,zkbmax, &
      cap_maxs,trash,trash2,frh
      real buo_flux,pgeoh,dp,entup,detup,totmas
     real xff_shal(3),blqe,xkshal
     character*50 :: ierrc(its:ite)
     real, dimension (its:ite,kts:kte) :: &
       up_massentr,up_massdetr,up_massentro,up_massdetro
     real :: C_up,x_add,qaver
     real, dimension (its:ite,kts:kte) :: dtempdz
     integer, dimension (its:ite,kts:kte) :: k_inv_layers
     integer, dimension (its:ite) :: start_level
     start_level(:)=0
     rand_vmas(:)=0.
     flux_tun=fluxtune
      do i=its,itf
        xland1(i)=int(xland(i)+.001)
        ktopx(i)=0
        if(xland(i).gt.1.5 .or. xland(i).lt.0.5)then
            xland1(i)=0
        endif
        pre(i)=0.
        xmb_out(i)=0.
        cap_max_increment(i)=25.
        ierrc(i)=" "
        entr_rate(i) = 9.e-5
      enddo
      do k=kts,ktf
      do i=its,itf
        up_massentro(i,k)=0.
        up_massdetro(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        qrco(i,k)=0.
        pwo(i,k)=0.
        cd(i,k)=1.*entr_rate(i)
        dellaqc(i,k)=0.
        cupclw(i,k)=0.
      enddo
      enddo
      cap_maxs=125.
      DO i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
      enddo
      do i=its,itf
          cap_max(i)=cap_maxs
          ztexec(i) = 0.
          zqexec(i) = 0.
          zws(i) = 0.
      enddo
      do i=its,itf
         buo_flux= (hfx(i)/cp+0.608*t(i,1)*qfx(i)/xlv)/rho(i,1)
         pgeoh = zo(i,2)*g
         zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,2)*g/t(i,1))
         if(zws(i) > TINY(pgeoh)) then
          zws(i) = 1.2*zws(i)**.3333
          ztexec(i) = MAX(flux_tun(i)*hfx(i)/(rho(i,1)*zws(i)*cp),0.0)
          zqexec(i) = MAX(flux_tun(i)*qfx(i)/xlv/(rho(i,1)*zws(i)),0.)
         endif
       zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,kpbl(i))*g/t(i,kpbl(i)))
       zws(i) = 1.2*zws(i)**.3333
       zws(i) = zws(i)*rho(i,kpbl(i))
      enddo
      zkbmax=3000.
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
      do i=its,itf
        if(ierr(i).eq.0)then
      do k=kts,ktf
        if(zo_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25 continue
      kbmax(i)=min(kbmax(i),ktf/2)
      endif
      enddo
       DO 36 i=its,itf
         if(kpbl(i).gt.3)cap_max(i)=po_cup(i,kpbl(i))
         IF(ierr(I) == 0)THEN
          k22(i)=maxloc(HEO_CUP(i,2:kbmax(i)),1)
          k22(i)=max(2,k22(i))
          IF(K22(I).GT.KBMAX(i))then
           ierr(i)=2
           ierrc(i)="could not find k22"
           ktop(i)=0
           k22(i)=0
           kbcon(i)=0
         endif
         endif
 36 CONTINUE
      do i=its,itf
       if(ierr(I).eq.0)then
             x_add = xlv*zqexec(i)+cp*ztexec(i)
             call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
             call get_cloud_bc(kte,heo_cup(i,1:kte),hkbo(i),k22(i),x_add)
       endif
      enddo
      do i=its,itf
      do k=kts,ktf
          dbyo(i,k)= 0.
      enddo
      enddo
      call cup_kbcon(ierrc,cap_max_increment,5,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           ztexec,zqexec, &
           0,itf,ktf, &
           its,ite, kts,kte, &
           z_cup,entr_rate,heo,0)
      call cup_minimi(HEso_cup,Kbcon,kbmax,kstabi,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
      call get_inversion_layers(ierr,p_cup,t_cup,z_cup,q_cup,qes_cup,k_inv_layers,&
                           kbcon,kstabi,dtempdz,itf,ktf,its,ite, kts,kte)
      DO i=its,itf
         entr_rate_2d(i,:)=entr_rate(i)
         IF(ierr(I) == 0)THEN
            start_level(i)=k22(i)
            x_add = xlv*zqexec(i)+cp*ztexec(i)
            call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
            if(kbcon(i).gt.ktf-4)then
                ierr(i)=231
            endif
            do k=kts,ktf
               frh = 2.*min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate(i)*(2.3-frh)
               cd(i,k)=entr_rate_2d(i,k)
            enddo
            ktop(i)=1
            if(k_inv_layers(i,1).gt.0 .and. &
               (po_cup(i,kbcon(i))-po_cup(i,k_inv_layers(i,1))).lt.200.)then
               ktop(i)=k_inv_layers(i,1)
            else
               do k=kbcon(i)+1,ktf
                  if((po_cup(i,kbcon(i))-po_cup(i,k)).gt.200.)then
                    ktop(i)=k
                    exit
                  endif
               enddo
            endif
         endif
      enddo
      call rates_up_pdf(rand_vmas,ipr,'shallow',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
           xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopx,ktopx,kbcon)
      do i=its,itf
        if(ierr(i).eq.0)then
           if(k22(i).gt.1)then
             do k=1,k22(i)-1
              zuo(i,k)=0.
              zu (i,k)=0.
              xzu(i,k)=0.
             enddo
           endif
           do k=maxloc(zuo(i,:),1),ktop(i)
             if(zuo(i,k).lt.1.e-6)then
               ktop(i)=k-1
               exit
             endif
           enddo
           do k=k22(i),ktop(i)
             xzu(i,k)= zuo(i,k)
              zu(i,k)= zuo(i,k)
           enddo
           do k=ktop(i)+1,ktf
             zuo(i,k)=0.
             zu (i,k)=0.
             xzu(i,k)=0.
           enddo
           k22(i)=max(2,k22(i))
        endif
      enddo
      CALL get_lateral_massflux(itf,ktf, its,ite, kts,kte &
                                ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d &
                                ,up_massentro, up_massdetro ,up_massentr, up_massdetr &
                                ,'shallow',kbcon,k22)
      do k=kts,ktf
      do i=its,itf
         hc(i,k)=0.
         qco(i,k)=0.
         qrco(i,k)=0.
         DBY(I,K)=0.
         hco(i,k)=0.
         DBYo(I,K)=0.
      enddo
      enddo
      do i=its,itf
       IF(ierr(I) /= 0) cycle
         do k=1,start_level(i)-1
            hc(i,k)=he_cup(i,k)
            hco(i,k)=heo_cup(i,k)
         enddo
         k=start_level(i)
         hc(i,k)=hkb(i)
         hco(i,k)=hkbo(i)
      enddo
      do 42 i=its,itf
        dbyt(i,:)=0.
        IF(ierr(I) /= 0) cycle
         do k=start_level(i)+1,ktop(i)
          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                         up_massentr(i,k-1)*he(i,k-1)) / &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          dby(i,k)=max(0.,hc(i,k)-hes_cup(i,k))
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                         up_massentro(i,k-1)*heo(i,k-1)) / &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
          DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
          dbyt(i,k)=dbyt(i,k-1)+dbyo(i,k)*dz
         enddo
       ki=maxloc(dbyt(i,:),1)
       if(ktop(i).gt.ki+1)then
         ktop(i)=ki+1
         zuo(i,ktop(i)+1:ktf)=0.
         zu(i,ktop(i)+1:ktf)=0.
         cd(i,ktop(i)+1:ktf)=0.
         up_massdetro(i,ktop(i))=zuo(i,ktop(i))
         up_massentro(i,ktop(i):ktf)=0.
         up_massdetro(i,ktop(i)+1:ktf)=0.
         entr_rate_2d(i,ktop(i)+1:ktf)=0.
       endif
         if(ktop(i).lt.kbcon(i)+1)then
            ierr(i)=5
            ierrc(i)='ktop is less than kbcon+1'
             go to 42
         endif
         if(ktop(i).gt.ktf-2)then
             ierr(i)=5
             ierrc(i)="ktop is larger than ktf-2"
             go to 42
         endif
         call get_cloud_bc(kte,qo_cup (i,1:kte),qaver,k22(i))
         qaver = qaver + zqexec(i)
         do k=1,start_level(i)-1
           qco (i,k)= qo_cup(i,k)
         enddo
         k=start_level(i)
         qco (i,k)= qaver
         do k=start_level(i)+1,ktop(i)
          trash=QESo_cup(I,K)+(1./XLV)*(GAMMAo_cup(i,k) &
                /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
          trash2 = qco(i,k-1)
          qco (i,k)= (trash2* ( zuo(i,k-1)-0.5*up_massdetr(i,k-1)) + &
                       up_massentr(i,k-1)*qo(i,k-1)) / &
                       (zuo(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          if(qco(i,k)>=trash ) then
              DZ=Z_cup(i,K)-Z_cup(i,K-1)
              qrco(i,k)= (qco(i,k)-trash)/(1.+(c0_shal+c1_shal)*dz)
              pwo(i,k)=c0_shal*dz*qrco(i,k)*zuo(i,k)
              qco (i,k)= trash+qrco(i,k)
          else
              qrco(i,k)= 0.0
          endif
          cupclw(i,k)=qrco(i,k)
         enddo
         trash=0.
         trash2=0.
         do k=k22(i)+1,ktop(i)
          dp=100.*(po_cup(i,k)-po_cup(i,k+1))
          cnvwt(i,k)=zuo(i,k)*cupclw(i,k)*g/dp
          trash2=trash2+entr_rate_2d(i,k)
          qco(i,k)=qco(i,k)-qrco(i,k)
         enddo
         do k=k22(i)+1,max(kbcon(i),k22(i)+1)
          trash=trash+entr_rate_2d(i,k)
         enddo
         do k=ktop(i)+1,ktf-1
           hc (i,k)=hes_cup (i,k)
           hco (i,k)=heso_cup(i,k)
           qco (i,k)=qeso_cup(i,k)
           qrco(i,k)=0.
           dby (i,k)=0.
           dbyo(i,k)=0.
           zu (i,k)=0.
           xzu (i,k)=0.
           zuo (i,k)=0.
         enddo
 42 continue
      IF(MAKE_CALC_FOR_XK) THEN
        call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
            kbcon,ktop,ierr, &
            itf,ktf, its,ite, kts,kte)
        call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
            kbcon,ktop,ierr, &
            itf,ktf, its,ite, kts,kte)
        do i=its,itf
          if(ierr(i) == 0)then
           if(aa1(i) <= 0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
         endif
       enddo
      ENDIF
      do k=kts,kte
       do i=its,itf
        dellah(i,k)=0.
        dellaq(i,k)=0.
       enddo
      enddo
      trash2=0.
      do i=its,itf
        if(ierr(i).eq.0)then
         do k=k22(i),ktop(i)
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
            totmas=detup-entup+zuo(i,k+1)-zuo(i,k)
            if(abs(totmas).gt.1.e-6)then
               write(0,*)'*********************',i,k,totmas
               write(0,*)k22(i),kbcon(i),ktop(i)
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k) =-(zuo(i,k+1)*(hco(i,k+1)-heo_cup(i,k+1) )- &
                           zuo(i,k )*(hco(i,k )-heo_cup(i,k ) ))*g/dp
            dz=zo_cup(i,k+1)-zo_cup(i,k)
            if(k.lt.ktop(i))then
             dellaqc(i,k)= zuo(i,k)*c1_shal*qrco(i,k)*dz/dp*g
            else
             dellaqc(i,k)= detup*qrco(i,k) *g/dp
            endif
            C_up = dellaqc(i,k)+(zuo(i,k+1)* qrco(i,k+1) - &
                                  zuo(i,k )* qrco(i,k ) )*g/dp
            dellaq(i,k) =-(zuo(i,k+1)*(qco(i,k+1)-qo_cup(i,k+1) ) - &
                           zuo(i,k )*(qco(i,k )-qo_cup(i,k ) ) )*g/dp &
                           - C_up - 0.5*(pwo (i,k)+pwo (i,k+1))*g/dp
          enddo
        endif
      enddo
      mbdt=.5
      do k=kts,ktf
       do i=its,itf
         dellat(i,k)=0.
         if(ierr(i)/=0)cycle
         xhe(i,k)=dellah(i,k)*mbdt+heo(i,k)
         xq (i,k)=max(1.e-16,(dellaq(i,k)+dellaqc(i,k))*mbdt+qo(i,k))
         dellat(i,k)=(1./cp)*(dellah(i,k)-xlv*(dellaq(i,k)))
         xt (i,k)= (-dellaqc(i,k)*xlv/cp+dellat(i,k))*mbdt+tn(i,k)
         xt (i,k)= max(190.,xt(i,k))
       enddo
      enddo
      do i=its,itf
       if(ierr(i).eq.0)then
        xhe(i,ktf)=heo(i,ktf)
        xq(i,ktf)=qo(i,ktf)
        xt(i,ktf)=tn(i,ktf)
       endif
      enddo
     IF(MAKE_CALC_FOR_XK) THEN
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
        xzu(i,1:ktf)=zuo(i,1:ktf)
        do k=start_level(i)+1,ktop(i)
         xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)+ &
                          up_massentro(i,k-1)*xhe(i,k-1)) / &
                          (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
         xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
        enddo
        do k=ktop(i)+1,ktf
           xHC (i,K)=xhes_cup(i,k)
           xDBY(I,K)=0.
           xzu (i,k)=0.
        enddo
       endif
      enddo
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr, &
           itf,ktf, &
           its,ite, kts,kte)
     ENDIF
       do i=its,itf
        xmb(i)=0.
        xff_shal(1:3)=0.
        if(ierr(i).eq.0)then
          xmbmax(i)=1.0
          xkshal=(xaa0(i)-aa1(i))/mbdt
             if(xkshal.le.0.and.xkshal.gt.-.01*mbdt) &
                           xkshal=-.01*mbdt
             if(xkshal.gt.0.and.xkshal.lt.1.e-2) &
                           xkshal=1.e-2
          xff_shal(1)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))
          xff_shal(2)=.03*zws(i)
          blqe=0.
          trash=0.
          do k=1,kpbl(i)
                blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
          enddo
          trash=max((hc(i,kbcon(i))-he_cup(i,kbcon(i))),1.e1)
          xff_shal(3)=max(0.,blqe/trash)
          xff_shal(3)=min(xmbmax(i),xff_shal(3))
          xmb(i)=(xff_shal(1)+xff_shal(2)+xff_shal(3))/3.
          xmb(i)=min(xmbmax(i),xmb(i))
          if(ichoice > 0)xmb(i)=min(xmbmax(i),xff_shal(ichoice))
          if(xmb(i) <= 0.)then
             ierr(i)=21
             ierrc(i)="21"
          endif
        endif
        if(ierr(i).ne.0)then
           k22 (i)=0
           kbcon(i)=0
           ktop (i)=0
           xmb (i)=0.
           outt (i,:)=0.
           outq (i,:)=0.
           outqc(i,:)=0.
        else if(ierr(i).eq.0)then
          xmb_out(i)=xmb(i)
          pre(i)=0.
          do k=2,ktop(i)
           outt (i,k)= dellat (i,k)*xmb(i)
           outq (i,k)= dellaq (i,k)*xmb(i)
           outqc(i,k)= dellaqc(i,k)*xmb(i)
           pre (i) = pre(i)+pwo(i,k)*xmb(i)
          enddo
        endif
       enddo
   END SUBROUTINE CUP_gf_sh
END MODULE module_cu_gf_sh
