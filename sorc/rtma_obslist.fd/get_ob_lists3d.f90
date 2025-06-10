         subroutine get_ob_lists3d()
!
!-------------------------------------------------------------------------
!
!  abstract: generate formatted "obs_listing" files from
!            the diagnostic files for 3drtma
!
!  to compile:
!  ifort -free -convert big_endian -c get_ob_lists3d.f90
!  
!  AUTHOR: 
!  Manuel Pondeca        date: 2022-03-18
! 
!  2024-03-20 pondeca - add subtype output
!  2025-02-03 pondeca - add gust and howv
!  2025-06-02 pondeca/gang - add vis
!
!  REVISION HISTORY
!  2022-03-18 Manuel Pondeca
!-------------------------------------------------------------------------
         implicit none

         character(1),parameter:: cblank1=' '
         real(4),parameter:: spval=-9999.
         real(4),parameter:: spval2=-99.

         integer(4),parameter::nflds=10 !t,q,ps,u,v,w,spd,gust,howv,vis

         integer(4),parameter::lun0=7
         integer(4),parameter::lun_t=10
         integer(4),parameter::lun_q=11
         integer(4),parameter::lun_ps=12
         integer(4),parameter::lun_u=13
         integer(4),parameter::lun_v=14
         integer(4),parameter::lun_w=15
         integer(4),parameter::lun_spd=16
         integer(4),parameter::lun_gust=17
         integer(4),parameter::lun_howv=18
         integer(4),parameter::lun_vis=19
         integer(4),parameter::nmplower=-10000
         integer(4),parameter::nmpupper=+100

         integer(4) miter

         character(8),allocatable,dimension(:):: cdiagbuf
         character(8),allocatable,dimension(:):: cprvstg
         character(8),allocatable,dimension(:):: csprvstg
         character(3) otype
         character(8) cstation
         character(8) cprovider,csubprovider
         character(8) cproviderp,csubproviderp
         character(3) clun3
         character(2) ctvts
         character(80) filename
         character(15) clistorig
         character(18) caux18
         character(15) cblank15
         character(1)  chvar1
         character(10) chvar10
         character(90) cfmt

         integer(4) idate,nchar,nreal,i,ii0,mypegsi,lun,n,m,k,naux,n1
         integer(4) itype,isubtype
         integer(4) ntrjs,nqrjs,nprjs,nwrjs
         integer(4) nspdrjs,ngustrjs,nhowvrjs,nvisrjs
         integer(4) nrjsmax,nrjs0 

         integer(4) ntot(nflds),nmp(nmplower:nmpupper,0:3,nflds)
         integer(4) ngross(nflds)
         integer(4) kuse,ifld,kfrac
         integer(4) kk
         integer(4) jiter,ierror


         real(4),allocatable,dimension(:,:)::rdiagbuf
         real(4) rlat,rlon,oberr,oberr2,ob,ob_model,ddiff, & 
                 dudiff,dvdiff,rmuse
         real(4) uob,uob_model,vob,vob_model,rfactor,qtflg
         real(4) wob,wob_model
         real(8) da8,alat18,elon18,elonv8,alatan8
         real(4) shgt0  !station height
         real(4) obpres0!observation pressure
         real(4) hgt0   !observation elevation
         real(4) hgt    !model terrain at ob location
         real(4) slm    !dominant surface type
         real(4) dtime
         real(4) percnumber
         real(4) rsign
         character(90),allocatable,dimension(:):: t_rjlist
         character(90),allocatable,dimension(:):: q_rjlist
         character(90),allocatable,dimension(:):: p_rjlist
         character(90),allocatable,dimension(:):: w_rjlist
         character(90),allocatable,dimension(:):: spd_rjlist
         character(90),allocatable,dimension(:):: gust_rjlist
         character(90),allocatable,dimension(:):: howv_rjlist
         character(90),allocatable,dimension(:):: vis_rjlist
         character(90),allocatable,dimension(:):: rjlist0
         logical tlistexist,qlistexist,plistexist,wlistexist
         logical spdlistexist,gustlistexist,howvlistexist,vislistexist
         logical sfctype,near_sfcob
         logical lprvinfoexist
         logical lrjlistapplicable
         logical fexist

        
         namelist /outerloop_info/miter 

         data miter /2/    !number of gsi minimization outerloops
!------------------------------------------------------------------------------------------------------
!==> get content of reject lists
!------------------------------------------------------------------------------------------------------
         filename='t_rejectlist'
         call rjlist_obcount(filename,tlistexist,ntrjs)
         allocate(t_rjlist(max(ntrjs,1)))
         if (tlistexist .and. ntrjs > 0) then
            call readin_rjlist(filename,t_rjlist,ntrjs)
         endif
         print*,'in get_ob_lists3d: tlistexist,ntrjs=',tlistexist,ntrjs

         filename='q_rejectlist'
         call rjlist_obcount(filename,qlistexist,nqrjs)
         allocate(q_rjlist(max(nqrjs,1)))
         if (qlistexist .and. nqrjs > 0) then
            call readin_rjlist(filename,q_rjlist,nqrjs)
         endif
         print*,'in get_ob_lists3d: qlistexist,nqrjs=',qlistexist,nqrjs

         filename='p_rejectlist'
         call rjlist_obcount(filename,plistexist,nprjs)
         allocate(p_rjlist(max(nprjs,1)))
         if (plistexist .and. nprjs > 0) then
            call readin_rjlist(filename,p_rjlist,nprjs)
         endif
         print*,'in get_ob_lists3d: plistexist,nprjs=',plistexist,nprjs

         filename='w_rejectlist'
         call rjlist_obcount(filename,wlistexist,nwrjs)
         allocate(w_rjlist(max(nwrjs,1)))
         if (wlistexist .and. nwrjs > 0) then
            call readin_rjlist(filename,w_rjlist,nwrjs)
         endif
         print*,'in get_ob_lists3d: wlistexist,nwrjs=',wlistexist,nwrjs

         filename='spd_rejectlist'
         call rjlist_obcount(filename,spdlistexist,nspdrjs)
         allocate(spd_rjlist(max(nspdrjs,1)))
         if (spdlistexist .and. nspdrjs > 0) then
            call readin_rjlist(filename,spd_rjlist,nspdrjs)
         endif
         print*,'in get_ob_lists3d: spdlistexist,nspdrjs=',spdlistexist,nspdrjs

         filename='gust_rejectlist'
         call rjlist_obcount(filename,gustlistexist,ngustrjs)
         allocate(gust_rjlist(max(ngustrjs,1)))
         if (gustlistexist .and. ngustrjs > 0) then
            call readin_rjlist(filename,gust_rjlist,ngustrjs)
         endif
         print*,'in get_ob_lists3d: gustlistexist,ngustrjs=',gustlistexist,ngustrjs

         filename='howv_rejectlist'
         call rjlist_obcount(filename,howvlistexist,nhowvrjs)
         allocate(howv_rjlist(max(nhowvrjs,1)))
         if (howvlistexist .and. nhowvrjs > 0) then
            call readin_rjlist(filename,howv_rjlist,nhowvrjs)
         endif
         print*,'in get_ob_lists3d: howvlistexist,nhowvrjs=',howvlistexist,nhowvrjs

         filename='vis_rejectlist'
         call rjlist_obcount(filename,vislistexist,nvisrjs)
         allocate(vis_rjlist(max(nvisrjs,1)))
         if (vislistexist .and. nvisrjs > 0) then
            call readin_rjlist(filename,vis_rjlist,nvisrjs)
         endif
         print*,'in get_ob_lists3d: vislistexist,nvisrjs=',vislistexist,nvisrjs

         nrjsmax=max(ntrjs,nqrjs,nprjs,nwrjs,nspdrjs,ngustrjs,nhowvrjs,nvisrjs)
         allocate(rjlist0(max(nrjsmax,1)))
         !----------------------------------------------------------------------------------
         !==> get number of gsi outer loops
         !----------------------------------------------------------------------------------
         inquire(file='outerloop_info_input',exist=fexist)
         if (fexist) then 
             open (55,file='outerloop_info_input',form='unformatted')
             read (55,outerloop_info)
             close(55)
         endif
         print*,'in get_ob_lists:: miter=',miter

         !----------------------------------------------------------------------------------
         !==> loop over all diagnostics files and create the obs listing files
         !----------------------------------------------------------------------------------
         do 5000 jiter=1,miter+1
            if (jiter==1) clun3='ges'

            if (jiter>1) then
               if((jiter-1)==miter) then
                  clun3='anl'
                 else
                  write(clun3(1:2),'(i2.2)')jiter
                  clun3(3:3)=' '
               endif
            endif
            print*,'in get_ob_lists:: jiter,clun3=',jiter,trim(clun3)

            inquire(file='diag_conv_'//trim(clun3)//'.dat',exist=fexist)
            if (fexist) then 
                open (lun0,file='diag_conv_'//trim(clun3)//'.dat',form='unformatted')
             else
                cycle
            endif

            !----------------------------------------------------------------------------------
            !==> Initialize output files
            !----------------------------------------------------------------------------------

            call open_and_header_V2 (lun_t,       't',       clun3)
            call open_and_header_V2 (lun_q,       'q',       clun3)
            call open_and_header_V2 (lun_ps,      'ps',      clun3)
            call open_and_header_V2 (lun_u,       'u',       clun3)
            call open_and_header_V2 (lun_v,       'v',       clun3)
            call open_and_header_V2 (lun_w,       'w' ,      clun3)
            call open_and_header_V2 (lun_spd,     'spd',     clun3)
            call open_and_header_V2 (lun_gust,    'gust',    clun3)
            call open_and_header_V2 (lun_howv,    'howv',    clun3)
            call open_and_header_V2 (lun_vis,     'vis',     clun3)

            !----------------------------------------------------------------------------------

            do m=1,15 ; cblank15(m:m)=cblank1 ; enddo


            ntot=0
            nmp=0
            ngross=0

            read(lun0,end=200) idate

            loop_read_obs: do
              read(lun0,end=200)otype,nchar,nreal,ii0,mypegsi
            
              if (ii0==0) cycle loop_read_obs

              lprvinfoexist=otype(1:3)=='  t'.or. & 
                            otype(1:3)=='  q'.or. & 
                            otype(2:3)=='ps' .or. & 
                            otype(2:3)=='uv' .or. &
                            otype(1:3)=='spd'.or. &
                            otype(1:3)=='gst'.or. &
                            otype(1:3)=='hwv'.or. &
                            otype(1:3)=='vis'
            
              if (allocated(cdiagbuf)) deallocate(cdiagbuf) ; allocate(cdiagbuf(ii0))
              if (allocated(cprvstg))  deallocate(cprvstg)  ; allocate(cprvstg(ii0))
              if (allocated(csprvstg)) deallocate(csprvstg) ; allocate(csprvstg(ii0))
              if (allocated(rdiagbuf)) deallocate(rdiagbuf) ; allocate(rdiagbuf(nreal,ii0))

              read(lun0) cdiagbuf,rdiagbuf
              if (lprvinfoexist) read(lun0) cprvstg,csprvstg

              print*,'in get_ob_lists, idate=',idate
              print*,'in get_ob_lists, otype,nchar,nreal,ii0,mypegsi=', & 
                         otype,nchar,nreal,ii0,mypegsi

              if (.not.lprvinfoexist) cycle loop_read_obs  !WHY THIS?????????????????? / MPondeca

              if       (otype(1:3)=='  t') then ; lun=lun_t  ; n1=size(t_rjlist) ; rjlist0(1:n1)=t_rjlist(1:n1) ; nrjs0=ntrjs ; ifld=1
                elseif (otype(1:3)=='  q') then ; lun=lun_q  ; n1=size(q_rjlist) ; rjlist0(1:n1)=q_rjlist(1:n1) ; nrjs0=nqrjs ; ifld=2
                elseif (otype(2:3)=='ps' ) then ; lun=lun_ps ; n1=size(p_rjlist) ; rjlist0(1:n1)=p_rjlist(1:n1) ; nrjs0=nprjs ; ifld=3
                elseif (otype(2:3)=='uv' ) then ; lun=lun_u  ; n1=size(w_rjlist) ; rjlist0(1:n1)=w_rjlist(1:n1) ; nrjs0=nwrjs ; ifld=4
                elseif (otype(1:3)=='spd') then ; lun=lun_spd  ; n1=size(spd_rjlist)  ; rjlist0(1:n1)=spd_rjlist(1:n1)  ; nrjs0=nspdrjs  ; ifld=7
                elseif (otype(1:3)=='gst') then ; lun=lun_gust ; n1=size(gust_rjlist) ; rjlist0(1:n1)=gust_rjlist(1:n1) ; nrjs0=ngustrjs ; ifld=8
                elseif (otype(1:3)=='hwv') then ; lun=lun_howv ; n1=size(howv_rjlist) ; rjlist0(1:n1)=howv_rjlist(1:n1) ; nrjs0=nhowvrjs ; ifld=9
                elseif (otype(1:3)=='vis') then ; lun=lun_vis  ; n1=size(vis_rjlist)  ; rjlist0(1:n1)=vis_rjlist(1:n1)  ; nrjs0=nvisrjs  ; ifld=10
              endif

              lrjlistapplicable=otype(1:3)=='  t'.or. & 
                                otype(1:3)=='  q'.or. & 
                                otype(2:3)=='ps' .or. & 
                                otype(2:3)=='uv' .or. &
                                otype(1:3)=='gst' .or. &
                                otype(1:3)=='hwv' .or. &
                                otype(1:3)=='vis'

              do i=1,ii0
                 cstation=cdiagbuf(i)
                 cprovider=cprvstg(i)
                 csubprovider=csprvstg(i)
                 itype=nint(rdiagbuf(1,i))
                 isubtype=nint(rdiagbuf(2,i))
                 rlat=rdiagbuf(3,i)
                 rlon=rdiagbuf(4,i)
                 dtime=rdiagbuf(8,i)
                 shgt0=rdiagbuf(5,i)
                 obpres0=rdiagbuf(6,i)
                 hgt0=spval
                 qtflg=rdiagbuf(10,i)
                 if (rdiagbuf(11,i) .ge. 1.) then 
                     rmuse=rdiagbuf(11,i)*rdiagbuf(12,i)
                   else                                        !for usage=0., 0.25, 0.50, 0.75, display ob info as
                     rsign=rdiagbuf(12,i)                      !if usage were "1". it is convenient because in the 
                     rmuse=rdiagbuf(12,i)+rsign*rdiagbuf(11,i) !obs_listing files we want to associate the "1" with
                 endif                                         !the 1st outer-loop

                 oberr=rdiagbuf(16,i)
                 ob=rdiagbuf(17,i)
                 ddiff=rdiagbuf(18,i)
                 ob_model=rdiagbuf(17,i)-rdiagbuf(18,i)
                 slm=spval2
                 hgt=spval

                 !station names B7Hv and vH7B may have some strange, non-readable trailing characters. account for it:
                 if (cstation(1:4)=='B7Hv' .or. cstation(5:8)=='vH7B') then
                     cstation(1:4)='B7Hv'
                     cstation(5:8)='   '
                 endif

                 sfctype=(itype>179.and.itype<190).or.(itype>=280.and.itype<=290).or. &
                         (itype>=192.and.itype<=199).or.(itype>=292.and.itype<=299)

                 near_sfcob=sfctype !Will want to expand the definition of near_sfcob to
                                    !include upper level obs that are close enough to
                                    !the surface to influence the surface anlaysis.
                                    !for that, need either the local terrain
                                    !elevation or the surface pressure to build a
                                    !delta_elevation or delta_p condition. Note
                                    !that the station elevation and observation pressure
                                    !are contained in the diagnostic file

                 if (near_sfcob) then
                    clistorig=cblank15
                    if (sfctype .and. lrjlistapplicable) then
                      if (rmuse<-1. .and. nrjs0>0) call read_rejfileorig(itype,cstation,rjlist0(1:nrjs0),nrjs0,clistorig)
                    endif

                    oberr2=1.e10
                    if (oberr.gt.1.e-05) oberr2=1./oberr  

                    if (otype(1:3)=='  t') then
                       ctvts='tv'
                       if (qtflg .gt. 0.) ctvts='ts'
                       caux18=clistorig//cblank1//ctvts   !use this to get around formatting pbs in write statement
                       write(lun,124) cstation,itype,isubtype,rlat,rlon,dtime,oberr2,ob,ob_model,rmuse,caux18
                    else if (otype(2:3)=='uv') then
                       uob=ob
                       uob_model=ob_model
                       vob=rdiagbuf(20,i)
                       dvdiff=rdiagbuf(21,i)
                       vob_model=rdiagbuf(20,i)-rdiagbuf(21,i)
                       rfactor=rdiagbuf(23,i)
                       wob=uob*uob+vob*vob ; if (wob > 0.) wob=sqrt(wob)
                       wob_model=uob_model*uob_model+vob_model*vob_model ; if (wob_model > 0.) wob_model=sqrt(wob_model)
                       write(lun,125)   cstation,itype,isubtype,rlat,rlon,dtime,oberr2,uob,uob_model,rmuse,clistorig,rfactor
                       write(lun_v,125) cstation,itype,isubtype,rlat,rlon,dtime,oberr2,vob,vob_model,rmuse,clistorig,rfactor
                       write(lun_w,125) cstation,itype,isubtype,rlat,rlon,dtime,oberr2,wob,wob_model,rmuse,clistorig,rfactor
                    else if (otype(1:3)=='spd' .or. otype(1:3)=='gst' ) then
                       rfactor=rdiagbuf(20,i)
                       write(lun,125) cstation,itype,isubtype,rlat,rlon,dtime,oberr2,ob,ob_model,rmuse,clistorig,rfactor
                    else
                       write(lun,123) cstation,itype,isubtype,rlat,rlon,dtime,oberr2,ob,ob_model,rmuse,clistorig
                    endif

                    call add2auxlist(lun*10,cstation,cprovider,csubprovider,itype,isubtype,shgt0,hgt0,hgt,slm)
                    if (otype(2:3)=='uv') then 
                       call add2auxlist(lun_v*10,cstation,cprovider,csubprovider,itype,isubtype,shgt0,hgt0,hgt,slm)
                       call add2auxlist(lun_w*10,cstation,cprovider,csubprovider,itype,isubtype,shgt0,hgt0,hgt,slm)
                    endif

                    ntot(ifld)=ntot(ifld)+1
                    kuse=int(rmuse)
                    kfrac=nint (abs(rmuse-float(kuse))/0.25 + 0.001 )
                    nmp(kuse,kfrac,ifld)=nmp(kuse,kfrac,ifld)+1
                    if ( (kuse>=-3 .and. kuse<=-1) .and. oberr2 > 1.e09) ngross(ifld)=ngross(ifld)+1
                 endif
              enddo

           enddo loop_read_obs
200        continue

           !----------------------------------------------------------------------------------
           !==> Obs stats summary
           !----------------------------------------------------------------------------------
           !v-wind component and w-wind
           ntot(5)=ntot(4)
           ntot(6)=ntot(4)
           nmp(:,:,5)=nmp(:,:,4)
           nmp(:,:,6)=nmp(:,:,4)

           do n=1,nflds
             if       ( n==1 ) then ; lun=lun_t
               elseif ( n==2 ) then ; lun=lun_q
               elseif ( n==3 ) then ; lun=lun_ps
               elseif ( n==4 ) then ; lun=lun_u
               elseif ( n==5 ) then ; lun=lun_v
               elseif ( n==6 ) then ; lun=lun_w
               elseif ( n==7 ) then ; lun=lun_spd
               elseif ( n==8 ) then ; lun=lun_gust
               elseif ( n==9 ) then ; lun=lun_howv
               elseif ( n==10) then ; lun=lun_vis
             endif

              write (lun,'(a)') '=================================================================================================='
              write (lun,'(a)') '=================================================================================================='
              write (lun,'(1x,a,i8)') 'total number of obs                            :', ntot(n)

              naux=sum(nmp(1:3,0:3,n))
              write (lun,'(1x,a,i8)') 'number of obs assimilated                      :', naux
              percnumber=0.
              if (ntot(n) .gt. 0) percnumber=(float(naux)/float(ntot(n)))*100.
              write (lun,'(1x,a,3x,f8.3,a)') 'percentage of obs assimilated                  :',percnumber,'%'

              write (lun,'(1x,a,i8)') 'number of obs rejected by gross error check    :', ngross(n)
              percnumber=0.
              if (ngross(n) .gt. 0) percnumber=(float(ngross(n))/float(ntot(n)))*100.
              write (lun,'(1x,a,3x,f8.3,a)') 'percentage of obs rejected by gross error check:',percnumber,'%'


              do m=nmplower,nmpupper
                 do k=0,3
                    if ( nmp(m,k,n) .gt. 0) then
                        write(chvar10,'(i10)') nmp(m,k,n)
                        chvar10=adjustl(chvar10)
                        kk=len_trim(chvar10)
                        chvar1='8'
                        cfmt="(1x,"//"'number of obs with rmuse ='"//',f9.2,'//"'            :'"//',i'//chvar1//")"
                        if (m <  0) write (lun,cfmt) float(m)-0.25*float(k),nmp(m,k,n)
                        if (m >= 0) write (lun,cfmt) float(m)+0.25*float(k),nmp(m,k,n)
                    endif
                 enddo
              enddo

              if (n.ne.4 .and. n.ne.5 .and. n.ne.6)  then
                 write (lun,'(1x,a,i8)') 'number of obs in the all day reject list       :', sum(nmp(-5000,0:3,n))
                 write (lun,'(1x,a,i8)') 'number of obs in the diurnal reject list       :', sum(nmp(-5100,0:3,n))
              else
                 write (lun,'(1x,a,i8)') 'number of non-mesonets in the reject list      :', sum(nmp(-5000,0:3,n))
                 write (lun,'(1x,a,i8)') 'number of mesonet winds in none of the uselists and also not in the reject list:', & 
                                                                                            sum(nmp(-6000,0:3,n))
                 write (lun,'(1x,a,i8)') 'number of mesonets winds in the rejectlist and in at least one of the uselists :', & 
                                                                                             sum(nmp(-6100,0:3,n))
                 write (lun,'(1x,a,9x,i8)') 'number of mesonets winds in the rejectlist but in none of the uselists:', & 
                                                                                             sum(nmp(-6200,0:3,n))
              endif
              write (lun,'(1x,a,i8)') 'sum of all obs                                 :', sum(nmp(nmplower:nmpupper,0:3,n))
           enddo

123 format(a8,4x,i3,4x,i3,4x,f6.2,4x,f6.2,4x,f6.2,2x,3(2x,E11.4),2x,f8.2,1x,a15)
124 format(a8,4x,i3,4x,i3,4x,f6.2,4x,f6.2,4x,f6.2,2x,3(2x,E11.4),2x,f8.2,1x,a18)
125 format(a8,4x,i3,4x,i3,4x,f6.2,4x,f6.2,4x,f6.2,2x,3(2x,E11.4),2x,f8.2,1x,a15,1x,f5.2)

           if (allocated(cdiagbuf)) deallocate(cdiagbuf)
           if (allocated(cprvstg))  deallocate(cprvstg)
           if (allocated(csprvstg)) deallocate(csprvstg)
           if (allocated(rdiagbuf)) deallocate(rdiagbuf)

           close(lun_t)     ; close(lun_t*10)
           close(lun_q)     ; close(lun_q*10)
           close(lun_ps)    ; close(lun_ps*10)
           close(lun_u)     ; close(lun_u*10)
           close(lun_v)     ; close(lun_v*10)
           close(lun_w)     ; close(lun_w*10)
           close(lun_spd)   ; close(lun_spd*10)
           close(lun_gust)  ; close(lun_gust*10)
           close(lun_howv)  ; close(lun_howv*10)
           close(lun_vis)   ; close(lun_vis*10)

           close(lun0)
5000     continue
         deallocate(t_rjlist)
         deallocate(q_rjlist)
         deallocate(p_rjlist)
         deallocate(w_rjlist)
         deallocate(spd_rjlist)
         deallocate(gust_rjlist)
         deallocate(howv_rjlist)
         deallocate(vis_rjlist)
         deallocate(rjlist0)

         end subroutine get_ob_lists3d
!=======================================================================================================
!=======================================================================================================
         subroutine rjlist_obcount(filename,fexist,nrjs)

         implicit none

         integer(4),intent(out)::nrjs
         character(*),intent(in)::filename
         logical,intent(out)::fexist

         integer(4),parameter::meso_unit=61
         integer(4) ncount,m,n
         character(80) filename2
         character(1) cvar
         logical fexist0,fexist1,fexist2
         integer(4) nrjs0

         nrjs=0

         fexist1=.false.
         fexist2=.false.

         inquire(file=trim(filename),exist=fexist)
         if(fexist) then
             open (meso_unit,file=trim(filename),form='formatted')
             ncount=0
             do m=1,3
              read(meso_unit,*,end=151)
             enddo
             do 
                ncount=ncount+1
                read(meso_unit,*,end=151)
             enddo
151          continue
             nrjs=ncount-1 ; nrjs=max(nrjs,0)
         endif
         fexist0=fexist
         print*,'in rjlist_obcount: filename,nrjs=',trim(filename),nrjs
         close(meso_unit)

         if(trim(filename)=='t_rejectlist' .or. & 
            trim(filename)=='q_rejectlist') then

            cvar=filename(1:1)

            do n=1,2
               if (n==1) filename2=cvar//'_day_rejectlist'
               if (n==2) filename2=cvar//'_night_rejectlist'

               inquire(file=trim(filename2),exist=fexist)
               if (fexist) then
                  open (meso_unit,file=trim(filename2),form='formatted')
                  ncount=nrjs ; nrjs0=nrjs
                  do m=1,3
                     read(meso_unit,*,end=251)
                  enddo
                  do
                     ncount=ncount+1
                     read(meso_unit,*,end=251)
                  enddo
251               continue
                  nrjs=ncount-1 ; nrjs=max(nrjs,nrjs0)
                  close(meso_unit)
                  print*,'in readin_rjlist: add diurnal lists:n,filename2,nrjs=',n,trim(filename2),nrjs
               endif
               if (n==1) fexist1=fexist
               if (n==2) fexist2=fexist
            enddo
         endif

         fexist=fexist0.or.fexist1.or.fexist2

         return
         end
!=======================================================================================================
!=======================================================================================================
         subroutine readin_rjlist(filename,rjlist,nrjs)

         implicit none

         integer(4),intent(in)::nrjs
         character(*),intent(in)::filename
         character(*),intent(out)::rjlist(nrjs)

         integer(4),parameter::meso_unit=61
         integer(4) ncount,m,n
         character(1),parameter::cblank=' '
         character(90) cstring
         character(80) filename2
         character(1) cvar
         integer(4) nrjs0
         logical fexist

         do n=1,nrjs
           do m=1,90
              rjlist(n)(m:m)=cblank
           enddo
         enddo

         fexist=.false.

         inquire(file=trim(filename),exist=fexist)
         if(fexist) then
             open (meso_unit,file=trim(filename),form='formatted')
             ncount=0
             do m=1,3
              read(meso_unit,*,end=151) cstring
             enddo
             do
                ncount=ncount+1
                read(meso_unit,*,end=151) rjlist(ncount)
             enddo
151          continue
             ncount=ncount-1 ; ncount=max(ncount,0)
         endif
         close(meso_unit)
         print*,'in readin_rjlist: filename,ncount=',trim(filename),ncount

         if(trim(filename)=='t_rejectlist' .or. & 
            trim(filename)=='q_rejectlist') then

            cvar=filename(1:1)

            do n=1,2
               if (n==1) filename2=cvar//'_day_rejectlist'
               if (n==2) filename2=cvar//'_night_rejectlist'

               inquire(file=trim(filename2),exist=fexist)
               if (fexist) then
                  open (meso_unit,file=trim(filename2),form='formatted')
                  nrjs0=ncount
                  do m=1,3
                     read(meso_unit,*,end=251) cstring
                  enddo
                  do
                     ncount=ncount+1
                     read(meso_unit,*,end=251) rjlist(ncount)
                  enddo
251               continue
                  ncount=ncount-1 ; ncount=max(ncount,nrjs0)
                  close(meso_unit)
                  print*,'in readin_rjlist: add diurnal lists:n,filename2,ncount=',n,trim(filename2),ncount
               endif
            enddo
         endif
         if (ncount .ne. nrjs) then
            print*,'trouble in readin_rjlist. ncount shoud be equal to nrjs'
            print*,' ... aborting readin_rjlist'
            call abort
            stop
         endif

         return
         end
!=======================================================================================================
!=======================================================================================================
         subroutine open_and_header_V2(lun,cvar,clun3)

         implicit none

         integer(4),intent(in):: lun
         character(*),intent(in)::cvar
         character(*),intent(in)::clun3

         character(70) cnames(30),cname0
         character(120) cheader
         character(96) cheader_1,cheader_2,cheader_3,cheader_4,cheader_5, &
                       cheader_6,cheader_7,cheader_8,cheader_9
         character(96) cheader_10

         integer(4) n
!======================================================================================
         cnames(1)='TEMPERATURE'
         cnames(2)='SPECIFIC HUMIDITY'
         cnames(3)='SURFACE PRESSURE'
         cnames(4)='U-WIND'
         cnames(5)='V-WIND'
         cnames(6)='UV-WIND SPEED'
         cnames(7)='WIND SPEED'
         cnames(8)='WIND GUST'
         cnames(9)='SIGNIFICANT WAVE HGHT'
         cnames(10)='VISIBILITY'

         cheader_1='shgt0 ==> station height'
         cheader_2='hgt0  ==> observation elevation'
         cheader_3='hgt   ==> model terrain at ob (x,y) location'
         cheader_4='slm   ==> dominant surface type. this is the surface type of the grid point'
         cheader_5='          of the enclosing box nearest to the observation.'
         cheader_6='          slm is 0 for water and 1 for land. note: must subtract 3 if slm>=3'
         cheader_7='          values of slm>=3 are used to indicate that at least two of the'
         cheader_8='          grid points of the enclosing box are of different surface types.'
         cheader_9='                              '
         cheader_10='stnname   obtype provider  subprovider         shgt0         hgt0         hgt      slm'


         open (lun,file=trim(cvar)//'_obs.listing_iter_'//trim(clun3),form='formatted') !output file

         cheader='stnname   obtype  subtype  lat(dg)  lon(dg E)   dtime      oberr         ob         guess         rmuse'

         if (trim(cvar)=='t') then
            n=1
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are K. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000. ==> ob was in the reject list ; rmuse=-5100. ==> in diurnal list' 
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list.'
            write(lun,'(a)')   'tv=virtual T, and ts=sensible T. Note that the GSI can assimilate obs as either tv or ts' 
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist  tv-or-ts'
 

         elseif (trim(cvar)=='q') then
            n=2
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are g/Kg. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000. ==> ob was in the reject list ; rmuse=-5100. ==> in diurnal list'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: lists of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist'


         elseif (trim(cvar)=='ps') then
            n=3
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are Pa. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000. ==> ob was in the reject list'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist'


         elseif (trim(cvar)=='u') then
            n=4
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m/s. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000.==> this non-mesonet wind was was in the reject list' 
            write(lun,'(a)')   'rmuse=-6000.==> this mesonet wind did not belong to any of the GSD uselists, and neither was it'
            write(lun,'(a)')   '                in the reject list'
            write(lun,'(a)')   'rmuse=-6100.==> this mesonet wind was in the reject list and in at least one of the GSD uselists'
            write(lun,'(a)')   'rmuse=-6200.==> this mesonet wind was in the reject list and in neither one of the GSD uselists'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist      factw'


         elseif (trim(cvar)=='v') then
            n=5
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m/s. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000.==> this non-mesonet wind was was in the reject list' 
            write(lun,'(a)')   'rmuse=-6000.==> this mesonet wind did not belong to any of the GSD uselists, and neither was it'
            write(lun,'(a)')   '                in the reject list'
            write(lun,'(a)')   'rmuse=-6100.==> this mesonet wind was in the reject list and on at least one of the GSD uselists'
            write(lun,'(a)')   'rmuse=-6200.==> this mesonet wind was in the reject list and on neither one of the GSD uselists'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list' 
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist      factw'

         elseif (trim(cvar)=='w') then
            n=6
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m/s. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000.==> this non-mesonet wind was was in the reject list' 
            write(lun,'(a)')   'rmuse=-6000.==> this mesonet wind did not belong to any of the GSD uselists, and neither was it'
            write(lun,'(a)')   '                in the reject list'
            write(lun,'(a)')   'rmuse=-6100.==> this mesonet wind was in the reject list and on at least one of the GSD uselists'
            write(lun,'(a)')   'rmuse=-6200.==> this mesonet wind was in the reject list and on neither one of the GSD uselists'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list' 
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist      factw'

         elseif (trim(cvar)=='spd') then
            n=7
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m/s. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-500. ==> ob was in the reject list'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist      factw'


         elseif (trim(cvar)=='gust') then
            n=8
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m/s. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000.==> this non-mesonet wind was was in the reject list' 
            write(lun,'(a)')   'rmuse=-6000.==> this mesonet wind did not belong to any of the GSD uselists, and neither was it'
            write(lun,'(a)')   '                in the reject list'
            write(lun,'(a)')   'rmuse=-6100.==> this mesonet wind was in the reject list and on at least one of the GSD uselists'
            write(lun,'(a)')   'rmuse=-6200.==> this mesonet wind was in the reject list and on neither one of the GSD uselists'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list' 
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist      factw'


         elseif (trim(cvar)=='howv') then
            n=9
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess are m. Ob is used only if rmuse=+1.0 or +2.0' 
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess' 
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000. ==> ob was in the reject list'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1' 
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or' 
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   trim(cheader)//' rejectlist'

         elseif (trim(cvar)=='vis') then
            n=10
            cname0='RTMA '//trim(cnames(n))//' OBS'
            write(lun,'(a)')   trim(cname0)
            write(lun,'(a)')   'UNITS of  oberr, ob, and guess is m. Ob is used only if rmuse=+1.0 or +2.0'
            write(lun,'(a)')   'rmuse=-100. ==> user chose to monitor this ob and see how well it agrees with the guess'
            write(lun,'(a)')   'rmuse=-150. ==> ob is being monitored. Internally selected based on MADIS QC flag values'
            write(lun,'(a)')   'rmuse=-5000.==> this non-mesonet wind was was in the reject list'
            write(lun,'(a)')   'rmuse=-6000.==> this mesonet wind did not belong to any of the GSD uselists, and neither was it'
            write(lun,'(a)')   '                in the reject list'
            write(lun,'(a)')   'rmuse=-6100.==> this mesonet wind was in the reject list and on at least one of the GSD uselists'
            write(lun,'(a)')   'rmuse=-6200.==> this mesonet wind was in the reject list and on neither one of the GSD uselists'
            write(lun,'(a)')   'dtime is the hour relative to the valid analysis time. For example, dtime=-0.1'
            write(lun,'(a)')   '       means 0.1h (i.e. 6 minutes) before the valid analysis time'
            write(lun,'(a)')   'rejectlist: list of sub-standard obs where ob was found. It can be (i) static (sta),'
            write(lun,'(a)')   '            (ii) from weather forecast office (wfo), (iii) global based on MADIS QC stats (glb), or'
            write(lun,'(a)')   '            (iv) dynamic (dyn). Note that the ob can be in more than one reject list'
            write(lun,'(a)')   '                              '
            write(lun,'(a)')   cheader//' rejectlist'
         endif


         open(lun*10,file=trim(cvar)//'_obs.listing_iter_'//trim(clun3)//'_aux',form='formatted')
         cname0='RTMA '//trim(cnames(n))//' OBS'

         write(lun*10,'(a)')   trim(cname0)
         write(lun*10,'(a)')   trim(cheader_1)
         write(lun*10,'(a)')   trim(cheader_2)
         write(lun*10,'(a)')   trim(cheader_3)
         write(lun*10,'(a)')   trim(cheader_4)
         write(lun*10,'(a)')   trim(cheader_5)
         write(lun*10,'(a)')   trim(cheader_6)
         write(lun*10,'(a)')   trim(cheader_7)
         write(lun*10,'(a)')   trim(cheader_8)
         write(lun*10,'(a)')   trim(cheader_9)
         write(lun*10,'(a)')   trim(cheader_10)

         return
         end
!***************************************************************
!=======================================================================================================
!=======================================================================================================
        subroutine read_rejfileorig(kx,c_station_id,rjlist,nrjs,clistorig)
        implicit none

         character(*),intent(in)::c_station_id
         character(*),intent(in)::rjlist(nrjs)
         character(*),intent(out)::clistorig
         integer(4),intent(in)::kx,nrjs

         character(8) ch8
         integer(4) m,nlen


         do m=1,nrjs
           ch8(1:8)=rjlist(m)(1:8)
            nlen=len_trim(ch8)
            if ((trim(c_station_id) == trim(ch8)) .or. &
                ((kx==188.or.kx==288.or.kx==195.or.kx==295) .and. c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
               clistorig(1:15)=rjlist(m)(70:84)                          !an "a" or "x" in the eight position following blanks
               exit
            endif
         enddo

        return
        end
!=======================================================================================================
!=======================================================================================================
         subroutine add2auxlist(lun,cstation,cprovider,csubprovider, & 
                                itype,isubtype,shgt0,hgt0,hgt,slm)

         implicit none

         character(*),intent(in):: cstation
         character(*),intent(inout):: cprovider,csubprovider

         integer(4),intent(in):: lun,itype,isubtype
         real(4),intent(in):: shgt0,hgt0,hgt,slm

         if (cprovider(1:4)=='B7Hv' .or. cprovider(5:8)=='vH7B') then !this provider name comes with strange, non-readable trailing characters
             cprovider(1:4)='B7Hv'
             cprovider(5:8)='   '
         endif

         if (csubprovider(1:4)=='B7Hv' .or. csubprovider(5:8)=='vH7B') then
             csubprovider(1:4)='B7Hv'
             csubprovider(5:8)='   '
         endif
         

         write(lun,121) cstation,itype,isubtype,cprovider,csubprovider,shgt0,hgt0,hgt,slm

!121      format(a8,4x,i3,4x,i3,4x,a8,4x,a8,4x,f10.3,2x,e10.3,2x,f10.3,2x,f7.3)
121      format(a8,4x,i3,4x,i3,4x,a8,4x,a8,4x,f11.3,2x,f11.3,2x,f10.3,2x,f7.3)

         return
         end
!=======================================================================================================
!=======================================================================================================
