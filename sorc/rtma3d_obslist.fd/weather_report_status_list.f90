subroutine weather_report_status_list()
!$$$ subprogram documentation block
! 
! subprogram:    weather_report_status_list    interpolate analysis to select locations
!   prgmmr: pondeca           org: emc                date: 2024-03-28
!
! abstract: Driver subroutine to interpolate analysis to select locations
!
! program history log:
!   2024-03-28  pondeca
!
!
! attributes:
!   language: f90
!   machine:  cray/wcoss2
!
!$$$
   implicit none 

!Declare local parameters
   integer(4),parameter::mype=0
   integer(4),parameter::ngridsmax=100

!Declare local variables
   integer(4) n
   integer(4) ngrids
   logical fexist
   character(60)  cgridset(ngridsmax)
   character(200) anlfileset(ngridsmax)
   character(250) stnlistset(ngridsmax)
   character(250) non_viableset(ngridsmax)
   character(250) outfileset(ngridsmax)
   character(250) outsideset(ngridsmax)

   namelist/faa_grid_and_anlfile_info/ngrids,cgridset,anlfileset, &
                                      stnlistset, non_viableset, outfileset,outsideset
   cgridset(:)=' '
   anlfileset(:)=' '
   stnlistset(:)=' '
   non_viableset(:)=' '
   outfileset(:)=' '
   outsideset(:)=' '


   inquire(file='faa_related_input',exist=fexist)
   if (fexist) then
      open (55,file='faa_related_input',form='formatted')
      read (55,faa_grid_and_anlfile_info)
      close(55)

      if (mype==0) then  
         print*,'-------- in weather_report_status_list -----'
         print*,'ngrids=',ngrids
         do n=1,ngrids
            print*,'n,cgridset(n),anlfileset(n)=',n,trim(cgridset(n)),',',trim(anlfileset(n))
            print*,'n,stnlistset(n),non_viableset(n)=',n,stnlistset(n),non_viableset(n)
            print*,'n,outfileset(n),outsideset(n)=',n,outfileset(n),outsideset(n)
            print*,'*************************************************************************'
            print*,'*************************************************************************'
         enddo
      endif

      do n=1,ngrids
        call stn_interpolator(cgridset(n),anlfileset(n), & 
                              stnlistset(n),non_viableset(n),outfileset(n),outsideset(n)) 
      enddo
    else
      if (mype==0) print*,'no request made for weather report statis list'
   endif
   end subroutine weather_report_status_list
!--------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------
       subroutine stn_interpolator(cgrid,file1,input_stnlist,non_viablestnlist,outfile,outsidefile)
!$$$ subprogram documentation block
!
! subprogram:    stn_interpolator   interpolate analysis to select locations
!   prgmmr: pondeca           org: emc                date: 2024-03-28
!
! abstract: Interpolate analysis of psfc, 2m-T, 2m-TD, 10m-wind, 10-m gust, vis, and cldch
!           to select locations. developed to support faa operations
!
! program history log:
!   2015-xx-xx  pondeca - original code written for operational 2DRTMA
!   2024-03-28  pondeca/colon - adapt for 3DRTMA
!
!
! attributes:
!   language: f90
!   machine:  cray/wcoss2
!
! to compile on wcoss2:
!  ifort -c -free -I/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.8_wmo/include \
!                 -L/apps/ops/prod/libs/intel/19.1.3.304/wgr/2.0.8_wmo/lib -lwgrib2 -convert big_endian stn_interpolator.f90
!
!$$$
!      use mpi
!      use mpitaskmod, only: mype,npe
       use wgrib2api
       use mod_rll_trans, only : rll_trans       !<--- procedure to transform between regular earth lat/lon
                                                 !     and rotated latlon
       use mod_rll_trans, only : rll2xy          !<--- procedure to go from rotated latlon
                                                 !     to its cartesain (x/y) coordinates
       use mod_rll_trans, only : rll_grid        !<--- variable of a derived data type containing parameters
                                                 !     that define the rotated latlon (RLL)grid

       implicit none
 
! Declare parameters
       integer(4), parameter:: mype=0 
       integer(4), parameter:: nt=7 !order is: p,t,td,sqrt(u**2+v***2),gust,vis,cldch
       real(4), parameter:: spval=-9999.
       real(4), parameter:: spval2=1.e18
       real(4), parameter :: gravity=9.81
       real(4), parameter:: half=0.5
       real(4), parameter:: half_tlapse=0.00325
       real(4), parameter:: g_over_rd=9.81/287.04

! Declare passed variables
       character(*),intent(in)::cgrid
       character(*),intent(in)::file1,input_stnlist,non_viablestnlist,outfile,outsidefile

! Declare local variables
       integer(4) nx,ny
       integer(4) ifield,i,j,k,n,nstns
       integer(4) lun
       integer(4) n0,nlen
       integer(4) ii,jj
       integer(4) iret
       real(8) alat1,elon1,dx,elonv,alatan
       real(8) alat2,elon2,alad
       real(8) rlat8,rlon8,xx8,yy8
       real(8) rlat_rll8,rlon_rll8
       character(120) cstring,cstring2,bstring
       character(8) cstn0
       character (200) :: vars(nt),varsshort(nt),var_u, var_v
       character (200) :: inv,var,str_date
       character (99) :: invline
       character (len=400) :: gridline
       character(10) thiscdate
       logical fexist

       real(4) fint0,altsetting
       character(40) cfldname(nt)
       character(80)cinterp_p,cinterp_t,cinterp_td,cinterp_wind, & 
                    cinterp_gust,cinterp_vis,cinterp_cldch

       character(80) cinterp_method(nt)
       real(4),allocatable,dimension(:):: rlat,rlon
       real(4),allocatable,dimension(:):: xx,yy,elev
       character(8),allocatable,dimension(:):: cstation
       character(10),allocatable,dimension(:,:):: cfint
       logical,allocatable,dimension(:,:):: viable
       logical,allocatable,dimension(:):: inside

       character(4) cyyyy0
       character(2) cmm0,cdd0,chh0,cmin0
       character(3) chh0Z,cmonth0,cmin0Z

       character(4) cyyyy
       character(2) cmm,cdd,chh,cmin
       character(3) chhZ,cmonth,cminZ

       character(4) cyyyyp1
       character(2) cmmp1,cddp1,chhp1,cminp1
       character(3) chhp1Z,cmonthp1,cminp1Z


       real(4) rdelz,rdp,rtvts
       real(4) tges,tob,pges,zsges
       real(4),allocatable,dimension(:,:)::field1,field2
       real(4),allocatable,dimension(:,:)::terrain,tfield,tvfield
       real(4),allocatable,dimension(:,:)::grid,lat,lon

       character(60) cgprocess

       integer(4) iyyyy0,imm0,idd0,ihh0,imin0      !report issued at this time
       integer(4) iyyyy,imm,idd,ihh,imin           !time range beginning time
       integer(4) iyyyyp1,immp1,iddp1,ihhp1,iminp1 !time range ending time

       namelist/generatingprocess/cgprocess
       namelist/faa_timerange/iyyyy0,imm0,idd0,ihh0,imin0, & 
                              iyyyy,imm,idd,ihh,imin, & 
                              iyyyyp1,immp1,iddp1,ihhp1,iminp1, &
                              thiscdate

       namelist/faa_stninterp_method/cinterp_p,cinterp_t,cinterp_td,cinterp_wind, & 
                                 cinterp_gust,cinterp_vis,cinterp_cldch

       data cgprocess/'RTMA'/

       data cinterp_p     /'bilinear'/
       data cinterp_t     /'bilinear'/
       data cinterp_td    /'bilinear'/
       data cinterp_wind  /'bilinear'/
       data cinterp_gust  /'bilinear'/
       data cinterp_vis   /'bilinear'/  !nearest_neighbor
       data cinterp_cldch /'nearest_neighbor'/

       data cfldname(1) /'SFCP'/      ; data vars(1) /':PRES:surface:'/           ; data varsshort(1) /'PRES'/
       data cfldname(2) /'2mT'/       ; data vars(2) /':TMP:2 m above ground:'/   ; data varsshort(2) /'TMP'/
       data cfldname(3) /'2mTD'/      ; data vars(3) /':DPT:2 m above ground:'/   ; data varsshort(3) /'DPT'/
       data cfldname(4) /'10mW'/      ; data vars(4) /':WIND:10 m above ground:'/ ; data varsshort(4) /'WIND'/
       data cfldname(5) /'10mGUST'/   ; data vars(5) /':GUST:surface:'/           ; data varsshort(5) /'GUST'/ !I suppose this is in reality 10 m gust / MPondeca 
       data cfldname(6) /'VIS'/       ; data vars(6) /':VIS:surface:'/            ; data varsshort(6) /'VIS'/
       data cfldname(7) /'CLDCH'/     ; data vars(7) /':HGT:cloud ceiling:'/     ; data varsshort(7) /'CEIL'/
                                             var_u=':UGRD:10 m above ground:'  
                                             var_v=':VGRD:10 m above ground:'  

       inquire(file=trim(input_stnlist),exist=fexist)
       if (.not.fexist) then
          if (mype==0) print*,'No request made for analysis interpolation to select locations'
          return
       endif

       inquire(file='faa_related_input',exist=fexist)
       if(fexist) then
          open (55,file='faa_related_input',form='formatted')
          read (55,faa_timerange)
          read (55,faa_stninterp_method)
          close(55)
          if (mype==0) then
             print*,'in stn_interpolator :'
             print*,'iyyyy0,imm0,idd0,ihh0,imin0=',iyyyy0,imm0,idd0,ihh0,imin0
             print*,'iyyyy,imm,idd,ihh,imin=',iyyyy,imm,idd,ihh,imin
             print*,'iyyyyp1,immp1,iddp1,ihhp1,iminp1=',iyyyyp1,immp1,iddp1,ihhp1,iminp1
             print*,'thiscdate=',thiscdate
             print*,'cinterp_p=',trim(cinterp_p)
             print*,'cinterp_t=',trim(cinterp_t)
             print*,'cinterp_td=',trim(cinterp_td)
             print*,'cinterp_wind=',trim(cinterp_wind)
             print*,'cinterp_gust=',trim(cinterp_gust)
             print*,'cinterp_vis=',trim(cinterp_vis)
             print*,'cinterp_cldch=',trim(cinterp_cldch)
          endif
         else
          if (mype==0) print*,'Missing valid time range for analysis interpolation!!!'
          return
       endif

       cinterp_method(1)=cinterp_p
       cinterp_method(2)=cinterp_t
       cinterp_method(3)=cinterp_td
       cinterp_method(4)=cinterp_wind
       cinterp_method(5)=cinterp_gust
       cinterp_method(6)=cinterp_vis
       cinterp_method(7)=cinterp_cldch

       inquire(file='generatingprocess_input',exist=fexist)
       if(fexist) then
         open (55,file='generatingprocess_input',form='formatted')
         read (55,generatingprocess)
         close(55)
       endif

       if (trim(cgprocess)=='rtma' .or. trim(cgprocess)=='RTMA') cgprocess='RTMA'
       if (trim(cgprocess)=='urma' .or. trim(cgprocess)=='URMA') cgprocess='URMA'

       if (mype==0) then 
          print*,'in stn_interpolator: cgprocess=',trim(cgprocess)
       endif

       !------------------------------------
       write(cyyyy0,"(i4.4)") iyyyy0 
       write(cmm0,"(i2.2)")   imm0
       write(cdd0,"(i2.2)")   idd0
       write(chh0,"(i2.2)")   ihh0
       write(cmin0,"(i2.2)")  imin0 ; cmin0Z=cmin0//'Z'

       call getmonth(imm0,cmonth0)
       !------------------------------------
       write(cyyyy,"(i4.4)") iyyyy 
       write(cmm,"(i2.2)")   imm
       write(cdd,"(i2.2)")   idd
       write(chh,"(i2.2)")   ihh
       write(cmin,"(i2.2)")  imin ; cminZ=cmin//'Z'

       call getmonth(imm,cmonth)
       !------------------------------------
       write(cyyyyp1,"(i4.4)") iyyyyp1 
       write(cmmp1,"(i2.2)")   immp1
       write(cddp1,"(i2.2)")   iddp1
       write(chhp1,"(i2.2)")   ihhp1
       write(cminp1,"(i2.2)")  iminp1 ; cminp1Z=cminp1//'Z'

       call getmonth(immp1,cmonthp1)
       !------------------------------------

       call gridspecs(cgrid,nx,ny,dx,alat1,elon1,elonv,alatan,alat2,elon2,alad, & 
                      rll_grid%sp_lon,   rll_grid%sp_lat,   rll_grid%origin_rlon, rll_grid%origin_rlat, & 
                      rll_grid%dlon,     rll_grid%dlat,     rll_grid%offset_x,    rll_grid%offset_y, & 
                      rll_grid%llcnr(1), rll_grid%llcnr(2), rll_grid%urcnr(1),    rll_grid%urcnr(2))

       if (mype==0) then
          print*,'in stn_interpolator: cgrid=',trim(cgrid)
          print*,'in stn_interpolator: nx,ny=',nx,ny
          if (trim(cgrid).eq.'na_rotated_ll') then
             print*,'in stn_interpolator: sp_lon=',rll_grid%sp_lon
             print*,'in stn_interpolator: sp_lat=',rll_grid%sp_lat
             print*,'in stn_interpolator: origin_rlon=',rll_grid%origin_rlon
             print*,'in stn_interpolator: origin_rlat=',rll_grid%origin_rlat
             print*,'in stn_interpolator: dlon=',rll_grid%dlon
             print*,'in stn_interpolator: dlat,=',rll_grid%dlat
             print*,'in stn_interpolator: offset_x=',rll_grid%offset_x
             print*,'in stn_interpolator: offset_y=',rll_grid%offset_y
             print*,'in stn_interpolator: llcnr(1)=',rll_grid%llcnr(1)
             print*,'in stn_interpolator: llcnr(2)=',rll_grid%llcnr(2)
             print*,'in stn_interpolator: urcnr(1)=',rll_grid%urcnr(1)
             print*,'in stn_interpolator: urcnr(2)=',rll_grid%urcnr(2)
          else
             print*,'in stn_interpolator: alat1=',alat1
             print*,'in stn_interpolator: elon1=',elon1
             print*,'in stn_interpolator: dx=',dx
             print*,'in stn_interpolator: elonv=',elonv
             print*,'in stn_interpolator: alatan=',alatan
          endif
          print*,'in stn_interpolator: anlfile=',trim(file1)
       endif


! Determine number of stations on faa list
       open (55,file=trim(input_stnlist),form='formatted')

       do n=1,3
          read(55,*) cstring
       enddo

       nstns=0
       read_faastn: do
         read(55,'(a8,1x,f5.2,1x,f7.2)',end=101)
         nstns=nstns+1 
       enddo read_faastn
101    continue
       if (mype==0) print*,'in stn_interpolator: nstns=',nstns

       allocate(cstation(nstns))
       allocate(rlat(nstns))
       allocate(rlon(nstns))
       allocate(xx(nstns))
       allocate(yy(nstns))
       allocate(elev(nstns))
       allocate(inside(nstns))
       allocate(cfint(nstns,nt))
       allocate(viable(nstns,nt))

       rewind(55)
       do n=1,3
          read(55,*) cstring
       enddo

       inside=.true.
       do n=1,nstns
          read(55,'(a8,1x,f5.2,1x,f7.2,1x,f7.2)') cstation(n),rlat(n),rlon(n),elev(n)
          rlat8=dble(rlat(n))
          rlon8=dble(rlon(n)) ; if(rlon8 < 0._8) rlon8=rlon8+360._8

          if (trim(cgrid).eq.'na_rotated_ll') then
             call rll_trans(rlon8,rlat8,1,rlon_rll8,rlat_rll8)
             call rll2xy(rlon_rll8,rlat_rll8,xx8,yy8)
          else
             call latlon_to_grid(dx,alat1,elon1,elonv,alatan, &
                                rlat8,rlon8,cgrid,xx8,yy8)
          endif
          xx(n)=xx8
          yy(n)=yy8
          if (xx(n)<1. .or. xx(n)>float(nx) .or. yy(n)<1. .or. yy(n)>float(ny)) inside(n)=.false.
       enddo
       close(55)

       viable=.true.
       inquire(file=trim(non_viablestnlist),exist=fexist)
       if(fexist) then
          open (55,file=trim(non_viablestnlist),form='formatted')
          do n=1,3
             read(55,*) bstring
          enddo
          
          do
             read(55,'(a)',end=181) bstring
             nlen=len_trim(bstring)

             cstn0='        '
             do k=1,min(8,nlen)
                if (bstring(k:k)==',') exit
                cstn0(k:k)=bstring(k:k)
             enddo

             n0=-1
             do n=1,nstns
                if (cstn0==cstation(n)) then
                   n0=n
                   exit
                endif
             enddo

             if (n0 > 0) then
                do k=1,nlen
                   if (k+5<=nlen) then
                      if ( bstring(k:k+5)=='P=skip'     )  viable(n0,1)=.false.
                      if ( bstring(k:k+5)=='T=skip'     )  viable(n0,2)=.false.
                   endif

                   if (k+6<=nlen) then
                      if ( bstring(k:k+6)=='TD=skip'    )  viable(n0,3)=.false.
                   endif

                   if (k+8<=nlen) then
                      if ( bstring(k:k+8)=='WIND=skip'  )  viable(n0,4)=.false.
                      if ( bstring(k:k+8)=='GUST=skip'  )  viable(n0,5)=.false.
                   endif

                   if (k+7<=nlen) then
                      if ( bstring(k:k+7)=='VIS=skip'   )  viable(n0,6)=.false.
                   endif

                   if (k+9<=nlen) then
                      if ( bstring(k:k+9)=='CLDCH=skip' )  viable(n0,7)=.false.
                   endif
                enddo
             endif
          enddo
181       continue
          close(55)
       endif

       if (mype==0) then
          open (75,file=trim(outfile),form='formatted')
          open (76,file=trim(outsidefile),form='formatted')
       endif

       cstring2=trim(cgprocess)//' 2m-temperature (degrees Celsius) and altimeter setting (inHg)'

       if (mype==0) then
          write(75,'(a)') trim(cstring)
          write(75,'(a)') trim(cstring2)
          write(75,'(a10,a2,a3,1x,a2,1x,a3,1x,a4)') 'COMPUTED: ',chh0,cmin0Z,cdd0,cmonth0,cyyyy0
          write(75,'(a7,a2,a3,1x,a2,1x,a3,1x,a4,a4,a2,a3,1x,a2,1x,a3,1x,a4)') 'VALID: ',chh,cminZ,cdd,cmonth,cyyyy,' to ',chhp1,cminp1Z,cddp1,cmonthp1,cyyyyp1

          write(75,'(a)') trim(cstring)
          write(75,'(a)') 'station   Lat    Lon        2m-T      ALT'
          write(75,*)
       endif

       allocate(field1(nx,ny))
       allocate(field2(nx,ny))
       allocate(terrain(nx,ny))
       allocate(tfield(nx,ny))
       allocate(tvfield(nx,ny))

       inv = '@mem:0'

!      make inv file, save in memory file #0
       iret = grb2_mk_inv(file1, inv)
       if (iret.ne.0) stop 1
       str_date = ":d="//thiscdate//":"

!       readin 2m-T, 2m-spfh, and terrain
       var = ':TMP:2 m above ground:'
       iret = grb2_inq(file1,inv,var,str_date,data2=tfield,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
       if (iret.ne.1) stop 4
       write(*,*) 'tfield,min,max=',minval(tfield,mask=tfield<spval2),maxval(tfield,mask=tfield<spval2)
       write(*,*) 'lat/lon=',lat(1,1),lon(1,1)
       write(*,*) 'inventory=',trim(invline)
       write(*,*) 'grid=',trim(gridline)

       var = ':SPFH:2 m above ground:'
       iret = grb2_inq(file1,inv,var,str_date,data2=field1,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
       if (iret.ne.1) stop 4
       write(*,*) 'spfh,min,max=',minval(field1,mask=field1<spval2),maxval(field1,mask=field1<spval2)
       write(*,*) 'lat/lon=',lat(1,1),lon(1,1)
       write(*,*) 'inventory=',trim(invline)
       write(*,*) 'grid=',trim(gridline)

       var = ':HGT:surface:'
       iret = grb2_inq(file1,inv,var,str_date,data2=terrain,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
       if (iret.ne.1) stop 4
       write(*,*) 'terrain,min,max=',minval(terrain,mask=terrain<spval2),maxval(terrain,mask=terrain<spval2)
       write(*,*) 'lat/lon=',lat(1,1),lon(1,1)
       write(*,*) 'inventory=',trim(invline)
       write(*,*) 'grid=',trim(gridline)

!       compute virtual temperature 
       do j=1,ny
       do i=1,nx
         if (tfield(i,j) < spval2)  then 
             tvfield(i,j)=tfield(i,j)*(1.+0.608*max(0.,field1(i,j))) !virtual temperature
           else
             tvfield(i,j)=tfield(i,j) ! port bitmap value over
         endif
       enddo
       enddo

       if (mype==0) then
          print*,'in stn_interpolator: terrain,min,max=',minval(terrain,mask=terrain<spval2), & 
                                                         maxval(terrain,mask=terrain<spval2)

          print*,'in stn_interpolator: tfield,min,max=', minval(tfield,mask=tfield<spval2), & 
                                                         maxval(tfield,mask=tfield<spval2)

          print*,'in stn_interpolator: tvfield,min,max=',minval(tvfield,mask=tvfield<spval2), & 
                                                         maxval(tvfield,mask=tvfield<spval2)
       endif
 
!compute airport weather status list
       do ifield=1,nt 

          if (mype==0) then
             print*,'======================================================================'
             print*,'======================================================================'
             print*,'in stn_interpolator: ifield,cfldname=',ifield,trim(cfldname(ifield))
          endif

          if (ifield==4) then
             iret = grb2_inq(file1,inv,trim(var_u),str_date,data2=field1,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
             if (iret.ne.1) then; write(*,*) 'grib2 trouble found for UGRD'; stop 4; endif
             write(*,*) 'UGRD,min,max=',minval(field1,mask=field1<spval2), & 
                                          maxval(field1,mask=field1<spval2)

             iret = grb2_inq(file1,inv,trim(var_v),str_date,data2=field2,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
             if (iret.ne.1) then; write(*,*) 'grib2 trouble found for VGRD'; stop 4; endif
             write(*,*) 'VGRD,min,max=',minval(field2,mask=field2<spval2), & 
                                          maxval(field2,mask=field2<spval2)
 
             where(abs(field1)<spval2) field1=sqrt(field1*field1+field2*field2)
           else
             iret = grb2_inq(file1,inv,trim(vars(ifield)),str_date,data2=field1,lat=lat,lon=lon,desc=invline,grid_desc=gridline)
             if (iret.ne.1) then
               write(*,*) 'grib2 trouble found for field=',trim(cfldname(ifield))
               stop 4
             endif
             write(*,*) 'inventory=',trim(invline)
             write(*,*) 'grid=',trim(gridline)
          endif

          if (mype==0) then
             print*,'in stn_interpolator:min,max ',trim(varsshort(ifield)),'=',minval(field1,mask=field1<spval2), & 
                                                                               maxval(field1,mask=field1<spval2)
             print*
             print*,'station   rlat          rlon            xx             yy           fint0'
          endif

          do n=1,nstns
             if (inside(n)) then
                if (viable(n,ifield)) then 
                   
                   if (trim(cinterp_method(ifield))=='nearest_neighbor') then
                      ii=max(1,min(nint(xx(n)),nx))
                      jj=max(1,min(nint(yy(n)),ny))
                      fint0=field1(ii,jj)

                      if (ifield==1) then 
                         zsges=terrain(ii,jj)
                         tges=tvfield(ii,jj)
                      endif
                   else
                      call bilinear_2d0v2(field1,1,nx,1,ny,fint0,yy(n),xx(n)) 
                      if (ifield==1) then 
                         call bilinear_2d0v2(terrain,1,nx,1,ny,zsges,yy(n),xx(n)) 
                         call bilinear_2d0v2(tvfield,1,nx,1,ny,tges,yy(n),xx(n))
                      endif
                   endif

                   if (ifield==1) then 
                       call retrieve_tob_from_diagfile(mype,cstation(n),tob,rtvts,spval)

                      if (mype==0) then 
                          print*,'in stn_interpolator: pressure case / non-adjusted values'
                          print*,'cstation(n),elev(n),zsges=',trim(cstation(n)),elev(n),zsges
                          print*,'cstation(n),tob,tges,fint0=',trim(cstation(n)),tob,tges,fint0
                      endif

                      rdp=0.
                      rdelz=elev(n)-zsges
                      if (abs(tob-spval)>0.001) then
                         tges = half*(tges+tob)
                        else
                         if(rdelz < 0.)then
                            tges=tges-half_tlapse*rdelz
                         endif
                      endif

                      rdp = g_over_rd*rdelz/tges
                      !Adjust hydrostatically
                      pges=fint0
                      fint0=exp(log(pges) - rdp) !output in Pa

                      if (mype==0) then 
                          print*,'in stn_interpolator: pressure case / adjusted values'
                          print*,'cstation(n),tges,fint0=',tges,fint0
                      endif
                   endif

                   if (ifield==1) fint0=altsetting(fint0,elev(n))    !altimeter setting in In Hg
                   if (ifield==2 .or. ifield==3) fint0=fint0-273.15  !temp and dew point in Celsius dg
                   if (ifield==4 .or. ifield==5) fint0=fint0*1.94384 !wind and gust in kts
                   if (ifield==6) fint0=fint0*0.000621371            !vis in mi
                   if (ifield==7 .and. fint0<spval2) fint0=fint0*3.28084 !ceil in ft
                   write(cfint(n,ifield),'(f10.2)') fint0
                else
                   cfint(n,ifield)='N/A'
                   cfint(n,ifield)=adjustr(cfint(n,ifield))
                endif
                lun=75
              else
                fint0=-9999.99
                write(cfint(n,ifield),'(f10.2)') fint0
                lun=76
             endif
             if (mype==0) print*,trim(cstation(n)),rlat(n),rlon(n),xx(n),yy(n),fint0
          enddo
          if (mype==0) then
             print*,'======================================================================'
             print*,'======================================================================'
          endif

       enddo

       do n=1,nstns
          if (inside(n)) then
             lun=75
           else
             lun=76
          endif
          if (mype==0) write(lun,'(a8,f6.2,f8.2, a10,a10)') cstation(n),rlat(n),rlon(n),cfint(n,2),cfint(n,1)
       enddo

       if (mype==0) then
          close(75)
          close(76)
       endif

       deallocate(cstation)
       deallocate(rlat)
       deallocate(rlon)
       deallocate(xx)
       deallocate(yy)
       deallocate(elev)
       deallocate(cfint)
       deallocate(field1)
       deallocate(field2)
       deallocate(inside)
       deallocate(terrain)
       deallocate(tfield)
       deallocate(tvfield)
       deallocate(viable)

       return
       end
!--------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------
       real(4) function altsetting(p,elev0) 
!
!  prgmmr: pondeca
!
!  abstract: compute altimeter setting in "In Hg" given pressure
!            in hPa and station elevation in meters 
!$$$
       implicit none
       real(4),intent(in):: p,elev0

       real(8) p8mb, elev08, result8, a8, b8, c8

       p8mb=dble(p)/100._8
       elev08=dble(elev0)

       a8=(1013.25_8**0.190284_8 * 0.0065_8)/288._8
       b8=elev08/(p8mb-0.3_8)**0.190284_8
       c8= (p8mb-0.3_8)*(1._8+a8*b8)**(1._8/0.190284_8)
       c8=c8*100._8  !convert to Pa
       altsetting=sngl(c8)*0.0002953 !ALT in In Hg

       end function altsetting
!--------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------
         subroutine retrieve_tob_from_diagfile(mype, this_cstn, this_tob, this_qtflg, spval)
!
!  prgmmr: pondeca
!  to compile:
!    ifort -c -convert big_endian retrieve_tob_from_diagfile.f90
!
!  abstract: 
!    read in tob and surface height z0 from the anl diagnostic file
!    when applied to 3dvar, it assumes gsi is run with l_obsprvdiag=.true.

         implicit none

!Declare passed variables
         integer(4),intent(in):: mype
         character(8),intent(in):: this_cstn
         real(4),intent(in):: spval
         real(4),intent(out):: this_tob,this_qtflg

         !Note:
         ! this_shgt0  :station height
         ! this_hgt0   :observation elevation
         ! this_z0     :model terrain at ob location

!Declare local parameters
         integer(4),parameter::lun0=7
         character(8),allocatable,dimension(:):: cdiagbuf
         character(8),allocatable,dimension(:):: cprvstg
         character(8),allocatable,dimension(:):: csprvstg
         character(8) cstation
         character(3) otype

         integer(4) idate,nchar,nreal,i,ii0,mypeout,m,k
         real(4),allocatable,dimension(:,:)::rdiagbuf
         real(4) rlat,rlon
         real(4) dtime,qtflg,rmuse
         integer(4) itype,isubtype
         logical fexist
         integer(4) nlen1, nlen2
         real(4) rsign,dtime0
         real(4) this_dtime_t, this_dtime_q, this_dtime_ps
         real(4) this_qob,this_psob,this_psmodel_orig,this_psmodel,this_shgt0,this_hgt0,this_z0
         integer(4) this_itype
         character(8) this_cstation
         logical ltob,lqob,lpsob
         logical lprvinfoexist

         itype=nint(spval)
         rlat=spval
         rlon=spval

         this_tob=spval
         this_qtflg=spval
         this_qob=spval
         this_psob=spval
         this_psmodel=spval
         this_psmodel_orig=spval
         this_dtime_t=huge(this_dtime_t)
         this_dtime_q=huge(this_dtime_q)
         this_dtime_ps=huge(this_dtime_ps)
         this_shgt0=spval
         this_hgt0=spval
         this_z0=spval
         this_itype=nint(spval)
         this_cstation="88888888"

         nlen1=len_trim(this_cstn)

         inquire(file='diag_conv_anl.dat',exist=fexist)
         if(fexist) then
           open (lun0,file='diag_conv_anl.dat',form='unformatted')
          else
           print*,'WARNING: Missing file diag_conv_anl.dat in subroutine retrieve_tob_from_diagfile.' 
           print*,'Therefore will not used value of observed temperature in height adjustment calculation for surface pressurea'
           print*,'It is acceptable though, since the observed temp is only used to get a better estimate of tges by doing tges=0.5*(tges+tob)'
           return
         endif

         read(lun0,end=200) idate
  
         loop_read_obs: do
            read(lun0,end=200)otype,nchar,nreal,ii0,mypeout

            if (ii0==0) cycle loop_read_obs

            ltob=otype(1:3)=='  t'
            lqob=otype(1:3)=='  q'
            lpsob=otype(2:3)=='ps'

            lprvinfoexist=ltob.or.lqob.or.lpsob.or. &
                          otype(2:3)=='uv' .or. &
                          otype(1:3)=='spd' .or. & 
                          otype(1:3)=='gst' .or. & 
                          otype(1:3)=='hwv' .or. &
                          otype(1:3)=='vis'           !GZ: read provider info for visibility


            allocate(cdiagbuf(ii0))
            allocate(cprvstg(ii0))
            allocate(csprvstg(ii0))
            allocate(rdiagbuf(nreal,ii0))

            read(lun0) cdiagbuf,rdiagbuf
            if (lprvinfoexist) read(lun0) cprvstg,csprvstg

!           if (mype==0) then
!              print*,'in retrieve_tob_from_diagfile: idate=',idate
!              print*,'in retrieve_tob_from_diagfile: otype,nchar,nreal,ii0,mypeout=', & 
!                      otype,nchar,nreal,ii0,mypeout
!           endif

           if (ltob.or.lqob.or.lpsob) then
             do i=1,ii0
              if ((ltob.or.lqob).and.isubtype==-1) cycle !since we do not care for pseudo obs here
              cstation=cdiagbuf(i)
              itype=nint(rdiagbuf(1,i))
              isubtype=nint(rdiagbuf(2,i))
              dtime=rdiagbuf(8,i)
              if (rdiagbuf(11,i) .gt. 1.) then
                  rmuse=rdiagbuf(11,i)*rdiagbuf(12,i)
                else
                  rsign=1.
                  if (rdiagbuf(12,i) /= 0 ) rsign=rdiagbuf(12,i)/abs(rdiagbuf(12,i))
                  rmuse=rdiagbuf(12,i)+rsign*(rdiagbuf(11,i)-float(int(rdiagbuf(11,i))))
                  rmuse=rdiagbuf(12,i)
              endif

              if (ltob) then 
                  dtime0=this_dtime_t
              elseif (lqob) then 
                  dtime0=this_dtime_q
              elseif (lpsob) then 
                  dtime0=this_dtime_ps
              endif
           
              nlen2=len_trim(cstation)
              if ( (itype.eq.187.or.itype.eq.193) .and. (rmuse.gt.0.) .and. (nlen1.eq.nlen2) .and. & 
                    this_cstn(1:nlen1)==cstation(1:nlen1).and. (abs(dtime).lt.abs(dtime0)) ) then
   
                 this_itype=itype        !used for diagnostic purposes only
                 this_cstation=cstation  !used for diagnostic purposes only

                 if (ltob) then 
                     rlat=rdiagbuf(3,i)
                     rlon=rdiagbuf(4,i)
                     this_shgt0=rdiagbuf(5,i)
                     this_hgt0=rdiagbuf(7,i)
                     this_tob=rdiagbuf(17,i)
                     this_qtflg=rdiagbuf(10,i)
                     this_dtime_t=dtime
                     this_z0=rdiagbuf(22,i)
                 elseif (lqob) then 
                     this_qob=rdiagbuf(17,i)
                     this_dtime_q=dtime
                 elseif (lpsob) then 
                     this_psob=rdiagbuf(17,i)
                     this_psmodel=rdiagbuf(17,i)-rdiagbuf(18,i)
                     this_psmodel_orig=rdiagbuf(17,i)-rdiagbuf(19,i)
                     this_dtime_ps=dtime
                 endif

              endif
             enddo
           end if

           deallocate(cdiagbuf,rdiagbuf)
           deallocate(cprvstg,csprvstg)
        enddo loop_read_obs
200     continue
        close(lun0)

        if (mype==0) then 
            print*,'in in retrieve_tob_from_diagfile'
            print*,'this_cstn,this_cstation=',trim(this_cstn),trim(this_cstation)
            print*,'rlat,rlon=',rlat,rlon
            print*,'this_itype=',this_itype
            print*,'this_dtime_t,this_dtime_q,this_dtime_ps=',this_dtime_t,this_dtime_q,this_dtime_ps
            print*,'this_tob,this_qtflg=',this_tob,this_qtflg
            print*,'this_qob,this_psob,this_psmodel_orig,this_psmodel=',this_qob,this_psob,this_psmodel_orig,this_psmodel
            print*,'this_shgt0,this_hgt0,this_z0=',this_shgt0,this_hgt0,this_z0
        endif

        return
end subroutine retrieve_tob_from_diagfile
!--------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------
       subroutine  getmonth(n,month)

       implicit none

       integer(4),intent(in)::n
       character(3),intent(out)::month

       if (n==1)  month='Jan'
       if (n==2)  month='Feb'
       if (n==3)  month='Mar'
       if (n==4)  month='Apr'
       if (n==5)  month='May'
       if (n==6)  month='Jun'
       if (n==7)  month='Jul'
       if (n==8)  month='Aug'
       if (n==9)  month='Sep'
       if (n==10) month='Oct'
       if (n==11) month='Nov'
       if (n==12) month='Dec'

       end

!------------------------------------------------------
!------------------------------------------------------
      subroutine bilinear_2d0v2(rffcst,ix1,ix2,jx1,jx2,rfobs,xx,yy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilinear_2d0v2
!   prgmmr:
!
! abstract:
!
!
!   input argument list:
!    rffcst               - model grid value
!    ix,jx
!    xx,yy                - define coordinates in grid units
!                         of point for which interpolation is
!                         performed
!
!   output argument list:
!    rfobs                - interpolated value
!
! notes:
!
!     i+1,j |          | i+1,j+1
!         --+----------+---
!           |          | dym
!           |    *     + -
!           |   x,y    | dy
!           |          |
!         --+----+-----+---
!        i,j|<dx>|<dxm>| i,j+1
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

!declare passed variables
      integer(4),intent(in   ) :: ix1,ix2,jx1,jx2
      real(4) ,intent(in   ) :: rffcst(ix1:ix2,jx1:jx2)
      real(4) ,intent(in   ) :: xx,yy
      real(4) ,intent(  out) :: rfobs

!declare local variables
      integer(4) i,j,ip,jp
      real(4) dx,dy,dxm,dym
!      print*,'ix1,ix2,jx1,jx2,rfobs,xx,yy=',ix1,ix2,jx1,jx2,rfobs,xx,yy
      i  = ifix(yy)
      j  = ifix(xx)
      
      dx = xx - float(j)
      dy = yy - float(i)
      dxm= 1.0-dx
      dym= 1.0-dy
 
      i=min(max(ix1,i),ix2) ; j=min(max(jx1,j),jx2)
      ip=min(ix2,i+1)     ; jp=min(jx2,j+1) 

      rfobs=dxm*(dym*rffcst(i,j)+dy*rffcst(ip,j)) &
               + dx *(dym*rffcst(i,jp)+dy*rffcst(ip,jp))

      return
      end subroutine bilinear_2d0v2

