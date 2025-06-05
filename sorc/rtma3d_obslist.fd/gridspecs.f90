       subroutine gridspecs(cgrid,nx,ny,ds,alat1,elon1,elonv,alatan,alat2,elon2,alad, &
                            sp_lon, sp_lat, origin_rlon, origin_rlat, & 
                            dlon,   dlat,   offset_x,    offset_y, & 
                            llcnr1, llcnr2,  urcnr1,     urcnr2) 

!***********************************************************************
! abstract: given the rtma grid name, retrieve the grid's navigation   *
!           information                                                *
!                                                                      *
! program history log:                                                 *
!   2024-03-27  pondeca                                                *
!***********************************************************************

       implicit none

       character(*),intent(in):: cgrid
       integer(4),intent(out):: nx,ny
       real(8),intent(out):: ds,alat1,elon1,elonv,alatan,elon2,alat2,alad
       real(8),intent(out):: sp_lon,sp_lat,origin_rlon,origin_rlat,dlon,dlat, & 
                             offset_x,offset_y,llcnr1,llcnr2,urcnr1,urcnr2 

       elon2=-9999._8
       alad=-9999._8
       elonv=-9999._8
       alatan=-9999._8

       if (trim(cgrid) == 'conus_ndfd') then
         nx=2145
         ny=1377
         alat1=20.192_8
         elon1=238.446_8
         ds=2539.703_8
         elonv=265.000_8
         alatan=25.000_8

        elseif (trim(cgrid) == 'alaska_ndfd') then
         nx=1649
         ny=1105
         alat1=40.530101_8
         elon1=181.429000_8
         ds=2976.563_8
         elonv=210.000000_8
         alatan=60.000000_8

        elseif (trim(cgrid) == 'hawaii_ndfd') then
         nx=321
         ny=225
         alat1=18.072699_8
         elon1=198.474999_8
         alat2=23.087799_8
         elon2=206.130999_8
         alad=20.000000_8
         ds=2500.000_8

        elseif (trim(cgrid) == 'prico_ndfd') then
         nx=353
         ny=257
         alat1=16.828700_8
         elon1=291.804700_8
         alat2=19.736200_8 
         elon2=296.015500_8
         alad=20.000000_8
         ds=1250.000_8

        elseif (trim(cgrid) == 'guam_ndfd') then
         nx=193
         ny=193
         alat1=12.349884_8
         elon1=143.686538_8
         alat2=16.794399_8
         elon2=148.280000_8
         alad=20.000000_8
         ds=2500.000_8

        elseif (trim(cgrid) == 'hrrr') then
         nx=1799
         ny=1059
         alat1=21.138000_8
         elon1=237.280000_8
         ds=3000.000_8
         elonv=262.500000_8
         alatan=38.500000_8

        elseif (trim(cgrid) == 'conus_ndfd_ext') then
         nx=2145
         ny=1597
         alat1=20.192_8
         elon1=238.446_8
         ds=2539.703_8
         elonv=265.000_8
         alatan=25.000_8

        elseif (trim(cgrid) == 'conus_ndfd_wexp') then
         nx=2345
         ny=1597
         alat1=19.228976_8
         elon1=233.723448_8
         ds=2539.703_8
         elonv=265.000_8
         alatan=25.000_8

        elseif (trim(cgrid) == 'nwrfc') then
         nx=709
         ny=795
         alat1=37.979684_8
         elon1=234.042704_8
         ds=2539.703_8
         elonv=265.000_8
         alatan=25.000_8
        elseif (trim(cgrid) == 'na_rotated_ll') then
         nx=4881
         ny=2961
         sp_lon = 247.0_8       !lon of south pole of rotated grid
         sp_lat = -35.0_8       !lat of south pole of rotated grid
         origin_rlon = 299.0_8  !lon of origin point of the rotated grid
         origin_rlat = -37.0_8  !lat of origin point of the rotated grid
         dlon = 0.025_8         !longitudonal grid spacing in degress
         dlat = 0.025_8         !latitudinal grid spacing in degress
         offset_x = 1.0_8       !x-coordinate of origin point
         offset_y = 1.0_8       !y-coordinate of origin point
         llcnr1 = 299.0_8       !lon of lower-left corner of rotated grid
         llcnr2 = -37.0_8       !lat of lower-left corner of rotated grid
         urcnr1 =  61.0_8       !lon of upper-right corner of rotated grid
         urcnr2 =  37.0_8       !lat of upper-right corner of rotated grid
        elseif (trim(cgrid) == 'akhrrr') then
         nx=1299
         ny=919
         alat1=41.612949_8
         elon1=185.117126_8
         ds=3000.000000_8
         elonv=225.000000_8
         alatan=60.000000_8
        else
         print*,'in  gridspecs: unknown grid: ',trim(cgrid),'... aborting ...'
         call abort
         stop
       endif
       end subroutine gridspecs
