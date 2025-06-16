         subroutine latlon_to_grid(da8,alat18,elon18,elonv8,alatan8, &
                                   rlat8,rlon8,cgrid,xx8,yy8)

!***********************************************************************
! abstract: use w3 subroutines to convert from lat/lon units to        *
!           grid units xx, yy                                          *
!                                                                      *
! program history log:                                                 *
!   2005-10-08  pondeca                                                *
!***********************************************************************

         implicit none


         character(60),intent(in):: cgrid

         real(8),intent(in):: da8,alat18,elon18,elonv8,alatan8, & 
                 rlat8,rlon8
         real(8),intent(in):: xx8,yy8

         if (trim(cgrid)=='conus' .or. trim(cgrid)=='cohres' .or. trim(cgrid)=='hrrr' .or.  & 
                                trim(cgrid)=='cohresext' .or. trim(cgrid)=='cohreswexp') then
          call w3fb11_8(rlat8,rlon8,alat18,elon18,da8,elonv8,alatan8,xx8,yy8)

         elseif (trim(cgrid)=='alaska' .or. trim(cgrid)=='akhres' .or. trim(cgrid)=='juneau' .or. & 
                                                                       trim(cgrid)=='akhrrr') then
          call w3fb06_8(rlat8,rlon8,alat18,elon18,da8,elonv8,xx8,yy8) 

         elseif (trim(cgrid)=='hawaii' .or. trim(cgrid)=='guam' .or. trim(cgrid)=='prico') then
          call w3fb08_8(rlat8,rlon8,alat18,elon18,alatan8,da8,xx8,yy8)
         else
          print*,'trouble in latlon_to_grid: unknown cgrid: ',trim(cgrid)
         endif

         return
         end
     

