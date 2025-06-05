  program prdgen

!***********************************************************************
! abstract: Perform various post UPP taska, inclusing:                 *
!     1. Compose "text" obs listing files from the gsi diagnostic file *
!     2. Reconcile wind gust analysis with wind speed analysis         *
!     3. Peform dignificant wave height analysis adjustments           *
!     4. Evoke smartinit to downcale 3drtma to NDFD grids              *
!     5. Write out final 3drtma grib2 output files                     *
!     6. Compute RTMA weather status report list for FAA usage         *               
!                                                                      *
! program history log:                                                 *
!   2024-03-26  pondeca                                                *
!***********************************************************************

  implicit none

  integer(4),parameter:: mype=0 !in anticipation for adding mpi

  logical gustadjust,howvadjust,sfcfld_donwnscale, & 
          finalgrib2file,sfcobs_lists,airportlist
  logical fexist

  namelist /do_steps/gustadjust,howvadjust,sfcfld_donwnscale, & 
                     finalgrib2file,sfcobs_lists,airportlist

  data gustadjust          /.false./
  data howvadjust          /.false./
  data sfcfld_donwnscale   /.false./
  data finalgrib2file      /.false./
  data sfcobs_lists        /.false./
  data airportlist         /.false./

  inquire(file='do_steps_input',exist=fexist)
  if(fexist) then
     open (55,file='do_steps_input',form='formatted')
     read (55,do_steps)
     close(55)
     if (mype==0) then
        print*,'in prdgen :'
        print*,'gustadjust=',gustadjust
        print*,'howvadjust=',howvadjust
        print*,'sfcfld_donwnscale=',sfcfld_donwnscale
        print*,'finalgrib2file=',finalgrib2file
        print*,'sfcobs_lists=',sfcobs_lists
        print*,'airportlist=',airportlist
     endif
  endif
!
!----------------------------------------------------------------
!==> adjust gust analysis
!----------------------------------------------------------------
!if (gustadjust) call gust_adjustment()

!----------------------------------------------------------------
!==> adjust significant wave height analysis
!----------------------------------------------------------------
! if (howvadjust) call howv_adjustment()

!----------------------------------------------------------------
!==> downscale 3drtma output to consensus terrain
!----------------------------------------------------------------
! if (sfcfld_donwnscale) call smartinit

!----------------------------------------------------------------
!==> write final grb2 aalysis files
!----------------------------------------------------------------
! if (finalgrib2file) call final_grb2_analysis()

!----------------------------------------------------------------
!==> compose 'obs listing' text files from gsi diagnostics files
!----------------------------------------------------------------
  if (sfcobs_lists) call get_ob_lists3d()

!----------------------------------------------------------------
!==> compute rtma weather report status list for faa
!----------------------------------------------------------------
  if (airportlist) call weather_report_status_list()
!----------------------------------------------------------------
end
