   subroutine smoke_init (id,chem, &
                         config_flags,grid, &
                         ids,ide, jds,jde, kds,kde, &
                         ims,ime, jms,jme, kms,kme, &
                         its,ite, jts,jte, kts,kte )
   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_dep_simple
   USE module_input_smoke_data, only: last_chem_time
   IMPLICIT NONE
   INTEGER, INTENT(IN ) :: id, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) , &
          INTENT(INOUT ) :: chem
   TYPE (grid_config_rec_type) , INTENT (in) :: config_flags
   TYPE(domain) , INTENT (inout) :: grid
   CHARACTER*256 :: mminlu_loc
   CHARACTER*256 :: message_txt
   TYPE(WRFU_TimeInterval) :: tmpTimeInterval
   integer :: i,j,k,l,numgas,ixhour,n,ndystep,kk,nv
   real, DIMENSION (1,1) :: sza,cosszax
   real :: xtime,xhour,xmin,gmtp,xlonn,rlat
   CHARACTER (LEN=10) :: release_version = 'V3.9pre#2 '
   program_name = "*             PROGRAM:WRF-Smoke " // TRIM(release_version) // " MODEL"
call wrf_message("*********************************************************************")
call wrf_message(program_name)
call wrf_message("*                                                                   *")
call wrf_message("*            PLEASE REPORT ANY BUGS TO                              *")
call wrf_message("*              ravan.ahmadov@noaa.gov                               *")
call wrf_message("*                                                                   *")
call wrf_message("*********************************************************************")
    numgas = 0
    IF ( config_flags%chem_opt /= 18 .OR. config_flags%tracer_opt>0) THEN
        call wrf_error_fatal3("<stdin>",56,&
" ERROR: This version of WRF-Chem doesn't work with other chemistry or tracer options ")
    ENDIF
    IF ( config_flags%flam_part>1.) THEN
        call wrf_error_fatal3("<stdin>",60,&
" ERROR: too high flam_part ")
    ENDIF
    IF (config_flags%plumerise_flag/=2 .AND. config_flags%flam_part>0.) THEN
       call wrf_error_fatal3("<stdin>",64,&
" ERROR: check flam_part and plumerise_flag for consistency! ")
    ENDIF
    CALL nl_get_mminlu( 1, mminlu_loc )
    IF ( trim(mminlu_loc) /= 'MODIFIED_IGBP_MODIS_NOAH' ) THEN
         print*,mminlu_loc
         message_txt = " ERROR: HRRR-Smoke: The smoke routines require MODIS_NOAH land use maps. Need to change land use option."
         call wrf_error_fatal3("<stdin>",71,&
trim(message_txt) )
    ELSEIF (trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' .and. grid%num_land_cat <= 19 ) THEN
            message_txt = " ERROR: CHEM_INIT: MODIS_NOAH land use map should have 20 or more catagories."
            call wrf_error_fatal3("<stdin>",75,&
trim(message_txt) )
    ENDIF
    grid%stepchem = nint(grid%chemdt*60./grid%dt)
    grid%stepchem = max(grid%stepchem,1)
    grid%stepfirepl= nint(grid%plumerisefire_frq*60/grid%dt)
    grid%stepfirepl= max(grid%stepfirepl,1)
    call wrf_debug( 15, 'in smoke_init' )
     do j=jts,jte
       do i=its,ite
          grid%lu_fire1(i,j)= grid%landusef(i,11,j) + grid%landusef(i,12,j) + grid%landusef(i,13,j) + grid%landusef(i,14,j) + grid%landusef(i,15,j) + &
                              grid%landusef(i,16,j) + grid%landusef(i,17,j) + grid%landusef(i,18,j) + grid%landusef(i,19,j) + grid%landusef(i,20,j)
          if (grid%xlong(i,j)<-130.) then
              grid%peak_hr(i,j)= 0.0* 3600.
          elseif(grid%xlong(i,j)<-115.) then
              grid%peak_hr(i,j)= 23.0* 3600.
          elseif (grid%xlong(i,j)<-100.) then
              grid%peak_hr(i,j)= 22.0* 3600.
          elseif (grid%xlong(i,j)<-85.) then
              grid%peak_hr(i,j)= 21.0* 3600.
          elseif (grid%xlong(i,j)<-70.) then
              grid%peak_hr(i,j)= 20.0* 3600.
          else
              grid%peak_hr(i,j)= 19.0* 3600.
          endif
       enddo
     enddo
   if( .NOT. config_flags%restart ) then
         if(config_flags%chem_in_opt == 0 )then
               do j=jts,jte
                 do i=its,ite
                    do k=kts,kte
                      chem(i,k,j,:)= 1.e-16
                    enddo
                 enddo
               enddo
          endif
    endif
   if( config_flags%restart ) then
       call wrf_debug( 15, "Setting last_chem_time from restart file" )
       call WRFU_TimeSet( last_chem_time(id), &
                          YY = grid%last_chem_time_year, &
                          MM = grid%last_chem_time_month, &
                          DD = grid%last_chem_time_day, &
                          H = grid%last_chem_time_hour, &
                          M = grid%last_chem_time_minute, &
                          S = grid%last_chem_time_second )
   else
       call wrf_debug( 15, "Setting last_chem_time to model start time-dt" )
       call WRFU_TimeIntervalSet(tmpTimeInterval, s_=real(grid%dt,8))
       last_chem_time(id) = domain_get_current_time(grid) - tmpTimeInterval
   end if
   IF (config_flags%debug_chem) THEN
      WRITE(*,*) 'smoke_init: grid%xlong(its,jts),grid%xlat(its,jts),peak_hr(its,jts) ',grid%xlong(its,jts),grid%xlat(its,jts),grid%peak_hr(its,jts)
      WRITE(*,*) 'smoke_init: grid%xlong(ite,jte),grid%xlat(ite,jte),peak_hr(ite,jte) ',grid%xlong(ite,jte),grid%xlat(ite,jte),grid%peak_hr(ite,jte)
   END IF
    END SUBROUTINE smoke_init
