   MODULE module_check_a_mundo
      USE module_state_description
      USE module_wrf_error
      USE module_configure
      IMPLICIT NONE
   CONTAINS
   SUBROUTINE check_nml_consistency
      IMPLICIT NONE
      LOGICAL :: exists, vnest
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      INTEGER :: i, j, oops, d1_value
      LOGICAL :: km_opt_already_done , diff_opt_already_done
      LOGICAL :: rinblw_already_done
      LOGICAL :: fatal_error
      INTEGER :: count_fatal_error
   fatal_error = .false.
   count_fatal_error = 0
   model_config_rec % wrf_hydro = 0
      IF ( ( model_config_rec % use_theta_m .EQ. 1 ) .AND. &
           ( model_config_rec % damp_opt .EQ. 2 ) ) THEN
         CALL wrf_message ( "The use_theta_m option may not be paired with damp_opt=2." )
         wrf_err_message = '--- ERROR: Either turn off use_theta_m, or select a different damp_opt option'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      IF ( ( model_config_rec % use_theta_m .EQ. 1 ) .AND. &
           ( model_config_rec % rad_nudge .EQ. 1 ) ) THEN
         CALL wrf_message ( "The use_theta_m option may not be paired with rad_nudge=1." )
         wrf_err_message = '--- ERROR: Either turn off use_theta_m, or turn off the rad_nudge option'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      km_opt_already_done = .FALSE.
      diff_opt_already_done = .FALSE.
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. -1 ) THEN
            model_config_rec % km_opt(i) = model_config_rec % km_opt(1)
            IF ( .NOT. km_opt_already_done ) THEN
               CALL wrf_message ( "Setting blank km_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The km_opt entry in the namelist.input is now max_domains." )
            END IF
            km_opt_already_done = .TRUE.
         END IF
         IF ( model_config_rec % diff_opt(i) .EQ. -1 ) THEN
            model_config_rec % diff_opt(i) = model_config_rec % diff_opt(1)
            IF ( .NOT. diff_opt_already_done ) THEN
               CALL wrf_message ( "Setting blank diff_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The diff_opt entry in the namelist.input is now max_domains." )
            END IF
            diff_opt_already_done = .TRUE.
         END IF
      ENDDO
      IF ( ( model_config_rec % km_opt(1) .EQ. -1 ) .OR. &
           ( model_config_rec % diff_opt(1) .EQ. -1 ) ) THEN
            wrf_err_message = '--- ERROR: Both km_opt and diff_opt need to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_surface_physics(i) .NE. &
              model_config_rec % sf_surface_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_surface_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_surface_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_sfclay_physics(i) .NE. &
              model_config_rec % sf_sfclay_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_sfclay_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_sfclay_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % mp_physics(i) .NE. &
              model_config_rec % mp_physics(1) ) THEN
            wrf_err_message = '--- NOTE: mp_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE:     ----> Setting all mp_physics entries to value defined in the inner most domain'
            CALL wrf_message ( TRIM( wrf_err_message ) )
         END IF
      ENDDO
      d1_value = model_config_rec%mp_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%mp_physics(i) = d1_value
      END DO
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_lw_physics(i) .NE. &
              model_config_rec % ra_lw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_lw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_sw_physics(i) .NE. &
              model_config_rec % ra_sw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_sw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_sw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
         IF ( ( model_config_rec % use_wps_input == 0 ) .AND. &
              ( model_config_rec % time_step .EQ. -1 ) ) THEN
            wrf_err_message = '--- ERROR: Known problem.  time_step must be set to a positive integer'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. model_config_rec % bl_pbl_physics(1) ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. 0 ) ) THEN
            wrf_err_message = '--- ERROR: bl_pbl_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix bl_pbl_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % cu_physics(i) .NE. model_config_rec % cu_physics(1) ) .AND. &
              ( model_config_rec % cu_physics(i) .NE. 0 ) ) THEN
            wrf_err_message = '--- ERROR: cu_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix cu_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      IF ( ( model_config_rec%fractional_seaice .EQ. 0 ).AND. &
              ( model_config_rec%tice2tsk_if2cold ) ) THEN
            wrf_err_message = '--- WARNING: You set tice2tsk_if2cold = .true.,  but fractional_seaice = 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- WARNING: tice2tsk_if2cold will have no effect on results.'
            CALL wrf_message ( wrf_err_message )
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%fine_input_stream(i) .NE. 0 ).AND. &
              ( model_config_rec%io_form_auxinput2 .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If fine_input_stream /= 0, io_form_auxinput2 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput2 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
            IF ( model_config_rec%num_metgrid_levels .LE. 20 ) THEN
            CALL wrf_message ( 'Linear vertical interpolation is recommended with input vertical resolution this coarse, changing lagrange_order to 1' )
            model_config_rec%lagrange_order = 1
            END IF
      d1_value = model_config_rec%sf_urban_physics(1)
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec%sf_urban_physics(i) /= d1_value ) THEN
            WRITE(wrf_err_message, * ) '--- NOTE:   sf_urban_physics option must be identical in each domain'
            CALL wrf_message ( TRIM ( wrf_err_message ) )
            WRITE(wrf_err_message, * ) '--- NOTE:   ----> Resetting namelist values to that defined on the inner most domain'
            CALL wrf_message ( TRIM ( wrf_err_message ) )
         ENDIF
      END DO
      d1_value = model_config_rec%sf_urban_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%sf_urban_physics(i) = d1_value
      END DO
      IF ( model_config_rec%seaice_albedo_opt == 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( model_config_rec%sf_surface_physics(i) /= LSMSCHEME ) .AND. &
                 ( model_config_rec%sf_surface_physics(i) /= NOAHMPSCHEME ) ) THEN
               write (wrf_err_message, '(" --- ERROR:   seaice_albedo_opt == 1 works only with ")')
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               write (wrf_err_message, '("              sf_surface_physics == ", I2, " (Noah) or ", I2, " (Noah-MP).")') &
               LSMSCHEME, NOAHMPSCHEME
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
            END IF
         END DO
      END IF
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % sppt(i) .ne. 0) then
           model_config_rec % sppt_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or. &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc. are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for SPPT'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               fatal_error = .true.
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % rand_perturb(i) .ne. 0) then
           model_config_rec % rand_perturb_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or. &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               fatal_error = .true.
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF (( model_config_rec % spp_conv(i) .ne. 0).or.( model_config_rec % spp_pbl(i) .ne. 0).or.( model_config_rec % spp_mp(i) .ne. 0)&
              .or. (model_config_rec % spp_lsm(i) .ne. 0).or. ( model_config_rec % spp(i) .ne. 0)) then
           model_config_rec % spp_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or. &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               fatal_error = .true.
               count_fatal_error = count_fatal_error + 1
           endif
         endif
         IF ( model_config_rec % spp(i) .ne. 0) then
           model_config_rec % spp_conv=1
           model_config_rec % spp_pbl=1
           model_config_rec % spp_mp=1
           model_config_rec % spp_lsm=1
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_vertstruc_opt(i) ==1 ) then
           model_config_rec % skebs_vertstruc=1
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_vertstruc_opt" is obsolete.'
           CALL wrf_message ( wrf_err_message )
           wrf_err_message = '             Please replace with namelist parameter "skebs_vertstruc" in V3.7 and later versions.'
           CALL wrf_message ( wrf_err_message )
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_force_opt(i) ==1 ) then
           model_config_rec % skebs(i)=1
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_force_opt" is obsolete.'
           CALL wrf_message ( wrf_err_message )
           wrf_err_message = '             Please replace with namelist parameter "skebs" in V3.7 and later versions.'
           CALL wrf_message ( wrf_err_message )
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % skebs(i) .ne. 0) then
           model_config_rec % skebs_on=1
         endif
   ENDDO
   IF ( model_config_rec % skebs_vertstruc .ne. 99 ) then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % sppt_vertstruc .ne. 99 ) then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % rand_pert_vertstruc .ne. 99 ) then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % perturb_bdy .EQ. 1 ) then
        model_config_rec % skebs_on=1
      wrf_err_message = '--- WARNING: perturb_bdy=1 option uses SKEBS pattern and may'
      CALL wrf_message ( wrf_err_message )
      wrf_err_message = '             increase computation time.'
      CALL wrf_message ( wrf_err_message )
   ENDIF
   IF ( model_config_rec % perturb_chem_bdy .EQ. 1 ) then
      wrf_err_message = '--- ERROR: This option is only for WRF_CHEM.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      model_config_rec % rand_perturb_on=1
      wrf_err_message = '--- WARNING: perturb_chem_bdy=1 option uses RAND pattern and may'
      CALL wrf_message ( wrf_err_message )
      wrf_err_message = '             increase computation time.'
      CALL wrf_message ( wrf_err_message )
   ENDIF
   IF ( ( model_config_rec%traj_opt .EQ. 0 ) .AND. &
        ( model_config_rec%num_traj .NE. 0 ) ) THEN
         WRITE (wrf_err_message, FMT='(A,A)') '--- WARNING: traj_opt is zero, but ', &
                'num_traj is not zero; setting num_traj to zero.'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%num_traj = 0
   END IF
      IF ( model_config_rec%hypsometric_opt .EQ. 2 &
           .AND. model_config_rec%adjust_heights ) THEN
           WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: hypsometric_opt is 2, ', &
                  'setting adjust_heights = F'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%adjust_heights = .false.
      ENDIF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. YSUSCHEME ) .AND. &
              ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) ) THEN
            oops = oops + 1
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: bl_pbl_physics must be set to 1 for cu_physics = 11 '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Fix bl_pbl_physics in namelist.input OR use another cu_physics option '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) THEN
            WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: cu_physics is 11, ', &
                  'setting icloud = 1 and cu_rad_feedback = T'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%cu_rad_feedback(i) = .true.
            model_config_rec%icloud = 1
         END IF
      ENDDO
      IF ( model_config_rec%sst_update .EQ. 0 ) THEN
         model_config_rec%io_form_auxinput4 = 0
         DO i = 1, model_config_rec % max_dom
            WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: sst_update is 0, ', &
                  'setting io_form_auxinput4 = 0 and auxinput4_interval = 0 for all domains'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%auxinput4_interval(i) = 0
            model_config_rec%auxinput4_interval_y(i) = 0
            model_config_rec%auxinput4_interval_d(i) = 0
            model_config_rec%auxinput4_interval_h(i) = 0
            model_config_rec%auxinput4_interval_m(i) = 0
            model_config_rec%auxinput4_interval_s(i) = 0
         ENDDO
      ELSE
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF
      model_config_rec%alloc_qndropsource = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%progn(i) .EQ. 1 ) THEN
            model_config_rec%alloc_qndropsource = 1
         END IF
      END DO
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%grid_sfdda(i) .GT. 0 ).AND. &
              ( model_config_rec%grid_fdda (i) .NE. 1 ) ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda >= 1, then grid_fdda must also = 1 for that domain '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Change grid_fdda or grid_sfdda in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%grid_fdda(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: grid_fdda is 0 for domain ', &
                         i, ', setting gfdda interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%gfdda_end_y(i) = 0
            model_config_rec%gfdda_end_d(i) = 0
            model_config_rec%gfdda_end_h(i) = 0
            model_config_rec%gfdda_end_m(i) = 0
            model_config_rec%gfdda_end_s(i) = 0
            model_config_rec%gfdda_interval(i) = 0
            model_config_rec%gfdda_interval_y(i) = 0
            model_config_rec%gfdda_interval_d(i) = 0
            model_config_rec%gfdda_interval_h(i) = 0
            model_config_rec%gfdda_interval_m(i) = 0
            model_config_rec%gfdda_interval_s(i) = 0
         END IF
         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 0 ) .AND. &
              ( model_config_rec%pxlsm_soil_nudge(i) .EQ. 0 ) ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') &
                         '--- NOTE: both grid_sfdda and pxlsm_soil_nudge are 0 for domain ', &
                         i, ', setting sgfdda interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%sgfdda_end_y(i) = 0
            model_config_rec%sgfdda_end_d(i) = 0
            model_config_rec%sgfdda_end_h(i) = 0
            model_config_rec%sgfdda_end_m(i) = 0
            model_config_rec%sgfdda_end_s(i) = 0
            model_config_rec%sgfdda_interval(i) = 0
            model_config_rec%sgfdda_interval_y(i) = 0
            model_config_rec%sgfdda_interval_d(i) = 0
            model_config_rec%sgfdda_interval_h(i) = 0
            model_config_rec%sgfdda_interval_m(i) = 0
            model_config_rec%sgfdda_interval_s(i) = 0
         END IF
         IF ( model_config_rec%obs_nudge_opt(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: obs_nudge_opt is 0 for domain ', &
                         i, ', setting obs nudging interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%fdda_end(i) = 0
            model_config_rec%auxinput11_interval(i) = 0
            model_config_rec%auxinput11_interval_y(i) = 0
            model_config_rec%auxinput11_interval_d(i) = 0
            model_config_rec%auxinput11_interval_h(i) = 0
            model_config_rec%auxinput11_interval_m(i) = 0
            model_config_rec%auxinput11_interval_s(i) = 0
            model_config_rec%auxinput11_end(i) = 0
            model_config_rec%auxinput11_end_y(i) = 0
            model_config_rec%auxinput11_end_d(i) = 0
            model_config_rec%auxinput11_end_h(i) = 0
            model_config_rec%auxinput11_end_m(i) = 0
            model_config_rec%auxinput11_end_s(i) = 0
         END IF
      ENDDO
      DO i = 1, model_config_rec % max_dom
         model_config_rec%fasdas(i) = 0
         IF ( model_config_rec%grid_sfdda(i) .EQ. 2 ) THEN
            model_config_rec%fasdas(i) = 1
         END IF
      ENDDO
    rinblw_already_done = .FALSE.
    DO j = 1, model_config_rec%max_dom
    IF (model_config_rec%grid_sfdda(j) .EQ. 1 ) THEN
      DO i = 2, model_config_rec%max_dom
         IF ( model_config_rec%rinblw(i) .EQ. -1 ) THEN
            model_config_rec%rinblw(i) = model_config_rec % rinblw(1)
            IF ( .NOT. rinblw_already_done ) THEN
               CALL wrf_message ( "Setting blank rinblw entries to domain #1 values.")
               CALL wrf_message ( " --> The rinblw entry in the namelist.input is now max_domains." )
            END IF
            rinblw_already_done = .TRUE.
         END IF
       ENDDO
       IF ( model_config_rec%rinblw(1) .EQ. -1 ) THEN
            wrf_err_message = '--- ERROR: rinblw needs to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
       END IF
    END IF
    END DO
    DO i = 1, model_config_rec%max_dom
     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
         CALL wrf_message ( "FASDAS is active. Mixed Layer fdda is inactive")
     END IF
     END DO
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. QNSEPBLSCHEME ) .AND. &
              ( model_config_rec%mfshconv(i) .NE. 0 ) ) THEN
            model_config_rec%mfshconv(i) = 0
            oops = oops + 1
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 4, implies mfshconv must be 0, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .EQ. GRIMSSHCUSCHEME ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
            ELSE
               model_config_rec%shcu_physics(i) = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6 implies shcu_physics cannot be 3, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%bl_mynn_edmf(i) .EQ. MYNN_DMP_EDMF ) THEN
               model_config_rec%shcu_physics(i) = 0
               model_config_rec%ishallow = 0
               oops = oops + 1
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'bl_mynn_edmf > 0 requires both shcu_physics=0 & ishallow=0'
         CALL wrf_message ( wrf_err_message )
      END IF
      model_config_rec%cu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .NE. NOCUSCHEME ) THEN
            model_config_rec%cu_used = 1
         END IF
      ENDDO
      model_config_rec%shcu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .NE. NOSHCUSCHEME ) THEN
            model_config_rec%shcu_used = 1
         END IF
      ENDDO
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF (model_config_rec%gwd_opt .GE. 1 ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEMEV36 ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEMEV34 ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2 ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
            ELSE
               model_config_rec%gwd_opt = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6 implies gwd_opt cannot be 1, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%icloud_bl .eq. 1) THEN
           IF ( model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2 .OR. &
                model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3 ) THEN
           ELSE
              model_config_rec%icloud_bl = 0
              oops = oops + 1
           END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'Need MYNN PBL for icloud_bl = 1, resetting to 0'
         CALL wrf_message ( wrf_err_message )
      END IF
      IF ( MAXVAL( model_config_rec%grid_fdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_gfdda = 0
      ELSE
         IF ( model_config_rec%io_form_gfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_fdda /= 0, io_form_gfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_gfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF
      IF ( MAXVAL( model_config_rec%grid_sfdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_sgfdda = 0
      ELSE
         IF ( model_config_rec%io_form_sgfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda /= 0, io_form_sgfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_sgfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF
      IF ( model_config_rec%p_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( MAX ( model_config_rec%auxhist23_interval (i) , &
                         model_config_rec%auxhist23_interval_d(i) , &
                         model_config_rec%auxhist23_interval_h(i) , &
                         model_config_rec%auxhist23_interval_m(i) , &
                         model_config_rec%auxhist23_interval_s(i) ) == 0 ) .OR. &
                 ( model_config_rec%io_form_auxhist23 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: p_lev_diags requires auxhist23 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist23_interval (max_dom) and io_form_auxhist23'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 23 for pressure-level diags'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            model_config_rec%p_lev_interval(i) = model_config_rec%auxhist23_interval (i)* 60 + &
                                                 model_config_rec%auxhist23_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist23_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist23_interval_m(i)* 60 + &
                                                 model_config_rec%auxhist23_interval_s(i)
         END DO
      END IF
      IF ( model_config_rec%z_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( MAX ( model_config_rec%auxhist22_interval (i) , &
                         model_config_rec%auxhist22_interval_d(i) , &
                         model_config_rec%auxhist22_interval_h(i) , &
                         model_config_rec%auxhist22_interval_m(i) , &
                         model_config_rec%auxhist22_interval_s(i) ) == 0 ) .OR. &
                 ( model_config_rec%io_form_auxhist22 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: z_lev_diags requires auxhist22 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist22_interval (max_dom) and io_form_auxhist22'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 22 for height-level diags'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            model_config_rec%z_lev_interval(i) = model_config_rec%auxhist22_interval (i)* 60 + &
                                                 model_config_rec%auxhist22_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist22_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist22_interval_m(i)* 60 + &
                                                 model_config_rec%auxhist22_interval_s(i)
         END DO
      END IF
      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%history_interval(1) .EQ. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  nwp_diagnostics requires the use of "history_interval" namelist.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace interval variable with "history_interval".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%gsd_diagnostics .NE. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  resetting nwp_diagnostics=0 because gsd_diagnostics=1'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%nwp_diagnostics = 0
      END IF
      IF ( model_config_rec%bucket_J .GE. 0. ) THEN
         wrf_err_message = '--- setting bucketf_opt=1 since bucket_J>0.0'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%bucketf_opt = 1
      END IF
      IF ( model_config_rec%bucket_mm .GE. 0. ) THEN
         wrf_err_message = '--- setting bucketr_opt=1 sinse bucket_mm>0.0'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%bucketr_opt = 1
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%hailcast_opt(i) .NE. 0 ) .AND. &
              (model_config_rec%cu_physics(i) .NE. 0) ) THEN
              wrf_err_message = '--- hailcast_opt and cu_physics cannot both be turned on for the same domain. You must turn one of them off (=0).'
              CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
              count_fatal_error = count_fatal_error + 1
         ENDIF
      ENDDO
      IF ( model_config_rec%omlcall .NE. 0 ) THEN
         wrf_err_message = '--- ERROR:  The namelist.input variable "omlcall" has been renamed.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace "omlcall" with the new name "sf_ocean_physics".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      IF ( model_config_rec%use_adaptive_time_step ) THEN
         IF ( ( model_config_rec%cu_physics(1) .EQ. BMJSCHEME ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SCALESASSCHEME) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SASSCHEME ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. OSASSCHEME ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. NSASSCHEME ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. TIEDTKESCHEME ) ) THEN
            wrf_err_message = '--- WARNING: If use_adaptive_time_step, must use cudt=0 for the following CU schemes:'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          BMJ, all SAS, Tiedtke'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          CUDT=0 has been done for you.'
            CALL wrf_message ( wrf_err_message )
            DO i = 1, model_config_rec % max_dom
               model_config_rec%cudt(i) = 0
            END DO
         END IF
      END IF
      IF ( .NOT. model_config_rec%dfi_opt .EQ. DFI_NODFI ) THEN
         IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
            model_config_rec%time_step_dfi = model_config_rec%time_step
            IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
               wrf_err_message = '--- ERROR: DFI Timestep or standard WRF time step must be specified.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
            END IF
         END IF
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_rad_feedback(i) .EQV. .TRUE. ) .OR. &
              ( model_config_rec%cu_rad_feedback(i) .EQV. .true. ) ) THEN
            IF ( ( model_config_rec%cu_physics(1) .EQ. GFSCHEME ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. G3SCHEME ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. GDSCHEME ) ) THEN
               wrf_err_message = '--- WARNING: Turning on cu_rad_feedback also requires setting cu_diag== 1'
               CALL wrf_message ( wrf_err_message )
               model_config_rec%cu_diag(i) = 1
            ELSE
               model_config_rec%cu_diag(i) = 0
            END IF
         END IF
      END DO
       DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_diag(i) .EQ. G3TAVE ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. GDSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. GFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFCUPSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. G3SCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using cu_diag=1 requires use of one of the following CU schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell (G3) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Devenyi (GD) CU scheme'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO
       DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%kf_edrates(i) .EQ. KFEDRATES ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. KFETASCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. MSKFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFSCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using kf_edrates=1 requires use of one of the following KF schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Kain-Fritsch (cu_physics=1)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Multi-scale Kain-Fritsch (cu_physics=11)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          old Kain-Fritsch (cu_physics=99)'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO
      IF ( wrf_dm_on_monitor() ) THEN
         CALL wrf_tsin_exist ( exists )
         IF ( exists ) THEN
            model_config_rec%process_time_series = 1
         ELSE
            model_config_rec%process_time_series = 0
         END IF
      END IF
      CALL wrf_dm_bcast_integer(model_config_rec%process_time_series, 1)
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_physics(i) .EQ. GDSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. GFSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. KFCUPSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. G3SCHEME ) ) THEN
            model_config_rec%cu_diag(i) = 1
         ELSE
            model_config_rec%cu_diag(i) = 0
         END IF
      END DO
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              ( model_config_rec%sf_sfclay_physics(i) .NE. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using bl_pbl_physics=10 requires sf_sfclay_physics=10 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         ELSEIF ( ( model_config_rec%bl_pbl_physics(i) .NE. TEMFPBLSCHEME ) .AND. &
                  ( model_config_rec%sf_sfclay_physics(i) .EQ. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using sf_sfclay_physics=10 requires bl_pbl_physics=10 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      IF ( model_config_rec%tmn_update .EQ. 1 .AND. &
           model_config_rec%lagday .EQ. 1 ) THEN
           wrf_err_message = '--- ERROR: Using tmn_update=1 requires lagday=150 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              (model_config_rec%dfi_opt .NE. DFI_NODFI) ) THEN
            wrf_err_message = '--- ERROR: DFI not available for bl_pbl_physics=10 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
      IF ( model_config_rec%restart ) THEN
         model_config_rec%dfi_opt = DFI_NODFI
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%grav_settling(i) .NE. FOGSETTLING0 ) THEN
                model_config_rec%grav_settling(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: mp_physics == 28, already has gravitational fog settling; resetting grav_settling to 0'
         CALL wrf_message ( wrf_err_message )
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%use_aero_icbc .AND. model_config_rec%scalar_pblmix(i) .NE. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 1
                oops = oops + 1
            END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: For mp_physics == 28 and use_aero_icbc is true, recommend to turn on scalar_pblmix'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = 'resetting scalar_pblmix = 1'
         CALL wrf_message ( wrf_err_message )
      END IF
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ((model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
             (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
            IF ( model_config_rec%bl_mynn_mixscalars(i) .EQ. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: MYNN is set to mix scalars, turning off scalar_pblmix'
         CALL wrf_message ( wrf_err_message )
      END IF
     DO i=1,model_config_rec%max_dom
        IF ((model_config_rec%vert_refine_method(i) .EQ. 2) .AND. (model_config_rec%hybrid_opt .EQ. 2)) THEN
           WRITE(wrf_err_message,'(A)') '--- ERROR: The hybrid vertical coordinate does not work with vertical refinement.'
           CALL wrf_message( wrf_err_message )
           fatal_error = .true.
           count_fatal_error = count_fatal_error + 1
        ENDIF
     END DO
     DO i=1,model_config_rec%max_dom
       IF ((model_config_rec%vert_refine_method(i) .NE. 0) .AND. (model_config_rec%vert_refine_fact .NE. 1)) THEN
         write(wrf_err_message,'(A)') '--- ERROR: vert_refine_fact is ndown specific and cannot be used with vert_refine_method, and vice versa.'
         CALL wrf_message( wrf_err_message )
       ENDIF
     ENDDO
     DO i=1,model_config_rec%max_dom
       IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
         DO j=1,model_config_rec%max_dom
           IF ((model_config_rec%vert_refine_method(i) .NE. model_config_rec%vert_refine_method(j)) .AND. (model_config_rec%vert_refine_method(j) .NE. 0)) THEN
             write(wrf_err_message,'(A,I1,A,I2,A,I1,A,I2,A)') '--- ERROR: vert_refine_method differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Only one type of vertical grid nesting can be used at a time.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
           ENDIF
         ENDDO
       ENDIF
     ENDDO
      IF ((model_config_rec%max_dom .GT. 1) .AND. (model_config_rec%vert_refine_fact .EQ. 1)) THEN
        DO i=1,model_config_rec%max_dom
          IF (((model_config_rec%parent_id(i) .NE. 0) .AND. (model_config_rec%parent_id(i) .NE. model_config_rec%grid_id(i))) .AND. (model_config_rec%vert_refine_method(i) .EQ. 0)) THEN
            DO j=1,model_config_rec%max_dom
              IF ((i .NE. j) .AND. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(j))) THEN
                IF (model_config_rec%e_vert(i) .NE. model_config_rec%e_vert(j)) THEN
                  write(wrf_err_message,'(A,I2,A,I2,A)') '--- ERROR: e_vert differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Set vert_refine_method or make e_vert consistent.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      DO i=1,model_config_rec%max_dom
        IF ((model_config_rec%parent_id(i) .EQ. 0) .OR. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(i))) THEN
          IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
            write(wrf_err_message,'(A,I1,A,I2,A)') '--- ERROR: vert_refine_method=',model_config_rec%vert_refine_method(i),' for grid_id=',model_config_rec%grid_id(i),', must be 0 for a non-nested domain.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO
      DO i = 1, model_config_rec % max_dom
        IF (model_config_rec%vert_refine_method(i) .EQ. 1) THEN
          j = model_config_rec%parent_id(i)
          IF (MOD(model_config_rec%e_vert(i)-1, model_config_rec%e_vert(j)-1) .NE. 0) THEN
            write(wrf_err_message,'(A,I2,A,I2,A)') "--- ERROR: grid_id=",i," and parent (grid_id=",j,") have incompatible e_vert's for vertical nesting with integer refinement."
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO
      DO i = 2, model_config_rec % max_dom
        IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
          IF ( ( ( model_config_rec%ra_lw_physics(i) .EQ. 0 ) .OR. &
                 ( model_config_rec%ra_lw_physics(i) .EQ. RRTMSCHEME ) ) .AND. &
               ( ( model_config_rec%ra_sw_physics(i) .EQ. 0 ) .OR. &
                 ( model_config_rec%ra_sw_physics(i) .EQ. SWRADSCHEME ) ) ) THEN
          ELSE
            wrf_err_message = '--- ERROR: vert_refine_method=2 only works with either RRTM or RRTMG'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          END IF
        END IF
      END DO
      model_config_rec%auxinput10_begin_d = model_config_rec%gfdda_begin_d
      model_config_rec%auxinput10_begin_h = model_config_rec%gfdda_begin_h
      model_config_rec%auxinput10_begin_m = model_config_rec%gfdda_begin_m
      model_config_rec%auxinput10_begin_s = model_config_rec%gfdda_begin_s
      model_config_rec%auxinput10_begin_y = model_config_rec%gfdda_begin_y
      model_config_rec%auxinput10_end_d = model_config_rec%gfdda_end_d
      model_config_rec%auxinput10_end_h = model_config_rec%gfdda_end_h
      model_config_rec%auxinput10_end_m = model_config_rec%gfdda_end_m
      model_config_rec%auxinput10_end_s = model_config_rec%gfdda_end_s
      model_config_rec%auxinput10_end_y = model_config_rec%gfdda_end_y
      model_config_rec%auxinput10_inname = model_config_rec%gfdda_inname
      model_config_rec%auxinput10_interval = model_config_rec%gfdda_interval
      model_config_rec%auxinput10_interval_d = model_config_rec%gfdda_interval_d
      model_config_rec%auxinput10_interval_h = model_config_rec%gfdda_interval_h
      model_config_rec%auxinput10_interval_m = model_config_rec%gfdda_interval_m
      model_config_rec%auxinput10_interval_s = model_config_rec%gfdda_interval_s
      model_config_rec%auxinput10_interval_y = model_config_rec%gfdda_interval_y
      model_config_rec%io_form_auxinput10 = model_config_rec%io_form_gfdda
      model_config_rec%auxinput9_begin_d = model_config_rec%sgfdda_begin_d
      model_config_rec%auxinput9_begin_h = model_config_rec%sgfdda_begin_h
      model_config_rec%auxinput9_begin_m = model_config_rec%sgfdda_begin_m
      model_config_rec%auxinput9_begin_s = model_config_rec%sgfdda_begin_s
      model_config_rec%auxinput9_begin_y = model_config_rec%sgfdda_begin_y
      model_config_rec%auxinput9_end_d = model_config_rec%sgfdda_end_d
      model_config_rec%auxinput9_end_h = model_config_rec%sgfdda_end_h
      model_config_rec%auxinput9_end_m = model_config_rec%sgfdda_end_m
      model_config_rec%auxinput9_end_s = model_config_rec%sgfdda_end_s
      model_config_rec%auxinput9_end_y = model_config_rec%sgfdda_end_y
      model_config_rec%auxinput9_inname = model_config_rec%sgfdda_inname
      model_config_rec%auxinput9_interval = model_config_rec%sgfdda_interval
      model_config_rec%auxinput9_interval_d = model_config_rec%sgfdda_interval_d
      model_config_rec%auxinput9_interval_h = model_config_rec%sgfdda_interval_h
      model_config_rec%auxinput9_interval_m = model_config_rec%sgfdda_interval_m
      model_config_rec%auxinput9_interval_s = model_config_rec%sgfdda_interval_s
      model_config_rec%auxinput9_interval_y = model_config_rec%sgfdda_interval_y
      model_config_rec%io_form_auxinput9 = model_config_rec%io_form_sgfdda
      IF (model_config_rec%prec_acc_dt(1) .gt. 0.) model_config_rec%prec_acc_opt = 1
      IF (model_config_rec%bucket_mm .gt. 0.) model_config_rec%bucketr_opt = 1
      IF ( model_config_rec % use_wps_input .EQ. 1 ) THEN
         IF ( ( .NOT. model_config_rec % use_surface ) .AND. &
              ( model_config_rec % force_sfc_in_vinterp .GT. 0 ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting force_sfc_in_vinterp=0.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec % force_sfc_in_vinterp = 0
         END IF
         IF ( ( .NOT. model_config_rec % use_surface ) .AND. &
              ( model_config_rec % lowest_lev_from_sfc ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting lowest_lev_from_sfc=F.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec % lowest_lev_from_sfc = .FALSE.
         END IF
      END IF
      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME ) .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME ) .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST ) .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST ) ) THEN
         wrf_err_message = '--- NOTE: RRTMG radiation is used, namelist ' // &
                           'value for o3input (ozone input) is used '
      ELSE
         model_config_rec % o3input = 0
         wrf_err_message = '--- NOTE: RRTMG radiation is not used, setting:  ' // &
                           'o3input=0 to avoid data pre-processing'
         CALL wrf_message ( wrf_err_message )
      END IF
      IF ( fatal_error ) THEN
      WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  ', count_fatal_error, &
                                            ' namelist settings are wrong. Please check and reset these options'
      CALL wrf_error_fatal3("<stdin>",1347,&
wrf_err_message )
      END IF
   END SUBROUTINE
   SUBROUTINE setup_physics_suite
      USE module_domain, ONLY : change_to_lower_case
      IMPLICIT NONE
      INTEGER :: i
      INTEGER :: max_dom
      LOGICAL :: have_mods
      INTEGER, DIMENSION( max_domains ) :: orig_mp_physics, orig_cu_physics, orig_ra_lw_physics, orig_ra_sw_physics, &
                                           orig_bl_pbl_physics, orig_sf_sfclay_physics, orig_sf_surface_physics
      CHARACTER, DIMENSION( max_domains ) :: modified_mp_option, modified_cu_option, modified_ra_lw_option, modified_ra_sw_option, &
                                             modified_bl_pbl_option, modified_sf_sfclay_option, modified_sf_surface_option
      CHARACTER (LEN=256) :: physics_suite_lowercase
      CHARACTER (LEN=32) :: formatstring
      max_dom = model_config_rec % max_dom
      modified_mp_option(1:max_dom) = ' '
      orig_mp_physics(1:max_dom) = model_config_rec % mp_physics(1:max_dom)
      modified_cu_option(1:max_dom) = ' '
      orig_cu_physics(1:max_dom) = model_config_rec % cu_physics(1:max_dom)
      modified_ra_lw_option(1:max_dom) = ' '
      orig_ra_lw_physics(1:max_dom) = model_config_rec % ra_lw_physics(1:max_dom)
      modified_ra_sw_option(1:max_dom) = ' '
      orig_ra_sw_physics(1:max_dom) = model_config_rec % ra_sw_physics(1:max_dom)
      modified_bl_pbl_option(1:max_dom) = ' '
      orig_bl_pbl_physics(1:max_dom) = model_config_rec % bl_pbl_physics(1:max_dom)
      modified_sf_sfclay_option(1:max_dom) = ' '
      orig_sf_sfclay_physics(1:max_dom) = model_config_rec % sf_sfclay_physics(1:max_dom)
      modified_sf_surface_option(1:max_dom) = ' '
      orig_sf_surface_physics(1:max_dom) = model_config_rec % sf_surface_physics(1:max_dom)
      CALL change_to_lower_case(trim(model_config_rec % physics_suite), physics_suite_lowercase)
      IF ( trim(physics_suite_lowercase) == 'none' ) THEN
         CALL wrf_message ('*************************************')
         CALL wrf_message ('No physics suite selected.')
         CALL wrf_message ('Physics options will be used directly from the namelist.')
         CALL wrf_message ('*************************************')
         RETURN
      END IF
      CALL wrf_message ('*************************************')
      CALL wrf_message ('Configuring physics suite '''//trim(physics_suite_lowercase)//'''')
      CALL wrf_message ('')
      SELECT CASE ( trim(physics_suite_lowercase) )
      CASE ('conus')
         DO i = 1, max_dom
            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = TIEDTKESCHEME
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = THOMPSON
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = MYJPBLSCHEME
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = MYJSFCSCHEME
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME
         END DO
      CASE ('tropical')
         DO i = 1, max_dom
            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = NTIEDTKESCHEME
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = WSM6SCHEME
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = YSUSCHEME
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = SFCLAYSCHEME
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME
         END DO
      CASE DEFAULT
         CALL wrf_error_fatal3("<stdin>",1435,&
'Unrecognized physics suite' )
      END SELECT
      WRITE (formatstring, '(A,I3,A)') '(A21,', max_dom, '(I6,A1))'
      WHERE (model_config_rec % mp_physics(1:max_dom) == orig_mp_physics(1:max_dom)) modified_mp_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'mp_physics: ', &
                                                    (model_config_rec % mp_physics(i), modified_mp_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % cu_physics(1:max_dom) == orig_cu_physics(1:max_dom)) modified_cu_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'cu_physics: ', &
                                                    (model_config_rec % cu_physics(i), modified_cu_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % ra_lw_physics(1:max_dom) == orig_ra_lw_physics(1:max_dom)) modified_ra_lw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_lw_physics: ', &
                                                    (model_config_rec % ra_lw_physics(i), modified_ra_lw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % ra_sw_physics(1:max_dom) == orig_ra_sw_physics(1:max_dom)) modified_ra_sw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_sw_physics: ', &
                                                    (model_config_rec % ra_sw_physics(i), modified_ra_sw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % bl_pbl_physics(1:max_dom) == orig_bl_pbl_physics(1:max_dom)) modified_bl_pbl_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'bl_pbl_physics: ', &
                                                    (model_config_rec % bl_pbl_physics(i), modified_bl_pbl_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % sf_sfclay_physics(1:max_dom) == orig_sf_sfclay_physics(1:max_dom)) &
            modified_sf_sfclay_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_sfclay_physics: ', (model_config_rec % sf_sfclay_physics(i), modified_sf_sfclay_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      WHERE (model_config_rec % sf_surface_physics(1:max_dom) == orig_sf_surface_physics(1:max_dom)) &
            modified_sf_surface_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_surface_physics: ', (model_config_rec % sf_surface_physics(i), modified_sf_surface_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)
      have_mods = ANY (modified_mp_option(1:max_dom) == '*') &
             .OR. ANY (modified_cu_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_lw_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_sw_option(1:max_dom) == '*') &
             .OR. ANY (modified_bl_pbl_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_sfclay_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_surface_option(1:max_dom) == '*')
      IF (have_mods) THEN
         CALL wrf_message ('')
         CALL wrf_message ('(* = option overrides suite setting)')
      END IF
      CALL wrf_message ('*************************************')
   END SUBROUTINE
   SUBROUTINE set_physics_rconfigs
      IMPLICIT NONE
      INTEGER :: numsoiltemp , nummosaictemp
      INTEGER :: i
      IF ( model_config_rec % sf_surface_mosaic .EQ. 1 ) THEN
      numsoiltemp = model_config_rec % num_soil_layers
      nummosaictemp = model_config_rec % mosaic_cat
         model_config_rec % mosaic_cat_soil = numsoiltemp * nummosaictemp
         wrf_err_message = '--- NOTE: Noah-mosaic is in use, setting:  ' // &
                           'mosaic_cat_soil = mosaic_cat * num_soil_layers'
         CALL wrf_message ( wrf_err_message )
      END IF
       CALL RANDOM_SEED ( SIZE = model_config_rec % seed_dim )
      model_config_rec % fft_used = 0
      IF ( ( model_config_rec % polar(1) ) .AND. &
           ( model_config_rec % fft_filter_lat .LT. 90. ) ) THEN
         model_config_rec % fft_used = 1
      END IF
      IF ( model_config_rec % polar(1) .AND. &
            model_config_rec % fft_filter_lat .LT. 90. .AND. &
            model_config_rec % traj_opt .NE. 0 ) THEN
         CALL wrf_error_fatal3("<stdin>",1549,&
'--- ERROR: Trajectories not supported on global domain' )
      END IF
      model_config_rec % cam_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % mp_physics(i) .EQ. CAMMGMPSCHEME ) .OR. &
              ( model_config_rec % bl_pbl_physics(i) .EQ. CAMUWPBLSCHEME ) .OR. &
              ( model_config_rec % shcu_physics(i) .EQ. CAMUWSHCUSCHEME ) ) THEN
            model_config_rec % cam_used = 1
         END IF
      ENDDO
      IF (( model_config_rec % ra_lw_physics(1) .EQ. CAMLWSCHEME ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. CAMSWSCHEME )) THEN
         model_config_rec % paerlev = 29
         model_config_rec % levsiz = 59
         model_config_rec % cam_abs_dim1 = 4
         model_config_rec % cam_abs_dim2 = model_config_rec % e_vert(1)
         wrf_err_message = '--- NOTE: CAM radiation is in use, setting:  ' // &
                           'paerlev=29, levsiz=59, cam_abs_dim1=4, cam_abs_dim2=e_vert'
         CALL wrf_message ( wrf_err_message )
      END IF
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % mp_physics(i) .EQ. MILBRANDT2MOM ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOM ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMG ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMCCN ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOM ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOMLFO ) .OR. &
              ( model_config_rec % do_radar_ref .EQ. 1 ) ) THEN
            model_config_rec % compute_radar_ref = 1
         END IF
      ENDDO
      IF (( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )) THEN
         model_config_rec % levsiz = 59
         model_config_rec % alevsiz = 12
         model_config_rec % no_src_types = 6
         wrf_err_message = '--- NOTE: RRTMG radiation is in use, setting:  ' // &
                           'levsiz=59, alevsiz=12, no_src_types=6'
         CALL wrf_message ( wrf_err_message )
      END IF
      IF ( model_config_rec % sf_surface_physics(1) .EQ. 0 ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. SLABSCHEME ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. LSMSCHEME ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. NOAHMPSCHEME ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME .AND. &
           (model_config_rec % num_soil_layers .NE. 6 .AND. model_config_rec % num_soil_layers .NE. 9) ) &
           model_config_rec % num_soil_layers = 6
      IF ( model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME ) &
           model_config_rec % num_soil_layers = 2
      IF ( model_config_rec % sf_surface_physics(1) .EQ. CLMSCHEME ) &
           model_config_rec % num_soil_layers = 10
      IF ( model_config_rec % sf_surface_physics(1) .EQ. 88 ) &
           model_config_rec % num_soil_layers = 4
      WRITE (wrf_err_message, FMT='(A,I6)') '--- NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_message ( wrf_err_message )
   END SUBROUTINE set_physics_rconfigs
   END MODULE module_check_a_mundo
