SUBROUTINE med_nest_move ( parent, nest )
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move
   USE module_driver_constants, ONLY : max_nests
   USE module_utility
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_state_description
   USE module_dm, ONLY : wrf_dm_move_nest,nest_task_offsets,mpi_comm_to_kid,mpi_comm_to_mom, which_kid
   IMPLICIT NONE
   TYPE(domain) , POINTER :: parent, nest, grid
   INTEGER dx, dy, origdy
END SUBROUTINE med_nest_move
LOGICAL FUNCTION time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid, adjust_domain_dims_for_move
   USE module_driver_constants, ONLY : max_moves
   USE module_compute_geop
   USE module_dm, ONLY : wrf_dm_max_real, wrf_dm_move_nest
   USE module_utility
   USE module_streams, ONLY : compute_vortex_center_alarm
   IMPLICIT NONE
   TYPE(domain) , POINTER :: parent, grid
   INTEGER, INTENT(OUT) :: move_cd_x , move_cd_y
   time_for_move2 = .FALSE.
END FUNCTION time_for_move2
LOGICAL FUNCTION time_for_move ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move
   USE module_dm, ONLY : wrf_dm_move_nest
USE module_timing
   USE module_utility
   IMPLICIT NONE
   TYPE(domain) , POINTER :: parent, grid, par, nst
   INTEGER, INTENT(OUT) :: move_cd_x , move_cd_y
   time_for_move = .FALSE.
END FUNCTION time_for_move
LOGICAL FUNCTION should_not_move ( id )
  USE module_state_description
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id
  LOGICAL retval
  INTEGER cu_physics, ra_sw_physics, ra_lw_physics, sf_urban_physics, sf_surface_physics, obs_nudge_opt
  retval = .FALSE.
  CALL nl_get_cu_physics( id , cu_physics )
  IF ( cu_physics .EQ. GDSCHEME ) THEN
    CALL wrf_message('Grell cumulus can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_ra_sw_physics( id , ra_sw_physics )
  IF ( ra_sw_physics .EQ. CAMSWSCHEME ) THEN
    CALL wrf_message('CAM SW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_ra_lw_physics( id , ra_lw_physics )
  IF ( ra_lw_physics .EQ. CAMLWSCHEME ) THEN
    CALL wrf_message('CAM LW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_sf_urban_physics( id , sf_urban_physics )
  IF ( sf_urban_physics .EQ. 1 .OR. sf_urban_physics .EQ. 2 ) THEN
    CALL wrf_message('UCMs Noah LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_sf_surface_physics( id , sf_surface_physics )
  IF ( sf_surface_physics .EQ. PXLSMSCHEME ) THEN
    CALL wrf_message('PX LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_obs_nudge_opt( id , obs_nudge_opt )
  IF ( obs_nudge_opt .EQ. 1 ) THEN
    CALL wrf_message('Observation nudging can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  should_not_move = retval
END FUNCTION
SUBROUTINE reconcile_nest_positions_over_tasks ( grid )
   USE module_driver_constants, ONLY : max_nests, max_domains
   USE module_domain, ONLY : domain, find_grid_by_id
   USE module_utility
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_state_description
   USE module_dm, ONLY : wrf_dm_move_nest, nest_task_offsets,mpi_comm_to_kid,mpi_comm_to_mom, which_kid &
                        ,comm_start, nest_pes_x, nest_pes_y,local_communicator
   IMPLICIT NONE
   TYPE(domain) , POINTER :: grid, result_grid
   INTEGER kid
   INTEGER itask
   INTEGER max_dom, id
   INTEGER buf(max_domains,2)
   CALL nl_get_max_dom( 1 , max_dom )
   IF ( grid%num_nests .GT. 1 ) THEN
    IF ( grid%active_this_task ) THEN
     DO kid = 1, max_nests
       IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          itask = comm_start( grid%nests(kid)%ptr%id ) - comm_start( grid%id )
          buf(:,1) = model_config_rec%i_parent_start
          buf(:,2) = model_config_rec%j_parent_start
          IF ( itask .GE. 0 .AND. itask .LT. nest_pes_x(grid%id)*nest_pes_y(grid%id) ) THEN
            CALL push_communicators_for_domain(grid%id)
            CALL BYTE_BCAST_FROM_ROOT( buf, 2*max_domains*4, itask, local_communicator)
            CALL pop_communicators_for_domain
          ENDIF
          DO id = 1, max_dom
            CALL find_grid_by_id ( id, grid%nests(kid)%ptr, result_grid )
            IF ( ASSOCIATED(result_grid) .AND. .NOT. result_grid%active_this_task ) THEN
              model_config_rec%i_parent_start(id) = buf(id,1)
              model_config_rec%j_parent_start(id) = buf(id,2)
              result_grid%i_parent_start = model_config_rec%i_parent_start(id)
              result_grid%j_parent_start = model_config_rec%j_parent_start(id)
            ENDIF
          ENDDO
       END IF
     END DO
    ENDIF
   ENDIF
END SUBROUTINE reconcile_nest_positions_over_tasks
