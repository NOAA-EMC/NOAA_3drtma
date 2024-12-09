MODULE module_after_all_rk_steps
CONTAINS
   SUBROUTINE after_all_rk_steps ( grid, config_flags, &
                                   moist, chem, tracer, scalar, &
                                   th_phy, pi_phy, p_phy, rho_phy, &
                                   p8w, t8w, dz8w, &
                                   curr_secs2, &
                                   diag_flag, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                                   ipsy, ipey, jpsy, jpey, kpsy, kpey )
      USE module_state_description, ONLY: num_moist, num_chem, num_tracer, num_scalar
      USE module_domain, ONLY : domain
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y &
                 ,local_communicator_periodic, wrf_dm_maxval
      USE module_comm_dm, ONLY : &
                  halo_em_phys_w_sub, halo_em_phys_hcw_sub
      USE module_diagnostics_driver, ONLY : diagnostics_driver
      IMPLICIT NONE
      TYPE ( domain ), INTENT(INOUT) :: grid
      TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_moist ) , INTENT(IN) :: moist
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_chem ) , INTENT(IN) :: chem
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer) , INTENT(IN) :: tracer
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar) , INTENT(IN) :: scalar
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: th_phy , &
                                                                           p_phy , &
                                                                           pi_phy , &
                                                                           rho_phy , &
                                                                           dz8w , &
                                                                           p8w , &
                                                                           t8w
      REAL :: curr_secs2
      LOGICAL :: diag_flag
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe
      INTEGER , INTENT(IN) :: imsx,imex,jmsx,jmex,kmsx,kmex, &
                              ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                              imsy,imey,jmsy,jmey,kmsy,kmey, &
                              ipsy,ipey,jpsy,jpey,kpsy,kpey
CALL HALO_EM_PHYS_W_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_EM_PHYS_HCW_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
      CALL wrf_debug ( 100 , '--> TOP OF AFTER ALL RK STEPS' )
      CALL wrf_debug ( 100 , '--> CALLING DIAGNOSTICS DRIVER' )
      CALL diagnostics_driver ( grid, config_flags, &
                                moist, chem, tracer, scalar, &
                                th_phy, pi_phy, p_phy, rho_phy, &
                                p8w, t8w, dz8w, &
                                curr_secs2, &
                                diag_flag, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                                imsy, imey, jmsy, jmey, kmsy, kmey, &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey )
   END SUBROUTINE after_all_rk_steps
END MODULE module_after_all_rk_steps
