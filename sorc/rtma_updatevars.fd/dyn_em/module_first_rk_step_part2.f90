MODULE module_first_rk_step_part2
CONTAINS
  SUBROUTINE first_rk_step_part2 ( grid , config_flags &
                             , stoch_restart_flag &
                             , moist , moist_tend &
                             , chem , chem_tend &
                             , tracer, tracer_tend &
                             , scalar , scalar_tend &
                             , fdda3d, fdda2d &
                             , ru_tendf, rv_tendf &
                             , rw_tendf, t_tendf &
                             , ph_tendf, mu_tendf &
                             , tke_tend &
                             , adapt_step_flag , curr_secs &
                             , psim , psih , wspd , gz1oz0 , chklowq &
                             , cu_act_flag , hol , th_phy &
                             , pi_phy , p_phy , t_phy &
                             , dz8w , p8w , t8w &
                             , nba_mij, n_nba_mij &
                             , nba_rij, n_nba_rij &
                             , ids, ide, jds, jde, kds, kde &
                             , ims, ime, jms, jme, kms, kme &
                             , ips, ipe, jps, jpe, kps, kpe &
                             , imsx,imex,jmsx,jmex,kmsx,kmex &
                             , ipsx,ipex,jpsx,jpex,kpsx,kpex &
                             , imsy,imey,jmsy,jmey,kmsy,kmey &
                             , ipsy,ipey,jpsy,jpey,kpsy,kpey &
                             , k_start , k_end &
                            )
    USE module_state_description
    USE module_model_constants
    USE module_domain, ONLY : domain
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, &
                          wrf_dm_maxval, wrf_err_message, local_communicator_x, local_communicator_y
    USE module_comm_dm, ONLY : halo_em_tke_c_sub,halo_em_tke_d_sub,halo_em_tke_e_sub &
            ,halo_em_phys_pbl_sub,halo_em_phys_shcu_sub &
            ,halo_em_fdda_sub,halo_em_phys_diffusion_sub,halo_em_tke_3_sub &
            ,halo_em_tke_5_sub,halo_obs_nudge_sub,period_bdy_em_a1_sub,period_bdy_em_phy_bc_sub &
            ,period_bdy_em_fdda_bc_sub,period_bdy_em_chem_sub,halo_em_phys_cu_sub,halo_em_helicity_sub
    USE module_driver_constants
    USE module_diffusion_em, ONLY : phy_bc, cal_deform_and_div, compute_diff_metrics, &
                                    vertical_diffusion_2, horizontal_diffusion_2, calculate_km_kh, &
                                    tke_rhs, cal_helicity
    USE module_em, ONLY : calculate_phy_tend
    USE module_fddaobs_driver, ONLY : fddaobs_driver
    USE module_bc, ONLY : set_physical_bc3d, set_physical_bc2d
    USE module_physics_addtendc, ONLY : update_phy_ten
    USE module_sfs_driver
    USE module_stoch, ONLY : update_stoch_ten, perturb_physics_tend,RAND_PERT_UPDATE, read_write_stochrestart
    IMPLICIT NONE
    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey
    LOGICAL ,INTENT(IN) :: adapt_step_flag
    LOGICAL ,INTENT(IN) :: stoch_restart_flag
    REAL, INTENT(IN) :: curr_secs
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT) :: moist
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT) :: moist_tend
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT) :: chem
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT) :: chem_tend
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT) :: tracer
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT) :: tracer_tend
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT) :: scalar
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT) :: scalar_tend
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme,num_fdda3d),INTENT(INOUT) :: fdda3d
    REAL ,DIMENSION(ims:ime,1:1,jms:jme,num_fdda2d),INTENT(INOUT) :: fdda2d
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: psim
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: psih
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wspd
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: gz1oz0
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chklowq
    LOGICAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cu_act_flag
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: hol
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: th_phy
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: pi_phy
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p_phy
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_phy
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: dz8w
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p8w
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t8w
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ru_tendf
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rv_tendf
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rw_tendf
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ph_tendf
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tendf
    REAL ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_tend
    REAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: mu_tendf
    INTEGER , INTENT(IN) :: k_start, k_end
  INTEGER, INTENT( IN ) :: n_nba_mij, n_nba_rij
  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij
  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij
  INTEGER::set_stepstoch
    REAL, DIMENSION( ims:ime, jms:jme ) :: ht_loc
    REAL :: scale_factor
    INTEGER, DIMENSION( ims:ime, jms:jme ) :: shadowmask
    INTEGER :: ij, i, j, k
    INTEGER num_roof_layers
    INTEGER num_wall_layers
    INTEGER num_road_layers
    INTEGER iswater
    INTEGER rk_step
    INTEGER i_start,i_end,j_start,j_end
   rk_step = 1
      set_stepstoch=0
      if (grid%stepstoch>1) set_stepstoch=1
      if (mod(grid%itimestep,grid%stepstoch)==set_stepstoch)then
      IF ((grid%skebs_on==1).and.(grid%id .EQ. 1 )) then
          CALL RAND_PERT_UPDATE(grid,'T', &
                          grid%SPTFORCS,grid%SPTFORCC, &
                          grid%SPT_AMP,grid%ALPH_T, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          k_start, k_end, &
                          imsx,imex,jmsx,jmex,kmsx,kmex, &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                          imsy,imey,jmsy,jmey,kmsy,kmey, &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          config_flags%restart, grid%iseedarr_skebs, &
                          config_flags%seed_dim, &
                          grid%DX,grid%DY,grid%skebs_vertstruc, &
                          grid%rt_tendf_stoch, &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
             call read_write_stochrestart ('stoch_skebs_T',grid,config_flags,grid%lmax_ideal, &
             grid%kmax_ideal,grid%SPTFORCS,grid%SPTFORCC,1)
       endif
           CALL RAND_PERT_UPDATE(grid,'U', &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC, &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_skebs, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%skebs_vertstruc, &
                           grid%ru_tendf_stoch, &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
             call read_write_stochrestart ('stoch_skebs_U',grid,config_flags,grid%lmax_ideal, &
             grid%kmax_ideal,grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,1)
       endif
           CALL RAND_PERT_UPDATE(grid,'V', &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC, &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_skebs, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%skebs_vertstruc, &
                           grid%rv_tendf_stoch, &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       ENDIF
     if ((grid%sppt_on==1).and.(grid%id .EQ. 1 )) then
          CALL RAND_PERT_UPDATE(grid,'T', &
                          grid%SPPTFORCS,grid%SPPTFORCC, &
                          grid%SPPT_AMP,grid%ALPH_SPPT, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          k_start, k_end, &
                          imsx,imex,jmsx,jmex,kmsx,kmex, &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                          imsy,imey,jmsy,jmey,kmsy,kmey, &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          config_flags%restart, grid%iseedarr_sppt, &
                          config_flags%seed_dim, &
                          grid%DX,grid%DY,grid%sppt_vertstruc, &
                          grid%rstoch, &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
             call read_write_stochrestart ('stoch_sppt_xx',grid,config_flags,grid%lmax_ideal, &
             grid%kmax_ideal,grid%SPPTFORCS,grid%SPPTFORCC,1)
       endif
       ENDIF
      if ((grid%rand_perturb_on==1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS,grid%SPFORCC, &
                           grid%SP_AMP,grid%ALPH_RAND, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_rand_pert, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%rand_pert_vertstruc, &
                           grid%RAND_PERT, &
                           grid%stddev_cutoff_rand_pert,grid%gridpt_stddev_rand_pert, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
           call read_write_stochrestart ('stoch_ranpert',grid,config_flags,grid%lmax_ideal, &
           grid%kmax_ideal,grid%SPFORCS,grid%SPFORCC,1)
       endif
      ENDIF
      if ((grid%spp_conv==1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS2,grid%SPFORCC2, &
                           grid%SP_AMP2,grid%ALPH_RAND2, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_conv, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_conv, &
                           grid%pattern_spp_conv, &
                           grid%stddev_cutoff_spp_conv,grid%gridpt_stddev_spp_conv, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
           print*,'writing spp_conv'
           call read_write_stochrestart ('stoch_spp_con',grid,config_flags,grid%lmax_ideal, &
           grid%kmax_ideal,grid%SPFORCS2,grid%SPFORCC2,1)
       endif
      ENDIF
      if ((grid%spp_pbl==1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS3,grid%SPFORCC3, &
                           grid%SP_AMP3,grid%ALPH_RAND3, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal,grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_pbl, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_pbl, &
                           grid%pattern_spp_pbl, &
                           grid%stddev_cutoff_spp_pbl,grid%gridpt_stddev_spp_pbl, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
           print*,'writing spp_pbl'
           call read_write_stochrestart ('stoch_spp_pbl',grid,config_flags,grid%lmax_ideal, &
           grid%kmax_ideal,grid%SPFORCS3,grid%SPFORCC3,1)
       endif
      ENDIF
      if ((grid%spp_mp .GE. 1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS5,grid%SPFORCC5, &
                           grid%SP_AMP5,grid%ALPH_RAND5, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal,grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_mp,&
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_mp, &
                           grid%pattern_spp_mp, &
                           grid%stddev_cutoff_spp_mp,grid%gridpt_stddev_spp_mp,&
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
           print*,'writing spp_mp'
           call read_write_stochrestart ('stoch_spp_mph',grid,config_flags,grid%lmax_ideal, &
           grid%kmax_ideal,grid%SPFORCS5,grid%SPFORCC5,1)
       endif
      ENDIF
      if ((grid%spp_lsm==1).and.(grid%id .EQ. 1)) then
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS4,grid%SPFORCC4, &
                           grid%SP_AMP4,grid%ALPH_RAND4, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           k_start, k_end, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal,grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_lsm, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_lsm, &
                           grid%pattern_spp_lsm, &
                           grid%stddev_cutoff_spp_lsm,grid%gridpt_stddev_spp_lsm, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
       if ((stoch_restart_flag).and.(.not. config_flags%restart)) then
           print*,'writing spp_lsm'
           call read_write_stochrestart ('stoch_spp_lsm',grid,config_flags,grid%lmax_ideal, &
           grid%kmax_ideal,grid%SPFORCS4,grid%SPFORCC4,1)
       endif
      ENDIF
     ENDIF
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles
        CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
        CALL calculate_phy_tend (config_flags,grid%c1h,grid%c2h, &
                     grid%mut,grid%muu,grid%muv,pi_phy, &
                     grid%rthraten, &
                     grid%rublten,grid%rvblten,grid%rthblten, &
                     grid%rqvblten,grid%rqcblten,grid%rqiblten, &
                     grid%rucuten,grid%rvcuten,grid%rthcuten, &
                     grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                     grid%rqicuten,grid%rqscuten, &
                     grid%rushten,grid%rvshten,grid%rthshten, &
                     grid%rqvshten,grid%rqcshten,grid%rqrshten, &
                     grid%rqishten,grid%rqsshten,grid%rqgshten, &
                     grid%RUNDGDTEN,grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN, &
                     grid%RMUNDGDTEN, &
                     scalar, scalar_tend, num_scalar, &
                     tracer, tracer_tend, num_tracer, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1), &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1), &
                     k_start , min(k_end,kde-1) )
      ENDDO
      !$OMP END PARALLEL DO
       IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
           CALL compute_diff_metrics ( config_flags, grid%ph_2, grid%phb, grid%z, grid%rdz, grid%rdzw, &
                                       grid%zx, grid%zy, grid%rdx, grid%rdy, &
                                       ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       grid%i_start(ij), grid%i_end(ij), &
                                       grid%j_start(ij), grid%j_end(ij), &
                                       k_start , k_end )
         ENDDO
         !$OMP END PARALLEL DO
CALL HALO_EM_TKE_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL PERIOD_BDY_EM_A1_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
           CALL set_physical_bc3d( grid%rdzw , 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start , k_end )
           CALL set_physical_bc3d( grid%rdz , 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start , k_end )
           CALL set_physical_bc3d( grid%z , 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start , k_end )
           CALL set_physical_bc3d( grid%zx , 'e', config_flags, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start , k_end )
           CALL set_physical_bc3d( grid%zy , 'f', config_flags, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start , k_end )
           CALL set_physical_bc2d( grid%ustm, 't', config_flags, &
                                   ids, ide, jds, jde, &
                                   ims, ime, jms, jme, &
                                   ips, ipe, jps, jpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij) )
          CALL set_physical_bc2d( grid%ust, 't', config_flags, &
                                   ids, ide, jds, jde, &
                                   ims, ime, jms, jme, &
                                   ips, ipe, jps, jpe, &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij) )
         ENDDO
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
           CALL cal_deform_and_div ( config_flags,grid%u_2,grid%v_2,grid%w_2,grid%div, &
                                     grid%defor11,grid%defor22,grid%defor33, &
                                     grid%defor12,grid%defor13,grid%defor23, &
                                     nba_rij, n_nba_rij, &
                                     grid%u_base, grid%v_base,grid%msfux,grid%msfuy, &
                                     grid%msfvx,grid%msfvy,grid%msftx,grid%msfty, &
                                     grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz, &
                                     grid%rdzw,grid%fnm,grid%fnp,grid%cf1,grid%cf2, &
                                     grid%cf3,grid%zx,grid%zy, &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     grid%i_start(ij), grid%i_end(ij), &
                                     grid%j_start(ij), grid%j_end(ij), &
                                     k_start , k_end )
         ENDDO
         !$OMP END PARALLEL DO
CALL HALO_EM_HELICITY_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       IF ( ( config_flags%nwp_diagnostics .eq. 1 ) .OR. (config_flags%gsd_diagnostics .eq. 1 ) .OR. &
            ( ( config_flags%afwa_diag_opt .eq. 1 ) .AND. ( config_flags%afwa_severe_opt .EQ. 1 ) ) ) THEN
       !$OMP PARALLEL DO &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
          CALL wrf_debug ( 200 , ' call cal_helicity' )
          CALL cal_helicity ( config_flags,grid%itimestep,grid%dt,grid%u_2,grid%v_2,grid%w_2, &
                              grid%uh, &
                              grid%uh16, &
                              grid%uh02, &
                              grid%uh03, &
                              grid%up_heli_max, &
                              grid%up_heli_max16, &
                              grid%up_heli_max03, &
                              grid%up_heli_max02, &
                              grid%up_heli_min, &
                              grid%up_heli_min16, &
                              grid%up_heli_min03, &
                              grid%up_heli_min02, &
                              grid%rel_vort, &
                              grid%rel_vort_max, &
                              grid%rel_vort_max01, &
                              grid%z,grid%rlv,grid%rlvn, &
                              grid%ph_2,grid%phb, &
                              grid%msfux,grid%msfuy, &
                              grid%msfvx,grid%msfvy, &
                              grid%ht, &
                              grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz, grid%rdzw, &
                              grid%fnm,grid%fnp,grid%cf1,grid%cf2,grid%cf3,grid%zx,grid%zy, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start , k_end )
       ENDDO
       !$OMP END PARALLEL DO
       ENDIF
CALL HALO_EM_TKE_D_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call calculate_km_kh' )
           CALL calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp, &
                                 config_flags%damp_opt, &
                                 grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv,grid%bn2, &
                                 grid%khdif,grid%kvdif,grid%div, &
                                 grid%defor11,grid%defor22,grid%defor33,grid%defor12, &
                                 grid%defor13,grid%defor23, &
                                 grid%tke_2,p8w,t8w,th_phy, &
                                 t_phy,p_phy,moist,grid%dn,grid%dnw, &
                                 grid%dx,grid%dy,grid%rdz,grid%rdzw, &
                                 config_flags%mix_isotropic,num_moist, &
                                 grid%cf1, grid%cf2, grid%cf3, grid%warm_rain, &
                                 grid%mix_upper_bound, &
                                 grid%msftx, grid%msfty, &
                                 grid%zx, grid%zy, &
                                 grid%spp_pbl, grid%pattern_spp_pbl, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start , k_end )
         ENDDO
       !$OMP END PARALLEL DO
CALL HALO_EM_TKE_E_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
CALL PERIOD_BDY_EM_PHY_BC_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       IF ( config_flags%grid_fdda .eq. 1) THEN
CALL PERIOD_BDY_EM_FDDA_BC_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
CALL PERIOD_BDY_EM_CHEM_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       !$OMP PARALLEL DO &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call phy_bc' )
         CALL phy_bc (config_flags,grid%div,grid%defor11,grid%defor22,grid%defor33, &
                      grid%defor12,grid%defor13,grid%defor23, &
                      grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv, &
                      grid%tke_2,grid%rho, &
                      grid%rublten, grid%rvblten, &
                      grid%rucuten, grid%rvcuten, &
                      grid%rushten, grid%rvshten, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      ips, ipe, jps, jpe, kps, kpe, &
                      grid%i_start(ij), grid%i_end(ij), &
                      grid%j_start(ij), grid%j_end(ij), &
                      k_start , k_end )
       ENDDO
       !$OMP END PARALLEL DO
IF ( ( config_flags%sfs_opt .GT. 0 ) .AND. ( config_flags%diff_opt .eq. 2 ) ) THEN
 CALL sfs_driver( grid, config_flags, &
                  nba_mij, n_nba_mij, &
                  nba_rij, n_nba_rij )
ENDIF
       IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
CALL HALO_EM_PHYS_PBL_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
       IF ( config_flags%shcu_physics .gt. 1 ) THEN
CALL HALO_EM_PHYS_SHCU_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
       IF ( config_flags%cu_physics == SASSCHEME .or. &
            config_flags%cu_physics == TIEDTKESCHEME .or. &
            config_flags%cu_physics == NTIEDTKESCHEME .or. &
            config_flags%cu_physics == CAMZMSCHEME .or. &
            config_flags%cu_physics == SCALESASSCHEME .or. &
            config_flags%cu_physics == GFSCHEME .or. &
            config_flags%cu_physics == NSASSCHEME ) THEN
CALL HALO_EM_PHYS_CU_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
       IF ( config_flags%grid_fdda .ge. 1) THEN
CALL HALO_EM_FDDA_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
       IF ( config_flags%diff_opt .ge. 1 ) THEN
CALL HALO_EM_PHYS_DIFFUSION_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ENDIF
       IF ( config_flags%h_mom_adv_order <= 4 ) THEN
CALL HALO_EM_TKE_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
CALL HALO_EM_TKE_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal3("<stdin>",852,&
TRIM(wrf_err_message))
       ENDIF
       !$OMP PARALLEL DO &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call update_phy_ten' )
         if (config_flags%cu_physics .gt. 0) then
           i_start = grid%i_start(ij)
           i_end = min( grid%i_end(ij),ide-1 )
           j_start = grid%j_start(ij)
           j_end = min( grid%j_end(ij),jde-1 )
           if (grid%dfi_stage == DFI_FWD ) &
                 CALL wrf_debug ( 200 , ' Zero out cu_physics' )
           DO j = j_start, j_end
           DO k = k_start, min( k_end,kde-1 ) - 1
           DO i = i_start, i_end
             if (grid%dfi_stage ==DFI_FWD &
             .and. grid%dfi_tten_rad(i,k,j) >= 1.0e-7 .and. &
                   grid%dfi_tten_rad(i,k,j) <= 10.) then
                grid%rthcuten(i,k,j) = 0.0
             endif
           ENDDO
           ENDDO
           ENDDO
         ENDIF
         CALL update_phy_ten(ph_tendf,t_tendf, ru_tendf, rv_tendf,moist_tend ,&
                           scalar_tend, mu_tendf, &
                           grid%rthraten,grid%rthblten,grid%rthcuten,grid%rthshten, &
                           grid%rublten,grid%rucuten,grid%rushten, &
                           grid%rvblten,grid%rvcuten,grid%rvshten, &
                           grid%rqvblten,grid%rqcblten,grid%rqiblten, &
                           grid%rqniblten, &
                           grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                           grid%rqicuten,grid%rqscuten, &
                           grid%rqcncuten,grid%rqincuten, &
                           grid%rqvshten,grid%rqcshten,grid%rqrshten, &
                           grid%rqishten,grid%rqsshten,grid%rqgshten, &
                           grid%rqcnshten,grid%rqinshten, &
                           grid%RUNDGDTEN, &
                           grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RPHNDGDTEN, &
                           grid%RQVNDGDTEN,grid%RMUNDGDTEN, &
                           grid%rthfrten,grid%rqvfrten, &
                           num_moist,num_scalar,config_flags,rk_step, &
                           grid%adv_moist_cond, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end )
       END DO
       !$OMP END PARALLEL DO
      IF (grid%skebs_on==1) then
          !$OMP PARALLEL DO &
          !$OMP PRIVATE ( ij )
          DO ij = 1 , grid%num_tiles
               CALL wrf_debug ( 200 , ' call update_stoch_ten' )
                CALL update_stoch_ten(ru_tendf, rv_tendf, t_tendf,&
                               grid%ru_tendf_stoch, &
                               grid%rv_tendf_stoch, &
                               grid%rt_tendf_stoch, &
                               grid%mu_2, grid%mub, grid%c1h, grid%c2h, &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start, k_end, &
                               grid%num_stoch_levels,grid%num_stoch_levels )
           ENDDO
           !$OMP END PARALLEL DO
      ENDIF
      IF (grid%sppt_on==1) then
          !$OMP PARALLEL DO &
          !$OMP PRIVATE ( ij )
          DO ij = 1 , grid%num_tiles
                 call perturb_physics_tend(grid%gridpt_stddev_sppt, &
                        grid%stddev_cutoff_sppt,grid%rstoch, &
                        ru_tendf,rv_tendf,t_tendf,moist_tend(ims,kms,jms,2), &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end, &
                        grid%num_stoch_levels,grid%num_stoch_levels )
           ENDDO
          !$OMP END PARALLEL DO
  ENDIF
       IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL tke_rhs ( tke_tend,grid%bn2, &
                         config_flags,grid%defor11,grid%defor22, &
                         grid%defor33, &
                         grid%defor12,grid%defor13,grid%defor23, &
                         grid%u_2,grid%v_2,grid%w_2,grid%div, &
                         grid%tke_2,grid%mut,grid%c1h,grid%c2h, &
                         th_phy,p_phy,p8w,t8w,grid%z,grid%fnm, &
                         grid%fnp,grid%cf1,grid%cf2,grid%cf3, &
                         grid%msftx,grid%msfty,grid%xkmh, &
                         grid%xkmv,grid%xkhv,grid%rdx,grid%rdy, &
                         grid%dx,grid%dy,grid%dt,grid%zx,grid%zy, &
                         grid%rdz,grid%rdzw,grid%dn, &
                         grid%dnw,config_flags%mix_isotropic, &
                         grid%hfx, grid%qfx, moist(ims,kms,jms,P_QV), &
                         grid%ustm, grid%rho, &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         grid%i_start(ij), grid%i_end(ij), &
                         grid%j_start(ij), grid%j_end(ij), &
                         k_start , k_end )
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF
       IF(config_flags%diff_opt .eq. 2) THEN
         IF (config_flags%bl_pbl_physics .eq. 0) THEN
           !$OMP PARALLEL DO &
           !$OMP PRIVATE ( ij )
           DO ij = 1 , grid%num_tiles
             CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
             CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf, &
                                      t_tendf, tke_tend, &
                                      moist_tend, num_moist, &
                                      chem_tend, num_chem, &
                                      scalar_tend, num_scalar, &
                                      tracer_tend, num_tracer, &
                                      grid%u_2, grid%v_2, &
                                      grid%t_2,grid%u_base,grid%v_base,grid%t_base,grid%qv_base, &
                                      grid%tke_2, config_flags, &
                                      grid%defor13,grid%defor23,grid%defor33, &
                                      nba_mij, num_nba_mij, &
                                      grid%div, moist, chem, scalar,tracer, &
                                      grid%xkmv, grid%xkhv, grid%xkmh, config_flags%km_opt, &
                                      grid%fnm, grid%fnp, grid%dn, grid%dnw, grid%rdz, grid%rdzw, &
                                      grid%hfx, grid%qfx, grid%ustm, grid%rho, &
                                      ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      grid%i_start(ij), grid%i_end(ij), &
                                      grid%j_start(ij), grid%j_end(ij), &
                                      k_start, k_end )
           ENDDO
           !$OMP END PARALLEL DO
         ENDIF
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
           CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend, &
                                      moist_tend, num_moist, &
                                      chem_tend, num_chem, &
                                      scalar_tend, num_scalar, &
                                      tracer_tend, num_tracer, &
                                      grid%t_2, th_phy, &
                                      grid%tke_2, config_flags, &
                                      grid%defor11, grid%defor22, grid%defor12, &
                                      grid%defor13, grid%defor23, &
                                      nba_mij, num_nba_mij, &
                                      grid%div, &
                                      moist, chem, scalar,tracer, &
                                      grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx, &
                                      grid%msfty, grid%xkmh, grid%xkhh, config_flags%km_opt, &
                                      grid%rdx, grid%rdy, grid%rdz, grid%rdzw, &
                                      grid%fnm, grid%fnp, grid%cf1, grid%cf2, grid%cf3, &
                                      grid%zx, grid%zy, grid%dn, grid%dnw, grid%rho, &
                                      ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      grid%i_start(ij), grid%i_end(ij), &
                                      grid%j_start(ij), grid%j_end(ij), &
                                      k_start , k_end )
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF
       IF ( grid%obs_nudge_opt .EQ. 1 .AND. grid%xtime <= grid%fdda_end ) THEN
CALL HALO_OBS_NUDGE_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL fddaobs_driver (grid%grid_id, model_config_rec%grid_id, &
                   model_config_rec%parent_id, config_flags%restart, &
                   config_flags, &
                   grid%obs_nudge_opt, &
                   grid%obs_ipf_errob, &
                   grid%obs_ipf_nudob, &
                   grid%fdda_start, &
                   grid%fdda_end, &
                   grid%obs_nudge_wind, &
                   grid%obs_nudge_temp, &
                   grid%obs_nudge_mois, &
                   grid%obs_nudge_pstr, &
                   grid%obs_coef_wind, &
                   grid%obs_coef_temp, &
                   grid%obs_coef_mois, &
                   grid%obs_coef_pstr, &
                   grid%obs_rinxy, &
                   grid%obs_rinsig, &
                   grid%obs_npfi, &
                   grid%obs_ionf, &
                   grid%obs_prt_max, &
                   grid%obs_prt_freq, &
                   grid%obs_idynin, &
                   grid%obs_dtramp, &
                   grid%parent_grid_ratio, &
                   grid%max_dom, grid%itimestep, &
                   grid%xtime, &
                   grid%dt, grid%gmt, grid%julday, grid%fdob, &
                   grid%max_obs, &
                   model_config_rec%nobs_ndg_vars, &
                   model_config_rec%nobs_err_flds, &
                   grid%fdob%nstat, grid%fdob%varobs, grid%fdob%errf, &
                   grid%dx, grid%KPBL,grid%HT, &
                   grid%mut, grid%muu, grid%muv, grid%c1h, grid%c2h, &
                   grid%msftx, grid%msfty, grid%msfux, grid%msfuy, grid%msfvx, grid%msfvy, &
                   p_phy, t_tendf, t0, &
                   grid%u_2, grid%v_2, grid%t_2, &
                   moist(ims,kms,jms,P_QV), &
                   grid%pb, grid%p_top, grid%p, grid%phb, grid%ph_2, &
                   grid%uratx, grid%vratx, grid%tratx, &
                   ru_tendf, rv_tendf, &
                   moist_tend(ims,kms,jms,P_QV), grid%obs_savwt, &
                   grid%regime, grid%pblh, grid%z_at_w, grid%z, &
                   ids,ide, jds,jde, kds,kde, &
                   ims,ime, jms,jme, kms,kme, &
                   grid%i_start(ij), min(grid%i_end(ij),ide-1), &
                   grid%j_start(ij), min(grid%j_end(ij),jde-1), &
                   k_start , min(k_end,kde-1) )
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF
  END SUBROUTINE first_rk_step_part2
END MODULE module_first_rk_step_part2
