module module_stoch
      implicit none
      public :: SETUP_RAND_PERTURB, UPDATE_STOCH,&
                         rand_pert_update
      INTEGER :: LMINFORC, LMAXFORC, KMINFORC, KMAXFORC, &
      & LMINFORCT, LMAXFORCT, KMINFORCT, KMAXFORCT
      REAL :: ALPH, ALPH_PSI, ALPH_T, TOT_BACKSCAT_PSI, TOT_BACKSCAT_T, REXPONENT_PSI,REXPONENT_T
      INTEGER :: LENSAV
      INTEGER,ALLOCATABLE:: wavenumber_k(:), wavenumber_l(:)
      REAL, ALLOCATABLE :: WSAVE1(:),WSAVE2(:), WSAVE1_ideal(:),WSAVE2_ideal(:)
      REAL, ALLOCATABLE :: rindarrayik(:),rindarrayil(:),rindarrayl(:),rindarrayk(:)
      REAL, PARAMETER:: RPI= 3.141592653589793
      REAL, PARAMETER:: CP= 1006.0
      REAL, PARAMETER:: T0= 300.0
      character(len=33) :: filenamesave
      save
contains
      SUBROUTINE INITIALIZE_STOCH (grid, config_flags, &
                          first_trip_for_this_domain, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte, &
                          imsx, imex, jmsx, jmex, kmsx, kmex, &
                          ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                          imsy, imey, jmsy, jmey, kmsy, kmey, &
                          ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_configure
    USE module_domain, ONLY : domain
      IMPLICIT NONE
      TYPE (grid_config_rec_type) :: config_flags
      TYPE ( domain ), INTENT(INOUT) :: grid
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                                ims, ime, jms, jme, kms, kme, &
                                                ips, ipe, jps, jpe, kps, kpe, &
                                                its, ite, jts, jte, kts, kte
      INTEGER , INTENT(IN) :: imsx,imex,jmsx,jmex,kmsx,kmex, &
                                                ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                                                imsy,imey,jmsy,jmey,kmsy,kmey, &
                                                ipsy,ipey,jpsy,jpey,kpsy,kpey
      LOGICAL :: first_trip_for_this_domain
      INTEGER, PARAMETER :: maxspinup=100
      INTEGER :: K
   IF ( first_trip_for_this_domain ) THEN
     grid%did_stoch = .FALSE.
   END IF
   IF ((( grid%id == 1) .AND. (.NOT. grid%did_stoch)) .AND. &
       (( grid%skebs_on== 1) .OR.( grid%sppt_on== 1) .OR. ( grid%rand_perturb_on== 1) .OR. &
         ( grid%spp_conv== 1) .OR. ( grid%spp_pbl== 1) .OR. ( grid%spp_mp .GE. 1) .OR. &
         ( grid%spp_lsm== 1)) ) THEN
     grid%did_stoch = .TRUE.
     IF (grid%skebs_on==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%ISEED_SKEBS, grid%iseedarr_skebs , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_skebs_U',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,2)
         call read_write_stochrestart ('stoch_skebs_T',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPTFORCS,grid%SPTFORCC,2)
           CALL wrf_dm_bcast_real ( grid%SPSTREAMFORCS ,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPSTREAMFORCC ,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPTFORCS,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPTFORCC,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('W', &
                       grid%skebs_vertstruc,config_flags%restart, &
                       grid%SPSTREAM_AMP, &
                       grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,grid%ALPH_PSI,&
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_sppt, &
                       grid%lengthscale_sppt, &
                       grid%timescale_sppt, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
          CALL RAND_PERT_UPDATE(grid,'U', &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC, &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts,kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_skebs, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%skebs_vertstruc, &
                           grid%ru_tendf_stoch, &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV )
          CALL RAND_PERT_UPDATE(grid,'V', &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC, &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts,kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           grid% num_stoch_levels,grid% num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_skebs, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%skebs_vertstruc, &
                           grid%rv_tendf_stoch, &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     call SETUP_RAND_PERTURB('T', &
                       grid%skebs_vertstruc,config_flags%restart, &
                       grid%SPT_AMP, &
                       grid%SPTFORCS,grid%SPTFORCC,grid%ALPH_T, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_sppt, &
                       grid%lengthscale_sppt, &
                       grid%timescale_sppt, &
                       grid%TOT_BACKSCAT_T,grid%ZTAU_T, &
                       grid%REXPONENT_T, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                          grid%SPTFORCS,grid%SPTFORCC, &
                          grid%SPT_AMP,grid%ALPH_T, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          kts,kte, &
                          imsx,imex,jmsx,jmex,kmsx,kmex, &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                          imsy,imey,jmsy,jmey,kmsy,kmey, &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal, grid%lmax_ideal, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          config_flags%restart, grid%iseedarr_skebs, &
                          config_flags%seed_dim, &
                          grid%DX,grid%DY,grid%skebs_vertstruc, &
                          grid%rt_tendf_stoch, &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV )
         enddo
       endif
     ENDIF
IF (grid%sppt_on==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%ISEED_SPPT, grid%iseedarr_sppt , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_sppt_xx',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPPTFORCS,grid%SPPTFORCC,2)
           CALL wrf_dm_bcast_real ( grid%SPPTFORCS,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPPTFORCC,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('P', &
                       grid%sppt_vertstruc,config_flags%restart, &
                       grid%SPPT_AMP, &
                       grid%SPPTFORCC,grid%SPPTFORCS,grid%ALPH_SPPT, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_sppt, &
                       grid%lengthscale_sppt, &
                       grid%timescale_sppt, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
          CALL RAND_PERT_UPDATE(grid,'T', &
                          grid%SPPTFORCS,grid%SPPTFORCC, &
                          grid%SPPT_AMP,grid%ALPH_SPPT, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          kts,kte, &
                          imsx,imex,jmsx,jmex,kmsx,kmex, &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                          imsy,imey,jmsy,jmey,kmsy,kmey, &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          grid%kmax_ideal, grid%lmax_ideal, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          grid%num_stoch_levels,grid%num_stoch_levels, &
                          config_flags%restart, grid%iseedarr_sppt, &
                          config_flags%seed_dim, &
                          grid%DX,grid%DY,grid%sppt_vertstruc, &
                          grid%rstoch, &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt, &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     ENDIF
     IF (grid%rand_perturb_on==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%ISEED_RAND_PERT, grid%iseedarr_rand_pert , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_ranpert',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPFORCS,grid%SPFORCC,2)
           CALL wrf_dm_bcast_real ( grid%SPFORCS , grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPFORCC , grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('R', &
                       grid%rand_pert_vertstruc,config_flags%restart, &
                       grid%SP_AMP, &
                       grid%SPFORCC,grid%SPFORCS,grid%ALPH_RAND, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_rand_pert, &
                       grid%lengthscale_rand_pert, &
                       grid%timescale_rand_pert, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS,grid%SPFORCC, &
                           grid%SP_AMP,grid%ALPH_RAND, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts, kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_rand_pert, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%rand_pert_vertstruc, &
                           grid%RAND_PERT, &
                           grid%gridpt_stddev_rand_pert, &
                           grid%lengthscale_rand_pert, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       ENDIF
     ENDIF
     IF (grid%spp_conv==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%iseed_spp_conv, grid%iseedarr_spp_conv , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_spp_con',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPFORCS2,grid%SPFORCC2,2)
           CALL wrf_dm_bcast_real ( grid%SPFORCS2 ,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPFORCC2 ,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('R', &
                       grid%vertstruc_spp_conv,config_flags%restart, &
                       grid%SP_AMP2, &
                       grid%SPFORCC2,grid%SPFORCS2,grid%ALPH_RAND2, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_spp_conv, &
                       grid%lengthscale_spp_conv, &
                       grid%timescale_spp_conv, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS2,grid%SPFORCC2, &
                           grid%SP_AMP2,grid%ALPH_RAND2, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts, kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_conv, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_conv, &
                           grid%pattern_spp_conv, &
                           grid%stddev_cutoff_spp_conv,grid%gridpt_stddev_spp_conv, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     ENDIF
     IF (grid%spp_pbl==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%iseed_spp_pbl, grid%iseedarr_spp_pbl , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_spp_pbl',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPFORCS3,grid%SPFORCC3,2)
           CALL wrf_dm_bcast_real ( grid%SPFORCS3,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPFORCC3,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('R', &
                       grid%vertstruc_spp_pbl,config_flags%restart, &
                       grid%SP_AMP3, &
                       grid%SPFORCC3,grid%SPFORCS3,grid%ALPH_RAND3, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_spp_pbl, &
                       grid%lengthscale_spp_pbl, &
                       grid%timescale_spp_pbl, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS3,grid%SPFORCC3, &
                           grid%SP_AMP3,grid%ALPH_RAND3, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts, kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_pbl, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_pbl, &
                           grid%pattern_spp_pbl, &
                           grid%stddev_cutoff_spp_pbl,grid%gridpt_stddev_spp_pbl, &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     ENDIF
     IF (grid%spp_mp .GE. 1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%iseed_spp_mp, grid%iseedarr_spp_mp, 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_spp_mph',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPFORCS5,grid%SPFORCC5,2)
           CALL wrf_dm_bcast_real ( grid%SPFORCS5,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPFORCC5,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('R', &
                       grid%vertstruc_spp_mp,config_flags%restart, &
                       grid%SP_AMP5, &
                       grid%SPFORCC5,grid%SPFORCS5,grid%ALPH_RAND5, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_spp_mp, &
                       grid%lengthscale_spp_mp, &
                       grid%timescale_spp_mp, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS5,grid%SPFORCC5, &
                           grid%SP_AMP5,grid%ALPH_RAND5, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts, kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_mp, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_mp, &
                           grid%pattern_spp_mp, &
                           grid%stddev_cutoff_spp_mp,grid%gridpt_stddev_spp_mp,&
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     ENDIF
     IF (grid%spp_lsm==1) then
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
         call rand_seed (config_flags, grid%iseed_spp_lsm, grid%iseedarr_spp_lsm , 1, config_flags%seed_dim)
     else
         call read_write_stochrestart ('stoch_spp_lsm',grid,config_flags,grid%lmax_ideal,grid%kmax_ideal, &
                                       grid%SPFORCS4,grid%SPFORCC4,2)
           CALL wrf_dm_bcast_real ( grid%SPFORCS4,grid%kmax_ideal*grid%lmax_ideal )
           CALL wrf_dm_bcast_real ( grid%SPFORCC4,grid%kmax_ideal*grid%lmax_ideal )
     endif
     call SETUP_RAND_PERTURB('R', &
                       grid%vertstruc_spp_lsm,config_flags%restart, &
                       grid%SP_AMP4, &
                       grid%SPFORCC4,grid%SPFORCS4,grid%ALPH_RAND4, &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT, &
                       grid%KMINFORCT,grid%KMAXFORCT, &
                       grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       grid%time_step,grid%DX,grid%DY, &
                       grid%stepstoch, &
                       grid%gridpt_stddev_spp_lsm, &
                       grid%lengthscale_spp_lsm, &
                       grid%timescale_spp_lsm, &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI, &
                       grid%REXPONENT_PSI, &
                       grid%kmax_ideal, grid%lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
     if ((.not.config_flags%restart) .and. (.not.config_flags%hrrr_cycling)) then
        do k = 1,maxspinup
           CALL RAND_PERT_UPDATE(grid,'T', &
                           grid%SPFORCS4,grid%SPFORCC4, &
                           grid%SP_AMP4,grid%ALPH_RAND4, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           kts, kte, &
                           imsx,imex,jmsx,jmex,kmsx,kmex, &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                           imsy,imey,jmsy,jmey,kmsy,kmey, &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                           grid%kmax_ideal, grid%lmax_ideal, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           grid%num_stoch_levels,grid%num_stoch_levels, &
                           config_flags%restart, grid%iseedarr_spp_lsm, &
                           config_flags%seed_dim, &
                           grid%DX,grid%DY,grid%vertstruc_spp_lsm, &
                           grid%pattern_spp_lsm, &
                           grid%stddev_cutoff_spp_lsm,grid%gridpt_stddev_spp_lsm,&
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT )
         enddo
       endif
     ENDIF
     ENDIF
      END SUBROUTINE INITIALIZE_STOCH
      subroutine SETUP_RAND_PERTURB( variable_in,&
                       skebs_vertstruc,restart, &
                       SP_AMP,SPFORCC,SPFORCS,ALPH, &
                       VERTSTRUCC,VERTSTRUCS,VERTAMP, &
                       KMINFORCT,KMAXFORCTH,LMINFORCT,LMAXFORCTH, &
                       KMAXFORCT,LMAXFORCT, &
                       itime_step,DX,DY, &
                       stepstoch, &
                       gridpt_stddev_rand_perturb, l_rand_perturb, &
                       tau_rand_perturb, &
                       TOT_BACKSCAT,ZTAU,REXPONENT, &
                       kmax_ideal, lmax_ideal, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
      IMPLICIT NONE
      LOGICAL :: restart
      REAL, PARAMETER :: RPI= 3.141592653589793
      CHARACTER, INTENT(IN) :: variable_in
      CHARACTER :: variable
      INTEGER , INTENT(IN) :: kmax_ideal, lmax_ideal
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                                   ims, ime, jms, jme, kms, kme, &
                                                   its, ite, jts, jte, kts, kte
      INTEGER :: IER,IK,IL,I,J,itime_step,stepstoch,skebs_vertstruc, &
                                                   KMINFORCT,LMINFORCT,KMAXFORCT,LMAXFORCT,KMAXFORCTH,LMAXFORCTH, &
                                                   KMAX,LMAX,LENSAV,ILEV
      REAL :: DX,DY,RY,RX,ALPH,RHOKLMAX,ZREF,RHOKL,EPS
      REAL, DIMENSION (lmax_ideal,kmax_ideal) :: SPFORCS,SPFORCC,SP_AMP
      REAL, DIMENSION (ims:ime,kms:kme,jms:jme) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (kms:kme) :: VERTAMP
      REAL, DIMENSION (ids:ide,jds:jde) :: ZCHI
      REAL :: gridpt_stddev_rand_perturb,kappat,tau_rand_perturb,l_rand_perturb
      REAL, DIMENSION (ims:ime,jms:jme) :: var_sigma1
      REAL :: z,phi,ZGAMMAN,ZCONSTF0,TOT_BACKSCAT,ZTAU,REXPONENT,ZSIGMA2
      LOGICAL :: is_print = .true.
      variable = variable_in
      KMAX=(jde-jds)+1
      LMAX=(ide-ids)+1
      RY= KMAX*DY
      RX= LMAX*DX
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8
      LENSAV=MAX(LENSAV,4*(KMAX_ideal+LMAX_ideal)+INT(LOG(REAL(KMAX_ideal))) + INT(LOG(REAL(LMAX_ideal))) + 8)
      IF ( ALLOCATED(WSAVE1) ) DEALLOCATE(WSAVE1)
      IF ( ALLOCATED(WSAVE2) ) DEALLOCATE(WSAVE2)
      IF ( ALLOCATED(WSAVE1_ideal) ) DEALLOCATE(WSAVE1_ideal)
      IF ( ALLOCATED(WSAVE2_ideal) ) DEALLOCATE(WSAVE2_ideal)
      IF ( ALLOCATED(rindarrayil) ) DEALLOCATE(rindarrayil)
      IF ( ALLOCATED(rindarrayik) ) DEALLOCATE(rindarrayik)
      IF ( ALLOCATED(rindarrayl) ) DEALLOCATE(rindarrayl)
      IF ( ALLOCATED(rindarrayk) ) DEALLOCATE(rindarrayk)
      IF ( ALLOCATED(WAVENUMBER_K)) DEALLOCATE(WAVENUMBER_K)
      IF ( ALLOCATED(WAVENUMBER_L)) DEALLOCATE(WAVENUMBER_L)
      ALLOCATE(WSAVE1(LENSAV),WSAVE2(LENSAV))
      ALLOCATE(WSAVE1_ideal(LENSAV),WSAVE2_ideal(LENSAV))
      ALLOCATE(rindarrayil(LMAX_ideal),rindarrayik(KMAX_ideal),rindarrayl(LMAX),rindarrayk(KMAX))
      ALLOCATE (wavenumber_k(jds:jde),wavenumber_l(ids:ide))
      SP_AMP=0.0
      call CFFT1I (LMAX, WSAVE1, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      call CFFT1I (KMAX, WSAVE2, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      call CFFT1I (LMAX_ideal, WSAVE1_ideal, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      call CFFT1I (KMAX_ideal, WSAVE2_ideal, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      do i=1,LMAX_ideal
        rindarrayil(i)=i
      enddo
      do i=1,KMAX_ideal
        rindarrayik(i)=i
      enddo
      do i=1,LMAX
        rindarrayl(i) = 1. + real(LMAX_ideal-1)/(LMAX-1)*(i-1)
      enddo
      do i=1,KMAX
        rindarrayk(i) = 1. + real(KMAX_ideal-1)/(KMAX-1)*(i-1)
      enddo
      95 format('error in cFFT2I=  ',i5)
     call findindex( wavenumber_k, wavenumber_l, &
                      KMAX_ideal, LMAX_ideal)
     KMAXFORCT=min0(((ide-ids)+1)/2,((jde-jds)+1 )/2)-5
     LMAXFORCT=KMAXFORCT
     if (KMAXFORCT > KMAXFORCTH) then
        KMAXFORCT=KMAXFORCTH
     endif
     if (LMAXFORCT > LMAXFORCTH) then
        LMAXFORCT=LMAXFORCTH
     endif
      ALPH = float(itime_step)/ZTAU/float(stepstoch)
      ZSIGMA2=1./(12.0*ALPH)
      if (is_print) then
      IF (variable == 'W') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing STREAMFUNCTION forcing pattern of  << '')')
      WRITE(*,'('' >> stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_PSI '',E12.5)') TOT_BACKSCAT
      WRITE(*,'('' Exponent for energy spectra, REXPONENT_PSI ='',E12.5)') REXPONENT
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, LMINFORC ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, LMAXFORC ='',I10)') LMAXFORCT
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, KMINFORC ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, KMAXFORC ='',I10)') KMAXFORCT
      WRITE(*,'('' skebs_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Time step: itime_step='',I10)') itime_step
      WRITE(*,'('' Decorrelation time of noise, ZTAU_PSI ='',E12.5)') ZTAU
      WRITE(*,'('' Variance of noise, ZSIGMA2_EPS  ='',E12.5)') ZSIGMA2
      WRITE(*,'('' Autoregressive parameter 1-ALPH_PSI ='',E12.5)') 1.-ALPH
      WRITE(*,'('' =============================================='')')
      ELSEIF (variable == 'T') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing TEMPERATURE forcing pattern of  << '')')
      WRITE(*,'('' >> stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_T   '',E12.5)') TOT_BACKSCAT
      WRITE(*,'('' Exponent for energy spectra, REXPONENT_T   ='',E12.5)') REXPONENT
      WRITE(*,'('' Minimal wavenumber of tempearature forcing, LMINFORC ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of tempearature forcing, LMAXFORC ='',I10)') LMAXFORCT
      WRITE(*,'('' Minimal wavenumber of tempearature forcing, KMINFORC ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of tempearature forcing, KMAXFORC ='',I10)') KMAXFORCT
      WRITE(*,'('' skebs_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, ZTAU_T ='',E12.5)') ZTAU
      WRITE(*,'('' Variance of noise, ZSIGMA2_ETA  ='',E12.5)') ZSIGMA2
      WRITE(*,'('' Autoregressive parameter 1-ALPH_T ='',E12.5)') 1.-ALPH
      WRITE(*,'('' =============================================='')')
      endif
     IF ((variable == 'P') .or. (variable == 'R')) then
      kappat= L_rand_perturb**2
      phi = exp (-float(itime_step*stepstoch)/tau_rand_perturb)
      alph = 1.-phi
     endif
     if (variable == 'P') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing Stochastically Perturbed Physics Tendency scheme << '')')
      WRITE(*,'('' sppt_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
      endif
     if (variable == 'R') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing random perturbations << '')')
      WRITE(*,'('' rand_pert_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
     endif
     endif
     ZCHI = 0.0
     ZGAMMAN = 0.0
      DO IK=jds-1,jde
      DO IL=ids-1,ide
      if (((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((KMAXFORCT+0.5)/RX)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((KMINFORCT-0.5)/RX))) .or. &
          ((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((LMAXFORCT+0.5)/RY)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((LMINFORCT-0.5)/RY))))then
        if ((IK>0).or.(IL>0)) then
          if (variable == 'W') then
            ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)
            ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT+1)
          else if (variable == 'T') then
            ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)
            ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT)
          else if ((variable == 'P') .or. (variable == 'R')) then
            ZCHI(IL+1,IK+1)=exp( -2*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) )
            ZGAMMAN= ZGAMMAN + exp( -4*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) )
          endif
        endif
      endif
      enddo
      enddo
      ZGAMMAN=4.0*ZGAMMAN
      if (variable == 'W') then
         ZCONSTF0=SQRT(ALPH*TOT_BACKSCAT/(float(itime_step)*ZSIGMA2*ZGAMMAN))/(2*RPI)
      elseif (variable == 'T') then
         ZCONSTF0=SQRT(T0*ALPH*TOT_BACKSCAT)/SQRT(float(itime_step)*cp*ZSIGMA2)
         ZCONSTF0=ZCONSTF0/SQRT(ZGAMMAN)
      elseif ((variable == 'P') .or. (variable == 'R')) then
         ZCONSTF0= gridpt_stddev_rand_perturb*sqrt((1.-phi**2)/(2.*ZGAMMAN))
      endif
      SP_AMP=0.0
      DO IL=1, (LMAX_ideal/2+1)
        DO IK = 1, (KMAX_ideal/2+1)
          SP_AMP(IL,IK)=ZCONSTF0*ZCHI(IL,IK)
        ENDDO
      ENDDO
      DO IL=1, (LMAX_ideal/2+1)
        DO IK = 1, (KMAX_ideal/2+1)
          SP_AMP(LMAX_ideal-IL+1,KMAX_ideal-IK+1)= SP_AMP(IL,IK)
          SP_AMP(IL,KMAX_ideal-IK+1)= SP_AMP(IL,IK)
          SP_AMP(LMAX_ideal-IL+1,IK)= SP_AMP(IL,IK)
        ENDDO
      ENDDO
      VERTAMP=1.0
      IF (skebs_vertstruc==1) then
        VERTSTRUCC=0.0
        VERTSTRUCS=0.0
        RHOKLMAX= sqrt(KMAX**2/DY**2 + LMAX**2/DX**2)
        ZREF=32.0
        DO ILEV=kts,kte
          DO IK=jts,jte
            DO IL=its,ite
            if (IL.le.(LMAX/2)) then
              RHOKL = sqrt((IK+1)**2/DY**2 + (IL+1)**2/DX**2)
              EPS = ((RHOKLMAX - RHOKL)/ RHOKLMAX) * (ILEV/ZREF) * RPI
              VERTSTRUCC(IL,ILEV,IK) = cos ( eps* (IL+1) )
              VERTSTRUCS(IL,ILEV,IK) = sin ( eps* (IL+1) )
             else
              RHOKL = sqrt((IK+1)**2/DY**2 + (LMAX-IL+2)**2/DX**2)
              EPS = ((RHOKLMAX - RHOKL)/ RHOKLMAX) * (ILEV/ZREF) * RPI
              VERTSTRUCC (IL,ILEV,IK) = cos ( eps* (LMAX-IL+2) )
              VERTSTRUCS (IL,ILEV,IK) = - sin ( eps* (LMAX-IL+2) )
            endif
            ENDDO
          ENDDO
        ENDDO
      ENDIF
     END subroutine SETUP_RAND_PERTURB
     subroutine UPDATE_STOCH( &
                      SPFORCS,SPFORCC,SP_AMP,ALPH, &
                      restart,iseedarr,seed_dim, &
                      kmax_ideal, lmax_ideal, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
     IMPLICIT NONE
     REAL, DIMENSION(lmax_ideal,kmax_ideal) ,INTENT(INOUT) :: SPFORCS,SPFORCC,SP_AMP
     INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                               ims, ime, jms, jme, kms, kme, &
                                               its, ite, jts, jte, kts, kte
     INTEGER , INTENT(IN) :: seed_dim
     INTEGER, DIMENSION (seed_dim), INTENT(INOUT) :: iseedarr
     INTEGER , INTENT(IN) :: kmax_ideal, lmax_ideal
     REAL, DIMENSION(LMAX_ideal,KMAX_ideal) :: ZRANDNOSS,ZRANDNOSC
     INTEGER , ALLOCATABLE , DIMENSION(:) :: iseed
     REAL :: Z,ALPH
     REAL, PARAMETER :: thresh = 3.0
     INTEGER ::IL, IK,LMAX,KMAX,J
     INTEGER :: how_many
     LOGICAL :: LGAUSS,RESTART
     KMAX=(jde-jds)+1
     LMAX=(ide-ids)+1
     call random_seed(put=iseedarr)
     LGAUSS=.true.
     IF (LGAUSS) then
       DO IK=1,KMAX_ideal
         DO IL=1,LMAX_ideal
          do
           call gauss_noise(z)
           if (abs(z)<thresh) exit
          ENDDO
          ZRANDNOSS(IL,IK)=z
          do
           call gauss_noise(z)
           if (abs(z)<thresh) exit
          ENDDO
          ZRANDNOSC(IL,IK)=z
         ENDDO
       ENDDO
     ELSE
       DO IK=1,KMAX_ideal
         DO IL=1,LMAX_ideal
           CALL RANDOM_NUMBER(z)
           ZRANDNOSS(IL,IK)=z-0.5
           CALL RANDOM_NUMBER(z)
           ZRANDNOSC(IL,IK)=z-0.5
          ENDDO
        ENDDO
      ENDIF
        DO IK=1,KMAX_ideal
        if ((IK.le.(KMAX_ideal/2+1)) .and. (IK>1)) then
          DO IL=1,LMAX_ideal
            SPFORCC(IL,IK) = (1.-ALPH)*SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC(IL,IK)
            SPFORCS(IL,IK) = (1.-ALPH)*SPFORCS(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSS(IL,IK)
          ENDDO
        ELSEIF (IK==1) then
          DO IL=1,LMAX_ideal
          if ((IL.le.(LMAX_ideal/2+1))) then
            SPFORCC(IL,IK) = (1.-ALPH)*SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC(IL,IK)
            SPFORCS(IL,IK) = (1.-ALPH)*SPFORCS(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSS(IL,IK)
          elseif ((IL.gt.(LMAX_ideal/2+1))) then
            SPFORCC(IL,IK) = (1.-ALPH)*SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC(LMAX_ideal-IL+2,IK)
            SPFORCS(IL,IK) = (1.-ALPH)*SPFORCS(IL,IK) - SP_AMP(IL,IK) * ZRANDNOSS(LMAX_ideal-IL+2,IK)
          endif
          ENDDO
        ENDIF
        ENDDO
        DO IK=1,KMAX_ideal
        if (IK.gt.(KMAX_ideal/2+1)) then
          DO IL=1,LMAX_ideal
           if (IL.le.(LMAX_ideal/2+1).and.(IL.gt.1)) then
             SPFORCC(IL,IK) = (1.-ALPH)* SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC(LMAX_ideal-IL+2,KMAX_ideal-IK+2)
             SPFORCS(IL,IK) = (1.-ALPH)* SPFORCS(IL,IK) - SP_AMP(IL,IK) * ZRANDNOSS(LMAX_ideal-IL+2,KMAX_ideal-IK+2)
            elseif (IL.eq.1) then
             SPFORCC(IL,IK) = (1.-ALPH)* SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC( 1,KMAX_ideal-IK+2)
             SPFORCS(IL,IK) = (1.-ALPH)* SPFORCS(IL,IK) - SP_AMP(IL,IK) * ZRANDNOSS( 1,KMAX_ideal-IK+2)
            elseif (IL.gt.(LMAX_ideal/2+1)) then
             SPFORCC(IL,IK) = (1.-ALPH)* SPFORCC(IL,IK) + SP_AMP(IL,IK) * ZRANDNOSC(LMAX_ideal-IL+2,KMAX_ideal-IK+2)
             SPFORCS(IL,IK) = (1.-ALPH)* SPFORCS(IL,IK) - SP_AMP(IL,IK) * ZRANDNOSS(LMAX_ideal-IL+2,KMAX_ideal-IK+2)
            endif
          ENDDO
        endif
        ENDDO
      call random_seed(get=iseedarr)
     END subroutine UPDATE_STOCH
      SUBROUTINE UPDATE_STOCH_TEN(ru_tendf,rv_tendf,t_tendf, &
                       ru_tendf_stoch,rv_tendf_stoch,rt_tendf_stoch,&
                       mu,mub,c1h,c2h, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
                       kte_stoch,kme_stoch )
       IMPLICIT NONE
       INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, &
                                       kte_stoch,kme_stoch
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                       ru_tendf, rv_tendf, t_tendf
       REAL , DIMENSION(ims:ime , kms:kme_stoch, jms:jme) :: &
                      ru_tendf_stoch,rv_tendf_stoch,rt_tendf_stoch
       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
       REAL , DIMENSION(kms:kme) , INTENT(IN) :: c1h,c2h
       INTEGER :: I,J,K,kh
       REAL :: dt,xm
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,ite
             ru_tendf(i,k,j) = ru_tendf(i,k,j) + ru_tendf_stoch(i,kh,j) * ((c1h(k)*mu(i,j))+(c1h(k)*mub(i,j)+c2h(k)))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,jte
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,MIN(ide-1,ite)
             rv_tendf(i,k,j) = rv_tendf(i,k,j) + rv_tendf_stoch(i,kh,j) * ((c1h(k)*mu(i,j))+(c1h(k)*mub(i,j)+c2h(k)))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,MIN(ide-1,ite)
             t_tendf(i,k,j) = t_tendf(i,k,j) + rt_tendf_stoch(i,kh,j) * ((c1h(k)*mu(i,j))+(c1h(k)*mub(i,j)+c2h(k)))
           ENDDO
         ENDDO
       ENDDO
       END SUBROUTINE UPDATE_STOCH_TEN
      subroutine perturb_physics_tend(gridpt_stddev_sppt, &
                       sppt_thresh_fact,rstoch, &
                       ru_tendf,rv_tendf,t_tendf,moist_tend, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
                       kte_stoch,kme_stoch )
       IMPLICIT NONE
       INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, &
                                       kte_stoch,kme_stoch
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                        ru_tendf, rv_tendf, t_tendf,moist_tend
       REAL , DIMENSION(ims:ime,kms:kme_stoch, jms:jme),INTENT(INOUT) :: rstoch
       REAL :: gridpt_stddev_sppt ,thresh,sppt_thresh_fact
       INTEGER :: I,J,K,kh
       thresh=sppt_thresh_fact*gridpt_stddev_sppt
       DO j = jts,jte
         DO k = kts,min(kte-1,kte_stoch-1)
           DO i = its,ite
             if (rstoch(i,k,j).lt.-thresh) then
                 rstoch(i,k,j)=-thresh
             endif
             if (rstoch(i,k,j).gt.thresh) then
                 rstoch(i,k,j)=thresh
             endif
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 )
           DO i = its,ite
              ru_tendf(i,k,j) = ru_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,jte
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 )
            DO i = its,MIN(ide-1,ite)
              rv_tendf(i,k,j) = rv_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 )
           DO i = its,MIN(ide-1,ite)
              moist_tend(i,k,j) = moist_tend(i,k,j)*(1.0 + rstoch(i,kh,j))
              t_tendf (i,k,j) = t_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO
      end subroutine perturb_physics_tend
      SUBROUTINE RAND_PERT_UPDATE (grid, variable_in, &
                          SPFORCS,SPFORCC,SP_AMP,ALPH_RAND, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          kts, kte, &
                          imsx,imex,jmsx,jmex,kmsx,kmex, &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                          imsy,imey,jmsy,jmey,kmsy,kmey, &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                          kmax_ideal, lmax_ideal, &
                          kpe_stoch,kde_stoch,kme_stoch,kte_stoch, &
                          restart,iseedarr,seed_dim, &
                          DX,DY,skebs_vertstruc, &
                          RAND_PERT,thresh_fact,gridpt_stddev, &
                          VERTSTRUCC,VERTSTRUCS,VERTAMP )
    USE module_domain, ONLY : domain
    USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, &
                          wrf_dm_maxval, wrf_err_message, local_communicator_x, local_communicator_y, data_order_xzy
      IMPLICIT NONE
      TYPE ( domain ), INTENT(INOUT) :: grid
      INTEGER , INTENT(IN) :: kmax_ideal, lmax_ideal
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                                ims, ime, jms, jme, kms, kme, &
                                                ips, ipe, jps, jpe, kps, kpe, &
                                                kts, kte
      INTEGER , INTENT(IN) :: imsx,imex,jmsx,jmex,kmsx,kmex, &
                                                ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                                                imsy,imey,jmsy,jmey,kmsy,kmey, &
                                                ipsy,ipey,jpsy,jpey,kpsy,kpey
      INTEGER , INTENT(IN) :: seed_dim
      INTEGER :: kpe_stoch,kde_stoch,kme_stoch,kte_stoch
      REAL , INTENT(IN) :: ALPH_RAND,dx,dy,thresh_fact,gridpt_stddev
      INTEGER , INTENT(IN) :: skebs_vertstruc
      CHARACTER, INTENT(IN) :: variable_in
      INTEGER, DIMENSION (seed_dim), INTENT(INOUT) :: iseedarr
      REAL, DIMENSION(ims:ime,kms:kme, jms:jme),INTENT(IN) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION(lmax_ideal,kmax_ideal) ,INTENT(INOUT) :: SPFORCS,SPFORCC,SP_AMP
      REAL, DIMENSION(kms:kme ) ,INTENT(IN) :: VERTAMP
      REAL, DIMENSION(ims:ime,kms:kme_stoch, jms:jme) :: RAND_PERT
      REAL :: RY,RX
      INTEGER :: IK,IL,ILEV,NLON,NLAT,IJ,I,J,K,numbands
      COMPLEX, DIMENSION (LMAX_ideal) :: dummy_complex2
      COMPLEX, DIMENSION (KMAX_ideal) :: dummy_complex3
      COMPLEX, DIMENSION (ipsx:ipex) :: dummy_complex
      REAL :: thresh
      REAL, DIMENSION(LMAX_ideal,KMAX_ideal) :: ZFORCC,ZFORCS
      REAL, DIMENSION(ids:ide,jds:jde) :: ZFORCg
      INTEGER :: IER,LENWRK,KMAX,LMAX,LENSAV,JJ,II
      INTEGER :: its,ite,jts,jte,ind
      REAL, ALLOCATABLE :: WORK(:)
      CHARACTER (LEN=160) :: mess
      real, dimension (LMAX_ideal) :: b,c,d
      LOGICAL :: RESTART
      CHARACTER :: variable
      variable = variable_in
      NLAT=(jde-jds)+1
      NLON=(ide-ids)+1
      KMAX = NLAT
      LMAX = NLON
      RY= NLAT*DY
      RX= NLON*DX
      LENWRK=2*KMAX*LMAX
      ALLOCATE(WORK(LENWRK))
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8
              IF (variable .ne. 'V') THEN
               CALL UPDATE_STOCH( &
                          SPFORCS,SPFORCC,SP_AMP,ALPH_RAND, &
                          restart,iseedarr,seed_dim, &
                          kmax_ideal, lmax_ideal, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
     kts,kte,kts,kte,kts, kte )
              endif
             ZFORCC=SPFORCC
             ZFORCS=SPFORCS
              IF (variable == 'U') THEN
               DO J = 1, KMAX_ideal
                 DO I = 1, LMAX_ideal
                       ZFORCC(I,J) = 2*RPI/RY* wavenumber_k(J) * ZFORCS(I,J)
                       ZFORCS(I,J) = -2*RPI/RY* wavenumber_k(J) * ZFORCC(I,J)
                 ENDDO
               ENDDO
              ELSEIF (variable == 'V') THEN
               DO J = 1, KMAX_ideal
                 DO I = 1, LMAX_ideal
                       ZFORCC(I,J) = -2*RPI/RX* wavenumber_l(I) * ZFORCS(I,J)
                       ZFORCS(I,J) = 2*RPI/RX* wavenumber_l(I) * ZFORCC(I,J)
                 ENDDO
               ENDDO
              endif
         DO J = 1, KMAX_ideal
           DO i = 1, LMAX_ideal
             dummy_complex2(i)=cmplx(ZFORCC(i,j),ZFORCS(i,j))
           ENDDO
           CALL cFFT1B (LMAX_ideal, 1 ,dummy_complex2,LMAX_ideal, WSAVE1_ideal, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field U'
              CALL wrf_debug(0,mess)
           end if
           DO i = 1, LMAX_ideal
             ZFORCC(i,j)=real(dummy_complex2(i))
             ZFORCS(i,j)=imag(dummy_complex2(i))
           END DO
         END DO
          DO I = 1, LMAX_ideal
           DO j = 1, KMAX_ideal
            dummy_complex3(j)=cmplx(ZFORCC(i,j),ZFORCS(i,j))
           ENDDO
            CALL cFFT1B (KMAX_ideal, 1 ,dummy_complex3,KMAX_ideal, WSAVE2_ideal, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field U'
               CALL wrf_debug(0,mess)
            end if
            DO j = 1, KMAX_ideal
                 ZFORCC(i,j)=real(dummy_complex3(j))
                 ZFORCS(i,j)=imag(dummy_complex3(j))
            END DO
          END DO
         if ((LMAX_ideal.ne.ide).or.(KMAX_ideal.ne.jde)) then
             DO J = 1, KMAX_ideal
              DO I = 1, LMAX_ideal
               dummy_complex2(i)=cmplx(ZFORCC(i,j),0.0)
              ENDDO
              call spline (rindarrayil, real(dummy_complex2) , b, c, d, LMAX_ideal )
              do i=1,LMAX
                ZFORCg(i,j)=ispline (rindarrayl(i),rindarrayil, real(dummy_complex2) , b, c, d, LMAX_ideal )
              enddo
             ENDDO
             DO I = 1, LMAX
               DO J = 1, KMAX_ideal
                 dummy_complex3(j)=cmplx(ZFORCg(i,j),0.0)
               ENDDO
               call spline (rindarrayik, real(dummy_complex3) , b, c, d, KMAX_ideal )
               do J=1,KMAX
                 ZFORCg(i,j)=ispline (rindarrayk(j),rindarrayik, real(dummy_complex3) , b, c, d, KMAX_ideal )
               enddo
             ENDDO
         else
          DO J = 1, KMAX_ideal
            DO I = 1, LMAX_ideal
              ZFORCg(i,j)=ZFORCC(i,j)
            ENDDO
           ENDDO
         endif
      thresh=thresh_fact*gridpt_stddev
      RAND_PERT=0.0
      !$OMP PARALLEL DO &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles
               DO k=kts,min(kte,grid%num_stoch_levels)
                 DO I=grid%i_start(ij), grid%i_end(ij)
                   DO j=grid%j_start(ij), grid%j_end(ij)
                    RAND_PERT(I,K,J)=MAX(MIN(ZFORCg(I,J),thresh),-1.0*thresh)
                   ENDDO
                 ENDDO
               ENDDO
         ENDDO
        !$OMP END PARALLEL DO
       END SUBROUTINE RAND_PERT_UPDATE
        subroutine read_write_stochrestart (filename1,grid,config_flags,lmax_ideal,kmax_ideal,SPFORS,SPFORC,nswitch)
      USE module_configure , ONLY : grid_config_rec_type
      USE module_date_time
      USE module_timing
      USE module_utility
      USE module_domain
      USE module_dm
      IMPLICIT NONE
      TYPE(domain) :: grid
      TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
      REAL, DIMENSION(lmax_ideal,kmax_ideal) ,INTENT(INOUT) :: SPFORC, SPFORS
      INTEGER , INTENT(IN) :: kmax_ideal,lmax_ideal,nswitch
      INTEGER :: ierr
      INTEGER , PARAMETER :: write_restart = 1
      INTEGER , PARAMETER :: read_restart = 2
      INTEGER , PARAMETER :: fid=19
      TYPE(WRFU_Time) :: next_time, currentTime, startTime
      CHARACTER (LEN=256) :: current_date_char, date_string, next_timestr_char
      character(len=13) :: filename1
      character(len=50) :: filename
      character(len=70) :: message
      CHARACTER (len=len_current_date) current_timestr, next_timestr
      logical itsopen
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor
     if(wrf_dm_on_monitor()) then
      IF ( nswitch .eq. write_restart) then
         CALL domain_clock_get( grid, current_timestr=current_date_char )
         write(filename,FMT='(A,A,A)'),trim(filename1),'_rst_d01'
         inquire(unit=19, opened=itsopen)
         if ( itsopen ) then
            write(*,*) 'Its open already'
         else
          print*,'opening ', filename, ' for writing restart file for stochastic suite'
          OPEN(19, FILE=filename,FORM='UNFORMATTED',STATUS='REPLACE',IOSTAT=ierr)
          IF(ierr .NE. 0 ) THEN
            WRITE(message,FMT='(A)') &
            'problem with writing restart files for stochastic physics suite'
             CALL wrf_error_fatal3("<stdin>",1265,&
message )
          END IF
         endif
         write(19),SPFORC,SPFORS
         close(19)
      else IF ( nswitch .eq. read_restart) then
        CALL domain_clock_get( grid, current_timestr=current_date_char )
         write(filename,FMT='(A,A,A)'),trim(filename1),'_rst_d01'
         inquire(unit=19, opened=itsopen)
         if ( itsopen ) then
           write(*,*) 'Its open already'
         else
           print*,'RESTART run: opening ', filename, ' for reading'
           OPEN(19, FILE=filename,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ierr)
         IF(ierr .NE. 0 ) THEN
           WRITE(message,FMT='(A)') &
           'problem with reading restart files for stochastic physics suite'
            CALL wrf_error_fatal3("<stdin>",1285,&
message )
         END IF
         end if
         read(19),SPFORC,SPFORS
         close(19)
      endif
     endif
      END SUBROUTINE read_write_stochrestart
            subroutine findindex( wavenumber_k, wavenumber_L, &
                       KMAX_ideal, LMAX_ideal)
      IMPLICIT NONE
      INTEGER :: IK,IL,KMAX,LMAX,KMAX_ideal, LMAX_ideal
      INTEGER, DIMENSION (1:KMAX_ideal):: wavenumber_k
      INTEGER, DIMENSION (1:LMAX_ideal):: wavenumber_l
      KMAX=KMAX_ideal
      LMAX=LMAX_ideal
      DO IK=1,KMAX/2+1
        wavenumber_k(IK)=IK-1
      ENDDO
      DO IK=KMAX,KMAX/2+2,-1
        wavenumber_k(IK)=IK-KMAX-1
      ENDDO
      DO IL=1,LMAX/2+1
        wavenumber_l(IL)=IL-1
      ENDDO
      DO IL=LMAX,LMAX/2+2,-1
        wavenumber_l(IL)=IL-LMAX-1
      ENDDO
      END subroutine findindex
     subroutine gauss_noise(z)
      real :: z
      real :: x,y,r, coeff
      do
      call random_number( x )
      call random_number( y )
      x = 2.0 * x - 1.0
      y = 2.0 * y - 1.0
      r = x * x + y * y
      if ( r > 0.0 .and. r < 1.0 ) exit
      end do
      coeff = sqrt( -2.0 * log(r) / r )
      z = coeff * x
     end subroutine gauss_noise
     SUBROUTINE rand_seed (config_flags, iseed1, iseedarr, seed_start, seed_dim )
     USE module_configure
     IMPLICIT NONE
      TYPE (grid_config_rec_type) :: config_flags
     INTEGER :: iseed1 , seed_start, seed_dim
     INTEGER, DIMENSION (seed_start:seed_dim), INTENT(OUT):: iseedarr
      integer*8 :: fctime, one_big
      integer :: i
      fctime = config_flags%start_year * ( config_flags%start_month*100+config_flags%start_day) + config_flags%start_hour
      one_big = 1
      iseedarr=0
      do i = seed_start,seed_dim
         iseedarr(i)=mod(fctime+iseed1*config_flags%nens*1000000,19211*one_big)
      enddo
      end SUBROUTINE rand_seed
   subroutine spline (x, y, b, c, d, n)
implicit none
integer n
real x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
real h
gap = n-1
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)
do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)
end subroutine spline
  function ispline(u, x, y, b, c, d, n)
implicit none
real ispline
integer n
real u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
real dx
if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline
      end module module_stoch
