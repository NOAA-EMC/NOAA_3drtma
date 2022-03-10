MODULE module_diagnostics_driver
CONTAINS
   SUBROUTINE diagnostics_driver ( grid, config_flags, &
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
      USE module_state_description, ONLY: num_moist, num_chem, num_tracer, num_scalar, &
                                          SKIP_PRESS_DIAGS, SKIP_Z_DIAGS, &
                                          P_QG, P_QH, P_QV, P_QC, P_QI, P_QS, &
                                          P_QNG, P_QH, P_QNH, P_QR, P_QNR, &
                                          DFI_TDFI, DFI_FST, DFI_NODFI, &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME, &
                     WSM6SCHEME, ETAMPNEW, THOMPSON, THOMPSONAERO, &
                     MORR_TWO_MOMENT, GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, &
                     NSSL_2MOM, NSSL_2MOMCCN, NSSL_1MOM, NSSL_1MOMLFO, &
                     MILBRANDT2MOM , CAMMGMPSCHEME, FAST_KHAIN_LYNN, FULL_KHAIN_LYNN
      USE module_driver_constants, ONLY: max_plevs, max_zlevs
      USE module_model_constants, ONLY: g
      USE module_domain, ONLY : domain, domain_clock_get, domain_get_current_time
      USE module_configure, ONLY : grid_config_rec_type, &
                                   model_config_rec
      USE module_streams
      USE module_utility, ONLY : WRFU_Time
      USE module_lightning_driver, ONLY : lightning_driver
      USE module_diag_misc, ONLY : diagnostic_output_calc, refl_first_time_step
      USE module_diag_gsd, ONLY : gsd_diagnostic_driver, gsd_refl_first_time_step
      USE module_diag_cl, ONLY : clwrf_output_calc
      USE module_diag_pld, ONLY : pld
      USE module_diag_zld, ONLY : zld
      USE module_diag_afwa, ONLY : afwa_diagnostics_driver
      USE module_diag_rasm, ONLY : mean_output_calc, diurnalcycle_output_calc
      USE module_diag_hailcast, ONLY : hailcast_diagnostic_driver
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
      CHARACTER (LEN=1000) :: diag_message
      INTEGER :: ij
      INTEGER :: k_start, k_end
      LOGICAL :: first_time_step
      TYPE(WRFU_Time) :: currentTime
      CALL wrf_debug ( 100 , '--> TOP OF DIAGNOSTICS PACKAGE' )
      k_start = kps
      k_end = kpe-1
      LIGHTNING: IF ( config_flags%lightning_option /= 0 ) THEN
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: LIGHTNING_DRIVER' )
         CALL lightning_driver ( &
            grid%itimestep, grid%dt, grid%dx, grid%dy, &
            grid%xlat, grid%xlong, grid%xland, grid%ht, &
            grid%t_phy, p_phy, grid%rho, &
            grid%u_phy, grid%v_phy, grid%w_2, &
            th_phy, pi_phy,dz8w, &
            grid%z, moist, &
            grid%ktop_deep, grid%refl_10cm, &
            domain_get_current_time( grid ), &
            config_flags%lightning_option, &
            config_flags%lightning_dt, &
            config_flags%lightning_start_seconds, &
            config_flags%flashrate_factor, &
            config_flags%iccg_method, &
            config_flags%iccg_prescribed_num, &
            config_flags%iccg_prescribed_den, &
            grid%iccg_in_num, grid%iccg_in_den, &
            config_flags%cellcount_method, &
            config_flags%cldtop_adjustment, &
            ids, ide, jds, jde, kds, kde, &
            ims, ime, jms, jme, kms, kme, &
            ips, ipe, jps, jpe, kps, kpe, &
            grid%ic_flashcount, grid%ic_flashrate, &
            grid%cg_flashcount, grid%cg_flashrate, &
            grid%lpi &
      )
      END IF LIGHTNING
      HAILCAST: IF ( config_flags%hailcast_opt /= 0 ) THEN
      IF ( ( config_flags%history_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "HAILCAST Error : No 'history_interval' defined in namelist"
            CALL wrf_error_fatal3("<stdin>",173,&
diag_message )
        END IF
        !$OMP PARALLEL DO &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 100 , &
             '--> CALL DIAGNOSTICS PACKAGE: HAILCAST_DIAGNOSTIC_DRIVER' )
           CALL hailcast_diagnostic_driver ( grid , config_flags &
                         ,moist, grid%rho &
                         ,ids, ide, jds, jde, kds, kde &
                         ,ims, ime, jms, jme, kms, kme &
                         ,ips, ipe, jps, jpe, kps, kpe &
                         ,grid%i_start(ij),grid%i_end(ij) &
                         ,grid%j_start(ij),grid%j_end(ij) &
                         ,k_start,k_end &
                         ,dt=grid%dt, itimestep=grid%itimestep &
                         ,haildt=grid%haildt, curr_secs=curr_secs2 &
                         ,reset_interval1=grid%reset_interval1 &
                         ,haildtacttime=grid%haildtacttime )
        END DO
        !$OMP END PARALLEL DO
      END IF HAILCAST
      CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: NWP DIAGNOSTICS' )
IF (config_flags%gsd_diagnostics /= 1) THEN
      mp_select: SELECT CASE(config_flags%mp_physics)
        CASE (LINSCHEME, WSM6SCHEME, WDM6SCHEME, GSFCGCESCHEME, NSSL_1MOMLFO)
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
        CASE (THOMPSON, THOMPSONAERO)
      first_time_step = (grid%itimestep .eq. 1)
      IF (first_time_step .and. (config_flags%do_radar_ref .eq. 1)) THEN
        CALL refl_first_time_step( &
                 REFL_10CM=grid%refl_10cm &
                ,MP_PHYSICS=config_flags%mp_physics &
                ,QV_CURR=moist(ims,kms,jms,P_QV) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QNR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,RHO_CURR=grid%rho &
                ,TH=th_phy, PII=pi_phy, P=p_phy &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                       )
        diag_flag = .true.
      ENDIF
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,NR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
        CASE (MORR_TWO_MOMENT)
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,NG_CURR=scalar(ims,kms,jms,P_QNG) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
        CASE (NSSL_1MOM)
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QH_CURR=moist(ims,kms,jms,P_QH) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
        CASE (MILBRANDT2MOM, NSSL_2MOM, NSSL_2MOMCCN)
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,NG_CURR=scalar(ims,kms,jms,P_QNG) &
                ,QH_CURR=moist(ims,kms,jms,P_QH) &
                ,NH_CURR=scalar(ims,kms,jms,P_QNH) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
        CASE DEFAULT
      CALL diagnostic_output_calc( &
                 DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUPELNCV=grid%graupelncv &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,CURR_SECS2=curr_secs2 &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,NR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
   END SELECT mp_select
END IF
      CLIMATE_DIAGS : IF ( config_flags%output_diagnostics == 1 ) THEN
         IF ((grid%dfi_opt .EQ. DFI_TDFI .AND. grid%dfi_stage .EQ. DFI_FST) &
            .OR. (grid%dfi_opt .EQ. DFI_NODFI)) THEN
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: CLIMATE DIAGNOSTICS' )
         CALL clwrf_output_calc( &
                     is_restart=config_flags%restart &
                    ,clwrfH=config_flags%diag_int &
                    ,wind_int=config_flags%wind_int &
                    ,T2=grid%t2, Q2=grid%q2, U10=grid%u10, V10=grid%v10 &
                    ,SKINTEMP=grid%tsk &
                    ,SWRAD=grid%swdown &
                    ,SWNORM=grid%swddni &
                    ,T2CLMIN=grid%t2min, T2CLMAX=grid%t2max &
                    ,TT2CLMIN=grid%tt2min, TT2CLMAX=grid%tt2max &
                    ,T2CLMEAN=grid%t2mean, T2CLSTD=grid%t2std &
                    ,Q2CLMIN=grid%q2min, Q2CLMAX=grid%q2max &
                    ,TQ2CLMIN=grid%tq2min, TQ2CLMAX=grid%tq2max &
                    ,Q2CLMEAN=grid%q2mean, Q2CLSTD=grid%q2std &
                    ,U10CLMAX=grid%u10max, V10CLMAX=grid%v10max &
                    ,SPDUV10CLMAX=grid%spduv10max &
                    ,TSPDUV10CLMAX=grid%tspduv10max &
                    ,U10CLMEAN=grid%u10mean, V10CLMEAN=grid%v10mean &
                    ,SPDUV10CLMEAN=grid%spduv10mean &
                    ,U10CLSTD=grid%u10std, V10CLSTD=grid%v10std &
                    ,SPDUV10CLSTD=grid%spduv10std &
                    ,RAINCCLMAX=grid%raincvmax &
                    ,RAINNCCLMAX=grid%rainncvmax &
                    ,TRAINCCLMAX=grid%traincvmax &
                    ,TRAINNCCLMAX=grid%trainncvmax &
                    ,RAINCCLMEAN=grid%raincvmean &
                    ,RAINNCCLMEAN=grid%rainncvmean &
                    ,RAINCCLSTD=grid%raincvstd &
                    ,RAINNCCLSTD=grid%rainncvstd &
                    ,SKINTEMPCLMIN=grid%skintempmin &
                    ,SKINTEMPCLMAX=grid%skintempmax &
                    ,TSKINTEMPCLMIN=grid%tskintempmin &
                    ,TSKINTEMPCLMAX=grid%tskintempmax &
                    ,SKINTEMPCLMEAN=grid%skintempmean &
                    ,SKINTEMPCLSTD=grid%skintempstd &
        & ,SWRADMIN=grid%swradmin &
        & ,SWRADMAX=grid%swradmax &
        & ,TSWRADMIN=grid%tswradmin &
        & ,TSWRADMAX=grid%tswradmax &
        & ,SWRADMEAN=grid%swradmean &
        & ,SWRADSTD=grid%swradstd &
        & ,SWNORMMIN=grid%swnormmin &
        & ,SWNORMMAX=grid%swnormmax &
        & ,TSWNORMMIN=grid%tswnormmin &
        & ,TSWNORMMAX=grid%tswnormmax &
        & ,SWNORMMEAN=grid%swnormmean &
        & ,SWNORMSTD=grid%swnormstd &
                    ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                    ,DT=grid%dt &
                    ,XTIME=grid%xtime,CURR_SECS2=curr_secs2 &
                    ,NSTEPS=grid%nsteps,NSTEPS_wind=grid%nsteps_wind &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                    ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                    ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                    ,KTS=k_start, KTE=k_end &
                    ,NUM_TILES=grid%num_tiles &
                                                                   )
         ENDIF
      END IF CLIMATE_DIAGS
      PL_DIAGNOSTICS : IF ( config_flags%p_lev_diags .NE. SKIP_PRESS_DIAGS ) THEN
         TIME_TO_DO_PL_DIAGS : IF ( ( ( MOD(NINT(curr_secs2+grid%dt),NINT(config_flags%p_lev_interval)) .EQ. 0 ) ) .OR. &
               ( config_flags%use_adaptive_time_step ) ) THEN
            !$OMP PARALLEL DO &
            !$OMP PRIVATE ( ij )
            DO ij = 1 , grid%num_tiles
               CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: PRESSURE LEVEL DIAGNOSTICS' )
               CALL pld ( &
                       U=grid%u_2 &
                      ,V=grid%v_2 &
                      ,W=grid%w_2 &
                      ,t=grid%t_2 &
                      ,qv=moist(:,:,:,P_QV) &
                      ,zp=grid%ph_2 &
                      ,zb=grid%phb &
                      ,pp=grid%p &
                      ,pb=grid%pb &
                      ,p=grid%p_hyd &
                      ,pw=grid%p_hyd_w &
                      ,msfux=grid%msfux &
                      ,msfuy=grid%msfuy &
                      ,msfvx=grid%msfvx &
                      ,msfvy=grid%msfvy &
                      ,msftx=grid%msftx &
                      ,msfty=grid%msfty &
                      ,f=grid%f &
                      ,e=grid%e &
                      ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p &
                      ,extrap_below_grnd=config_flags%extrap_below_grnd &
                      ,missing=config_flags%p_lev_missing &
                      ,num_press_levels=config_flags%num_press_levels &
                      ,max_press_levels=max_plevs &
                      ,press_levels=model_config_rec%press_levels &
                      ,p_pl = grid%p_pl &
                      ,u_pl = grid%u_pl &
                      ,v_pl = grid%v_pl &
                      ,t_pl = grid%t_pl &
                      ,rh_pl = grid%rh_pl &
                      ,ght_pl= grid%ght_pl &
                      ,s_pl = grid%s_pl &
                      ,td_pl = grid%td_pl &
                      ,q_pl = grid%q_pl &
                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                      ,ITS=grid%i_start(ij),ITE=grid%i_end(ij) &
                      ,JTS=grid%j_start(ij),JTE=grid%j_end(ij) &
                      ,KTS=k_start,KTE=k_end+1 )
            END DO
            !$OMP END PARALLEL DO
         END IF TIME_TO_DO_PL_DIAGS
      END IF PL_DIAGNOSTICS
      ZL_DIAGNOSTICS : IF ( config_flags%z_lev_diags .NE. SKIP_Z_DIAGS ) THEN
         TIME_TO_DO_ZL_DIAGS : IF ( ( ( MOD(NINT(curr_secs2+grid%dt),NINT(config_flags%z_lev_interval)) .EQ. 0 ) ) .OR. &
               ( config_flags%use_adaptive_time_step ) ) THEN
            !$OMP PARALLEL DO &
            !$OMP PRIVATE ( ij )
            DO ij = 1 , grid%num_tiles
               CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: HEIGHT LEVEL AND AGL DIAGNOSTICS' )
               CALL zld ( &
                       U=grid%u_2 &
                      ,V=grid%v_2 &
                      ,W=grid%w_2 &
                      ,t=grid%t_2 &
                      ,qv=moist(:,:,:,P_QV) &
                      ,zp=grid%ph_2 &
                      ,zb=grid%phb &
                      ,pp=grid%p &
                      ,pb=grid%pb &
                      ,p=grid%p_hyd &
                      ,pw=grid%p_hyd_w &
                      ,msfux=grid%msfux &
                      ,msfuy=grid%msfuy &
                      ,msfvx=grid%msfvx &
                      ,msfvy=grid%msfvy &
                      ,msftx=grid%msftx &
                      ,msfty=grid%msfty &
                      ,f=grid%f &
                      ,e=grid%e &
                      ,ht=grid%ht &
                      ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p &
                      ,extrap_below_grnd=config_flags%extrap_below_grnd &
                      ,missing=config_flags%z_lev_missing &
                      ,num_z_levels=config_flags%num_z_levels &
                      ,max_z_levels=max_zlevs &
                      ,z_levels=model_config_rec%z_levels &
                      ,z_zl = grid%z_zl &
                      ,u_zl = grid%u_zl &
                      ,v_zl = grid%v_zl &
                      ,t_zl = grid%t_zl &
                      ,rh_zl = grid%rh_zl &
                      ,ght_zl= grid%ght_zl &
                      ,s_zl = grid%s_zl &
                      ,td_zl = grid%td_zl &
                      ,q_zl = grid%q_zl &
                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                      ,ITS=grid%i_start(ij),ITE=grid%i_end(ij) &
                      ,JTS=grid%j_start(ij),JTE=grid%j_end(ij) &
                      ,KTS=k_start,KTE=k_end+1 )
            END DO
            !$OMP END PARALLEL DO
         END IF TIME_TO_DO_ZL_DIAGS
      END IF ZL_DIAGNOSTICS
      AFWA_DIAGS : IF ( config_flags%afwa_diag_opt == 1 ) THEN
         IF ( ( config_flags%history_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "AFWA Diagnostics Error : No 'history_interval' defined in namelist"
            CALL wrf_error_fatal3("<stdin>",1165,&
diag_message )
         END IF
         !$OMP PARALLEL DO &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
            CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: AFWA DIAGNOSTICS' )
            CALL afwa_diagnostics_driver ( grid , config_flags &
                         ,moist &
                         ,scalar &
                         ,chem &
                         ,th_phy , pi_phy , p_phy &
                         ,grid%u_phy , grid%v_phy &
                         ,dz8w , p8w , t8w , rho_phy, grid%rho &
                         ,ids, ide, jds, jde, kds, kde &
                         ,ims, ime, jms, jme, kms, kme &
                         ,ips, ipe, jps, jpe, kps, kpe &
                         ,ITS=grid%i_start(ij),ITE=grid%i_end(ij) &
                         ,JTS=grid%j_start(ij),JTE=grid%j_end(ij) &
                         ,K_START=k_start,K_END=k_end )
            END DO
            !$OMP END PARALLEL DO
      ENDIF AFWA_DIAGS
      RASM_DIAGS_MEAN : IF ( config_flags%mean_diag == 1 ) THEN
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: RASM DIAGNOSTICS - MEAN' )
         CALL domain_clock_get ( grid, current_time=currentTime)
         CALL mean_output_calc( &
                        is_restart=config_flags%restart &
                       ,CURRENTTIME=currentTime &
                       ,stats_interval=config_flags%mean_interval &
                       ,output_freq=config_flags%mean_freq &
                       ,run_days=config_flags%run_days &
                       ,DT=grid%dt, XTIME=grid%xtime &
                       ,PSFC=grid%psfc, PSFC_MEAN=grid%psfc_mean &
                       ,TSK=grid%tsk, TSK_MEAN=grid%tsk_mean &
                       ,PMSL_MEAN=grid%pmsl_mean &
                       ,T2=grid%t2, T2_MEAN=grid%t2_mean &
                       ,T=grid%t_2, P=grid%p, PB=grid%pb &
                       ,MOIST=grid%moist(:,:,:,P_QV), HT=grid%ht &
                       ,TH2=grid%th2, TH2_MEAN=grid%th2_mean &
                       ,Q2=grid%q2, Q2_MEAN=grid%q2_mean &
                       ,U10=grid%u10, U10_MEAN=grid%u10_mean &
                       ,V10=grid%v10, V10_MEAN=grid%v10_mean &
                       ,HFX=grid%hfx, HFX_MEAN=grid%hfx_mean &
                       ,LH=grid%lh, LH_MEAN=grid%lh_mean &
                       ,SWDNB=grid%swdnb, SWDNB_MEAN=grid%swdnb_mean &
                       ,GLW=grid%glw , GLW_MEAN=grid%glw_mean &
                       ,LWUPB=grid%lwupb, LWUPB_MEAN=grid%lwupb_mean &
                       ,SWUPB=grid%swupb, SWUPB_MEAN=grid%swupb_mean &
                       ,SWUPT=grid%swupt, SWUPT_MEAN=grid%swupt_mean &
                       ,SWDNT=grid%swdnt, SWDNT_MEAN=grid%swdnt_mean &
                       ,LWUPT=grid%lwupt, LWUPT_MEAN=grid%lwupt_mean &
                       ,LWDNT=grid%lwdnt, LWDNT_MEAN=grid%lwdnt_mean &
                       ,AVGOUTALARM=grid%alarms(AUXHIST5_ALARM) &
                       ,AVGOUTDATESTR=grid%OUTDATE_MEAN &
                       ,NSTEPS=grid%NSTEPS_MEAN &
                       ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                       ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                       ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                       ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                       ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                       ,NUM_TILES=grid%num_tiles &
                                                                      )
      END IF RASM_DIAGS_MEAN
      RASM_DIAGS_DIURNAL : IF ( config_flags%diurnal_diag == 1 ) THEN
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: RASM DIAGNOSTICS - DIURNAL' )
         CALL domain_clock_get ( grid, current_time=currentTime)
         CALL diurnalcycle_output_calc( &
            is_restart=config_flags%restart &
           ,CURRENTTIME=currentTime &
           ,DT=grid%dt, XTIME=grid%xtime &
           ,PSFC=grid%psfc, PSFC_DTMP=grid%psfc_dtmp &
           ,TSK=grid%tsk, TSK_DTMP=grid%tsk_dtmp &
           ,T2=grid%t2, T2_DTMP=grid%t2_dtmp &
           ,T=grid%t_2, P=grid%p, PB=grid%pb, MOIST=grid%moist(:,:,:,P_QV) &
           ,TH2=grid%th2, TH2_DTMP=grid%th2_dtmp &
           ,Q2=grid%q2, Q2_DTMP=grid%q2_dtmp &
           ,U10=grid%u10, U10_DTMP=grid%u10_dtmp &
           ,V10=grid%v10, V10_DTMP=grid%v10_dtmp &
           ,HFX=grid%hfx, HFX_DTMP=grid%hfx_dtmp &
           ,LH=grid%lh, LH_DTMP=grid%lh_dtmp &
           ,SWDNB=grid%swdnb, SWDNB_DTMP=grid%swdnb_dtmp &
           ,GLW=grid%glw, GLW_DTMP=grid%glw_dtmp &
           ,LWUPB=grid%lwupb, LWUPB_DTMP=grid%lwupb_dtmp &
           ,SWUPB=grid%swupb, SWUPB_DTMP=grid%swupb_dtmp &
           ,SWUPT=grid%swupt, SWUPT_DTMP=grid%swupt_dtmp &
           ,SWDNT=grid%swdnt, SWDNT_DTMP=grid%swdnt_dtmp &
           ,LWUPT=grid%lwupt, LWUPT_DTMP=grid%lwupt_dtmp &
           ,LWDNT=grid%lwdnt, LWDNT_DTMP=grid%lwdnt_dtmp &
           ,AVGOUTALARM=grid%alarms(AUXHIST6_ALARM) &
           ,DIURNOUTDATESTR=grid%OUTDATE_DIURN &
           ,AVG_NSTEPS=grid%NSTEPSMEAN_DIURN &
           ,DIURNAL_NSTEPS=grid%NSTEPS_DIURN &
           ,PSFC_DIURN=grid%PSFC_DIURN &
           ,TSK_DIURN=grid%TSK_DIURN, T2_DIURN=grid%T2_DIURN &
           ,TH2_DIURN=grid%TH2_DIURN, Q2_DIURN=grid%Q2_DIURN &
           ,U10_DIURN=grid%U10_DIURN, V10_DIURN=grid%V10_DIURN &
           ,HFX_DIURN=grid%HFX_DIURN, LH_DIURN=grid%LH_DIURN &
           ,SWDNB_DIURN=grid%SWDNB_DIURN, GLW_DIURN=grid%GLW_DIURN &
           ,LWUPB_DIURN=grid%LWUPB_DIURN, SWUPB_DIURN=grid%SWUPB_DIURN &
           ,SWUPT_DIURN=grid%SWUPT_DIURN, SWDNT_DIURN=grid%SWDNT_DIURN &
           ,LWUPT_DIURN=grid%LWUPT_DIURN, LWDNT_DIURN=grid%LWDNT_DIURN &
           ,IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde &
           ,IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme &
           ,IPS=ips, IPE=ipe, JPS=jps, JPE=jpe, KPS=kps, KPE=kpe &
           ,I_START=grid%i_start, I_END=min(grid%i_end, ide-1) &
           ,J_START=grid%j_start, J_END=min(grid%j_end, jde-1) &
           ,NUM_TILES=grid%num_tiles &
                                                                   )
      END IF RASM_DIAGS_DIURNAL
      GSD_DIAGS : IF ( config_flags%gsd_diagnostics .GT. 0 ) THEN
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: GSD DIAGNOSTICS' )
         CALL domain_clock_get ( grid, current_time=currentTime)
      select_mp: SELECT CASE(config_flags%mp_physics)
      CASE (THOMPSON, THOMPSONAERO)
      first_time_step = (grid%itimestep .eq. 1)
      IF (first_time_step .and. (config_flags%do_radar_ref .eq. 1)) THEN
        CALL gsd_refl_first_time_step( &
                 REFL_10CM=grid%refl_10cm &
                ,MP_PHYSICS=config_flags%mp_physics &
                ,QV_CURR=moist(ims,kms,jms,P_QV) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QNR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,RHO_CURR=grid%rho &
                ,TH=th_phy, PII=pi_phy, P=p_phy &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                       )
        diag_flag = .true.
      ENDIF
         CALL gsd_diagnostic_driver( &
                 gsd_diagnostics=config_flags%gsd_diagnostics &
                ,DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUP_ACC_NC1=grid%graup_acc_nc1 &
                ,SNOW_ACC_NC1=grid%snow_acc_nc1 &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,PREC_ACC_C1=grid%prec_acc_c1 &
                ,PREC_ACC_NC1=grid%prec_acc_nc1 &
                ,PREC_ACC_DT1=config_flags%prec_acc_dt1 &
                ,CURR_SECS2=curr_secs2 &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,NR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
       CASE DEFAULT
         CALL gsd_diagnostic_driver( &
                 gsd_diagnostics=config_flags%gsd_diagnostics &
                ,DPSDT=grid%dpsdt ,DMUDT=grid%dmudt &
                ,P8W=p8w ,PK1M=grid%pk1m &
                ,MU_2=grid%mu_2 ,MU_2M=grid%mu_2m &
                ,U=grid%u_2 ,V=grid%v_2 &
                ,TEMP=grid%t_phy &
                ,RAINCV=grid%raincv ,RAINNCV=grid%rainncv &
                ,RAINC=grid%rainc ,RAINNC=grid%rainnc &
                ,FRAIN=grid%frain ,ACFRAIN=grid%acfrain &
                ,I_RAINC=grid%i_rainc ,I_RAINNC=grid%i_rainnc &
                ,HFX=grid%hfx ,SFCEVP=grid%sfcevp ,LH=grid%lh &
                ,DT=grid%dt ,SBW=config_flags%spec_bdy_width &
                ,XTIME=grid%xtime ,T2=grid%t2 &
           ,ACSWUPT=grid%acswupt ,ACSWUPTC=grid%acswuptc &
           ,ACSWDNT=grid%acswdnt ,ACSWDNTC=grid%acswdntc &
           ,ACSWUPB=grid%acswupb ,ACSWUPBC=grid%acswupbc &
           ,ACSWDNB=grid%acswdnb ,ACSWDNBC=grid%acswdnbc &
           ,ACLWUPT=grid%aclwupt ,ACLWUPTC=grid%aclwuptc &
           ,ACLWDNT=grid%aclwdnt ,ACLWDNTC=grid%aclwdntc &
           ,ACLWUPB=grid%aclwupb ,ACLWUPBC=grid%aclwupbc &
           ,ACLWDNB=grid%aclwdnb ,ACLWDNBC=grid%aclwdnbc &
         ,I_ACSWUPT=grid%i_acswupt ,I_ACSWUPTC=grid%i_acswuptc &
         ,I_ACSWDNT=grid%i_acswdnt ,I_ACSWDNTC=grid%i_acswdntc &
         ,I_ACSWUPB=grid%i_acswupb ,I_ACSWUPBC=grid%i_acswupbc &
         ,I_ACSWDNB=grid%i_acswdnb ,I_ACSWDNBC=grid%i_acswdnbc &
         ,I_ACLWUPT=grid%i_aclwupt ,I_ACLWUPTC=grid%i_aclwuptc &
         ,I_ACLWDNT=grid%i_aclwdnt ,I_ACLWDNTC=grid%i_aclwdntc &
         ,I_ACLWUPB=grid%i_aclwupb ,I_ACLWUPBC=grid%i_aclwupbc &
         ,I_ACLWDNB=grid%i_aclwdnb ,I_ACLWDNBC=grid%i_aclwdnbc &
                ,DIAG_PRINT=config_flags%diag_print &
                ,dfi_stage=grid%dfi_stage &
                ,BUCKET_MM=config_flags%bucket_mm &
                ,BUCKET_J =config_flags%bucket_J &
                ,MPHYSICS_OPT=config_flags%mp_physics &
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail &
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice &
                ,MPUSE_HAIL=config_flags%hail_opt &
                ,NSSL_ALPHAH=config_flags%nssl_alphah &
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl &
                ,NSSL_CNOH=config_flags%nssl_cnoh &
                ,NSSL_CNOHL=config_flags%nssl_cnohl &
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh &
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl &
                ,SFCRUNOFF=grid%sfcrunoff,UDRUNOFF=grid%udrunoff &
                ,ACRUNOFF=grid%acrunoff &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc &
                ,GRAUP_ACC_NC=grid%graup_acc_nc &
                ,GRAUP_ACC_NC1=grid%graup_acc_nc1 &
                ,SNOW_ACC_NC1=grid%snow_acc_nc1 &
                ,GRAUPELNCV=grid%graupelncv &
                ,ACSNOW=grid%acsnow &
                ,ACGRAUP=grid%acgraup &
                ,PREC_ACC_C=grid%prec_acc_c &
                ,PREC_ACC_NC=grid%prec_acc_nc &
                ,PREC_ACC_DT=config_flags%prec_acc_dt &
                ,PREC_ACC_C1=grid%prec_acc_c1 &
                ,PREC_ACC_NC1=grid%prec_acc_nc1 &
                ,PREC_ACC_DT1=config_flags%prec_acc_dt1 &
                ,CURR_SECS2=curr_secs2 &
                ,DIAGFLAG=diag_flag &
                ,HISTORY_INTERVAL=grid%history_interval &
                ,RESET_INTERVAL1=grid%reset_interval1 &
                ,ITIMESTEP=grid%itimestep &
                ,NTIMESTEPS=grid%ntimesteps &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2 &
                ,T=grid%t_2,U_PHY=grid%u_phy,V_PHY=grid%v_phy &
                ,P=grid%p,PB=grid%pb &
                ,WSPD10MAX=grid%wspd10max &
                ,WSPD10UMAX=grid%wspd10umax &
                ,WSPD10VMAX=grid%wspd10vmax &
                ,WSPD80MAX=grid%wspd80max &
                ,WSPD80UMAX=grid%wspd80umax &
                ,WSPD80VMAX=grid%wspd80vmax &
                ,WSPD10=grid%wspd10,WSPD80=grid%wspd80 &
                ,LWP=grid%lwp,IWP=grid%iwp &
                ,MAXCLDFRA=grid%maxcldfra &
                ,UP_HELI_MAX=grid%up_heli_max &
                ,UP_HELI_MIN=grid%up_heli_min &
                ,UP_HELI_MAX16=grid%up_heli_max16 &
                ,UP_HELI_MIN16=grid%up_heli_min16 &
                ,UP_HELI_MAX02=grid%up_heli_max02 &
                ,UP_HELI_MIN02=grid%up_heli_min02 &
                ,UP_HELI_MAX03=grid%up_heli_max03 &
                ,UP_HELI_MIN03=grid%up_heli_min03 &
                ,REL_VORT_MAX=grid%rel_vort_max &
                ,REL_VORT_MAX01=grid%rel_vort_max01 &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN_SUM=grid%w_mean_sum &
                ,W_MEAN=grid%w_mean &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max &
                ,refl_10cm=grid%refl_10cm &
                ,REFDM10C_MAX=grid%refdm10c_max &
                ,REFDM10C_CALC=grid%refdm10c_calc &
                ,LTG1_MAX=grid%ltg1_max,LTG2_MAX=grid%ltg2_max &
                ,LTG3_MAX=grid%ltg3_max &
                ,LTG1=grid%ltg1,LTG2=grid%ltg2,LTG3=grid%ltg3 &
                ,LTG1_CALC=grid%ltg1_calc &
                ,TOTICE_COLINT=grid%totice_colint,MSFT=grid%msft &
                ,REFL_10CM_1KM=grid%refl_10cm_1km &
                ,REFL_10CM_4KM=grid%refl_10cm_4km &
                ,COMPOSITE_REFL_10CM=grid%composite_refl_10cm &
                ,HT=grid%ht &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &
                ,QG_CURR=moist(ims,kms,jms,P_QG) &
                ,QS_CURR=moist(ims,kms,jms,P_QS) &
                ,QR_CURR=moist(ims,kms,jms,P_QR) &
                ,NR_CURR=scalar(ims,kms,jms,P_QNR) &
                ,QC_CURR=moist(ims,kms,jms,P_QC) &
                ,QI_CURR=moist(ims,kms,jms,P_QI) &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g &
                ,CLDFRA=grid%cldfra &
                ,icloud_bl=config_flags%icloud_bl &
                ,CLDFRA_BL=grid%cldfra_bl,QC_BL=grid%qc_bl &
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1) &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1) &
                ,KTS=k_start, KTE=min(k_end,kde-1) &
                ,NUM_TILES=grid%num_tiles &
                                                                    )
       END SELECT select_mp
   END IF GSD_DIAGS
   END SUBROUTINE diagnostics_driver
END MODULE module_diagnostics_driver
