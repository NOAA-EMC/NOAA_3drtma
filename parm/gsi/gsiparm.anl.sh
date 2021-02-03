gsi_namelist="
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   qoption=2,
   gencode=78,factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.false.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,emiss_bc=.true.,
   diag_precon=.true.,step_start=1.e-3,
   diag_light=.false.,print_obs_para=.true.,diag_radardbz=.false.,
   reset_bad_radbc=.false.,
   l4densvar=.false.,nhr_obsbin=3,
   use_gfs_nemsio=.true.,
 /     
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
   grid_ratio_wrfmass=${grid_ratio},
   wrf_mass_hybridcord=.true.,
 /
 &BKGERR
   vs=0.125, !vs=1.0,
   hzscl=0.373,0.746,1.5, !tuned in berror_stats
   !hzscl=0.046625,0.09325,0.1875 !hzscl/8
   bw=0.,fstat=.true.,
/
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',njqc=.false.,vqc=.false.,nvqc=.true.,
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=1.5,time_window_rad=1.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null      ps                   1.0     0     0
   prepbufr       t           null      t                    1.0     0     0
   prepbufr       q           null      q                    1.0     0     0
   prepbufr       pw          null      pw                   1.0     0     0
   satwndbufr     uv          null      uv                   1.0     0     0
   prepbufr       uv          null      uv                   1.0     0     0
   prepbufr       spd         null      spd                  1.0     0     0
   prepbufr       dw          null      dw                   1.0     0     0
   l2rwbufr       rw          null      l2rw                 1.0     0     0
   prepbufr       sst         null      sst                  1.0     0     0
   gpsrobufr      gps_ref     null      gps                  1.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             1.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              1.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            1.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            1.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            1.0     0     0
   hirs3bufr      hirs3       n16       hirs3_n16            0.0     1     0
   hirs3bufr      hirs3       n17       hirs3_n17            6.0     1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        6.0     2     0
   hirs4bufr      hirs4       n18       hirs4_n18            0.0     1     0
   hirs4bufr      hirs4       n19       hirs4_n19            1.0     2     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        1.0     1     0
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  20.0     2     0
   amsuabufr      amsua       n15       amsua_n15           10.0     2     0
   amsuabufr      amsua       n18       amsua_n18           10.0     2     0
   amsuabufr      amsua       n19       amsua_n19           10.0     2     0
   amsuabufr      amsua       metop-a   amsua_metop-a       10.0     2     0
   amsuabufr      amsua       metop-b   amsua_metop-b       10.0     2     0
   airsbufr       amsua       aqua      amsua_aqua           5.0     2     0
   amsubbufr      amsub       n17       amsub_n17            1.0     1     0
   mhsbufr        mhs         n18       mhs_n18              3.0     2     0
   mhsbufr        mhs         n19       mhs_n19              3.0     2     0
   mhsbufr        mhs         metop-a   mhs_metop-a          3.0     2     0
   mhsbufr        mhs         metop-b   mhs_metop-b          3.0     2     0
   ssmitbufr      ssmi        f13       ssmi_f13             0.0     2     0
   ssmitbufr      ssmi        f14       ssmi_f14             0.0     2     0
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     2     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     2     0
   ssmisbufr      ssmis_las   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_img   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_env   f16       ssmis_f16            0.0     2     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           1.5     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           1.5     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           1.5     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           1.5     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           1.5     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           1.5     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           1.5     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           1.5     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           1.5     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           1.5     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           1.5     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           1.5     1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15           1.5     2     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15           1.5     2     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15           1.5     2     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15           1.5     2     0
   iasibufr       iasi        metop-a   iasi616_metop-a     20.0     1     0
   gomebufr       gome        metop-a   gome_metop-a         1.0     2     0
   omibufr        omi         aura      omi_aura             1.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            1.0     0     0
   tcvitl         tcp         null      tcp                  1.0     0     0
   seviribufr     seviri      m08       seviri_m08           1.0     1     0
   seviribufr     seviri      m09       seviri_m09           1.0     1     0
   seviribufr     seviri      m10       seviri_m10           1.0     1     0
   iasibufr       iasi        metop-b   iasi616_metop-b      0.0     1     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     1     0
   crisbufr       cris        npp       cris_npp             0.0     1     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
   prepbufr       mta_cld     null      mta_cld              1.0     0     0
   prepbufr       gos_ctp     null      gos_ctp              1.0     0     0
   refInGSI       rad_ref     null      rad_ref              1.0     0     0
   lghtInGSI      lghtn       null      lghtn                1.0     0     0
   larcInGSI      larccld     null      larccld              1.0     0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000., l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${ifhyb},
   uv_hyb_ens=.true.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${nummem},
!  beta_s0=${beta1_inv},s_ens_h=110,s_ens_v=3,
   beta_s0=${beta1_inv},s_ens_h=${ens_h},s_ens_v=${ens_v},
   regional_ensemble_option=${regional_ensemble_option},
   pseudo_hybens = .false.,
   grid_ratio_ens = ${grid_ratio_ens},
   l_ens_in_diff_time=.true.,
   ensemble_path='',
   i_en_perts_io=${i_en_perts_io},
   ens_fast_read=${ens_fast_read},
   jcap_ens=574,
   readin_localization=.true.
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=10.0,
   metar_impact_radius=20.0,
   metar_impact_radius_lowCloud=8.0,
   l_metar_impact_radius_change = .false.,
   metar_impact_radius_max        = 50000.0,
   metar_impact_radius_min        = 20000.0,
   metar_impact_radius_max_height = 3000.0,
   metar_impact_radius_min_height = 200.0,
   l_gsd_terrain_match_surfTobs=.true.,
   l_sfcobserror_ramp_t=.true.,
   l_sfcobserror_ramp_q=.true.,
   l_PBL_pseudo_SurfobsT=.true.,
   l_PBL_pseudo_SurfobsQ=.true.,
   l_PBL_pseudo_SurfobsUV=.false.,
   pblH_ration=0.4,
   pps_press_incr=40.0,
   l_gsd_limit_ocean_q=.true.,
   l_pw_hgt_adjust=.true.,
   l_limit_pw_innov=.true.,
   max_innov_pct=0.1,
   l_cleanSnow_WarmTs=.true.,
   r_cleanSnow_WarmTs_threshold=5.0,
   l_conserve_thetaV=.true.,
   i_conserve_thetaV_iternum=3,
   l_gsd_soilTQ_nudge=${ifsoilnudge},
   l_cld_bld=.true.,
   cld_bld_hgt=30000.0,
   l_numconc=.true.,
   l_closeobs=.true.,
   build_cloud_frac_p=0.50,
   clear_cloud_frac_p=0.10,
   iclean_hydro_withRef_allcol=1,
   i_use_2mQ4B=2,
   i_use_2mT4B=1,
   i_gsdcldanal_type=${cloudanalysistype},
   i_gsdsfc_uselist=1,
   i_lightpcp=1,
   i_sfct_gross=1,
   i_coastline=3,
   i_gsdqc=2,
   l_use_hydroretrieval_all=.true.,
   ioption=1, 
   i_T_Q_adjust= 2,
   l_rtma3d            = .true.,
   i_precip_vertical_check = 3,
   !i_cloud_q_innovation=22,
 /
 &CHEM
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${YYYYMMDDHH},
   obhourset=0.,
 /
"
