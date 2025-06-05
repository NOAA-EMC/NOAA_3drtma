 subroutine chem_driver ( grid , config_flags &
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,emis_ant,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par,conv_ct, &
chem_ct,vmix_ct,advh_ct,advz_ct,dvel,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs,chem_btxe,chem_btys, &
chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys,tracer_btye,nba_mij,nba_rij &
                 )
  USE module_domain
  USE module_configure
  USE module_driver_constants
  USE module_machine
  USE module_tiles
  USE module_dm
  USE module_model_constants
  USE module_state_description
  USE module_dep_simple
  USE module_dry_dep_driver
  USE module_emissions_driver
  USE module_wetdep_ls, only:wetdep_ls
  USE module_input_smoke_data, only: last_chem_time
  IMPLICIT NONE
   TYPE(domain) , TARGET :: grid
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist) :: moist
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist) :: moist_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist) :: dfi_moist
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist) :: dfi_moist_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar) :: scalar
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar) :: scalar_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar) :: dfi_scalar
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar) :: dfi_scalar_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod) :: aerod
real ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm) :: ozmixm
real ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc) :: aerosolc_1
real ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc) :: aerosolc_2
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d) :: fdda3d
real ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d) :: fdda2d
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t) :: advh_t
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t) :: advz_t
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit,grid%sm33:grid%em33,num_emis_ant) :: emis_ant
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_dust) :: emis_dust
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas) :: emis_seas
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas2) :: emis_seas2
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_emis_vol) :: emis_vol
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ebu) :: ebu
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kfire,grid%sm33:grid%em33,num_ebu_in) :: ebu_in
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit_aircraft,grid%sm33:grid%em33,num_emis_aircraft) :: emis_aircraft
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ext_coef) :: ext_coef
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_bscat_coef) :: bscat_coef
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_asym_par) :: asym_par
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_conv_ct) :: conv_ct
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem_ct) :: chem_ct
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_vmix_ct) :: vmix_ct
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_ct) :: advh_ct
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_ct) :: advz_ct
real ,DIMENSION(grid%sm31:grid%em31,1:grid%kdvel,grid%sm33:grid%em33,num_dvel) :: dvel
real ,DIMENSION(grid%sm31:grid%em31,1:8,grid%sm33:grid%em33,num_vprm_in) :: vprm_in
real ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_wet_in) :: wet_in
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem) :: chem
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem) :: chem_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer) :: tracer
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btye
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij) :: nba_mij
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij) :: nba_rij
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER :: ids,ide, jds,jde, kds,kde, &
                ims,ime, jms,jme, kms,kme, &
                ips,ipe, jps,jpe, kps,kpe, &
                its,ite, jts,jte, kts,kte
   INTEGER :: ij,i,j,k,l,numgas,nv,n, nr,ktau,k_start,k_end,idf,jdf,kdf, ijulian
   REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: p_phy,t_phy,dz8w,rho,rel_hum
      TYPE(WRFU_TimeInterval) :: tmpTimeInterval
      REAL(KIND=8) :: curr_secs
      REAL(KIND=8) :: real_time_r8
      LOGICAL :: adapt_step_flag, do_chemstep
      CHARACTER (LEN=1000) :: msg
      CHARACTER (LEN=256) :: current_date_char
      integer :: current_month
      INTEGER, SAVE :: icall
      INTRINSIC max, min
      adapt_step_flag = .TRUE.
      ktau = grid%itimestep
      tmpTimeInterval = domain_get_time_since_sim_start(grid)
      curr_secs = real_time_r8(tmpTimeInterval)
      ijulian=ifix(grid%julian)
      do_chemstep = .false.
      IF ( ktau==1 ) then
         do_chemstep = .true.
      ELSE IF ( adapt_step_flag ) THEN
        IF ( (grid%chemdt<=0) .or. ( curr_secs+real(grid%dt,8)+0.01 >= &
           ( INT( curr_secs/real(grid%chemdt*60.,8)+1,8 )*real(grid%chemdt*60.,8) ) ) ) then
             do_chemstep = .true.
        ENDIF
      ELSE IF ( (MOD(ktau,grid%stepchem)==0) .or. (grid%stepchem==1) ) THEN
        do_chemstep = .true.
      ENDIF
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: ktau,grid%chemdt,grid%stepchem: ',ktau,grid%ktauc,grid%chemdt,grid%stepchem
        WRITE(*,*) 'chem_driver: adapt_step_flag,do_chemstep: ',adapt_step_flag,do_chemstep
        icall=icall+1
    end if
  CALL get_ijk_from_grid ( grid , &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe )
  CALL domain_clock_get( grid, current_timestr=current_date_char )
  read(current_date_char(6:7),FMT='(I2)') current_month
  numgas = 0
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
  k_start = kps
  k_end = kpe
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: ids, ide, jds, jde, kds, kde ', ids, ide, jds, jde, kds, kde
        WRITE(*,*) 'chem_driver: ims, ime, jms, jme, kms, kme ', ims, ime, jms, jme, kms, kme
        WRITE(*,*) 'chem_driver: ips, ipe, jps, jpe, kps, kpe ', ips, ipe, jps, jpe, kps, kpe
        WRITE(*,*) 'chem_driver: ktau,grid%chemdt,grid%stepchem:', ktau,grid%ktauc,grid%chemdt,grid%stepchem
        WRITE(*,*) 'chem_driver: adapt_step_flag,do_chemstep:', adapt_step_flag,do_chemstep
        icall=icall+1
    end if
chem_time: IF (do_chemstep) THEN
   chem_tile_loop_1: DO ij = 1, grid%num_tiles
       its = grid%i_start(ij)
       ite = min(grid%i_end(ij),ide-1)
       jts = grid%j_start(ij)
       jte = min(grid%j_end(ij),jde-1)
       kts = k_start
       kte = min(k_end,kde-1)
        do j = jts,jte
         do k = kts,kte
          do i = its,ite
             rho(i,k,j) = 1./grid%alt(i,k,j)
             p_phy(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
             dz8w(i,k,j) = grid%z_at_w(i,k+1,j) - grid%z_at_w(i,k,j)
             rel_hum(i,k,j)= max(.1,MIN( .95, moist(i,k,j,p_qv) / &
                                (3.80*exp(17.27*(grid%t_phy(i,k,j)-273.)/ &
                                (grid%t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j)))))
          enddo
         enddo
        enddo
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: num_tiles,grid%gmt,num_chem,numgas ', grid%num_tiles,grid%gmt,num_chem,numgas
        WRITE(*,*) 'chem_driver: ktau,grid%dt,curr_secs: ',ktau,grid%dt,curr_secs
        WRITE(*,*) 'chem_driver: its,ite,jts,jte,kts,kte ',its,ite,jts,jte,kts,kte
        WRITE(*,*) 'chem_driver: rho(its,kts,jts),p_phy(its,kts,jts),dz8w(its,kts,jts) ', rho(its,kts,jts),p_phy(its,kts,jts),dz8w(its,kts,jts)
        WRITE(*,*) 'chem_driver: rho(its,kte,jts),p_phy(its,kte,jts),dz8w(its,kte,jte) ', rho(its,kte,jts),p_phy(its,kte,jts),dz8w(its,kte,jte)
        WRITE(*,*) 'chem_driver: rel_hum(its,kts,jts), rel_hum(ite,kte,jte) ',rel_hum(its,kts,jts), rel_hum(ite,kte,jte)
        WRITE(*,*) 'chem_driver: grid%plumerisefire_frq,grid%stepfirepl: ',grid%plumerisefire_frq,grid%stepfirepl
        WRITE(*,*) 'chem_driver: dz8w(its,kts,jts),grid%z_at_w(ite,kte,jte),grid%z_at_w(ite,kte+1,jte): ',dz8w(its,kts,jts),grid%z_at_w(ite,kte,jte),grid%z_at_w(ite,kte+1,jte)
    end if
    if (config_flags%biomass_burn_opt==biomassb_smoke)then
      call emissions_driver(grid%id,ktau,grid%dt, &
              adapt_step_flag, curr_secs, &
              grid%plumerisefire_frq,grid%stepfirepl, &
              config_flags, &
              grid%gmt,ijulian,grid%t_phy,moist(:,:,:,p_qv), &
              grid%u_phy,grid%v_phy,grid%w_2, &
              p_phy,rho,dz8w, rel_hum, &
              ebu, emis_ant, chem, &
              grid%mean_frp,grid%std_frp,grid%mean_fsize,grid%std_fsize, &
              grid%coef_bb_dc,grid%fire_hist,grid%aod3d_smoke, &
              grid%min_fplume,grid%max_fplume,grid%flam_frac, &
              grid%ebb_smoke,grid%lu_fire1,grid%peak_hr, &
              grid%xlat,grid%xlong,grid%LANDUSEF,grid%num_land_cat, &
              grid%z_at_w,grid%z, &
              grid%T2,grid%swdown,grid%RAINC,grid%RAINNC, &
              current_month, &
              ids,ide, jds,jde, kds,kde, &
              ims,ime, jms,jme, kms,kme, &
              its,ite,jts,jte,kts,kte )
     endif
     if (config_flags%vertmix_onoff>0) then
         if (ktau.gt.2) then
            call wrf_debug(15,'calling dry_deposition_driver')
            call dry_dep_driver( grid%id,ktau,grid%dt,config_flags, &
                                 dz8w,rho,rel_hum,grid%exch_h,grid%hfx, &
                                 grid%pblh,grid%rmol,grid%ust, &
                                 grid%z,grid%z_at_w, &
                                 grid%LU_INDEX,grid%ddmass_smoke, &
                                 grid%mean_frp,grid%min_fplume,grid%coef_bb_dc, &
                                 chem, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite,jts,jte,kts,kte )
         end if
     end if
        if(config_flags%wetscav_onoff<0)then
           call wrf_debug(15,'calculate LS wet deposition')
           call wetdep_ls(grid%dt,chem,grid%rainncv,moist,rho,num_moist, &
                num_chem,numgas,dz8w,grid%w_2,grid%chem_opt, &
                ids,ide, jds,jde, kds,kde, &
                ims,ime, jms,jme, kms,kme, &
                its,ite, jts,jte, kts,kte )
        endif
      do nv=2,num_chem
         do j=jts,jte
            do i=its,ite
                  chem(i,k_end,j,nv)=chem(i,kte,j,nv)
            enddo
         enddo
      enddo
   END DO chem_tile_loop_1
END IF chem_time
   END subroutine chem_driver
