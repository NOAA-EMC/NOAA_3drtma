SUBROUTINE shift_domain_em ( grid , disp_x, disp_y &
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,emis_ant,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par,conv_ct, &
chem_ct,vmix_ct,advh_ct,advz_ct,dvel,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs,chem_btxe,chem_btys, &
chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys,tracer_btye,nba_mij,nba_rij &
                           )
   USE module_state_description
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_domain_type, ONLY : fieldlist
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, itrace
   USE module_comm_dm, ONLY : SHIFT_HALO_sub
   IMPLICIT NONE
   INTEGER disp_x, disp_y
   TYPE(domain) , POINTER :: grid
   INTEGER i, j, ii, ipf
   INTEGER px, py
   INTEGER :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
   TYPE (grid_config_rec_type) :: config_flags
   TYPE( fieldlist ), POINTER :: p
   INTERFACE
       SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,emis_ant,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par,conv_ct, &
chem_ct,vmix_ct,advh_ct,advz_ct,dvel,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs,chem_btxe,chem_btys, &
chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys,tracer_btye,nba_mij,nba_rij &
                           )
          USE module_state_description
          USE module_domain, ONLY : domain
          IMPLICIT NONE
          INTEGER disp_x, disp_y
          TYPE(domain) , POINTER :: grid
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
       END SUBROUTINE shift_domain_em2
   END INTERFACE
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
END SUBROUTINE shift_domain_em
SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,emis_ant,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par,conv_ct, &
chem_ct,vmix_ct,advh_ct,advz_ct,dvel,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs,chem_btxe,chem_btys, &
chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys,tracer_btye,nba_mij,nba_rij &
                           )
   USE module_state_description
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_domain_type, ONLY : fieldlist
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, itrace
   USE module_comm_dm, ONLY : SHIFT_HALO_sub
   IMPLICIT NONE
   INTEGER disp_x, disp_y
   TYPE(domain) , POINTER :: grid
   INTEGER i, j, ii, jpf
   INTEGER px, py
   INTEGER :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
   TYPE (grid_config_rec_type) :: config_flags
   TYPE( fieldlist ), POINTER :: p
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
END SUBROUTINE shift_domain_em2
