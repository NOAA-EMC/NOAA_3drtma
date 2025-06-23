MODULE module_emissions_driver
   IMPLICIT NONE
CONTAINS
    subroutine emissions_driver(id,ktau,dtstep, &
                                adapt_step_flag,curr_secs, &
                                plumerisefire_frq,stepfirepl, &
                                config_flags, &
                                gmt,julday,t_phy,qvapor, &
                                u_phy,v_phy,vvel, &
                                p_phy,rho_phy,dz8w,rel_hum, &
                                ebu, emis_ant, chem, &
                                mean_frp,std_frp,mean_fsize,std_fsize, &
                                coef_bb_dc,fire_hist,aod3d_smoke, &
                                min_fplume, max_fplume,flam_frac, &
                                ebb_smoke,lu_fire1,peak_hr, &
                                xlat,xlong,luf_igbp,nlcat, &
                                z_at_w,zmid, &
         T2,swdown,rainc,rainnc, &
         current_month, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte )
  USE module_configure
  USE module_state_description
  USE module_add_emiss_burn
  USE module_plumerise1
  IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: id,julday,nlcat, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   INTEGER,INTENT(IN) :: ktau,stepfirepl
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN ) :: qvapor
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
         INTENT(INOUT ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ), &
         INTENT(INOUT ) :: ebu
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: ebb_smoke, peak_hr, lu_fire1
   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant), INTENT(IN ) :: emis_ant
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN ) :: mean_frp,std_frp,mean_fsize,std_fsize
   REAL, DIMENSION(ims:ime,jms:jme ), INTENT(OUT ) :: coef_bb_dc, flam_frac
   REAL, DIMENSION(ims:ime,jms:jme ), INTENT(INOUT ) :: fire_hist
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme ), INTENT(OUT ) :: aod3d_smoke
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme ), INTENT(IN) :: t_phy,p_phy,dz8w, rel_hum, &
                                                             z_at_w , zmid , &
                                                             u_phy,v_phy,vvel,rho_phy
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: xlat,xlong,rainc,rainnc
   REAL, DIMENSION( ims:ime,1:nlcat,jms:jme ),INTENT(IN) :: luf_igbp
   INTEGER,DIMENSION( ims:ime,jms:jme ), INTENT(OUT) :: min_fplume, max_fplume
      real, dimension (ims:ime, jms:jme), intent(in) :: T2, swdown
      integer, intent(in) :: current_month
      REAL(KIND=8), INTENT(IN ) :: curr_secs
      Integer :: endhr,endmin,beghr,begmin,ko,kk4,kl,k_initial,k_final
      real :: eh,area,x1,percen_mass_umbrel,base_umbrel,ashz_above_vent
      REAL, INTENT(IN ) :: dtstep, gmt
      REAL :: smold_frac
      INTEGER, INTENT(IN ) :: plumerisefire_frq
      LOGICAL, INTENT(IN ) :: adapt_step_flag
      INTEGER :: begday,endday,i, j, k
      REAL :: conv,conv3,conv4,oconv3,oconv4
      CHARACTER (LEN=80) :: message
      LOGICAL :: do_plumerisefire
    INTEGER, SAVE :: icall
    REAL, DIMENSION(ims:ime,jms:jme,4) :: plume_fre
 IF (ktau==1) THEN
    do j=jts,jte
       do i=its,ite
          ebu(i,kts,j,p_ebu_smoke)= ebb_smoke(i,j)
          do k=kts+1,kte
            ebu(i,k,j,p_ebu_smoke)= 0.
          enddo
       enddo
    enddo
 ENDIF
      do_plumerisefire = .false.
        IF ( ktau==2 ) then
           do_plumerisefire = .true.
        ELSE IF ( adapt_step_flag ) THEN
           IF ( (plumerisefire_frq<=0) .or. ( curr_secs+real(dtstep,8)+0.01 >= &
                ( INT( curr_secs/real(plumerisefire_frq*60.,8)+1,8 )*real(plumerisefire_frq*60.,8) ) ) ) then
              do_plumerisefire = .true.
           ENDIF
        ELSE IF ( (MOD(ktau,stepfirepl)==0) .or. (stepfirepl==1) ) THEN
           do_plumerisefire = .true.
        ENDIF
       if (icall<3000 .AND. config_flags%debug_chem) then
           WRITE(6,*) 'emissions_driver: time is ',curr_secs
           WRITE(6,*) 'emissions_driver: do_plumerisefire,plumerisefire_frq: ',do_plumerisefire,plumerisefire_frq
       end if
       IF ( do_plumerisefire ) THEN
          CALL wrf_debug(15,'fire emissions: calling biomassb')
          do j=jts,jte
            do i=its,ite
               plume_fre(i,j,1)= 1.e+6*coef_bb_dc(i,j)* mean_frp(i,j)
               plume_fre(i,j,2)= 1.e+6*coef_bb_dc(i,j)* std_frp(i,j)
               plume_fre(i,j,3)= 1.e+6*coef_bb_dc(i,j)* mean_fsize(i,j)
               plume_fre(i,j,4)= 1.e+6*coef_bb_dc(i,j)* std_fsize(i,j)
            enddo
          enddo
          IF (icall<5000 .AND. config_flags%debug_chem) then
              WRITE(6,*) 'emissions_driver: plumerise is called at'
              WRITE(6,*) 'curr_secs= ',curr_secs
              WRITE(6,*) 'emissions_driver: ktau ',ktau
              icall=icall+1
          END IF
          call plumerise_driver (id, &
                      flam_frac,ebb_smoke,ebu, &
                      config_flags, &
                      t_phy,qvapor, &
                      rho_phy,vvel,u_phy,v_phy,p_phy, &
                      z_at_w,zmid,ktau, &
                      plume_fre, min_fplume, max_fplume, &
                      ids,ide, jds,jde, kds,kde, &
                      ims,ime, jms,jme, kms,kme, &
                      its,ite, jts,jte, kts,kte )
       ENDIF
               call wrf_debug(15,'Add BB fluxes for the CHEM_SMOKE option')
               call add_emis_burn(id,dtstep,ktau,dz8w,rho_phy,rel_hum,chem, &
                                  julday,gmt,xlat,xlong,luf_igbp,nlcat, &
                                  lu_fire1,peak_hr, &
                                  curr_secs,ebu, &
                                  coef_bb_dc,fire_hist,aod3d_smoke, &
                                  rainc, rainnc,swdown, &
                                  config_flags, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte )
    END subroutine emissions_driver
END module module_emissions_driver
