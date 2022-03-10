SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE(domain) :: parent , nest
   INTEGER iswater, islake, isice, isurban, isoilwater, map_proj, julyr, julday
   REAL truelat1 , truelat2 , gmt , moad_cen_lat , stand_lon, pole_lat, pole_lon
   CHARACTER (LEN=256) :: char_junk
   nest%p_top = parent%p_top
   nest%save_topo_from_real = parent%save_topo_from_real
   nest%cfn = parent%cfn
   nest%cfn1 = parent%cfn1
   nest%rdx = 1./nest%dx
   nest%rdy = 1./nest%dy
   nest%dtseps = parent%dtseps
   nest%resm = parent%resm
   nest%zetatop = parent%zetatop
   nest%cf1 = parent%cf1
   nest%cf2 = parent%cf2
   nest%cf3 = parent%cf3
   nest%gmt = parent%gmt
   nest%julyr = parent%julyr
   nest%julday = parent%julday
   nest%iswater = parent%iswater
   nest%isice = parent%isice
   nest%isurban = parent%isurban
   nest%islake = parent%islake
   nest%isoilwater = parent%isoilwater
   nest%mminlu = trim(parent%mminlu)
   nest%tiso = parent%tiso
   nest%tlp = parent%tlp
   nest%p00 = parent%p00
   nest%t00 = parent%t00
   nest%tlp_strat= parent%tlp_strat
   nest%p_strat = parent%p_strat
   nest%traj_k = parent%traj_k
   nest%traj_long = parent%traj_long
   nest%traj_lat = parent%traj_lat
   nest%this_is_an_ideal_run = parent%this_is_an_ideal_run
   nest%lake_depth_flag = parent%lake_depth_flag
   CALL nl_get_mminlu ( 1, char_junk )
   CALL nl_get_iswater( 1, iswater )
   CALL nl_get_islake ( 1, islake )
   CALL nl_get_isice ( 1, isice )
   CALL nl_get_isurban( 1, isurban )
   CALL nl_get_isoilwater(1, isoilwater )
   CALL nl_get_truelat1 ( 1 , truelat1 )
   CALL nl_get_truelat2 ( 1 , truelat2 )
   CALL nl_get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL nl_get_stand_lon ( 1 , stand_lon )
   CALL nl_get_pole_lat ( 1 , pole_lat )
   CALL nl_get_pole_lon ( 1 , pole_lon )
   CALL nl_get_map_proj ( 1 , map_proj )
   CALL nl_get_gmt ( 1 , gmt)
   CALL nl_get_julyr ( 1 , julyr)
   CALL nl_get_julday ( 1 , julday)
   IF ( nest%id .NE. 1 ) THEN
     CALL nl_set_gmt (nest%id, gmt)
     CALL nl_set_julyr (nest%id, julyr)
     CALL nl_set_julday (nest%id, julday)
     CALL nl_set_iswater ( nest%id, iswater )
     CALL nl_set_islake ( nest%id, islake )
     CALL nl_set_isice ( nest%id, isice )
     CALL nl_set_isurban ( nest%id, isurban )
     CALL nl_set_isoilwater ( nest%id, isoilwater )
     CALL nl_set_mminlu ( nest%id, char_junk )
     CALL nl_set_truelat1 ( nest%id , truelat1 )
     CALL nl_set_truelat2 ( nest%id , truelat2 )
     CALL nl_set_moad_cen_lat ( nest%id , moad_cen_lat )
     CALL nl_set_stand_lon ( nest%id , stand_lon )
     CALL nl_set_pole_lat ( nest%id , pole_lat )
     CALL nl_set_pole_lon ( nest%id , pole_lon )
     CALL nl_set_map_proj ( nest%id , map_proj )
   END IF
   nest%gmt = gmt
   nest%julday = julday
   nest%julyr = julyr
   nest%iswater = iswater
   nest%islake = islake
   nest%isice = isice
   nest%isoilwater = isoilwater
   nest%mminlu = trim(char_junk)
   nest%truelat1= truelat1
   nest%truelat2= truelat2
   nest%moad_cen_lat= moad_cen_lat
   nest%stand_lon= stand_lon
   nest%pole_lat= pole_lat
   nest%pole_lon= pole_lon
   nest%map_proj= map_proj
   nest%step_number = parent%step_number
      nest%fnm(1:parent%e_vert) = parent%fnm(1:parent%e_vert)
      nest%fnp(1:parent%e_vert) = parent%fnp(1:parent%e_vert)
      nest%rdnw(1:parent%e_vert) = parent%rdnw(1:parent%e_vert)
      nest%rdn(1:parent%e_vert) = parent%rdn(1:parent%e_vert)
      nest%dnw(1:parent%e_vert) = parent%dnw(1:parent%e_vert)
      nest%dn(1:parent%e_vert) = parent%dn(1:parent%e_vert)
      nest%znu(1:parent%e_vert) = parent%znu(1:parent%e_vert)
      nest%znw(1:parent%e_vert) = parent%znw(1:parent%e_vert)
      nest%t_base(1:parent%e_vert) = parent%t_base(1:parent%e_vert)
      nest%u_base(1:parent%e_vert) = parent%u_base(1:parent%e_vert)
      nest%v_base(1:parent%e_vert) = parent%v_base(1:parent%e_vert)
      nest%qv_base(1:parent%e_vert) = parent%qv_base(1:parent%e_vert)
      nest%z_base(1:parent%e_vert) = parent%z_base(1:parent%e_vert)
      nest%c1h(1:parent%e_vert) = parent%c1h(1:parent%e_vert)
      nest%c2h(1:parent%e_vert) = parent%c2h(1:parent%e_vert)
      nest%c3h(1:parent%e_vert) = parent%c3h(1:parent%e_vert)
      nest%c4h(1:parent%e_vert) = parent%c4h(1:parent%e_vert)
      nest%c1f(1:parent%e_vert) = parent%c1f(1:parent%e_vert)
      nest%c2f(1:parent%e_vert) = parent%c2f(1:parent%e_vert)
      nest%c3f(1:parent%e_vert) = parent%c3f(1:parent%e_vert)
      nest%c4f(1:parent%e_vert) = parent%c4f(1:parent%e_vert)
   nest%dzs = parent%dzs
   nest%zs = parent%zs
END SUBROUTINE init_domain_constants_em
SUBROUTINE init_domain_vert_nesting ( parent, nest, use_baseparam_fr_nml )
   USE module_domain
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent, nest
   LOGICAL :: use_baseparam_fr_nml
   REAL, DIMENSION(parent%e_vert) :: znw_c
   INTERFACE
      SUBROUTINE vert_cor_vertical_nesting_integer(nest,znw_c,k_dim_c)
         USE module_domain
         TYPE(domain), POINTER :: nest
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c
      END SUBROUTINE vert_cor_vertical_nesting_integer
      SUBROUTINE vert_cor_vertical_nesting_arbitrary(nest,znw_c,kde_c,use_baseparam_fr_nml)
          USE module_domain
          TYPE(domain), POINTER :: nest
          INTEGER, INTENT(IN ) :: kde_c
          REAL, DIMENSION(kde_c), INTENT(IN ) :: znw_c
          LOGICAL, INTENT(IN ) :: use_baseparam_fr_nml
      END SUBROUTINE vert_cor_vertical_nesting_arbitrary
   END INTERFACE
   znw_c = nest%znw(1:parent%e_vert)
   if (nest%vert_refine_method .EQ. 1) then
      CALL vert_cor_vertical_nesting_integer(nest,znw_c,parent%e_vert)
   elseif (nest%vert_refine_method .EQ. 2) then
      CALL vert_cor_vertical_nesting_arbitrary(nest,znw_c,parent%e_vert,use_baseparam_fr_nml)
   endif
END SUBROUTINE init_domain_vert_nesting
      SUBROUTINE vert_cor_vertical_nesting_integer(nest,znw_c,k_dim_c)
         USE module_domain
   IMPLICIT NONE
         TYPE(domain), POINTER :: nest
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c
       integer :: kde_c , kde_n ,n_refine,ii,kkk,k
       real :: dznw_m,cof1,cof2
        kde_c = k_dim_c
        kde_n = nest%e_vert
        n_refine = (kde_n-1)/(kde_c-1)
         kkk = 0
         do k = 1 , kde_c-1
         dznw_m = znw_c(k+1) - znw_c(k)
         do ii = 1,n_refine
         kkk = kkk + 1
         nest%znw(kkk) = znw_c(k) + float(ii-1)/float(n_refine)*dznw_m
         enddo
         enddo
         nest%znw(kde_n) = znw_c(kde_c)
         nest%znw(1) = znw_c(1)
      DO k=1, kde_n-1
         nest%dnw(k) = nest%znw(k+1) - nest%znw(k)
         nest%rdnw(k) = 1./nest%dnw(k)
         nest%znu(k) = 0.5*(nest%znw(k+1)+nest%znw(k))
      END DO
      DO k=2, kde_n-1
         nest%dn(k) = 0.5*(nest%dnw(k)+nest%dnw(k-1))
         nest%rdn(k) = 1./nest%dn(k)
         nest%fnp(k) = .5* nest%dnw(k )/nest%dn(k)
         nest%fnm(k) = .5* nest%dnw(k-1)/nest%dn(k)
      END DO
  cof1 = (2.*nest%dn(2)+nest%dn(3))/(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(2)
  cof2 = nest%dn(2) /(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(3)
      nest%cf1 = nest%fnp(2) + cof1
      nest%cf2 = nest%fnm(2) - cof1 - cof2
      nest%cf3 = cof2
      nest%cfn = (.5*nest%dnw(kde_n-1)+nest%dn(kde_n-1))/nest%dn(kde_n-1)
      nest%cfn1 = -.5*nest%dnw(kde_n-1)/nest%dn(kde_n-1)
      END SUBROUTINE vert_cor_vertical_nesting_integer
SUBROUTINE vert_cor_vertical_nesting_arbitrary(nest,znw_c,kde_c,use_baseparam_fr_nml)
    USE module_domain
    USE module_model_constants
    IMPLICIT NONE
    TYPE(domain), POINTER :: nest
    INTEGER, INTENT(IN ) :: kde_c
    REAL, DIMENSION(kde_c), INTENT(IN ) :: znw_c
    LOGICAL, INTENT(IN ) :: use_baseparam_fr_nml
    INTEGER :: k, kde_n, ks, id
    REAL :: cof1, cof2
    REAL :: max_dz = 1000
    REAL :: p00, t00, a, tiso, a_strat, p_strat
    INTEGER :: ids, ide, jds, jde, kds, kde, &
               ims, ime, jms, jme, kms, kme, &
               its, ite, jts, jte, kts, kte
    REAL, DIMENSION(max_eta) :: eta_levels
    IF ( use_baseparam_fr_nml ) then
      CALL nl_get_base_pres ( 1 , p00 )
      CALL nl_get_base_temp ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a )
      CALL nl_get_iso_temp ( 1 , tiso )
      CALL nl_get_base_lapse_strat ( 1 , a_strat )
      CALL nl_get_base_pres_strat ( 1 , p_strat )
      IF ((t00 .LT. 100.0) .OR. (p00 .LT. 10000.0)) THEN
        WRITE(wrf_err_message,*) '--- ERROR: bad base state for T00 or P00 in namelist.input file'
        CALL wrf_error_fatal3("<stdin>",233,&
TRIM(wrf_err_message))
      END IF
    ELSE
      p00 = nest%p00
      t00 = nest%t00
      a = nest%tlp
      tiso = nest%tiso
      a_strat = nest%tlp_strat
      p_strat = nest%p_strat
      IF ((t00 .LT. 100.0) .OR. (p00 .LT. 10000.0)) THEN
        WRITE(wrf_err_message,*) '--- ERROR: did not find base state parameters in nest. Add use_baseparam_fr_nml = .true. in &dynamics and rerun'
        CALL wrf_error_fatal3("<stdin>",245,&
TRIM(wrf_err_message))
      ENDIF
    ENDIF
    kde_n = nest%e_vert
    IF (nest%id .NE. 1) THEN
      id = 1
      ks = 1
      DO WHILE (nest%id .GT. id)
        id = id+1
        ks = ks+model_config_rec%e_vert(id-1)
      ENDDO
    ENDIF
    IF ((nest%this_is_an_ideal_run) .AND. (model_config_rec%eta_levels(1) .EQ. -1.0)) THEN
      DO k=1,kde_n
        CALL wrf_debug(0, "nest_init_utils: eta_levels are not specified in the namelist, setting levels with constant spacing in eta.")
        nest%znw(k) = 1.0-(k-1)/FLOAT((kde_n-1))
      ENDDO
    ELSEIF (.NOT.(nest%this_is_an_ideal_run) .AND. (model_config_rec%eta_levels(1) .EQ. -1.0)) THEN
      write(*,'(A,I2,A)') "--- WARNING: eta_levels are not specified in the namelist for grid_id=",nest%grid_id,", using WRF's default levels."
      CALL get_ijk_from_grid( nest, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte )
      write(*,'(A,F10.3)') "--- USING: nest%p_top   = ",nest%p_top
      write(*,'(A,F10.3)') "--- USING: g            = ",g
      write(*,'(A,F10.3)') "--- USING: cvpm         = ",cvpm
      write(*,'(A,F10.3)') "--- USING: r_d          = ",r_d
      write(*,'(A,F10.3)') "--- USING: cp           = ",cp
      write(*,'(A,F10.3)') "--- USING: p1000mb      = ",p1000mb
      write(*,'(A,F10.3)') "--- USING: t0           = ",t0
      write(*,'(A,F10.3)') "--- USING: p00          = ",p00
      write(*,'(A,F10.3)') "--- USING: t00          = ",t00
      write(*,'(A,F10.3)') "--- USING: a            = ",a
      write(*,'(A,F10.3)') "--- USING: tiso         = ",tiso
      write(*,'(A,F10.3)') "--- USING: a_strat      = ",a_strat
      write(*,'(A,F10.3)') "--- USING: p_strat      = ",p_strat
      CALL compute_eta ( nest%znw, &
                         eta_levels, max_eta, max_dz, &
                         nest%p_top, g, p00, cvpm, a, r_d, cp, &
                         t00, p1000mb, t0, tiso, p_strat, a_strat, &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )
    ELSE
    DO k=1,kde_n
      nest%znw(k) = model_config_rec%eta_levels(ks+k-1)
        write(*,'(A,I3,A,F6.3)') "nest%znw(",k,") = ",nest%znw(k)
    ENDDO
    IF (nest%znw(1) .NE. 1.0) THEN
        write(wrf_err_message,'(A,I2,A)') "--- ERROR: first eta_level for grid_id=",nest%grid_id," is not 1.0. Check namelist."
        CALL wrf_error_fatal3("<stdin>",300,&
wrf_err_message )
    ENDIF
      write(*,'(A,F10.3)') "--- USING: g            = ",g
      write(*,'(A,F10.3)') "--- USING: cvpm         = ",cvpm
      write(*,'(A,F10.3)') "--- USING: r_d          = ",r_d
      write(*,'(A,F10.3)') "--- USING: cp           = ",cp
      write(*,'(A,F10.3)') "--- USING: p1000mb      = ",p1000mb
      write(*,'(A,F10.3)') "--- USING: t0           = ",t0
    IF (nest%znw(kde_n) .NE. 0.0) THEN
        write(wrf_err_message,'(A,I2,A)') "--- ERROR: last eta_level for grid_id=",nest%grid_id," is not 0.0. Check namelist."
        CALL wrf_error_fatal3("<stdin>",311,&
wrf_err_message )
    ENDIF
    DO k=2,kde_n
      IF (nest%znw(k) .GT. nest%znw(k-1)) THEN
          write(wrf_err_message,'(A,I2,A)') "--- ERROR: eta_level for grid_id=",nest%grid_id," are not monotonically decreasing. Check namelist."
          CALL wrf_error_fatal3("<stdin>",317,&
wrf_err_message )
      ENDIF
    ENDDO
    ENDIF
    DO k=1,kde_n-1
      nest%dnw(k) = nest%znw(k+1)-nest%znw(k)
      nest%rdnw(k) = 1./nest%dnw(k)
      nest%znu(k) = 0.5*(nest%znw(k+1)+nest%znw(k))
    ENDDO
    nest%znu(kde_n) = 0.0
    DO k=2,kde_n-1
      nest%dn(k) = 0.5*(nest%dnw(k)+nest%dnw(k-1))
      nest%rdn(k) = 1./nest%dn(k)
      nest%fnp(k) = .5* nest%dnw(k )/nest%dn(k)
      nest%fnm(k) = .5* nest%dnw(k-1)/nest%dn(k)
    ENDDO
    cof1 = (2.*nest%dn(2)+nest%dn(3))/(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(2)
    cof2 = nest%dn(2) /(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(3)
    nest%cf1 = nest%fnp(2) + cof1
    nest%cf2 = nest%fnm(2) - cof1 - cof2
    nest%cf3 = cof2
    nest%cfn = (.5*nest%dnw(kde_n-1)+nest%dn(kde_n-1))/nest%dn(kde_n-1)
    nest%cfn1 = -.5*nest%dnw(kde_n-1)/nest%dn(kde_n-1)
END SUBROUTINE vert_cor_vertical_nesting_arbitrary
SUBROUTINE compute_eta ( znw , &
                         eta_levels , max_eta , max_dz , &
                         p_top_def , g_def , p00_def , &
                         cvpm_def , a_def , r_d_def , cp_def , &
                         t00_def , p1000mb_def , t0_def , &
                         tiso_def , p_strat_def , a_strat_def , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      REAL , INTENT(IN) :: max_dz
      REAL , INTENT(IN) :: p_top_def , g_def , p00_def , cvpm_def , &
                                     a_def , r_d_def , cp_def , t00_def , &
                                     p1000mb_def , t0_def , tiso_def , &
                                     p_strat_def , a_strat_def
      INTEGER , INTENT(IN) :: max_eta
      REAL , DIMENSION (max_eta) :: eta_levels
      REAL , DIMENSION (kts:kte) , INTENT(OUT) :: znw
      INTEGER :: k , kk
      REAL(KIND=8) :: mub , t_init , p_surf , pb, ztop, ztop_pbl , dz , temp
      REAL(KIND=8) , DIMENSION(kts:kte) :: dnw
      REAL(KIND=8) :: p_top , g , p00 , cvpm , &
                      a , r_d , cp , t00 , &
                      p1000mb , t0 , tiso , &
                      p_strat , a_strat
      INTEGER , PARAMETER :: prac_levels = 59
      INTEGER :: loop , loop1
      REAL(KIND=8) , DIMENSION(prac_levels) :: znw_prac , znu_prac , dnw_prac
      REAL(KIND=8) , DIMENSION(MAX(prac_levels,kde)) :: alb , phb
      REAL(KIND=8) :: alb_max, t_init_max, pb_max, phb_max
      CHARACTER(LEN=256) :: message
      p_top = p_top_def
      g = g_def
      p00 = p00_def
      cvpm = cvpm_def
      a = a_def
      r_d = r_d_def
      cp = cp_def
      t00 = t00_def
      p1000mb = p1000mb_def
      t0 = t0_def
      tiso = tiso_def
      p_strat = p_strat_def
      a_strat = a_strat_def
      p_surf = p00
      znw_prac = (/ 1.0000_8 , 0.9930_8 , 0.9830_8 , 0.9700_8 , 0.9540_8 , 0.9340_8 , 0.9090_8 , 0.8800_8 , &
                    0.8500_8 , 0.8000_8 , 0.7500_8 , 0.7000_8 , 0.6500_8 , 0.6000_8 , 0.5500_8 , 0.5000_8 , &
                    0.4500_8 , 0.4000_8 , 0.3500_8 , 0.3000_8 , 0.2500_8 , 0.2000_8 , 0.1500_8 , 0.1000_8 , &
                    0.0800_8 , 0.0600_8 , 0.0400_8 , 0.0200_8 , &
                    0.0150_8 , 0.0100_8 , 0.0090_8 , 0.0080_8 , 0.0070_8 , 0.0060_8 , 0.0050_8 , 0.0040_8 , &
                    0.0035_8 , 0.0030_8 , &
                    0.0028_8 , 0.0026_8 , 0.0024_8 , 0.0022_8 , 0.0020_8 , &
                    0.0018_8 , 0.0016_8 , 0.0014_8 , 0.0012_8 , 0.0010_8 , &
                    0.0009_8 , 0.0008_8 , 0.0007_8 , 0.0006_8 , 0.0005_8 , 0.0004_8 , 0.0003_8 , &
                    0.0002_8 , 0.0001_8 , 0.00005_8, 0.0000_8 /)
      DO k = 1 , prac_levels - 1
        znu_prac(k) = ( znw_prac(k) + znw_prac(k+1) ) * 0.5_8
        dnw_prac(k) = znw_prac(k+1) - znw_prac(k)
      END DO
      DO k = 1, prac_levels-1
        pb = znu_prac(k)*(p_surf - p_top) + p_top
        temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
        IF ( pb .LT. p_strat ) THEN
          temp = tiso + A_strat*LOG(pb/p_strat)
        END IF
        t_init = temp*(p00/pb)**(r_d/cp) - t0
        alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
      END DO
      mub = p_surf - p_top
      phb(1) = 0._8
      DO k = 2,prac_levels
        phb(k) = phb(k-1) - dnw_prac(k-1)*mub*alb(k-1)
      END DO
      ztop = phb(prac_levels) / g
      ztop_pbl = phb(8 ) / g
      dz = ( ztop - ztop_pbl ) / REAL ( kde - 8 )
      IF ( dz .GE. max_dz ) THEN
        WRITE (message,FMT='("With a requested ",F7.1," Pa model top, the model lid will be about ",F7.1," m.")') p_top, ztop
        CALL wrf_message ( message )
        WRITE (message,FMT='("With ",I3," levels above the PBL, the level thickness will be about ",F6.1," m.")') kde-8, dz
        CALL wrf_message ( message )
        WRITE (message,FMT='("Thicknesses greater than ",F7.1," m are not recommended.")') max_dz
        CALL wrf_message ( message )
        CALL wrf_error_fatal3("<stdin>",443,&
'Add more levels to namelist.input for e_vert' )
      END IF
      DO k = 1 , 8
        eta_levels(k) = znw_prac(k)
      END DO
      DO k = 8, kte-1-2
        find_prac : DO kk = 1 , prac_levels
          IF (znw_prac(kk) .LT. eta_levels(k) ) THEN
            EXIT find_prac
          END IF
        end do find_prac
        pb = 0.5*(eta_levels(k)+znw_prac(kk)) * (p_surf - p_top) + p_top
        temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
        IF ( pb .LT. p_strat ) THEN
          temp = tiso + A_strat * LOG ( pb/p_strat )
        END IF
        t_init = temp*(p00/pb)**(r_d/cp) - t0
        alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
        eta_levels(k+1) = eta_levels(k) - dz*g / ( mub*alb(k) )
        pb = 0.5*(eta_levels(k)+eta_levels(k+1)) * (p_surf - p_top) + p_top
        temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
        IF ( pb .LT. p_strat ) THEN
          temp = tiso + A_strat * LOG ( pb/p_strat )
        END IF
        t_init = temp*(p00/pb)**(r_d/cp) - t0
        alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
        eta_levels(k+1) = eta_levels(k) - dz*g / ( mub*alb(k) )
        pb = 0.5*(eta_levels(k)+eta_levels(k+1)) * (p_surf - p_top) + p_top
        phb(k+1) = phb(k) - (eta_levels(k+1)-eta_levels(k)) * mub*alb(k)
      END DO
      alb_max = alb(kte-1-2)
      t_init_max = t_init
      pb_max = pb
      phb_max = phb(kte-1)
      DO k = 1 , kte-1-2
        znw(k) = eta_levels(k)
      END DO
      znw(kte-2) = 0.000
      DO loop1 = 1 , 5
        DO loop = 1 , 10
          DO k = 8, kte-1-2-1
            pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
            IF ( pb .LT. p_strat ) THEN
              temp = tiso + A_strat * LOG ( pb/p_strat )
            END IF
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
          END DO
          pb = pb_max
          t_init = t_init_max
          alb(kte-1-2) = alb_max
          znw(kte-2) = znw(kte-1-2) - dz*g / ( mub*alb(kte-1-2) )
          IF ( ( loop1 .EQ. 5 ) .AND. ( loop .EQ. 10 ) ) THEN
            print *,'Converged znw(kte) should be about 0.0 = ',znw(kte-2)
          END IF
          znw(kte-2) = 0.000
        END DO
        DO k = 1, kde-1-2
          pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
          temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
          IF ( pb .LT. p_strat ) THEN
            temp = tiso + A_strat * LOG ( pb/p_strat )
          END IF
          t_init = temp*(p00/pb)**(r_d/cp) - t0
          alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
        END DO
        phb(1) = 0.
        DO k = 2,kde-2
          phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
        END DO
        ztop = phb(kde-2)/g
        ztop_pbl = phb(8)/g
        dz = ( ztop - ztop_pbl ) / REAL ( (kde-2) - 8 )
      END DO
      IF ( dz .GT. max_dz ) THEN
        print *,'z (m)            = ',phb(1)/g
        do k = 2 ,kte-2
          print *,'z (m) and dz (m) = ',phb(k)/g,(phb(k)-phb(k-1))/g
        end do
        print *,'dz (m) above fixed eta levels = ',dz
        print *,'namelist max_dz (m) = ',max_dz
        print *,'namelist p_top (Pa) = ',p_top
        CALL wrf_debug ( 0, 'You need one of three things:' )
        CALL wrf_debug ( 0, '1) More eta levels to reduce the dz: e_vert' )
        CALL wrf_debug ( 0, '2) A lower p_top so your total height is reduced: p_top_requested')
        CALL wrf_debug ( 0, '3) Increase the maximum allowable eta thickness: max_dz')
        CALL wrf_debug ( 0, 'All are namelist options')
        CALL wrf_error_fatal3("<stdin>",546,&
'dz above fixed eta levels is too large')
      END IF
      DO k = kte-2 , 9 , -1
        znw(k+2) = znw(k)
      END DO
      znw( 9) = 0.75 * znw( 8) + 0.25 * znw(12)
      znw(10) = 0.50 * znw( 8) + 0.50 * znw(12)
      znw(11) = 0.25 * znw( 8) + 0.75 * znw(12)
      DO k = 8, kte-1
        pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
        temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
        IF ( pb .LT. p_strat ) THEN
          temp = tiso + A_strat * LOG ( pb/p_strat )
        END IF
        t_init = temp*(p00/pb)**(r_d/cp) - t0
        alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
        phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
      END DO
      phb(kte) = phb(kte-1) - (znw(kte)-znw(kte-1)) * mub*alb(kte-1)
      k=1
      WRITE (*,FMT='("Full level index = ",I4,"     Height = ",F7.1," m")') k,phb(1)/g
      do k = 2 ,kte
        WRITE (*,FMT='("Full level index = ",I4,"     Height = ",F7.1," m      Thickness = ",F6.1," m")') k,phb(k)/g,(phb(k)-phb(k-1))/g
      end do
END SUBROUTINE compute_eta
SUBROUTINE blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
   USE module_configure
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: ter_input
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: ter_temp
   INTEGER :: i , j , k , spec_bdy_width
   REAL :: r_blend_zones
   INTEGER blend_cell, blend_width
   CALL nl_get_spec_bdy_width ( 1, spec_bdy_width)
   CALL nl_get_blend_width ( 1, blend_width)
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_temp(i,k,j) = ter_input(i,k,j)
         END DO
      END DO
   END DO
   r_blend_zones = 1./(blend_width+1)
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            DO blend_cell = blend_width,1,-1
               IF ( ( i .EQ. spec_bdy_width + blend_cell ) .OR. ( j .EQ. spec_bdy_width + blend_cell ) .OR. &
                      ( i .EQ. ide - spec_bdy_width - blend_cell ) .OR. ( j .EQ. jde - spec_bdy_width - blend_cell ) ) THEN
                  ter_temp(i,k,j) = ( (blend_cell)*ter_input(i,k,j) + (blend_width+1-blend_cell)*ter_interpolated(i,k,j) ) &
                                    * r_blend_zones
               END IF
            ENDDO
            IF ( ( i .LE. spec_bdy_width ) .OR. ( j .LE. spec_bdy_width ) .OR. &
                      ( i .GE. ide - spec_bdy_width ) .OR. ( j .GE. jde - spec_bdy_width ) ) THEN
               ter_temp(i,k,j) = ter_interpolated(i,k,j)
            END IF
         END DO
      END DO
   END DO
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_input(i,k,j) = ter_temp(i,k,j)
         END DO
      END DO
   END DO
END SUBROUTINE blend_terrain
SUBROUTINE copy_3d_field ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT) :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: ter_input
   INTEGER :: i , j , k
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_interpolated(i,k,j) = ter_input(i,k,j)
         END DO
      END DO
   END DO
END SUBROUTINE copy_3d_field
SUBROUTINE adjust_tempqv ( mub, save_mub, c3, c4, znw, p_top, &
                           th, pp, qv, &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
   USE module_model_constants
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mub, save_mub
   REAL , DIMENSION(kms:kme) , INTENT(IN) :: znw
   REAL , DIMENSION(kms:kme) , INTENT(IN) :: c3, c4
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: th, pp, qv
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: p_old, p_new, rh
   REAL :: es,dth,tc,e,dth1
   INTEGER :: i , j , k
   real p_top
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_old(i,k,j) = c4(k) + c3(k)*save_mub(i,j) + p_top + pp(i,k,j)
            tc = (th(i,k,j)+300.)*(p_old(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = qv(i,k,j)*p_old(i,k,j)/(0.622+qv(i,k,j))
            rh(i,k,j) = e/es
         END DO
      END DO
   END DO
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_new(i,k,j) = c4(k) + c3(k)*mub(i,j) + p_top + pp(i,k,j)
            dth1 = -191.86e-3*(th(i,k,j)+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            dth = -191.86e-3*(th(i,k,j)+0.5*dth1+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            th(i,k,j) = th(i,k,j)+dth
            tc = (th(i,k,j)+300.)*(p_new(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = rh(i,k,j)*es
            qv(i,k,j) = 0.622*e/(p_new(i,k,j)-e)
         END DO
      END DO
   END DO
END SUBROUTINE adjust_tempqv
SUBROUTINE input_terrain_rsmas ( grid , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE ( domain ) :: grid
   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER :: i , j , k , myproc
   INTEGER, DIMENSION(256) :: ipath
   CHARACTER*256 :: message, message2
   CHARACTER*256 :: rsmas_data_path
   REAL , DIMENSION(ids:ide,jds:jde) :: ht_g, xlat_g, xlon_g
   CALL wrf_get_myproc ( myproc )
   CALL nl_get_rsmas_data_path(1,rsmas_data_path)
   do i = 1, LEN(TRIM(rsmas_data_path))
      ipath(i) = ICHAR(rsmas_data_path(i:i))
   enddo
   CALL wrf_patch_to_global_real ( grid%xlat , xlat_g , grid%domdesc, ' ' , 'xy' , &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime , jms , jme , 1 , 1 , &
                                   ips, ipe , jps , jpe , 1 , 1 )
   CALL wrf_patch_to_global_real ( grid%xlong , xlon_g , grid%domdesc, ' ' , 'xy' , &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime , jms , jme , 1 , 1 , &
                                   ips, ipe , jps , jpe , 1 , 1 )
   IF ( wrf_dm_on_monitor() ) THEN
     CALL get_terrain ( grid%dx/1000., xlat_g(ids:ide,jds:jde), xlon_g(ids:ide,jds:jde), ht_g(ids:ide,jds:jde), &
                        ide-ids+1,jde-jds+1,ide-ids+1,jde-jds+1, ipath, LEN(TRIM(rsmas_data_path)) )
     WHERE ( ht_g(ids:ide,jds:jde) < -1000. ) ht_g(ids:ide,jds:jde) = 0.
   ENDIF
   CALL wrf_global_to_patch_real ( ht_g , grid%ht , grid%domdesc, ' ' , 'xy' , &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime , jms , jme , 1 , 1 , &
                                   ips, ipe , jps , jpe , 1 , 1 )
END SUBROUTINE input_terrain_rsmas
SUBROUTINE update_after_feedback_em ( grid &
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &
                 )
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask
   USE module_comm_dm, ONLY : HALO_EM_FEEDBACK_sub
   USE module_bc
   USE module_state_description
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
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij) :: nba_mij
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij) :: nba_rij
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem) :: chem
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer) :: tracer
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_bye
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btxs
real ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btxe
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btys
real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer) :: tracer_btye
   INTEGER :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
  CALL wrf_debug( 500, "entering update_after_feedback_em" )
  CALL get_ijk_from_grid ( grid , &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe )
  CALL wrf_debug( 500, "before HALO_EM_FEEDBACK.inc in update_after_feedback_em" )
CALL HALO_EM_FEEDBACK_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
  CALL wrf_debug( 500, "leaving update_after_feedback_em" )
END SUBROUTINE update_after_feedback_em
