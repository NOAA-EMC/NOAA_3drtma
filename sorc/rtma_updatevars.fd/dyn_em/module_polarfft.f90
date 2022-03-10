MODULE module_polarfft
  USE module_model_constants
  USE module_wrf_error
  CHARACTER (LEN=256) , PRIVATE :: a_message
CONTAINS
SUBROUTINE couple_scalars_for_filter ( field &
                 ,mu,mub,c1,c2 &
                 ,ids,ide,jds,jde,kds,kde &
                 ,ims,ime,jms,jme,kms,kme &
                 ,ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde &
                         ,ims,ime,jms,jme,kms,kme &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
   REAL , DIMENSION(kms:kme) , INTENT(IN) :: c1,c2
   INTEGER :: i , j , k
   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
   END DO
   END DO
   END DO
END SUBROUTINE couple_scalars_for_filter
SUBROUTINE uncouple_scalars_for_filter ( field &
                 ,mu,mub,c1,c2 &
                 ,ids,ide,jds,jde,kds,kde &
                 ,ims,ime,jms,jme,kms,kme &
                 ,ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde &
                         ,ims,ime,jms,jme,kms,kme &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
   REAL , DIMENSION(kms:kme) , INTENT(IN) :: c1,c2
   INTEGER :: i , j , k
   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)/((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
   END DO
   END DO
   END DO
END SUBROUTINE uncouple_scalars_for_filter
SUBROUTINE pxft ( grid &
                 ,lineno &
                 ,flag_uv,flag_rurv &
                 ,flag_wph,flag_ww &
                 ,flag_t &
                 ,flag_mu,flag_mut &
                 ,flag_moist &
                 ,flag_chem &
                 ,flag_tracer &
                 ,flag_scalar &
                 ,fft_filter_lat, dclat &
                 ,actual_distance_average &
                 ,pos_def &
                 ,swap_pole_with_next_j &
                 ,moist,chem,tracer,scalar &
                 ,ids,ide,jds,jde,kds,kde &
                 ,ims,ime,jms,jme,kms,kme &
                 ,ips,ipe,jps,jpe,kps,kpe &
                 ,imsx,imex,jmsx,jmex,kmsx,kmex &
                 ,ipsx,ipex,jpsx,jpex,kpsx,kpex )
   USE module_state_description
   USE module_domain, ONLY : domain
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y &
                       , local_communicator_periodic, itrace &
                       , local_communicator_x
   USE module_driver_constants
   IMPLICIT NONE
   TYPE(domain) , TARGET :: grid
integer, intent(in) :: lineno
integer myproc, i, j, k
   LOGICAL, INTENT(IN) :: actual_distance_average
   LOGICAL, INTENT(IN) :: pos_def
   LOGICAL, INTENT(IN) :: swap_pole_with_next_j
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde &
                         ,ims,ime,jms,jme,kms,kme &
                         ,ips,ipe,jps,jpe,kps,kpe &
                         ,imsx,imex,jmsx,jmex,kmsx,kmex &
                         ,ipsx,ipex,jpsx,jpex,kpsx,kpex
   REAL , INTENT(IN) :: fft_filter_lat
   REAL, INTENT(IN) :: dclat
   INTEGER, INTENT(IN) :: flag_uv &
                         ,flag_rurv &
                         ,flag_ww &
                         ,flag_t,flag_wph &
                         ,flag_mu,flag_mut &
                         ,flag_moist &
                         ,flag_chem &
                         ,flag_tracer &
                         ,flag_scalar
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,*) , INTENT(INOUT) :: moist, chem, scalar,tracer
   LOGICAL piggyback_mu, piggyback_mut
   INTEGER ij, k_end
   piggyback_mu = flag_mu .EQ. 1
   piggyback_mut = flag_mut .EQ. 1
call wrf_get_myproc(myproc)
   IF ( flag_uv .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%u_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe)
     ENDIF
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%v_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%v_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%v_xxx, grid%clat_xxx, .false., &
                                fft_filter_lat, dclat, &
                                ids, ide, jds, jde, kds, kde-1, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex ) )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%v_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%v_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%u_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%u_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)
     CALL polar_filter_3d( grid%u_xxx, grid%clat_xxx, piggyback_mu, &
                                fft_filter_lat, 0., &
                                ids, ide, jds, jde, kds, kde, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%u_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%u_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%u_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF
   IF ( flag_t .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%t_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe)
     ENDIF
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%t_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%t_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)
     CALL polar_filter_3d( grid%t_xxx, grid%clat_xxx,piggyback_mu, &
                                fft_filter_lat, 0., &
                                ids, ide, jds, jde, kds, kde-1, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )
     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%t_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , &
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%t_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%t_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%t_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF
   IF ( flag_wph .EQ. 1 ) THEN
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%w_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%w_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
      CALL polar_filter_3d( grid%w_xxx, grid%clat_xxx, .false., &
                                 fft_filter_lat, 0., &
                                 ids, ide, jds, jde, kds, kde, &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%w_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%w_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ph_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ph_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
      CALL polar_filter_3d( grid%ph_xxx, grid%clat_xxx, .false., &
                                 fft_filter_lat, 0., &
                                 ids, ide, jds, jde, kds, kde, &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ph_2, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ph_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_ww .EQ. 1 ) THEN
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ww_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ww_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
      CALL polar_filter_3d( grid%ww_xxx, grid%clat_xxx, .false., &
                                 fft_filter_lat, 0., &
                                 ids, ide, jds, jde, kds, kde, &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ww_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ww_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_rurv .EQ. 1 ) THEN
     IF ( piggyback_mut ) THEN
       grid%ru_m(ips:ipe,kde,jps:jpe) = grid%mut(ips:ipe,jps:jpe)
     ENDIF
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%rv_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%rv_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%rv_xxx, grid%clat_xxx, .false., &
                                fft_filter_lat, dclat, &
                                ids, ide, jds, jde, kds, kde, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%rv_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%rv_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ru_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ru_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mut ) k_end = MIN(kde,kpex)
     CALL polar_filter_3d( grid%ru_xxx, grid%clat_xxx, piggyback_mut, &
                                fft_filter_lat, 0., &
                                ids, ide, jds, jde, kds, kde, &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ru_m, &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ru_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     IF ( piggyback_mut ) THEN
       grid%mut(ips:ipe,jps:jpe) = grid%ru_m(ips:ipe,kde,jps:jpe)
       piggyback_mut = .FALSE.
     ENDIF
   ENDIF
   IF ( flag_moist .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_moist
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   moist(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. , &
                           fft_filter_lat, 0., &
                           ids, ide, jds, jde, kds, kde, &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )
     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , &
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   moist(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_chem .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_chem
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   chem(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. , &
                           fft_filter_lat, 0., &
                           ids, ide, jds, jde, kds, kde, &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )
     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , &
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   chem(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_tracer .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_tracer
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   tracer(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. , &
                           fft_filter_lat, 0., &
                           ids, ide, jds, jde, kds, kde, &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )
     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , &
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   tracer(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_scalar .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_scalar
  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   scalar(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
     CALL polar_filter_3d( grid%fourd_xxx , grid%clat_xxx, .false. , &
                           fft_filter_lat, 0., &
                           ids, ide, jds, jde, kds, kde, &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )
     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , &
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   scalar(grid%sm31,grid%sm32,grid%sm33,itrace ), &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
   ENDIF
   IF ( flag_mu .EQ. 1 .AND. piggyback_mu ) THEN
      CALL wrf_error_fatal3("<stdin>",667,&
"mu needed to get piggybacked on a transpose and did not")
   ENDIF
   IF ( flag_mut .EQ. 1 .AND. piggyback_mut ) THEN
      CALL wrf_error_fatal3("<stdin>",671,&
"mut needed to get piggybacked on a transpose and did not")
   ENDIF
   RETURN
END SUBROUTINE pxft
SUBROUTINE polar_filter_3d( f, xlat, piggyback, fft_filter_lat, dvlat, &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )
  IMPLICIT NONE
  INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
  REAL , INTENT(IN ) :: fft_filter_lat
  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: f
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: xlat
  REAL , INTENT(IN) :: dvlat
  LOGICAL , INTENT(IN) :: piggyback
  REAL , DIMENSION(1:ide-ids,1:kte-kts+1) :: sheet
  REAL , DIMENSION(1:kte-kts+1) :: sheet_total
  REAL :: lat, avg, rnboxw
  INTEGER :: ig, jg, i, j, j_end, nx, ny, nmax, kw
  INTEGER :: k, nboxw, nbox2, istart, iend, overlap
  INTEGER, DIMENSION(6) :: wavenumber = (/ 1, 3, 7, 10, 13, 16 /)
  INTEGER :: fftflag
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_polarfft: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal3("<stdin>",703,&
TRIM( wrf_err_message ) )
  END IF
  fftflag= 1
  nx = ide-ids
  ny = kte-kts+1
  lat = 0.
  j_end = MIN(jte, jde-1)
  IF (dvlat /= 0. .and. j_end == jde-1) j_end = jde
  DO j = jts, j_end
     jg = j-jds+1
     if(xlat(ids,j).gt.0.) then
        lat = xlat(ids,j)-dvlat
     else
        lat = xlat(ids,j)+dvlat
     endif
     IF (abs(lat) >= fft_filter_lat) THEN
        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
        END DO
        call polar_filter_fft_2d_ncar(nx,ny,sheet,lat,fft_filter_lat,piggyback,fftflag)
        DO k=kts,kte
           DO i=ids,ide-1
              f(i,k,j) = sheet(i-ids+1,k-kts+1)
           END DO
           DO i=1,ids-ims
              f(ids-i,k,j)=f(ide-i,k,j)
           END DO
           DO i=1,ime-ide+1
              f(ide+i-1,k,j)=f(ids+i-1,k,j)
           END DO
        END DO
     END IF
  END DO
END SUBROUTINE polar_filter_3d
SUBROUTINE polar_filter_fft_2d_ncar(nx,ny,fin,lat,filter_latitude,piggyback,fftflag)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin
  REAL , INTENT(IN) :: lat, filter_latitude
  LOGICAL, INTENT(IN) :: piggyback
  REAL :: pi, rcosref, freq, c, cf
  INTEGER :: k, fftflag
  REAL, DIMENSION(NX,NY) :: work
  REAL, DIMENSION(NX+15) :: wsave
  REAL(KIND=8), DIMENSION(NX,NY) :: fin8, work8
  REAL(KIND=8), DIMENSION(NX+15) :: wsave8
  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp
  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk
  REAL, PARAMETER :: alpha = 0.0
  REAL :: factor_k
  INTEGER :: ntop
  pi = ACOS(-1.)
  rcosref = 1./COS(filter_latitude*pi/180.)
  n = nx
  lot = ny
  lensav = n+15
  inc = 1
  lenr = nx*ny
  jump = nx
  lenwrk = lenr
  ntop = ny
  IF(piggyback) ntop = ny-1
  if(fftflag.eq.0) then
     call rfftmi(n,wsave,lensav,ier)
  else
     call dfft1i(n,wsave8,lensav,ier)
  endif
  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmi ',ier
    CALL wrf_message ( a_message )
  END IF
  if(fftflag.eq.0) then
     call rfftmf(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else
     fin8 = fin
     do k=1,ny
        call dfft1f(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo
  endif
  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmf ',ier
    CALL wrf_message ( a_message )
  END IF
  if(MOD(n,2) == 0) then
    nh = n/2 - 1
  else
    nh = (n-1)/2
  end if
  DO j=1,ny
   fp(1,j) = 1.
  ENDDO
  DO i=2,nh+1
    freq=REAL(i-1)/REAL(n)
    c = (rcosref*COS(lat*pi/180.)/SIN(freq*pi))**2
    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(2*(i-1),j) = cf
      fp(2*(i-1)+1,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(2*(i-1),ny) = cf
      fp(2*(i-1)+1,ny) = cf
    endif
  END DO
  IF(MOD(n,2) == 0) THEN
    c = (rcosref*COS(lat*pi/180.))**2
    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(n,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(n,ny) = cf
    endif
  END IF
  if(fftflag.eq.0) then
     do j=1,ny
       do i=1,nx
          fin(i,j) = fp(i,j)*fin(i,j)
       enddo
     enddo
  else
     do j=1,ny
       do i=1,nx
          fin8(i,j) = fp(i,j)*fin8(i,j)
       enddo
     enddo
  endif
  if(fftflag.eq.0) then
     call rfftmb(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else
     do k=1,ny
        call dfft1b(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo
     fin= fin8
  endif
  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmb ',ier
    CALL wrf_message ( a_message )
  END IF
END SUBROUTINE polar_filter_fft_2d_ncar
   SUBROUTINE filter_tracer ( tr3d_in , xlat , msftx , &
                              fft_filter_lat , mf_fft , &
                              pos_def , swap_pole_with_next_j , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      REAL , INTENT(IN) :: fft_filter_lat , mf_fft
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: tr3d_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx
      LOGICAL , INTENT(IN) :: pos_def , swap_pole_with_next_j
      INTEGER :: i , j , j_lat_pos , j_lat_neg , k
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      INTEGER :: i_left , i_right , ii , count
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: tr_in, tr_out
      CHARACTER (LEN=256) :: message
      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal3("<stdin>",894,&
'filtering assumes all values on X' )
      END IF
      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = MIN(jde-1,jte) , jts , -1
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .GE. fft_filter_lat ) THEN
               j_lat_neg = j
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg
      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos
      DO k = kts, kte
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr_in(i,j) = tr3d_in(i,k,j)
               tr_out(i,j) = tr_in(i,j)
            END DO
         END DO
         DO j = MIN(j_lat_neg,jte) , jts , -1
            i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
            DO i = its , MIN(ide-1,ite)
               sum = 0.
               count = 0
               DO ik = 1 , i_kicker/2
                  ii = i-ik
                  IF ( ii .GE. ids ) THEN
                     i_left = ii
                  ELSE
                     i_left = ( ii - ids ) + (ide-1)+1
                  END IF
                  ii = i+ik
                  IF ( ii .LE. ide-1 ) THEN
                     i_right = ii
                  ELSE
                     i_right = ( ii - (ide-1) ) + its-1
                  END IF
                  sum = sum + tr_in(i_left,j) + tr_in(i_right,j)
                  count = count + 1
               END DO
               tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * count + 1 )
            END DO
         END DO
         DO j = MAX(j_lat_pos,jts) , MIN(jde-1,jte)
            i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
            DO i = its , MIN(ide-1,ite)
               count = 0
               sum = 0.
               DO ik = 1 , i_kicker/2
                  ii = i-ik
                  IF ( ii .GE. ids ) THEN
                     i_left = ii
                  ELSE
                     i_left = ( ii - ids ) + (ide-1)+1
                  END IF
                  ii = i+ik
                  IF ( ii .LE. ide-1 ) THEN
                     i_right = ii
                  ELSE
                     i_right = ( ii - (ide-1) ) + its-1
                  END IF
                  sum = sum + tr_in(i_left,j) + tr_in(i_right,j)
                  count = count + 1
               END DO
               tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * count + 1 )
            END DO
         END DO
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr3d_in(i,k,j) = tr_out(i,j)
            END DO
         END DO
         IF ( pos_def ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,j) = MAX( tr3d_in(i,k,j) , 0. )
               END DO
            END DO
         END IF
         IF ( swap_pole_with_next_j ) THEN
            IF ( jts .EQ. jds ) THEN
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,jts) = tr3d_in(i,k,jts+1)
               END DO
            END IF
            IF ( jte .EQ. jde ) THEN
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,jte-1) = tr3d_in(i,k,jte-2)
               END DO
            END IF
         END IF
      END DO
   END SUBROUTINE filter_tracer
   SUBROUTINE filter_tracer_old ( tr3d_in , xlat , msftx , fft_filter_lat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      REAL , INTENT(IN) :: fft_filter_lat
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: tr3d_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx
      INTEGER :: i , j , j_lat_pos , j_lat_neg , k
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: tr_in, tr_out
      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal3("<stdin>",1031,&
'filtering assumes all values on X' )
      END IF
      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .LT. fft_filter_lat ) THEN
               j_lat_neg = j - 1
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg
      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos
      DO k = kts, kte
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr_in(i,j) = tr3d_in(i,k,j)
               tr_out(i,j) = tr_in(i,j)
            END DO
         END DO
         DO j = j_lat_neg , jts , -1
            i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
            DO i = its , MIN(ide-1,ite)
               IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j) + tr_in(i-ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j)
                  END DO
                  i1 = i - i_kicker + ide -1
                  i2 = ide-1
                  i3 = ids
                  i4 = i-1
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i-ik,j)
                  END DO
                  i1 = i+1
                  i2 = ide-1
                  i3 = ids
                  i4 = ids + ( i_kicker+i ) - ide
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               END IF
            END DO
         END DO
         DO j = j_lat_pos , MIN(jde-1,jte)
            i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
            DO i = its , MIN(ide-1,ite)
               IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j) + tr_in(i-ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j)
                  END DO
                  i1 = i - i_kicker + ide -1
                  i2 = ide-1
                  i3 = ids
                  i4 = i-1
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i-ik,j)
                  END DO
                  i1 = i+1
                  i2 = ide-1
                  i3 = ids
                  i4 = ids + ( i_kicker+i ) - ide
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               END IF
            END DO
         END DO
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr3d_in(i,k,j) = tr_out(i,j)
            END DO
         END DO
      END DO
   END SUBROUTINE filter_tracer_old
END MODULE module_polarfft
