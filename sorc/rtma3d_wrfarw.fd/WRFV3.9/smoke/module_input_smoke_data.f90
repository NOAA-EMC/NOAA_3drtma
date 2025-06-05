MODULE module_input_smoke_data
    USE module_io_domain
    USE module_domain
    USE module_get_file_names, ONLY : eligible_file_name, number_of_eligible_files, unix_ls
   IMPLICIT NONE
    REAL, PARAMETER :: conmin=1.E-16, epsilc=1.E-16
    TYPE(WRFU_Time), DIMENSION(max_domains) :: last_chem_time
    INTEGER :: k_loop
    INTEGER :: lo
    INTEGER :: logg
    INTEGER :: kx
    INTEGER :: kxm1
    PARAMETER( kx=16, kxm1=kx-1, logg=350, lo=34)
    INTEGER, DIMENSION(logg) :: iref
    REAL, DIMENSION(logg) :: fracref
    REAL, DIMENSION(kx) :: dens
    REAL, DIMENSION(kx+1) :: zfa
    REAL, DIMENSION(kx+1) :: zfa_bdy
    REAL, DIMENSION(lo,kx) :: xl
CONTAINS
SUBROUTINE vinterp_chem(nx1, nx2, ny1, ny2, nz1, nz_in, nz_out, nch, z_in, z_out, &
                 data_in, data_out, extrapolate)
    INTEGER, INTENT(IN) :: nx1, nx2
    INTEGER, INTENT(IN) :: ny1, ny2
    INTEGER, INTENT(IN) :: nz1
    INTEGER, INTENT(IN) :: nz_in
    INTEGER, INTENT(IN) :: nz_out
    INTEGER, INTENT(IN) :: nch
    REAL, INTENT(IN) :: z_in (nx1:nx2,nz1:nz_in ,ny1:ny2)
    REAL, INTENT(IN) :: z_out(nx1:nx2,nz1:nz_out,ny1:ny2)
    REAL, INTENT(IN) :: data_in (nx1:nx2,nz1:nz_in ,ny1:ny2,nch)
    REAL, INTENT(OUT) :: data_out(nx1:nx2,nz1:nz_out,ny1:ny2,nch)
    LOGICAL, INTENT(IN) :: extrapolate
    INTEGER :: i,j,l
    INTEGER :: k,kk
    REAL :: desired_z
    REAL :: dvaldz
    REAL :: wgt0
    chem_loop: DO l = 2, nch
      data_out(:,:,:,l) = -99999.9
      DO j = ny1, ny2
        DO i = nx1, nx2
          output_loop: DO k = nz1, nz_out
            desired_z = z_out(i,k,j)
            IF (desired_z .LT. z_in(i,1,j)) THEN
              IF ((desired_z - z_in(i,1,j)).LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,1,j,l)
              ELSE
                IF (extrapolate) THEN
                  IF ( (z_in(i,1,j) - z_in(i,2,j)) .GT. 0.001) THEN
                    dvaldz = (data_in(i,1,j,l) - data_in(i,2,j,l)) / &
                              (z_in(i,1,j) - z_in(i,2,j) )
                  ELSE
                    dvaldz = (data_in(i,1,j,l) - data_in(i,3,j,l)) / &
                              (z_in(i,1,j) - z_in(i,3,j) )
                  ENDIF
                  data_out(i,k,j,l) = MAX( data_in(i,1,j,l) + &
                                dvaldz * (desired_z-z_in(i,1,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,1,j,l)
                ENDIF
              ENDIF
            ELSE IF (desired_z .GT. z_in(i,nz_in,j)) THEN
              IF ( (z_in(i,nz_in,j) - desired_z) .LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,nz_in,j,l)
              ELSE
                IF (extrapolate) THEN
                  IF ( (z_in(i,nz_in-1,j)-z_in(i,nz_in,j)) .GT. 0.0005) THEN
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-1,j,l)) / &
                               (z_in(i,nz_in,j) - z_in(i,nz_in-1,j))
                  ELSE
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-2,j,l)) / &
                               (z_in(i,nz_in,j) - z_in(i,nz_in-2,j))
                  ENDIF
                  data_out(i,k,j,l) = MAX( data_in(i,nz_in,j,l) + &
                           dvaldz * (desired_z-z_in(i,nz_in,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,nz_in,j,l)
                ENDIF
              ENDIF
            ELSE
              input_loop: DO kk = 1, nz_in-1
                IF (desired_z .EQ. z_in(i,kk,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk,j,l)
                  EXIT input_loop
                ELSE IF (desired_z .EQ. z_in(i,kk+1,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk+1,j,l)
                  EXIT input_loop
                ELSE IF ( (desired_z .GT. z_in(i,kk,j)) .AND. &
                          (desired_z .LT. z_in(i,kk+1,j)) ) THEN
                  wgt0 = (desired_z - z_in(i,kk+1,j)) / &
                         (z_in(i,kk,j)-z_in(i,kk+1,j))
                  data_out(i,k,j,l) = MAX( wgt0*data_in(i,kk,j,l) + &
                                    (1.-wgt0)*data_in(i,kk+1,j,l), 0.)
                  EXIT input_loop
                ENDIF
              ENDDO input_loop
            ENDIF
          ENDDO output_loop
        ENDDO
      ENDDO
    ENDDO chem_loop
    RETURN
  END SUBROUTINE vinterp_chem
SUBROUTINE input_chem_profile (si_grid)
   IMPLICIT NONE
   TYPE(domain) :: si_grid
   INTEGER :: i , j , k, &
              ids, ide, jds, jde, kds, kde, &
              ims, ime, jms, jme, kms, kme, &
              ips, ipe, jps, jpe, kps, kpe
   INTEGER :: fid, ierr, numgas
   INTEGER :: debug_level
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: si_zsigf, si_zsig
   CHARACTER (LEN=80) :: inpname, message
   write(message,'(A)') 'Subroutine input_chem_profile: '
   CALL wrf_message ( TRIM(message) )
   CALL nl_get_debug_level ( 1,debug_level )
   CALL set_wrf_debug_level ( debug_level )
   CALL get_ijk_from_grid ( si_grid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe )
   IF ( si_grid%chem_opt == CHEM_TRACER ) THEN
     si_grid%chem(ims:ime,kms:kme,jms:jme,:) = 0.0
   ENDIF
    RETURN
  END SUBROUTINE input_chem_profile
  SUBROUTINE make_chem_profile ( nx1, nx2, ny1, ny2, nz1, nz2, nch, numgas, &
                                 chem_opt, zgrid, chem )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nx1, ny1, nz1
    INTEGER, INTENT(IN) :: nx2, ny2, nz2
    INTEGER, INTENT(IN) :: nch, numgas, chem_opt
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid
    CHARACTER (LEN=80) :: message
    INTEGER :: i, j, k, l, is
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2,lo+1):: chprof
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2) :: zprof
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,nch) :: chem
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,lo ) :: stor
    REAL :: hc358
    REAL :: olit
     if( nch .NE. num_chem) then
       message = ' Input_chem_profile: wrong number of chemical species'
     endif
      DO j=ny1,ny2
      DO k= 1,kx
      DO i=nx1,nx2
         chprof(i,k,j,2:lo+1) = xl(1:lo,kx-k+1)
         zprof(i,k,j) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
      ENDDO
      ENDDO
      do k=1,kx
         chprof(:,k,:,lo-5:lo+1) = chprof(:,k,:,lo-5:lo+1)/dens(k)
      end do
      call vinterp_chem(nx1, nx2, ny1, ny2, nz1, kx, nz2, lo, zprof, zgrid, &
                          chprof, chem, .false.)
      stor(nx1:nx2,nz1:nz2,ny1:ny2,1:lo) = chem(nx1:nx2,nz1:nz2,ny1:ny2,2:lo+1)
      chem(nx1:nx2,nz1:nz2,ny1:ny2,1) = -999.
      DO l=2,numgas
        is=iref(l-1)
        DO j=ny1,ny2
        DO k=nz1,nz2
        DO i=nx1,nx2
          chem(i,k,j,l)=fracref(l-1)*stor(i,k,j,is)*1.E6
        ENDDO
        ENDDO
        ENDDO
      ENDDO
      SELECT CASE(chem_opt)
      CASE (CBM4_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            olit = ( 0.9*fracref(numgas+4)*stor(i,k,j,iref(numgas+4)) &
                     +2.8*fracref(numgas+5)*stor(i,k,j,iref(numgas+5)) &
                     +1.8*fracref(numgas+6)*stor(i,k,j,iref(numgas+6)) &
                     +1.0*fracref(numgas+7)*stor(i,k,j,iref(numgas+7)) &
                    )*1.E6
            chem(i,k,j,p_par) = 0.4*chem(i,k,j,p_ald2) + hc358 + olit
         end do
         end do
         end do
      CASE (CB05_SORG_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) = &
                 0.4*chem(i,k,j,p_ald2) + hc358 &
                 +0.4*chem(i,k,j,p_aldx) + 2.8*chem(i,k,j,p_ole) &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do
      CASE (CB05_SORG_VBS_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) = &
                 0.4*chem(i,k,j,p_ald2) + hc358 &
                 +0.4*chem(i,k,j,p_aldx) + 2.8*chem(i,k,j,p_ole) &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do
      END SELECT
      RETURN
  END SUBROUTINE make_chem_profile
  SUBROUTINE bdy_chem_value_tracer ( chem, nch )
    IMPLICIT NONE
    REAL, intent(OUT) :: chem
    INTEGER, intent(IN) :: nch
    if( nch .ne. p_co ) then
       chem = 0.0001
    else if( nch == p_co ) then
       chem = 0.08
    else
       chem = conmin
    end if
    if( nch .eq. p_tracer_1 ) then
       chem = 0.08
    endif
  END SUBROUTINE bdy_chem_value_tracer
  SUBROUTINE bdy_chem_value ( chem, z, nch, numgas )
    IMPLICIT NONE
    REAL, intent(OUT) :: chem
    REAL, intent(IN) :: z
    INTEGER, intent(IN) :: nch
    INTEGER, intent(IN) :: numgas
    INTEGER :: i, k, irefcur
    REAL, DIMENSION(kx):: cprof
    REAL, DIMENSION(1:kx):: zprof
    REAL :: stor
    REAL :: wgt0
    CHARACTER (LEN=80) :: message
     if(p_co2.gt.1)then
     if (nch.eq.p_co2)then
       chem=370.
       return
     endif
     if (nch.eq.p_ch4)then
       chem=1.7
       return
     endif
     endif
      irefcur = iref(nch-1)
      DO k = 1,kx
        zprof(k) = 0.5*(zfa_bdy(k)+zfa_bdy(k+1))
        if (irefcur .lt. lo-6) then
          cprof(k) = xl(irefcur,kx+1-k)
        else
          cprof(k) = xl(irefcur,kx+1-k)/dens(kx+1-k)
        end if
      ENDDO
      IF (z .LT. zprof(1)) THEN
        stor = cprof(1)
      ELSE IF (z .GT. zprof(kx)) THEN
        stor = cprof(kx)
      ELSE
        input_loop: DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN
            stor = cprof(k)
            EXIT input_loop
          ELSE IF (z .EQ. zprof(k+1) )THEN
            stor = cprof(k+1)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor = MAX( wgt0 *cprof(k ) + &
                     (1.-wgt0)*cprof(k+1), 0.)
            EXIT input_loop
          ENDIF
        ENDDO input_loop
      ENDIF
      chem = fracref(nch-1)*stor*1.E6
      RETURN
  END SUBROUTINE bdy_chem_value
SUBROUTINE flow_dep_bdy_smoke (chem, &
                              chem_bxs,chem_btxs, &
                              chem_bxe,chem_btxe, &
                              chem_bys,chem_btys, &
                              chem_bye,chem_btye, &
                              dtbdy, &
                              u, v, config_flags, alt, &
                              ic, &
                              ids,ide, jds,jde, kds,kde, &
                              ims,ime, jms,jme, kms,kme, &
                              ips,ipe, jps,jpe, kps,kpe, &
                              its,ite, jts,jte, kts,kte )
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde
      INTEGER, INTENT(IN ) :: ims,ime, jms,jme, kms,kme
      INTEGER, INTENT(IN ) :: ips,ipe, jps,jpe, kps,kpe
      INTEGER, INTENT(IN ) :: its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN ) :: ic
      REAL, INTENT(IN ) :: dtbdy
      TYPE( grid_config_rec_type ) :: config_flags
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL, DIMENSION( jms:jme , kds:kde , config_flags%spec_bdy_width), INTENT(IN ) :: chem_bxs, chem_bxe, chem_btxs, chem_btxe
      REAL, DIMENSION( ims:ime , kds:kde , config_flags%spec_bdy_width), INTENT(IN ) :: chem_bys, chem_bye, chem_btys, chem_btye
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: alt
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: u
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: v
      INTEGER :: i, j, k, numgas
      INTEGER :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER :: i_inner, j_inner
      INTEGER :: b_dist
      integer :: itestbc, i_bdy_method, spec_zone, spec_bdy_width
      real :: chem_bv_def
      logical :: have_bcs_chem
      INTEGER, SAVE :: icall
      chem_bv_def = conmin
      numgas = 0
      itestbc=0
      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      have_bcs_chem = config_flags%have_bcs_chem
      spec_bdy_width= config_flags%spec_bdy_width
      spec_zone = config_flags%spec_zone
      i_bdy_method = 0
      if (have_bcs_chem) i_bdy_method =6
      if (ic .lt. param_first_scalar) i_bdy_method = 0
      IF (jts - jbs .lt. spec_zone) THEN
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
                 i_inner = max(i,ibs+spec_zone)
                 i_inner = min(i_inner,ibe-spec_zone)
                 IF(v(i,k,j) .lt. 0.)THEN
                    chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)
                 ELSE
                 if (i_bdy_method .eq. 6) then
                    CALL bdy_chem_value_rapsmoke( chem(i,k,j),chem_bys(i,k,1),chem_btys(i,k,1),alt(i,k,j),dtbdy )
                 else
                    chem(i,k,j) = chem_bv_def
                 endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jbe - jtf .lt. spec_zone) THEN
        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
                if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_rapsmoke ( chem(i,k,j),chem_bye(i,k,1),chem_btye(i,k,1),alt(i,k,j),dtbdy)
                else
                   chem(i,k,j) = chem_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (its - ibs .lt. spec_zone) THEN
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_rapsmoke (chem(i,k,j),chem_bxs(j,k,1),chem_btxs(j,k,1),alt(i,k,j),dtbdy)
                 else
                    chem(i,k,j) = chem_bv_def
                 endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (ibe - itf .lt. spec_zone) THEN
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_rapsmoke ( chem(i,k,j),chem_bxe(j,k,1),chem_btxe(j,k,1),alt(i,k,j),dtbdy)
                 else
                   chem(i,k,j) = chem_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (config_flags%debug_chem .AND. icall<2000) THEN
         WRITE(*,*) 'flow_dep_bdy_smoke: dtbdy=dt_rk+dtbc ',dtbdy
         WRITE(*,*) 'flow_dep_bdy_smoke: its,ite,jts,jte,kts,kte ',its,ite,jts,jte,kts,kte
         WRITE(*,*) 'flow_dep_bdy_smoke: alt(its,kts,jts),alt(ite,kte,jte) ',alt(its,kts,jts),alt(ite,kte,jte)
         WRITE(*,*) 'flow_dep_bdy_smoke: chem(its,kts,jts),chem_bxe(jts,kts,1),chem_btxe(jts,kts,1) ',chem(its,kts,jts),chem_bxe(jts,kts,1),chem_btxe(jts,kts,1)
         icall=icall+1
      END IF
   END SUBROUTINE flow_dep_bdy_smoke
  SUBROUTINE bdy_chem_value_rapsmoke ( chem, chem_b, chem_bt, irho, dt)
    IMPLICIT NONE
    REAL, intent(OUT) :: chem
    REAL, intent(IN) :: chem_b
    REAL, intent(IN) :: chem_bt, irho
    REAL, intent(IN) :: dt
    CHARACTER (LEN=80) :: message
      chem=max(epsilc,chem_b + chem_bt * dt) * irho
      RETURN
  END SUBROUTINE bdy_chem_value_rapsmoke
   SUBROUTINE cv_mmdd_jday ( YY, MM, DD, JDAY)
      INTEGER, INTENT(IN ) :: YY, MM, DD
      INTEGER, INTENT(OUT) :: JDAY
      INTEGER, DIMENSION(12) :: imon, imon_a
      INTEGER :: i
      DATA imon_a /0,31,59,90,120,151,181,212,243,273,304,334/
      do i=1,12
         imon(i) = imon_a(i)
      enddo
      if(YY .eq. (YY/4)*4) then
         do i=3,12
            imon(i) = imon(i) + 1
         enddo
      endif
      jday = imon(mm) + dd
   END SUBROUTINE cv_mmdd_jday
   integer FUNCTION get_last_gas(chem_opt)
     implicit none
     integer, intent(in) :: chem_opt
     select case (chem_opt)
     case (0)
        get_last_gas = 0
     case (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
           RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, &
           RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACMSORG_KPP, RACM_SOA_VBS_KPP,&
           RACM_SOA_VBS_AQCHEM_KPP,GOCARTRACM_KPP,GOCARTRADM2)
        get_last_gas = p_ho2
     case (CHEM_TRACER)
        get_last_gas = p_co
     case (CHEM_TRACE2)
        get_last_gas = p_tracer_1
     case (GOCART_SIMPLE)
        get_last_gas = p_msa
     case (CBM4_KPP)
        get_last_gas = p_ho2
     case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
        get_last_gas = p_nh3
     case (CHEM_VASH)
        get_last_gas = 0
     case (CHEM_VOLC)
        get_last_gas = p_sulf
     case (CHEM_VOLC_4BIN)
        get_last_gas = 0
     case (DUST)
        get_last_gas = 0
     case (MOZART_KPP)
        get_last_gas = p_meko2
     case (CO2_TRACER,GHG_TRACER)
        get_last_gas = 0
     case (CHEM_SMOKE)
        get_last_gas = 0
     case ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
        get_last_gas = p_soag
     case default
        call wrf_error_fatal3("<stdin>",771,&
"get_last_gas: could not decipher chem_opt value")
     end select
   END FUNCTION get_last_gas
    SUBROUTINE sorgam_set_aer_bc_pnnl( chem, z, nch, config_flags )
      implicit none
      INTEGER,INTENT(IN ) :: nch
      real,intent(in ) :: z
      REAL,INTENT(INOUT ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags
      REAL :: mult, &
              m3acc, m3cor, m3nuc, &
              bv_so4ai, bv_so4aj, &
              bv_nh4ai, bv_nh4aj, &
              bv_no3ai, bv_no3aj, &
              bv_eci, bv_ecj, &
              bv_p25i, bv_p25j, &
              bv_orgpai,bv_orgpaj, &
              bv_antha, bv_seas, bv_soila
    END SUBROUTINE sorgam_set_aer_bc_pnnl
    SUBROUTINE sorgam_vbs_set_aer_bc_pnnl( chem, z, nch, config_flags )
      implicit none
      INTEGER,INTENT(IN ) :: nch
      real,intent(in ) :: z
      REAL,INTENT(INOUT ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags
      REAL :: mult, &
              m3acc, m3cor, m3nuc, &
              bv_so4ai, bv_so4aj, &
              bv_nh4ai, bv_nh4aj, &
              bv_no3ai, bv_no3aj, &
              bv_eci, bv_ecj, &
              bv_p25i, bv_p25j, &
              bv_orgpai,bv_orgpaj, &
              bv_antha, bv_seas, bv_soila
    END SUBROUTINE sorgam_vbs_set_aer_bc_pnnl
END MODULE module_input_smoke_data
