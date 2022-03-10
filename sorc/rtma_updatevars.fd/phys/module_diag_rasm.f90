MODULE module_diag_rasm
CONTAINS
  SUBROUTINE mean_output_calc( &
        is_restart, currentTime &
       ,stats_interval, output_freq, run_days &
       ,dt, xtime &
       ,psfc, psfc_mean, tsk, tsk_mean &
       ,pmsl_mean, t2, t2_mean &
       ,t, p, pb, moist, ht &
       ,th2, th2_mean, q2, q2_mean &
       ,u10, u10_mean, v10, v10_mean &
       ,hfx, hfx_mean, lh, lh_mean &
       ,swdnb, swdnb_mean, glw, glw_mean &
       ,lwupb, lwupb_mean, swupb, swupb_mean &
       ,swupt, swupt_mean, swdnt, swdnt_mean &
       ,lwupt, lwupt_mean, lwdnt, lwdnt_mean &
       ,avgoutalarm, avgOutDateStr &
       ,nsteps &
       ,ids, ide, jds, jde, kds, kde &
       ,ims, ime, jms, jme, kms, kme &
       ,ips, ipe, jps, jpe, kps, kpe &
       ,i_start, i_end, j_start, j_end &
       ,num_tiles &
       )
    USE module_utility
    USE module_streams
    USE module_domain, ONLY : domain_clock_get
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(IN) :: num_tiles
    INTEGER, INTENT(IN) :: stats_interval
    INTEGER, INTENT(IN) :: output_freq
    INTEGER, INTENT(IN) :: run_days
    INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start, i_end, j_start, j_end
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    TYPE(WRFU_Alarm), INTENT(INOUT) :: avgOutAlarm
    INTEGER, INTENT(INOUT) :: nsteps
    CHARACTER(*), INTENT(INOUT) :: avgOutDateStr
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    REAL, INTENT(IN) :: dt, xtime
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: ht
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: psfc, tsk, t2, th2, q2
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: u10, v10
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: hfx, lh
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: swdnb, glw, lwupb, swupb
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: swupt, swdnt, lwupt, lwdnt
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN) :: t, p, pb, moist
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: psfc_mean, tsk_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: pmsl_mean, t2_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: th2_mean, q2_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: u10_mean, v10_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: hfx_mean, lh_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swdnb_mean, glw_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: lwupb_mean, swupb_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swupt_mean, swdnt_mean
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: lwupt_mean, lwdnt_mean
    INTEGER :: i, j, ij
    REAL :: value
    LOGICAL :: is_restart
    INTEGER :: rc
    LOGICAL :: is_reset
    LOGICAL :: compute_avg
    INTEGER :: mean_interval
    CHARACTER (LEN=1024) :: message
    IF ( output_freq .eq. MONTHLY) THEN
      mean_interval = (run_days + 1) * 24 * 60 * 60
      WRITE(message, *) "RASM Diagnostics: Set average output to MONTHLY_INTERVAL ... "
      CALL wrf_debug(200, message)
    ELSE IF (output_freq .eq. DAYS ) THEN
      mean_interval = stats_interval * 24 * 60 * 60
      WRITE(message, *) "RASM Diagnostics: Set average output to DAYS  ... mean_interval (secs) =", mean_interval
      CALL wrf_debug(200, message)
    ELSE IF (output_freq .eq. HRS ) THEN
      mean_interval = stats_interval * 60 * 60
      WRITE(message, *) "RASM Diagnostics: Set average output to HRS  ... mean_interval (secs) =", mean_interval
      CALL wrf_debug(200, message)
    ELSE IF (output_freq .eq. MINS ) THEN
      mean_interval = stats_interval * 60
      WRITE(message, *) "RASM Diagnostics: Set average output to MINS  ... mean_interval (secs) =", mean_interval
      CALL wrf_debug(200, message)
    ELSE IF (output_freq .eq. SECS ) THEN
      mean_interval = stats_interval
      WRITE(message, *) "RASM Diagnostics: Set average output to SECS  ... mean_interval (secs) =", mean_interval
      CALL wrf_debug(200, message)
    ELSE
    END IF
    CALL getResetState(currentTime, xtime, dt, mean_interval, output_freq, is_reset)
    IF (is_reset) THEN
       DO ij = 1 , num_tiles
          DO j = j_start(ij), j_end(ij)
             DO i = i_start(ij), i_end(ij)
                psfc_mean(i,j)=0.0
                tsk_mean(i,j)=0.0
                pmsl_mean(i,j)=0.0
                t2_mean(i,j)=0.0
                th2_mean(i,j)=0.0
                q2_mean(i,j)=0.0
                u10_mean(i,j)=0.0
                v10_mean(i,j)=0.0
                hfx_mean(i,j)=0.0
                lh_mean(i,j)=0.0
                swdnb_mean(i,j)=0.0
                glw_mean(i,j)=0.0
                lwupb_mean(i,j)=0.0
                swupb_mean(i,j)=0.0
                swupt_mean(i,j)=0.0
                swdnt_mean(i,j)=0.0
                lwupt_mean(i,j)=0.0
                lwdnt_mean(i,j)=0.0
             ENDDO
          ENDDO
       ENDDO
       nsteps = 0.0
       WRITE(message, *) "RASM Statistics: RESET accumaltions and means ..................... nsteps=", nsteps
       CALL wrf_debug(200, message)
    ENDIF
    nsteps = nsteps+1.0
    WRITE(message, *) "RASM Statistics: Start accumulate .........................................................."
    CALL wrf_debug(200, message)
    WRITE(message, *) "RASM Statistics: nsteps=",nsteps, "time_step=", (xtime+dt/60.)*60./dt, "xtime=", xtime
    CALL wrf_debug(200, message)
    CALL var_accum_2d(psfc,ime-ims+1,jme-jms+1,psfc_mean)
    CALL var_accum_2d(tsk,ime-ims+1,jme-jms+1,tsk_mean)
    CALL PMSL_accum_01(ims, ime, jms, jme, kms, kme, &
                       ide, jde, ips, ipe, jps, jpe, &
                       t, p, pb, moist, ht, psfc, pmsl_mean)
    CALL var_accum_2d(t2,ime-ims+1,jme-jms+1,t2_mean)
    CALL var_accum_2d(th2,ime-ims+1,jme-jms+1,th2_mean)
    CALL var_accum_2d(q2,ime-ims+1,jme-jms+1,q2_mean)
    CALL var_accum_2d(u10,ime-ims+1,jme-jms+1,u10_mean)
    CALL var_accum_2d(v10,ime-ims+1,jme-jms+1,v10_mean)
    CALL var_accum_2d(hfx,ime-ims+1,jme-jms+1,hfx_mean)
    CALL var_accum_2d(lh,ime-ims+1,jme-jms+1,lh_mean)
    CALL var_accum_2d(swdnb,ime-ims+1,jme-jms+1,swdnb_mean)
    CALL var_accum_2d(glw,ime-ims+1,jme-jms+1,glw_mean)
    CALL var_accum_2d(lwupb,ime-ims+1,jme-jms+1,lwupb_mean)
    CALL var_accum_2d(swupb,ime-ims+1,jme-jms+1,swupb_mean)
    CALL var_accum_2d(swupt,ime-ims+1,jme-jms+1,swupt_mean)
    CALL var_accum_2d(swdnt,ime-ims+1,jme-jms+1,swdnt_mean)
    CALL var_accum_2d(lwupt,ime-ims+1,jme-jms+1,lwupt_mean)
    CALL var_accum_2d(lwdnt,ime-ims+1,jme-jms+1,lwdnt_mean)
    CALL getAvgState(currentTime, xtime, dt, mean_interval, output_freq, compute_avg, avgOutDateStr)
    IF (compute_avg) THEN
       psfc_mean=psfc_mean/nsteps
       tsk_mean=tsk_mean/nsteps
       pmsl_mean=pmsl_mean/nsteps
       t2_mean=t2_mean/nsteps
       th2_mean=th2_mean/nsteps
       q2_mean=q2_mean/nsteps
       u10_mean=u10_mean/nsteps
       v10_mean=v10_mean/nsteps
       hfx_mean=hfx_mean/nsteps
       lh_mean=lh_mean/nsteps
       swdnb_mean=swdnb_mean/nsteps
       glw_mean=glw_mean/nsteps
       lwupb_mean=lwupb_mean/nsteps
       swupb_mean=swupb_mean/nsteps
       swupt_mean=swupt_mean/nsteps
       swdnt_mean=swdnt_mean/nsteps
       lwupt_mean=lwupt_mean/nsteps
       lwdnt_mean=lwdnt_mean/nsteps
       if ( output_freq .EQ. MONTHLY) then
             WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL turn ON ALARM to generate output ........................"
             CALL wrf_debug(200, message)
       endif
       CALL WRFU_AlarmRingerOn (avgOutAlarm, rc=rc)
       WRITE(message, *) "RASM Statistics: Mean computed .........................................................."
       CALL wrf_debug(200, message)
    END IF
  END SUBROUTINE mean_output_calc
  SUBROUTINE diurnalcycle_output_calc( &
        is_restart, currentTime &
       ,dt, xtime &
       ,psfc, psfc_dtmp, tsk, tsk_dtmp &
       ,t2, t2_dtmp &
       ,t, p, pb, moist &
       ,th2, th2_dtmp, q2, q2_dtmp &
       ,u10, u10_dtmp, v10, v10_dtmp &
       ,hfx, hfx_dtmp, lh, lh_dtmp &
       ,swdnb, swdnb_dtmp, glw, glw_dtmp &
       ,lwupb, lwupb_dtmp, swupb, swupb_dtmp &
       ,swupt, swupt_dtmp, swdnt, swdnt_dtmp &
       ,lwupt, lwupt_dtmp, lwdnt, lwdnt_dtmp &
       ,avgoutalarm &
       ,diurnOutDateStr, avg_nsteps &
       ,diurnal_nsteps &
       ,psfc_diurn, tsk_diurn, t2_diurn &
       ,th2_diurn, q2_diurn, u10_diurn, v10_diurn &
       ,hfx_diurn, lh_diurn &
       ,swdnb_diurn, glw_diurn &
       ,lwupb_diurn, swupb_diurn &
       ,swupt_diurn, swdnt_diurn &
       ,lwupt_diurn, lwdnt_diurn &
       ,ids, ide, jds, jde, kds, kde &
       ,ims, ime, jms, jme, kms, kme &
       ,ips, ipe, jps, jpe, kps, kpe &
       ,i_start, i_end, j_start, j_end &
       ,num_tiles )
    USE module_utility
    USE module_domain, ONLY : domain_clock_get
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(IN) :: num_tiles
    INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start, i_end, j_start, j_end
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    TYPE(WRFU_Alarm), INTENT(INOUT) :: avgOutAlarm
    INTEGER, INTENT(INOUT) :: avg_nsteps
    INTEGER, INTENT(INOUT) :: diurnal_nsteps
    CHARACTER(*), INTENT(INOUT) :: diurnOutDateStr
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    INTEGER, PARAMETER :: NUM_DIURN_CYCLES = 8
    INTEGER, PARAMETER :: DIURNAL_3HR = 10800
    REAL, INTENT(IN) :: dt, xtime
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: psfc, tsk, t2, th2, q2
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: u10, v10
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: hfx, lh
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: swdnb, glw, lwupb, swupb
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: swupt, swdnt, lwupt, lwdnt
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN) :: t, p, pb, moist
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: psfc_dtmp, tsk_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: t2_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: th2_dtmp, q2_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: u10_dtmp, v10_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: hfx_dtmp, lh_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swdnb_dtmp, glw_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: lwupb_dtmp, swupb_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swupt_dtmp, swdnt_dtmp
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: lwupt_dtmp, lwdnt_dtmp
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: psfc_diurn, tsk_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: t2_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: th2_diurn, q2_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: u10_diurn, v10_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: hfx_diurn, lh_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: swdnb_diurn, glw_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: lwupb_diurn, swupb_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: swupt_diurn, swdnt_diurn
    REAL, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: lwupt_diurn, lwdnt_diurn
    INTEGER :: i, j, k, ij
    REAL :: value
    LOGICAL :: is_restart
    INTEGER :: rc
    INTEGER :: current_diurn_cycle
    INTEGER :: diurnal_output_freq
    INTEGER :: mean_output_freq
    INTEGER :: mean_interval
    LOGICAL :: is_avg_reset
    LOGICAL :: is_diurnal_reset
    LOGICAL :: compute_avg
    LOGICAL :: compute_diurnalcycle
    CHARACTER (LEN=1024) :: message
    CHARACTER (LEN=1024) :: EmptyStr
    LOGICAL :: diurn_test
    INTEGER :: diurn_interval
    mean_interval = DIURNAL_3HR
    mean_output_freq = HRS
    diurn_interval = 2 * 24 * 60 * 60
    diurnal_output_freq = MONTHLY
    EmptyStr =""
    diurn_test = .false.
    if (diurn_test) then
       diurnal_output_freq = DAYS
    else
       diurnal_output_freq = MONTHLY
    endif
    CALL getResetState(currentTime, xtime, dt, mean_interval, mean_output_freq, is_avg_reset)
    IF (is_avg_reset) THEN
       DO ij = 1 , num_tiles
          DO j = j_start(ij), j_end(ij)
             DO i = i_start(ij), i_end(ij)
                psfc_dtmp(i,j)=0.0
                tsk_dtmp(i,j)=0.0
                t2_dtmp(i,j)=0.0
                th2_dtmp(i,j)=0.0
                q2_dtmp(i,j)=0.0
                u10_dtmp(i,j)=0.0
                v10_dtmp(i,j)=0.0
                hfx_dtmp(i,j)=0.0
                lh_dtmp(i,j)=0.0
                swdnb_dtmp(i,j)=0.0
                glw_dtmp(i,j)=0.0
                lwupb_dtmp(i,j)=0.0
                swupb_dtmp(i,j)=0.0
                swupt_dtmp(i,j)=0.0
                swdnt_dtmp(i,j)=0.0
                lwupt_dtmp(i,j)=0.0
                lwdnt_dtmp(i,j)=0.0
             ENDDO
          ENDDO
       ENDDO
       avg_nsteps = 0.0
       WRITE(message, *) "RASM Statistics: RESET accumaltions and means ..................... avg_nsteps=", avg_nsteps
       CALL wrf_debug(200, message)
    ENDIF
    CALL getResetState(currentTime, xtime, dt, diurn_interval, diurnal_output_freq, is_diurnal_reset)
    IF (is_diurnal_reset) THEN
       DO ij = 1 , num_tiles
          DO k = 1 , NUM_DIURN_CYCLES
             DO j = j_start(ij), j_end(ij)
                DO i = i_start(ij), i_end(ij)
                   psfc_diurn(i,k,j)=0.0
                   tsk_diurn(i,k,j)=0.0
                   t2_diurn(i,k,j)=0.0
                   th2_diurn(i,k,j)=0.0
                   q2_diurn(i,k,j)=0.0
                   u10_diurn(i,k,j)=0.0
                   v10_diurn(i,k,j)=0.0
                   hfx_diurn(i,k,j)=0.0
                   lh_diurn(i,k,j)=0.0
                   swdnb_diurn(i,k,j)=0.0
                   glw_diurn(i,k,j)=0.0
                   lwupb_diurn(i,k,j)=0.0
                   swupb_diurn(i,k,j)=0.0
                   swupt_diurn(i,k,j)=0.0
                   swdnt_diurn(i,k,j)=0.0
                   lwupt_diurn(i,k,j)=0.0
                   lwdnt_diurn(i,k,j)=0.0
                ENDDO
             ENDDO
          ENDDO
       ENDDO
       diurnal_nsteps = 0.0
       WRITE(message, *) "RASM Statistics: RESET Diurnal means ..................... diurnal_nsteps=", diurnal_nsteps
       CALL wrf_debug(200, message)
    ENDIF
    avg_nsteps = avg_nsteps+1.0
    CALL var_accum_2d(psfc,ime-ims+1,jme-jms+1,psfc_dtmp)
    CALL var_accum_2d(tsk,ime-ims+1,jme-jms+1,tsk_dtmp)
    CALL var_accum_2d(t2,ime-ims+1,jme-jms+1,t2_dtmp)
    CALL var_accum_2d(th2,ime-ims+1,jme-jms+1,th2_dtmp)
    CALL var_accum_2d(q2,ime-ims+1,jme-jms+1,q2_dtmp)
    CALL var_accum_2d(u10,ime-ims+1,jme-jms+1,u10_dtmp)
    CALL var_accum_2d(v10,ime-ims+1,jme-jms+1,v10_dtmp)
    CALL var_accum_2d(hfx,ime-ims+1,jme-jms+1,hfx_dtmp)
    CALL var_accum_2d(lh,ime-ims+1,jme-jms+1,lh_dtmp)
    CALL var_accum_2d(swdnb,ime-ims+1,jme-jms+1,swdnb_dtmp)
    CALL var_accum_2d(glw,ime-ims+1,jme-jms+1,glw_dtmp)
    CALL var_accum_2d(lwupb,ime-ims+1,jme-jms+1,lwupb_dtmp)
    CALL var_accum_2d(swupb,ime-ims+1,jme-jms+1,swupb_dtmp)
    CALL var_accum_2d(swupt,ime-ims+1,jme-jms+1,swupt_dtmp)
    CALL var_accum_2d(swdnt,ime-ims+1,jme-jms+1,swdnt_dtmp)
    CALL var_accum_2d(lwupt,ime-ims+1,jme-jms+1,lwupt_dtmp)
    CALL var_accum_2d(lwdnt,ime-ims+1,jme-jms+1,lwdnt_dtmp)
    CALL getAvgState(currentTime, xtime, dt, mean_interval, mean_output_freq, compute_avg, EmptyStr)
    IF (compute_avg) THEN
       psfc_dtmp=psfc_dtmp/avg_nsteps
       tsk_dtmp=tsk_dtmp/avg_nsteps
       t2_dtmp=t2_dtmp/avg_nsteps
       th2_dtmp=th2_dtmp/avg_nsteps
       q2_dtmp=q2_dtmp/avg_nsteps
       u10_dtmp=u10_dtmp/avg_nsteps
       v10_dtmp=v10_dtmp/avg_nsteps
       hfx_dtmp=hfx_dtmp/avg_nsteps
       lh_dtmp=lh_dtmp/avg_nsteps
       swdnb_dtmp=swdnb_dtmp/avg_nsteps
       glw_dtmp=glw_dtmp/avg_nsteps
       lwupb_dtmp=lwupb_dtmp/avg_nsteps
       swupb_dtmp=swupb_dtmp/avg_nsteps
       swupt_dtmp=swupt_dtmp/avg_nsteps
       swdnt_dtmp=swdnt_dtmp/avg_nsteps
       lwupt_dtmp=lwupt_dtmp/avg_nsteps
       lwdnt_dtmp=lwdnt_dtmp/avg_nsteps
       CALL get_diurn_cycle(currentTime, xtime, dt, current_diurn_cycle)
       CALL var_accum_diurnal(psfc_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, psfc_diurn)
       CALL var_accum_diurnal(tsk_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, tsk_diurn)
       CALL var_accum_diurnal(t2_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, t2_diurn)
       CALL var_accum_diurnal(th2_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, th2_diurn)
       CALL var_accum_diurnal(q2_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, q2_diurn)
       CALL var_accum_diurnal(u10_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, u10_diurn)
       CALL var_accum_diurnal(v10_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, v10_diurn)
       CALL var_accum_diurnal(hfx_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, hfx_diurn)
       CALL var_accum_diurnal(lh_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lh_diurn)
       CALL var_accum_diurnal(swdnb_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swdnb_diurn)
       CALL var_accum_diurnal(glw_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, glw_diurn)
       CALL var_accum_diurnal(lwupb_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lwupb_diurn)
       CALL var_accum_diurnal(swupb_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swupb_diurn)
       CALL var_accum_diurnal(swupt_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swupt_diurn)
       CALL var_accum_diurnal(swdnt_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swdnt_diurn)
       CALL var_accum_diurnal(lwupt_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lwupt_diurn)
       CALL var_accum_diurnal(lwdnt_dtmp, ime-ims+1, NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lwdnt_diurn)
       if (current_diurn_cycle .eq. 8) then
          diurnal_nsteps = diurnal_nsteps + 1.0
       endif
    END IF
    CALL getDiurnalState(currentTime, xtime, dt, diurn_interval, diurnal_output_freq, compute_diurnalcycle, diurnOutDateStr)
    IF (compute_diurnalcycle) THEN
       psfc_diurn=psfc_diurn/diurnal_nsteps
       tsk_diurn=tsk_diurn/diurnal_nsteps
       t2_diurn=t2_diurn/diurnal_nsteps
       th2_diurn=th2_diurn/diurnal_nsteps
       q2_diurn=q2_diurn/diurnal_nsteps
       u10_diurn=u10_diurn/diurnal_nsteps
       v10_diurn=v10_diurn/diurnal_nsteps
       hfx_diurn=hfx_diurn/diurnal_nsteps
       lh_diurn=lh_diurn/diurnal_nsteps
       swdnb_diurn=swdnb_diurn/diurnal_nsteps
       glw_diurn=glw_diurn/diurnal_nsteps
       lwupb_diurn=lwupb_diurn/diurnal_nsteps
       swupb_diurn=swupb_diurn/diurnal_nsteps
       swupt_diurn=swupt_diurn/diurnal_nsteps
       swdnt_diurn=swdnt_diurn/diurnal_nsteps
       lwupt_diurn=lwupt_diurn/diurnal_nsteps
       lwdnt_diurn=lwdnt_diurn/diurnal_nsteps
       CALL WRFU_AlarmRingerOn (avgOutAlarm, rc=rc)
       WRITE(message, *) "RASM Statistics: Diurnal Mean Cycle computed .........................................................."
       CALL wrf_debug(200, message)
    END IF
  END SUBROUTINE diurnalcycle_output_calc
  SUBROUTINE var_accum_diurnal(var, dx, dz, dy, current_cycle, var_accum)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: dx, dz, dy, current_cycle
    REAL, DIMENSION(dx,dy), INTENT(IN) :: var
    REAL, DIMENSION(dx, dz, dy), INTENT(INOUT) :: var_accum
    INTEGER :: k, i, j
    k = current_cycle
    DO j=1,dy
       DO i=1,dx
          var_accum(i, k, j) = var_accum(i, k, j) + var(i,j)
       ENDDO
    ENDDO
  END SUBROUTINE var_accum_diurnal
  SUBROUTINE var_accum_2d(var, dx, dy, var_accum)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: dx, dy
    REAL, DIMENSION(dx,dy), INTENT(IN) :: var
    REAL, DIMENSION(dx,dy), INTENT(INOUT) :: var_accum
    var_accum = var_accum + var
  END SUBROUTINE var_accum_2d
  SUBROUTINE var_accum_3d_01(ims, ime, jms, jme, kms, kme, &
                             ide, jde, ips, ipe, jps, jpe, &
                             var, var_accum)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN):: var
    REAL, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: var_accum
    INTEGER :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message
    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1
    k=1
    DO j=jps, j_end
       DO i=ips, i_end
          var_accum(i,j) = var_accum(i,j) + var(i,k,j)
       ENDDO
    ENDDO
  END SUBROUTINE var_accum_3d_01
  SUBROUTINE shum_accum_01(ims, ime, jms, jme, kms, kme, &
                           ide, jde, ips, ipe, jps, jpe, &
                           moist, var_accum)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN):: moist
    REAL, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: var_accum
    REAL :: tmp_shum
    INTEGER :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message
    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1
    k=1
    DO j=jps, j_end
       DO i=ips, i_end
          if( moist(i,k,j) .gt. 0 ) then
             tmp_shum = moist(i,k,j) / (1+moist(i,k,j))
          else
             tmp_shum = 0.0
          endif
          var_accum(i,j) = var_accum(i,j) + tmp_shum
       ENDDO
    ENDDO
  END SUBROUTINE shum_accum_01
  SUBROUTINE T_accum_01( ims, ime, jms, jme, kms, kme, &
                         ide, jde, ips, ipe, jps, jpe, &
                         t, p, pb, t_accum)
    USE module_model_constants, only: t0,p0
    USE shr_const_mod
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN):: t, p, pb
    REAL, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: t_accum
    REAL :: t_tmp, cp, rd
    INTEGER :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message
    rd=SHR_CONST_RDAIR
    cp=SHR_CONST_CPDAIR
    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1
    k=1
    DO j=jps, j_end
       DO i=ips, i_end
          t_tmp = (t(i,k,j) + t0) * (((p(i,k,j) + pb(i,k,j))/p0) ** (rd/cp))
          t_accum(i,j) = t_accum(i,j) + t_tmp
       ENDDO
    ENDDO
  END SUBROUTINE T_accum_01
  SUBROUTINE PMSL_accum_01( ims, ime, jms, jme, kms, kme, &
                            ide, jde, ips, ipe, jps, jpe, &
                            t, p, pb, moist, ht, psfc, pmsl_accum)
    USE module_model_constants, only: t0,p0
    USE shr_const_mod
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN):: t, p, pb, moist
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN):: ht, psfc
    REAL, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: pmsl_accum
    REAL :: t_tmp, tmp_pmsl, z, tv, g, cp, rd, xlapse
    REAL :: alpha, tstar, tt0, alph, beta, psfc_tmp, p_tmp
    INTEGER :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message
    xlapse = 6.5D-3
    rd=SHR_CONST_RDAIR
    g=SHR_CONST_G
    cp=SHR_CONST_CPDAIR
    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1
    k=1
    do j=jps, j_end
       do i=ips, i_end
             t_tmp = (t(i,k,j) + t0) * (((p(i,k,j) + pb(i,k,j))/p0) ** (rd/cp))
             tv = t_tmp * (1 + moist(i,k,j) * 0.61)
             z = ht(i,j) * g
             alpha = rd * xlapse/g
             psfc_tmp = psfc(i,j)
             p_tmp = (p(i,k,j) + pb(i,k,j))
             if ( abs(z/g) < 1.0E-4 )then
                tmp_pmsl = psfc_tmp
             else
                tstar = tv * (1.0 + alpha * (psfc_tmp/p_tmp-1.0))
                tt0 = tstar + xlapse*z/g
                if ( tstar <= 290.5 .and. tt0 > 290.5 ) then
                   alph = rd/z * (290.5 - tstar)
                else if (tstar > 290.5 .and. tt0 > 290.5) then
                   alph = 0.0
                   tstar = 0.5 * (290.5 + tstar)
                else
                   alph=alpha
                   if (tstar < 255.0) then
                      tstar = 0.5 * (255.0 + tstar)
                   endif
                endif
                beta = z/(rd*tstar)
                tmp_pmsl = psfc_tmp * exp( beta*(1.0 - alph * beta/2.0 + ((alph*beta)**2)/3.0))
             end if
             pmsl_accum(i,j) = pmsl_accum(i,j) + tmp_pmsl
          ENDDO
       ENDDO
  END SUBROUTINE PMSL_accum_01
  SUBROUTINE getResetState(currentTime, xtime, dt, mean_interval, output_freq, is_reset)
    USE module_utility
    IMPLICIT NONE
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    INTEGER, INTENT(IN) :: mean_interval
    REAL, INTENT(IN) :: dt, xtime
    INTEGER, INTENT(IN) :: output_freq
    LOGICAL, INTENT(INOUT) :: is_reset
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time) :: prevTime
    integer :: yr
    integer :: mon
    integer :: prevMon
    integer :: day
    integer :: dtime
    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=10) ::str_day
    CHARACTER (LEN=80) ::filedate
    CHARACTER (LEN=1024) :: message
    dtime = INT (dt)
    is_reset = .false.
    if (output_freq .eq. MONTHLY) then
       call WRFU_TimeGet( currentTime, mm=mon)
       call WRFU_TimeIntervalSet( off, s=dtime)
       prevTime = currentTime - off
       call WRFU_TimeGet( prevTime, mm=prevMon)
       if ( (mon-prevMon) /= 0) then
          is_reset = .true.
          WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL RESET condition met (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    else
       if ( MOD(NINT(xtime*60./dt),NINT(mean_interval/dt)) == 0 ) then
          is_reset = .true.
          WRITE(message, *) "RASM Statistics: STATIC_INTERVAL RESET condition met (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    endif
  END SUBROUTINE getResetState
  SUBROUTINE getAvgState(currentTime, xtime, dt, mean_interval, output_freq, compute_avg, OutDateStr)
    USE module_utility
    IMPLICIT NONE
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    INTEGER, INTENT(IN) :: mean_interval
    REAL, INTENT(IN) :: dt, xtime
    INTEGER, INTENT(IN) :: output_freq
    LOGICAL, INTENT(INOUT) :: compute_avg
    CHARACTER(*), INTENT(INOUT) :: OutDateStr
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time) :: nextTime
    TYPE(WRFU_Time) :: prevTime
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    integer :: yr
    integer :: mon
    integer :: nextMon
    integer :: prevMon
    integer :: day
    integer :: hr
    integer :: min
    integer :: sec
    integer :: totalsec
    integer :: dtime
    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=10) ::str_day
    CHARACTER (LEN=10) ::str_sec
    CHARACTER (LEN=80) ::filedate
    CHARACTER (LEN=1024) :: message
    dtime = INT (dt)
    compute_avg = .false.
    if ( output_freq .EQ. MONTHLY) then
       call WRFU_TimeGet( currentTime, mm=mon)
       call WRFU_TimeIntervalSet( off, s=dtime)
       nextTime = currentTime + off
       call WRFU_TimeGet( nextTime, mm=nextMon)
       if ( (nextMon-mon) /= 0) then
          compute_avg = .true.
          WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL AVG condition met (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    else
       if ((MOD(NINT((xtime+dt/60.)*60./dt),NINT(mean_interval/dt)) == 0)) then
          compute_avg = .true.
          WRITE(message, *) "RASM Statistics: STATIC_INTERVAL AVG condition met (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    endif
    if (compute_avg) then
       IF ( (output_freq .ne. MONTHLY) .and. (output_freq .ne. DAYS)) THEN
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon, dd=day, h=hr, m=min, s=sec)
          WRITE(str_yr, '(I4.4)') yr
          WRITE(str_mon, '(I2.2)') mon
          WRITE(str_day, '(I2.2)') day
          totalsec = (hr * 60 * 60) + (min * 60) + sec
          WRITE(str_sec, '(I5.5)') totalsec
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)//"-"//trim(str_sec) 
          OutDateStr = filedate
          WRITE(message, *) "RASM Statistics:  STATIC_INTERVAL AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)
       ELSE IF ( output_freq .eq. MONTHLY ) THEN
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon)
          IF (mon .eq. 1) THEN
             mon = 12
             yr = yr - 1
          ELSE
             mon = mon - 1
          ENDIF
          WRITE(str_yr, '(I4.4)') yr
          WRITE(str_mon, '(I2.2)') mon
          filedate = trim(str_yr)//"-"//trim(str_mon)
          OutDateStr = filedate
          WRITE(message, *) "RASM Statistics:  AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)
       ELSE IF (output_freq .eq. DAYS ) THEN
          call WRFU_TimeIntervalSet( off, s=mean_interval-dtime)
          prevTime = currentTime - off
          call WRFU_TimeGet( prevTime, yy=yr, mm=mon, dd=day)
          WRITE(str_yr, '(I4.4)') yr
          WRITE(str_mon, '(I2.2)') mon
          WRITE(str_day, '(I2.2)') day
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)
          OutDateStr = filedate
          WRITE(message, *) "RASM Statistics:  AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)
       ENDIF
    endif
  END SUBROUTINE getAvgState
  SUBROUTINE getDiurnalState(currentTime, xtime, dt, diurn_interval, output_freq, compute_diurn, OutDateStr)
    USE module_utility
    IMPLICIT NONE
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    REAL, INTENT(IN) :: dt, xtime
    INTEGER, INTENT(IN) :: output_freq
    integer, INTENT(IN) :: diurn_interval
    LOGICAL, INTENT(INOUT) :: compute_diurn
    CHARACTER(*), INTENT(INOUT) :: OutDateStr
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time) :: nextTime
    TYPE(WRFU_Time) :: prevTime
    integer :: yr
    integer :: mon
    integer :: nextMon
    integer :: dtime
    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=80) ::filedate
    CHARACTER (LEN=1024) :: message
    integer :: mean_interval
    CHARACTER (LEN=10) ::str_day
    integer :: day
    dtime = INT (dt)
    compute_diurn = .false.
    if ( output_freq .EQ. MONTHLY) then
       call WRFU_TimeGet( currentTime, mm=mon)
       call WRFU_TimeIntervalSet( off, s=dtime)
       nextTime = currentTime + off
       call WRFU_TimeGet( nextTime, mm=nextMon)
       if ( (nextMon-mon) /= 0) then
          compute_diurn = .true.
          WRITE(message, *) "RASM Statistics: Diurnal AVG condition met (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    else
       if ((MOD(NINT((xtime+dt/60.)*60./dt),NINT(diurn_interval/dt)) == 0)) then
          compute_diurn = .true.
          WRITE(message, *) "RASM Statistics: Diurnal AVG condition met DAILY TEST (return TRUE) "
          CALL wrf_debug(200, message)
       endif
    endif
    if (compute_diurn) then
       if ( output_freq .EQ. MONTHLY) then
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon)
          IF (mon .eq. 1) THEN
             mon = 12
             yr = yr - 1
          ELSE
             mon = mon - 1
          ENDIF
          WRITE(str_yr, '(I4.4)') yr
          WRITE(str_mon, '(I2.2)') mon
          filedate = trim(str_yr)//"-"//trim(str_mon)
          OutDateStr = filedate
          WRITE(message, *) "RASM Statistics:  Diurnal ACG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)
       else
          call WRFU_TimeIntervalSet( off, s=diurn_interval-dtime)
          prevTime = currentTime - off
          call WRFU_TimeGet( prevTime, yy=yr, mm=mon, dd=day)
          WRITE(str_yr, '(I4.4)') yr
          WRITE(str_mon, '(I2.2)') mon
          WRITE(str_day, '(I2.2)') day
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)
          OutDateStr = filedate
          WRITE(message, *) "RASM Statistics:  Diurnal AVG condition met DAILY TEST......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)
       endif
    endif
  END SUBROUTINE getDiurnalState
  SUBROUTINE get_diurn_cycle(currentTime, xtime, dt, diurn_cycle)
    USE module_utility
    IMPLICIT NONE
    TYPE(WRFU_Time), INTENT(IN) :: currentTime
    REAL, INTENT(IN) :: dt, xtime
    INTEGER, INTENT(INOUT) :: diurn_cycle
    INTEGER, PARAMETER :: NONE = 0
    INTEGER, PARAMETER :: SECS = 1
    INTEGER, PARAMETER :: MINS = 2
    INTEGER, PARAMETER :: HRS = 3
    INTEGER, PARAMETER :: DAYS = 4
    INTEGER, PARAMETER :: MONTHLY = 5
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time) :: nextTime
    TYPE(WRFU_Time) :: prevTime
    integer :: yr
    integer :: mon
    integer :: day
    integer :: hr
    integer :: dtime
    CHARACTER (LEN=1024) :: message
    dtime = INT (dt)
    diurn_cycle = -1
    call WRFU_TimeIntervalSet( off, s=dtime)
    nextTime = currentTime + off
    call WRFU_TimeGet( nextTime, yy=yr, mm=mon, dd=day, h=hr)
    if (hr .eq. 3) then
       diurn_cycle = 1
    else if (hr .eq. 6) then
       diurn_cycle = 2
    else if (hr .eq. 9) then
       diurn_cycle = 3
    else if (hr .eq. 12) then
       diurn_cycle = 4
    else if (hr .eq. 15) then
       diurn_cycle = 5
    else if (hr .eq. 18) then
       diurn_cycle = 6
    else if (hr .eq. 21) then
       diurn_cycle = 7
    else if (hr .eq. 0) then
       diurn_cycle = 8
    else
       WRITE (message, * )"RASM Statistics:: DIURNAL ERROR -- error -- ERROR -- error : Did not find valid diurnal cycle"
       CALL wrf_debug(0, message)
       WRITE (message, * )"RASM Statistics:: DIURNAL ERROR -- Valid diurnal cycles (0,3,6,9,12,15,18 or 21) ... reported ", diurn_cycle
       CALL wrf_error_fatal3("<stdin>",999,&
TRIM(message) )
    endif
  END SUBROUTINE get_diurn_cycle
END MODULE module_diag_rasm
