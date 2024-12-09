module module_add_emiss_burn
CONTAINS
  subroutine add_emis_burn(id,dtstep,ktau,dz8w,rho_phy,rel_hum, &
                           chem,julday,gmt,xlat,xlong, &
                           luf_igbp,nlcat, &
                           lu_fire1,peak_hr, &
                           time_int,ebu, &
                           r_q,fhist,aod3d, &
                           rainc,rainnc, swdown, &
                           config_flags, &
                           ids,ide, jds,jde, kds,kde, &
                           ims,ime, jms,jme, kms,kme, &
                           its,ite, jts,jte, kts,kte )
   USE module_configure, only: grid_config_rec_type
   USE module_state_description
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: ktau, id, julday, nlcat, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
         INTENT(INOUT ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ), &
         INTENT(IN) :: ebu
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: xlat,xlong, rainc,rainnc,swdown, lu_fire1, peak_hr
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: r_q
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fhist
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(OUT) :: aod3d
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz8w,rho_phy,rel_hum
   REAL, DIMENSION(ims:ime,1:nlcat,jms:jme), INTENT(IN) :: luf_igbp
    REAL, INTENT(IN ) :: dtstep,gmt
    REAL(KIND=8), INTENT(IN) :: time_int
    integer :: i,j,k,n,m
    real :: conv_rho, conv, ext2, dm_smoke, daero_num_wfa, daero_num_ifa
    INTEGER, PARAMETER :: kfire_max=35
    REAL, PARAMETER :: dgvem_i= 0.08E-6
    REAL, PARAMETER :: sgem_i = 1.8
    REAL, PARAMETER :: dgvem_j= 0.3E-6
    REAL, PARAMETER :: sgem_j = 2.0
    REAL, PARAMETER :: dgvem_c= 6.0E-6
    REAL, PARAMETER :: sgem_c= 2.2
    REAL, PARAMETER :: pic= 3.14159
    REAL, PARAMETER :: fact_numn= 1.e-9*6.0/pic*exp(4.5*log(sgem_i)**2)/dgvem_i**3
    REAL, PARAMETER :: fact_numa= 1.e-9*6.0/pic*exp(4.5*log(sgem_j)**2)/dgvem_j**3
    REAL, PARAMETER :: fact_numc= 1.e-9*6.0/pic*exp(4.5*log(sgem_c)**2)/dgvem_c**3
    REAL, PARAMETER :: dens_oc_aer=1.4e3, dens_ec_aer=1.7e3
    REAL, PARAMETER :: ax1=531., cx1=7800.
    REAL, PARAMETER :: rinti=2.1813936e-8, ax2=3400., const2=130., coef2=10.6712963e-4, cx2=7200., timeq_max=3600.*24.
    REAL, PARAMETER :: sc_me= 4.0, ab_me=0.5
    REAL :: timeq, dt1,dt2,dtm
    INTEGER, SAVE :: icall
    timeq= gmt*3600. + REAL(time_int,4)
    timeq= mod(timeq,timeq_max)
    do j=jts,jte
       do i=its,ite
          if( luf_igbp(i,17,j)>0.99 .OR. ebu(i,1,j,p_ebu_smoke) < 1.e-6) cycle
          IF (time_int>64800. .AND. swdown(i,j)<.1 .AND. fhist(i,j)>.75 ) THEN
              fhist(i,j)= 0.75
          ENDIF
          IF (time_int>129600. .AND. swdown(i,j)<.1 .AND. fhist(i,j)>.5 ) THEN
              fhist(i,j)= 0.5
          ENDIF
          IF ( (rainc(i,j) + rainnc(i,j))>=10. .AND. fhist(i,j)>.3 ) THEN
              fhist(i,j)= 0.3
          ENDIF
           IF ( lu_fire1(i,j)>0.9 ) then
               r_q(i,j) = rinti* ax1 * exp(- (time_int**2)/(cx1**2) )
           ELSE
               dt1= abs(timeq - peak_hr(i,j))
               dt2= timeq_max - peak_hr(i,j) + timeq
               dtm= MIN(dt1,dt2)
               r_q(i,j) = rinti*( ax2 * exp(- dtm**2/(2.*cx2**2) ) + const2 - coef2*timeq )
           ENDIF
           r_q(i,j) = fhist(i,j)* max(0.,r_q(i,j)*timeq_max)
           IF (swdown(i,j)<.1) THEN
               r_q(i,j)= MIN(0.5,r_q(i,j))
           ENDIF
           IF (.NOT. config_flags%bb_dcycle) THEN
               r_q(i,j)= fhist(i,j)
           END IF
           do k=kts,kfire_max
              conv= r_q(i,j)*dtstep/(rho_phy(i,k,j)* dz8w(i,k,j))
              dm_smoke= conv*ebu(i,k,j,p_ebu_smoke)
              chem(i,k,j,p_smoke) = chem(i,k,j,p_smoke) + dm_smoke
              chem(i,k,j,p_smoke) = MIN(chem(i,k,j,p_smoke),5.e+3)
             if (icall<5000 .AND. config_flags%debug_chem) then
               if ( k==kts ) then
                 WRITE(6,*) 'add_emiss_burn: ktau,gmt,dtstep,time_int ',ktau,gmt,dtstep,time_int
                 WRITE(*,*) 'add_emiss_burn: i,j,xlat(i,j),xlong(i,j) ',i,j,xlat(i,j),xlong(i,j)
                 WRITE(*,*) 'add_emiss_burn: luf_igbp(i,:,j) ',luf_igbp(i,:,j)
                 WRITE(*,*) 'add_emiss_burn: lu_fire1(i,j) ',lu_fire1(i,j)
                 WRITE(6,*) 'add_emiss_burn: timeq,peak_hr(i,j),fhist(i,j),r_q(i,j) ',timeq,peak_hr(i,j),fhist(i,j),r_q(i,j)
                 WRITE(*,*) 'add_emiss_burn: rainc(i,j),rainnc(i,j) ', rainc(i,j),rainnc(i,j)
                 icall= icall+1
               endif
               if ( k==kts .OR. k==kfire_max ) then
                 WRITE(6,*) 'add_emiss_burn: i,j,k ',i,j,k
                 WRITE(6,*) 'add_emiss_burn: rho_phy(i,k,j),dz8w(i,k,j),conv ',rho_phy(i,k,j),dz8w(i,k,j),conv
                 WRITE(6,*) 'add_emiss_burn: ebu(i,k,j,p_ebu_smoke),dm_smoke ', ebu(i,k,j,p_smoke),dm_smoke
               endif
             endif
              enddo
            enddo
          enddo
          ext2= sc_me + ab_me
          do j=jts,jte
           do k=kts,kte
            do i=its,ite
               IF (.NOT. (chem(i,k,j,p_smoke)>=0. .AND. chem(i,k,j,p_smoke)<1.1e+4)) THEN
                   chem(i,k,j,p_smoke)=1.e-16
               END IF
               aod3d(i,k,j)= 1.e-6* ext2* chem(i,k,j,p_smoke)*rho_phy(i,k,j)*dz8w(i,k,j)
            enddo
           enddo
          enddo
     IF ( icall<2000 .AND. config_flags%debug_chem ) then
         WRITE(*,*) 'add_emis_burn: i,j,k,ext2 ',i,j,k,ext2
         WRITE(*,*) 'add_emis_burn: rel_hum(its,kts,jts),rel_hum(ite,kfire_max,jte) ',rel_hum(its,kts,jts),rel_hum(ite,kfire_max,jte)
         WRITE(*,*) 'add_emis_burn: aod3d(its,kts,jts),aod3d(ite,kfire_max,jte) ',aod3d(its,kts,jts),aod3d(ite,kfire_max,jte)
     END IF
    END subroutine add_emis_burn
END module module_add_emiss_burn
