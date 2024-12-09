MODULE module_dry_dep_driver
  IMPLICIT NONE
CONTAINS
    subroutine dry_dep_driver( id,ktau,dtstep,config_flags, &
                               dz8w,rho_phy,rel_hum,exch_h,hfx,pbl, &
                               rmol,ust, &
                               z_at_mid,z_at_w, &
                               lu_index,ddflx, &
                               frp, min_fplume, coef_bb_dc, &
                               chem, &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )
  USE module_model_constants
  USE module_configure
  USE module_state_description
  USE module_domain_type, only : domain
  USE module_vertmx_wrf
  IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: id, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   INTEGER, INTENT(IN ) :: ktau
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
         INTENT(INOUT ) :: chem
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: frp, lu_index, rmol, coef_bb_dc
   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: min_fplume
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz8w,rel_hum, &
                                                             z_at_mid, z_at_w , &
                                                             exch_h, rho_phy
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: pbl, ust, hfx
   REAL, DIMENSION( ims:ime ,jms:jme ), INTENT(INOUT) :: ddflx
      REAL, INTENT(IN) :: dtstep
      REAL :: clwchem, dvfog, ta, vegfrac, z1,zntt
      REAL :: old, new, fac, dvpart, curr_frp
      INTEGER :: n, nr, ipr, jpr, nvr
      INTEGER :: l2,m, l
      REAL, DIMENSION( ims:ime, jms:jme, num_chem ) :: ddvel
   REAL, DIMENSION( kms:kme ) :: dryrho_1d
   real :: pblst(kts:kte),ekmfull(kts:kte+1),zzfull(kts:kte+1),zz(kts:kte)
   integer :: kk,i,j,k,nv
   integer :: k_a, k_c, kmax, m_mam
   REAL, PARAMETER :: epsilc=1.E-16, kpart=500.
   INTEGER, SAVE :: icall
   INTRINSIC max, min
       do 100 j=jts,jte
          do 100 i=its,ite
            do k=kts,kte+1
              zzfull(k)=z_at_w(i,k,j)-z_at_w(i,kts,j)
            enddo
            do k=kts,kte
              zz(k)=z_at_mid(i,k,j)-z_at_w(i,kts,j)
              ekmfull(k)=max(1.e-6,exch_h(i,k,j))
            enddo
            ekmfull(kts)=0.
            ekmfull(kte+1)=0.
check_mx: IF (config_flags%enh_vermix) THEN
         if (frp(i,j) > 1.) then
            do kk=1,4
              ekmfull(kts+kk) = MAX(1.1*ekmfull(kts+kk),10.)
            enddo
         endif
         curr_frp= coef_bb_dc(i,j)* frp(i,j)
         if (curr_frp>250.) then
            do kk=1,5
              ekmfull(kts+kk) = MAX(1.1*ekmfull(kts+kk),20.)
            end do
         endif
         IF (icall<3000 .AND. config_flags%debug_chem) then
          IF (i==its .AND. j==jts) THEN
            WRITE(6,*) 'dry_dep_driver: coef_bb_dc(i,j),frp(i,j) ',coef_bb_dc(i,j),frp(i,j)
            WRITE(6,*) 'dry_dep_driver: after correction- ekmfull(kts:kts+10) ',ekmfull(kts:kts+10)
          END IF
         END IF
  ENDIF check_mx
         do k=kts,kte
            zz(k)=z_at_mid(i,k,j)-z_at_w(i,kts,j)
         enddo
loop_c: do nv=2,num_chem
         do k=kts,kte
            pblst(k)= max(epsilc,chem(i,k,j,nv))
            dryrho_1d(k) = rho_phy(i,k,j)
         enddo
         ddvel(i,j,nv) = 0.0
sm_dep: IF (config_flags%aer_drydep_opt == 111 ) then
            dvpart = ust(i,j)/kpart
            IF (rmol(i,j)<0.) THEN
               dvpart = dvpart*(1.+(-300.*rmol(i,j))**0.66667)
            ENDIF
            IF (rel_hum(i,1,j)>0.8) THEN
               dvpart = dvpart*(1.+0.37*exp((rel_hum(i,1,j)-0.8)/0.2))
            END IF
            ddvel(i,j,nv) = MIN(0.50,dvpart)
         ENDIF sm_dep
         call vertmx(dtstep,pblst,ekmfull,dryrho_1d,zzfull,zz,ddvel(i,j,nv),kts,kte)
        IF (icall<100 .AND. config_flags%debug_chem) then
         if (i==its .AND. j==jts) then
            WRITE(*,*) 'dry_dep_driver: 2) nv,ims,ime,jms,jme,kms,kme ',nv,ims,ime,jms,jme,kms,kme
            WRITE(*,*) 'dry_dep_driver: 2) i,j,lu_index(i,j) ',i,j,lu_index(i,j)
            WRITE(*,*) 'dry_dep_driver: 2) pblst(1:5),pblst(kte-4:kte) ',pblst(1:5),pblst(kte-4:kte)
         endif
        END IF
        IF (icall<4000 .AND. config_flags%debug_chem) then
         if (i==its .AND. j==jts) then
            WRITE(*,*) 'dry_dep_driver: 2) dvpart, ddvel(i,j,nv) ',dvpart,ddvel(i,j,nv)
            icall=icall+1
         endif
        END IF
         old = 0.0
         new = 0.0
         do k=kts,kte
              fac = dryrho_1d(k) * dz8w(i,k,j)
            old = old + max(epsilc,chem(i,k,j,nv)) * fac
            new = new + max(epsilc,pblst(k)) * fac
         enddo
         ddflx(i,j)= ddflx(i,j) + max( 0.0, (old - new) )
         do k=kts,kte-1
            chem(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
      enddo loop_c
      IF (icall<1000 .AND. config_flags%debug_chem) then
        if (i==its .AND. j==jts) then
          WRITE(6,*) 'dry_dep_driver: 2)fac ',fac
          WRITE(6,*) 'dry_dep_driver: 2)old,new,ddflx(i,j) ',old,new,ddflx(i,j)
        endif
      END IF
100 continue
   CALL wrf_debug(15,'end of dry_dep_driver')
END SUBROUTINE dry_dep_driver
END MODULE module_dry_dep_driver
