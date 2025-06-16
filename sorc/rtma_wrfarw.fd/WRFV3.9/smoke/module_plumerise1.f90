 module module_plumerise1
CONTAINS
subroutine plumerise_driver (id, &
                             flam_frac,ebb_smoke,ebu, &
                             config_flags, &
                             t_phy,q_vap, &
                             rho_phy,vvel,u_phy,v_phy,p_phy, &
                             z_at_w,z,ktau, &
                             plume_fre, k_min, k_max, &
                             ids,ide, jds,jde, kds,kde, &
                             ims,ime, jms,jme, kms,kme, &
                             its,ite, jts,jte, kts,kte )
  USE module_configure
  USE module_model_constants
  USE module_state_description
  USE module_zero_plumegen_coms
  USE module_smoke_plumerise
  IMPLICIT NONE
   REAL, DIMENSION( ims:ime, jms:jme, 4 ), INTENT(IN ) :: plume_fre
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: id,ktau, &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ), INTENT(INOUT ) :: ebu
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: ebb_smoke
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: flam_frac
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(IN ) :: t_phy,z,z_at_w,vvel,u_phy,v_phy,rho_phy,p_phy,q_vap
      INTEGER :: nv, i, j, k, kp1, kp2
      INTEGER, DIMENSION(ims:ime, jms:jme), INTENT (OUT) :: k_min, k_max
      real, dimension (kte) :: u_in ,v_in ,w_in ,theta_in ,pi_in, rho_phyin ,qv_in ,zmid, z_lev
      REAL :: dz_plume
      INTEGER, SAVE :: icall
        IF (icall<2000 .AND. config_flags%debug_chem) then
           WRITE(*,*) 'module_plumerise1: its,ite,jts,jte ', its,ite,jts,jte
           WRITE(*,*) 'module_plumerise1: ims,ime,jms,jme ', ims,ime,jms,jme
           WRITE(*,*) 'module_plumerise1: p_ebu_smoke,num_ebu: ', p_ebu_smoke,num_ebu
           WRITE(*,*) 'module_plumerise1: maxval(ebu(:,kts,:,p_ebu_smoke)) ', maxval(ebu(:,kts,:,p_ebu_smoke))
           icall=icall+1
         END IF
       do nv=2,num_ebu
          do j=jts,jte
            do k=kts+1,kte
               do i=its,ite
                 ebu(i,k,j,nv)=0.
               enddo
            enddo
          enddo
       enddo
       do j=jts,jte
        do i=its,ite
           flam_frac(i,j)= config_flags%flam_part;
        enddo
       enddo
check_pl: IF (config_flags%plumerise_flag == 2 ) THEN
       do j=jts,jte
          do i=its,ite
               k_min(i,j)=0
               k_max(i,j)=0
               if (plume_fre(i,j,1) < 1.e+5) cycle
               do k=kts,kte
                  u_in(k)= u_phy(i,k,j)
                  v_in(k)= v_phy(i,k,j)
                  w_in(k)= vvel(i,k,j)
                  qv_in(k)= q_vap(i,k,j)
                  pi_in(k)= cp*(p_phy(i,k,j)/p1000mb)**rcp
                  zmid(k)= z(i,k,j)-z_at_w(i,kts,j)
                  z_lev(k)= z_at_w(i,k,j)-z_at_w(i,kts,j)
                  rho_phyin(k)= rho_phy(i,k,j)
                  theta_in(k)= t_phy(i,k,j)/pi_in(k)*cp
               enddo
             IF (icall<2000 .AND. config_flags%debug_chem) then
               WRITE(*,*) 'module_plumerise1: i,j ',i,j
               WRITE(*,*) 'module_plumerise1: plume_fre(i,j,:) ',plume_fre(i,j,:)
               WRITE(*,*) 'module_plumerise1: ebu(i,kts,j,p_ebu_smoke) ',ebu(i,kts,j,p_ebu_smoke)
               WRITE(*,*) 'module_plumerise1: u_in(10),v_in(10),w_in(kte),qv_in(10),pi_in(10) ',u_in(10),v_in(10),w_in(kte),qv_in(10),pi_in(10)
               WRITE(*,*) 'module_plumerise1: zmid(kte),z_lev(kte),rho_phyin(kte),theta_in(kte) ',zmid(kte),z_lev(kte),rho_phyin(kte),theta_in(kte)
             END IF
               CALL plumerise(kte,1,1,1,1,1,1, &
                              u_in, v_in, w_in, theta_in ,pi_in, &
                              rho_phyin, qv_in, zmid, z_lev, &
                              plume_fre(i,j,:), k_min(i,j), &
                              k_max(i,j), ktau, config_flags%debug_chem )
               kp1= k_min(i,j)
               kp2= k_max(i,j)
               dz_plume= z_at_w(i,kp2,j) - z_at_w(i,kp1,j)
               do nv=2,num_ebu
                  do k=kp1,kp2-1
                     ebu(i,k,j,nv)= flam_frac(i,j)* ebb_smoke(i,j)* (z_at_w(i,k+1,j)-z_at_w(i,k,j))/dz_plume
                  enddo
                  ebu(i,kts,j,nv)= (1.-flam_frac(i,j))* ebb_smoke(i,j)
               enddo
               IF (icall<2000 .AND. config_flags%debug_chem) then
                   WRITE(*,*) 'module_plumerise1: kts,flam_frac(i,j) ',kts,flam_frac(i,j)
                   WRITE(*,*) 'module_plumerise1: k_min(i,j), k_max(i,j) ',k_min(i,j), k_max(i,j)
               END IF
            enddo
          enddo
        ENDIF check_pl
end subroutine plumerise_driver
END module module_plumerise1
