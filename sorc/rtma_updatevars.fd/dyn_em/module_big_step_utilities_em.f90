MODULE module_big_step_utilities_em
   USE module_model_constants
   USE module_state_description, only: p_qg, p_qs, p_qi, gdscheme, tiedtkescheme, ntiedtkescheme, kfetascheme, mskfscheme, &
       g3scheme, gfscheme,p_qv, param_first_scalar, p_qr, p_qc, DFI_FWD
   USE module_configure, ONLY : grid_config_rec_type
   USE module_wrf_error
CONTAINS
SUBROUTINE calc_mu_uv ( config_flags, &
                        mu, mub, muu, muv, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type ) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT( OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: mu, mub
   INTEGER :: i, j, itf, jtf, im, jm
      itf=ite
      jtf=MIN(jte,jde-1)
      IF ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j)+MUB(i,j)+MUB(im,j))
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j)+MUB(i-1,j)+MUB(im,j))
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j)+MUB(i,j)+MUB(im,j))
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j)+MUB(i-1,j)+MUB(im,j))
         ENDDO
      END IF
      itf=MIN(ite,ide-1)
      jtf=jte
      IF ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm)+MUB(i,j)+MUB(i,jm))
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm)+MUB(i,j-1)+MUB(i,jm))
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm)+MUB(i,j)+MUB(i,jm))
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm)+MUB(i,j-1)+MUB(i,jm))
         ENDDO
      END IF
END SUBROUTINE calc_mu_uv
SUBROUTINE calc_mu_uv_1 ( config_flags, &
                          mu, muu, muv, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type ) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT( OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: mu
   INTEGER :: i, j, itf, jtf, im, jm
      itf=ite
      jtf=MIN(jte,jde-1)
      IF ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j))
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j))
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j))
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j))
         ENDDO
      END IF
      itf=MIN(ite,ide-1)
      jtf=jte
      IF ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm))
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm))
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm))
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm))
         ENDDO
      END IF
END SUBROUTINE calc_mu_uv_1
SUBROUTINE couple_momentum ( muu, ru, u, msfu, &
                             muv, rv, v, msfv, msfv_inv, &
                             mut, rw, w, msft, &
                             c1h, c2h, c1f, c2f, &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT( OUT) :: ru, rv, rw
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: muu, muv, mut
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfu, msfv, msft, msfv_inv
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: u, v, w
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h, c1f, c2f
   INTEGER :: i, j, k, itf, jtf, ktf
   ktf=MIN(kte,kde-1)
      itf=ite
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         ru(i,k,j)=u(i,k,j)*(c1h(k)*muu(i,j)+c2h(k))/msfu(i,j)
      ENDDO
      ENDDO
      ENDDO
      itf=MIN(ite,ide-1)
      jtf=jte
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
           rv(i,k,j)=v(i,k,j)*(c1h(k)*muv(i,j)+c2h(k))*msfv_inv(i,j)
      ENDDO
      ENDDO
      ENDDO
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rw(i,k,j)=w(i,k,j)*(c1f(k)*mut(i,j)+c2f(k))/msft(i,j)
      ENDDO
      ENDDO
      ENDDO
END SUBROUTINE couple_momentum
SUBROUTINE calc_mu_staggered ( mu, mub, muu, muv, &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT( OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: mu, mub
   INTEGER :: i, j, itf, jtf
      itf=ite
      jtf=MIN(jte,jde-1)
      IF ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         DO j=jts,jtf
            MUU(i,j) = MU(i,j) +MUB(i,j)
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=ite
         DO j=jts,jtf
            MUU(i,j) = MU(i-1,j) +MUB(i-1,j)
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         DO j=jts,jtf
            MUU(i,j) = MU(i,j) +MUB(i,j)
         ENDDO
         i=ite
         DO j=jts,jtf
            MUU(i,j) = MU(i-1,j) +MUB(i-1,j)
         ENDDO
      END IF
      itf=MIN(ite,ide-1)
      jtf=jte
      IF ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         DO i=its,itf
             MUV(i,j) = MU(i,j) +MUB(i,j)
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jte
         DO i=its,itf
             MUV(i,j) = MU(i,j-1) +MUB(i,j-1)
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         DO i=its,itf
             MUV(i,j) = MU(i,j) +MUB(i,j)
         ENDDO
         j=jte
         DO i=its,itf
             MUV(i,j) = MU(i,j-1) +MUB(i,j-1)
         ENDDO
      END IF
END SUBROUTINE calc_mu_staggered
SUBROUTINE couple ( mu, mub, rfield, field, name, &
                    msf, c1h, c2h, c1, c2, &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte
   CHARACTER(LEN=1) , INTENT(IN ) :: name
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT( OUT) :: rfield
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: mu, mub, msf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: field
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h, c1, c2
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION(ims:ime,jms:jme) :: muu , muv
   ktf=MIN(kte,kde-1)
   IF (name .EQ. 'u')THEN
      CALL calc_mu_staggered ( mu, mub, muu, muv, &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )
      itf=ite
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*(c1h(k)*muu(i,j)+c2h(k))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO
   ELSE IF (name .EQ. 'v')THEN
      CALL calc_mu_staggered ( mu, mub, muu, muv, &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte )
      itf=ite
      itf=MIN(ite,ide-1)
      jtf=jte
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
           rfield(i,k,j)=field(i,k,j)*(c1h(k)*muv(i,j)+c2h(k))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO
   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO
   ELSE IF (name .EQ. 'h')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
      ENDDO
      ENDDO
      ENDDO
   ELSE
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
      ENDDO
      ENDDO
      ENDDO
   ENDIF
END SUBROUTINE couple
SUBROUTINE calc_ww_cp ( u, v, mup, mub, c1h, c2h, ww, &
                        rdx, rdy, msftx, msfty, &
                        msfux, msfuy, msfvx, msfvx_inv, &
                        msfvy, dnw, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: u, v
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: mup, mub, &
                                                            msftx, msfty, &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msfvx_inv
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: dnw
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT ) :: ww
   REAL , INTENT(IN ) :: rdx, rdy
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION( its:ite ) :: dmdt
   REAL , DIMENSION( its:ite, kts:kte ) :: divv
   REAL , DIMENSION( its:ite+1, jts:jte+1 ) :: muu, muv
    jtf=MIN(jte,jde-1)
    ktf=MIN(kte,kde-1)
    itf=MIN(ite,ide-1)
      DO j=jts,jtf
      DO i=its,min(ite+1,ide)
        MUU(i,j) = 0.5*(MUP(i,j)+MUB(i,j)+MUP(i-1,j)+MUB(i-1,j))
      ENDDO
      ENDDO
      DO j=jts,min(jte+1,jde)
      DO i=its,itf
        MUV(i,j) = 0.5*(MUP(i,j)+MUB(i,j)+MUP(i,j-1)+MUB(i,j-1))
      ENDDO
      ENDDO
      DO j=jts,jtf
        DO i=its,ite
          dmdt(i) = 0.
          ww(i,1,j) = 0.
          ww(i,kte,j) = 0.
        ENDDO
        DO k=kts,ktf
        DO i=its,itf
          divv(i,k) = msftx(i,j)*dnw(k)*( rdx*((c1h(k)*muu(i+1,j)+c2h(k))*u(i+1,k,j)/msfuy(i+1,j)-(c1h(k)*muu(i,j)+c2h(k))*u(i,k,j)/msfuy(i,j)) &
                                        +rdy*((c1h(k)*muv(i,j+1)+c2h(k))*v(i,k,j+1)*msfvx_inv(i,j+1)-(c1h(k)*muv(i,j)+c2h(k))*v(i,k,j)*msfvx_inv(i,j)) )
          dmdt(i) = dmdt(i) + divv(i,k)
        ENDDO
        ENDDO
        DO k=2,ktf
        DO i=its,itf
           ww(i,k,j)=ww(i,k-1,j) - dnw(k-1)*c1h(k-1)*dmdt(i) - divv(i,k-1)
        ENDDO
        ENDDO
     ENDDO
END SUBROUTINE calc_ww_cp
SUBROUTINE calc_cq ( moist, cqu, cqv, cqw, n_moist, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN ) :: n_moist
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), INTENT(IN ) :: moist
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT( OUT) :: cqu, cqv, cqw
   REAL :: qtot(its:ite)
   INTEGER :: i, j, k, itf, jtf, ktf, ispe
      ktf=MIN(kte,kde-1)
      IF( n_moist >= PARAM_FIRST_SCALAR ) THEN
        itf=ite
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
          DO k=kts,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i-1,k,j,ispe)
              ENDDO
            ENDDO
            DO i=its,itf
              cqu(i,k,j) = 1./(1.+0.5*qtot(i))
            ENDDO
        ENDDO
        ENDDO
        itf=MIN(ite,ide-1)
        jtf=jte
        DO j=jts,jtf
          DO k=kts,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i,k,j-1,ispe)
              ENDDO
            ENDDO
            DO i = its,itf
               cqv(i,k,j) = 1./(1.+0.5*qtot(i))
            ENDDO
          ENDDO
        ENDDO
        itf=MIN(ite,ide-1)
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
          DO k=kts+1,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i,k-1,j,ispe)
              ENDDO
            ENDDO
            DO i = its,itf
              cqw(i,k,j) = 0.5*qtot(i)
            ENDDO
        ENDDO
        ENDDO
      ELSE
        itf=ite
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cqu(i,k,j) = 1.
        ENDDO
        ENDDO
        ENDDO
        itf=MIN(ite,ide-1)
        jtf=jte
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cqv(i,k,j) = 1.
        ENDDO
        ENDDO
        ENDDO
        itf=MIN(ite,ide-1)
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
        DO k=kts+1,ktf
        DO i=its,itf
           cqw(i,k,j) = 0.
        ENDDO
        ENDDO
        ENDDO
      END IF
END SUBROUTINE calc_cq
SUBROUTINE calc_alt ( alt, al, alb, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: alb, al
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT( OUT) :: alt
   INTEGER :: i, j, k, itf, jtf, ktf
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
        alt(i,k,j) = al(i,k,j)+alb(i,k,j)
      ENDDO
      ENDDO
      ENDDO
END SUBROUTINE calc_alt
SUBROUTINE calc_p_rho_phi ( moist, n_moist, hypsometric_opt, &
                            al, alb, mu, muts, &
                            c1, c2, c3h, c4h, c3f, c4f, &
                            ph, phb, p, pb, &
                            t, p0, t0, ptop, znu, znw, dnw, rdnw, &
                            rdn, non_hydrostatic, &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )
  IMPLICIT NONE
  LOGICAL , INTENT(IN ) :: non_hydrostatic
  INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
  INTEGER , INTENT(IN ) :: n_moist
  INTEGER , INTENT(IN ) :: hypsometric_opt
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN ) :: alb, &
                                                                   pb, &
                                                                   t
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ), INTENT(IN ) :: moist
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT( OUT) :: al, p
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: ph, phb
  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN ) :: mu, muts
  REAL, DIMENSION( kms:kme ), INTENT(IN ) :: znu, znw, dnw, rdnw, rdn
  REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1, c2, c3h, c4h, c3f, c4f
  REAL, INTENT(IN ) :: t0, p0, ptop
  INTEGER :: i, j, k, kk, itf, jtf, ktf, ispe
  REAL :: qvf, qtot, qf1, qf2
  REAL, DIMENSION( its:ite) :: temp,cpovcv_v
  REAL :: pfu, phm, pfd
  itf=MIN(ite,ide-1)
  jtf=MIN(jte,jde-1)
  ktf=MIN(kte,kde-1)
  cpovcv_v = cpovcv
  IF (non_hydrostatic) THEN
      IF (hypsometric_opt == 1) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          al(i,k,j)=-1./(c1(k)*muts(i,j)+c2(k))*(alb(i,k,j)*(c1(k)*mu(i,j)) + rdnw(k)*(ph(i,k+1,j)-ph(i,k,j)))
        END DO
        END DO
        END DO
      ELSE IF (hypsometric_opt == 2) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          pfu = c3f(k+1)*MUTS(i,j) + c4f(k+1) + ptop
          pfd = c3f(k )*MUTS(i,j) + c4f(k ) + ptop
          phm = c3h(k )*MUTS(i,j) + c4h(k ) + ptop
          al(i,k,j) = (ph(i,k+1,j)-ph(i,k,j)+phb(i,k+1,j)-phb(i,k,j))/phm/LOG(pfd/pfu)-alb(i,k,j)
        END DO
        END DO
        END DO
      ELSE
        CALL wrf_error_fatal3("<stdin>",847,&
'calc_p_rho_phi: hypsometric_opt should be 1 or 2' )
      END IF
      IF (n_moist >= PARAM_FIRST_SCALAR ) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          qvf = 1.+rvovrd*moist(i,k,j,P_QV)
          temp(i)=(r_d*(t0+t(i,k,j))*qvf)/(p0*(al(i,k,j)+alb(i,k,j)))
        ENDDO
        CALL vspow ( p(its,k,j), temp(its), cpovcv_v(its), itf-its+1 )
        DO i=its,itf
           p(i,k,j)= p(i,k,j)*p0-pb(i,k,j)
        ENDDO
        ENDDO
        ENDDO
      ELSE
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          p(i,k,j)=p0*( (r_d*(t0+t(i,k,j)))/ &
                        (p0*(al(i,k,j)+alb(i,k,j))) )**cpovcv &
                           -pb(i,k,j)
        ENDDO
        ENDDO
        ENDDO
      END IF
   ELSE
      IF (n_moist >= PARAM_FIRST_SCALAR ) THEN
        DO j=jts,jtf
          k=ktf
          DO i=its,itf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + moist(i,k,j,ispe)
            ENDDO
            qf2 = 1.
            qf1 = qtot*qf2
            p(i,k,j) = - 0.5*((c1(k)*mu(i,j))+qf1*(c1(k)*muts(i,j)+c2(k)))/rdnw(k)/qf2
            qvf = 1.+rvovrd*moist(i,k,j,P_QV)
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
          ENDDO
          DO k=ktf-1,kts,-1
            DO i=its,itf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + 0.5*( moist(i,k ,j,ispe) + moist(i,k+1,j,ispe) )
            ENDDO
            qf2 = 1.
            qf1 = qtot*qf2
            p(i,k,j) = p(i,k+1,j) - ((c1(k)*mu(i,j)) + qf1*(c1(k)*muts(i,j)+c2(k)))/qf2/rdn(k+1)
            qvf = 1.+rvovrd*moist(i,k,j,P_QV)
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            ENDDO
          ENDDO
        ENDDO
      ELSE
        DO j=jts,jtf
          k=ktf
          DO i=its,itf
            qtot = 0.
            qf2 = 1.
            qf1 = qtot*qf2
            p(i,k,j) = - 0.5*((c1(k)*mu(i,j))+qf1*(c1(k)*muts(i,j)+c2(k)))/rdnw(k)/qf2
            qvf = 1.
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
          ENDDO
          DO k=ktf-1,kts,-1
            DO i=its,itf
            qtot = 0.
            qf2 = 1.
            qf1 = qtot*qf2
            p(i,k,j) = p(i,k+1,j) - ((c1(k)*mu(i,j)) + qf1*(c1(k)*muts(i,j)+c2(k)))/qf2/rdn(k+1)
            qvf = 1.
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            ENDDO
          ENDDO
        ENDDO
     END IF
     IF (hypsometric_opt == 1) THEN
        DO j=jts,jtf
          DO kk=2,ktf+1
            k = kk-1
            DO i=its,itf
              ph(i,k+1,j) = ph(i,k,j) - (dnw(k))*( &
                           ((c1(k)*muts(i,j)+c2(k)))*al(i,k,j)+ &
                            (c1(k)*mu(i,j))*alb(i,k,j) )
            ENDDO
          ENDDO
        ENDDO
     ELSE
      DO j=jts,jtf
        DO i=its,itf
           ph(i,kts,j) = phb(i,kts,j)
        END DO
        DO k=kts+1,ktf+1
          DO i=its,itf
            pfu = c3f(k )*MUTS(i,j) + c4f(k ) + ptop
            pfd = c3f(k-1)*MUTS(i,j) + c4f(k-1) + ptop
            phm = c3h(k-1)*MUTS(i,j) + c4h(k-1) + ptop
            ph(i,k,j) = ph(i,k-1,j) + (al(i,k-1,j)+alb(i,k-1,j))*phm*LOG(pfd/pfu)
          ENDDO
        ENDDO
        DO k=kts,ktf+1
          DO i=its,itf
             ph(i,k,j) = ph(i,k,j) - phb(i,k,j)
          END DO
        END DO
      END DO
     END IF
   END IF
END SUBROUTINE calc_p_rho_phi
SUBROUTINE calc_php ( php, ph, phb, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: phb, ph
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT( OUT) :: php
   INTEGER :: i, j, k, itf, jtf, ktf
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
        php(i,k,j) = 0.5*(phb(i,k,j)+phb(i,k+1,j)+ph(i,k,j)+ph(i,k+1,j))
      ENDDO
      ENDDO
      ENDDO
END SUBROUTINE calc_php
SUBROUTINE diagnose_w( ph_tend, ph_new, ph_old, w, mut, &
                       c1f, c2f, dt, &
                       u, v, ht, &
                       cf1, cf2, cf3, rdx, rdy, &
                       msftx, msfty, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: ph_tend, &
                                                                     ph_new, &
                                                                     ph_old, &
                                                                     u, &
                                                                     v
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT( OUT) :: w
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mut, ht, msftx, msfty
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1f, c2f
   REAL, INTENT(IN ) :: dt, cf1, cf2, cf3, rdx, rdy
   INTEGER :: i, j, k, itf, jtf
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   DO j = jts, jtf
     DO i = its, itf
         w(i,1,j)= msfty(i,j)*.5*rdy*( &
                           (ht(i,j+1)-ht(i,j )) &
          *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1)) &
                          +(ht(i,j )-ht(i,j-1)) &
          *(cf1*v(i,1,j )+cf2*v(i,2,j )+cf3*v(i,3,j )) ) &
                 +msftx(i,j)*.5*rdx*( &
                           (ht(i+1,j)-ht(i,j )) &
          *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j)) &
                          +(ht(i,j )-ht(i-1,j)) &
          *(cf1*u(i ,1,j)+cf2*u(i ,2,j)+cf3*u(i ,3,j)) )
     ENDDO
     DO k = 2, kte
     DO i = its, itf
       w(i,k,j) = msfty(i,j)*( (ph_new(i,k,j)-ph_old(i,k,j))/dt &
                               - ph_tend(i,k,j)/(c1f(k)*mut(i,j)+c2f(k)) )/g
     ENDDO
     ENDDO
   ENDDO
END SUBROUTINE diagnose_w
SUBROUTINE rhs_ph( ph_tend, u, v, ww, &
                   ph, ph_old, phb, w, &
                   mut, muuf, muvf, &
                   c1f, c2f, &
                   fnm, fnp, &
                   rdnw, cfn, cfn1, rdx, rdy, &
                   msfux, msfuy, msfvx, &
                   msfvx_inv, msfvy, &
                   msftx, msfty, &
                   non_hydrostatic, &
                   config_flags, &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: &
                                                                     u, &
                                                                     v, &
                                                                     ww, &
                                                                     ph, &
                                                                     ph_old, &
                                                                     phb, &
                                                                    w
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: ph_tend
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: muuf, muvf, mut, &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msftx, msfty, &
                                                            msfvx_inv
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: rdnw, fnm, fnp
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1f, c2f
   REAL, INTENT(IN ) :: cfn, cfn1, rdx, rdy
   LOGICAL, INTENT(IN ) :: non_hydrostatic
   INTEGER :: i, j, k, itf, jtf, ktf, kz, i_start, j_start
   REAL :: ur, ul, ub, vr, vl, vb
   REAL, DIMENSION(its:ite,kts:kte) :: wdwn
   INTEGER :: advective_order
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   advective_order = config_flags%h_sca_adv_order
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)
   DO j = jts, jtf
     DO k = 2, kte
     DO i = its, itf
          wdwn(i,k) = .5*(ww(i,k,j)+ww(i,k-1,j))*rdnw(k-1) &
                        *(ph(i,k,j)-ph(i,k-1,j)+phb(i,k,j)-phb(i,k-1,j))
     ENDDO
     ENDDO
    DO k = 2, kte-1
    DO i = its, itf
          ph_tend(i,k,j) = ph_tend(i,k,j) &
                            - (fnm(k)*wdwn(i,k+1)+fnp(k)*wdwn(i,k))
    ENDDO
    ENDDO
   ENDDO
   IF (non_hydrostatic) THEN
   DO j = jts, jtf
     DO i = its, itf
        ph_tend(i,kde,j) = 0.
     ENDDO
     DO k = 2, kte
     DO i = its, itf
        ph_tend(i,k,j) = ph_tend(i,k,j) + (c1f(k)*mut(i,j)+c2f(k))*g*w(i,k,j)/msfty(i,j)
     ENDDO
     ENDDO
   ENDDO
   END IF
   IF (advective_order <= 2) THEN
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF ( (config_flags%open_ys .or. specified) .and. jts == jds ) j_start = jts+1
   IF ( (config_flags%open_ye .or. specified) .and. jte == jde ) jtf = jtf-2
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)* &
                  (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j )* &
                  (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)* &
                   (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                   +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j )* &
                   (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
   ENDDO
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF ( (config_flags%open_xs .or. specified) .and. its == ids ) i_start = its+1
   IF ( (config_flags%open_xe .or. specified) .and. ite == ide ) itf = itf-2
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))* &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)* &
                  (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                  +(c1f(k)*muuf(i ,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j)* &
                  (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))* &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)* &
                   (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                   +(c1f(k)*muuf(i ,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux( i,j)* &
                   (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
   ENDDO
   ELSE IF (advective_order <= 4) THEN
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF ( (config_flags%open_ys .or. specified) .and. jts == jds ) j_start = jts+2
   IF ( (config_flags%open_ye .or. specified) .and. jte == jde ) jtf = jtf-3
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j ))* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j ))* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
   ENDDO
   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+1 ) THEN
     j = jds+1
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)* &
                  (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j )* &
                  (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)* &
                   (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                   +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j )* &
                   (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
   END IF
   IF ( (config_flags%open_ye .or. specified) .and. jte >= jde-2 ) THEN
     j = jde-2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)* &
                  (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j )* &
                  (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)* &
                   (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                   +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j )* &
                   (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
   END IF
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF ( (config_flags%open_xs) .and. its == ids ) i_start = its+2
   IF ( (config_flags%open_xe) .and. ite == ide ) itf = itf-3
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j) )* (1./12.)*( &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                      -(ph(i+2,k,j)-ph(i-2,k,j)) &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                      -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux(i ,j) )* (1./12.)*( &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                      -(ph(i+2,k,j)-ph(i-2,k,j)) &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                      -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
     ENDDO
   ENDDO
   IF ( (config_flags%open_xs .or. specified) .and. its <= ids+1 ) THEN
     i = ids + 1
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))* &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)* &
                  (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                  +(c1f(k)*muuf(i ,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j)* &
                  (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
     ENDDO
     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))* &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)* &
                   (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                   +(c1f(k)*muuf(i ,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux( i,j)* &
                   (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
   END IF
   IF ( (config_flags%open_xe .or. specified) .and. ite >= ide-2 ) THEN
     i = ide-2
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))* &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)* &
                  (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                  +(c1f(k)*muuf(i ,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j)* &
                  (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
     ENDDO
     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))* &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)* &
                   (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                   +(c1f(k)*muuf(i ,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux( i,j)* &
                   (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
   END IF
   ELSE IF (advective_order <= 6) THEN
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF (config_flags%open_ys .or. specified ) j_start = max(jts,jds+3)
   IF (config_flags%open_ye .or. specified ) jtf = min(jtf,jde-4)
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j ) )* (1./60.)*( &
                   45.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                   -9.*(ph(i,k,j+2)-ph(i,k,j-2)) &
                      +(ph(i,k,j+3)-ph(i,k,j-3)) &
                  +45.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                   -9.*(phb(i,k,j+2)-phb(i,k,j-2)) &
                      +(phb(i,k,j+3)-phb(i,k,j-3)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j ) )* (1./60.)*( &
                   45.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                   -9.*(ph(i,k,j+2)-ph(i,k,j-2)) &
                      +(ph(i,k,j+3)-ph(i,k,j-3)) &
                  +45.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                   -9.*(phb(i,k,j+2)-phb(i,k,j-2)) &
                      +(phb(i,k,j+3)-phb(i,k,j-3)) ) )
     ENDDO
   ENDDO
   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+2 .and. jds+2 <= jte ) THEN
     j = jds+2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j ) )* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j) )* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
   END IF
   IF ( (config_flags%open_ye .or. specified) .and. jts <= jde-3 .and. jde-3 <= jte ) THEN
     j = jde-3
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j) )* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* ( &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j) )* (1./12.)*( &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1)) &
                      -(ph(i,k,j+2)-ph(i,k,j-2)) &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1)) &
                      -(phb(i,k,j+2)-phb(i,k,j-2)) ) )
     ENDDO
   END IF
   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+1 .and. jds+1 <= jte ) THEN
     j = jds+1
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)* &
                  (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j )* &
                  (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)* &
                   (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                   +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j )* &
                   (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
   END IF
   IF ( (config_flags%open_ye .or. specified) .and. jts <= jde-2 .and. jde-2 <= jte ) THEN
     j = jde-2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)* &
                  (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                  +(c1f(k)*muvf(i,j)+c2f(k))*(v(i,k,j )+v(i,k-1,j ))*msfvy(i,j )* &
                  (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)* &
                   (phb(i,k,j+1)-phb(i,k,j )+ph(i,k,j+1)-ph(i,k,j )) &
                   +(c1f(k)*muvf(i,j)+c2f(k))*(cfn*v(i,k-1,j )+cfn1*v(i,k-2,j ))*msfvy(i,j )* &
                   (phb(i,k,j )-phb(i,k,j-1)+ph(i,k,j )-ph(i,k,j-1)) )
     ENDDO
   END IF
   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   IF (config_flags%open_xs .or. specified ) i_start = max(its,ids+3)
   IF (config_flags%open_xe .or. specified ) itf = min(itf,ide-4)
   DO j = j_start, jtf
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(u(i,k,j )+u(i,k-1,j ))*msfux(i,j) )* (1./60.)*( &
                   45.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                   -9.*(ph(i+2,k,j)-ph(i-2,k,j)) &
                      +(ph(i+3,k,j)-ph(i-3,k,j)) &
                  +45.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                   -9.*(phb(i+2,k,j)-phb(i-2,k,j)) &
                      +(phb(i+3,k,j)-phb(i-3,k,j)) ) )
     ENDDO
     ENDDO
     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./60.)*( &
                   45.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                   -9.*(ph(i+2,k,j)-ph(i-2,k,j)) &
                      +(ph(i+3,k,j)-ph(i-3,k,j)) &
                  +45.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                   -9.*(phb(i+2,k,j)-phb(i-2,k,j)) &
                      +(phb(i+3,k,j)-phb(i-3,k,j)) ) )
     ENDDO
   ENDDO
   IF ( (config_flags%open_xs) .and. its <= ids+2 .and. ids+2 <= ite ) THEN
     i = ids + 2
     DO j = j_start, jtf
       DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(u(i,k,j )+u(i,k-1,j ))*msfux(i,j) )* (1./12.)*( &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                      -(ph(i+2,k,j)-ph(i-2,k,j)) &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                      -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
       ENDDO
       k = kte
       ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*( &
                ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j) &
                 +(c1f(k)*muuf(i,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./12.)*( &
                   8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                     -(ph(i+2,k,j)-ph(i-2,k,j)) &
                  +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                     -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
     ENDDO
   END IF
   IF ( (config_flags%open_xe) .and. its <= ide-3 .and. ide-3 <= ite ) THEN
     i = ide-3
     DO j = j_start, jtf
       DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*( &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j) &
                  +(c1f(k)*muuf(i,j)+c2f(k))*(u(i,k,j )+u(i,k-1,j ))*msfux(i,j) )* (1./12.)*( &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                      -(ph(i+2,k,j)-ph(i-2,k,j)) &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                      -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
       ENDDO
       k = kte
       ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*( &
                ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j) &
                 +(c1f(k)*muuf(i,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./12.)*( &
                   8.*(ph(i+1,k,j)-ph(i-1,k,j)) &
                     -(ph(i+2,k,j)-ph(i-2,k,j)) &
                  +8.*(phb(i+1,k,j)-phb(i-1,k,j)) &
                     -(phb(i+2,k,j)-phb(i-2,k,j)) ) )
     ENDDO
   END IF
   IF ( (config_flags%open_xs .or. specified) .and. its <= ids+1 .and. ids+1 <= ite ) THEN
     i = ids + 1
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))* &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)* &
                  (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                  +(c1f(k)*muuf(i ,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j)* &
                  (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
     ENDDO
     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))* &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)* &
                   (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                   +(c1f(k)*muuf(i ,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux( i,j)* &
                   (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
   END IF
   IF ( (config_flags%open_xe .or. specified) .and. its <= ide-2 .and. ide-2 <= ite ) THEN
     i = ide-2
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))* &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)* &
                  (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                  +(c1f(k)*muuf(i ,j)+c2f(k))*(u(i ,k,j)+u(i ,k-1,j))*msfux(i ,j)* &
                  (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
     ENDDO
     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))* &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)* &
                   (phb(i+1,k,j)-phb(i ,k,j)+ph(i+1,k,j)-ph(i ,k,j)) &
                   +(c1f(k)*muuf(i ,j)+c2f(k))*(cfn*u(i ,k-1,j)+cfn1*u(i ,k-2,j))*msfux( i,j)* &
                   (phb(i ,k,j)-phb(i-1,k,j)+ph(i ,k,j)-ph(i-1,k,j)) )
     ENDDO
   END IF
   END IF
   i_start = its
   itf=MIN(ite,ide-1)
   IF ( (config_flags%open_ys) .and. jts == jds ) THEN
     j=jts
     DO k=2,kde
       kz = min(k,kde-1)
       DO i = its,itf
         vb =.5*( fnm(kz)*(v(i,kz ,j+1)+v(i,kz ,j )) &
                 +fnp(kz)*(v(i,kz-1,j+1)+v(i,kz-1,j )) )
         vl=amin1(vb,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-rdy*(c1f(k)*mut(i,j)+c2f(k))*( &
                              +vl*(ph_old(i,k,j+1)-ph_old(i,k,j)))
       ENDDO
     ENDDO
   END IF
   IF ( (config_flags%open_ye) .and. jte == jde ) THEN
     j=jte-1
     DO k=2,kde
       kz = min(k,kde-1)
       DO i = its,itf
        vb=.5*( fnm(kz)*(v(i,kz ,j+1)+v(i,kz ,j)) &
               +fnp(kz)*(v(i,kz-1,j+1)+v(i,kz-1,j)) )
        vr=amax1(vb,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-rdy*(c1f(k)*mut(i,j)+c2f(k))*( &
                   +vr*(ph_old(i,k,j)-ph_old(i,k,j-1)))
       ENDDO
     ENDDO
   END IF
   j_start = its
   jtf=MIN(jte,jde-1)
   IF ( (config_flags%open_xs) .and. its == ids ) THEN
     i=its
     DO j = jts,jtf
       DO k=2,kde-1
         kz = k
         ub =.5*( fnm(kz)*(u(i+1,kz ,j)+u(i ,kz ,j)) &
                 +fnp(kz)*(u(i+1,kz-1,j)+u(i ,kz-1,j)) )
         ul=amin1(ub,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*( &
                              +ul*(ph_old(i+1,k,j)-ph_old(i,k,j)))
       ENDDO
         k = kde
         kz = k
         ub =.5*( fnm(kz)*(u(i+1,kz ,j)+u(i ,kz ,j)) &
                 +fnp(kz)*(u(i+1,kz-1,j)+u(i ,kz-1,j)) )
         ul=amin1(ub,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*( &
                              +ul*(ph_old(i+1,k,j)-ph_old(i,k,j)))
     ENDDO
   END IF
   IF ( (config_flags%open_xe) .and. ite == ide ) THEN
     i = ite-1
     DO j = jts,jtf
       DO k=2,kde-1
        kz = k
        ub=.5*( fnm(kz)*(u(i+1,kz ,j)+u(i,kz ,j)) &
               +fnp(kz)*(u(i+1,kz-1,j)+u(i,kz-1,j)) )
        ur=amax1(ub,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*( &
                   +ur*(ph_old(i,k,j)-ph_old(i-1,k,j)))
       ENDDO
        k = kde
        kz = k-1
        ub=.5*( fnm(kz)*(u(i+1,kz ,j)+u(i,kz ,j)) &
               +fnp(kz)*(u(i+1,kz-1,j)+u(i,kz-1,j)) )
        ur=amax1(ub,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*( &
                   +ur*(ph_old(i,k,j)-ph_old(i-1,k,j)))
     ENDDO
   END IF
  END SUBROUTINE rhs_ph
SUBROUTINE horizontal_pressure_gradient( ru_tend,rv_tend, &
                                         ph,alt,p,pb,al,php,cqu,cqv, &
                                         muu,muv,mu,c1h,c2h,fnm,fnp,rdnw,&
                                         cf1,cf2,cf3,cfn,cfn1, &
                                         rdx,rdy,msfux,msfuy,&
                                         msfvx,msfvy,msftx,msfty, &
                                         config_flags, non_hydrostatic, &
                                         top_lid, &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   LOGICAL, INTENT (IN ) :: non_hydrostatic, top_lid
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: &
                                                                     ph, &
                                                                     alt, &
                                                                     al, &
                                                                     p, &
                                                                     pb, &
                                                                     php, &
                                                                     cqu, &
                                                                     cqv
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: &
                                                                    ru_tend, &
                                                                    rv_tend
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: muu, muv, mu, &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msftx, msfty
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: rdnw, fnm, fnp
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1h, c2h
   REAL, INTENT(IN ) :: rdx, rdy, cf1, cf2, cf3, cfn, cfn1
   INTEGER :: i,j,k, itf, jtf, ktf, i_start, j_start
   REAL, DIMENSION( ims:ime, kms:kme ) :: dpn
   REAL :: dpx, dpy
   REAL, DIMENSION( kms:kme ) :: c1
   LOGICAL :: specified
   c1 = c1h
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   itf=MIN(ite,ide-1)
   jtf=jte
   ktf=MIN(kte,kde-1)
   i_start = its
   j_start = jts
   IF ( (config_flags%open_ys .or. specified .or. &
         config_flags%nested .or. config_flags%polar ) .and. jts == jds ) j_start = jts+1
   IF ( (config_flags%open_ye .or. specified .or. &
         config_flags%nested .or. config_flags%polar ) .and. jte == jde ) jtf = jtf-1
   DO j = j_start, jtf
     IF ( non_hydrostatic ) THEN
        k=1
        DO i = i_start, itf
          dpn(i,k) = .5*( cf1*(p(i,k ,j-1)+p(i,k ,j)) &
                         +cf2*(p(i,k+1,j-1)+p(i,k+1,j)) &
                         +cf3*(p(i,k+2,j-1)+p(i,k+2,j)) )
          dpn(i,kde) = 0.
        ENDDO
        IF (top_lid) THEN
          DO i = i_start, itf
            dpn(i,kde) = .5*( cfn *(p(i,kde-1,j-1)+p(i,kde-1,j)) &
                             +cfn1*(p(i,kde-2,j-1)+p(i,kde-2,j)) )
          ENDDO
        ENDIF
        DO k=2,ktf
          DO i = i_start, itf
            dpn(i,k) = .5*( fnm(k)*(p(i,k ,j-1)+p(i,k ,j)) &
                           +fnp(k)*(p(i,k-1,j-1)+p(i,k-1,j)) )
          END DO
        END DO
        DO K=1,ktf
          DO i = i_start, itf
            dpy = (msfvy(i,j)/msfvx(i,j))*.5*rdy*(c1h(k)*muv(i,j)+c2h(k))*( &
                     (ph (i,k+1,j)-ph (i,k+1,j-1) + ph(i,k,j)-ph(i,k,j-1)) &
                    +(alt(i,k ,j)+alt(i,k ,j-1))*(p (i,k,j)-p (i,k,j-1)) &
                    +(al (i,k ,j)+al (i,k ,j-1))*(pb(i,k,j)-pb(i,k,j-1)) )
            dpy = dpy + (msfvy(i,j)/msfvx(i,j))*rdy*(php(i,k,j)-php(i,k,j-1))* &
                (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*((c1(k)*mu(i,j-1))+(c1(k)*mu(i,j))))
            rv_tend(i,k,j) = rv_tend(i,k,j)-cqv(i,k,j)*dpy
          END DO
        END DO
     ELSE
        DO K=1,ktf
          DO i = i_start, itf
            dpy = (msfvy(i,j)/msfvx(i,j))*.5*rdy*(c1h(k)*muv(i,j)+c2h(k))*( &
                     (ph (i,k+1,j)-ph (i,k+1,j-1) + ph(i,k,j)-ph(i,k,j-1)) &
                    +(alt(i,k ,j)+alt(i,k ,j-1))*(p (i,k,j)-p (i,k,j-1)) &
                    +(al (i,k ,j)+al (i,k ,j-1))*(pb(i,k,j)-pb(i,k,j-1)) )
            rv_tend(i,k,j) = rv_tend(i,k,j)-dpy
          END DO
        END DO
     END IF
   ENDDO
   itf=ite
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)
   i_start = its
   j_start = jts
   IF ( (config_flags%open_xs .or. specified .or. &
           config_flags%nested ) .and. its == ids ) i_start = its+1
   IF ( (config_flags%open_xe .or. specified .or. &
           config_flags%nested ) .and. ite == ide ) itf = itf-1
   IF ( config_flags%periodic_x ) i_start = its
   IF ( config_flags%periodic_x ) itf=ite
   DO j = j_start, jtf
     IF ( non_hydrostatic ) THEN
        k=1
        DO i = i_start, itf
          dpn(i,k) = .5*( cf1*(p(i-1,k ,j)+p(i,k ,j)) &
                         +cf2*(p(i-1,k+1,j)+p(i,k+1,j)) &
                         +cf3*(p(i-1,k+2,j)+p(i,k+2,j)) )
          dpn(i,kde) = 0.
        ENDDO
        IF (top_lid) THEN
          DO i = i_start, itf
            dpn(i,kde) = .5*( cfn *(p(i-1,kde-1,j)+p(i,kde-1,j)) &
                             +cfn1*(p(i-1,kde-2,j)+p(i,kde-2,j)) )
          ENDDO
        ENDIF
        DO k=2,ktf
          DO i = i_start, itf
            dpn(i,k) = .5*( fnm(k)*(p(i-1,k ,j)+p(i,k ,j)) &
                           +fnp(k)*(p(i-1,k-1,j)+p(i,k-1,j)) )
          END DO
        END DO
        DO K=1,ktf
          DO i = i_start, itf
            dpx = (msfux(i,j)/msfuy(i,j))*.5*rdx*(c1h(k)*muu(i,j)+c2h(k))*( &
                        (ph (i,k+1,j)-ph (i-1,k+1,j) + ph(i,k,j)-ph(i-1,k,j)) &
                       +(alt(i,k ,j)+alt(i-1,k ,j))*(p (i,k,j)-p (i-1,k,j)) &
                       +(al (i,k ,j)+al (i-1,k ,j))*(pb(i,k,j)-pb(i-1,k,j)) )
            dpx = dpx + (msfux(i,j)/msfuy(i,j))*rdx*(php(i,k,j)-php(i-1,k,j))* &
                (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*((c1(k)*mu(i-1,j))+(c1(k)*mu(i,j))))
            ru_tend(i,k,j) = ru_tend(i,k,j)-cqu(i,k,j)*dpx
          END DO
        END DO
     ELSE
        DO K=1,ktf
          DO i = i_start, itf
            dpx = (msfux(i,j)/msfuy(i,j))*.5*rdx*(c1h(k)*muu(i,j)+c2h(k))*( &
                        (ph (i,k+1,j)-ph (i-1,k+1,j) + ph(i,k,j)-ph(i-1,k,j)) &
                       +(alt(i,k ,j)+alt(i-1,k ,j))*(p (i,k,j)-p (i-1,k,j)) &
                       +(al (i,k ,j)+al (i-1,k ,j))*(pb(i,k,j)-pb(i-1,k,j)) )
            ru_tend(i,k,j) = ru_tend(i,k,j)-dpx
          END DO
        END DO
     END IF
   ENDDO
END SUBROUTINE horizontal_pressure_gradient
SUBROUTINE pg_buoy_w( rw_tend, p, cqw, muf, mubf, &
                      c1f, c2f, &
                      rdnw, rdn, g, msftx, msfty, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: p
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: cqw
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: rw_tend
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mubf, muf, msftx, msfty
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: rdnw, rdn
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1f, c2f
   REAL, INTENT(IN ) :: g
   INTEGER :: itf, jtf, i, j, k
   REAL :: cq1, cq2
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   DO j = jts,jtf
     k=kde
     DO i=its,itf
       cq1 = 1./(1.+cqw(i,k-1,j))
       cq2 = cqw(i,k-1,j)*cq1
       rw_tend(i,k,j) = rw_tend(i,k,j)+(1./msfty(i,j))*g*( &
                        cq1*2.*rdnw(k-1)*( -p(i,k-1,j)) &
                        -(c1f(k)*muf(i,j))-cq2*(c1f(k)*mubf(i,j)+c2f(k)) )
     END DO
     DO k = 2, kde-1
     DO i = its,itf
      cq1 = 1./(1.+cqw(i,k,j))
      cq2 = cqw(i,k,j)*cq1
      cqw(i,k,j) = cq1
      rw_tend(i,k,j) = rw_tend(i,k,j)+(1./msfty(i,j))*g*( &
                       cq1*rdn(k)*(p(i,k,j)-p(i,k-1,j)) &
                       -(c1f(k)*muf(i,j))-cq2*(c1f(k)*mubf(i,j)+c2f(k)) )
     END DO
     ENDDO
   ENDDO
END SUBROUTINE pg_buoy_w
SUBROUTINE w_damp( rw_tend, max_vert_cfl, max_horiz_cfl, &
                   u, v, ww, w, mut, c1f, c2f, rdnw, &
                   rdx, rdy, msfux, msfuy, &
                   msfvx, msfvy, dt, &
                   config_flags, &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )
   USE module_llxy
   IMPLICIT NONE
   TYPE(grid_config_rec_type ) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: u, v, ww, w
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: rw_tend
   REAL, INTENT(OUT) :: max_vert_cfl
   REAL, INTENT(OUT) :: max_horiz_cfl
   REAL :: horiz_cfl
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mut
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: rdnw
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: c1f, c2f
   REAL, INTENT(IN) :: dt
   REAL, INTENT(IN) :: rdx, rdy
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, msfuy
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfvx, msfvy
   REAL :: vert_cfl, cf_n, cf_d, maxdub, maxdeta
   INTEGER :: itf, jtf, i, j, k, maxi, maxj, maxk
   INTEGER :: some
   CHARACTER*512 :: temp
   CHARACTER (LEN=256) :: time_str
   CHARACTER (LEN=256) :: grid_str
   integer :: total
   REAL :: msfuxt , msfxffl
   INTEGER, PARAMETER :: k_depth = 1000
   REAL :: w_crit_cfl = 1.00
   REAL :: w_alpha2
   LOGICAL :: print_flag = .true.
   SAVE :: print_flag
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   some = 0
   max_vert_cfl = 0.
   max_horiz_cfl = 0.
   total = 0
   IF( print_flag ) THEN
     w_crit_cfl = config_flags%w_crit_cfl
     write(wrf_err_message,*) '----------------------------------------'
     CALL wrf_debug( 0, wrf_err_message )
     WRITE(temp,*) 'W_DAMP CRITICAL COURANT NUMBER = ',w_crit_cfl
     CALL wrf_debug ( 0 , TRIM(temp) )
     write(wrf_err_message,*) '----------------------------------------'
     CALL wrf_debug( 0, wrf_err_message )
     print_flag = .false.
   ENDIF
   IF(config_flags%polar ) then
     msfxffl = 1.0/COS(config_flags%fft_filter_lat*degrad)
   END IF
   IF ( config_flags%w_damping == 1 ) THEN
     DO j = jts,jtf
     DO k = 2, kde-1
     DO i = its,itf
        IF(config_flags%polar ) then
           msfuxt = MIN(msfux(i,j), msfxffl)
        ELSE
           msfuxt = msfux(i,j)
        END IF
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)
        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl ; maxi = i ; maxj = j ; maxk = k
           maxdub = w(i,k,j) ; maxdeta = -1./rdnw(k)
        ENDIF
        horiz_cfl = max( abs(u(i,k,j) * rdx * msfuxt * dt), &
             abs(v(i,k,j) * rdy * msfvy(i,j) * dt) )
        if (horiz_cfl > max_horiz_cfl) then
           max_horiz_cfl = horiz_cfl
        endif
        if(vert_cfl .gt. w_crit_cfl)then
           WRITE(temp,FMT="(3(1x,i5,1x),'W: ',f7.4,2x,'W-CFL: ',f7.4,2x,'dETA: ',f7.4))") i,j,k,vert_cfl,w(i,k,j),-1./rdnw(k)
           CALL wrf_debug ( 100 , TRIM(temp) )
           if ( vert_cfl > w_crit_cfl ) some = some + 1
            IF( k .le. k_depth ) THEN
               rw_tend(i,k,j) = rw_tend(i,k,j)-sign(1.,w(i,k,j))*w_alpha*(vert_cfl-w_crit_cfl)*(c1f(k)*mut(i,j)+c2f(k))
            ENDIF
        ENDIF
     END DO
     ENDDO
     ENDDO
   ELSE
     DO j = jts,jtf
     DO k = 2, kde-1
     DO i = its,itf
        IF(config_flags%polar ) then
           msfuxt = MIN(msfux(i,j), msfxffl)
        ELSE
           msfuxt = msfux(i,j)
        END IF
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)
        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl ; maxi = i ; maxj = j ; maxk = k
           maxdub = w(i,k,j) ; maxdeta = -1./rdnw(k)
        ENDIF
        horiz_cfl = max( abs(u(i,k,j) * rdx * msfuxt * dt), abs(v(i,k,j) * rdy * msfvy(i,j) * dt) )
        if (horiz_cfl > max_horiz_cfl) then
           max_horiz_cfl = horiz_cfl
        endif
        if(vert_cfl .gt. w_crit_cfl)then
           WRITE(temp,FMT="(3(1x,i5,1x),'W: ',f7.2,2x,'W-CFL: ',f7.2,2x,'dETA: ',f7.2))") i,j,k,vert_cfl,w(i,k,j),-1./rdnw(k)
           CALL wrf_debug ( 100 , TRIM(temp) )
           if ( vert_cfl > w_crit_cfl ) some = some + 1
        endif
     END DO
     ENDDO
     ENDDO
   ENDIF
   IF ( some .GT. 0 ) THEN
     CALL get_current_time_string( time_str )
     CALL get_current_grid_name( grid_str )
     WRITE(temp,*)some, &
            ' points exceeded cfl=w_crit_cfl in domain '//TRIM(grid_str)//' at time '//TRIM(time_str)//' hours'
     CALL wrf_debug ( 0 , TRIM(temp) )
     WRITE(temp,FMT="('Max   W: ',3(1x,i5,1x),'W: ',f7.2,2x,'W-CFL: ',f7.2,2x,'dETA: ',f7.2))") &
           maxi, maxj, maxk, maxdub, max_vert_cfl, maxdeta
     CALL wrf_debug ( 0 , TRIM(temp) )
     WRITE(temp,FMT="('Max U/V: ',3(1x,i5,1x),'U: ',f7.2,2x,'U-CFL: ',f7.2,2x,'V: ',f7.2,2x,'V-CFL: ',f7.2))") &
           maxi, maxj, maxk, u(maxi,maxk,maxj), dt*u(maxi,maxk,maxj)*rdx, v(maxi,maxk,maxj), dt*v(maxi,maxk,maxj)*rdy
     CALL wrf_debug ( 0 , TRIM(temp) )
   ENDIF
END SUBROUTINE w_damp
SUBROUTINE WW_SPLIT(wwE, wwI, ph, phb, &
                    u, v, ww, w, mut, rdnw, msfty, &
                    c1f, c2f, &
                    rdx, rdy, msfux, msfuy, &
                    msfvx, msfvy, dt, &
                    config_flags, rk_step, &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )
   TYPE( grid_config_rec_type ) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, &
                                       rk_step
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN ) :: u, v, ww, w, ph, phb
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: wwE, wwI
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mut
   REAL, DIMENSION( kms:kme ), INTENT(IN ) :: rdnw, c1f, c2f
   REAL, INTENT(IN) :: dt
   REAL, INTENT(IN) :: rdx, rdy
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, msfuy
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfvx, msfvy, msfty
   REAL(KIND=4) :: cr, cx, cy, c0, c2d, cw_max2, cw_min, cw, cff, rdz
   INTEGER :: i, j, k, ii, jj, i_start, i_end, j_start, j_end, zadvect_implicit, rk_order
   LOGICAL :: print_flag = .true.
   SAVE :: print_flag
   REAL, PARAMETER :: alpha_max = 1.0
   REAL, PARAMETER :: alpha_min = 0.8
   REAL, PARAMETER :: cmnx_ratio = alpha_min/alpha_max
   REAL, PARAMETER :: cutoff = 2.0 - cmnx_ratio
   REAL, PARAMETER :: r4cmx = 1.0/(4.0 - 4.0*cmnx_ratio )
   REAL, PARAMETER :: Ceps = 0.90
   REAL, PARAMETER :: binfrac = 0.1
   INTEGER, PARAMETER :: binsize = 11
   INTEGER :: count(binsize)
   real(KIND=4) :: binavg(binsize), crs(binsize)
   zadvect_implicit = config_flags%zadvect_implicit
   rk_order = config_flags%rk_ord
     IF( print_flag ) THEN
       write(wrf_err_message,*) '----------------------------------------'
       CALL wrf_debug( 0, wrf_err_message )
       write(wrf_err_message,*) 'WW_SPLIT:  ZADVECT_IMPLICT = ', zadvect_implicit
       CALL wrf_debug( 0, wrf_err_message )
       write(wrf_err_message,*) 'WW_SPLIT:  dt = ', dt
       CALL wrf_debug( 0, wrf_err_message )
       write(wrf_err_message,*) 'WW_SPLIT:  alpha_max/min = ', alpha_max, alpha_min
       CALL wrf_debug( 0, wrf_err_message )
       print_flag = .false.
      ENDIF
   i_start = its-1
   i_end = MIN(ite,ide-1) + 1
   j_start = jts - 1
   j_end = MIN(jte,jde-1) + 1
   IF( zadvect_implicit == 0 .or. rk_step < rk_order ) THEN
     DO j = j_start, j_end
       DO k = kds, kde
         DO i = i_start,i_end
           wwE(i,k,j) = ww(i,k,j)
           wwI(i,k,j) = 0.0
         ENDDO
       ENDDO
     ENDDO
   ELSE
     binavg(:) = 0.0
     crs(:) = 0.0
     count(:) = 0
     DO j = j_start, j_end
       DO k = 2, kde-1
         IF( k == 2 ) THEN
           wwE(i_start:i_end,kde,j) = ww(i_start:i_end,1,j)
           wwI(i_start:i_end,1, j) = 0.0
         ENDIF
         IF( k == kde-1 ) THEN
           wwE(i_start:i_end,kde,j) = ww(i_start:i_end,kde,j)
           wwI(i_start:i_end,kde,j) = 0.0
         ENDIF
         DO i = i_start,i_end
           wfrac = 1.0
           IF( ww(i,k,j) < 0.0 ) THEN
             cx = msfty(i,j)*rdx*(max(u(i+1,k,j), 0.0) - min(u(i,k,j), 0.0))
             cy = msfty(i,j)*rdy*(max(v(i,k,j+1), 0.0) - min(v(i,k,j), 0.0))
           ELSE
             cx = msfty(i,j)*rdx*(max(u(i+1,k-1,j), 0.0) - min(u(i,k-1,j), 0.0))
             cy = msfty(i,j)*rdy*(max(v(i,k-1,j+1), 0.0) - min(v(i,k-1,j), 0.0))
           ENDIF
           cw_max = alpha_max - dt*Ceps*(cx + cy)
           cr = ww(i,k,j) * dt * rdnw(k) / (c1f(k)*mut(i,j)+c2f(k))
           IF( cw_max > 0.0 ) THEN
             cw_max2 = cw_max**2
             cw_min = cw_max*cmnx_ratio
             cw = abs(cr)
             if ( cw < cw_min ) then
               cff = cw_max2
             elseif ( cw < cutoff*cw_min ) then
               cff = cw_max2 + r4cmx*(cw-cw_min)**2
             else
               cff = cw_max*cw
             endif
             wfrac = cw_max2 / cff
             wfrac = amax1(amin1(wfrac, 1.0), 0.0)
             wwE(i,k,j) = ww(i,k,j) * wfrac
             wwI(i,k,j) = ww(i,k,j) * (1.0 - wfrac)
           ELSE
             wwE(i,k,j) = 0.0
             wwI(i,k,j) = ww(i,k,j)
           ENDIF
         ENDDO
       ENDDO
     ENDDO
   ENDIF
 RETURN
 END SUBROUTINE WW_SPLIT
SUBROUTINE horizontal_diffusion ( name, field, tendency, MUT, c1, c2, &
                                  config_flags, &
                                  msfux, msfuy, msfvx, msfvx_inv, &
                                  msfvy, msftx, msfty, &
                                  khdif, xkmhd, rdx, rdy, &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte
   CHARACTER(LEN=1) , INTENT(IN ) :: name
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: field, xkmhd
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: MUT
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, &
                                                                    msfuy, &
                                                                    msfvx, &
                                                                    msfvx_inv, &
                                                                    msfvy, &
                                                                    msftx, &
                                                                    msfty
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1, c2
   REAL , INTENT(IN ) :: rdx, &
                                                                rdy, &
                                                                khdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL :: mrdx, mkrdxm, mkrdxp, &
           mrdy, mkrdym, mkrdyp
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
   IF (name .EQ. 'u') THEN
      i_start = its
      i_end = ite
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-1,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mkrdxm=(msftx(i-1,j)/msfty(i-1,j))*(c1(k)*MUT(i-1,j)+c2(k))*xkmhd(i-1,k,j)*rdx
         mkrdxp=(msftx(i,j)/msfty(i,j))*(c1(k)*MUT(i,j)+c2(k))*xkmhd(i,k,j)*rdx
         mrdx=msfux(i,j)*msfuy(i,j)*rdx
         mkrdym=( (msfuy(i,j)+msfuy(i,j-1))/(msfux(i,j)+msfux(i,j-1)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i-1,j-1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i-1,k,j-1)+xkmhd(i-1,k,j))*rdy
         mkrdyp=( (msfuy(i,j)+msfuy(i,j+1))/(msfux(i,j)+msfux(i,j+1)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i-1,j+1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j+1)+xkmhd(i-1,k,j+1)+xkmhd(i-1,k,j))*rdy
         mrdy=msfux(i,j)*msfuy(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i ,k,j)) &
                                 -mkrdxm*(field(i ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j )) &
                                 -mkrdym*(field(i,k,j )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   ELSE IF (name .EQ. 'v')THEN
      i_start = its
      i_end = MIN(ite,ide-1)
      j_start = jts
      j_end = jte
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-1,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
      IF ( config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%polar ) j_end = MIN(jde-1,jte)
      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mkrdxm=( (msfvx(i,j)+msfvx(i-1,j))/(msfvy(i,j)+msfvy(i-1,j)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i-1,j-1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i-1,k,j-1)+xkmhd(i-1,k,j))*rdx
         mkrdxp=( (msfvx(i,j)+msfvx(i+1,j))/(msfvy(i,j)+msfvy(i+1,j)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i+1,j-1)+c2(k))+(c1(k)*MUT(i+1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i+1,k,j-1)+xkmhd(i+1,k,j))*rdx
         mrdx=msfvx(i,j)*msfvy(i,j)*rdx
         mkrdym=(msfty(i,j-1)/msftx(i,j-1))*xkmhd(i,k,j-1)*rdy
         mkrdyp=(msfty(i,j)/msftx(i,j))*xkmhd(i,k,j)*rdy
         mrdy=msfvx(i,j)*msfvy(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i ,k,j)) &
                                 -mkrdxm*(field(i ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j )) &
                                 -mkrdym*(field(i,k,j )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   ELSE IF (name .EQ. 'w')THEN
      i_start = its
      i_end = MIN(ite,ide-1)
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end
         mkrdxm=(msfux(i,j)/msfuy(i,j))* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i-1,k,j)+xkmhd(i,k-1,j)+xkmhd(i-1,k-1,j))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))* &
                0.25*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))* &
                0.25*(xkmhd(i+1,k,j)+xkmhd(i,k,j)+xkmhd(i+1,k-1,j)+xkmhd(i,k-1,j))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i,k-1,j)+xkmhd(i,k-1,j-1))*rdy
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))* &
                0.25*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j+1)+xkmhd(i,k,j)+xkmhd(i,k-1,j+1)+xkmhd(i,k-1,j))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i ,k,j)) &
                                 -mkrdxm*(field(i ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j )) &
                                 -mkrdym*(field(i,k,j )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   ELSE
      i_start = its
      i_end = MIN(ite,ide-1)
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mkrdxm=(msfux(i,j)/msfuy(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i-1,k,j))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))*0.5*(xkmhd(i+1,k,j)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i ,k,j)) &
                                 -mkrdxm*(field(i ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j )) &
                                 -mkrdym*(field(i,k,j )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   ENDIF
END SUBROUTINE horizontal_diffusion
SUBROUTINE horizontal_diffusion_3dmp ( name, field, tendency, MUT, c1, c2, &
                                       config_flags, base_3d, &
                                       msfux, msfuy, msfvx, msfvx_inv, &
                                       msfvy, msftx, msfty, &
                                       khdif, xkmhd, rdx, rdy, &
                                       ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte
   CHARACTER(LEN=1) , INTENT(IN ) :: name
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: field, &
                                                                      xkmhd, &
                                                                      base_3d
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: MUT
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, &
                                                                    msfuy, &
                                                                    msfvx, &
                                                                    msfvx_inv, &
                                                                    msfvy, &
                                                                    msftx, &
                                                                    msfty
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1, c2
   REAL , INTENT(IN ) :: rdx, &
                                                                rdy, &
                                                                khdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL :: mrdx, mkrdxm, mkrdxp, &
           mrdy, mkrdym, mkrdyp
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
      i_start = its
      i_end = MIN(ite,ide-1)
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mkrdxm=(msfux(i,j)/msfuy(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i-1,k,j))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))*0.5*(xkmhd(i+1,k,j)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)+( &
                    mrdx*( mkrdxp*( field(i+1,k,j) -field(i ,k,j) &
                                   -base_3d(i+1,k,j)+base_3d(i ,k,j) ) &
                          -mkrdxm*( field(i ,k,j) -field(i-1,k,j) &
                                   -base_3d(i ,k,j)+base_3d(i-1,k,j) ) ) &
                   +mrdy*( mkrdyp*( field(i,k,j+1) -field(i,k,j ) &
                                   -base_3d(i,k,j+1)+base_3d(i,k,j ) ) &
                          -mkrdym*( field(i,k,j ) -field(i,k,j-1) &
                                   -base_3d(i,k,j )+base_3d(i,k,j-1) ) ) &
                                                                         )
      ENDDO
      ENDDO
      ENDDO
END SUBROUTINE horizontal_diffusion_3dmp
SUBROUTINE vertical_diffusion ( name, field, tendency, &
                                config_flags, c1, c2, &
                                alt, MUT, rdn, rdnw, kvdif, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   CHARACTER(LEN=1) , INTENT(IN ) :: name
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: field, &
                                                                alt
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: MUT
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: rdn, rdnw
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1, c2
   REAL , INTENT(IN ) :: kvdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION(its:ite, jts:jte) :: vfluxm, vfluxp, zz
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux
   REAL :: rdz
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
   IF (name .EQ. 'w')THEN
   i_start = its
   i_end = MIN(ite,ide-1)
   j_start = jts
   j_end = MIN(jte,jde-1)
j_loop_w : DO j = j_start, j_end
     DO k=kts,ktf-1
       DO i = i_start, i_end
          vflux(i,k)= (kvdif/alt(i,k,j))*rdnw(k)*(field(i,k+1,j)-field(i,k,j))
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts+1,ktf
       DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j) &
                              +rdn(k)*g*g/(c1(k)*MUT(i,j)+c2(k))/(0.5*(alt(i,k,j)+alt(i,k-1,j))) &
                                         *(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
    ENDDO j_loop_w
   ELSE IF(name .EQ. 'm')THEN
     i_start = its
     i_end = MIN(ite,ide-1)
     j_start = jts
     j_end = MIN(jte,jde-1)
j_loop_s : DO j = j_start, j_end
     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j))) &
                  *(field(i,k+1,j)-field(i,k,j))
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j) &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
 ENDDO j_loop_s
   ENDIF
END SUBROUTINE vertical_diffusion
SUBROUTINE vertical_diffusion_mp ( field, tendency, config_flags, &
                                   base, c1, c2, &
                                   alt, MUT, rdn, rdnw, kvdif, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: field, &
                                                                alt
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: MUT
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: rdn, &
                                                                  rdnw, &
                                                                  base
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1, c2
   REAL , INTENT(IN ) :: kvdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux
   REAL :: rdz
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
     i_start = its
     i_end = MIN(ite,ide-1)
     j_start = jts
     j_end = MIN(jte,jde-1)
j_loop_s : DO j = j_start, j_end
     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j))) &
                    *(field(i,k+1,j)-field(i,k,j)-base(k+1)+base(k))
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j) &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
 ENDDO j_loop_s
END SUBROUTINE vertical_diffusion_mp
SUBROUTINE vertical_diffusion_3dmp ( field, tendency, config_flags, &
                                     base_3d, c1, c2, &
                                     alt, MUT, rdn, rdnw, kvdif, &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: field, &
                                                                alt, &
                                                                base_3d
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: MUT
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: rdn, &
                                                                  rdnw
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1, c2
   REAL , INTENT(IN ) :: kvdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux
   REAL :: rdz
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
     i_start = its
     i_end = MIN(ite,ide-1)
     j_start = jts
     j_end = MIN(jte,jde-1)
j_loop_s : DO j = j_start, j_end
     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j))) &
                    *( field(i,k+1,j) -field(i,k,j) &
                      -base_3d(i,k+1,j)+base_3d(i,k,j) )
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j) &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
 ENDDO j_loop_s
END SUBROUTINE vertical_diffusion_3dmp
SUBROUTINE vertical_diffusion_u ( field, tendency, &
                                  config_flags, u_base, c1h,c2h,&
                                  alt, muu, rdn, rdnw, kvdif, &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: field, &
                                                                alt
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: muu
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: rdn, rdnw, u_base
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h
   REAL , INTENT(IN ) :: kvdif
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux
   REAL :: rdz, zz
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
      i_start = its
      i_end = ite
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
j_loop_u : DO j = j_start, j_end
     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.25*( alt(i ,k ,j) &
                                        +alt(i-1,k ,j) &
                                        +alt(i ,k+1,j) &
                                        +alt(i-1,k+1,j) ) ) &
                             *(field(i,k+1,j)-field(i,k,j) &
                               -u_base(k+1) +u_base(k) )
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts,ktf-1
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+ &
                g*g*rdnw(k)/(c1h(k)*muu(i,j)+c2h(k))/(0.5*(alt(i-1,k,j)+alt(i,k,j)))* &
                              (vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
 ENDDO j_loop_u
END SUBROUTINE vertical_diffusion_u
SUBROUTINE vertical_diffusion_v ( field, tendency, &
                                  config_flags, v_base, c1h,c2h,&
                                  alt, muv, rdn, rdnw, kvdif, &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: field, &
                                                                alt
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: rdn, rdnw, v_base
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: muv
   REAL , INTENT(IN ) :: kvdif
   INTEGER :: i, j, k, itf, jtf, ktf, jm1
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux
   REAL :: rdz, zz
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
      i_start = its
      i_end = MIN(ite,ide-1)
      j_start = jts
      j_end = MIN(jte,jde-1)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end = MIN(jde-1,jte)
j_loop_v : DO j = j_start, j_end
     jm1 = j-1
     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.25*( alt(i,k ,j ) &
                                        +alt(i,k ,jm1) &
                                        +alt(i,k+1,j ) &
                                        +alt(i,k+1,jm1) ) ) &
                             *(field(i,k+1,j)-field(i,k,j) &
                               -v_base(k+1) +v_base(k) )
       ENDDO
     ENDDO
     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO
     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO
     DO k=kts,ktf-1
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+ &
                g*g*rdnw(k)/(c1h(k)*muv(i,j)+c2h(k))/(0.5*(alt(i,k,jm1)+alt(i,k,j)))* &
                              (vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO
 ENDDO j_loop_v
END SUBROUTINE vertical_diffusion_v
SUBROUTINE calculate_full ( rfield, rfieldb, rfieldp, &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: rfieldb, &
                                                                      rfieldp
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT ) :: rfield
   INTEGER :: i, j, k, itf, jtf, ktf
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)
   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
      rfield(i,k,j)=rfieldb(i,k,j)+rfieldp(i,k,j)
   ENDDO
   ENDDO
   ENDDO
END SUBROUTINE calculate_full
SUBROUTINE coriolis ( ru, rv, rw, ru_tend, rv_tend, rw_tend, &
                      config_flags, &
                      msftx, msfty, msfux, msfuy, &
                      msfvx, msfvy, &
                      f, e, sina, cosa, fzm, fzp, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: ru, &
                                                                rv, &
                                                                rw
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, &
                                                                msfuy, &
                                                                msfvx, &
                                                                msfvy, &
                                                                msftx, &
                                                                msfty
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: f, &
                                                                    e, &
                                                                    sina, &
                                                                    cosa
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: fzm, &
                                                                  fzp
   INTEGER :: i, j , k, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
   i_start = its
   i_end = ite
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
   DO j = jts, MIN(jte,jde-1)
   DO k=kts,ktf
   DO i = i_start, i_end
     ru_tend(i,k,j)=ru_tend(i,k,j) + (msfux(i,j)/msfuy(i,j))*0.5*(f(i,j)+f(i-1,j)) &
       *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
           - 0.5*(e(i,j)+e(i-1,j))*0.5*(cosa(i,j)+cosa(i-1,j)) &
       *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))
   ENDDO
   ENDDO
   ENDDO
   j_start = jts
   j_end = jte
   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end = MIN(jde-1,jte)
   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=its,MIN(ide-1,ite)
      rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*0.5*(f(i,j)+f(i,j-1)) &
       *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
           + (msfvy(i,j)/msfvx(i,j))*0.5*(e(i,j)+e(i,j-1))*0.5*(sina(i,j)+sina(i,j-1)) &
           *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))
   ENDDO
   ENDDO
   ENDDO
   DO j=jts,MIN(jte, jde-1)
   DO k=kts+1,ktf
   DO i=its,MIN(ite, ide-1)
       rw_tend(i,k,j)=rw_tend(i,k,j) + e(i,j)* &
          (cosa(i,j)*0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j)) &
          +fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j))) &
          -(msftx(i,j)/msfty(i,j))* &
           sina(i,j)*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1)) &
          +fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))))
   ENDDO
   ENDDO
   ENDDO
END SUBROUTINE coriolis
SUBROUTINE perturbation_coriolis ( ru_in, rv_in, rw, ru_tend, rv_tend, rw_tend, &
                                   config_flags, &
                                   u_base, v_base, z_base, &
                                   muu, muv, c1h, c2h, phb, ph, &
                                   msftx, msfty, msfux, msfuy, msfvx, msfvy, &
                                   f, e, sina, cosa, fzm, fzp, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN ) :: ru_in, &
                                                                      rv_in, &
                                                                      rw, &
                                                                      ph, &
                                                                      phb
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, &
                                                                msfuy, &
                                                                msfvx, &
                                                                msfvy, &
                                                                msftx, &
                                                                msfty
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: f, &
                                                                    e, &
                                                                    sina, &
                                                                    cosa
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: muu, &
                                                                    muv
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: fzm, &
                                                                  fzp
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: u_base, &
                                                                  v_base, &
                                                                  z_base
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) :: ru, &
                                                      rv
   REAL :: z_at_u, z_at_v, wkp1, wk, wkm1
   INTEGER :: i, j , k, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
   ktf=MIN(kte,kde-1)
   i_start = its
   i_end = ite
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
   DO j = jts, MIN(jte,jde-1)+1
   DO k=kts+1,ktf-1
   DO i = i_start-1, i_end
     z_at_v = 0.25*( phb(i,k,j )+phb(i,k+1,j ) &
                    +phb(i,k,j-1)+phb(i,k+1,j-1) &
                    +ph(i,k,j )+ph(i,k+1,j ) &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkp1 = min(1.,max(0.,z_at_v-z_base(k))/(z_base(k+1)-z_base(k)))
     wkm1 = min(1.,max(0.,z_base(k)-z_at_v)/(z_base(k)-z_base(k-1)))
     wk = 1.-wkp1-wkm1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*( &
                                  wkm1*v_base(k-1) &
                                 +wk *v_base(k ) &
                                 +wkp1*v_base(k+1) )
   ENDDO
   ENDDO
   ENDDO
   DO j = jts, MIN(jte,jde-1)+1
   DO i = i_start-1, i_end
     k = kts
     z_at_v = 0.25*( phb(i,k,j )+phb(i,k+1,j ) &
                    +phb(i,k,j-1)+phb(i,k+1,j-1) &
                    +ph(i,k,j )+ph(i,k+1,j ) &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkp1 = min(1.,max(0.,z_at_v-z_base(k))/(z_base(k+1)-z_base(k)))
     wk = 1.-wkp1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*( &
                                 +wk *v_base(k ) &
                                 +wkp1*v_base(k+1) )
     k = ktf
     z_at_v = 0.25*( phb(i,k,j )+phb(i,k+1,j ) &
                    +phb(i,k,j-1)+phb(i,k+1,j-1) &
                    +ph(i,k,j )+ph(i,k+1,j ) &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkm1 = min(1.,max(0.,z_base(k)-z_at_v)/(z_base(k)-z_base(k-1)))
     wk = 1.-wkm1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*( &
                                  wkm1*v_base(k-1) &
                                 +wk *v_base(k ) )
   ENDDO
   ENDDO
   DO j = jts, MIN(jte,jde-1)
   DO k=kts,ktf
     DO i = i_start, i_end
       ru_tend(i,k,j)=ru_tend(i,k,j) + (msfux(i,j)/msfuy(i,j))*0.5*(f(i,j)+f(i-1,j)) &
         *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
             - 0.5*(e(i,j)+e(i-1,j))*0.5*(cosa(i,j)+cosa(i-1,j)) &
         *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))
     ENDDO
   ENDDO
   IF ( (config_flags%open_xs) .and. (its == ids) ) THEN
     DO k=kts,ktf
       ru_tend(its,k,j)=ru_tend(its,k,j) + (msfux(its,j)/msfuy(its,j))*0.5*(f(its,j)+f(its,j)) &
         *0.25*(rv(its,k,j+1)+rv(its,k,j+1)+rv(its,k,j)+rv(its,k,j)) &
             - 0.5*(e(its,j)+e(its,j))*0.5*(cosa(its,j)+cosa(its,j)) &
         *0.25*(rw(its,k+1,j)+rw(its,k,j)+rw(its,k+1,j)+rw(its,k,j))
     ENDDO
   ENDIF
   IF ( (config_flags%open_xe) .and. (ite == ide) ) THEN
     DO k=kts,ktf
       ru_tend(ite,k,j)=ru_tend(ite,k,j) + (msfux(ite,j)/msfuy(ite,j))*0.5*(f(ite-1,j)+f(ite-1,j)) &
         *0.25*(rv(ite-1,k,j+1)+rv(ite-1,k,j+1)+rv(ite-1,k,j)+rv(ite-1,k,j)) &
             - 0.5*(e(ite-1,j)+e(ite-1,j))*0.5*(cosa(ite-1,j)+cosa(ite-1,j)) &
         *0.25*(rw(ite-1,k+1,j)+rw(ite-1,k,j)+rw(ite-1,k+1,j)+rw(ite-1,k,j))
     ENDDO
   ENDIF
   ENDDO
   j_start = jts
   j_end = jte
   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end = MIN(jde-1,jte)
   DO j = j_start-1,j_end
   DO k=kts+1,ktf-1
   DO i = its, MIN(ite,ide-1)+1
     z_at_u = 0.25*( phb(i ,k,j)+phb(i ,k+1,j) &
                    +phb(i-1,k,j)+phb(i-1,k+1,j) &
                    +ph(i ,k,j)+ph(i ,k+1,j) &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkp1 = min(1.,max(0.,z_at_u-z_base(k))/(z_base(k+1)-z_base(k)))
     wkm1 = min(1.,max(0.,z_base(k)-z_at_u)/(z_base(k)-z_base(k-1)))
     wk = 1.-wkp1-wkm1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*( &
                                  wkm1*u_base(k-1) &
                                 +wk *u_base(k ) &
                                 +wkp1*u_base(k+1) )
   ENDDO
   ENDDO
   ENDDO
   DO j = j_start-1,j_end
   DO i = its, MIN(ite,ide-1)+1
     k = kts
     z_at_u = 0.25*( phb(i ,k,j)+phb(i ,k+1,j) &
                    +phb(i-1,k,j)+phb(i-1,k+1,j) &
                    +ph(i ,k,j)+ph(i ,k+1,j) &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkp1 = min(1.,max(0.,z_at_u-z_base(k))/(z_base(k+1)-z_base(k)))
     wk = 1.-wkp1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*( &
                                 +wk *u_base(k ) &
                                 +wkp1*u_base(k+1) )
     k = ktf
     z_at_u = 0.25*( phb(i ,k,j)+phb(i ,k+1,j) &
                    +phb(i-1,k,j)+phb(i-1,k+1,j) &
                    +ph(i ,k,j)+ph(i ,k+1,j) &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkm1 = min(1.,max(0.,z_base(k)-z_at_u)/(z_base(k)-z_base(k-1)))
     wk = 1.-wkm1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*( &
                                  wkm1*u_base(k-1) &
                                 +wk *u_base(k ) )
   ENDDO
   ENDDO
   IF ( (config_flags%open_ys) .and. (jts == jds) ) THEN
     DO k=kts,ktf
     DO i=its,MIN(ide-1,ite)
        rv_tend(i,k,jts)=rv_tend(i,k,jts) - (msfvy(i,jts)/msfvx(i,jts))*0.5*(f(i,jts)+f(i,jts)) &
         *0.25*(ru(i,k,jts)+ru(i+1,k,jts)+ru(i,k,jts)+ru(i+1,k,jts)) &
             + (msfvy(i,jts)/msfvx(i,jts))*0.5*(e(i,jts)+e(i,jts))*0.5*(sina(i,jts)+sina(i,jts)) &
             *0.25*(rw(i,k+1,jts)+rw(i,k,jts)+rw(i,k+1,jts)+rw(i,k,jts))
     ENDDO
     ENDDO
   ENDIF
   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=its,MIN(ide-1,ite)
      rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*0.5*(f(i,j)+f(i,j-1)) &
       *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
           + (msfvy(i,j)/msfvx(i,j))*0.5*(e(i,j)+e(i,j-1))*0.5*(sina(i,j)+sina(i,j-1)) &
           *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))
   ENDDO
   ENDDO
   ENDDO
   IF ( (config_flags%open_ye) .and. (jte == jde) ) THEN
     DO k=kts,ktf
     DO i=its,MIN(ide-1,ite)
        rv_tend(i,k,jte)=rv_tend(i,k,jte) - (msfvy(i,jte)/msfvx(i,jte))*0.5*(f(i,jte-1)+f(i,jte-1)) &
         *0.25*(ru(i,k,jte-1)+ru(i+1,k,jte-1)+ru(i,k,jte-1)+ru(i+1,k,jte-1)) &
             + (msfvy(i,jte)/msfvx(i,jte))*0.5*(e(i,jte-1)+e(i,jte-1))*0.5*(sina(i,jte-1)+sina(i,jte-1)) &
             *0.25*(rw(i,k+1,jte-1)+rw(i,k,jte-1)+rw(i,k+1,jte-1)+rw(i,k,jte-1))
     ENDDO
     ENDDO
   ENDIF
   DO j=jts,MIN(jte, jde-1)
   DO k=kts+1,ktf
   DO i=its,MIN(ite, ide-1)
       rw_tend(i,k,j)=rw_tend(i,k,j) + e(i,j)* &
          (cosa(i,j)*0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j)) &
          +fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j))) &
          -(msftx(i,j)/msfty(i,j))*sina(i,j)*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1)) &
          +fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))))
   ENDDO
   ENDDO
   ENDDO
END SUBROUTINE perturbation_coriolis
SUBROUTINE curvature ( ru, rv, rw, u, v, w, ru_tend, rv_tend, rw_tend, &
                        config_flags, &
                        msfux, msfuy, msfvx, msfvy, msftx, msfty, &
                        xlat, fzm, fzp, rdx, rdy, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                               ims, ime, jms, jme, kms, kme, &
                                               its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
                                               INTENT(IN ) :: ru, &
                                                                rv, &
                                                                rw, &
                                                                u, &
                                                                v, &
                                                                w
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN ) :: msfux, &
                                                                msfuy, &
                                                                msfvx, &
                                                                msfvy, &
                                                                msftx, &
                                                                msfty, &
                                                                xlat
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: fzm, &
                                                                fzp
   REAL , INTENT(IN ) :: rdx, &
                                                                rdy
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   REAL , DIMENSION( its-1:ite , kts:kte, jts-1:jte ) :: vxgm
   LOGICAL :: specified
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)
   i_start = its-1
   i_end = ite
   j_start = jts-1
   j_end = jte
   IF ( ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) .and. (its == ids) ) i_start = its
   IF ( ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) .and. (ite == ide) ) i_end = ite-1
   IF ( ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jts == jds) ) j_start = jts
   IF ( ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jte == jde) ) j_end = jte-1
      IF ( config_flags%periodic_x ) i_start = its-1
      IF ( config_flags%periodic_x ) i_end = ite
   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=i_start, i_end
      vxgm(i,k,j)=0.5*(u(i,k,j)+u(i+1,k,j))*(msfvx(i,j+1)-msfvx(i,j))*rdy - &
                  0.5*(v(i,k,j)+v(i,k,j+1))*(msfuy(i+1,j)-msfuy(i,j))*rdx
   ENDDO
   ENDDO
   ENDDO
   IF ( ( config_flags%open_xs .or. (specified .AND. .NOT. config_flags%periodic_x) .or. &
        config_flags%nested) .and. (its == ids) ) THEN
     DO j = jts, jte-1
     DO k = kts, ktf
       vxgm(its-1,k,j) = vxgm(its,k,j)
     ENDDO
     ENDDO
   ENDIF
   IF ( ( config_flags%open_xe .or. (specified .AND. .NOT. config_flags%periodic_x) .or. &
        config_flags%nested) .and. (ite == ide) ) THEN
     DO j = jts, jte-1
     DO k = kts, ktf
       vxgm(ite,k,j) = vxgm(ite-1,k,j)
     ENDDO
     ENDDO
   ENDIF
   IF ( ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jts == jds) ) THEN
     DO k = kts, ktf
     DO i = its-1, ite
       vxgm(i,k,jts-1) = vxgm(i,k,jts)
     ENDDO
     ENDDO
   ENDIF
   IF ( ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jte == jde) ) THEN
     DO k = kts, ktf
     DO i = its-1, ite
       vxgm(i,k,jte) = vxgm(i,k,jte-1)
     ENDDO
     ENDDO
   ENDIF
   i_start = its
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX ( ids+1 , its )
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end = MIN ( ide-1 , ite )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
   IF ((config_flags%map_proj == 6) .OR. (config_flags%polar)) THEN
      DO j=jts,MIN(jde-1,jte)
      DO k=kts,ktf
      DO i=i_start,i_end
            ru_tend(i,k,j)=ru_tend(i,k,j) + u(i,k,j)*reradius* ( &
                        (msfux(i,j)/msfuy(i,j))*0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+ &
                                    rv(i-1,k,j)+rv(i,k,j))*tan(xlat(i,j)*degrad) &
                        - 0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j)) )
      ENDDO
      ENDDO
      ENDDO
   ELSE
      DO j=jts,MIN(jde-1,jte)
      DO k=kts,ktf
      DO i=i_start,i_end
         ru_tend(i,k,j)=ru_tend(i,k,j) + 0.5*(vxgm(i,k,j)+vxgm(i-1,k,j)) &
                 *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
                  - u(i,k,j)*reradius &
                 *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))
      ENDDO
      ENDDO
      ENDDO
   END IF
   j_start = jts
   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX ( jds+1 , jts )
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end = MIN ( jde-1 , jte )
   IF ((config_flags%map_proj == 6) .OR. (config_flags%polar)) THEN
      DO j=j_start,j_end
      DO k=kts,ktf
      DO i=its,MIN(ite,ide-1)
            rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*reradius* ( &
                        0.25*(u(i,k,j)+u(i+1,k,j)+u(i,k,j-1)+u(i+1,k,j-1))* &
                        tan((xlat(i,j)+xlat(i,j-1))*0.5*degrad)* &
                        0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
                       + v(i,k,j)*0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+ &
                                                      rw(i,k+1,j)+rw(i,k,j)) )
      ENDDO
      ENDDO
      ENDDO
   ELSE
      DO j=j_start,j_end
      DO k=kts,ktf
      DO i=its,MIN(ite,ide-1)
         rv_tend(i,k,j)=rv_tend(i,k,j) - 0.5*(vxgm(i,k,j)+vxgm(i,k,j-1)) &
                 *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
                       - (msfvy(i,j)/msfvx(i,j))*v(i,k,j)*reradius &
                 *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))
      ENDDO
      ENDDO
      ENDDO
   END IF
   DO j=jts,MIN(jte,jde-1)
   DO k=MAX(2,kts),ktf
   DO i=its,MIN(ite,ide-1)
      rw_tend(i,k,j)=rw_tend(i,k,j) + reradius* &
    (0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j))+fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j))) &
    *0.5*(fzm(k)*( u(i,k,j) +u(i+1,k,j))+fzp(k)*( u(i,k-1,j) +u(i+1,k-1,j))) &
    +(msftx(i,j)/msfty(i,j))*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1))+fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))) &
    *0.5*(fzm(k)*( v(i,k,j) +v(i,k,j+1))+fzp(k)*( v(i,k-1,j) +v(i,k-1,j+1))))
   ENDDO
   ENDDO
   ENDDO
END SUBROUTINE curvature
SUBROUTINE zero_tend ( tendency, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                                                ims, ime, jms, jme, kms, kme, &
                                                                its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   INTEGER :: i, j, k, itf, jtf, ktf
      DO j = jts, jte
      DO k = kts, kte
      DO i = its, ite
        tendency(i,k,j) = 0.
      ENDDO
      ENDDO
      ENDDO
      END SUBROUTINE zero_tend
SUBROUTINE zero_tend2d( tendency, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                                                ims, ime, jms, jme, kms, kme, &
                                                                its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: tendency
   INTEGER :: i, j, k, itf, jtf, ktf
      DO j = jts, jte
      DO i = its, ite
        tendency(i,j) = 0.
      ENDDO
      ENDDO
      END SUBROUTINE zero_tend2d
SUBROUTINE zero_pole ( field, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
  IMPLICIT NONE
  INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte
  REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: field
  INTEGER :: i, k
  IF (jts == jds) THEN
     DO k = kts, kte
     DO i = its-1, ite+1
        field(i,k,jts) = 0.
     END DO
     END DO
  END IF
  IF (jte == jde) THEN
     DO k = kts, kte
     DO i = its-1, ite+1
        field(i,k,jte) = 0.
     END DO
     END DO
  END IF
END SUBROUTINE zero_pole
SUBROUTINE pole_point_bc ( field, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
  IMPLICIT NONE
  INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte
  REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: field
  INTEGER :: i, k
  IF (jts == jds) THEN
     DO k = kts, kte
     DO i = its, ite
        field(i,k,jts) = field(i,k,jts+1)
     END DO
     END DO
  END IF
  IF (jte == jde) THEN
     DO k = kts, kte
     DO i = its, ite
        field(i,k,jte) = field(i,k,jte-1)
     END DO
     END DO
  END IF
END SUBROUTINE pole_point_bc
   SUBROUTINE phy_prep ( config_flags, &
                         mut, muu, muv, &
                         c1h, c2h, c1f, c2f, &
                         u, v, p, pb, alt, ph, &
                         phb, t, moist, n_moist, &
                         rho, th_phy, p_phy , pi_phy , &
                         u_phy, v_phy, p8w, t_phy, t8w, &
                         z, z_at_w, dz8w, &
                         p_hyd, p_hyd_w, dnw, &
                         fzm, fzp, znw, p_top, &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN ) :: n_moist
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), INTENT(IN) :: moist
   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mut, muu, muv
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT( OUT) :: u_phy, &
                                                              v_phy, &
                                                             pi_phy, &
                                                              p_phy, &
                                                                p8w, &
                                                              t_phy, &
                                                             th_phy, &
                                                                t8w, &
                                                                rho, &
                                                                  z, &
                                                               dz8w, &
                                                              z_at_w
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT( OUT) :: p_hyd, &
                                                              p_hyd_w
   REAL , INTENT(IN ) :: p_top
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(IN ) :: pb, &
                                                                  p, &
                                                                  u, &
                                                                  v, &
                                                                alt, &
                                                                 ph, &
                                                                phb, &
                                                                  t
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: fzm, &
                                                                fzp
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: znw, &
                                                                dnw
   REAL, DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h, c1f, c2f
   REAL, DIMENSION( kms:kme ) :: c1, c2
   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end, i_startu, j_startv
   INTEGER :: i, j, k
   REAL :: w1, w2, z0, z1, z2
   REAL :: qtot
   INTEGER :: n
    c1 = c1h
    c2 = c2h
    i_start = its
    i_end = min( ite,ide-1 )
    j_start = jts
    j_end = min( jte,jde-1 )
    k_start = kts
    k_end = min( kte, kde-1 )
    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
      th_phy(i,k,j) = t(i,k,j) + t0
      p_phy(i,k,j) = p(i,k,j) + pb(i,k,j)
      pi_phy(i,k,j) = (p_phy(i,k,j)/p1000mb)**rcp
      t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
      rho(i,k,j) = 1./alt(i,k,j)*(1.+moist(i,k,j,P_QV))
      u_phy(i,k,j) = 0.5*(u(i,k,j)+u(i+1,k,j))
      v_phy(i,k,j) = 0.5*(v(i,k,j)+v(i,k,j+1))
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do k = k_start, kte
    do i = i_start, i_end
      z_at_w(i,k,j) = (phb(i,k,j)+ph(i,k,j))/g
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do k = k_start, kte-1
    do i = i_start, i_end
      dz8w(i,k,j) = z_at_w(i,k+1,j)-z_at_w(i,k,j)
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do i = i_start, i_end
      dz8w(i,kte,j) = 0.
    enddo
    enddo
    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
      z(i,k,j) = 0.5*(z_at_w(i,k,j) +z_at_w(i,k+1,j) )
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do k = 2, k_end
    do i = i_start, i_end
      p8w(i,k,j) = fzm(k)*p_phy(i,k,j)+fzp(k)*p_phy(i,k-1,j)
      t8w(i,k,j) = fzm(k)*t_phy(i,k,j)+fzp(k)*t_phy(i,k-1,j)
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do i = i_start, i_end
      z0 = z_at_w(i,1,j)
      z1 = z(i,1,j)
      z2 = z(i,2,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,1,j) = w1*p_phy(i,1,j)+w2*p_phy(i,2,j)
      t8w(i,1,j) = w1*t_phy(i,1,j)+w2*t_phy(i,2,j)
      z0 = z_at_w(i,kte,j)
      z1 = z(i,k_end,j)
      z2 = z(i,k_end-1,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,kde,j) = exp(w1*log(p_phy(i,kde-1,j))+w2*log(p_phy(i,kde-2,j)))
      t8w(i,kde,j) = w1*t_phy(i,kde-1,j)+w2*t_phy(i,kde-2,j)
    enddo
    enddo
    do j = j_start,j_end
    do i = i_start, i_end
       p_hyd_w(i,kte,j) = p_top
    enddo
    enddo
    do j = j_start,j_end
    do k = kte-1, k_start, -1
    do i = i_start, i_end
       qtot = 0.
       do n = PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + moist(i,k,j,n)
       enddo
       p_hyd_w(i,k,j) = p_hyd_w(i,k+1,j) - (1.+qtot)*(c1(k)*MUT(i,j)+c2(k))*dnw(k)
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
       p_hyd(i,k,j) = 0.5*(p_hyd_w(i,k,j)+p_hyd_w(i,k+1,j))
    enddo
    enddo
    enddo
END SUBROUTINE phy_prep
   SUBROUTINE phy_prep_part2 ( config_flags, &
                         mut,muu,muv, &
                         c1h, c2h, c1f, c2f, &
                         RTHRATEN, &
                         RTHBLTEN, RUBLTEN, RVBLTEN, &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN, &
                         RUCUTEN, RVCUTEN, RTHCUTEN, &
                         RQVCUTEN, RQCCUTEN, RQRCUTEN, &
                         RQICUTEN, RQSCUTEN, &
                         RUSHTEN, RVSHTEN, RTHSHTEN, &
                         RQVSHTEN, RQCSHTEN, RQRSHTEN, &
                         RQISHTEN, RQSSHTEN, RQGSHTEN, &
                         RTHFTEN, RQVFTEN, &
                         RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, &
                         RPHNDGDTEN,RQVNDGDTEN, RMUNDGDTEN, &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type) , INTENT(IN ) :: config_flags
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: mut, muu, muv
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
          INTENT(INOUT) :: RTHRATEN
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
          INTENT(INOUT) :: RUCUTEN, &
                                                            RVCUTEN, &
                                                           RTHCUTEN, &
                                                           RQVCUTEN, &
                                                           RQCCUTEN, &
                                                           RQRCUTEN, &
                                                           RQICUTEN, &
                                                           RQSCUTEN, &
                                                            RUSHTEN, &
                                                            RVSHTEN, &
                                                           RTHSHTEN, &
                                                           RQVSHTEN, &
                                                           RQCSHTEN, &
                                                           RQRSHTEN, &
                                                           RQISHTEN, &
                                                           RQSSHTEN, &
                                                           RQGSHTEN
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
          INTENT(INOUT) :: RUBLTEN, &
                                                            RVBLTEN, &
                                                           RTHBLTEN, &
                                                           RQVBLTEN, &
                                                           RQCBLTEN, &
                                                           RQIBLTEN
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
          INTENT(INOUT) :: RTHFTEN, &
                                                            RQVFTEN
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
          INTENT(INOUT) :: RUNDGDTEN, &
                                                            RVNDGDTEN, &
                                                           RTHNDGDTEN, &
                                                           RPHNDGDTEN, &
                                                           RQVNDGDTEN
   REAL, DIMENSION( ims:ime, jms:jme ) , &
          INTENT(INOUT) :: RMUNDGDTEN
   REAL, DIMENSION( kms:kme ) , INTENT(IN ) :: c1h, c2h, c1f, c2f
   REAL, DIMENSION( kms:kme ) :: c1, c2
   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end, i_startu, j_startv
   INTEGER :: i, j, k
    i_start = its
    i_end = min( ite,ide-1 )
    j_start = jts
    j_end = min( jte,jde-1 )
    k_start = kts
    k_end = min( kte, kde-1 )
    c1 = c1h
    c2 = c2h
   IF (config_flags%ra_lw_physics .gt. 0 .or. config_flags%ra_sw_physics .gt. 0) THEN
      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RTHRATEN(I,K,J)=RTHRATEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO
   ENDIF
   IF (config_flags%cu_physics .gt. 0) THEN
      DO J=j_start,j_end
      DO I=i_start,i_end
      DO K=k_start,k_end
         RUCUTEN(I,K,J) =RUCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVCUTEN(I,K,J) =RVCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHCUTEN(I,K,J)=RTHCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO
      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQVCUTEN(I,K,J)=RQVCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QC .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQCCUTEN(I,K,J)=RQCCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QR .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQRCUTEN(I,K,J)=RQRCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QI .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQICUTEN(I,K,J)=RQICUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF(P_QS .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQSCUTEN(I,K,J)=RQSCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
   ENDIF
   IF (config_flags%shcu_physics .gt. 0) THEN
      DO J=j_start,j_end
      DO I=i_start,i_end
      DO K=k_start,k_end
         RUSHTEN(I,K,J) =RUSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVSHTEN(I,K,J) =RVSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHSHTEN(I,K,J)=RTHSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO
      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQVSHTEN(I,K,J)=RQVSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QC .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQCSHTEN(I,K,J)=RQCSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QR .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQRSHTEN(I,K,J)=RQRSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QI .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQISHTEN(I,K,J)=RQISHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF(P_QS .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQSSHTEN(I,K,J)=RQSSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF(P_QG .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQGSHTEN(I,K,J)=RQGSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
   ENDIF
   IF (config_flags%bl_pbl_physics .gt. 0) THEN
      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RUBLTEN(I,K,J) =RUBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVBLTEN(I,K,J) =RVBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHBLTEN(I,K,J)=RTHBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO
      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQVBLTEN(I,K,J)=RQVBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QC .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
           RQCBLTEN(I,K,J)=RQCBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF (P_QI .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQIBLTEN(I,K,J)=RQIBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
    ENDIF
   if(( config_flags%cu_physics == GDSCHEME ) .OR. &
      ( config_flags%cu_physics == GFSCHEME ) .OR. &
      ( config_flags%cu_physics == G3SCHEME ) .OR. &
      ( config_flags%cu_physics == KFETASCHEME ) .OR. &
      ( config_flags%cu_physics == MSKFSCHEME ) .OR. &
      ( config_flags%cu_physics == TIEDTKESCHEME ) .OR. &
      ( config_flags%cu_physics == NTIEDTKESCHEME )) then
      DO J=j_start,j_end
      DO I=i_start,i_end
         DO K=k_start,k_end
            RTHFTEN(I,K,J)=RTHFTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
      ENDDO
      ENDDO
      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
            DO K=k_start,k_end
               RQVFTEN(I,K,J)=RQVFTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
            ENDDO
         ENDDO
         ENDDO
      ENDIF
   END IF
   IF (config_flags%grid_fdda .gt. 0) THEN
      i_startu=MAX(its,ids+1)
      j_startv=MAX(jts,jds+1)
      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_startu,i_end
         RUNDGDTEN(I,K,J) =RUNDGDTEN(I,K,J)/(c1h(k)*muu(I,J)+c2h(k))
      ENDDO
      ENDDO
      ENDDO
      DO J=j_startv,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RVNDGDTEN(I,K,J) =RVNDGDTEN(I,K,J)/(c1h(k)*muv(I,J)+c2h(k))
      ENDDO
      ENDDO
      ENDDO
      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RTHNDGDTEN(I,K,J)=RTHNDGDTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO
      IF (config_flags%grid_fdda .EQ. 2) THEN
      DO J=j_start,j_end
      DO K=k_start,kte
      DO I=i_start,i_end
         RPHNDGDTEN(I,K,J)=RPHNDGDTEN(I,K,J)/(c1f(k)*mut(I,J)+c2f(k))
      ENDDO
      ENDDO
      ENDDO
      ELSE IF (config_flags%grid_fdda .EQ. 1) THEN
      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQVNDGDTEN(I,K,J)=RQVNDGDTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      ENDIF
    ENDIF
END SUBROUTINE phy_prep_part2
   SUBROUTINE moist_physics_prep_em( t_new, t_old, t0, rho, al, alb, &
                                     p, p8w, p0, pb, ph, phb, &
                                     th_phy, pii, pf, &
                                     z, z_at_w, dz8w, &
                                     dt,h_diabatic, &
                                     qv,qv_diabatic, &
                                     qc,qc_diabatic, &
                                     config_flags,fzm, fzp, &
                                     ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde
   INTEGER, INTENT(IN ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN ) :: its,ite, jts,jte, kts,kte
   REAL, INTENT(IN ) :: dt
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(IN ) :: al, &
                                                    alb, &
                                                    p, &
                                                    pb, &
                                                    ph, &
                                                    phb, &
                                                    qv, &
                                                    qc
   REAL , DIMENSION( kms:kme ) , INTENT(IN ) :: fzm, &
                                                              fzp
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT( OUT) :: rho, &
                                               th_phy, &
                                                  pii, &
                                                  pf, &
                                                    z, &
                                               z_at_w, &
                                                 dz8w, &
                                                  p8w
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(INOUT) :: h_diabatic, &
                                                 qv_diabatic, &
                                                 qc_diabatic
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(INOUT) :: t_new, &
                                                  t_old
   REAL, INTENT(IN ) :: t0, p0
   REAL :: z0,z1,z2,w1,w2
   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k
    i_start = its
    i_end = min( ite,ide-1 )
    j_start = jts
    j_end = min( jte,jde-1 )
    k_start = kts
    k_end = min( kte, kde-1 )
     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       z_at_w(i,k,j) = (ph(i,k,j)+phb(i,k,j))/g
     ENDDO
     ENDDO
     ENDDO
    do j = j_start,j_end
    do k = k_start, kte-1
    do i = i_start, i_end
      dz8w(i,k,j) = z_at_w(i,k+1,j)-z_at_w(i,k,j)
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do i = i_start, i_end
      dz8w(i,kte,j) = 0.
    enddo
    enddo
     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
       th_phy(i,k,j) = t_new(i,k,j) + t0
       h_diabatic(i,k,j) = th_phy(i,k,j)
       qv_diabatic(i,k,j) = qv(i,k,j)
       qc_diabatic(i,k,j) = qc(i,k,j)
       rho(i,k,j) = 1./(al(i,k,j)+alb(i,k,j))
       pii(i,k,j) = ((p(i,k,j)+pb(i,k,j))/p0)**rcp
       z(i,k,j) = 0.5*(z_at_w(i,k,j) +z_at_w(i,k+1,j) )
       pf(i,k,j) = p(i,k,j)+pb(i,k,j)
     ENDDO
     ENDDO
     ENDDO
    do j = j_start,j_end
    do k = 2, k_end
    do i = i_start, i_end
      p8w(i,k,j) = fzm(k)*pf(i,k,j)+fzp(k)*pf(i,k-1,j)
    enddo
    enddo
    enddo
    do j = j_start,j_end
    do i = i_start, i_end
      z0 = z_at_w(i,1,j)
      z1 = z(i,1,j)
      z2 = z(i,2,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,1,j) = w1*pf(i,1,j)+w2*pf(i,2,j)
      z0 = z_at_w(i,kte,j)
      z1 = z(i,k_end,j)
      z2 = z(i,k_end-1,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,kde,j) = exp(w1*log(pf(i,kde-1,j))+w2*log(pf(i,kde-2,j)))
    enddo
    enddo
   END SUBROUTINE moist_physics_prep_em
   SUBROUTINE moist_physics_finish_em( t_new, t_old, t0, mut, &
                                       th_phy, h_diabatic, dt, &
                                       qv,qv_diabatic, &
                                       qc,qc_diabatic, &
                                       config_flags, &
                                       dfi_opt, &
                                       dfi_tten_rad,dfi_stage, &
                                       dfi_tten_rad_1, dfi_tten_rad_2, &
                                       dfi_tten_rad_3, dfi_tten_rad_4, &
                                       tten_times, xtime, &
                                       ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte )
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags
   INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde
   INTEGER, INTENT(IN ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN ) :: its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(INOUT) :: t_new, &
                                                  t_old, &
                                                 th_phy, &
                                                  h_diabatic, &
                                                 qv_diabatic, &
                                                 qc_diabatic
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(IN ) :: qv, &
                                                    qc
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
         INTENT(IN), OPTIONAL :: dfi_tten_rad
   INTEGER, INTENT(IN ) ,OPTIONAL :: dfi_stage, dfi_opt
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), TARGET, &
         INTENT(IN), OPTIONAL :: dfi_tten_rad_1, dfi_tten_rad_2, &
                                 dfi_tten_rad_3, dfi_tten_rad_4
   REAL, DIMENSION( 4 ), &
         INTENT(IN), OPTIONAL :: tten_times
   REAL, INTENT(IN) ,OPTIONAL :: xtime
   REAL, POINTER, DIMENSION( :, :, : ) :: current_tten_rad
   INTEGER :: tten_time_index
   LOGICAL :: RAP_tten
   LOGICAL :: HRRR_tten
   REAL :: dfi_tten_max, old_max
   REAL mpten, mptenmax, mptenmin
   REAL :: qvten,qcten
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: mut
   REAL, INTENT(IN ) :: t0, dt
   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k, imax, jmax, imin, jmin
    i_start = its
    i_end = min( ite,ide-1 )
    j_start = jts
    j_end = min( jte,jde-1 )
    k_start = kts
    k_end = min( kte, kde-1 )
     RAP_tten = .false.
     HRRR_tten = .false.
     dfi_tten_max=-999
     old_max=-999
     IF ( dfi_opt.ne.0 .and. PRESENT(dfi_tten_rad) .and. dfi_stage==DFI_FWD ) THEN
       RAP_tten = .true.
       WRITE(wrf_err_message,*)'Add RAP static radar tendency: i_start,j_start: ', i_start, j_start
     ENDIF
     IF ( (config_flags%mp_tend_radar .eq. 1) .and. &
          PRESENT(tten_times) .and. PRESENT(xtime) .and. &
          PRESENT(dfi_tten_rad_1) .and. PRESENT(dfi_tten_rad_2) .and. &
          PRESENT(dfi_tten_rad_3) .and. PRESENT(dfi_tten_rad_4) ) THEN
       tten_time_index = 1
       do while ( (tten_time_index.lt.4) .and. (xtime .gt. tten_times(tten_time_index)) )
         tten_time_index = tten_time_index + 1
       end do
       if (tten_time_index.le.4) then
         HRRR_tten = .true.
         select case (tten_time_index)
           case (1)
             current_tten_rad => dfi_tten_rad_1
           case (2)
             current_tten_rad => dfi_tten_rad_2
           case (3)
             current_tten_rad => dfi_tten_rad_3
           case (4)
             current_tten_rad => dfi_tten_rad_4
         end select
         WRITE(wrf_err_message,*)'Add HRRR time-varying radar-based tten: tten_time_index,i_start,j_start: ', &
                                 tten_time_index, i_start, j_start
       endif
     ENDIF
     CALL wrf_debug ( 0 , TRIM(wrf_err_message) )
     IF ( config_flags%no_mp_heating .eq. 0 ) THEN
       mptenmax = 0.
       mptenmin = 999.
     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
          mpten = th_phy(i,k,j)-h_diabatic(i,k,j)
          qvten = qv(i,k,j)-qv_diabatic(i,k,j)
          qcten = qc(i,k,j)-qc_diabatic(i,k,j)
       if(mpten.gt.mptenmax) then
          mptenmax=mpten
          imax=i
          jmax=j
       endif
       if(mpten.lt.mptenmin) then
          mptenmin=mpten
          imin=i
          jmin=j
       endif
          mpten=min(config_flags%mp_tend_lim*dt, mpten)
          mpten=max(-config_flags%mp_tend_lim*dt, mpten)
       IF ( RAP_tten ) THEN
         if(k < k_end ) then
           if(dfi_tten_max < dfi_tten_rad(i,k,j) ) dfi_tten_max = dfi_tten_rad(i,k,j)
           if(old_max < (th_phy(i,k,j)-h_diabatic(i,k,j)) ) old_max=th_phy(i,k,j)-h_diabatic(i,k,j)
         endif
       ENDIF
       IF ( RAP_tten ) THEN
         if ( dfi_tten_rad(i,k,j) >= -0.1 .and. &
              dfi_tten_rad(i,k,j) <= 0.1 .and. k < k_end ) then
           t_new(i,k,j) = t_new(i,k,j) + (dfi_tten_rad(i,k,j))*dt
         else
           t_new(i,k,j) = t_new(i,k,j) + mpten
         endif
       ENDIF
       IF ( HRRR_tten) THEN
         if ( current_tten_rad(i,k_end,j) > 0.99 .and. &
              current_tten_rad(i,k,j) >= -1.0 .and. &
              current_tten_rad(i,k,j) <= 1.0 .and. k < k_end) then
           t_new(i,k,j) = t_new(i,k,j) + current_tten_rad(i,k,j)*dt
         elseif ( current_tten_rad(i,k,j) >= -1.0 .and. &
                  current_tten_rad(i,k,j) <= 1.0 .and. k < k_end) then
           t_new(i,k,j) = t_new(i,k,j) + current_tten_rad(i,k,j)*dt
         else
           t_new(i,k,j) = t_new(i,k,j) + mpten
         endif
       ENDIF
       IF ( (.not. RAP_tten) .and. (.not. HRRR_tten) ) THEN
         t_new(i,k,j) = t_new(i,k,j) + mpten
       ENDIF
         h_diabatic(i,k,j) = mpten/dt
         qv_diabatic(i,k,j) = qvten/dt
         qc_diabatic(i,k,j) = qcten/dt
     ENDDO
     ENDDO
     ENDDO
     ELSE
     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
         h_diabatic(i,k,j) = 0.
         qv_diabatic(i,k,j) = 0.
         qc_diabatic(i,k,j) = 0.
     ENDDO
     ENDDO
     ENDDO
     ENDIF
   END SUBROUTINE moist_physics_finish_em
   SUBROUTINE init_module_big_step
   END SUBROUTINE init_module_big_step
SUBROUTINE set_tend ( field, field_adv_tend, msf, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN ) :: ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: field
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN) :: field_adv_tend
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: msf
   INTEGER :: i, j, k, itf, jtf, ktf
      jtf = MIN(jte,jde-1)
      ktf = MIN(kte,kde-1)
      itf = MIN(ite,ide-1)
      DO j = jts, jtf
      DO k = kts, ktf
      DO i = its, itf
         field(i,k,j) = field_adv_tend(i,k,j)*msf(i,j)
      ENDDO
      ENDDO
      ENDDO
END SUBROUTINE set_tend
    SUBROUTINE rk_rayleigh_damp( ru_tendf, rv_tendf, &
                                 rw_tendf, t_tendf, &
                                 u, v, w, t, t_init, &
                                 c1h, c2h, c1f, c2f, &
                                 mut, muu, muv, ph, phb, &
                                 u_base, v_base, t_base, z_base, &
                                 dampcoef, zdamp, &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte )
    IMPLICIT NONE
    INTEGER, INTENT( IN ) &
    :: ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) &
    :: ru_tendf, rv_tendf, rw_tendf, t_tendf
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN ) &
    :: u, v, w, t, t_init, ph, phb
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN ) &
    :: mut, muu, muv
    REAL, DIMENSION( kms:kme ) , INTENT(IN ) &
    :: u_base, v_base, t_base, z_base
    REAL, DIMENSION( kms:kme ) , INTENT(IN ) &
    :: c1h, c2h, c1f, c2f
    REAL, INTENT(IN ) &
    :: dampcoef, zdamp
    INTEGER &
    :: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k, ktf, k1, k2
    REAL, DIMENSION( kms:kme ) &
    :: c1, c2
    REAL &
    :: pii, dcoef, z, ztop
    REAL :: wkp1, wk, wkm1
    REAL, DIMENSION( kms:kme ) :: z00, u00, v00, t00
    c1 = c1h
    c2 = c2h
    pii = 2.0 * asin(1.0)
    ktf = MIN( kte, kde-1 )
    DO j = jts, MIN( jte, jde-1 )
    DO i = its, MIN( ite, ide )
      ztop = 0.5*( phb(i ,kde,j)+phb(i-1,kde,j) &
                  +ph(i ,kde,j)+ph(i-1,kde,j) )/g
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.25*( phb(i ,k1,j)+phb(i ,k1+1,j) &
                  +phb(i-1,k1,j)+phb(i-1,k1+1,j) &
                  +ph(i ,k1,j)+ph(i ,k1+1,j) &
                  +ph(i-1,k1,j)+ph(i-1,k1+1,j))/g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          u00(k) = u_base(k2) + ( u_base(k2) - u_base(k2-1) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          u00(k) = u_base(k2) + ( u_base(k2+1) - u_base(k2) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        ru_tendf(i,k,j) = ru_tendf(i,k,j) - &
                          (c1h(k)*muu(i,j)+c2h(k)) * ( dcoef * dampcoef ) * &
                          ( u(i,k,j) - u00(k) )
      END DO
    END DO
    END DO
    DO j = jts, MIN( jte, jde )
    DO i = its, MIN( ite, ide-1 )
      ztop = 0.5*( phb(i,kde,j )+phb(i,kde,j-1) &
                  +ph(i,kde,j )+ph(i,kde,j-1) )/g
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.25*( phb(i,k1,j )+phb(i,k1+1,j ) &
                  +phb(i,k1,j-1)+phb(i,k1+1,j-1) &
                  +ph(i,k1,j )+ph(i,k1+1,j ) &
                  +ph(i,k1,j-1)+ph(i,k1+1,j-1))/g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          v00(k) = v_base(k2) + ( v_base(k2) - v_base(k2-1) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          v00(k) = v_base(k2) + ( v_base(k2+1) - v_base(k2) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        rv_tendf(i,k,j) = rv_tendf(i,k,j) - &
                          (c1h(k)*muv(i,j)+c2h(k)) * ( dcoef * dampcoef ) * &
                          ( v(i,k,j) - v00(k) )
      END DO
    END DO
    END DO
    DO j = jts, MIN( jte, jde-1 )
    DO i = its, MIN( ite, ide-1 )
      ztop = ( phb(i,kde,j) + ph(i,kde,j) ) / g
      DO k = kts, MIN( kte, kde )
        z = ( phb(i,k,j) + ph(i,k,j) ) / g
        IF ( z >= (ztop-zdamp) ) THEN
          dcoef = 1.0 - MIN( 1.0, ( ztop - z ) / zdamp )
          dcoef = ( SIN( 0.5 * pii * dcoef ) )**2
          rw_tendf(i,k,j) = rw_tendf(i,k,j) - &
                            (c1f(k)*mut(i,j)+c2f(k)) * ( dcoef * dampcoef ) * w(i,k,j)
        END IF
      END DO
    END DO
    END DO
    DO j = jts, MIN( jte, jde-1 )
    DO i = its, MIN( ite, ide-1 )
      ztop = ( phb(i,kde,j) + ph(i,kde,j) ) / g
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.5 * ( phb(i,k1,j) + phb(i,k1+1,j) + &
                     ph(i,k1,j) + ph(i,k1+1,j) ) / g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          t00(k) = t_base(k2) + ( t_base(k2) - t_base(k2-1) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          t00(k) = t_base(k2) + ( t_base(k2+1) - t_base(k2) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        t_tendf(i,k,j) = t_tendf(i,k,j) - &
                         (c1(k)*MUT(i,j)+c2(k)) * ( dcoef * dampcoef ) * &
                         ( t(i,k,j) - t00(k) )
      END DO
    END DO
    END DO
    END SUBROUTINE rk_rayleigh_damp
 SUBROUTINE theta_relaxation( t_tendf, t, t_init, &
                              MUT, c1, c2, ph, phb, &
                              t_base, z_base, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte )
    IMPLICIT NONE
    INTEGER, INTENT( IN ) &
    :: ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) &
    :: t_tendf
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN ) &
    :: t, t_init, ph, phb
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN ) &
    :: MUT
    REAL, DIMENSION( kms:kme), INTENT( IN ) &
    :: c1, c2
    REAL, DIMENSION( kms:kme ) , INTENT(IN ) &
    :: t_base, z_base
    INTEGER :: i, j, k, ktf, k2
    REAL :: tau_r , rmax , rmin , inv_tau_r , inv_g , rterm
    REAL, DIMENSION( kms:kme ) :: z00,t00
    tau_r = 12.0*3600.0
    rmax = 2.0/86400.0
    rmin = -rmax
    ktf = MIN( kte, kde-1 )
    inv_tau_r = 1.0/tau_r
    inv_g = 1.0/g
    DO j = jts, MIN( jte, jde-1 )
    DO i = its, MIN( ite, ide-1 )
      DO k = kts, ktf
        z00(k) = 0.5 * ( phb(i,k,j) + phb(i,k+1,j) + &
                          ph(i,k,j) + ph(i,k+1,j) ) * inv_g
      ENDDO
      DO k = kts, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) .and. k2 .gt. 1 )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          t00(k) = t_base(k2) + ( t_base(k2) - t_base(k2-1) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          t00(k) = t_base(k2) + ( t_base(k2+1) - t_base(k2) ) &
                              * ( z00(k) - z_base(k2) ) &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO
      DO k = kts, ktf
        rterm = -( t(i,k,j) - t00(k) )*inv_tau_r
        rterm = min( rterm , rmax )
        rterm = max( rterm , rmin )
        t_tendf(i,k,j) = t_tendf(i,k,j) + (c1(k)*MUT(i,j)+c2(k))*rterm
      END DO
    END DO
    END DO
 END SUBROUTINE theta_relaxation
      SUBROUTINE sixth_order_diffusion( name, field, tendency, MUT, dt, &
                                        config_flags, c1, c2, &
                                        diff_6th_opt, diff_6th_factor, &
                                        phb, ph, &
                                        rdx, rdy, &
                                        msftx, msfty, &
                                        msfux, msfuy, &
                                        msfvx, msfvy, &
                                        ids, ide, jds, jde, kds, kde, &
                                        ims, ime, jms, jme, kms, kme, &
                                        its, ite, jts, jte, kts, kte )
    IMPLICIT NONE
    INTEGER, INTENT(IN) &
    :: ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte
    TYPE(grid_config_rec_type), INTENT(IN) &
    :: config_flags
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) &
    :: tendency
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) &
    :: field
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) &
    :: ph, phb
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) &
    :: MUT
    REAL, DIMENSION( kms:kme ), INTENT(IN) &
    :: c1, c2
    REAL, INTENT(IN) &
    :: dt, rdx, rdy
    REAL, INTENT(IN) &
    :: diff_6th_factor
    INTEGER, INTENT(IN) &
    :: diff_6th_opt
    CHARACTER(LEN=1) , INTENT(IN) &
    :: name
    INTEGER &
    :: i, j, k, &
       i_start, i_end, &
       j_start, j_end, &
       k_start, k_end, &
       ktf
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msfux
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msfuy
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msfvx
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msfvy
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msftx
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN ) :: msfty
    REAL &
    :: dflux_x_p0, dflux_y_p0, &
       dflux_x_p1, dflux_y_p1, &
       tendency_x, tendency_y, &
       mu_avg_p0, mu_avg_p1, &
       diff_6th_coef
    LOGICAL &
    :: specified
    REAL &
    :: dzmax_p0, dzmax_p1, dx, dy, slopedamp_p0, slopedamp_p1, dzthresh
    IF ( diff_6th_opt .EQ. 3) THEN
       diff_6th_coef = diff_6th_factor * 0.00390625 / ( 2.0 * dt )
    ELSE
       diff_6th_coef = diff_6th_factor * 0.015625 / ( 2.0 * dt )
    END IF
    ktf = MIN( kte, kde-1 )
    IF ( name .EQ. 'u' ) THEN
      i_start = its
      i_end = ite
      j_start = jts
      j_end = MIN(jde-1,jte)
      k_start = kts
      k_end = ktf
    ELSE IF ( name .EQ. 'v' ) THEN
      i_start = its
      i_end = MIN(ide-1,ite)
      j_start = jts
      j_end = jte
      k_start = kts
      k_end = ktf
    ELSE IF ( name .EQ. 'w' ) THEN
      i_start = its
      i_end = MIN(ide-1,ite)
      j_start = jts
      j_end = MIN(jde-1,jte)
      k_start = kts+1
      k_end = ktf
    ELSE
      i_start = its
      i_end = MIN(ide-1,ite)
      j_start = jts
      j_end = MIN(jde-1,jte)
      k_start = kts
      k_end = ktf
    ENDIF
    DO j = j_start, j_end
    DO k = k_start, k_end
    DO i = i_start, i_end
      IF ( diff_6th_opt .EQ. 3 ) THEN
         dflux_x_p0 = ( ( field(i+2,k,j) - field(i-3,k,j) ) &
                + 2.0 * ( field(i+1,k,j+1) - field(i-2,k,j+1) ) &
                - 9.0 * ( field(i+1,k,j) - field(i-2,k,j) ) &
                - 10.0 * ( field(i,k,j+1) - field(i-1,k,j+1) ) &
                + 28.0 * ( field(i,k,j) - field(i-1,k,j) ) &
                - 10.0 * ( field(i,k,j-1) - field(i-1,k,j-1) ) &
                + 2.0 * ( field(i+1,k,j-1) - field(i-2,k,j-1) ) &
                + ( field(i,k,j-2) - field(i-1,k,j-2) ) &
                + ( field(i,k,j+2) - field(i-1,k,j+2) ) )
         dflux_x_p1 = ( ( field(i+3,k,j) - field(i-2,k,j) ) &
                + 2.0 * ( field(i+2,k,j+1) - field(i-1,k,j+1) ) &
                - 9.0 * ( field(i+2,k,j) - field(i-1,k,j) ) &
                - 10.0 * ( field(i+1,k,j+1) - field(i,k,j+1) ) &
                + 28.0 * ( field(i+1,k,j) - field(i,k,j) ) &
                - 10.0 * ( field(i+1,k,j-1) - field(i,k,j-1) ) &
                + 2.0 * ( field(i+2,k,j-1) - field(i-1,k,j-1) ) &
                + ( field(i+1,k,j-2) - field(i,k,j-2) ) &
                + ( field(i+1,k,j+2) - field(i,k,j+2) ) )
      ELSE
         dflux_x_p0 = ( 10.0 * ( field(i, k,j) - field(i-1,k,j) ) &
                        - 5.0 * ( field(i+1,k,j) - field(i-2,k,j) ) &
                        + ( field(i+2,k,j) - field(i-3,k,j) ) )
         dflux_x_p1 = ( 10.0 * ( field(i+1,k,j) - field(i ,k,j) ) &
                        - 5.0 * ( field(i+2,k,j) - field(i-1,k,j) ) &
                        + ( field(i+3,k,j) - field(i-2,k,j) ) )
      END IF
      IF ( (diff_6th_opt .EQ. 2) .OR. (diff_6th_opt .EQ. 3) ) THEN
        IF ( dflux_x_p0 * ( field(i ,k,j)-field(i-1,k,j) ) .LE. 0.0 ) THEN
          dflux_x_p0 = 0.0
        END IF
        IF ( dflux_x_p1 * ( field(i+1,k,j)-field(i ,k,j) ) .LE. 0.0 ) THEN
          dflux_x_p1 = 0.0
        END IF
      END IF
      slopedamp_p0 = 1.
      slopedamp_p1 = 1.
      IF (config_flags%diff_6th_slopeopt .ge. 1) THEN
         dx=1./rdx
         dzthresh = config_flags%diff_6th_thresh*9.81*dx
         IF ( name .EQ. 'u' ) THEN
           dzmax_p0 = MAX( ABS(phb(i ,k,j)-phb(i-1,k,j))*msfux(i ,j), ABS(phb(i-1,k,j)-phb(i-2,k,j))*msfux(i-1,j) )
           dzmax_p1 = MAX( ABS(phb(i+1,k,j)-phb(i ,k,j))*msfux(i+1,j), ABS(phb(i ,k,j)-phb(i-1,k,j))*msfux(i ,j) )
         ELSE IF ( name .EQ. 'v' ) THEN
           dzmax_p0 = max( ABS(phb(i ,k,j) - phb(i-1,k,j))*msfux(i ,j), ABS(phb(i ,k,j-1) - phb(i-1,k,j-1))*msfux(i ,j-1) )
           dzmax_p1 = max( ABS(phb(i+1,k,j) - phb(i ,k,j))*msfux(i+1,j), ABS(phb(i+1,k,j-1) - phb(i ,k,j-1))*msfux(i+1,j-1) )
         ELSE
           dzmax_p0 = ABS(phb(i ,k,j) - phb(i-1,k,j))*msfux(i,j)
           dzmax_p1 = ABS(phb(i+1,k,j) - phb(i ,k,j))*msfux(i+1,j)
         END IF
         slopedamp_p0 = MAX(1.- (dzmax_p0/dzthresh) , 0.)
         slopedamp_p1 = MAX(1.- (dzmax_p1/dzthresh) , 0.)
      ENDIF
      IF ( name .EQ. 'u' ) THEN
        mu_avg_p0 = (c1(k)*MUT(i-1,j)+c2(k))
        mu_avg_p1 = (c1(k)*MUT(i ,j)+c2(k))
         tendency_x = diff_6th_coef * msfux(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      ELSE IF ( name .EQ. 'v' ) THEN
        mu_avg_p0 = 0.25 * ( &
                    (c1(k)*MUT(i-1,j-1)+c2(k)) + &
                    (c1(k)*MUT(i ,j-1)+c2(k)) + &
                    (c1(k)*MUT(i-1,j)+c2(k)) + &
                    (c1(k)*MUT(i ,j)+c2(k)) )
        mu_avg_p1 = 0.25 * ( &
                    (c1(k)*MUT(i ,j-1)+c2(k)) + &
                    (c1(k)*MUT(i+1,j-1)+c2(k)) + &
                    (c1(k)*MUT(i ,j)+c2(k)) + &
                    (c1(k)*MUT(i+1,j)+c2(k)) )
        tendency_x = diff_6th_coef * msfvx(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      ELSE
        mu_avg_p0 = 0.5 * ( &
                    (c1(k)*MUT(i-1,j)+c2(k)) + &
                    (c1(k)*MUT(i ,j)+c2(k)) )
        mu_avg_p1 = 0.5 * ( &
                    (c1(k)*MUT(i ,j)+c2(k)) + &
                    (c1(k)*MUT(i+1,j)+c2(k)) )
        tendency_x = diff_6th_coef * msftx(i,j) * &
                 ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      END IF
      IF ( diff_6th_opt .EQ. 3 ) THEN
         dflux_y_p0 = ( ( field(i,k,j+2) - field(i,k,j-3) ) &
                + 2.0 * ( field(i+1,k,j+1) - field(i+1,k,j-2) ) &
                - 9.0 * ( field(i,k,j+1) - field(i,k,j-2) ) &
                - 10.0 * ( field(i+1,k,j) - field(i+1,k,j-1) ) &
                + 28.0 * ( field(i,k,j) - field(i,k,j-1) ) &
                - 10.0 * ( field(i-1,k,j) - field(i-1,k,j-1) ) &
                + 2.0 * ( field(i-1,k,j+1) - field(i-1,k,j-2) ) &
                + ( field(i-2,k,j) - field(i-2,k,j-1) ) &
                + ( field(i+2,k,j) - field(i+2,k,j-1) ) )
         dflux_y_p1 = ( ( field(i,k,j+3) - field(i,k,j-2) ) &
                + 2.0 * ( field(i+1,k,j+2) - field(i+1,k,j-1) ) &
                - 9.0 * ( field(i,k,j+2) - field(i,k,j-1) ) &
                - 10.0 * ( field(i+1,k,j+1) - field(i+1,k,j) ) &
                + 28.0 * ( field(i,k,j+1) - field(i,k,j) ) &
                - 10.0 * ( field(i-1,k,j+1) - field(i-1,k,j) ) &
                + 2.0 * ( field(i-1,k,j+2) - field(i-1,k,j-1) ) &
                + ( field(i-2,k,j+1) - field(i-2,k,j) ) &
                + ( field(i+2,k,j+1) - field(i+2,k,j) ) )
      ELSE
         dflux_y_p0 = ( 10.0 * ( field(i,k,j ) - field(i,k,j-1) ) &
                        - 5.0 * ( field(i,k,j+1) - field(i,k,j-2) ) &
                        + ( field(i,k,j+2) - field(i,k,j-3) ) )
         dflux_y_p1 = ( 10.0 * ( field(i,k,j+1) - field(i,k,j ) ) &
                        - 5.0 * ( field(i,k,j+2) - field(i,k,j-1) ) &
                        + ( field(i,k,j+3) - field(i,k,j-2) ) )
     END IF
      IF ( (diff_6th_opt .EQ. 2) .OR. (diff_6th_opt .EQ. 3) ) THEN
        IF ( dflux_y_p0 * ( field(i,k,j )-field(i,k,j-1) ) .LE. 0.0 ) THEN
          dflux_y_p0 = 0.0
        END IF
        IF ( dflux_y_p1 * ( field(i,k,j+1)-field(i,k,j ) ) .LE. 0.0 ) THEN
          dflux_y_p1 = 0.0
        END IF
      END IF
      IF (config_flags%diff_6th_slopeopt .ge. 1) THEN
         dy=1./rdy
         dzthresh = config_flags%diff_6th_thresh*9.81*dy
         IF ( name .EQ. 'u' ) THEN
           dzmax_p0 = max( ABS(phb(i,k,j ) - phb(i,k,j-1))*msfvy(i,j ), ABS(phb(i-1,k,j ) - phb(i-1,k,j-1))*msfvy(i-1,j ) )
           dzmax_p1 = max( ABS(phb(i,k,j+1) - phb(i,k,j ))*msfvy(i,j+1), ABS(phb(i-1,k,j+1) - phb(i-1,k,j ))*msfvy(i-1,j+1) )
         ELSE IF ( name .EQ. 'v' ) THEN
           dzmax_p0 = MAX( ABS(phb(i,k,j )-phb(i,k,j-1))*msfvy(i,j ), ABS(phb(i,k,j-1)-phb(i,k,j-2))*msfvy(i,j-1) )
           dzmax_p1 = MAX( ABS(phb(i,k,j+1)-phb(i,k,j ))*msfvy(i,j+1), ABS(phb(i,k,j )-phb(i,k,j-1))*msfvy(i,j ) )
         ELSE
           dzmax_p0 = ABS(phb(i,k,j ) - phb(i,k,j-1))*msfvy(i,j )
           dzmax_p1 = ABS(phb(i,k,j+1) - phb(i,k,j ))*msfvy(i,j+1)
         END IF
         slopedamp_p0 = MAX(1.- (dzmax_p0/dzthresh) , 0.)
         slopedamp_p1 = MAX(1.- (dzmax_p1/dzthresh) , 0.)
      ENDIF
      IF ( name .EQ. 'u' ) THEN
        mu_avg_p0 = 0.25 * ( &
                    (c1(k)*MUT(i-1,j-1)+c2(k)) + &
                    (c1(k)*MUT(i ,j-1)+c2(k)) + &
                    (c1(k)*MUT(i-1,j)+c2(k)) + &
                    (c1(k)*MUT(i ,j)+c2(k)) )
        mu_avg_p1 = 0.25 * ( &
                    (c1(k)*MUT(i-1,j)+c2(k)) + &
                    (c1(k)*MUT(i ,j)+c2(k)) + &
                    (c1(k)*MUT(i-1,j+1)+c2(k)) + &
                    (c1(k)*MUT(i ,j+1)+c2(k)) )
        tendency_y = diff_6th_coef * msfuy(i,j) * &
                 ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
      ELSE IF ( name .EQ. 'v' ) THEN
        mu_avg_p0 = (c1(k)*MUT(i,j-1)+c2(k))
        mu_avg_p1 = (c1(k)*MUT(i,j)+c2(k))
        tendency_y = diff_6th_coef * msfvy(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
      ELSE
        mu_avg_p0 = 0.5 * ( &
                    (c1(k)*MUT(i,j-1)+c2(k)) + &
                    (c1(k)*MUT(i,j)+c2(k)) )
        mu_avg_p1 = 0.5 * ( &
                    (c1(k)*MUT(i,j)+c2(k)) + &
                    (c1(k)*MUT(i,j+1)+c2(k)) )
        tendency_y = diff_6th_coef * msfty(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
      END IF
      tendency(i,k,j) = tendency(i,k,j) + tendency_x + tendency_y
    ENDDO
    ENDDO
    ENDDO
    END SUBROUTINE sixth_order_diffusion
SUBROUTINE initialize_moist_old ( moist_old , moist , &
                                  ids, ide, jds, jde, kds, kde , &
                                  ims, ime, jms, jme, kms, kme , &
                                  its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte
   REAL , INTENT(IN ) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist
   REAL , INTENT( OUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist_old
   INTEGER :: i , j , k
   DO j = jts , MIN(jte,jde-1)
      DO k = kts , kte-1
         DO i = its , MIN(ite,ide-1)
            moist_old(i,k,j) = moist(i,k,j)
         END DO
      END DO
   END DO
END SUBROUTINE initialize_moist_old
SUBROUTINE theta_to_thetam ( t_1 , moist_old , &
                             t_tendf , moist_tend , &
                             t_2 , moist , &
                             h_diabatic , &
                             itimestep , &
                             rk_step , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN) :: rk_step, itimestep
   REAL , INTENT(IN ) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist, moist_tend
   REAL , INTENT(INOUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist_old
   REAL , INTENT(INOUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: t_1, t_tendf, t_2, h_diabatic
   INTEGER :: i , j , k
   IF ( rk_step .EQ. 1 ) THEN
      DO j = jts , MIN(jte,jde-1)
         DO k = kts , kte-1
            DO i = its , MIN(ite,ide-1)
               t_tendf(i,k,j) = (1. + (R_v/R_d) * moist_old(i,k,j))*t_tendf(i,k,j) + (R_v/R_d)*(t_1(i,k,j)+T0)*moist_tend(i,k,j)
               h_diabatic(i,k,j) = (1. + (R_v/R_d) * moist_old(i,k,j))*h_diabatic(i,k,j)
            END DO
         END DO
      END DO
   END IF
   DO j = jts , MIN(jte,jde-1)
      DO k = kts , kte-1
         DO i = its , MIN(ite,ide-1)
            t_1(i,k,j) = ( t_1(i,k,j) + T0 ) * (1. + (R_v/R_d) * moist_old(i,k,j)) - T0
            t_2(i,k,j) = ( t_2(i,k,j) + T0 ) * (1. + (R_v/R_d) * moist(i,k,j)) - T0
         END DO
      END DO
   END DO
END SUBROUTINE theta_to_thetam
SUBROUTINE thetam_to_theta ( t_1 , moist_old , &
                             t_2 , moist , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte
   REAL , INTENT(IN ) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist, moist_old
   REAL , INTENT(INOUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: t_1, t_2
   INTEGER :: i , j , k
   DO j = jts , MIN(jte,jde-1)
      DO k = kts , kte-1
         DO i = its , MIN(ite,ide-1)
            t_1(i,k,j) = -T0 + (t_1(i,k,j)+T0)/(1. + (R_v/R_d) * moist_old(i,k,j))
            t_2(i,k,j) = -T0 + (t_2(i,k,j)+T0)/(1. + (R_v/R_d) * moist(i,k,j))
         END DO
      END DO
   END DO
END SUBROUTINE thetam_to_theta
END MODULE module_big_step_utilities_em
