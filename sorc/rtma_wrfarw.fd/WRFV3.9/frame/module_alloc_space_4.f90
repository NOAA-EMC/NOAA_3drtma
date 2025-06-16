MODULE module_alloc_space_4
CONTAINS
   SUBROUTINE alloc_space_field_core_4 ( grid, id, setinitval_in , tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated , &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )
      USE module_domain_type
      USE module_configure, ONLY : model_config_rec, grid_config_rec_type, in_use_for_config, model_to_grid_config_rec
      USE module_scalar_tables
      IMPLICIT NONE
      TYPE(domain) , POINTER :: grid
      INTEGER , INTENT(IN) :: id
      INTEGER , INTENT(IN) :: setinitval_in
      INTEGER , INTENT(IN) :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN) :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN) :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN) :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN) :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN) :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN) :: sm31y, em31y, sm32y, em32y, sm33y, em33y
      INTEGER , INTENT(IN) :: tl_in
      LOGICAL , INTENT(IN) :: inter_domain_in, okay_to_alloc_in
      INTEGER(KIND=8) , INTENT(INOUT) :: num_bytes_allocated
      INTEGER idum1, idum2, spec_bdy_width
      REAL initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain, okay_to_alloc
      INTEGER setinitval
      INTEGER sr_x, sr_y
      INTEGER ierr
      INTEGER :: loop
      TYPE ( grid_config_rec_type ) :: config_flags
      INTEGER :: k_start , k_end, its, ite, jts, jte
      INTEGER :: ids , ide , jds , jde , kds , kde , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe
      INTEGER :: sids , side , sjds , sjde , skds , skde , &
                                         sims , sime , sjms , sjme , skms , skme , &
                                         sips , sipe , sjps , sjpe , skps , skpe
      INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex, &
                              ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                              imsy, imey, jmsy, jmey, kmsy, kmey, &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey
      data_ordering : SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_XYZ )
             ids = sd31 ; ide = ed31 ; jds = sd32 ; jde = ed32 ; kds = sd33 ; kde = ed33 ;
             ims = sm31 ; ime = em31 ; jms = sm32 ; jme = em32 ; kms = sm33 ; kme = em33 ;
             ips = sp31 ; ipe = ep31 ; jps = sp32 ; jpe = ep32 ; kps = sp33 ; kpe = ep33 ;
             imsx = sm31x ; imex = em31x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm33x ; kmex = em33x ;
             ipsx = sp31x ; ipex = ep31x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp33x ; kpex = ep33x ;
             imsy = sm31y ; imey = em31y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm33y ; kmey = em33y ;
             ipsy = sp31y ; ipey = ep31y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp33y ; kpey = ep33y ;
         CASE ( DATA_ORDER_YXZ )
             ids = sd32 ; ide = ed32 ; jds = sd31 ; jde = ed31 ; kds = sd33 ; kde = ed33 ;
             ims = sm32 ; ime = em32 ; jms = sm31 ; jme = em31 ; kms = sm33 ; kme = em33 ;
             ips = sp32 ; ipe = ep32 ; jps = sp31 ; jpe = ep31 ; kps = sp33 ; kpe = ep33 ;
             imsx = sm32x ; imex = em32x ; jmsx = sm31x ; jmex = em31x ; kmsx = sm33x ; kmex = em33x ;
             ipsx = sp32x ; ipex = ep32x ; jpsx = sp31x ; jpex = ep31x ; kpsx = sp33x ; kpex = ep33x ;
             imsy = sm32y ; imey = em32y ; jmsy = sm31y ; jmey = em31y ; kmsy = sm33y ; kmey = em33y ;
             ipsy = sp32y ; ipey = ep32y ; jpsy = sp31y ; jpey = ep31y ; kpsy = sp33y ; kpey = ep33y ;
         CASE ( DATA_ORDER_ZXY )
             ids = sd32 ; ide = ed32 ; jds = sd33 ; jde = ed33 ; kds = sd31 ; kde = ed31 ;
             ims = sm32 ; ime = em32 ; jms = sm33 ; jme = em33 ; kms = sm31 ; kme = em31 ;
             ips = sp32 ; ipe = ep32 ; jps = sp33 ; jpe = ep33 ; kps = sp31 ; kpe = ep31 ;
             imsx = sm32x ; imex = em32x ; jmsx = sm33x ; jmex = em33x ; kmsx = sm31x ; kmex = em31x ;
             ipsx = sp32x ; ipex = ep32x ; jpsx = sp33x ; jpex = ep33x ; kpsx = sp31x ; kpex = ep31x ;
             imsy = sm32y ; imey = em32y ; jmsy = sm33y ; jmey = em33y ; kmsy = sm31y ; kmey = em31y ;
             ipsy = sp32y ; ipey = ep32y ; jpsy = sp33y ; jpey = ep33y ; kpsy = sp31y ; kpey = ep31y ;
         CASE ( DATA_ORDER_ZYX )
             ids = sd33 ; ide = ed33 ; jds = sd32 ; jde = ed32 ; kds = sd31 ; kde = ed31 ;
             ims = sm33 ; ime = em33 ; jms = sm32 ; jme = em32 ; kms = sm31 ; kme = em31 ;
             ips = sp33 ; ipe = ep33 ; jps = sp32 ; jpe = ep32 ; kps = sp31 ; kpe = ep31 ;
             imsx = sm33x ; imex = em33x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm31x ; kmex = em31x ;
             ipsx = sp33x ; ipex = ep33x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp31x ; kpex = ep31x ;
             imsy = sm33y ; imey = em33y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm31y ; kmey = em31y ;
             ipsy = sp33y ; ipey = ep33y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp31y ; kpey = ep31y ;
         CASE ( DATA_ORDER_XZY )
             ids = sd31 ; ide = ed31 ; jds = sd33 ; jde = ed33 ; kds = sd32 ; kde = ed32 ;
             ims = sm31 ; ime = em31 ; jms = sm33 ; jme = em33 ; kms = sm32 ; kme = em32 ;
             ips = sp31 ; ipe = ep31 ; jps = sp33 ; jpe = ep33 ; kps = sp32 ; kpe = ep32 ;
             imsx = sm31x ; imex = em31x ; jmsx = sm33x ; jmex = em33x ; kmsx = sm32x ; kmex = em32x ;
             ipsx = sp31x ; ipex = ep31x ; jpsx = sp33x ; jpex = ep33x ; kpsx = sp32x ; kpex = ep32x ;
             imsy = sm31y ; imey = em31y ; jmsy = sm33y ; jmey = em33y ; kmsy = sm32y ; kmey = em32y ;
             ipsy = sp31y ; ipey = ep31y ; jpsy = sp33y ; jpey = ep33y ; kpsy = sp32y ; kpey = ep32y ;
         CASE ( DATA_ORDER_YZX )
             ids = sd33 ; ide = ed33 ; jds = sd31 ; jde = ed31 ; kds = sd32 ; kde = ed32 ;
             ims = sm33 ; ime = em33 ; jms = sm31 ; jme = em31 ; kms = sm32 ; kme = em32 ;
             ips = sp33 ; ipe = ep33 ; jps = sp31 ; jpe = ep31 ; kps = sp32 ; kpe = ep32 ;
             imsx = sm33x ; imex = em33x ; jmsx = sm31x ; jmex = em31x ; kmsx = sm32x ; kmex = em32x ;
             ipsx = sp33x ; ipex = ep33x ; jpsx = sp31x ; jpex = ep31x ; kpsx = sp32x ; kpex = ep32x ;
             imsy = sm33y ; imey = em33y ; jmsy = sm31y ; jmey = em31y ; kmsy = sm32y ; kmey = em32y ;
             ipsy = sp33y ; ipey = ep33y ; jpsy = sp31y ; jpey = ep31y ; kpsy = sp32y ; kpey = ep32y ;
      END SELECT data_ordering
      CALL model_to_grid_config_rec ( id , model_config_rec , config_flags )
      CALL nl_get_sr_x( id , sr_x )
      CALL nl_get_sr_y( id , sr_y )
      tl = tl_in
      inter_domain = inter_domain_in
      okay_to_alloc = okay_to_alloc_in
      CALL get_initial_data_value ( initial_data_value )
      setinitval = setinitval_in
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist12=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist12=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist13=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist13=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist14=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist14=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist15=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist15=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist16=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist16=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist17=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist17=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist18=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist18=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist19=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist19=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist20_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist20=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist20=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist21_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist21=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist21=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist22_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist22=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist22=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist23_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist23=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist23=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist24_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist24=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist24=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
   END SUBROUTINE alloc_space_field_core_4
END MODULE module_alloc_space_4
