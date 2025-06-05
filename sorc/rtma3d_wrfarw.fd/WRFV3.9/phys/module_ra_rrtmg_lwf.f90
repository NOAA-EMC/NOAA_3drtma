module memory
end module
      module parrrtm_f
      save
      integer , parameter :: mxlay = 100
      integer , parameter :: mg = 16
      integer , parameter :: nbndlw = 16
      integer , parameter :: maxxsec= 4
      integer , parameter :: mxmol = 38
      integer , parameter :: maxinpx= 38
      integer , parameter :: nmol = 7
      integer , parameter :: ngptlw = 140
      integer , parameter :: ng1 = 10
      integer , parameter :: ng2 = 12
      integer , parameter :: ng3 = 16
      integer , parameter :: ng4 = 14
      integer , parameter :: ng5 = 16
      integer , parameter :: ng6 = 8
      integer , parameter :: ng7 = 12
      integer , parameter :: ng8 = 8
      integer , parameter :: ng9 = 12
      integer , parameter :: ng10 = 6
      integer , parameter :: ng11 = 8
      integer , parameter :: ng12 = 8
      integer , parameter :: ng13 = 4
      integer , parameter :: ng14 = 2
      integer , parameter :: ng15 = 2
      integer , parameter :: ng16 = 2
      integer , parameter :: ngs1 = 10
      integer , parameter :: ngs2 = 22
      integer , parameter :: ngs3 = 38
      integer , parameter :: ngs4 = 52
      integer , parameter :: ngs5 = 68
      integer , parameter :: ngs6 = 76
      integer , parameter :: ngs7 = 88
      integer , parameter :: ngs8 = 96
      integer , parameter :: ngs9 = 108
      integer , parameter :: ngs10 = 114
      integer , parameter :: ngs11 = 122
      integer , parameter :: ngs12 = 130
      integer , parameter :: ngs13 = 134
      integer , parameter :: ngs14 = 136
      integer , parameter :: ngs15 = 138
      end module parrrtm_f
      module rrlw_cld_f
      save
      real :: abscld1
      real , dimension(2) :: absice0
      real , dimension(2,5) :: absice1
      real , dimension(43,16) :: absice2
      real , dimension(46,16) :: absice3
      real :: absliq0
      real , dimension(58,16) :: absliq1
      end module rrlw_cld_f
      module rrlw_con_f
      save
      real :: fluxfac, heatfac
      real :: oneminus, pi, grav
      real :: planck, boltz, clight
      real :: avogad, alosmt, gascon
      real :: radcn1, radcn2
      real :: sbcnst, secdy
      end module rrlw_con_f
      module rrlw_kg01_f
      use memory
      save
      integer , parameter :: no1 = 16
      real :: fracrefao(no1) , fracrefbo(no1)
      real :: kao(5,13,no1)
      real :: kbo(5,13:59,no1)
      real :: kao_mn2(19,no1) , kbo_mn2(19,no1)
      real :: selfrefo(10,no1), forrefo(4,no1)
      integer , parameter :: ng1 = 10
      real :: ka(5,13,ng1) , absa(65,ng1)
      real :: kb(5,13:59,ng1), absb(235,ng1)
      real ,target :: fracrefa(ng1) , fracrefb(ng1)
      real ,target :: ka_mn2(19,ng1) , kb_mn2(19,ng1)
      real ,target :: selfref(10,ng1), forref(4,ng1)
      real ,allocatable :: kad(:,:,:), absad(:,:), absbd(:,:)
      real ,allocatable :: kbd(:,:,:)
      real ,pointer :: fracrefad(:) , fracrefbd(:)
      real ,pointer :: ka_mn2d(:,:) , kb_mn2d(:,:)
      real ,pointer :: selfrefd(:,:), forrefd(:,:)
      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU1
        fracrefad=>fracrefa
        fracrefbd=>fracrefb
        ka_mn2d=>ka_mn2
        kb_mn2d=>kb_mn2
        selfrefd=>selfref
        forrefd=>forref
        if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng1)); absad=absa
        if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng1)); absbd=absb
      end subroutine
      subroutine reg1
      end subroutine
      end module rrlw_kg01_f
      module rrlw_kg02_f
      use memory
      save
      integer , parameter :: no2 = 16
      real ,target :: kao(5,13,no2)
      real ,target :: kbo(5,13:59,no2)
      real ,target :: fracrefao(no2) , fracrefbo(no2)
      real ,target :: selfrefo(10,no2) , forrefo(4,no2)
      real ,pointer :: fracrefaod(:) , fracrefbod(:)
      real ,pointer :: selfrefod(:,:) , forrefod(:,:)
      integer , parameter :: ng2 = 12
      real ,target :: fracrefa(ng2) , fracrefb(ng2)
      real :: ka(5,13,ng2) , absa(65,ng2)
      real :: kb(5,13:59,ng2), absb(235,ng2)
      real ,target :: selfref(10,ng2), forref(4,ng2)
      real ,pointer :: fracrefad(:) , fracrefbd(:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: selfrefd(:,:), forrefd(:,:)
      real :: refparam(13)
      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU2
        fracrefaod=>fracrefao
        fracrefbod=>fracrefbo
        selfrefod=>selfrefo
        forrefod=>forrefo
        fracrefad=>fracrefa
        fracrefbd=>fracrefb
        if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng2)); absad=absa
        if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng2)); absbd=absb
        selfrefd=>selfref
        forrefd=>forref
      end subroutine
      subroutine reg2
      end subroutine
      end module rrlw_kg02_f
      module rrlw_kg03_f
      use memory
      save
      integer , parameter :: no3 = 16
      real ,target :: fracrefao(no3,9) ,fracrefbo(no3,5)
      real ,target :: kao(9,5,13,no3)
      real ,target :: kbo(5,5,13:59,no3)
      real ,target :: kao_mn2o(9,19,no3), kbo_mn2o(5,19,no3)
      real ,target :: selfrefo(10,no3)
      real ,target :: forrefo(4,no3)
      real ,pointer :: fracrefaod(:,:) ,fracrefbod(:,:)
      real ,pointer :: kao_mn2od(:,:,:), kbo_mn2od(:,:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng3 = 16
      real ,target :: fracrefa(ng3,9) ,fracrefb(ng3,5)
      real :: ka(9,5,13,ng3) ,absa(585,ng3)
      real :: kb(5,5,13:59,ng3),absb(1175,ng3)
      real ,target :: ka_mn2o(9,19,ng3), kb_mn2o(5,19,ng3)
      real ,target :: selfref(10,ng3)
      real ,target :: forref(4,ng3)
      real ,pointer :: fracrefad(:,:) ,fracrefbd(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mn2od(:,:,:), kb_mn2od(:,:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
      contains
      subroutine copyToGPU3
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       kao_mn2od=>kao_mn2o
       kbo_mn2od=>kbo_mn2o
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng3)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 1175, ng3)); absbd=absb
       ka_mn2od=>ka_mn2o
       kb_mn2od=>kb_mn2o
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg3
      end subroutine
      end module rrlw_kg03_f
      module rrlw_kg04_f
      use memory
      save
      integer , parameter :: ng4 = 14
      integer , parameter :: no4 = 16
      real ,target :: kao(9,5,13,no4)
      real ,target :: kbo(5,5,13:59,no4)
      real :: ka(9,5,13,ng4) ,absa(585,ng4)
      real :: kb(5,5,13:59,ng4),absb(1175,ng4)
      real ,target :: fracrefao(no4,9) ,fracrefbo(no4,5)
      real ,target :: selfrefo(10,no4) ,forrefo(4,no4)
      real ,pointer :: fracrefaod(:,:) ,fracrefbod(:,:)
      real ,pointer :: selfrefod(:,:) ,forrefod(:,:)
      real ,target :: fracrefa(ng4,9) ,fracrefb(ng4,5)
      real ,target :: selfref(10,ng4) ,forref(4,ng4)
      real ,pointer :: fracrefad(:,:) ,fracrefbd(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: selfrefd(:,:) ,forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
      contains
      subroutine copyToGPU4
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng4)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 1175, ng4)); absbd=absb
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg4
      end subroutine
      end module rrlw_kg04_f
      module rrlw_kg05_f
      use memory
      save
      integer , parameter :: no5 = 16
      integer , parameter :: ng5 = 16
      real :: ka(9,5,13,ng5),kb(5,5,13:59,ng5)
      real ,target :: kao(9,5,13,no5)
      real ,target :: kbo(5,5,13:59,no5)
      real ,target :: fracrefao(no5,9) ,fracrefbo(no5,5)
      real :: absa(585,ng5)
      real ,target :: kao_mo3(9,19,no5)
      real ,target :: selfrefo(10,no5)
      real ,target :: forrefo(4,no5)
      real ,target :: ccl4o(no5)
      real ,pointer :: fracrefaod(:,:) ,fracrefbod(:,:)
      real :: kaod(9,5,13,no5)
      real :: kbod(5,5,13:59,no5)
      real ,pointer :: kao_mo3d(:,:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      real ,pointer :: ccl4od(:)
      real :: absb(1175,ng5)
      real ,target :: fracrefa(ng5,9) ,fracrefb(ng5,5)
      real ,target :: ka_mo3(9,19,ng5)
      real ,target :: selfref(10,ng5)
      real ,target :: forref(4,ng5)
      real ,target :: ccl4(ng5)
      real ,pointer :: fracrefad(:,:) ,fracrefbd(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mo3d(:,:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      real ,pointer :: ccl4d(:)
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
      contains
      subroutine copyToGPU5
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       kao_mo3d=>kao_mo3
       selfrefod=>selfrefo
       forrefod=>forrefo
       ccl4od=>ccl4o
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng5)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 1175, ng5)); absbd=absb
       ka_mo3d=>ka_mo3
       selfrefd=>selfref
       forrefd=>forref
       ccl4d=>ccl4
      end subroutine
      subroutine reg5
      end subroutine
      end module rrlw_kg05_f
      module rrlw_kg06_f
      use memory
      save
      integer , parameter :: no6 = 16
      integer , parameter :: ng6 = 8
      real :: ka(5,13,ng6),absa(65,ng6)
      real ,target, dimension(no6) :: fracrefao
      real ,target :: kao(5,13,no6)
      real ,target :: kao_mco2(19,no6)
      real ,target :: selfrefo(10,no6)
      real ,target :: forrefo(4,no6)
      real ,target, dimension(no6) :: cfc11adjo
      real ,target, dimension(no6) :: cfc12o
      real ,pointer , dimension(:) :: fracrefaod
      real ,pointer :: kaod(:,:,:)
      real ,pointer :: kao_mco2d(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      real ,pointer , dimension(:) :: cfc11adjod
      real ,pointer , dimension(:) :: cfc12od
      real ,target, dimension(ng6) :: fracrefa
      real ,target :: ka_mco2(19,ng6)
      real ,target :: selfref(10,ng6)
      real ,target :: forref(4,ng6)
      real ,target, dimension(ng6) :: cfc11adj
      real ,target, dimension(ng6) :: cfc12
      real ,pointer , dimension(:) :: fracrefad
      real ,allocatable :: absad(:,:)
      real ,pointer :: ka_mco2d(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      real ,pointer , dimension(:) :: cfc11adjd
      real ,pointer , dimension(:) :: cfc12d
      equivalence (ka(1,1,1),absa(1,1))
      contains
      subroutine copyToGPU6
       fracrefaod=>fracrefao
       kaod=>kao
       kao_mco2d=>kao_mco2
       selfrefod=>selfrefo
       forrefod=>forrefo
       cfc11adjod=>cfc11adjo
       cfc12od=>cfc12o
       fracrefad=>fracrefa
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng6)); absad=absa
       ka_mco2d=>ka_mco2
       selfrefd=>selfref
       forrefd=>forref
       cfc11adjd=>cfc11adj
       cfc12d=>cfc12
      end subroutine
      subroutine reg6
      end subroutine
      end module rrlw_kg06_f
      module rrlw_kg07_f
      use memory
      save
      integer , parameter :: no7 = 16
      integer , parameter :: ng7 = 12
      real :: kaod(9,5,13,no7)
      real :: kbod(5,13:59,no7)
      real :: ka(9,5,13,ng7) ,kb(5,13:59,ng7),absa(585,ng7)
      real :: absb(235,ng7)
      real ,target, dimension(no7) :: fracrefbo
      real ,target :: fracrefao(no7,9)
      real ,target :: kao(9,5,13,no7)
      real ,target :: kbo(5,13:59,no7)
      real ,target :: kao_mco2(9,19,no7)
      real ,target :: kbo_mco2(19,no7)
      real ,target :: selfrefo(10,no7)
      real ,target :: forrefo(4,no7)
      real ,pointer , dimension(:) :: fracrefbod
      real ,pointer :: fracrefaod(:,:)
      real ,pointer :: kao_mco2d(:,:,:)
      real ,pointer :: kbo_mco2d(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      real ,target, dimension(ng7) :: fracrefb
      real ,target :: fracrefa(ng7,9)
      real ,target :: ka_mco2(9,19,ng7)
      real ,target :: kb_mco2(19,ng7)
      real ,target :: selfref(10,ng7)
      real ,target :: forref(4,ng7)
      real ,pointer , dimension(:) :: fracrefbd
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mco2d(:,:,:)
      real ,pointer :: kb_mco2d(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU7
       fracrefbd=>fracrefb
       fracrefad=>fracrefa
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng7)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng7)); absbd=absb
       ka_mco2d=>ka_mco2
       kb_mco2d=>kb_mco2
       selfrefd=>selfref
       forrefd=>forref
       fracrefbod=>fracrefbo
       fracrefaod=>fracrefao
       kao_mco2d=>kao_mco2
       kbo_mco2d=>kbo_mco2
       selfrefod=>selfrefo
       forrefod=>forrefo
      end subroutine
      subroutine reg7
      end subroutine
      end module rrlw_kg07_f
      module rrlw_kg08_f
      use memory
      save
      integer , parameter :: no8 = 16
      real ,target, dimension(no8) :: fracrefao
      real ,target, dimension(no8) :: fracrefbo
      real ,target, dimension(no8) :: cfc12o
      real ,target, dimension(no8) :: cfc22adjo
      real ,target :: kao(5,13,no8)
      real ,target :: kao_mco2(19,no8)
      real ,target :: kao_mn2o(19,no8)
      real ,target :: kao_mo3(19,no8)
      real ,target :: kbo(5,13:59,no8)
      real ,target :: kbo_mco2(19,no8)
      real ,target :: kbo_mn2o(19,no8)
      real ,target :: selfrefo(10,no8)
      real ,target :: forrefo(4,no8)
      real ,pointer , dimension(:) :: fracrefaod
      real ,pointer , dimension(:) :: fracrefbod
      real ,pointer , dimension(:) :: cfc12od
      real ,pointer , dimension(:) :: cfc22adjod
      real :: kaod(5,13,no8)
      real ,pointer :: kao_mco2d(:,:)
      real ,pointer :: kao_mn2od(:,:)
      real ,pointer :: kao_mo3d(:,:)
      real :: kbod(5,13:59,no8)
      real ,pointer :: kbo_mco2d(:,:)
      real ,pointer :: kbo_mn2od(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng8 = 8
      real ,target, dimension(ng8) :: fracrefa
      real ,target, dimension(ng8) :: fracrefb
      real ,target, dimension(ng8) :: cfc12
      real ,target, dimension(ng8) :: cfc22adj
      real :: ka(5,13,ng8) ,absa(65,ng8)
      real :: kb(5,13:59,ng8) ,absb(235,ng8)
      real ,target :: ka_mco2(19,ng8)
      real ,target :: ka_mn2o(19,ng8)
      real ,target :: ka_mo3(19,ng8)
      real ,target :: kb_mco2(19,ng8)
      real ,target :: kb_mn2o(19,ng8)
      real ,target :: selfref(10,ng8)
      real ,target :: forref(4,ng8)
      real ,pointer , dimension(:) :: fracrefad
      real ,pointer , dimension(:) :: fracrefbd
      real ,pointer , dimension(:) :: cfc12d
      real ,pointer , dimension(:) :: cfc22adjd
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mco2d(:,:)
      real ,pointer :: ka_mn2od(:,:)
      real ,pointer :: ka_mo3d(:,:)
      real ,pointer :: kb_mco2d(:,:)
      real ,pointer :: kb_mn2od(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU8
       kaod = kao
       kbod = kbo
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       cfc12od=>cfc12o
       cfc22adjod=>cfc22adjo
       kao_mco2d=>kao_mco2
       kao_mn2od=>kao_mn2o
       kao_mo3d=>kao_mo3
       kbo_mco2d=>kbo_mco2
       kbo_mn2od=>kbo_mn2o
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       cfc12d=>cfc12
       cfc22adjd=>cfc22adj
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng8)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng8)); absbd=absb
       ka_mco2d=>ka_mco2
       ka_mn2od=>ka_mn2o
       ka_mo3d=>ka_mo3
       kb_mco2d=>kb_mco2
       kb_mn2od=>kb_mn2o
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg8
      end subroutine
      end module rrlw_kg08_f
      module rrlw_kg09_f
      use memory
      save
      integer , parameter :: no9 = 16
      real ,target, dimension(no9) :: fracrefbo
      real ,target :: fracrefao(no9,9)
      real ,target :: kao(9,5,13,no9)
      real ,target :: kbo(5,13:59,no9)
      real ,target :: kao_mn2o(9,19,no9)
      real ,target :: kbo_mn2o(19,no9)
      real ,target :: selfrefo(10,no9)
      real ,target :: forrefo(4,no9)
      real ,pointer , dimension(:) :: fracrefbod
      real ,pointer :: fracrefaod(:,:)
      real :: kaod(9,5,13,no9)
      real :: kbod(5,13:59,no9)
      real ,pointer :: kao_mn2od(:,:,:)
      real ,pointer :: kbo_mn2od(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng9 = 12
      real ,target, dimension(ng9) :: fracrefb
      real ,target :: fracrefa(ng9,9)
      real :: ka(9,5,13,ng9) ,absa(585,ng9)
      real :: kb(5,13:59,ng9) ,absb(235,ng9)
      real ,target :: ka_mn2o(9,19,ng9)
      real ,target :: kb_mn2o(19,ng9)
      real ,target :: selfref(10,ng9)
      real ,target :: forref(4,ng9)
      real ,pointer , dimension(:) :: fracrefbd
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mn2od(:,:,:)
      real ,pointer :: kb_mn2od(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU9
       kaod = kao
       kbod = kbo
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng9)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng9)); absbd=absb
       kao_mn2od=>kao_mn2o
       kbo_mn2od=>kbo_mn2o
       selfrefd=>selfref
       forrefd=>forref
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       ka_mn2od=>ka_mn2o
       kb_mn2od=>kb_mn2o
       selfrefod=>selfrefo
       forrefod=>forrefo
      end subroutine
      subroutine reg9
      end subroutine
      end module rrlw_kg09_f
      module rrlw_kg10_f
      use memory
      save
      integer , parameter :: no10 = 16
      real ,target, dimension(no10) :: fracrefao
      real ,target, dimension(no10) :: fracrefbo
      real ,target :: kao(5,13,no10)
      real ,target :: kbo(5,13:59,no10)
      real ,target :: selfrefo(10,no10)
      real ,target :: forrefo(4,no10)
      real ,pointer , dimension(:) :: fracrefaod
      real ,pointer , dimension(:) :: fracrefbod
      real :: kaod(5,13,no10)
      real :: kbod(5,13:59,no10)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng10 = 6
      real ,target , dimension(ng10) :: fracrefa
      real ,target , dimension(ng10) :: fracrefb
      real :: ka(5,13,ng10) , absa(65,ng10)
      real :: kb(5,13:59,ng10), absb(235,ng10)
      real ,target :: selfref(10,ng10)
      real ,target :: forref(4,ng10)
      real ,pointer , dimension(:) :: fracrefad
      real ,pointer , dimension(:) :: fracrefbd
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU10
       kaod = kao
       kbod = kbo
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng10)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng10)); absbd=absb
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg10
      end subroutine
      end module rrlw_kg10_f
      module rrlw_kg11_f
      use memory
      save
      integer , parameter :: no11 = 16
      real ,target, dimension(no11) :: fracrefao
      real ,target, dimension(no11) :: fracrefbo
      real ,target :: kao(5,13,no11)
      real ,target :: kbo(5,13:59,no11)
      real ,target :: kao_mo2(19,no11)
      real ,target :: kbo_mo2(19,no11)
      real ,target :: selfrefo(10,no11)
      real ,target :: forrefo(4,no11)
      real ,pointer , dimension(:) :: fracrefaod
      real ,pointer , dimension(:) :: fracrefbod
      real :: kaod(5,13,no11)
      real :: kbod(5,13:59,no11)
      real ,pointer :: kao_mo2d(:,:)
      real ,pointer :: kbo_mo2d(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng11 = 8
      real ,target, dimension(ng11) :: fracrefa
      real ,target, dimension(ng11) :: fracrefb
      real :: ka(5,13,ng11) , absa(65,ng11)
      real :: kb(5,13:59,ng11), absb(235,ng11)
      real ,target :: ka_mo2(19,ng11)
      real ,target :: kb_mo2(19,ng11)
      real ,target :: selfref(10,ng11)
      real ,target :: forref(4,ng11)
      real ,pointer , dimension(:) :: fracrefad
      real ,pointer , dimension(:) :: fracrefbd
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: ka_mo2d(:,:)
      real ,pointer :: kb_mo2d(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU11
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng11)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng11)); absbd=absb
       ka_mo2d=>ka_mo2
       kb_mo2d=>kb_mo2
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg11
      end subroutine
      end module rrlw_kg11_f
      module rrlw_kg12_f
      use memory
      save
      integer , parameter :: no12 = 16
      real ,target :: fracrefao(no12,9)
      real ,target :: kao(9,5,13,no12)
      real ,target :: selfrefo(10,no12)
      real ,target :: forrefo(4,no12)
      real ,pointer :: fracrefaod(:,:)
      real :: kaod(9,5,13,no12)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng12 = 8
      real ,target :: fracrefa(ng12,9)
      real :: ka(9,5,13,ng12) ,absa(585,ng12)
      real ,target :: selfref(10,ng12)
      real ,target :: forref(4,ng12)
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1))
      contains
      subroutine copyToGPU12
       kao = kaod
       fracrefaod=>fracrefao
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng12)); absad=absa
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg12
      end subroutine
      end module rrlw_kg12_f
      module rrlw_kg13_f
      use memory
      save
      integer , parameter :: no13 = 16
      real ,target, dimension(no13) :: fracrefbo
      real ,target :: fracrefao(no13,9)
      real ,target :: kao(9,5,13,no13)
      real ,target :: kao_mco2(9,19,no13)
      real ,target :: kao_mco(9,19,no13)
      real ,target :: kbo_mo3(19,no13)
      real ,target :: selfrefo(10,no13)
      real ,target :: forrefo(4,no13)
      real ,pointer , dimension(:) :: fracrefbod
      real ,pointer :: fracrefaod(:,:)
      real :: kaod(9,5,13,no13)
      real ,pointer :: kao_mco2d(:,:,:)
      real ,pointer :: kao_mcod(:,:,:)
      real ,pointer :: kbo_mo3d(:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng13 = 4
      real ,target, dimension(ng13) :: fracrefb
      real ,target :: fracrefa(ng13,9)
      real :: ka(9,5,13,ng13) ,absa(585,ng13)
      real ,target :: ka_mco2(9,19,ng13)
      real ,target :: ka_mco(9,19,ng13)
      real ,target :: kb_mo3(19,ng13)
      real ,target :: selfref(10,ng13)
      real ,target :: forref(4,ng13)
      real ,pointer , dimension(:) :: fracrefbd
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,pointer :: ka_mco2d(:,:,:)
      real ,pointer :: ka_mcod(:,:,:)
      real ,pointer :: kb_mo3d(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1))
      contains
      subroutine copyToGPU13
       kaod = kao
       fracrefbod=>fracrefbo
       fracrefaod=>fracrefao
       kao_mco2d=>kao_mco2
       kao_mcod=>kao_mco
       kbo_mo3d=>kbo_mo3
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefbd=>fracrefb
       fracrefad=>fracrefa
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng13)); absad=absa
       ka_mco2d=>ka_mco2
       ka_mcod=>ka_mco
       kb_mo3d=>kb_mo3
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg13
      end subroutine
      end module rrlw_kg13_f
      module rrlw_kg14_f
      use memory
      save
      integer , parameter :: no14 = 16
      real ,target, dimension(no14) :: fracrefao
      real ,target, dimension(no14) :: fracrefbo
      real ,target :: kao(5,13,no14)
      real ,target :: kbo(5,13:59,no14)
      real ,target :: selfrefo(10,no14)
      real ,target :: forrefo(4,no14)
      real ,pointer , dimension(:) :: fracrefaod
      real ,pointer , dimension(:) :: fracrefbod
      real :: kaod(5,13,no14)
      real :: kbod(5,13:59,no14)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng14 = 2
      real ,target, dimension(ng14) :: fracrefa
      real ,target, dimension(ng14) :: fracrefb
      real :: ka(5,13,ng14) ,absa(65,ng14)
      real :: kb(5,13:59,ng14),absb(235,ng14)
      real ,target :: selfref(10,ng14)
      real ,target :: forref(4,ng14)
      real ,pointer , dimension(:) :: fracrefad
      real ,pointer , dimension(:) :: fracrefbd
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU14
       kaod = kao
       kbod = kbo
       fracrefaod=>fracrefao
       fracrefbod=>fracrefbo
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 65, ng14)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng14)); absbd=absb
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg14
      end subroutine
      end module rrlw_kg14_f
      module rrlw_kg15_f
      use memory
      save
      integer , parameter :: no15 = 16
      real ,target :: fracrefao(no15,9)
      real ,target :: kao(9,5,13,no15)
      real ,target :: kao_mn2(9,19,no15)
      real ,target :: selfrefo(10,no15)
      real ,target :: forrefo(4,no15)
      real ,pointer :: fracrefaod(:,:)
      real :: kaod(9,5,13,no15)
      real ,pointer :: kao_mn2d(:,:,:)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng15 = 2
      real ,target :: fracrefa(ng15,9)
      real :: ka(9,5,13,ng15) ,absa(585,ng15)
      real ,target :: ka_mn2(9,19,ng15)
      real ,target :: selfref(10,ng15)
      real ,target :: forref(4,ng15)
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,pointer :: ka_mn2d(:,:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1))
      contains
      subroutine copyToGPU15
       kaod = kao
       fracrefaod=>fracrefao
       kao_mn2d=>kao_mn2
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng15)); absad=absa
       ka_mn2d=>ka_mn2
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg15
      end subroutine
      end module rrlw_kg15_f
      module rrlw_kg16_f
      use memory
      save
      integer , parameter :: no16 = 16
      real ,target, dimension(no16) :: fracrefbo
      real ,target :: fracrefao(no16,9)
      real ,target :: kao(9,5,13,no16)
      real ,target :: kbo(5,13:59,no16)
      real ,target :: selfrefo(10,no16)
      real ,target :: forrefo(4,no16)
      real ,pointer , dimension(:) :: fracrefbod
      real ,pointer :: fracrefaod(:,:)
      real :: kaod(9,5,13,no16)
      real :: kbod(5,13:59,no16)
      real ,pointer :: selfrefod(:,:)
      real ,pointer :: forrefod(:,:)
      integer , parameter :: ng16 = 2
      real ,target, dimension(ng16) :: fracrefb
      real ,target :: fracrefa(ng16,9)
      real :: ka(9,5,13,ng16) ,absa(585,ng16)
      real :: kb(5,13:59,ng16), absb(235,ng16)
      real ,target :: selfref(10,ng16)
      real ,target :: forref(4,ng16)
      real ,pointer , dimension(:) :: fracrefbd
      real ,pointer :: fracrefad(:,:)
      real ,allocatable :: absad(:,:)
      real ,allocatable :: absbd(:,:)
      real ,pointer :: selfrefd(:,:)
      real ,pointer :: forrefd(:,:)
      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
      contains
      subroutine copyToGPU16
       kaod = kao
       kbod = kbo
       fracrefaod=>fracrefao
       selfrefod=>selfrefo
       forrefod=>forrefo
       fracrefad=>fracrefa
       fracrefbd=>fracrefb
       if (allocated(absad).eqv..true.) deallocate(absad) ;allocate( absad( 585, ng16)); absad=absa
       if (allocated(absbd).eqv..true.) deallocate(absbd) ;allocate( absbd( 235, ng16)); absbd=absb
       selfrefd=>selfref
       forrefd=>forref
      end subroutine
      subroutine reg16
      end subroutine
      end module rrlw_kg16_f
      module rrlw_ncpar
      save
      real , parameter :: cpdair = 1003.5
      integer , parameter :: maxAbsorberNameLength = 5, &
                             Absorber = 12
      character(len = maxAbsorberNameLength), dimension(Absorber), parameter :: &
      AbsorberNames = (/ &
                                'N2   ', &
                                'CCL4 ', &
                                'CFC11', &
                                'CFC12', &
                                'CFC22', &
                                'H2O  ', &
                                'CO2  ', &
                                'O3   ', &
                                'N2O  ', &
                                'CO   ', &
                                'CH4  ', &
                                'O2   ' /)
       integer , dimension(40) :: status
       integer :: i
       integer , parameter :: keylower = 9, &
                               keyupper = 5, &
                               Tdiff = 5, &
                               ps = 59, &
                               plower = 13, &
                               pupper = 47, &
                               Tself = 10, &
                               Tforeign = 4, &
                               pforeign = 4, &
                               T = 19, &
                               Tplanck = 181, &
                               band = 16, &
                               GPoint = 16, &
                               GPointSet = 2
      contains
      subroutine getAbsorberIndex(AbsorberName,AbsorberIndex)
                character(len = *), intent(in) :: AbsorberName
                integer , intent(out) :: AbsorberIndex
                integer :: m
                AbsorberIndex = -1
                do m = 1, Absorber
                        if (trim(AbsorberNames(m)) == trim(AbsorberName)) then
                                AbsorberIndex = m
                        end if
                end do
                if (AbsorberIndex == -1) then
                        print*, "Absorber name index lookup failed."
                end if
      end subroutine getAbsorberIndex
      end module rrlw_ncpar
      module rrlw_ref_f
      real , dimension(59) :: pref
      real , dimension(59) :: preflog
      real , dimension(59) :: tref
      real :: chi_mls(7,59)
      real :: chi_mlsd(7,59)
      real :: preflogd(59)
      real :: trefd(59)
      contains
      subroutine copyToGPUref()
        chi_mls = chi_mls
        preflog = preflog
        tref = tref
      end subroutine
      end module rrlw_ref_f
      module rrlw_tbl_f
      save
      integer , parameter :: ntbl = 10000
      real , parameter :: tblint = 10000.0
      real , dimension(0:ntbl) :: tau_tbl
      real , dimension(0:ntbl) :: exp_tbl
      real , dimension(0:ntbl) :: tfn_tbl
      real , parameter :: pade = 0.278
      real :: bpade
      end module rrlw_tbl_f
      module rrlw_vsn_f
      save
      character*18 hvrrtm,hvrini,hvrcld,hvrclc,hvrrtr,hvrrtx, &
                   hvrrtc,hvrset,hvrtau,hvratm,hvrutl,hvrext
      character*20 hnamrtm,hnamini,hnamcld,hnamclc,hnamrtr,hnamrtx, &
                   hnamrtc,hnamset,hnamtau,hnamatm,hnamutl,hnamext
      character*18 hvrkg
      character*20 hnamkg
      end module rrlw_vsn_f
      module rrlw_wvn_f
      use parrrtm_f, only : nbndlw, mg, ngptlw, maxinpx
      save
      integer :: ng(nbndlw)
      integer :: nspa(nbndlw)
      integer :: nspb(nbndlw)
      real :: wavenum1(nbndlw)
      real :: wavenum2(nbndlw)
      real :: delwave(nbndlw)
      real :: totplnk(181,nbndlw)
      real :: totplk16(181)
      real :: totplnkderiv(181,nbndlw)
      real :: totplk16deriv(181)
      integer :: ngc(nbndlw)
      integer :: ngs(nbndlw)
      integer :: ngn(ngptlw)
      integer :: ngb(ngptlw)
      integer :: ngm(nbndlw*mg)
      real :: wt(mg)
      real :: rwgt(nbndlw*mg)
      integer :: nxmol
      integer :: ixindx(maxinpx)
      end module rrlw_wvn_f
  module MersenneTwister_f
  implicit none
  private
  integer , parameter :: blockSize = 624, &
                        M = 397, &
                        MATRIX_A = -1727483681, &
                        UMASK = -2147483647, &
                        LMASK = 2147483647
  integer , parameter :: TMASKB= -1658038656, &
                        TMASKC= -272236544
  type randomNumberSequence
    integer :: currentElement
    integer , dimension(0:blockSize -1) :: state
  end type randomNumberSequence
  interface new_RandomNumberSequence
    module procedure initialize_scalar, initialize_vector
  end interface new_RandomNumberSequence
  public :: randomNumberSequence
  public :: new_RandomNumberSequence, finalize_RandomNumberSequence, &
            getRandomInt, getRandomPositiveInt, getRandomReal
contains
  function mixbits(u, v)
    integer , intent( in) :: u, v
    integer :: mixbits
    mixbits = ior(iand(u, UMASK), iand(v, LMASK))
  end function mixbits
  function twist(u, v)
    integer , intent( in) :: u, v
    integer :: twist
    integer , parameter, dimension(0:1) :: t_matrix = (/ 0 , MATRIX_A /)
    twist = ieor(ishft(mixbits(u, v), -1 ), t_matrix(iand(v, 1 )))
    twist = ieor(ishft(mixbits(u, v), -1 ), t_matrix(iand(v, 1 )))
  end function twist
  subroutine nextState(twister)
    type(randomNumberSequence), intent(inout) :: twister
    integer :: k
    do k = 0, blockSize - M - 1
      twister%state(k) = ieor(twister%state(k + M), &
                              twist(twister%state(k), twister%state(k + 1 )))
    end do
    do k = blockSize - M, blockSize - 2
      twister%state(k) = ieor(twister%state(k + M - blockSize), &
                              twist(twister%state(k), twister%state(k + 1 )))
    end do
    twister%state(blockSize - 1 ) = ieor(twister%state(M - 1 ), &
                                        twist(twister%state(blockSize - 1 ), twister%state(0 )))
    twister%currentElement = 0
  end subroutine nextState
  elemental function temper(y)
    integer , intent(in) :: y
    integer :: temper
    integer :: x
    x = ieor(y, ishft(y, -11))
    x = ieor(x, iand(ishft(x, 7), TMASKB))
    x = ieor(x, iand(ishft(x, 15), TMASKC))
    temper = ieor(x, ishft(x, -18))
  end function temper
  function initialize_scalar(seed) result(twister)
    integer , intent(in ) :: seed
    type(randomNumberSequence) :: twister
    integer :: i
    twister%state(0) = iand(seed, -1 )
    do i = 1, blockSize - 1
       twister%state(i) = 1812433253 * ieor(twister%state(i-1), &
                                            ishft(twister%state(i-1), -30 )) + i
       twister%state(i) = iand(twister%state(i), -1 )
    end do
    twister%currentElement = blockSize
  end function initialize_scalar
  function initialize_vector(seed) result(twister)
    integer , dimension(0:), intent(in) :: seed
    type(randomNumberSequence) :: twister
    integer :: i, j, k, nFirstLoop, nWraps
    nWraps = 0
    twister = initialize_scalar(19650218 )
    nFirstLoop = max(blockSize, size(seed))
    do k = 1, nFirstLoop
       i = mod(k + nWraps, blockSize)
       j = mod(k - 1, size(seed))
       if(i == 0) then
         twister%state(i) = twister%state(blockSize - 1)
         twister%state(1) = ieor(twister%state(1), &
                                 ieor(twister%state(1-1), &
                                      ishft(twister%state(1-1), -30 )) * 1664525 ) + &
                            seed(j) + j
         twister%state(i) = iand(twister%state(i), -1 )
         nWraps = nWraps + 1
       else
         twister%state(i) = ieor(twister%state(i), &
                                 ieor(twister%state(i-1), &
                                      ishft(twister%state(i-1), -30 )) * 1664525 ) + &
                            seed(j) + j
         twister%state(i) = iand(twister%state(i), -1 )
      end if
    end do
    do i = mod(nFirstLoop, blockSize) + nWraps + 1, blockSize - 1
      twister%state(i) = ieor(twister%state(i), &
                              ieor(twister%state(i-1), &
                                   ishft(twister%state(i-1), -30 )) * 1566083941 ) - i
      twister%state(i) = iand(twister%state(i), -1 )
    end do
    twister%state(0) = twister%state(blockSize - 1)
    do i = 1, mod(nFirstLoop, blockSize) + nWraps
      twister%state(i) = ieor(twister%state(i), &
                              ieor(twister%state(i-1), &
                                   ishft(twister%state(i-1), -30 )) * 1566083941 ) - i
      twister%state(i) = iand(twister%state(i), -1 )
    end do
    twister%state(0) = UMASK
    twister%currentElement = blockSize
  end function initialize_vector
  function getRandomInt(twister)
    type(randomNumberSequence), intent(inout) :: twister
    integer :: getRandomInt
    if(twister%currentElement >= blockSize) call nextState(twister)
    getRandomInt = temper(twister%state(twister%currentElement))
    twister%currentElement = twister%currentElement + 1
  end function getRandomInt
  function getRandomPositiveInt(twister)
    type(randomNumberSequence), intent(inout) :: twister
    integer :: getRandomPositiveInt
    integer :: localInt
    localInt = getRandomInt(twister)
    getRandomPositiveInt = ishft(localInt, -1)
  end function getRandomPositiveInt
  function getRandomReal(twister)
    type(randomNumberSequence), intent(inout) :: twister
    real :: getRandomReal
    integer :: localInt
    localInt = getRandomInt(twister)
    if(localInt < 0) then
      getRandomReal = (localInt + 2.0**32 )/(2.0**32 - 1.0 )
    else
      getRandomReal = (localInt )/(2.0**32 - 1.0 )
    end if
  end function getRandomReal
  subroutine finalize_RandomNumberSequence(twister)
    type(randomNumberSequence), intent(inout) :: twister
      twister%currentElement = blockSize
      twister%state(:) = 0
  end subroutine finalize_RandomNumberSequence
  end module MersenneTwister_f
  module mcica_random_numbers_f
  use MersenneTwister_f, only: randomNumberSequence, &
                             new_RandomNumberSequence, getRandomReal
  implicit none
  private
  type randomNumberStream
    type(randomNumberSequence) :: theNumbers
  end type randomNumberStream
  interface getRandomNumbers
    module procedure getRandomNumber_Scalar, getRandomNumber_1D, getRandomNumber_2D
  end interface getRandomNumbers
  interface initializeRandomNumberStream
    module procedure initializeRandomNumberStream_S, initializeRandomNumberStream_V
  end interface initializeRandomNumberStream
  public :: randomNumberStream, &
            initializeRandomNumberStream, getRandomNumbers
contains
  function initializeRandomNumberStream_S(seed) result(new)
    integer , intent( in) :: seed
    type(randomNumberStream) :: new
    new%theNumbers = new_RandomNumberSequence(seed)
  end function initializeRandomNumberStream_S
  function initializeRandomNumberStream_V(seed) result(new)
    integer , dimension(:), intent( in) :: seed
    type(randomNumberStream) :: new
    new%theNumbers = new_RandomNumberSequence(seed)
  end function initializeRandomNumberStream_V
  subroutine getRandomNumber_Scalar(stream, number)
    type(randomNumberStream), intent(inout) :: stream
    real , intent( out) :: number
    number = getRandomReal(stream%theNumbers)
  end subroutine getRandomNumber_Scalar
  subroutine getRandomNumber_1D(stream, numbers)
    type(randomNumberStream), intent(inout) :: stream
    real , dimension(:), intent( out) :: numbers
    integer :: i
    do i = 1, size(numbers)
      numbers(i) = getRandomReal(stream%theNumbers)
    end do
  end subroutine getRandomNumber_1D
  subroutine getRandomNumber_2D(stream, numbers)
    type(randomNumberStream), intent(inout) :: stream
    real , dimension(:, :), intent( out) :: numbers
    integer :: i
    do i = 1, size(numbers, 2)
      call getRandomNumber_1D(stream, numbers(:, i))
    end do
  end subroutine getRandomNumber_2D
  end module mcica_random_numbers_f
      module gpu_mcica_subcol_gen_lw
      use parrrtm_f, only : nbndlw, ngptlw, mxlay
      use rrlw_con_f, only: grav
      use rrlw_wvn_f, only: ngb
      use rrlw_vsn_f
      implicit none
      contains
      subroutine mcica_subcol_lwg(colstart, ncol, nlay, icld, permuteseed, irng, &
                       pmidd,clwpd,ciwpd,cswpd,taucd, &
                       play, cldfrac, ciwp, clwp, cswp, tauc, ngbd, cldfmcl, &
                       ciwpmcl, clwpmcl, cswpmcl, taucmcl)
      integer , intent(in) :: colstart
      integer , intent(in) :: ncol
      integer , intent(in) :: nlay
      integer , intent(in) :: icld
      integer , intent(in) :: permuteseed
      integer , intent(in) :: irng
      real , intent(in) :: play(:,:)
      real , intent(in) :: cldfrac(:,:)
      real , intent(in) :: tauc(:,:,:)
      real , intent(in) :: ciwp(:,:)
      real , intent(in) :: clwp(:,:)
      real , intent(in) :: cswp(:,:)
      integer , intent(in) :: ngbd(:)
      real , intent(out) :: cldfmcl(:,:,:)
      real , intent(out) :: ciwpmcl(:,:,:)
      real , intent(out) :: clwpmcl(:,:,:)
      real , intent(out) :: cswpmcl(:,:,:)
      real , intent(out) :: taucmcl(:,:,:)
      real :: pmidd(:, :)
      real :: clwpd(:,:), ciwpd(:,:), cswpd(:,:), taucd(:,:,:)
      integer , parameter :: nsubclw = ngptlw
      integer :: ilev
      real :: pmid(ncol, nlay)
      integer, save :: counter = 0
      integer :: i,j,k,tk
      real :: t1, t2
      if (icld.eq.0) then
        cldfmcl = 0.0
        ciwpmcl = 0.0
        clwpmcl = 0.0
        cswpmcl = 0.0
        taucmcl = 0.0
        return
      end if
      if (icld.lt.0.or.icld.gt.4) then
         stop 'MCICA_SUBCOL: INVALID ICLD'
      endif
      pmidd(1:ncol,:nlay) = play(colstart:colstart+ncol-1,:nlay)*1.e2
      end subroutine mcica_subcol_lwg
       subroutine generate_stochastic_cloudsg(ncol, nlay, icld, ngbd, &
                                 pmidd,cldfracd,clwpd,ciwpd,cswpd,taucd,changeSeed, &
                                 cld_stoch, clwp_stoch, ciwp_stoch, cswp_stoch, &
                                 tauc_stoch)
      integer , intent(in) :: ncol
      integer , intent(in) :: nlay
      integer , intent(in) :: icld
       integer , intent(in) :: ngbd(:)
      real :: pmidd(:, :)
      real :: cldfracd(:,:), clwpd(:,:), ciwpd(:,:), cswpd(:,:), taucd(:,:,:)
      integer, intent(in) :: changeSeed
      real , intent(out) :: cld_stoch(:,:,:)
      real , intent(out) :: clwp_stoch(:,:,:)
      real , intent(out) :: ciwp_stoch(:,:,:)
      real , intent(out) :: cswp_stoch(:,:,:)
      real , intent(out) :: tauc_stoch(:,:,:)
       real :: RIND1, RIND2, ZCW, SIGMA_QCW
       integer :: IND1, IND2
       real :: CDF3(mxlay)
       real :: cfs
       integer, parameter :: nsubcol = 140
      real :: CDF(ncol,mxlay), CDF2(mxlay)
      integer,dimension(ncol) :: seed1, seed2, seed3, seed4
      real ,dimension(ncol) :: rand_num
      integer :: iseed
      real :: rand_num_mt
      integer :: ilev, isubcol, i, n
      integer :: iplon, gp
      integer :: m, k, n1, kiss
      m(k, n1) = ieor (k, ishft (k, n1) )
    do iplon = 1, ncol
       seed1(iplon) = (pmidd(iplon,1) - int(pmidd(iplon,1))) * 1000000000
       seed2(iplon) = (pmidd(iplon,2) - int(pmidd(iplon,2))) * 1000000000
       seed3(iplon) = (pmidd(iplon,3) - int(pmidd(iplon,3))) * 1000000000
       seed4(iplon) = (pmidd(iplon,4) - int(pmidd(iplon,4))) * 1000000000
       do i=1,changeSeed
          seed1(iplon) = 69069 * seed1(iplon) + 1327217885
          seed2(iplon) = m (m (m (seed2(iplon), 13), - 17), 5)
          seed3(iplon) = 18000 * iand (seed3(iplon), 65535) + ishft (seed3(iplon), - 16)
          seed4(iplon) = 30903 * iand (seed4(iplon), 65535) + ishft (seed4(iplon), - 16)
          kiss = seed1(iplon) + seed2(iplon) + ishft (seed3(iplon), 16) + seed4(iplon)
          rand_num(iplon) = kiss*2.328306e-10 + 0.5
       enddo
    enddo
     do gp = 1, nsubcol
       select case (icld)
       case(1)
   CALL wrf_error_fatal3("<stdin>",2735,&
"icld == 1 not supported: module_ra_rrtmg_lwf.F")
       case(2)
           do ilev = 1,nlay
            do iplon = 1, ncol
             seed1(iplon) = 69069 * seed1(iplon) + 1327217885
             seed2(iplon) = m (m (m (seed2(iplon), 13), - 17), 5)
             seed3(iplon) = 18000 * iand (seed3(iplon), 65535) + ishft (seed3(iplon), - 16)
             seed4(iplon) = 30903 * iand (seed4(iplon), 65535) + ishft (seed4(iplon), - 16)
             kiss = seed1(iplon) + seed2(iplon) + ishft (seed3(iplon), 16) + seed4(iplon)
             CDF(iplon,ilev) = kiss*2.328306e-10 + 0.5
            enddo
           end do
           do ilev = 2,nlay
            do iplon = 1, ncol
             if (CDF(iplon,ilev-1) > 1. - cldfracd(iplon, ilev-1)) then
                CDF(iplon,ilev) = CDF(iplon,ilev-1)
             else
                 CDF(iplon,ilev) = CDF(iplon,ilev) * (1. - cldfracd(iplon, ilev-1))
             end if
            enddo
           end do
       case(3)
            do iplon = 1, ncol
          seed1(iplon) = 69069 * seed1(iplon) + 1327217885
          seed2(iplon) = m (m (m (seed2(iplon), 13), - 17), 5)
          seed3(iplon) = 18000 * iand (seed3(iplon), 65535) + ishft (seed3(iplon), - 16)
          seed4(iplon) = 30903 * iand (seed4(iplon), 65535) + ishft (seed4(iplon), - 16)
          kiss = seed1(iplon) + seed2(iplon) + ishft (seed3(iplon), 16) + seed4(iplon)
          rand_num(iplon) = kiss*2.328306e-10 + 0.5
            enddo
           do ilev = 1,nlay
            do iplon = 1, ncol
             CDF(iplon,ilev) = rand_num(iplon)
            enddo
           end do
       end select
      n = ngbd(gp)
      do ilev = 1,nlay
       do iplon = 1, ncol
        cfs = cldfracd(iplon, ilev)
               if (CDF(iplon,ilev) >=1. - cfs) then
                  cld_stoch(iplon,gp,ilev) = 1.
                  clwp_stoch(iplon,gp,ilev) = clwpd(iplon,ilev)
                  ciwp_stoch(iplon,gp,ilev) = ciwpd(iplon,ilev)
                  cswp_stoch(iplon,gp,ilev) = cswpd(iplon,ilev)
                  tauc_stoch(iplon,gp,ilev) = taucd(iplon,n,ilev)
               else
                  cld_stoch(iplon,gp,ilev) = 0.
                  clwp_stoch(iplon,gp,ilev) = 0.
                  ciwp_stoch(iplon,gp,ilev) = 0.
                  cswp_stoch(iplon,gp,ilev) = 0.
                  tauc_stoch(iplon,gp,ilev) = 0.
               endif
       enddo
      enddo
      end do
      end subroutine generate_stochastic_cloudsg
      subroutine kissvec(seed1,seed2,seed3,seed4,ran_arr)
      real , intent(inout) :: ran_arr
      integer , intent(inout) :: seed1,seed2,seed3,seed4
      integer :: i,sz,kiss
      integer :: m, k, n
      m(k, n) = ieor (k, ishft (k, n) )
      seed1 = 69069 * seed1 + 1327217885
      seed2 = m (m (m (seed2, 13), - 17), 5)
      seed3 = 18000 * iand (seed3, 65535) + ishft (seed3, - 16)
      seed4 = 30903 * iand (seed4, 65535) + ishft (seed4, - 16)
      kiss = seed1 + seed2 + ishft (seed3, 16) + seed4
      ran_arr = kiss*2.328306e-10 + 0.5
      end subroutine kissvec
      end module gpu_mcica_subcol_gen_lw
      module gpu_rrtmg_lw_cldprmc
      use parrrtm_f, only : ngptlw, nbndlw
      use rrlw_cld_f, only: abscld1, absliq0, absliq1, &
                          absice0, absice1, absice2, absice3
      use rrlw_vsn_f, only: hvrclc, hnamclc
      implicit none
      contains
      subroutine cldprmcg(ncol, nlayers, &
                inflagd,iceflagd,liqflagd,ciwpmcd,clwpmcd,cswpmcd,relqmcd,reicmcd,resnmcd, &
                absice0d,absice1d,absice2d,absice3d,absliq1d, &
                                  cldfmc, taucmc, ngb, icb, ncbands, icldlyr)
      integer, value, intent(in) :: ncol
      integer, value, intent(in) :: nlayers
      real , intent(in) :: cldfmc(8, ngptlw, nlayers+1)
      integer , intent(out) :: icldlyr( 8, nlayers+1)
      integer , dimension(140), intent(in) :: ngb
      integer , intent(in) :: icb(16)
      real , intent(inout) :: taucmc(:,:,:)
      real , parameter :: absliq0 = 0.0903614
      integer , intent(out) :: ncbands(:)
      integer :: inflagd(:), iceflagd(:), liqflagd(:)
      real :: ciwpmcd(:,:,:)
      real :: clwpmcd(:,:,:)
      real :: cswpmcd(:,:,:)
      real :: relqmcd(:,:)
      real :: reicmcd(:,:)
      real :: resnmcd(:,:)
      real, dimension(2) :: absice0d
      real, dimension(2,5) :: absice1d
      real, dimension(43,16) :: absice2d
      real, dimension(46,16) :: absice3d
      real, dimension(58,16) :: absliq1d
      integer :: iplon
      integer :: lay
      integer :: ib
      integer :: ig
      integer :: index
      real :: abscoice
      real :: abscoliq
      real :: abscosno
      real :: cwp
      real :: radice
      real :: radliq
      real :: radsno
      real :: factor
      real :: fint
      real , parameter :: eps = 1.e-6
      real , parameter :: cldmin = 1.e-20
      character*256 errmess
    do iplon = 1, 8
      do lay = 1, nlayers
        do ig = 1, ngptlw
          ncbands(iplon) = 1
          if (cldfmc(iplon,ig,lay) .eq. 1. ) then
            icldlyr(iplon, lay)=1
          endif
          cwp = ciwpmcd(iplon,ig,lay) + clwpmcd(iplon,ig,lay) + cswpmcd(iplon,ig,lay)
          if (cldfmc(iplon,ig,lay) .ge. cldmin .and. &
             (cwp .ge. cldmin .or. taucmc(iplon,ig,lay) .ge. cldmin)) then
            if(inflagd(iplon) .ge. 2) then
               radice = reicmcd(iplon, lay)
               if (ciwpmcd(iplon,ig,lay)+cswpmcd(iplon,ig,lay) .eq. 0.0) then
                  abscoice = 0.0
                  abscosno = 0.0
               elseif (iceflagd(iplon) .eq. 0) then
                  abscoice= absice0d(1) + absice0d(2)/radice
                  abscosno = 0.0
               elseif (iceflagd(iplon) .eq. 1) then
                  ncbands(iplon) = 5
                  ib = icb(ngb(ig))
                  abscoice = absice1d(1,ib) + absice1d(2,ib)/radice
                  abscosno = 0.0
               elseif (iceflagd(iplon) .eq. 2) then
                  ncbands(iplon) = 16
                  factor = (radice - 2.)/3.
                  index = int(factor)
                     if (index .le. 0) index = 1
                     if (index .ge. 43) index = 42
                  fint = factor - float(index)
                  ib = ngb(ig)
                  abscoice = &
                      absice2d(index,ib) + fint * &
                      (absice2d(index+1,ib) - (absice2d(index,ib)))
                  abscosno = 0.0
               elseif (iceflagd(iplon) .ge. 3) then
                  ncbands(iplon) = 16
                  factor = (radice - 2.)/3.
                  index = int(factor)
                  if (index .le. 0) index = 1
                  if (index .ge. 46) index = 45
                  fint = factor - float(index)
                  ib = ngb(ig)
                  abscoice= &
                      absice3d(index,ib) + fint * &
                      (absice3d(index+1,ib) - (absice3d(index,ib)))
                  abscosno = 0.0
               endif
               if (cswpmcd(iplon,ig,lay).gt.0.0 .and. iceflagd(iplon) .eq. 5) then
                  radsno = resnmcd(iplon,lay)
                  if (radsno .lt. 5.0 .or. radsno .gt. 140.0) then
                         write(errmess,'(A,i5,i5,i5,f8.2,f8.2)' ) &
               'ERROR: SNOW GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS' &
               ,iplon,ig, lay, cswpmcd(iplon,ig,lay), radsno
                         call wrf_error_fatal3("<stdin>",3035,&
errmess)
                  end if
                  ncbands(iplon) = 16
                  factor = (radsno - 2.)/3.
                  index = int(factor)
                  if (index .le. 0) index = 1
                  if (index .ge. 46) index = 45
                  fint = factor - float(index)
                  ib = ngb(ig)
                  abscosno = &
                      absice3d(index,ib) + fint * &
                      (absice3d(index+1,ib) - (absice3d(index,ib)))
               endif
               if (clwpmcd(iplon,ig,lay) .eq. 0.0) then
                 abscoliq = 0.0
               else if (liqflagd(iplon) .eq. 0) then
                 abscoliq = absliq0
               else if (liqflagd(iplon) .eq. 1) then
                 radliq = relqmcd(iplon, lay)
                 index = int(radliq - 1.5 )
                     if (index .le. 0) index = 1
                     if (index .ge. 58) index = 57
                 fint = radliq - 1.5 - float(index)
                 ib = ngb(ig)
                 abscoliq = &
                     absliq1d(index,ib) + fint * &
                     (absliq1d(index+1,ib) - (absliq1d(index,ib)))
               endif
               taucmc(iplon,ig,lay) = ciwpmcd(iplon,ig,lay) * abscoice + &
                                      clwpmcd(iplon,ig,lay) * abscoliq + &
                                      cswpmcd(iplon,ig,lay) * abscosno
            endif
          endif
        end do
      end do
    end do
      end subroutine cldprmcg
      subroutine allocateGPUcldprmcg(ncol, nlay, ngptlw)
         integer , intent(in) :: nlay, ngptlw, ncol
      end subroutine
      subroutine deallocateGPUcldprmcg()
      end subroutine
      subroutine copyGPUcldprmcg(inflag, iceflag, liqflag,&
                                 absice0, absice1, absice2, absice3, absliq1)
         integer :: inflag(:), iceflag(:), liqflag(:)
         real , dimension(:) :: absice0
         real , dimension(:,:) :: absice1
         real , dimension(:,:) :: absice2
         real , dimension(:,:) :: absice3
         real , dimension(:,:) :: absliq1
      end subroutine
      end module gpu_rrtmg_lw_cldprmc
      module gpu_rrtmg_lw_rtrnmc
      use parrrtm_f, only : mg, nbndlw, ngptlw, mxlay
      use rrlw_con_f, only: fluxfac, heatfac
      use rrlw_tbl_f, only: bpade, tblint, tau_tbl, exp_tbl, tfn_tbl, ntbl
      implicit none
      contains
      subroutine rtrnmcg(ncol, nlayers, istart, iend, iout &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pzd,pwvcmd,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrvd,bpaded,heatfacd,fluxfacd,a0d,a1d,a2d &
         ,delwaved,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                                 ,ngb,icldlyr, taug, fracsd, cldfmcd)
      integer(kind=4), value, intent(in) :: nlayers
      integer(kind=4), value, intent(in) :: ncol
      integer(kind=4), value, intent(in) :: istart
      integer(kind=4), value, intent(in) :: iend
      integer(kind=4), value, intent(in) :: iout
      integer , intent(in) :: ngb(:)
      integer , intent(in) :: icldlyr(:,:)
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      real :: cldfmcd(:,:,:)
      integer :: ncol_,nlayers_,nbndlw_,ngptlw_
      integer :: ngsd(nbndlw)
      real :: taucmcd(8, ngptlw_, nlayers_+1)
      real , dimension(8, 0:nlayers_+1) :: pzd
      real , dimension(8) :: pwvcmd
      real , dimension(8,nbndlw_) :: semissd
      real , dimension(8,nlayers_+1,nbndlw_) :: planklayd
      real , dimension(8,0:nlayers_+1,nbndlw_) :: planklevd
      real, dimension(8,nbndlw_) :: plankbndd
      real :: gurad(8,ngptlw_,0:nlayers_+1)
      real :: gdrad(8,ngptlw_,0:nlayers_+1)
      real :: gclrurad(8,ngptlw_,0:nlayers_+1)
      real :: gclrdrad(8,ngptlw_,0:nlayers_+1)
      real :: gdtotuflux_dtd(8, ngptlw_, 0:nlayers_+1)
      real :: gdtotuclfl_dtd(8, ngptlw_, 0:nlayers_+1)
      integer :: idrvd
      real :: bpaded
      real :: heatfacd
      real :: fluxfacd
      real :: a0d(nbndlw_), a1d(nbndlw_), a2d(nbndlw_)
      real :: delwaved(nbndlw_)
      real :: totufluxd(8, 0:nlayers_+1)
      real :: totdfluxd(8, 0:nlayers_+1)
      real :: fnetd(8, 0:nlayers_+1)
      real :: htrd(8, 0:nlayers_+1)
      real :: totuclfld(8, 0:nlayers_+1)
      real :: totdclfld(8, 0:nlayers_+1)
      real :: fnetcd(8, 0:nlayers_+1)
      real :: htrcd(8, 0:nlayers_+1)
      real :: dtotuflux_dtd(8, 0:nlayers_+1)
      real :: dtotuclfl_dtd(8, 0:nlayers_+1)
      real :: dplankbnd_dtd(8,nbndlw_)
      real :: atot( ncol, mxlay)
      real :: atrans( ncol, mxlay)
      real :: bbugas( ncol, mxlay)
      real :: bbutot( ncol, mxlay)
      real :: uflux( ncol, 0:mxlay)
      real :: dflux( ncol, 0:mxlay)
      real :: uclfl( ncol, 0:mxlay)
      real :: dclfl( ncol, 0:mxlay)
      real :: odclds
      real :: efclfracs
      real :: absclds
      real :: secdiff (ncol)
      real :: transcld, radld (ncol), radclrd (ncol), plfrac, blay, dplankup, dplankdn
      real :: odepth, odtot, odepth_rec, odtot_rec, gassrc
      real :: tblind, tfactot, bbd, bbdtot, tfacgas, transc, tausfac
      real :: rad0, reflect, radlu (ncol) , radclru (ncol)
      real :: d_rad0_dt, d_radlu_dt (ncol) , d_radclru_dt (ncol)
      integer :: ibnd, ib, lay, lev, l, ig
      integer :: igc
      integer :: iclddn (ncol)
      integer :: ittot, itgas, itr
      real , parameter :: wtdiff = 0.5
      real , parameter :: rec_6 = 0.166667
      integer :: iplon
      real :: bbb
      do igc = 1, 140
         ibnd = ngb(igc)
       do iplon = 1, ncol
         if (ibnd.eq.1 .or. ibnd.eq.4 .or. ibnd.ge.10) then
           SECDIFF(iplon) = 1.66
         else
           SECDIFF(iplon) = a0d(ibnd) + a1d(ibnd)*exp(a2d(ibnd)*pwvcmd(iplon))
           if (SECDIFF(iplon) .gt. 1.80 ) SECDIFF(iplon) = 1.80
           if (SECDIFF(iplon) .lt. 1.50 ) SECDIFF(iplon) = 1.50
         endif
         gurad(iplon, igc, 0) = 0.0
         gdrad(iplon, igc, 0) = 0.0
         gclrurad(iplon, igc, 0) = 0.0
         gclrdrad(iplon, igc, 0) = 0.0
         if (idrvd .eq. 1) then
            gdtotuflux_dtd(iplon,igc,0) = 0.0
            gdtotuclfl_dtd(iplon,igc,0) = 0.0
         endif
       enddo
         do lay = 1, nlayers
       do iplon = 1, ncol
            gurad(iplon, igc, lay) = 0.0
            gdrad(iplon, igc, lay) = 0.0
            gclrurad(iplon, igc, lay) = 0.0
            gclrdrad(iplon, igc, lay) = 0.0
            if (idrvd .eq. 1) then
               gdtotuflux_dtd(iplon,igc,lay) = 0.0
               gdtotuclfl_dtd(iplon,igc,lay) = 0.0
            endif
       enddo
         enddo
         radld = 0.
         radclrd = 0.
         iclddn = 0
         do lev = nlayers, 1, -1
       do iplon = 1, ncol
               plfrac = fracsd(iplon,lev,igc)
               blay = planklayd(iplon,lev,ibnd)
               dplankup = planklevd(iplon,lev,ibnd) - blay
               dplankdn = planklevd(iplon,lev-1,ibnd) - blay
               odepth = SECDIFF(iplon) * taug(iplon,lev,igc)
               if (odepth .lt. 0.0 ) odepth = 0.0
               if (icldlyr(iplon, lev).eq.1) then
                  ICLDDN(iplon) = 1
                  odclds = SECDIFF(iplon) * taucmcd(iplon,igc,lev)
                  absclds = 1. - exp(-odclds)
                  efclfracs = absclds * cldfmcd(iplon, igc,lev)
                  odtot = odepth + odclds
                  tblind = odepth/(bpade+odepth)
                  itgas = tblint*tblind+0.5
                  odepth = tau_tbl(itgas)
                  ATRANS(iplon,lev) = 1. - exp_tbl(itgas)
                  tfacgas = tfn_tbl(itgas)
                  gassrc = ATRANS(iplon,lev) * plfrac * (blay + tfacgas*dplankdn)
                  odtot = odepth + odclds
                  tblind = odtot/(bpade+odtot)
                  ittot = tblint*tblind + 0.5
                  tfactot = tfn_tbl(ittot)
                  bbdtot = plfrac * (blay + tfactot*dplankdn)
                  bbd = plfrac*(blay+tfacgas*dplankdn)
                  ATOT(iplon,lev) = 1. - exp_tbl(ittot)
                  RADLD(iplon) = RADLD(iplon) - RADLD(iplon) * (ATRANS(iplon,lev) + &
                  efclfracs * (1. - ATRANS(iplon,lev))) + &
                  gassrc + cldfmcd(iplon, igc,lev) * &
                  (bbdtot * ATOT(iplon,lev) - gassrc)
                  gdrad(iplon, igc, lev-1) = gdrad(iplon, igc, lev-1) + RADLD(iplon)
                  BBUGAS(iplon,lev) = plfrac * (blay + tfacgas * dplankup)
                  BBUTOT(iplon,lev) = plfrac * (blay + tfactot * dplankup)
               else
                  if (odepth .le. 0.06) then
                     ATRANS(iplon,lev) = odepth-0.5*odepth*odepth
                     odepth = rec_6*odepth
                     bbd = plfrac*(blay+dplankdn*odepth)
                     BBUGAS(iplon,lev) = plfrac*(blay+dplankup*odepth)
                  else
                     tblind = odepth/(bpade+odepth)
                     itr = tblint*tblind+0.5
                     transc = exp_tbl(itr)
                     ATRANS(iplon,lev) = 1.-transc
                     tausfac = tfn_tbl(itr)
                     bbd = plfrac*(blay+tausfac*dplankdn)
                     BBUGAS(iplon,lev) = plfrac * (blay + tausfac * dplankup)
                  endif
                  RADLD(iplon) = RADLD(iplon) + (bbd-RADLD(iplon) )*ATRANS(iplon,lev)
                  gdrad(iplon, igc, lev-1) = gdrad(iplon, igc, lev-1) + RADLD(iplon)
               endif
               if (ICLDDN(iplon) .eq.1) then
                  RADCLRD(iplon) = RADCLRD(iplon) + (bbd-RADCLRD(iplon)) * ATRANS(iplon,lev)
                  gclrdrad(iplon, igc, lev-1) = gclrdrad(iplon, igc, lev-1) + RADCLRD(iplon)
               else
                  RADCLRD(iplon) = RADLD(iplon)
                  gclrdrad(iplon, igc, lev-1) = gdrad(iplon, igc, lev-1)
               endif
       enddo
         enddo
       do iplon = 1, ncol
         rad0 = fracsd(iplon,1,igc) * plankbndd(iplon,ibnd)
         reflect = 1. - semissd(iplon,ibnd)
         RADLU(iplon) = rad0 + reflect * RADLD(iplon)
         RADCLRU(iplon) = rad0 + reflect * RADCLRD(iplon)
         gurad(iplon, igc, 0) = gurad(iplon, igc, 0) + RADLU(iplon)
         gclrurad(iplon, igc, 0) = gclrurad(iplon, igc, 0) + RADCLRU(iplon)
       enddo
         do lev = 1, nlayers
       do iplon = 1, ncol
            if (icldlyr(iplon, lev) .eq. 1) then
               gassrc = BBUGAS(iplon,lev) * ATRANS(iplon,lev)
               odclds = SECDIFF(iplon) * taucmcd(iplon,igc,lev)
               absclds = 1. - exp(-odclds)
               efclfracs = absclds * cldfmcd(iplon, igc,lev)
               RADLU(iplon) = RADLU(iplon) - RADLU(iplon) * (ATRANS(iplon,lev) + &
                   efclfracs * (1. - ATRANS(iplon,lev))) + &
                   gassrc + cldfmcd(iplon, igc,lev) * &
                   (BBUTOT(iplon,lev) * ATOT(iplon,lev) - gassrc)
               gurad(iplon, igc, lev) = gurad(iplon, igc, lev) + RADLU(iplon)
            else
               RADLU(iplon) = RADLU(iplon) + (BBUGAS(iplon,lev)-RADLU(iplon))*ATRANS(iplon,lev)
               gurad(iplon, igc, lev) = gurad(iplon, igc, lev) + RADLU(iplon)
            endif
               if (ICLDDN(iplon).eq.1) then
                  RADCLRU(iplon) = RADCLRU(iplon) + (BBUGAS(iplon,lev)-RADCLRU(iplon))*ATRANS(iplon,lev)
                  gclrurad(iplon, igc, lev) = gclrurad(iplon, igc, lev) + RADCLRU(iplon)
               else
                  RADCLRU(iplon) = RADLU(iplon)
                  gclrurad(iplon, igc, lev) = gurad(iplon, igc, lev)
               endif
       enddo
          enddo
          tblind = wtdiff * delwaved(ibnd) * fluxfacd
          do lev = 0, nlayers
       do iplon = 1, ncol
           gurad(iplon, igc, lev) = gurad(iplon, igc, lev) * tblind
           gdrad(iplon, igc, lev) = gdrad(iplon, igc, lev) * tblind
           gclrurad(iplon, igc, lev) = gclrurad(iplon, igc, lev) * tblind
           gclrdrad(iplon, igc, lev) = gclrdrad(iplon, igc, lev) * tblind
       enddo
          end do
      end do
      end subroutine rtrnmcg
      subroutine rtrnadd(ncol, nlay, ngpt, drvf &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pzd,pwvcmd,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrvd,bpaded,heatfacd,fluxfacd,a0d,a1d,a2d &
         ,delwaved,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                                )
      integer, intent(in), value :: ncol
      integer, intent(in), value :: nlay
      integer, intent(in), value :: ngpt
      integer, intent(in), value :: drvf
      integer :: ncol_,nlayers_,nbndlw_,ngptlw_
      integer :: ngsd(nbndlw)
      real :: taucmcd(8, ngptlw_, nlayers_+1)
      real , dimension(8, 0:nlayers_+1) :: pzd
      real , dimension(8) :: pwvcmd
      real , dimension(8,nbndlw_) :: semissd
      real , dimension(8,nlayers_+1,nbndlw_) :: planklayd
      real , dimension(8,0:nlayers_+1,nbndlw_) :: planklevd
      real, dimension(8,nbndlw_) :: plankbndd
      real :: gurad(8,ngptlw_,0:nlayers_+1)
      real :: gdrad(8,ngptlw_,0:nlayers_+1)
      real :: gclrurad(8,ngptlw_,0:nlayers_+1)
      real :: gclrdrad(8,ngptlw_,0:nlayers_+1)
      real :: gdtotuflux_dtd(8, ngptlw_, 0:nlayers_+1)
      real :: gdtotuclfl_dtd(8, ngptlw_, 0:nlayers_+1)
      integer :: idrvd
      real :: bpaded
      real :: heatfacd
      real :: fluxfacd
      real :: a0d(nbndlw_), a1d(nbndlw_), a2d(nbndlw_)
      real :: delwaved(nbndlw_)
      real :: totufluxd(8, 0:nlayers_+1)
      real :: totdfluxd(8, 0:nlayers_+1)
      real :: fnetd(8, 0:nlayers_+1)
      real :: htrd(8, 0:nlayers_+1)
      real :: totuclfld(8, 0:nlayers_+1)
      real :: totdclfld(8, 0:nlayers_+1)
      real :: fnetcd(8, 0:nlayers_+1)
      real :: htrcd(8, 0:nlayers_+1)
      real :: dtotuflux_dtd(8, 0:nlayers_+1)
      real :: dtotuclfl_dtd(8, 0:nlayers_+1)
      real :: dplankbnd_dtd(8,nbndlw_)
      integer :: iplon, ilay, igp
      do iplon = 1, ncol
        do ilay = 0, nlay
          do igp = 1, ngpt
            totufluxd(iplon, ilay)=totufluxd(iplon, ilay)+gurad(iplon, igp, ilay)
            totdfluxd(iplon, ilay)=totdfluxd(iplon, ilay)+gdrad(iplon, igp, ilay)
            totuclfld(iplon, ilay)=totuclfld(iplon, ilay)+gclrurad(iplon, igp, ilay)
            totdclfld(iplon, ilay)=totdclfld(iplon, ilay)+gclrdrad(iplon, igp, ilay)
          end do
          if (drvf .eq. 1) then
            do igp = 1, ngpt
              dtotuflux_dtd(iplon, ilay) = dtotuflux_dtd(iplon, ilay) + gdtotuflux_dtd( iplon, igp, ilay)
              dtotuclfl_dtd(iplon, ilay) = dtotuclfl_dtd(iplon, ilay) + gdtotuclfl_dtd( iplon, igp, ilay)
            end do
          end if
        end do
      end do
      end subroutine
      subroutine rtrnheatrates(ncol, nlay &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pzd,pwvcmd,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrvd,bpaded,heatfacd,fluxfacd,a0d,a1d,a2d &
         ,delwaved,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                                      )
      integer, intent(in), value :: ncol
      integer, intent(in), value :: nlay
      integer :: ncol_,nlayers_,nbndlw_,ngptlw_
      integer :: ngsd(nbndlw)
      real :: taucmcd(8, ngptlw_, nlayers_+1)
      real , dimension(8, 0:nlayers_+1) :: pzd
      real , dimension(8) :: pwvcmd
      real , dimension(8,nbndlw_) :: semissd
      real , dimension(8,nlayers_+1,nbndlw_) :: planklayd
      real , dimension(8,0:nlayers_+1,nbndlw_) :: planklevd
      real, dimension(8,nbndlw_) :: plankbndd
      real :: gurad(8,ngptlw_,0:nlayers_+1)
      real :: gdrad(8,ngptlw_,0:nlayers_+1)
      real :: gclrurad(8,ngptlw_,0:nlayers_+1)
      real :: gclrdrad(8,ngptlw_,0:nlayers_+1)
      real :: gdtotuflux_dtd(8, ngptlw_, 0:nlayers_+1)
      real :: gdtotuclfl_dtd(8, ngptlw_, 0:nlayers_+1)
      integer :: idrvd
      real :: bpaded
      real :: heatfacd
      real :: fluxfacd
      real :: a0d(nbndlw_), a1d(nbndlw_), a2d(nbndlw_)
      real :: delwaved(nbndlw_)
      real :: totufluxd(8, 0:nlayers_+1)
      real :: totdfluxd(8, 0:nlayers_+1)
      real :: fnetd(8, 0:nlayers_+1)
      real :: htrd(8, 0:nlayers_+1)
      real :: totuclfld(8, 0:nlayers_+1)
      real :: totdclfld(8, 0:nlayers_+1)
      real :: fnetcd(8, 0:nlayers_+1)
      real :: htrcd(8, 0:nlayers_+1)
      real :: dtotuflux_dtd(8, 0:nlayers_+1)
      real :: dtotuclfl_dtd(8, 0:nlayers_+1)
      real :: dplankbnd_dtd(8,nbndlw_)
      real :: t2
      integer :: iplon, ilay
      do iplon = 1, ncol
        do ilay = 0, nlay - 1
          t2 = pzd(iplon, ilay ) - pzd(iplon, ilay + 1)
          htrd(iplon, ilay) = heatfacd * ((totufluxd(iplon, ilay) - totdfluxd(iplon, ilay)) &
                 - (totufluxd(iplon, ilay+1) - totdfluxd(iplon, ilay+1)))/t2
          htrcd(iplon, ilay) = heatfacd * ((totuclfld(iplon, ilay) - totdclfld(iplon, ilay)) &
                 - (totuclfld(iplon, ilay+1) - totdclfld(iplon, ilay+1)))/t2
        end do
      end do
      end subroutine
      subroutine copyGPUrtrnmcg(pz, pwvcm, idrv, taut)
      real , intent(in) :: pz(:,:)
      integer , intent(in) :: idrv
      real , intent(in) :: taut(:,:,:)
      real , intent(in) :: pwvcm(:)
      end subroutine
      subroutine allocateGPUrtrnmcg(ncol, nlay, ngptlw, drvf)
      integer , intent(in) :: ncol, nlay, ngptlw, drvf
integer,external :: omp_get_thread_num
      end subroutine
      subroutine deallocateGPUrtrnmcg( drvf )
      integer , intent(in) :: drvf
      end subroutine
      end module gpu_rrtmg_lw_rtrnmc
      module gpu_rrtmg_lw_taumol
      use parrrtm_f, only : mg, nbndlw, maxxsec, ngptlw
      use rrlw_con_f, only: oneminus
      use rrlw_wvn_f, only: nspa, nspb
      use rrlw_vsn_f, only: hvrtau, hnamtau
      use rrlw_wvn_f, only: ngb
      use rrlw_ref_f
      use memory
      implicit none
      contains
      subroutine taugb1g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use rrlw_kg01_f
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      real :: pp, corradj, scalen2, tauself, taufor, taun2
      integer , value, intent(in) :: ncol, nlayers
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
       if (lay <= laytrop(iplon)) then
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(1) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(1) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         pp = pavel(iplon, lay)
         corradj = 1.
         if (pp .lt. 250. ) then
            corradj = 1. - 0.15 * (250. -pp) / 154.4
         endif
         scalen2 = colbrd(iplon,lay) * scaleminorn2(iplon,lay)
         do ig = 1, ng1
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            taun2 = scalen2*(ka_mn2(indm,ig) + &
                 minorfrac(iplon,lay) * (ka_mn2(indm+1,ig) - ka_mn2(indm,ig)))
            taug(iplon,lay,ig) = corradj * (colh2o(iplon,lay) * &
                (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor + taun2)
             fracsd(iplon,lay,ig) = fracrefa(ig)
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(1) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(1) + 1
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         pp = pavel(iplon, lay)
         corradj = 1. - 0.15 * (pp / 95.6 )
         scalen2 = colbrd(iplon,lay) * scaleminorn2(iplon,lay)
         do ig = 1, ng1
            taufor = forfac(iplon,lay) * (forref(indf,ig) + &
                 forfrac(iplon,lay) * (forref(indf+1,ig) - forref(indf,ig)))
            taun2 = scalen2*(kb_mn2(indm,ig) + &
                 minorfrac(iplon,lay) * (kb_mn2(indm+1,ig) - kb_mn2(indm,ig)))
            taug(iplon,lay,ig) = corradj * (colh2o(iplon,lay) * &
                (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + taufor + taun2)
            fracsd(iplon,lay,ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb1g
      subroutine taugb2g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs1
      use rrlw_kg02_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, ig
      real :: pp, corradj, tauself, taufor
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(2) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(2) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         pp = pavel(iplon, lay)
         corradj = 1. - .05 * (pp - 100. ) / 900.
         do ig = 1, ng2
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            taug(iplon,lay,ngs1+ig) = corradj * (colh2o(iplon,lay) * &
                (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor)
            fracsd(iplon,lay,ngs1+ig) = fracrefa(ig)
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(2) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(2) + 1
         indf = indfor(iplon,lay)
         do ig = 1, ng2
            taufor = forfac(iplon,lay) * (forref(indf,ig) + &
                 forfrac(iplon,lay) * (forref(indf+1,ig) - forref(indf,ig)))
            taug(iplon,lay,ngs1+ig) = colh2o(iplon,lay) * &
                (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + taufor
            fracsd(iplon,lay,ngs1+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb2g
      subroutine taugb3g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs2
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg03_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmn2o, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, &
                       fmn2o, fmn2omf, chi_n2o, ratn2o, adjfac, adjcoln2o
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor, n2om1, n2om2, absn2o
      real :: refrat_planck_a, refrat_planck_b, refrat_m_a, refrat_m_b
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,9)/chi_mls(2,9)
      refrat_planck_b = chi_mls(1,13)/chi_mls(2,13)
      refrat_m_a = chi_mls(1,3)/chi_mls(2,3)
      refrat_m_b = chi_mls(1,13)/chi_mls(2,13)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mn2o = colh2o(iplon,lay) + refrat_m_a*colco2(iplon,lay)
         specparm_mn2o = colh2o(iplon,lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminusd) specparm_mn2o = oneminusd
         specmult_mn2o = 8. *specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0 )
         fmn2omf = minorfrac(iplon,lay)*fmn2o
         chi_n2o = coln2o(iplon,lay)/coldry(iplon,lay)
         ratn2o = 1.e20 *chi_n2o/chi_mls(4,jp(iplon,lay)+1)
         if (ratn2o .gt. 1.5 ) then
            adjfac = 0.5 +(ratn2o-0.5 )**0.65
            adjcoln2o = adjfac*chi_mls(4,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcoln2o = coln2o(iplon,lay)
         endif
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colco2(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(3) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(3) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng3
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
            n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(iplon,lay) * (n2om2 - n2om1)
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs2+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcoln2o*absn2o
            fracsd(iplon,lay,ngs2+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         speccomb = colh2o(iplon,lay) + rat_h2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 4. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 4. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         fac000 = (1. - fs) * fac00(iplon,lay)
         fac010 = (1. - fs) * fac10(iplon,lay)
         fac100 = fs * fac00(iplon,lay)
         fac110 = fs * fac10(iplon,lay)
         fac001 = (1. - fs1) * fac01(iplon,lay)
         fac011 = (1. - fs1) * fac11(iplon,lay)
         fac101 = fs1 * fac01(iplon,lay)
         fac111 = fs1 * fac11(iplon,lay)
         speccomb_mn2o = colh2o(iplon,lay) + refrat_m_b*colco2(iplon,lay)
         specparm_mn2o = colh2o(iplon,lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminusd) specparm_mn2o = oneminusd
         specmult_mn2o = 4. *specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0 )
         fmn2omf = minorfrac(iplon,lay)*fmn2o
         chi_n2o = coln2o(iplon,lay)/coldry(iplon,lay)
         ratn2o = 1.e20*chi_n2o/chi_mls(4,jp(iplon,lay)+1)
         if (ratn2o .gt. 1.5 ) then
            adjfac = 0.5 +(ratn2o-0.5 )**0.65
            adjcoln2o = adjfac*chi_mls(4,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcoln2o = coln2o(iplon,lay)
         endif
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_b*colco2(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 4. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(3) + js
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(3) + js1
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         do ig = 1, ng3
            taufor = forfac(iplon,lay) * (forref(indf,ig) + &
                 forfrac(iplon,lay) * (forref(indf+1,ig) - forref(indf,ig)))
            n2om1 = kb_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (kb_mn2o(jmn2o+1,indm,ig)-kb_mn2o(jmn2o,indm,ig))
            n2om2 = kb_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (kb_mn2o(jmn2o+1,indm+1,ig)-kb_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(iplon,lay) * (n2om2 - n2om1)
            taug(iplon,lay,ngs2+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) + &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig)) &
                + taufor &
                + adjcoln2o*absn2o
            fracsd(iplon,lay,ngs2+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo
      endif
      end do
      end do
      end subroutine taugb3g
      subroutine taugb4g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs3
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg04_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: lay, ind0, ind1, inds, indf, ig
      integer :: js, js1, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor
      real :: refrat_planck_a, refrat_planck_b
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,11)/chi_mls(2,11)
      refrat_planck_b = chi_mls(3,13)/chi_mls(2,13)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colco2(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(4) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(4) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng4
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs3+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracsd(iplon,lay,ngs3+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         speccomb = colo3(iplon,lay) + rat_o3co2(iplon,lay)*colco2(iplon,lay)
         specparm = colo3(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 4. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colo3(iplon,lay) + rat_o3co2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colo3(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 4. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         fac000 = (1. - fs) * fac00(iplon,lay)
         fac010 = (1. - fs) * fac10(iplon,lay)
         fac100 = fs * fac00(iplon,lay)
         fac110 = fs * fac10(iplon,lay)
         fac001 = (1. - fs1) * fac01(iplon,lay)
         fac011 = (1. - fs1) * fac11(iplon,lay)
         fac101 = fs1 * fac01(iplon,lay)
         fac111 = fs1 * fac11(iplon,lay)
         speccomb_planck = colo3(iplon,lay)+refrat_planck_b*colco2(iplon,lay)
         specparm_planck = colo3(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 4. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(4) + js
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(4) + js1
         do ig = 1, ng4
            taug(iplon,lay,ngs3+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) + &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig))
            fracsd(iplon,lay,ngs3+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo
         taug(iplon,lay,ngs3+8)=taug(iplon,lay,ngs3+8)*0.92
         taug(iplon,lay,ngs3+9)=taug(iplon,lay,ngs3+9)*0.88
         taug(iplon,lay,ngs3+10)=taug(iplon,lay,ngs3+10)*1.07
         taug(iplon,lay,ngs3+11)=taug(iplon,lay,ngs3+11)*1.1
         taug(iplon,lay,ngs3+12)=taug(iplon,lay,ngs3+12)*0.99
         taug(iplon,lay,ngs3+13)=taug(iplon,lay,ngs3+13)*0.88
         taug(iplon,lay,ngs3+14)=taug(iplon,lay,ngs3+14)*0.943
      endif
      end do
      end do
      end subroutine taugb4g
      subroutine taugb5g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs4
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg05_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmo3, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mo3, specparm_mo3, specmult_mo3, fmo3
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor, o3m1, o3m2, abso3
      real :: refrat_planck_a, refrat_planck_b, refrat_m_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,5)/chi_mls(2,5)
      refrat_planck_b = chi_mls(3,43)/chi_mls(2,43)
      refrat_m_a = chi_mls(1,7)/chi_mls(2,7)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mo3 = colh2o(iplon,lay) + refrat_m_a*colco2(iplon,lay)
         specparm_mo3 = colh2o(iplon,lay)/speccomb_mo3
         if (specparm_mo3 .ge. oneminusd) specparm_mo3 = oneminusd
         specmult_mo3 = 8. *specparm_mo3
         jmo3 = 1 + int(specmult_mo3)
         fmo3 = mod(specmult_mo3,1.0 )
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colco2(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(5) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(5) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng5
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            o3m1 = ka_mo3(jmo3,indm,ig) + fmo3 * &
                 (ka_mo3(jmo3+1,indm,ig)-ka_mo3(jmo3,indm,ig))
            o3m2 = ka_mo3(jmo3,indm+1,ig) + fmo3 * &
                 (ka_mo3(jmo3+1,indm+1,ig)-ka_mo3(jmo3,indm+1,ig))
            abso3 = o3m1 + minorfrac(iplon,lay)*(o3m2-o3m1)
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs4+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + abso3*colo3(iplon,lay) &
                 + wx1(iplon,lay) * coldry(iplon,lay) * 1.e-20 * ccl4(ig)
            fracsd(iplon,lay,ngs4+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         speccomb = colo3(iplon,lay) + rat_o3co2(iplon,lay)*colco2(iplon,lay)
         specparm = colo3(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 4. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colo3(iplon,lay) + rat_o3co2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colo3(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 4. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         fac000 = (1. - fs) * fac00(iplon,lay)
         fac010 = (1. - fs) * fac10(iplon,lay)
         fac100 = fs * fac00(iplon,lay)
         fac110 = fs * fac10(iplon,lay)
         fac001 = (1. - fs1) * fac01(iplon,lay)
         fac011 = (1. - fs1) * fac11(iplon,lay)
         fac101 = fs1 * fac01(iplon,lay)
         fac111 = fs1 * fac11(iplon,lay)
         speccomb_planck = colo3(iplon,lay)+refrat_planck_b*colco2(iplon,lay)
         specparm_planck = colo3(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 4. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(5) + js
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(5) + js1
         do ig = 1, ng5
            taug(iplon,lay,ngs4+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) + &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig)) &
                + wx1(iplon, lay) * coldry(iplon,lay) * 1.e-20 * ccl4(ig)
            fracsd(iplon,lay,ngs4+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo
      endif
      end do
      end do
      end subroutine taugb5g
      subroutine taugb6g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs5
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg06_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      real :: chi_co2, ratco2, adjfac, adjcolco2
      real :: tauself, taufor, absco2
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         chi_co2 = colco2(iplon,lay)/(coldry(iplon,lay))
         ratco2 = 1.e20 *chi_co2/chi_mls(2,jp(iplon,lay)+1)
         if (ratco2 .gt. 3.0 ) then
            adjfac = 2.0 +(ratco2-2.0 )**0.77
            adjcolco2 = adjfac*chi_mls(2,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(6) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(6) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         do ig = 1, ng6
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            absco2 = (ka_mco2(indm,ig) + minorfrac(iplon,lay) * &
                 (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
            taug(iplon,lay,ngs5+ig) = colh2o(iplon,lay) * &
                (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor &
                 + adjcolco2 * absco2 &
                 + wx2(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc11adj(ig) &
                 + wx3(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc12(ig)
            fracsd(iplon,lay,ngs5+ig) = fracrefa(ig)
         enddo
      else
         do ig = 1, ng6
            taug(iplon,lay,ngs5+ig) = 0.0 &
                 + wx2(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc11adj(ig) &
                 + wx3(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc12(ig)
            fracsd(iplon,lay,ngs5+ig) = fracrefa(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb6g
      subroutine taugb7g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs6
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg07_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmco2, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor, co2m1, co2m2, absco2
      real :: chi_co2, ratco2, adjfac, adjcolco2
      real :: refrat_planck_a, refrat_m_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,3)/chi_mls(3,3)
      refrat_m_a = chi_mls(1,3)/chi_mls(3,3)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2oo3(iplon,lay)*colo3(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oo3_1(iplon,lay)*colo3(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mco2 = colh2o(iplon,lay) + refrat_m_a*colo3(iplon,lay)
         specparm_mco2 = colh2o(iplon,lay)/speccomb_mco2
         if (specparm_mco2 .ge. oneminusd) specparm_mco2 = oneminusd
         specmult_mco2 = 8. *specparm_mco2
         jmco2 = 1 + int(specmult_mco2)
         fmco2 = mod(specmult_mco2,1.0 )
         chi_co2 = colco2(iplon,lay)/(coldry(iplon,lay))
         ratco2 = 1.e20*chi_co2/chi_mls(2,jp(iplon,lay)+1)
         if (ratco2 .gt. 3.0 ) then
            adjfac = 3.0 +(ratco2-3.0 )**0.79
            adjcolco2 = adjfac*chi_mls(2,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colo3(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(7) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(7) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng7
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
            co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
            absco2 = co2m1 + minorfrac(iplon,lay) * (co2m2 - co2m1)
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs6+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcolco2*absco2
            fracsd(iplon,lay,ngs6+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
    else
         chi_co2 = colco2(iplon,lay)/(coldry(iplon,lay))
         ratco2 = 1.e20*chi_co2/chi_mls(2,jp(iplon,lay)+1)
         if (ratco2 .gt. 3.0 ) then
            adjfac = 2.0 +(ratco2-2.0 )**0.79
            adjcolco2 = adjfac*chi_mls(2,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(7) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(7) + 1
         indm = indminor(iplon,lay)
         do ig = 1, ng7
            absco2 = kb_mco2(indm,ig) + minorfrac(iplon,lay) * &
                 (kb_mco2(indm+1,ig) - kb_mco2(indm,ig))
            taug(iplon,lay,ngs6+ig) = colo3(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + adjcolco2 * absco2
            fracsd(iplon,lay,ngs6+ig) = fracrefb(ig)
         enddo
         taug(iplon,lay,ngs6+6)=taug(iplon,lay,ngs6+6)*0.92
         taug(iplon,lay,ngs6+7)=taug(iplon,lay,ngs6+7)*0.88
         taug(iplon,lay,ngs6+8)=taug(iplon,lay,ngs6+8)*1.07
         taug(iplon,lay,ngs6+9)=taug(iplon,lay,ngs6+9)*1.1
         taug(iplon,lay,ngs6+10)=taug(iplon,lay,ngs6+10)*0.99
         taug(iplon,lay,ngs6+11)=taug(iplon,lay,ngs6+11)*0.855
      endif
      end do
      end do
      end subroutine taugb7g
      subroutine taugb8g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs7
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg08_f
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      real :: tauself, taufor, absco2, abso3, absn2o
      real :: chi_co2, ratco2, adjfac, adjcolco2
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         chi_co2 = colco2(iplon,lay)/(coldry(iplon,lay))
         ratco2 = 1.e20 *chi_co2/chi_mls(2,jp(iplon,lay)+1)
         if (ratco2 .gt. 3.0 ) then
            adjfac = 2.0 +(ratco2-2.0 )**0.65
            adjcolco2 = adjfac*chi_mls(2,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(8) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(8) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         do ig = 1, ng8
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            absco2 = (ka_mco2(indm,ig) + minorfrac(iplon,lay) * &
                 (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
            abso3 = (ka_mo3(indm,ig) + minorfrac(iplon,lay) * &
                 (ka_mo3(indm+1,ig) - ka_mo3(indm,ig)))
            absn2o = (ka_mn2o(indm,ig) + minorfrac(iplon,lay) * &
                 (ka_mn2o(indm+1,ig) - ka_mn2o(indm,ig)))
            taug(iplon,lay,ngs7+ig) = colh2o(iplon,lay) * &
                 (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor &
                 + adjcolco2*absco2 &
                 + colo3(iplon,lay) * abso3 &
                 + coln2o(iplon,lay) * absn2o &
                 + wx3(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc12(ig) &
                 + wx4(iplon, lay) * coldry(iplon,lay) * 1.e-20 * cfc22adj(ig)
            fracsd(iplon,lay,ngs7+ig) = fracrefa(ig)
         enddo
      else
         chi_co2 = colco2(iplon,lay)/coldry(iplon,lay)
         ratco2 = 1.e20 *chi_co2/chi_mls(2,jp(iplon,lay)+1)
         if (ratco2 .gt. 3.0 ) then
            adjfac = 2.0 +(ratco2-2.0 )**0.65
            adjcolco2 = adjfac*chi_mls(2,jp(iplon,lay)+1) * coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(8) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(8) + 1
         indm = indminor(iplon,lay)
         do ig = 1, ng8
            absco2 = (kb_mco2(indm,ig) + minorfrac(iplon,lay) * &
                 (kb_mco2(indm+1,ig) - kb_mco2(indm,ig)))
            absn2o = (kb_mn2o(indm,ig) + minorfrac(iplon,lay) * &
                 (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig)))
            taug(iplon,lay,ngs7+ig) = colo3(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + adjcolco2*absco2 &
                 + coln2o(iplon,lay)*absn2o &
                 + wx3(iplon,lay) * coldry(iplon,lay) * 1.e-20 * cfc12(ig) &
                 + wx4(iplon,lay) * coldry(iplon,lay) * 1.e-20 * cfc22adj(ig)
            fracsd(iplon,lay,ngs7+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb8g
      subroutine taugb9g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                )
      use parrrtm_f, only : ngs8
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg09_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmn2o, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, fmn2o
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor, n2om1, n2om2, absn2o
      real :: chi_n2o, ratn2o, adjfac, adjcoln2o
      real :: refrat_planck_a, refrat_m_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,9)/chi_mls(6,9)
      refrat_m_a = chi_mls(1,3)/chi_mls(6,3)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2och4(iplon,lay)*colch4(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2och4_1(iplon,lay)*colch4(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mn2o = colh2o(iplon,lay) + refrat_m_a*colch4(iplon,lay)
         specparm_mn2o = colh2o(iplon,lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminusd) specparm_mn2o = oneminusd
         specmult_mn2o = 8. *specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0 )
         chi_n2o = coln2o(iplon,lay)/(coldry(iplon,lay))
         ratn2o = 1.e20 *chi_n2o/chi_mls(4,jp(iplon,lay)+1)
         if (ratn2o .gt. 1.5 ) then
            adjfac = 0.5 +(ratn2o-0.5 )**0.65
            adjcoln2o = adjfac*chi_mls(4,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcoln2o = coln2o(iplon,lay)
         endif
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colch4(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(9) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(9) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng9
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
            n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(iplon,lay) * (n2om2 - n2om1)
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs8+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcoln2o*absn2o
            fracsd(iplon,lay,ngs8+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         chi_n2o = coln2o(iplon,lay)/(coldry(iplon,lay))
         ratn2o = 1.e20 *chi_n2o/chi_mls(4,jp(iplon,lay)+1)
         if (ratn2o .gt. 1.5 ) then
            adjfac = 0.5 +(ratn2o-0.5 )**0.65
            adjcoln2o = adjfac*chi_mls(4,jp(iplon,lay)+1)*coldry(iplon,lay)*1.e-20
         else
            adjcoln2o = coln2o(iplon,lay)
         endif
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(9) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(9) + 1
         indm = indminor(iplon,lay)
         do ig = 1, ng9
            absn2o = kb_mn2o(indm,ig) + minorfrac(iplon,lay) * &
                (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig))
            taug(iplon,lay,ngs8+ig) = colch4(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + adjcoln2o*absn2o
            fracsd(iplon,lay,ngs8+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb9g
      subroutine taugb10g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs9
      use rrlw_kg10_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, ig
      real :: tauself, taufor
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(10) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(10) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         do ig = 1, ng10
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            taug(iplon,lay,ngs9+ig) = colh2o(iplon,lay) * &
                 (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor
            fracsd(iplon,lay,ngs9+ig) = fracrefa(ig)
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(10) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(10) + 1
         indf = indfor(iplon,lay)
         do ig = 1, ng10
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            taug(iplon,lay,ngs9+ig) = colh2o(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + taufor
            fracsd(iplon,lay,ngs9+ig) = fracrefb(ig)
         enddo
      end if
      end do
      end do
      end subroutine taugb10g
      subroutine taugb11g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs10
      use rrlw_kg11_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      real :: scaleo2, tauself, taufor, tauo2
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(11) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(11) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         scaleo2 = colo2(iplon,lay)*scaleminor(iplon,lay)
         do ig = 1, ng11
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            tauo2 = scaleo2 * (ka_mo2(indm,ig) + minorfrac(iplon,lay) * &
                 (ka_mo2(indm+1,ig) - ka_mo2(indm,ig)))
            taug(iplon,lay,ngs10+ig) = colh2o(iplon,lay) * &
                 (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor &
                 + tauo2
            fracsd(iplon,lay,ngs10+ig) = fracrefa(ig)
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(11) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(11) + 1
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         scaleo2 = colo2(iplon,lay)*scaleminor(iplon,lay)
         do ig = 1, ng11
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            tauo2 = scaleo2 * (kb_mo2(indm,ig) + minorfrac(iplon,lay) * &
                 (kb_mo2(indm+1,ig) - kb_mo2(indm,ig)))
            taug(iplon,lay,ngs10+ig) = colh2o(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig)) &
                 + taufor &
                 + tauo2
            fracsd(iplon,lay,ngs10+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb11g
      subroutine taugb12g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs11
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg12_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, ig
      integer :: js, js1, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor
      real :: refrat_planck_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,10)/chi_mls(2,10)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colco2(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(12) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(12) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng12
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs11+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracsd(iplon,lay,ngs11+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         do ig = 1, ng12
            taug(iplon,lay,ngs11+ig) = 0.0
            fracsd(iplon,lay,ngs11+ig) = 0.0
         enddo
      endif
      end do
      end do
      end subroutine taugb12g
      subroutine taugb13g( ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs12
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg13_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmco2, jmco, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
      real :: speccomb_mco, specparm_mco, specmult_mco, fmco
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor, co2m1, co2m2, absco2
      real :: com1, com2, absco, abso3
      real :: chi_co2, ratco2, adjfac, adjcolco2
      real :: refrat_planck_a, refrat_m_a, refrat_m_a3
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,5)/chi_mls(4,5)
      refrat_m_a = chi_mls(1,1)/chi_mls(4,1)
      refrat_m_a3 = chi_mls(1,3)/chi_mls(4,3)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2on2o(iplon,lay)*coln2o(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2on2o_1(iplon,lay)*coln2o(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mco2 = colh2o(iplon,lay) + refrat_m_a*coln2o(iplon,lay)
         specparm_mco2 = colh2o(iplon,lay)/speccomb_mco2
         if (specparm_mco2 .ge. oneminusd) specparm_mco2 = oneminusd
         specmult_mco2 = 8. *specparm_mco2
         jmco2 = 1 + int(specmult_mco2)
         fmco2 = mod(specmult_mco2,1.0 )
         chi_co2 = colco2(iplon,lay)/(coldry(iplon,lay))
         ratco2 = 1.e20 *chi_co2/3.55e-4
         if (ratco2 .gt. 3.0 ) then
            adjfac = 2.0 +(ratco2-2.0 )**0.68
            adjcolco2 = adjfac*3.55e-4*coldry(iplon,lay)*1.e-20
         else
            adjcolco2 = colco2(iplon,lay)
         endif
         speccomb_mco = colh2o(iplon,lay) + refrat_m_a3*coln2o(iplon,lay)
         specparm_mco = colh2o(iplon,lay)/speccomb_mco
         if (specparm_mco .ge. oneminusd) specparm_mco = oneminusd
         specmult_mco = 8. *specparm_mco
         jmco = 1 + int(specmult_mco)
         fmco = mod(specmult_mco,1.0 )
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*coln2o(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(13) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(13) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng13
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
            co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
            absco2 = co2m1 + minorfrac(iplon,lay) * (co2m2 - co2m1)
            com1 = ka_mco(jmco,indm,ig) + fmco * &
                 (ka_mco(jmco+1,indm,ig) - ka_mco(jmco,indm,ig))
            com2 = ka_mco(jmco,indm+1,ig) + fmco * &
                 (ka_mco(jmco+1,indm+1,ig) - ka_mco(jmco,indm+1,ig))
            absco = com1 + minorfrac(iplon,lay) * (com2 - com1)
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs12+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcolco2*absco2 &
                 + colco(iplon,lay)*absco
            fracsd(iplon,lay,ngs12+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         indm = indminor(iplon,lay)
         do ig = 1, ng13
            abso3 = kb_mo3(indm,ig) + minorfrac(iplon,lay) * &
                 (kb_mo3(indm+1,ig) - kb_mo3(indm,ig))
            taug(iplon,lay,ngs12+ig) = colo3(iplon,lay)*abso3
            fracsd(iplon,lay,ngs12+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb13g
      subroutine taugb14g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs13
      use rrlw_kg14_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, ig
      real :: tauself, taufor
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      if (lay <= laytrop(iplon)) then
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(14) + 1
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(14) + 1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         do ig = 1, ng14
            tauself = selffac(iplon,lay) * (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            taug(iplon,lay,ngs13+ig) = colco2(iplon,lay) * &
                 (fac00(iplon,lay) * absa(ind0,ig) + &
                 fac10(iplon,lay) * absa(ind0+1,ig) + &
                 fac01(iplon,lay) * absa(ind1,ig) + &
                 fac11(iplon,lay) * absa(ind1+1,ig)) &
                 + tauself + taufor
            fracsd(iplon,lay,ngs13+ig) = fracrefa(ig)
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(14) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(14) + 1
         do ig = 1, ng14
            taug(iplon,lay,ngs13+ig) = colco2(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig))
            fracsd(iplon,lay,ngs13+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb14g
      subroutine taugb15g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs14
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg15_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, indm, ig
      integer :: js, js1, jmn2, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_mn2, specparm_mn2, specmult_mn2, fmn2
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: scalen2, tauself, taufor, n2m1, n2m2, taun2
      real :: refrat_planck_a, refrat_m_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(4,1)/chi_mls(2,1)
      refrat_m_a = chi_mls(4,1)/chi_mls(2,1)
      if (lay <= laytrop(iplon)) then
         speccomb = coln2o(iplon,lay) + rat_n2oco2(iplon,lay)*colco2(iplon,lay)
         specparm = coln2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = coln2o(iplon,lay) + rat_n2oco2_1(iplon,lay)*colco2(iplon,lay)
         specparm1 = coln2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_mn2 = coln2o(iplon,lay) + refrat_m_a*colco2(iplon,lay)
         specparm_mn2 = coln2o(iplon,lay)/speccomb_mn2
         if (specparm_mn2 .ge. oneminusd) specparm_mn2 = oneminusd
         specmult_mn2 = 8. *specparm_mn2
         jmn2 = 1 + int(specmult_mn2)
         fmn2 = mod(specmult_mn2,1.0 )
         speccomb_planck = coln2o(iplon,lay)+refrat_planck_a*colco2(iplon,lay)
         specparm_planck = coln2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(15) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(15) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         indm = indminor(iplon,lay)
         scalen2 = colbrd(iplon,lay)*scaleminor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng15
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            n2m1 = ka_mn2(jmn2,indm,ig) + fmn2 * &
                 (ka_mn2(jmn2+1,indm,ig) - ka_mn2(jmn2,indm,ig))
            n2m2 = ka_mn2(jmn2,indm+1,ig) + fmn2 * &
                 (ka_mn2(jmn2+1,indm+1,ig) - ka_mn2(jmn2,indm+1,ig))
            taun2 = scalen2 * (n2m1 + minorfrac(iplon,lay) * (n2m2 - n2m1))
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs14+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + taun2
            fracsd(iplon,lay,ngs14+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         do ig = 1, ng15
            taug(iplon,lay,ngs14+ig) = 0.0
            fracsd(iplon,lay,ngs14+ig) = 0.0
         enddo
      endif
      end do
      end do
      end subroutine taugb15g
      subroutine taugb16g( ncol, nlayers , taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                 )
      use parrrtm_f, only : ngs15
      use rrlw_ref_f, only : chi_mls
      use rrlw_kg16_f
      real :: taug(:,:,:)
      real :: fracsd(:,:,:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: lay, ind0, ind1, inds, indf, ig
      integer :: js, js1, jpl
      real :: speccomb, specparm, specmult, fs
      real :: speccomb1, specparm1, specmult1, fs1
      real :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real :: p, p4, fk0, fk1, fk2
      real :: fac000, fac100, fac200, fac010, fac110, fac210
      real :: fac001, fac101, fac201, fac011, fac111, fac211
      real :: tauself, taufor
      real :: refrat_planck_a
      real :: tau_major, tau_major1
      integer , value, intent(in) :: ncol, nlayers
      integer :: iplon
      do iplon = 1, ncol
      do lay = 1, nlayers
      refrat_planck_a = chi_mls(1,6)/chi_mls(6,6)
      if (lay <= laytrop(iplon)) then
         speccomb = colh2o(iplon,lay) + rat_h2och4(iplon,lay)*colch4(iplon,lay)
         specparm = colh2o(iplon,lay)/speccomb
         if (specparm .ge. oneminusd) specparm = oneminusd
         specmult = 8. *(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0 )
         speccomb1 = colh2o(iplon,lay) + rat_h2och4_1(iplon,lay)*colch4(iplon,lay)
         specparm1 = colh2o(iplon,lay)/speccomb1
         if (specparm1 .ge. oneminusd) specparm1 = oneminusd
         specmult1 = 8. *(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0 )
         speccomb_planck = colh2o(iplon,lay)+refrat_planck_a*colch4(iplon,lay)
         specparm_planck = colh2o(iplon,lay)/speccomb_planck
         if (specparm_planck .ge. oneminusd) specparm_planck=oneminusd
         specmult_planck = 8. *specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0 )
         ind0 = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspad(16) + js
         ind1 = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspad(16) + js1
         inds = indself(iplon,lay)
         indf = indfor(iplon,lay)
         if (specparm .lt. 0.125 ) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else if (specparm .gt. 0.875 ) then
            p = -fs
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac000 = fk0*fac00(iplon,lay)
            fac100 = fk1*fac00(iplon,lay)
            fac200 = fk2*fac00(iplon,lay)
            fac010 = fk0*fac10(iplon,lay)
            fac110 = fk1*fac10(iplon,lay)
            fac210 = fk2*fac10(iplon,lay)
         else
            fac000 = (1. - fs) * fac00(iplon,lay)
            fac010 = (1. - fs) * fac10(iplon,lay)
            fac100 = fs * fac00(iplon,lay)
            fac110 = fs * fac10(iplon,lay)
         endif
         if (specparm1 .lt. 0.125 ) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else if (specparm1 .gt. 0.875 ) then
            p = -fs1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0 *p4
            fk2 = p + p4
            fac001 = fk0*fac01(iplon,lay)
            fac101 = fk1*fac01(iplon,lay)
            fac201 = fk2*fac01(iplon,lay)
            fac011 = fk0*fac11(iplon,lay)
            fac111 = fk1*fac11(iplon,lay)
            fac211 = fk2*fac11(iplon,lay)
         else
            fac001 = (1. - fs1) * fac01(iplon,lay)
            fac011 = (1. - fs1) * fac11(iplon,lay)
            fac101 = fs1 * fac01(iplon,lay)
            fac111 = fs1 * fac11(iplon,lay)
         endif
         do ig = 1, ng16
            tauself = selffac(iplon,lay)* (selfref(inds,ig) + selffrac(iplon,lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(iplon,lay) * (forref(indf,ig) + forfrac(iplon,lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            if (specparm .lt. 0.125 ) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875 ) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif
            if (specparm1 .lt. 0.125 ) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875 ) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif
            taug(iplon,lay,ngs15+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracsd(iplon,lay,ngs15+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      else
         ind0 = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspbd(16) + 1
         ind1 = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspbd(16) + 1
         do ig = 1, ng16
            taug(iplon,lay,ngs15+ig) = colch4(iplon,lay) * &
                 (fac00(iplon,lay) * absb(ind0,ig) + &
                 fac10(iplon,lay) * absb(ind0+1,ig) + &
                 fac01(iplon,lay) * absb(ind1,ig) + &
                 fac11(iplon,lay) * absb(ind1+1,ig))
            fracsd(iplon,lay,ngs15+ig) = fracrefb(ig)
         enddo
      endif
      end do
      end do
      end subroutine taugb16g
      subroutine addAerosols( ncol, nlayers, ngptlw, nbndlw, ngbd, taug &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                                    )
      integer , intent(in), value :: ncol, nlayers, ngptlw, nbndlw
      integer , intent(in) :: ngbd(:)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer :: iplon, lay, ig
      real :: taug(:,:,:)
      do iplon = 1, ncol
      do lay = 1, nlayers
      do ig = 1, ngptlw
        taug(iplon, lay, ig) = taug(iplon, lay, ig) + tauaa(iplon, lay, ngbd(ig))
      end do
      end do
      end do
      end subroutine
      subroutine taumolg(iplon, ncol, nlayers, ngbd, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                        )
      use parrrtm_f, only : ng1
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      integer , intent(in) :: iplon
      integer , intent(in) :: ncol
      integer , intent(in) :: nlayers
      integer , intent(in) :: ngbd(:)
      real , intent(in) :: fracsd(:,:,:)
      real , intent(in) :: taug(:,:,:)
      integer :: i,j,err
      real :: t1, t2
      call taugb1g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb2g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb3g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb4g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb5g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb6g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb7g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb8g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb9g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb10g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb11g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb12g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb13g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb14g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb15g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call taugb16g (ncol, nlayers, taug, fracsd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                           )
      call addAerosols (ncol, nlayers, ngptlw, nbndlw, ngbd, taug &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
                               )
      end subroutine taumolg
      subroutine allocateGPUTaumol(ncol, nlayers, npart)
      integer , intent(in) :: ncol
      integer , intent(in) :: nlayers
      integer , intent(in) :: npart
      integer :: i
      end subroutine
      subroutine deallocateGPUTaumol()
      end subroutine
      subroutine copyGPUTaumolMol( colstart, pncol, nlayers, colh2oc, colco2c, colo3c, coln2oc, colch4c, colo2c,&
                                   px1,px2,px3,px4, npart)
      integer, value, intent(in) :: colstart, pncol, nlayers, npart
      real , intent(in) :: colh2oc(:,:), colco2c(:,:), colo3c(:,:), coln2oc(:,:), &
                                     colch4c(:,:), colo2c(:,:), px1(:,:), px2(:,:), px3(:,:), px4(:,:)
      end subroutine
      subroutine copyGPUTaumol(pavelc, wxc, coldryc, tauap, pncol, colstart, nlay, npart)
      use rrlw_kg01_f, only : copyToGPU1, reg1
      use rrlw_kg02_f, only : copyToGPU2, reg2
      use rrlw_kg03_f, only : copyToGPU3, reg3
      use rrlw_kg04_f, only : copyToGPU4, reg4
      use rrlw_kg05_f, only : copyToGPU5, reg5
      use rrlw_kg06_f, only : copyToGPU6, reg6
      use rrlw_kg07_f, only : copyToGPU7, reg7
      use rrlw_kg08_f, only : copyToGPU8, reg8
      use rrlw_kg09_f, only : copyToGPU9, reg9
      use rrlw_kg10_f, only : copyToGPU10, reg10
      use rrlw_kg11_f, only : copyToGPU11, reg11
      use rrlw_kg12_f, only : copyToGPU12, reg12
      use rrlw_kg13_f, only : copyToGPU13, reg13
      use rrlw_kg14_f, only : copyToGPU14, reg14
      use rrlw_kg15_f, only : copyToGPU15, reg15
      use rrlw_kg16_f, only : copyToGPU16, reg16
      use rrlw_ref_f, only : copyToGPUref
      real , intent(in) :: pavelc(:,:)
      real , intent(in) :: wxc(:,:,:)
      real , intent(in) :: coldryc(:,:)
      real , intent(in) :: tauap(:,:,:)
      integer, intent(in) :: pncol, colstart, nlay, npart
      end subroutine
      end module gpu_rrtmg_lw_taumol
      module gpu_rrtmg_lw_setcoef
      use gpu_rrtmg_lw_rtrnmc
      use parrrtm_f, only : nbndlw, mg, maxxsec, mxmol
      use rrlw_wvn_f, only: totplnk, totplk16, totplnkderiv, totplk16deriv
      use rrlw_vsn_f, only: hvrset, hnamset
      use rrlw_ref_f, only : chi_mls
      use gpu_rrtmg_lw_taumol
      implicit none
      contains
      subroutine allocateGPUSetCoef( ncol, nlayers )
         integer, intent(in) :: ncol
         integer, intent(in) :: nlayers
      end subroutine
      subroutine deallocateGPUSetCoef( )
      end subroutine
      subroutine copyGPUSetCoef()
      end subroutine
      subroutine setcoefg(ncol, nlayers, istart &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pzd,pwvcmd,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrvd,bpaded,heatfacd,fluxfacd,a0d,a1d,a2d &
         ,delwaved,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspad,nspbd,oneminusd &
   ,taveld,tzd,tboundd,wbroadd,totplnkd,totplk16d,totplnkderivd,totplk16derivd &
                                 )
      integer :: ncol_,nlayers_,nbndlw_,ngptlw_
      integer :: ngsd(nbndlw)
      real :: taucmcd(8, ngptlw_, nlayers_+1)
      real , dimension(8, 0:nlayers_+1) :: pzd
      real , dimension(8) :: pwvcmd
      real , dimension(8,nbndlw_) :: semissd
      real , dimension(8,nlayers_+1,nbndlw_) :: planklayd
      real , dimension(8,0:nlayers_+1,nbndlw_) :: planklevd
      real, dimension(8,nbndlw_) :: plankbndd
      real :: gurad(8,ngptlw_,0:nlayers_+1)
      real :: gdrad(8,ngptlw_,0:nlayers_+1)
      real :: gclrurad(8,ngptlw_,0:nlayers_+1)
      real :: gclrdrad(8,ngptlw_,0:nlayers_+1)
      real :: gdtotuflux_dtd(8, ngptlw_, 0:nlayers_+1)
      real :: gdtotuclfl_dtd(8, ngptlw_, 0:nlayers_+1)
      integer :: idrvd
      real :: bpaded
      real :: heatfacd
      real :: fluxfacd
      real :: a0d(nbndlw_), a1d(nbndlw_), a2d(nbndlw_)
      real :: delwaved(nbndlw_)
      real :: totufluxd(8, 0:nlayers_+1)
      real :: totdfluxd(8, 0:nlayers_+1)
      real :: fnetd(8, 0:nlayers_+1)
      real :: htrd(8, 0:nlayers_+1)
      real :: totuclfld(8, 0:nlayers_+1)
      real :: totdclfld(8, 0:nlayers_+1)
      real :: fnetcd(8, 0:nlayers_+1)
      real :: htrcd(8, 0:nlayers_+1)
      real :: dtotuflux_dtd(8, 0:nlayers_+1)
      real :: dtotuclfl_dtd(8, 0:nlayers_+1)
      real :: dplankbnd_dtd(8,nbndlw_)
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pavel(8, nlayers__)
      real :: wx1(8,nlayers__)
      real :: wx2(8,nlayers__)
      real :: wx3(8,nlayers__)
      real :: wx4(8,nlayers__)
      real :: coldry(8, nlayers__)
      integer :: laytrop(8)
      integer :: jp(8,nlayers__)
      integer :: jt(8,nlayers__)
      integer :: jt1(8,nlayers__)
      real :: colh2o(8,nlayers__)
      real :: colco2(8,nlayers__)
      real :: colo3(8,nlayers__)
      real :: coln2o(8,nlayers__)
      real :: colco(8,nlayers__)
      real :: colch4(8,nlayers__)
      real :: colo2(8,nlayers__)
      real :: colbrd(8,nlayers__)
      integer :: indself(8,nlayers__)
      integer :: indfor(8,nlayers__)
      real :: selffac(8,nlayers__)
      real :: selffrac(8,nlayers__)
      real :: forfac(8,nlayers__)
      real :: forfrac(8,nlayers__)
      integer :: indminor(8,nlayers__)
      real :: minorfrac(8,nlayers__)
      real :: scaleminor(8,nlayers__)
      real :: scaleminorn2(8,nlayers__)
      real :: fac00(8,nlayers__), fac01(8,nlayers__), fac10(8,nlayers__), fac11(8,nlayers__)
      real :: rat_h2oco2(8,nlayers__),rat_h2oco2_1(8,nlayers__), &
               rat_h2oo3(8,nlayers__),rat_h2oo3_1(8,nlayers__), &
               rat_h2on2o(8,nlayers__),rat_h2on2o_1(8,nlayers__), &
               rat_h2och4(8,nlayers__),rat_h2och4_1(8,nlayers__), &
               rat_n2oco2(8,nlayers__),rat_n2oco2_1(8,nlayers__), &
               rat_o3co2(8,nlayers__),rat_o3co2_1(8,nlayers__)
      real :: tauaa(8, nlayers__, nbndlw__)
      integer :: nspad(nbndlw__)
      integer :: nspbd(nbndlw__)
      real :: oneminusd
      real :: taveld(8,nlayers+1)
      real :: tzd(8,0:nlayers+1)
      real :: tboundd(8)
      real :: wbroadd(8,nlayers+1)
      real :: totplnkd(181,nbndlw)
      real :: totplk16d(181)
      real :: totplnkderivd(181,nbndlw)
      real :: totplk16derivd(181)
      integer , value, intent(in) :: ncol
      integer , value, intent(in) :: nlayers
      integer , value, intent(in) :: istart
      integer :: indbound, indlev0
      integer :: lay, indlay, indlev, iband
      integer :: jp1
      real :: stpfac, tbndfrac, t0frac, tlayfrac, tlevfrac
      real :: dbdtlev, dbdtlay
      real :: plog, fp, ft, ft1, water, scalefac, factor, compfp
      integer :: iplon
      real :: wv, lcoldry
      do iplon = 1, ncol
        stpfac = 296. /1013.
        indbound = tboundd(iplon) - 159.
        if (indbound .lt. 1) then
           indbound = 1
        elseif (indbound .gt. 180) then
           indbound = 180
        endif
        tbndfrac = tboundd(iplon) - 159. - float(indbound)
        indlev0 = tzd(iplon, 0) - 159.
        if (indlev0 .lt. 1) then
           indlev0 = 1
        elseif (indlev0 .gt. 180) then
           indlev0 = 180
        endif
        t0frac = tzd(iplon, 0) - 159. - float(indlev0)
        laytrop(iplon) = 0
        do lay = 1, nlayers
          indlay = taveld(iplon, lay) - 159.
          lcoldry = coldry( iplon, lay)
          wv = colh2o(iplon, lay) * lcoldry
          if (indlay .lt. 1) then
             indlay = 1
          elseif (indlay .gt. 180) then
             indlay = 180
          endif
          tlayfrac = taveld(iplon, lay) - 159. - float(indlay)
          indlev = tzd(iplon, lay) - 159.
          if (indlev .lt. 1) then
             indlev = 1
          elseif (indlev .gt. 180) then
             indlev = 180
          endif
          tlevfrac = tzd(iplon, lay) - 159. - float(indlev)
          do iband = 1, 15
            if (lay.eq.1) then
               dbdtlev = totplnkd(indbound+1,iband) - totplnkd(indbound,iband)
               plankbndd(iplon, iband) = semissd(iplon, iband) * &
                   (totplnkd(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnkd(indlev0+1,iband)-totplnkd(indlev0,iband)
               planklevd(iplon, 0,iband) = totplnkd(indlev0,iband) + t0frac * dbdtlev
               if (idrvd .eq. 1) then
                  dbdtlev = totplnkderivd(indbound+1,iband) - totplnkderivd(indbound,iband)
                  dplankbnd_dtd(iplon, iband) = semissd(iplon, iband) * &
                        (totplnkderivd(indbound,iband) + tbndfrac * dbdtlev)
               endif
            endif
            dbdtlev = totplnkd(indlev+1,iband) - totplnkd(indlev,iband)
            dbdtlay = totplnkd(indlay+1,iband) - totplnkd(indlay,iband)
            planklayd(iplon, lay,iband) = totplnkd(indlay,iband) + tlayfrac * dbdtlay
            planklevd(iplon, lay,iband) = totplnkd(indlev,iband) + tlevfrac * dbdtlev
          enddo
          iband = 16
          if (istart .eq. 16) then
             if (lay.eq.1) then
                dbdtlev = totplk16d( indbound+1) - totplk16d( indbound)
                plankbndd(iplon, iband) = semissd(iplon, iband) * &
                     (totplk16d( indbound) + tbndfrac * dbdtlev)
                if (idrvd .eq. 1) then
                   dbdtlev = totplk16derivd( indbound+1) - totplk16derivd( indbound)
                   dplankbnd_dtd(iplon, iband) = semissd(iplon, iband) * &
                        (totplk16derivd(indbound) + tbndfrac * dbdtlev)
                endif
                dbdtlev = totplnkd(indlev0+1,iband)-totplnkd(indlev0,iband)
                planklevd(iplon, 0,iband) = totplk16d( indlev0) + &
                     t0frac * dbdtlev
             endif
             dbdtlev = totplk16d( indlev+1) - totplk16d( indlev)
             dbdtlay = totplk16d( indlay+1) - totplk16d( indlay)
             planklayd(iplon, lay,iband) = totplk16d( indlay) + tlayfrac * dbdtlay
             planklevd(iplon, lay,iband) = totplk16d( indlev) + tlevfrac * dbdtlev
          else
             if (lay.eq.1) then
                dbdtlev = totplnkd(indbound+1,iband) - totplnkd(indbound,iband)
                plankbndd(iplon, iband) = semissd(iplon, iband) * &
                     (totplnkd(indbound,iband) + tbndfrac * dbdtlev)
                if (idrvd .eq. 1) then
                   dbdtlev = totplnkderivd( indbound+1,iband) - totplnkderivd( indbound,iband)
                   dplankbnd_dtd(iplon, iband) = semissd(iplon, iband) * &
                        (totplnkderivd( indbound,iband) + tbndfrac * dbdtlev)
                endif
                dbdtlev = totplnkd(indlev0+1,iband)-totplnkd(indlev0,iband)
                planklevd(iplon, 0,iband) = totplnkd(indlev0,iband) + t0frac * dbdtlev
             endif
             dbdtlev = totplnkd(indlev+1,iband) - totplnkd(indlev,iband)
             dbdtlay = totplnkd(indlay+1,iband) - totplnkd(indlay,iband)
             planklayd(iplon, lay,iband) = totplnkd(indlay,iband) + tlayfrac * dbdtlay
             planklevd(iplon, lay,iband) = totplnkd(indlev,iband) + tlevfrac * dbdtlev
          endif
          plog = alog(pavel(iplon, lay))
          jp(iplon, lay) = int(36. - 5*(plog+0.04 ))
          if (jp(iplon, lay) .lt. 1) then
             jp(iplon, lay) = 1
          elseif (jp(iplon, lay) .gt. 58) then
             jp(iplon, lay) = 58
          endif
          jp1 = jp(iplon, lay) + 1
          fp = 5. *(preflog(jp(iplon, lay)) - plog)
          jt(iplon, lay) = int(3. + (taveld(iplon, lay)-tref(jp(iplon, lay)))/15. )
          if (jt(iplon, lay) .lt. 1) then
             jt(iplon, lay) = 1
          elseif (jt(iplon, lay) .gt. 4) then
             jt(iplon, lay) = 4
          endif
          ft = ((taveld(iplon, lay)-tref(jp(iplon, lay)))/15. ) - float(jt(iplon, lay)-3)
          jt1(iplon, lay) = int(3. + (taveld(iplon, lay)-tref( jp1))/15. )
          if (jt1(iplon, lay) .lt. 1) then
             jt1(iplon, lay) = 1
          elseif (jt1(iplon, lay) .gt. 4) then
             jt1(iplon, lay) = 4
          endif
          ft1 = ((taveld(iplon, lay)-tref(jp1))/15. ) - float(jt1(iplon, lay)-3)
          water = wv/lcoldry
          scalefac = pavel(iplon, lay) * stpfac / taveld(iplon, lay)
          if (plog .le. 4.56 ) go to 5300
          laytrop(iplon) = laytrop(iplon) + 1
          forfac(iplon, lay) = scalefac / (1.+water)
          factor = (332.0 -taveld(iplon, lay))/36.0
          indfor(iplon, lay) = min(2, max(1, int(factor)))
          forfrac(iplon, lay) = factor - float(indfor(iplon, lay))
          selffac(iplon, lay) = water * forfac(iplon, lay)
          factor = (taveld(iplon, lay)-188.0 )/7.2
          indself(iplon, lay) = min(9, max(1, int(factor)-7))
          selffrac(iplon, lay) = factor - float(indself(iplon, lay) + 7)
          scaleminor(iplon, lay) = pavel(iplon, lay)/taveld(iplon, lay)
          scaleminorn2(iplon, lay) = (pavel(iplon, lay)/taveld(iplon, lay)) &
              *(wbroadd(iplon, lay)/(lcoldry+wv))
          factor = (taveld(iplon, lay)-180.8 )/7.2
          indminor(iplon, lay) = min(18, max(1, int(factor)))
          minorfrac(iplon, lay) = factor - float(indminor(iplon, lay))
          rat_h2oco2(iplon, lay)=chi_mls( 1,jp(iplon, lay))/chi_mls( 2,jp(iplon, lay))
          rat_h2oco2_1(iplon, lay)=chi_mls( 1,jp(iplon, lay)+1)/chi_mls( 2,jp(iplon, lay)+1)
          rat_h2oo3(iplon, lay)=chi_mls( 1,jp(iplon, lay))/chi_mls( 3,jp(iplon, lay))
          rat_h2oo3_1(iplon, lay)=chi_mls( 1,jp(iplon, lay)+1)/chi_mls( 3,jp(iplon, lay)+1)
          rat_h2on2o(iplon, lay)=chi_mls( 1,jp(iplon, lay))/chi_mls( 4,jp(iplon, lay))
          rat_h2on2o_1(iplon, lay)=chi_mls( 1,jp(iplon, lay)+1)/chi_mls( 4,jp(iplon, lay)+1)
          rat_h2och4(iplon, lay)=chi_mls( 1,jp(iplon, lay))/chi_mls( 6,jp(iplon, lay))
          rat_h2och4_1(iplon, lay)=chi_mls( 1,jp(iplon, lay)+1)/chi_mls( 6,jp(iplon, lay)+1)
          rat_n2oco2(iplon, lay)=chi_mls( 4,jp(iplon, lay))/chi_mls( 2,jp(iplon, lay))
          rat_n2oco2_1(iplon, lay)=chi_mls( 4,jp(iplon, lay)+1)/chi_mls( 2,jp(iplon, lay)+1)
          colh2o(iplon, lay) = 1.e-20 * colh2o(iplon, lay) * lcoldry
          colco2(iplon, lay) = 1.e-20 * colco2(iplon, lay) * lcoldry
          colo3(iplon, lay) = 1.e-20 * colo3(iplon, lay) * lcoldry
          coln2o(iplon, lay) = 1.e-20 * coln2o(iplon, lay) * lcoldry
          colco(iplon, lay) = 1.e-20 * colco(iplon, lay) * lcoldry
          colch4(iplon, lay) = 1.e-20 * colch4(iplon, lay) * lcoldry
          colo2(iplon, lay) = 1.e-20 * colo2(iplon, lay) * lcoldry
          if (colco2(iplon, lay) .eq. 0. ) colco2(iplon, lay) = 1.e-32 * lcoldry
          if (colo3(iplon, lay) .eq. 0. ) colo3(iplon, lay) = 1.e-32 * lcoldry
          if (coln2o(iplon, lay) .eq. 0. ) coln2o(iplon, lay) = 1.e-32 * lcoldry
          if (colco(iplon, lay) .eq. 0. ) colco(iplon, lay) = 1.e-32 * lcoldry
          if (colch4(iplon, lay) .eq. 0. ) colch4(iplon, lay) = 1.e-32 * lcoldry
          colbrd(iplon, lay) = 1.e-20 * wbroadd(iplon, lay)
          go to 5400
 5300 continue
          forfac(iplon, lay) = scalefac / (1.+water)
          factor = (taveld(iplon, lay)-188.0 )/36.0
          indfor(iplon, lay) = 3
          forfrac(iplon, lay) = factor - 1.0
          selffac(iplon, lay) = water * forfac(iplon, lay)
          scaleminor(iplon, lay) = pavel(iplon, lay)/taveld(iplon, lay)
          scaleminorn2(iplon, lay) = (pavel(iplon, lay)/taveld(iplon, lay)) &
              * (wbroadd(iplon, lay)/(coldry(iplon, lay)+wv))
          factor = (taveld(iplon, lay)-180.8 )/7.2
          indminor(iplon, lay) = min(18, max(1, int(factor)))
          minorfrac(iplon, lay) = factor - float(indminor(iplon, lay))
          rat_h2oco2(iplon, lay)=chi_mls( 1,jp(iplon, lay))/chi_mls( 2,jp(iplon, lay))
          rat_h2oco2_1(iplon, lay)=chi_mls( 1,jp(iplon, lay)+1)/chi_mls( 2,jp(iplon, lay)+1)
          rat_o3co2(iplon, lay)=chi_mls( 3,jp(iplon, lay))/chi_mls( 2,jp(iplon, lay))
          rat_o3co2_1(iplon, lay)=chi_mls( 3,jp(iplon, lay)+1)/chi_mls( 2,jp(iplon, lay)+1)
          colh2o(iplon, lay) = 1.e-20 * colh2o(iplon, lay) * lcoldry
          colco2(iplon, lay) = 1.e-20 * colco2(iplon, lay) * lcoldry
          colo3(iplon, lay) = 1.e-20 * colo3(iplon, lay) * lcoldry
          coln2o(iplon, lay) = 1.e-20 * coln2o(iplon, lay) * lcoldry
          colco(iplon, lay) = 1.e-20 * colco(iplon, lay) * lcoldry
          colch4(iplon, lay) = 1.e-20 * colch4(iplon, lay) * lcoldry
          colo2(iplon, lay) = 1.e-20 * colo2(iplon, lay) * lcoldry
          if (colco2(iplon, lay) .eq. 0. ) colco2(iplon, lay) = 1.e-32 * lcoldry
          if (colo3(iplon, lay) .eq. 0. ) colo3(iplon, lay) = 1.e-32 * lcoldry
          if (coln2o(iplon, lay) .eq. 0. ) coln2o(iplon, lay) = 1.e-32 * lcoldry
          if (colco(iplon, lay) .eq. 0. ) colco(iplon, lay) = 1.e-32 * lcoldry
          if (colch4(iplon, lay) .eq. 0. ) colch4(iplon, lay) = 1.e-32 * lcoldry
          colbrd(iplon, lay) = 1.e-20 * wbroadd(iplon, lay)
 5400 continue
          compfp = 1. - fp
          fac10(iplon, lay) = compfp * ft
          fac00(iplon, lay) = compfp * (1. - ft)
          fac11(iplon, lay) = fp * ft1
          fac01(iplon, lay) = fp * (1. - ft1)
          selffac(iplon, lay) = colh2o(iplon, lay)*selffac(iplon, lay)
          forfac(iplon, lay) = colh2o(iplon, lay)*forfac(iplon, lay)
        enddo
      end do
      end subroutine setcoefg
      end module gpu_rrtmg_lw_setcoef
      module rrtmg_lw_setcoef_f
      use parrrtm_f, only : nbndlw, mg, maxxsec, mxmol
      use rrlw_wvn_f, only: totplnk, totplk16, totplnkderiv, totplk16deriv
      use rrlw_ref_f
      implicit none
      contains
      subroutine lwatmref
      save
      pref(:) = (/ &
          1.05363e+03 ,8.62642e+02 ,7.06272e+02 ,5.78246e+02 ,4.73428e+02 , &
          3.87610e+02 ,3.17348e+02 ,2.59823e+02 ,2.12725e+02 ,1.74164e+02 , &
          1.42594e+02 ,1.16746e+02 ,9.55835e+01 ,7.82571e+01 ,6.40715e+01 , &
          5.24573e+01 ,4.29484e+01 ,3.51632e+01 ,2.87892e+01 ,2.35706e+01 , &
          1.92980e+01 ,1.57998e+01 ,1.29358e+01 ,1.05910e+01 ,8.67114e+00 , &
          7.09933e+00 ,5.81244e+00 ,4.75882e+00 ,3.89619e+00 ,3.18993e+00 , &
          2.61170e+00 ,2.13828e+00 ,1.75067e+00 ,1.43333e+00 ,1.17351e+00 , &
          9.60789e-01 ,7.86628e-01 ,6.44036e-01 ,5.27292e-01 ,4.31710e-01 , &
          3.53455e-01 ,2.89384e-01 ,2.36928e-01 ,1.93980e-01 ,1.58817e-01 , &
          1.30029e-01 ,1.06458e-01 ,8.71608e-02 ,7.13612e-02 ,5.84256e-02 , &
          4.78349e-02 ,3.91639e-02 ,3.20647e-02 ,2.62523e-02 ,2.14936e-02 , &
          1.75975e-02 ,1.44076e-02 ,1.17959e-02 ,9.65769e-03 /)
      preflog(:) = (/ &
           6.9600e+00 , 6.7600e+00 , 6.5600e+00 , 6.3600e+00 , 6.1600e+00 , &
           5.9600e+00 , 5.7600e+00 , 5.5600e+00 , 5.3600e+00 , 5.1600e+00 , &
           4.9600e+00 , 4.7600e+00 , 4.5600e+00 , 4.3600e+00 , 4.1600e+00 , &
           3.9600e+00 , 3.7600e+00 , 3.5600e+00 , 3.3600e+00 , 3.1600e+00 , &
           2.9600e+00 , 2.7600e+00 , 2.5600e+00 , 2.3600e+00 , 2.1600e+00 , &
           1.9600e+00 , 1.7600e+00 , 1.5600e+00 , 1.3600e+00 , 1.1600e+00 , &
           9.6000e-01 , 7.6000e-01 , 5.6000e-01 , 3.6000e-01 , 1.6000e-01 , &
          -4.0000e-02 ,-2.4000e-01 ,-4.4000e-01 ,-6.4000e-01 ,-8.4000e-01 , &
          -1.0400e+00 ,-1.2400e+00 ,-1.4400e+00 ,-1.6400e+00 ,-1.8400e+00 , &
          -2.0400e+00 ,-2.2400e+00 ,-2.4400e+00 ,-2.6400e+00 ,-2.8400e+00 , &
          -3.0400e+00 ,-3.2400e+00 ,-3.4400e+00 ,-3.6400e+00 ,-3.8400e+00 , &
          -4.0400e+00 ,-4.2400e+00 ,-4.4400e+00 ,-4.6400e+00 /)
      tref(:) = (/ &
           2.9420e+02 , 2.8799e+02 , 2.7894e+02 , 2.6925e+02 , 2.5983e+02 , &
           2.5017e+02 , 2.4077e+02 , 2.3179e+02 , 2.2306e+02 , 2.1578e+02 , &
           2.1570e+02 , 2.1570e+02 , 2.1570e+02 , 2.1706e+02 , 2.1858e+02 , &
           2.2018e+02 , 2.2174e+02 , 2.2328e+02 , 2.2479e+02 , 2.2655e+02 , &
           2.2834e+02 , 2.3113e+02 , 2.3401e+02 , 2.3703e+02 , 2.4022e+02 , &
           2.4371e+02 , 2.4726e+02 , 2.5085e+02 , 2.5457e+02 , 2.5832e+02 , &
           2.6216e+02 , 2.6606e+02 , 2.6999e+02 , 2.7340e+02 , 2.7536e+02 , &
           2.7568e+02 , 2.7372e+02 , 2.7163e+02 , 2.6955e+02 , 2.6593e+02 , &
           2.6211e+02 , 2.5828e+02 , 2.5360e+02 , 2.4854e+02 , 2.4348e+02 , &
           2.3809e+02 , 2.3206e+02 , 2.2603e+02 , 2.2000e+02 , 2.1435e+02 , &
           2.0887e+02 , 2.0340e+02 , 1.9792e+02 , 1.9290e+02 , 1.8809e+02 , &
           1.8329e+02 , 1.7849e+02 , 1.7394e+02 , 1.7212e+02 /)
       chi_mls(1,1:12) = (/ &
        1.8760e-02 , 1.2223e-02 , 5.8909e-03 , 2.7675e-03 , 1.4065e-03 , &
        7.5970e-04 , 3.8876e-04 , 1.6542e-04 , 3.7190e-05 , 7.4765e-06 , &
        4.3082e-06 , 3.3319e-06 /)
       chi_mls(1,13:59) = (/ &
        3.2039e-06 , 3.1619e-06 , 3.2524e-06 , 3.4226e-06 , 3.6288e-06 , &
        3.9148e-06 , 4.1488e-06 , 4.3081e-06 , 4.4420e-06 , 4.5778e-06 , &
        4.7087e-06 , 4.7943e-06 , 4.8697e-06 , 4.9260e-06 , 4.9669e-06 , &
        4.9963e-06 , 5.0527e-06 , 5.1266e-06 , 5.2503e-06 , 5.3571e-06 , &
        5.4509e-06 , 5.4830e-06 , 5.5000e-06 , 5.5000e-06 , 5.4536e-06 , &
        5.4047e-06 , 5.3558e-06 , 5.2533e-06 , 5.1436e-06 , 5.0340e-06 , &
        4.8766e-06 , 4.6979e-06 , 4.5191e-06 , 4.3360e-06 , 4.1442e-06 , &
        3.9523e-06 , 3.7605e-06 , 3.5722e-06 , 3.3855e-06 , 3.1988e-06 , &
        3.0121e-06 , 2.8262e-06 , 2.6407e-06 , 2.4552e-06 , 2.2696e-06 , &
        4.3360e-06 , 4.1442e-06 /)
       chi_mls(2,1:12) = (/ &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 /)
       chi_mls(2,13:59) = (/ &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , 3.5500e-04 , &
        3.5500e-04 , 3.5471e-04 , 3.5427e-04 , 3.5384e-04 , 3.5340e-04 , &
        3.5500e-04 , 3.5500e-04 /)
       chi_mls(3,1:12) = (/ &
        3.0170e-08 , 3.4725e-08 , 4.2477e-08 , 5.2759e-08 , 6.6944e-08 , &
        8.7130e-08 , 1.1391e-07 , 1.5677e-07 , 2.1788e-07 , 3.2443e-07 , &
        4.6594e-07 , 5.6806e-07 /)
       chi_mls(3,13:59) = (/ &
        6.9607e-07 , 1.1186e-06 , 1.7618e-06 , 2.3269e-06 , 2.9577e-06 , &
        3.6593e-06 , 4.5950e-06 , 5.3189e-06 , 5.9618e-06 , 6.5113e-06 , &
        7.0635e-06 , 7.6917e-06 , 8.2577e-06 , 8.7082e-06 , 8.8325e-06 , &
        8.7149e-06 , 8.0943e-06 , 7.3307e-06 , 6.3101e-06 , 5.3672e-06 , &
        4.4829e-06 , 3.8391e-06 , 3.2827e-06 , 2.8235e-06 , 2.4906e-06 , &
        2.1645e-06 , 1.8385e-06 , 1.6618e-06 , 1.5052e-06 , 1.3485e-06 , &
        1.1972e-06 , 1.0482e-06 , 8.9926e-07 , 7.6343e-07 , 6.5381e-07 , &
        5.4419e-07 , 4.3456e-07 , 3.6421e-07 , 3.1194e-07 , 2.5967e-07 , &
        2.0740e-07 , 1.9146e-07 , 1.9364e-07 , 1.9582e-07 , 1.9800e-07 , &
        7.6343e-07 , 6.5381e-07 /)
       chi_mls(4,1:12) = (/ &
        3.2000e-07 , 3.2000e-07 , 3.2000e-07 , 3.2000e-07 , 3.2000e-07 , &
        3.1965e-07 , 3.1532e-07 , 3.0383e-07 , 2.9422e-07 , 2.8495e-07 , &
        2.7671e-07 , 2.6471e-07 /)
       chi_mls(4,13:59) = (/ &
        2.4285e-07 , 2.0955e-07 , 1.7195e-07 , 1.3749e-07 , 1.1332e-07 , &
        1.0035e-07 , 9.1281e-08 , 8.5463e-08 , 8.0363e-08 , 7.3372e-08 , &
        6.5975e-08 , 5.6039e-08 , 4.7090e-08 , 3.9977e-08 , 3.2979e-08 , &
        2.6064e-08 , 2.1066e-08 , 1.6592e-08 , 1.3017e-08 , 1.0090e-08 , &
        7.6249e-09 , 6.1159e-09 , 4.6672e-09 , 3.2857e-09 , 2.8484e-09 , &
        2.4620e-09 , 2.0756e-09 , 1.8551e-09 , 1.6568e-09 , 1.4584e-09 , &
        1.3195e-09 , 1.2072e-09 , 1.0948e-09 , 9.9780e-10 , 9.3126e-10 , &
        8.6472e-10 , 7.9818e-10 , 7.5138e-10 , 7.1367e-10 , 6.7596e-10 , &
        6.3825e-10 , 6.0981e-10 , 5.8600e-10 , 5.6218e-10 , 5.3837e-10 , &
        9.9780e-10 , 9.3126e-10 /)
       chi_mls(5,1:12) = (/ &
        1.5000e-07 , 1.4306e-07 , 1.3474e-07 , 1.3061e-07 , 1.2793e-07 , &
        1.2038e-07 , 1.0798e-07 , 9.4238e-08 , 7.9488e-08 , 6.1386e-08 , &
        4.5563e-08 , 3.3475e-08 /)
       chi_mls(5,13:59) = (/ &
        2.5118e-08 , 1.8671e-08 , 1.4349e-08 , 1.2501e-08 , 1.2407e-08 , &
        1.3472e-08 , 1.4900e-08 , 1.6079e-08 , 1.7156e-08 , 1.8616e-08 , &
        2.0106e-08 , 2.1654e-08 , 2.3096e-08 , 2.4340e-08 , 2.5643e-08 , &
        2.6990e-08 , 2.8456e-08 , 2.9854e-08 , 3.0943e-08 , 3.2023e-08 , &
        3.3101e-08 , 3.4260e-08 , 3.5360e-08 , 3.6397e-08 , 3.7310e-08 , &
        3.8217e-08 , 3.9123e-08 , 4.1303e-08 , 4.3652e-08 , 4.6002e-08 , &
        5.0289e-08 , 5.5446e-08 , 6.0603e-08 , 6.8946e-08 , 8.3652e-08 , &
        9.8357e-08 , 1.1306e-07 , 1.4766e-07 , 1.9142e-07 , 2.3518e-07 , &
        2.7894e-07 , 3.5001e-07 , 4.3469e-07 , 5.1938e-07 , 6.0407e-07 , &
        6.8946e-08 , 8.3652e-08 /)
       chi_mls(6,1:12) = (/ &
        1.7000e-06 , 1.7000e-06 , 1.6999e-06 , 1.6904e-06 , 1.6671e-06 , &
        1.6351e-06 , 1.6098e-06 , 1.5590e-06 , 1.5120e-06 , 1.4741e-06 , &
        1.4385e-06 , 1.4002e-06 /)
       chi_mls(6,13:59) = (/ &
        1.3573e-06 , 1.3130e-06 , 1.2512e-06 , 1.1668e-06 , 1.0553e-06 , &
        9.3281e-07 , 8.1217e-07 , 7.5239e-07 , 7.0728e-07 , 6.6722e-07 , &
        6.2733e-07 , 5.8604e-07 , 5.4769e-07 , 5.1480e-07 , 4.8206e-07 , &
        4.4943e-07 , 4.1702e-07 , 3.8460e-07 , 3.5200e-07 , 3.1926e-07 , &
        2.8646e-07 , 2.5498e-07 , 2.2474e-07 , 1.9588e-07 , 1.8295e-07 , &
        1.7089e-07 , 1.5882e-07 , 1.5536e-07 , 1.5304e-07 , 1.5072e-07 , &
        1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , &
        1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , &
        1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , 1.5000e-07 , &
        1.5000e-07 , 1.5000e-07 /)
       chi_mls(7,1:12) = (/ &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 /)
       chi_mls(7,13:59) = (/ &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 , 0.2090 , 0.2090 , 0.2090 , &
        0.2090 , 0.2090 /)
      end subroutine lwatmref
      subroutine lwavplank
      save
      totplnk(1:50, 1) = (/ &
      0.14783e-05 ,0.15006e-05 ,0.15230e-05 ,0.15455e-05 ,0.15681e-05 , &
      0.15908e-05 ,0.16136e-05 ,0.16365e-05 ,0.16595e-05 ,0.16826e-05 , &
      0.17059e-05 ,0.17292e-05 ,0.17526e-05 ,0.17762e-05 ,0.17998e-05 , &
      0.18235e-05 ,0.18473e-05 ,0.18712e-05 ,0.18953e-05 ,0.19194e-05 , &
      0.19435e-05 ,0.19678e-05 ,0.19922e-05 ,0.20166e-05 ,0.20412e-05 , &
      0.20658e-05 ,0.20905e-05 ,0.21153e-05 ,0.21402e-05 ,0.21652e-05 , &
      0.21902e-05 ,0.22154e-05 ,0.22406e-05 ,0.22659e-05 ,0.22912e-05 , &
      0.23167e-05 ,0.23422e-05 ,0.23678e-05 ,0.23934e-05 ,0.24192e-05 , &
      0.24450e-05 ,0.24709e-05 ,0.24968e-05 ,0.25229e-05 ,0.25490e-05 , &
      0.25751e-05 ,0.26014e-05 ,0.26277e-05 ,0.26540e-05 ,0.26805e-05 /)
      totplnk(51:100, 1) = (/ &
      0.27070e-05 ,0.27335e-05 ,0.27602e-05 ,0.27869e-05 ,0.28136e-05 , &
      0.28404e-05 ,0.28673e-05 ,0.28943e-05 ,0.29213e-05 ,0.29483e-05 , &
      0.29754e-05 ,0.30026e-05 ,0.30298e-05 ,0.30571e-05 ,0.30845e-05 , &
      0.31119e-05 ,0.31393e-05 ,0.31669e-05 ,0.31944e-05 ,0.32220e-05 , &
      0.32497e-05 ,0.32774e-05 ,0.33052e-05 ,0.33330e-05 ,0.33609e-05 , &
      0.33888e-05 ,0.34168e-05 ,0.34448e-05 ,0.34729e-05 ,0.35010e-05 , &
      0.35292e-05 ,0.35574e-05 ,0.35857e-05 ,0.36140e-05 ,0.36424e-05 , &
      0.36708e-05 ,0.36992e-05 ,0.37277e-05 ,0.37563e-05 ,0.37848e-05 , &
      0.38135e-05 ,0.38421e-05 ,0.38708e-05 ,0.38996e-05 ,0.39284e-05 , &
      0.39572e-05 ,0.39861e-05 ,0.40150e-05 ,0.40440e-05 ,0.40730e-05 /)
      totplnk(101:150, 1) = (/ &
      0.41020e-05 ,0.41311e-05 ,0.41602e-05 ,0.41893e-05 ,0.42185e-05 , &
      0.42477e-05 ,0.42770e-05 ,0.43063e-05 ,0.43356e-05 ,0.43650e-05 , &
      0.43944e-05 ,0.44238e-05 ,0.44533e-05 ,0.44828e-05 ,0.45124e-05 , &
      0.45419e-05 ,0.45715e-05 ,0.46012e-05 ,0.46309e-05 ,0.46606e-05 , &
      0.46903e-05 ,0.47201e-05 ,0.47499e-05 ,0.47797e-05 ,0.48096e-05 , &
      0.48395e-05 ,0.48695e-05 ,0.48994e-05 ,0.49294e-05 ,0.49594e-05 , &
      0.49895e-05 ,0.50196e-05 ,0.50497e-05 ,0.50798e-05 ,0.51100e-05 , &
      0.51402e-05 ,0.51704e-05 ,0.52007e-05 ,0.52309e-05 ,0.52612e-05 , &
      0.52916e-05 ,0.53219e-05 ,0.53523e-05 ,0.53827e-05 ,0.54132e-05 , &
      0.54436e-05 ,0.54741e-05 ,0.55047e-05 ,0.55352e-05 ,0.55658e-05 /)
      totplnk(151:181, 1) = (/ &
      0.55964e-05 ,0.56270e-05 ,0.56576e-05 ,0.56883e-05 ,0.57190e-05 , &
      0.57497e-05 ,0.57804e-05 ,0.58112e-05 ,0.58420e-05 ,0.58728e-05 , &
      0.59036e-05 ,0.59345e-05 ,0.59653e-05 ,0.59962e-05 ,0.60272e-05 , &
      0.60581e-05 ,0.60891e-05 ,0.61201e-05 ,0.61511e-05 ,0.61821e-05 , &
      0.62131e-05 ,0.62442e-05 ,0.62753e-05 ,0.63064e-05 ,0.63376e-05 , &
      0.63687e-05 ,0.63998e-05 ,0.64310e-05 ,0.64622e-05 ,0.64935e-05 , &
      0.65247e-05 /)
      totplnk(1:50, 2) = (/ &
      0.20262e-05 ,0.20757e-05 ,0.21257e-05 ,0.21763e-05 ,0.22276e-05 , &
      0.22794e-05 ,0.23319e-05 ,0.23849e-05 ,0.24386e-05 ,0.24928e-05 , &
      0.25477e-05 ,0.26031e-05 ,0.26591e-05 ,0.27157e-05 ,0.27728e-05 , &
      0.28306e-05 ,0.28889e-05 ,0.29478e-05 ,0.30073e-05 ,0.30673e-05 , &
      0.31279e-05 ,0.31890e-05 ,0.32507e-05 ,0.33129e-05 ,0.33757e-05 , &
      0.34391e-05 ,0.35029e-05 ,0.35674e-05 ,0.36323e-05 ,0.36978e-05 , &
      0.37638e-05 ,0.38304e-05 ,0.38974e-05 ,0.39650e-05 ,0.40331e-05 , &
      0.41017e-05 ,0.41708e-05 ,0.42405e-05 ,0.43106e-05 ,0.43812e-05 , &
      0.44524e-05 ,0.45240e-05 ,0.45961e-05 ,0.46687e-05 ,0.47418e-05 , &
      0.48153e-05 ,0.48894e-05 ,0.49639e-05 ,0.50389e-05 ,0.51143e-05 /)
      totplnk(51:100, 2) = (/ &
      0.51902e-05 ,0.52666e-05 ,0.53434e-05 ,0.54207e-05 ,0.54985e-05 , &
      0.55767e-05 ,0.56553e-05 ,0.57343e-05 ,0.58139e-05 ,0.58938e-05 , &
      0.59742e-05 ,0.60550e-05 ,0.61362e-05 ,0.62179e-05 ,0.63000e-05 , &
      0.63825e-05 ,0.64654e-05 ,0.65487e-05 ,0.66324e-05 ,0.67166e-05 , &
      0.68011e-05 ,0.68860e-05 ,0.69714e-05 ,0.70571e-05 ,0.71432e-05 , &
      0.72297e-05 ,0.73166e-05 ,0.74039e-05 ,0.74915e-05 ,0.75796e-05 , &
      0.76680e-05 ,0.77567e-05 ,0.78459e-05 ,0.79354e-05 ,0.80252e-05 , &
      0.81155e-05 ,0.82061e-05 ,0.82970e-05 ,0.83883e-05 ,0.84799e-05 , &
      0.85719e-05 ,0.86643e-05 ,0.87569e-05 ,0.88499e-05 ,0.89433e-05 , &
      0.90370e-05 ,0.91310e-05 ,0.92254e-05 ,0.93200e-05 ,0.94150e-05 /)
      totplnk(101:150, 2) = (/ &
      0.95104e-05 ,0.96060e-05 ,0.97020e-05 ,0.97982e-05 ,0.98948e-05 , &
      0.99917e-05 ,0.10089e-04 ,0.10186e-04 ,0.10284e-04 ,0.10382e-04 , &
      0.10481e-04 ,0.10580e-04 ,0.10679e-04 ,0.10778e-04 ,0.10877e-04 , &
      0.10977e-04 ,0.11077e-04 ,0.11178e-04 ,0.11279e-04 ,0.11380e-04 , &
      0.11481e-04 ,0.11583e-04 ,0.11684e-04 ,0.11786e-04 ,0.11889e-04 , &
      0.11992e-04 ,0.12094e-04 ,0.12198e-04 ,0.12301e-04 ,0.12405e-04 , &
      0.12509e-04 ,0.12613e-04 ,0.12717e-04 ,0.12822e-04 ,0.12927e-04 , &
      0.13032e-04 ,0.13138e-04 ,0.13244e-04 ,0.13349e-04 ,0.13456e-04 , &
      0.13562e-04 ,0.13669e-04 ,0.13776e-04 ,0.13883e-04 ,0.13990e-04 , &
      0.14098e-04 ,0.14206e-04 ,0.14314e-04 ,0.14422e-04 ,0.14531e-04 /)
      totplnk(151:181, 2) = (/ &
      0.14639e-04 ,0.14748e-04 ,0.14857e-04 ,0.14967e-04 ,0.15076e-04 , &
      0.15186e-04 ,0.15296e-04 ,0.15407e-04 ,0.15517e-04 ,0.15628e-04 , &
      0.15739e-04 ,0.15850e-04 ,0.15961e-04 ,0.16072e-04 ,0.16184e-04 , &
      0.16296e-04 ,0.16408e-04 ,0.16521e-04 ,0.16633e-04 ,0.16746e-04 , &
      0.16859e-04 ,0.16972e-04 ,0.17085e-04 ,0.17198e-04 ,0.17312e-04 , &
      0.17426e-04 ,0.17540e-04 ,0.17654e-04 ,0.17769e-04 ,0.17883e-04 , &
      0.17998e-04 /)
      totplnk(1:50, 3) = (/ &
      1.34822e-06 ,1.39134e-06 ,1.43530e-06 ,1.48010e-06 ,1.52574e-06 , &
      1.57222e-06 ,1.61956e-06 ,1.66774e-06 ,1.71678e-06 ,1.76666e-06 , &
      1.81741e-06 ,1.86901e-06 ,1.92147e-06 ,1.97479e-06 ,2.02898e-06 , &
      2.08402e-06 ,2.13993e-06 ,2.19671e-06 ,2.25435e-06 ,2.31285e-06 , &
      2.37222e-06 ,2.43246e-06 ,2.49356e-06 ,2.55553e-06 ,2.61837e-06 , &
      2.68207e-06 ,2.74664e-06 ,2.81207e-06 ,2.87837e-06 ,2.94554e-06 , &
      3.01356e-06 ,3.08245e-06 ,3.15221e-06 ,3.22282e-06 ,3.29429e-06 , &
      3.36662e-06 ,3.43982e-06 ,3.51386e-06 ,3.58876e-06 ,3.66451e-06 , &
      3.74112e-06 ,3.81857e-06 ,3.89688e-06 ,3.97602e-06 ,4.05601e-06 , &
      4.13685e-06 ,4.21852e-06 ,4.30104e-06 ,4.38438e-06 ,4.46857e-06 /)
      totplnk(51:100, 3) = (/ &
      4.55358e-06 ,4.63943e-06 ,4.72610e-06 ,4.81359e-06 ,4.90191e-06 , &
      4.99105e-06 ,5.08100e-06 ,5.17176e-06 ,5.26335e-06 ,5.35573e-06 , &
      5.44892e-06 ,5.54292e-06 ,5.63772e-06 ,5.73331e-06 ,5.82970e-06 , &
      5.92688e-06 ,6.02485e-06 ,6.12360e-06 ,6.22314e-06 ,6.32346e-06 , &
      6.42455e-06 ,6.52641e-06 ,6.62906e-06 ,6.73247e-06 ,6.83664e-06 , &
      6.94156e-06 ,7.04725e-06 ,7.15370e-06 ,7.26089e-06 ,7.36883e-06 , &
      7.47752e-06 ,7.58695e-06 ,7.69712e-06 ,7.80801e-06 ,7.91965e-06 , &
      8.03201e-06 ,8.14510e-06 ,8.25891e-06 ,8.37343e-06 ,8.48867e-06 , &
      8.60463e-06 ,8.72128e-06 ,8.83865e-06 ,8.95672e-06 ,9.07548e-06 , &
      9.19495e-06 ,9.31510e-06 ,9.43594e-06 ,9.55745e-06 ,9.67966e-06 /)
      totplnk(101:150, 3) = (/ &
      9.80254e-06 ,9.92609e-06 ,1.00503e-05 ,1.01752e-05 ,1.03008e-05 , &
      1.04270e-05 ,1.05539e-05 ,1.06814e-05 ,1.08096e-05 ,1.09384e-05 , &
      1.10679e-05 ,1.11980e-05 ,1.13288e-05 ,1.14601e-05 ,1.15922e-05 , &
      1.17248e-05 ,1.18581e-05 ,1.19920e-05 ,1.21265e-05 ,1.22616e-05 , &
      1.23973e-05 ,1.25337e-05 ,1.26706e-05 ,1.28081e-05 ,1.29463e-05 , &
      1.30850e-05 ,1.32243e-05 ,1.33642e-05 ,1.35047e-05 ,1.36458e-05 , &
      1.37875e-05 ,1.39297e-05 ,1.40725e-05 ,1.42159e-05 ,1.43598e-05 , &
      1.45044e-05 ,1.46494e-05 ,1.47950e-05 ,1.49412e-05 ,1.50879e-05 , &
      1.52352e-05 ,1.53830e-05 ,1.55314e-05 ,1.56803e-05 ,1.58297e-05 , &
      1.59797e-05 ,1.61302e-05 ,1.62812e-05 ,1.64327e-05 ,1.65848e-05 /)
      totplnk(151:181, 3) = (/ &
      1.67374e-05 ,1.68904e-05 ,1.70441e-05 ,1.71982e-05 ,1.73528e-05 , &
      1.75079e-05 ,1.76635e-05 ,1.78197e-05 ,1.79763e-05 ,1.81334e-05 , &
      1.82910e-05 ,1.84491e-05 ,1.86076e-05 ,1.87667e-05 ,1.89262e-05 , &
      1.90862e-05 ,1.92467e-05 ,1.94076e-05 ,1.95690e-05 ,1.97309e-05 , &
      1.98932e-05 ,2.00560e-05 ,2.02193e-05 ,2.03830e-05 ,2.05472e-05 , &
      2.07118e-05 ,2.08768e-05 ,2.10423e-05 ,2.12083e-05 ,2.13747e-05 , &
      2.15414e-05 /)
      totplnk(1:50, 4) = (/ &
      8.90528e-07 ,9.24222e-07 ,9.58757e-07 ,9.94141e-07 ,1.03038e-06 , &
      1.06748e-06 ,1.10545e-06 ,1.14430e-06 ,1.18403e-06 ,1.22465e-06 , &
      1.26618e-06 ,1.30860e-06 ,1.35193e-06 ,1.39619e-06 ,1.44136e-06 , &
      1.48746e-06 ,1.53449e-06 ,1.58246e-06 ,1.63138e-06 ,1.68124e-06 , &
      1.73206e-06 ,1.78383e-06 ,1.83657e-06 ,1.89028e-06 ,1.94495e-06 , &
      2.00060e-06 ,2.05724e-06 ,2.11485e-06 ,2.17344e-06 ,2.23303e-06 , &
      2.29361e-06 ,2.35519e-06 ,2.41777e-06 ,2.48134e-06 ,2.54592e-06 , &
      2.61151e-06 ,2.67810e-06 ,2.74571e-06 ,2.81433e-06 ,2.88396e-06 , &
      2.95461e-06 ,3.02628e-06 ,3.09896e-06 ,3.17267e-06 ,3.24741e-06 , &
      3.32316e-06 ,3.39994e-06 ,3.47774e-06 ,3.55657e-06 ,3.63642e-06 /)
      totplnk(51:100, 4) = (/ &
      3.71731e-06 ,3.79922e-06 ,3.88216e-06 ,3.96612e-06 ,4.05112e-06 , &
      4.13714e-06 ,4.22419e-06 ,4.31227e-06 ,4.40137e-06 ,4.49151e-06 , &
      4.58266e-06 ,4.67485e-06 ,4.76806e-06 ,4.86229e-06 ,4.95754e-06 , &
      5.05383e-06 ,5.15113e-06 ,5.24946e-06 ,5.34879e-06 ,5.44916e-06 , &
      5.55053e-06 ,5.65292e-06 ,5.75632e-06 ,5.86073e-06 ,5.96616e-06 , &
      6.07260e-06 ,6.18003e-06 ,6.28848e-06 ,6.39794e-06 ,6.50838e-06 , &
      6.61983e-06 ,6.73229e-06 ,6.84573e-06 ,6.96016e-06 ,7.07559e-06 , &
      7.19200e-06 ,7.30940e-06 ,7.42779e-06 ,7.54715e-06 ,7.66749e-06 , &
      7.78882e-06 ,7.91110e-06 ,8.03436e-06 ,8.15859e-06 ,8.28379e-06 , &
      8.40994e-06 ,8.53706e-06 ,8.66515e-06 ,8.79418e-06 ,8.92416e-06 /)
      totplnk(101:150, 4) = (/ &
      9.05510e-06 ,9.18697e-06 ,9.31979e-06 ,9.45356e-06 ,9.58826e-06 , &
      9.72389e-06 ,9.86046e-06 ,9.99793e-06 ,1.01364e-05 ,1.02757e-05 , &
      1.04159e-05 ,1.05571e-05 ,1.06992e-05 ,1.08422e-05 ,1.09861e-05 , &
      1.11309e-05 ,1.12766e-05 ,1.14232e-05 ,1.15707e-05 ,1.17190e-05 , &
      1.18683e-05 ,1.20184e-05 ,1.21695e-05 ,1.23214e-05 ,1.24741e-05 , &
      1.26277e-05 ,1.27822e-05 ,1.29376e-05 ,1.30939e-05 ,1.32509e-05 , &
      1.34088e-05 ,1.35676e-05 ,1.37273e-05 ,1.38877e-05 ,1.40490e-05 , &
      1.42112e-05 ,1.43742e-05 ,1.45380e-05 ,1.47026e-05 ,1.48680e-05 , &
      1.50343e-05 ,1.52014e-05 ,1.53692e-05 ,1.55379e-05 ,1.57074e-05 , &
      1.58778e-05 ,1.60488e-05 ,1.62207e-05 ,1.63934e-05 ,1.65669e-05 /)
      totplnk(151:181, 4) = (/ &
      1.67411e-05 ,1.69162e-05 ,1.70920e-05 ,1.72685e-05 ,1.74459e-05 , &
      1.76240e-05 ,1.78029e-05 ,1.79825e-05 ,1.81629e-05 ,1.83440e-05 , &
      1.85259e-05 ,1.87086e-05 ,1.88919e-05 ,1.90760e-05 ,1.92609e-05 , &
      1.94465e-05 ,1.96327e-05 ,1.98199e-05 ,2.00076e-05 ,2.01961e-05 , &
      2.03853e-05 ,2.05752e-05 ,2.07658e-05 ,2.09571e-05 ,2.11491e-05 , &
      2.13418e-05 ,2.15352e-05 ,2.17294e-05 ,2.19241e-05 ,2.21196e-05 , &
      2.23158e-05 /)
      totplnk(1:50, 5) = (/ &
      5.70230e-07 ,5.94788e-07 ,6.20085e-07 ,6.46130e-07 ,6.72936e-07 , &
      7.00512e-07 ,7.28869e-07 ,7.58019e-07 ,7.87971e-07 ,8.18734e-07 , &
      8.50320e-07 ,8.82738e-07 ,9.15999e-07 ,9.50110e-07 ,9.85084e-07 , &
      1.02093e-06 ,1.05765e-06 ,1.09527e-06 ,1.13378e-06 ,1.17320e-06 , &
      1.21353e-06 ,1.25479e-06 ,1.29698e-06 ,1.34011e-06 ,1.38419e-06 , &
      1.42923e-06 ,1.47523e-06 ,1.52221e-06 ,1.57016e-06 ,1.61910e-06 , &
      1.66904e-06 ,1.71997e-06 ,1.77192e-06 ,1.82488e-06 ,1.87886e-06 , &
      1.93387e-06 ,1.98991e-06 ,2.04699e-06 ,2.10512e-06 ,2.16430e-06 , &
      2.22454e-06 ,2.28584e-06 ,2.34821e-06 ,2.41166e-06 ,2.47618e-06 , &
      2.54178e-06 ,2.60847e-06 ,2.67626e-06 ,2.74514e-06 ,2.81512e-06 /)
      totplnk(51:100, 5) = (/ &
      2.88621e-06 ,2.95841e-06 ,3.03172e-06 ,3.10615e-06 ,3.18170e-06 , &
      3.25838e-06 ,3.33618e-06 ,3.41511e-06 ,3.49518e-06 ,3.57639e-06 , &
      3.65873e-06 ,3.74221e-06 ,3.82684e-06 ,3.91262e-06 ,3.99955e-06 , &
      4.08763e-06 ,4.17686e-06 ,4.26725e-06 ,4.35880e-06 ,4.45150e-06 , &
      4.54537e-06 ,4.64039e-06 ,4.73659e-06 ,4.83394e-06 ,4.93246e-06 , &
      5.03215e-06 ,5.13301e-06 ,5.23504e-06 ,5.33823e-06 ,5.44260e-06 , &
      5.54814e-06 ,5.65484e-06 ,5.76272e-06 ,5.87177e-06 ,5.98199e-06 , &
      6.09339e-06 ,6.20596e-06 ,6.31969e-06 ,6.43460e-06 ,6.55068e-06 , &
      6.66793e-06 ,6.78636e-06 ,6.90595e-06 ,7.02670e-06 ,7.14863e-06 , &
      7.27173e-06 ,7.39599e-06 ,7.52142e-06 ,7.64802e-06 ,7.77577e-06 /)
      totplnk(101:150, 5) = (/ &
      7.90469e-06 ,8.03477e-06 ,8.16601e-06 ,8.29841e-06 ,8.43198e-06 , &
      8.56669e-06 ,8.70256e-06 ,8.83957e-06 ,8.97775e-06 ,9.11706e-06 , &
      9.25753e-06 ,9.39915e-06 ,9.54190e-06 ,9.68580e-06 ,9.83085e-06 , &
      9.97704e-06 ,1.01243e-05 ,1.02728e-05 ,1.04224e-05 ,1.05731e-05 , &
      1.07249e-05 ,1.08779e-05 ,1.10320e-05 ,1.11872e-05 ,1.13435e-05 , &
      1.15009e-05 ,1.16595e-05 ,1.18191e-05 ,1.19799e-05 ,1.21418e-05 , &
      1.23048e-05 ,1.24688e-05 ,1.26340e-05 ,1.28003e-05 ,1.29676e-05 , &
      1.31361e-05 ,1.33056e-05 ,1.34762e-05 ,1.36479e-05 ,1.38207e-05 , &
      1.39945e-05 ,1.41694e-05 ,1.43454e-05 ,1.45225e-05 ,1.47006e-05 , &
      1.48797e-05 ,1.50600e-05 ,1.52413e-05 ,1.54236e-05 ,1.56070e-05 /)
      totplnk(151:181, 5) = (/ &
      1.57914e-05 ,1.59768e-05 ,1.61633e-05 ,1.63509e-05 ,1.65394e-05 , &
      1.67290e-05 ,1.69197e-05 ,1.71113e-05 ,1.73040e-05 ,1.74976e-05 , &
      1.76923e-05 ,1.78880e-05 ,1.80847e-05 ,1.82824e-05 ,1.84811e-05 , &
      1.86808e-05 ,1.88814e-05 ,1.90831e-05 ,1.92857e-05 ,1.94894e-05 , &
      1.96940e-05 ,1.98996e-05 ,2.01061e-05 ,2.03136e-05 ,2.05221e-05 , &
      2.07316e-05 ,2.09420e-05 ,2.11533e-05 ,2.13657e-05 ,2.15789e-05 , &
      2.17931e-05 /)
      totplnk(1:50, 6) = (/ &
      2.73493e-07 ,2.87408e-07 ,3.01848e-07 ,3.16825e-07 ,3.32352e-07 , &
      3.48439e-07 ,3.65100e-07 ,3.82346e-07 ,4.00189e-07 ,4.18641e-07 , &
      4.37715e-07 ,4.57422e-07 ,4.77774e-07 ,4.98784e-07 ,5.20464e-07 , &
      5.42824e-07 ,5.65879e-07 ,5.89638e-07 ,6.14115e-07 ,6.39320e-07 , &
      6.65266e-07 ,6.91965e-07 ,7.19427e-07 ,7.47666e-07 ,7.76691e-07 , &
      8.06516e-07 ,8.37151e-07 ,8.68607e-07 ,9.00896e-07 ,9.34029e-07 , &
      9.68018e-07 ,1.00287e-06 ,1.03860e-06 ,1.07522e-06 ,1.11274e-06 , &
      1.15117e-06 ,1.19052e-06 ,1.23079e-06 ,1.27201e-06 ,1.31418e-06 , &
      1.35731e-06 ,1.40141e-06 ,1.44650e-06 ,1.49257e-06 ,1.53965e-06 , &
      1.58773e-06 ,1.63684e-06 ,1.68697e-06 ,1.73815e-06 ,1.79037e-06 /)
      totplnk(51:100, 6) = (/ &
      1.84365e-06 ,1.89799e-06 ,1.95341e-06 ,2.00991e-06 ,2.06750e-06 , &
      2.12619e-06 ,2.18599e-06 ,2.24691e-06 ,2.30895e-06 ,2.37212e-06 , &
      2.43643e-06 ,2.50189e-06 ,2.56851e-06 ,2.63628e-06 ,2.70523e-06 , &
      2.77536e-06 ,2.84666e-06 ,2.91916e-06 ,2.99286e-06 ,3.06776e-06 , &
      3.14387e-06 ,3.22120e-06 ,3.29975e-06 ,3.37953e-06 ,3.46054e-06 , &
      3.54280e-06 ,3.62630e-06 ,3.71105e-06 ,3.79707e-06 ,3.88434e-06 , &
      3.97288e-06 ,4.06270e-06 ,4.15380e-06 ,4.24617e-06 ,4.33984e-06 , &
      4.43479e-06 ,4.53104e-06 ,4.62860e-06 ,4.72746e-06 ,4.82763e-06 , &
      4.92911e-06 ,5.03191e-06 ,5.13603e-06 ,5.24147e-06 ,5.34824e-06 , &
      5.45634e-06 ,5.56578e-06 ,5.67656e-06 ,5.78867e-06 ,5.90213e-06 /)
      totplnk(101:150, 6) = (/ &
      6.01694e-06 ,6.13309e-06 ,6.25060e-06 ,6.36947e-06 ,6.48968e-06 , &
      6.61126e-06 ,6.73420e-06 ,6.85850e-06 ,6.98417e-06 ,7.11120e-06 , &
      7.23961e-06 ,7.36938e-06 ,7.50053e-06 ,7.63305e-06 ,7.76694e-06 , &
      7.90221e-06 ,8.03887e-06 ,8.17690e-06 ,8.31632e-06 ,8.45710e-06 , &
      8.59928e-06 ,8.74282e-06 ,8.88776e-06 ,9.03409e-06 ,9.18179e-06 , &
      9.33088e-06 ,9.48136e-06 ,9.63323e-06 ,9.78648e-06 ,9.94111e-06 , &
      1.00971e-05 ,1.02545e-05 ,1.04133e-05 ,1.05735e-05 ,1.07351e-05 , &
      1.08980e-05 ,1.10624e-05 ,1.12281e-05 ,1.13952e-05 ,1.15637e-05 , &
      1.17335e-05 ,1.19048e-05 ,1.20774e-05 ,1.22514e-05 ,1.24268e-05 , &
      1.26036e-05 ,1.27817e-05 ,1.29612e-05 ,1.31421e-05 ,1.33244e-05 /)
      totplnk(151:181, 6) = (/ &
      1.35080e-05 ,1.36930e-05 ,1.38794e-05 ,1.40672e-05 ,1.42563e-05 , &
      1.44468e-05 ,1.46386e-05 ,1.48318e-05 ,1.50264e-05 ,1.52223e-05 , &
      1.54196e-05 ,1.56182e-05 ,1.58182e-05 ,1.60196e-05 ,1.62223e-05 , &
      1.64263e-05 ,1.66317e-05 ,1.68384e-05 ,1.70465e-05 ,1.72559e-05 , &
      1.74666e-05 ,1.76787e-05 ,1.78921e-05 ,1.81069e-05 ,1.83230e-05 , &
      1.85404e-05 ,1.87591e-05 ,1.89791e-05 ,1.92005e-05 ,1.94232e-05 , &
      1.96471e-05 /)
      totplnk(1:50, 7) = (/ &
      1.25349e-07 ,1.32735e-07 ,1.40458e-07 ,1.48527e-07 ,1.56954e-07 , &
      1.65748e-07 ,1.74920e-07 ,1.84481e-07 ,1.94443e-07 ,2.04814e-07 , &
      2.15608e-07 ,2.26835e-07 ,2.38507e-07 ,2.50634e-07 ,2.63229e-07 , &
      2.76301e-07 ,2.89864e-07 ,3.03930e-07 ,3.18508e-07 ,3.33612e-07 , &
      3.49253e-07 ,3.65443e-07 ,3.82195e-07 ,3.99519e-07 ,4.17428e-07 , &
      4.35934e-07 ,4.55050e-07 ,4.74785e-07 ,4.95155e-07 ,5.16170e-07 , &
      5.37844e-07 ,5.60186e-07 ,5.83211e-07 ,6.06929e-07 ,6.31355e-07 , &
      6.56498e-07 ,6.82373e-07 ,7.08990e-07 ,7.36362e-07 ,7.64501e-07 , &
      7.93420e-07 ,8.23130e-07 ,8.53643e-07 ,8.84971e-07 ,9.17128e-07 , &
      9.50123e-07 ,9.83969e-07 ,1.01868e-06 ,1.05426e-06 ,1.09073e-06 /)
      totplnk(51:100, 7) = (/ &
      1.12810e-06 ,1.16638e-06 ,1.20558e-06 ,1.24572e-06 ,1.28680e-06 , &
      1.32883e-06 ,1.37183e-06 ,1.41581e-06 ,1.46078e-06 ,1.50675e-06 , &
      1.55374e-06 ,1.60174e-06 ,1.65078e-06 ,1.70087e-06 ,1.75200e-06 , &
      1.80421e-06 ,1.85749e-06 ,1.91186e-06 ,1.96732e-06 ,2.02389e-06 , &
      2.08159e-06 ,2.14040e-06 ,2.20035e-06 ,2.26146e-06 ,2.32372e-06 , &
      2.38714e-06 ,2.45174e-06 ,2.51753e-06 ,2.58451e-06 ,2.65270e-06 , &
      2.72210e-06 ,2.79272e-06 ,2.86457e-06 ,2.93767e-06 ,3.01201e-06 , &
      3.08761e-06 ,3.16448e-06 ,3.24261e-06 ,3.32204e-06 ,3.40275e-06 , &
      3.48476e-06 ,3.56808e-06 ,3.65271e-06 ,3.73866e-06 ,3.82595e-06 , &
      3.91456e-06 ,4.00453e-06 ,4.09584e-06 ,4.18851e-06 ,4.28254e-06 /)
      totplnk(101:150, 7) = (/ &
      4.37796e-06 ,4.47475e-06 ,4.57293e-06 ,4.67249e-06 ,4.77346e-06 , &
      4.87583e-06 ,4.97961e-06 ,5.08481e-06 ,5.19143e-06 ,5.29948e-06 , &
      5.40896e-06 ,5.51989e-06 ,5.63226e-06 ,5.74608e-06 ,5.86136e-06 , &
      5.97810e-06 ,6.09631e-06 ,6.21597e-06 ,6.33713e-06 ,6.45976e-06 , &
      6.58388e-06 ,6.70950e-06 ,6.83661e-06 ,6.96521e-06 ,7.09531e-06 , &
      7.22692e-06 ,7.36005e-06 ,7.49468e-06 ,7.63084e-06 ,7.76851e-06 , &
      7.90773e-06 ,8.04846e-06 ,8.19072e-06 ,8.33452e-06 ,8.47985e-06 , &
      8.62674e-06 ,8.77517e-06 ,8.92514e-06 ,9.07666e-06 ,9.22975e-06 , &
      9.38437e-06 ,9.54057e-06 ,9.69832e-06 ,9.85762e-06 ,1.00185e-05 , &
      1.01810e-05 ,1.03450e-05 ,1.05106e-05 ,1.06777e-05 ,1.08465e-05 /)
      totplnk(151:181, 7) = (/ &
      1.10168e-05 ,1.11887e-05 ,1.13621e-05 ,1.15372e-05 ,1.17138e-05 , &
      1.18920e-05 ,1.20718e-05 ,1.22532e-05 ,1.24362e-05 ,1.26207e-05 , &
      1.28069e-05 ,1.29946e-05 ,1.31839e-05 ,1.33749e-05 ,1.35674e-05 , &
      1.37615e-05 ,1.39572e-05 ,1.41544e-05 ,1.43533e-05 ,1.45538e-05 , &
      1.47558e-05 ,1.49595e-05 ,1.51647e-05 ,1.53716e-05 ,1.55800e-05 , &
      1.57900e-05 ,1.60017e-05 ,1.62149e-05 ,1.64296e-05 ,1.66460e-05 , &
      1.68640e-05 /)
      totplnk(1:50, 8) = (/ &
      6.74445e-08 ,7.18176e-08 ,7.64153e-08 ,8.12456e-08 ,8.63170e-08 , &
      9.16378e-08 ,9.72168e-08 ,1.03063e-07 ,1.09184e-07 ,1.15591e-07 , &
      1.22292e-07 ,1.29296e-07 ,1.36613e-07 ,1.44253e-07 ,1.52226e-07 , &
      1.60540e-07 ,1.69207e-07 ,1.78236e-07 ,1.87637e-07 ,1.97421e-07 , &
      2.07599e-07 ,2.18181e-07 ,2.29177e-07 ,2.40598e-07 ,2.52456e-07 , &
      2.64761e-07 ,2.77523e-07 ,2.90755e-07 ,3.04468e-07 ,3.18673e-07 , &
      3.33381e-07 ,3.48603e-07 ,3.64352e-07 ,3.80638e-07 ,3.97474e-07 , &
      4.14871e-07 ,4.32841e-07 ,4.51395e-07 ,4.70547e-07 ,4.90306e-07 , &
      5.10687e-07 ,5.31699e-07 ,5.53357e-07 ,5.75670e-07 ,5.98652e-07 , &
      6.22315e-07 ,6.46672e-07 ,6.71731e-07 ,6.97511e-07 ,7.24018e-07 /)
      totplnk(51:100, 8) = (/ &
      7.51266e-07 ,7.79269e-07 ,8.08038e-07 ,8.37584e-07 ,8.67922e-07 , &
      8.99061e-07 ,9.31016e-07 ,9.63797e-07 ,9.97417e-07 ,1.03189e-06 , &
      1.06722e-06 ,1.10343e-06 ,1.14053e-06 ,1.17853e-06 ,1.21743e-06 , &
      1.25726e-06 ,1.29803e-06 ,1.33974e-06 ,1.38241e-06 ,1.42606e-06 , &
      1.47068e-06 ,1.51630e-06 ,1.56293e-06 ,1.61056e-06 ,1.65924e-06 , &
      1.70894e-06 ,1.75971e-06 ,1.81153e-06 ,1.86443e-06 ,1.91841e-06 , &
      1.97350e-06 ,2.02968e-06 ,2.08699e-06 ,2.14543e-06 ,2.20500e-06 , &
      2.26573e-06 ,2.32762e-06 ,2.39068e-06 ,2.45492e-06 ,2.52036e-06 , &
      2.58700e-06 ,2.65485e-06 ,2.72393e-06 ,2.79424e-06 ,2.86580e-06 , &
      2.93861e-06 ,3.01269e-06 ,3.08803e-06 ,3.16467e-06 ,3.24259e-06 /)
      totplnk(101:150, 8) = (/ &
      3.32181e-06 ,3.40235e-06 ,3.48420e-06 ,3.56739e-06 ,3.65192e-06 , &
      3.73779e-06 ,3.82502e-06 ,3.91362e-06 ,4.00359e-06 ,4.09494e-06 , &
      4.18768e-06 ,4.28182e-06 ,4.37737e-06 ,4.47434e-06 ,4.57273e-06 , &
      4.67254e-06 ,4.77380e-06 ,4.87651e-06 ,4.98067e-06 ,5.08630e-06 , &
      5.19339e-06 ,5.30196e-06 ,5.41201e-06 ,5.52356e-06 ,5.63660e-06 , &
      5.75116e-06 ,5.86722e-06 ,5.98479e-06 ,6.10390e-06 ,6.22453e-06 , &
      6.34669e-06 ,6.47042e-06 ,6.59569e-06 ,6.72252e-06 ,6.85090e-06 , &
      6.98085e-06 ,7.11238e-06 ,7.24549e-06 ,7.38019e-06 ,7.51646e-06 , &
      7.65434e-06 ,7.79382e-06 ,7.93490e-06 ,8.07760e-06 ,8.22192e-06 , &
      8.36784e-06 ,8.51540e-06 ,8.66459e-06 ,8.81542e-06 ,8.96786e-06 /)
      totplnk(151:181, 8) = (/ &
      9.12197e-06 ,9.27772e-06 ,9.43513e-06 ,9.59419e-06 ,9.75490e-06 , &
      9.91728e-06 ,1.00813e-05 ,1.02471e-05 ,1.04144e-05 ,1.05835e-05 , &
      1.07543e-05 ,1.09267e-05 ,1.11008e-05 ,1.12766e-05 ,1.14541e-05 , &
      1.16333e-05 ,1.18142e-05 ,1.19969e-05 ,1.21812e-05 ,1.23672e-05 , &
      1.25549e-05 ,1.27443e-05 ,1.29355e-05 ,1.31284e-05 ,1.33229e-05 , &
      1.35193e-05 ,1.37173e-05 ,1.39170e-05 ,1.41185e-05 ,1.43217e-05 , &
      1.45267e-05 /)
      totplnk(1:50, 9) = (/ &
      2.61522e-08 ,2.80613e-08 ,3.00838e-08 ,3.22250e-08 ,3.44899e-08 , &
      3.68841e-08 ,3.94129e-08 ,4.20820e-08 ,4.48973e-08 ,4.78646e-08 , &
      5.09901e-08 ,5.42799e-08 ,5.77405e-08 ,6.13784e-08 ,6.52001e-08 , &
      6.92126e-08 ,7.34227e-08 ,7.78375e-08 ,8.24643e-08 ,8.73103e-08 , &
      9.23832e-08 ,9.76905e-08 ,1.03240e-07 ,1.09039e-07 ,1.15097e-07 , &
      1.21421e-07 ,1.28020e-07 ,1.34902e-07 ,1.42075e-07 ,1.49548e-07 , &
      1.57331e-07 ,1.65432e-07 ,1.73860e-07 ,1.82624e-07 ,1.91734e-07 , &
      2.01198e-07 ,2.11028e-07 ,2.21231e-07 ,2.31818e-07 ,2.42799e-07 , &
      2.54184e-07 ,2.65983e-07 ,2.78205e-07 ,2.90862e-07 ,3.03963e-07 , &
      3.17519e-07 ,3.31541e-07 ,3.46039e-07 ,3.61024e-07 ,3.76507e-07 /)
      totplnk(51:100, 9) = (/ &
      3.92498e-07 ,4.09008e-07 ,4.26050e-07 ,4.43633e-07 ,4.61769e-07 , &
      4.80469e-07 ,4.99744e-07 ,5.19606e-07 ,5.40067e-07 ,5.61136e-07 , &
      5.82828e-07 ,6.05152e-07 ,6.28120e-07 ,6.51745e-07 ,6.76038e-07 , &
      7.01010e-07 ,7.26674e-07 ,7.53041e-07 ,7.80124e-07 ,8.07933e-07 , &
      8.36482e-07 ,8.65781e-07 ,8.95845e-07 ,9.26683e-07 ,9.58308e-07 , &
      9.90732e-07 ,1.02397e-06 ,1.05803e-06 ,1.09292e-06 ,1.12866e-06 , &
      1.16526e-06 ,1.20274e-06 ,1.24109e-06 ,1.28034e-06 ,1.32050e-06 , &
      1.36158e-06 ,1.40359e-06 ,1.44655e-06 ,1.49046e-06 ,1.53534e-06 , &
      1.58120e-06 ,1.62805e-06 ,1.67591e-06 ,1.72478e-06 ,1.77468e-06 , &
      1.82561e-06 ,1.87760e-06 ,1.93066e-06 ,1.98479e-06 ,2.04000e-06 /)
      totplnk(101:150, 9) = (/ &
      2.09631e-06 ,2.15373e-06 ,2.21228e-06 ,2.27196e-06 ,2.33278e-06 , &
      2.39475e-06 ,2.45790e-06 ,2.52222e-06 ,2.58773e-06 ,2.65445e-06 , &
      2.72238e-06 ,2.79152e-06 ,2.86191e-06 ,2.93354e-06 ,3.00643e-06 , &
      3.08058e-06 ,3.15601e-06 ,3.23273e-06 ,3.31075e-06 ,3.39009e-06 , &
      3.47074e-06 ,3.55272e-06 ,3.63605e-06 ,3.72072e-06 ,3.80676e-06 , &
      3.89417e-06 ,3.98297e-06 ,4.07315e-06 ,4.16474e-06 ,4.25774e-06 , &
      4.35217e-06 ,4.44802e-06 ,4.54532e-06 ,4.64406e-06 ,4.74428e-06 , &
      4.84595e-06 ,4.94911e-06 ,5.05376e-06 ,5.15990e-06 ,5.26755e-06 , &
      5.37671e-06 ,5.48741e-06 ,5.59963e-06 ,5.71340e-06 ,5.82871e-06 , &
      5.94559e-06 ,6.06403e-06 ,6.18404e-06 ,6.30565e-06 ,6.42885e-06 /)
      totplnk(151:181, 9) = (/ &
      6.55364e-06 ,6.68004e-06 ,6.80806e-06 ,6.93771e-06 ,7.06898e-06 , &
      7.20190e-06 ,7.33646e-06 ,7.47267e-06 ,7.61056e-06 ,7.75010e-06 , &
      7.89133e-06 ,8.03423e-06 ,8.17884e-06 ,8.32514e-06 ,8.47314e-06 , &
      8.62284e-06 ,8.77427e-06 ,8.92743e-06 ,9.08231e-06 ,9.23893e-06 , &
      9.39729e-06 ,9.55741e-06 ,9.71927e-06 ,9.88291e-06 ,1.00483e-05 , &
      1.02155e-05 ,1.03844e-05 ,1.05552e-05 ,1.07277e-05 ,1.09020e-05 , &
      1.10781e-05 /)
      totplnk(1:50,10) = (/ &
      8.89300e-09 ,9.63263e-09 ,1.04235e-08 ,1.12685e-08 ,1.21703e-08 , &
      1.31321e-08 ,1.41570e-08 ,1.52482e-08 ,1.64090e-08 ,1.76428e-08 , &
      1.89533e-08 ,2.03441e-08 ,2.18190e-08 ,2.33820e-08 ,2.50370e-08 , &
      2.67884e-08 ,2.86402e-08 ,3.05969e-08 ,3.26632e-08 ,3.48436e-08 , &
      3.71429e-08 ,3.95660e-08 ,4.21179e-08 ,4.48040e-08 ,4.76294e-08 , &
      5.05996e-08 ,5.37201e-08 ,5.69966e-08 ,6.04349e-08 ,6.40411e-08 , &
      6.78211e-08 ,7.17812e-08 ,7.59276e-08 ,8.02670e-08 ,8.48059e-08 , &
      8.95508e-08 ,9.45090e-08 ,9.96873e-08 ,1.05093e-07 ,1.10733e-07 , &
      1.16614e-07 ,1.22745e-07 ,1.29133e-07 ,1.35786e-07 ,1.42711e-07 , &
      1.49916e-07 ,1.57410e-07 ,1.65202e-07 ,1.73298e-07 ,1.81709e-07 /)
      totplnk(51:100,10) = (/ &
      1.90441e-07 ,1.99505e-07 ,2.08908e-07 ,2.18660e-07 ,2.28770e-07 , &
      2.39247e-07 ,2.50101e-07 ,2.61340e-07 ,2.72974e-07 ,2.85013e-07 , &
      2.97467e-07 ,3.10345e-07 ,3.23657e-07 ,3.37413e-07 ,3.51623e-07 , &
      3.66298e-07 ,3.81448e-07 ,3.97082e-07 ,4.13212e-07 ,4.29848e-07 , &
      4.47000e-07 ,4.64680e-07 ,4.82898e-07 ,5.01664e-07 ,5.20991e-07 , &
      5.40888e-07 ,5.61369e-07 ,5.82440e-07 ,6.04118e-07 ,6.26410e-07 , &
      6.49329e-07 ,6.72887e-07 ,6.97095e-07 ,7.21964e-07 ,7.47506e-07 , &
      7.73732e-07 ,8.00655e-07 ,8.28287e-07 ,8.56635e-07 ,8.85717e-07 , &
      9.15542e-07 ,9.46122e-07 ,9.77469e-07 ,1.00960e-06 ,1.04251e-06 , &
      1.07623e-06 ,1.11077e-06 ,1.14613e-06 ,1.18233e-06 ,1.21939e-06 /)
      totplnk(101:150,10) = (/ &
      1.25730e-06 ,1.29610e-06 ,1.33578e-06 ,1.37636e-06 ,1.41785e-06 , &
      1.46027e-06 ,1.50362e-06 ,1.54792e-06 ,1.59319e-06 ,1.63942e-06 , &
      1.68665e-06 ,1.73487e-06 ,1.78410e-06 ,1.83435e-06 ,1.88564e-06 , &
      1.93797e-06 ,1.99136e-06 ,2.04582e-06 ,2.10137e-06 ,2.15801e-06 , &
      2.21576e-06 ,2.27463e-06 ,2.33462e-06 ,2.39577e-06 ,2.45806e-06 , &
      2.52153e-06 ,2.58617e-06 ,2.65201e-06 ,2.71905e-06 ,2.78730e-06 , &
      2.85678e-06 ,2.92749e-06 ,2.99946e-06 ,3.07269e-06 ,3.14720e-06 , &
      3.22299e-06 ,3.30007e-06 ,3.37847e-06 ,3.45818e-06 ,3.53923e-06 , &
      3.62161e-06 ,3.70535e-06 ,3.79046e-06 ,3.87695e-06 ,3.96481e-06 , &
      4.05409e-06 ,4.14477e-06 ,4.23687e-06 ,4.33040e-06 ,4.42538e-06 /)
      totplnk(151:181,10) = (/ &
      4.52180e-06 ,4.61969e-06 ,4.71905e-06 ,4.81991e-06 ,4.92226e-06 , &
      5.02611e-06 ,5.13148e-06 ,5.23839e-06 ,5.34681e-06 ,5.45681e-06 , &
      5.56835e-06 ,5.68146e-06 ,5.79614e-06 ,5.91242e-06 ,6.03030e-06 , &
      6.14978e-06 ,6.27088e-06 ,6.39360e-06 ,6.51798e-06 ,6.64398e-06 , &
      6.77165e-06 ,6.90099e-06 ,7.03198e-06 ,7.16468e-06 ,7.29906e-06 , &
      7.43514e-06 ,7.57294e-06 ,7.71244e-06 ,7.85369e-06 ,7.99666e-06 , &
      8.14138e-06 /)
      totplnk(1:50,11) = (/ &
      2.53767e-09 ,2.77242e-09 ,3.02564e-09 ,3.29851e-09 ,3.59228e-09 , &
      3.90825e-09 ,4.24777e-09 ,4.61227e-09 ,5.00322e-09 ,5.42219e-09 , &
      5.87080e-09 ,6.35072e-09 ,6.86370e-09 ,7.41159e-09 ,7.99628e-09 , &
      8.61974e-09 ,9.28404e-09 ,9.99130e-09 ,1.07437e-08 ,1.15436e-08 , &
      1.23933e-08 ,1.32953e-08 ,1.42522e-08 ,1.52665e-08 ,1.63410e-08 , &
      1.74786e-08 ,1.86820e-08 ,1.99542e-08 ,2.12985e-08 ,2.27179e-08 , &
      2.42158e-08 ,2.57954e-08 ,2.74604e-08 ,2.92141e-08 ,3.10604e-08 , &
      3.30029e-08 ,3.50457e-08 ,3.71925e-08 ,3.94476e-08 ,4.18149e-08 , &
      4.42991e-08 ,4.69043e-08 ,4.96352e-08 ,5.24961e-08 ,5.54921e-08 , &
      5.86277e-08 ,6.19081e-08 ,6.53381e-08 ,6.89231e-08 ,7.26681e-08 /)
      totplnk(51:100,11) = (/ &
      7.65788e-08 ,8.06604e-08 ,8.49187e-08 ,8.93591e-08 ,9.39879e-08 , &
      9.88106e-08 ,1.03834e-07 ,1.09063e-07 ,1.14504e-07 ,1.20165e-07 , &
      1.26051e-07 ,1.32169e-07 ,1.38525e-07 ,1.45128e-07 ,1.51982e-07 , &
      1.59096e-07 ,1.66477e-07 ,1.74132e-07 ,1.82068e-07 ,1.90292e-07 , &
      1.98813e-07 ,2.07638e-07 ,2.16775e-07 ,2.26231e-07 ,2.36015e-07 , &
      2.46135e-07 ,2.56599e-07 ,2.67415e-07 ,2.78592e-07 ,2.90137e-07 , &
      3.02061e-07 ,3.14371e-07 ,3.27077e-07 ,3.40186e-07 ,3.53710e-07 , &
      3.67655e-07 ,3.82031e-07 ,3.96848e-07 ,4.12116e-07 ,4.27842e-07 , &
      4.44039e-07 ,4.60713e-07 ,4.77876e-07 ,4.95537e-07 ,5.13706e-07 , &
      5.32392e-07 ,5.51608e-07 ,5.71360e-07 ,5.91662e-07 ,6.12521e-07 /)
      totplnk(101:150,11) = (/ &
      6.33950e-07 ,6.55958e-07 ,6.78556e-07 ,7.01753e-07 ,7.25562e-07 , &
      7.49992e-07 ,7.75055e-07 ,8.00760e-07 ,8.27120e-07 ,8.54145e-07 , &
      8.81845e-07 ,9.10233e-07 ,9.39318e-07 ,9.69113e-07 ,9.99627e-07 , &
      1.03087e-06 ,1.06286e-06 ,1.09561e-06 ,1.12912e-06 ,1.16340e-06 , &
      1.19848e-06 ,1.23435e-06 ,1.27104e-06 ,1.30855e-06 ,1.34690e-06 , &
      1.38609e-06 ,1.42614e-06 ,1.46706e-06 ,1.50886e-06 ,1.55155e-06 , &
      1.59515e-06 ,1.63967e-06 ,1.68512e-06 ,1.73150e-06 ,1.77884e-06 , &
      1.82715e-06 ,1.87643e-06 ,1.92670e-06 ,1.97797e-06 ,2.03026e-06 , &
      2.08356e-06 ,2.13791e-06 ,2.19330e-06 ,2.24975e-06 ,2.30728e-06 , &
      2.36589e-06 ,2.42560e-06 ,2.48641e-06 ,2.54835e-06 ,2.61142e-06 /)
      totplnk(151:181,11) = (/ &
      2.67563e-06 ,2.74100e-06 ,2.80754e-06 ,2.87526e-06 ,2.94417e-06 , &
      3.01429e-06 ,3.08562e-06 ,3.15819e-06 ,3.23199e-06 ,3.30704e-06 , &
      3.38336e-06 ,3.46096e-06 ,3.53984e-06 ,3.62002e-06 ,3.70151e-06 , &
      3.78433e-06 ,3.86848e-06 ,3.95399e-06 ,4.04084e-06 ,4.12907e-06 , &
      4.21868e-06 ,4.30968e-06 ,4.40209e-06 ,4.49592e-06 ,4.59117e-06 , &
      4.68786e-06 ,4.78600e-06 ,4.88561e-06 ,4.98669e-06 ,5.08926e-06 , &
      5.19332e-06 /)
      totplnk(1:50,12) = (/ &
      2.73921e-10 ,3.04500e-10 ,3.38056e-10 ,3.74835e-10 ,4.15099e-10 , &
      4.59126e-10 ,5.07214e-10 ,5.59679e-10 ,6.16857e-10 ,6.79103e-10 , &
      7.46796e-10 ,8.20335e-10 ,9.00144e-10 ,9.86671e-10 ,1.08039e-09 , &
      1.18180e-09 ,1.29142e-09 ,1.40982e-09 ,1.53757e-09 ,1.67529e-09 , &
      1.82363e-09 ,1.98327e-09 ,2.15492e-09 ,2.33932e-09 ,2.53726e-09 , &
      2.74957e-09 ,2.97710e-09 ,3.22075e-09 ,3.48145e-09 ,3.76020e-09 , &
      4.05801e-09 ,4.37595e-09 ,4.71513e-09 ,5.07672e-09 ,5.46193e-09 , &
      5.87201e-09 ,6.30827e-09 ,6.77205e-09 ,7.26480e-09 ,7.78794e-09 , &
      8.34304e-09 ,8.93163e-09 ,9.55537e-09 ,1.02159e-08 ,1.09151e-08 , &
      1.16547e-08 ,1.24365e-08 ,1.32625e-08 ,1.41348e-08 ,1.50554e-08 /)
      totplnk(51:100,12) = (/ &
      1.60264e-08 ,1.70500e-08 ,1.81285e-08 ,1.92642e-08 ,2.04596e-08 , &
      2.17171e-08 ,2.30394e-08 ,2.44289e-08 ,2.58885e-08 ,2.74209e-08 , &
      2.90290e-08 ,3.07157e-08 ,3.24841e-08 ,3.43371e-08 ,3.62782e-08 , &
      3.83103e-08 ,4.04371e-08 ,4.26617e-08 ,4.49878e-08 ,4.74190e-08 , &
      4.99589e-08 ,5.26113e-08 ,5.53801e-08 ,5.82692e-08 ,6.12826e-08 , &
      6.44245e-08 ,6.76991e-08 ,7.11105e-08 ,7.46634e-08 ,7.83621e-08 , &
      8.22112e-08 ,8.62154e-08 ,9.03795e-08 ,9.47081e-08 ,9.92066e-08 , &
      1.03879e-07 ,1.08732e-07 ,1.13770e-07 ,1.18998e-07 ,1.24422e-07 , &
      1.30048e-07 ,1.35880e-07 ,1.41924e-07 ,1.48187e-07 ,1.54675e-07 , &
      1.61392e-07 ,1.68346e-07 ,1.75543e-07 ,1.82988e-07 ,1.90688e-07 /)
      totplnk(101:150,12) = (/ &
      1.98650e-07 ,2.06880e-07 ,2.15385e-07 ,2.24172e-07 ,2.33247e-07 , &
      2.42617e-07 ,2.52289e-07 ,2.62272e-07 ,2.72571e-07 ,2.83193e-07 , &
      2.94147e-07 ,3.05440e-07 ,3.17080e-07 ,3.29074e-07 ,3.41430e-07 , &
      3.54155e-07 ,3.67259e-07 ,3.80747e-07 ,3.94631e-07 ,4.08916e-07 , &
      4.23611e-07 ,4.38725e-07 ,4.54267e-07 ,4.70245e-07 ,4.86666e-07 , &
      5.03541e-07 ,5.20879e-07 ,5.38687e-07 ,5.56975e-07 ,5.75751e-07 , &
      5.95026e-07 ,6.14808e-07 ,6.35107e-07 ,6.55932e-07 ,6.77293e-07 , &
      6.99197e-07 ,7.21656e-07 ,7.44681e-07 ,7.68278e-07 ,7.92460e-07 , &
      8.17235e-07 ,8.42614e-07 ,8.68606e-07 ,8.95223e-07 ,9.22473e-07 , &
      9.50366e-07 ,9.78915e-07 ,1.00813e-06 ,1.03802e-06 ,1.06859e-06 /)
      totplnk(151:181,12) = (/ &
      1.09986e-06 ,1.13184e-06 ,1.16453e-06 ,1.19796e-06 ,1.23212e-06 , &
      1.26703e-06 ,1.30270e-06 ,1.33915e-06 ,1.37637e-06 ,1.41440e-06 , &
      1.45322e-06 ,1.49286e-06 ,1.53333e-06 ,1.57464e-06 ,1.61679e-06 , &
      1.65981e-06 ,1.70370e-06 ,1.74847e-06 ,1.79414e-06 ,1.84071e-06 , &
      1.88821e-06 ,1.93663e-06 ,1.98599e-06 ,2.03631e-06 ,2.08759e-06 , &
      2.13985e-06 ,2.19310e-06 ,2.24734e-06 ,2.30260e-06 ,2.35888e-06 , &
      2.41619e-06 /)
      totplnk(1:50,13) = (/ &
      4.53634e-11 ,5.11435e-11 ,5.75754e-11 ,6.47222e-11 ,7.26531e-11 , &
      8.14420e-11 ,9.11690e-11 ,1.01921e-10 ,1.13790e-10 ,1.26877e-10 , &
      1.41288e-10 ,1.57140e-10 ,1.74555e-10 ,1.93665e-10 ,2.14613e-10 , &
      2.37548e-10 ,2.62633e-10 ,2.90039e-10 ,3.19948e-10 ,3.52558e-10 , &
      3.88073e-10 ,4.26716e-10 ,4.68719e-10 ,5.14331e-10 ,5.63815e-10 , &
      6.17448e-10 ,6.75526e-10 ,7.38358e-10 ,8.06277e-10 ,8.79625e-10 , &
      9.58770e-10 ,1.04410e-09 ,1.13602e-09 ,1.23495e-09 ,1.34135e-09 , &
      1.45568e-09 ,1.57845e-09 ,1.71017e-09 ,1.85139e-09 ,2.00268e-09 , &
      2.16464e-09 ,2.33789e-09 ,2.52309e-09 ,2.72093e-09 ,2.93212e-09 , &
      3.15740e-09 ,3.39757e-09 ,3.65341e-09 ,3.92579e-09 ,4.21559e-09 /)
      totplnk(51:100,13) = (/ &
      4.52372e-09 ,4.85115e-09 ,5.19886e-09 ,5.56788e-09 ,5.95928e-09 , &
      6.37419e-09 ,6.81375e-09 ,7.27917e-09 ,7.77168e-09 ,8.29256e-09 , &
      8.84317e-09 ,9.42487e-09 ,1.00391e-08 ,1.06873e-08 ,1.13710e-08 , &
      1.20919e-08 ,1.28515e-08 ,1.36514e-08 ,1.44935e-08 ,1.53796e-08 , &
      1.63114e-08 ,1.72909e-08 ,1.83201e-08 ,1.94008e-08 ,2.05354e-08 , &
      2.17258e-08 ,2.29742e-08 ,2.42830e-08 ,2.56545e-08 ,2.70910e-08 , &
      2.85950e-08 ,3.01689e-08 ,3.18155e-08 ,3.35373e-08 ,3.53372e-08 , &
      3.72177e-08 ,3.91818e-08 ,4.12325e-08 ,4.33727e-08 ,4.56056e-08 , &
      4.79342e-08 ,5.03617e-08 ,5.28915e-08 ,5.55270e-08 ,5.82715e-08 , &
      6.11286e-08 ,6.41019e-08 ,6.71951e-08 ,7.04119e-08 ,7.37560e-08 /)
      totplnk(101:150,13) = (/ &
      7.72315e-08 ,8.08424e-08 ,8.45927e-08 ,8.84866e-08 ,9.25281e-08 , &
      9.67218e-08 ,1.01072e-07 ,1.05583e-07 ,1.10260e-07 ,1.15107e-07 , &
      1.20128e-07 ,1.25330e-07 ,1.30716e-07 ,1.36291e-07 ,1.42061e-07 , &
      1.48031e-07 ,1.54206e-07 ,1.60592e-07 ,1.67192e-07 ,1.74015e-07 , &
      1.81064e-07 ,1.88345e-07 ,1.95865e-07 ,2.03628e-07 ,2.11643e-07 , &
      2.19912e-07 ,2.28443e-07 ,2.37244e-07 ,2.46318e-07 ,2.55673e-07 , &
      2.65316e-07 ,2.75252e-07 ,2.85489e-07 ,2.96033e-07 ,3.06891e-07 , &
      3.18070e-07 ,3.29576e-07 ,3.41417e-07 ,3.53600e-07 ,3.66133e-07 , &
      3.79021e-07 ,3.92274e-07 ,4.05897e-07 ,4.19899e-07 ,4.34288e-07 , &
      4.49071e-07 ,4.64255e-07 ,4.79850e-07 ,4.95863e-07 ,5.12300e-07 /)
      totplnk(151:181,13) = (/ &
      5.29172e-07 ,5.46486e-07 ,5.64250e-07 ,5.82473e-07 ,6.01164e-07 , &
      6.20329e-07 ,6.39979e-07 ,6.60122e-07 ,6.80767e-07 ,7.01922e-07 , &
      7.23596e-07 ,7.45800e-07 ,7.68539e-07 ,7.91826e-07 ,8.15669e-07 , &
      8.40076e-07 ,8.65058e-07 ,8.90623e-07 ,9.16783e-07 ,9.43544e-07 , &
      9.70917e-07 ,9.98912e-07 ,1.02754e-06 ,1.05681e-06 ,1.08673e-06 , &
      1.11731e-06 ,1.14856e-06 ,1.18050e-06 ,1.21312e-06 ,1.24645e-06 , &
      1.28049e-06 /)
      totplnk(1:50,14) = (/ &
      1.40113e-11 ,1.59358e-11 ,1.80960e-11 ,2.05171e-11 ,2.32266e-11 , &
      2.62546e-11 ,2.96335e-11 ,3.33990e-11 ,3.75896e-11 ,4.22469e-11 , &
      4.74164e-11 ,5.31466e-11 ,5.94905e-11 ,6.65054e-11 ,7.42522e-11 , &
      8.27975e-11 ,9.22122e-11 ,1.02573e-10 ,1.13961e-10 ,1.26466e-10 , &
      1.40181e-10 ,1.55206e-10 ,1.71651e-10 ,1.89630e-10 ,2.09265e-10 , &
      2.30689e-10 ,2.54040e-10 ,2.79467e-10 ,3.07128e-10 ,3.37190e-10 , &
      3.69833e-10 ,4.05243e-10 ,4.43623e-10 ,4.85183e-10 ,5.30149e-10 , &
      5.78755e-10 ,6.31255e-10 ,6.87910e-10 ,7.49002e-10 ,8.14824e-10 , &
      8.85687e-10 ,9.61914e-10 ,1.04385e-09 ,1.13186e-09 ,1.22631e-09 , &
      1.32761e-09 ,1.43617e-09 ,1.55243e-09 ,1.67686e-09 ,1.80992e-09 /)
      totplnk(51:100,14) = (/ &
      1.95212e-09 ,2.10399e-09 ,2.26607e-09 ,2.43895e-09 ,2.62321e-09 , &
      2.81949e-09 ,3.02844e-09 ,3.25073e-09 ,3.48707e-09 ,3.73820e-09 , &
      4.00490e-09 ,4.28794e-09 ,4.58819e-09 ,4.90647e-09 ,5.24371e-09 , &
      5.60081e-09 ,5.97875e-09 ,6.37854e-09 ,6.80120e-09 ,7.24782e-09 , &
      7.71950e-09 ,8.21740e-09 ,8.74271e-09 ,9.29666e-09 ,9.88054e-09 , &
      1.04956e-08 ,1.11434e-08 ,1.18251e-08 ,1.25422e-08 ,1.32964e-08 , &
      1.40890e-08 ,1.49217e-08 ,1.57961e-08 ,1.67140e-08 ,1.76771e-08 , &
      1.86870e-08 ,1.97458e-08 ,2.08553e-08 ,2.20175e-08 ,2.32342e-08 , &
      2.45077e-08 ,2.58401e-08 ,2.72334e-08 ,2.86900e-08 ,3.02122e-08 , &
      3.18021e-08 ,3.34624e-08 ,3.51954e-08 ,3.70037e-08 ,3.88899e-08 /)
      totplnk(101:150,14) = (/ &
      4.08568e-08 ,4.29068e-08 ,4.50429e-08 ,4.72678e-08 ,4.95847e-08 , &
      5.19963e-08 ,5.45058e-08 ,5.71161e-08 ,5.98309e-08 ,6.26529e-08 , &
      6.55857e-08 ,6.86327e-08 ,7.17971e-08 ,7.50829e-08 ,7.84933e-08 , &
      8.20323e-08 ,8.57035e-08 ,8.95105e-08 ,9.34579e-08 ,9.75488e-08 , &
      1.01788e-07 ,1.06179e-07 ,1.10727e-07 ,1.15434e-07 ,1.20307e-07 , &
      1.25350e-07 ,1.30566e-07 ,1.35961e-07 ,1.41539e-07 ,1.47304e-07 , &
      1.53263e-07 ,1.59419e-07 ,1.65778e-07 ,1.72345e-07 ,1.79124e-07 , &
      1.86122e-07 ,1.93343e-07 ,2.00792e-07 ,2.08476e-07 ,2.16400e-07 , &
      2.24568e-07 ,2.32988e-07 ,2.41666e-07 ,2.50605e-07 ,2.59813e-07 , &
      2.69297e-07 ,2.79060e-07 ,2.89111e-07 ,2.99455e-07 ,3.10099e-07 /)
      totplnk(151:181,14) = (/ &
      3.21049e-07 ,3.32311e-07 ,3.43893e-07 ,3.55801e-07 ,3.68041e-07 , &
      3.80621e-07 ,3.93547e-07 ,4.06826e-07 ,4.20465e-07 ,4.34473e-07 , &
      4.48856e-07 ,4.63620e-07 ,4.78774e-07 ,4.94325e-07 ,5.10280e-07 , &
      5.26648e-07 ,5.43436e-07 ,5.60652e-07 ,5.78302e-07 ,5.96397e-07 , &
      6.14943e-07 ,6.33949e-07 ,6.53421e-07 ,6.73370e-07 ,6.93803e-07 , &
      7.14731e-07 ,7.36157e-07 ,7.58095e-07 ,7.80549e-07 ,8.03533e-07 , &
      8.27050e-07 /)
      totplnk(1:50,15) = (/ &
      3.90483e-12 ,4.47999e-12 ,5.13122e-12 ,5.86739e-12 ,6.69829e-12 , &
      7.63467e-12 ,8.68833e-12 ,9.87221e-12 ,1.12005e-11 ,1.26885e-11 , &
      1.43534e-11 ,1.62134e-11 ,1.82888e-11 ,2.06012e-11 ,2.31745e-11 , &
      2.60343e-11 ,2.92087e-11 ,3.27277e-11 ,3.66242e-11 ,4.09334e-11 , &
      4.56935e-11 ,5.09455e-11 ,5.67338e-11 ,6.31057e-11 ,7.01127e-11 , &
      7.78096e-11 ,8.62554e-11 ,9.55130e-11 ,1.05651e-10 ,1.16740e-10 , &
      1.28858e-10 ,1.42089e-10 ,1.56519e-10 ,1.72243e-10 ,1.89361e-10 , &
      2.07978e-10 ,2.28209e-10 ,2.50173e-10 ,2.73999e-10 ,2.99820e-10 , &
      3.27782e-10 ,3.58034e-10 ,3.90739e-10 ,4.26067e-10 ,4.64196e-10 , &
      5.05317e-10 ,5.49631e-10 ,5.97347e-10 ,6.48689e-10 ,7.03891e-10 /)
      totplnk(51:100,15) = (/ &
      7.63201e-10 ,8.26876e-10 ,8.95192e-10 ,9.68430e-10 ,1.04690e-09 , &
      1.13091e-09 ,1.22079e-09 ,1.31689e-09 ,1.41957e-09 ,1.52922e-09 , &
      1.64623e-09 ,1.77101e-09 ,1.90401e-09 ,2.04567e-09 ,2.19647e-09 , &
      2.35690e-09 ,2.52749e-09 ,2.70875e-09 ,2.90127e-09 ,3.10560e-09 , &
      3.32238e-09 ,3.55222e-09 ,3.79578e-09 ,4.05375e-09 ,4.32682e-09 , &
      4.61574e-09 ,4.92128e-09 ,5.24420e-09 ,5.58536e-09 ,5.94558e-09 , &
      6.32575e-09 ,6.72678e-09 ,7.14964e-09 ,7.59526e-09 ,8.06470e-09 , &
      8.55897e-09 ,9.07916e-09 ,9.62638e-09 ,1.02018e-08 ,1.08066e-08 , &
      1.14420e-08 ,1.21092e-08 ,1.28097e-08 ,1.35446e-08 ,1.43155e-08 , &
      1.51237e-08 ,1.59708e-08 ,1.68581e-08 ,1.77873e-08 ,1.87599e-08 /)
      totplnk(101:150,15) = (/ &
      1.97777e-08 ,2.08423e-08 ,2.19555e-08 ,2.31190e-08 ,2.43348e-08 , &
      2.56045e-08 ,2.69302e-08 ,2.83140e-08 ,2.97578e-08 ,3.12636e-08 , &
      3.28337e-08 ,3.44702e-08 ,3.61755e-08 ,3.79516e-08 ,3.98012e-08 , &
      4.17265e-08 ,4.37300e-08 ,4.58143e-08 ,4.79819e-08 ,5.02355e-08 , &
      5.25777e-08 ,5.50114e-08 ,5.75393e-08 ,6.01644e-08 ,6.28896e-08 , &
      6.57177e-08 ,6.86521e-08 ,7.16959e-08 ,7.48520e-08 ,7.81239e-08 , &
      8.15148e-08 ,8.50282e-08 ,8.86675e-08 ,9.24362e-08 ,9.63380e-08 , &
      1.00376e-07 ,1.04555e-07 ,1.08878e-07 ,1.13349e-07 ,1.17972e-07 , &
      1.22751e-07 ,1.27690e-07 ,1.32793e-07 ,1.38064e-07 ,1.43508e-07 , &
      1.49129e-07 ,1.54931e-07 ,1.60920e-07 ,1.67099e-07 ,1.73473e-07 /)
      totplnk(151:181,15) = (/ &
      1.80046e-07 ,1.86825e-07 ,1.93812e-07 ,2.01014e-07 ,2.08436e-07 , &
      2.16082e-07 ,2.23957e-07 ,2.32067e-07 ,2.40418e-07 ,2.49013e-07 , &
      2.57860e-07 ,2.66963e-07 ,2.76328e-07 ,2.85961e-07 ,2.95868e-07 , &
      3.06053e-07 ,3.16524e-07 ,3.27286e-07 ,3.38345e-07 ,3.49707e-07 , &
      3.61379e-07 ,3.73367e-07 ,3.85676e-07 ,3.98315e-07 ,4.11287e-07 , &
      4.24602e-07 ,4.38265e-07 ,4.52283e-07 ,4.66662e-07 ,4.81410e-07 , &
      4.96535e-07 /)
      totplnk(1:50,16) = (/ &
      0.28639e-12 ,0.33349e-12 ,0.38764e-12 ,0.44977e-12 ,0.52093e-12 , &
      0.60231e-12 ,0.69522e-12 ,0.80111e-12 ,0.92163e-12 ,0.10586e-11 , &
      0.12139e-11 ,0.13899e-11 ,0.15890e-11 ,0.18138e-11 ,0.20674e-11 , &
      0.23531e-11 ,0.26744e-11 ,0.30352e-11 ,0.34401e-11 ,0.38936e-11 , &
      0.44011e-11 ,0.49681e-11 ,0.56010e-11 ,0.63065e-11 ,0.70919e-11 , &
      0.79654e-11 ,0.89357e-11 ,0.10012e-10 ,0.11205e-10 ,0.12526e-10 , &
      0.13986e-10 ,0.15600e-10 ,0.17380e-10 ,0.19342e-10 ,0.21503e-10 , &
      0.23881e-10 ,0.26494e-10 ,0.29362e-10 ,0.32509e-10 ,0.35958e-10 , &
      0.39733e-10 ,0.43863e-10 ,0.48376e-10 ,0.53303e-10 ,0.58679e-10 , &
      0.64539e-10 ,0.70920e-10 ,0.77864e-10 ,0.85413e-10 ,0.93615e-10 /)
      totplnk(51:100,16) = (/ &
      0.10252e-09 ,0.11217e-09 ,0.12264e-09 ,0.13397e-09 ,0.14624e-09 , &
      0.15950e-09 ,0.17383e-09 ,0.18930e-09 ,0.20599e-09 ,0.22399e-09 , &
      0.24339e-09 ,0.26427e-09 ,0.28674e-09 ,0.31090e-09 ,0.33686e-09 , &
      0.36474e-09 ,0.39466e-09 ,0.42676e-09 ,0.46115e-09 ,0.49800e-09 , &
      0.53744e-09 ,0.57964e-09 ,0.62476e-09 ,0.67298e-09 ,0.72448e-09 , &
      0.77945e-09 ,0.83809e-09 ,0.90062e-09 ,0.96725e-09 ,0.10382e-08 , &
      0.11138e-08 ,0.11941e-08 ,0.12796e-08 ,0.13704e-08 ,0.14669e-08 , &
      0.15694e-08 ,0.16781e-08 ,0.17934e-08 ,0.19157e-08 ,0.20453e-08 , &
      0.21825e-08 ,0.23278e-08 ,0.24815e-08 ,0.26442e-08 ,0.28161e-08 , &
      0.29978e-08 ,0.31898e-08 ,0.33925e-08 ,0.36064e-08 ,0.38321e-08 /)
      totplnk(101:150,16) = (/ &
      0.40700e-08 ,0.43209e-08 ,0.45852e-08 ,0.48636e-08 ,0.51567e-08 , &
      0.54652e-08 ,0.57897e-08 ,0.61310e-08 ,0.64897e-08 ,0.68667e-08 , &
      0.72626e-08 ,0.76784e-08 ,0.81148e-08 ,0.85727e-08 ,0.90530e-08 , &
      0.95566e-08 ,0.10084e-07 ,0.10638e-07 ,0.11217e-07 ,0.11824e-07 , &
      0.12458e-07 ,0.13123e-07 ,0.13818e-07 ,0.14545e-07 ,0.15305e-07 , &
      0.16099e-07 ,0.16928e-07 ,0.17795e-07 ,0.18699e-07 ,0.19643e-07 , &
      0.20629e-07 ,0.21656e-07 ,0.22728e-07 ,0.23845e-07 ,0.25010e-07 , &
      0.26223e-07 ,0.27487e-07 ,0.28804e-07 ,0.30174e-07 ,0.31600e-07 , &
      0.33084e-07 ,0.34628e-07 ,0.36233e-07 ,0.37902e-07 ,0.39637e-07 , &
      0.41440e-07 ,0.43313e-07 ,0.45259e-07 ,0.47279e-07 ,0.49376e-07 /)
      totplnk(151:181,16) = (/ &
      0.51552e-07 ,0.53810e-07 ,0.56153e-07 ,0.58583e-07 ,0.61102e-07 , &
      0.63713e-07 ,0.66420e-07 ,0.69224e-07 ,0.72129e-07 ,0.75138e-07 , &
      0.78254e-07 ,0.81479e-07 ,0.84818e-07 ,0.88272e-07 ,0.91846e-07 , &
      0.95543e-07 ,0.99366e-07 ,0.10332e-06 ,0.10740e-06 ,0.11163e-06 , &
      0.11599e-06 ,0.12050e-06 ,0.12515e-06 ,0.12996e-06 ,0.13493e-06 , &
      0.14005e-06 ,0.14534e-06 ,0.15080e-06 ,0.15643e-06 ,0.16224e-06 , &
      0.16823e-06 /)
      totplk16(1:50) = (/ &
      0.28481e-12 ,0.33159e-12 ,0.38535e-12 ,0.44701e-12 ,0.51763e-12 , &
      0.59836e-12 ,0.69049e-12 ,0.79549e-12 ,0.91493e-12 ,0.10506e-11 , &
      0.12045e-11 ,0.13788e-11 ,0.15758e-11 ,0.17984e-11 ,0.20493e-11 , &
      0.23317e-11 ,0.26494e-11 ,0.30060e-11 ,0.34060e-11 ,0.38539e-11 , &
      0.43548e-11 ,0.49144e-11 ,0.55387e-11 ,0.62344e-11 ,0.70086e-11 , &
      0.78692e-11 ,0.88248e-11 ,0.98846e-11 ,0.11059e-10 ,0.12358e-10 , &
      0.13794e-10 ,0.15379e-10 ,0.17128e-10 ,0.19055e-10 ,0.21176e-10 , &
      0.23508e-10 ,0.26070e-10 ,0.28881e-10 ,0.31963e-10 ,0.35339e-10 , &
      0.39034e-10 ,0.43073e-10 ,0.47484e-10 ,0.52299e-10 ,0.57548e-10 , &
      0.63267e-10 ,0.69491e-10 ,0.76261e-10 ,0.83616e-10 ,0.91603e-10 /)
      totplk16(51:100) = (/ &
      0.10027e-09 ,0.10966e-09 ,0.11983e-09 ,0.13084e-09 ,0.14275e-09 , &
      0.15562e-09 ,0.16951e-09 ,0.18451e-09 ,0.20068e-09 ,0.21810e-09 , &
      0.23686e-09 ,0.25704e-09 ,0.27875e-09 ,0.30207e-09 ,0.32712e-09 , &
      0.35400e-09 ,0.38282e-09 ,0.41372e-09 ,0.44681e-09 ,0.48223e-09 , &
      0.52013e-09 ,0.56064e-09 ,0.60392e-09 ,0.65015e-09 ,0.69948e-09 , &
      0.75209e-09 ,0.80818e-09 ,0.86794e-09 ,0.93157e-09 ,0.99929e-09 , &
      0.10713e-08 ,0.11479e-08 ,0.12293e-08 ,0.13157e-08 ,0.14074e-08 , &
      0.15047e-08 ,0.16079e-08 ,0.17172e-08 ,0.18330e-08 ,0.19557e-08 , &
      0.20855e-08 ,0.22228e-08 ,0.23680e-08 ,0.25214e-08 ,0.26835e-08 , &
      0.28546e-08 ,0.30352e-08 ,0.32257e-08 ,0.34266e-08 ,0.36384e-08 /)
      totplk16(101:150) = (/ &
      0.38615e-08 ,0.40965e-08 ,0.43438e-08 ,0.46041e-08 ,0.48779e-08 , &
      0.51658e-08 ,0.54683e-08 ,0.57862e-08 ,0.61200e-08 ,0.64705e-08 , &
      0.68382e-08 ,0.72240e-08 ,0.76285e-08 ,0.80526e-08 ,0.84969e-08 , &
      0.89624e-08 ,0.94498e-08 ,0.99599e-08 ,0.10494e-07 ,0.11052e-07 , &
      0.11636e-07 ,0.12246e-07 ,0.12884e-07 ,0.13551e-07 ,0.14246e-07 , &
      0.14973e-07 ,0.15731e-07 ,0.16522e-07 ,0.17347e-07 ,0.18207e-07 , &
      0.19103e-07 ,0.20037e-07 ,0.21011e-07 ,0.22024e-07 ,0.23079e-07 , &
      0.24177e-07 ,0.25320e-07 ,0.26508e-07 ,0.27744e-07 ,0.29029e-07 , &
      0.30365e-07 ,0.31753e-07 ,0.33194e-07 ,0.34691e-07 ,0.36246e-07 , &
      0.37859e-07 ,0.39533e-07 ,0.41270e-07 ,0.43071e-07 ,0.44939e-07 /)
      totplk16(151:181) = (/ &
      0.46875e-07 ,0.48882e-07 ,0.50961e-07 ,0.53115e-07 ,0.55345e-07 , &
      0.57655e-07 ,0.60046e-07 ,0.62520e-07 ,0.65080e-07 ,0.67728e-07 , &
      0.70466e-07 ,0.73298e-07 ,0.76225e-07 ,0.79251e-07 ,0.82377e-07 , &
      0.85606e-07 ,0.88942e-07 ,0.92386e-07 ,0.95942e-07 ,0.99612e-07 , &
      0.10340e-06 ,0.10731e-06 ,0.11134e-06 ,0.11550e-06 ,0.11979e-06 , &
      0.12421e-06 ,0.12876e-06 ,0.13346e-06 ,0.13830e-06 ,0.14328e-06 , &
      0.14841e-06 /)
      end subroutine lwavplank
      subroutine lwavplankderiv
      save
      totplnkderiv(1:50, 1) = (/ &
      2.22125e-08 ,2.23245e-08 ,2.24355e-08 ,2.25435e-08 ,2.26560e-08 , &
      2.27620e-08 ,2.28690e-08 ,2.29760e-08 ,2.30775e-08 ,2.31800e-08 , &
      2.32825e-08 ,2.33825e-08 ,2.34820e-08 ,2.35795e-08 ,2.36760e-08 , &
      2.37710e-08 ,2.38655e-08 ,2.39595e-08 ,2.40530e-08 ,2.41485e-08 , &
      2.42395e-08 ,2.43300e-08 ,2.44155e-08 ,2.45085e-08 ,2.45905e-08 , &
      2.46735e-08 ,2.47565e-08 ,2.48465e-08 ,2.49315e-08 ,2.50100e-08 , &
      2.50905e-08 ,2.51705e-08 ,2.52490e-08 ,2.53260e-08 ,2.54075e-08 , &
      2.54785e-08 ,2.55555e-08 ,2.56340e-08 ,2.57050e-08 ,2.57820e-08 , &
      2.58525e-08 ,2.59205e-08 ,2.59945e-08 ,2.60680e-08 ,2.61375e-08 , &
      2.61980e-08 ,2.62745e-08 ,2.63335e-08 ,2.63995e-08 ,2.64710e-08 /)
      totplnkderiv(51:100, 1) = (/ &
      2.65300e-08 ,2.66005e-08 ,2.66685e-08 ,2.67310e-08 ,2.67915e-08 , &
      2.68540e-08 ,2.69065e-08 ,2.69730e-08 ,2.70270e-08 ,2.70690e-08 , &
      2.71420e-08 ,2.71985e-08 ,2.72560e-08 ,2.73180e-08 ,2.73760e-08 , &
      2.74285e-08 ,2.74840e-08 ,2.75290e-08 ,2.75950e-08 ,2.76360e-08 , &
      2.76975e-08 ,2.77475e-08 ,2.78080e-08 ,2.78375e-08 ,2.79120e-08 , &
      2.79510e-08 ,2.79955e-08 ,2.80625e-08 ,2.80920e-08 ,2.81570e-08 , &
      2.81990e-08 ,2.82330e-08 ,2.82830e-08 ,2.83365e-08 ,2.83740e-08 , &
      2.84295e-08 ,2.84910e-08 ,2.85275e-08 ,2.85525e-08 ,2.86085e-08 , &
      2.86535e-08 ,2.86945e-08 ,2.87355e-08 ,2.87695e-08 ,2.88105e-08 , &
      2.88585e-08 ,2.88945e-08 ,2.89425e-08 ,2.89580e-08 ,2.90265e-08 /)
      totplnkderiv(101:150, 1) = (/ &
      2.90445e-08 ,2.90905e-08 ,2.91425e-08 ,2.91560e-08 ,2.91970e-08 , &
      2.91905e-08 ,2.92880e-08 ,2.92950e-08 ,2.93630e-08 ,2.93995e-08 , &
      2.94425e-08 ,2.94635e-08 ,2.94770e-08 ,2.95290e-08 ,2.95585e-08 , &
      2.95815e-08 ,2.95995e-08 ,2.96745e-08 ,2.96725e-08 ,2.97040e-08 , &
      2.97750e-08 ,2.97905e-08 ,2.98175e-08 ,2.98355e-08 ,2.98705e-08 , &
      2.99040e-08 ,2.99680e-08 ,2.99860e-08 ,3.00270e-08 ,3.00200e-08 , &
      3.00770e-08 ,3.00795e-08 ,3.01065e-08 ,3.01795e-08 ,3.01815e-08 , &
      3.02025e-08 ,3.02360e-08 ,3.02360e-08 ,3.03090e-08 ,3.03155e-08 , &
      3.03725e-08 ,3.03635e-08 ,3.04270e-08 ,3.04610e-08 ,3.04635e-08 , &
      3.04610e-08 ,3.05180e-08 ,3.05430e-08 ,3.05290e-08 ,3.05885e-08 /)
      totplnkderiv(151:181, 1) = (/ &
      3.05750e-08 ,3.05775e-08 ,3.06795e-08 ,3.07025e-08 ,3.07365e-08 , &
      3.07435e-08 ,3.07525e-08 ,3.07680e-08 ,3.08115e-08 ,3.07930e-08 , &
      3.08155e-08 ,3.08660e-08 ,3.08865e-08 ,3.08390e-08 ,3.09340e-08 , &
      3.09685e-08 ,3.09340e-08 ,3.09820e-08 ,3.10365e-08 ,3.10705e-08 , &
      3.10750e-08 ,3.10475e-08 ,3.11685e-08 ,3.11455e-08 ,3.11500e-08 , &
      3.11775e-08 ,3.11890e-08 ,3.12045e-08 ,3.12185e-08 ,3.12415e-08 , &
      3.12590e-08 /)
      totplnkderiv(1:50, 2) = (/ &
      4.91150e-08 ,4.97290e-08 ,5.03415e-08 ,5.09460e-08 ,5.15550e-08 , &
      5.21540e-08 ,5.27575e-08 ,5.33500e-08 ,5.39500e-08 ,5.45445e-08 , &
      5.51290e-08 ,5.57235e-08 ,5.62955e-08 ,5.68800e-08 ,5.74620e-08 , &
      5.80425e-08 ,5.86145e-08 ,5.91810e-08 ,5.97435e-08 ,6.03075e-08 , &
      6.08625e-08 ,6.14135e-08 ,6.19775e-08 ,6.25185e-08 ,6.30675e-08 , &
      6.36145e-08 ,6.41535e-08 ,6.46920e-08 ,6.52265e-08 ,6.57470e-08 , &
      6.62815e-08 ,6.68000e-08 ,6.73320e-08 ,6.78550e-08 ,6.83530e-08 , &
      6.88760e-08 ,6.93735e-08 ,6.98790e-08 ,7.03950e-08 ,7.08810e-08 , &
      7.13815e-08 ,7.18795e-08 ,7.23415e-08 ,7.28505e-08 ,7.33285e-08 , &
      7.38075e-08 ,7.42675e-08 ,7.47605e-08 ,7.52380e-08 ,7.57020e-08 /)
      totplnkderiv(51:100, 2) = (/ &
      7.61495e-08 ,7.65955e-08 ,7.70565e-08 ,7.75185e-08 ,7.79735e-08 , &
      7.83915e-08 ,7.88625e-08 ,7.93215e-08 ,7.97425e-08 ,8.02195e-08 , &
      8.05905e-08 ,8.10335e-08 ,8.14770e-08 ,8.19025e-08 ,8.22955e-08 , &
      8.27115e-08 ,8.31165e-08 ,8.35645e-08 ,8.39440e-08 ,8.43785e-08 , &
      8.47380e-08 ,8.51495e-08 ,8.55405e-08 ,8.59720e-08 ,8.63135e-08 , &
      8.67065e-08 ,8.70930e-08 ,8.74545e-08 ,8.78780e-08 ,8.82160e-08 , &
      8.85625e-08 ,8.89850e-08 ,8.93395e-08 ,8.97080e-08 ,9.00675e-08 , &
      9.04085e-08 ,9.07360e-08 ,9.11315e-08 ,9.13815e-08 ,9.18320e-08 , &
      9.21500e-08 ,9.24725e-08 ,9.28640e-08 ,9.31955e-08 ,9.35185e-08 , &
      9.38645e-08 ,9.41780e-08 ,9.45465e-08 ,9.48470e-08 ,9.51375e-08 /)
      totplnkderiv(101:150, 2) = (/ &
      9.55245e-08 ,9.57925e-08 ,9.61195e-08 ,9.64750e-08 ,9.68110e-08 , &
      9.71715e-08 ,9.74150e-08 ,9.77250e-08 ,9.79600e-08 ,9.82600e-08 , &
      9.85300e-08 ,9.88400e-08 ,9.91600e-08 ,9.95350e-08 ,9.97500e-08 , &
      1.00090e-07 ,1.00370e-07 ,1.00555e-07 ,1.00935e-07 ,1.01275e-07 , &
      1.01400e-07 ,1.01790e-07 ,1.01945e-07 ,1.02225e-07 ,1.02585e-07 , &
      1.02895e-07 ,1.03010e-07 ,1.03285e-07 ,1.03540e-07 ,1.03890e-07 , &
      1.04015e-07 ,1.04420e-07 ,1.04640e-07 ,1.04810e-07 ,1.05090e-07 , &
      1.05385e-07 ,1.05600e-07 ,1.05965e-07 ,1.06050e-07 ,1.06385e-07 , &
      1.06390e-07 ,1.06795e-07 ,1.06975e-07 ,1.07240e-07 ,1.07435e-07 , &
      1.07815e-07 ,1.07960e-07 ,1.08010e-07 ,1.08535e-07 ,1.08670e-07 /)
      totplnkderiv(151:181, 2) = (/ &
      1.08855e-07 ,1.09210e-07 ,1.09195e-07 ,1.09510e-07 ,1.09665e-07 , &
      1.09885e-07 ,1.10130e-07 ,1.10440e-07 ,1.10640e-07 ,1.10760e-07 , &
      1.11125e-07 ,1.11195e-07 ,1.11345e-07 ,1.11710e-07 ,1.11765e-07 , &
      1.11960e-07 ,1.12225e-07 ,1.12460e-07 ,1.12595e-07 ,1.12730e-07 , &
      1.12880e-07 ,1.13295e-07 ,1.13215e-07 ,1.13505e-07 ,1.13665e-07 , &
      1.13870e-07 ,1.14025e-07 ,1.14325e-07 ,1.14495e-07 ,1.14605e-07 , &
      1.14905e-07 /)
      totplnkderiv(1:50, 3) = (/ &
      4.27040e-08 ,4.35430e-08 ,4.43810e-08 ,4.52210e-08 ,4.60630e-08 , &
      4.69135e-08 ,4.77585e-08 ,4.86135e-08 ,4.94585e-08 ,5.03230e-08 , &
      5.11740e-08 ,5.20250e-08 ,5.28940e-08 ,5.37465e-08 ,5.46175e-08 , &
      5.54700e-08 ,5.63430e-08 ,5.72085e-08 ,5.80735e-08 ,5.89430e-08 , &
      5.98015e-08 ,6.06680e-08 ,6.15380e-08 ,6.24130e-08 ,6.32755e-08 , &
      6.41340e-08 ,6.50060e-08 ,6.58690e-08 ,6.67315e-08 ,6.76025e-08 , &
      6.84585e-08 ,6.93205e-08 ,7.01845e-08 ,7.10485e-08 ,7.19160e-08 , &
      7.27695e-08 ,7.36145e-08 ,7.44840e-08 ,7.53405e-08 ,7.61770e-08 , &
      7.70295e-08 ,7.78745e-08 ,7.87350e-08 ,7.95740e-08 ,8.04150e-08 , &
      8.12565e-08 ,8.20885e-08 ,8.29455e-08 ,8.37830e-08 ,8.46035e-08 /)
      totplnkderiv(51:100, 3) = (/ &
      8.54315e-08 ,8.62770e-08 ,8.70975e-08 ,8.79140e-08 ,8.87190e-08 , &
      8.95625e-08 ,9.03625e-08 ,9.11795e-08 ,9.19930e-08 ,9.27685e-08 , &
      9.36095e-08 ,9.43785e-08 ,9.52375e-08 ,9.59905e-08 ,9.67680e-08 , &
      9.75840e-08 ,9.83755e-08 ,9.91710e-08 ,9.99445e-08 ,1.00706e-07 , &
      1.01477e-07 ,1.02255e-07 ,1.03021e-07 ,1.03776e-07 ,1.04544e-07 , &
      1.05338e-07 ,1.06082e-07 ,1.06843e-07 ,1.07543e-07 ,1.08298e-07 , &
      1.09103e-07 ,1.09812e-07 ,1.10536e-07 ,1.11268e-07 ,1.12027e-07 , &
      1.12727e-07 ,1.13464e-07 ,1.14183e-07 ,1.15037e-07 ,1.15615e-07 , &
      1.16329e-07 ,1.17057e-07 ,1.17734e-07 ,1.18448e-07 ,1.19149e-07 , &
      1.19835e-07 ,1.20512e-07 ,1.21127e-07 ,1.21895e-07 ,1.22581e-07 /)
      totplnkderiv(101:150, 3) = (/ &
      1.23227e-07 ,1.23928e-07 ,1.24560e-07 ,1.25220e-07 ,1.25895e-07 , &
      1.26565e-07 ,1.27125e-07 ,1.27855e-07 ,1.28490e-07 ,1.29195e-07 , &
      1.29790e-07 ,1.30470e-07 ,1.31070e-07 ,1.31690e-07 ,1.32375e-07 , &
      1.32960e-07 ,1.33570e-07 ,1.34230e-07 ,1.34840e-07 ,1.35315e-07 , &
      1.35990e-07 ,1.36555e-07 ,1.37265e-07 ,1.37945e-07 ,1.38425e-07 , &
      1.38950e-07 ,1.39640e-07 ,1.40220e-07 ,1.40775e-07 ,1.41400e-07 , &
      1.42020e-07 ,1.42500e-07 ,1.43085e-07 ,1.43680e-07 ,1.44255e-07 , &
      1.44855e-07 ,1.45385e-07 ,1.45890e-07 ,1.46430e-07 ,1.46920e-07 , &
      1.47715e-07 ,1.48090e-07 ,1.48695e-07 ,1.49165e-07 ,1.49715e-07 , &
      1.50130e-07 ,1.50720e-07 ,1.51330e-07 ,1.51725e-07 ,1.52350e-07 /)
      totplnkderiv(151:181, 3) = (/ &
      1.52965e-07 ,1.53305e-07 ,1.53915e-07 ,1.54280e-07 ,1.54950e-07 , &
      1.55370e-07 ,1.55850e-07 ,1.56260e-07 ,1.56825e-07 ,1.57470e-07 , &
      1.57760e-07 ,1.58295e-07 ,1.58780e-07 ,1.59470e-07 ,1.59940e-07 , &
      1.60325e-07 ,1.60825e-07 ,1.61100e-07 ,1.61605e-07 ,1.62045e-07 , &
      1.62670e-07 ,1.63020e-07 ,1.63625e-07 ,1.63900e-07 ,1.64420e-07 , &
      1.64705e-07 ,1.65430e-07 ,1.65610e-07 ,1.66220e-07 ,1.66585e-07 , &
      1.66965e-07 /)
      totplnkderiv(1:50, 4) = (/ &
      3.32829e-08 ,3.41160e-08 ,3.49626e-08 ,3.58068e-08 ,3.66765e-08 , &
      3.75320e-08 ,3.84095e-08 ,3.92920e-08 ,4.01830e-08 ,4.10715e-08 , &
      4.19735e-08 ,4.28835e-08 ,4.37915e-08 ,4.47205e-08 ,4.56410e-08 , &
      4.65770e-08 ,4.75090e-08 ,4.84530e-08 ,4.93975e-08 ,5.03470e-08 , &
      5.13000e-08 ,5.22560e-08 ,5.32310e-08 ,5.41865e-08 ,5.51655e-08 , &
      5.61590e-08 ,5.71120e-08 ,5.81075e-08 ,5.91060e-08 ,6.00895e-08 , &
      6.10750e-08 ,6.20740e-08 ,6.30790e-08 ,6.40765e-08 ,6.50940e-08 , &
      6.60895e-08 ,6.71230e-08 ,6.81200e-08 ,6.91260e-08 ,7.01485e-08 , &
      7.11625e-08 ,7.21870e-08 ,7.32010e-08 ,7.42080e-08 ,7.52285e-08 , &
      7.62930e-08 ,7.73040e-08 ,7.83185e-08 ,7.93410e-08 ,8.03560e-08 /)
      totplnkderiv(51:100, 4) = (/ &
      8.14115e-08 ,8.24200e-08 ,8.34555e-08 ,8.45100e-08 ,8.55265e-08 , &
      8.65205e-08 ,8.75615e-08 ,8.85870e-08 ,8.96175e-08 ,9.07015e-08 , &
      9.16475e-08 ,9.27525e-08 ,9.37055e-08 ,9.47375e-08 ,9.57995e-08 , &
      9.67635e-08 ,9.77980e-08 ,9.87735e-08 ,9.98485e-08 ,1.00904e-07 , &
      1.01900e-07 ,1.02876e-07 ,1.03905e-07 ,1.04964e-07 ,1.05956e-07 , &
      1.06870e-07 ,1.07952e-07 ,1.08944e-07 ,1.10003e-07 ,1.10965e-07 , &
      1.11952e-07 ,1.12927e-07 ,1.13951e-07 ,1.14942e-07 ,1.15920e-07 , &
      1.16968e-07 ,1.17877e-07 ,1.18930e-07 ,1.19862e-07 ,1.20817e-07 , &
      1.21817e-07 ,1.22791e-07 ,1.23727e-07 ,1.24751e-07 ,1.25697e-07 , &
      1.26634e-07 ,1.27593e-07 ,1.28585e-07 ,1.29484e-07 ,1.30485e-07 /)
      totplnkderiv(101:150, 4) = (/ &
      1.31363e-07 ,1.32391e-07 ,1.33228e-07 ,1.34155e-07 ,1.35160e-07 , &
      1.36092e-07 ,1.37070e-07 ,1.37966e-07 ,1.38865e-07 ,1.39740e-07 , &
      1.40770e-07 ,1.41620e-07 ,1.42605e-07 ,1.43465e-07 ,1.44240e-07 , &
      1.45305e-07 ,1.46220e-07 ,1.47070e-07 ,1.47935e-07 ,1.48890e-07 , &
      1.49905e-07 ,1.50640e-07 ,1.51435e-07 ,1.52335e-07 ,1.53235e-07 , &
      1.54045e-07 ,1.54895e-07 ,1.55785e-07 ,1.56870e-07 ,1.57360e-07 , &
      1.58395e-07 ,1.59185e-07 ,1.60060e-07 ,1.60955e-07 ,1.61770e-07 , &
      1.62445e-07 ,1.63415e-07 ,1.64170e-07 ,1.65125e-07 ,1.65995e-07 , &
      1.66545e-07 ,1.67580e-07 ,1.68295e-07 ,1.69130e-07 ,1.69935e-07 , &
      1.70800e-07 ,1.71610e-07 ,1.72365e-07 ,1.73215e-07 ,1.73770e-07 /)
      totplnkderiv(151:181, 4) = (/ &
      1.74590e-07 ,1.75525e-07 ,1.76095e-07 ,1.77125e-07 ,1.77745e-07 , &
      1.78580e-07 ,1.79315e-07 ,1.80045e-07 ,1.80695e-07 ,1.81580e-07 , &
      1.82360e-07 ,1.83205e-07 ,1.84055e-07 ,1.84315e-07 ,1.85225e-07 , &
      1.85865e-07 ,1.86660e-07 ,1.87445e-07 ,1.88350e-07 ,1.88930e-07 , &
      1.89420e-07 ,1.90275e-07 ,1.90630e-07 ,1.91650e-07 ,1.92485e-07 , &
      1.93285e-07 ,1.93695e-07 ,1.94595e-07 ,1.94895e-07 ,1.95960e-07 , &
      1.96525e-07 /)
      totplnkderiv(1:50, 5) = (/ &
      2.41948e-08 ,2.49273e-08 ,2.56705e-08 ,2.64263e-08 ,2.71899e-08 , &
      2.79687e-08 ,2.87531e-08 ,2.95520e-08 ,3.03567e-08 ,3.11763e-08 , &
      3.20014e-08 ,3.28390e-08 ,3.36865e-08 ,3.45395e-08 ,3.54083e-08 , &
      3.62810e-08 ,3.71705e-08 ,3.80585e-08 ,3.89650e-08 ,3.98750e-08 , &
      4.07955e-08 ,4.17255e-08 ,4.26635e-08 ,4.36095e-08 ,4.45605e-08 , &
      4.55190e-08 ,4.64910e-08 ,4.74670e-08 ,4.84480e-08 ,4.94430e-08 , &
      5.04460e-08 ,5.14440e-08 ,5.24500e-08 ,5.34835e-08 ,5.44965e-08 , &
      5.55325e-08 ,5.65650e-08 ,5.76050e-08 ,5.86615e-08 ,5.97175e-08 , &
      6.07750e-08 ,6.18400e-08 ,6.29095e-08 ,6.39950e-08 ,6.50665e-08 , &
      6.61405e-08 ,6.72290e-08 ,6.82800e-08 ,6.94445e-08 ,7.05460e-08 /)
      totplnkderiv(51:100, 5) = (/ &
      7.16400e-08 ,7.27475e-08 ,7.38790e-08 ,7.49845e-08 ,7.61270e-08 , &
      7.72375e-08 ,7.83770e-08 ,7.95045e-08 ,8.06315e-08 ,8.17715e-08 , &
      8.29275e-08 ,8.40555e-08 ,8.52110e-08 ,8.63565e-08 ,8.75045e-08 , &
      8.86735e-08 ,8.98150e-08 ,9.09970e-08 ,9.21295e-08 ,9.32730e-08 , &
      9.44605e-08 ,9.56170e-08 ,9.67885e-08 ,9.79275e-08 ,9.91190e-08 , &
      1.00278e-07 ,1.01436e-07 ,1.02625e-07 ,1.03792e-07 ,1.04989e-07 , &
      1.06111e-07 ,1.07320e-07 ,1.08505e-07 ,1.09626e-07 ,1.10812e-07 , &
      1.11948e-07 ,1.13162e-07 ,1.14289e-07 ,1.15474e-07 ,1.16661e-07 , &
      1.17827e-07 ,1.19023e-07 ,1.20167e-07 ,1.21356e-07 ,1.22499e-07 , &
      1.23653e-07 ,1.24876e-07 ,1.25983e-07 ,1.27175e-07 ,1.28325e-07 /)
      totplnkderiv(101:150, 5) = (/ &
      1.29517e-07 ,1.30685e-07 ,1.31840e-07 ,1.33013e-07 ,1.34160e-07 , &
      1.35297e-07 ,1.36461e-07 ,1.37630e-07 ,1.38771e-07 ,1.39913e-07 , &
      1.41053e-07 ,1.42218e-07 ,1.43345e-07 ,1.44460e-07 ,1.45692e-07 , &
      1.46697e-07 ,1.47905e-07 ,1.49010e-07 ,1.50210e-07 ,1.51285e-07 , &
      1.52380e-07 ,1.53555e-07 ,1.54655e-07 ,1.55805e-07 ,1.56850e-07 , &
      1.58055e-07 ,1.59115e-07 ,1.60185e-07 ,1.61255e-07 ,1.62465e-07 , &
      1.63575e-07 ,1.64675e-07 ,1.65760e-07 ,1.66765e-07 ,1.67945e-07 , &
      1.69070e-07 ,1.70045e-07 ,1.71145e-07 ,1.72260e-07 ,1.73290e-07 , &
      1.74470e-07 ,1.75490e-07 ,1.76515e-07 ,1.77555e-07 ,1.78660e-07 , &
      1.79670e-07 ,1.80705e-07 ,1.81895e-07 ,1.82745e-07 ,1.83950e-07 /)
      totplnkderiv(151:181, 5) = (/ &
      1.84955e-07 ,1.85940e-07 ,1.87080e-07 ,1.88010e-07 ,1.89145e-07 , &
      1.90130e-07 ,1.91110e-07 ,1.92130e-07 ,1.93205e-07 ,1.94230e-07 , &
      1.95045e-07 ,1.96070e-07 ,1.97155e-07 ,1.98210e-07 ,1.99080e-07 , &
      2.00280e-07 ,2.01135e-07 ,2.02150e-07 ,2.03110e-07 ,2.04135e-07 , &
      2.05110e-07 ,2.06055e-07 ,2.07120e-07 ,2.08075e-07 ,2.08975e-07 , &
      2.09950e-07 ,2.10870e-07 ,2.11830e-07 ,2.12960e-07 ,2.13725e-07 , &
      2.14765e-07 /)
      totplnkderiv(1:50, 6) = (/ &
      1.36567e-08 ,1.41766e-08 ,1.47079e-08 ,1.52499e-08 ,1.58075e-08 , &
      1.63727e-08 ,1.69528e-08 ,1.75429e-08 ,1.81477e-08 ,1.87631e-08 , &
      1.93907e-08 ,2.00297e-08 ,2.06808e-08 ,2.13432e-08 ,2.20183e-08 , &
      2.27076e-08 ,2.34064e-08 ,2.41181e-08 ,2.48400e-08 ,2.55750e-08 , &
      2.63231e-08 ,2.70790e-08 ,2.78502e-08 ,2.86326e-08 ,2.94259e-08 , &
      3.02287e-08 ,3.10451e-08 ,3.18752e-08 ,3.27108e-08 ,3.35612e-08 , &
      3.44198e-08 ,3.52930e-08 ,3.61785e-08 ,3.70690e-08 ,3.79725e-08 , &
      3.88845e-08 ,3.98120e-08 ,4.07505e-08 ,4.16965e-08 ,4.26515e-08 , &
      4.36190e-08 ,4.45925e-08 ,4.55760e-08 ,4.65735e-08 ,4.75835e-08 , &
      4.85970e-08 ,4.96255e-08 ,5.06975e-08 ,5.16950e-08 ,5.27530e-08 /)
      totplnkderiv(51:100, 6) = (/ &
      5.38130e-08 ,5.48860e-08 ,5.59715e-08 ,5.70465e-08 ,5.81385e-08 , &
      5.92525e-08 ,6.03565e-08 ,6.14815e-08 ,6.26175e-08 ,6.37475e-08 , &
      6.48855e-08 ,6.60340e-08 ,6.71980e-08 ,6.83645e-08 ,6.95430e-08 , &
      7.07145e-08 ,7.19015e-08 ,7.30995e-08 ,7.43140e-08 ,7.55095e-08 , &
      7.67115e-08 ,7.79485e-08 ,7.91735e-08 ,8.03925e-08 ,8.16385e-08 , &
      8.28775e-08 ,8.41235e-08 ,8.53775e-08 ,8.66405e-08 ,8.78940e-08 , &
      8.91805e-08 ,9.04515e-08 ,9.17290e-08 ,9.30230e-08 ,9.43145e-08 , &
      9.56200e-08 ,9.69160e-08 ,9.82140e-08 ,9.95285e-08 ,1.00829e-07 , &
      1.02145e-07 ,1.03478e-07 ,1.04787e-07 ,1.06095e-07 ,1.07439e-07 , &
      1.08785e-07 ,1.10078e-07 ,1.11466e-07 ,1.12795e-07 ,1.14133e-07 /)
      totplnkderiv(101:150, 6) = (/ &
      1.15479e-07 ,1.16825e-07 ,1.18191e-07 ,1.19540e-07 ,1.20908e-07 , &
      1.22257e-07 ,1.23634e-07 ,1.24992e-07 ,1.26345e-07 ,1.27740e-07 , &
      1.29098e-07 ,1.30447e-07 ,1.31831e-07 ,1.33250e-07 ,1.34591e-07 , &
      1.36011e-07 ,1.37315e-07 ,1.38721e-07 ,1.40103e-07 ,1.41504e-07 , &
      1.42882e-07 ,1.44259e-07 ,1.45674e-07 ,1.46997e-07 ,1.48412e-07 , &
      1.49794e-07 ,1.51167e-07 ,1.52577e-07 ,1.53941e-07 ,1.55369e-07 , &
      1.56725e-07 ,1.58125e-07 ,1.59460e-07 ,1.60895e-07 ,1.62260e-07 , &
      1.63610e-07 ,1.65085e-07 ,1.66410e-07 ,1.67805e-07 ,1.69185e-07 , &
      1.70570e-07 ,1.71915e-07 ,1.73375e-07 ,1.74775e-07 ,1.76090e-07 , &
      1.77485e-07 ,1.78905e-07 ,1.80190e-07 ,1.81610e-07 ,1.82960e-07 /)
      totplnkderiv(151:181, 6) = (/ &
      1.84330e-07 ,1.85750e-07 ,1.87060e-07 ,1.88470e-07 ,1.89835e-07 , &
      1.91250e-07 ,1.92565e-07 ,1.93925e-07 ,1.95220e-07 ,1.96620e-07 , &
      1.98095e-07 ,1.99330e-07 ,2.00680e-07 ,2.02090e-07 ,2.03360e-07 , &
      2.04775e-07 ,2.06080e-07 ,2.07440e-07 ,2.08820e-07 ,2.10095e-07 , &
      2.11445e-07 ,2.12785e-07 ,2.14050e-07 ,2.15375e-07 ,2.16825e-07 , &
      2.18080e-07 ,2.19345e-07 ,2.20710e-07 ,2.21980e-07 ,2.23425e-07 , &
      2.24645e-07 /)
      totplnkderiv(1:50, 7) = (/ &
      7.22270e-09 ,7.55350e-09 ,7.89480e-09 ,8.24725e-09 ,8.60780e-09 , &
      8.98215e-09 ,9.36430e-09 ,9.76035e-09 ,1.01652e-08 ,1.05816e-08 , &
      1.10081e-08 ,1.14480e-08 ,1.18981e-08 ,1.23600e-08 ,1.28337e-08 , &
      1.33172e-08 ,1.38139e-08 ,1.43208e-08 ,1.48413e-08 ,1.53702e-08 , &
      1.59142e-08 ,1.64704e-08 ,1.70354e-08 ,1.76178e-08 ,1.82065e-08 , &
      1.88083e-08 ,1.94237e-08 ,2.00528e-08 ,2.06913e-08 ,2.13413e-08 , &
      2.20058e-08 ,2.26814e-08 ,2.33686e-08 ,2.40729e-08 ,2.47812e-08 , &
      2.55099e-08 ,2.62449e-08 ,2.69966e-08 ,2.77569e-08 ,2.85269e-08 , &
      2.93144e-08 ,3.01108e-08 ,3.09243e-08 ,3.17433e-08 ,3.25756e-08 , &
      3.34262e-08 ,3.42738e-08 ,3.51480e-08 ,3.60285e-08 ,3.69160e-08 /)
      totplnkderiv(51:100, 7) = (/ &
      3.78235e-08 ,3.87390e-08 ,3.96635e-08 ,4.06095e-08 ,4.15600e-08 , &
      4.25180e-08 ,4.34895e-08 ,4.44800e-08 ,4.54715e-08 ,4.64750e-08 , &
      4.74905e-08 ,4.85210e-08 ,4.95685e-08 ,5.06135e-08 ,5.16725e-08 , &
      5.27480e-08 ,5.38265e-08 ,5.49170e-08 ,5.60120e-08 ,5.71275e-08 , &
      5.82610e-08 ,5.93775e-08 ,6.05245e-08 ,6.17025e-08 ,6.28355e-08 , &
      6.40135e-08 ,6.52015e-08 ,6.63865e-08 ,6.75790e-08 ,6.88120e-08 , &
      7.00070e-08 ,7.12335e-08 ,7.24720e-08 ,7.37340e-08 ,7.49775e-08 , &
      7.62415e-08 ,7.75185e-08 ,7.87915e-08 ,8.00875e-08 ,8.13630e-08 , &
      8.26710e-08 ,8.39645e-08 ,8.53060e-08 ,8.66305e-08 ,8.79915e-08 , &
      8.93080e-08 ,9.06560e-08 ,9.19860e-08 ,9.33550e-08 ,9.47305e-08 /)
      totplnkderiv(101:150, 7) = (/ &
      9.61180e-08 ,9.74500e-08 ,9.88850e-08 ,1.00263e-07 ,1.01688e-07 , &
      1.03105e-07 ,1.04489e-07 ,1.05906e-07 ,1.07345e-07 ,1.08771e-07 , &
      1.10220e-07 ,1.11713e-07 ,1.13098e-07 ,1.14515e-07 ,1.16019e-07 , &
      1.17479e-07 ,1.18969e-07 ,1.20412e-07 ,1.21852e-07 ,1.23387e-07 , &
      1.24851e-07 ,1.26319e-07 ,1.27811e-07 ,1.29396e-07 ,1.30901e-07 , &
      1.32358e-07 ,1.33900e-07 ,1.35405e-07 ,1.36931e-07 ,1.38443e-07 , &
      1.39985e-07 ,1.41481e-07 ,1.43072e-07 ,1.44587e-07 ,1.46133e-07 , &
      1.47698e-07 ,1.49203e-07 ,1.50712e-07 ,1.52363e-07 ,1.53795e-07 , &
      1.55383e-07 ,1.56961e-07 ,1.58498e-07 ,1.60117e-07 ,1.61745e-07 , &
      1.63190e-07 ,1.64790e-07 ,1.66370e-07 ,1.67975e-07 ,1.69555e-07 /)
      totplnkderiv(151:181, 7) = (/ &
      1.71060e-07 ,1.72635e-07 ,1.74345e-07 ,1.75925e-07 ,1.77395e-07 , &
      1.78960e-07 ,1.80620e-07 ,1.82180e-07 ,1.83840e-07 ,1.85340e-07 , &
      1.86940e-07 ,1.88550e-07 ,1.90095e-07 ,1.91670e-07 ,1.93385e-07 , &
      1.94895e-07 ,1.96500e-07 ,1.98090e-07 ,1.99585e-07 ,2.01280e-07 , &
      2.02950e-07 ,2.04455e-07 ,2.06075e-07 ,2.07635e-07 ,2.09095e-07 , &
      2.10865e-07 ,2.12575e-07 ,2.14050e-07 ,2.15630e-07 ,2.17060e-07 , &
      2.18715e-07 /)
      totplnkderiv(1:50, 8) = (/ &
      4.26397e-09 ,4.48470e-09 ,4.71299e-09 ,4.94968e-09 ,5.19542e-09 , &
      5.44847e-09 ,5.71195e-09 ,5.98305e-09 ,6.26215e-09 ,6.55290e-09 , &
      6.85190e-09 ,7.15950e-09 ,7.47745e-09 ,7.80525e-09 ,8.14190e-09 , &
      8.48915e-09 ,8.84680e-09 ,9.21305e-09 ,9.59105e-09 ,9.98130e-09 , &
      1.03781e-08 ,1.07863e-08 ,1.12094e-08 ,1.16371e-08 ,1.20802e-08 , &
      1.25327e-08 ,1.29958e-08 ,1.34709e-08 ,1.39592e-08 ,1.44568e-08 , &
      1.49662e-08 ,1.54828e-08 ,1.60186e-08 ,1.65612e-08 ,1.71181e-08 , &
      1.76822e-08 ,1.82591e-08 ,1.88487e-08 ,1.94520e-08 ,2.00691e-08 , &
      2.06955e-08 ,2.13353e-08 ,2.19819e-08 ,2.26479e-08 ,2.33234e-08 , &
      2.40058e-08 ,2.47135e-08 ,2.54203e-08 ,2.61414e-08 ,2.68778e-08 /)
      totplnkderiv(51:100, 8) = (/ &
      2.76265e-08 ,2.83825e-08 ,2.91632e-08 ,2.99398e-08 ,3.07389e-08 , &
      3.15444e-08 ,3.23686e-08 ,3.31994e-08 ,3.40487e-08 ,3.49020e-08 , &
      3.57715e-08 ,3.66515e-08 ,3.75465e-08 ,3.84520e-08 ,3.93675e-08 , &
      4.02985e-08 ,4.12415e-08 ,4.21965e-08 ,4.31630e-08 ,4.41360e-08 , &
      4.51220e-08 ,4.61235e-08 ,4.71440e-08 ,4.81515e-08 ,4.91905e-08 , &
      5.02395e-08 ,5.12885e-08 ,5.23735e-08 ,5.34460e-08 ,5.45245e-08 , &
      5.56375e-08 ,5.67540e-08 ,5.78780e-08 ,5.90065e-08 ,6.01520e-08 , &
      6.13000e-08 ,6.24720e-08 ,6.36530e-08 ,6.48500e-08 ,6.60500e-08 , &
      6.72435e-08 ,6.84735e-08 ,6.97025e-08 ,7.09530e-08 ,7.21695e-08 , &
      7.34270e-08 ,7.47295e-08 ,7.59915e-08 ,7.72685e-08 ,7.85925e-08 /)
      totplnkderiv(101:150, 8) = (/ &
      7.98855e-08 ,8.12205e-08 ,8.25120e-08 ,8.38565e-08 ,8.52005e-08 , &
      8.65570e-08 ,8.79075e-08 ,8.92920e-08 ,9.06535e-08 ,9.20455e-08 , &
      9.34230e-08 ,9.48355e-08 ,9.62720e-08 ,9.76890e-08 ,9.90755e-08 , &
      1.00528e-07 ,1.01982e-07 ,1.03436e-07 ,1.04919e-07 ,1.06368e-07 , &
      1.07811e-07 ,1.09326e-07 ,1.10836e-07 ,1.12286e-07 ,1.13803e-07 , &
      1.15326e-07 ,1.16809e-07 ,1.18348e-07 ,1.19876e-07 ,1.21413e-07 , &
      1.22922e-07 ,1.24524e-07 ,1.26049e-07 ,1.27573e-07 ,1.29155e-07 , &
      1.30708e-07 ,1.32327e-07 ,1.33958e-07 ,1.35480e-07 ,1.37081e-07 , &
      1.38716e-07 ,1.40326e-07 ,1.41872e-07 ,1.43468e-07 ,1.45092e-07 , &
      1.46806e-07 ,1.48329e-07 ,1.49922e-07 ,1.51668e-07 ,1.53241e-07 /)
      totplnkderiv(151:181, 8) = (/ &
      1.54996e-07 ,1.56561e-07 ,1.58197e-07 ,1.59884e-07 ,1.61576e-07 , &
      1.63200e-07 ,1.64885e-07 ,1.66630e-07 ,1.68275e-07 ,1.69935e-07 , &
      1.71650e-07 ,1.73245e-07 ,1.75045e-07 ,1.76710e-07 ,1.78330e-07 , &
      1.79995e-07 ,1.81735e-07 ,1.83470e-07 ,1.85200e-07 ,1.86890e-07 , &
      1.88595e-07 ,1.90300e-07 ,1.91995e-07 ,1.93715e-07 ,1.95495e-07 , &
      1.97130e-07 ,1.98795e-07 ,2.00680e-07 ,2.02365e-07 ,2.04090e-07 , &
      2.05830e-07 /)
      totplnkderiv(1:50, 9) = (/ &
      1.85410e-09 ,1.96515e-09 ,2.08117e-09 ,2.20227e-09 ,2.32861e-09 , &
      2.46066e-09 ,2.59812e-09 ,2.74153e-09 ,2.89058e-09 ,3.04567e-09 , &
      3.20674e-09 ,3.37442e-09 ,3.54854e-09 ,3.72892e-09 ,3.91630e-09 , &
      4.11013e-09 ,4.31150e-09 ,4.52011e-09 ,4.73541e-09 ,4.95870e-09 , &
      5.18913e-09 ,5.42752e-09 ,5.67340e-09 ,5.92810e-09 ,6.18995e-09 , &
      6.46055e-09 ,6.73905e-09 ,7.02620e-09 ,7.32260e-09 ,7.62700e-09 , &
      7.94050e-09 ,8.26370e-09 ,8.59515e-09 ,8.93570e-09 ,9.28535e-09 , &
      9.64575e-09 ,1.00154e-08 ,1.03944e-08 ,1.07839e-08 ,1.11832e-08 , &
      1.15909e-08 ,1.20085e-08 ,1.24399e-08 ,1.28792e-08 ,1.33280e-08 , &
      1.37892e-08 ,1.42573e-08 ,1.47408e-08 ,1.52345e-08 ,1.57371e-08 /)
      totplnkderiv(51:100, 9) = (/ &
      1.62496e-08 ,1.67756e-08 ,1.73101e-08 ,1.78596e-08 ,1.84161e-08 , &
      1.89869e-08 ,1.95681e-08 ,2.01632e-08 ,2.07626e-08 ,2.13800e-08 , &
      2.20064e-08 ,2.26453e-08 ,2.32970e-08 ,2.39595e-08 ,2.46340e-08 , &
      2.53152e-08 ,2.60158e-08 ,2.67235e-08 ,2.74471e-08 ,2.81776e-08 , &
      2.89233e-08 ,2.96822e-08 ,3.04488e-08 ,3.12298e-08 ,3.20273e-08 , &
      3.28304e-08 ,3.36455e-08 ,3.44765e-08 ,3.53195e-08 ,3.61705e-08 , &
      3.70385e-08 ,3.79155e-08 ,3.88065e-08 ,3.97055e-08 ,4.06210e-08 , &
      4.15490e-08 ,4.24825e-08 ,4.34355e-08 ,4.43920e-08 ,4.53705e-08 , &
      4.63560e-08 ,4.73565e-08 ,4.83655e-08 ,4.93815e-08 ,5.04180e-08 , &
      5.14655e-08 ,5.25175e-08 ,5.35865e-08 ,5.46720e-08 ,5.57670e-08 /)
      totplnkderiv(101:150, 9) = (/ &
      5.68640e-08 ,5.79825e-08 ,5.91140e-08 ,6.02515e-08 ,6.13985e-08 , &
      6.25525e-08 ,6.37420e-08 ,6.49220e-08 ,6.61145e-08 ,6.73185e-08 , &
      6.85520e-08 ,6.97760e-08 ,7.10050e-08 ,7.22650e-08 ,7.35315e-08 , &
      7.48035e-08 ,7.60745e-08 ,7.73740e-08 ,7.86870e-08 ,7.99845e-08 , &
      8.13325e-08 ,8.26615e-08 ,8.40010e-08 ,8.53640e-08 ,8.67235e-08 , &
      8.80960e-08 ,8.95055e-08 ,9.08945e-08 ,9.23045e-08 ,9.37100e-08 , &
      9.51555e-08 ,9.65630e-08 ,9.80235e-08 ,9.94920e-08 ,1.00966e-07 , &
      1.02434e-07 ,1.03898e-07 ,1.05386e-07 ,1.06905e-07 ,1.08418e-07 , &
      1.09926e-07 ,1.11454e-07 ,1.13010e-07 ,1.14546e-07 ,1.16106e-07 , &
      1.17652e-07 ,1.19264e-07 ,1.20817e-07 ,1.22395e-07 ,1.24024e-07 /)
      totplnkderiv(151:181, 9) = (/ &
      1.25585e-07 ,1.27213e-07 ,1.28817e-07 ,1.30472e-07 ,1.32088e-07 , &
      1.33752e-07 ,1.35367e-07 ,1.37018e-07 ,1.38698e-07 ,1.40394e-07 , &
      1.42026e-07 ,1.43796e-07 ,1.45438e-07 ,1.47175e-07 ,1.48866e-07 , &
      1.50576e-07 ,1.52281e-07 ,1.54018e-07 ,1.55796e-07 ,1.57515e-07 , &
      1.59225e-07 ,1.60989e-07 ,1.62754e-07 ,1.64532e-07 ,1.66285e-07 , &
      1.68070e-07 ,1.69870e-07 ,1.71625e-07 ,1.73440e-07 ,1.75275e-07 , &
      1.77040e-07 /)
      totplnkderiv(1:50,10) = (/ &
      7.14917e-10 ,7.64833e-10 ,8.17460e-10 ,8.72980e-10 ,9.31380e-10 , &
      9.92940e-10 ,1.05746e-09 ,1.12555e-09 ,1.19684e-09 ,1.27162e-09 , &
      1.35001e-09 ,1.43229e-09 ,1.51815e-09 ,1.60831e-09 ,1.70271e-09 , &
      1.80088e-09 ,1.90365e-09 ,2.01075e-09 ,2.12261e-09 ,2.23924e-09 , &
      2.36057e-09 ,2.48681e-09 ,2.61814e-09 ,2.75506e-09 ,2.89692e-09 , &
      3.04423e-09 ,3.19758e-09 ,3.35681e-09 ,3.52113e-09 ,3.69280e-09 , &
      3.86919e-09 ,4.05205e-09 ,4.24184e-09 ,4.43877e-09 ,4.64134e-09 , &
      4.85088e-09 ,5.06670e-09 ,5.29143e-09 ,5.52205e-09 ,5.75980e-09 , &
      6.00550e-09 ,6.25840e-09 ,6.51855e-09 ,6.78800e-09 ,7.06435e-09 , &
      7.34935e-09 ,7.64220e-09 ,7.94470e-09 ,8.25340e-09 ,8.57030e-09 /)
      totplnkderiv(51:100,10) = (/ &
      8.89680e-09 ,9.23255e-09 ,9.57770e-09 ,9.93045e-09 ,1.02932e-08 , &
      1.06649e-08 ,1.10443e-08 ,1.14348e-08 ,1.18350e-08 ,1.22463e-08 , &
      1.26679e-08 ,1.30949e-08 ,1.35358e-08 ,1.39824e-08 ,1.44425e-08 , &
      1.49126e-08 ,1.53884e-08 ,1.58826e-08 ,1.63808e-08 ,1.68974e-08 , &
      1.74159e-08 ,1.79447e-08 ,1.84886e-08 ,1.90456e-08 ,1.96124e-08 , &
      2.01863e-08 ,2.07737e-08 ,2.13720e-08 ,2.19837e-08 ,2.26044e-08 , &
      2.32396e-08 ,2.38856e-08 ,2.45344e-08 ,2.52055e-08 ,2.58791e-08 , &
      2.65706e-08 ,2.72758e-08 ,2.79852e-08 ,2.87201e-08 ,2.94518e-08 , &
      3.02063e-08 ,3.09651e-08 ,3.17357e-08 ,3.25235e-08 ,3.33215e-08 , &
      3.41285e-08 ,3.49485e-08 ,3.57925e-08 ,3.66330e-08 ,3.74765e-08 /)
      totplnkderiv(101:150,10) = (/ &
      3.83675e-08 ,3.92390e-08 ,4.01330e-08 ,4.10340e-08 ,4.19585e-08 , &
      4.28815e-08 ,4.38210e-08 ,4.47770e-08 ,4.57575e-08 ,4.67325e-08 , &
      4.77170e-08 ,4.87205e-08 ,4.97410e-08 ,5.07620e-08 ,5.18180e-08 , &
      5.28540e-08 ,5.39260e-08 ,5.50035e-08 ,5.60885e-08 ,5.71900e-08 , &
      5.82940e-08 ,5.94380e-08 ,6.05690e-08 ,6.17185e-08 ,6.28860e-08 , &
      6.40670e-08 ,6.52300e-08 ,6.64225e-08 ,6.76485e-08 ,6.88715e-08 , &
      7.00750e-08 ,7.13760e-08 ,7.25910e-08 ,7.38860e-08 ,7.51290e-08 , &
      7.64420e-08 ,7.77550e-08 ,7.90725e-08 ,8.03825e-08 ,8.17330e-08 , &
      8.30810e-08 ,8.44330e-08 ,8.57720e-08 ,8.72115e-08 ,8.85800e-08 , &
      8.99945e-08 ,9.13905e-08 ,9.28345e-08 ,9.42665e-08 ,9.56765e-08 /)
      totplnkderiv(151:181,10) = (/ &
      9.72000e-08 ,9.86780e-08 ,1.00105e-07 ,1.01616e-07 ,1.03078e-07 , &
      1.04610e-07 ,1.06154e-07 ,1.07639e-07 ,1.09242e-07 ,1.10804e-07 , &
      1.12384e-07 ,1.13871e-07 ,1.15478e-07 ,1.17066e-07 ,1.18703e-07 , &
      1.20294e-07 ,1.21930e-07 ,1.23543e-07 ,1.25169e-07 ,1.26806e-07 , &
      1.28503e-07 ,1.30233e-07 ,1.31834e-07 ,1.33596e-07 ,1.35283e-07 , &
      1.36947e-07 ,1.38594e-07 ,1.40362e-07 ,1.42131e-07 ,1.43823e-07 , &
      1.45592e-07 /)
      totplnkderiv(1:50,11) = (/ &
      2.25919e-10 ,2.43810e-10 ,2.62866e-10 ,2.83125e-10 ,3.04676e-10 , &
      3.27536e-10 ,3.51796e-10 ,3.77498e-10 ,4.04714e-10 ,4.33528e-10 , &
      4.64000e-10 ,4.96185e-10 ,5.30165e-10 ,5.65999e-10 ,6.03749e-10 , &
      6.43579e-10 ,6.85479e-10 ,7.29517e-10 ,7.75810e-10 ,8.24440e-10 , &
      8.75520e-10 ,9.29065e-10 ,9.85175e-10 ,1.04405e-09 ,1.10562e-09 , &
      1.17005e-09 ,1.23742e-09 ,1.30780e-09 ,1.38141e-09 ,1.45809e-09 , &
      1.53825e-09 ,1.62177e-09 ,1.70884e-09 ,1.79942e-09 ,1.89390e-09 , &
      1.99205e-09 ,2.09429e-09 ,2.20030e-09 ,2.31077e-09 ,2.42510e-09 , &
      2.54410e-09 ,2.66754e-09 ,2.79529e-09 ,2.92777e-09 ,3.06498e-09 , &
      3.20691e-09 ,3.35450e-09 ,3.50653e-09 ,3.66427e-09 ,3.82723e-09 /)
      totplnkderiv(51:100,11) = (/ &
      3.99549e-09 ,4.16911e-09 ,4.34892e-09 ,4.53415e-09 ,4.72504e-09 , &
      4.92197e-09 ,5.12525e-09 ,5.33485e-09 ,5.55085e-09 ,5.77275e-09 , &
      6.00105e-09 ,6.23650e-09 ,6.47855e-09 ,6.72735e-09 ,6.98325e-09 , &
      7.24695e-09 ,7.51730e-09 ,7.79480e-09 ,8.07975e-09 ,8.37170e-09 , &
      8.67195e-09 ,8.98050e-09 ,9.29575e-09 ,9.61950e-09 ,9.95150e-09 , &
      1.02912e-08 ,1.06397e-08 ,1.09964e-08 ,1.13611e-08 ,1.17348e-08 , &
      1.21158e-08 ,1.25072e-08 ,1.29079e-08 ,1.33159e-08 ,1.37342e-08 , &
      1.41599e-08 ,1.45966e-08 ,1.50438e-08 ,1.54964e-08 ,1.59605e-08 , &
      1.64337e-08 ,1.69189e-08 ,1.74134e-08 ,1.79136e-08 ,1.84272e-08 , &
      1.89502e-08 ,1.94845e-08 ,2.00248e-08 ,2.05788e-08 ,2.11455e-08 /)
      totplnkderiv(101:150,11) = (/ &
      2.17159e-08 ,2.23036e-08 ,2.28983e-08 ,2.35033e-08 ,2.41204e-08 , &
      2.47485e-08 ,2.53860e-08 ,2.60331e-08 ,2.66891e-08 ,2.73644e-08 , &
      2.80440e-08 ,2.87361e-08 ,2.94412e-08 ,3.01560e-08 ,3.08805e-08 , &
      3.16195e-08 ,3.23690e-08 ,3.31285e-08 ,3.39015e-08 ,3.46820e-08 , &
      3.54770e-08 ,3.62805e-08 ,3.70960e-08 ,3.79295e-08 ,3.87715e-08 , &
      3.96185e-08 ,4.04860e-08 ,4.13600e-08 ,4.22500e-08 ,4.31490e-08 , &
      4.40610e-08 ,4.49810e-08 ,4.59205e-08 ,4.68650e-08 ,4.78260e-08 , &
      4.87970e-08 ,4.97790e-08 ,5.07645e-08 ,5.17730e-08 ,5.27960e-08 , &
      5.38285e-08 ,5.48650e-08 ,5.59205e-08 ,5.69960e-08 ,5.80690e-08 , &
      5.91570e-08 ,6.02640e-08 ,6.13750e-08 ,6.25015e-08 ,6.36475e-08 /)
      totplnkderiv(151:181,11) = (/ &
      6.47950e-08 ,6.59510e-08 ,6.71345e-08 ,6.83175e-08 ,6.95250e-08 , &
      7.07325e-08 ,7.19490e-08 ,7.31880e-08 ,7.44315e-08 ,7.56880e-08 , &
      7.69500e-08 ,7.82495e-08 ,7.95330e-08 ,8.08450e-08 ,8.21535e-08 , &
      8.34860e-08 ,8.48330e-08 ,8.61795e-08 ,8.75480e-08 ,8.89235e-08 , &
      9.03060e-08 ,9.17045e-08 ,9.31140e-08 ,9.45240e-08 ,9.59720e-08 , &
      9.74140e-08 ,9.88825e-08 ,1.00347e-07 ,1.01825e-07 ,1.03305e-07 , &
      1.04826e-07 /)
      totplnkderiv(1:50,12) = (/ &
      2.91689e-11 ,3.20300e-11 ,3.51272e-11 ,3.84803e-11 ,4.21014e-11 , &
      4.60107e-11 ,5.02265e-11 ,5.47685e-11 ,5.96564e-11 ,6.49111e-11 , &
      7.05522e-11 ,7.66060e-11 ,8.30974e-11 ,9.00441e-11 ,9.74820e-11 , &
      1.05435e-10 ,1.13925e-10 ,1.22981e-10 ,1.32640e-10 ,1.42933e-10 , &
      1.53882e-10 ,1.65527e-10 ,1.77903e-10 ,1.91054e-10 ,2.05001e-10 , &
      2.19779e-10 ,2.35448e-10 ,2.52042e-10 ,2.69565e-10 ,2.88128e-10 , &
      3.07714e-10 ,3.28370e-10 ,3.50238e-10 ,3.73235e-10 ,3.97433e-10 , &
      4.22964e-10 ,4.49822e-10 ,4.78042e-10 ,5.07721e-10 ,5.38915e-10 , &
      5.71610e-10 ,6.05916e-10 ,6.41896e-10 ,6.79600e-10 ,7.19110e-10 , &
      7.60455e-10 ,8.03625e-10 ,8.48870e-10 ,8.96080e-10 ,9.45490e-10 /)
      totplnkderiv(51:100,12) = (/ &
      9.96930e-10 ,1.05071e-09 ,1.10679e-09 ,1.16521e-09 ,1.22617e-09 , &
      1.28945e-09 ,1.35554e-09 ,1.42427e-09 ,1.49574e-09 ,1.56984e-09 , &
      1.64695e-09 ,1.72715e-09 ,1.81034e-09 ,1.89656e-09 ,1.98613e-09 , &
      2.07898e-09 ,2.17515e-09 ,2.27498e-09 ,2.37826e-09 ,2.48517e-09 , &
      2.59566e-09 ,2.71004e-09 ,2.82834e-09 ,2.95078e-09 ,3.07686e-09 , &
      3.20739e-09 ,3.34232e-09 ,3.48162e-09 ,3.62515e-09 ,3.77337e-09 , &
      3.92614e-09 ,4.08317e-09 ,4.24567e-09 ,4.41272e-09 ,4.58524e-09 , &
      4.76245e-09 ,4.94450e-09 ,5.13235e-09 ,5.32535e-09 ,5.52415e-09 , &
      5.72770e-09 ,5.93815e-09 ,6.15315e-09 ,6.37525e-09 ,6.60175e-09 , &
      6.83485e-09 ,7.07490e-09 ,7.32060e-09 ,7.57225e-09 ,7.83035e-09 /)
      totplnkderiv(101:150,12) = (/ &
      8.09580e-09 ,8.36620e-09 ,8.64410e-09 ,8.93110e-09 ,9.22170e-09 , &
      9.52055e-09 ,9.82595e-09 ,1.01399e-08 ,1.04613e-08 ,1.07878e-08 , &
      1.11223e-08 ,1.14667e-08 ,1.18152e-08 ,1.21748e-08 ,1.25410e-08 , &
      1.29147e-08 ,1.32948e-08 ,1.36858e-08 ,1.40827e-08 ,1.44908e-08 , &
      1.49040e-08 ,1.53284e-08 ,1.57610e-08 ,1.61995e-08 ,1.66483e-08 , &
      1.71068e-08 ,1.75714e-08 ,1.80464e-08 ,1.85337e-08 ,1.90249e-08 , &
      1.95309e-08 ,2.00407e-08 ,2.05333e-08 ,2.10929e-08 ,2.16346e-08 , &
      2.21829e-08 ,2.27402e-08 ,2.33112e-08 ,2.38922e-08 ,2.44802e-08 , &
      2.50762e-08 ,2.56896e-08 ,2.63057e-08 ,2.69318e-08 ,2.75705e-08 , &
      2.82216e-08 ,2.88787e-08 ,2.95505e-08 ,3.02335e-08 ,3.09215e-08 /)
      totplnkderiv(151:181,12) = (/ &
      3.16235e-08 ,3.23350e-08 ,3.30590e-08 ,3.37960e-08 ,3.45395e-08 , &
      3.52955e-08 ,3.60615e-08 ,3.68350e-08 ,3.76265e-08 ,3.84255e-08 , &
      3.92400e-08 ,4.00485e-08 ,4.08940e-08 ,4.17310e-08 ,4.25860e-08 , &
      4.34585e-08 ,4.43270e-08 ,4.52220e-08 ,4.61225e-08 ,4.70345e-08 , &
      4.79560e-08 ,4.89000e-08 ,4.98445e-08 ,5.07985e-08 ,5.17705e-08 , &
      5.27575e-08 ,5.37420e-08 ,5.47495e-08 ,5.57725e-08 ,5.68105e-08 , &
      5.78395e-08 /)
      totplnkderiv(1:50,13) = (/ &
      5.47482e-12 ,6.09637e-12 ,6.77874e-12 ,7.52703e-12 ,8.34784e-12 , &
      9.24486e-12 ,1.02246e-11 ,1.12956e-11 ,1.24615e-11 ,1.37321e-11 , &
      1.51131e-11 ,1.66129e-11 ,1.82416e-11 ,2.00072e-11 ,2.19187e-11 , &
      2.39828e-11 ,2.62171e-11 ,2.86290e-11 ,3.12283e-11 ,3.40276e-11 , &
      3.70433e-11 ,4.02847e-11 ,4.37738e-11 ,4.75070e-11 ,5.15119e-11 , &
      5.58120e-11 ,6.04059e-11 ,6.53208e-11 ,7.05774e-11 ,7.61935e-11 , &
      8.21832e-11 ,8.85570e-11 ,9.53575e-11 ,1.02592e-10 ,1.10298e-10 , &
      1.18470e-10 ,1.27161e-10 ,1.36381e-10 ,1.46161e-10 ,1.56529e-10 , &
      1.67521e-10 ,1.79142e-10 ,1.91423e-10 ,2.04405e-10 ,2.18123e-10 , &
      2.32608e-10 ,2.47889e-10 ,2.63994e-10 ,2.80978e-10 ,2.98843e-10 /)
      totplnkderiv(51:100,13) = (/ &
      3.17659e-10 ,3.37423e-10 ,3.58206e-10 ,3.80090e-10 ,4.02996e-10 , &
      4.27065e-10 ,4.52298e-10 ,4.78781e-10 ,5.06493e-10 ,5.35576e-10 , &
      5.65942e-10 ,5.97761e-10 ,6.31007e-10 ,6.65740e-10 ,7.02095e-10 , &
      7.39945e-10 ,7.79575e-10 ,8.20845e-10 ,8.63870e-10 ,9.08680e-10 , &
      9.55385e-10 ,1.00416e-09 ,1.05464e-09 ,1.10737e-09 ,1.16225e-09 , &
      1.21918e-09 ,1.27827e-09 ,1.33988e-09 ,1.40370e-09 ,1.46994e-09 , &
      1.53850e-09 ,1.60993e-09 ,1.68382e-09 ,1.76039e-09 ,1.83997e-09 , &
      1.92182e-09 ,2.00686e-09 ,2.09511e-09 ,2.18620e-09 ,2.28034e-09 , &
      2.37753e-09 ,2.47805e-09 ,2.58193e-09 ,2.68935e-09 ,2.80064e-09 , &
      2.91493e-09 ,3.03271e-09 ,3.15474e-09 ,3.27987e-09 ,3.40936e-09 /)
      totplnkderiv(101:150,13) = (/ &
      3.54277e-09 ,3.68019e-09 ,3.82173e-09 ,3.96703e-09 ,4.11746e-09 , &
      4.27104e-09 ,4.43020e-09 ,4.59395e-09 ,4.76060e-09 ,4.93430e-09 , &
      5.11085e-09 ,5.29280e-09 ,5.48055e-09 ,5.67300e-09 ,5.86950e-09 , &
      6.07160e-09 ,6.28015e-09 ,6.49295e-09 ,6.71195e-09 ,6.93455e-09 , &
      7.16470e-09 ,7.39985e-09 ,7.64120e-09 ,7.88885e-09 ,8.13910e-09 , &
      8.39930e-09 ,8.66535e-09 ,8.93600e-09 ,9.21445e-09 ,9.49865e-09 , &
      9.78845e-09 ,1.00856e-08 ,1.04361e-08 ,1.07018e-08 ,1.10164e-08 , &
      1.13438e-08 ,1.16748e-08 ,1.20133e-08 ,1.23575e-08 ,1.27117e-08 , &
      1.30708e-08 ,1.34383e-08 ,1.38138e-08 ,1.41985e-08 ,1.45859e-08 , &
      1.49846e-08 ,1.53879e-08 ,1.58042e-08 ,1.62239e-08 ,1.66529e-08 /)
      totplnkderiv(151:181,13) = (/ &
      1.70954e-08 ,1.75422e-08 ,1.79943e-08 ,1.84537e-08 ,1.89280e-08 , &
      1.94078e-08 ,1.98997e-08 ,2.03948e-08 ,2.08956e-08 ,2.14169e-08 , &
      2.19330e-08 ,2.24773e-08 ,2.30085e-08 ,2.35676e-08 ,2.41237e-08 , &
      2.46919e-08 ,2.52720e-08 ,2.58575e-08 ,2.64578e-08 ,2.70675e-08 , &
      2.76878e-08 ,2.83034e-08 ,2.89430e-08 ,2.95980e-08 ,3.02480e-08 , &
      3.09105e-08 ,3.15980e-08 ,3.22865e-08 ,3.29755e-08 ,3.36775e-08 , &
      3.43990e-08 /)
      totplnkderiv(1:50,14) = (/ &
      1.81489e-12 ,2.03846e-12 ,2.28659e-12 ,2.56071e-12 ,2.86352e-12 , &
      3.19789e-12 ,3.56668e-12 ,3.97211e-12 ,4.41711e-12 ,4.90616e-12 , &
      5.44153e-12 ,6.02790e-12 ,6.67001e-12 ,7.37018e-12 ,8.13433e-12 , &
      8.96872e-12 ,9.87526e-12 ,1.08601e-11 ,1.19328e-11 ,1.30938e-11 , &
      1.43548e-11 ,1.57182e-11 ,1.71916e-11 ,1.87875e-11 ,2.05091e-11 , &
      2.23652e-11 ,2.43627e-11 ,2.65190e-11 ,2.88354e-11 ,3.13224e-11 , &
      3.39926e-11 ,3.68664e-11 ,3.99372e-11 ,4.32309e-11 ,4.67496e-11 , &
      5.05182e-11 ,5.45350e-11 ,5.88268e-11 ,6.34126e-11 ,6.82878e-11 , &
      7.34973e-11 ,7.90201e-11 ,8.49075e-11 ,9.11725e-11 ,9.78235e-11 , &
      1.04856e-10 ,1.12342e-10 ,1.20278e-10 ,1.28680e-10 ,1.37560e-10 /)
      totplnkderiv(51:100,14) = (/ &
      1.46953e-10 ,1.56900e-10 ,1.67401e-10 ,1.78498e-10 ,1.90161e-10 , &
      2.02523e-10 ,2.15535e-10 ,2.29239e-10 ,2.43665e-10 ,2.58799e-10 , &
      2.74767e-10 ,2.91522e-10 ,3.09141e-10 ,3.27625e-10 ,3.47011e-10 , &
      3.67419e-10 ,3.88720e-10 ,4.11066e-10 ,4.34522e-10 ,4.59002e-10 , &
      4.84657e-10 ,5.11391e-10 ,5.39524e-10 ,5.68709e-10 ,5.99240e-10 , &
      6.31295e-10 ,6.64520e-10 ,6.99200e-10 ,7.35525e-10 ,7.73135e-10 , &
      8.12440e-10 ,8.53275e-10 ,8.95930e-10 ,9.40165e-10 ,9.86260e-10 , &
      1.03423e-09 ,1.08385e-09 ,1.13567e-09 ,1.18916e-09 ,1.24469e-09 , &
      1.30262e-09 ,1.36268e-09 ,1.42479e-09 ,1.48904e-09 ,1.55557e-09 , &
      1.62478e-09 ,1.69642e-09 ,1.77023e-09 ,1.84696e-09 ,1.92646e-09 /)
      totplnkderiv(101:150,14) = (/ &
      2.00831e-09 ,2.09299e-09 ,2.18007e-09 ,2.27093e-09 ,2.36398e-09 , &
      2.46020e-09 ,2.55985e-09 ,2.66230e-09 ,2.76795e-09 ,2.87667e-09 , &
      2.98971e-09 ,3.10539e-09 ,3.22462e-09 ,3.34779e-09 ,3.47403e-09 , &
      3.60419e-09 ,3.73905e-09 ,3.87658e-09 ,4.01844e-09 ,4.16535e-09 , &
      4.31470e-09 ,4.46880e-09 ,4.62765e-09 ,4.78970e-09 ,4.95735e-09 , &
      5.12890e-09 ,5.30430e-09 ,5.48595e-09 ,5.67010e-09 ,5.86145e-09 , &
      6.05740e-09 ,6.25725e-09 ,6.46205e-09 ,6.67130e-09 ,6.88885e-09 , &
      7.10845e-09 ,7.33450e-09 ,7.56700e-09 ,7.80440e-09 ,8.04465e-09 , &
      8.29340e-09 ,8.54820e-09 ,8.80790e-09 ,9.07195e-09 ,9.34605e-09 , &
      9.62005e-09 ,9.90685e-09 ,1.01939e-08 ,1.04938e-08 ,1.07957e-08 /)
      totplnkderiv(151:181,14) = (/ &
      1.11059e-08 ,1.14208e-08 ,1.17447e-08 ,1.20717e-08 ,1.24088e-08 , &
      1.27490e-08 ,1.31020e-08 ,1.34601e-08 ,1.38231e-08 ,1.41966e-08 , &
      1.45767e-08 ,1.49570e-08 ,1.53503e-08 ,1.57496e-08 ,1.61663e-08 , &
      1.65784e-08 ,1.70027e-08 ,1.74290e-08 ,1.78730e-08 ,1.83235e-08 , &
      1.87810e-08 ,1.92418e-08 ,1.97121e-08 ,2.01899e-08 ,2.05787e-08 , &
      2.11784e-08 ,2.16824e-08 ,2.21931e-08 ,2.27235e-08 ,2.32526e-08 , &
      2.37850e-08 /)
      totplnkderiv(1:50,15) = (/ &
      5.39905e-13 ,6.11835e-13 ,6.92224e-13 ,7.81886e-13 ,8.81851e-13 , &
      9.93072e-13 ,1.11659e-12 ,1.25364e-12 ,1.40562e-12 ,1.57359e-12 , &
      1.75937e-12 ,1.96449e-12 ,2.19026e-12 ,2.43892e-12 ,2.71249e-12 , &
      3.01233e-12 ,3.34163e-12 ,3.70251e-12 ,4.09728e-12 ,4.52885e-12 , &
      4.99939e-12 ,5.51242e-12 ,6.07256e-12 ,6.68167e-12 ,7.34274e-12 , &
      8.06178e-12 ,8.84185e-12 ,9.68684e-12 ,1.06020e-11 ,1.15909e-11 , &
      1.26610e-11 ,1.38158e-11 ,1.50620e-11 ,1.64047e-11 ,1.78508e-11 , &
      1.94055e-11 ,2.10805e-11 ,2.28753e-11 ,2.48000e-11 ,2.68699e-11 , &
      2.90824e-11 ,3.14526e-11 ,3.39882e-11 ,3.67020e-11 ,3.95914e-11 , &
      4.26870e-11 ,4.59824e-11 ,4.94926e-11 ,5.32302e-11 ,5.72117e-11 /)
      totplnkderiv(51:100,15) = (/ &
      6.14475e-11 ,6.59483e-11 ,7.07393e-11 ,7.57999e-11 ,8.11980e-11 , &
      8.68920e-11 ,9.29390e-11 ,9.93335e-11 ,1.06101e-10 ,1.13263e-10 , &
      1.20827e-10 ,1.28819e-10 ,1.37255e-10 ,1.46163e-10 ,1.55547e-10 , &
      1.65428e-10 ,1.75837e-10 ,1.86816e-10 ,1.98337e-10 ,2.10476e-10 , &
      2.23218e-10 ,2.36600e-10 ,2.50651e-10 ,2.65425e-10 ,2.80895e-10 , &
      2.97102e-10 ,3.14100e-10 ,3.31919e-10 ,3.50568e-10 ,3.70064e-10 , &
      3.90464e-10 ,4.11813e-10 ,4.34111e-10 ,4.57421e-10 ,4.81717e-10 , &
      5.07039e-10 ,5.33569e-10 ,5.61137e-10 ,5.89975e-10 ,6.19980e-10 , &
      6.51170e-10 ,6.83650e-10 ,7.17520e-10 ,7.52735e-10 ,7.89390e-10 , &
      8.27355e-10 ,8.66945e-10 ,9.08020e-10 ,9.50665e-10 ,9.95055e-10 /)
      totplnkderiv(101:150,15) = (/ &
      1.04101e-09 ,1.08864e-09 ,1.13823e-09 ,1.18923e-09 ,1.24257e-09 , &
      1.29741e-09 ,1.35442e-09 ,1.41347e-09 ,1.47447e-09 ,1.53767e-09 , &
      1.60322e-09 ,1.67063e-09 ,1.74033e-09 ,1.81256e-09 ,1.88704e-09 , &
      1.96404e-09 ,2.04329e-09 ,2.12531e-09 ,2.21032e-09 ,2.29757e-09 , &
      2.38739e-09 ,2.48075e-09 ,2.57628e-09 ,2.67481e-09 ,2.77627e-09 , &
      2.88100e-09 ,2.98862e-09 ,3.09946e-09 ,3.21390e-09 ,3.33105e-09 , &
      3.45185e-09 ,3.57599e-09 ,3.70370e-09 ,3.83512e-09 ,3.96909e-09 , &
      4.10872e-09 ,4.25070e-09 ,4.39605e-09 ,4.54670e-09 ,4.70015e-09 , &
      4.85850e-09 ,5.02050e-09 ,5.18655e-09 ,5.35815e-09 ,5.53180e-09 , &
      5.71225e-09 ,5.89495e-09 ,6.08260e-09 ,6.27485e-09 ,6.47345e-09 /)
      totplnkderiv(151:181,15) = (/ &
      6.67520e-09 ,6.88310e-09 ,7.09400e-09 ,7.31140e-09 ,7.53350e-09 , &
      7.76040e-09 ,7.99215e-09 ,8.22850e-09 ,8.47235e-09 ,8.71975e-09 , &
      8.97360e-09 ,9.23365e-09 ,9.49950e-09 ,9.76965e-09 ,1.00441e-08 , &
      1.03270e-08 ,1.06158e-08 ,1.09112e-08 ,1.12111e-08 ,1.15172e-08 , &
      1.18263e-08 ,1.21475e-08 ,1.24735e-08 ,1.28027e-08 ,1.32023e-08 , &
      1.34877e-08 ,1.38399e-08 ,1.42000e-08 ,1.45625e-08 ,1.49339e-08 , &
      1.53156e-08 /)
      totplnkderiv(1:50,16) = (/ &
      4.38799e-14 ,5.04835e-14 ,5.79773e-14 ,6.64627e-14 ,7.60706e-14 , &
      8.69213e-14 ,9.91554e-14 ,1.12932e-13 ,1.28419e-13 ,1.45809e-13 , &
      1.65298e-13 ,1.87109e-13 ,2.11503e-13 ,2.38724e-13 ,2.69058e-13 , &
      3.02878e-13 ,3.40423e-13 ,3.82128e-13 ,4.28390e-13 ,4.79625e-13 , &
      5.36292e-13 ,5.98933e-13 ,6.68066e-13 ,7.44216e-13 ,8.28159e-13 , &
      9.20431e-13 ,1.02180e-12 ,1.13307e-12 ,1.25504e-12 ,1.38863e-12 , &
      1.53481e-12 ,1.69447e-12 ,1.86896e-12 ,2.05903e-12 ,2.26637e-12 , &
      2.49193e-12 ,2.73736e-12 ,3.00416e-12 ,3.29393e-12 ,3.60781e-12 , &
      3.94805e-12 ,4.31675e-12 ,4.71543e-12 ,5.14627e-12 ,5.61226e-12 , &
      6.11456e-12 ,6.65585e-12 ,7.23969e-12 ,7.86811e-12 ,8.54456e-12 /)
      totplnkderiv(51:100,16) = (/ &
      9.27075e-12 ,1.00516e-11 ,1.08898e-11 ,1.17884e-11 ,1.27514e-11 , &
      1.37839e-11 ,1.48893e-11 ,1.60716e-11 ,1.73333e-11 ,1.86849e-11 , &
      2.01237e-11 ,2.16610e-11 ,2.33001e-11 ,2.50440e-11 ,2.69035e-11 , &
      2.88827e-11 ,3.09881e-11 ,3.32234e-11 ,3.55981e-11 ,3.81193e-11 , &
      4.07946e-11 ,4.36376e-11 ,4.66485e-11 ,4.98318e-11 ,5.32080e-11 , &
      5.67754e-11 ,6.05524e-11 ,6.45450e-11 ,6.87639e-11 ,7.32160e-11 , &
      7.79170e-11 ,8.28780e-11 ,8.81045e-11 ,9.36200e-11 ,9.94280e-11 , &
      1.05545e-10 ,1.11982e-10 ,1.18752e-10 ,1.25866e-10 ,1.33350e-10 , &
      1.41210e-10 ,1.49469e-10 ,1.58143e-10 ,1.67233e-10 ,1.76760e-10 , &
      1.86758e-10 ,1.97236e-10 ,2.08227e-10 ,2.19723e-10 ,2.31737e-10 /)
      totplnkderiv(101:150,16) = (/ &
      2.44329e-10 ,2.57503e-10 ,2.71267e-10 ,2.85647e-10 ,3.00706e-10 , &
      3.16391e-10 ,3.32807e-10 ,3.49887e-10 ,3.67748e-10 ,3.86369e-10 , &
      4.05746e-10 ,4.25984e-10 ,4.47060e-10 ,4.68993e-10 ,4.91860e-10 , &
      5.15601e-10 ,5.40365e-10 ,5.66085e-10 ,5.92855e-10 ,6.20640e-10 , &
      6.49605e-10 ,6.79585e-10 ,7.10710e-10 ,7.43145e-10 ,7.76805e-10 , &
      8.11625e-10 ,8.47800e-10 ,8.85300e-10 ,9.24220e-10 ,9.64550e-10 , &
      1.00623e-09 ,1.04957e-09 ,1.09429e-09 ,1.14079e-09 ,1.18882e-09 , &
      1.23848e-09 ,1.28986e-09 ,1.34301e-09 ,1.39796e-09 ,1.45493e-09 , &
      1.51372e-09 ,1.57440e-09 ,1.63702e-09 ,1.70173e-09 ,1.76874e-09 , &
      1.83753e-09 ,1.90898e-09 ,1.98250e-09 ,2.05836e-09 ,2.13646e-09 /)
      totplnkderiv(151:181,16) = (/ &
      2.21710e-09 ,2.30027e-09 ,2.38591e-09 ,2.47432e-09 ,2.56503e-09 , &
      2.65878e-09 ,2.75516e-09 ,2.85432e-09 ,2.95688e-09 ,3.06201e-09 , &
      3.17023e-09 ,3.28153e-09 ,3.39604e-09 ,3.51391e-09 ,3.63517e-09 , &
      3.75955e-09 ,3.88756e-09 ,4.01880e-09 ,4.15405e-09 ,4.29255e-09 , &
      4.43535e-09 ,4.58145e-09 ,4.73165e-09 ,4.88560e-09 ,5.04390e-09 , &
      5.20630e-09 ,5.37255e-09 ,5.54355e-09 ,5.71915e-09 ,5.89855e-09 , &
      6.08280e-09 /)
      totplk16deriv(1:50) = (/ &
      4.35811e-14 ,5.01270e-14 ,5.75531e-14 ,6.59588e-14 ,7.54735e-14 , &
      8.62147e-14 ,9.83225e-14 ,1.11951e-13 ,1.27266e-13 ,1.44456e-13 , &
      1.63715e-13 ,1.85257e-13 ,2.09343e-13 ,2.36209e-13 ,2.66136e-13 , &
      2.99486e-13 ,3.36493e-13 ,3.77582e-13 ,4.23146e-13 ,4.73578e-13 , &
      5.29332e-13 ,5.90936e-13 ,6.58891e-13 ,7.33710e-13 ,8.16135e-13 , &
      9.06705e-13 ,1.00614e-12 ,1.11524e-12 ,1.23477e-12 ,1.36561e-12 , &
      1.50871e-12 ,1.66488e-12 ,1.83552e-12 ,2.02123e-12 ,2.22375e-12 , &
      2.44389e-12 ,2.68329e-12 ,2.94338e-12 ,3.22570e-12 ,3.53129e-12 , &
      3.86236e-12 ,4.22086e-12 ,4.60827e-12 ,5.02666e-12 ,5.47890e-12 , &
      5.96595e-12 ,6.49057e-12 ,7.05592e-12 ,7.66401e-12 ,8.31821e-12 /)
      totplk16deriv(51:100) = (/ &
      9.01998e-12 ,9.77390e-12 ,1.05826e-11 ,1.14491e-11 ,1.23769e-11 , &
      1.33709e-11 ,1.44341e-11 ,1.55706e-11 ,1.67821e-11 ,1.80793e-11 , &
      1.94586e-11 ,2.09316e-11 ,2.25007e-11 ,2.41685e-11 ,2.59454e-11 , &
      2.78356e-11 ,2.98440e-11 ,3.19744e-11 ,3.42355e-11 ,3.66340e-11 , &
      3.91772e-11 ,4.18773e-11 ,4.47339e-11 ,4.77509e-11 ,5.09490e-11 , &
      5.43240e-11 ,5.78943e-11 ,6.16648e-11 ,6.56445e-11 ,6.98412e-11 , &
      7.42680e-11 ,7.89335e-11 ,8.38450e-11 ,8.90220e-11 ,9.44695e-11 , &
      1.00197e-10 ,1.06221e-10 ,1.12550e-10 ,1.19193e-10 ,1.26175e-10 , &
      1.33498e-10 ,1.41188e-10 ,1.49251e-10 ,1.57693e-10 ,1.66530e-10 , &
      1.75798e-10 ,1.85495e-10 ,1.95661e-10 ,2.06275e-10 ,2.17357e-10 /)
      totplk16deriv(101:150) = (/ &
      2.28959e-10 ,2.41085e-10 ,2.53739e-10 ,2.66944e-10 ,2.80755e-10 , &
      2.95121e-10 ,3.10141e-10 ,3.25748e-10 ,3.42057e-10 ,3.59026e-10 , &
      3.76668e-10 ,3.95066e-10 ,4.14211e-10 ,4.34111e-10 ,4.54818e-10 , &
      4.76295e-10 ,4.98681e-10 ,5.21884e-10 ,5.46000e-10 ,5.71015e-10 , &
      5.97065e-10 ,6.23965e-10 ,6.51865e-10 ,6.80905e-10 ,7.11005e-10 , &
      7.42100e-10 ,7.74350e-10 ,8.07745e-10 ,8.42355e-10 ,8.78185e-10 , &
      9.15130e-10 ,9.53520e-10 ,9.93075e-10 ,1.03415e-09 ,1.07649e-09 , &
      1.12021e-09 ,1.16539e-09 ,1.21207e-09 ,1.26025e-09 ,1.31014e-09 , &
      1.36156e-09 ,1.41453e-09 ,1.46909e-09 ,1.52540e-09 ,1.58368e-09 , &
      1.64334e-09 ,1.70527e-09 ,1.76888e-09 ,1.83442e-09 ,1.90182e-09 /)
      totplk16deriv(151:181) = (/ &
      1.97128e-09 ,2.04281e-09 ,2.11635e-09 ,2.19219e-09 ,2.26979e-09 , &
      2.34989e-09 ,2.43219e-09 ,2.51660e-09 ,2.60396e-09 ,2.69317e-09 , &
      2.78501e-09 ,2.87927e-09 ,2.97600e-09 ,3.07548e-09 ,3.17772e-09 , &
      3.28235e-09 ,3.38982e-09 ,3.49985e-09 ,3.61307e-09 ,3.72883e-09 , &
      3.84805e-09 ,3.96975e-09 ,4.09465e-09 ,4.22240e-09 ,4.35370e-09 , &
      4.48800e-09 ,4.62535e-09 ,4.76640e-09 ,4.91110e-09 ,5.05850e-09 , &
      5.20965e-09 /)
      end subroutine lwavplankderiv
      end module rrtmg_lw_setcoef_f
      module rrtmg_lw_init_f
      use rrlw_wvn_f
      use rrtmg_lw_setcoef_f, only: lwatmref, lwavplank, lwavplankderiv
      implicit none
      contains
      subroutine rrtmg_lw_ini(cpdair)
      use parrrtm_f, only : mg, nbndlw, ngptlw
      use rrlw_tbl_f, only: ntbl, tblint, pade, bpade, tau_tbl, exp_tbl, tfn_tbl
      use rrlw_vsn_f, only: hvrini, hnamini
      real , intent(in) :: cpdair
      integer :: itr, ibnd, igc, ig, ind, ipr
      integer :: igcsm, iprsm
      real :: wtsum, wtsm(mg)
      real :: tfn
      real , parameter :: expeps = 1.e-20
      hvrini = '$Revision: 1.1.1.2 $'
      call lwdatinit(cpdair)
      call lwcmbdat
      call lwcldpr
      call lwatmref
      call lwavplank
      call lwavplankderiv
      tau_tbl(0) = 0.0
      tau_tbl(ntbl) = 1.e10
      exp_tbl(0) = 1.0
      exp_tbl(ntbl) = expeps
      tfn_tbl(0) = 0.0
      tfn_tbl(ntbl) = 1.0
      bpade = 1.0 / pade
      do itr = 1, ntbl-1
         tfn = float(itr) / float(ntbl)
         tau_tbl(itr) = bpade * tfn / (1. - tfn)
         exp_tbl(itr) = exp(-tau_tbl(itr))
         if (exp_tbl(itr) .le. expeps) exp_tbl(itr) = expeps
         if (tau_tbl(itr) .lt. 0.06 ) then
            tfn_tbl(itr) = tau_tbl(itr)/6.
         else
            tfn_tbl(itr) = 1. -2. *((1. /tau_tbl(itr))-(exp_tbl(itr)/(1.-exp_tbl(itr))))
         endif
      enddo
      igcsm = 0
      do ibnd = 1,nbndlw
         iprsm = 0
         if (ngc(ibnd).lt.mg) then
            do igc = 1,ngc(ibnd)
               igcsm = igcsm + 1
               wtsum = 0.
               do ipr = 1, ngn(igcsm)
                  iprsm = iprsm + 1
                  wtsum = wtsum + wt(iprsm)
               enddo
               wtsm(igc) = wtsum
            enddo
            do ig = 1, ng(ibnd)
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = wt(ig)/wtsm(ngm(ind))
            enddo
         else
            do ig = 1, ng(ibnd)
               igcsm = igcsm + 1
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = 1.0
            enddo
         endif
      enddo
      call cmbgb1
      call cmbgb2
      call cmbgb3
      call cmbgb4
      call cmbgb5
      call cmbgb6
      call cmbgb7
      call cmbgb8
      call cmbgb9
      call cmbgb10
      call cmbgb11
      call cmbgb12
      call cmbgb13
      call cmbgb14
      call cmbgb15
      call cmbgb16
      end subroutine rrtmg_lw_ini
      subroutine lwdatinit(cpdair)
      use parrrtm_f, only : maxxsec, maxinpx
      use rrlw_con_f, only: heatfac, grav, planck, boltz, &
                          clight, avogad, alosmt, gascon, radcn1, radcn2, &
                          sbcnst, secdy
      use rrlw_vsn_f
      save
      real , intent(in) :: cpdair
      wavenum1(:) = (/ 10. , 350. , 500. , 630. , 700. , 820. , &
                      980. ,1080. ,1180. ,1390. ,1480. ,1800. , &
                     2080. ,2250. ,2380. ,2600. /)
      wavenum2(:) = (/350. , 500. , 630. , 700. , 820. , 980. , &
                     1080. ,1180. ,1390. ,1480. ,1800. ,2080. , &
                     2250. ,2380. ,2600. ,3250. /)
      delwave(:) = (/340. , 150. , 130. , 70. , 120. , 160. , &
                      100. , 100. , 210. , 90. , 320. , 280. , &
                      170. , 130. , 220. , 650. /)
      ng(:) = (/16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16/)
      nspa(:) = (/1,1,9,9,9,1,9,1,9,1,1,9,9,1,9,9/)
      nspb(:) = (/1,1,5,5,5,0,1,1,1,1,1,0,0,1,0,0/)
      nxmol = 4
      ixindx(1) = 1
      ixindx(2) = 2
      ixindx(3) = 3
      ixindx(4) = 4
      ixindx(5:maxinpx) = 0
      grav = 9.8066
      planck = 6.62606876e-27
      boltz = 1.3806503e-16
      clight = 2.99792458e+10
      avogad = 6.02214199e+23
      alosmt = 2.6867775e+19
      gascon = 8.31447200e+07
      radcn1 = 1.191042722e-12
      radcn2 = 1.4387752
      sbcnst = 5.670400e-04
      secdy = 8.6400e4
      heatfac = grav * secdy / (cpdair * 1.e2 )
      end subroutine lwdatinit
      subroutine lwcmbdat
      save
      ngc(:) = (/10,12,16,14,16,8,12,8,12,6,8,8,4,2,2,2/)
      ngs(:) = (/10,22,38,52,68,76,88,96,108,114,122,130,134,136,138,140/)
      ngm(:) = (/1,2,3,3,4,4,5,5,6,6,7,7,8,8,9,10, &
                 1,2,3,4,5,6,7,8,9,9,10,10,11,11,12,12, &
                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &
                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,14,14, &
                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &
                 1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8, &
                 1,1,2,2,3,4,5,6,7,8,9,10,11,11,12,12, &
                 1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8, &
                 1,2,3,4,5,6,7,8,9,9,10,10,11,11,12,12, &
                 1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6, &
                 1,2,3,3,4,4,5,5,6,6,7,7,7,8,8,8, &
                 1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8, &
                 1,1,1,2,2,2,3,3,3,3,4,4,4,4,4,4, &
                 1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2, &
                 1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2, &
                 1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2/)
      ngn(:) = (/1,1,2,2,2,2,2,2,1,1, &
                 1,1,1,1,1,1,1,1,2,2,2,2, &
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                 1,1,1,1,1,1,1,1,1,1,1,1,1,3, &
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                 2,2,2,2,2,2,2,2, &
                 2,2,1,1,1,1,1,1,1,1,2,2, &
                 2,2,2,2,2,2,2,2, &
                 1,1,1,1,1,1,1,1,2,2,2,2, &
                 2,2,2,2,4,4, &
                 1,1,2,2,2,2,3,3, &
                 1,1,1,1,2,2,4,4, &
                 3,3,4,6, &
                 8,8, &
                 8,8, &
                 4,12/)
      ngb(:) = (/1,1,1,1,1,1,1,1,1,1, &
                 2,2,2,2,2,2,2,2,2,2,2,2, &
                 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, &
                 4,4,4,4,4,4,4,4,4,4,4,4,4,4, &
                 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5, &
                 6,6,6,6,6,6,6,6, &
                 7,7,7,7,7,7,7,7,7,7,7,7, &
                 8,8,8,8,8,8,8,8, &
                 9,9,9,9,9,9,9,9,9,9,9,9, &
                 10,10,10,10,10,10, &
                 11,11,11,11,11,11,11,11, &
                 12,12,12,12,12,12,12,12, &
                 13,13,13,13, &
                 14,14, &
                 15,15, &
                 16,16/)
      wt(:) = (/ 0.1527534276 , 0.1491729617 , 0.1420961469 , &
                 0.1316886544 , 0.1181945205 , 0.1019300893 , &
                 0.0832767040 , 0.0626720116 , 0.0424925000 , &
                 0.0046269894 , 0.0038279891 , 0.0030260086 , &
                 0.0022199750 , 0.0014140010 , 0.0005330000 , &
                 0.0000750000 /)
      end subroutine lwcmbdat
      subroutine cmbgb1
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng1
      use rrlw_kg01_f, only: fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, ka_mn2, kb_mn2, &
                           selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumk1, sumk2, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(1)
               sumk = 0.
               do ipr = 1, ngn(igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(1)
               sumk = 0.
               do ipr = 1, ngn(igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(1)
            sumk1 = 0.
            sumk2 = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk1 = sumk1 + kao_mn2(jt,iprsm)*rwgt(iprsm)
               sumk2 = sumk2 + kbo_mn2(jt,iprsm)*rwgt(iprsm)
            enddo
            ka_mn2(jt,igc) = sumk1
            kb_mn2(jt,igc) = sumk2
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(1)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
      enddo
      end subroutine cmbgb1
      subroutine cmbgb2
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng2
      use rrlw_kg02_f, only: fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(2)
               sumk = 0.
               do ipr = 1, ngn(ngs(1)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+16)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(2)
               sumk = 0.
               do ipr = 1, ngn(ngs(1)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+16)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(2)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(1)+igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
      enddo
      end subroutine cmbgb2
      subroutine cmbgb3
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng3
      use rrlw_kg03_f, only: fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, ka_mn2o, kb_mn2o, &
                           selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(3)
                 sumk = 0.
                  do ipr = 1, ngn(ngs(2)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+32)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(3)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(2)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+32)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(3)
              sumk = 0.
               do ipr = 1, ngn(ngs(2)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao_mn2o(jn,jt,iprsm)*rwgt(iprsm+32)
               enddo
               ka_mn2o(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jn = 1,5
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(3)
              sumk = 0.
               do ipr = 1, ngn(ngs(2)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo_mn2o(jn,jt,iprsm)*rwgt(iprsm+32)
               enddo
               kb_mn2o(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(3)
            sumf = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(3)
            sumf = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefbo(iprsm,jp)
            enddo
            fracrefb(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb3
      subroutine cmbgb4
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng4
      use rrlw_kg04_f, only: fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(4)
                 sumk = 0.
                  do ipr = 1, ngn(ngs(3)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+48)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(4)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(3)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+48)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(4)
            sumf = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(4)
            sumf = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefbo(iprsm,jp)
            enddo
            fracrefb(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb4
      subroutine cmbgb5
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng5
      use rrlw_kg05_f, only: fracrefao, fracrefbo, kao, kbo, kao_mo3, ccl4o, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, ka_mo3, ccl4, &
                           selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(5)
                 sumk = 0.
                  do ipr = 1, ngn(ngs(4)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+64)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(5)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(4)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+64)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(5)
              sumk = 0.
               do ipr = 1, ngn(ngs(4)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao_mo3(jn,jt,iprsm)*rwgt(iprsm+64)
               enddo
               ka_mo3(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(5)
            sumf = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(5)
            sumf = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefbo(iprsm,jp)
            enddo
            fracrefb(igc,jp) = sumf
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(5)
         sumk = 0.
         do ipr = 1, ngn(ngs(4)+igc)
            iprsm = iprsm + 1
            sumk = sumk + ccl4o(iprsm)*rwgt(iprsm+64)
         enddo
         ccl4(igc) = sumk
      enddo
      end subroutine cmbgb5
      subroutine cmbgb6
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng6
      use rrlw_kg06_f, only: fracrefao, kao, kao_mco2, cfc11adjo, cfc12o, &
                           selfrefo, forrefo, &
                           fracrefa, absa, ka, ka_mco2, cfc11adj, cfc12, &
                           selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumf, sumk1, sumk2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(6)
               sumk = 0.
               do ipr = 1, ngn(ngs(5)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+80)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + kao_mco2(jt,iprsm)*rwgt(iprsm+80)
            enddo
            ka_mco2(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(6)
         sumf = 0.
         sumk1= 0.
         sumk2= 0.
         do ipr = 1, ngn(ngs(5)+igc)
            iprsm = iprsm + 1
            sumf = sumf + fracrefao(iprsm)
            sumk1= sumk1+ cfc11adjo(iprsm)*rwgt(iprsm+80)
            sumk2= sumk2+ cfc12o(iprsm)*rwgt(iprsm+80)
         enddo
         fracrefa(igc) = sumf
         cfc11adj(igc) = sumk1
         cfc12(igc) = sumk2
      enddo
      end subroutine cmbgb6
      subroutine cmbgb7
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng7
      use rrlw_kg07_f, only: fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, ka_mco2, kb_mco2, &
                           selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(7)
                 sumk = 0.
                  do ipr = 1, ngn(ngs(6)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+96)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(7)
               sumk = 0.
               do ipr = 1, ngn(ngs(6)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+96)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(7)
              sumk = 0.
               do ipr = 1, ngn(ngs(6)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao_mco2(jn,jt,iprsm)*rwgt(iprsm+96)
               enddo
               ka_mco2(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + kbo_mco2(jt,iprsm)*rwgt(iprsm+96)
            enddo
            kb_mco2(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(7)
            sumf = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(7)
         sumf = 0.
         do ipr = 1, ngn(ngs(6)+igc)
            iprsm = iprsm + 1
            sumf = sumf + fracrefbo(iprsm)
         enddo
         fracrefb(igc) = sumf
      enddo
      end subroutine cmbgb7
      subroutine cmbgb8
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng8
      use rrlw_kg08_f, only: fracrefao, fracrefbo, kao, kao_mco2, kao_mn2o, &
                           kao_mo3, kbo, kbo_mco2, kbo_mn2o, selfrefo, forrefo, &
                           cfc12o, cfc22adjo, &
                           fracrefa, fracrefb, absa, ka, ka_mco2, ka_mn2o, &
                           ka_mo3, absb, kb, kb_mco2, kb_mn2o, selfref, forref, &
                           cfc12, cfc22adj
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumk1, sumk2, sumk3, sumk4, sumk5, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(8)
              sumk = 0.
               do ipr = 1, ngn(ngs(7)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+112)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(8)
               sumk = 0.
               do ipr = 1, ngn(ngs(7)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+112)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(8)
            sumk1 = 0.
            sumk2 = 0.
            sumk3 = 0.
            sumk4 = 0.
            sumk5 = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk1 = sumk1 + kao_mco2(jt,iprsm)*rwgt(iprsm+112)
               sumk2 = sumk2 + kbo_mco2(jt,iprsm)*rwgt(iprsm+112)
               sumk3 = sumk3 + kao_mo3(jt,iprsm)*rwgt(iprsm+112)
               sumk4 = sumk4 + kao_mn2o(jt,iprsm)*rwgt(iprsm+112)
               sumk5 = sumk5 + kbo_mn2o(jt,iprsm)*rwgt(iprsm+112)
            enddo
            ka_mco2(jt,igc) = sumk1
            kb_mco2(jt,igc) = sumk2
            ka_mo3(jt,igc) = sumk3
            ka_mn2o(jt,igc) = sumk4
            kb_mn2o(jt,igc) = sumk5
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(8)
         sumf1= 0.
         sumf2= 0.
         sumk1= 0.
         sumk2= 0.
         do ipr = 1, ngn(ngs(7)+igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
            sumk1= sumk1+ cfc12o(iprsm)*rwgt(iprsm+112)
            sumk2= sumk2+ cfc22adjo(iprsm)*rwgt(iprsm+112)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
         cfc12(igc) = sumk1
         cfc22adj(igc) = sumk2
      enddo
      end subroutine cmbgb8
      subroutine cmbgb9
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng9
      use rrlw_kg09_f, only: fracrefao, fracrefbo, kao, kao_mn2o, &
                           kbo, kbo_mn2o, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, ka_mn2o, &
                           absb, kb, kb_mn2o, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(9)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(8)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+128)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(9)
               sumk = 0.
               do ipr = 1, ngn(ngs(8)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+128)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(9)
              sumk = 0.
               do ipr = 1, ngn(ngs(8)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao_mn2o(jn,jt,iprsm)*rwgt(iprsm+128)
               enddo
               ka_mn2o(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + kbo_mn2o(jt,iprsm)*rwgt(iprsm+128)
            enddo
            kb_mn2o(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(9)
            sumf = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(9)
         sumf = 0.
         do ipr = 1, ngn(ngs(8)+igc)
            iprsm = iprsm + 1
            sumf = sumf + fracrefbo(iprsm)
         enddo
         fracrefb(igc) = sumf
      enddo
      end subroutine cmbgb9
      subroutine cmbgb10
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng10
      use rrlw_kg10_f, only: fracrefao, fracrefbo, kao, kbo, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, &
                           selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(10)
               sumk = 0.
               do ipr = 1, ngn(ngs(9)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+144)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(10)
               sumk = 0.
               do ipr = 1, ngn(ngs(9)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+144)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(10)
            sumk = 0.
            do ipr = 1, ngn(ngs(9)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+144)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(10)
            sumk = 0.
            do ipr = 1, ngn(ngs(9)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+144)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(10)
         sumf1= 0.
         sumf2= 0.
         do ipr = 1, ngn(ngs(9)+igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
      enddo
      end subroutine cmbgb10
      subroutine cmbgb11
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng11
      use rrlw_kg11_f, only: fracrefao, fracrefbo, kao, kao_mo2, &
                           kbo, kbo_mo2, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, ka_mo2, &
                           absb, kb, kb_mo2, selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumk1, sumk2, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(11)
               sumk = 0.
               do ipr = 1, ngn(ngs(10)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+160)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(11)
               sumk = 0.
               do ipr = 1, ngn(ngs(10)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+160)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(11)
            sumk1 = 0.
            sumk2 = 0.
            do ipr = 1, ngn(ngs(10)+igc)
               iprsm = iprsm + 1
               sumk1 = sumk1 + kao_mo2(jt,iprsm)*rwgt(iprsm+160)
               sumk2 = sumk2 + kbo_mo2(jt,iprsm)*rwgt(iprsm+160)
            enddo
            ka_mo2(jt,igc) = sumk1
            kb_mo2(jt,igc) = sumk2
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(11)
            sumk = 0.
            do ipr = 1, ngn(ngs(10)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+160)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(11)
            sumk = 0.
            do ipr = 1, ngn(ngs(10)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+160)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(11)
         sumf1= 0.
         sumf2= 0.
         do ipr = 1, ngn(ngs(10)+igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
      enddo
      end subroutine cmbgb11
      subroutine cmbgb12
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng12
      use rrlw_kg12_f, only: fracrefao, kao, selfrefo, forrefo, &
                           fracrefa, absa, ka, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(12)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(11)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+176)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(12)
            sumk = 0.
            do ipr = 1, ngn(ngs(11)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+176)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(12)
            sumk = 0.
            do ipr = 1, ngn(ngs(11)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+176)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(12)
            sumf = 0.
            do ipr = 1, ngn(ngs(11)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb12
      subroutine cmbgb13
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng13
      use rrlw_kg13_f, only: fracrefao, fracrefbo, kao, kao_mco2, kao_mco, &
                           kbo_mo3, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, ka_mco2, ka_mco, &
                           kb_mo3, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumk1, sumk2, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(13)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(12)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+192)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(13)
              sumk1 = 0.
              sumk2 = 0.
               do ipr = 1, ngn(ngs(12)+igc)
                  iprsm = iprsm + 1
                  sumk1 = sumk1 + kao_mco2(jn,jt,iprsm)*rwgt(iprsm+192)
                  sumk2 = sumk2 + kao_mco(jn,jt,iprsm)*rwgt(iprsm+192)
               enddo
               ka_mco2(jn,jt,igc) = sumk1
               ka_mco(jn,jt,igc) = sumk2
            enddo
         enddo
      enddo
      do jt = 1,19
         iprsm = 0
         do igc = 1,ngc(13)
            sumk = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumk = sumk + kbo_mo3(jt,iprsm)*rwgt(iprsm+192)
            enddo
            kb_mo3(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(13)
            sumk = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+192)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(13)
            sumk = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+192)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(13)
         sumf = 0.
         do ipr = 1, ngn(ngs(12)+igc)
            iprsm = iprsm + 1
            sumf = sumf + fracrefbo(iprsm)
         enddo
         fracrefb(igc) = sumf
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(13)
            sumf = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb13
      subroutine cmbgb14
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng14
      use rrlw_kg14_f, only: fracrefao, fracrefbo, kao, kbo, &
                           selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, &
                           selfref, forref
      integer :: jt, jp, igc, ipr, iprsm
      real :: sumk, sumf1, sumf2
      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(14)
               sumk = 0.
               do ipr = 1, ngn(ngs(13)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+208)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(14)
               sumk = 0.
               do ipr = 1, ngn(ngs(13)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+208)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(14)
         sumf1= 0.
         sumf2= 0.
         do ipr = 1, ngn(ngs(13)+igc)
            iprsm = iprsm + 1
            sumf1= sumf1+ fracrefao(iprsm)
            sumf2= sumf2+ fracrefbo(iprsm)
         enddo
         fracrefa(igc) = sumf1
         fracrefb(igc) = sumf2
      enddo
      end subroutine cmbgb14
      subroutine cmbgb15
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng15
      use rrlw_kg15_f, only: fracrefao, kao, kao_mn2, selfrefo, forrefo, &
                           fracrefa, absa, ka, ka_mn2, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(15)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(14)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+224)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jn = 1,9
         do jt = 1,19
            iprsm = 0
            do igc = 1,ngc(15)
              sumk = 0.
               do ipr = 1, ngn(ngs(14)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao_mn2(jn,jt,iprsm)*rwgt(iprsm+224)
               enddo
               ka_mn2(jn,jt,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(15)
            sumk = 0.
            do ipr = 1, ngn(ngs(14)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+224)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(15)
            sumk = 0.
            do ipr = 1, ngn(ngs(14)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+224)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(15)
            sumf = 0.
            do ipr = 1, ngn(ngs(14)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb15
      subroutine cmbgb16
      use parrrtm_f, only : mg, nbndlw, ngptlw, ng16
      use rrlw_kg16_f, only: fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo, &
                           fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
      integer :: jn, jt, jp, igc, ipr, iprsm
      real :: sumk, sumf
      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(16)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(15)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+240)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo
      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(16)
               sumk = 0.
               do ipr = 1, ngn(ngs(15)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+240)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo
      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(16)
            sumk = 0.
            do ipr = 1, ngn(ngs(15)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+240)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo
      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(16)
            sumk = 0.
            do ipr = 1, ngn(ngs(15)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+240)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo
      iprsm = 0
      do igc = 1,ngc(16)
         sumf = 0.
         do ipr = 1, ngn(ngs(15)+igc)
            iprsm = iprsm + 1
            sumf = sumf + fracrefbo(iprsm)
         enddo
         fracrefb(igc) = sumf
      enddo
      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(16)
            sumf = 0.
            do ipr = 1, ngn(ngs(15)+igc)
               iprsm = iprsm + 1
               sumf = sumf + fracrefao(iprsm,jp)
            enddo
            fracrefa(igc,jp) = sumf
         enddo
      enddo
      end subroutine cmbgb16
      subroutine lwcldpr
      use rrlw_cld_f, only: abscld1, absliq0, absliq1, &
                          absice0, absice1, absice2, absice3
      save
      abscld1 = 0.0602410
      absice0(:)= (/0.005 , 1.0 /)
      absice1(1,:) = (/0.0036 , 0.0068 , 0.0003 , 0.0016 , 0.0020 /)
      absice1(2,:) = (/1.136 , 0.600 , 1.338 , 1.166 , 1.118 /)
      absice2(:,1) = (/ &
       7.798999e-02 ,6.340479e-02 ,5.417973e-02 ,4.766245e-02 ,4.272663e-02 , &
       3.880939e-02 ,3.559544e-02 ,3.289241e-02 ,3.057511e-02 ,2.855800e-02 , &
       2.678022e-02 ,2.519712e-02 ,2.377505e-02 ,2.248806e-02 ,2.131578e-02 , &
       2.024194e-02 ,1.925337e-02 ,1.833926e-02 ,1.749067e-02 ,1.670007e-02 , &
       1.596113e-02 ,1.526845e-02 ,1.461739e-02 ,1.400394e-02 ,1.342462e-02 , &
       1.287639e-02 ,1.235656e-02 ,1.186279e-02 ,1.139297e-02 ,1.094524e-02 , &
       1.051794e-02 ,1.010956e-02 ,9.718755e-03 ,9.344316e-03 ,8.985139e-03 , &
       8.640223e-03 ,8.308656e-03 ,7.989606e-03 ,7.682312e-03 ,7.386076e-03 , &
       7.100255e-03 ,6.824258e-03 ,6.557540e-03 /)
      absice2(:,2) = (/ &
       2.784879e-02 ,2.709863e-02 ,2.619165e-02 ,2.529230e-02 ,2.443225e-02 , &
       2.361575e-02 ,2.284021e-02 ,2.210150e-02 ,2.139548e-02 ,2.071840e-02 , &
       2.006702e-02 ,1.943856e-02 ,1.883064e-02 ,1.824120e-02 ,1.766849e-02 , &
       1.711099e-02 ,1.656737e-02 ,1.603647e-02 ,1.551727e-02 ,1.500886e-02 , &
       1.451045e-02 ,1.402132e-02 ,1.354084e-02 ,1.306842e-02 ,1.260355e-02 , &
       1.214575e-02 ,1.169460e-02 ,1.124971e-02 ,1.081072e-02 ,1.037731e-02 , &
       9.949167e-03 ,9.526021e-03 ,9.107615e-03 ,8.693714e-03 ,8.284096e-03 , &
       7.878558e-03 ,7.476910e-03 ,7.078974e-03 ,6.684586e-03 ,6.293589e-03 , &
       5.905839e-03 ,5.521200e-03 ,5.139543e-03 /)
      absice2(:,3) = (/ &
       1.065397e-01 ,8.005726e-02 ,6.546428e-02 ,5.589131e-02 ,4.898681e-02 , &
       4.369932e-02 ,3.947901e-02 ,3.600676e-02 ,3.308299e-02 ,3.057561e-02 , &
       2.839325e-02 ,2.647040e-02 ,2.475872e-02 ,2.322164e-02 ,2.183091e-02 , &
       2.056430e-02 ,1.940407e-02 ,1.833586e-02 ,1.734787e-02 ,1.643034e-02 , &
       1.557512e-02 ,1.477530e-02 ,1.402501e-02 ,1.331924e-02 ,1.265364e-02 , &
       1.202445e-02 ,1.142838e-02 ,1.086257e-02 ,1.032445e-02 ,9.811791e-03 , &
       9.322587e-03 ,8.855053e-03 ,8.407591e-03 ,7.978763e-03 ,7.567273e-03 , &
       7.171949e-03 ,6.791728e-03 ,6.425642e-03 ,6.072809e-03 ,5.732424e-03 , &
       5.403748e-03 ,5.086103e-03 ,4.778865e-03 /)
      absice2(:,4) = (/ &
       1.804566e-01 ,1.168987e-01 ,8.680442e-02 ,6.910060e-02 ,5.738174e-02 , &
       4.902332e-02 ,4.274585e-02 ,3.784923e-02 ,3.391734e-02 ,3.068690e-02 , &
       2.798301e-02 ,2.568480e-02 ,2.370600e-02 ,2.198337e-02 ,2.046940e-02 , &
       1.912777e-02 ,1.793016e-02 ,1.685420e-02 ,1.588193e-02 ,1.499882e-02 , &
       1.419293e-02 ,1.345440e-02 ,1.277496e-02 ,1.214769e-02 ,1.156669e-02 , &
       1.102694e-02 ,1.052412e-02 ,1.005451e-02 ,9.614854e-03 ,9.202335e-03 , &
       8.814470e-03 ,8.449077e-03 ,8.104223e-03 ,7.778195e-03 ,7.469466e-03 , &
       7.176671e-03 ,6.898588e-03 ,6.634117e-03 ,6.382264e-03 ,6.142134e-03 , &
       5.912913e-03 ,5.693862e-03 ,5.484308e-03 /)
      absice2(:,5) = (/ &
       2.131806e-01 ,1.311372e-01 ,9.407171e-02 ,7.299442e-02 ,5.941273e-02 , &
       4.994043e-02 ,4.296242e-02 ,3.761113e-02 ,3.337910e-02 ,2.994978e-02 , &
       2.711556e-02 ,2.473461e-02 ,2.270681e-02 ,2.095943e-02 ,1.943839e-02 , &
       1.810267e-02 ,1.692057e-02 ,1.586719e-02 ,1.492275e-02 ,1.407132e-02 , &
       1.329989e-02 ,1.259780e-02 ,1.195618e-02 ,1.136761e-02 ,1.082583e-02 , &
       1.032552e-02 ,9.862158e-03 ,9.431827e-03 ,9.031157e-03 ,8.657217e-03 , &
       8.307449e-03 ,7.979609e-03 ,7.671724e-03 ,7.382048e-03 ,7.109032e-03 , &
       6.851298e-03 ,6.607615e-03 ,6.376881e-03 ,6.158105e-03 ,5.950394e-03 , &
       5.752942e-03 ,5.565019e-03 ,5.385963e-03 /)
      absice2(:,6) = (/ &
       1.546177e-01 ,1.039251e-01 ,7.910347e-02 ,6.412429e-02 ,5.399997e-02 , &
       4.664937e-02 ,4.104237e-02 ,3.660781e-02 ,3.300218e-02 ,3.000586e-02 , &
       2.747148e-02 ,2.529633e-02 ,2.340647e-02 ,2.174723e-02 ,2.027731e-02 , &
       1.896487e-02 ,1.778492e-02 ,1.671761e-02 ,1.574692e-02 ,1.485978e-02 , &
       1.404543e-02 ,1.329489e-02 ,1.260066e-02 ,1.195636e-02 ,1.135657e-02 , &
       1.079664e-02 ,1.027257e-02 ,9.780871e-03 ,9.318505e-03 ,8.882815e-03 , &
       8.471458e-03 ,8.082364e-03 ,7.713696e-03 ,7.363817e-03 ,7.031264e-03 , &
       6.714725e-03 ,6.413021e-03 ,6.125086e-03 ,5.849958e-03 ,5.586764e-03 , &
       5.334707e-03 ,5.093066e-03 ,4.861179e-03 /)
      absice2(:,7) = (/ &
       7.583404e-02 ,6.181558e-02 ,5.312027e-02 ,4.696039e-02 ,4.225986e-02 , &
       3.849735e-02 ,3.538340e-02 ,3.274182e-02 ,3.045798e-02 ,2.845343e-02 , &
       2.667231e-02 ,2.507353e-02 ,2.362606e-02 ,2.230595e-02 ,2.109435e-02 , &
       1.997617e-02 ,1.893916e-02 ,1.797328e-02 ,1.707016e-02 ,1.622279e-02 , &
       1.542523e-02 ,1.467241e-02 ,1.395997e-02 ,1.328414e-02 ,1.264164e-02 , &
       1.202958e-02 ,1.144544e-02 ,1.088697e-02 ,1.035218e-02 ,9.839297e-03 , &
       9.346733e-03 ,8.873057e-03 ,8.416980e-03 ,7.977335e-03 ,7.553066e-03 , &
       7.143210e-03 ,6.746888e-03 ,6.363297e-03 ,5.991700e-03 ,5.631422e-03 , &
       5.281840e-03 ,4.942378e-03 ,4.612505e-03 /)
      absice2(:,8) = (/ &
       9.022185e-02 ,6.922700e-02 ,5.710674e-02 ,4.898377e-02 ,4.305946e-02 , &
       3.849553e-02 ,3.484183e-02 ,3.183220e-02 ,2.929794e-02 ,2.712627e-02 , &
       2.523856e-02 ,2.357810e-02 ,2.210286e-02 ,2.078089e-02 ,1.958747e-02 , &
       1.850310e-02 ,1.751218e-02 ,1.660205e-02 ,1.576232e-02 ,1.498440e-02 , &
       1.426107e-02 ,1.358624e-02 ,1.295474e-02 ,1.236212e-02 ,1.180456e-02 , &
       1.127874e-02 ,1.078175e-02 ,1.031106e-02 ,9.864433e-03 ,9.439878e-03 , &
       9.035637e-03 ,8.650140e-03 ,8.281981e-03 ,7.929895e-03 ,7.592746e-03 , &
       7.269505e-03 ,6.959238e-03 ,6.661100e-03 ,6.374317e-03 ,6.098185e-03 , &
       5.832059e-03 ,5.575347e-03 ,5.327504e-03 /)
      absice2(:,9) = (/ &
       1.294087e-01 ,8.788217e-02 ,6.728288e-02 ,5.479720e-02 ,4.635049e-02 , &
       4.022253e-02 ,3.555576e-02 ,3.187259e-02 ,2.888498e-02 ,2.640843e-02 , &
       2.431904e-02 ,2.253038e-02 ,2.098024e-02 ,1.962267e-02 ,1.842293e-02 , &
       1.735426e-02 ,1.639571e-02 ,1.553060e-02 ,1.474552e-02 ,1.402953e-02 , &
       1.337363e-02 ,1.277033e-02 ,1.221336e-02 ,1.169741e-02 ,1.121797e-02 , &
       1.077117e-02 ,1.035369e-02 ,9.962643e-03 ,9.595509e-03 ,9.250088e-03 , &
       8.924447e-03 ,8.616876e-03 ,8.325862e-03 ,8.050057e-03 ,7.788258e-03 , &
       7.539388e-03 ,7.302478e-03 ,7.076656e-03 ,6.861134e-03 ,6.655197e-03 , &
       6.458197e-03 ,6.269543e-03 ,6.088697e-03 /)
      absice2(:,10) = (/ &
       1.593628e-01 ,1.014552e-01 ,7.458955e-02 ,5.903571e-02 ,4.887582e-02 , &
       4.171159e-02 ,3.638480e-02 ,3.226692e-02 ,2.898717e-02 ,2.631256e-02 , &
       2.408925e-02 ,2.221156e-02 ,2.060448e-02 ,1.921325e-02 ,1.799699e-02 , &
       1.692456e-02 ,1.597177e-02 ,1.511961e-02 ,1.435289e-02 ,1.365933e-02 , &
       1.302890e-02 ,1.245334e-02 ,1.192576e-02 ,1.144037e-02 ,1.099230e-02 , &
       1.057739e-02 ,1.019208e-02 ,9.833302e-03 ,9.498395e-03 ,9.185047e-03 , &
       8.891237e-03 ,8.615185e-03 ,8.355325e-03 ,8.110267e-03 ,7.878778e-03 , &
       7.659759e-03 ,7.452224e-03 ,7.255291e-03 ,7.068166e-03 ,6.890130e-03 , &
       6.720536e-03 ,6.558794e-03 ,6.404371e-03 /)
      absice2(:,11) = (/ &
       1.656227e-01 ,1.032129e-01 ,7.487359e-02 ,5.871431e-02 ,4.828355e-02 , &
       4.099989e-02 ,3.562924e-02 ,3.150755e-02 ,2.824593e-02 ,2.560156e-02 , &
       2.341503e-02 ,2.157740e-02 ,2.001169e-02 ,1.866199e-02 ,1.748669e-02 , &
       1.645421e-02 ,1.554015e-02 ,1.472535e-02 ,1.399457e-02 ,1.333553e-02 , &
       1.273821e-02 ,1.219440e-02 ,1.169725e-02 ,1.124104e-02 ,1.082096e-02 , &
       1.043290e-02 ,1.007336e-02 ,9.739338e-03 ,9.428223e-03 ,9.137756e-03 , &
       8.865964e-03 ,8.611115e-03 ,8.371686e-03 ,8.146330e-03 ,7.933852e-03 , &
       7.733187e-03 ,7.543386e-03 ,7.363597e-03 ,7.193056e-03 ,7.031072e-03 , &
       6.877024e-03 ,6.730348e-03 ,6.590531e-03 /)
      absice2(:,12) = (/ &
       9.194591e-02 ,6.446867e-02 ,4.962034e-02 ,4.042061e-02 ,3.418456e-02 , &
       2.968856e-02 ,2.629900e-02 ,2.365572e-02 ,2.153915e-02 ,1.980791e-02 , &
       1.836689e-02 ,1.714979e-02 ,1.610900e-02 ,1.520946e-02 ,1.442476e-02 , &
       1.373468e-02 ,1.312345e-02 ,1.257858e-02 ,1.209010e-02 ,1.164990e-02 , &
       1.125136e-02 ,1.088901e-02 ,1.055827e-02 ,1.025531e-02 ,9.976896e-03 , &
       9.720255e-03 ,9.483022e-03 ,9.263160e-03 ,9.058902e-03 ,8.868710e-03 , &
       8.691240e-03 ,8.525312e-03 ,8.369886e-03 ,8.224042e-03 ,8.086961e-03 , &
       7.957917e-03 ,7.836258e-03 ,7.721400e-03 ,7.612821e-03 ,7.510045e-03 , &
       7.412648e-03 ,7.320242e-03 ,7.232476e-03 /)
      absice2(:,13) = (/ &
       1.437021e-01 ,8.872535e-02 ,6.392420e-02 ,4.991833e-02 ,4.096790e-02 , &
       3.477881e-02 ,3.025782e-02 ,2.681909e-02 ,2.412102e-02 ,2.195132e-02 , &
       2.017124e-02 ,1.868641e-02 ,1.743044e-02 ,1.635529e-02 ,1.542540e-02 , &
       1.461388e-02 ,1.390003e-02 ,1.326766e-02 ,1.270395e-02 ,1.219860e-02 , &
       1.174326e-02 ,1.133107e-02 ,1.095637e-02 ,1.061442e-02 ,1.030126e-02 , &
       1.001352e-02 ,9.748340e-03 ,9.503256e-03 ,9.276155e-03 ,9.065205e-03 , &
       8.868808e-03 ,8.685571e-03 ,8.514268e-03 ,8.353820e-03 ,8.203272e-03 , &
       8.061776e-03 ,7.928578e-03 ,7.803001e-03 ,7.684443e-03 ,7.572358e-03 , &
       7.466258e-03 ,7.365701e-03 ,7.270286e-03 /)
      absice2(:,14) = (/ &
       1.288870e-01 ,8.160295e-02 ,5.964745e-02 ,4.703790e-02 ,3.888637e-02 , &
       3.320115e-02 ,2.902017e-02 ,2.582259e-02 ,2.330224e-02 ,2.126754e-02 , &
       1.959258e-02 ,1.819130e-02 ,1.700289e-02 ,1.598320e-02 ,1.509942e-02 , &
       1.432666e-02 ,1.364572e-02 ,1.304156e-02 ,1.250220e-02 ,1.201803e-02 , &
       1.158123e-02 ,1.118537e-02 ,1.082513e-02 ,1.049605e-02 ,1.019440e-02 , &
       9.916989e-03 ,9.661116e-03 ,9.424457e-03 ,9.205005e-03 ,9.001022e-03 , &
       8.810992e-03 ,8.633588e-03 ,8.467646e-03 ,8.312137e-03 ,8.166151e-03 , &
       8.028878e-03 ,7.899597e-03 ,7.777663e-03 ,7.662498e-03 ,7.553581e-03 , &
       7.450444e-03 ,7.352662e-03 ,7.259851e-03 /)
      absice2(:,15) = (/ &
       8.254229e-02 ,5.808787e-02 ,4.492166e-02 ,3.675028e-02 ,3.119623e-02 , &
       2.718045e-02 ,2.414450e-02 ,2.177073e-02 ,1.986526e-02 ,1.830306e-02 , &
       1.699991e-02 ,1.589698e-02 ,1.495199e-02 ,1.413374e-02 ,1.341870e-02 , &
       1.278883e-02 ,1.223002e-02 ,1.173114e-02 ,1.128322e-02 ,1.087900e-02 , &
       1.051254e-02 ,1.017890e-02 ,9.873991e-03 ,9.594347e-03 ,9.337044e-03 , &
       9.099589e-03 ,8.879842e-03 ,8.675960e-03 ,8.486341e-03 ,8.309594e-03 , &
       8.144500e-03 ,7.989986e-03 ,7.845109e-03 ,7.709031e-03 ,7.581007e-03 , &
       7.460376e-03 ,7.346544e-03 ,7.238978e-03 ,7.137201e-03 ,7.040780e-03 , &
       6.949325e-03 ,6.862483e-03 ,6.779931e-03 /)
      absice2(:,16) = (/ &
       1.382062e-01 ,8.643227e-02 ,6.282935e-02 ,4.934783e-02 ,4.063891e-02 , &
       3.455591e-02 ,3.007059e-02 ,2.662897e-02 ,2.390631e-02 ,2.169972e-02 , &
       1.987596e-02 ,1.834393e-02 ,1.703924e-02 ,1.591513e-02 ,1.493679e-02 , &
       1.407780e-02 ,1.331775e-02 ,1.264061e-02 ,1.203364e-02 ,1.148655e-02 , &
       1.099099e-02 ,1.054006e-02 ,1.012807e-02 ,9.750215e-03 ,9.402477e-03 , &
       9.081428e-03 ,8.784143e-03 ,8.508107e-03 ,8.251146e-03 ,8.011373e-03 , &
       7.787140e-03 ,7.577002e-03 ,7.379687e-03 ,7.194071e-03 ,7.019158e-03 , &
       6.854061e-03 ,6.697986e-03 ,6.550224e-03 ,6.410138e-03 ,6.277153e-03 , &
       6.150751e-03 ,6.030462e-03 ,5.915860e-03 /)
      absice3(:,1) = (/ &
       3.110649e-03 ,4.666352e-02 ,6.606447e-02 ,6.531678e-02 ,6.012598e-02 , &
       5.437494e-02 ,4.906411e-02 ,4.441146e-02 ,4.040585e-02 ,3.697334e-02 , &
       3.403027e-02 ,3.149979e-02 ,2.931596e-02 ,2.742365e-02 ,2.577721e-02 , &
       2.433888e-02 ,2.307732e-02 ,2.196644e-02 ,2.098437e-02 ,2.011264e-02 , &
       1.933561e-02 ,1.863992e-02 ,1.801407e-02 ,1.744812e-02 ,1.693346e-02 , &
       1.646252e-02 ,1.602866e-02 ,1.562600e-02 ,1.524933e-02 ,1.489399e-02 , &
       1.455580e-02 ,1.423098e-02 ,1.391612e-02 ,1.360812e-02 ,1.330413e-02 , &
       1.300156e-02 ,1.269801e-02 ,1.239127e-02 ,1.207928e-02 ,1.176014e-02 , &
       1.143204e-02 ,1.109334e-02 ,1.074243e-02 ,1.037786e-02 ,9.998198e-03 , &
       9.602126e-03 /)
      absice3(:,2) = (/ &
       3.984966e-04 ,1.681097e-02 ,2.627680e-02 ,2.767465e-02 ,2.700722e-02 , &
       2.579180e-02 ,2.448677e-02 ,2.323890e-02 ,2.209096e-02 ,2.104882e-02 , &
       2.010547e-02 ,1.925003e-02 ,1.847128e-02 ,1.775883e-02 ,1.710358e-02 , &
       1.649769e-02 ,1.593449e-02 ,1.540829e-02 ,1.491429e-02 ,1.444837e-02 , &
       1.400704e-02 ,1.358729e-02 ,1.318654e-02 ,1.280258e-02 ,1.243346e-02 , &
       1.207750e-02 ,1.173325e-02 ,1.139941e-02 ,1.107487e-02 ,1.075861e-02 , &
       1.044975e-02 ,1.014753e-02 ,9.851229e-03 ,9.560240e-03 ,9.274003e-03 , &
       8.992020e-03 ,8.713845e-03 ,8.439074e-03 ,8.167346e-03 ,7.898331e-03 , &
       7.631734e-03 ,7.367286e-03 ,7.104742e-03 ,6.843882e-03 ,6.584504e-03 , &
       6.326424e-03 /)
      absice3(:,3) = (/ &
       6.933163e-02 ,8.540475e-02 ,7.701816e-02 ,6.771158e-02 ,5.986953e-02 , &
       5.348120e-02 ,4.824962e-02 ,4.390563e-02 ,4.024411e-02 ,3.711404e-02 , &
       3.440426e-02 ,3.203200e-02 ,2.993478e-02 ,2.806474e-02 ,2.638464e-02 , &
       2.486516e-02 ,2.348288e-02 ,2.221890e-02 ,2.105780e-02 ,1.998687e-02 , &
       1.899552e-02 ,1.807490e-02 ,1.721750e-02 ,1.641693e-02 ,1.566773e-02 , &
       1.496515e-02 ,1.430509e-02 ,1.368398e-02 ,1.309865e-02 ,1.254634e-02 , &
       1.202456e-02 ,1.153114e-02 ,1.106409e-02 ,1.062166e-02 ,1.020224e-02 , &
       9.804381e-03 ,9.426771e-03 ,9.068205e-03 ,8.727578e-03 ,8.403876e-03 , &
       8.096160e-03 ,7.803564e-03 ,7.525281e-03 ,7.260560e-03 ,7.008697e-03 , &
       6.769036e-03 /)
      absice3(:,4) = (/ &
       1.765735e-01 ,1.382700e-01 ,1.095129e-01 ,8.987475e-02 ,7.591185e-02 , &
       6.554169e-02 ,5.755500e-02 ,5.122083e-02 ,4.607610e-02 ,4.181475e-02 , &
       3.822697e-02 ,3.516432e-02 ,3.251897e-02 ,3.021073e-02 ,2.817876e-02 , &
       2.637607e-02 ,2.476582e-02 ,2.331871e-02 ,2.201113e-02 ,2.082388e-02 , &
       1.974115e-02 ,1.874983e-02 ,1.783894e-02 ,1.699922e-02 ,1.622280e-02 , &
       1.550296e-02 ,1.483390e-02 ,1.421064e-02 ,1.362880e-02 ,1.308460e-02 , &
       1.257468e-02 ,1.209611e-02 ,1.164628e-02 ,1.122287e-02 ,1.082381e-02 , &
       1.044725e-02 ,1.009154e-02 ,9.755166e-03 ,9.436783e-03 ,9.135163e-03 , &
       8.849193e-03 ,8.577856e-03 ,8.320225e-03 ,8.075451e-03 ,7.842755e-03 , &
       7.621418e-03 /)
      absice3(:,5) = (/ &
       2.339673e-01 ,1.692124e-01 ,1.291656e-01 ,1.033837e-01 ,8.562949e-02 , &
       7.273526e-02 ,6.298262e-02 ,5.537015e-02 ,4.927787e-02 ,4.430246e-02 , &
       4.017061e-02 ,3.669072e-02 ,3.372455e-02 ,3.116995e-02 ,2.894977e-02 , &
       2.700471e-02 ,2.528842e-02 ,2.376420e-02 ,2.240256e-02 ,2.117959e-02 , &
       2.007567e-02 ,1.907456e-02 ,1.816271e-02 ,1.732874e-02 ,1.656300e-02 , &
       1.585725e-02 ,1.520445e-02 ,1.459852e-02 ,1.403419e-02 ,1.350689e-02 , &
       1.301260e-02 ,1.254781e-02 ,1.210941e-02 ,1.169468e-02 ,1.130118e-02 , &
       1.092675e-02 ,1.056945e-02 ,1.022757e-02 ,9.899560e-03 ,9.584021e-03 , &
       9.279705e-03 ,8.985479e-03 ,8.700322e-03 ,8.423306e-03 ,8.153590e-03 , &
       7.890412e-03 /)
      absice3(:,6) = (/ &
       1.145369e-01 ,1.174566e-01 ,9.917866e-02 ,8.332990e-02 ,7.104263e-02 , &
       6.153370e-02 ,5.405472e-02 ,4.806281e-02 ,4.317918e-02 ,3.913795e-02 , &
       3.574916e-02 ,3.287437e-02 ,3.041067e-02 ,2.828017e-02 ,2.642292e-02 , &
       2.479206e-02 ,2.335051e-02 ,2.206851e-02 ,2.092195e-02 ,1.989108e-02 , &
       1.895958e-02 ,1.811385e-02 ,1.734245e-02 ,1.663573e-02 ,1.598545e-02 , &
       1.538456e-02 ,1.482700e-02 ,1.430750e-02 ,1.382150e-02 ,1.336499e-02 , &
       1.293447e-02 ,1.252685e-02 ,1.213939e-02 ,1.176968e-02 ,1.141555e-02 , &
       1.107508e-02 ,1.074655e-02 ,1.042839e-02 ,1.011923e-02 ,9.817799e-03 , &
       9.522962e-03 ,9.233688e-03 ,8.949041e-03 ,8.668171e-03 ,8.390301e-03 , &
       8.114723e-03 /)
      absice3(:,7) = (/ &
       1.222345e-02 ,5.344230e-02 ,5.523465e-02 ,5.128759e-02 ,4.676925e-02 , &
       4.266150e-02 ,3.910561e-02 ,3.605479e-02 ,3.342843e-02 ,3.115052e-02 , &
       2.915776e-02 ,2.739935e-02 ,2.583499e-02 ,2.443266e-02 ,2.316681e-02 , &
       2.201687e-02 ,2.096619e-02 ,2.000112e-02 ,1.911044e-02 ,1.828481e-02 , &
       1.751641e-02 ,1.679866e-02 ,1.612598e-02 ,1.549360e-02 ,1.489742e-02 , &
       1.433392e-02 ,1.380002e-02 ,1.329305e-02 ,1.281068e-02 ,1.235084e-02 , &
       1.191172e-02 ,1.149171e-02 ,1.108936e-02 ,1.070341e-02 ,1.033271e-02 , &
       9.976220e-03 ,9.633021e-03 ,9.302273e-03 ,8.983216e-03 ,8.675161e-03 , &
       8.377478e-03 ,8.089595e-03 ,7.810986e-03 ,7.541170e-03 ,7.279706e-03 , &
       7.026186e-03 /)
      absice3(:,8) = (/ &
       6.711058e-02 ,6.918198e-02 ,6.127484e-02 ,5.411944e-02 ,4.836902e-02 , &
       4.375293e-02 ,3.998077e-02 ,3.683587e-02 ,3.416508e-02 ,3.186003e-02 , &
       2.984290e-02 ,2.805671e-02 ,2.645895e-02 ,2.501733e-02 ,2.370689e-02 , &
       2.250808e-02 ,2.140532e-02 ,2.038609e-02 ,1.944018e-02 ,1.855918e-02 , &
       1.773609e-02 ,1.696504e-02 ,1.624106e-02 ,1.555990e-02 ,1.491793e-02 , &
       1.431197e-02 ,1.373928e-02 ,1.319743e-02 ,1.268430e-02 ,1.219799e-02 , &
       1.173682e-02 ,1.129925e-02 ,1.088393e-02 ,1.048961e-02 ,1.011516e-02 , &
       9.759543e-03 ,9.421813e-03 ,9.101089e-03 ,8.796559e-03 ,8.507464e-03 , &
       8.233098e-03 ,7.972798e-03 ,7.725942e-03 ,7.491940e-03 ,7.270238e-03 , &
       7.060305e-03 /)
      absice3(:,9) = (/ &
       1.236780e-01 ,9.222386e-02 ,7.383997e-02 ,6.204072e-02 ,5.381029e-02 , &
       4.770678e-02 ,4.296928e-02 ,3.916131e-02 ,3.601540e-02 ,3.335878e-02 , &
       3.107493e-02 ,2.908247e-02 ,2.732282e-02 ,2.575276e-02 ,2.433968e-02 , &
       2.305852e-02 ,2.188966e-02 ,2.081757e-02 ,1.982974e-02 ,1.891599e-02 , &
       1.806794e-02 ,1.727865e-02 ,1.654227e-02 ,1.585387e-02 ,1.520924e-02 , &
       1.460476e-02 ,1.403730e-02 ,1.350416e-02 ,1.300293e-02 ,1.253153e-02 , &
       1.208808e-02 ,1.167094e-02 ,1.127862e-02 ,1.090979e-02 ,1.056323e-02 , &
       1.023786e-02 ,9.932665e-03 ,9.646744e-03 ,9.379250e-03 ,9.129409e-03 , &
       8.896500e-03 ,8.679856e-03 ,8.478852e-03 ,8.292904e-03 ,8.121463e-03 , &
       7.964013e-03 /)
      absice3(:,10) = (/ &
       1.655966e-01 ,1.134205e-01 ,8.714344e-02 ,7.129241e-02 ,6.063739e-02 , &
       5.294203e-02 ,4.709309e-02 ,4.247476e-02 ,3.871892e-02 ,3.559206e-02 , &
       3.293893e-02 ,3.065226e-02 ,2.865558e-02 ,2.689288e-02 ,2.532221e-02 , &
       2.391150e-02 ,2.263582e-02 ,2.147549e-02 ,2.041476e-02 ,1.944089e-02 , &
       1.854342e-02 ,1.771371e-02 ,1.694456e-02 ,1.622989e-02 ,1.556456e-02 , &
       1.494415e-02 ,1.436491e-02 ,1.382354e-02 ,1.331719e-02 ,1.284339e-02 , &
       1.239992e-02 ,1.198486e-02 ,1.159647e-02 ,1.123323e-02 ,1.089375e-02 , &
       1.057679e-02 ,1.028124e-02 ,1.000607e-02 ,9.750376e-03 ,9.513303e-03 , &
       9.294082e-03 ,9.092003e-03 ,8.906412e-03 ,8.736702e-03 ,8.582314e-03 , &
       8.442725e-03 /)
      absice3(:,11) = (/ &
       1.775615e-01 ,1.180046e-01 ,8.929607e-02 ,7.233500e-02 ,6.108333e-02 , &
       5.303642e-02 ,4.696927e-02 ,4.221206e-02 ,3.836768e-02 ,3.518576e-02 , &
       3.250063e-02 ,3.019825e-02 ,2.819758e-02 ,2.643943e-02 ,2.487953e-02 , &
       2.348414e-02 ,2.222705e-02 ,2.108762e-02 ,2.004936e-02 ,1.909892e-02 , &
       1.822539e-02 ,1.741975e-02 ,1.667449e-02 ,1.598330e-02 ,1.534084e-02 , &
       1.474253e-02 ,1.418446e-02 ,1.366325e-02 ,1.317597e-02 ,1.272004e-02 , &
       1.229321e-02 ,1.189350e-02 ,1.151915e-02 ,1.116859e-02 ,1.084042e-02 , &
       1.053338e-02 ,1.024636e-02 ,9.978326e-03 ,9.728357e-03 ,9.495613e-03 , &
       9.279327e-03 ,9.078798e-03 ,8.893383e-03 ,8.722488e-03 ,8.565568e-03 , &
       8.422115e-03 /)
      absice3(:,12) = (/ &
       9.465447e-02 ,6.432047e-02 ,5.060973e-02 ,4.267283e-02 ,3.741843e-02 , &
       3.363096e-02 ,3.073531e-02 ,2.842405e-02 ,2.651789e-02 ,2.490518e-02 , &
       2.351273e-02 ,2.229056e-02 ,2.120335e-02 ,2.022541e-02 ,1.933763e-02 , &
       1.852546e-02 ,1.777763e-02 ,1.708528e-02 ,1.644134e-02 ,1.584009e-02 , &
       1.527684e-02 ,1.474774e-02 ,1.424955e-02 ,1.377957e-02 ,1.333549e-02 , &
       1.291534e-02 ,1.251743e-02 ,1.214029e-02 ,1.178265e-02 ,1.144337e-02 , &
       1.112148e-02 ,1.081609e-02 ,1.052642e-02 ,1.025178e-02 ,9.991540e-03 , &
       9.745130e-03 ,9.512038e-03 ,9.291797e-03 ,9.083980e-03 ,8.888195e-03 , &
       8.704081e-03 ,8.531306e-03 ,8.369560e-03 ,8.218558e-03 ,8.078032e-03 , &
       7.947730e-03 /)
      absice3(:,13) = (/ &
       1.560311e-01 ,9.961097e-02 ,7.502949e-02 ,6.115022e-02 ,5.214952e-02 , &
       4.578149e-02 ,4.099731e-02 ,3.724174e-02 ,3.419343e-02 ,3.165356e-02 , &
       2.949251e-02 ,2.762222e-02 ,2.598073e-02 ,2.452322e-02 ,2.321642e-02 , &
       2.203516e-02 ,2.096002e-02 ,1.997579e-02 ,1.907036e-02 ,1.823401e-02 , &
       1.745879e-02 ,1.673819e-02 ,1.606678e-02 ,1.544003e-02 ,1.485411e-02 , &
       1.430574e-02 ,1.379215e-02 ,1.331092e-02 ,1.285996e-02 ,1.243746e-02 , &
       1.204183e-02 ,1.167164e-02 ,1.132567e-02 ,1.100281e-02 ,1.070207e-02 , &
       1.042258e-02 ,1.016352e-02 ,9.924197e-03 ,9.703953e-03 ,9.502199e-03 , &
       9.318400e-03 ,9.152066e-03 ,9.002749e-03 ,8.870038e-03 ,8.753555e-03 , &
       8.652951e-03 /)
      absice3(:,14) = (/ &
       1.559547e-01 ,9.896700e-02 ,7.441231e-02 ,6.061469e-02 ,5.168730e-02 , &
       4.537821e-02 ,4.064106e-02 ,3.692367e-02 ,3.390714e-02 ,3.139438e-02 , &
       2.925702e-02 ,2.740783e-02 ,2.578547e-02 ,2.434552e-02 ,2.305506e-02 , &
       2.188910e-02 ,2.082842e-02 ,1.985789e-02 ,1.896553e-02 ,1.814165e-02 , &
       1.737839e-02 ,1.666927e-02 ,1.600891e-02 ,1.539279e-02 ,1.481712e-02 , &
       1.427865e-02 ,1.377463e-02 ,1.330266e-02 ,1.286068e-02 ,1.244689e-02 , &
       1.205973e-02 ,1.169780e-02 ,1.135989e-02 ,1.104492e-02 ,1.075192e-02 , &
       1.048004e-02 ,1.022850e-02 ,9.996611e-03 ,9.783753e-03 ,9.589361e-03 , &
       9.412924e-03 ,9.253977e-03 ,9.112098e-03 ,8.986903e-03 ,8.878039e-03 , &
       8.785184e-03 /)
      absice3(:,15) = (/ &
       1.102926e-01 ,7.176622e-02 ,5.530316e-02 ,4.606056e-02 ,4.006116e-02 , &
       3.579628e-02 ,3.256909e-02 ,3.001360e-02 ,2.791920e-02 ,2.615617e-02 , &
       2.464023e-02 ,2.331426e-02 ,2.213817e-02 ,2.108301e-02 ,2.012733e-02 , &
       1.925493e-02 ,1.845331e-02 ,1.771269e-02 ,1.702531e-02 ,1.638493e-02 , &
       1.578648e-02 ,1.522579e-02 ,1.469940e-02 ,1.420442e-02 ,1.373841e-02 , &
       1.329931e-02 ,1.288535e-02 ,1.249502e-02 ,1.212700e-02 ,1.178015e-02 , &
       1.145348e-02 ,1.114612e-02 ,1.085730e-02 ,1.058633e-02 ,1.033263e-02 , &
       1.009564e-02 ,9.874895e-03 ,9.669960e-03 ,9.480449e-03 ,9.306014e-03 , &
       9.146339e-03 ,9.001138e-03 ,8.870154e-03 ,8.753148e-03 ,8.649907e-03 , &
       8.560232e-03 /)
      absice3(:,16) = (/ &
       1.688344e-01 ,1.077072e-01 ,7.994467e-02 ,6.403862e-02 ,5.369850e-02 , &
       4.641582e-02 ,4.099331e-02 ,3.678724e-02 ,3.342069e-02 ,3.065831e-02 , &
       2.834557e-02 ,2.637680e-02 ,2.467733e-02 ,2.319286e-02 ,2.188299e-02 , &
       2.071701e-02 ,1.967121e-02 ,1.872692e-02 ,1.786931e-02 ,1.708641e-02 , &
       1.636846e-02 ,1.570743e-02 ,1.509665e-02 ,1.453052e-02 ,1.400433e-02 , &
       1.351407e-02 ,1.305631e-02 ,1.262810e-02 ,1.222688e-02 ,1.185044e-02 , &
       1.149683e-02 ,1.116436e-02 ,1.085153e-02 ,1.055701e-02 ,1.027961e-02 , &
       1.001831e-02 ,9.772141e-03 ,9.540280e-03 ,9.321966e-03 ,9.116517e-03 , &
       8.923315e-03 ,8.741803e-03 ,8.571472e-03 ,8.411860e-03 ,8.262543e-03 , &
       8.123136e-03 /)
      absliq0 = 0.0903614
      absliq1(:, 1) = (/ &
       1.64047e-03 , 6.90533e-02 , 7.72017e-02 , 7.78054e-02 , 7.69523e-02 , &
       7.58058e-02 , 7.46400e-02 , 7.35123e-02 , 7.24162e-02 , 7.13225e-02 , &
       6.99145e-02 , 6.66409e-02 , 6.36582e-02 , 6.09425e-02 , 5.84593e-02 , &
       5.61743e-02 , 5.40571e-02 , 5.20812e-02 , 5.02245e-02 , 4.84680e-02 , &
       4.67959e-02 , 4.51944e-02 , 4.36516e-02 , 4.21570e-02 , 4.07015e-02 , &
       3.92766e-02 , 3.78747e-02 , 3.64886e-02 , 3.53632e-02 , 3.41992e-02 , &
       3.31016e-02 , 3.20643e-02 , 3.10817e-02 , 3.01490e-02 , 2.92620e-02 , &
       2.84171e-02 , 2.76108e-02 , 2.68404e-02 , 2.61031e-02 , 2.53966e-02 , &
       2.47189e-02 , 2.40678e-02 , 2.34418e-02 , 2.28392e-02 , 2.22586e-02 , &
       2.16986e-02 , 2.11580e-02 , 2.06356e-02 , 2.01305e-02 , 1.96417e-02 , &
       1.91682e-02 , 1.87094e-02 , 1.82643e-02 , 1.78324e-02 , 1.74129e-02 , &
       1.70052e-02 , 1.66088e-02 , 1.62231e-02 /)
      absliq1(:, 2) = (/ &
       2.19486e-01 , 1.80687e-01 , 1.59150e-01 , 1.44731e-01 , 1.33703e-01 , &
       1.24355e-01 , 1.15756e-01 , 1.07318e-01 , 9.86119e-02 , 8.92739e-02 , &
       8.34911e-02 , 7.70773e-02 , 7.15240e-02 , 6.66615e-02 , 6.23641e-02 , &
       5.85359e-02 , 5.51020e-02 , 5.20032e-02 , 4.91916e-02 , 4.66283e-02 , &
       4.42813e-02 , 4.21236e-02 , 4.01330e-02 , 3.82905e-02 , 3.65797e-02 , &
       3.49869e-02 , 3.35002e-02 , 3.21090e-02 , 3.08957e-02 , 2.97601e-02 , &
       2.86966e-02 , 2.76984e-02 , 2.67599e-02 , 2.58758e-02 , 2.50416e-02 , &
       2.42532e-02 , 2.35070e-02 , 2.27997e-02 , 2.21284e-02 , 2.14904e-02 , &
       2.08834e-02 , 2.03051e-02 , 1.97536e-02 , 1.92271e-02 , 1.87239e-02 , &
       1.82425e-02 , 1.77816e-02 , 1.73399e-02 , 1.69162e-02 , 1.65094e-02 , &
       1.61187e-02 , 1.57430e-02 , 1.53815e-02 , 1.50334e-02 , 1.46981e-02 , &
       1.43748e-02 , 1.40628e-02 , 1.37617e-02 /)
      absliq1(:, 3) = (/ &
       2.95174e-01 , 2.34765e-01 , 1.98038e-01 , 1.72114e-01 , 1.52083e-01 , &
       1.35654e-01 , 1.21613e-01 , 1.09252e-01 , 9.81263e-02 , 8.79448e-02 , &
       8.12566e-02 , 7.44563e-02 , 6.86374e-02 , 6.36042e-02 , 5.92094e-02 , &
       5.53402e-02 , 5.19087e-02 , 4.88455e-02 , 4.60951e-02 , 4.36124e-02 , &
       4.13607e-02 , 3.93096e-02 , 3.74338e-02 , 3.57119e-02 , 3.41261e-02 , &
       3.26610e-02 , 3.13036e-02 , 3.00425e-02 , 2.88497e-02 , 2.78077e-02 , &
       2.68317e-02 , 2.59158e-02 , 2.50545e-02 , 2.42430e-02 , 2.34772e-02 , &
       2.27533e-02 , 2.20679e-02 , 2.14181e-02 , 2.08011e-02 , 2.02145e-02 , &
       1.96561e-02 , 1.91239e-02 , 1.86161e-02 , 1.81311e-02 , 1.76673e-02 , &
       1.72234e-02 , 1.67981e-02 , 1.63903e-02 , 1.59989e-02 , 1.56230e-02 , &
       1.52615e-02 , 1.49138e-02 , 1.45791e-02 , 1.42565e-02 , 1.39455e-02 , &
       1.36455e-02 , 1.33559e-02 , 1.30761e-02 /)
      absliq1(:, 4) = (/ &
       3.00925e-01 , 2.36949e-01 , 1.96947e-01 , 1.68692e-01 , 1.47190e-01 , &
       1.29986e-01 , 1.15719e-01 , 1.03568e-01 , 9.30028e-02 , 8.36658e-02 , &
       7.71075e-02 , 7.07002e-02 , 6.52284e-02 , 6.05024e-02 , 5.63801e-02 , &
       5.27534e-02 , 4.95384e-02 , 4.66690e-02 , 4.40925e-02 , 4.17664e-02 , &
       3.96559e-02 , 3.77326e-02 , 3.59727e-02 , 3.43561e-02 , 3.28662e-02 , &
       3.14885e-02 , 3.02110e-02 , 2.90231e-02 , 2.78948e-02 , 2.69109e-02 , &
       2.59884e-02 , 2.51217e-02 , 2.43058e-02 , 2.35364e-02 , 2.28096e-02 , &
       2.21218e-02 , 2.14700e-02 , 2.08515e-02 , 2.02636e-02 , 1.97041e-02 , &
       1.91711e-02 , 1.86625e-02 , 1.81769e-02 , 1.77126e-02 , 1.72683e-02 , &
       1.68426e-02 , 1.64344e-02 , 1.60427e-02 , 1.56664e-02 , 1.53046e-02 , &
       1.49565e-02 , 1.46214e-02 , 1.42985e-02 , 1.39871e-02 , 1.36866e-02 , &
       1.33965e-02 , 1.31162e-02 , 1.28453e-02 /)
      absliq1(:, 5) = (/ &
       2.64691e-01 , 2.12018e-01 , 1.78009e-01 , 1.53539e-01 , 1.34721e-01 , &
       1.19580e-01 , 1.06996e-01 , 9.62772e-02 , 8.69710e-02 , 7.87670e-02 , &
       7.29272e-02 , 6.70920e-02 , 6.20977e-02 , 5.77732e-02 , 5.39910e-02 , &
       5.06538e-02 , 4.76866e-02 , 4.50301e-02 , 4.26374e-02 , 4.04704e-02 , &
       3.84981e-02 , 3.66948e-02 , 3.50394e-02 , 3.35141e-02 , 3.21038e-02 , &
       3.07957e-02 , 2.95788e-02 , 2.84438e-02 , 2.73790e-02 , 2.64390e-02 , &
       2.55565e-02 , 2.47263e-02 , 2.39437e-02 , 2.32047e-02 , 2.25056e-02 , &
       2.18433e-02 , 2.12149e-02 , 2.06177e-02 , 2.00495e-02 , 1.95081e-02 , &
       1.89917e-02 , 1.84984e-02 , 1.80269e-02 , 1.75755e-02 , 1.71431e-02 , &
       1.67283e-02 , 1.63303e-02 , 1.59478e-02 , 1.55801e-02 , 1.52262e-02 , &
       1.48853e-02 , 1.45568e-02 , 1.42400e-02 , 1.39342e-02 , 1.36388e-02 , &
       1.33533e-02 , 1.30773e-02 , 1.28102e-02 /)
      absliq1(:, 6) = (/ &
       8.81182e-02 , 1.06745e-01 , 9.79753e-02 , 8.99625e-02 , 8.35200e-02 , &
       7.81899e-02 , 7.35939e-02 , 6.94696e-02 , 6.56266e-02 , 6.19148e-02 , &
       5.83355e-02 , 5.49306e-02 , 5.19642e-02 , 4.93325e-02 , 4.69659e-02 , &
       4.48148e-02 , 4.28431e-02 , 4.10231e-02 , 3.93332e-02 , 3.77563e-02 , &
       3.62785e-02 , 3.48882e-02 , 3.35758e-02 , 3.23333e-02 , 3.11536e-02 , &
       3.00310e-02 , 2.89601e-02 , 2.79365e-02 , 2.70502e-02 , 2.62618e-02 , &
       2.55025e-02 , 2.47728e-02 , 2.40726e-02 , 2.34013e-02 , 2.27583e-02 , &
       2.21422e-02 , 2.15522e-02 , 2.09869e-02 , 2.04453e-02 , 1.99260e-02 , &
       1.94280e-02 , 1.89501e-02 , 1.84913e-02 , 1.80506e-02 , 1.76270e-02 , &
       1.72196e-02 , 1.68276e-02 , 1.64500e-02 , 1.60863e-02 , 1.57357e-02 , &
       1.53975e-02 , 1.50710e-02 , 1.47558e-02 , 1.44511e-02 , 1.41566e-02 , &
       1.38717e-02 , 1.35960e-02 , 1.33290e-02 /)
      absliq1(:, 7) = (/ &
       4.32174e-02 , 7.36078e-02 , 6.98340e-02 , 6.65231e-02 , 6.41948e-02 , &
       6.23551e-02 , 6.06638e-02 , 5.88680e-02 , 5.67124e-02 , 5.38629e-02 , &
       4.99579e-02 , 4.86289e-02 , 4.70120e-02 , 4.52854e-02 , 4.35466e-02 , &
       4.18480e-02 , 4.02169e-02 , 3.86658e-02 , 3.71992e-02 , 3.58168e-02 , &
       3.45155e-02 , 3.32912e-02 , 3.21390e-02 , 3.10538e-02 , 3.00307e-02 , &
       2.90651e-02 , 2.81524e-02 , 2.72885e-02 , 2.62821e-02 , 2.55744e-02 , &
       2.48799e-02 , 2.42029e-02 , 2.35460e-02 , 2.29108e-02 , 2.22981e-02 , &
       2.17079e-02 , 2.11402e-02 , 2.05945e-02 , 2.00701e-02 , 1.95663e-02 , &
       1.90824e-02 , 1.86174e-02 , 1.81706e-02 , 1.77411e-02 , 1.73281e-02 , &
       1.69307e-02 , 1.65483e-02 , 1.61801e-02 , 1.58254e-02 , 1.54835e-02 , &
       1.51538e-02 , 1.48358e-02 , 1.45288e-02 , 1.42322e-02 , 1.39457e-02 , &
       1.36687e-02 , 1.34008e-02 , 1.31416e-02 /)
      absliq1(:, 8) = (/ &
       1.41881e-01 , 7.15419e-02 , 6.30335e-02 , 6.11132e-02 , 6.01931e-02 , &
       5.92420e-02 , 5.78968e-02 , 5.58876e-02 , 5.28923e-02 , 4.84462e-02 , &
       4.60839e-02 , 4.56013e-02 , 4.45410e-02 , 4.31866e-02 , 4.17026e-02 , &
       4.01850e-02 , 3.86892e-02 , 3.72461e-02 , 3.58722e-02 , 3.45749e-02 , &
       3.33564e-02 , 3.22155e-02 , 3.11494e-02 , 3.01541e-02 , 2.92253e-02 , &
       2.83584e-02 , 2.75488e-02 , 2.67925e-02 , 2.57692e-02 , 2.50704e-02 , &
       2.43918e-02 , 2.37350e-02 , 2.31005e-02 , 2.24888e-02 , 2.18996e-02 , &
       2.13325e-02 , 2.07870e-02 , 2.02623e-02 , 1.97577e-02 , 1.92724e-02 , &
       1.88056e-02 , 1.83564e-02 , 1.79241e-02 , 1.75079e-02 , 1.71070e-02 , &
       1.67207e-02 , 1.63482e-02 , 1.59890e-02 , 1.56424e-02 , 1.53077e-02 , &
       1.49845e-02 , 1.46722e-02 , 1.43702e-02 , 1.40782e-02 , 1.37955e-02 , &
       1.35219e-02 , 1.32569e-02 , 1.30000e-02 /)
      absliq1(:, 9) = (/ &
       6.72726e-02 , 6.61013e-02 , 6.47866e-02 , 6.33780e-02 , 6.18985e-02 , &
       6.03335e-02 , 5.86136e-02 , 5.65876e-02 , 5.39839e-02 , 5.03536e-02 , &
       4.71608e-02 , 4.63630e-02 , 4.50313e-02 , 4.34526e-02 , 4.17876e-02 , &
       4.01261e-02 , 3.85171e-02 , 3.69860e-02 , 3.55442e-02 , 3.41954e-02 , &
       3.29384e-02 , 3.17693e-02 , 3.06832e-02 , 2.96745e-02 , 2.87374e-02 , &
       2.78662e-02 , 2.70557e-02 , 2.63008e-02 , 2.52450e-02 , 2.45424e-02 , &
       2.38656e-02 , 2.32144e-02 , 2.25885e-02 , 2.19873e-02 , 2.14099e-02 , &
       2.08554e-02 , 2.03230e-02 , 1.98116e-02 , 1.93203e-02 , 1.88482e-02 , &
       1.83944e-02 , 1.79578e-02 , 1.75378e-02 , 1.71335e-02 , 1.67440e-02 , &
       1.63687e-02 , 1.60069e-02 , 1.56579e-02 , 1.53210e-02 , 1.49958e-02 , &
       1.46815e-02 , 1.43778e-02 , 1.40841e-02 , 1.37999e-02 , 1.35249e-02 , &
       1.32585e-02 , 1.30004e-02 , 1.27502e-02 /)
      absliq1(:,10) = (/ &
       7.97040e-02 , 7.63844e-02 , 7.36499e-02 , 7.13525e-02 , 6.93043e-02 , &
       6.72807e-02 , 6.50227e-02 , 6.22395e-02 , 5.86093e-02 , 5.37815e-02 , &
       5.14682e-02 , 4.97214e-02 , 4.77392e-02 , 4.56961e-02 , 4.36858e-02 , &
       4.17569e-02 , 3.99328e-02 , 3.82224e-02 , 3.66265e-02 , 3.51416e-02 , &
       3.37617e-02 , 3.24798e-02 , 3.12887e-02 , 3.01812e-02 , 2.91505e-02 , &
       2.81900e-02 , 2.72939e-02 , 2.64568e-02 , 2.54165e-02 , 2.46832e-02 , &
       2.39783e-02 , 2.33017e-02 , 2.26531e-02 , 2.20314e-02 , 2.14359e-02 , &
       2.08653e-02 , 2.03187e-02 , 1.97947e-02 , 1.92924e-02 , 1.88106e-02 , &
       1.83483e-02 , 1.79043e-02 , 1.74778e-02 , 1.70678e-02 , 1.66735e-02 , &
       1.62941e-02 , 1.59286e-02 , 1.55766e-02 , 1.52371e-02 , 1.49097e-02 , &
       1.45937e-02 , 1.42885e-02 , 1.39936e-02 , 1.37085e-02 , 1.34327e-02 , &
       1.31659e-02 , 1.29075e-02 , 1.26571e-02 /)
      absliq1(:,11) = (/ &
       1.49438e-01 , 1.33535e-01 , 1.21542e-01 , 1.11743e-01 , 1.03263e-01 , &
       9.55774e-02 , 8.83382e-02 , 8.12943e-02 , 7.42533e-02 , 6.70609e-02 , &
       6.38761e-02 , 5.97788e-02 , 5.59841e-02 , 5.25318e-02 , 4.94132e-02 , &
       4.66014e-02 , 4.40644e-02 , 4.17706e-02 , 3.96910e-02 , 3.77998e-02 , &
       3.60742e-02 , 3.44947e-02 , 3.30442e-02 , 3.17079e-02 , 3.04730e-02 , &
       2.93283e-02 , 2.82642e-02 , 2.72720e-02 , 2.61789e-02 , 2.53277e-02 , &
       2.45237e-02 , 2.37635e-02 , 2.30438e-02 , 2.23615e-02 , 2.17140e-02 , &
       2.10987e-02 , 2.05133e-02 , 1.99557e-02 , 1.94241e-02 , 1.89166e-02 , &
       1.84317e-02 , 1.79679e-02 , 1.75238e-02 , 1.70983e-02 , 1.66901e-02 , &
       1.62983e-02 , 1.59219e-02 , 1.55599e-02 , 1.52115e-02 , 1.48761e-02 , &
       1.45528e-02 , 1.42411e-02 , 1.39402e-02 , 1.36497e-02 , 1.33690e-02 , &
       1.30976e-02 , 1.28351e-02 , 1.25810e-02 /)
      absliq1(:,12) = (/ &
       3.71985e-02 , 3.88586e-02 , 3.99070e-02 , 4.04351e-02 , 4.04610e-02 , &
       3.99834e-02 , 3.89953e-02 , 3.74886e-02 , 3.54551e-02 , 3.28870e-02 , &
       3.32576e-02 , 3.22444e-02 , 3.12384e-02 , 3.02584e-02 , 2.93146e-02 , &
       2.84120e-02 , 2.75525e-02 , 2.67361e-02 , 2.59618e-02 , 2.52280e-02 , &
       2.45327e-02 , 2.38736e-02 , 2.32487e-02 , 2.26558e-02 , 2.20929e-02 , &
       2.15579e-02 , 2.10491e-02 , 2.05648e-02 , 1.99749e-02 , 1.95704e-02 , &
       1.91731e-02 , 1.87839e-02 , 1.84032e-02 , 1.80315e-02 , 1.76689e-02 , &
       1.73155e-02 , 1.69712e-02 , 1.66362e-02 , 1.63101e-02 , 1.59928e-02 , &
       1.56842e-02 , 1.53840e-02 , 1.50920e-02 , 1.48080e-02 , 1.45318e-02 , &
       1.42631e-02 , 1.40016e-02 , 1.37472e-02 , 1.34996e-02 , 1.32586e-02 , &
       1.30239e-02 , 1.27954e-02 , 1.25728e-02 , 1.23559e-02 , 1.21445e-02 , &
       1.19385e-02 , 1.17376e-02 , 1.15417e-02 /)
      absliq1(:,13) = (/ &
       3.11868e-02 , 4.48357e-02 , 4.90224e-02 , 4.96406e-02 , 4.86806e-02 , &
       4.69610e-02 , 4.48630e-02 , 4.25795e-02 , 4.02138e-02 , 3.78236e-02 , &
       3.74266e-02 , 3.60384e-02 , 3.47074e-02 , 3.34434e-02 , 3.22499e-02 , &
       3.11264e-02 , 3.00704e-02 , 2.90784e-02 , 2.81463e-02 , 2.72702e-02 , &
       2.64460e-02 , 2.56698e-02 , 2.49381e-02 , 2.42475e-02 , 2.35948e-02 , &
       2.29774e-02 , 2.23925e-02 , 2.18379e-02 , 2.11793e-02 , 2.07076e-02 , &
       2.02470e-02 , 1.97981e-02 , 1.93613e-02 , 1.89367e-02 , 1.85243e-02 , &
       1.81240e-02 , 1.77356e-02 , 1.73588e-02 , 1.69935e-02 , 1.66392e-02 , &
       1.62956e-02 , 1.59624e-02 , 1.56393e-02 , 1.53259e-02 , 1.50219e-02 , &
       1.47268e-02 , 1.44404e-02 , 1.41624e-02 , 1.38925e-02 , 1.36302e-02 , &
       1.33755e-02 , 1.31278e-02 , 1.28871e-02 , 1.26530e-02 , 1.24253e-02 , &
       1.22038e-02 , 1.19881e-02 , 1.17782e-02 /)
      absliq1(:,14) = (/ &
       1.58988e-02 , 3.50652e-02 , 4.00851e-02 , 4.07270e-02 , 3.98101e-02 , &
       3.83306e-02 , 3.66829e-02 , 3.50327e-02 , 3.34497e-02 , 3.19609e-02 , &
       3.13712e-02 , 3.03348e-02 , 2.93415e-02 , 2.83973e-02 , 2.75037e-02 , &
       2.66604e-02 , 2.58654e-02 , 2.51161e-02 , 2.44100e-02 , 2.37440e-02 , &
       2.31154e-02 , 2.25215e-02 , 2.19599e-02 , 2.14282e-02 , 2.09242e-02 , &
       2.04459e-02 , 1.99915e-02 , 1.95594e-02 , 1.90254e-02 , 1.86598e-02 , &
       1.82996e-02 , 1.79455e-02 , 1.75983e-02 , 1.72584e-02 , 1.69260e-02 , &
       1.66013e-02 , 1.62843e-02 , 1.59752e-02 , 1.56737e-02 , 1.53799e-02 , &
       1.50936e-02 , 1.48146e-02 , 1.45429e-02 , 1.42782e-02 , 1.40203e-02 , &
       1.37691e-02 , 1.35243e-02 , 1.32858e-02 , 1.30534e-02 , 1.28270e-02 , &
       1.26062e-02 , 1.23909e-02 , 1.21810e-02 , 1.19763e-02 , 1.17766e-02 , &
       1.15817e-02 , 1.13915e-02 , 1.12058e-02 /)
      absliq1(:,15) = (/ &
       5.02079e-03 , 2.17615e-02 , 2.55449e-02 , 2.59484e-02 , 2.53650e-02 , &
       2.45281e-02 , 2.36843e-02 , 2.29159e-02 , 2.22451e-02 , 2.16716e-02 , &
       2.11451e-02 , 2.05817e-02 , 2.00454e-02 , 1.95372e-02 , 1.90567e-02 , &
       1.86028e-02 , 1.81742e-02 , 1.77693e-02 , 1.73866e-02 , 1.70244e-02 , &
       1.66815e-02 , 1.63563e-02 , 1.60477e-02 , 1.57544e-02 , 1.54755e-02 , &
       1.52097e-02 , 1.49564e-02 , 1.47146e-02 , 1.43684e-02 , 1.41728e-02 , &
       1.39762e-02 , 1.37797e-02 , 1.35838e-02 , 1.33891e-02 , 1.31961e-02 , &
       1.30051e-02 , 1.28164e-02 , 1.26302e-02 , 1.24466e-02 , 1.22659e-02 , &
       1.20881e-02 , 1.19131e-02 , 1.17412e-02 , 1.15723e-02 , 1.14063e-02 , &
       1.12434e-02 , 1.10834e-02 , 1.09264e-02 , 1.07722e-02 , 1.06210e-02 , &
       1.04725e-02 , 1.03269e-02 , 1.01839e-02 , 1.00436e-02 , 9.90593e-03 , &
       9.77080e-03 , 9.63818e-03 , 9.50800e-03 /)
      absliq1(:,16) = (/ &
       5.64971e-02 , 9.04736e-02 , 8.11726e-02 , 7.05450e-02 , 6.20052e-02 , &
       5.54286e-02 , 5.03503e-02 , 4.63791e-02 , 4.32290e-02 , 4.06959e-02 , &
       3.74690e-02 , 3.52964e-02 , 3.33799e-02 , 3.16774e-02 , 3.01550e-02 , &
       2.87856e-02 , 2.75474e-02 , 2.64223e-02 , 2.53953e-02 , 2.44542e-02 , &
       2.35885e-02 , 2.27894e-02 , 2.20494e-02 , 2.13622e-02 , 2.07222e-02 , &
       2.01246e-02 , 1.95654e-02 , 1.90408e-02 , 1.84398e-02 , 1.80021e-02 , &
       1.75816e-02 , 1.71775e-02 , 1.67889e-02 , 1.64152e-02 , 1.60554e-02 , &
       1.57089e-02 , 1.53751e-02 , 1.50531e-02 , 1.47426e-02 , 1.44428e-02 , &
       1.41532e-02 , 1.38734e-02 , 1.36028e-02 , 1.33410e-02 , 1.30875e-02 , &
       1.28420e-02 , 1.26041e-02 , 1.23735e-02 , 1.21497e-02 , 1.19325e-02 , &
       1.17216e-02 , 1.15168e-02 , 1.13177e-02 , 1.11241e-02 , 1.09358e-02 , &
       1.07525e-02 , 1.05741e-02 , 1.04003e-02 /)
      end subroutine lwcldpr
      end module rrtmg_lw_init_f
      module rrtmg_lw_rad_f
      use gpu_mcica_subcol_gen_lw
      use gpu_rrtmg_lw_rtrnmc
      use gpu_rrtmg_lw_setcoef
      use gpu_rrtmg_lw_cldprmc
      use gpu_rrtmg_lw_taumol, only: taumolg, copyGPUTaumol
      use rrlw_cld_f, only: abscld1, absliq0, absliq1, &
                          absice0, absice1, absice2, absice3
      use rrlw_wvn_f, only: ngb, ngs
      use rrlw_tbl_f, only: tblint, bpade, tau_tbl, exp_tbl, tfn_tbl, ntbl
      use rrlw_con_f, only: fluxfac, heatfac, oneminus, pi, grav, avogad
      use rrlw_vsn_f
      implicit none
      real :: timings(10)
      INTEGER, PARAMETER :: debug_level_lwf=100
      contains
      subroutine rrtmg_lw( &
             ncol ,nlay ,icld ,idrv , &
             play ,plev ,tlay ,tlev ,tsfc , &
             h2ovmr ,o3vmr ,co2vmr ,ch4vmr ,n2ovmr ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis , &
             inflglw ,iceflglw,liqflglw,cldfrac , &
             tauc ,ciwp ,clwp ,cswp ,rei ,rel , res , &
             tauaer , &
             uflx ,dflx ,hr ,uflxc ,dflxc ,hrc , &
             duflx_dt,duflxc_dt)
      use parrrtm_f, only : nbndlw, ngptlw, maxxsec, mxmol, mxlay, nbndlw
      use rrlw_con_f, only: fluxfac, heatfac, oneminus, pi
      use rrlw_wvn_f, only: ng, ngb, nspa, nspb, wavenum1, wavenum2, delwave
      integer , intent(in) :: ncol
      integer , intent(in) :: nlay
      integer , intent(inout) :: icld
      integer , intent(in) :: idrv
      real , intent(in) :: play(:,:)
      real , intent(in) :: plev(:,0:)
      real , intent(in) :: tlay(:,:)
      real , intent(in) :: tlev(:,0:)
      real , intent(in) :: tsfc(:)
      real , intent(in) :: h2ovmr(:,:)
      real , intent(in) :: o3vmr(:,:)
      real , intent(in) :: co2vmr(:,:)
      real , intent(in) :: ch4vmr(:,:)
      real , intent(in) :: n2ovmr(:,:)
      real , intent(in) :: o2vmr(:,:)
      real , intent(in) :: cfc11vmr(:, :)
      real , intent(in) :: cfc12vmr(:, :)
      real , intent(in) :: cfc22vmr(:, :)
      real , intent(in) :: ccl4vmr(:, :)
      real , intent(in) :: emis(:, :)
      integer , intent(in) :: inflglw
      integer , intent(in) :: iceflglw
      integer , intent(in) :: liqflglw
      real , intent(in) :: cldfrac(:,:)
      real , intent(in) :: ciwp(:,:)
      real , intent(in) :: clwp(:,:)
      real , intent(in) :: cswp(:,:)
      real , intent(in) :: rei(:,:)
      real , intent(in) :: rel(:, :)
      real , intent(in) :: res(:, :)
      real , intent(in) :: tauc(:, :, :)
      real , intent(in) :: tauaer(:,:,:)
      real , intent(out) :: uflx(:,:)
      real , intent(out) :: dflx(:,:)
      real , intent(out) :: hr(:,:)
      real , intent(out) :: uflxc(:,:)
      real , intent(out) :: dflxc(:,:)
      real , intent(out) :: hrc(:,:)
      real , intent(out), optional :: duflx_dt(:,:)
      real , intent(out), optional :: duflxc_dt(:,:)
      real, pointer :: alp(:,:)
      integer :: pncol
      integer :: colstart
      integer :: cn, ns, i, np, mns
      real :: minmem
      integer :: hetflag
      integer :: numDevices, err
      integer :: numThreads
integer,external :: omp_get_thread_num
      CHARACTER(LEN=256) :: message
      real gmem, cmem
      real t1,t2
      minmem = 2.0 * (1024.0**3)
      cn = 8
      WRITE(message,*)'RRTMG_LWF: Number of columns is               ',ncol
      call wrf_debug( debug_level_lwf, message)
      WRITE(message,*)'RRTMG_LWF: Number of columns per partition is ',cn
      call wrf_debug( debug_level_lwf, message)
      ns = ceiling( real(ncol) / real(cn) )
      WRITE(message,*)'RRTMG_LWF: Number of partitions is            ',ns
      call wrf_debug( debug_level_lwf, message)
      call cpu_time(t1)
      do i = 1, ns
      call rrtmg_lw_part &
            (ns, ncol, (i-1)*cn + 1, min(cn, ncol - (i-1)*cn), &
             nlay ,icld ,idrv,&
             play ,plev ,tlay ,tlev ,tsfc , &
             h2ovmr ,o3vmr ,co2vmr ,ch4vmr ,n2ovmr ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis , &
             inflglw ,iceflglw,liqflglw,cldfrac , &
             tauc ,ciwp ,clwp ,cswp ,rei ,rel ,res , &
             tauaer , &
             uflx ,dflx ,hr ,uflxc ,dflxc, hrc, &
             duflx_dt,duflxc_dt)
      end do
      call cpu_time(t2)
      WRITE(message,*)'------------------------------------------------'
      call wrf_debug( debug_level_lwf, message)
      WRITE(message,*)'TOTAL RRTMG_LWF RUN TIME IS   ', t2-t1
      call wrf_debug( debug_level_lwf, message)
      WRITE(message,*)'------------------------------------------------'
      call wrf_debug( debug_level_lwf, message)
      end subroutine
      subroutine rrtmg_lw_part &
            (npart, ncol , colstart, pncol , &
             nlay ,icld ,idrv , &
             play ,plev ,tlay ,tlev ,tsfc , &
             h2ovmr ,o3vmr ,co2vmr ,ch4vmr ,n2ovmr ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis , &
             inflglw ,iceflglw,liqflglw,cldfrac , &
             tauc ,ciwp ,clwp ,cswp ,rei ,rel ,res , &
             tauaer , &
             uflx ,dflx ,hr ,uflxc ,dflxc, hrc, &
             duflx_dt,duflxc_dt)
      use gpu_mcica_subcol_gen_lw, only: mcica_subcol_lwg, generate_stochastic_cloudsg
      use parrrtm_f, only : nbndlw, ngptlw, maxxsec, mxmol, mxlay, nbndlw, nmol
      use rrlw_con_f, only: fluxfac, heatfac, oneminus, pi
      use rrlw_wvn_f, only: ng, ngb, nspa, nspb, wavenum1, wavenum2, delwave, ixindx
      integer , intent(in) :: npart
      integer , intent(in) :: ncol
      integer , intent(in) :: nlay
      integer , intent(inout) :: icld
      integer , intent(in) :: idrv
      real , intent(in) :: play(:,:)
      real , intent(in) :: plev(:,0:)
      real , intent(in) :: tlay(:,:)
      real , intent(in) :: tlev(:,0:)
      real , intent(in) :: tsfc(:)
      real , intent(in) :: h2ovmr(:,:)
      real , intent(in) :: o3vmr(:,:)
      real , intent(in) :: co2vmr(:,:)
      real , intent(in) :: ch4vmr(:,:)
      real , intent(in) :: n2ovmr(:,:)
      real , intent(in) :: o2vmr(:,:)
      real , intent(in) :: cfc11vmr(:, :)
      real , intent(in) :: cfc12vmr(:, :)
      real , intent(in) :: cfc22vmr(:, :)
      real , intent(in) :: ccl4vmr(:, :)
      real , intent(in) :: emis(:, :)
      integer , intent(in) :: inflglw
      integer , intent(in) :: iceflglw
      integer , intent(in) :: liqflglw
      real , intent(in) :: cldfrac(:,:)
      real , intent(in) :: ciwp(:,:)
      real , intent(in) :: clwp(:,:)
      real , intent(in) :: cswp(:,:)
      real , intent(in) :: rei(:,:)
      real , intent(in) :: rel(:, :)
      real , intent(in) :: res(:, :)
      real , intent(in) :: tauc(:, :,:)
      real , intent(in) :: tauaer(:,:,:)
      integer , intent(in) :: pncol
      integer , intent(in) :: colstart
      real , intent(out) :: uflx(:,:)
      real , intent(out) :: dflx(:,:)
      real , intent(out) :: hr(:,:)
      real , intent(out) :: uflxc(:,:)
      real , intent(out) :: dflxc(:,:)
      real , intent(out) :: hrc(:,:)
      real , intent(out), optional :: duflx_dt(:,:)
      real , intent(out), optional :: duflxc_dt(:,:)
      real :: cldfmcd(8, ngptlw, nlay+1)
      integer ncol_,nlayers_,nbndlw_,ngptlw_
      integer ncol__,nlayers__,nbndlw__,ngptlw__
      real :: pmid(8, nlay)
      real :: relqmc(8, nlay+1), reicmc(8, nlay+1)
      real :: resnmc(8, nlay+1)
      real :: ciwpmcd(8, ngptlw, nlay+1)
      real :: clwpmcd(8, ngptlw, nlay+1)
      real :: cswpmcd(8, ngptlw, nlay+1)
      real :: taucmcd(8, ngptlw, nlay+1)
      real :: pzd(8, 0:nlay+1)
      real :: pwvcmd(8)
      real :: semissd(8, nbndlw)
      real :: planklayd(8,nlay+1,nbndlw)
      real :: planklevd(8, 0:nlay+1, nbndlw)
      real :: plankbndd(8,nbndlw)
      real :: gurad(8,ngptlw,0:nlay+1)
      real :: gdrad(8,ngptlw,0:nlay+1)
      real :: gclrurad(8,ngptlw,0:nlay+1)
      real :: gclrdrad(8,ngptlw,0:nlay+1)
      real :: gdtotuflux_dtd( 8, ngptlw, 0:nlay+1)
      real :: gdtotuclfl_dtd( 8, ngptlw, 0:nlay+1)
      real :: totufluxd(8, 0:nlay+1)
      real :: totdfluxd(8, 0:nlay+1)
      real :: fnetd(8, 0:nlay+1)
      real :: htrd(8, 0:nlay+1)
      real :: totuclfld(8, 0:nlay+1)
      real :: totdclfld(8, 0:nlay+1)
      real :: fnetcd(8, 0:nlay+1)
      real :: htrcd(8, 0:nlay+1)
      real :: dtotuflux_dtd(8, 0:nlay+1)
      real :: dtotuclfl_dtd(8, 0:nlay+1)
      real :: dplankbnd_dtd(8,nbndlw)
      real :: taveld( 8, nlay)
      real :: tzd( 8, 0:nlay)
      real :: tboundd( 8 )
      real :: wbroadd( 8, nlay)
      real :: wx1( 8, nlay )
      real :: wx2( 8, nlay )
      real :: wx3( 8, nlay )
      real :: wx4( 8, nlay )
      real :: tauaa( 8, nlay, nbndlw )
      integer :: icbd(16)
      integer :: ncbandsd(8)
      integer :: icldlyr(8, nlay+1)
      real :: fracsd( 8, nlay+1, ngptlw )
      real :: taug( 8, nlay+1, ngptlw )
      integer(kind=4) :: nlayers
      integer(kind=4) :: istart
      integer(kind=4) :: iend
      integer(kind=4) :: iout
      integer :: iaer
      integer(kind=4) :: iplon
      integer :: imca
      integer :: ims
      integer :: k
      integer :: ig
      real :: t1, t2
      real :: pavel(8,nlay+1)
      real :: tavel(8,nlay+1)
      real :: pz(8,0:nlay+1)
      real :: tz(8,0:nlay+1)
      real :: tbound(8)
      real :: coldry(8,nlay+1)
      real :: wbrodl(8,nlay+1)
      real :: wkl(8,mxmol,nlay+1)
      real :: wx(8,maxxsec,nlay+1)
      real :: pwvcm(8)
      real :: semiss(8,nbndlw)
      real :: fracs(8,nlay+1,ngptlw)
      real :: taut(8,nlay+1,ngptlw)
      real :: taua(8,nlay+1,nbndlw)
      integer :: laytrop(8)
      integer :: jp(8,nlay+1)
      integer :: jt(8,nlay+1)
      integer :: jt1(8,nlay+1)
      real :: planklay(8,nlay+1,nbndlw)
      real :: planklev(8,0:nlay+1,nbndlw)
      real :: plankbnd(8,nbndlw)
      real :: dplankbnd_dt(8,nbndlw)
      real :: colh2o(8,nlay+1)
      real :: colco2(8,nlay+1)
      real :: colo3(8,nlay+1)
      real :: coln2o(8,nlay+1)
      real :: colco(8,nlay+1)
      real :: colch4(8,nlay+1)
      real :: colo2(8,nlay+1)
      real :: colbrd(8,nlay+1)
      integer :: indself(8,nlay+1)
      integer :: indfor(8,nlay+1)
      real :: selffac(8,nlay+1)
      real :: selffrac(8,nlay+1)
      real :: forfac(8,nlay+1)
      real :: forfrac(8,nlay+1)
      integer :: indminor(8,nlay+1)
      real :: minorfrac(8,nlay+1)
      real :: scaleminor(8,nlay+1)
      real :: scaleminorn2(8,nlay+1)
      real :: &
                         fac00(8,nlay+1), fac01(8,nlay+1), &
                         fac10(8,nlay+1), fac11(8,nlay+1)
      real :: &
                         rat_h2oco2(8,nlay+1),rat_h2oco2_1(8,nlay+1), &
                         rat_h2oo3(8,nlay+1),rat_h2oo3_1(8,nlay+1), &
                         rat_h2on2o(8,nlay+1),rat_h2on2o_1(8,nlay+1), &
                         rat_h2och4(8,nlay+1),rat_h2och4_1(8,nlay+1), &
                         rat_n2oco2(8,nlay+1),rat_n2oco2_1(8,nlay+1), &
                         rat_o3co2(8,nlay+1),rat_o3co2_1(8,nlay+1)
      integer :: ncbands(8)
      integer :: inflag(8)
      integer :: iceflag(8)
      integer :: liqflag(8)
      real :: totuflux(8,0:nlay+1)
      real :: totdflux(8,0:nlay+1)
      real :: fnet(8,0:nlay+1)
      real :: htr(8,0:nlay+1)
      real :: totuclfl(8,0:nlay+1)
      real :: totdclfl(8,0:nlay+1)
      real :: fnetc(8,0:nlay+1)
      real :: htrc(8,0:nlay+1)
      real :: dtotuflux_dt(8,0:nlay+1)
      real :: dtotuclfl_dt(8,0:nlay+1)
      real :: curad(8,ngptlw,0:nlay+1)
      real :: cdrad(8,ngptlw,0:nlay+1)
      real :: cclrurad(8,ngptlw,0:nlay+1)
      real :: cclrdrad(8,ngptlw,0:nlay+1)
      real :: cldfracq(8,mxlay+1)
      real :: ciwpq(8,mxlay+1)
      real :: clwpq(8,mxlay+1)
      real :: cswpq(8,mxlay+1)
      real :: reiq(8,mxlay)
      real :: relq(8, mxlay)
      real :: resq(8, mxlay)
      real :: taucq(8, nbndlw, mxlay)
      real :: tauaq(8, mxlay, nbndlw)
      integer :: permuteseed
      integer :: icb(16)
      integer :: i,j,kk, piplon
          integer :: ierr
      real , dimension(16) :: a0 =(/ 1.66 , 1.55 , 1.58 , 1.66 , &
                1.54 , 1.454 , 1.89 , 1.33 , &
               1.668 , 1.66 , 1.66 , 1.66 , &
                1.66 , 1.66 , 1.66 , 1.66 /)
      real , dimension(16) :: a1=(/ 0.00 , 0.25 , 0.22 , 0.00 , &
                0.13 , 0.446 , -0.10 , 0.40 , &
              -0.006 , 0.00 , 0.00 , 0.00 , &
                0.00 , 0.00 , 0.00 , 0.00 /)
      real , dimension(16) :: a2 =(/ 0.00 , -12.0 , -11.7 , 0.00 , &
               -0.72 ,-0.243 , 0.19 ,-0.062 , &
               0.414 , 0.00 , 0.00 , 0.00 , &
                0.00 , 0.00 , 0.00 , 0.00 /)
      real , parameter :: amd = 28.9660
      real , parameter :: amw = 18.0160
      real , parameter :: amdw = 1.607793
      real , parameter :: amdc = 0.658114
      real , parameter :: amdo = 0.603428
      real , parameter :: amdm = 1.805423
      real , parameter :: amdn = 0.658090
      real , parameter :: amdo2 = 0.905140
      real , parameter :: amdc1 = 0.210852
      real , parameter :: amdc2 = 0.239546
      real :: amm, amttl, wvttl, wvsh, summol
      integer :: isp, l, ix, n, imol, ib
      integer, save :: counter =0
      real :: btemp
      integer :: pncold, nlayd, icldd
integer,external :: omp_get_thread_num
      ncol_ = pncol ; nlayers_ = nlay ; nbndlw_ = nbndlw ; ngptlw_ = ngptlw
      ncol__ = pncol ; nlayers__ = nlay ; nbndlw__ = nbndlw ; ngptlw__ = ngptlw
      icb(:) = (/ 1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5 /)
      oneminus = 1. - 1.e-6
      pi = 2. * asin(1. )
      fluxfac = pi * 2.e4
      istart = 1
      iend = 16
      iout = 0
      ims = 1
      pncold = pncol
      nlayd = nlay
      cldfracq(1:pncol,1:nlay) = cldfrac(colstart:(colstart+pncol-1), 1:nlay)
      ciwpq(1:pncol,1:nlay) = ciwp(colstart:(colstart+pncol-1), 1:nlay)
      clwpq(1:pncol,1:nlay) = clwp(colstart:(colstart+pncol-1), 1:nlay)
      cswpq(1:pncol,1:nlay) = cswp(colstart:(colstart+pncol-1), 1:nlay)
      reiq(1:pncol,1:nlay) = rei(colstart:(colstart+pncol-1), 1:nlay)
      relq(1:pncol,1:nlay) = rel(colstart:(colstart+pncol-1), 1:nlay)
      resq(1:pncol,1:nlay) = res(colstart:(colstart+pncol-1), 1:nlay)
      taucq(1:pncol,1:nbndlw,1:nlay) = tauc(colstart:(colstart+pncol-1), 1:nbndlw, 1:nlay)
      tauaq(1:pncol,1:nlay,1:nbndlw) = tauaer(colstart:(colstart+pncol-1), 1:nlay, 1:nbndlw)
      if (icld.lt.0.or.icld.gt.4) icld = 2
      iaer = 10
      nlayers = nlay
      call allocateGPUTaumol( 8, nlayers, npart)
      tbound = tsfc(colstart:(colstart+8 -1))
      pz(:,0:nlay) = plev(colstart:(colstart+8 -1),0:nlay)
      tz(:,0:nlay) = tlev(colstart:(colstart+8 -1),0:nlay)
      pavel(:,1:nlay) = play(colstart:(colstart+8 -1),1:nlay)
      tavel(:,1:nlay) = tlay(colstart:(colstart+8 -1),1:nlay)
      colh2o(1:8, 1:nlayers) = h2ovmr( colstart:(colstart+8 -1), 1:nlayers)
      colco2(1:8, 1:nlayers) = co2vmr( colstart:(colstart+8 -1), 1:nlayers)
      colo3(1:8, 1:nlayers) = o3vmr( colstart:(colstart+8 -1), 1:nlayers)
      coln2o(1:8, 1:nlayers) = n2ovmr( colstart:(colstart+8 -1), 1:nlayers)
      colch4(1:8, 1:nlayers) = ch4vmr( colstart:(colstart+8 -1), 1:nlayers)
      colo2(1:8, 1:nlayers) = o2vmr( colstart:(colstart+8 -1), 1:nlayers)
      wx1(1:8, 1:nlayers) = ccl4vmr(colstart:(colstart+8 -1), 1:nlayers)
      wx2(1:8, 1:nlayers) = cfc11vmr(colstart:(colstart+8 -1), 1:nlayers)
      wx3(1:8, 1:nlayers) = cfc12vmr(colstart:(colstart+8 -1), 1:nlayers)
      wx4(1:8, 1:nlayers) = cfc22vmr(colstart:(colstart+8 -1), 1:nlayers)
      colco(1:8, :) = 0
      if (npart > 1) then
         tauaa(1:8, :, :) = tauaer(colstart:(colstart+8 -1), :, :)
      else
         tauaa = tauaer
      endif
      permuteseed=150
      call mcica_subcol_lwg(colstart, pncol, nlay, icld, counter, permuteseed, &
                            pmid,clwp,ciwp,cswp,tauc, &
                            play, cldfracq, ciwpq, &
                            clwpq, cswpq, taucq,ngb, cldfmcd, ciwpmcd, clwpmcd, cswpmcd, &
                            taucmcd)
      if (icld > 0) then
         call generate_stochastic_cloudsg (pncold, nlayd, icld, ngb, &
                               pmid,cldfracq,clwpq,ciwpq,cswpq,taucq,permuteseed, &
                               cldfmcd, clwpmcd, ciwpmcd, cswpmcd, taucmcd)
      end if
      do iplon = 1, pncol
        piplon = iplon + colstart - 1
        amttl = 0.0
        wvttl = 0.0
        do l = 1, nlayers
          amm = (1. - h2ovmr(piplon,l)) * amd +h2ovmr(piplon,l) * amw
          coldry(iplon, l) = (pz(iplon, l-1)-pz(iplon, l)) * 1.e3 * avogad / &
                     (1.e2 * grav * amm * (1. + h2ovmr(piplon,l)))
        end do
        do l = 1, nlayers
          summol = co2vmr(piplon,l) + o3vmr(piplon,l) + n2ovmr(piplon,l) + ch4vmr(piplon,l) + o2vmr(piplon,l)
          btemp = h2ovmr(piplon, l) * coldry(iplon, l)
          wbrodl(iplon, l) = coldry(iplon, l) * (1. - summol)
          amttl = amttl + coldry(iplon, l)+btemp
          wvttl = wvttl + btemp
        enddo
        wvsh = (amw * wvttl) / (amd * amttl)
        pwvcm(iplon) = wvsh * (1.e3 * pz(iplon, 0)) / (1.e2 * grav)
        if (icld .ge. 1) then
          inflag(iplon) = inflglw
          iceflag(iplon) = iceflglw
          liqflag(iplon) = liqflglw
        endif
      enddo
      icldlyr = 0.0
      semissd(1:pncol,1:nbndlw) = emis(colstart:(colstart+pncol-1),1:nbndlw)
      call cldprmcg (pncol, nlayers, &
                inflag,iceflag,liqflag,ciwpmcd,clwpmcd,cswpmcd,relq,reiq,resq, &
                absice0,absice1,absice2,absice3,absliq1, &
                cldfmcd, taucmcd, ngb, icb, ncbandsd, icldlyr)
      call setcoefg (pncol, nlayers, istart &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pz,pwvcm,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrv,bpade,heatfac,fluxfac,a0,a1,a2 &
         ,delwave,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspa,nspb,oneminus &
   ,tavel,tz,tbound,wbrodl,totplnk,totplk16,totplnkderiv,totplk16deriv &
                            )
      call taumolg(1, pncol,nlayers, ngb, taug, fracs &
                    ,ncol__,nlayers__,nbndlw__,ngptlw__ &
                    ,pavel,wx1,wx2,wx3,wx4,coldry,laytrop,jp,jt,jt1,colh2o,colco2,colo3,coln2o &
                    ,colco,colch4,colo2,colbrd,indself,indfor,selffac,selffrac,forfac,forfrac &
                    ,indminor,minorfrac,scaleminor,scaleminorn2,fac00,fac01,fac10,fac11 &
                    ,rat_h2oco2,rat_h2oco2_1,rat_h2oo3,rat_h2oo3_1,rat_h2on2o,rat_h2on2o_1 &
                    ,rat_h2och4,rat_h2och4_1,rat_n2oco2,rat_n2oco2_1,rat_o3co2,rat_o3co2_1 &
                    ,tauaa,nspa,nspb,oneminus &
                  )
      call rtrnmcg (pncol,nlayers, istart, iend, iout &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pz,pwvcm,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrv,bpade,heatfac,fluxfac,a0,a1,a2 &
         ,delwave,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                    ,ngb, icldlyr, taug, fracs, cldfmcd)
      totufluxd = 0.0
      totdfluxd = 0.0
      totuclfld = 0.0
      totdclfld = 0.0
      dtotuflux_dtd = 0.0
      dtotuclfl_dtd = 0.0
      uflx(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totufluxd(1:pncol,0:(nlayers))
      dflx(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totdfluxd(1:pncol,0:(nlayers))
      call rtrnadd (pncol, nlayers, ngptlw, idrv &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pz,pwvcm,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrv,bpade,heatfac,fluxfac,a0,a1,a2 &
         ,delwave,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                           )
      uflx(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totufluxd(1:pncol,0:(nlayers))
      dflx(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totdfluxd(1:pncol,0:(nlayers))
      call rtrnheatrates (pncol, nlayers &
         ,ncol_,nlayers_,nbndlw_,ngptlw_ &
         ,taucmcd,pz,pwvcm,semissd,planklayd,planklevd,plankbndd,gurad,gdrad,gclrurad,gclrdrad &
         ,gdtotuflux_dtd,gdtotuclfl_dtd,idrv,bpade,heatfac,fluxfac,a0,a1,a2 &
         ,delwave,totufluxd,totdfluxd,fnetd,htrd,totuclfld,totdclfld,fnetcd,htrcd,dtotuflux_dtd &
         ,dtotuclfl_dtd,dplankbnd_dtd &
                                 )
      uflxc(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totuclfld(1:pncol,0:(nlayers))
      dflxc(colstart:(colstart+pncol-1), 1:(nlayers+1)) = totdclfld(1:pncol,0:(nlayers))
      hr(colstart:(colstart+pncol-1), 1:(nlayers+1)) = htrd(1:pncol,0:(nlayers))
      hrc(colstart:(colstart+pncol-1), 1:(nlayers+1)) = htrcd(1:pncol,0:(nlayers))
      if (idrv .eq. 1) then
         duflx_dt(colstart:(colstart+pncol-1), 1:(nlayers+1)) = dtotuflux_dtd(1:pncol,0:(nlayers))
         duflxc_dt(colstart:(colstart+pncol-1), 1:(nlayers+1)) = dtotuclfl_dtd(1:pncol,0:(nlayers))
      end if
      end subroutine rrtmg_lw_part
      end module rrtmg_lw_rad_f
      MODULE module_ra_rrtmg_lwf
      use module_model_constants, only : cp
      use module_wrf_error
      use parrrtm_f, only : nbndlw, ngptlw
      use rrtmg_lw_init_f, only: rrtmg_lw_ini
      use rrtmg_lw_rad_f, only: rrtmg_lw
      real retab(95)
      data retab / &
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234, &
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930, &
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319, &
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955, &
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125, &
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943, &
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601, &
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078, &
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635, &
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221, &
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898, &
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833, &
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424, &
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956, &
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699, &
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639/
      save retab
      INTEGER , SAVE :: nlayers
      REAL, PARAMETER :: deltap = 4.
      CONTAINS
      SUBROUTINE RRTMG_LWRAD_FAST( &
                       rthratenlw, &
                       lwupt, lwuptc, lwdnt, lwdntc, &
                       lwupb, lwupbc, lwdnb, lwdnbc, &
                       glw, olr, lwcf, emiss, &
                       p8w, p3d, pi3d, &
                       dz8w, tsk, t3d, t8w, rho3d, r, g, &
                       icloud, warm_rain, cldfra3d, &
                       lradius,iradius, &
                       is_cammgmp_used, &
                       f_ice_phy, f_rain_phy, &
                       xland, xice, snow, &
                       qv3d, qc3d, qr3d, &
                       qi3d, qs3d, qg3d, &
                       o3input, o33d, &
                       f_qv, f_qc, f_qr, f_qi, f_qs, f_qg, &
                       re_cloud, re_ice, re_snow, &
                       has_reqc, has_reqi, has_reqs, &
                       tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4, &
                       tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8, &
                       tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12, &
                       tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16, &
                       aer_ra_feedback, &
                       progn, &
                       qndrop3d,f_qndrop, &
                       yr,julian, &
                       ids,ide, jds,jde, kds,kde, &
                       ims,ime, jms,jme, kms,kme, &
                       its,ite, jts,jte, kts,kte, &
                       lwupflx, lwupflxc, lwdnflx, lwdnflxc &
                                                                  )
   USE MODULE_RA_CLWRF_SUPPORT, ONLY : read_CAMgases
   IMPLICIT NONE
   LOGICAL, INTENT(IN ) :: warm_rain
   LOGICAL, INTENT(IN ) :: is_CAMMGMP_used
   INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
   INTEGER, INTENT(IN ) :: ICLOUD
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
         INTENT(IN ) :: dz8w, &
                                                             t3d, &
                                                             t8w, &
                                                             p8w, &
                                                             p3d, &
                                                            pi3d, &
                                                           rho3d
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
         INTENT(INOUT) :: RTHRATENLW
   REAL, DIMENSION( ims:ime, jms:jme ) , &
         INTENT(INOUT) :: GLW, &
                                                             OLR, &
                                                            LWCF
   REAL, DIMENSION( ims:ime, jms:jme ) , &
         INTENT(IN ) :: EMISS, &
                                                             TSK
   REAL, INTENT(IN ) :: R,G
   REAL, DIMENSION( ims:ime, jms:jme ) , &
         INTENT(IN ) :: XLAND, &
                                                            XICE, &
                                                            SNOW
   INTEGER, INTENT(IN ) :: yr
   REAL, INTENT(IN ) :: julian
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
         OPTIONAL , &
         INTENT(IN ) :: &
                                                        CLDFRA3D, &
                                                         LRADIUS, &
                                                         IRADIUS, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        QNDROP3D
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN):: &
                                                        re_cloud, &
                                                          re_ice, &
                                                         re_snow
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs
   real pi,third,relconst,lwpmin,rhoh2o
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
         OPTIONAL , &
         INTENT(IN ) :: &
                                                       F_ICE_PHY, &
                                                      F_RAIN_PHY
   LOGICAL, OPTIONAL, INTENT(IN) :: &
                                   F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,F_QNDROP
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL , &
         INTENT(IN ) :: tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4, &
                           tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8, &
                           tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12, &
                           tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16
   INTEGER, INTENT(IN ), OPTIONAL :: aer_ra_feedback
   INTEGER, INTENT(IN ), OPTIONAL :: progn
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
         OPTIONAL , &
         INTENT(IN ) :: O33D
   INTEGER, OPTIONAL, INTENT(IN ) :: o3input
      real, parameter :: thresh=1.e-9
      real slope
      character(len=200) :: msg
   REAL, DIMENSION( ims:ime, jms:jme ), &
         OPTIONAL, INTENT(INOUT) :: &
                                       LWUPT,LWUPTC,LWDNT,LWDNTC, &
                                       LWUPB,LWUPBC,LWDNB,LWDNBC
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ), &
         OPTIONAL, INTENT(OUT) :: &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC
   REAL, DIMENSION( kts:kte+1 ) :: Pw1D, &
                                                            Tw1D
   REAL, DIMENSION( kts:kte ) :: TTEN1D, &
                                                        CLDFRA1D, &
                                                            DZ1D, &
                                                             P1D, &
                                                             T1D, &
                                                            QV1D, &
                                                            QC1D, &
                                                            QR1D, &
                                                            QI1D, &
                                                            QS1D, &
                                                            QG1D, &
                                                            O31D, &
                                                          qndrop1d
    integer :: ncol, &
                                                            nlay, &
                                                            idrv, &
                                                            icld, &
                                                         inflglw, &
                                                        iceflglw, &
                                                        liqflglw
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers+1 ) :: &
                                                            plev, &
                                                            tlev
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers ) :: &
                                                            play, &
                                                            tlay, &
                                                          h2ovmr, &
                                                           o3vmr, &
                                                          co2vmr, &
                                                           o2vmr, &
                                                          ch4vmr, &
                                                          n2ovmr, &
                                                        cfc11vmr, &
                                                        cfc12vmr, &
                                                        cfc22vmr, &
                                                         ccl4vmr
    real, dimension( kts:nlayers ) :: o3mmr
    real, dimension( kts:kte ) :: clwp, &
                                                            ciwp, &
                                                            cswp, &
                                                            plwp, &
                                                            piwp
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), nbndlw ) :: &
                                                            emis
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers ) :: &
                                                          clwpth, &
                                                          ciwpth, &
                                                          cswpth, &
                                                             rel, &
                                                             rei, &
                                                             res, &
                                                         cldfrac
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), nbndlw, kts:nlayers ) :: &
                                                          taucld
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers, nbndlw ) :: &
                                                          tauaer
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers+1 ) :: &
                                                            uflx, &
                                                            dflx, &
                                                           uflxc, &
                                                           dflxc
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers+1 ) :: &
                                                        duflx_dt, &
                                                       duflxc_dt
    real, dimension( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8), kts:nlayers+1 ) :: &
                                                              hr, &
                                                             hrc
    real, dimension ( (jte-jts+1)*(ite-its+1)+mod((jte-jts+1)*(ite-its+1),8) ) :: &
                                                            tsfc, &
                                                              ps
    real :: ro, &
                                                              dz
    real:: snow_mass_factor
      CHARACTER(LEN=256) :: message
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor
    real :: co2
    data co2 / 379.e-6 /
    real :: ch4
    data ch4 / 1774.e-9 /
    real :: n2o
    data n2o / 319.e-9 /
    real :: cfc11
    data cfc11 / 0.251e-9 /
    real :: cfc12
    data cfc12 / 0.538e-9 /
    real :: cfc22
    data cfc22 / 0.169e-9 /
    real :: ccl4
    data ccl4 / 0.093e-9 /
    real :: o2
    data o2 / 0.209488 /
    integer :: iplon, irng, permuteseed
    integer :: nb
    real :: abcw,abice,abrn,absn
    data abcw /0.144/
    data abice /0.0735/
    data abrn /0.330e-3/
    data absn /2.34e-3/
    real :: amdw
    real :: amdo
    real :: amdo2
    data amdw / 1.607793 /
    data amdo / 0.603461 /
    data amdo2 / 0.905190 /
    real, dimension( (jte-jts+1)*(ite-its+1), 1:kte-kts+1 ) :: pdel
    real, dimension( (jte-jts+1)*(ite-its+1), 1:kte-kts+1) :: cicewp, &
                                             cliqwp, &
                                             csnowp, &
                                              reliq, &
                                              reice
    real, dimension( (jte-jts+1)*(ite-its+1), 1:kte-kts+1):: recloud1d, &
                                            reice1d, &
                                           resnow1d
    real :: gliqwp, gicewp, gsnowp, gravmks
    real, dimension ((jte-jts+1)*(ite-its+1)) :: landfrac, landm, snowh, icefrac
    integer :: pcols, pver
    integer :: icol
    INTEGER :: i,j,K, idx_rei
    REAL :: corr
    LOGICAL :: predicate
    INTEGER, PARAMETER :: nproflevs = 60
    INTEGER :: L, LL, klev
    REAL, DIMENSION( kts:nlayers+1 ) :: varint
    REAL :: wght,vark,vark1
    REAL :: PPROF(nproflevs), TPROF(nproflevs)
    DATA PPROF /1000.00,855.47,731.82,626.05,535.57,458.16, &
                  391.94,335.29,286.83,245.38,209.91,179.57, &
                  153.62,131.41,112.42,96.17,82.27,70.38, &
                  60.21,51.51,44.06,37.69,32.25,27.59, &
                  23.60,20.19,17.27,14.77,12.64,10.81, &
                  9.25,7.91,6.77,5.79,4.95,4.24, &
                  3.63,3.10,2.65,2.27,1.94,1.66, &
                  1.42,1.22,1.04,0.89,0.76,0.65, &
                  0.56,0.48,0.41,0.35,0.30,0.26, &
                  0.22,0.19,0.16,0.14,0.12,0.10/
    DATA TPROF /286.96,281.07,275.16,268.11,260.56,253.02, &
                  245.62,238.41,231.57,225.91,221.72,217.79, &
                  215.06,212.74,210.25,210.16,210.69,212.14, &
                  213.74,215.37,216.82,217.94,219.03,220.18, &
                  221.37,222.64,224.16,225.88,227.63,229.51, &
                  231.50,233.73,236.18,238.78,241.60,244.44, &
                  247.35,250.33,253.32,256.30,259.22,262.12, &
                  264.80,266.50,267.59,268.44,268.69,267.76, &
                  266.13,263.96,261.54,258.93,256.15,253.23, &
                  249.89,246.67,243.48,240.25,236.66,233.86/
   ncol = (jte-jts+1)*(ite-its+1)
   j_loop: do j = jts,jte
      i_loop: do i = its,ite
         icol = i-its+1 + (j-jts)*(ite-its+1)
         do k=kts,kte+1
            Pw1D(K) = p8w(I,K,J)/100.
            Tw1D(K) = t8w(I,K,J)
         enddo
         DO K=kts,kte
            QV1D(K)=0.
            QC1D(K)=0.
            QR1D(K)=0.
            QI1D(K)=0.
            QS1D(K)=0.
            CLDFRA1D(k)=0.
         ENDDO
         DO K=kts,kte
            QV1D(K)=QV3D(I,K,J)
            QV1D(K)=max(0.,QV1D(K))
         ENDDO
         IF (PRESENT(O33D)) THEN
            DO K=kts,kte
               O31D(K)=O33D(I,K,J)
            ENDDO
         ELSE
            DO K=kts,kte
               O31D(K)=0.0
            ENDDO
         ENDIF
         DO K=kts,kte
            TTEN1D(K)=0.
            T1D(K)=T3D(I,K,J)
            P1D(K)=P3D(I,K,J)/100.
            DZ1D(K)=dz8w(I,K,J)
         ENDDO
         IF (ICLOUD .ne. 0) THEN
            IF ( PRESENT( CLDFRA3D ) ) THEN
              DO K=kts,kte
                 CLDFRA1D(k)=CLDFRA3D(I,K,J)
              ENDDO
            ENDIF
            IF (PRESENT(F_QC) .AND. PRESENT(QC3D)) THEN
              IF ( F_QC) THEN
                 DO K=kts,kte
                    QC1D(K)=QC3D(I,K,J)
                    QC1D(K)=max(0.,QC1D(K))
                 ENDDO
              ENDIF
            ENDIF
            IF (PRESENT(F_QR) .AND. PRESENT(QR3D)) THEN
              IF ( F_QR) THEN
                 DO K=kts,kte
                    QR1D(K)=QR3D(I,K,J)
                    QR1D(K)=max(0.,QR1D(K))
                 ENDDO
              ENDIF
            ENDIF
            IF ( PRESENT(F_QNDROP).AND.PRESENT(QNDROP3D)) THEN
             IF (F_QNDROP) THEN
              DO K=kts,kte
               qndrop1d(K)=qndrop3d(I,K,J)
              ENDDO
             ENDIF
            ENDIF
            IF ( PRESENT ( F_QI ) ) THEN
              predicate = F_QI
            ELSE
              predicate = .FALSE.
            ENDIF
            IF (.NOT. predicate .and. .not. warm_rain) THEN
               DO K=kts,kte
                  IF (T1D(K) .lt. 273.15) THEN
                  QI1D(K)=QC1D(K)
                  QS1D(K)=QR1D(K)
                  QC1D(K)=0.
                  QR1D(K)=0.
                  ENDIF
               ENDDO
            ENDIF
            IF (PRESENT(F_QI) .AND. PRESENT(QI3D)) THEN
               IF (F_QI) THEN
                  DO K=kts,kte
                     QI1D(K)=QI3D(I,K,J)
                     QI1D(K)=max(0.,QI1D(K))
                  ENDDO
               ENDIF
            ENDIF
            IF (PRESENT(F_QS) .AND. PRESENT(QS3D)) THEN
               IF (F_QS) THEN
                  DO K=kts,kte
                     QS1D(K)=QS3D(I,K,J)
                     QS1D(K)=max(0.,QS1D(K))
                  ENDDO
               ENDIF
            ENDIF
            IF (PRESENT(F_QG) .AND. PRESENT(QG3D)) THEN
               IF (F_QG) THEN
                  DO K=kts,kte
                     QG1D(K)=QG3D(I,K,J)
                     QG1D(K)=max(0.,QG1D(K))
                  ENDDO
               ENDIF
            ENDIF
            IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) .and. PRESENT(F_ICE_PHY) ) THEN
               IF ( F_QC .and. .not. F_QI .and. F_QS ) THEN
                  DO K=kts,kte
                     qi1d(k) = 0.1*qs3d(i,k,j)
                     qs1d(k) = 0.9*qs3d(i,k,j)
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
               ENDIF
            ENDIF
        ENDIF
         DO K=kts,kte
            QV1D(K)=AMAX1(QV1D(K),1.E-12)
         ENDDO
         nlay = nlayers
         idrv = 0
         icld = 2
         inflglw = 2
         iceflglw = 3
         liqflglw = 1
         IF (ICLOUD .ne. 0) THEN
            IF ( has_reqc .ne. 0) THEN
               inflglw = 3
               DO K=kts,kte
                  recloud1D(icol,K) = MAX(2.5, re_cloud(I,K,J)*1.E6)
                  if (recloud1D(icol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     & .AND. (XLAND(I,J)-1.5).GT.0.) then
                     recloud1D(icol,K) = 10.5
                  elseif(recloud1D(icol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     & .AND. (XLAND(I,J)-1.5).LT.0.) then
                     recloud1D(icol,K) = 7.5
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
                  recloud1D(icol,K) = 5.0
               ENDDO
            ENDIF
            IF ( has_reqi .ne. 0) THEN
               inflglw = 4
               iceflglw = 4
               DO K=kts,kte
                  reice1D(icol,K) = MAX(5., re_ice(I,K,J)*1.E6)
                  if (reice1D(icol,K).LE.5..AND.cldfra3d(i,k,j).gt.0.) then
                     idx_rei = int(t3d(i,k,j)-179.)
                     idx_rei = min(max(idx_rei,1),75)
                     corr = t3d(i,k,j) - int(t3d(i,k,j))
                     reice1D(icol,K) = retab(idx_rei)*(1.-corr) + &
     & retab(idx_rei+1)*corr
                     reice1D(icol,K) = MAX(reice1D(icol,K), 5.0)
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
                  reice1D(icol,K) = 10.0
               ENDDO
            ENDIF
            IF ( has_reqs .ne. 0) THEN
               inflglw = 5
               iceflglw = 5
               DO K=kts,kte
                  resnow1D(icol,K) = MAX(10., re_snow(I,K,J)*1.E6)
               ENDDO
            ELSE
               DO K=kts,kte
                  resnow1D(icol,K) = 10.0
               ENDDO
            ENDIF
            IF (has_reqs .eq. 0 .and. has_reqi .ne. 0 .and. has_reqc .ne. 0) THEN
               inflglw = 5
               iceflglw = 5
               DO K=kts,kte
                  resnow1D(ncol,K) = MAX(10., re_ice(I,K,J)*1.E6)
                  QS1D(K)=QI3D(I,K,J)
                  QI1D(K)=0.
                  reice1D(ncol,K)=10.
               END DO
            END IF
         ENDIF
         plev(icol,1) = pw1d(1)
         tlev(icol,1) = tw1d(1)
         tsfc(icol) = tsk(i,j)
         do k = kts, kte
            play(icol,k) = p1d(k)
            plev(icol,k+1) = pw1d(k+1)
            pdel(icol,k) = plev(icol,k) - plev(icol,k+1)
            tlay(icol,k) = t1d(k)
            tlev(icol,k+1) = tw1d(k+1)
            h2ovmr(icol,k) = qv1d(k) * amdw
            co2vmr(icol,k) = co2
            o2vmr(icol,k) = o2
            ch4vmr(icol,k) = ch4
            n2ovmr(icol,k) = n2o
            cfc11vmr(icol,k) = cfc11
            cfc12vmr(icol,k) = cfc12
            cfc22vmr(icol,k) = cfc22
            ccl4vmr(icol,k) = ccl4
         enddo
         if ( 1 == 0 ) then
         play(icol,kte+1) = 0.5 * plev(icol,kte+1)
         tlay(icol,kte+1) = tlev(icol,kte+1) + 0.0
         plev(icol,kte+2) = 1.0e-5
         tlev(icol,kte+2) = tlev(icol,kte+1) + 0.0
         h2ovmr(icol,kte+1) = h2ovmr(icol,kte)
         co2vmr(icol,kte+1) = co2vmr(icol,kte)
         o2vmr(icol,kte+1) = o2vmr(icol,kte)
         ch4vmr(icol,kte+1) = ch4vmr(icol,kte)
         n2ovmr(icol,kte+1) = n2ovmr(icol,kte)
         cfc11vmr(icol,kte+1) = cfc11vmr(icol,kte)
         cfc12vmr(icol,kte+1) = cfc12vmr(icol,kte)
         cfc22vmr(icol,kte+1) = cfc22vmr(icol,kte)
         ccl4vmr(icol,kte+1) = ccl4vmr(icol,kte)
         endif
       do L=kte+1,nlayers,1
          plev(icol,L+1) = plev(icol,L) - deltap
          play(icol,L) = 0.5*(plev(icol,L) + plev(icol,L+1))
       enddo
       plev(icol,nlayers+1) = 0.00
       play(icol,nlayers) = 0.5*(plev(icol,nlayers) + plev(icol,nlayers+1))
       do L=1,nlayers+1,1
          if ( PPROF(nproflevs) .lt. plev(icol,L) ) then
             do LL=2,nproflevs,1
                if ( PPROF(LL) .lt. plev(icol,L) ) then
                   klev = LL - 1
                   exit
                endif
             enddo
          else
             klev = nproflevs
          endif
          if (klev .ne. nproflevs ) then
             vark = TPROF(klev)
             vark1 = TPROF(klev+1)
             wght=(plev(icol,L)-PPROF(klev) )/( PPROF(klev+1)-PPROF(klev))
          else
             vark = TPROF(klev)
             vark1 = TPROF(klev)
             wght = 0.0
          endif
          varint(L) = wght*(vark1-vark)+vark
       enddo
       do L=kte+1,nlayers+1,1
          tlev(icol,L) = varint(L) + (tlev(icol,kte) - varint(kte))
          tlay(icol,L-1) = 0.5*(tlev(icol,L) + tlev(icol,L-1))
       enddo
       do L=kte+1,nlayers,1
          h2ovmr(icol,L) = h2ovmr(icol,kte)
          co2vmr(icol,L) = co2vmr(icol,kte)
          o2vmr(icol,L) = o2vmr(icol,kte)
          ch4vmr(icol,L) = ch4vmr(icol,kte)
          n2ovmr(icol,L) = n2ovmr(icol,kte)
          cfc11vmr(icol,L) = cfc11vmr(icol,kte)
          cfc12vmr(icol,L) = cfc12vmr(icol,kte)
          cfc22vmr(icol,L) = cfc22vmr(icol,kte)
          ccl4vmr(icol,L) = ccl4vmr(icol,kte)
       enddo
         call inirad (o3mmr,plev(icol,:),kts,nlay-1)
        if(present(o33d)) then
         do k = kts, nlayers
            o3vmr(icol,k) = o3mmr(k) * amdo
            IF ( PRESENT( O33D ) ) THEN
            if(o3input .eq. 2)then
               if(k.le.kte)then
                 o3vmr(icol,k) = o31d(k)
               else
                 o3vmr(icol,k) = o31d(kte) - o3mmr(kte)*amdo + o3mmr(k)*amdo
                 if(o3vmr(icol,k) .le. 0.)o3vmr(icol,k) = o3mmr(k)*amdo
               endif
            endif
            ENDIF
         enddo
        else
         do k = kts, nlayers
            o3vmr(icol,k) = o3mmr(k) * amdo
         enddo
        endif
         do nb = 1, nbndlw
            emis(icol, nb) = emiss(i,j)
         enddo
         if (inflglw .eq. 0) then
            do k = kts,kte
               ro = p1d(k) / (r * t1d(k))*100.
               dz = dz1d(k)
               clwp(k) = ro*qc1d(k)*dz*1000.
               ciwp(k) = ro*qi1d(k)*dz*1000.
               plwp(k) = (ro*qr1d(k))**0.75*dz*1000.
               piwp(k) = (ro*qs1d(k))**0.75*dz*1000.
            enddo
            do k = kts, kte
               cldfrac(icol,k) = cldfra1d(k)
               do nb = 1, nbndlw
                  taucld(icol,nb,k) = abcw*clwp(k) + abice*ciwp(k) &
                            +abrn*plwp(k) + absn*piwp(k)
                  if (taucld(icol,nb,k) .gt. 0.01) cldfrac(icol,k) = 1.
               enddo
            enddo
            do k = kts, kte
               clwpth(icol,k) = 0.0
               ciwpth(icol,k) = 0.0
               rel(icol,k) = 10.0
               rei(icol,k) = 10.0
            enddo
         endif
         if (inflglw .gt. 0) then
            do k = kts, kte
               cldfrac(icol,k) = cldfra1d(k)
            enddo
            pcols = ncol
            pver = kte - kts + 1
            gravmks = g
            landfrac(icol) = 2.-XLAND(I,J)
            landm(icol) = landfrac(icol)
            snowh(icol) = 0.001*SNOW(I,J)
            icefrac(icol) = XICE(I,J)
            do k = kts, kte
               gicewp = (qi1d(k)+qs1d(k)) * pdel(icol,k)*100.0 / gravmks * 1000.0
               gliqwp = qc1d(k) * pdel(icol,k)*100.0 / gravmks * 1000.0
               cicewp(icol,k) = gicewp / max(0.01,cldfrac(icol,k))
               cliqwp(icol,k) = gliqwp / max(0.01,cldfrac(icol,k))
            end do
           if(iceflglw.ge.4)then
              do k = kts, kte
                     gicewp = qi1d(k) * pdel(icol,k)*100.0 / gravmks * 1000.0
                     cicewp(icol,k) = gicewp / max(0.01,cldfrac(icol,k))
              end do
           end if
           if(iceflglw.eq.5)then
              do k = kts, kte
                 snow_mass_factor = 1.0
                 if (resnow1d(icol,k) .gt. 130.)then
                     snow_mass_factor = (130.0/resnow1d(icol,k))*(130.0/resnow1d(icol,k))
                     resnow1d(icol,k) = 130.0
                     IF ( wrf_dm_on_monitor() ) THEN
                       WRITE(message,*)'RRTMG:  reducing snow mass (cloud path) to ', &
                                       nint(snow_mass_factor*100.), ' percent of full value'
                       call wrf_debug(150, message)
                     ENDIF
                 endif
                 gsnowp = qs1d(k) * snow_mass_factor * pdel(icol,k)*100.0 / gravmks * 1000.0
                 csnowp(icol,k) = gsnowp / max(0.01,cldfrac(icol,k))
              end do
           end if
  if( PRESENT( progn ) ) then
    if (progn == 1) then
      pi = 4.*atan(1.0)
      third=1./3.
      rhoh2o=1.e3
      relconst=3/(4.*pi*rhoh2o)
      lwpmin=3.e-5
      do k = kts, kte
         reliq(icol,k) = 10.
         if( PRESENT( F_QNDROP ) ) then
            if( F_QNDROP ) then
              if ( qc1d(k)*pdel(icol,k).gt.lwpmin.and. &
                   qndrop1d(k).gt.1000. ) then
               reliq(icol,k)=(relconst*qc1d(k)/qndrop1d(k))**third
               reliq(icol,k)=1.1*reliq(icol,k)
               reliq(icol,k)=reliq(icol,k)*1.e6
               reliq(icol,k)=max(reliq(icol,k),4.)
               reliq(icol,k)=min(reliq(icol,k),20.)
              end if
            end if
         end if
      end do
    else
      call relcalc(icol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
    endif
  else
      call relcalc(icol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
  endif
            call reicalc(icol, pcols, pver, tlay, reice)
      if (inflglw .ge. 3) then
         do k = kts, kte
            reliq(icol,k) = recloud1d(icol,k)
         end do
      endif
      if (iceflglw .ge. 4) then
         do k = kts, kte
            reice(icol,k) = reice1d(icol,k)
         end do
      endif
            if (iceflglw .eq. 3) then
               do k = kts, kte
                  reice(icol,k) = reice(icol,k) * 1.0315
                  reice(icol,k) = min(140.0,reice(icol,k))
               end do
            endif
            if(is_CAMMGMP_used) then
               do k = kts, kte
                  if ( qi1d(k) .gt. 1.e-20 .or. qs1d(k) .gt. 1.e-20) then
                     reice(icol,k) = iradius(i,k,j)
                  else
                     reice(icol,k) = 25.
                  end if
                  reice(icol,k) = max(5., min(140.0,reice(icol,k)))
                  if ( qc1d(k) .gt. 1.e-20) then
                     reliq(icol,k) = lradius(i,k,j)
                  else
                     reliq(icol,k) = 10.
                  end if
                  reliq(icol,k) = max(2.5, min(60.0,reliq(icol,k)))
               enddo
            endif
            do k = kts, kte
               clwpth(icol,k) = cliqwp(icol,k)
               ciwpth(icol,k) = cicewp(icol,k)
               rel(icol,k) = reliq(icol,k)
               rei(icol,k) = reice(icol,k)
            enddo
            if (inflglw .eq. 5) then
               do k = kts, kte
                  cswpth(icol,k) = csnowp(icol,k)
                  res(icol,k) = resnow1d(icol,k)
               end do
            else
               do k = kts, kte
                  cswpth(icol,k) = 0.
                  res(icol,k) = 10.
               end do
            endif
            do k = kts, kte
               do nb = 1, nbndlw
                  taucld(icol,nb,k) = 0.0
               enddo
            enddo
         endif
         if ( 1 == 0 ) then
         clwpth(icol,kte+1) = 0.
         ciwpth(icol,kte+1) = 0.
         cswpth(icol,kte+1) = 0.
         rel(icol,kte+1) = 10.
         rei(icol,kte+1) = 10.
         res(icol,kte+1) = 10.
         cldfrac(icol,kte+1) = 0.
         do nb = 1, nbndlw
            taucld(icol,nb,kte+1) = 0.
         enddo
         endif
         do k=kte+1,nlayers
            clwpth(icol,k) = 0.
            ciwpth(icol,k) = 0.
            cswpth(icol,k) = 0.
            rel(icol,k) = 10.
            rei(icol,k) = 10.
            res(icol,k) = 10.
            cldfrac(icol,k) = 0.
            do nb = 1,nbndlw
               taucld(icol,nb,k) = 0.
            enddo
         enddo
      do nb = 1, nbndlw
      do k = kts,nlayers
         tauaer(icol,k,nb) = 0.
      end do
      end do
      end do i_loop
   end do j_loop
         call rrtmg_lw &
            (ncol ,nlay ,icld ,idrv , &
             play ,plev ,tlay ,tlev ,tsfc , &
             h2ovmr ,o3vmr ,co2vmr ,ch4vmr ,n2ovmr ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis , &
             inflglw ,iceflglw,liqflglw,cldfrac , &
             taucld ,ciwpth ,clwpth ,cswpth ,rei ,rel ,res , &
             tauaer , &
             uflx ,dflx ,hr ,uflxc ,dflxc, hrc, &
             duflx_dt,duflxc_dt)
   j_loop2: do j = jts,jte
      i_loop2: do i = its,ite
         icol = i-its+1 + (j-jts)*(ite-its+1)
         glw(i,j) = dflx(icol,1)
         olr(i,j) = uflx(icol,nlayers+1)
         lwcf(i,j) = uflxc(icol,nlayers+1) - uflx(icol,nlayers+1)
         if (present(lwupt)) then
            lwupt(i,j) = uflx(icol,nlayers+1)
            lwuptc(i,j) = uflxc(icol,nlayers+1)
            lwdnt(i,j) = dflx(icol,nlayers+1)
            lwdntc(i,j) = dflxc(icol,nlayers+1)
            lwupb(i,j) = uflx(icol,1)
            lwupbc(i,j) = uflxc(icol,1)
            lwdnb(i,j) = dflx(icol,1)
            lwdnbc(i,j) = dflxc(icol,1)
         endif
         if ( present (lwupflx) ) then
         do k=kts,kte+2
            lwupflx(i,k,j) = uflx(icol,k)
            lwupflxc(i,k,j) = uflxc(icol,k)
            lwdnflx(i,k,j) = dflx(icol,k)
            lwdnflxc(i,k,j) = dflxc(icol,k)
         enddo
         endif
         do k=kts,kte
            tten1d(k) = hr(icol,k)/86400.
            rthratenlw(i,k,j) = tten1d(k)/pi3d(i,k,j)
         enddo
      end do i_loop2
   end do j_loop2
   END SUBROUTINE RRTMG_LWRAD_FAST
   SUBROUTINE INIRAD (O3PROF,Plev, kts, kte)
      IMPLICIT NONE
   INTEGER, INTENT(IN ) :: kts,kte
   REAL, DIMENSION( kts:kte+1 ),INTENT(INOUT) :: O3PROF
   REAL, DIMENSION( kts:kte+2 ),INTENT(IN ) :: Plev
   INTEGER :: k
   DO K=kts,kte+1
      O3PROF(K)=0.
   ENDDO
   CALL O3DATA(O3PROF, Plev, kts, kte)
   END SUBROUTINE INIRAD
   SUBROUTINE O3DATA (O3PROF, Plev, kts, kte)
   IMPLICIT NONE
   INTEGER, INTENT(IN ) :: kts, kte
   REAL, DIMENSION( kts:kte+1 ),INTENT(INOUT) :: O3PROF
   REAL, DIMENSION( kts:kte+2 ),INTENT(IN ) :: Plev
   INTEGER :: K, JJ
   REAL :: PRLEVH(kts:kte+2),PPWRKH(32), &
               O3WRK(31),PPWRK(31),O3SUM(31),PPSUM(31), &
               O3WIN(31),PPWIN(31),O3ANN(31),PPANN(31)
   REAL :: PB1, PB2, PT1, PT2
   DATA O3SUM /5.297E-8,5.852E-8,6.579E-8,7.505E-8, &
        8.577E-8,9.895E-8,1.175E-7,1.399E-7,1.677E-7,2.003E-7, &
        2.571E-7,3.325E-7,4.438E-7,6.255E-7,8.168E-7,1.036E-6, &
        1.366E-6,1.855E-6,2.514E-6,3.240E-6,4.033E-6,4.854E-6, &
        5.517E-6,6.089E-6,6.689E-6,1.106E-5,1.462E-5,1.321E-5, &
        9.856E-6,5.960E-6,5.960E-6/
   DATA PPSUM /955.890,850.532,754.599,667.742,589.841, &
        519.421,455.480,398.085,347.171,301.735,261.310,225.360, &
        193.419,165.490,141.032,120.125,102.689, 87.829, 75.123, &
         64.306, 55.086, 47.209, 40.535, 34.795, 29.865, 19.122, &
          9.277, 4.660, 2.421, 1.294, 0.647/
   DATA O3WIN /4.629E-8,4.686E-8,5.017E-8,5.613E-8, &
        6.871E-8,8.751E-8,1.138E-7,1.516E-7,2.161E-7,3.264E-7, &
        4.968E-7,7.338E-7,1.017E-6,1.308E-6,1.625E-6,2.011E-6, &
        2.516E-6,3.130E-6,3.840E-6,4.703E-6,5.486E-6,6.289E-6, &
        6.993E-6,7.494E-6,8.197E-6,9.632E-6,1.113E-5,1.146E-5, &
        9.389E-6,6.135E-6,6.135E-6/
   DATA PPWIN /955.747,841.783,740.199,649.538,568.404, &
        495.815,431.069,373.464,322.354,277.190,237.635,203.433, &
        174.070,148.949,127.408,108.915, 93.114, 79.551, 67.940, &
         58.072, 49.593, 42.318, 36.138, 30.907, 26.362, 16.423, &
          7.583, 3.620, 1.807, 0.938, 0.469/
   DO K=1,31
     PPANN(K)=PPSUM(K)
   ENDDO
   O3ANN(1)=0.5*(O3SUM(1)+O3WIN(1))
   DO K=2,31
      O3ANN(K)=O3WIN(K-1)+(O3WIN(K)-O3WIN(K-1))/(PPWIN(K)-PPWIN(K-1))* &
               (PPSUM(K)-PPWIN(K-1))
   ENDDO
   DO K=2,31
      O3ANN(K)=0.5*(O3ANN(K)+O3SUM(K))
   ENDDO
   DO K=1,31
      O3WRK(K)=O3ANN(K)
      PPWRK(K)=PPANN(K)
   ENDDO
   DO K=kts,kte+2
      PRLEVH(K)=Plev(K)
   ENDDO
   PPWRKH(1)=1100.
   DO K=2,31
      PPWRKH(K)=(PPWRK(K)+PPWRK(K-1))/2.
   ENDDO
   PPWRKH(32)=0.
   DO K=kts,kte+1
      DO 25 JJ=1,31
         IF((-(PRLEVH(K)-PPWRKH(JJ))).GE.0.)THEN
           PB1=0.
         ELSE
           PB1=PRLEVH(K)-PPWRKH(JJ)
         ENDIF
         IF((-(PRLEVH(K)-PPWRKH(JJ+1))).GE.0.)THEN
           PB2=0.
         ELSE
           PB2=PRLEVH(K)-PPWRKH(JJ+1)
         ENDIF
         IF((-(PRLEVH(K+1)-PPWRKH(JJ))).GE.0.)THEN
           PT1=0.
         ELSE
           PT1=PRLEVH(K+1)-PPWRKH(JJ)
         ENDIF
         IF((-(PRLEVH(K+1)-PPWRKH(JJ+1))).GE.0.)THEN
           PT2=0.
         ELSE
           PT2=PRLEVH(K+1)-PPWRKH(JJ+1)
         ENDIF
         O3PROF(K)=O3PROF(K)+(PB2-PB1-PT2+PT1)*O3WRK(JJ)
  25 CONTINUE
      O3PROF(K)=O3PROF(K)/(PRLEVH(K)-PRLEVH(K+1))
   ENDDO
   END SUBROUTINE O3DATA
   SUBROUTINE rrtmg_lwinit_fast( &
                       p_top, allowed_to_read , &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
   IMPLICIT NONE
   LOGICAL , INTENT(IN) :: allowed_to_read
   INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte
   REAL, INTENT(IN) :: p_top
   NLAYERS = kme + nint(p_top*0.01/deltap)- 1
   IF ( allowed_to_read ) THEN
     CALL rrtmg_lwlookuptable
   ENDIF
   call rrtmg_lw_ini(cp)
   END SUBROUTINE rrtmg_lwinit_fast
      SUBROUTINE rrtmg_lwlookuptable
IMPLICIT NONE
      INTEGER :: i
      LOGICAL :: opened
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      CHARACTER*80 errmess
      INTEGER rrtmg_unit
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 10,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            rrtmg_unit = i
            GOTO 2010
          ENDIF
        ENDDO
        rrtmg_unit = -1
 2010 CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( rrtmg_unit , 4 )
      IF ( rrtmg_unit < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",14052,&
'module_ra_rrtmg_lwf: rrtm_lwlookuptable: Can not '// &
                               'find unused fortran unit to read in lookup table.' )
      ENDIF
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(rrtmg_unit,FILE='RRTMG_LW_DATA', &
             FORM='UNFORMATTED',STATUS='OLD',ERR=9009)
      ENDIF
      call lw_kgb01(rrtmg_unit)
      call lw_kgb02(rrtmg_unit)
      call lw_kgb03(rrtmg_unit)
      call lw_kgb04(rrtmg_unit)
      call lw_kgb05(rrtmg_unit)
      call lw_kgb06(rrtmg_unit)
      call lw_kgb07(rrtmg_unit)
      call lw_kgb08(rrtmg_unit)
      call lw_kgb09(rrtmg_unit)
      call lw_kgb10(rrtmg_unit)
      call lw_kgb11(rrtmg_unit)
      call lw_kgb12(rrtmg_unit)
      call lw_kgb13(rrtmg_unit)
      call lw_kgb14(rrtmg_unit)
      call lw_kgb15(rrtmg_unit)
      call lw_kgb16(rrtmg_unit)
     IF ( wrf_dm_on_monitor() ) CLOSE (rrtmg_unit)
     RETURN
9009 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error opening RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14080,&
errmess)
     END SUBROUTINE rrtmg_lwlookuptable
      subroutine lw_kgb01(rrtmg_unit)
      use rrlw_kg01_f, only : fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, &
                           absa, absb, &
                      selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mn2 , size ( kao_mn2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mn2 , size ( kbo_mn2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14164,&
errmess)
      end subroutine lw_kgb01
      subroutine lw_kgb02(rrtmg_unit)
      use rrlw_kg02_f, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14226,&
errmess)
      end subroutine lw_kgb02
      subroutine lw_kgb03(rrtmg_unit)
      use rrlw_kg03_f, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o, &
                            kbo_mn2o, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14328,&
errmess)
      end subroutine lw_kgb03
      subroutine lw_kgb04(rrtmg_unit)
      use rrlw_kg04_f, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14401,&
errmess)
      end subroutine lw_kgb04
      subroutine lw_kgb05(rrtmg_unit)
      use rrlw_kg05_f, only : fracrefao, fracrefbo, kao, kbo, kao_mo3, &
                            selfrefo, forrefo, ccl4o
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mo3, ccl4o, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mo3 , size ( kao_mo3 ) * 4 )
      CALL wrf_dm_bcast_bytes ( ccl4o , size ( ccl4o ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14493,&
errmess)
      end subroutine lw_kgb05
      subroutine lw_kgb06(rrtmg_unit)
      use rrlw_kg06_f, only : fracrefao, kao, kao_mco2, selfrefo, forrefo, &
                            cfc11adjo, cfc12o
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, kao, kao_mco2, cfc11adjo, cfc12o, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( cfc11adjo , size ( cfc11adjo ) * 4 )
      CALL wrf_dm_bcast_bytes ( cfc12o , size ( cfc12o ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14558,&
errmess)
      end subroutine lw_kgb06
      subroutine lw_kgb07(rrtmg_unit)
      use rrlw_kg07_f, only : fracrefao, fracrefbo, kao, kbo, kao_mco2, &
                            kbo_mco2, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mco2 , size ( kbo_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14646,&
errmess)
      end subroutine lw_kgb07
      subroutine lw_kgb08(rrtmg_unit)
      use rrlw_kg08_f, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mn2o, &
                            kao_mo3, kbo, kbo_mco2, kbo_mn2o, selfrefo, forrefo, &
                            cfc12o, cfc22adjo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, kao_mn2o, &
         kbo_mn2o, kao_mo3, cfc12o, cfc22adjo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mco2 , size ( kbo_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mo3 , size ( kao_mo3 ) * 4 )
      CALL wrf_dm_bcast_bytes ( cfc12o , size ( cfc12o ) * 4 )
      CALL wrf_dm_bcast_bytes ( cfc22adjo , size ( cfc22adjo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14743,&
errmess)
      end subroutine lw_kgb08
      subroutine lw_kgb09(rrtmg_unit)
      use rrlw_kg09_f, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o, &
                            kbo_mn2o, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14831,&
errmess)
      end subroutine lw_kgb09
      subroutine lw_kgb10(rrtmg_unit)
      use rrlw_kg10_f, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14893,&
errmess)
      end subroutine lw_kgb10
      subroutine lw_kgb11(rrtmg_unit)
      use rrlw_kg11_f, only : fracrefao, fracrefbo, kao, kbo, kao_mo2, &
                            kbo_mo2, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, kao_mo2, kbo_mo2, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mo2 , size ( kao_mo2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mo2 , size ( kbo_mo2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",14970,&
errmess)
      end subroutine lw_kgb11
      subroutine lw_kgb12(rrtmg_unit)
      use rrlw_kg12_f, only : fracrefao, kao, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, kao, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",15024,&
errmess)
      end subroutine lw_kgb12
      subroutine lw_kgb13(rrtmg_unit)
      use rrlw_kg13_f, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mco, &
                            kbo_mo3, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kao_mco2, kao_mco, kbo_mo3, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mco , size ( kao_mco ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo_mo3 , size ( kbo_mo3 ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",15101,&
errmess)
      end subroutine lw_kgb13
      subroutine lw_kgb14(rrtmg_unit)
      use rrlw_kg14_f, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",15169,&
errmess)
      end subroutine lw_kgb14
      subroutine lw_kgb15(rrtmg_unit)
      use rrlw_kg15_f, only : fracrefao, kao, kao_mn2, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, kao, kao_mn2, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao_mn2 , size ( kao_mn2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",15235,&
errmess)
      end subroutine lw_kgb15
      subroutine lw_kgb16(rrtmg_unit)
      use rrlw_kg16_f, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      implicit none
      save
      integer, intent(in) :: rrtmg_unit
      character*80 errmess
      logical, external :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
      CALL wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
      CALL wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
      CALL wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
      CALL wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
      CALL wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_lwf: error reading RRTMG_LW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal3("<stdin>",15303,&
errmess)
      end subroutine lw_kgb16
  subroutine relcalc(icol, pcols, pver, t, landfrac, landm, icefrac, rel, snowh)
    implicit none
    integer, intent(in) :: icol
    integer, intent(in) :: pcols, pver
    real, intent(in) :: landfrac(pcols)
    real, intent(in) :: icefrac(pcols)
    real, intent(in) :: snowh(pcols)
    real, intent(in) :: landm(pcols)
    real, intent(in) :: t(pcols,pver)
    real, intent(out) :: rel(pcols,pver)
    integer i,k
    real tmelt
    real rliqland
    real rliqocean
    real rliqice
    tmelt = 273.16
    rliqocean = 14.0
    rliqice = 14.0
    rliqland = 8.0
    do k=1,pver
          rel(icol,k) = rliqland + (rliqocean-rliqland) * min(1.0,max(0.0,(tmelt-t(icol,k))*0.05))
          rel(icol,k) = rel(icol,k) + (rliqocean-rel(icol,k)) * min(1.0,max(0.0,snowh(icol)*10.))
          rel(icol,k) = rel(icol,k) + (rliqocean-rel(icol,k)) * min(1.0,max(0.0,1.0-landm(icol)))
          rel(icol,k) = rel(icol,k) + (rliqice-rel(icol,k)) * min(1.0,max(0.0,icefrac(icol)))
    end do
  end subroutine relcalc
  subroutine reicalc(icol, pcols, pver, t, re)
    integer, intent(in) :: icol, pcols, pver
    real, intent(out) :: re(pcols,pver)
    real, intent(in) :: t(pcols,pver)
    real corr
    integer i
    integer k
    integer index
    do k=1,pver
          index = int(t(icol,k)-179.)
          index = min(max(index,1),94)
          corr = t(icol,k) - int(t(icol,k))
          re(icol,k) = retab(index)*(1.-corr) &
               +retab(index+1)*corr
    end do
    return
  end subroutine reicalc
END MODULE module_ra_rrtmg_lwf
