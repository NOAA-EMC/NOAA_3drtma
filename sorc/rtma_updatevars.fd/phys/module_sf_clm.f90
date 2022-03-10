   subroutine endrun
   call abort
   return
   end
module TridiagonalMod
  implicit none
  save
  public :: Tridiagonal
contains
  subroutine Tridiagonal (lbc, ubc, lbj, ubj, jtop, numf, filter, &
                          a, b, c, r, u)
    use shr_kind_mod, only: r8 => shr_kind_r8
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbj, ubj
    integer , intent(in) :: jtop(lbc:ubc)
    integer , intent(in) :: numf
    integer , intent(in) :: filter(1:numf)
    real(r8), intent(in) :: a(lbc:ubc, lbj:ubj)
    real(r8), intent(in) :: b(lbc:ubc, lbj:ubj)
    real(r8), intent(in) :: c(lbc:ubc, lbj:ubj)
    real(r8), intent(in) :: r(lbc:ubc, lbj:ubj)
    real(r8), intent(inout) :: u(lbc:ubc, lbj:ubj)
    integer :: j,ci,fc
    real(r8) :: gam(lbc:ubc,lbj:ubj)
    real(r8) :: bet(lbc:ubc)
!dir$ concurrent
    do fc = 1,numf
       ci = filter(fc)
       bet(ci) = b(ci,jtop(ci))
    end do
    do j = lbj, ubj
!dir$ prefervector
!dir$ concurrent
       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             if (j == jtop(ci)) then
                u(ci,j) = r(ci,j) / bet(ci)
              else
                gam(ci,j) = c(ci,j-1) / bet(ci)
                bet(ci) = b(ci,j) - a(ci,j) * gam(ci,j)
                u(ci,j) = (r(ci,j) - a(ci,j)*u(ci,j-1)) / bet(ci)
             end if
          end if
       end do
    end do
!dir$ unroll 0
    do j = ubj-1,lbj,-1
!dir$ prefervector
!dir$ concurrent
       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             u(ci,j) = u(ci,j) - gam(ci,j+1) * u(ci,j+1)
          end if
       end do
    end do
  end subroutine Tridiagonal
end module TridiagonalMod
module globals
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  integer :: nstep
  real(r8):: dtime
  real(r8):: dt
  integer :: iyear0
  integer :: day_per_year
  logical :: is_perpetual = .false.
  integer :: year
  integer :: month
  integer :: day
  integer :: secs
  real(r8):: calday
  integer :: yrp1
  integer :: monp1
  integer :: dayp1
  integer :: secp1
  real(r8):: caldayp1
  integer :: nbdate
 contains
   subroutine globals_mod
   end subroutine globals_mod
end module globals
module nanMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  save
  real(r8), parameter :: inf = 1.e19
  real(r8), parameter :: nan = 1.e21
  integer, parameter :: bigint = O'17777777777'
 contains
   subroutine nanMod_mod
   end subroutine nanMod_mod
end module nanMod
subroutine mkrank (n, a, miss, iv, num)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use module_cam_support, only: endrun
  implicit none
  integer , intent(in) :: n
  real(r8), intent(in) :: a(0:n)
  integer , intent(in) :: miss
  integer , intent(in) :: num
  integer , intent(out):: iv(num)
  real(r8) a_max
  integer i
  real(r8) delmax
  integer m
  integer k
  logical exclude
  delmax = 1.e-06
  iv(1) = miss
  a_max = -9999.
  do i = 0, n
     if (a(i)>0. .and. (a(i)-a_max)>delmax) then
        a_max = a(i)
        iv(1) = i
     end if
  end do
  if (iv(1) == miss) then
     write (6,*) 'MKRANK error: iv(1) = missing'
     call endrun
  end if
  do m = 2, num
     iv(m) = miss
     a_max = -9999.
     do i = 0, n
        exclude = .false.
        do k = 1, m-1
           if (i == iv(k)) exclude = .true.
        end do
        if (.not. exclude) then
           if (a(i)>0. .and. (a(i)-a_max)>delmax) then
              a_max = a(i)
              iv(m) = i
           end if
        end if
     end do
  end do
  return
end subroutine mkrank
module clm_varpar
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  save
  integer, parameter :: lsmlon = 1
  integer, parameter :: lsmlat = 1
  integer, parameter :: nlevsoi = 10
  integer, parameter :: nlevlak = 10
  integer, parameter :: nlevsno = 5
  integer, parameter :: nlevgrnd = 10
  integer, parameter :: nlevurb = nlevgrnd
  integer, parameter :: numwat = 5
  integer, parameter :: numrad = 2
  integer, parameter :: numsolar = 2
  integer, parameter :: ndst = 4
  integer, parameter :: dst_src_nbr = 3
  integer, parameter :: sz_nbr = 200
  integer, parameter :: nvoc = 5
  integer, parameter :: numcol = 8
  integer, parameter :: rtmlon = 720
  integer, parameter :: rtmlat = 360
  integer, parameter :: numpft = 16
  integer, parameter :: numcft = 2
  integer, parameter :: numveg = numpft
  integer, parameter :: maxpatch_urb = 1
  integer, parameter :: maxpatch_cft = 2
  integer, parameter :: maxpatch_pft = 4
  integer, parameter :: npatch_urban = maxpatch_pft + 1
  integer, parameter :: npatch_lake = npatch_urban + maxpatch_urb
  integer, parameter :: npatch_wet = npatch_lake + 1
  integer, parameter :: npatch_glacier = npatch_wet + 1
  integer, parameter :: npatch_crop = npatch_glacier + maxpatch_cft
  integer, parameter :: maxpatch = npatch_crop
  integer, parameter :: max_pft_per_gcell = numpft+1 + 3 + maxpatch_urb + numcft
 integer, parameter :: max_pft_per_lu = max(numpft+1, numcft, maxpatch_urb)
  integer, parameter :: max_pft_per_col = max(numpft+1, numcft, maxpatch_urb)
  integer :: num_landcover_types
contains
 subroutine clm_varpar_mod(nlcat)
          integer,intent(in) :: nlcat
          num_landcover_types = nlcat
 end subroutine clm_varpar_mod
end module clm_varpar
module clm_varcon
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar, only : numcol,numrad,nlevlak,&
                         maxpatch_pft,numpft,nlevgrnd,&
                         num_landcover_types
  implicit none
  save
  integer, private :: i
  real(r8), parameter :: cday = 86400.0_r8
  integer, parameter :: idx_Mie_snw_mx = 1471
  integer, parameter :: idx_T_max = 11
  integer, parameter :: idx_Tgrd_max = 31
  integer, parameter :: idx_rhos_max = 8
  integer, parameter :: numrad_snw = 5
  real(r8), parameter :: pie = 3.141592653589793_r8
  real(r8), parameter :: rpi = 3.141592653589793_r8
  real(r8), parameter :: grav = 9.80616_r8
  real(r8), parameter :: sb = 5.67e-8_r8
  real(r8), parameter :: vkc = 0.4_r8
  real(r8), parameter :: rgas = 8314.468_r8
  real(r8), parameter :: rwat = 461.5046_r8
  real(r8), parameter :: rair = 287.0423_r8
  real(r8), parameter :: roverg = 47062.73_r8
  real(r8), parameter :: cpliq = 4.188e3_r8
  real(r8), parameter :: cpice = 2.11727e3_r8
  real(r8), parameter :: cpair = 1.00464e3_r8
  real(r8), parameter :: hvap = 2.501e6_r8
  real(r8), parameter :: hfus = 3.337e5_r8
  real(r8), parameter :: hsub = 2.501e6_r8+3.337e5_r8
  real(r8), parameter :: denh2o = 1.000e3_r8
  real(r8), parameter :: denice = 0.917e3_r8
  real(r8), parameter :: tkair = 0.023_r8
  real(r8), parameter :: tkice = 2.290_r8
  real(r8), parameter :: tkwat = 0.6_r8
  real(r8), parameter :: tfrz = 273.16_r8
  real(r8), parameter :: tcrit = 2.5_r8
  real(r8), parameter :: po2 = 0.209_r8
  real(r8), parameter :: pco2 = 355.e-06
  real(r8), parameter :: pstd = 101325.0_r8
  real(r8), parameter :: bdsno = 250.
  real(r8), parameter :: re = 6.37122e6_r8*0.001
  real(r8), public, parameter :: secspday= 86400.0_r8
  real(r8), public, parameter :: spval = 1.e36_r8
  integer , public, parameter :: ispval = -9999
  real(r8) :: alpha_aero = 1.0_r8
  real(r8) :: tlsai_crit = 2.0_r8
  real(r8) :: watmin = 0.01_r8
  real(r8), parameter :: zlnd = 0.01
  real(r8), parameter :: zsno = 0.0024
  real(r8), parameter :: csoilc = 0.004
  real(r8), parameter :: capr = 0.34
  real(r8), parameter :: cnfac = 0.5
  real(r8), parameter :: ssi = 0.033
  real(r8), parameter :: wimp = 0.05
  real(r8), parameter :: pondmx = 10.0
  real(r8) :: pondmx_urban = 1.0_r8
  real(r8) :: o2_molar_const = 0.209_r8
  real(r8), parameter :: maxwattabfract = 1.0
  real(r8), parameter :: ht_efficiency_factor = 0.75_r8
  real(r8), parameter :: ac_efficiency_factor = 0.25_r8
  real(r8) :: ht_wasteheat_factor = 1.0_r8/ht_efficiency_factor
  real(r8) :: ac_wasteheat_factor = 1.0_r8/ac_efficiency_factor
  real(r8) :: wasteheat_limit = 100._r8
  integer,parameter :: istsoil = 1
  integer,parameter :: istice = 2
  integer,parameter :: istdlak = 3
  integer,parameter :: istslak = 4
  integer,parameter :: istwet = 5
  integer,parameter :: isturb = 6
  integer,parameter :: icol_roof = 61
  integer,parameter :: icol_sunwall = 62
  integer,parameter :: icol_shadewall = 63
  integer,parameter :: icol_road_imperv = 64
  integer,parameter :: icol_road_perv = 65
  logical, public :: set_caerdep_from_file = .true.
  logical, public :: set_dustdep_from_file = .true.
  logical, public :: create_crop_landunit = .false.
  logical, public :: allocate_all_vegpfts = .false.
  character(len=256), public :: faerdep = ' '
  character(len=256), public :: fndepdyn = ' '
  character(len=256), public :: fpftdyn = ' '
  real(r8) :: ss_alb_snw_drc(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: asm_prm_snw_drc(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: ext_cff_mss_snw_drc(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: ss_alb_snw_dfs(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: asm_prm_snw_dfs(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: ext_cff_mss_snw_dfs(idx_Mie_snw_mx,numrad_snw)
  real(r8) :: ss_alb_bc1(1,numrad_snw)
  real(r8) :: asm_prm_bc1(1,numrad_snw)
  real(r8) :: ext_cff_mss_bc1(1,numrad_snw)
  real(r8) :: ss_alb_bc2(1,numrad_snw)
  real(r8) :: asm_prm_bc2(1,numrad_snw)
  real(r8) :: ext_cff_mss_bc2(1,numrad_snw)
  real(r8) :: ss_alb_oc1(1,numrad_snw)
  real(r8) :: asm_prm_oc1(1,numrad_snw)
  real(r8) :: ext_cff_mss_oc1(1,numrad_snw)
  real(r8) :: ss_alb_oc2(1,numrad_snw)
  real(r8) :: asm_prm_oc2(1,numrad_snw)
  real(r8) :: ext_cff_mss_oc2(1,numrad_snw)
  real(r8) :: ss_alb_dst1(1,numrad_snw)
  real(r8) :: asm_prm_dst1(1,numrad_snw)
  real(r8) :: ext_cff_mss_dst1(1,numrad_snw)
  real(r8) :: ss_alb_dst2(1,numrad_snw)
  real(r8) :: asm_prm_dst2(1,numrad_snw)
  real(r8) :: ext_cff_mss_dst2(1,numrad_snw)
  real(r8) :: ss_alb_dst3(1,numrad_snw)
  real(r8) :: asm_prm_dst3(1,numrad_snw)
  real(r8) :: ext_cff_mss_dst3(1,numrad_snw)
  real(r8) :: ss_alb_dst4(1,numrad_snw)
  real(r8) :: asm_prm_dst4(1,numrad_snw)
  real(r8) :: ext_cff_mss_dst4(1,numrad_snw)
 data(ss_alb_bc1(1,i),i=1,5) / 0.515945305512804, 0.434313626536424, 0.346103765992635,&
    0.275522926330555, 0.138576096442815/
 data(asm_prm_bc1(1,i),i=1,5) / 0.521517715996158, 0.34457189840306, 0.244048159248401,&
    0.188518513380877, 0.103316928297739/
 data(ext_cff_mss_bc1(1,i),i=1,5) /25368.6111954733, 12520.3846877849, 7738.643174918, &
    5744.35461327268, 3526.76546641382/
 data(ss_alb_bc2(1,i),i=1,5) /0.287685315976181, 0.186577277125224, 0.123152237089201, &
    0.0883462885905543, 0.0403421562269378/
 data(asm_prm_bc2(1,i),i=1,5) /0.350231881885906, 0.211924244128064, 0.146188682542913, &
    0.112009439045293, 0.060565694843084/
 data(ext_cff_mss_bc2(1,i),i=1,5) / 11398.4540724821, 5922.76076637376, 4039.88947595266,&
    3261.62137894056, 2223.60028513459/
 data(ss_alb_oc1(1,i),i=1,5) / 0.996738033108225, 0.993951726870337, 0.98995641641622, &
    0.986792757460599, 0.950852907010411/
 data(asm_prm_oc1(1,i),i=1,5) / 0.771317243327679, 0.745701825432596, 0.721705644101165,&
    0.702407207901621, 0.643447858916726/
 data(ext_cff_mss_oc1(1,i),i=1,5) / 37773.5353898986, 22112.4459872647, 14719.3405499929,&
    10940.4200945733, 5441.11949854352/
 data(ss_alb_oc2(1,i),i=1,5) / 0.963132440682188, 0.920560323320592, 0.860191636407288, &
    0.813824138511211, 0.744011091642019/
 data(asm_prm_oc2(1,i),i=1,5) / 0.618810265705101, 0.57310868510342, 0.537906606684992, &
    0.511257182926184, 0.440320412154112/
 data(ext_cff_mss_oc2(1,i),i=1,5) /3288.85206279517, 1485.50576885264, 871.90125135612, &
    606.005758817735, 247.996083891168/
 data(ss_alb_dst1(1,i),i=1,5) /0.97891105715305, 0.994175916042451, 0.993357580762207, &
    0.992545751316266, 0.953291550046772/
 data(asm_prm_dst1(1,i),i=1,5) /0.690908112844937, 0.717759065247993, 0.671511248292627,&
    0.614225462567888, 0.436682950958558/
 data(ext_cff_mss_dst1(1,i),i=1,5) /2686.90326329624, 2419.98140297723, 1627.51690973548,&
    1138.23252303209, 466.104227277046/
 data(ss_alb_dst2(1,i),i=1,5) / 0.943752248802793, 0.984191668599419, 0.989309063917025, &
    0.991793946836264, 0.982999590668913/
 data(asm_prm_dst2(1,i),i=1,5) /0.699478684452806, 0.651992387581091, 0.695738438913831, &
    0.724417176862696, 0.701481090364134/
 data(ext_cff_mss_dst2(1,i),i=1,5) /841.089434044834, 987.406197502421, 1183.52284776972, &
    1267.30625580205, 993.497508579304/
 data(ss_alb_dst3(1,i),i=1,5) /0.904044530646049, 0.964651629694555, 0.968275809551522, &
    0.972598419874107, 0.977612418329876/
 data(asm_prm_dst3(1,i),i=1,5) /0.785636278417498, 0.749796744517699, 0.683301177698451, &
    0.629720518882672, 0.665531587501598/
 data(ext_cff_mss_dst3(1,i),i=1,5) /387.85423560755, 419.109723948302, 399.559447343404, &
    397.191283865122, 503.14317519429/
 data(ss_alb_dst4(1,i),i=1,5) /0.849818195355416, 0.940460325044343, 0.948316305534169, &
    0.952841175117807, 0.955379528193802/
 data(asm_prm_dst4(1,i),i=1,5) /0.849818195355416, 0.940460325044343, 0.948316305534169, &
    0.952841175117807, 0.955379528193802/
 data(ext_cff_mss_dst4(1,i),i=1,5) /196.638063554016, 202.877379461792, 208.304425287341, &
    204.723737634461, 228.755667038372/
  real(r8) :: snowage_tau(idx_T_max,idx_Tgrd_max,idx_rhos_max)
  real(r8) :: snowage_kappa(idx_T_max,idx_Tgrd_max,idx_rhos_max)
  real(r8) :: snowage_drdt0(idx_T_max,idx_Tgrd_max,idx_rhos_max)
  real, dimension(idx_Mie_snw_mx*numrad_snw) :: &
                         xx_ext_cff_mss_snw_dfs &
                        ,xx_ss_alb_snw_drc &
                        ,xx_asm_prm_snw_drc &
                        ,xx_ext_cff_mss_snw_drc &
                        ,xx_ss_alb_snw_dfs &
                        ,xx_asm_prm_snw_dfs
  real, dimension(idx_rhos_max*idx_Tgrd_max*idx_T_max) :: &
                         xx_snowage_tau &
                        ,xx_snowage_kappa &
                        ,xx_snowage_drdt0
  real(r8) :: ndep
  data ndep/0.1600056/
  real(r8),dimension(1:12) :: bcphidry,bcphodry,bcphiwet,ocphidry,ocphodry,ocphiwet,dstx01wd,dstx01dd,dstx02wd,&
                          dstx02dd,dstx03wd,dstx03dd,dstx04wd,dstx04dd
  data(bcphiwet(i),i=1,12)/2.825279e-13,2.804302e-13,2.806464e-13,2.776603e-13,2.867702e-13,2.840975e-13,&
                           3.122134e-13,3.540193e-13,3.618796e-13,3.123423e-13,2.668725e-13,2.721869e-13/
  data(bcphidry(i),i=1,12)/4.379167e-14,4.140940e-14,3.956216e-14,3.461795e-14,3.561638e-14,3.812630e-14,&
                           4.509564e-14,5.387520e-14,4.985846e-14,4.057210e-14,3.778306e-14,4.178772e-14/
  data(bcphodry(i),i=1,12)/4.192595e-14,3.831034e-14,3.536048e-14,3.209042e-14,3.280311e-14,3.226350e-14,&
                           3.723765e-14,4.297412e-14,4.106369e-14,3.602615e-14,3.536953e-14,4.030912e-14/
  data(ocphiwet(i),i=1,12)/1.162276e-12,1.151254e-12,1.188579e-12,1.186147e-12,1.340542e-12,1.292835e-12,&
                           1.628738e-12,2.033289e-12,1.964814e-12,1.479005e-12,1.043205e-12,1.068595e-12/
  data(ocphidry(i),i=1,12)/2.152982e-13,1.993085e-13,1.982182e-13,1.799778e-13,2.096774e-13,2.264119e-13,&
                           3.075992e-13,3.972984e-13,3.344011e-13,2.181304e-13,1.666979e-13,1.974062e-13/
  data(ocphodry(i),i=1,12)/1.041400e-13,9.450685e-14,9.076748e-14,8.334433e-14,9.459879e-14,9.190213e-14,&
                           1.252610e-13,1.566317e-13,1.342872e-13,9.783121e-14,8.087127e-14,9.675401e-14/
  data(dstx01wd(i),i=1,12)/3.954503e-12,4.835873e-12,5.138886e-12,4.327863e-12,4.352995e-12,5.446991e-12,&
                           5.994205e-12,5.140828e-12,3.412828e-12,2.943823e-12,3.267167e-12,3.414306e-12/
  data(dstx01dd(i),i=1,12)/1.926454e-13,2.188806e-13,2.054299e-13,1.452168e-13,1.216905e-13,1.291714e-13,&
                           1.238305e-13,1.022406e-13,8.948773e-14,1.024716e-13,1.347662e-13,1.688275e-13/
  data(dstx02wd(i),i=1,12)/9.846976e-12,1.203580e-11,1.324912e-11,1.146517e-11,1.176165e-11,1.479383e-11,&
                           1.656127e-11,1.427957e-11,9.381504e-12,7.933820e-12,8.429268e-12,8.695841e-12/
  data(dstx02dd(i),i=1,12)/2.207384e-12,2.523390e-12,2.099760e-12,1.318037e-12,1.071989e-12,1.305896e-12,&
                           1.065086e-12,8.545297e-13,7.591564e-13,9.132561e-13,1.344110e-12,1.683045e-12/
  data(dstx03wd(i),i=1,12)/5.689729e-12,7.006299e-12,8.480560e-12,8.957637e-12,1.042770e-11,1.315425e-11,&
                           1.529579e-11,1.397714e-11,9.306412e-12,7.171395e-12,6.230214e-12,5.392280e-12/
  data(dstx03dd(i),i=1,12)/1.344186e-11,1.552927e-11,1.442798e-11,9.362479e-12,8.622053e-12,1.158499e-11,&
                           1.128677e-11,8.671572e-12,6.141916e-12,6.720502e-12,8.372052e-12,1.090343e-11/
  data(dstx04wd(i),i=1,12)/5.657587e-12,7.503811e-12,1.001585e-11,1.095202e-11,1.382148e-11,1.919693e-11,&
                           2.390845e-11,2.121497e-11,1.201019e-11,7.470685e-12,5.650550e-12,4.622456e-12/
  data(dstx04dd(i),i=1,12)/7.075009e-11,8.168510e-11,8.081875e-11,6.024911e-11,6.014012e-11,7.693025e-11,&
                           7.988822e-11,6.632887e-11,4.771782e-11,4.599348e-11,4.981839e-11,5.885732e-11/
  real(r8) :: organic(1:nlevgrnd)
  data(organic(i),i=1,nlevgrnd)/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
 real(r8) :: fmax
 data fmax/0.366/
 real(r8) :: efisop(1:6)
 data(efisop(i), i=1,6)/3025.2,&
                        554.6 ,&
                        131.0 ,&
                        2629.5,&
                        164.3 ,&
                        14.1/
  real(r8) :: albsat(numcol,numrad)
  data(albsat(i,1),i=1,8)/0.12,0.11,0.10,0.09,0.08,0.07,0.06,0.05/
  data(albsat(i,2),i=1,8)/0.24,0.22,0.20,0.18,0.16,0.14,0.12,0.10/
  real(r8) :: albdry(numcol,numrad)
  data(albdry(i,1),i=1,8)/0.24,0.22,0.20,0.18,0.16,0.14,0.12,0.10/
  data(albdry(i,2),i=1,8)/0.48,0.44,0.40,0.36,0.32,0.28,0.24,0.20/
  real(r8) :: albice(numrad)
  data (albice(i),i=1,numrad) /0.80, 0.55/
  real(r8) :: alblak(numrad)
  data (alblak(i),i=1,numrad) /0.60, 0.40/
  real(r8),parameter :: betads = 0.5
  real(r8),parameter :: betais = 0.5
  real(r8) :: omegas(numrad)
  data (omegas(i),i=1,numrad) /0.8, 0.4/
  real(r8) :: zlak(1:nlevlak)
  real(r8) :: dzlak(1:nlevlak)
  real(r8) :: zsoi(1:nlevgrnd)
  real(r8) :: dzsoi(1:nlevgrnd)
  real(r8) :: zisoi(0:nlevgrnd)
    real(r8) :: sand(19)
    real(r8) :: clay(19)
    integer :: soic(19)
    integer, allocatable :: plant(:,:)
    real(r8),allocatable :: cover(:,:)
    data(sand(i), i=1,19)/92.,80.,66.,20.,5.,43.,60.,&
      10.,32.,51., 6.,22.,39.7,0.,100.,54.,17.,100.,92./
    data(clay(i), i=1,19)/ 3., 5.,10.,15.,5.,18.,27.,&
      33.,33.,41.,47.,58.,14.7,0., 0., 8.5,54., 0., 3./
    data(soic(i), i=1,19)/1,2,2,3,3,4,5,5,6,7,7,8,8,0,&
                          1,1,4,7,1/
    real(r8):: lai(numpft,12),sai(numpft,12)
    real(r8):: hvt(16),hvb(16)
    data (hvt(i),i=1,16) /17.0,17.0,14.0,35.0,35.0,18.0,20.0,20.0,&
      0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5/
    data (hvb(i),i=1,16) /8.50, 8.50,7.00,1.00,1.00,10.00,11.50,11.50,&
      0.10,0.10,0.10,0.01,0.10,0.01,0.01,0.01/
    data (lai(1,i),i=1,12) &
        /4.1,4.2,4.6,4.8,4.9,5.0,4.8,4.7,4.6,4.2,4.0,4.0/
    data (lai(2,i),i=1,12) &
        /4.1,4.2,4.6,4.8,4.9,5.0,4.8,4.7,4.6,4.2,4.0,4.0/
    data (lai(3,i),i=1,12) &
        /0.0,0.0,0.0,0.6,1.2,2.0,2.6,1.7,1.0,0.5,0.2,0.0/
    data (lai(4,i),i=1,12) &
        /4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5/
    data (lai(5,i),i=1,12) &
        /4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5/
    data (lai(6,i),i=1,12) &
        /0.8,0.7,0.4,0.5,0.5,0.7,1.7,3.0,2.5,1.6,1.0,1.0/
    data (lai(7,i),i=1,12) &
        /0.0,0.0,0.3,1.2,3.0,4.7,4.5,3.4,1.2,0.3,0.0,0.0/
    data (lai(8,i),i=1,12) &
        /0.0,0.0,0.3,1.2,3.0,4.7,4.5,3.4,1.2,0.3,0.0,0.0/
    data (lai(9,i),i=1,12) &
        /1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
    data (lai(10,i),i=1,12) &
        /0.9,0.8,0.2,0.2,0.0,0.0,0.0,0.2,0.4,0.5,0.6,0.8/
    data (lai(11,i),i=1,12) &
        /0.0,0.0,0.0,0.0,0.0,0.2,1.4,1.2,0.0,0.0,0.0,0.0/
    data (lai(12,i),i=1,12) &
        /0.4,0.5,0.6,0.7,1.2,3.0,3.5,1.5,0.7,0.6,0.5,0.4/
    data (lai(13,i),i=1,12) &
        /0.0,0.0,0.0,0.0,0.0,0.2,1.4,1.2,0.0,0.0,0.0,0.0/
    data (lai(14,i),i=1,12) &
        /0.4,0.5,0.6,0.7,1.2,3.0,3.5,1.5,0.7,0.6,0.5,0.4/
    data (lai(15,i),i=1,12) &
        /0.0,0.0,0.0,0.0,1.0,2.0,3.0,3.0,1.5,0.0,0.0,0.0/
    data (lai(16,i),i=1,12) &
        /0.0,0.0,0.0,0.0,1.0,2.0,3.0,3.0,1.5,0.0,0.0,0.0/
    data (sai(1,i),i=1,12) &
       /0.4,0.5,0.4,0.3,0.4,0.5,0.5,0.6,0.6,0.7,0.6,0.5/
    data (sai(2,i),i=1,12) &
       /0.4,0.5,0.4,0.3,0.4,0.5,0.5,0.6,0.6,0.7,0.6,0.5/
    data (sai(3,i),i=1,12) &
       /0.3,0.3,0.3,0.4,0.4,0.4,1.7,1.2,1.0,0.8,0.6,0.5/
    data (sai(4,i),i=1,12) &
       /0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5/
    data (sai(5,i),i=1,12) &
       /0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5/
    data (sai(6,i),i=1,12) &
       /0.4,0.3,0.5,0.3,0.3,0.3,0.3,0.7,0.7,1.1,0.9,0.2/
    data (sai(7,i),i=1,12) &
       /0.4,0.4,0.4,0.4,0.5,0.4,0.9,1.4,2.6,1.4,0.6,0.4/
    data (sai(8,i),i=1,12) &
       /0.4,0.4,0.4,0.4,0.5,0.4,0.9,1.4,2.6,1.4,0.6,0.4/
    data (sai(9,i),i=1,12) &
       /0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3/
    data (sai(10,i),i=1,12) &
       /0.1,0.2,0.6,0.1,0.6,0.0,0.1,0.1,0.1,0.1,0.1,0.1/
    data (sai(11,i),i=1,12) &
       /0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.5,1.4,0.1,0.1,0.1/
    data (sai(12,i),i=1,12) &
       /0.3,0.3,0.3,0.3,0.3,0.4,0.8,2.3,1.1,0.4,0.4,0.4/
    data (sai(13,i),i=1,12) &
       /0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.5,1.4,0.1,0.1,0.1/
    data (sai(14,i),i=1,12) &
       /0.3,0.3,0.3,0.3,0.3,0.4,0.8,2.3,1.1,0.4,0.4,0.4/
    data (sai(15,i),i=1,12) &
       /0.0,0.0,0.0,0.0,1.0,2.0,3.0,3.0,1.5,0.0,0.0,0.0/
    data (sai(16,i),i=1,12) &
       /0.0,0.0,0.0,0.0,1.0,2.0,3.0,3.0,1.5,0.0,0.0,0.0/
  character(len=40) pftname(0:numpft)
  real(r8) dleaf(0:numpft)
  real(r8) c3psn(0:numpft)
  real(r8) vcmx25(0:numpft)
  real(r8) mp(0:numpft)
  real(r8) qe25(0:numpft)
  real(r8) xl(0:numpft)
  real(r8) rhol(0:numpft,numrad)
  real(r8) rhos(0:numpft,numrad)
  real(r8) taul(0:numpft,numrad)
  real(r8) taus(0:numpft,numrad)
  real(r8) z0mr(0:numpft)
  real(r8) displar(0:numpft)
  real(r8) roota_par(0:numpft)
  real(r8) rootb_par(0:numpft)
data (pftname(i),i=1,16)/'needleleaf_evergreen_temperate_tree',&
                           'needleleaf_evergreen_boreal_tree' ,&
                           'needleleaf_deciduous_boreal_tree' ,&
                           'broadleaf_evergreen_tropical_tree' ,&
                           'broadleaf_evergreen_temperate_tree' ,&
                           'broadleaf_deciduous_tropical_tree' ,&
                           'broadleaf_deciduous_temperate_tree' ,&
                           'broadleaf_deciduous_boreal_tree' ,&
                           'broadleaf_evergreen_shrub' ,&
                           'broadleaf_deciduous_temperate_shrub',&
                           'broadleaf_deciduous_boreal_shrub' ,&
                           'c3_arctic_grass' ,&
                           'c3_non-arctic_grass' ,&
                           'c4_grass' ,&
                           'corn' ,&
                           'wheat'/
  data (z0mr(i),i=1,16)/ 0.055, 0.055, 0.055, 0.075, 0.075, &
       0.055,0.055, 0.055, 0.120, 0.120, 0.120, 0.120, 0.120,&
       0.120, 0.120, 0.120/
  data (displar(i),i=1,16)/ 0.67, 0.67, 0.67, 0.67, 0.67, &
         0.67, 0.67, 0.67, 0.68, 0.68, 0.68, 0.68, 0.68, &
         0.68, 0.68, 0.68/
  data (dleaf(i),i=1,16)/ 0.04, 0.04, 0.04, 0.04, 0.04,&
         0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04,&
         0.04, 0.04, 0.04/
  data (c3psn(i),i=1,16)/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
        1.0,1.0,1.0,1.0,1.0,0.0,1.0,1.0/
  data (vcmx25(i),i=1,16)/51.0,43.0,43.0,75.0,69.0,40.0,&
       51.0,51.0,17.0,17.0,33.0,43.0,43.0,24.0,50.0,50.0/
  data (mp(i),i=1,16)/6.0,6.0,6.0,9.0,9.0,9.0,9.0,9.0,&
        9.0,9.0,9.0,9.0,9.0,5.0,9.0,9.0/
  data (qe25(i),i=1,16)/ 0.06, 0.06, 0.06, 0.06, 0.06,&
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,&
        0.04, 0.06, 0.06/
  data (rhol(i,1),i=1,16)/ 0.07, 0.07, 0.07, 0.10, 0.10,&
        0.10, 0.10, 0.10, 0.07, 0.10, 0.10, 0.11, 0.11,&
        0.11, 0.11, 0.11/
  data (rhol(i,2),i=1,16)/ 0.35, 0.35, 0.35, 0.45, 0.45,&
        0.45, 0.45, 0.45, 0.35, 0.45, 0.45, 0.58, 0.58, &
        0.58, 0.58, 0.58/
  data (rhos(i,1),i=1,16) /0.16, 0.16, 0.16, 0.16, 0.16,&
         0.16, 0.16, 0.16, 0.16, 0.16, 0.16, 0.36, 0.36,&
         0.36, 0.36, 0.36/
  data (rhos(i,2),i=1,16)/ 0.39, 0.39, 0.39, 0.39, 0.39,&
        0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.58, 0.58, &
        0.58, 0.58, 0.58/
  data (taul(i,1),i=1,16)/ 0.05, 0.05, 0.05, 0.05, 0.05,&
        0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.07, 0.07,&
        0.07, 0.07, 0.07/
  data (taul(i,2),i=1,16)/ 0.10, 0.10, 0.10, 0.25, 0.25,&
        0.25, 0.25, 0.25, 0.10, 0.25, 0.25, 0.25, 0.25, &
        0.25, 0.25, 0.25/
  data (taus(i,1),i=1,16)/0.001, 0.001, 0.001, 0.001,&
       0.001,0.001, 0.001, 0.001, 0.001, 0.001, 0.001,&
       0.220, 0.220, 0.220, 0.220, 0.220/
  data (taus(i,2),i=1,16)/ 0.001, 0.001, 0.001, 0.001,&
       0.001, 0.001, 0.001, 0.001, 0.001, 0.001, &
       0.001, 0.380, 0.380, 0.380, 0.380, 0.380/
  data (xl(i),i=1,16)/0.01,0.01,0.01,0.10,0.10, 0.01,&
       0.25, 0.25, 0.01, 0.25, 0.25, -0.30, -0.30,&
       -0.30, -0.30, -0.30/
  data (roota_par(i),i=1,16)/ 7.0, 7.0, 7.0, 7.0,&
      7.0, 6.0, 6.0, 6.0, 7.0, 7.0, 7.0, 11.0, &
      11.0, 11.0, 6.0, 6.0/
  data (rootb_par(i),i=1,16)/ 2.0, 2.0, 2.0, &
     1.0, 1.0, 2.0, 2.0, 2.0, 1.5, 1.5, 1.5, &
     2.0, 2.0, 2.0, 3.0, 3.0/
contains
 subroutine var_par
    allocate (plant(num_landcover_types,maxpatch_pft))
    allocate (cover(num_landcover_types,maxpatch_pft))
  if(num_landcover_types== 24.or. num_landcover_types==28) then
      plant(:,1) = (/ 0, 15, 15, 15, 15, 15, &
                            14, 9, 9, 14, 7, 3, &
                             4, 1, 1, 0, 0, 4, &
                            11, 11, 2, 11, 11, 0/)
      cover(:,1) = (/100., 85., 85., 85., 50., 40., &
                             60., 80., 50., 70., 75., 50., &
                             95., 75., 37., 100., 100., 80., &
                             10., 30., 13., 20., 10., 100./)
      plant(:,2) = (/ 0, 0, 0, 0, 14, 3, &
                            13, 0, 14, 6, 0, 0, &
                             0, 0, 7, 0, 0, 0, &
                             0, 12, 3, 12, 12, 0/)
      cover(:,2) = (/0., 15., 15., 15., 35., 30., &
                             20., 20., 30., 30., 25., 50., &
                              5., 25., 37., 0., 0., 20., &
                             90., 30., 13., 20., 10., 0./)
      plant(:,3) = (/ 0, 0, 0, 0, 0, 0, &
                             0, 0, 0, 0, 0, 0, &
                             0, 0, 0, 0, 0, 0, &
                             0, 0, 10, 0, 0, 0/)
      cover(:,3) = (/0., 0., 0., 0., 15., 30., &
                             20., 0., 20., 0., 0., 0., &
                              0., 0., 26., 0., 0., 0., &
                              0., 40., 24., 60., 80., 0./)
      plant(:,4) = (/ 0, 0, 0, 0, 0, 0, &
                             0, 0, 0, 0, 0, 0, &
                             0, 0, 0, 0, 0, 0, &
                             0, 0, 0, 0, 0, 0/)
      cover(:,4) = (/ 0., 0., 0., 0., 0., 0., &
                             0., 0., 0., 0., 0., 0., &
                             0., 0., 0., 0., 0., 0., &
                             0., 0., 50., 0., 0., 0./)
  else if (num_landcover_types== 20.or. num_landcover_types==21) then
        plant(:,1) = (/1, 4, 3, 7, 1, 9, 9, 9, 14, 14, &
                       0, 15, 0, 15, 0, 11, 0, 2, 11, 11/)
        cover(:,1) = (/75., 95.,50., 75., 37., 80., 50., 80., 70.,60.,&
                       100.,85.,100., 50., 100.,10., 100.,13., 20.,10./)
        plant(:,2) = (/0, 0, 0, 0, 7, 0, 14, 0, 6, 13, &
                       0, 0, 0, 14,0, 0, 0, 3, 12,12/)
        cover(:,2) = (/25., 5., 50.,25.,37.,20.,30.,20.,30.,20.,&
                       0., 15.,0., 35.,0., 90.,0., 13.,20.,10./)
        plant(:,3) = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                       0, 0, 0, 0, 0, 0, 0,10, 0, 0/)
        cover(:,3) = (/0.,0.,0., 0., 26.,0., 20., 0., 0., 20.,&
                       0.,0.,0., 15.,0., 0., 0., 24., 60.,80./)
        plant(:,4) = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        cover(:,4) = (/0.,0.,0.,0.,0.,0.,0.,0., 0.,0.,&
                       0.,0.,0.,0.,0.,0.,0.,50.,0.,0./)
  else
        write(6,*)'CLM works only for USGS (24) and MODIS(20) land use types, '
        write(6,*)'but the current number of land use types is ',num_landcover_types
        call endrun()
  end if
 end subroutine var_par
end module clm_varcon
module clm_varsur
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar, only : maxpatch,lsmlon, lsmlat, nlevsoi
  use module_cam_support, only: endrun
  implicit none
  save
  integer :: numlon(lsmlat)
  real(r8):: latixy(1)
  real(r8):: longxy(1)
  real(r8):: area(1)
  real(r8):: landarea
  real(r8):: lats(lsmlat+1)
  real(r8):: lonw(lsmlon+1,lsmlat)
  real(r8):: lsmedge(4)
  logical :: pole_points
  logical :: fullgrid = .true.
  logical :: offline_rdgrid
  real(r8), allocatable :: gti(:)
  integer , allocatable :: soic2d(:)
  real(r8) , allocatable :: efisop2d(:,:)
  real(r8), allocatable :: sand3d(:,:)
  real(r8), allocatable :: clay3d(:,:)
  real(r8), allocatable :: organic3d(:,:)
  real(r8), allocatable :: pctgla(:)
  real(r8), allocatable :: pctlak(:)
  real(r8), allocatable :: pctwet(:)
  real(r8), allocatable :: pcturb(:)
  integer , allocatable :: vegxy(:,:)
  real(r8), allocatable,target :: wtxy(:,:)
  public :: varsur_alloc
  public :: varsur_dealloc
contains
  subroutine varsur_alloc
    implicit none
    integer :: ier,begg,endg
  begg=1
  endg=1
    allocate (vegxy(1,maxpatch), &
              wtxy(1,maxpatch), &
              stat=ier)
    if (ier /= 0) then
       write(6,*)'initialize allocation error'
       call endrun()
    endif
    allocate (soic2d(begg:endg), &
              gti(begg:endg), &
              efisop2d(1:6,begg:endg),&
              sand3d(begg:endg,nlevsoi), &
              clay3d(begg:endg,nlevsoi), &
              organic3d(begg:endg,nlevsoi), &
              pctgla(begg:endg), &
              pctlak(begg:endg), &
              pctwet(begg:endg), &
              pcturb(begg:endg), stat=ier)
    if (ier /= 0) then
       write(6,*)'varsur_alloc(): allocation error'
       call endrun()
    endif
  end subroutine varsur_alloc
  subroutine varsur_dealloc
    implicit none
    deallocate (soic2d, &
                gti, &
               efisop2d,&
                sand3d, &
                clay3d, &
               organic3d,&
                pctgla, &
                pctlak, &
                pctwet, &
                pcturb, &
                wtxy, &
                vegxy)
  end subroutine varsur_dealloc
end module clm_varsur
module clmtype
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
type, public :: energy_balance_type
   real(r8), pointer :: errsoi(:)
   real(r8), pointer :: errseb(:)
   real(r8), pointer :: errsol(:)
   real(r8), pointer :: errlon(:)
end type energy_balance_type
type, public :: water_balance_type
   real(r8), pointer :: begwb(:)
   real(r8), pointer :: endwb(:)
   real(r8), pointer :: errh2o(:)
end type water_balance_type
type, public :: carbon_balance_type
   real(r8), pointer :: begcb(:)
   real(r8), pointer :: endcb(:)
   real(r8), pointer :: errcb(:)
end type carbon_balance_type
type, public :: nitrogen_balance_type
   real(r8), pointer :: begnb(:)
   real(r8), pointer :: endnb(:)
   real(r8), pointer :: errnb(:)
end type nitrogen_balance_type
type, public :: pft_pstate_type
   integer , pointer :: frac_veg_nosno(:)
   integer , pointer :: frac_veg_nosno_alb(:)
   real(r8), pointer :: emv(:)
   real(r8), pointer :: z0mv(:)
   real(r8), pointer :: z0hv(:)
   real(r8), pointer :: z0qv(:)
   real(r8), pointer :: rootfr(:,:)
   real(r8), pointer :: rootr(:,:)
   real(r8), pointer :: rresis(:,:)
   real(r8), pointer :: dewmx(:)
   real(r8), pointer :: rssun(:)
   real(r8), pointer :: rssha(:)
   real(r8), pointer :: laisun(:)
   real(r8), pointer :: laisha(:)
   real(r8), pointer :: btran(:)
   real(r8), pointer :: fsun(:)
   real(r8), pointer :: tlai(:)
   real(r8), pointer :: tsai(:)
   real(r8), pointer :: elai(:)
   real(r8), pointer :: esai(:)
   real(r8), pointer :: fwet(:)
   real(r8), pointer :: fdry(:)
   real(r8), pointer :: dt_veg(:)
   real(r8), pointer :: htop(:)
   real(r8), pointer :: hbot(:)
   real(r8), pointer :: z0m(:)
   real(r8), pointer :: displa(:)
   real(r8), pointer :: albd(:,:)
   real(r8), pointer :: albi(:,:)
   real(r8), pointer :: fabd(:,:)
   real(r8), pointer :: fabi(:,:)
   real(r8), pointer :: ftdd(:,:)
   real(r8), pointer :: ftid(:,:)
   real(r8), pointer :: ftii(:,:)
   real(r8), pointer :: u10(:)
   real(r8), pointer :: ram1(:)
   real(r8), pointer :: fv(:)
   real(r8), pointer :: forc_hgt_u_pft(:)
   real(r8), pointer :: forc_hgt_t_pft(:)
   real(r8), pointer :: forc_hgt_q_pft(:)
   real(r8), pointer :: vds(:)
   real(r8), pointer :: slasun(:)
   real(r8), pointer :: slasha(:)
   real(r8), pointer :: lncsun(:)
   real(r8), pointer :: lncsha(:)
   real(r8), pointer :: vcmxsun(:)
   real(r8), pointer :: vcmxsha(:)
   real(r8), pointer :: gdir(:)
   real(r8), pointer :: omega(:,:)
   real(r8), pointer :: eff_kid(:,:)
   real(r8), pointer :: eff_kii(:,:)
   real(r8), pointer :: sun_faid(:,:)
   real(r8), pointer :: sun_faii(:,:)
   real(r8), pointer :: sha_faid(:,:)
   real(r8), pointer :: sha_faii(:,:)
   real(r8), pointer :: cisun(:)
   real(r8), pointer :: cisha(:)
   real(r8), pointer :: sandfrac(:)
   real(r8), pointer :: clayfrac(:)
   real(r8), pointer :: mlaidiff(:)
   real(r8), pointer :: rb1(:)
   real(r8), pointer :: annlai(:,:)
end type pft_pstate_type
type, public :: pft_epc_type
   integer , pointer :: noveg(:)
   integer , pointer :: tree(:)
   real(r8), pointer :: smpso(:)
   real(r8), pointer :: smpsc(:)
   real(r8), pointer :: fnitr(:)
   real(r8), pointer :: foln(:)
   real(r8), pointer :: dleaf(:)
   real(r8), pointer :: c3psn(:)
   real(r8), pointer :: vcmx25(:)
   real(r8), pointer :: mp(:)
   real(r8), pointer :: qe25(:)
   real(r8), pointer :: xl(:)
   real(r8), pointer :: rhol(:,:)
   real(r8), pointer :: rhos(:,:)
   real(r8), pointer :: taul(:,:)
   real(r8), pointer :: taus(:,:)
   real(r8), pointer :: z0mr(:)
   real(r8), pointer :: displar(:)
   real(r8), pointer :: roota_par(:)
   real(r8), pointer :: rootb_par(:)
   real(r8), pointer :: sla(:)
   real(r8), pointer :: dwood(:)
   real(r8), pointer :: slatop(:)
   real(r8), pointer :: dsladlai(:)
   real(r8), pointer :: leafcn(:)
   real(r8), pointer :: flnr(:)
   real(r8), pointer :: woody(:)
   real(r8), pointer :: lflitcn(:)
   real(r8), pointer :: frootcn(:)
   real(r8), pointer :: livewdcn(:)
   real(r8), pointer :: deadwdcn(:)
   real(r8), pointer :: froot_leaf(:)
   real(r8), pointer :: stem_leaf(:)
   real(r8), pointer :: croot_stem(:)
   real(r8), pointer :: flivewd(:)
   real(r8), pointer :: fcur(:)
   real(r8), pointer :: lf_flab(:)
   real(r8), pointer :: lf_fcel(:)
   real(r8), pointer :: lf_flig(:)
   real(r8), pointer :: fr_flab(:)
   real(r8), pointer :: fr_fcel(:)
   real(r8), pointer :: fr_flig(:)
   real(r8), pointer :: dw_fcel(:)
   real(r8), pointer :: dw_flig(:)
   real(r8), pointer :: leaf_long(:)
   real(r8), pointer :: evergreen(:)
   real(r8), pointer :: stress_decid(:)
   real(r8), pointer :: season_decid(:)
   real(r8), pointer :: resist(:)
end type pft_epc_type
type, public :: pft_epv_type
   real(r8), pointer :: dormant_flag(:)
   real(r8), pointer :: days_active(:)
   real(r8), pointer :: onset_flag(:)
   real(r8), pointer :: onset_counter(:)
   real(r8), pointer :: onset_gddflag(:)
   real(r8), pointer :: onset_fdd(:)
   real(r8), pointer :: onset_gdd(:)
   real(r8), pointer :: onset_swi(:)
   real(r8), pointer :: offset_flag(:)
   real(r8), pointer :: offset_counter(:)
   real(r8), pointer :: offset_fdd(:)
   real(r8), pointer :: offset_swi(:)
   real(r8), pointer :: lgsf(:)
   real(r8), pointer :: bglfr(:)
   real(r8), pointer :: bgtr(:)
   real(r8), pointer :: dayl(:)
   real(r8), pointer :: prev_dayl(:)
   real(r8), pointer :: annavg_t2m(:)
   real(r8), pointer :: tempavg_t2m(:)
   real(r8), pointer :: gpp(:)
   real(r8), pointer :: availc(:)
   real(r8), pointer :: xsmrpool_recover(:)
   real(r8), pointer :: alloc_pnow(:)
   real(r8), pointer :: c_allometry(:)
   real(r8), pointer :: n_allometry(:)
   real(r8), pointer :: plant_ndemand(:)
   real(r8), pointer :: tempsum_potential_gpp(:)
   real(r8), pointer :: annsum_potential_gpp(:)
   real(r8), pointer :: tempmax_retransn(:)
   real(r8), pointer :: annmax_retransn(:)
   real(r8), pointer :: avail_retransn(:)
   real(r8), pointer :: plant_nalloc(:)
   real(r8), pointer :: plant_calloc(:)
   real(r8), pointer :: excess_cflux(:)
   real(r8), pointer :: downreg(:)
   real(r8), pointer :: prev_leafc_to_litter(:)
   real(r8), pointer :: prev_frootc_to_litter(:)
   real(r8), pointer :: tempsum_npp(:)
   real(r8), pointer :: annsum_npp(:)
end type pft_epv_type
type, public :: pft_estate_type
   real(r8), pointer :: t_ref2m(:)
   real(r8), pointer :: t_ref2m_min(:)
   real(r8), pointer :: t_ref2m_max(:)
   real(r8), pointer :: t_ref2m_min_inst(:)
   real(r8), pointer :: t_ref2m_max_inst(:)
   real(r8), pointer :: q_ref2m(:)
   real(r8), pointer :: t_ref2m_u(:)
   real(r8), pointer :: t_ref2m_r(:)
   real(r8), pointer :: t_ref2m_min_u(:)
   real(r8), pointer :: t_ref2m_min_r(:)
   real(r8), pointer :: t_ref2m_max_u(:)
   real(r8), pointer :: t_ref2m_max_r(:)
   real(r8), pointer :: t_ref2m_min_inst_u(:)
   real(r8), pointer :: t_ref2m_min_inst_r(:)
   real(r8), pointer :: t_ref2m_max_inst_u(:)
   real(r8), pointer :: t_ref2m_max_inst_r(:)
   real(r8), pointer :: rh_ref2m(:)
   real(r8), pointer :: rh_ref2m_u(:)
   real(r8), pointer :: rh_ref2m_r(:)
   real(r8), pointer :: t_veg(:)
   real(r8), pointer :: thm(:)
end type pft_estate_type
type, public :: pft_wstate_type
   real(r8), pointer :: h2ocan(:)
end type pft_wstate_type
type, public :: pft_cstate_type
   real(r8), pointer :: leafcmax(:)
   real(r8), pointer :: leafc(:)
   real(r8), pointer :: leafc_storage(:)
   real(r8), pointer :: leafc_xfer(:)
   real(r8), pointer :: frootc(:)
   real(r8), pointer :: frootc_storage(:)
   real(r8), pointer :: frootc_xfer(:)
   real(r8), pointer :: livestemc(:)
   real(r8), pointer :: livestemc_storage(:)
   real(r8), pointer :: livestemc_xfer(:)
   real(r8), pointer :: deadstemc(:)
   real(r8), pointer :: deadstemc_storage(:)
   real(r8), pointer :: deadstemc_xfer(:)
   real(r8), pointer :: livecrootc(:)
   real(r8), pointer :: livecrootc_storage(:)
   real(r8), pointer :: livecrootc_xfer(:)
   real(r8), pointer :: deadcrootc(:)
   real(r8), pointer :: deadcrootc_storage(:)
   real(r8), pointer :: deadcrootc_xfer(:)
   real(r8), pointer :: gresp_storage(:)
   real(r8), pointer :: gresp_xfer(:)
   real(r8), pointer :: cpool(:)
   real(r8), pointer :: xsmrpool(:)
   real(r8), pointer :: pft_ctrunc(:)
   real(r8), pointer :: dispvegc(:)
   real(r8), pointer :: storvegc(:)
   real(r8), pointer :: totvegc(:)
   real(r8), pointer :: totpftc(:)
end type pft_cstate_type
type, public :: pft_nstate_type
   real(r8), pointer :: leafn(:)
   real(r8), pointer :: leafn_storage(:)
   real(r8), pointer :: leafn_xfer(:)
   real(r8), pointer :: frootn(:)
   real(r8), pointer :: frootn_storage(:)
   real(r8), pointer :: frootn_xfer(:)
   real(r8), pointer :: livestemn(:)
   real(r8), pointer :: livestemn_storage(:)
   real(r8), pointer :: livestemn_xfer(:)
   real(r8), pointer :: deadstemn(:)
   real(r8), pointer :: deadstemn_storage(:)
   real(r8), pointer :: deadstemn_xfer(:)
   real(r8), pointer :: livecrootn(:)
   real(r8), pointer :: livecrootn_storage(:)
   real(r8), pointer :: livecrootn_xfer(:)
   real(r8), pointer :: deadcrootn(:)
   real(r8), pointer :: deadcrootn_storage(:)
   real(r8), pointer :: deadcrootn_xfer(:)
   real(r8), pointer :: retransn(:)
   real(r8), pointer :: npool(:)
   real(r8), pointer :: pft_ntrunc(:)
   real(r8), pointer :: dispvegn(:)
   real(r8), pointer :: storvegn(:)
   real(r8), pointer :: totvegn(:)
   real(r8), pointer :: totpftn(:)
end type pft_nstate_type
type, public :: pft_vstate_type
   real(r8), pointer :: t_veg24(:)
   real(r8), pointer :: t_veg240(:)
   real(r8), pointer :: fsd24(:)
   real(r8), pointer :: fsd240(:)
   real(r8), pointer :: fsi24(:)
   real(r8), pointer :: fsi240(:)
   real(r8), pointer :: fsun24(:)
   real(r8), pointer :: fsun240(:)
   real(r8), pointer :: elai_p(:)
end type pft_vstate_type
type, public :: pft_eflux_type
   real(r8), pointer :: sabg(:)
   real(r8), pointer :: sabv(:)
   real(r8), pointer :: fsa(:)
   real(r8), pointer :: fsa_u(:)
   real(r8), pointer :: fsa_r(:)
   real(r8), pointer :: fsr(:)
   real(r8), pointer :: parsun(:)
   real(r8), pointer :: parsha(:)
   real(r8), pointer :: dlrad(:)
   real(r8), pointer :: ulrad(:)
   real(r8), pointer :: eflx_lh_tot(:)
   real(r8), pointer :: eflx_lh_tot_u(:)
   real(r8), pointer :: eflx_lh_tot_r(:)
   real(r8), pointer :: eflx_lh_grnd(:)
   real(r8), pointer :: eflx_soil_grnd(:)
   real(r8), pointer :: eflx_soil_grnd_u(:)
   real(r8), pointer :: eflx_soil_grnd_r(:)
   real(r8), pointer :: eflx_sh_tot(:)
   real(r8), pointer :: eflx_sh_tot_u(:)
   real(r8), pointer :: eflx_sh_tot_r(:)
   real(r8), pointer :: eflx_sh_grnd(:)
   real(r8), pointer :: eflx_sh_veg(:)
   real(r8), pointer :: eflx_lh_vege(:)
   real(r8), pointer :: eflx_lh_vegt(:)
   real(r8), pointer :: eflx_wasteheat_pft(:)
   real(r8), pointer :: eflx_heat_from_ac_pft(:)
   real(r8), pointer :: eflx_traffic_pft(:)
   real(r8), pointer :: eflx_anthro(:)
   real(r8), pointer :: cgrnd(:)
   real(r8), pointer :: cgrndl(:)
   real(r8), pointer :: cgrnds(:)
   real(r8), pointer :: eflx_gnet(:)
   real(r8), pointer :: dgnetdT(:)
   real(r8), pointer :: eflx_lwrad_out(:)
   real(r8), pointer :: eflx_lwrad_net(:)
   real(r8), pointer :: eflx_lwrad_net_u(:)
   real(r8), pointer :: eflx_lwrad_net_r(:)
   real(r8), pointer :: netrad(:)
   real(r8), pointer :: fsds_vis_d(:)
   real(r8), pointer :: fsds_nir_d(:)
   real(r8), pointer :: fsds_vis_i(:)
   real(r8), pointer :: fsds_nir_i(:)
   real(r8), pointer :: fsr_vis_d(:)
   real(r8), pointer :: fsr_nir_d(:)
   real(r8), pointer :: fsr_vis_i(:)
   real(r8), pointer :: fsr_nir_i(:)
   real(r8), pointer :: fsds_vis_d_ln(:)
   real(r8), pointer :: fsds_nir_d_ln(:)
   real(r8), pointer :: fsr_vis_d_ln(:)
   real(r8), pointer :: fsr_nir_d_ln(:)
   real(r8), pointer :: sun_add(:,:)
   real(r8), pointer :: tot_aid(:,:)
   real(r8), pointer :: sun_aid(:,:)
   real(r8), pointer :: sun_aii(:,:)
   real(r8), pointer :: sha_aid(:,:)
   real(r8), pointer :: sha_aii(:,:)
   real(r8), pointer :: sun_atot(:,:)
   real(r8), pointer :: sha_atot(:,:)
   real(r8), pointer :: sun_alf(:,:)
   real(r8), pointer :: sha_alf(:,:)
   real(r8), pointer :: sun_aperlai(:,:)
   real(r8), pointer :: sha_aperlai(:,:)
   real(r8), pointer :: sabg_lyr(:,:)
   real(r8), pointer :: sfc_frc_aer(:)
   real(r8), pointer :: sfc_frc_bc(:)
   real(r8), pointer :: sfc_frc_oc(:)
   real(r8), pointer :: sfc_frc_dst(:)
   real(r8), pointer :: sfc_frc_aer_sno(:)
   real(r8), pointer :: sfc_frc_bc_sno(:)
   real(r8), pointer :: sfc_frc_oc_sno(:)
   real(r8), pointer :: sfc_frc_dst_sno(:)
   real(r8), pointer :: fsr_sno_vd(:)
   real(r8), pointer :: fsr_sno_nd(:)
   real(r8), pointer :: fsr_sno_vi(:)
   real(r8), pointer :: fsr_sno_ni(:)
   real(r8), pointer :: fsds_sno_vd(:)
   real(r8), pointer :: fsds_sno_nd(:)
   real(r8), pointer :: fsds_sno_vi(:)
   real(r8), pointer :: fsds_sno_ni(:)
end type pft_eflux_type
type, public :: pft_mflux_type
   real(r8),pointer :: taux(:)
   real(r8),pointer :: tauy(:)
end type pft_mflux_type
type, public :: pft_wflux_type
   real(r8), pointer :: qflx_prec_intr(:)
   real(r8), pointer :: qflx_prec_grnd(:)
   real(r8), pointer :: qflx_rain_grnd(:)
   real(r8), pointer :: qflx_snow_grnd(:)
   real(r8), pointer :: qflx_snwcp_ice(:)
   real(r8), pointer :: qflx_snwcp_liq(:)
   real(r8), pointer :: qflx_evap_veg(:)
   real(r8), pointer :: qflx_tran_veg(:)
   real(r8), pointer :: qflx_evap_can(:)
   real(r8), pointer :: qflx_evap_soi(:)
   real(r8), pointer :: qflx_evap_tot(:)
   real(r8), pointer :: qflx_evap_grnd(:)
   real(r8), pointer :: qflx_dew_grnd(:)
   real(r8), pointer :: qflx_sub_snow(:)
   real(r8), pointer :: qflx_dew_snow(:)
end type pft_wflux_type
type, public :: pft_cflux_type
   real(r8), pointer :: psnsun(:)
   real(r8), pointer :: psnsha(:)
   real(r8), pointer :: fpsn(:)
   real(r8), pointer :: fco2(:)
   real(r8), pointer :: m_leafc_to_litter(:)
   real(r8), pointer :: m_leafc_storage_to_litter(:)
   real(r8), pointer :: m_leafc_xfer_to_litter(:)
   real(r8), pointer :: m_frootc_to_litter(:)
   real(r8), pointer :: m_frootc_storage_to_litter(:)
   real(r8), pointer :: m_frootc_xfer_to_litter(:)
   real(r8), pointer :: m_livestemc_to_litter(:)
   real(r8), pointer :: m_livestemc_storage_to_litter(:)
   real(r8), pointer :: m_livestemc_xfer_to_litter(:)
   real(r8), pointer :: m_deadstemc_to_litter(:)
   real(r8), pointer :: m_deadstemc_storage_to_litter(:)
   real(r8), pointer :: m_deadstemc_xfer_to_litter(:)
   real(r8), pointer :: m_livecrootc_to_litter(:)
   real(r8), pointer :: m_livecrootc_storage_to_litter(:)
   real(r8), pointer :: m_livecrootc_xfer_to_litter(:)
   real(r8), pointer :: m_deadcrootc_to_litter(:)
   real(r8), pointer :: m_deadcrootc_storage_to_litter(:)
   real(r8), pointer :: m_deadcrootc_xfer_to_litter(:)
   real(r8), pointer :: m_gresp_storage_to_litter(:)
   real(r8), pointer :: m_gresp_xfer_to_litter(:)
   real(r8), pointer :: hrv_leafc_to_litter(:)
   real(r8), pointer :: hrv_leafc_storage_to_litter(:)
   real(r8), pointer :: hrv_leafc_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootc_to_litter(:)
   real(r8), pointer :: hrv_frootc_storage_to_litter(:)
   real(r8), pointer :: hrv_frootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemc_to_litter(:)
   real(r8), pointer :: hrv_livestemc_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_to_prod10c(:)
   real(r8), pointer :: hrv_deadstemc_to_prod100c(:)
   real(r8), pointer :: hrv_deadstemc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_gresp_storage_to_litter(:)
   real(r8), pointer :: hrv_gresp_xfer_to_litter(:)
   real(r8), pointer :: hrv_xsmrpool_to_atm(:)
   real(r8), pointer :: m_leafc_to_fire(:)
   real(r8), pointer :: m_leafc_storage_to_fire(:)
   real(r8), pointer :: m_leafc_xfer_to_fire(:)
   real(r8), pointer :: m_frootc_to_fire(:)
   real(r8), pointer :: m_frootc_storage_to_fire(:)
   real(r8), pointer :: m_frootc_xfer_to_fire(:)
   real(r8), pointer :: m_livestemc_to_fire(:)
   real(r8), pointer :: m_livestemc_storage_to_fire(:)
   real(r8), pointer :: m_livestemc_xfer_to_fire(:)
   real(r8), pointer :: m_deadstemc_to_fire(:)
   real(r8), pointer :: m_deadstemc_to_litter_fire(:)
   real(r8), pointer :: m_deadstemc_storage_to_fire(:)
   real(r8), pointer :: m_deadstemc_xfer_to_fire(:)
   real(r8), pointer :: m_livecrootc_to_fire(:)
   real(r8), pointer :: m_livecrootc_storage_to_fire(:)
   real(r8), pointer :: m_livecrootc_xfer_to_fire(:)
   real(r8), pointer :: m_deadcrootc_to_fire(:)
   real(r8), pointer :: m_deadcrootc_to_litter_fire(:)
   real(r8), pointer :: m_deadcrootc_storage_to_fire(:)
   real(r8), pointer :: m_deadcrootc_xfer_to_fire(:)
   real(r8), pointer :: m_gresp_storage_to_fire(:)
   real(r8), pointer :: m_gresp_xfer_to_fire(:)
   real(r8), pointer :: leafc_xfer_to_leafc(:)
   real(r8), pointer :: frootc_xfer_to_frootc(:)
   real(r8), pointer :: livestemc_xfer_to_livestemc(:)
   real(r8), pointer :: deadstemc_xfer_to_deadstemc(:)
   real(r8), pointer :: livecrootc_xfer_to_livecrootc(:)
   real(r8), pointer :: deadcrootc_xfer_to_deadcrootc(:)
   real(r8), pointer :: leafc_to_litter(:)
   real(r8), pointer :: frootc_to_litter(:)
   real(r8), pointer :: leaf_mr(:)
   real(r8), pointer :: froot_mr(:)
   real(r8), pointer :: livestem_mr(:)
   real(r8), pointer :: livecroot_mr(:)
   real(r8), pointer :: leaf_curmr(:)
   real(r8), pointer :: froot_curmr(:)
   real(r8), pointer :: livestem_curmr(:)
   real(r8), pointer :: livecroot_curmr(:)
   real(r8), pointer :: leaf_xsmr(:)
   real(r8), pointer :: froot_xsmr(:)
   real(r8), pointer :: livestem_xsmr(:)
   real(r8), pointer :: livecroot_xsmr(:)
   real(r8), pointer :: psnsun_to_cpool(:)
   real(r8), pointer :: psnshade_to_cpool(:)
   real(r8), pointer :: cpool_to_xsmrpool(:)
   real(r8), pointer :: cpool_to_leafc(:)
   real(r8), pointer :: cpool_to_leafc_storage(:)
   real(r8), pointer :: cpool_to_frootc(:)
   real(r8), pointer :: cpool_to_frootc_storage(:)
   real(r8), pointer :: cpool_to_livestemc(:)
   real(r8), pointer :: cpool_to_livestemc_storage(:)
   real(r8), pointer :: cpool_to_deadstemc(:)
   real(r8), pointer :: cpool_to_deadstemc_storage(:)
   real(r8), pointer :: cpool_to_livecrootc(:)
   real(r8), pointer :: cpool_to_livecrootc_storage(:)
   real(r8), pointer :: cpool_to_deadcrootc(:)
   real(r8), pointer :: cpool_to_deadcrootc_storage(:)
   real(r8), pointer :: cpool_to_gresp_storage(:)
   real(r8), pointer :: cpool_leaf_gr(:)
   real(r8), pointer :: cpool_leaf_storage_gr(:)
   real(r8), pointer :: transfer_leaf_gr(:)
   real(r8), pointer :: cpool_froot_gr(:)
   real(r8), pointer :: cpool_froot_storage_gr(:)
   real(r8), pointer :: transfer_froot_gr(:)
   real(r8), pointer :: cpool_livestem_gr(:)
   real(r8), pointer :: cpool_livestem_storage_gr(:)
   real(r8), pointer :: transfer_livestem_gr(:)
   real(r8), pointer :: cpool_deadstem_gr(:)
   real(r8), pointer :: cpool_deadstem_storage_gr(:)
   real(r8), pointer :: transfer_deadstem_gr(:)
   real(r8), pointer :: cpool_livecroot_gr(:)
   real(r8), pointer :: cpool_livecroot_storage_gr(:)
   real(r8), pointer :: transfer_livecroot_gr(:)
   real(r8), pointer :: cpool_deadcroot_gr(:)
   real(r8), pointer :: cpool_deadcroot_storage_gr(:)
   real(r8), pointer :: transfer_deadcroot_gr(:)
   real(r8), pointer :: leafc_storage_to_xfer(:)
   real(r8), pointer :: frootc_storage_to_xfer(:)
   real(r8), pointer :: livestemc_storage_to_xfer(:)
   real(r8), pointer :: deadstemc_storage_to_xfer(:)
   real(r8), pointer :: livecrootc_storage_to_xfer(:)
   real(r8), pointer :: deadcrootc_storage_to_xfer(:)
   real(r8), pointer :: gresp_storage_to_xfer(:)
   real(r8), pointer :: livestemc_to_deadstemc(:)
   real(r8), pointer :: livecrootc_to_deadcrootc(:)
   real(r8), pointer :: gpp(:)
   real(r8), pointer :: mr(:)
   real(r8), pointer :: current_gr(:)
   real(r8), pointer :: transfer_gr(:)
   real(r8), pointer :: storage_gr(:)
   real(r8), pointer :: gr(:)
   real(r8), pointer :: ar(:)
   real(r8), pointer :: rr(:)
   real(r8), pointer :: npp(:)
   real(r8), pointer :: agnpp(:)
   real(r8), pointer :: bgnpp(:)
   real(r8), pointer :: litfall(:)
   real(r8), pointer :: vegfire(:)
   real(r8), pointer :: wood_harvestc(:)
   real(r8), pointer :: pft_cinputs(:)
   real(r8), pointer :: pft_coutputs(:)
   real(r8), pointer :: pft_fire_closs(:)
end type pft_cflux_type
type, public :: pft_nflux_type
   real(r8), pointer :: m_leafn_to_litter(:)
   real(r8), pointer :: m_frootn_to_litter(:)
   real(r8), pointer :: m_leafn_storage_to_litter(:)
   real(r8), pointer :: m_frootn_storage_to_litter(:)
   real(r8), pointer :: m_livestemn_storage_to_litter(:)
   real(r8), pointer :: m_deadstemn_storage_to_litter(:)
   real(r8), pointer :: m_livecrootn_storage_to_litter(:)
   real(r8), pointer :: m_deadcrootn_storage_to_litter(:)
   real(r8), pointer :: m_leafn_xfer_to_litter(:)
   real(r8), pointer :: m_frootn_xfer_to_litter(:)
   real(r8), pointer :: m_livestemn_xfer_to_litter(:)
   real(r8), pointer :: m_deadstemn_xfer_to_litter(:)
   real(r8), pointer :: m_livecrootn_xfer_to_litter(:)
   real(r8), pointer :: m_deadcrootn_xfer_to_litter(:)
   real(r8), pointer :: m_livestemn_to_litter(:)
   real(r8), pointer :: m_deadstemn_to_litter(:)
   real(r8), pointer :: m_livecrootn_to_litter(:)
   real(r8), pointer :: m_deadcrootn_to_litter(:)
   real(r8), pointer :: m_retransn_to_litter(:)
   real(r8), pointer :: hrv_leafn_to_litter(:)
   real(r8), pointer :: hrv_frootn_to_litter(:)
   real(r8), pointer :: hrv_leafn_storage_to_litter(:)
   real(r8), pointer :: hrv_frootn_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_leafn_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemn_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_to_prod10n(:)
   real(r8), pointer :: hrv_deadstemn_to_prod100n(:)
   real(r8), pointer :: hrv_livecrootn_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_to_litter(:)
   real(r8), pointer :: hrv_retransn_to_litter(:)
   real(r8), pointer :: m_leafn_to_fire(:)
   real(r8), pointer :: m_leafn_storage_to_fire(:)
   real(r8), pointer :: m_leafn_xfer_to_fire(:)
   real(r8), pointer :: m_frootn_to_fire(:)
   real(r8), pointer :: m_frootn_storage_to_fire(:)
   real(r8), pointer :: m_frootn_xfer_to_fire(:)
   real(r8), pointer :: m_livestemn_to_fire(:)
   real(r8), pointer :: m_livestemn_storage_to_fire(:)
   real(r8), pointer :: m_livestemn_xfer_to_fire(:)
   real(r8), pointer :: m_deadstemn_to_fire(:)
   real(r8), pointer :: m_deadstemn_to_litter_fire(:)
   real(r8), pointer :: m_deadstemn_storage_to_fire(:)
   real(r8), pointer :: m_deadstemn_xfer_to_fire(:)
   real(r8), pointer :: m_livecrootn_to_fire(:)
   real(r8), pointer :: m_livecrootn_storage_to_fire(:)
   real(r8), pointer :: m_livecrootn_xfer_to_fire(:)
   real(r8), pointer :: m_deadcrootn_to_fire(:)
   real(r8), pointer :: m_deadcrootn_to_litter_fire(:)
   real(r8), pointer :: m_deadcrootn_storage_to_fire(:)
   real(r8), pointer :: m_deadcrootn_xfer_to_fire(:)
   real(r8), pointer :: m_retransn_to_fire(:)
   real(r8), pointer :: leafn_xfer_to_leafn(:)
   real(r8), pointer :: frootn_xfer_to_frootn(:)
   real(r8), pointer :: livestemn_xfer_to_livestemn(:)
   real(r8), pointer :: deadstemn_xfer_to_deadstemn(:)
   real(r8), pointer :: livecrootn_xfer_to_livecrootn(:)
   real(r8), pointer :: deadcrootn_xfer_to_deadcrootn(:)
   real(r8), pointer :: leafn_to_litter(:)
   real(r8), pointer :: leafn_to_retransn(:)
   real(r8), pointer :: frootn_to_litter(:)
   real(r8), pointer :: retransn_to_npool(:)
   real(r8), pointer :: sminn_to_npool(:)
   real(r8), pointer :: npool_to_leafn(:)
   real(r8), pointer :: npool_to_leafn_storage(:)
   real(r8), pointer :: npool_to_frootn(:)
   real(r8), pointer :: npool_to_frootn_storage(:)
   real(r8), pointer :: npool_to_livestemn(:)
   real(r8), pointer :: npool_to_livestemn_storage(:)
   real(r8), pointer :: npool_to_deadstemn(:)
   real(r8), pointer :: npool_to_deadstemn_storage(:)
   real(r8), pointer :: npool_to_livecrootn(:)
   real(r8), pointer :: npool_to_livecrootn_storage(:)
   real(r8), pointer :: npool_to_deadcrootn(:)
   real(r8), pointer :: npool_to_deadcrootn_storage(:)
   real(r8), pointer :: leafn_storage_to_xfer(:)
   real(r8), pointer :: frootn_storage_to_xfer(:)
   real(r8), pointer :: livestemn_storage_to_xfer(:)
   real(r8), pointer :: deadstemn_storage_to_xfer(:)
   real(r8), pointer :: livecrootn_storage_to_xfer(:)
   real(r8), pointer :: deadcrootn_storage_to_xfer(:)
   real(r8), pointer :: livestemn_to_deadstemn(:)
   real(r8), pointer :: livestemn_to_retransn(:)
   real(r8), pointer :: livecrootn_to_deadcrootn(:)
   real(r8), pointer :: livecrootn_to_retransn(:)
   real(r8), pointer :: ndeploy(:)
   real(r8), pointer :: pft_ninputs(:)
   real(r8), pointer :: pft_noutputs(:)
   real(r8), pointer :: wood_harvestn(:)
   real(r8), pointer :: pft_fire_nloss(:)
end type pft_nflux_type
type, public :: pft_vflux_type
   real(r8), pointer :: vocflx_tot(:)
   real(r8), pointer :: vocflx(:,:)
   real(r8), pointer :: vocflx_1(:)
   real(r8), pointer :: vocflx_2(:)
   real(r8), pointer :: vocflx_3(:)
   real(r8), pointer :: vocflx_4(:)
   real(r8), pointer :: vocflx_5(:)
   real(r8), pointer :: Eopt_out(:)
   real(r8), pointer :: topt_out(:)
   real(r8), pointer :: alpha_out(:)
   real(r8), pointer :: cp_out(:)
   real(r8), pointer :: paru_out(:)
   real(r8), pointer :: par24u_out(:)
   real(r8), pointer :: par240u_out(:)
   real(r8), pointer :: para_out(:)
   real(r8), pointer :: par24a_out(:)
   real(r8), pointer :: par240a_out(:)
   real(r8), pointer :: gamma_out(:)
   real(r8), pointer :: gammaL_out(:)
   real(r8), pointer :: gammaT_out(:)
   real(r8), pointer :: gammaP_out(:)
   real(r8), pointer :: gammaA_out(:)
   real(r8), pointer :: gammaS_out(:)
end type pft_vflux_type
type, public :: pft_depvd_type
   real(r8), pointer :: drydepvel(:,:)
end type pft_depvd_type
type, public :: pft_dflux_type
   real(r8), pointer :: flx_mss_vrt_dst(:,:)
   real(r8), pointer :: flx_mss_vrt_dst_tot(:)
   real(r8), pointer :: vlc_trb(:,:)
   real(r8), pointer :: vlc_trb_1(:)
   real(r8), pointer :: vlc_trb_2(:)
   real(r8), pointer :: vlc_trb_3(:)
   real(r8), pointer :: vlc_trb_4(:)
end type pft_dflux_type
type, public :: column_pstate_type
   type(pft_pstate_type) :: pps_a
   integer , pointer :: snl(:)
   integer , pointer :: isoicol(:)
   real(r8), pointer :: bsw(:,:)
   real(r8), pointer :: watsat(:,:)
   real(r8), pointer :: watdry(:,:)
   real(r8), pointer :: watopt(:,:)
   real(r8), pointer :: hksat(:,:)
   real(r8), pointer :: sucsat(:,:)
   real(r8), pointer :: hkdepth(:)
   real(r8), pointer :: wtfact(:)
   real(r8), pointer :: fracice(:,:)
   real(r8), pointer :: csol(:,:)
   real(r8), pointer :: tkmg(:,:)
   real(r8), pointer :: tkdry(:,:)
   real(r8), pointer :: tksatu(:,:)
   real(r8), pointer :: smpmin(:)
   real(r8), pointer :: gwc_thr(:)
   real(r8), pointer :: mss_frc_cly_vld(:)
   real(r8), pointer :: mbl_bsn_fct(:)
   logical , pointer :: do_capsnow(:)
   real(r8), pointer :: snowdp(:)
   real(r8), pointer :: frac_sno(:)
   real(r8), pointer :: zi(:,:)
   real(r8), pointer :: dz(:,:)
   real(r8), pointer :: z(:,:)
   real(r8), pointer :: frac_iceold(:,:)
   integer , pointer :: imelt(:,:)
   real(r8), pointer :: eff_porosity(:,:)
   real(r8), pointer :: emg(:)
   real(r8), pointer :: z0mg(:)
   real(r8), pointer :: z0hg(:)
   real(r8), pointer :: z0qg(:)
   real(r8), pointer :: htvp(:)
   real(r8), pointer :: beta(:)
   real(r8), pointer :: zii(:)
   real(r8), pointer :: albgrd(:,:)
   real(r8), pointer :: albgri(:,:)
   real(r8), pointer :: rootr_column(:,:)
   real(r8), pointer :: rootfr_road_perv(:,:)
   real(r8), pointer :: rootr_road_perv(:,:)
   real(r8), pointer :: wf(:)
   real(r8), pointer :: max_dayl(:)
   real(r8), pointer :: bsw2(:,:)
   real(r8), pointer :: psisat(:,:)
   real(r8), pointer :: vwcsat(:,:)
   real(r8), pointer :: decl(:)
   real(r8), pointer :: coszen(:)
   real(r8), pointer :: soilpsi(:,:)
   real(r8), pointer :: fpi(:)
   real(r8), pointer :: fpg(:)
   real(r8), pointer :: annsum_counter(:)
   real(r8), pointer :: cannsum_npp(:)
   real(r8), pointer :: cannavg_t2m(:)
   real(r8), pointer :: watfc(:,:)
   real(r8), pointer :: me(:)
   real(r8), pointer :: fire_prob(:)
   real(r8), pointer :: mean_fire_prob(:)
   real(r8), pointer :: fireseasonl(:)
   real(r8), pointer :: farea_burned(:)
   real(r8), pointer :: ann_farea_burned(:)
   real(r8), pointer :: albsnd_hst(:,:)
   real(r8), pointer :: albsni_hst(:,:)
   real(r8), pointer :: albsod(:,:)
   real(r8), pointer :: albsoi(:,:)
   real(r8), pointer :: flx_absdv(:,:)
   real(r8), pointer :: flx_absdn(:,:)
   real(r8), pointer :: flx_absiv(:,:)
   real(r8), pointer :: flx_absin(:,:)
   real(r8), pointer :: snw_rds(:,:)
   real(r8), pointer :: snw_rds_top(:)
   real(r8), pointer :: sno_liq_top(:)
   real(r8), pointer :: mss_bcpho(:,:)
   real(r8), pointer :: mss_bcphi(:,:)
   real(r8), pointer :: mss_bctot(:,:)
   real(r8), pointer :: mss_bc_col(:)
   real(r8), pointer :: mss_bc_top(:)
   real(r8), pointer :: mss_ocpho(:,:)
   real(r8), pointer :: mss_ocphi(:,:)
   real(r8), pointer :: mss_octot(:,:)
   real(r8), pointer :: mss_oc_col(:)
   real(r8), pointer :: mss_oc_top(:)
   real(r8), pointer :: mss_dst1(:,:)
   real(r8), pointer :: mss_dst2(:,:)
   real(r8), pointer :: mss_dst3(:,:)
   real(r8), pointer :: mss_dst4(:,:)
   real(r8), pointer :: mss_dsttot(:,:)
   real(r8), pointer :: mss_dst_col(:)
   real(r8), pointer :: mss_dst_top(:)
   real(r8), pointer :: h2osno_top(:)
   real(r8), pointer :: mss_cnc_bcphi(:,:)
   real(r8), pointer :: mss_cnc_bcpho(:,:)
   real(r8), pointer :: mss_cnc_ocphi(:,:)
   real(r8), pointer :: mss_cnc_ocpho(:,:)
   real(r8), pointer :: mss_cnc_dst1(:,:)
   real(r8), pointer :: mss_cnc_dst2(:,:)
   real(r8), pointer :: mss_cnc_dst3(:,:)
   real(r8), pointer :: mss_cnc_dst4(:,:)
   real(r8), pointer :: albgrd_pur(:,:)
   real(r8), pointer :: albgri_pur(:,:)
   real(r8), pointer :: albgrd_bc(:,:)
   real(r8), pointer :: albgri_bc(:,:)
   real(r8), pointer :: albgrd_oc(:,:)
   real(r8), pointer :: albgri_oc(:,:)
   real(r8), pointer :: albgrd_dst(:,:)
   real(r8), pointer :: albgri_dst(:,:)
   real(r8), pointer :: dTdz_top(:)
   real(r8), pointer :: snot_top(:)
end type column_pstate_type
type, public :: column_estate_type
   type(pft_estate_type):: pes_a
   real(r8), pointer :: t_grnd(:)
   real(r8), pointer :: t_grnd_u(:)
   real(r8), pointer :: t_grnd_r(:)
   real(r8), pointer :: dt_grnd(:)
   real(r8), pointer :: t_soisno(:,:)
   real(r8), pointer :: t_soi_10cm(:)
   real(r8), pointer :: t_lake(:,:)
   real(r8), pointer :: tssbef(:,:)
   real(r8), pointer :: thv(:)
   real(r8), pointer :: hc_soi(:)
   real(r8), pointer :: hc_soisno(:)
end type column_estate_type
type, public :: column_wstate_type
   type(pft_wstate_type):: pws_a
   real(r8), pointer :: h2osno(:)
   real(r8), pointer :: h2osoi_liq(:,:)
   real(r8), pointer :: h2osoi_ice(:,:)
   real(r8), pointer :: h2osoi_liqice_10cm(:)
   real(r8), pointer :: h2osoi_vol(:,:)
   real(r8), pointer :: h2osno_old(:)
   real(r8), pointer :: qg(:)
   real(r8), pointer :: dqgdT(:)
   real(r8), pointer :: snowice(:)
   real(r8), pointer :: snowliq(:)
   real(r8) ,pointer :: soilalpha(:)
   real(r8), pointer :: soilbeta(:)
   real(r8) ,pointer :: soilalpha_u(:)
   real(r8), pointer :: zwt(:)
   real(r8), pointer :: fcov(:)
   real(r8), pointer :: wa(:)
   real(r8), pointer :: wt(:)
   real(r8), pointer :: qcharge(:)
   real(r8), pointer :: smp_l(:,:)
   real(r8), pointer :: hk_l(:,:)
   real(r8), pointer :: fsat(:)
end type column_wstate_type
type, public :: column_cstate_type
   type(pft_cstate_type):: pcs_a
   real(r8), pointer :: soilc(:)
   real(r8), pointer :: cwdc(:)
   real(r8), pointer :: litr1c(:)
   real(r8), pointer :: litr2c(:)
   real(r8), pointer :: litr3c(:)
   real(r8), pointer :: soil1c(:)
   real(r8), pointer :: soil2c(:)
   real(r8), pointer :: soil3c(:)
   real(r8), pointer :: soil4c(:)
   real(r8), pointer :: col_ctrunc(:)
   real(r8), pointer :: seedc(:)
   real(r8), pointer :: prod10c(:)
   real(r8), pointer :: prod100c(:)
   real(r8), pointer :: totprodc(:)
   real(r8), pointer :: totlitc(:)
   real(r8), pointer :: totsomc(:)
   real(r8), pointer :: totecosysc(:)
   real(r8), pointer :: totcolc(:)
end type column_cstate_type
type, public :: column_nstate_type
   type(pft_nstate_type):: pns_a
   real(r8), pointer :: cwdn(:)
   real(r8), pointer :: litr1n(:)
   real(r8), pointer :: litr2n(:)
   real(r8), pointer :: litr3n(:)
   real(r8), pointer :: soil1n(:)
   real(r8), pointer :: soil2n(:)
   real(r8), pointer :: soil3n(:)
   real(r8), pointer :: soil4n(:)
   real(r8), pointer :: sminn(:)
   real(r8), pointer :: col_ntrunc(:)
   real(r8), pointer :: seedn(:)
   real(r8), pointer :: prod10n(:)
   real(r8), pointer :: prod100n(:)
   real(r8), pointer :: totprodn(:)
   real(r8), pointer :: totlitn(:)
   real(r8), pointer :: totsomn(:)
   real(r8), pointer :: totecosysn(:)
   real(r8), pointer :: totcoln(:)
end type column_nstate_type
type, public :: column_vstate_type
   type(pft_vstate_type):: pvs_a
end type column_vstate_type
type, public :: column_dstate_type
   real(r8), pointer :: dummy_entry(:)
end type column_dstate_type
type, public :: column_eflux_type
   type(pft_eflux_type):: pef_a
   real(r8), pointer :: eflx_snomelt(:)
   real(r8), pointer :: eflx_snomelt_u(:)
   real(r8), pointer :: eflx_snomelt_r(:)
   real(r8), pointer :: eflx_impsoil(:)
   real(r8), pointer :: eflx_fgr12(:)
   real(r8), pointer :: eflx_building_heat(:)
   real(r8), pointer :: eflx_urban_ac(:)
   real(r8), pointer :: eflx_urban_heat(:)
end type column_eflux_type
type, public :: column_mflux_type
   type(pft_mflux_type):: pmf_a
end type column_mflux_type
type, public :: column_wflux_type
   type(pft_wflux_type):: pwf_a
   real(r8), pointer :: qflx_infl(:)
   real(r8), pointer :: qflx_surf(:)
   real(r8), pointer :: qflx_drain(:)
   real(r8), pointer :: qflx_top_soil(:)
   real(r8), pointer :: qflx_snomelt(:)
   real(r8), pointer :: qflx_qrgwl(:)
   real(r8), pointer :: qflx_runoff(:)
   real(r8), pointer :: qflx_runoff_u(:)
   real(r8), pointer :: qflx_runoff_r(:)
   real(r8), pointer :: qmelt(:)
   real(r8), pointer :: h2ocan_loss(:)
   real(r8), pointer :: qflx_rsub_sat(:)
   real(r8), pointer :: flx_bc_dep_dry(:)
   real(r8), pointer :: flx_bc_dep_wet(:)
   real(r8), pointer :: flx_bc_dep_pho(:)
   real(r8), pointer :: flx_bc_dep_phi(:)
   real(r8), pointer :: flx_bc_dep(:)
   real(r8), pointer :: flx_oc_dep_dry(:)
   real(r8), pointer :: flx_oc_dep_wet(:)
   real(r8), pointer :: flx_oc_dep_pho(:)
   real(r8), pointer :: flx_oc_dep_phi(:)
   real(r8), pointer :: flx_oc_dep(:)
   real(r8), pointer :: flx_dst_dep_dry1(:)
   real(r8), pointer :: flx_dst_dep_wet1(:)
   real(r8), pointer :: flx_dst_dep_dry2(:)
   real(r8), pointer :: flx_dst_dep_wet2(:)
   real(r8), pointer :: flx_dst_dep_dry3(:)
   real(r8), pointer :: flx_dst_dep_wet3(:)
   real(r8), pointer :: flx_dst_dep_dry4(:)
   real(r8), pointer :: flx_dst_dep_wet4(:)
   real(r8), pointer :: flx_dst_dep(:)
   real(r8), pointer :: qflx_snofrz_lyr(:,:)
end type column_wflux_type
type, public :: column_cflux_type
   type(pft_cflux_type):: pcf_a
   real(r8), pointer :: m_leafc_to_litr1c(:)
   real(r8), pointer :: m_leafc_to_litr2c(:)
   real(r8), pointer :: m_leafc_to_litr3c(:)
   real(r8), pointer :: m_frootc_to_litr1c(:)
   real(r8), pointer :: m_frootc_to_litr2c(:)
   real(r8), pointer :: m_frootc_to_litr3c(:)
   real(r8), pointer :: m_livestemc_to_cwdc(:)
   real(r8), pointer :: m_deadstemc_to_cwdc(:)
   real(r8), pointer :: m_livecrootc_to_cwdc(:)
   real(r8), pointer :: m_deadcrootc_to_cwdc(:)
   real(r8), pointer :: m_leafc_storage_to_litr1c(:)
   real(r8), pointer :: m_frootc_storage_to_litr1c(:)
   real(r8), pointer :: m_livestemc_storage_to_litr1c(:)
   real(r8), pointer :: m_deadstemc_storage_to_litr1c(:)
   real(r8), pointer :: m_livecrootc_storage_to_litr1c(:)
   real(r8), pointer :: m_deadcrootc_storage_to_litr1c(:)
   real(r8), pointer :: m_gresp_storage_to_litr1c(:)
   real(r8), pointer :: m_leafc_xfer_to_litr1c(:)
   real(r8), pointer :: m_frootc_xfer_to_litr1c(:)
   real(r8), pointer :: m_livestemc_xfer_to_litr1c(:)
   real(r8), pointer :: m_deadstemc_xfer_to_litr1c(:)
   real(r8), pointer :: m_livecrootc_xfer_to_litr1c(:)
   real(r8), pointer :: m_deadcrootc_xfer_to_litr1c(:)
   real(r8), pointer :: m_gresp_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_leafc_to_litr1c(:)
   real(r8), pointer :: hrv_leafc_to_litr2c(:)
   real(r8), pointer :: hrv_leafc_to_litr3c(:)
   real(r8), pointer :: hrv_frootc_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_to_litr2c(:)
   real(r8), pointer :: hrv_frootc_to_litr3c(:)
   real(r8), pointer :: hrv_livestemc_to_cwdc(:)
   real(r8), pointer :: hrv_deadstemc_to_prod10c(:)
   real(r8), pointer :: hrv_deadstemc_to_prod100c(:)
   real(r8), pointer :: hrv_livecrootc_to_cwdc(:)
   real(r8), pointer :: hrv_deadcrootc_to_cwdc(:)
   real(r8), pointer :: hrv_leafc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_livestemc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_deadstemc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_livecrootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_deadcrootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_gresp_storage_to_litr1c(:)
   real(r8), pointer :: hrv_leafc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_livestemc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_deadstemc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_livecrootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_deadcrootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_gresp_xfer_to_litr1c(:)
   real(r8), pointer :: m_deadstemc_to_cwdc_fire(:)
   real(r8), pointer :: m_deadcrootc_to_cwdc_fire(:)
   real(r8), pointer :: m_litr1c_to_fire(:)
   real(r8), pointer :: m_litr2c_to_fire(:)
   real(r8), pointer :: m_litr3c_to_fire(:)
   real(r8), pointer :: m_cwdc_to_fire(:)
   real(r8), pointer :: leafc_to_litr1c(:)
   real(r8), pointer :: leafc_to_litr2c(:)
   real(r8), pointer :: leafc_to_litr3c(:)
   real(r8), pointer :: frootc_to_litr1c(:)
   real(r8), pointer :: frootc_to_litr2c(:)
   real(r8), pointer :: frootc_to_litr3c(:)
   real(r8), pointer :: cwdc_to_litr2c(:)
   real(r8), pointer :: cwdc_to_litr3c(:)
   real(r8), pointer :: litr1_hr(:)
   real(r8), pointer :: litr1c_to_soil1c(:)
   real(r8), pointer :: litr2_hr(:)
   real(r8), pointer :: litr2c_to_soil2c(:)
   real(r8), pointer :: litr3_hr(:)
   real(r8), pointer :: litr3c_to_soil3c(:)
   real(r8), pointer :: soil1_hr(:)
   real(r8), pointer :: soil1c_to_soil2c(:)
   real(r8), pointer :: soil2_hr(:)
   real(r8), pointer :: soil2c_to_soil3c(:)
   real(r8), pointer :: soil3_hr(:)
   real(r8), pointer :: soil3c_to_soil4c(:)
   real(r8), pointer :: soil4_hr(:)
   real(r8), pointer :: lithr(:)
   real(r8), pointer :: somhr(:)
   real(r8), pointer :: hr(:)
   real(r8), pointer :: sr(:)
   real(r8), pointer :: er(:)
   real(r8), pointer :: litfire(:)
   real(r8), pointer :: somfire(:)
   real(r8), pointer :: totfire(:)
   real(r8), pointer :: nep(:)
   real(r8), pointer :: nbp(:)
   real(r8), pointer :: nee(:)
   real(r8), pointer :: col_cinputs(:)
   real(r8), pointer :: col_coutputs(:)
   real(r8), pointer :: col_fire_closs(:)
end type column_cflux_type
type, public :: column_nflux_type
   type(pft_nflux_type):: pnf_a
   real(r8), pointer :: ndep_to_sminn(:)
   real(r8), pointer :: nfix_to_sminn(:)
   real(r8), pointer :: m_leafn_to_litr1n(:)
   real(r8), pointer :: m_leafn_to_litr2n(:)
   real(r8), pointer :: m_leafn_to_litr3n(:)
   real(r8), pointer :: m_frootn_to_litr1n(:)
   real(r8), pointer :: m_frootn_to_litr2n(:)
   real(r8), pointer :: m_frootn_to_litr3n(:)
   real(r8), pointer :: m_livestemn_to_cwdn(:)
   real(r8), pointer :: m_deadstemn_to_cwdn(:)
   real(r8), pointer :: m_livecrootn_to_cwdn(:)
   real(r8), pointer :: m_deadcrootn_to_cwdn(:)
   real(r8), pointer :: m_retransn_to_litr1n(:)
   real(r8), pointer :: m_leafn_storage_to_litr1n(:)
   real(r8), pointer :: m_frootn_storage_to_litr1n(:)
   real(r8), pointer :: m_livestemn_storage_to_litr1n(:)
   real(r8), pointer :: m_deadstemn_storage_to_litr1n(:)
   real(r8), pointer :: m_livecrootn_storage_to_litr1n(:)
   real(r8), pointer :: m_deadcrootn_storage_to_litr1n(:)
   real(r8), pointer :: m_leafn_xfer_to_litr1n(:)
   real(r8), pointer :: m_frootn_xfer_to_litr1n(:)
   real(r8), pointer :: m_livestemn_xfer_to_litr1n(:)
   real(r8), pointer :: m_deadstemn_xfer_to_litr1n(:)
   real(r8), pointer :: m_livecrootn_xfer_to_litr1n(:)
   real(r8), pointer :: m_deadcrootn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_to_litr2n(:)
   real(r8), pointer :: hrv_leafn_to_litr3n(:)
   real(r8), pointer :: hrv_frootn_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_to_litr2n(:)
   real(r8), pointer :: hrv_frootn_to_litr3n(:)
   real(r8), pointer :: hrv_livestemn_to_cwdn(:)
   real(r8), pointer :: hrv_deadstemn_to_prod10n(:)
   real(r8), pointer :: hrv_deadstemn_to_prod100n(:)
   real(r8), pointer :: hrv_livecrootn_to_cwdn(:)
   real(r8), pointer :: hrv_deadcrootn_to_cwdn(:)
   real(r8), pointer :: hrv_retransn_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_livestemn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_deadstemn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_livecrootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_deadcrootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_livestemn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_deadstemn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_livecrootn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_deadcrootn_xfer_to_litr1n(:)
   real(r8), pointer :: m_deadstemn_to_cwdn_fire(:)
   real(r8), pointer :: m_deadcrootn_to_cwdn_fire(:)
   real(r8), pointer :: m_litr1n_to_fire(:)
   real(r8), pointer :: m_litr2n_to_fire(:)
   real(r8), pointer :: m_litr3n_to_fire(:)
   real(r8), pointer :: m_cwdn_to_fire(:)
   real(r8), pointer :: leafn_to_litr1n(:)
   real(r8), pointer :: leafn_to_litr2n(:)
   real(r8), pointer :: leafn_to_litr3n(:)
   real(r8), pointer :: frootn_to_litr1n(:)
   real(r8), pointer :: frootn_to_litr2n(:)
   real(r8), pointer :: frootn_to_litr3n(:)
   real(r8), pointer :: cwdn_to_litr2n(:)
   real(r8), pointer :: cwdn_to_litr3n(:)
   real(r8), pointer :: litr1n_to_soil1n(:)
   real(r8), pointer :: sminn_to_soil1n_l1(:)
   real(r8), pointer :: litr2n_to_soil2n(:)
   real(r8), pointer :: sminn_to_soil2n_l2(:)
   real(r8), pointer :: litr3n_to_soil3n(:)
   real(r8), pointer :: sminn_to_soil3n_l3(:)
   real(r8), pointer :: soil1n_to_soil2n(:)
   real(r8), pointer :: sminn_to_soil2n_s1(:)
   real(r8), pointer :: soil2n_to_soil3n(:)
   real(r8), pointer :: sminn_to_soil3n_s2(:)
   real(r8), pointer :: soil3n_to_soil4n(:)
   real(r8), pointer :: sminn_to_soil4n_s3(:)
   real(r8), pointer :: soil4n_to_sminn(:)
   real(r8), pointer :: sminn_to_denit_l1s1(:)
   real(r8), pointer :: sminn_to_denit_l2s2(:)
   real(r8), pointer :: sminn_to_denit_l3s3(:)
   real(r8), pointer :: sminn_to_denit_s1s2(:)
   real(r8), pointer :: sminn_to_denit_s2s3(:)
   real(r8), pointer :: sminn_to_denit_s3s4(:)
   real(r8), pointer :: sminn_to_denit_s4(:)
   real(r8), pointer :: sminn_to_denit_excess(:)
   real(r8), pointer :: sminn_leached(:)
   real(r8), pointer :: dwt_seedn_to_leaf(:)
   real(r8), pointer :: dwt_seedn_to_deadstem(:)
   real(r8), pointer :: dwt_conv_nflux(:)
   real(r8), pointer :: dwt_prod10n_gain(:)
   real(r8), pointer :: dwt_prod100n_gain(:)
   real(r8), pointer :: dwt_frootn_to_litr1n(:)
   real(r8), pointer :: dwt_frootn_to_litr2n(:)
   real(r8), pointer :: dwt_frootn_to_litr3n(:)
   real(r8), pointer :: dwt_livecrootn_to_cwdn(:)
   real(r8), pointer :: dwt_deadcrootn_to_cwdn(:)
   real(r8), pointer :: dwt_nloss(:)
   real(r8), pointer :: prod10n_loss(:)
   real(r8), pointer :: prod100n_loss(:)
   real(r8), pointer :: product_nloss(:)
   real(r8), pointer :: potential_immob(:)
   real(r8), pointer :: actual_immob(:)
   real(r8), pointer :: sminn_to_plant(:)
   real(r8), pointer :: supplement_to_sminn(:)
   real(r8), pointer :: gross_nmin(:)
   real(r8), pointer :: net_nmin(:)
   real(r8), pointer :: denit(:)
   real(r8), pointer :: col_ninputs(:)
   real(r8), pointer :: col_noutputs(:)
   real(r8), pointer :: col_fire_nloss(:)
end type column_nflux_type
type, public :: column_vflux_type
   type(pft_vflux_type):: pvf_a
end type column_vflux_type
type, public :: column_dflux_type
   type(pft_dflux_type):: pdf_a
end type column_dflux_type
type, public :: landunit_pstate_type
   type(column_pstate_type):: cps_a
   real(r8), pointer :: t_building(:)
   real(r8), pointer :: t_building_max(:)
   real(r8), pointer :: t_building_min(:)
   real(r8), pointer :: tk_wall(:,:)
   real(r8), pointer :: tk_roof(:,:)
   real(r8), pointer :: tk_improad(:,:)
   real(r8), pointer :: cv_wall(:,:)
   real(r8), pointer :: cv_roof(:,:)
   real(r8), pointer :: cv_improad(:,:)
   real(r8), pointer :: thick_wall(:)
   real(r8), pointer :: thick_roof(:)
   integer, pointer :: nlev_improad(:)
   real(r8), pointer :: vf_sr(:)
   real(r8), pointer :: vf_wr(:)
   real(r8), pointer :: vf_sw(:)
   real(r8), pointer :: vf_rw(:)
   real(r8), pointer :: vf_ww(:)
   real(r8), pointer :: taf(:)
   real(r8), pointer :: qaf(:)
   real(r8), pointer :: sabs_roof_dir(:,:)
   real(r8), pointer :: sabs_roof_dif(:,:)
   real(r8), pointer :: sabs_sunwall_dir(:,:)
   real(r8), pointer :: sabs_sunwall_dif(:,:)
   real(r8), pointer :: sabs_shadewall_dir(:,:)
   real(r8), pointer :: sabs_shadewall_dif(:,:)
   real(r8), pointer :: sabs_improad_dir(:,:)
   real(r8), pointer :: sabs_improad_dif(:,:)
   real(r8), pointer :: sabs_perroad_dir(:,:)
   real(r8), pointer :: sabs_perroad_dif(:,:)
end type landunit_pstate_type
type, public :: landunit_estate_type
   type(column_estate_type):: ces_a
end type landunit_estate_type
type, public :: landunit_wstate_type
   type(column_wstate_type):: cws_a
end type landunit_wstate_type
type, public :: landunit_cstate_type
   type(column_cstate_type):: ccs_a
end type landunit_cstate_type
type, public :: landunit_nstate_type
   type(column_nstate_type):: cns_a
end type landunit_nstate_type
type, public :: landunit_vstate_type
   real(r8):: dummy_entry
end type landunit_vstate_type
type, public :: landunit_dgvstate_type
   real(r8):: dummy_entry
end type landunit_dgvstate_type
type, public :: landunit_dstate_type
   type(column_dstate_type):: cds_a
end type landunit_dstate_type
type, public :: landunit_eflux_type
   type(column_eflux_type):: cef_a
   real(r8), pointer :: eflx_traffic_factor(:)
   real(r8), pointer :: eflx_traffic(:)
   real(r8), pointer :: eflx_wasteheat(:)
   real(r8), pointer :: eflx_heat_from_ac(:)
end type landunit_eflux_type
type, public :: landunit_mflux_type
   type(pft_mflux_type):: pmf_a
end type landunit_mflux_type
type, public :: landunit_wflux_type
   type(column_wflux_type):: cwf_a
end type landunit_wflux_type
type, public :: landunit_cflux_type
   type(column_cflux_type):: ccf_a
end type landunit_cflux_type
type, public :: landunit_nflux_type
   type(column_nflux_type):: cnf_a
end type landunit_nflux_type
type, public :: landunit_vflux_type
   type(pft_vflux_type):: pvf_a
end type landunit_vflux_type
type, public :: landunit_dflux_type
   type(pft_dflux_type):: pdf_a
end type landunit_dflux_type
type, public :: gridcell_pstate_type
   type(column_pstate_type):: cps_a
end type gridcell_pstate_type
type, public :: gridcell_estate_type
   type(column_estate_type):: ces_a
   real(r8), pointer :: gc_heat1(:)
   real(r8), pointer :: gc_heat2(:)
end type gridcell_estate_type
type, public :: gridcell_wstate_type
   type(column_wstate_type):: cws_a
   real(r8), pointer :: gc_liq1(:)
   real(r8), pointer :: gc_liq2(:)
   real(r8), pointer :: gc_ice1(:)
   real(r8), pointer :: gc_ice2(:)
end type gridcell_wstate_type
type, public :: gridcell_cstate_type
   type(column_cstate_type):: ccs_a
end type gridcell_cstate_type
type, public :: gridcell_nstate_type
   type(column_nstate_type):: cns_a
end type gridcell_nstate_type
type, public :: gridcell_vstate_type
   type(column_vstate_type):: cvs_a
end type gridcell_vstate_type
type, public :: gridcell_efstate_type
   real(r8), pointer :: efisop(:,:)
end type gridcell_efstate_type
type, public :: gridcell_dstate_type
   type(column_dstate_type):: cds_a
end type gridcell_dstate_type
type, public :: gridcell_eflux_type
   type(column_eflux_type):: cef_a
   real(r8), pointer :: eflx_sh_totg(:)
   real(r8), pointer :: eflx_dynbal(:)
end type gridcell_eflux_type
type, public :: gridcell_mflux_type
   type(pft_mflux_type):: pmf_a
end type gridcell_mflux_type
type, public :: gridcell_wflux_type
   type(column_wflux_type):: cwf_a
   real(r8), pointer :: qflx_runoffg(:)
   real(r8), pointer :: qflx_snwcp_iceg(:)
   real(r8), pointer :: qflx_liq_dynbal(:)
   real(r8), pointer :: qflx_ice_dynbal(:)
end type gridcell_wflux_type
type, public :: gridcell_cflux_type
   type(column_cflux_type):: ccf_a
end type gridcell_cflux_type
type, public :: gridcell_nflux_type
   type(column_nflux_type):: cnf_a
end type gridcell_nflux_type
type, public :: gridcell_vflux_type
   type(pft_vflux_type):: pvf_a
end type gridcell_vflux_type
type, public :: gridcell_dflux_type
   type(pft_dflux_type):: pdf_a
end type gridcell_dflux_type
type, public :: model_pstate_type
   type(column_pstate_type) :: cps_a
end type model_pstate_type
type, public :: model_estate_type
   type(column_estate_type):: ces_a
end type model_estate_type
type, public :: model_wstate_type
   type(column_wstate_type):: cws_a
end type model_wstate_type
type, public :: model_cstate_type
   type(column_cstate_type):: ccs_a
end type model_cstate_type
type, public :: model_nstate_type
   type(column_nstate_type):: cns_a
end type model_nstate_type
type, public :: model_vstate_type
   type(column_vstate_type):: cvs_a
end type model_vstate_type
type, public :: model_dstate_type
   type(column_dstate_type):: cds_a
end type model_dstate_type
type, public :: model_eflux_type
   type(column_eflux_type):: cef_a
end type model_eflux_type
type, public :: model_mflux_type
   type(pft_mflux_type):: pmf_a
end type model_mflux_type
type, public :: model_wflux_type
   type(column_wflux_type):: cwf_a
end type model_wflux_type
type, public :: model_cflux_type
   type(column_cflux_type):: ccf_a
end type model_cflux_type
type, public :: model_nflux_type
   type(column_nflux_type):: cnf_a
end type model_nflux_type
type, public :: model_vflux_type
   type(pft_vflux_type):: pvf_a
end type model_vflux_type
type, public :: model_dflux_type
   type(pft_dflux_type):: pdf_a
end type model_dflux_type
type, public :: pft_type
   integer, pointer :: column(:)
   real(r8), pointer :: wtcol(:)
   integer, pointer :: landunit(:)
   real(r8), pointer :: wtlunit(:)
   integer, pointer :: gridcell(:)
   real(r8), pointer :: wtgcell(:)
   integer , pointer :: itype(:)
   integer , pointer :: mxy(:)
   real(r8), pointer :: area(:)
   type(energy_balance_type) :: pebal
   type(water_balance_type) :: pwbal
   type(carbon_balance_type) :: pcbal
   type(nitrogen_balance_type) :: pnbal
   type(pft_epv_type) :: pepv
   type(pft_pstate_type) :: pps
   type(pft_estate_type) :: pes
   type(pft_wstate_type) :: pws
   type(pft_cstate_type) :: pcs
   type(pft_nstate_type) :: pns
   type(pft_vstate_type) :: pvs
   type(pft_eflux_type) :: pef
   type(pft_mflux_type) :: pmf
   type(pft_wflux_type) :: pwf
   type(pft_cflux_type) :: pcf
   type(pft_nflux_type) :: pnf
   type(pft_vflux_type) :: pvf
   type(pft_dflux_type) :: pdf
   type(pft_depvd_type) :: pdd
end type pft_type
type, public :: column_type
   type(pft_type) :: p
   integer , pointer :: landunit(:)
   real(r8), pointer :: wtlunit(:)
   integer , pointer :: gridcell(:)
   real(r8), pointer :: wtgcell(:)
   integer , pointer :: pfti(:)
   integer , pointer :: pftf(:)
   integer , pointer :: npfts(:)
   integer , pointer :: itype(:)
   real(r8), pointer :: area(:)
   type(energy_balance_type) :: cebal
   type(water_balance_type) :: cwbal
   type(carbon_balance_type) :: ccbal
   type(nitrogen_balance_type) :: cnbal
   type(column_pstate_type) :: cps
   type(column_estate_type) :: ces
   type(column_wstate_type) :: cws
   type(column_cstate_type) :: ccs
   type(column_nstate_type) :: cns
   type(column_dstate_type) :: cds
   type(column_eflux_type) :: cef
   type(column_mflux_type) :: cmf
   type(column_wflux_type) :: cwf
   type(column_cflux_type) :: ccf
   type(column_nflux_type) :: cnf
   type(column_vflux_type) :: cvf
   type(column_dflux_type) :: cdf
end type column_type
type, public :: landunit_type
   type(column_type) :: c
   integer , pointer :: gridcell(:)
   real(r8), pointer :: wtgcell(:)
   integer , pointer :: coli(:)
   integer , pointer :: colf(:)
   integer , pointer :: ncolumns(:)
   integer , pointer :: pfti(:)
   integer , pointer :: pftf(:)
   integer , pointer :: npfts(:)
   real(r8), pointer :: area(:)
   real(r8), pointer :: canyon_hwr(:)
   real(r8), pointer :: wtroad_perv(:)
   real(r8), pointer :: wtlunit_roof(:)
   real(r8), pointer :: ht_roof(:)
   real(r8), pointer :: wind_hgt_canyon(:)
   real(r8), pointer :: z_0_town(:)
   real(r8), pointer :: z_d_town(:)
   integer , pointer :: itype(:)
   logical , pointer :: ifspecial(:)
   logical , pointer :: lakpoi(:)
   logical , pointer :: urbpoi(:)
   type(energy_balance_type) :: lebal
   type(water_balance_type) :: lwbal
   type(carbon_balance_type) :: lcbal
   type(nitrogen_balance_type) :: lnbal
   type(landunit_pstate_type) :: lps
   type(landunit_estate_type) :: les
   type(landunit_wstate_type) :: lws
   type(landunit_cstate_type) :: lcs
   type(landunit_nstate_type) :: lns
   type(landunit_vstate_type) :: lvs
   type(landunit_dstate_type) :: lds
   type(landunit_eflux_type) :: lef
   type(landunit_mflux_type) :: lmf
   type(landunit_wflux_type) :: lwf
   type(landunit_cflux_type) :: lcf
   type(landunit_nflux_type) :: lnf
   type(landunit_vflux_type) :: lvf
   type(landunit_dflux_type) :: ldf
end type landunit_type
type, public :: gridcell_type
   type(landunit_type) :: l
   integer, pointer :: luni(:)
   integer, pointer :: lunf(:)
   integer, pointer :: nlandunits(:)
   integer, pointer :: coli(:)
   integer, pointer :: colf(:)
   integer, pointer :: ncolumns(:)
   integer, pointer :: pfti(:)
   integer, pointer :: pftf(:)
   integer, pointer :: npfts(:)
   integer , pointer :: gindex(:)
   real(r8), pointer :: area(:)
   real(r8), pointer :: lat(:)
   real(r8), pointer :: lon(:)
   real(r8), pointer :: latdeg(:)
   real(r8), pointer :: londeg(:)
   integer , pointer :: gindex_a(:)
   real(r8), pointer :: lat_a(:)
   real(r8), pointer :: lon_a(:)
   real(r8), pointer :: latdeg_a(:)
   real(r8), pointer :: londeg_a(:)
   type(energy_balance_type) :: gebal
   type(water_balance_type) :: gwbal
   type(carbon_balance_type) :: gcbal
   type(nitrogen_balance_type) :: gnbal
   type(gridcell_pstate_type) :: gps
   type(gridcell_estate_type) :: ges
   type(gridcell_wstate_type) :: gws
   type(gridcell_cstate_type) :: gcs
   type(gridcell_nstate_type) :: gns
   type(gridcell_vstate_type) :: gvs
   type(gridcell_efstate_type):: gve
   type(gridcell_dstate_type) :: gds
   type(gridcell_eflux_type) :: gef
   type(gridcell_wflux_type) :: gwf
   type(gridcell_cflux_type) :: gcf
   type(gridcell_nflux_type) :: gnf
   type(gridcell_vflux_type) :: gvf
   type(gridcell_dflux_type) :: gdf
end type gridcell_type
type, public :: model_type
   type(gridcell_type) :: g
   integer :: ngridcells
   real(r8) :: area
   type(energy_balance_type) :: mebal
   type(water_balance_type) :: mwbal
   type(carbon_balance_type) :: mcbal
   type(nitrogen_balance_type) :: mnbal
   type(model_pstate_type) :: mps
   type(model_estate_type) :: mes
   type(model_wstate_type) :: mws
   type(model_cstate_type) :: mcs
   type(model_nstate_type) :: mns
   type(model_vstate_type) :: mvs
   type(model_dstate_type) :: mds
   type(model_eflux_type) :: mef
   type(model_wflux_type) :: mwf
   type(model_cflux_type) :: mcf
   type(model_nflux_type) :: mnf
   type(model_vflux_type) :: mvf
   type(model_dflux_type) :: mdf
end type model_type
type atm2lnd_type
  real(r8), pointer :: forc_t(:)
  real(r8), pointer :: forc_u(:)
  real(r8), pointer :: forc_v(:)
  real(r8), pointer :: forc_wind(:)
  real(r8), pointer :: forc_q(:)
  real(r8), pointer :: forc_hgt(:)
  real(r8), pointer :: forc_hgt_u(:)
  real(r8), pointer :: forc_hgt_t(:)
  real(r8), pointer :: forc_hgt_q(:)
  real(r8), pointer :: forc_pbot(:)
  real(r8), pointer :: forc_th(:)
  real(r8), pointer :: forc_vp(:)
  real(r8), pointer :: forc_rho(:)
  real(r8), pointer :: forc_rh(:)
  real(r8), pointer :: forc_psrf(:)
  real(r8), pointer :: forc_pco2(:)
  real(r8), pointer :: forc_lwrad(:)
  real(r8), pointer :: forc_solad(:,:)
  real(r8), pointer :: forc_solai(:,:)
  real(r8), pointer :: forc_solar(:)
  real(r8), pointer :: forc_rain(:)
  real(r8), pointer :: forc_snow(:)
  real(r8), pointer :: forc_ndep(:)
  real(r8), pointer :: rainf(:)
  real(r8), pointer :: forc_po2(:)
  real(r8), pointer :: forc_aer(:,:)
end type atm2lnd_type
type(atm2lnd_type), public, target, save :: clm_a2l
type(model_type) , public, target , save :: clm3
type(pft_epc_type), public, target, save :: pftcon
character(len=8), parameter, public :: gratm = 'atmgrid'
character(len=8), parameter, public :: grlnd = 'lndgrid'
character(len=8), parameter, public :: nameg = 'gridcell'
character(len=8), parameter, public :: namel = 'landunit'
character(len=8), parameter, public :: namec = 'column'
character(len=8), parameter, public :: namep = 'pft'
character(len=8), parameter, public :: allrof = 'allrof'
 contains
   subroutine clmtype_mod
   end subroutine clmtype_mod
end module clmtype
MODULE module_sf_clm
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar, only: numpft, clm_varpar_mod,nlevgrnd, nlevsoi,nlevlak,nlevsno,maxpatch_pft
  use clm_varcon, only: hvap, hsub,tfrz, vkc, sb ,&
                        snowage_drdt0,ndep,organic,fmax,efisop
  use module_cam_support, only: endrun
CONTAINS
    subroutine clmdrv(zgcmxy ,forc_qxy ,ps ,forc_txy ,tsxy &
                   ,shxy ,qfx ,lhxy ,soiflx ,qgh &
                   ,gsw, swdown,ra_sw_physics &
                   ,history_interval ,flwdsxy ,smstav ,smstot ,qsfxy &
                   ,qdnxy ,ivgtyp ,isltyp ,vegfra ,albxy &
                   ,znt ,z0 ,tmn ,xland ,xice &
                   ,emiss ,snowc ,qsfc ,prec ,maxpatch &
                   ,num_soil_layers ,dt ,xtime ,dtwrf ,dzs &
                   ,smois ,tslb ,snow ,canwat ,chs &
                   ,chs2 &
                   ,sh2o ,snowh ,forc_uxy ,forc_vxy ,shdmin &
                   ,shdmax ,acsnom ,acsnow ,dx ,xlat &
                   ,xlong,ht &
                   ,ids,ide, jds,jde, kds,kde &
                   ,ims,ime, jms,jme, kms,kme &
                   ,its,ite, jts,jte, kts,kte &
                   ,inest, sf_urban_physics,nlcat, &
                CMR_SFCDIF,CHR_SFCDIF,CMC_SFCDIF,CHC_SFCDIF, &
                CMGR_SFCDIF,CHGR_SFCDIF, &
                tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d, &
                uc_urb2d, &
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d, &
                trl_urb3d,tbl_urb3d,tgl_urb3d, &
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d, &
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d, &
                GZ1OZ0_urb2d, AKMS_URB2D, &
                th2_urb2d,q2_urb2d,ust_urb2d, &
               declin_urb,cosz_urb2d,omg_urb2d, &
                xlat_urb2d, &
                num_roof_layers, num_wall_layers, &
                num_road_layers, DZR, DZB, DZG, &
                FRC_URB2D, UTYPE_URB2D, &
                cmcr_urb2d,tgr_urb2d,tgrl_urb3d,smr_urb3d, &
                drelr_urb2d,drelb_urb2d,drelg_urb2d, &
                flxhumr_urb2d,flxhumb_urb2d,flxhumg_urb2d, &
               numc,nump,sabv,sabg,lwup,snl, &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg, &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t2clm , &
                t_ref2m,h2osoi_liq_s1, &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4, &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2, &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6, &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10, &
                h2osoi_ice_s1,h2osoi_ice_s2, &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5, &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4, &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7, &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10, &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4, &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3, &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7, &
                t_soisno8,t_soisno9,t_soisno10, &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5, &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5, &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5, &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10, &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3, &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6, &
                h2osoi_vol7,h2osoi_vol8, &
                h2osoi_vol9,h2osoi_vol10, &
                q_ref2m, &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid, &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,SWUPsubgrid,&
                LHsoi,LHveg,LHtran,&
                alswvisdir, alswvisdif, alswnirdir, alswnirdif, &
                swvisdir, swvisdif, swnirdir, swnirdif &
                 )
  USE module_date_time
  USE module_sf_urban, only: urban
  USE module_sf_noahlsm, only: low_density_residential, high_density_residential, high_intensity_industrial
  USE module_ra_gfdleta, only: cal_mon_day
  USE module_configure
  implicit none
  integer, intent(in) :: ids,ide, jds,jde, kds,kde, &
                                    ims,ime, jms,jme, kms,kme, &
                                    its,ite, jts,jte, kts,kte
  integer,intent(in) :: num_soil_layers,maxpatch,sf_urban_physics,&
                         ra_sw_physics,history_interval
  real,dimension(ims:ime,1:num_soil_layers,jms:jme ),intent(inout) ::&
                                                         smois, &
                                                         sh2o, &
                                                         tslb
   integer,intent(in) :: nlcat
  real,intent(in) :: dt,dx
  real,intent(in) :: xtime, dtwrf
  real(r8) :: dtt
  real, dimension(1:num_soil_layers), intent(in)::dzs
  real,dimension(ims:ime,jms:jme ),intent(inout) ::&
                    smstav ,smstot &
                   ,znt ,snowc ,qsfc ,snow ,snowh &
                   ,canwat ,acsnom ,acsnow, emiss, z0
  real,dimension(ims:ime,jms:jme ),intent(in) ::&
                 vegfra, tmn,shdmin,shdmax
  real,dimension(ims:ime,jms:jme ),intent(in) ::&
                   qgh,chs,chs2
  real(r8) :: efisop_buf(6)
  logical :: found = .false.
  integer, dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  real, dimension(ims:ime,jms:jme ),intent(inout) :: soiflx,sabv,sabg,lwup,t2m_max,t2m_min
  integer, dimension(ims:ime,1:maxpatch,jms:jme ) :: snl,snl1
  real, dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg, &
                h2ocan,h2ocan_col, &
                t_ref2m,h2osoi_liq_s1, &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4, &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2, &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6, &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10, &
                h2osoi_ice_s1,h2osoi_ice_s2, &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5, &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4, &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7, &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10, &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4, &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3, &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7, &
                t_soisno8,t_soisno9,t_soisno10, &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5, &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5, &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5, &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10, &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3, &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6, &
                h2osoi_vol7,h2osoi_vol8, &
                h2osoi_vol9,h2osoi_vol10, &
                q_ref2m, &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid, &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid, &
                SWUPsubgrid,LHsoi,LHveg,LHtran
 real(r8) :: gti_buf
  integer :: nstep
  integer :: i,j,m,inest,k
  real, dimension(ims:ime, kms:kme,jms:jme),intent(in) ::&
            forc_txy,forc_uxy,forc_vxy,forc_qxy,zgcmxy,ps
  real :: flwdsxy(ims:ime,jms:jme)
  real :: gsw(ims:ime,jms:jme)
  real :: swdown(ims:ime,jms:jme)
  real, dimension(ims:ime,jms:jme),intent(in) :: swvisdir, swvisdif, swnirdir,swnirdif
  real, dimension(ims:ime,jms:jme),intent(out):: alswvisdir,alswvisdif,alswnirdir,alswnirdif
  real :: xlat (ims:ime,jms:jme)
  real :: xlong(ims:ime,jms:jme)
  real :: ht(ims:ime,jms:jme)
  real :: xland (ims:ime,jms:jme)
  real :: xice (ims:ime,jms:jme)
  real :: prec (ims:ime,jms:jme)
  integer :: ivgtyp(ims:ime,jms:jme)
  integer :: isltyp (ims:ime,jms:jme)
  real :: albxy(ims:ime,jms:jme)
  real :: tsxy(ims:ime,jms:jme)
  real :: t2clm(ims:ime,jms:jme)
  real :: shxy(ims:ime,jms:jme)
  real :: lhxy(ims:ime,jms:jme)
  real :: qfx(ims:ime,jms:jme)
  real :: qsfxy(ims:ime,jms:jme)
  real :: qdnxy(ims:ime,jms:jme)
  real(r8) :: alswvisdir_buf,alswvisdif_buf,alswnirdir_buf,alswnirdif_buf
  real(r8) :: swvisdir_buf,swvisdif_buf,swnirdir_buf,swnirdif_buf
  real(r8) :: albxy_buf
  real(r8) :: tsxy_buf,trefxy_buf
  real(r8) :: shxy_buf
  real(r8) :: lhxy_buf
  real(r8) :: qsfxy_buf
  real(r8) :: qdnxy_buf
  real(r8) :: soiflx_buf
  real(r8) :: sabv_buf
  real(r8) :: sabg_buf
  real(r8) :: lwup_buf
  real(r8) :: znt_buf
  real(r8) :: rhoxy_buf
  real(r8) :: swd_buf
  real(r8) :: forc_sols_buf
  real(r8) :: forc_soll_buf
  real(r8) :: forc_solsd_buf
  real(r8) :: forc_solld_buf
  real(r8) :: area_buf
  real(r8) :: forc_pbot_buf
  real(r8) :: forc_txy_buf
  real(r8) :: forc_uxy_buf
  real(r8) :: forc_vxy_buf
  real(r8) :: forc_qxy_buf
  real(r8) :: zgcmxy_buf
  real(r8) :: prec_buf
  real(r8) :: flwdsxy_buf
  real(r8) :: forc_psrfxy_buf
  real(r8) :: forc_ndepxy_buf
  real(r8) :: xlat_buf
  real(r8) :: xlon_buf
  real(r8),dimension(maxpatch,-nlevsno+1:nlevgrnd) :: dzclm
  real(r8),dimension(maxpatch,-nlevsno+1:nlevgrnd) :: zclm
  real(r8),dimension(maxpatch,-nlevsno:nlevgrnd) :: ziclm
  real(r8),dimension(maxpatch,-nlevsno+1:nlevgrnd) :: &
                          h2osoi_liq_buf, &
                          h2osoi_ice_buf, &
                          t_soisno_buf
  real(r8),dimension(maxpatch,-nlevsno+1:0) ::snw_rds_buf
  real(r8),dimension(maxpatch,1:num_soil_layers) :: &
                         t_lake_buf, h2osoi_vol_buf
  integer :: lndmsk
 real(r8),dimension(maxpatch) :: organic_buf
  real(r8), dimension(maxpatch) :: &
                snowdp_buf,wtc_buf,wtp_buf,h2osno_buf,t_grnd_buf,t_veg_buf, &
                h2ocan_buf,h2ocan_col_buf, &
                t_ref2m_buf, q_ref2m_buf, &
                albedosubgrid_buf, lhsubgrid_buf, hfxsubgrid_buf, lwupsubgrid_buf, &
                q2subgrid_buf,sabgsubgrid_buf,sabvsubgrid_buf,nrasubgrid_buf,swupsubgrid_buf,&
                lhsoi_buf,lhveg_buf,lhtran_buf,tlai_buf,tsai_buf,htop_buf,hbot_buf
     INTEGER, INTENT(IN) :: num_roof_layers
     INTEGER, INTENT(IN) :: num_wall_layers
     INTEGER, INTENT(IN) :: num_road_layers
     REAL, OPTIONAL, DIMENSION(1:num_roof_layers), INTENT(IN) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers), INTENT(IN) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers), INTENT(IN) :: DZG
     REAL, OPTIONAL, INTENT(IN) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D
     INTEGER :: UTYPE_URB
     REAL :: TA_URB
     REAL :: QA_URB
     REAL :: UA_URB
     REAL :: U1_URB
     REAL :: V1_URB
     REAL :: SSG_URB
     REAL :: LLG_URB
     REAL :: RAIN_URB
     REAL :: RHOO_URB
     REAL :: ZA_URB
     REAL :: DELT_URB
     REAL :: SSGD_URB
     REAL :: SSGQ_URB
     REAL :: XLAT_URB
     REAL :: COSZ_URB
     REAL :: OMG_URB
     REAL :: ZNT_URB
     REAL :: TR_URB
     REAL :: TB_URB
     REAL :: TG_URB
     REAL :: TC_URB
     REAL :: QC_URB
     REAL :: UC_URB
     REAL :: XXXR_URB
     REAL :: XXXB_URB
     REAL :: XXXG_URB
     REAL :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB
     LOGICAL :: LSOLAR_URB
     INTEGER :: jmonth, jday
     REAL :: DRELR_URB
     REAL :: DRELB_URB
     REAL :: DRELG_URB
     REAL :: FLXHUMR_URB
     REAL :: FLXHUMB_URB
     REAL :: FLXHUMG_URB
     REAL :: CMCR_URB
     REAL :: TGR_URB
     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D
   REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: FRC_URB2D
     INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: UTYPE_URB2D
     REAL :: TS_URB
     REAL :: QS_URB
     REAL :: SH_URB
     REAL :: LH_URB
     REAL :: LH_KINEMATIC_URB
     REAL :: SW_URB
     REAL :: ALB_URB
     REAL :: LW_URB
     REAL :: G_URB
     REAL :: RN_URB
     REAL :: PSIM_URB
     REAL :: PSIH_URB
     REAL :: GZ1OZ0_URB
     REAL :: U10_URB
     REAL :: V10_URB
     REAL :: TH2_URB
     REAL :: Q2_URB
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB
 CHARACTER(len=24) :: nextstep_date, cdate,simulation_start_date
 INTEGER simulation_start_year , &
         simulation_start_month , &
         simulation_start_day , &
         simulation_start_hour , &
         simulation_start_minute , &
         simulation_start_second
 integer :: myr,mon,mday,mhr,mint,msc,mtsec,myr1,mon1,mday1,mhr1,mint1,msc1,mtsec1
 integer :: myrs,mons,mdays,mhrs,mints,mscs,mtsecs
 integer :: julyr,julday, julyr1,julday1
 integer :: mbdate
 integer :: msec,msec1
 integer :: ns
 real(r8) :: calday,calday1
 real :: gmt,gmt1
 integer(selected_int_kind(12)) :: idts
 integer :: idt
 real(r8) :: dsqmin, dsq
 character*256 :: msg
 real :: mh_urb,stdh_urb,lp_urb,hgt_urb,frc_urb,lb_urb,check
 real, dimension(4) :: lf_urb
   call clm_varpar_mod(nlcat)
   call CLMDebug('Now in clmdrv')
  nstep = nint( (xtime*60. + dtwrf) / dt)
  if( nstep .le. 1 ) nstep = 1
  dtt = dt
   write(msg,*) 'dt=',dt,'jts=',jts,'jte=',jte,'its=',its,'ite=',ite
   call CLMDebug(msg)
  CALL nl_get_simulation_start_year ( 1, simulation_start_year )
  CALL nl_get_simulation_start_month ( 1, simulation_start_month )
  CALL nl_get_simulation_start_day ( 1, simulation_start_day )
  CALL nl_get_simulation_start_hour ( 1, simulation_start_hour )
  CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
  CALL nl_get_simulation_start_second ( 1, simulation_start_second )
  WRITE ( simulation_start_date(1:19) , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           simulation_start_year,simulation_start_month,simulation_start_day,&
           simulation_start_hour,simulation_start_minute,simulation_start_second
  simulation_start_date(1:24) = simulation_start_date(1:19)//'.0000'
  CALL split_date_char (simulation_start_date, myrs, mons, mdays, mhrs, mints, mscs, mtsecs)
  idt = nint(dtt)*nstep
  idts = nint(dtt)*nstep
  if(idt/=idts) then
     print*,'The integer idt and idts is too large; Stop in module_sf_clm.F', idt,idts
     call endrun()
  end if
  CALL geth_newdate (cdate(1:19), simulation_start_date(1:19), idt)
  cdate(1:24) = cdate(1:19)//'.0000'
  CALL split_date_char (cdate, myr, mon, mday, mhr, mint, msc, mtsec )
  CALL geth_newdate (nextstep_date(1:19), cdate(1:19), nint(dtt))
  nextstep_date(1:24) = nextstep_date(1:19)//'.0000'
  CALL split_date_char (nextstep_date, myr1, mon1, mday1, mhr1, mint1, msc1, mtsec1)
  CALL get_julgmt(cdate,julyr,julday, gmt)
  CALL get_julgmt(nextstep_date,julyr1,julday1, gmt1)
  msec = mhr*3600 + mint*60
  msec1 = mhr1*3600 + mint1*60
  calday = julday + gmt/24.0
  calday1= julday1 + gmt1/24.0
  mbdate = myrs*10000 + mons*100 + mdays
  do j=jts,jte
   do i=its,ite
      if(xland(i,j) == 1.0) then
        lndmsk = 1
      else
        lndmsk = 0
      end if
    if(lndmsk == 1) then
      qsfxy_buf = qsfxy(i,j)
      qdnxy_buf = qdnxy(i,j)
      xlon_buf = xlong(i,j) + 360.0
      xlat_buf = xlat(i,j)
      albxy_buf = albxy(i,j)
      if(gsw(i,j)>0.0.and.albxy_buf<0.99.and.albxy_buf>0.0)then
         swd_buf = gsw(i,j)/(1.-albxy_buf)
         swdown(i,j) = gsw(i,j)/(1.-albxy_buf)
      else
         swd_buf = 0.0
         swdown(i,j) = 0.0
      end if
    if(ra_sw_physics .ne. 3) then
      forc_sols_buf = swd_buf*0.35
      forc_soll_buf = swd_buf*0.35
      forc_solsd_buf = swd_buf*0.15
      forc_solld_buf = swd_buf*0.15
    else
      forc_sols_buf = swvisdir(i,j)
      forc_soll_buf = swnirdir(i,j)
      forc_solsd_buf = swvisdif(i,j)
      forc_solld_buf = swnirdif(i,j)
    end if
      area_buf = dx*dx/1.e6
      forc_pbot_buf = ps(i,1,j)
      forc_txy_buf = forc_txy(i,1,j)
      forc_uxy_buf = forc_uxy(i,1,j)
      forc_vxy_buf = forc_vxy(i,1,j)
      forc_qxy_buf = forc_qxy(i,1,j)/(1.0+forc_qxy(i,1,j))
      zgcmxy_buf = zgcmxy(i,1,j)
      prec_buf = prec(i,j)/dtt
      flwdsxy_buf = flwdsxy(i,j)
      forc_psrfxy_buf= ps(i,1,j)
      forc_ndepxy_buf=ndep/(86400._r8 * 365._r8)
      efisop_buf(1:6) = efisop(1:6)
      gti_buf = fmax
      soiflx(i,j) = 0.0
      sabv(i,j) = 0.0
      sabg(i,j) = 0.0
      lwup(i,j) = 0.0
      soiflx_buf = 0.0
      sabv_buf = 0.0
      sabg_buf = 0.0
      lwup_buf = 0.0
     swvisdir_buf = swvisdir(i,j)
     swvisdif_buf = swvisdif(i,j)
     swnirdir_buf = swnirdir(i,j)
     swnirdif_buf = swnirdif(i,j)
     do m=1,maxpatch
      do k =1,nlevgrnd
        zclm(m,k) = 0.025*(exp(0.5*(k-0.5))-1.)
      end do
       dzclm(m,1) = 0.5*(zclm(m,1)+zclm(m,2))
       do k = 2,nlevgrnd-1
         dzclm(m,k)= 0.5*(zclm(m,k+1)-zclm(m,k-1))
       enddo
       dzclm(m,nlevgrnd) = zclm(m,nlevgrnd)-zclm(m,nlevgrnd-1)
      ziclm(m,0) = 0.0
      do k =1,nlevgrnd-1
        ziclm(m,k) = 0.5*(zclm(m,k) + zclm(m,k+1))
      end do
        ziclm(m,nlevgrnd) = zclm(m,nlevgrnd) + 0.5*dzclm(m,nlevgrnd)
         dzclm(m,-4) = dzsnow5(i,m,j)
         dzclm(m,-3) = dzsnow4(i,m,j)
         dzclm(m,-2) = dzsnow3(i,m,j)
         dzclm(m,-1) = dzsnow2(i,m,j)
         dzclm(m,0) = dzsnow1(i,m,j)
      do k=0,-nlevsno+1, -1
        zclm(m,k) = ziclm(m,k) - 0.5*dzclm(m,k)
        ziclm(m,k-1) = ziclm(m,k) - dzclm(m,k)
      end do
      snl1(i,m,j) = snl(i,m,j)
      snowdp_buf(m) = snowdp(i,m,j)
      snw_rds_buf(m,-4) = snowrds5(i,m,j)
      snw_rds_buf(m,-3) = snowrds4(i,m,j)
      snw_rds_buf(m,-2) = snowrds3(i,m,j)
      snw_rds_buf(m,-1) = snowrds2(i,m,j)
      snw_rds_buf(m,0) = snowrds1(i,m,j)
      h2osoi_liq_buf(m,-4) = h2osoi_liq_s5(i,m,j)
      h2osoi_liq_buf(m,-3) = h2osoi_liq_s4(i,m,j)
      h2osoi_liq_buf(m,-2) = h2osoi_liq_s3(i,m,j)
      h2osoi_liq_buf(m,-1) = h2osoi_liq_s2(i,m,j)
      h2osoi_liq_buf(m,0) = h2osoi_liq_s1(i,m,j)
      h2osoi_liq_buf(m,1) = h2osoi_liq1(i,m,j)
      h2osoi_liq_buf(m,2) = h2osoi_liq2(i,m,j)
      h2osoi_liq_buf(m,3) = h2osoi_liq3(i,m,j)
      h2osoi_liq_buf(m,4) = h2osoi_liq4(i,m,j)
      h2osoi_liq_buf(m,5) = h2osoi_liq5(i,m,j)
      h2osoi_liq_buf(m,6) = h2osoi_liq6(i,m,j)
      h2osoi_liq_buf(m,7) = h2osoi_liq7(i,m,j)
      h2osoi_liq_buf(m,8) = h2osoi_liq8(i,m,j)
      h2osoi_liq_buf(m,9) = h2osoi_liq9(i,m,j)
      h2osoi_liq_buf(m,10) = h2osoi_liq10(i,m,j)
      h2osoi_ice_buf(m,-4) = h2osoi_ice_s5(i,m,j)
      h2osoi_ice_buf(m,-3) = h2osoi_ice_s4(i,m,j)
      h2osoi_ice_buf(m,-2) = h2osoi_ice_s3(i,m,j)
      h2osoi_ice_buf(m,-1) = h2osoi_ice_s2(i,m,j)
      h2osoi_ice_buf(m,0) = h2osoi_ice_s1(i,m,j)
      h2osoi_ice_buf(m,1) = h2osoi_ice1(i,m,j)
      h2osoi_ice_buf(m,2) = h2osoi_ice2(i,m,j)
      h2osoi_ice_buf(m,3) = h2osoi_ice3(i,m,j)
      h2osoi_ice_buf(m,4) = h2osoi_ice4(i,m,j)
      h2osoi_ice_buf(m,5) = h2osoi_ice5(i,m,j)
      h2osoi_ice_buf(m,6) = h2osoi_ice6(i,m,j)
      h2osoi_ice_buf(m,7) = h2osoi_ice7(i,m,j)
      h2osoi_ice_buf(m,8) = h2osoi_ice8(i,m,j)
      h2osoi_ice_buf(m,9) = h2osoi_ice9(i,m,j)
      h2osoi_ice_buf(m,10) = h2osoi_ice10(i,m,j)
      t_soisno_buf(m,-4) = t_soisno_s5(i,m,j)
      t_soisno_buf(m,-3) = t_soisno_s4(i,m,j)
      t_soisno_buf(m,-2) = t_soisno_s3(i,m,j)
      t_soisno_buf(m,-1) = t_soisno_s2(i,m,j)
      t_soisno_buf(m,0) = t_soisno_s1(i,m,j)
      t_soisno_buf(m,1) = t_soisno1(i,m,j)
      t_soisno_buf(m,2) = t_soisno2(i,m,j)
      t_soisno_buf(m,3) = t_soisno3(i,m,j)
      t_soisno_buf(m,4) = t_soisno4(i,m,j)
      t_soisno_buf(m,5) = t_soisno5(i,m,j)
      t_soisno_buf(m,6) = t_soisno6(i,m,j)
      t_soisno_buf(m,7) = t_soisno7(i,m,j)
      t_soisno_buf(m,8) = t_soisno8(i,m,j)
      t_soisno_buf(m,9) = t_soisno9(i,m,j)
      t_soisno_buf(m,10) = t_soisno10(i,m,j)
      t_lake_buf(m,1) = t_lake1(i,m,j)
      t_lake_buf(m,2) = t_lake2(i,m,j)
      t_lake_buf(m,3) = t_lake3(i,m,j)
      t_lake_buf(m,4) = t_lake4(i,m,j)
      t_lake_buf(m,5) = t_lake5(i,m,j)
      t_lake_buf(m,6) = t_lake6(i,m,j)
      t_lake_buf(m,7) = t_lake7(i,m,j)
      t_lake_buf(m,8) = t_lake8(i,m,j)
      t_lake_buf(m,9) = t_lake9(i,m,j)
      t_lake_buf(m,10) = t_lake10(i,m,j)
      h2osoi_vol_buf(m,1) = h2osoi_vol1(i,m,j)
      h2osoi_vol_buf(m,2) = h2osoi_vol2(i,m,j)
      h2osoi_vol_buf(m,3) = h2osoi_vol3(i,m,j)
      h2osoi_vol_buf(m,4) = h2osoi_vol4(i,m,j)
      h2osoi_vol_buf(m,5) = h2osoi_vol5(i,m,j)
      h2osoi_vol_buf(m,6) = h2osoi_vol6(i,m,j)
      h2osoi_vol_buf(m,7) = h2osoi_vol7(i,m,j)
      h2osoi_vol_buf(m,8) = h2osoi_vol8(i,m,j)
      h2osoi_vol_buf(m,9) = h2osoi_vol9(i,m,j)
      h2osoi_vol_buf(m,10) = h2osoi_vol10(i,m,j)
      t_grnd_buf(m) = t_grnd(i,m,j)
      t_veg_buf(m) = t_veg(i,m,j)
      h2ocan_buf(m) = h2ocan(i,m,j)
      h2ocan_col_buf(m) = h2ocan_col(i,m,j)
      h2osno_buf(m) = h2osno(i,m,j)
      albedosubgrid_buf(m) = albedosubgrid(i,m,j)
      lhsubgrid_buf(m) = lhsubgrid(i,m,j)
      hfxsubgrid_buf(m) = hfxsubgrid(i,m,j)
      lwupsubgrid_buf(m)= lwupsubgrid(i,m,j)
      q2subgrid_buf(m) = q2subgrid(i,m,j)
      sabvsubgrid_buf(m) = sabvsubgrid(i,m,j)
      sabgsubgrid_buf(m) = sabgsubgrid(i,m,j)
      nrasubgrid_buf(m) = nrasubgrid(i,m,j)
      swupsubgrid_buf(m) = swupsubgrid(i,m,j)
     lhsoi_buf(m) = lhsoi(i,m,j)
     lhveg_buf(m) = lhveg(i,m,j)
     lhtran_buf(m) = lhtran(i,m,j)
     organic_buf(m) = organic(m)
      t_ref2m_buf(m) = t_ref2m(i,m,j)
      q_ref2m_buf(m) = q_ref2m(i,m,j)
    end do
          call clm(forc_txy_buf ,forc_uxy_buf ,forc_vxy_buf &
                  ,forc_qxy_buf ,zgcmxy_buf ,prec_buf &
                  ,flwdsxy_buf ,forc_sols_buf ,forc_soll_buf &
                  ,forc_solsd_buf ,forc_solld_buf ,forc_pbot_buf &
                  ,forc_psrfxy_buf ,ivgtyp(i,j) ,isltyp(i,j) &
                  ,lndmsk ,xlat_buf ,xlon_buf &
                  ,area_buf ,dtt ,myr &
                  ,mon ,mday ,msec &
                  ,calday ,myr1 ,mon1 &
                  ,mday1 ,msec1 ,calday1 &
                  ,mbdate ,qsfxy_buf ,qdnxy_buf &
                  ,snl1(i,:,j) ,snowdp_buf ,snw_rds_buf &
                  ,dzclm ,zclm ,ziclm &
                  ,h2osno_buf ,h2osoi_liq_buf ,h2osoi_ice_buf &
                  ,t_grnd_buf ,t_soisno_buf ,t_lake_buf &
                  ,t_veg_buf ,h2ocan_buf ,h2ocan_col_buf &
                  ,h2osoi_vol_buf ,wtc_buf ,wtp_buf &
                  ,numc(i,j) ,nump(i,j) &
                  ,t_ref2m_buf ,albxy_buf ,tsxy_buf, trefxy_buf &
                  ,shxy_buf ,lhxy_buf ,nstep &
                  ,inest ,i ,j &
                  ,soiflx_buf ,sabv_buf ,sabg_buf &
                  ,lwup_buf ,znt_buf ,q_ref2m_buf &
                  ,rhoxy_buf &
                  ,albedosubgrid_buf ,lhsubgrid_buf ,hfxsubgrid_buf &
                  ,lwupsubgrid_buf ,q2subgrid_buf ,sabvsubgrid_buf &
                  ,sabgsubgrid_buf ,nrasubgrid_buf ,swupsubgrid_buf &
                  ,lhsoi_buf ,lhveg_buf ,lhtran_buf &
                  ,organic_buf ,efisop_buf ,gti_buf &
                  ,alswnirdir_buf ,alswnirdif_buf,alswvisdir_buf,alswvisdif_buf&
                  )
                 if(albxy_buf == 1) albxy_buf = 0.991
                  albxy(i,j) = albxy_buf
                  call CLMDebug('get albxy')
                  snowh(i,j) = sum(snowdp_buf(1:numc(i,j))*wtc_buf(1:numc(i,j)))
                  call CLMDebug('get snowh')
                  snow(i,j) = sum(h2osno_buf(1:numc(i,j))*wtc_buf(1:numc(i,j)))
                  call CLMDebug('get snow')
                  canwat(i,j) = sum(h2ocan_buf(1:nump(i,j))*wtp_buf(1:nump(i,j)))
                 call CLMDebug('get canwat')
              if (ivgtyp(i,j) /= 16 .and. ivgtyp(i,j) /= 24) then
                  do k=1,nlevgrnd
                     smois(i,k,j) = sum(h2osoi_vol_buf(1:numc(i,j),k)*wtc_buf(1:numc(i,j)))
                     tslb (i,k,j) = sum(t_soisno_buf(1:numc(i,j),k)*wtc_buf(1:numc(i,j)))
                  end do
              end if
                  call CLMDebug('get tslb')
                  tsxy(i,j) = tsxy_buf
                  qsfxy(i,j) = qsfxy_buf
                  qdnxy(i,j) = qdnxy_buf
                  soiflx(i,j) = soiflx_buf
                  sabv(i,j) = sabv_buf
                  sabg(i,j) = sabg_buf
                  lwup(i,j) = lwup_buf
                  znt(i,j) = znt_buf
                  z0(i,j) = znt(i,j)
                  alswvisdir(i,j) = alswvisdir_buf
                  alswvisdif(i,j) = alswvisdif_buf
                  alswnirdir(i,j) = alswnirdir_buf
                  alswnirdif(i,j) = alswnirdif_buf
                  t2clm(i,j) = trefxy_buf
                  if(mod(dt*(nstep-1),60.*history_interval)==0) then
                    t2m_max(i,j) = 0.0
                    t2m_min(i,j) = 999.0
                  else
                    t2m_max(i,j) = max(t2m_max(i,j),t2clm(i,j))
                    t2m_min(i,j) = min(t2m_min(i,j),t2clm(i,j))
                  end if
             call CLMDebug('module clm mark1')
                  emiss(i,j) = lwup(i,j)/(sb * tsxy(i,j)**4)
                  shxy(i,j) = shxy_buf
                  lhxy(i,j) = lhxy_buf
                  if(tsxy(i,j)>=tfrz) then
                    qfx(i,j) = lhxy_buf/hvap
                  else
                    qfx(i,j) = lhxy_buf/hsub
                  end if
                  qsfc(i,j) = forc_qxy(i,1,j) +qfx(i,j)/(rhoxy_buf*chs(i,j))
              do m=1,maxpatch
                  snowdp(i,m,j) = snowdp_buf(m)
                  snl(i,m,j) = snl1(i,m,j)
                  dzsnow5(i,m,j) = dzclm(m,-4)
                  dzsnow4(i,m,j) = dzclm(m,-3)
                  dzsnow3(i,m,j) = dzclm(m,-2)
                  dzsnow2(i,m,j) = dzclm(m,-1)
                  dzsnow1(i,m,j) = dzclm(m,0)
                  snowrds5(i,m,j) = snw_rds_buf(m,-4)
                  snowrds4(i,m,j) = snw_rds_buf(m,-3)
                  snowrds3(i,m,j) = snw_rds_buf(m,-2)
                  snowrds2(i,m,j) = snw_rds_buf(m,-1)
                  snowrds1(i,m,j) = snw_rds_buf(m,0)
                  h2osno(i,m,j) = h2osno_buf(m)
                  t_grnd(i,m,j) = t_grnd_buf(m)
                  t_veg(i,m,j) = t_veg_buf(m)
                  h2ocan(i,m,j) = h2ocan_buf(m)
                  h2ocan_col(i,m,j) = h2ocan_col_buf(m)
                  wtc(i,m,j) = wtc_buf(m)
                  wtp(i,m,j) = wtp_buf(m)
            call CLMDebug('module clm mark2')
                  h2osoi_liq_s5(i,m,j) = h2osoi_liq_buf(m,-4)
                  h2osoi_liq_s4(i,m,j) = h2osoi_liq_buf(m,-3)
                  h2osoi_liq_s3(i,m,j) = h2osoi_liq_buf(m,-2)
                  h2osoi_liq_s2(i,m,j) = h2osoi_liq_buf(m,-1)
                  h2osoi_liq_s1(i,m,j) = h2osoi_liq_buf(m,0)
                  h2osoi_liq1(i,m,j) = h2osoi_liq_buf(m,1)
                  h2osoi_liq2(i,m,j) = h2osoi_liq_buf(m,2)
                  h2osoi_liq3(i,m,j) = h2osoi_liq_buf(m,3)
                  h2osoi_liq4(i,m,j) = h2osoi_liq_buf(m,4)
                  h2osoi_liq5(i,m,j) = h2osoi_liq_buf(m,5)
                  h2osoi_liq6(i,m,j) = h2osoi_liq_buf(m,6)
                  h2osoi_liq7(i,m,j) = h2osoi_liq_buf(m,7)
                  h2osoi_liq8(i,m,j) = h2osoi_liq_buf(m,8)
                  h2osoi_liq9(i,m,j) = h2osoi_liq_buf(m,9)
                  h2osoi_liq10(i,m,j) = h2osoi_liq_buf(m,10)
                  h2osoi_ice_s5(i,m,j) = h2osoi_ice_buf(m,-4)
                  h2osoi_ice_s4(i,m,j) = h2osoi_ice_buf(m,-3)
                  h2osoi_ice_s3(i,m,j) = h2osoi_ice_buf(m,-2)
                  h2osoi_ice_s2(i,m,j) = h2osoi_ice_buf(m,-1)
                  h2osoi_ice_s1(i,m,j) = h2osoi_ice_buf(m,0)
                  h2osoi_ice1(i,m,j) = h2osoi_ice_buf(m,1)
                  h2osoi_ice2(i,m,j) = h2osoi_ice_buf(m,2)
                  h2osoi_ice3(i,m,j) = h2osoi_ice_buf(m,3)
                  h2osoi_ice4(i,m,j) = h2osoi_ice_buf(m,4)
                  h2osoi_ice5(i,m,j) = h2osoi_ice_buf(m,5)
                  h2osoi_ice6(i,m,j) = h2osoi_ice_buf(m,6)
                  h2osoi_ice7(i,m,j) = h2osoi_ice_buf(m,7)
                  h2osoi_ice8(i,m,j) = h2osoi_ice_buf(m,8)
                  h2osoi_ice9(i,m,j) = h2osoi_ice_buf(m,9)
                  h2osoi_ice10(i,m,j) = h2osoi_ice_buf(m,10)
            call CLMDebug('module clm mark3')
                  t_soisno_s5(i,m,j) = t_soisno_buf(m,-4)
                  t_soisno_s4(i,m,j) = t_soisno_buf(m,-3)
                  t_soisno_s3(i,m,j) = t_soisno_buf(m,-2)
                  t_soisno_s2(i,m,j) = t_soisno_buf(m,-1)
                  t_soisno_s1(i,m,j) = t_soisno_buf(m,0)
                  t_soisno1(i,m,j) = t_soisno_buf(m,1)
                  t_soisno2(i,m,j) = t_soisno_buf(m,2)
                  t_soisno3(i,m,j) = t_soisno_buf(m,3)
                  t_soisno4(i,m,j) = t_soisno_buf(m,4)
                  t_soisno5(i,m,j) = t_soisno_buf(m,5)
                  t_soisno6(i,m,j) = t_soisno_buf(m,6)
                  t_soisno7(i,m,j) = t_soisno_buf(m,7)
                  t_soisno8(i,m,j) = t_soisno_buf(m,8)
                  t_soisno9(i,m,j) = t_soisno_buf(m,9)
                  t_soisno10(i,m,j) = t_soisno_buf(m,10)
                  t_lake1(i,m,j) = t_lake_buf(m,1)
                  t_lake2(i,m,j) = t_lake_buf(m,2)
                  t_lake3(i,m,j) = t_lake_buf(m,3)
                  t_lake4(i,m,j) = t_lake_buf(m,4)
                  t_lake5(i,m,j) = t_lake_buf(m,5)
                  t_lake6(i,m,j) = t_lake_buf(m,6)
                  t_lake7(i,m,j) = t_lake_buf(m,7)
                  t_lake8(i,m,j) = t_lake_buf(m,8)
                  t_lake9(i,m,j) = t_lake_buf(m,9)
                  t_lake10(i,m,j) = t_lake_buf(m,10)
                  h2osoi_vol1(i,m,j) = h2osoi_vol_buf(m,1)
                  h2osoi_vol2(i,m,j) = h2osoi_vol_buf(m,2)
                  h2osoi_vol3(i,m,j) = h2osoi_vol_buf(m,3)
                  h2osoi_vol4(i,m,j) = h2osoi_vol_buf(m,4)
                  h2osoi_vol5(i,m,j) = h2osoi_vol_buf(m,5)
                  h2osoi_vol6(i,m,j) = h2osoi_vol_buf(m,6)
                  h2osoi_vol7(i,m,j) = h2osoi_vol_buf(m,7)
                  h2osoi_vol8(i,m,j) = h2osoi_vol_buf(m,8)
                  h2osoi_vol9(i,m,j) = h2osoi_vol_buf(m,9)
                  h2osoi_vol10(i,m,j) = h2osoi_vol_buf(m,10)
            call CLMDebug('module clm mark4')
                  t_ref2m(i,m,j) = t_ref2m_buf(m)
                  q_ref2m(i,m,j) = q_ref2m_buf(m)
                  albedosubgrid(i,m,j)= albedosubgrid_buf(m)
                  lhsubgrid(i,m,j) = lhsubgrid_buf(m)
                  hfxsubgrid(i,m,j) = hfxsubgrid_buf(m)
                  lwupsubgrid(i,m,j) = lwupsubgrid_buf(m)
                  q2subgrid(i,m,j) = q2subgrid_buf(m)
                  sabvsubgrid(i,m,j) = sabvsubgrid_buf(m)
                  sabgsubgrid(i,m,j) = sabgsubgrid_buf(m)
                  nrasubgrid(i,m,j) = nrasubgrid_buf(m)
                  swupsubgrid(i,m,j) = swupsubgrid_buf(m)
                  lhsoi(i,m,j) = lhsoi_buf(m)
                  lhveg(i,m,j) = lhveg_buf(m)
                  lhtran(i,m,j) = lhtran_buf(m)
              end do
      end if
        call CLMDebug('good before call urban')
        IF (sf_urban_physics == 1 ) THEN
          IF( IVGTYP(I,J) == 1 .or. IVGTYP(I,J) == low_density_residential .or. &
              IVGTYP(I,J) == high_density_residential .or. IVGTYP(I,J) == high_intensity_industrial ) THEN
      forc_sols_buf = swd_buf*0.35
      forc_soll_buf = swd_buf*0.35
      forc_solsd_buf = swd_buf*0.15
      forc_solld_buf = swd_buf*0.15
      area_buf = dx*dx/1.e6
      forc_pbot_buf = ps(i,1,j)
      forc_txy_buf = forc_txy(i,1,j)
      forc_uxy_buf = forc_uxy(i,1,j)
      forc_vxy_buf = forc_vxy(i,1,j)
      forc_qxy_buf = forc_qxy(i,1,j)
      zgcmxy_buf = zgcmxy(i,1,j)
      prec_buf = prec(i,j)/dtt
      flwdsxy_buf = flwdsxy(i,j)
      forc_psrfxy_buf= ps(i,1,j)
            UTYPE_URB = UTYPE_URB2D(I,J)
            TA_URB = forc_txy(i,1,j)
            QA_URB = forc_qxy(i,1,j)
            UA_URB = SQRT(forc_uxy(i,1,j)**2.+forc_vxy(i,1,j)**2.)
            U1_URB = forc_uxy(i,1,j)
            V1_URB = forc_vxy(i,1,j)
            IF(UA_URB < 1.) UA_URB=1.
            SSG_URB = swd_buf
            SSGD_URB = 0.8*swd_buf
            SSGQ_URB = SSG_URB-SSGD_URB
            LLG_URB = flwdsxy(i,j)
            RAIN_URB = prec(i,j)
            RHOO_URB = ps(i,1,j)/(287.04 * forc_txy(i,1,j) * (1.0+ 0.61 * forc_qxy(i,1,j)))
            ZA_URB = zgcmxy_buf
            DELT_URB = DT
            XLAT_URB = XLAT_URB2D(I,J)
            COSZ_URB = COSZ_URB2D(I,J)
            OMG_URB = OMG_URB2D(I,J)
            ZNT_URB = ZNT(I,J)
            LSOLAR_URB = .FALSE.
            TR_URB = TR_URB2D(I,J)
            TB_URB = TB_URB2D(I,J)
            TG_URB = TG_URB2D(I,J)
            TC_URB = TC_URB2D(I,J)
            QC_URB = QC_URB2D(I,J)
            UC_URB = UC_URB2D(I,J)
            DO K = 1,num_roof_layers
              TRL_URB(K) = TRL_URB3D(I,K,J)
              SMR_URB(K) = SMR_URB3D(I,K,J)
              TGRL_URB(K)= TGRL_URB3D(I,K,J)
            END DO
            DO K = 1,num_wall_layers
              TBL_URB(K) = TBL_URB3D(I,K,J)
            END DO
            DO K = 1,num_road_layers
              TGL_URB(K) = TGL_URB3D(I,K,J)
            END DO
            TGR_URB = TGR_URB2D(I,J)
            CMCR_URB = CMCR_URB2D(I,J)
            FLXHUMR_URB = FLXHUMR_URB2D(I,J)
            FLXHUMB_URB = FLXHUMB_URB2D(I,J)
            FLXHUMG_URB = FLXHUMG_URB2D(I,J)
            DRELR_URB = DRELR_URB2D(I,J)
            DRELB_URB = DRELB_URB2D(I,J)
            DRELG_URB = DRELG_URB2D(I,J)
            XXXR_URB = XXXR_URB2D(I,J)
            XXXB_URB = XXXB_URB2D(I,J)
            XXXG_URB = XXXG_URB2D(I,J)
            XXXC_URB = XXXC_URB2D(I,J)
            CHS_URB = CHS(I,J)
            CHS2_URB = CHS2(I,J)
            IF (PRESENT(CMR_SFCDIF)) THEN
               CMR_URB = CMR_SFCDIF(I,J)
               CHR_URB = CHR_SFCDIF(I,J)
               CMGR_URB = CMGR_SFCDIF(I,J)
               CHGR_URB = CHGR_SFCDIF(I,J)
               CMC_URB = CMC_SFCDIF(I,J)
               CHC_URB = CHC_SFCDIF(I,J)
            ENDIF
            lp_urb = 0.
            lb_urb = 0.
            hgt_urb = 0.
            mh_urb = 0.
            stdh_urb = 0.
            do k = 1,4
              lf_urb(k) = 0.
            enddo
            frc_urb = FRC_URB2D(I,J)
            check = 0.
            CALL cal_mon_day(julday,julyr,jmonth,jday)
            CALL urban(LSOLAR_URB, &
                       num_roof_layers,num_wall_layers,num_road_layers, &
                       DZR,DZB,DZG, &
                       UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, &
                       SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB, &
                       ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB, &
                       XLAT_URB,DELT_URB,ZNT_URB, &
                       CHS_URB, CHS2_URB, &
                       TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB, &
                       TRL_URB,TBL_URB,TGL_URB, &
                       XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB, &
                       TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB, &
                       SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, &
                       GZ1OZ0_URB, &
                       CMR_URB, CHR_URB, CMC_URB, CHC_URB, &
                       U10_URB, V10_URB, TH2_URB, Q2_URB, &
                       UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb, &
                       hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB, &
                       TGRL_URB,SMR_URB,CMGR_URB, CHGR_URB, jmonth, &
                       DRELR_URB,DRELB_URB, &
                       DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)
            TS_URB2D(I,J) = TS_URB
            albxy(i,j) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*albxy_buf
            shxy(i,j) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*shxy_buf
            qfx(i,j) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                     + (1-FRC_URB2D(I,J))*qfx(i,j)
            lhxy(i,j) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*lhxy_buf
            soiflx(i,j) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*soiflx_buf
            tsxy(i,j) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*tsxy_buf
            qsfc(i,j) = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*qsfc(i,j)
            TR_URB2D(I,J) = TR_URB
            TB_URB2D(I,J) = TB_URB
            TG_URB2D(I,J) = TG_URB
            TC_URB2D(I,J) = TC_URB
            QC_URB2D(I,J) = QC_URB
            UC_URB2D(I,J) = UC_URB
            DO K = 1,num_roof_layers
              TRL_URB3D(I,K,J) = TRL_URB(K)
              SMR_URB3D(I,K,J) = SMR_URB(K)
              TGRL_URB3D(I,K,J)= TGRL_URB(K)
            END DO
            DO K = 1,num_wall_layers
              TBL_URB3D(I,K,J) = TBL_URB(K)
            END DO
            DO K = 1,num_road_layers
              TGL_URB3D(I,K,J) = TGL_URB(K)
            END DO
            TGR_URB2D(I,J) =TGR_URB
            CMCR_URB2D(I,J)=CMCR_URB
            FLXHUMR_URB2D(I,J)=FLXHUMR_URB
            FLXHUMB_URB2D(I,J)=FLXHUMB_URB
            FLXHUMG_URB2D(I,J)=FLXHUMG_URB
            DRELR_URB2D(I,J) = DRELR_URB
            DRELB_URB2D(I,J) = DRELB_URB
            DRELG_URB2D(I,J) = DRELG_URB
            XXXR_URB2D(I,J) = XXXR_URB
            XXXB_URB2D(I,J) = XXXB_URB
            XXXG_URB2D(I,J) = XXXG_URB
            XXXC_URB2D(I,J) = XXXC_URB
            SH_URB2D(I,J) = SH_URB
            LH_URB2D(I,J) = LH_URB
            G_URB2D(I,J) = G_URB
            RN_URB2D(I,J) = RN_URB
            PSIM_URB2D(I,J) = PSIM_URB
            PSIH_URB2D(I,J) = PSIH_URB
            GZ1OZ0_URB2D(I,J)= GZ1OZ0_URB
            U10_URB2D(I,J) = U10_URB
            V10_URB2D(I,J) = V10_URB
            TH2_URB2D(I,J) = TH2_URB
            Q2_URB2D(I,J) = Q2_URB
            UST_URB2D(I,J) = UST_URB
            AKMS_URB2D(I,J) = vkc * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
            IF (PRESENT(CMR_SFCDIF)) THEN
               CMR_SFCDIF(I,J) = CMR_URB
               CHR_SFCDIF(I,J) = CHR_URB
               CMGR_SFCDIF(I,J) = CMGR_URB
               CHGR_SFCDIF(I,J) = CHGR_URB
               CMC_SFCDIF(I,J) = CMC_URB
               CHC_SFCDIF(I,J) = CHC_URB
            ENDIF
          END IF
         ENDIF
    do m=1,maxpatch
      if(snl(i,m,j)<-10 .or. snl(i,m,j) >10) found=.true.
    end do
   if(found) then
        write(6,*) 'in module_sf_clm, right after clm(), found snl ERROR! at i=',i,'j=',j
        found=.false.
   end if
  end do
  end do
   do i=its,ite
    do j=jts,jte
     do m=1,maxpatch
      if(snl(i,m,j)<-10 .or. snl(i,m,j) >10) found=.true.
     end do
      if(found) then
        write(6,*) 'in module_sf_clm, finish all clm loop, found snl ERROR! at i=',i,'j=',j
        write(6,*) 'snl(',i,':',j,')=',snl(i,:,j)
        found=.false.
       end if
    end do
  end do
        call CLMDebug('clmdrv() success finished')
end subroutine clmdrv
  subroutine clminit(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV, &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW, &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     FNDSOILW, FNDSNOWH, &
                     num_soil_layers, restart, &
                     allowed_to_read , &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte, &
                     maxpatch &
                    ,numc,nump,snl, &
                     snowdp,wtc,wtp,h2osno,t_grnd,t_veg, &
                     h2ocan,h2ocan_col,t2m_max,t2m_min, &
                     t_ref2m,h2osoi_liq_s1, &
                     h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4, &
                     h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2, &
                     h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6, &
                     h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10, &
                     h2osoi_ice_s1,h2osoi_ice_s2, &
                     h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5, &
                     h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4, &
                     h2osoi_ice5,h2osoi_ice6,h2osoi_ice7, &
                     h2osoi_ice8,h2osoi_ice9,h2osoi_ice10, &
                     t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4, &
                     t_soisno_s5,t_soisno1,t_soisno2,t_soisno3, &
                     t_soisno4,t_soisno5,t_soisno6,t_soisno7, &
                     t_soisno8,t_soisno9,t_soisno10, &
                     dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5, &
                     snowrds1,snowrds2,snowrds3,snowrds4,snowrds5, &
                     t_lake1,t_lake2,t_lake3,t_lake4,t_lake5, &
                     t_lake6,t_lake7,t_lake8,t_lake9,t_lake10, &
                     h2osoi_vol1,h2osoi_vol2,h2osoi_vol3, &
                     h2osoi_vol4,h2osoi_vol5,h2osoi_vol6, &
                     h2osoi_vol7,h2osoi_vol8, &
                     h2osoi_vol9,h2osoi_vol10, &
                     ht,xland,xice &
                    ,albedosubgrid,lhsubgrid,hfxsubgrid,lwupsubgrid,q2subgrid &
                    ,sabvsubgrid,sabgsubgrid,nrasubgrid,swupsubgrid, &
                    lhsoi,lhveg,lhtran &
                                                                         )
  USE module_wrf_error
   use clm_varcon, only :snowage_tau,snowage_kappa,snowage_drdt0 &
                        ,ss_alb_snw_drc,asm_prm_snw_drc &
                        ,ext_cff_mss_snw_drc,ss_alb_snw_dfs,asm_prm_snw_dfs &
                        ,ext_cff_mss_snw_dfs &
                        ,xx_ss_alb_snw_drc &
                        ,xx_asm_prm_snw_drc &
                        ,xx_ext_cff_mss_snw_drc &
                        ,xx_ss_alb_snw_dfs &
                        ,xx_asm_prm_snw_dfs &
                        ,xx_ext_cff_mss_snw_dfs &
                        ,xx_snowage_tau &
                        ,xx_snowage_kappa &
                        ,xx_snowage_drdt0 &
                        ,idx_Mie_snw_mx &
                        ,idx_T_max &
                        ,idx_Tgrd_max &
                        ,idx_rhos_max &
                        ,numrad_snw
  implicit none
  INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                   ims,ime, jms,jme, kms,kme, &
                                   its,ite, jts,jte, kts,kte
  logical, external :: wrf_dm_on_monitor
  integer :: ix
  INTEGER, INTENT(IN) :: num_soil_layers,maxpatch
   LOGICAL , INTENT(IN) :: restart , allowed_to_read
   REAL, DIMENSION( num_soil_layers), INTENT(INOUT) :: zs, dzs
   REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , &
            INTENT(INOUT) :: SMOIS, &
                                                         SH2O, &
                                                         TSLB
   REAL, DIMENSION( ims:ime, jms:jme ) , &
            INTENT(INOUT) :: SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: HT
   INTEGER, DIMENSION( ims:ime, jms:jme ) , &
            INTENT(IN) :: IVGTYP, &
                                                        ISLTYP
   REAL, DIMENSION( ims:ime, jms:jme ) , &
            INTENT(IN) :: XLAND,xice
   LOGICAL, DIMENSION( ims:ime, jms:jme ) :: lake
   LOGICAL, INTENT(IN) :: FNDSOILW , &
                                                     FNDSNOWH
  integer, dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  integer, dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real, dimension(ims:ime,jms:jme ),intent(inout) :: t2m_max,t2m_min
  real, dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg, &
                h2ocan,h2ocan_col, &
                t_ref2m,h2osoi_liq_s1, &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4, &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2, &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6, &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10, &
                h2osoi_ice_s1,h2osoi_ice_s2, &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5, &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4, &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7, &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10, &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4, &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3, &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7, &
                t_soisno8,t_soisno9,t_soisno10, &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5, &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5, &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5, &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10, &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3, &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6, &
                h2osoi_vol7,h2osoi_vol8, &
                h2osoi_vol9,h2osoi_vol10, &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid, &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,SWUPsubgrid,&
                LHsoi,LHveg,LHtran
   INTEGER :: L
   REAL :: BX, SMCMAX, PSISAT, FREE
   INTEGER :: errflag
   INTEGER :: itf,jtf,j,i,k,m
   LOGICAL :: opened
   integer :: lu_unit
   call CLMDebug('Now in clminit.')
IF ( wrf_dm_on_monitor() ) THEN
     DO i=10,99
        INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
                lu_unit=i
                GOTO 2011
            ENDIF
         ENDDO
         lu_unit = -1
 2011 CONTINUE
        if(lu_unit<0) then
          write(6,*) 'Can not assign unit to read CLM input data in clminit'
          call endrun()
        end if
  open(lu_unit,file='CLM_ALB_ICE_DRC_DATA')
  read(lu_unit,*) ((ss_alb_snw_drc(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_ASM_ICE_DRC_DATA')
  read(lu_unit,*) ((asm_prm_snw_drc(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_EXT_ICE_DRC_DATA')
  read(lu_unit,*) ((ext_cff_mss_snw_drc(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_ALB_ICE_DFS_DATA')
  read(lu_unit,*) ((ss_alb_snw_dfs(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_ASM_ICE_DFS_DATA')
  read(lu_unit,*) ((asm_prm_snw_dfs(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_EXT_ICE_DFS_DATA')
  read(lu_unit,*) ((ext_cff_mss_snw_dfs(i,j),j=1,numrad_snw),i=1,idx_Mie_snw_mx)
  close(lu_unit)
  open(lu_unit,file='CLM_TAU_DATA')
  read(lu_unit,*) &
  (((snowage_tau(i,j,k),i=1,idx_T_max),j=1,idx_Tgrd_max),k=1,idx_rhos_max)
  close(lu_unit)
  open(lu_unit,file='CLM_KAPPA_DATA')
  read(lu_unit,*) &
  (((snowage_kappa(i,j,k),i=1,idx_T_max),j=1,idx_Tgrd_max),k=1,idx_rhos_max)
  close(lu_unit)
  open(lu_unit,file='CLM_DRDSDT0_DATA')
  read(lu_unit,*)&
  (((snowage_drdt0(i,j,k),i=1,idx_T_max),j=1,idx_Tgrd_max),k=1,idx_rhos_max)
  close(lu_unit)
END IF
  ix = 0
  do i=1, idx_Mie_snw_mx
  do j=1, numrad_snw
    ix = ix + 1
    xx_ss_alb_snw_drc(ix) = ss_alb_snw_drc(i,j)
    xx_asm_prm_snw_drc(ix) = asm_prm_snw_drc(i,j)
    xx_ext_cff_mss_snw_drc(ix) = ext_cff_mss_snw_drc(i,j)
    xx_ss_alb_snw_dfs(ix) = ss_alb_snw_dfs(i,j)
    xx_asm_prm_snw_dfs(ix) = asm_prm_snw_dfs(i,j)
    xx_ext_cff_mss_snw_dfs(ix) = ext_cff_mss_snw_dfs(i,j)
  end do
  end do
 ix = 0
 do i=1,idx_T_max
 do j=1,idx_Tgrd_max
 do k=1,idx_rhos_max
    ix = ix + 1
    xx_snowage_tau(ix) = snowage_tau(i,j,k)
    xx_snowage_kappa(ix) = snowage_kappa(i,j,k)
    xx_snowage_drdt0(ix) = snowage_drdt0(i,j,k)
 end do
 end do
 end do
  CALL wrf_dm_bcast_real(xx_ss_alb_snw_drc, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_asm_prm_snw_drc, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_ext_cff_mss_snw_drc, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_ss_alb_snw_dfs, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_asm_prm_snw_dfs, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_ext_cff_mss_snw_dfs, numrad_snw*idx_Mie_snw_mx )
  CALL wrf_dm_bcast_real(xx_snowage_tau, idx_T_max*idx_Tgrd_max*idx_rhos_max)
  CALL wrf_dm_bcast_real(xx_snowage_kappa,idx_T_max*idx_Tgrd_max*idx_rhos_max)
  CALL wrf_dm_bcast_real(xx_snowage_drdt0,idx_T_max*idx_Tgrd_max*idx_rhos_max)
 IF(restart) return
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
   errflag = 0
   DO j = jts,jtf
     DO i = its,itf
       IF ( ISLTYP( i,j ) .LT. 1 ) THEN
         errflag = 1
         WRITE(wrf_err_message,*)"CLM: clminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
         CALL wrf_message(wrf_err_message)
       ENDIF
     ENDDO
   ENDDO
   IF ( errflag .EQ. 1 ) THEN
      CALL wrf_error_fatal3("<stdin>",4302,&
"CLM: clminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
   ENDIF
          DO j = jts,jtf
          DO i = its,itf
          if((xland(i,j)-1.5).ge.0.)then
             If(xice(i,j).eq.1)print*,' SEA-ICE AT WATER POINT, i=',i,'j=',j
              smstav(i,j)=1.0
              smstot(i,j)=1.0
              smois(i,:,j)=1.0
              tslb(i,:,j)=273.16
            else if(xice(i,j).eq.1.)then
              smstav(i,j)=1.0
              smstot(i,j)=1.0
              smois(i,:,j)=1.0
            endif
            snowh(i,j)=snow(i,j)*0.005
            snowdp(i,:,j) = snowh(i,j)
          ENDDO
          ENDDO
       do i=its,itf
       do j=jts,jtf
           snl(i,:,j) = 0
           h2osoi_liq_s5(i,:,j) = -999.0
           h2osoi_liq_s4(i,:,j) = -999.0
           h2osoi_liq_s3(i,:,j) = -999.0
           h2osoi_liq_s2(i,:,j) = -999.0
           h2osoi_liq_s1(i,:,j) = -999.0
           h2osoi_liq1(i,:,j) = -999.0
           h2osoi_liq2(i,:,j) = -999.0
           h2osoi_liq3(i,:,j) = -999.0
           h2osoi_liq4(i,:,j) = -999.0
           h2osoi_liq5(i,:,j) = -999.0
           h2osoi_liq6(i,:,j) = -999.0
           h2osoi_liq7(i,:,j) = -999.0
           h2osoi_liq8(i,:,j) = -999.0
           h2osoi_liq9(i,:,j) = -999.0
           h2osoi_liq10(i,:,j) = -999.0
           h2osoi_ice_s5(i,:,j) = -999.0
           h2osoi_ice_s4(i,:,j) = -999.0
           h2osoi_ice_s3(i,:,j) = -999.0
           h2osoi_ice_s2(i,:,j) = -999.0
           h2osoi_ice_s1(i,:,j) = -999.0
           h2osoi_ice1(i,:,j) = -999.0
           h2osoi_ice2(i,:,j) = -999.0
           h2osoi_ice3(i,:,j) = -999.0
           h2osoi_ice4(i,:,j) = -999.0
           h2osoi_ice5(i,:,j) = -999.0
           h2osoi_ice6(i,:,j) = -999.0
           h2osoi_ice7(i,:,j) = -999.0
           h2osoi_ice8(i,:,j) = -999.0
           h2osoi_ice9(i,:,j) = -999.0
           h2osoi_ice10(i,:,j) = -999.0
             h2osno(i,:,j) = snow(i,j)
       end do
       end do
       do i=its,itf
       do j=jts,jtf
         numc(i,j) = 0
         nump(i,j) = 0
         wtc(i,:,j) = 0.0
         wtp(i,:,j) = 0.0
       end do
       end do
       do i=its,itf
         do j=jts,jtf
            if(0 == 1) then
                lake(i,j) = .true.
            else
                lake(i,j) = .false.
            end if
        end do
       end do
      do m=1,maxpatch
      do i=its,itf
      do j=jts,jtf
               dzsnow1(i,m,j) = 0.0
               dzsnow2(i,m,j) = 0.0
               dzsnow3(i,m,j) = 0.0
               dzsnow4(i,m,j) = 0.0
               dzsnow5(i,m,j) = 0.0
               if(snowdp(i,m,j).lt.0.01) then
                     snl(i,m,j) = 0
                     dzsnow1(i,m,j) = 0.0
                     dzsnow2(i,m,j) = 0.0
                     dzsnow3(i,m,j) = 0.0
                     dzsnow4(i,m,j) = 0.0
                     dzsnow5(i,m,j) = 0.0
               else
                if(snowdp(i,m,j).ge.0.01.and.snowdp(i,m,j).le.0.03) then
                   snl(i,m,j) = -1
                   dzsnow1(i,m,j) = snowdp(i,m,j)
                else if(snowdp(i,m,j).gt.0.03.and.snowdp(i,m,j).le.0.04) then
                   snl(i,m,j) = -2
                   dzsnow2(i,m,j) = snowdp(i,m,j)/2.
                   dzsnow1(i,m,j) = snowdp(i,m,j)/2.
                else if(snowdp(i,m,j).gt.0.04.and.snowdp(i,m,j).le.0.07) then
                   snl(i,m,j) = -2
                   dzsnow2(i,m,j) = 0.02
                   dzsnow1(i,m,j) = snowdp(i,m,j)- dzsnow2(i,m,j)
                else if(snowdp(i,m,j).gt.0.07.and.snowdp(i,m,j).le.0.12) then
                   snl(i,m,j) = -3
                   dzsnow3(i,m,j) = 0.02
                   dzsnow2(i,m,j) = (snowdp(i,m,j) - 0.02)/2.0
                   dzsnow1(i,m,j) = (snowdp(i,m,j) - 0.02)/2.0
                else if(snowdp(i,m,j).gt.0.12.and.snowdp(i,m,j).le.0.18) then
                   snl(i,m,j) = -3
                   dzsnow3(i,m,j) = 0.02
                   dzsnow2(i,m,j) = 0.05
                   dzsnow1(i,m,j)= snowdp(i,m,j)-dzsnow3(i,m,j)-dzsnow2(i,m,j)
                else if(snowdp(i,m,j).gt.0.18.and.snowdp(i,m,j).le.0.29) then
                   snl(i,m,j) = -4
                   dzsnow4(i,m,j) = 0.02
                   dzsnow3(i,m,j) = 0.05
                   dzsnow2(i,m,j) = (snowdp(i,m,j)-dzsnow4(i,m,j)-dzsnow3(i,m,j))/2.0
                   dzsnow1(i,m,j) = dzsnow2(i,m,j)
                else if(snowdp(i,m,j).gt.0.29.and.snowdp(i,m,j).le.0.41) then
                   snl(i,m,j) = -4
                   dzsnow4(i,m,j) = 0.02
                   dzsnow3(i,m,j) = 0.05
                   dzsnow2(i,m,j) = 0.11
                   dzsnow1(i,m,j) = snowdp(i,m,j)-dzsnow4(i,m,j)-dzsnow3(i,m,j)-dzsnow2(i,m,j)
                else if(snowdp(i,m,j).gt.0.41.and.snowdp(i,m,j).le.0.64) then
                   snl(i,m,j) = -5
                   dzsnow5(i,m,j) = 0.02
                   dzsnow4(i,m,j) = 0.05
                   dzsnow3(i,m,j) = 0.11
                   dzsnow2(i,m,j) = (snowdp(i,m,j)-dzsnow5(i,m,j)-dzsnow4(i,m,j)-dzsnow3(i,m,j))/2.0
                   dzsnow1(i,m,j) = (snowdp(i,m,j)-dzsnow5(i,m,j)-dzsnow4(i,m,j)-dzsnow3(i,m,j))/2.0
                else if(snowdp(i,m,j).gt.0.64) then
                   snl(i,m,j) = -5
                   dzsnow5(i,m,j) = 0.02
                   dzsnow4(i,m,j)= 0.05
                   dzsnow3(i,m,j) = 0.11
                   dzsnow2(i,m,j) = 0.23
                   dzsnow1(i,m,j) = snowdp(i,m,j)-dzsnow5(i,m,j)-dzsnow4(i,m,j)-dzsnow3(i,m,j)-dzsnow2(i,m,j)
                end if
            end if
       end do
       end do
       end do
    do m=1,maxpatch
      do i=its,itf
        do j=jts,jtf
          if(snl(i,m,j) == -5) then
          snowrds1(i,m,j) = 54.526
          snowrds2(i,m,j) = 54.526
          snowrds3(i,m,j) = 54.526
          snowrds4(i,m,j) = 54.526
          snowrds5(i,m,j) = 54.526
          else if(snl(i,m,j) == -4) then
          snowrds1(i,m,j) = 54.526
          snowrds2(i,m,j) = 54.526
          snowrds3(i,m,j) = 54.526
          snowrds4(i,m,j) = 54.526
          snowrds5(i,m,j) = 0.0
           else if(snl(i,m,j) == -3) then
          snowrds1(i,m,j) = 54.526
          snowrds2(i,m,j) = 54.526
          snowrds3(i,m,j) = 54.526
          snowrds4(i,m,j) = 0.0
          snowrds5(i,m,j) = 0.0
          else if(snl(i,m,j) == -2) then
          snowrds1(i,m,j) = 54.526
          snowrds2(i,m,j) = 54.526
          snowrds3(i,m,j) = 0.0
          snowrds4(i,m,j) = 0.0
          snowrds5(i,m,j) = 0.0
          else if(snl(i,m,j) == -1) then
          snowrds1(i,m,j) = 54.526
          snowrds2(i,m,j) = 0.0
          snowrds3(i,m,j) = 0.0
          snowrds4(i,m,j) = 0.0
          snowrds5(i,m,j) = 0.0
          else if(snl(i,m,j) == 0) then
          snowrds1(i,m,j) = 0.0
          snowrds2(i,m,j) = 0.0
          snowrds3(i,m,j) = 0.0
          snowrds4(i,m,j) = 0.0
          snowrds5(i,m,j) = 0.0
         end if
        end do
       end do
    end do
       do i=its,itf
          do j=jts,jtf
              h2ocan(i,:,j) = 0.0
              h2ocan_col(i,:,j) = 0.0
              sfcrunoff(i,j) = 0.0
              udrunoff(i,j) = 0.0
          end do
       end do
      do i=its,itf
      do j=jts,jtf
           t_soisno_s5(i,:,j) = -999.0
           t_soisno_s4(i,:,j) = -999.0
           t_soisno_s3(i,:,j) = -999.0
           t_soisno_s2(i,:,j) = -999.0
           t_soisno_s1(i,:,j) = -999.0
           t_soisno1(i,:,j) = -999.0
           t_soisno2(i,:,j) = -999.0
           t_soisno3(i,:,j) = -999.0
           t_soisno4(i,:,j) = -999.0
           t_soisno5(i,:,j) = -999.0
           t_soisno6(i,:,j) = -999.0
           t_soisno7(i,:,j) = -999.0
           t_soisno8(i,:,j) = -999.0
           t_soisno9(i,:,j) = -999.0
           t_soisno10(i,:,j) = -999.0
           t_lake1(i,:,j) = -999.0
           t_lake2(i,:,j) = -999.0
           t_lake3(i,:,j) = -999.0
           t_lake4(i,:,j) = -999.0
           t_lake5(i,:,j) = -999.0
           t_lake6(i,:,j) = -999.0
           t_lake7(i,:,j) = -999.0
           t_lake8(i,:,j) = -999.0
           t_lake9(i,:,j) = -999.0
           t_lake10(i,:,j) = -999.0
      end do
      end do
      do i=its,itf
      do j=jts,jtf
           do k=1,num_soil_layers
             if(ivgtyp(i,j).eq.24.and.tslb(i,k,j) .gt.tfrz) tslb(i,k,j)=tfrz
           end do
           t_soisno_s5(i,:,j) = tslb(i,1,j)
           t_soisno_s4(i,:,j) = tslb(i,1,j)
           t_soisno_s3(i,:,j) = tslb(i,1,j)
           t_soisno_s2(i,:,j) = tslb(i,1,j)
           t_soisno_s1(i,:,j) = tslb(i,1,j)
           t_soisno1(i,:,j) = tslb(i,1,j)
           t_soisno2(i,:,j) = tslb(i,2,j)
           t_soisno3(i,:,j) = tslb(i,3,j)
           t_soisno4(i,:,j) = tslb(i,4,j)
           t_soisno5(i,:,j) = tslb(i,5,j)
           t_soisno6(i,:,j) = tslb(i,6,j)
           t_soisno7(i,:,j) = tslb(i,7,j)
           t_soisno8(i,:,j) = tslb(i,8,j)
           t_soisno9(i,:,j) = tslb(i,9,j)
           t_soisno10(i,:,j)= tslb(i,10,j)
           t_grnd(i,:,j) = tslb(i,1,j)
           t_veg(i,:,j) = tslb(i,1,j)
      end do
      end do
      do i=its,itf
      do j=jts,jtf
        if(lake(i,j)) then
              t_lake1(i,:,j) = 277.0
              t_lake2(i,:,j) = 277.0
              t_lake3(i,:,j) = 277.0
              t_lake4(i,:,j) = 277.0
              t_lake5(i,:,j) = 277.0
              t_lake6(i,:,j) = 277.0
              t_lake7(i,:,j) = 277.0
              t_lake8(i,:,j) = 277.0
              t_lake9(i,:,j) = 277.0
              t_lake10(i,:,j) = 277.0
              t_grnd(i,:,j) = 277.0
        end if
      end do
      end do
        call CLMDebug('clminit mark2')
      do i=its,itf
      do j=jts,jtf
         h2osoi_vol1(i,:,j) = smois(i,1,j)
         h2osoi_vol2(i,:,j) = smois(i,2,j)
         h2osoi_vol3(i,:,j) = smois(i,3,j)
         h2osoi_vol4(i,:,j) = smois(i,4,j)
         h2osoi_vol5(i,:,j) = smois(i,5,j)
         h2osoi_vol6(i,:,j) = smois(i,6,j)
         h2osoi_vol7(i,:,j) = smois(i,7,j)
         h2osoi_vol8(i,:,j) = smois(i,8,j)
         h2osoi_vol9(i,:,j) = smois(i,9,j)
         h2osoi_vol10(i,:,j) = smois(i,10,j)
         h2osoi_liq_s5(i,:,j) = 0.0
         h2osoi_liq_s4(i,:,j) = 0.0
         h2osoi_liq_s3(i,:,j) = 0.0
         h2osoi_liq_s2(i,:,j) = 0.0
         h2osoi_liq_s1(i,:,j) = 0.0
         h2osoi_ice_s5(i,:,j) = 1.0
         h2osoi_ice_s4(i,:,j) = 1.0
         h2osoi_ice_s3(i,:,j) = 1.0
         h2osoi_ice_s2(i,:,j) = 1.0
         h2osoi_ice_s1(i,:,j) = 1.0
         do m = 1, maxpatch
          if(t_soisno1(i,m,j) <tfrz.and.t_soisno1(i,m,j)/=-999.0) then
             h2osoi_ice1(i,m,j) = dzs(1)*0.917e3*h2osoi_vol1(i,m,j)
             h2osoi_liq1(i,m,j) = 0.0
          else if (t_soisno1(i,m,j) >= tfrz) then
             h2osoi_ice1(i,m,j) = 0.0
             h2osoi_liq1(i,m,j) = dzs(1)*1000.0*h2osoi_vol1(i,m,j)
          end if
          if(t_soisno2(i,m,j) <tfrz.and.t_soisno2(i,m,j)/=-999.0) then
             h2osoi_ice2(i,m,j) = dzs(2)*0.917e3*h2osoi_vol2(i,m,j)
             h2osoi_liq2(i,m,j) = 0.0
          else if (t_soisno2(i,m,j) >= tfrz) then
             h2osoi_ice2(i,m,j) = 0.0
             h2osoi_liq2(i,m,j) = dzs(2)*1000.0*h2osoi_vol2(i,m,j)
          end if
          if(t_soisno3(i,m,j) <tfrz.and.t_soisno3(i,m,j)/=-999.0) then
             h2osoi_ice3(i,m,j) = dzs(3)*0.917e3*h2osoi_vol3(i,m,j)
             h2osoi_liq3(i,m,j) = 0.0
          else if (t_soisno3(i,m,j) >= tfrz) then
             h2osoi_ice3(i,m,j) = 0.0
             h2osoi_liq3(i,m,j) = dzs(3)*1000.0*h2osoi_vol3(i,m,j)
          end if
          if(t_soisno4(i,m,j) <tfrz.and.t_soisno4(i,m,j)/=-999.0) then
             h2osoi_ice4(i,m,j) = dzs(4)*0.917e4*h2osoi_vol4(i,m,j)
             h2osoi_liq4(i,m,j) = 0.0
          else if (t_soisno4(i,m,j) >= tfrz) then
             h2osoi_ice4(i,m,j) = 0.0
             h2osoi_liq4(i,m,j) = dzs(4)*1000.0*h2osoi_vol4(i,m,j)
          end if
          if(t_soisno5(i,m,j) <tfrz.and.t_soisno5(i,m,j)/=-999.0) then
             h2osoi_ice5(i,m,j) = dzs(5)*0.917e4*h2osoi_vol5(i,m,j)
             h2osoi_liq5(i,m,j) = 0.0
          else if (t_soisno5(i,m,j) >= tfrz) then
             h2osoi_ice5(i,m,j) = 0.0
             h2osoi_liq5(i,m,j) = dzs(5)*1000.0*h2osoi_vol5(i,m,j)
          end if
          if(t_soisno6(i,m,j) <tfrz.and.t_soisno6(i,m,j)/=-999.0) then
             h2osoi_ice6(i,m,j) = dzs(6)*0.917e4*h2osoi_vol6(i,m,j)
             h2osoi_liq6(i,m,j) = 0.0
          else if (t_soisno6(i,m,j) >= tfrz) then
             h2osoi_ice6(i,m,j) = 0.0
             h2osoi_liq6(i,m,j) = dzs(6)*1000.0*h2osoi_vol6(i,m,j)
          end if
          if(t_soisno7(i,m,j) <tfrz.and.t_soisno7(i,m,j)/=-999.0) then
             h2osoi_ice7(i,m,j) = dzs(7)*0.917e4*h2osoi_vol7(i,m,j)
             h2osoi_liq7(i,m,j) = 0.0
          else if (t_soisno7(i,m,j) >= tfrz) then
             h2osoi_ice7(i,m,j) = 0.0
             h2osoi_liq7(i,m,j) = dzs(7)*1000.0*h2osoi_vol7(i,m,j)
          end if
          if(t_soisno8(i,m,j) <tfrz.and.t_soisno8(i,m,j)/=-999.0) then
             h2osoi_ice8(i,m,j) = dzs(8)*0.917e4*h2osoi_vol8(i,m,j)
             h2osoi_liq8(i,m,j) = 0.0
          else if (t_soisno8(i,m,j) >= tfrz) then
             h2osoi_ice8(i,m,j) = 0.0
             h2osoi_liq8(i,m,j) = dzs(8)*1000.0*h2osoi_vol8(i,m,j)
          end if
          if(t_soisno9(i,m,j) <tfrz.and.t_soisno9(i,m,j)/=-999.0) then
             h2osoi_ice9(i,m,j) = dzs(9)*0.917e4*h2osoi_vol9(i,m,j)
             h2osoi_liq9(i,m,j) = 0.0
          else if (t_soisno9(i,m,j) >= tfrz) then
             h2osoi_ice9(i,m,j) = 0.0
             h2osoi_liq9(i,m,j) = dzs(9)*1000.0*h2osoi_vol9(i,m,j)
          end if
          if(t_soisno10(i,m,j) <tfrz.and.t_soisno10(i,m,j)/=-999.0) then
             h2osoi_ice10(i,m,j) = dzs(10)*0.917e4*h2osoi_vol10(i,m,j)
             h2osoi_liq10(i,m,j) = 0.0
          else if (t_soisno10(i,m,j) >= tfrz) then
             h2osoi_ice10(i,m,j) = 0.0
             h2osoi_liq10(i,m,j) = dzs(10)*1000.0*h2osoi_vol10(i,m,j)
          end if
        end do
      end do
      end do
      call CLMDebug('clminit mark 4')
      do i=its,itf
      do j=jts,jtf
        t2m_max(i,j) = tslb(i,1,j)
        t2m_min(i,j) = tslb(i,1,j)
        t_ref2m(i,:,j) = tslb(i,1,j)
        albedosubgrid(i,:,j) = 0.0
        lhsubgrid(i,:,j) = 0.0
        hfxsubgrid(i,:,j) = 0.0
        lwupsubgrid(i,:,j) = 0.0
        q2subgrid(i,:,j) = 0.0
        sabvsubgrid(i,:,j) = 0.0
        sabgsubgrid(i,:,j) = 0.0
        nrasubgrid(i,:,j) = 0.0
        swupsubgrid(i,:,j) = 0.0
        lhsoi(i,:,j) = 0.0
        lhveg(i,:,j) = 0.0
        lhtran(i,:,j) = 0.0
      end do
      end do
      do i=its,itf
      do j=jts,jtf
       do k=1, num_soil_layers
          if(tslb(i,k,j) >= tfrz ) then
            sh2o(i,k,j) = smois(i,k,j)
          else
            sh2o(i,k,j) = 0.0
          end if
       end do
      end do
      end do
   call CLMDebug('clminit done')
 END SUBROUTINE clminit
END MODULE module_sf_clm
module decompMod
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar , only : lsmlon, lsmlat, maxpatch, maxpatch_pft, &
                           npatch_crop, npatch_urban, npatch_glacier
  use clm_varsur , only : numlon
  implicit none
  integer, public :: ncells
  integer, public :: nlunits
  integer, public :: ncols
  integer, public :: npfts
  public initDecomp
  public get_gcell_info
  public get_gcell_xyind
  public get_proc_bounds
  save
  private
  type gcell_decomp
     integer :: gsn
     integer :: li
     integer :: lf
     integer :: ci
     integer :: cf
     integer :: pi
     integer :: pf
  end type gcell_decomp
  type(gcell_decomp), allocatable :: gcelldc(:)
contains
  subroutine initDecomp
    use clmtype
    implicit none
    integer :: ppc
    integer :: lpc
    integer :: ppclump
    integer :: i,j,cid,pid
    integer :: gi,li,ci,pi
    integer :: gf,lf,cf,pf
    integer :: g,l,c,p,n,m
    integer :: gdc,gsn
    integer :: nzero
    integer :: nveg
    integer :: numg
    integer :: numl
    integer :: numc
    integer :: nump
    logical :: error = .false.
    integer :: ilunits, icols, ipfts
    integer :: ng
    integer :: nl
    integer :: nc
    integer :: np
    integer :: ier
    character*256 :: msg
    integer :: begg,endg
    begg=1
    endg=1
    ncells = 0
    nlunits = 0
    ncols = 0
    npfts = 0
    msg= ''
    write(msg,*) 'lsmlat=',lsmlat,'numlon=',numlon
    call CLMDebug(msg)
       do g = begg,endg
             call get_gcell_info (g, nlunits=ilunits, ncols=icols, npfts=ipfts)
             ncells = ncells + 1
             nlunits = nlunits + ilunits
             ncols = ncols + icols
             npfts = npfts + ipfts
       end do
  end subroutine initDecomp
   subroutine get_gcell_info (g, nlunits, ncols, npfts, &
                              nveg, wtveg, ncrop, wtcrop)
   use clm_varsur , only: wtxy
     implicit none
     integer ,intent(in) :: g
     integer , optional, intent(out) :: nlunits
     integer , optional, intent(out) :: ncols
     integer , optional, intent(out) :: npfts
     integer , optional, intent(out) :: nveg
     real(r8), optional, intent(out) :: wtveg
     integer , optional, intent(out) :: ncrop
     real(r8), optional, intent(out) :: wtcrop
     integer :: m
     integer :: nvegl
     real(r8) :: wtvegl
     integer :: nvegc
     real(r8) :: wtvegc
     integer :: ilunits
     integer :: icols
     integer :: ipfts
     ipfts = 0
     icols = 0
     ilunits = 0
     do m = 1,maxpatch
        if (wtxy(g,m) > 0.0) ipfts = ipfts + 1
     end do
     nvegl = 0
     wtvegl = 0.0
     do m = 1, maxpatch_pft
        if (wtxy(g,m) > 0.0) then
           nvegl = nvegl + 1
           wtvegl = wtvegl + wtxy(g,m)
        end if
     end do
     if (nvegl > 0) ilunits = ilunits + 1
     if (nvegl > 0) icols = icols + 1
     do m = npatch_urban, npatch_glacier
        if (wtxy(g,m) > 0.0) ilunits = ilunits + 1
        if (wtxy(g,m) > 0.0) icols = icols + 1
     end do
     nvegc = 0
     wtvegc = 0.0
        do m = npatch_glacier+1, npatch_crop
           if (wtxy(g,m) > 0.0) then
              nvegc = nvegc + 1
              wtvegc = wtvegc + wtxy(g,m)
           end if
        end do
        if (nvegc > 0) ilunits = ilunits + 1
        if (nvegc > 0) icols = icols + nvegc
     if (present(nlunits)) nlunits = ilunits
     if (present(ncols)) ncols = icols
     if (present(npfts)) npfts = ipfts
     if (present(nveg)) nveg = nvegl
     if (present(wtveg)) wtveg = wtvegl
     if (present(ncrop)) ncrop = nvegc
     if (present(wtcrop)) wtcrop = wtvegc
 end subroutine get_gcell_info
   subroutine get_proc_bounds (begg, endg, begl, endl, begc, endc, &
                               begp, endp)
     implicit none
     integer,optional, intent(out) :: begp, endp
     integer,optional, intent(out) :: begc, endc
     integer,optional, intent(out) :: begl, endl
     integer,optional, intent(out) :: begg, endg
     if(present(begp)) begp = 1
     if(present(endp)) endp = npfts
     if(present(begc)) begc = 1
     if(present(endc)) endc = ncols
     if(present(begl)) begl = 1
     if(present(endl)) endl = nlunits
     if(present(begg)) begg = 1
     if(present(endg)) endg = 1
   end subroutine get_proc_bounds
   subroutine get_gcell_xyind(lbg, ubg)
     implicit none
     integer, intent(in) :: lbg
     integer, intent(in) :: ubg
     integer :: g
     integer :: i, j
     integer :: ier
!dir$ concurrent
    allocate(gcelldc(ncells), stat=ier)
    g = 0
       do j=1,lsmlat
          numlon(j) = lsmlon
       end do
    do j = 1, lsmlat
       do i = 1, numlon(j)
          g = g + 1
       end do
    end do
    do g = lbg,ubg
    end do
    deallocate(gcelldc)
   end subroutine get_gcell_xyind
end module decompMod
subroutine CLMDebug( str )
  IMPLICIT NONE
  CHARACTER*(*), str
end subroutine CLMDebug
module clmtypeInitMod
  use shr_kind_mod, only : r8 => shr_kind_r8
  use nanMod , only : nan, bigint
  use clmtype
  use clm_varpar , only : maxpatch_pft, nlevsno, nlevgrnd, numrad, nlevlak, &
                           numpft, ndst, nvoc, nlevurb, nlevsoi
  implicit none
  save
  public :: initClmtype
  private :: init_pft_type
  private :: init_column_type
  private :: init_landunit_type
  private :: init_gridcell_type
  private :: init_energy_balance_type
  private :: init_water_balance_type
  private :: init_pft_ecophys_constants
  private :: init_pft_pstate_type
  private :: init_pft_epv_type
  private :: init_pft_vstate_type
  private :: init_pft_estate_type
  private :: init_pft_wstate_type
  private :: init_pft_cstate_type
  private :: init_pft_nstate_type
  private :: init_pft_eflux_type
  private :: init_pft_mflux_type
  private :: init_pft_wflux_type
  private :: init_pft_cflux_type
  private :: init_pft_nflux_type
  private :: init_pft_vflux_type
  private :: init_pft_dflux_type
  private :: init_pft_depvd_type
  private :: init_column_pstate_type
  private :: init_column_estate_type
  private :: init_column_wstate_type
  private :: init_column_cstate_type
  private :: init_column_nstate_type
  private :: init_column_eflux_type
  private :: init_column_wflux_type
  private :: init_column_cflux_type
  private :: init_column_nflux_type
  private :: init_landunit_pstate_type
  private :: init_landunit_eflux_type
  private :: init_gridcell_pstate_type
  private :: init_gridcell_efstate_type
  private :: init_gridcell_wflux_type
  private :: init_gridcell_wstate_type
  private :: init_gridcell_estate_type
  private :: init_atm2lnd_type
  private :: dealloc_pft_type
  private :: dealloc_column_type
  private :: dealloc_landunit_type
  private :: dealloc_gridcell_type
  private :: dealloc_energy_balance_type
  private :: dealloc_water_balance_type
  private :: dealloc_pft_ecophys_constants
  private :: dealloc_pft_pstate_type
  private :: dealloc_pft_epv_type
  private :: dealloc_pft_vstate_type
  private :: dealloc_pft_estate_type
  private :: dealloc_pft_wstate_type
  private :: dealloc_pft_cstate_type
  private :: dealloc_pft_nstate_type
  private :: dealloc_pft_eflux_type
  private :: dealloc_pft_mflux_type
  private :: dealloc_pft_wflux_type
  private :: dealloc_pft_cflux_type
  private :: dealloc_pft_nflux_type
  private :: dealloc_pft_vflux_type
  private :: dealloc_pft_dflux_type
  private :: dealloc_pft_depvd_type
  private :: dealloc_column_pstate_type
  private :: dealloc_column_estate_type
  private :: dealloc_column_wstate_type
  private :: dealloc_column_cstate_type
  private :: dealloc_column_nstate_type
  private :: dealloc_column_eflux_type
  private :: dealloc_column_wflux_type
  private :: dealloc_column_cflux_type
  private :: dealloc_column_nflux_type
  private :: dealloc_landunit_pstate_type
  private :: dealloc_landunit_eflux_type
  private :: dealloc_gridcell_pstate_type
  private :: dealloc_gridcell_efstate_type
  private :: dealloc_gridcell_wflux_type
  private :: dealloc_gridcell_wstate_type
  private :: dealloc_gridcell_estate_type
  private :: dealloc_atm2lnd_type
contains
  subroutine initClmtype()
    use decompMod , only : get_proc_bounds
    implicit none
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    call init_pft_type (begp, endp, clm3%g%l%c%p)
    call init_column_type (begc, endc, clm3%g%l%c)
    call init_landunit_type(begl, endl, clm3%g%l)
    call init_gridcell_type(begg, endg, clm3%g)
    call init_pft_ecophys_constants()
    call init_energy_balance_type(begp, endp, clm3%g%l%c%p%pebal)
    call init_energy_balance_type(begc, endc, clm3%g%l%c%cebal)
    call init_energy_balance_type(begl, endl, clm3%g%l%lebal)
    call init_energy_balance_type(begg, endg, clm3%g%gebal)
    call init_energy_balance_type(1, 1, clm3%mebal)
    call init_water_balance_type(begp, endp, clm3%g%l%c%p%pwbal)
    call init_water_balance_type(begc, endc, clm3%g%l%c%cwbal)
    call init_water_balance_type(begl, endl, clm3%g%l%lwbal)
    call init_water_balance_type(begg, endg, clm3%g%gwbal)
    call init_water_balance_type(1, 1, clm3%mwbal)
    call init_carbon_balance_type(begp, endp, clm3%g%l%c%p%pcbal)
    call init_carbon_balance_type(begc, endc, clm3%g%l%c%ccbal)
    call init_nitrogen_balance_type(begp, endp, clm3%g%l%c%p%pnbal)
    call init_nitrogen_balance_type(begc, endc, clm3%g%l%c%cnbal)
    call init_pft_pstate_type(begp, endp, clm3%g%l%c%p%pps)
    call init_pft_pstate_type(begc, endc, clm3%g%l%c%cps%pps_a)
    call init_pft_epv_type(begp, endp, clm3%g%l%c%p%pepv)
    call init_pft_vstate_type(begp, endp, clm3%g%l%c%p%pvs)
    call init_pft_estate_type(begp, endp, clm3%g%l%c%p%pes)
    call init_pft_estate_type(begc, endc, clm3%g%l%c%ces%pes_a)
    call init_pft_wstate_type(begp, endp, clm3%g%l%c%p%pws)
    call init_pft_wstate_type(begc, endc, clm3%g%l%c%cws%pws_a)
    call init_pft_cstate_type(begp, endp, clm3%g%l%c%p%pcs)
    call init_pft_cstate_type(begc, endc, clm3%g%l%c%ccs%pcs_a)
    call init_pft_nstate_type(begp, endp, clm3%g%l%c%p%pns)
    call init_pft_nstate_type(begc, endc, clm3%g%l%c%cns%pns_a)
    call init_pft_eflux_type(begp, endp, clm3%g%l%c%p%pef)
    call init_pft_eflux_type(begc, endc, clm3%g%l%c%cef%pef_a)
    call init_pft_mflux_type(begp, endp, clm3%g%l%c%p%pmf)
    call init_pft_mflux_type(begc, endc, clm3%g%l%c%cmf%pmf_a)
    call init_pft_wflux_type(begp, endp, clm3%g%l%c%p%pwf)
    call init_pft_wflux_type(begc, endc, clm3%g%l%c%cwf%pwf_a)
    call init_pft_cflux_type(begp, endp, clm3%g%l%c%p%pcf)
    call init_pft_cflux_type(begc, endc, clm3%g%l%c%ccf%pcf_a)
    call init_pft_nflux_type(begp, endp, clm3%g%l%c%p%pnf)
    call init_pft_nflux_type(begc, endc, clm3%g%l%c%cnf%pnf_a)
    call init_pft_vflux_type(begp, endp, clm3%g%l%c%p%pvf)
    call init_pft_vflux_type(begc, endc, clm3%g%l%c%cvf%pvf_a)
    call init_gridcell_efstate_type(begg, endg, clm3%g%gve)
    call init_pft_dflux_type(begp, endp, clm3%g%l%c%p%pdf)
    call init_pft_dflux_type(begc, endc, clm3%g%l%c%cdf%pdf_a)
    call init_pft_depvd_type(begp, endp, clm3%g%l%c%p%pdd)
    call init_column_pstate_type(begc, endc, clm3%g%l%c%cps)
    call init_column_pstate_type(begl, endl, clm3%g%l%lps%cps_a)
    call init_column_pstate_type(begg, endg, clm3%g%gps%cps_a)
    call init_column_pstate_type(1, 1, clm3%mps%cps_a)
    call init_column_estate_type(begc, endc, clm3%g%l%c%ces)
    call init_column_estate_type(begl, endl, clm3%g%l%les%ces_a)
    call init_column_estate_type(begg, endg, clm3%g%ges%ces_a)
    call init_column_estate_type(1, 1, clm3%mes%ces_a)
    call init_column_wstate_type(begc, endc, clm3%g%l%c%cws)
    call init_column_wstate_type(begl, endl, clm3%g%l%lws%cws_a)
    call init_column_wstate_type(begg, endg, clm3%g%gws%cws_a)
    call init_column_wstate_type(1, 1, clm3%mws%cws_a)
    call init_column_cstate_type(begc, endc, clm3%g%l%c%ccs)
    call init_column_cstate_type(begl, endl, clm3%g%l%lcs%ccs_a)
    call init_column_cstate_type(begg, endg, clm3%g%gcs%ccs_a)
    call init_column_cstate_type(1, 1, clm3%mcs%ccs_a)
    call init_column_nstate_type(begc, endc, clm3%g%l%c%cns)
    call init_column_nstate_type(begl, endl, clm3%g%l%lns%cns_a)
    call init_column_nstate_type(begg, endg, clm3%g%gns%cns_a)
    call init_column_nstate_type(1, 1, clm3%mns%cns_a)
    call init_column_eflux_type(begc, endc, clm3%g%l%c%cef)
    call init_column_eflux_type(begl, endl, clm3%g%l%lef%cef_a)
    call init_column_eflux_type(begg, endg, clm3%g%gef%cef_a)
    call init_column_eflux_type(1, 1, clm3%mef%cef_a)
    call init_column_wflux_type(begc, endc, clm3%g%l%c%cwf)
    call init_column_wflux_type(begl, endl, clm3%g%l%lwf%cwf_a)
    call init_column_wflux_type(begg, endg, clm3%g%gwf%cwf_a)
    call init_column_wflux_type(1, 1, clm3%mwf%cwf_a)
    call init_column_cflux_type(begc, endc, clm3%g%l%c%ccf)
    call init_column_nflux_type(begc, endc, clm3%g%l%c%cnf)
    call init_landunit_pstate_type(begl, endl, clm3%g%l%lps)
    call init_landunit_eflux_type(begl, endl, clm3%g%l%lef)
    call init_gridcell_pstate_type(begg, endg, clm3%g%gps)
    call init_gridcell_wflux_type(begg, endg, clm3%g%gwf)
    call init_gridcell_eflux_type(begg, endg, clm3%g%gef)
    call init_gridcell_wstate_type(begg, endg, clm3%g%gws)
    call init_gridcell_estate_type(begg, endg, clm3%g%ges)
    call init_atm2lnd_type (begg , endg , clm_a2l)
  end subroutine initClmtype
  subroutine init_pft_type (beg, end, p)
    implicit none
    integer, intent(in) :: beg, end
    type(pft_type), intent(inout):: p
    allocate(p%gridcell(beg:end),p%wtgcell(beg:end))
    allocate(p%landunit(beg:end),p%wtlunit(beg:end))
    allocate(p%column (beg:end),p%wtcol (beg:end))
    allocate(p%itype(beg:end))
    allocate(p%mxy(beg:end))
    allocate(p%area(beg:end))
  end subroutine init_pft_type
  subroutine init_column_type (beg, end, c)
    implicit none
    integer, intent(in) :: beg, end
    type(column_type), intent(inout):: c
   allocate(c%gridcell(beg:end),c%wtgcell(beg:end))
   allocate(c%landunit(beg:end),c%wtlunit(beg:end))
   allocate(c%pfti(beg:end),c%pftf(beg:end),c%npfts(beg:end))
   allocate(c%itype(beg:end))
   allocate(c%area(beg:end))
  end subroutine init_column_type
  subroutine init_landunit_type (beg, end,l)
    implicit none
    integer, intent(in) :: beg, end
    type(landunit_type), intent(inout):: l
   allocate(l%gridcell(beg:end),l%wtgcell(beg:end))
   allocate(l%coli(beg:end),l%colf(beg:end),l%ncolumns(beg:end))
   allocate(l%pfti(beg:end),l%pftf(beg:end),l%npfts (beg:end))
   allocate(l%itype(beg:end))
   allocate(l%ifspecial(beg:end))
   allocate(l%lakpoi(beg:end))
   allocate(l%urbpoi(beg:end))
   allocate(l%canyon_hwr(beg:end))
   allocate(l%wtroad_perv(beg:end))
   allocate(l%ht_roof(beg:end))
   allocate(l%wtlunit_roof(beg:end))
   allocate(l%wind_hgt_canyon(beg:end))
   allocate(l%z_0_town(beg:end))
   allocate(l%z_d_town(beg:end))
   allocate(l%area(beg:end))
   l%canyon_hwr(beg:end) = nan
   l%wtroad_perv(beg:end) = nan
   l%ht_roof(beg:end) = nan
   l%wtlunit_roof(beg:end) = nan
   l%wind_hgt_canyon(beg:end) = nan
   l%z_0_town(beg:end) = nan
   l%z_d_town(beg:end) = nan
  end subroutine init_landunit_type
  subroutine init_gridcell_type (beg, end,g)
    implicit none
    integer, intent(in) :: beg, end
    type(gridcell_type), intent(inout):: g
   allocate(g%luni(beg:end),g%lunf(beg:end),g%nlandunits(beg:end))
   allocate(g%coli(beg:end),g%colf(beg:end),g%ncolumns (beg:end))
   allocate(g%pfti(beg:end),g%pftf(beg:end),g%npfts (beg:end))
   allocate(g%gindex(beg:end))
   allocate(g%area(beg:end))
   allocate(g%lat(beg:end))
   allocate(g%lon(beg:end))
   allocate(g%latdeg(beg:end))
   allocate(g%londeg(beg:end))
   allocate(g%gindex_a(beg:end))
   allocate(g%lat_a(beg:end))
   allocate(g%lon_a(beg:end))
   allocate(g%latdeg_a(beg:end))
   allocate(g%londeg_a(beg:end))
  end subroutine init_gridcell_type
  subroutine init_energy_balance_type(beg, end, ebal)
    implicit none
    integer, intent(in) :: beg, end
    type(energy_balance_type), intent(inout):: ebal
    allocate(ebal%errsoi(beg:end))
    allocate(ebal%errseb(beg:end))
    allocate(ebal%errsol(beg:end))
    allocate(ebal%errlon(beg:end))
    ebal%errsoi(beg:end) = nan
    ebal%errseb(beg:end) = nan
    ebal%errsol(beg:end) = nan
    ebal%errlon(beg:end) = nan
  end subroutine init_energy_balance_type
  subroutine init_water_balance_type(beg, end, wbal)
    implicit none
    integer, intent(in) :: beg, end
    type(water_balance_type), intent(inout):: wbal
    allocate(wbal%begwb(beg:end))
    allocate(wbal%endwb(beg:end))
    allocate(wbal%errh2o(beg:end))
    wbal%begwb(beg:end) = nan
    wbal%endwb(beg:end) = nan
    wbal%errh2o(beg:end) = nan
  end subroutine init_water_balance_type
  subroutine init_carbon_balance_type(beg, end, cbal)
    implicit none
    integer, intent(in) :: beg, end
    type(carbon_balance_type), intent(inout):: cbal
    allocate(cbal%begcb(beg:end))
    allocate(cbal%endcb(beg:end))
    allocate(cbal%errcb(beg:end))
    cbal%begcb(beg:end) = nan
    cbal%endcb(beg:end) = nan
    cbal%errcb(beg:end) = nan
  end subroutine init_carbon_balance_type
  subroutine init_nitrogen_balance_type(beg, end, nbal)
    implicit none
    integer, intent(in) :: beg, end
    type(nitrogen_balance_type), intent(inout):: nbal
    allocate(nbal%begnb(beg:end))
    allocate(nbal%endnb(beg:end))
    allocate(nbal%errnb(beg:end))
    nbal%begnb(beg:end) = nan
    nbal%endnb(beg:end) = nan
    nbal%errnb(beg:end) = nan
  end subroutine init_nitrogen_balance_type
  subroutine init_pft_ecophys_constants()
    implicit none
    allocate(pftcon%noveg(0:numpft))
    allocate(pftcon%tree(0:numpft))
    allocate(pftcon%smpso(0:numpft))
    allocate(pftcon%smpsc(0:numpft))
    allocate(pftcon%fnitr(0:numpft))
    allocate(pftcon%foln(0:numpft))
    allocate(pftcon%dleaf(0:numpft))
    allocate(pftcon%c3psn(0:numpft))
    allocate(pftcon%vcmx25(0:numpft))
    allocate(pftcon%mp(0:numpft))
    allocate(pftcon%qe25(0:numpft))
    allocate(pftcon%xl(0:numpft))
    allocate(pftcon%rhol(0:numpft,numrad))
    allocate(pftcon%rhos(0:numpft,numrad))
    allocate(pftcon%taul(0:numpft,numrad))
    allocate(pftcon%taus(0:numpft,numrad))
    allocate(pftcon%z0mr(0:numpft))
    allocate(pftcon%displar(0:numpft))
    allocate(pftcon%roota_par(0:numpft))
    allocate(pftcon%rootb_par(0:numpft))
    allocate(pftcon%sla(0:numpft))
    allocate(pftcon%slatop(0:numpft))
    allocate(pftcon%dsladlai(0:numpft))
    allocate(pftcon%leafcn(0:numpft))
    allocate(pftcon%flnr(0:numpft))
    allocate(pftcon%woody(0:numpft))
    allocate(pftcon%lflitcn(0:numpft))
    allocate(pftcon%frootcn(0:numpft))
    allocate(pftcon%livewdcn(0:numpft))
    allocate(pftcon%deadwdcn(0:numpft))
    allocate(pftcon%froot_leaf(0:numpft))
    allocate(pftcon%stem_leaf(0:numpft))
    allocate(pftcon%croot_stem(0:numpft))
    allocate(pftcon%flivewd(0:numpft))
    allocate(pftcon%fcur(0:numpft))
    allocate(pftcon%lf_flab(0:numpft))
    allocate(pftcon%lf_fcel(0:numpft))
    allocate(pftcon%lf_flig(0:numpft))
    allocate(pftcon%fr_flab(0:numpft))
    allocate(pftcon%fr_fcel(0:numpft))
    allocate(pftcon%fr_flig(0:numpft))
    allocate(pftcon%dw_fcel(0:numpft))
    allocate(pftcon%dw_flig(0:numpft))
    allocate(pftcon%leaf_long(0:numpft))
    allocate(pftcon%evergreen(0:numpft))
    allocate(pftcon%stress_decid(0:numpft))
    allocate(pftcon%season_decid(0:numpft))
    allocate(pftcon%resist(0:numpft))
    allocate(pftcon%dwood(0:numpft))
    pftcon%noveg(:) = bigint
    pftcon%tree(:) = bigint
    pftcon%smpso(:) = nan
    pftcon%smpsc(:) = nan
    pftcon%fnitr(:) = nan
    pftcon%foln(:) = nan
    pftcon%dleaf(:) = nan
    pftcon%c3psn(:) = nan
    pftcon%vcmx25(:) = nan
    pftcon%mp(:) = nan
    pftcon%qe25(:) = nan
    pftcon%xl(:) = nan
    pftcon%rhol(:,:numrad) = nan
    pftcon%rhos(:,:numrad) = nan
    pftcon%taul(:,:numrad) = nan
    pftcon%taus(:,:numrad) = nan
    pftcon%z0mr(:) = nan
    pftcon%displar(:) = nan
    pftcon%roota_par(:) = nan
    pftcon%rootb_par(:) = nan
    pftcon%sla(:) = nan
    pftcon%slatop(:) = nan
    pftcon%dsladlai(:) = nan
    pftcon%leafcn(:) = nan
    pftcon%flnr(:) = nan
    pftcon%woody(:) = nan
    pftcon%lflitcn(:) = nan
    pftcon%frootcn(:) = nan
    pftcon%livewdcn(:) = nan
    pftcon%deadwdcn(:) = nan
    pftcon%froot_leaf(:) = nan
    pftcon%stem_leaf(:) = nan
    pftcon%croot_stem(:) = nan
    pftcon%flivewd(:) = nan
    pftcon%fcur(:) = nan
    pftcon%lf_flab(:) = nan
    pftcon%lf_fcel(:) = nan
    pftcon%lf_flig(:) = nan
    pftcon%fr_flab(:) = nan
    pftcon%fr_fcel(:) = nan
    pftcon%fr_flig(:) = nan
    pftcon%dw_fcel(:) = nan
    pftcon%dw_flig(:) = nan
    pftcon%leaf_long(:) = nan
    pftcon%evergreen(:) = nan
    pftcon%stress_decid(:) = nan
    pftcon%season_decid(:) = nan
    pftcon%resist(:) = nan
    pftcon%dwood(:) = nan
  end subroutine init_pft_ecophys_constants
  subroutine init_pft_pstate_type(beg, end, pps)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (pft_pstate_type), intent(inout):: pps
    allocate(pps%frac_veg_nosno(beg:end))
    allocate(pps%frac_veg_nosno_alb(beg:end))
    allocate(pps%emv(beg:end))
    allocate(pps%z0mv(beg:end))
    allocate(pps%z0hv(beg:end))
    allocate(pps%z0qv(beg:end))
    allocate(pps%rootfr(beg:end,1:nlevgrnd))
    allocate(pps%rootr(beg:end,1:nlevgrnd))
    allocate(pps%rresis(beg:end,1:nlevgrnd))
    allocate(pps%dewmx(beg:end))
    allocate(pps%rssun(beg:end))
    allocate(pps%rssha(beg:end))
    allocate(pps%laisun(beg:end))
    allocate(pps%laisha(beg:end))
    allocate(pps%btran(beg:end))
    allocate(pps%fsun(beg:end))
    allocate(pps%tlai(beg:end))
    allocate(pps%tsai(beg:end))
    allocate(pps%elai(beg:end))
    allocate(pps%esai(beg:end))
    allocate(pps%fwet(beg:end))
    allocate(pps%fdry(beg:end))
    allocate(pps%dt_veg(beg:end))
    allocate(pps%htop(beg:end))
    allocate(pps%hbot(beg:end))
    allocate(pps%z0m(beg:end))
    allocate(pps%displa(beg:end))
    allocate(pps%albd(beg:end,1:numrad))
    allocate(pps%albi(beg:end,1:numrad))
    allocate(pps%fabd(beg:end,1:numrad))
    allocate(pps%fabi(beg:end,1:numrad))
    allocate(pps%ftdd(beg:end,1:numrad))
    allocate(pps%ftid(beg:end,1:numrad))
    allocate(pps%ftii(beg:end,1:numrad))
    allocate(pps%u10(beg:end))
    allocate(pps%fv(beg:end))
    allocate(pps%ram1(beg:end))
    allocate(pps%vds(beg:end))
    allocate(pps%slasun(beg:end))
    allocate(pps%slasha(beg:end))
    allocate(pps%lncsun(beg:end))
    allocate(pps%lncsha(beg:end))
    allocate(pps%vcmxsun(beg:end))
    allocate(pps%vcmxsha(beg:end))
    allocate(pps%gdir(beg:end))
    allocate(pps%omega(beg:end,1:numrad))
    allocate(pps%eff_kid(beg:end,1:numrad))
    allocate(pps%eff_kii(beg:end,1:numrad))
    allocate(pps%sun_faid(beg:end,1:numrad))
    allocate(pps%sun_faii(beg:end,1:numrad))
    allocate(pps%sha_faid(beg:end,1:numrad))
    allocate(pps%sha_faii(beg:end,1:numrad))
    allocate(pps%forc_hgt_u_pft(beg:end))
    allocate(pps%forc_hgt_t_pft(beg:end))
    allocate(pps%forc_hgt_q_pft(beg:end))
    allocate(pps%cisun(beg:end))
    allocate(pps%cisha(beg:end))
    allocate(pps%sandfrac(beg:end))
    allocate(pps%clayfrac(beg:end))
    pps%sandfrac(beg:end) = nan
    pps%clayfrac(beg:end) = nan
    allocate(pps%mlaidiff(beg:end))
    allocate(pps%rb1(beg:end))
    allocate(pps%annlai(12,beg:end))
    pps%mlaidiff(beg:end) = nan
    pps%rb1(beg:end) = nan
    pps%annlai(:,:) = nan
    pps%frac_veg_nosno(beg:end) = bigint
    pps%frac_veg_nosno_alb(beg:end) = 0
    pps%emv(beg:end) = nan
    pps%z0mv(beg:end) = nan
    pps%z0hv(beg:end) = nan
    pps%z0qv(beg:end) = nan
    pps%rootfr(beg:end,:nlevgrnd) = spval
    pps%rootr (beg:end,:nlevgrnd) = spval
    pps%rresis(beg:end,:nlevgrnd) = spval
    pps%dewmx(beg:end) = nan
    pps%rssun(beg:end) = nan
    pps%rssha(beg:end) = nan
    pps%laisun(beg:end) = nan
    pps%laisha(beg:end) = nan
    pps%btran(beg:end) = nan
    pps%fsun(beg:end) = spval
    pps%tlai(beg:end) = 0._r8
    pps%tsai(beg:end) = 0._r8
    pps%elai(beg:end) = 0._r8
    pps%esai(beg:end) = 0._r8
    pps%fwet(beg:end) = nan
    pps%fdry(beg:end) = nan
    pps%dt_veg(beg:end) = nan
    pps%htop(beg:end) = 0._r8
    pps%hbot(beg:end) = 0._r8
    pps%z0m(beg:end) = nan
    pps%displa(beg:end) = nan
    pps%albd(beg:end,:numrad) = nan
    pps%albi(beg:end,:numrad) = nan
    pps%fabd(beg:end,:numrad) = nan
    pps%fabi(beg:end,:numrad) = nan
    pps%ftdd(beg:end,:numrad) = nan
    pps%ftid(beg:end,:numrad) = nan
    pps%ftii(beg:end,:numrad) = nan
    pps%u10(beg:end) = nan
    pps%fv(beg:end) = nan
    pps%ram1(beg:end) = nan
    pps%vds(beg:end) = nan
    pps%slasun(beg:end) = nan
    pps%slasha(beg:end) = nan
    pps%lncsun(beg:end) = nan
    pps%lncsha(beg:end) = nan
    pps%vcmxsun(beg:end) = nan
    pps%vcmxsha(beg:end) = nan
    pps%gdir(beg:end) = nan
    pps%omega(beg:end,1:numrad) = nan
    pps%eff_kid(beg:end,1:numrad) = nan
    pps%eff_kii(beg:end,1:numrad) = nan
    pps%sun_faid(beg:end,1:numrad) = nan
    pps%sun_faii(beg:end,1:numrad) = nan
    pps%sha_faid(beg:end,1:numrad) = nan
    pps%sha_faii(beg:end,1:numrad) = nan
    pps%forc_hgt_u_pft(beg:end) = nan
    pps%forc_hgt_t_pft(beg:end) = nan
    pps%forc_hgt_q_pft(beg:end) = nan
    pps%cisun(beg:end) = nan
    pps%cisha(beg:end) = nan
  end subroutine init_pft_pstate_type
  subroutine init_pft_epv_type(beg, end, pepv)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_epv_type), intent(inout):: pepv
    allocate(pepv%dormant_flag(beg:end))
    allocate(pepv%days_active(beg:end))
    allocate(pepv%onset_flag(beg:end))
    allocate(pepv%onset_counter(beg:end))
    allocate(pepv%onset_gddflag(beg:end))
    allocate(pepv%onset_fdd(beg:end))
    allocate(pepv%onset_gdd(beg:end))
    allocate(pepv%onset_swi(beg:end))
    allocate(pepv%offset_flag(beg:end))
    allocate(pepv%offset_counter(beg:end))
    allocate(pepv%offset_fdd(beg:end))
    allocate(pepv%offset_swi(beg:end))
    allocate(pepv%lgsf(beg:end))
    allocate(pepv%bglfr(beg:end))
    allocate(pepv%bgtr(beg:end))
    allocate(pepv%dayl(beg:end))
    allocate(pepv%prev_dayl(beg:end))
    allocate(pepv%annavg_t2m(beg:end))
    allocate(pepv%tempavg_t2m(beg:end))
    allocate(pepv%gpp(beg:end))
    allocate(pepv%availc(beg:end))
    allocate(pepv%xsmrpool_recover(beg:end))
    allocate(pepv%alloc_pnow(beg:end))
    allocate(pepv%c_allometry(beg:end))
    allocate(pepv%n_allometry(beg:end))
    allocate(pepv%plant_ndemand(beg:end))
    allocate(pepv%tempsum_potential_gpp(beg:end))
    allocate(pepv%annsum_potential_gpp(beg:end))
    allocate(pepv%tempmax_retransn(beg:end))
    allocate(pepv%annmax_retransn(beg:end))
    allocate(pepv%avail_retransn(beg:end))
    allocate(pepv%plant_nalloc(beg:end))
    allocate(pepv%plant_calloc(beg:end))
    allocate(pepv%excess_cflux(beg:end))
    allocate(pepv%downreg(beg:end))
    allocate(pepv%prev_leafc_to_litter(beg:end))
    allocate(pepv%prev_frootc_to_litter(beg:end))
    allocate(pepv%tempsum_npp(beg:end))
    allocate(pepv%annsum_npp(beg:end))
    pepv%dormant_flag(beg:end) = nan
    pepv%days_active(beg:end) = nan
    pepv%onset_flag(beg:end) = nan
    pepv%onset_counter(beg:end) = nan
    pepv%onset_gddflag(beg:end) = nan
    pepv%onset_fdd(beg:end) = nan
    pepv%onset_gdd(beg:end) = nan
    pepv%onset_swi(beg:end) = nan
    pepv%offset_flag(beg:end) = nan
    pepv%offset_counter(beg:end) = nan
    pepv%offset_fdd(beg:end) = nan
    pepv%offset_swi(beg:end) = nan
    pepv%lgsf(beg:end) = nan
    pepv%bglfr(beg:end) = nan
    pepv%bgtr(beg:end) = nan
    pepv%dayl(beg:end) = nan
    pepv%prev_dayl(beg:end) = nan
    pepv%annavg_t2m(beg:end) = nan
    pepv%tempavg_t2m(beg:end) = nan
    pepv%gpp(beg:end) = nan
    pepv%availc(beg:end) = nan
    pepv%xsmrpool_recover(beg:end) = nan
    pepv%alloc_pnow(beg:end) = nan
    pepv%c_allometry(beg:end) = nan
    pepv%n_allometry(beg:end) = nan
    pepv%plant_ndemand(beg:end) = nan
    pepv%tempsum_potential_gpp(beg:end) = nan
    pepv%annsum_potential_gpp(beg:end) = nan
    pepv%tempmax_retransn(beg:end) = nan
    pepv%annmax_retransn(beg:end) = nan
    pepv%avail_retransn(beg:end) = nan
    pepv%plant_nalloc(beg:end) = nan
    pepv%plant_calloc(beg:end) = nan
    pepv%excess_cflux(beg:end) = nan
    pepv%downreg(beg:end) = nan
    pepv%prev_leafc_to_litter(beg:end) = nan
    pepv%prev_frootc_to_litter(beg:end) = nan
    pepv%tempsum_npp(beg:end) = nan
    pepv%annsum_npp(beg:end) = nan
  end subroutine init_pft_epv_type
  subroutine init_pft_vstate_type(beg, end, pvs)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (pft_vstate_type), intent(inout):: pvs
    allocate(pvs%t_veg24 (beg:end))
    allocate(pvs%t_veg240(beg:end))
    allocate(pvs%fsd24 (beg:end))
    allocate(pvs%fsd240 (beg:end))
    allocate(pvs%fsi24 (beg:end))
    allocate(pvs%fsi240 (beg:end))
    allocate(pvs%fsun24 (beg:end))
    allocate(pvs%fsun240 (beg:end))
    allocate(pvs%elai_p (beg:end))
    pvs%t_veg24 (beg:end) = spval
    pvs%t_veg240(beg:end) = spval
    pvs%fsd24 (beg:end) = spval
    pvs%fsd240 (beg:end) = spval
    pvs%fsi24 (beg:end) = spval
    pvs%fsi240 (beg:end) = spval
    pvs%fsun24 (beg:end) = spval
    pvs%fsun240 (beg:end) = spval
    pvs%elai_p (beg:end) = spval
  end subroutine init_pft_vstate_type
  subroutine init_pft_estate_type(beg, end, pes)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_estate_type), intent(inout):: pes
    allocate(pes%t_ref2m(beg:end))
    allocate(pes%t_ref2m_min(beg:end))
    allocate(pes%t_ref2m_max(beg:end))
    allocate(pes%t_ref2m_min_inst(beg:end))
    allocate(pes%t_ref2m_max_inst(beg:end))
    allocate(pes%q_ref2m(beg:end))
    allocate(pes%t_ref2m_u(beg:end))
    allocate(pes%t_ref2m_r(beg:end))
    allocate(pes%t_ref2m_min_u(beg:end))
    allocate(pes%t_ref2m_min_r(beg:end))
    allocate(pes%t_ref2m_max_u(beg:end))
    allocate(pes%t_ref2m_max_r(beg:end))
    allocate(pes%t_ref2m_min_inst_u(beg:end))
    allocate(pes%t_ref2m_min_inst_r(beg:end))
    allocate(pes%t_ref2m_max_inst_u(beg:end))
    allocate(pes%t_ref2m_max_inst_r(beg:end))
    allocate(pes%rh_ref2m(beg:end))
    allocate(pes%rh_ref2m_u(beg:end))
    allocate(pes%rh_ref2m_r(beg:end))
    allocate(pes%t_veg(beg:end))
    allocate(pes%thm(beg:end))
    pes%t_ref2m(beg:end) = nan
    pes%t_ref2m_min(beg:end) = nan
    pes%t_ref2m_max(beg:end) = nan
    pes%t_ref2m_min_inst(beg:end) = nan
    pes%t_ref2m_max_inst(beg:end) = nan
    pes%q_ref2m(beg:end) = nan
    pes%t_ref2m_u(beg:end) = nan
    pes%t_ref2m_r(beg:end) = nan
    pes%t_ref2m_min_u(beg:end) = nan
    pes%t_ref2m_min_r(beg:end) = nan
    pes%t_ref2m_max_u(beg:end) = nan
    pes%t_ref2m_max_r(beg:end) = nan
    pes%t_ref2m_min_inst_u(beg:end) = nan
    pes%t_ref2m_min_inst_r(beg:end) = nan
    pes%t_ref2m_max_inst_u(beg:end) = nan
    pes%t_ref2m_max_inst_r(beg:end) = nan
    pes%rh_ref2m(beg:end) = nan
    pes%rh_ref2m_u(beg:end) = nan
    pes%rh_ref2m_r(beg:end) = nan
    pes%t_veg(beg:end) = nan
    pes%thm(beg:end) = nan
  end subroutine init_pft_estate_type
  subroutine init_pft_wstate_type(beg, end, pws)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_wstate_type), intent(inout):: pws
    allocate(pws%h2ocan(beg:end))
    pws%h2ocan(beg:end) = nan
  end subroutine init_pft_wstate_type
  subroutine init_pft_cstate_type(beg, end, pcs)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_cstate_type), intent(inout):: pcs
    allocate(pcs%leafc(beg:end))
    allocate(pcs%leafc_storage(beg:end))
    allocate(pcs%leafc_xfer(beg:end))
    allocate(pcs%frootc(beg:end))
    allocate(pcs%frootc_storage(beg:end))
    allocate(pcs%frootc_xfer(beg:end))
    allocate(pcs%livestemc(beg:end))
    allocate(pcs%livestemc_storage(beg:end))
    allocate(pcs%livestemc_xfer(beg:end))
    allocate(pcs%deadstemc(beg:end))
    allocate(pcs%deadstemc_storage(beg:end))
    allocate(pcs%deadstemc_xfer(beg:end))
    allocate(pcs%livecrootc(beg:end))
    allocate(pcs%livecrootc_storage(beg:end))
    allocate(pcs%livecrootc_xfer(beg:end))
    allocate(pcs%deadcrootc(beg:end))
    allocate(pcs%deadcrootc_storage(beg:end))
    allocate(pcs%deadcrootc_xfer(beg:end))
    allocate(pcs%gresp_storage(beg:end))
    allocate(pcs%gresp_xfer(beg:end))
    allocate(pcs%cpool(beg:end))
    allocate(pcs%xsmrpool(beg:end))
    allocate(pcs%pft_ctrunc(beg:end))
    allocate(pcs%dispvegc(beg:end))
    allocate(pcs%storvegc(beg:end))
    allocate(pcs%totvegc(beg:end))
    allocate(pcs%totpftc(beg:end))
    allocate(pcs%leafcmax(beg:end))
    pcs%leafc(beg:end) = nan
    pcs%leafc_storage(beg:end) = nan
    pcs%leafc_xfer(beg:end) = nan
    pcs%frootc(beg:end) = nan
    pcs%frootc_storage(beg:end) = nan
    pcs%frootc_xfer(beg:end) = nan
    pcs%livestemc(beg:end) = nan
    pcs%livestemc_storage(beg:end) = nan
    pcs%livestemc_xfer(beg:end) = nan
    pcs%deadstemc(beg:end) = nan
    pcs%deadstemc_storage(beg:end) = nan
    pcs%deadstemc_xfer(beg:end) = nan
    pcs%livecrootc(beg:end) = nan
    pcs%livecrootc_storage(beg:end) = nan
    pcs%livecrootc_xfer(beg:end) = nan
    pcs%deadcrootc(beg:end) = nan
    pcs%deadcrootc_storage(beg:end) = nan
    pcs%deadcrootc_xfer(beg:end) = nan
    pcs%gresp_storage(beg:end) = nan
    pcs%gresp_xfer(beg:end) = nan
    pcs%cpool(beg:end) = nan
    pcs%xsmrpool(beg:end) = nan
    pcs%pft_ctrunc(beg:end) = nan
    pcs%dispvegc(beg:end) = nan
    pcs%storvegc(beg:end) = nan
    pcs%totvegc(beg:end) = nan
    pcs%totpftc(beg:end) = nan
    pcs%leafcmax(beg:end) = nan
  end subroutine init_pft_cstate_type
  subroutine init_pft_nstate_type(beg, end, pns)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_nstate_type), intent(inout):: pns
    allocate(pns%leafn(beg:end))
    allocate(pns%leafn_storage(beg:end))
    allocate(pns%leafn_xfer(beg:end))
    allocate(pns%frootn(beg:end))
    allocate(pns%frootn_storage(beg:end))
    allocate(pns%frootn_xfer(beg:end))
    allocate(pns%livestemn(beg:end))
    allocate(pns%livestemn_storage(beg:end))
    allocate(pns%livestemn_xfer(beg:end))
    allocate(pns%deadstemn(beg:end))
    allocate(pns%deadstemn_storage(beg:end))
    allocate(pns%deadstemn_xfer(beg:end))
    allocate(pns%livecrootn(beg:end))
    allocate(pns%livecrootn_storage(beg:end))
    allocate(pns%livecrootn_xfer(beg:end))
    allocate(pns%deadcrootn(beg:end))
    allocate(pns%deadcrootn_storage(beg:end))
    allocate(pns%deadcrootn_xfer(beg:end))
    allocate(pns%retransn(beg:end))
    allocate(pns%npool(beg:end))
    allocate(pns%pft_ntrunc(beg:end))
    allocate(pns%dispvegn(beg:end))
    allocate(pns%storvegn(beg:end))
    allocate(pns%totvegn(beg:end))
    allocate(pns%totpftn(beg:end))
    pns%leafn(beg:end) = nan
    pns%leafn_storage(beg:end) = nan
    pns%leafn_xfer(beg:end) = nan
    pns%frootn(beg:end) = nan
    pns%frootn_storage(beg:end) = nan
    pns%frootn_xfer(beg:end) = nan
    pns%livestemn(beg:end) = nan
    pns%livestemn_storage(beg:end) = nan
    pns%livestemn_xfer(beg:end) = nan
    pns%deadstemn(beg:end) = nan
    pns%deadstemn_storage(beg:end) = nan
    pns%deadstemn_xfer(beg:end) = nan
    pns%livecrootn(beg:end) = nan
    pns%livecrootn_storage(beg:end) = nan
    pns%livecrootn_xfer(beg:end) = nan
    pns%deadcrootn(beg:end) = nan
    pns%deadcrootn_storage(beg:end) = nan
    pns%deadcrootn_xfer(beg:end) = nan
    pns%retransn(beg:end) = nan
    pns%npool(beg:end) = nan
    pns%pft_ntrunc(beg:end) = nan
    pns%dispvegn(beg:end) = nan
    pns%storvegn(beg:end) = nan
    pns%totvegn(beg:end) = nan
    pns%totpftn(beg:end) = nan
  end subroutine init_pft_nstate_type
  subroutine init_pft_eflux_type(beg, end, pef)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_eflux_type), intent(inout):: pef
    allocate(pef%sabg(beg:end))
    allocate(pef%sabv(beg:end))
    allocate(pef%fsa(beg:end))
    allocate(pef%fsa_u(beg:end))
    allocate(pef%fsa_r(beg:end))
    allocate(pef%fsr(beg:end))
    allocate(pef%parsun(beg:end))
    allocate(pef%parsha(beg:end))
    allocate(pef%dlrad(beg:end))
    allocate(pef%ulrad(beg:end))
    allocate(pef%eflx_lh_tot(beg:end))
    allocate(pef%eflx_lh_tot_u(beg:end))
    allocate(pef%eflx_lh_tot_r(beg:end))
    allocate(pef%eflx_lh_grnd(beg:end))
    allocate(pef%eflx_soil_grnd(beg:end))
    allocate(pef%eflx_soil_grnd_u(beg:end))
    allocate(pef%eflx_soil_grnd_r(beg:end))
    allocate(pef%eflx_sh_tot(beg:end))
    allocate(pef%eflx_sh_tot_u(beg:end))
    allocate(pef%eflx_sh_tot_r(beg:end))
    allocate(pef%eflx_sh_grnd(beg:end))
    allocate(pef%eflx_sh_veg(beg:end))
    allocate(pef%eflx_lh_vege(beg:end))
    allocate(pef%eflx_lh_vegt(beg:end))
    allocate(pef%eflx_wasteheat_pft(beg:end))
    allocate(pef%eflx_heat_from_ac_pft(beg:end))
    allocate(pef%eflx_traffic_pft(beg:end))
    allocate(pef%eflx_anthro(beg:end))
    allocate(pef%cgrnd(beg:end))
    allocate(pef%cgrndl(beg:end))
    allocate(pef%cgrnds(beg:end))
    allocate(pef%eflx_gnet(beg:end))
    allocate(pef%dgnetdT(beg:end))
    allocate(pef%eflx_lwrad_out(beg:end))
    allocate(pef%eflx_lwrad_net(beg:end))
    allocate(pef%eflx_lwrad_net_u(beg:end))
    allocate(pef%eflx_lwrad_net_r(beg:end))
    allocate(pef%netrad(beg:end))
    allocate(pef%fsds_vis_d(beg:end))
    allocate(pef%fsds_nir_d(beg:end))
    allocate(pef%fsds_vis_i(beg:end))
    allocate(pef%fsds_nir_i(beg:end))
    allocate(pef%fsr_vis_d(beg:end))
    allocate(pef%fsr_nir_d(beg:end))
    allocate(pef%fsr_vis_i(beg:end))
    allocate(pef%fsr_nir_i(beg:end))
    allocate(pef%fsds_vis_d_ln(beg:end))
    allocate(pef%fsds_nir_d_ln(beg:end))
    allocate(pef%fsr_vis_d_ln(beg:end))
    allocate(pef%fsr_nir_d_ln(beg:end))
    allocate(pef%sun_add(beg:end,1:numrad))
    allocate(pef%tot_aid(beg:end,1:numrad))
    allocate(pef%sun_aid(beg:end,1:numrad))
    allocate(pef%sun_aii(beg:end,1:numrad))
    allocate(pef%sha_aid(beg:end,1:numrad))
    allocate(pef%sha_aii(beg:end,1:numrad))
    allocate(pef%sun_atot(beg:end,1:numrad))
    allocate(pef%sha_atot(beg:end,1:numrad))
    allocate(pef%sun_alf(beg:end,1:numrad))
    allocate(pef%sha_alf(beg:end,1:numrad))
    allocate(pef%sun_aperlai(beg:end,1:numrad))
    allocate(pef%sha_aperlai(beg:end,1:numrad))
    allocate(pef%sabg_lyr(beg:end,-nlevsno+1:1))
    allocate(pef%sfc_frc_aer(beg:end))
    allocate(pef%sfc_frc_bc(beg:end))
    allocate(pef%sfc_frc_oc(beg:end))
    allocate(pef%sfc_frc_dst(beg:end))
    allocate(pef%sfc_frc_aer_sno(beg:end))
    allocate(pef%sfc_frc_bc_sno(beg:end))
    allocate(pef%sfc_frc_oc_sno(beg:end))
    allocate(pef%sfc_frc_dst_sno(beg:end))
    allocate(pef%fsr_sno_vd(beg:end))
    allocate(pef%fsr_sno_nd(beg:end))
    allocate(pef%fsr_sno_vi(beg:end))
    allocate(pef%fsr_sno_ni(beg:end))
    allocate(pef%fsds_sno_vd(beg:end))
    allocate(pef%fsds_sno_nd(beg:end))
    allocate(pef%fsds_sno_vi(beg:end))
    allocate(pef%fsds_sno_ni(beg:end))
    pef%sabg(beg:end) = nan
    pef%sabv(beg:end) = nan
    pef%fsa(beg:end) = nan
    pef%fsa_u(beg:end) = nan
    pef%fsa_r(beg:end) = nan
    pef%fsr(beg:end) = nan
    pef%parsun(beg:end) = nan
    pef%parsha(beg:end) = nan
    pef%dlrad(beg:end) = nan
    pef%ulrad(beg:end) = nan
    pef%eflx_lh_tot(beg:end) = nan
    pef%eflx_lh_tot_u(beg:end) = nan
    pef%eflx_lh_tot_r(beg:end) = nan
    pef%eflx_lh_grnd(beg:end) = nan
    pef%eflx_soil_grnd(beg:end) = nan
    pef%eflx_soil_grnd_u(beg:end) = nan
    pef%eflx_soil_grnd_r(beg:end) = nan
    pef%eflx_sh_tot(beg:end) = nan
    pef%eflx_sh_tot_u(beg:end) = nan
    pef%eflx_sh_tot_r(beg:end) = nan
    pef%eflx_sh_grnd(beg:end) = nan
    pef%eflx_sh_veg(beg:end) = nan
    pef%eflx_lh_vege(beg:end) = nan
    pef%eflx_lh_vegt(beg:end) = nan
    pef%eflx_wasteheat_pft(beg:end) = nan
    pef%eflx_heat_from_ac_pft(beg:end) = nan
    pef%eflx_traffic_pft(beg:end) = nan
    pef%eflx_anthro(beg:end) = nan
    pef%cgrnd(beg:end) = nan
    pef%cgrndl(beg:end) = nan
    pef%cgrnds(beg:end) = nan
    pef%eflx_gnet(beg:end) = nan
    pef%dgnetdT(beg:end) = nan
    pef%eflx_lwrad_out(beg:end) = nan
    pef%eflx_lwrad_net(beg:end) = nan
    pef%eflx_lwrad_net_u(beg:end) = nan
    pef%eflx_lwrad_net_r(beg:end) = nan
    pef%netrad(beg:end) = nan
    pef%fsds_vis_d(beg:end) = nan
    pef%fsds_nir_d(beg:end) = nan
    pef%fsds_vis_i(beg:end) = nan
    pef%fsds_nir_i(beg:end) = nan
    pef%fsr_vis_d(beg:end) = nan
    pef%fsr_nir_d(beg:end) = nan
    pef%fsr_vis_i(beg:end) = nan
    pef%fsr_nir_i(beg:end) = nan
    pef%fsds_vis_d_ln(beg:end) = nan
    pef%fsds_nir_d_ln(beg:end) = nan
    pef%fsr_vis_d_ln(beg:end) = nan
    pef%fsr_nir_d_ln(beg:end) = nan
    pef%sun_add(beg:end,1:numrad) = nan
    pef%tot_aid(beg:end,1:numrad) = nan
    pef%sun_aid(beg:end,1:numrad) = nan
    pef%sun_aii(beg:end,1:numrad) = nan
    pef%sha_aid(beg:end,1:numrad) = nan
    pef%sha_aii(beg:end,1:numrad) = nan
    pef%sun_atot(beg:end,1:numrad) = nan
    pef%sha_atot(beg:end,1:numrad) = nan
    pef%sun_alf(beg:end,1:numrad) = nan
    pef%sha_alf(beg:end,1:numrad) = nan
    pef%sun_aperlai(beg:end,1:numrad) = nan
    pef%sha_aperlai(beg:end,1:numrad) = nan
    pef%sabg_lyr(beg:end,-nlevsno+1:1) = nan
    pef%sfc_frc_aer(beg:end) = nan
    pef%sfc_frc_bc(beg:end) = nan
    pef%sfc_frc_oc(beg:end) = nan
    pef%sfc_frc_dst(beg:end) = nan
    pef%sfc_frc_aer_sno(beg:end) = nan
    pef%sfc_frc_bc_sno(beg:end) = nan
    pef%sfc_frc_oc_sno(beg:end) = nan
    pef%sfc_frc_dst_sno(beg:end) = nan
    pef%fsr_sno_vd(beg:end) = nan
    pef%fsr_sno_nd(beg:end) = nan
    pef%fsr_sno_vi(beg:end) = nan
    pef%fsr_sno_ni(beg:end) = nan
    pef%fsds_sno_vd(beg:end) = nan
    pef%fsds_sno_nd(beg:end) = nan
    pef%fsds_sno_vi(beg:end) = nan
    pef%fsds_sno_ni(beg:end) = nan
  end subroutine init_pft_eflux_type
  subroutine init_pft_mflux_type(beg, end, pmf)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_mflux_type), intent(inout) :: pmf
    allocate(pmf%taux(beg:end))
    allocate(pmf%tauy(beg:end))
    pmf%taux(beg:end) = nan
    pmf%tauy(beg:end) = nan
  end subroutine init_pft_mflux_type
  subroutine init_pft_wflux_type(beg, end, pwf)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_wflux_type), intent(inout) :: pwf
    allocate(pwf%qflx_prec_intr(beg:end))
    allocate(pwf%qflx_prec_grnd(beg:end))
    allocate(pwf%qflx_rain_grnd(beg:end))
    allocate(pwf%qflx_snow_grnd(beg:end))
    allocate(pwf%qflx_snwcp_liq(beg:end))
    allocate(pwf%qflx_snwcp_ice(beg:end))
    allocate(pwf%qflx_evap_veg(beg:end))
    allocate(pwf%qflx_tran_veg(beg:end))
    allocate(pwf%qflx_evap_can(beg:end))
    allocate(pwf%qflx_evap_soi(beg:end))
    allocate(pwf%qflx_evap_tot(beg:end))
    allocate(pwf%qflx_evap_grnd(beg:end))
    allocate(pwf%qflx_dew_grnd(beg:end))
    allocate(pwf%qflx_sub_snow(beg:end))
    allocate(pwf%qflx_dew_snow(beg:end))
    pwf%qflx_prec_intr(beg:end) = nan
    pwf%qflx_prec_grnd(beg:end) = nan
    pwf%qflx_rain_grnd(beg:end) = nan
    pwf%qflx_snow_grnd(beg:end) = nan
    pwf%qflx_snwcp_liq(beg:end) = nan
    pwf%qflx_snwcp_ice(beg:end) = nan
    pwf%qflx_evap_veg(beg:end) = nan
    pwf%qflx_tran_veg(beg:end) = nan
    pwf%qflx_evap_can(beg:end) = nan
    pwf%qflx_evap_soi(beg:end) = nan
    pwf%qflx_evap_tot(beg:end) = nan
    pwf%qflx_evap_grnd(beg:end) = nan
    pwf%qflx_dew_grnd(beg:end) = nan
    pwf%qflx_sub_snow(beg:end) = nan
    pwf%qflx_dew_snow(beg:end) = nan
  end subroutine init_pft_wflux_type
  subroutine init_pft_cflux_type(beg, end, pcf)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_cflux_type), intent(inout) :: pcf
    allocate(pcf%psnsun(beg:end))
    allocate(pcf%psnsha(beg:end))
    allocate(pcf%fpsn(beg:end))
    allocate(pcf%fco2(beg:end))
    allocate(pcf%m_leafc_to_litter(beg:end))
    allocate(pcf%m_frootc_to_litter(beg:end))
    allocate(pcf%m_leafc_storage_to_litter(beg:end))
    allocate(pcf%m_frootc_storage_to_litter(beg:end))
    allocate(pcf%m_livestemc_storage_to_litter(beg:end))
    allocate(pcf%m_deadstemc_storage_to_litter(beg:end))
    allocate(pcf%m_livecrootc_storage_to_litter(beg:end))
    allocate(pcf%m_deadcrootc_storage_to_litter(beg:end))
    allocate(pcf%m_leafc_xfer_to_litter(beg:end))
    allocate(pcf%m_frootc_xfer_to_litter(beg:end))
    allocate(pcf%m_livestemc_xfer_to_litter(beg:end))
    allocate(pcf%m_deadstemc_xfer_to_litter(beg:end))
    allocate(pcf%m_livecrootc_xfer_to_litter(beg:end))
    allocate(pcf%m_deadcrootc_xfer_to_litter(beg:end))
    allocate(pcf%m_livestemc_to_litter(beg:end))
    allocate(pcf%m_deadstemc_to_litter(beg:end))
    allocate(pcf%m_livecrootc_to_litter(beg:end))
    allocate(pcf%m_deadcrootc_to_litter(beg:end))
    allocate(pcf%m_gresp_storage_to_litter(beg:end))
    allocate(pcf%m_gresp_xfer_to_litter(beg:end))
    allocate(pcf%hrv_leafc_to_litter(beg:end))
    allocate(pcf%hrv_leafc_storage_to_litter(beg:end))
    allocate(pcf%hrv_leafc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_frootc_to_litter(beg:end))
    allocate(pcf%hrv_frootc_storage_to_litter(beg:end))
    allocate(pcf%hrv_frootc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_livestemc_to_litter(beg:end))
    allocate(pcf%hrv_livestemc_storage_to_litter(beg:end))
    allocate(pcf%hrv_livestemc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_deadstemc_to_prod10c(beg:end))
    allocate(pcf%hrv_deadstemc_to_prod100c(beg:end))
    allocate(pcf%hrv_deadstemc_storage_to_litter(beg:end))
    allocate(pcf%hrv_deadstemc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_livecrootc_to_litter(beg:end))
    allocate(pcf%hrv_livecrootc_storage_to_litter(beg:end))
    allocate(pcf%hrv_livecrootc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_deadcrootc_to_litter(beg:end))
    allocate(pcf%hrv_deadcrootc_storage_to_litter(beg:end))
    allocate(pcf%hrv_deadcrootc_xfer_to_litter(beg:end))
    allocate(pcf%hrv_gresp_storage_to_litter(beg:end))
    allocate(pcf%hrv_gresp_xfer_to_litter(beg:end))
    allocate(pcf%hrv_xsmrpool_to_atm(beg:end))
    allocate(pcf%m_leafc_to_fire(beg:end))
    allocate(pcf%m_frootc_to_fire(beg:end))
    allocate(pcf%m_leafc_storage_to_fire(beg:end))
    allocate(pcf%m_frootc_storage_to_fire(beg:end))
    allocate(pcf%m_livestemc_storage_to_fire(beg:end))
    allocate(pcf%m_deadstemc_storage_to_fire(beg:end))
    allocate(pcf%m_livecrootc_storage_to_fire(beg:end))
    allocate(pcf%m_deadcrootc_storage_to_fire(beg:end))
    allocate(pcf%m_leafc_xfer_to_fire(beg:end))
    allocate(pcf%m_frootc_xfer_to_fire(beg:end))
    allocate(pcf%m_livestemc_xfer_to_fire(beg:end))
    allocate(pcf%m_deadstemc_xfer_to_fire(beg:end))
    allocate(pcf%m_livecrootc_xfer_to_fire(beg:end))
    allocate(pcf%m_deadcrootc_xfer_to_fire(beg:end))
    allocate(pcf%m_livestemc_to_fire(beg:end))
    allocate(pcf%m_deadstemc_to_fire(beg:end))
    allocate(pcf%m_deadstemc_to_litter_fire(beg:end))
    allocate(pcf%m_livecrootc_to_fire(beg:end))
    allocate(pcf%m_deadcrootc_to_fire(beg:end))
    allocate(pcf%m_deadcrootc_to_litter_fire(beg:end))
    allocate(pcf%m_gresp_storage_to_fire(beg:end))
    allocate(pcf%m_gresp_xfer_to_fire(beg:end))
    allocate(pcf%leafc_xfer_to_leafc(beg:end))
    allocate(pcf%frootc_xfer_to_frootc(beg:end))
    allocate(pcf%livestemc_xfer_to_livestemc(beg:end))
    allocate(pcf%deadstemc_xfer_to_deadstemc(beg:end))
    allocate(pcf%livecrootc_xfer_to_livecrootc(beg:end))
    allocate(pcf%deadcrootc_xfer_to_deadcrootc(beg:end))
    allocate(pcf%leafc_to_litter(beg:end))
    allocate(pcf%frootc_to_litter(beg:end))
    allocate(pcf%leaf_mr(beg:end))
    allocate(pcf%froot_mr(beg:end))
    allocate(pcf%livestem_mr(beg:end))
    allocate(pcf%livecroot_mr(beg:end))
    allocate(pcf%leaf_curmr(beg:end))
    allocate(pcf%froot_curmr(beg:end))
    allocate(pcf%livestem_curmr(beg:end))
    allocate(pcf%livecroot_curmr(beg:end))
    allocate(pcf%leaf_xsmr(beg:end))
    allocate(pcf%froot_xsmr(beg:end))
    allocate(pcf%livestem_xsmr(beg:end))
    allocate(pcf%livecroot_xsmr(beg:end))
    allocate(pcf%psnsun_to_cpool(beg:end))
    allocate(pcf%psnshade_to_cpool(beg:end))
    allocate(pcf%cpool_to_xsmrpool(beg:end))
    allocate(pcf%cpool_to_leafc(beg:end))
    allocate(pcf%cpool_to_leafc_storage(beg:end))
    allocate(pcf%cpool_to_frootc(beg:end))
    allocate(pcf%cpool_to_frootc_storage(beg:end))
    allocate(pcf%cpool_to_livestemc(beg:end))
    allocate(pcf%cpool_to_livestemc_storage(beg:end))
    allocate(pcf%cpool_to_deadstemc(beg:end))
    allocate(pcf%cpool_to_deadstemc_storage(beg:end))
    allocate(pcf%cpool_to_livecrootc(beg:end))
    allocate(pcf%cpool_to_livecrootc_storage(beg:end))
    allocate(pcf%cpool_to_deadcrootc(beg:end))
    allocate(pcf%cpool_to_deadcrootc_storage(beg:end))
    allocate(pcf%cpool_to_gresp_storage(beg:end))
    allocate(pcf%cpool_leaf_gr(beg:end))
    allocate(pcf%cpool_leaf_storage_gr(beg:end))
    allocate(pcf%transfer_leaf_gr(beg:end))
    allocate(pcf%cpool_froot_gr(beg:end))
    allocate(pcf%cpool_froot_storage_gr(beg:end))
    allocate(pcf%transfer_froot_gr(beg:end))
    allocate(pcf%cpool_livestem_gr(beg:end))
    allocate(pcf%cpool_livestem_storage_gr(beg:end))
    allocate(pcf%transfer_livestem_gr(beg:end))
    allocate(pcf%cpool_deadstem_gr(beg:end))
    allocate(pcf%cpool_deadstem_storage_gr(beg:end))
    allocate(pcf%transfer_deadstem_gr(beg:end))
    allocate(pcf%cpool_livecroot_gr(beg:end))
    allocate(pcf%cpool_livecroot_storage_gr(beg:end))
    allocate(pcf%transfer_livecroot_gr(beg:end))
    allocate(pcf%cpool_deadcroot_gr(beg:end))
    allocate(pcf%cpool_deadcroot_storage_gr(beg:end))
    allocate(pcf%transfer_deadcroot_gr(beg:end))
    allocate(pcf%leafc_storage_to_xfer(beg:end))
    allocate(pcf%frootc_storage_to_xfer(beg:end))
    allocate(pcf%livestemc_storage_to_xfer(beg:end))
    allocate(pcf%deadstemc_storage_to_xfer(beg:end))
    allocate(pcf%livecrootc_storage_to_xfer(beg:end))
    allocate(pcf%deadcrootc_storage_to_xfer(beg:end))
    allocate(pcf%gresp_storage_to_xfer(beg:end))
    allocate(pcf%livestemc_to_deadstemc(beg:end))
    allocate(pcf%livecrootc_to_deadcrootc(beg:end))
    allocate(pcf%gpp(beg:end))
    allocate(pcf%mr(beg:end))
    allocate(pcf%current_gr(beg:end))
    allocate(pcf%transfer_gr(beg:end))
    allocate(pcf%storage_gr(beg:end))
    allocate(pcf%gr(beg:end))
    allocate(pcf%ar(beg:end))
    allocate(pcf%rr(beg:end))
    allocate(pcf%npp(beg:end))
    allocate(pcf%agnpp(beg:end))
    allocate(pcf%bgnpp(beg:end))
    allocate(pcf%litfall(beg:end))
    allocate(pcf%vegfire(beg:end))
    allocate(pcf%wood_harvestc(beg:end))
    allocate(pcf%pft_cinputs(beg:end))
    allocate(pcf%pft_coutputs(beg:end))
    allocate(pcf%pft_fire_closs(beg:end))
    pcf%psnsun(beg:end) = nan
    pcf%psnsha(beg:end) = nan
    pcf%fpsn(beg:end) = nan
    pcf%fco2(beg:end) = 0._r8
    pcf%m_leafc_to_litter(beg:end) = nan
    pcf%m_frootc_to_litter(beg:end) = nan
    pcf%m_leafc_storage_to_litter(beg:end) = nan
    pcf%m_frootc_storage_to_litter(beg:end) = nan
    pcf%m_livestemc_storage_to_litter(beg:end) = nan
    pcf%m_deadstemc_storage_to_litter(beg:end) = nan
    pcf%m_livecrootc_storage_to_litter(beg:end) = nan
    pcf%m_deadcrootc_storage_to_litter(beg:end) = nan
    pcf%m_leafc_xfer_to_litter(beg:end) = nan
    pcf%m_frootc_xfer_to_litter(beg:end) = nan
    pcf%m_livestemc_xfer_to_litter(beg:end) = nan
    pcf%m_deadstemc_xfer_to_litter(beg:end) = nan
    pcf%m_livecrootc_xfer_to_litter(beg:end) = nan
    pcf%m_deadcrootc_xfer_to_litter(beg:end) = nan
    pcf%m_livestemc_to_litter(beg:end) = nan
    pcf%m_deadstemc_to_litter(beg:end) = nan
    pcf%m_livecrootc_to_litter(beg:end) = nan
    pcf%m_deadcrootc_to_litter(beg:end) = nan
    pcf%m_gresp_storage_to_litter(beg:end) = nan
    pcf%m_gresp_xfer_to_litter(beg:end) = nan
    pcf%hrv_leafc_to_litter(beg:end) = nan
    pcf%hrv_leafc_storage_to_litter(beg:end) = nan
    pcf%hrv_leafc_xfer_to_litter(beg:end) = nan
    pcf%hrv_frootc_to_litter(beg:end) = nan
    pcf%hrv_frootc_storage_to_litter(beg:end) = nan
    pcf%hrv_frootc_xfer_to_litter(beg:end) = nan
    pcf%hrv_livestemc_to_litter(beg:end) = nan
    pcf%hrv_livestemc_storage_to_litter(beg:end) = nan
    pcf%hrv_livestemc_xfer_to_litter(beg:end) = nan
    pcf%hrv_deadstemc_to_prod10c(beg:end) = nan
    pcf%hrv_deadstemc_to_prod100c(beg:end) = nan
    pcf%hrv_deadstemc_storage_to_litter(beg:end) = nan
    pcf%hrv_deadstemc_xfer_to_litter(beg:end) = nan
    pcf%hrv_livecrootc_to_litter(beg:end) = nan
    pcf%hrv_livecrootc_storage_to_litter(beg:end) = nan
    pcf%hrv_livecrootc_xfer_to_litter(beg:end) = nan
    pcf%hrv_deadcrootc_to_litter(beg:end) = nan
    pcf%hrv_deadcrootc_storage_to_litter(beg:end) = nan
    pcf%hrv_deadcrootc_xfer_to_litter(beg:end) = nan
    pcf%hrv_gresp_storage_to_litter(beg:end) = nan
    pcf%hrv_gresp_xfer_to_litter(beg:end) = nan
    pcf%hrv_xsmrpool_to_atm(beg:end) = nan
    pcf%m_leafc_to_fire(beg:end) = nan
    pcf%m_frootc_to_fire(beg:end) = nan
    pcf%m_leafc_storage_to_fire(beg:end) = nan
    pcf%m_frootc_storage_to_fire(beg:end) = nan
    pcf%m_livestemc_storage_to_fire(beg:end) = nan
    pcf%m_deadstemc_storage_to_fire(beg:end) = nan
    pcf%m_livecrootc_storage_to_fire(beg:end) = nan
    pcf%m_deadcrootc_storage_to_fire(beg:end) = nan
    pcf%m_leafc_xfer_to_fire(beg:end) = nan
    pcf%m_frootc_xfer_to_fire(beg:end) = nan
    pcf%m_livestemc_xfer_to_fire(beg:end) = nan
    pcf%m_deadstemc_xfer_to_fire(beg:end) = nan
    pcf%m_livecrootc_xfer_to_fire(beg:end) = nan
    pcf%m_deadcrootc_xfer_to_fire(beg:end) = nan
    pcf%m_livestemc_to_fire(beg:end) = nan
    pcf%m_deadstemc_to_fire(beg:end) = nan
    pcf%m_deadstemc_to_litter_fire(beg:end) = nan
    pcf%m_livecrootc_to_fire(beg:end) = nan
    pcf%m_deadcrootc_to_fire(beg:end) = nan
    pcf%m_deadcrootc_to_litter_fire(beg:end) = nan
    pcf%m_gresp_storage_to_fire(beg:end) = nan
    pcf%m_gresp_xfer_to_fire(beg:end) = nan
    pcf%leafc_xfer_to_leafc(beg:end) = nan
    pcf%frootc_xfer_to_frootc(beg:end) = nan
    pcf%livestemc_xfer_to_livestemc(beg:end) = nan
    pcf%deadstemc_xfer_to_deadstemc(beg:end) = nan
    pcf%livecrootc_xfer_to_livecrootc(beg:end) = nan
    pcf%deadcrootc_xfer_to_deadcrootc(beg:end) = nan
    pcf%leafc_to_litter(beg:end) = nan
    pcf%frootc_to_litter(beg:end) = nan
    pcf%leaf_mr(beg:end) = nan
    pcf%froot_mr(beg:end) = nan
    pcf%livestem_mr(beg:end) = nan
    pcf%livecroot_mr(beg:end) = nan
    pcf%leaf_curmr(beg:end) = nan
    pcf%froot_curmr(beg:end) = nan
    pcf%livestem_curmr(beg:end) = nan
    pcf%livecroot_curmr(beg:end) = nan
    pcf%leaf_xsmr(beg:end) = nan
    pcf%froot_xsmr(beg:end) = nan
    pcf%livestem_xsmr(beg:end) = nan
    pcf%livecroot_xsmr(beg:end) = nan
    pcf%psnsun_to_cpool(beg:end) = nan
    pcf%psnshade_to_cpool(beg:end) = nan
    pcf%cpool_to_xsmrpool(beg:end) = nan
    pcf%cpool_to_leafc(beg:end) = nan
    pcf%cpool_to_leafc_storage(beg:end) = nan
    pcf%cpool_to_frootc(beg:end) = nan
    pcf%cpool_to_frootc_storage(beg:end) = nan
    pcf%cpool_to_livestemc(beg:end) = nan
    pcf%cpool_to_livestemc_storage(beg:end) = nan
    pcf%cpool_to_deadstemc(beg:end) = nan
    pcf%cpool_to_deadstemc_storage(beg:end) = nan
    pcf%cpool_to_livecrootc(beg:end) = nan
    pcf%cpool_to_livecrootc_storage(beg:end) = nan
    pcf%cpool_to_deadcrootc(beg:end) = nan
    pcf%cpool_to_deadcrootc_storage(beg:end) = nan
    pcf%cpool_to_gresp_storage(beg:end) = nan
    pcf%cpool_leaf_gr(beg:end) = nan
    pcf%cpool_leaf_storage_gr(beg:end) = nan
    pcf%transfer_leaf_gr(beg:end) = nan
    pcf%cpool_froot_gr(beg:end) = nan
    pcf%cpool_froot_storage_gr(beg:end) = nan
    pcf%transfer_froot_gr(beg:end) = nan
    pcf%cpool_livestem_gr(beg:end) = nan
    pcf%cpool_livestem_storage_gr(beg:end) = nan
    pcf%transfer_livestem_gr(beg:end) = nan
    pcf%cpool_deadstem_gr(beg:end) = nan
    pcf%cpool_deadstem_storage_gr(beg:end) = nan
    pcf%transfer_deadstem_gr(beg:end) = nan
    pcf%cpool_livecroot_gr(beg:end) = nan
    pcf%cpool_livecroot_storage_gr(beg:end) = nan
    pcf%transfer_livecroot_gr(beg:end) = nan
    pcf%cpool_deadcroot_gr(beg:end) = nan
    pcf%cpool_deadcroot_storage_gr(beg:end) = nan
    pcf%transfer_deadcroot_gr(beg:end) = nan
    pcf%leafc_storage_to_xfer(beg:end) = nan
    pcf%frootc_storage_to_xfer(beg:end) = nan
    pcf%livestemc_storage_to_xfer(beg:end) = nan
    pcf%deadstemc_storage_to_xfer(beg:end) = nan
    pcf%livecrootc_storage_to_xfer(beg:end) = nan
    pcf%deadcrootc_storage_to_xfer(beg:end) = nan
    pcf%gresp_storage_to_xfer(beg:end) = nan
    pcf%livestemc_to_deadstemc(beg:end) = nan
    pcf%livecrootc_to_deadcrootc(beg:end) = nan
    pcf%gpp(beg:end) = nan
    pcf%mr(beg:end) = nan
    pcf%current_gr(beg:end) = nan
    pcf%transfer_gr(beg:end) = nan
    pcf%storage_gr(beg:end) = nan
    pcf%gr(beg:end) = nan
    pcf%ar(beg:end) = nan
    pcf%rr(beg:end) = nan
    pcf%npp(beg:end) = nan
    pcf%agnpp(beg:end) = nan
    pcf%bgnpp(beg:end) = nan
    pcf%litfall(beg:end) = nan
    pcf%vegfire(beg:end) = nan
    pcf%wood_harvestc(beg:end) = nan
    pcf%pft_cinputs(beg:end) = nan
    pcf%pft_coutputs(beg:end) = nan
    pcf%pft_fire_closs(beg:end) = nan
  end subroutine init_pft_cflux_type
  subroutine init_pft_nflux_type(beg, end, pnf)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_nflux_type), intent(inout) :: pnf
    allocate(pnf%m_leafn_to_litter(beg:end))
    allocate(pnf%m_frootn_to_litter(beg:end))
    allocate(pnf%m_leafn_storage_to_litter(beg:end))
    allocate(pnf%m_frootn_storage_to_litter(beg:end))
    allocate(pnf%m_livestemn_storage_to_litter(beg:end))
    allocate(pnf%m_deadstemn_storage_to_litter(beg:end))
    allocate(pnf%m_livecrootn_storage_to_litter(beg:end))
    allocate(pnf%m_deadcrootn_storage_to_litter(beg:end))
    allocate(pnf%m_leafn_xfer_to_litter(beg:end))
    allocate(pnf%m_frootn_xfer_to_litter(beg:end))
    allocate(pnf%m_livestemn_xfer_to_litter(beg:end))
    allocate(pnf%m_deadstemn_xfer_to_litter(beg:end))
    allocate(pnf%m_livecrootn_xfer_to_litter(beg:end))
    allocate(pnf%m_deadcrootn_xfer_to_litter(beg:end))
    allocate(pnf%m_livestemn_to_litter(beg:end))
    allocate(pnf%m_deadstemn_to_litter(beg:end))
    allocate(pnf%m_livecrootn_to_litter(beg:end))
    allocate(pnf%m_deadcrootn_to_litter(beg:end))
    allocate(pnf%m_retransn_to_litter(beg:end))
    allocate(pnf%hrv_leafn_to_litter(beg:end))
    allocate(pnf%hrv_frootn_to_litter(beg:end))
    allocate(pnf%hrv_leafn_storage_to_litter(beg:end))
    allocate(pnf%hrv_frootn_storage_to_litter(beg:end))
    allocate(pnf%hrv_livestemn_storage_to_litter(beg:end))
    allocate(pnf%hrv_deadstemn_storage_to_litter(beg:end))
    allocate(pnf%hrv_livecrootn_storage_to_litter(beg:end))
    allocate(pnf%hrv_deadcrootn_storage_to_litter(beg:end))
    allocate(pnf%hrv_leafn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_frootn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_livestemn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_deadstemn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_livecrootn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_deadcrootn_xfer_to_litter(beg:end))
    allocate(pnf%hrv_livestemn_to_litter(beg:end))
    allocate(pnf%hrv_deadstemn_to_prod10n(beg:end))
    allocate(pnf%hrv_deadstemn_to_prod100n(beg:end))
    allocate(pnf%hrv_livecrootn_to_litter(beg:end))
    allocate(pnf%hrv_deadcrootn_to_litter(beg:end))
    allocate(pnf%hrv_retransn_to_litter(beg:end))
    allocate(pnf%m_leafn_to_fire(beg:end))
    allocate(pnf%m_frootn_to_fire(beg:end))
    allocate(pnf%m_leafn_storage_to_fire(beg:end))
    allocate(pnf%m_frootn_storage_to_fire(beg:end))
    allocate(pnf%m_livestemn_storage_to_fire(beg:end))
    allocate(pnf%m_deadstemn_storage_to_fire(beg:end))
    allocate(pnf%m_livecrootn_storage_to_fire(beg:end))
    allocate(pnf%m_deadcrootn_storage_to_fire(beg:end))
    allocate(pnf%m_leafn_xfer_to_fire(beg:end))
    allocate(pnf%m_frootn_xfer_to_fire(beg:end))
    allocate(pnf%m_livestemn_xfer_to_fire(beg:end))
    allocate(pnf%m_deadstemn_xfer_to_fire(beg:end))
    allocate(pnf%m_livecrootn_xfer_to_fire(beg:end))
    allocate(pnf%m_deadcrootn_xfer_to_fire(beg:end))
    allocate(pnf%m_livestemn_to_fire(beg:end))
    allocate(pnf%m_deadstemn_to_fire(beg:end))
    allocate(pnf%m_deadstemn_to_litter_fire(beg:end))
    allocate(pnf%m_livecrootn_to_fire(beg:end))
    allocate(pnf%m_deadcrootn_to_fire(beg:end))
    allocate(pnf%m_deadcrootn_to_litter_fire(beg:end))
    allocate(pnf%m_retransn_to_fire(beg:end))
    allocate(pnf%leafn_xfer_to_leafn(beg:end))
    allocate(pnf%frootn_xfer_to_frootn(beg:end))
    allocate(pnf%livestemn_xfer_to_livestemn(beg:end))
    allocate(pnf%deadstemn_xfer_to_deadstemn(beg:end))
    allocate(pnf%livecrootn_xfer_to_livecrootn(beg:end))
    allocate(pnf%deadcrootn_xfer_to_deadcrootn(beg:end))
    allocate(pnf%leafn_to_litter(beg:end))
    allocate(pnf%leafn_to_retransn(beg:end))
    allocate(pnf%frootn_to_litter(beg:end))
    allocate(pnf%retransn_to_npool(beg:end))
    allocate(pnf%sminn_to_npool(beg:end))
    allocate(pnf%npool_to_leafn(beg:end))
    allocate(pnf%npool_to_leafn_storage(beg:end))
    allocate(pnf%npool_to_frootn(beg:end))
    allocate(pnf%npool_to_frootn_storage(beg:end))
    allocate(pnf%npool_to_livestemn(beg:end))
    allocate(pnf%npool_to_livestemn_storage(beg:end))
    allocate(pnf%npool_to_deadstemn(beg:end))
    allocate(pnf%npool_to_deadstemn_storage(beg:end))
    allocate(pnf%npool_to_livecrootn(beg:end))
    allocate(pnf%npool_to_livecrootn_storage(beg:end))
    allocate(pnf%npool_to_deadcrootn(beg:end))
    allocate(pnf%npool_to_deadcrootn_storage(beg:end))
    allocate(pnf%leafn_storage_to_xfer(beg:end))
    allocate(pnf%frootn_storage_to_xfer(beg:end))
    allocate(pnf%livestemn_storage_to_xfer(beg:end))
    allocate(pnf%deadstemn_storage_to_xfer(beg:end))
    allocate(pnf%livecrootn_storage_to_xfer(beg:end))
    allocate(pnf%deadcrootn_storage_to_xfer(beg:end))
    allocate(pnf%livestemn_to_deadstemn(beg:end))
    allocate(pnf%livestemn_to_retransn(beg:end))
    allocate(pnf%livecrootn_to_deadcrootn(beg:end))
    allocate(pnf%livecrootn_to_retransn(beg:end))
    allocate(pnf%ndeploy(beg:end))
    allocate(pnf%pft_ninputs(beg:end))
    allocate(pnf%pft_noutputs(beg:end))
    allocate(pnf%wood_harvestn(beg:end))
    allocate(pnf%pft_fire_nloss(beg:end))
    pnf%m_leafn_to_litter(beg:end) = nan
    pnf%m_frootn_to_litter(beg:end) = nan
    pnf%m_leafn_storage_to_litter(beg:end) = nan
    pnf%m_frootn_storage_to_litter(beg:end) = nan
    pnf%m_livestemn_storage_to_litter(beg:end) = nan
    pnf%m_deadstemn_storage_to_litter(beg:end) = nan
    pnf%m_livecrootn_storage_to_litter(beg:end) = nan
    pnf%m_deadcrootn_storage_to_litter(beg:end) = nan
    pnf%m_leafn_xfer_to_litter(beg:end) = nan
    pnf%m_frootn_xfer_to_litter(beg:end) = nan
    pnf%m_livestemn_xfer_to_litter(beg:end) = nan
    pnf%m_deadstemn_xfer_to_litter(beg:end) = nan
    pnf%m_livecrootn_xfer_to_litter(beg:end) = nan
    pnf%m_deadcrootn_xfer_to_litter(beg:end) = nan
    pnf%m_livestemn_to_litter(beg:end) = nan
    pnf%m_deadstemn_to_litter(beg:end) = nan
    pnf%m_livecrootn_to_litter(beg:end) = nan
    pnf%m_deadcrootn_to_litter(beg:end) = nan
    pnf%m_retransn_to_litter(beg:end) = nan
    pnf%hrv_leafn_to_litter(beg:end) = nan
    pnf%hrv_frootn_to_litter(beg:end) = nan
    pnf%hrv_leafn_storage_to_litter(beg:end) = nan
    pnf%hrv_frootn_storage_to_litter(beg:end) = nan
    pnf%hrv_livestemn_storage_to_litter(beg:end) = nan
    pnf%hrv_deadstemn_storage_to_litter(beg:end) = nan
    pnf%hrv_livecrootn_storage_to_litter(beg:end) = nan
    pnf%hrv_deadcrootn_storage_to_litter(beg:end) = nan
    pnf%hrv_leafn_xfer_to_litter(beg:end) = nan
    pnf%hrv_frootn_xfer_to_litter(beg:end) = nan
    pnf%hrv_livestemn_xfer_to_litter(beg:end) = nan
    pnf%hrv_deadstemn_xfer_to_litter(beg:end) = nan
    pnf%hrv_livecrootn_xfer_to_litter(beg:end) = nan
    pnf%hrv_deadcrootn_xfer_to_litter(beg:end) = nan
    pnf%hrv_livestemn_to_litter(beg:end) = nan
    pnf%hrv_deadstemn_to_prod10n(beg:end) = nan
    pnf%hrv_deadstemn_to_prod100n(beg:end) = nan
    pnf%hrv_livecrootn_to_litter(beg:end) = nan
    pnf%hrv_deadcrootn_to_litter(beg:end) = nan
    pnf%hrv_retransn_to_litter(beg:end) = nan
    pnf%m_leafn_to_fire(beg:end) = nan
    pnf%m_frootn_to_fire(beg:end) = nan
    pnf%m_leafn_storage_to_fire(beg:end) = nan
    pnf%m_frootn_storage_to_fire(beg:end) = nan
    pnf%m_livestemn_storage_to_fire(beg:end) = nan
    pnf%m_deadstemn_storage_to_fire(beg:end) = nan
    pnf%m_livecrootn_storage_to_fire(beg:end) = nan
    pnf%m_deadcrootn_storage_to_fire(beg:end) = nan
    pnf%m_leafn_xfer_to_fire(beg:end) = nan
    pnf%m_frootn_xfer_to_fire(beg:end) = nan
    pnf%m_livestemn_xfer_to_fire(beg:end) = nan
    pnf%m_deadstemn_xfer_to_fire(beg:end) = nan
    pnf%m_livecrootn_xfer_to_fire(beg:end) = nan
    pnf%m_deadcrootn_xfer_to_fire(beg:end) = nan
    pnf%m_livestemn_to_fire(beg:end) = nan
    pnf%m_deadstemn_to_fire(beg:end) = nan
    pnf%m_deadstemn_to_litter_fire(beg:end) = nan
    pnf%m_livecrootn_to_fire(beg:end) = nan
    pnf%m_deadcrootn_to_fire(beg:end) = nan
    pnf%m_deadcrootn_to_litter_fire(beg:end) = nan
    pnf%m_retransn_to_fire(beg:end) = nan
    pnf%leafn_xfer_to_leafn(beg:end) = nan
    pnf%frootn_xfer_to_frootn(beg:end) = nan
    pnf%livestemn_xfer_to_livestemn(beg:end) = nan
    pnf%deadstemn_xfer_to_deadstemn(beg:end) = nan
    pnf%livecrootn_xfer_to_livecrootn(beg:end) = nan
    pnf%deadcrootn_xfer_to_deadcrootn(beg:end) = nan
    pnf%leafn_to_litter(beg:end) = nan
    pnf%leafn_to_retransn(beg:end) = nan
    pnf%frootn_to_litter(beg:end) = nan
    pnf%retransn_to_npool(beg:end) = nan
    pnf%sminn_to_npool(beg:end) = nan
    pnf%npool_to_leafn(beg:end) = nan
    pnf%npool_to_leafn_storage(beg:end) = nan
    pnf%npool_to_frootn(beg:end) = nan
    pnf%npool_to_frootn_storage(beg:end) = nan
    pnf%npool_to_livestemn(beg:end) = nan
    pnf%npool_to_livestemn_storage(beg:end) = nan
    pnf%npool_to_deadstemn(beg:end) = nan
    pnf%npool_to_deadstemn_storage(beg:end) = nan
    pnf%npool_to_livecrootn(beg:end) = nan
    pnf%npool_to_livecrootn_storage(beg:end) = nan
    pnf%npool_to_deadcrootn(beg:end) = nan
    pnf%npool_to_deadcrootn_storage(beg:end) = nan
    pnf%leafn_storage_to_xfer(beg:end) = nan
    pnf%frootn_storage_to_xfer(beg:end) = nan
    pnf%livestemn_storage_to_xfer(beg:end) = nan
    pnf%deadstemn_storage_to_xfer(beg:end) = nan
    pnf%livecrootn_storage_to_xfer(beg:end) = nan
    pnf%deadcrootn_storage_to_xfer(beg:end) = nan
    pnf%livestemn_to_deadstemn(beg:end) = nan
    pnf%livestemn_to_retransn(beg:end) = nan
    pnf%livecrootn_to_deadcrootn(beg:end) = nan
    pnf%livecrootn_to_retransn(beg:end) = nan
    pnf%ndeploy(beg:end) = nan
    pnf%pft_ninputs(beg:end) = nan
    pnf%pft_noutputs(beg:end) = nan
    pnf%wood_harvestn(beg:end) = nan
    pnf%pft_fire_nloss(beg:end) = nan
  end subroutine init_pft_nflux_type
  subroutine init_pft_vflux_type(beg, end, pvf)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (pft_vflux_type), intent(inout) :: pvf
    allocate(pvf%vocflx_tot(beg:end))
    allocate(pvf%vocflx(beg:end,1:nvoc))
    allocate(pvf%vocflx_1(beg:end))
    allocate(pvf%vocflx_2(beg:end))
    allocate(pvf%vocflx_3(beg:end))
    allocate(pvf%vocflx_4(beg:end))
    allocate(pvf%vocflx_5(beg:end))
    allocate(pvf%Eopt_out(beg:end))
    allocate(pvf%topt_out(beg:end))
    allocate(pvf%alpha_out(beg:end))
    allocate(pvf%cp_out(beg:end))
    allocate(pvf%para_out(beg:end))
    allocate(pvf%par24a_out(beg:end))
    allocate(pvf%par240a_out(beg:end))
    allocate(pvf%paru_out(beg:end))
    allocate(pvf%par24u_out(beg:end))
    allocate(pvf%par240u_out(beg:end))
    allocate(pvf%gamma_out(beg:end))
    allocate(pvf%gammaL_out(beg:end))
    allocate(pvf%gammaT_out(beg:end))
    allocate(pvf%gammaP_out(beg:end))
    allocate(pvf%gammaA_out(beg:end))
    allocate(pvf%gammaS_out(beg:end))
    pvf%vocflx_tot(beg:end) = spval
    pvf%vocflx(beg:end,1:nvoc) = spval
    pvf%vocflx_1(beg:end) = spval
    pvf%vocflx_2(beg:end) = spval
    pvf%vocflx_3(beg:end) = spval
    pvf%vocflx_4(beg:end) = spval
    pvf%vocflx_5(beg:end) = spval
    pvf%Eopt_out(beg:end) = nan
    pvf%topt_out(beg:end) = nan
    pvf%alpha_out(beg:end) = nan
    pvf%cp_out(beg:end) = nan
    pvf%para_out(beg:end) = nan
    pvf%par24a_out(beg:end) = nan
    pvf%par240a_out(beg:end) = nan
    pvf%paru_out(beg:end) = nan
    pvf%par24u_out(beg:end) = nan
    pvf%par240u_out(beg:end) = nan
    pvf%gamma_out(beg:end) = nan
    pvf%gammaL_out(beg:end) = nan
    pvf%gammaT_out(beg:end) = nan
    pvf%gammaP_out(beg:end) = nan
    pvf%gammaA_out(beg:end) = nan
    pvf%gammaS_out(beg:end) = nan
  end subroutine init_pft_vflux_type
  subroutine init_pft_dflux_type(beg, end, pdf)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_dflux_type), intent(inout):: pdf
    allocate(pdf%flx_mss_vrt_dst(beg:end,1:ndst))
    allocate(pdf%flx_mss_vrt_dst_tot(beg:end))
    allocate(pdf%vlc_trb(beg:end,1:ndst))
    allocate(pdf%vlc_trb_1(beg:end))
    allocate(pdf%vlc_trb_2(beg:end))
    allocate(pdf%vlc_trb_3(beg:end))
    allocate(pdf%vlc_trb_4(beg:end))
    pdf%flx_mss_vrt_dst(beg:end,1:ndst) = nan
    pdf%flx_mss_vrt_dst_tot(beg:end) = nan
    pdf%vlc_trb(beg:end,1:ndst) = nan
    pdf%vlc_trb_1(beg:end) = nan
    pdf%vlc_trb_2(beg:end) = nan
    pdf%vlc_trb_3(beg:end) = nan
    pdf%vlc_trb_4(beg:end) = nan
  end subroutine init_pft_dflux_type
  subroutine init_pft_depvd_type(beg, end, pdd)
    implicit none
    integer, intent(in) :: beg, end
    type (pft_depvd_type), intent(inout):: pdd
    integer :: i
  end subroutine init_pft_depvd_type
  subroutine init_column_pstate_type(beg, end, cps)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (column_pstate_type), intent(inout):: cps
    allocate(cps%snl(beg:end))
    allocate(cps%isoicol(beg:end))
    allocate(cps%bsw(beg:end,nlevgrnd))
    allocate(cps%watsat(beg:end,nlevgrnd))
    allocate(cps%watfc(beg:end,nlevgrnd))
    allocate(cps%watdry(beg:end,nlevgrnd))
    allocate(cps%watopt(beg:end,nlevgrnd))
    allocate(cps%hksat(beg:end,nlevgrnd))
    allocate(cps%sucsat(beg:end,nlevgrnd))
    allocate(cps%csol(beg:end,nlevgrnd))
    allocate(cps%tkmg(beg:end,nlevgrnd))
    allocate(cps%tkdry(beg:end,nlevgrnd))
    allocate(cps%tksatu(beg:end,nlevgrnd))
    allocate(cps%smpmin(beg:end))
    allocate(cps%hkdepth(beg:end))
    allocate(cps%wtfact(beg:end))
    allocate(cps%fracice(beg:end,nlevgrnd))
    allocate(cps%gwc_thr(beg:end))
    allocate(cps%mss_frc_cly_vld(beg:end))
    allocate(cps%mbl_bsn_fct(beg:end))
    allocate(cps%do_capsnow(beg:end))
    allocate(cps%snowdp(beg:end))
    allocate(cps%frac_sno (beg:end))
    allocate(cps%zi(beg:end,-nlevsno+0:nlevgrnd))
    allocate(cps%dz(beg:end,-nlevsno+1:nlevgrnd))
    allocate(cps%z (beg:end,-nlevsno+1:nlevgrnd))
    allocate(cps%frac_iceold(beg:end,-nlevsno+1:nlevgrnd))
    allocate(cps%imelt(beg:end,-nlevsno+1:nlevgrnd))
    allocate(cps%eff_porosity(beg:end,nlevgrnd))
    allocate(cps%emg(beg:end))
    allocate(cps%z0mg(beg:end))
    allocate(cps%z0hg(beg:end))
    allocate(cps%z0qg(beg:end))
    allocate(cps%htvp(beg:end))
    allocate(cps%beta(beg:end))
    allocate(cps%zii(beg:end))
    allocate(cps%albgrd(beg:end,numrad))
    allocate(cps%albgri(beg:end,numrad))
    allocate(cps%rootr_column(beg:end,nlevgrnd))
    allocate(cps%rootfr_road_perv(beg:end,nlevgrnd))
    allocate(cps%rootr_road_perv(beg:end,nlevgrnd))
    allocate(cps%wf(beg:end))
    allocate(cps%max_dayl(beg:end))
    allocate(cps%bsw2(beg:end,nlevgrnd))
    allocate(cps%psisat(beg:end,nlevgrnd))
    allocate(cps%vwcsat(beg:end,nlevgrnd))
    allocate(cps%soilpsi(beg:end,nlevgrnd))
    allocate(cps%decl(beg:end))
    allocate(cps%coszen(beg:end))
    allocate(cps%fpi(beg:end))
    allocate(cps%fpg(beg:end))
    allocate(cps%annsum_counter(beg:end))
    allocate(cps%cannsum_npp(beg:end))
    allocate(cps%cannavg_t2m(beg:end))
    allocate(cps%me(beg:end))
    allocate(cps%fire_prob(beg:end))
    allocate(cps%mean_fire_prob(beg:end))
    allocate(cps%fireseasonl(beg:end))
    allocate(cps%farea_burned(beg:end))
    allocate(cps%ann_farea_burned(beg:end))
    allocate(cps%albsnd_hst(beg:end,numrad))
    allocate(cps%albsni_hst(beg:end,numrad))
    allocate(cps%albsod(beg:end,numrad))
    allocate(cps%albsoi(beg:end,numrad))
    allocate(cps%flx_absdv(beg:end,-nlevsno+1:1))
    allocate(cps%flx_absdn(beg:end,-nlevsno+1:1))
    allocate(cps%flx_absiv(beg:end,-nlevsno+1:1))
    allocate(cps%flx_absin(beg:end,-nlevsno+1:1))
    allocate(cps%snw_rds(beg:end,-nlevsno+1:0))
    allocate(cps%snw_rds_top(beg:end))
    allocate(cps%sno_liq_top(beg:end))
    allocate(cps%mss_bcpho(beg:end,-nlevsno+1:0))
    allocate(cps%mss_bcphi(beg:end,-nlevsno+1:0))
    allocate(cps%mss_bctot(beg:end,-nlevsno+1:0))
    allocate(cps%mss_bc_col(beg:end))
    allocate(cps%mss_bc_top(beg:end))
    allocate(cps%mss_ocpho(beg:end,-nlevsno+1:0))
    allocate(cps%mss_ocphi(beg:end,-nlevsno+1:0))
    allocate(cps%mss_octot(beg:end,-nlevsno+1:0))
    allocate(cps%mss_oc_col(beg:end))
    allocate(cps%mss_oc_top(beg:end))
    allocate(cps%mss_dst1(beg:end,-nlevsno+1:0))
    allocate(cps%mss_dst2(beg:end,-nlevsno+1:0))
    allocate(cps%mss_dst3(beg:end,-nlevsno+1:0))
    allocate(cps%mss_dst4(beg:end,-nlevsno+1:0))
    allocate(cps%mss_dsttot(beg:end,-nlevsno+1:0))
    allocate(cps%mss_dst_col(beg:end))
    allocate(cps%mss_dst_top(beg:end))
    allocate(cps%h2osno_top(beg:end))
    allocate(cps%mss_cnc_bcphi(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_bcpho(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_ocphi(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_ocpho(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_dst1(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_dst2(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_dst3(beg:end,-nlevsno+1:0))
    allocate(cps%mss_cnc_dst4(beg:end,-nlevsno+1:0))
    allocate(cps%albgrd_pur(beg:end,numrad))
    allocate(cps%albgri_pur(beg:end,numrad))
    allocate(cps%albgrd_bc(beg:end,numrad))
    allocate(cps%albgri_bc(beg:end,numrad))
    allocate(cps%albgrd_oc(beg:end,numrad))
    allocate(cps%albgri_oc(beg:end,numrad))
    allocate(cps%albgrd_dst(beg:end,numrad))
    allocate(cps%albgri_dst(beg:end,numrad))
    allocate(cps%dTdz_top(beg:end))
    allocate(cps%snot_top(beg:end))
    cps%isoicol(beg:end) = bigint
    cps%bsw(beg:end,1:nlevgrnd) = nan
    cps%watsat(beg:end,1:nlevgrnd) = nan
    cps%watfc(beg:end,1:nlevgrnd) = nan
    cps%watdry(beg:end,1:nlevgrnd) = nan
    cps%watopt(beg:end,1:nlevgrnd) = nan
    cps%hksat(beg:end,1:nlevgrnd) = nan
    cps%sucsat(beg:end,1:nlevgrnd) = nan
    cps%csol(beg:end,1:nlevgrnd) = nan
    cps%tkmg(beg:end,1:nlevgrnd) = nan
    cps%tkdry(beg:end,1:nlevgrnd) = nan
    cps%tksatu(beg:end,1:nlevgrnd) = nan
    cps%smpmin(beg:end) = nan
    cps%hkdepth(beg:end) = nan
    cps%wtfact(beg:end) = nan
    cps%fracice(beg:end,1:nlevgrnd) = nan
    cps%gwc_thr(beg:end) = nan
    cps%mss_frc_cly_vld(beg:end) = nan
    cps%mbl_bsn_fct(beg:end) = nan
    cps%do_capsnow (beg:end)= .false.
    cps%snowdp(beg:end) = nan
    cps%frac_sno(beg:end) = nan
    cps%zi(beg:end,-nlevsno+0:nlevgrnd) = nan
    cps%dz(beg:end,-nlevsno+1:nlevgrnd) = nan
    cps%z (beg:end,-nlevsno+1:nlevgrnd) = nan
    cps%frac_iceold(beg:end,-nlevsno+1:nlevgrnd) = spval
    cps%imelt(beg:end,-nlevsno+1:nlevgrnd) = bigint
    cps%eff_porosity(beg:end,1:nlevgrnd) = spval
    cps%emg(beg:end) = nan
    cps%z0mg(beg:end) = nan
    cps%z0hg(beg:end) = nan
    cps%z0qg(beg:end) = nan
    cps%htvp(beg:end) = nan
    cps%beta(beg:end) = nan
    cps%zii(beg:end) = nan
    cps%albgrd(beg:end,:numrad) = nan
    cps%albgri(beg:end,:numrad) = nan
    cps%rootr_column(beg:end,1:nlevgrnd) = spval
    cps%rootfr_road_perv(beg:end,1:nlevurb) = nan
    cps%rootr_road_perv(beg:end,1:nlevurb) = nan
    cps%wf(beg:end) = nan
    cps%bsw2(beg:end,1:nlevgrnd) = nan
    cps%psisat(beg:end,1:nlevgrnd) = nan
    cps%vwcsat(beg:end,1:nlevgrnd) = nan
    cps%soilpsi(beg:end,1:nlevgrnd) = spval
    cps%decl(beg:end) = nan
    cps%coszen(beg:end) = nan
    cps%fpi(beg:end) = nan
    cps%fpg(beg:end) = nan
    cps%annsum_counter(beg:end) = nan
    cps%cannsum_npp(beg:end) = nan
    cps%cannavg_t2m(beg:end) = nan
    cps%me(beg:end) = nan
    cps%fire_prob(beg:end) = nan
    cps%mean_fire_prob(beg:end) = nan
    cps%fireseasonl(beg:end) = nan
    cps%farea_burned(beg:end) = nan
    cps%ann_farea_burned(beg:end) = nan
    cps%albsnd_hst(beg:end,:numrad) = spval
    cps%albsni_hst(beg:end,:numrad) = spval
    cps%albsod(beg:end,:numrad) = nan
    cps%albsoi(beg:end,:numrad) = nan
    cps%flx_absdv(beg:end,-nlevsno+1:1) = spval
    cps%flx_absdn(beg:end,-nlevsno+1:1) = spval
    cps%flx_absiv(beg:end,-nlevsno+1:1) = spval
    cps%flx_absin(beg:end,-nlevsno+1:1) = spval
    cps%snw_rds(beg:end,-nlevsno+1:0) = nan
    cps%snw_rds_top(beg:end) = nan
    cps%sno_liq_top(beg:end) = nan
    cps%mss_bcpho(beg:end,-nlevsno+1:0) = nan
    cps%mss_bcphi(beg:end,-nlevsno+1:0) = nan
    cps%mss_bctot(beg:end,-nlevsno+1:0) = nan
    cps%mss_bc_col(beg:end) = nan
    cps%mss_bc_top(beg:end) = nan
    cps%mss_ocpho(beg:end,-nlevsno+1:0) = nan
    cps%mss_ocphi(beg:end,-nlevsno+1:0) = nan
    cps%mss_octot(beg:end,-nlevsno+1:0) = nan
    cps%mss_oc_col(beg:end) = nan
    cps%mss_oc_top(beg:end) = nan
    cps%mss_dst1(beg:end,-nlevsno+1:0) = nan
    cps%mss_dst2(beg:end,-nlevsno+1:0) = nan
    cps%mss_dst3(beg:end,-nlevsno+1:0) = nan
    cps%mss_dst4(beg:end,-nlevsno+1:0) = nan
    cps%mss_dsttot(beg:end,-nlevsno+1:0) = nan
    cps%mss_dst_col(beg:end) = nan
    cps%mss_dst_top(beg:end) = nan
    cps%h2osno_top(beg:end) = nan
    cps%mss_cnc_bcphi(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_bcpho(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_ocphi(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_ocpho(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_dst1(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_dst2(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_dst3(beg:end,-nlevsno+1:0) = nan
    cps%mss_cnc_dst4(beg:end,-nlevsno+1:0) = nan
    cps%albgrd_pur(beg:end,:numrad) = nan
    cps%albgri_pur(beg:end,:numrad) = nan
    cps%albgrd_bc(beg:end,:numrad) = nan
    cps%albgri_bc(beg:end,:numrad) = nan
    cps%albgrd_oc(beg:end,:numrad) = nan
    cps%albgri_oc(beg:end,:numrad) = nan
    cps%albgrd_dst(beg:end,:numrad) = nan
    cps%albgri_dst(beg:end,:numrad) = nan
    cps%dTdz_top(beg:end) = nan
    cps%snot_top(beg:end) = nan
  end subroutine init_column_pstate_type
  subroutine init_column_estate_type(beg, end, ces)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (column_estate_type), intent(inout):: ces
    allocate(ces%t_grnd(beg:end))
    allocate(ces%t_grnd_u(beg:end))
    allocate(ces%t_grnd_r(beg:end))
    allocate(ces%dt_grnd(beg:end))
    allocate(ces%t_soisno(beg:end,-nlevsno+1:nlevgrnd))
    allocate(ces%t_soi_10cm(beg:end))
    allocate(ces%t_lake(beg:end,1:nlevlak))
    allocate(ces%tssbef(beg:end,-nlevsno+1:nlevgrnd))
    allocate(ces%thv(beg:end))
    allocate(ces%hc_soi(beg:end))
    allocate(ces%hc_soisno(beg:end))
    ces%t_grnd(beg:end) = nan
    ces%t_grnd_u(beg:end) = nan
    ces%t_grnd_r(beg:end) = nan
    ces%dt_grnd(beg:end) = nan
    ces%t_soisno(beg:end,-nlevsno+1:nlevgrnd) = spval
    ces%t_soi_10cm(beg:end) = spval
    ces%t_lake(beg:end,1:nlevlak) = nan
    ces%tssbef(beg:end,-nlevsno+1:nlevgrnd) = nan
    ces%thv(beg:end) = nan
    ces%hc_soi(beg:end) = nan
    ces%hc_soisno(beg:end) = nan
  end subroutine init_column_estate_type
  subroutine init_column_wstate_type(beg, end, cws)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (column_wstate_type), intent(inout):: cws
    allocate(cws%h2osno(beg:end))
    allocate(cws%h2osoi_liq(beg:end,-nlevsno+1:nlevgrnd))
    allocate(cws%h2osoi_ice(beg:end,-nlevsno+1:nlevgrnd))
    allocate(cws%h2osoi_liqice_10cm(beg:end))
    allocate(cws%h2osoi_vol(beg:end,1:nlevgrnd))
    allocate(cws%h2osno_old(beg:end))
    allocate(cws%qg(beg:end))
    allocate(cws%dqgdT(beg:end))
    allocate(cws%snowice(beg:end))
    allocate(cws%snowliq(beg:end))
    allocate(cws%soilalpha(beg:end))
    allocate(cws%soilbeta(beg:end))
    allocate(cws%soilalpha_u(beg:end))
    allocate(cws%zwt(beg:end))
    allocate(cws%fcov(beg:end))
    allocate(cws%fsat(beg:end))
    allocate(cws%wa(beg:end))
    allocate(cws%wt(beg:end))
    allocate(cws%qcharge(beg:end))
    allocate(cws%smp_l(beg:end,1:nlevgrnd))
    allocate(cws%hk_l(beg:end,1:nlevgrnd))
    cws%h2osno(beg:end) = nan
    cws%h2osoi_liq(beg:end,-nlevsno+1:nlevgrnd)= spval
    cws%h2osoi_ice(beg:end,-nlevsno+1:nlevgrnd) = spval
    cws%h2osoi_liqice_10cm(beg:end) = spval
    cws%h2osoi_vol(beg:end,1:nlevgrnd) = spval
    cws%h2osno_old(beg:end) = nan
    cws%qg(beg:end) = nan
    cws%dqgdT(beg:end) = nan
    cws%snowice(beg:end) = nan
    cws%snowliq(beg:end) = nan
    cws%soilalpha(beg:end) = nan
    cws%soilbeta(beg:end) = nan
    cws%soilalpha_u(beg:end) = nan
    cws%zwt(beg:end) = nan
    cws%fcov(beg:end) = nan
    cws%fsat(beg:end) = nan
    cws%wa(beg:end) = nan
    cws%wt(beg:end) = nan
    cws%qcharge(beg:end) = nan
    cws%smp_l(beg:end,1:nlevgrnd) = spval
    cws%hk_l(beg:end,1:nlevgrnd) = spval
  end subroutine init_column_wstate_type
  subroutine init_column_cstate_type(beg, end, ccs)
    implicit none
    integer, intent(in) :: beg, end
    type (column_cstate_type), intent(inout):: ccs
    allocate(ccs%soilc(beg:end))
    allocate(ccs%cwdc(beg:end))
    allocate(ccs%litr1c(beg:end))
    allocate(ccs%litr2c(beg:end))
    allocate(ccs%litr3c(beg:end))
    allocate(ccs%soil1c(beg:end))
    allocate(ccs%soil2c(beg:end))
    allocate(ccs%soil3c(beg:end))
    allocate(ccs%soil4c(beg:end))
    allocate(ccs%seedc(beg:end))
    allocate(ccs%col_ctrunc(beg:end))
    allocate(ccs%prod10c(beg:end))
    allocate(ccs%prod100c(beg:end))
    allocate(ccs%totprodc(beg:end))
    allocate(ccs%totlitc(beg:end))
    allocate(ccs%totsomc(beg:end))
    allocate(ccs%totecosysc(beg:end))
    allocate(ccs%totcolc(beg:end))
    ccs%soilc(beg:end) = nan
    ccs%cwdc(beg:end) = nan
    ccs%litr1c(beg:end) = nan
    ccs%litr2c(beg:end) = nan
    ccs%litr3c(beg:end) = nan
    ccs%soil1c(beg:end) = nan
    ccs%soil2c(beg:end) = nan
    ccs%soil3c(beg:end) = nan
    ccs%soil4c(beg:end) = nan
    ccs%seedc(beg:end) = nan
    ccs%col_ctrunc(beg:end) = nan
    ccs%prod10c(beg:end) = nan
    ccs%prod100c(beg:end) = nan
    ccs%totprodc(beg:end) = nan
    ccs%totlitc(beg:end) = nan
    ccs%totsomc(beg:end) = nan
    ccs%totecosysc(beg:end) = nan
    ccs%totcolc(beg:end) = nan
  end subroutine init_column_cstate_type
  subroutine init_column_nstate_type(beg, end, cns)
    implicit none
    integer, intent(in) :: beg, end
    type (column_nstate_type), intent(inout):: cns
    allocate(cns%cwdn(beg:end))
    allocate(cns%litr1n(beg:end))
    allocate(cns%litr2n(beg:end))
    allocate(cns%litr3n(beg:end))
    allocate(cns%soil1n(beg:end))
    allocate(cns%soil2n(beg:end))
    allocate(cns%soil3n(beg:end))
    allocate(cns%soil4n(beg:end))
    allocate(cns%sminn(beg:end))
    allocate(cns%col_ntrunc(beg:end))
    allocate(cns%seedn(beg:end))
    allocate(cns%prod10n(beg:end))
    allocate(cns%prod100n(beg:end))
    allocate(cns%totprodn(beg:end))
    allocate(cns%totlitn(beg:end))
    allocate(cns%totsomn(beg:end))
    allocate(cns%totecosysn(beg:end))
    allocate(cns%totcoln(beg:end))
    cns%cwdn(beg:end) = nan
    cns%litr1n(beg:end) = nan
    cns%litr2n(beg:end) = nan
    cns%litr3n(beg:end) = nan
    cns%soil1n(beg:end) = nan
    cns%soil2n(beg:end) = nan
    cns%soil3n(beg:end) = nan
    cns%soil4n(beg:end) = nan
    cns%sminn(beg:end) = nan
    cns%col_ntrunc(beg:end) = nan
    cns%seedn(beg:end) = nan
    cns%prod10n(beg:end) = nan
    cns%prod100n(beg:end) = nan
    cns%totprodn(beg:end) = nan
    cns%totlitn(beg:end) = nan
    cns%totsomn(beg:end) = nan
    cns%totecosysn(beg:end) = nan
    cns%totcoln(beg:end) = nan
  end subroutine init_column_nstate_type
  subroutine init_column_eflux_type(beg, end, cef)
    implicit none
    integer, intent(in) :: beg, end
    type (column_eflux_type), intent(inout):: cef
    allocate(cef%eflx_snomelt(beg:end))
    allocate(cef%eflx_snomelt_u(beg:end))
    allocate(cef%eflx_snomelt_r(beg:end))
    allocate(cef%eflx_impsoil(beg:end))
    allocate(cef%eflx_fgr12(beg:end))
    allocate(cef%eflx_building_heat(beg:end))
    allocate(cef%eflx_urban_ac(beg:end))
    allocate(cef%eflx_urban_heat(beg:end))
    cef%eflx_snomelt(beg:end) = nan
    cef%eflx_snomelt_u(beg:end) = nan
    cef%eflx_snomelt_r(beg:end) = nan
    cef%eflx_impsoil(beg:end) = nan
    cef%eflx_fgr12(beg:end) = nan
    cef%eflx_building_heat(beg:end) = nan
    cef%eflx_urban_ac(beg:end) = nan
    cef%eflx_urban_heat(beg:end) = nan
  end subroutine init_column_eflux_type
  subroutine init_column_wflux_type(beg, end, cwf)
    use clm_varcon, only : spval
    implicit none
    integer, intent(in) :: beg, end
    type (column_wflux_type), intent(inout):: cwf
    allocate(cwf%qflx_infl(beg:end))
    allocate(cwf%qflx_surf(beg:end))
    allocate(cwf%qflx_drain(beg:end))
    allocate(cwf%qflx_top_soil(beg:end))
    allocate(cwf%qflx_snomelt(beg:end))
    allocate(cwf%qflx_qrgwl(beg:end))
    allocate(cwf%qflx_runoff(beg:end))
    allocate(cwf%qflx_runoff_u(beg:end))
    allocate(cwf%qflx_runoff_r(beg:end))
    allocate(cwf%qmelt(beg:end))
    allocate(cwf%h2ocan_loss(beg:end))
    allocate(cwf%qflx_rsub_sat(beg:end))
    allocate(cwf%flx_bc_dep_dry(beg:end))
    allocate(cwf%flx_bc_dep_wet(beg:end))
    allocate(cwf%flx_bc_dep_pho(beg:end))
    allocate(cwf%flx_bc_dep_phi(beg:end))
    allocate(cwf%flx_bc_dep(beg:end))
    allocate(cwf%flx_oc_dep_dry(beg:end))
    allocate(cwf%flx_oc_dep_wet(beg:end))
    allocate(cwf%flx_oc_dep_pho(beg:end))
    allocate(cwf%flx_oc_dep_phi(beg:end))
    allocate(cwf%flx_oc_dep(beg:end))
    allocate(cwf%flx_dst_dep_dry1(beg:end))
    allocate(cwf%flx_dst_dep_wet1(beg:end))
    allocate(cwf%flx_dst_dep_dry2(beg:end))
    allocate(cwf%flx_dst_dep_wet2(beg:end))
    allocate(cwf%flx_dst_dep_dry3(beg:end))
    allocate(cwf%flx_dst_dep_wet3(beg:end))
    allocate(cwf%flx_dst_dep_dry4(beg:end))
    allocate(cwf%flx_dst_dep_wet4(beg:end))
    allocate(cwf%flx_dst_dep(beg:end))
    allocate(cwf%qflx_snofrz_lyr(beg:end,-nlevsno+1:0))
    cwf%qflx_infl(beg:end) = nan
    cwf%qflx_surf(beg:end) = nan
    cwf%qflx_drain(beg:end) = nan
    cwf%qflx_top_soil(beg:end) = nan
    cwf%qflx_snomelt(beg:end) = nan
    cwf%qflx_qrgwl(beg:end) = nan
    cwf%qflx_runoff(beg:end) = nan
    cwf%qflx_runoff_u(beg:end) = nan
    cwf%qflx_runoff_r(beg:end) = nan
    cwf%qmelt(beg:end) = nan
    cwf%h2ocan_loss(beg:end) = nan
    cwf%qflx_rsub_sat(beg:end) = nan
    cwf%flx_bc_dep_dry(beg:end) = nan
    cwf%flx_bc_dep_wet(beg:end) = nan
    cwf%flx_bc_dep_pho(beg:end) = nan
    cwf%flx_bc_dep_phi(beg:end) = nan
    cwf%flx_bc_dep(beg:end) = nan
    cwf%flx_oc_dep_dry(beg:end) = nan
    cwf%flx_oc_dep_wet(beg:end) = nan
    cwf%flx_oc_dep_pho(beg:end) = nan
    cwf%flx_oc_dep_phi(beg:end) = nan
    cwf%flx_oc_dep(beg:end) = nan
    cwf%flx_dst_dep_dry1(beg:end) = nan
    cwf%flx_dst_dep_wet1(beg:end) = nan
    cwf%flx_dst_dep_dry2(beg:end) = nan
    cwf%flx_dst_dep_wet2(beg:end) = nan
    cwf%flx_dst_dep_dry3(beg:end) = nan
    cwf%flx_dst_dep_wet3(beg:end) = nan
    cwf%flx_dst_dep_dry4(beg:end) = nan
    cwf%flx_dst_dep_wet4(beg:end) = nan
    cwf%flx_dst_dep(beg:end) = nan
    cwf%qflx_snofrz_lyr(beg:end,-nlevsno+1:0) = spval
  end subroutine init_column_wflux_type
  subroutine init_column_cflux_type(beg, end, ccf)
    implicit none
    integer, intent(in) :: beg, end
    type (column_cflux_type), intent(inout):: ccf
    allocate(ccf%m_leafc_to_litr1c(beg:end))
    allocate(ccf%m_leafc_to_litr2c(beg:end))
    allocate(ccf%m_leafc_to_litr3c(beg:end))
    allocate(ccf%m_frootc_to_litr1c(beg:end))
    allocate(ccf%m_frootc_to_litr2c(beg:end))
    allocate(ccf%m_frootc_to_litr3c(beg:end))
    allocate(ccf%m_leafc_storage_to_litr1c(beg:end))
    allocate(ccf%m_frootc_storage_to_litr1c(beg:end))
    allocate(ccf%m_livestemc_storage_to_litr1c(beg:end))
    allocate(ccf%m_deadstemc_storage_to_litr1c(beg:end))
    allocate(ccf%m_livecrootc_storage_to_litr1c(beg:end))
    allocate(ccf%m_deadcrootc_storage_to_litr1c(beg:end))
    allocate(ccf%m_leafc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_frootc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_livestemc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_deadstemc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_livecrootc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_deadcrootc_xfer_to_litr1c(beg:end))
    allocate(ccf%m_livestemc_to_cwdc(beg:end))
    allocate(ccf%m_deadstemc_to_cwdc(beg:end))
    allocate(ccf%m_livecrootc_to_cwdc(beg:end))
    allocate(ccf%m_deadcrootc_to_cwdc(beg:end))
    allocate(ccf%m_gresp_storage_to_litr1c(beg:end))
    allocate(ccf%m_gresp_xfer_to_litr1c(beg:end))
    allocate(ccf%m_deadstemc_to_cwdc_fire(beg:end))
    allocate(ccf%m_deadcrootc_to_cwdc_fire(beg:end))
    allocate(ccf%hrv_leafc_to_litr1c(beg:end))
    allocate(ccf%hrv_leafc_to_litr2c(beg:end))
    allocate(ccf%hrv_leafc_to_litr3c(beg:end))
    allocate(ccf%hrv_frootc_to_litr1c(beg:end))
    allocate(ccf%hrv_frootc_to_litr2c(beg:end))
    allocate(ccf%hrv_frootc_to_litr3c(beg:end))
    allocate(ccf%hrv_livestemc_to_cwdc(beg:end))
    allocate(ccf%hrv_deadstemc_to_prod10c(beg:end))
    allocate(ccf%hrv_deadstemc_to_prod100c(beg:end))
    allocate(ccf%hrv_livecrootc_to_cwdc(beg:end))
    allocate(ccf%hrv_deadcrootc_to_cwdc(beg:end))
    allocate(ccf%hrv_leafc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_frootc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_livestemc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_deadstemc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_livecrootc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_deadcrootc_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_gresp_storage_to_litr1c(beg:end))
    allocate(ccf%hrv_leafc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_frootc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_livestemc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_deadstemc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_livecrootc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_deadcrootc_xfer_to_litr1c(beg:end))
    allocate(ccf%hrv_gresp_xfer_to_litr1c(beg:end))
    allocate(ccf%m_litr1c_to_fire(beg:end))
    allocate(ccf%m_litr2c_to_fire(beg:end))
    allocate(ccf%m_litr3c_to_fire(beg:end))
    allocate(ccf%m_cwdc_to_fire(beg:end))
    allocate(ccf%leafc_to_litr1c(beg:end))
    allocate(ccf%leafc_to_litr2c(beg:end))
    allocate(ccf%leafc_to_litr3c(beg:end))
    allocate(ccf%frootc_to_litr1c(beg:end))
    allocate(ccf%frootc_to_litr2c(beg:end))
    allocate(ccf%frootc_to_litr3c(beg:end))
    allocate(ccf%cwdc_to_litr2c(beg:end))
    allocate(ccf%cwdc_to_litr3c(beg:end))
    allocate(ccf%litr1_hr(beg:end))
    allocate(ccf%litr1c_to_soil1c(beg:end))
    allocate(ccf%litr2_hr(beg:end))
    allocate(ccf%litr2c_to_soil2c(beg:end))
    allocate(ccf%litr3_hr(beg:end))
    allocate(ccf%litr3c_to_soil3c(beg:end))
    allocate(ccf%soil1_hr(beg:end))
    allocate(ccf%soil1c_to_soil2c(beg:end))
    allocate(ccf%soil2_hr(beg:end))
    allocate(ccf%soil2c_to_soil3c(beg:end))
    allocate(ccf%soil3_hr(beg:end))
    allocate(ccf%soil3c_to_soil4c(beg:end))
    allocate(ccf%soil4_hr(beg:end))
    allocate(ccf%lithr(beg:end))
    allocate(ccf%somhr(beg:end))
    allocate(ccf%hr(beg:end))
    allocate(ccf%sr(beg:end))
    allocate(ccf%er(beg:end))
    allocate(ccf%litfire(beg:end))
    allocate(ccf%somfire(beg:end))
    allocate(ccf%totfire(beg:end))
    allocate(ccf%nep(beg:end))
    allocate(ccf%nbp(beg:end))
    allocate(ccf%nee(beg:end))
    allocate(ccf%col_cinputs(beg:end))
    allocate(ccf%col_coutputs(beg:end))
    allocate(ccf%col_fire_closs(beg:end))
    ccf%m_leafc_to_litr1c(beg:end) = nan
    ccf%m_leafc_to_litr2c(beg:end) = nan
    ccf%m_leafc_to_litr3c(beg:end) = nan
    ccf%m_frootc_to_litr1c(beg:end) = nan
    ccf%m_frootc_to_litr2c(beg:end) = nan
    ccf%m_frootc_to_litr3c(beg:end) = nan
    ccf%m_leafc_storage_to_litr1c(beg:end) = nan
    ccf%m_frootc_storage_to_litr1c(beg:end) = nan
    ccf%m_livestemc_storage_to_litr1c(beg:end) = nan
    ccf%m_deadstemc_storage_to_litr1c(beg:end) = nan
    ccf%m_livecrootc_storage_to_litr1c(beg:end) = nan
    ccf%m_deadcrootc_storage_to_litr1c(beg:end) = nan
    ccf%m_leafc_xfer_to_litr1c(beg:end) = nan
    ccf%m_frootc_xfer_to_litr1c(beg:end) = nan
    ccf%m_livestemc_xfer_to_litr1c(beg:end) = nan
    ccf%m_deadstemc_xfer_to_litr1c(beg:end) = nan
    ccf%m_livecrootc_xfer_to_litr1c(beg:end) = nan
    ccf%m_deadcrootc_xfer_to_litr1c(beg:end) = nan
    ccf%m_livestemc_to_cwdc(beg:end) = nan
    ccf%m_deadstemc_to_cwdc(beg:end) = nan
    ccf%m_livecrootc_to_cwdc(beg:end) = nan
    ccf%m_deadcrootc_to_cwdc(beg:end) = nan
    ccf%m_gresp_storage_to_litr1c(beg:end) = nan
    ccf%m_gresp_xfer_to_litr1c(beg:end) = nan
    ccf%m_deadstemc_to_cwdc_fire(beg:end) = nan
    ccf%m_deadcrootc_to_cwdc_fire(beg:end) = nan
    ccf%hrv_leafc_to_litr1c(beg:end) = nan
    ccf%hrv_leafc_to_litr2c(beg:end) = nan
    ccf%hrv_leafc_to_litr3c(beg:end) = nan
    ccf%hrv_frootc_to_litr1c(beg:end) = nan
    ccf%hrv_frootc_to_litr2c(beg:end) = nan
    ccf%hrv_frootc_to_litr3c(beg:end) = nan
    ccf%hrv_livestemc_to_cwdc(beg:end) = nan
    ccf%hrv_deadstemc_to_prod10c(beg:end) = nan
    ccf%hrv_deadstemc_to_prod100c(beg:end) = nan
    ccf%hrv_livecrootc_to_cwdc(beg:end) = nan
    ccf%hrv_deadcrootc_to_cwdc(beg:end) = nan
    ccf%hrv_leafc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_frootc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_livestemc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_deadstemc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_livecrootc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_deadcrootc_storage_to_litr1c(beg:end) = nan
    ccf%hrv_gresp_storage_to_litr1c(beg:end) = nan
    ccf%hrv_leafc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_frootc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_livestemc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_deadstemc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_livecrootc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_deadcrootc_xfer_to_litr1c(beg:end) = nan
    ccf%hrv_gresp_xfer_to_litr1c(beg:end) = nan
    ccf%m_litr1c_to_fire(beg:end) = nan
    ccf%m_litr2c_to_fire(beg:end) = nan
    ccf%m_litr3c_to_fire(beg:end) = nan
    ccf%m_cwdc_to_fire(beg:end) = nan
    ccf%leafc_to_litr1c(beg:end) = nan
    ccf%leafc_to_litr2c(beg:end) = nan
    ccf%leafc_to_litr3c(beg:end) = nan
    ccf%frootc_to_litr1c(beg:end) = nan
    ccf%frootc_to_litr2c(beg:end) = nan
    ccf%frootc_to_litr3c(beg:end) = nan
    ccf%cwdc_to_litr2c(beg:end) = nan
    ccf%cwdc_to_litr3c(beg:end) = nan
    ccf%litr1_hr(beg:end) = nan
    ccf%litr1c_to_soil1c(beg:end) = nan
    ccf%litr2_hr(beg:end) = nan
    ccf%litr2c_to_soil2c(beg:end) = nan
    ccf%litr3_hr(beg:end) = nan
    ccf%litr3c_to_soil3c(beg:end) = nan
    ccf%soil1_hr(beg:end) = nan
    ccf%soil1c_to_soil2c(beg:end) = nan
    ccf%soil2_hr(beg:end) = nan
    ccf%soil2c_to_soil3c(beg:end) = nan
    ccf%soil3_hr(beg:end) = nan
    ccf%soil3c_to_soil4c(beg:end) = nan
    ccf%soil4_hr(beg:end) = nan
    ccf%lithr(beg:end) = nan
    ccf%somhr(beg:end) = nan
    ccf%hr(beg:end) = nan
    ccf%sr(beg:end) = nan
    ccf%er(beg:end) = nan
    ccf%litfire(beg:end) = nan
    ccf%somfire(beg:end) = nan
    ccf%totfire(beg:end) = nan
    ccf%nep(beg:end) = nan
    ccf%nbp(beg:end) = nan
    ccf%nee(beg:end) = nan
    ccf%col_cinputs(beg:end) = nan
    ccf%col_coutputs(beg:end) = nan
    ccf%col_fire_closs(beg:end) = nan
  end subroutine init_column_cflux_type
  subroutine init_column_nflux_type(beg, end, cnf)
    implicit none
    integer, intent(in) :: beg, end
    type (column_nflux_type), intent(inout):: cnf
    allocate(cnf%ndep_to_sminn(beg:end))
    allocate(cnf%nfix_to_sminn(beg:end))
    allocate(cnf%m_leafn_to_litr1n(beg:end))
    allocate(cnf%m_leafn_to_litr2n(beg:end))
    allocate(cnf%m_leafn_to_litr3n(beg:end))
    allocate(cnf%m_frootn_to_litr1n(beg:end))
    allocate(cnf%m_frootn_to_litr2n(beg:end))
    allocate(cnf%m_frootn_to_litr3n(beg:end))
    allocate(cnf%m_leafn_storage_to_litr1n(beg:end))
    allocate(cnf%m_frootn_storage_to_litr1n(beg:end))
    allocate(cnf%m_livestemn_storage_to_litr1n(beg:end))
    allocate(cnf%m_deadstemn_storage_to_litr1n(beg:end))
    allocate(cnf%m_livecrootn_storage_to_litr1n(beg:end))
    allocate(cnf%m_deadcrootn_storage_to_litr1n(beg:end))
    allocate(cnf%m_leafn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_frootn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_livestemn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_deadstemn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_livecrootn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_deadcrootn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_livestemn_to_cwdn(beg:end))
    allocate(cnf%m_deadstemn_to_cwdn(beg:end))
    allocate(cnf%m_livecrootn_to_cwdn(beg:end))
    allocate(cnf%m_deadcrootn_to_cwdn(beg:end))
    allocate(cnf%m_retransn_to_litr1n(beg:end))
    allocate(cnf%hrv_leafn_to_litr1n(beg:end))
    allocate(cnf%hrv_leafn_to_litr2n(beg:end))
    allocate(cnf%hrv_leafn_to_litr3n(beg:end))
    allocate(cnf%hrv_frootn_to_litr1n(beg:end))
    allocate(cnf%hrv_frootn_to_litr2n(beg:end))
    allocate(cnf%hrv_frootn_to_litr3n(beg:end))
    allocate(cnf%hrv_livestemn_to_cwdn(beg:end))
    allocate(cnf%hrv_deadstemn_to_prod10n(beg:end))
    allocate(cnf%hrv_deadstemn_to_prod100n(beg:end))
    allocate(cnf%hrv_livecrootn_to_cwdn(beg:end))
    allocate(cnf%hrv_deadcrootn_to_cwdn(beg:end))
    allocate(cnf%hrv_retransn_to_litr1n(beg:end))
    allocate(cnf%hrv_leafn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_frootn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_livestemn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_deadstemn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_livecrootn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_deadcrootn_storage_to_litr1n(beg:end))
    allocate(cnf%hrv_leafn_xfer_to_litr1n(beg:end))
    allocate(cnf%hrv_frootn_xfer_to_litr1n(beg:end))
    allocate(cnf%hrv_livestemn_xfer_to_litr1n(beg:end))
    allocate(cnf%hrv_deadstemn_xfer_to_litr1n(beg:end))
    allocate(cnf%hrv_livecrootn_xfer_to_litr1n(beg:end))
    allocate(cnf%hrv_deadcrootn_xfer_to_litr1n(beg:end))
    allocate(cnf%m_deadstemn_to_cwdn_fire(beg:end))
    allocate(cnf%m_deadcrootn_to_cwdn_fire(beg:end))
    allocate(cnf%m_litr1n_to_fire(beg:end))
    allocate(cnf%m_litr2n_to_fire(beg:end))
    allocate(cnf%m_litr3n_to_fire(beg:end))
    allocate(cnf%m_cwdn_to_fire(beg:end))
    allocate(cnf%leafn_to_litr1n(beg:end))
    allocate(cnf%leafn_to_litr2n(beg:end))
    allocate(cnf%leafn_to_litr3n(beg:end))
    allocate(cnf%frootn_to_litr1n(beg:end))
    allocate(cnf%frootn_to_litr2n(beg:end))
    allocate(cnf%frootn_to_litr3n(beg:end))
    allocate(cnf%cwdn_to_litr2n(beg:end))
    allocate(cnf%cwdn_to_litr3n(beg:end))
    allocate(cnf%litr1n_to_soil1n(beg:end))
    allocate(cnf%sminn_to_soil1n_l1(beg:end))
    allocate(cnf%litr2n_to_soil2n(beg:end))
    allocate(cnf%sminn_to_soil2n_l2(beg:end))
    allocate(cnf%litr3n_to_soil3n(beg:end))
    allocate(cnf%sminn_to_soil3n_l3(beg:end))
    allocate(cnf%soil1n_to_soil2n(beg:end))
    allocate(cnf%sminn_to_soil2n_s1(beg:end))
    allocate(cnf%soil2n_to_soil3n(beg:end))
    allocate(cnf%sminn_to_soil3n_s2(beg:end))
    allocate(cnf%soil3n_to_soil4n(beg:end))
    allocate(cnf%sminn_to_soil4n_s3(beg:end))
    allocate(cnf%soil4n_to_sminn(beg:end))
    allocate(cnf%sminn_to_denit_l1s1(beg:end))
    allocate(cnf%sminn_to_denit_l2s2(beg:end))
    allocate(cnf%sminn_to_denit_l3s3(beg:end))
    allocate(cnf%sminn_to_denit_s1s2(beg:end))
    allocate(cnf%sminn_to_denit_s2s3(beg:end))
    allocate(cnf%sminn_to_denit_s3s4(beg:end))
    allocate(cnf%sminn_to_denit_s4(beg:end))
    allocate(cnf%sminn_to_denit_excess(beg:end))
    allocate(cnf%sminn_leached(beg:end))
    allocate(cnf%dwt_seedn_to_leaf(beg:end))
    allocate(cnf%dwt_seedn_to_deadstem(beg:end))
    allocate(cnf%dwt_conv_nflux(beg:end))
    allocate(cnf%dwt_prod10n_gain(beg:end))
    allocate(cnf%dwt_prod100n_gain(beg:end))
    allocate(cnf%dwt_frootn_to_litr1n(beg:end))
    allocate(cnf%dwt_frootn_to_litr2n(beg:end))
    allocate(cnf%dwt_frootn_to_litr3n(beg:end))
    allocate(cnf%dwt_livecrootn_to_cwdn(beg:end))
    allocate(cnf%dwt_deadcrootn_to_cwdn(beg:end))
    allocate(cnf%dwt_nloss(beg:end))
    allocate(cnf%prod10n_loss(beg:end))
    allocate(cnf%prod100n_loss(beg:end))
    allocate(cnf%product_nloss(beg:end))
    allocate(cnf%potential_immob(beg:end))
    allocate(cnf%actual_immob(beg:end))
    allocate(cnf%sminn_to_plant(beg:end))
    allocate(cnf%supplement_to_sminn(beg:end))
    allocate(cnf%gross_nmin(beg:end))
    allocate(cnf%net_nmin(beg:end))
    allocate(cnf%denit(beg:end))
    allocate(cnf%col_ninputs(beg:end))
    allocate(cnf%col_noutputs(beg:end))
    allocate(cnf%col_fire_nloss(beg:end))
    cnf%ndep_to_sminn(beg:end) = nan
    cnf%nfix_to_sminn(beg:end) = nan
    cnf%m_leafn_to_litr1n(beg:end) = nan
    cnf%m_leafn_to_litr2n(beg:end) = nan
    cnf%m_leafn_to_litr3n(beg:end) = nan
    cnf%m_frootn_to_litr1n(beg:end) = nan
    cnf%m_frootn_to_litr2n(beg:end) = nan
    cnf%m_frootn_to_litr3n(beg:end) = nan
    cnf%m_leafn_storage_to_litr1n(beg:end) = nan
    cnf%m_frootn_storage_to_litr1n(beg:end) = nan
    cnf%m_livestemn_storage_to_litr1n(beg:end) = nan
    cnf%m_deadstemn_storage_to_litr1n(beg:end) = nan
    cnf%m_livecrootn_storage_to_litr1n(beg:end) = nan
    cnf%m_deadcrootn_storage_to_litr1n(beg:end) = nan
    cnf%m_leafn_xfer_to_litr1n(beg:end) = nan
    cnf%m_frootn_xfer_to_litr1n(beg:end) = nan
    cnf%m_livestemn_xfer_to_litr1n(beg:end) = nan
    cnf%m_deadstemn_xfer_to_litr1n(beg:end) = nan
    cnf%m_livecrootn_xfer_to_litr1n(beg:end) = nan
    cnf%m_deadcrootn_xfer_to_litr1n(beg:end) = nan
    cnf%m_livestemn_to_cwdn(beg:end) = nan
    cnf%m_deadstemn_to_cwdn(beg:end) = nan
    cnf%m_livecrootn_to_cwdn(beg:end) = nan
    cnf%m_deadcrootn_to_cwdn(beg:end) = nan
    cnf%m_retransn_to_litr1n(beg:end) = nan
    cnf%hrv_leafn_to_litr1n(beg:end) = nan
    cnf%hrv_leafn_to_litr2n(beg:end) = nan
    cnf%hrv_leafn_to_litr3n(beg:end) = nan
    cnf%hrv_frootn_to_litr1n(beg:end) = nan
    cnf%hrv_frootn_to_litr2n(beg:end) = nan
    cnf%hrv_frootn_to_litr3n(beg:end) = nan
    cnf%hrv_livestemn_to_cwdn(beg:end) = nan
    cnf%hrv_deadstemn_to_prod10n(beg:end) = nan
    cnf%hrv_deadstemn_to_prod100n(beg:end) = nan
    cnf%hrv_livecrootn_to_cwdn(beg:end) = nan
    cnf%hrv_deadcrootn_to_cwdn(beg:end) = nan
    cnf%hrv_retransn_to_litr1n(beg:end) = nan
    cnf%hrv_leafn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_frootn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_livestemn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_deadstemn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_livecrootn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_deadcrootn_storage_to_litr1n(beg:end) = nan
    cnf%hrv_leafn_xfer_to_litr1n(beg:end) = nan
    cnf%hrv_frootn_xfer_to_litr1n(beg:end) = nan
    cnf%hrv_livestemn_xfer_to_litr1n(beg:end) = nan
    cnf%hrv_deadstemn_xfer_to_litr1n(beg:end) = nan
    cnf%hrv_livecrootn_xfer_to_litr1n(beg:end) = nan
    cnf%hrv_deadcrootn_xfer_to_litr1n(beg:end) = nan
    cnf%m_deadstemn_to_cwdn_fire(beg:end) = nan
    cnf%m_deadcrootn_to_cwdn_fire(beg:end) = nan
    cnf%m_litr1n_to_fire(beg:end) = nan
    cnf%m_litr2n_to_fire(beg:end) = nan
    cnf%m_litr3n_to_fire(beg:end) = nan
    cnf%m_cwdn_to_fire(beg:end) = nan
    cnf%leafn_to_litr1n(beg:end) = nan
    cnf%leafn_to_litr2n(beg:end) = nan
    cnf%leafn_to_litr3n(beg:end) = nan
    cnf%frootn_to_litr1n(beg:end) = nan
    cnf%frootn_to_litr2n(beg:end) = nan
    cnf%frootn_to_litr3n(beg:end) = nan
    cnf%cwdn_to_litr2n(beg:end) = nan
    cnf%cwdn_to_litr3n(beg:end) = nan
    cnf%litr1n_to_soil1n(beg:end) = nan
    cnf%sminn_to_soil1n_l1(beg:end) = nan
    cnf%litr2n_to_soil2n(beg:end) = nan
    cnf%sminn_to_soil2n_l2(beg:end) = nan
    cnf%litr3n_to_soil3n(beg:end) = nan
    cnf%sminn_to_soil3n_l3(beg:end) = nan
    cnf%soil1n_to_soil2n(beg:end) = nan
    cnf%sminn_to_soil2n_s1(beg:end) = nan
    cnf%soil2n_to_soil3n(beg:end) = nan
    cnf%sminn_to_soil3n_s2(beg:end) = nan
    cnf%soil3n_to_soil4n(beg:end) = nan
    cnf%sminn_to_soil4n_s3(beg:end) = nan
    cnf%soil4n_to_sminn(beg:end) = nan
    cnf%sminn_to_denit_l1s1(beg:end) = nan
    cnf%sminn_to_denit_l2s2(beg:end) = nan
    cnf%sminn_to_denit_l3s3(beg:end) = nan
    cnf%sminn_to_denit_s1s2(beg:end) = nan
    cnf%sminn_to_denit_s2s3(beg:end) = nan
    cnf%sminn_to_denit_s3s4(beg:end) = nan
    cnf%sminn_to_denit_s4(beg:end) = nan
    cnf%sminn_to_denit_excess(beg:end) = nan
    cnf%sminn_leached(beg:end) = nan
    cnf%dwt_seedn_to_leaf(beg:end) = nan
    cnf%dwt_seedn_to_deadstem(beg:end) = nan
    cnf%dwt_conv_nflux(beg:end) = nan
    cnf%dwt_prod10n_gain(beg:end) = nan
    cnf%dwt_prod100n_gain(beg:end) = nan
    cnf%dwt_frootn_to_litr1n(beg:end) = nan
    cnf%dwt_frootn_to_litr2n(beg:end) = nan
    cnf%dwt_frootn_to_litr3n(beg:end) = nan
    cnf%dwt_livecrootn_to_cwdn(beg:end) = nan
    cnf%dwt_deadcrootn_to_cwdn(beg:end) = nan
    cnf%dwt_nloss(beg:end) = nan
    cnf%prod10n_loss(beg:end) = nan
    cnf%prod100n_loss(beg:end) = nan
    cnf%product_nloss(beg:end) = nan
    cnf%potential_immob(beg:end) = nan
    cnf%actual_immob(beg:end) = nan
    cnf%sminn_to_plant(beg:end) = nan
    cnf%supplement_to_sminn(beg:end) = nan
    cnf%gross_nmin(beg:end) = nan
    cnf%net_nmin(beg:end) = nan
    cnf%denit(beg:end) = nan
    cnf%col_ninputs(beg:end) = nan
    cnf%col_noutputs(beg:end) = nan
    cnf%col_fire_nloss(beg:end) = nan
  end subroutine init_column_nflux_type
  subroutine init_landunit_pstate_type(beg, end, lps)
    implicit none
    integer, intent(in) :: beg, end
    type (landunit_pstate_type), intent(inout):: lps
    allocate(lps%t_building(beg:end))
    allocate(lps%t_building_max(beg:end))
    allocate(lps%t_building_min(beg:end))
    allocate(lps%tk_wall(beg:end,nlevurb))
    allocate(lps%tk_roof(beg:end,nlevurb))
    allocate(lps%tk_improad(beg:end,nlevgrnd))
    allocate(lps%cv_wall(beg:end,nlevurb))
    allocate(lps%cv_roof(beg:end,nlevurb))
    allocate(lps%cv_improad(beg:end,nlevgrnd))
    allocate(lps%thick_wall(beg:end))
    allocate(lps%thick_roof(beg:end))
    allocate(lps%nlev_improad(beg:end))
    allocate(lps%vf_sr(beg:end))
    allocate(lps%vf_wr(beg:end))
    allocate(lps%vf_sw(beg:end))
    allocate(lps%vf_rw(beg:end))
    allocate(lps%vf_ww(beg:end))
    allocate(lps%taf(beg:end))
    allocate(lps%qaf(beg:end))
    allocate(lps%sabs_roof_dir(beg:end,1:numrad))
    allocate(lps%sabs_roof_dif(beg:end,1:numrad))
    allocate(lps%sabs_sunwall_dir(beg:end,1:numrad))
    allocate(lps%sabs_sunwall_dif(beg:end,1:numrad))
    allocate(lps%sabs_shadewall_dir(beg:end,1:numrad))
    allocate(lps%sabs_shadewall_dif(beg:end,1:numrad))
    allocate(lps%sabs_improad_dir(beg:end,1:numrad))
    allocate(lps%sabs_improad_dif(beg:end,1:numrad))
    allocate(lps%sabs_perroad_dir(beg:end,1:numrad))
    allocate(lps%sabs_perroad_dif(beg:end,1:numrad))
    lps%t_building(beg:end) = nan
    lps%t_building_max(beg:end) = nan
    lps%t_building_min(beg:end) = nan
    lps%tk_wall(beg:end,1:nlevurb) = nan
    lps%tk_roof(beg:end,1:nlevurb) = nan
    lps%tk_improad(beg:end,1:nlevgrnd) = nan
    lps%cv_wall(beg:end,1:nlevurb) = nan
    lps%cv_roof(beg:end,1:nlevurb) = nan
    lps%cv_improad(beg:end,1:nlevgrnd) = nan
    lps%cv_improad(beg:end,1:5) = nan
    lps%thick_wall(beg:end) = nan
    lps%thick_roof(beg:end) = nan
    lps%nlev_improad(beg:end) = bigint
    lps%vf_sr(beg:end) = nan
    lps%vf_wr(beg:end) = nan
    lps%vf_sw(beg:end) = nan
    lps%vf_rw(beg:end) = nan
    lps%vf_ww(beg:end) = nan
    lps%taf(beg:end) = nan
    lps%qaf(beg:end) = nan
    lps%sabs_roof_dir(beg:end,1:numrad) = nan
    lps%sabs_roof_dif(beg:end,1:numrad) = nan
    lps%sabs_sunwall_dir(beg:end,1:numrad) = nan
    lps%sabs_sunwall_dif(beg:end,1:numrad) = nan
    lps%sabs_shadewall_dir(beg:end,1:numrad) = nan
    lps%sabs_shadewall_dif(beg:end,1:numrad) = nan
    lps%sabs_improad_dir(beg:end,1:numrad) = nan
    lps%sabs_improad_dif(beg:end,1:numrad) = nan
    lps%sabs_perroad_dir(beg:end,1:numrad) = nan
    lps%sabs_perroad_dif(beg:end,1:numrad) = nan
  end subroutine init_landunit_pstate_type
  subroutine init_landunit_eflux_type(beg, end, lef)
    implicit none
    integer, intent(in) :: beg, end
    type (landunit_eflux_type), intent(inout):: lef
    allocate(lef%eflx_traffic(beg:end))
    allocate(lef%eflx_traffic_factor(beg:end))
    allocate(lef%eflx_wasteheat(beg:end))
    allocate(lef%eflx_heat_from_ac(beg:end))
    lef%eflx_traffic(beg:end) = nan
    lef%eflx_traffic_factor(beg:end) = nan
    lef%eflx_wasteheat(beg:end) = nan
    lef%eflx_heat_from_ac(beg:end) = nan
  end subroutine init_landunit_eflux_type
  subroutine init_gridcell_pstate_type(beg, end, gps)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_pstate_type), intent(inout):: gps
  end subroutine init_gridcell_pstate_type
  subroutine init_gridcell_efstate_type(beg, end, gve)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_efstate_type), intent(inout) :: gve
    allocate(gve%efisop(6,beg:end))
    gve%efisop(:,beg:end) = nan
  end subroutine init_gridcell_efstate_type
  subroutine init_gridcell_wflux_type(beg, end, gwf)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_wflux_type), intent(inout):: gwf
    allocate(gwf%qflx_runoffg(beg:end))
    allocate(gwf%qflx_snwcp_iceg(beg:end))
    allocate(gwf%qflx_liq_dynbal(beg:end))
    allocate(gwf%qflx_ice_dynbal(beg:end))
    gwf%qflx_runoffg(beg:end) = nan
    gwf%qflx_snwcp_iceg(beg:end) = nan
    gwf%qflx_liq_dynbal(beg:end) = nan
    gwf%qflx_ice_dynbal(beg:end) = nan
  end subroutine init_gridcell_wflux_type
  subroutine init_gridcell_eflux_type(beg, end, gef)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_eflux_type), intent(inout):: gef
    allocate(gef%eflx_sh_totg(beg:end))
    allocate(gef%eflx_dynbal(beg:end))
    gef%eflx_sh_totg(beg:end) = nan
    gef%eflx_dynbal(beg:end) = nan
  end subroutine init_gridcell_eflux_type
  subroutine init_gridcell_wstate_type(beg, end, gws)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_wstate_type), intent(inout):: gws
    allocate(gws%gc_liq1(beg:end))
    allocate(gws%gc_liq2(beg:end))
    allocate(gws%gc_ice1(beg:end))
    allocate(gws%gc_ice2(beg:end))
    gws%gc_liq1(beg:end) = nan
    gws%gc_liq2(beg:end) = nan
    gws%gc_ice1(beg:end) = nan
    gws%gc_ice2(beg:end) = nan
  end subroutine init_gridcell_wstate_type
  subroutine init_gridcell_estate_type(beg, end, ges)
    implicit none
    integer, intent(in) :: beg, end
    type (gridcell_estate_type), intent(inout):: ges
    allocate(ges%gc_heat1(beg:end))
    allocate(ges%gc_heat2(beg:end))
    ges%gc_heat1(beg:end) = nan
    ges%gc_heat2(beg:end) = nan
  end subroutine init_gridcell_estate_type
  subroutine init_atm2lnd_type(beg, end, a2l)
  implicit none
  integer, intent(in) :: beg, end
  type (atm2lnd_type), intent(inout):: a2l
  real(r8) :: ival
  integer :: beg_atm, end_atm
  allocate(a2l%forc_t(beg:end))
  allocate(a2l%forc_u(beg:end))
  allocate(a2l%forc_v(beg:end))
  allocate(a2l%forc_wind(beg:end))
  allocate(a2l%forc_q(beg:end))
  allocate(a2l%forc_rh(beg:end))
  allocate(a2l%forc_hgt(beg:end))
  allocate(a2l%forc_hgt_u(beg:end))
  allocate(a2l%forc_hgt_t(beg:end))
  allocate(a2l%forc_hgt_q(beg:end))
  allocate(a2l%forc_pbot(beg:end))
  allocate(a2l%forc_th(beg:end))
  allocate(a2l%forc_vp(beg:end))
  allocate(a2l%forc_rho(beg:end))
  allocate(a2l%forc_psrf(beg:end))
  allocate(a2l%forc_pco2(beg:end))
  allocate(a2l%forc_lwrad(beg:end))
  allocate(a2l%forc_solad(beg:end,numrad))
  allocate(a2l%forc_solai(beg:end,numrad))
  allocate(a2l%forc_solar(beg:end))
  allocate(a2l%forc_rain(beg:end))
  allocate(a2l%forc_snow(beg:end))
  allocate(a2l%forc_ndep(beg:end))
  allocate(a2l%rainf(beg:end))
  allocate(a2l%forc_po2(beg:end))
  allocate(a2l%forc_aer(beg:end,14))
  ival = 0.0_r8
  a2l%forc_t(beg:end) = ival
  a2l%forc_u(beg:end) = ival
  a2l%forc_v(beg:end) = ival
  a2l%forc_wind(beg:end) = ival
  a2l%forc_q(beg:end) = ival
  a2l%forc_rh(beg:end) = ival
  a2l%forc_hgt(beg:end) = ival
  a2l%forc_hgt_u(beg:end) = ival
  a2l%forc_hgt_t(beg:end) = ival
  a2l%forc_hgt_q(beg:end) = ival
  a2l%forc_pbot(beg:end) = ival
  a2l%forc_th(beg:end) = ival
  a2l%forc_vp(beg:end) = ival
  a2l%forc_rho(beg:end) = ival
  a2l%forc_psrf(beg:end) = ival
  a2l%forc_pco2(beg:end) = ival
  a2l%forc_lwrad(beg:end) = ival
  a2l%forc_solad(beg:end,1:numrad) = ival
  a2l%forc_solai(beg:end,1:numrad) = ival
  a2l%forc_solar(beg:end) = ival
  a2l%forc_rain(beg:end) = ival
  a2l%forc_snow(beg:end) = ival
  a2l%forc_ndep(beg:end) = ival
  a2l%rainf(beg:end) = nan
  a2l%forc_po2(beg:end) = ival
  a2l%forc_aer(beg:end,:) = ival
end subroutine init_atm2lnd_type
  subroutine clmtype_dealloc()
    implicit none
    call dealloc_pft_type ( clm3%g%l%c%p)
    call dealloc_column_type ( clm3%g%l%c)
    call dealloc_landunit_type( clm3%g%l)
    call dealloc_gridcell_type( clm3%g)
    call dealloc_pft_ecophys_constants()
    call dealloc_energy_balance_type( clm3%g%l%c%p%pebal)
    call dealloc_energy_balance_type( clm3%g%l%c%cebal)
    call dealloc_energy_balance_type( clm3%g%l%lebal)
    call dealloc_energy_balance_type( clm3%g%gebal)
    call dealloc_energy_balance_type( clm3%mebal)
    call dealloc_water_balance_type( clm3%g%l%c%p%pwbal)
    call dealloc_water_balance_type( clm3%g%l%c%cwbal)
    call dealloc_water_balance_type( clm3%g%l%lwbal)
    call dealloc_water_balance_type( clm3%g%gwbal)
    call dealloc_water_balance_type( clm3%mwbal)
    call dealloc_carbon_balance_type( clm3%g%l%c%p%pcbal)
    call dealloc_carbon_balance_type( clm3%g%l%c%ccbal)
    call dealloc_nitrogen_balance_type( clm3%g%l%c%p%pnbal)
    call dealloc_nitrogen_balance_type( clm3%g%l%c%cnbal)
    call dealloc_pft_pstate_type( clm3%g%l%c%p%pps)
    call dealloc_pft_pstate_type( clm3%g%l%c%cps%pps_a)
    call dealloc_pft_epv_type( clm3%g%l%c%p%pepv)
    call dealloc_pft_vstate_type( clm3%g%l%c%p%pvs)
    call dealloc_pft_estate_type( clm3%g%l%c%p%pes)
    call dealloc_pft_estate_type( clm3%g%l%c%ces%pes_a)
    call dealloc_pft_wstate_type( clm3%g%l%c%p%pws)
    call dealloc_pft_wstate_type( clm3%g%l%c%cws%pws_a)
    call dealloc_pft_cstate_type( clm3%g%l%c%p%pcs)
    call dealloc_pft_cstate_type( clm3%g%l%c%ccs%pcs_a)
    call dealloc_pft_nstate_type( clm3%g%l%c%p%pns)
    call dealloc_pft_nstate_type( clm3%g%l%c%cns%pns_a)
    call dealloc_pft_eflux_type( clm3%g%l%c%p%pef)
    call dealloc_pft_eflux_type( clm3%g%l%c%cef%pef_a)
    call dealloc_pft_mflux_type( clm3%g%l%c%p%pmf)
    call dealloc_pft_mflux_type( clm3%g%l%c%cmf%pmf_a)
    call dealloc_pft_wflux_type( clm3%g%l%c%p%pwf)
    call dealloc_pft_wflux_type( clm3%g%l%c%cwf%pwf_a)
    call dealloc_pft_cflux_type( clm3%g%l%c%p%pcf)
    call dealloc_pft_cflux_type( clm3%g%l%c%ccf%pcf_a)
    call dealloc_pft_nflux_type( clm3%g%l%c%p%pnf)
    call dealloc_pft_nflux_type( clm3%g%l%c%cnf%pnf_a)
    call dealloc_pft_vflux_type( clm3%g%l%c%p%pvf)
    call dealloc_pft_vflux_type( clm3%g%l%c%cvf%pvf_a)
    call dealloc_gridcell_efstate_type( clm3%g%gve)
    call dealloc_pft_dflux_type( clm3%g%l%c%p%pdf)
    call dealloc_pft_dflux_type( clm3%g%l%c%cdf%pdf_a)
    call dealloc_pft_depvd_type( clm3%g%l%c%p%pdd)
    call dealloc_column_pstate_type( clm3%g%l%c%cps)
    call dealloc_column_pstate_type( clm3%g%l%lps%cps_a)
    call dealloc_column_pstate_type( clm3%g%gps%cps_a)
    call dealloc_column_pstate_type( clm3%mps%cps_a)
    call dealloc_column_estate_type( clm3%g%l%c%ces)
    call dealloc_column_estate_type( clm3%g%l%les%ces_a)
    call dealloc_column_estate_type( clm3%g%ges%ces_a)
    call dealloc_column_estate_type( clm3%mes%ces_a)
    call dealloc_column_wstate_type( clm3%g%l%c%cws)
    call dealloc_column_wstate_type( clm3%g%l%lws%cws_a)
    call dealloc_column_wstate_type( clm3%g%gws%cws_a)
    call dealloc_column_wstate_type( clm3%mws%cws_a)
    call dealloc_column_cstate_type( clm3%g%l%c%ccs)
    call dealloc_column_cstate_type( clm3%g%l%lcs%ccs_a)
    call dealloc_column_cstate_type( clm3%g%gcs%ccs_a)
    call dealloc_column_cstate_type( clm3%mcs%ccs_a)
    call dealloc_column_nstate_type( clm3%g%l%c%cns)
    call dealloc_column_nstate_type( clm3%g%l%lns%cns_a)
    call dealloc_column_nstate_type( clm3%g%gns%cns_a)
    call dealloc_column_nstate_type( clm3%mns%cns_a)
    call dealloc_column_eflux_type( clm3%g%l%c%cef)
    call dealloc_column_eflux_type( clm3%g%l%lef%cef_a)
    call dealloc_column_eflux_type( clm3%g%gef%cef_a)
    call dealloc_column_eflux_type( clm3%mef%cef_a)
    call dealloc_column_wflux_type( clm3%g%l%c%cwf)
    call dealloc_column_wflux_type( clm3%g%l%lwf%cwf_a)
    call dealloc_column_wflux_type( clm3%g%gwf%cwf_a)
    call dealloc_column_wflux_type( clm3%mwf%cwf_a)
    call dealloc_column_cflux_type( clm3%g%l%c%ccf)
    call dealloc_column_nflux_type( clm3%g%l%c%cnf)
    call dealloc_landunit_pstate_type( clm3%g%l%lps)
   call CLMDebug('mark1')
    call dealloc_landunit_eflux_type( clm3%g%l%lef)
    call dealloc_gridcell_pstate_type( clm3%g%gps)
    call dealloc_gridcell_wflux_type( clm3%g%gwf)
    call dealloc_gridcell_eflux_type( clm3%g%gef)
       call CLMDebug('mark2')
    call dealloc_gridcell_wstate_type( clm3%g%gws)
    call dealloc_gridcell_estate_type( clm3%g%ges)
     call CLMDebug('mark3')
    call dealloc_atm2lnd_type ( clm_a2l)
     call CLMDebug('done clmtype_dealloc')
end subroutine clmtype_dealloc
  subroutine dealloc_pft_type ( p)
    implicit none
    type(pft_type), intent(inout):: p
    deallocate(p%gridcell ,p%wtgcell )
    deallocate(p%landunit ,p%wtlunit )
    deallocate(p%column ,p%wtcol )
    deallocate(p%itype )
    deallocate(p%mxy )
    deallocate(p%area)
  end subroutine dealloc_pft_type
  subroutine dealloc_column_type ( c)
  implicit none
    type(column_type), intent(inout):: c
   deallocate(c%gridcell ,c%wtgcell )
   deallocate(c%landunit ,c%wtlunit )
   deallocate(c%pfti ,c%pftf ,c%npfts )
   deallocate(c%itype )
   deallocate(c%area)
  end subroutine dealloc_column_type
  subroutine dealloc_landunit_type ( l)
    implicit none
    type(landunit_type), intent(inout):: l
   deallocate(l%gridcell ,l%wtgcell )
   deallocate(l%coli ,l%colf ,l%ncolumns )
   deallocate(l%pfti ,l%pftf ,l%npfts )
   deallocate(l%itype )
   deallocate(l%ifspecial )
   deallocate(l%lakpoi )
   deallocate(l%urbpoi )
   deallocate(l%canyon_hwr )
   deallocate(l%wtroad_perv )
   deallocate(l%ht_roof )
   deallocate(l%wtlunit_roof )
   deallocate(l%wind_hgt_canyon )
   deallocate(l%z_0_town )
   deallocate(l%z_d_town )
   deallocate(l%area)
  end subroutine dealloc_landunit_type
  subroutine dealloc_gridcell_type ( g)
    implicit none
    type(gridcell_type), intent(inout):: g
   deallocate(g%luni ,g%lunf ,g%nlandunits )
   deallocate(g%coli ,g%colf ,g%ncolumns )
   deallocate(g%pfti ,g%pftf ,g%npfts )
   deallocate(g%gindex )
   deallocate(g%area )
   deallocate(g%lat )
   deallocate(g%lon )
   deallocate(g%latdeg )
   deallocate(g%londeg )
   deallocate(g%gindex_a )
   deallocate(g%lat_a )
   deallocate(g%lon_a )
   deallocate(g%latdeg_a )
   deallocate(g%londeg_a )
  end subroutine dealloc_gridcell_type
  subroutine dealloc_energy_balance_type( ebal)
    implicit none
    type(energy_balance_type), intent(inout):: ebal
    deallocate(ebal%errsoi )
    deallocate(ebal%errseb )
    deallocate(ebal%errsol )
    deallocate(ebal%errlon )
  end subroutine dealloc_energy_balance_type
  subroutine dealloc_water_balance_type( wbal)
    implicit none
    type(water_balance_type), intent(inout):: wbal
    deallocate(wbal%begwb )
    deallocate(wbal%endwb )
    deallocate(wbal%errh2o )
  end subroutine dealloc_water_balance_type
  subroutine dealloc_carbon_balance_type( cbal)
    implicit none
    type(carbon_balance_type), intent(inout):: cbal
    deallocate(cbal%begcb )
    deallocate(cbal%endcb )
    deallocate(cbal%errcb )
  end subroutine dealloc_carbon_balance_type
  subroutine dealloc_nitrogen_balance_type( nbal)
    implicit none
    type(nitrogen_balance_type), intent(inout):: nbal
    deallocate(nbal%begnb )
    deallocate(nbal%endnb )
    deallocate(nbal%errnb )
  end subroutine dealloc_nitrogen_balance_type
  subroutine dealloc_pft_ecophys_constants()
    implicit none
    deallocate(pftcon%noveg )
    deallocate(pftcon%tree )
    deallocate(pftcon%smpso )
    deallocate(pftcon%smpsc )
    deallocate(pftcon%fnitr )
    deallocate(pftcon%foln )
    deallocate(pftcon%dleaf )
    deallocate(pftcon%c3psn )
    deallocate(pftcon%vcmx25 )
    deallocate(pftcon%mp )
    deallocate(pftcon%qe25 )
    deallocate(pftcon%xl )
    deallocate(pftcon%rhol)
    deallocate(pftcon%rhos)
    deallocate(pftcon%taul)
    deallocate(pftcon%taus)
    deallocate(pftcon%z0mr )
    deallocate(pftcon%displar )
    deallocate(pftcon%roota_par )
    deallocate(pftcon%rootb_par )
    deallocate(pftcon%sla )
    deallocate(pftcon%slatop )
    deallocate(pftcon%dsladlai )
    deallocate(pftcon%leafcn )
    deallocate(pftcon%flnr )
    deallocate(pftcon%woody )
    deallocate(pftcon%lflitcn )
    deallocate(pftcon%frootcn )
    deallocate(pftcon%livewdcn )
    deallocate(pftcon%deadwdcn )
    deallocate(pftcon%froot_leaf )
    deallocate(pftcon%stem_leaf )
    deallocate(pftcon%croot_stem )
    deallocate(pftcon%flivewd )
    deallocate(pftcon%fcur )
    deallocate(pftcon%lf_flab )
    deallocate(pftcon%lf_fcel )
    deallocate(pftcon%lf_flig )
    deallocate(pftcon%fr_flab )
    deallocate(pftcon%fr_fcel )
    deallocate(pftcon%fr_flig )
    deallocate(pftcon%dw_fcel )
    deallocate(pftcon%dw_flig )
    deallocate(pftcon%leaf_long )
    deallocate(pftcon%evergreen )
    deallocate(pftcon%stress_decid )
    deallocate(pftcon%season_decid )
    deallocate(pftcon%resist )
    deallocate(pftcon%dwood )
  end subroutine dealloc_pft_ecophys_constants
  subroutine dealloc_pft_pstate_type( pps)
    use clm_varcon, only : spval
    implicit none
    type (pft_pstate_type), intent(inout):: pps
    deallocate(pps%frac_veg_nosno )
    deallocate(pps%frac_veg_nosno_alb )
    deallocate(pps%emv )
    deallocate(pps%z0mv )
    deallocate(pps%z0hv )
    deallocate(pps%z0qv )
    deallocate(pps%rootfr )
    deallocate(pps%rootr )
    deallocate(pps%rresis )
    deallocate(pps%dewmx )
    deallocate(pps%rssun )
    deallocate(pps%rssha )
    deallocate(pps%laisun )
    deallocate(pps%laisha )
    deallocate(pps%btran )
    deallocate(pps%fsun )
    deallocate(pps%tlai )
    deallocate(pps%tsai )
    deallocate(pps%elai )
    deallocate(pps%esai )
    deallocate(pps%fwet )
    deallocate(pps%fdry )
    deallocate(pps%dt_veg )
    deallocate(pps%htop )
    deallocate(pps%hbot )
    deallocate(pps%z0m )
    deallocate(pps%displa )
    deallocate(pps%albd )
    deallocate(pps%albi )
    deallocate(pps%fabd )
    deallocate(pps%fabi )
    deallocate(pps%ftdd )
    deallocate(pps%ftid )
    deallocate(pps%ftii )
    deallocate(pps%u10 )
    deallocate(pps%fv )
    deallocate(pps%ram1 )
    deallocate(pps%vds )
    deallocate(pps%slasun )
    deallocate(pps%slasha )
    deallocate(pps%lncsun )
    deallocate(pps%lncsha )
    deallocate(pps%vcmxsun )
    deallocate(pps%vcmxsha )
    deallocate(pps%gdir )
    deallocate(pps%omega )
    deallocate(pps%eff_kid )
    deallocate(pps%eff_kii )
    deallocate(pps%sun_faid )
    deallocate(pps%sun_faii )
    deallocate(pps%sha_faid )
    deallocate(pps%sha_faii )
    deallocate(pps%forc_hgt_u_pft )
    deallocate(pps%forc_hgt_t_pft )
    deallocate(pps%forc_hgt_q_pft )
    deallocate(pps%cisun )
    deallocate(pps%cisha )
    deallocate(pps%sandfrac )
    deallocate(pps%clayfrac )
    deallocate(pps%mlaidiff )
    deallocate(pps%rb1 )
    deallocate(pps%annlai)
  end subroutine dealloc_pft_pstate_type
  subroutine dealloc_pft_epv_type( pepv)
    implicit none
    type (pft_epv_type), intent(inout):: pepv
    deallocate(pepv%dormant_flag )
    deallocate(pepv%days_active )
    deallocate(pepv%onset_flag )
    deallocate(pepv%onset_counter )
    deallocate(pepv%onset_gddflag )
    deallocate(pepv%onset_fdd )
    deallocate(pepv%onset_gdd )
    deallocate(pepv%onset_swi )
    deallocate(pepv%offset_flag )
    deallocate(pepv%offset_counter )
    deallocate(pepv%offset_fdd )
    deallocate(pepv%offset_swi )
    deallocate(pepv%lgsf )
    deallocate(pepv%bglfr )
    deallocate(pepv%bgtr )
    deallocate(pepv%dayl )
    deallocate(pepv%prev_dayl )
    deallocate(pepv%annavg_t2m )
    deallocate(pepv%tempavg_t2m )
    deallocate(pepv%gpp )
    deallocate(pepv%availc )
    deallocate(pepv%xsmrpool_recover )
    deallocate(pepv%alloc_pnow )
    deallocate(pepv%c_allometry )
    deallocate(pepv%n_allometry )
    deallocate(pepv%plant_ndemand )
    deallocate(pepv%tempsum_potential_gpp )
    deallocate(pepv%annsum_potential_gpp )
    deallocate(pepv%tempmax_retransn )
    deallocate(pepv%annmax_retransn )
    deallocate(pepv%avail_retransn )
    deallocate(pepv%plant_nalloc )
    deallocate(pepv%plant_calloc )
    deallocate(pepv%excess_cflux )
    deallocate(pepv%downreg )
    deallocate(pepv%prev_leafc_to_litter )
    deallocate(pepv%prev_frootc_to_litter )
    deallocate(pepv%tempsum_npp )
    deallocate(pepv%annsum_npp )
  end subroutine dealloc_pft_epv_type
  subroutine dealloc_pft_vstate_type( pvs)
    implicit none
    type (pft_vstate_type), intent(inout):: pvs
    deallocate(pvs%t_veg24 )
    deallocate(pvs%t_veg240 )
    deallocate(pvs%fsd24 )
    deallocate(pvs%fsd240 )
    deallocate(pvs%fsi24 )
    deallocate(pvs%fsi240 )
    deallocate(pvs%fsun24 )
    deallocate(pvs%fsun240 )
    deallocate(pvs%elai_p )
  end subroutine dealloc_pft_vstate_type
  subroutine dealloc_pft_estate_type( pes)
    implicit none
    type (pft_estate_type), intent(inout):: pes
    deallocate(pes%t_ref2m )
    deallocate(pes%t_ref2m_min )
    deallocate(pes%t_ref2m_max )
    deallocate(pes%t_ref2m_min_inst )
    deallocate(pes%t_ref2m_max_inst )
    deallocate(pes%q_ref2m )
    deallocate(pes%t_ref2m_u )
    deallocate(pes%t_ref2m_r )
    deallocate(pes%t_ref2m_min_u )
    deallocate(pes%t_ref2m_min_r )
    deallocate(pes%t_ref2m_max_u )
    deallocate(pes%t_ref2m_max_r )
    deallocate(pes%t_ref2m_min_inst_u )
    deallocate(pes%t_ref2m_min_inst_r )
    deallocate(pes%t_ref2m_max_inst_u )
    deallocate(pes%t_ref2m_max_inst_r )
    deallocate(pes%rh_ref2m )
    deallocate(pes%rh_ref2m_u )
    deallocate(pes%rh_ref2m_r )
    deallocate(pes%t_veg )
    deallocate(pes%thm )
  end subroutine dealloc_pft_estate_type
  subroutine dealloc_pft_wstate_type( pws)
    implicit none
    type (pft_wstate_type), intent(inout):: pws
    deallocate(pws%h2ocan )
  end subroutine dealloc_pft_wstate_type
  subroutine dealloc_pft_cstate_type( pcs)
    implicit none
    type (pft_cstate_type), intent(inout):: pcs
    deallocate(pcs%leafc )
    deallocate(pcs%leafc_storage )
    deallocate(pcs%leafc_xfer )
    deallocate(pcs%frootc )
    deallocate(pcs%frootc_storage )
    deallocate(pcs%frootc_xfer )
    deallocate(pcs%livestemc )
    deallocate(pcs%livestemc_storage )
    deallocate(pcs%livestemc_xfer )
    deallocate(pcs%deadstemc )
    deallocate(pcs%deadstemc_storage )
    deallocate(pcs%deadstemc_xfer )
    deallocate(pcs%livecrootc )
    deallocate(pcs%livecrootc_storage )
    deallocate(pcs%livecrootc_xfer )
    deallocate(pcs%deadcrootc )
    deallocate(pcs%deadcrootc_storage )
    deallocate(pcs%deadcrootc_xfer )
    deallocate(pcs%gresp_storage )
    deallocate(pcs%gresp_xfer )
    deallocate(pcs%cpool )
    deallocate(pcs%xsmrpool )
    deallocate(pcs%pft_ctrunc )
    deallocate(pcs%dispvegc )
    deallocate(pcs%storvegc )
    deallocate(pcs%totvegc )
    deallocate(pcs%totpftc )
    deallocate(pcs%leafcmax )
  end subroutine dealloc_pft_cstate_type
  subroutine dealloc_pft_nstate_type( pns)
    implicit none
    type (pft_nstate_type), intent(inout):: pns
    deallocate(pns%leafn )
    deallocate(pns%leafn_storage )
    deallocate(pns%leafn_xfer )
    deallocate(pns%frootn )
    deallocate(pns%frootn_storage )
    deallocate(pns%frootn_xfer )
    deallocate(pns%livestemn )
    deallocate(pns%livestemn_storage )
    deallocate(pns%livestemn_xfer )
    deallocate(pns%deadstemn )
    deallocate(pns%deadstemn_storage )
    deallocate(pns%deadstemn_xfer )
    deallocate(pns%livecrootn )
    deallocate(pns%livecrootn_storage )
    deallocate(pns%livecrootn_xfer )
    deallocate(pns%deadcrootn )
    deallocate(pns%deadcrootn_storage )
    deallocate(pns%deadcrootn_xfer )
    deallocate(pns%retransn )
    deallocate(pns%npool )
    deallocate(pns%pft_ntrunc )
    deallocate(pns%dispvegn )
    deallocate(pns%storvegn )
    deallocate(pns%totvegn )
    deallocate(pns%totpftn )
 end subroutine dealloc_pft_nstate_type
  subroutine dealloc_pft_eflux_type( pef)
    implicit none
    type (pft_eflux_type), intent(inout):: pef
    deallocate(pef%sabg )
    deallocate(pef%sabv )
    deallocate(pef%fsa )
    deallocate(pef%fsa_u )
    deallocate(pef%fsa_r )
    deallocate(pef%fsr )
    deallocate(pef%parsun )
    deallocate(pef%parsha )
    deallocate(pef%dlrad )
    deallocate(pef%ulrad )
    deallocate(pef%eflx_lh_tot )
    deallocate(pef%eflx_lh_tot_u )
    deallocate(pef%eflx_lh_tot_r )
    deallocate(pef%eflx_lh_grnd )
    deallocate(pef%eflx_soil_grnd )
    deallocate(pef%eflx_soil_grnd_u )
    deallocate(pef%eflx_soil_grnd_r )
    deallocate(pef%eflx_sh_tot )
    deallocate(pef%eflx_sh_tot_u )
    deallocate(pef%eflx_sh_tot_r )
    deallocate(pef%eflx_sh_grnd )
    deallocate(pef%eflx_sh_veg )
    deallocate(pef%eflx_lh_vege )
    deallocate(pef%eflx_lh_vegt )
    deallocate(pef%eflx_wasteheat_pft )
    deallocate(pef%eflx_heat_from_ac_pft )
    deallocate(pef%eflx_traffic_pft )
    deallocate(pef%eflx_anthro )
    deallocate(pef%cgrnd )
    deallocate(pef%cgrndl )
    deallocate(pef%cgrnds )
    deallocate(pef%eflx_gnet )
    deallocate(pef%dgnetdT )
    deallocate(pef%eflx_lwrad_out )
    deallocate(pef%eflx_lwrad_net )
    deallocate(pef%eflx_lwrad_net_u )
    deallocate(pef%eflx_lwrad_net_r )
    deallocate(pef%netrad )
    deallocate(pef%fsds_vis_d )
    deallocate(pef%fsds_nir_d )
    deallocate(pef%fsds_vis_i )
    deallocate(pef%fsds_nir_i )
    deallocate(pef%fsr_vis_d )
    deallocate(pef%fsr_nir_d )
    deallocate(pef%fsr_vis_i )
    deallocate(pef%fsr_nir_i )
    deallocate(pef%fsds_vis_d_ln )
    deallocate(pef%fsds_nir_d_ln )
    deallocate(pef%fsr_vis_d_ln )
    deallocate(pef%fsr_nir_d_ln )
    deallocate(pef%sun_add )
    deallocate(pef%tot_aid )
    deallocate(pef%sun_aid )
    deallocate(pef%sun_aii )
    deallocate(pef%sha_aid )
    deallocate(pef%sha_aii )
    deallocate(pef%sun_atot )
    deallocate(pef%sha_atot )
    deallocate(pef%sun_alf )
    deallocate(pef%sha_alf )
    deallocate(pef%sun_aperlai )
    deallocate(pef%sha_aperlai )
    deallocate(pef%sabg_lyr)
    deallocate(pef%sfc_frc_aer )
    deallocate(pef%sfc_frc_bc )
    deallocate(pef%sfc_frc_oc )
    deallocate(pef%sfc_frc_dst )
    deallocate(pef%sfc_frc_aer_sno )
    deallocate(pef%sfc_frc_bc_sno )
    deallocate(pef%sfc_frc_oc_sno )
    deallocate(pef%sfc_frc_dst_sno )
    deallocate(pef%fsr_sno_vd )
    deallocate(pef%fsr_sno_nd )
    deallocate(pef%fsr_sno_vi )
    deallocate(pef%fsr_sno_ni )
    deallocate(pef%fsds_sno_vd )
    deallocate(pef%fsds_sno_nd )
    deallocate(pef%fsds_sno_vi )
    deallocate(pef%fsds_sno_ni )
  end subroutine dealloc_pft_eflux_type
  subroutine dealloc_pft_mflux_type( pmf)
    implicit none
    type (pft_mflux_type), intent(inout) :: pmf
   deallocate(pmf%taux )
    deallocate(pmf%tauy )
  end subroutine dealloc_pft_mflux_type
  subroutine dealloc_pft_wflux_type( pwf)
    implicit none
    type (pft_wflux_type), intent(inout) :: pwf
    deallocate(pwf%qflx_prec_intr )
    deallocate(pwf%qflx_prec_grnd )
    deallocate(pwf%qflx_rain_grnd )
    deallocate(pwf%qflx_snow_grnd )
    deallocate(pwf%qflx_snwcp_liq )
    deallocate(pwf%qflx_snwcp_ice )
    deallocate(pwf%qflx_evap_veg )
    deallocate(pwf%qflx_tran_veg )
    deallocate(pwf%qflx_evap_can )
    deallocate(pwf%qflx_evap_soi )
    deallocate(pwf%qflx_evap_tot )
    deallocate(pwf%qflx_evap_grnd )
    deallocate(pwf%qflx_dew_grnd )
    deallocate(pwf%qflx_sub_snow )
    deallocate(pwf%qflx_dew_snow )
  end subroutine dealloc_pft_wflux_type
  subroutine dealloc_pft_cflux_type( pcf)
    implicit none
    type (pft_cflux_type), intent(inout) :: pcf
    deallocate(pcf%psnsun )
    deallocate(pcf%psnsha )
    deallocate(pcf%fpsn )
    deallocate(pcf%fco2 )
    deallocate(pcf%m_leafc_to_litter )
    deallocate(pcf%m_frootc_to_litter )
    deallocate(pcf%m_leafc_storage_to_litter )
    deallocate(pcf%m_frootc_storage_to_litter )
    deallocate(pcf%m_livestemc_storage_to_litter )
    deallocate(pcf%m_deadstemc_storage_to_litter )
    deallocate(pcf%m_livecrootc_storage_to_litter )
    deallocate(pcf%m_deadcrootc_storage_to_litter )
    deallocate(pcf%m_leafc_xfer_to_litter )
    deallocate(pcf%m_frootc_xfer_to_litter )
    deallocate(pcf%m_livestemc_xfer_to_litter )
    deallocate(pcf%m_deadstemc_xfer_to_litter )
    deallocate(pcf%m_livecrootc_xfer_to_litter )
    deallocate(pcf%m_deadcrootc_xfer_to_litter )
    deallocate(pcf%m_livestemc_to_litter )
    deallocate(pcf%m_deadstemc_to_litter )
    deallocate(pcf%m_livecrootc_to_litter )
    deallocate(pcf%m_deadcrootc_to_litter )
    deallocate(pcf%m_gresp_storage_to_litter )
    deallocate(pcf%m_gresp_xfer_to_litter )
    deallocate(pcf%hrv_leafc_to_litter )
    deallocate(pcf%hrv_leafc_storage_to_litter )
    deallocate(pcf%hrv_leafc_xfer_to_litter )
    deallocate(pcf%hrv_frootc_to_litter )
    deallocate(pcf%hrv_frootc_storage_to_litter )
    deallocate(pcf%hrv_frootc_xfer_to_litter )
    deallocate(pcf%hrv_livestemc_to_litter )
    deallocate(pcf%hrv_livestemc_storage_to_litter )
    deallocate(pcf%hrv_livestemc_xfer_to_litter )
    deallocate(pcf%hrv_deadstemc_to_prod10c )
    deallocate(pcf%hrv_deadstemc_to_prod100c )
    deallocate(pcf%hrv_deadstemc_storage_to_litter )
    deallocate(pcf%hrv_deadstemc_xfer_to_litter )
    deallocate(pcf%hrv_livecrootc_to_litter )
    deallocate(pcf%hrv_livecrootc_storage_to_litter )
    deallocate(pcf%hrv_livecrootc_xfer_to_litter )
    deallocate(pcf%hrv_deadcrootc_to_litter )
    deallocate(pcf%hrv_deadcrootc_storage_to_litter )
    deallocate(pcf%hrv_deadcrootc_xfer_to_litter )
    deallocate(pcf%hrv_gresp_storage_to_litter )
    deallocate(pcf%hrv_gresp_xfer_to_litter )
    deallocate(pcf%hrv_xsmrpool_to_atm )
    deallocate(pcf%m_leafc_to_fire )
    deallocate(pcf%m_frootc_to_fire )
    deallocate(pcf%m_leafc_storage_to_fire )
    deallocate(pcf%m_frootc_storage_to_fire )
    deallocate(pcf%m_livestemc_storage_to_fire )
    deallocate(pcf%m_deadstemc_storage_to_fire )
    deallocate(pcf%m_livecrootc_storage_to_fire )
    deallocate(pcf%m_deadcrootc_storage_to_fire )
    deallocate(pcf%m_leafc_xfer_to_fire )
    deallocate(pcf%m_frootc_xfer_to_fire )
    deallocate(pcf%m_livestemc_xfer_to_fire )
    deallocate(pcf%m_deadstemc_xfer_to_fire )
    deallocate(pcf%m_livecrootc_xfer_to_fire )
    deallocate(pcf%m_deadcrootc_xfer_to_fire )
    deallocate(pcf%m_livestemc_to_fire )
    deallocate(pcf%m_deadstemc_to_fire )
    deallocate(pcf%m_deadstemc_to_litter_fire )
    deallocate(pcf%m_livecrootc_to_fire )
    deallocate(pcf%m_deadcrootc_to_fire )
    deallocate(pcf%m_deadcrootc_to_litter_fire )
    deallocate(pcf%m_gresp_storage_to_fire )
    deallocate(pcf%m_gresp_xfer_to_fire )
    deallocate(pcf%leafc_xfer_to_leafc )
    deallocate(pcf%frootc_xfer_to_frootc )
    deallocate(pcf%livestemc_xfer_to_livestemc )
    deallocate(pcf%deadstemc_xfer_to_deadstemc )
    deallocate(pcf%livecrootc_xfer_to_livecrootc )
    deallocate(pcf%deadcrootc_xfer_to_deadcrootc )
    deallocate(pcf%leafc_to_litter )
    deallocate(pcf%frootc_to_litter )
    deallocate(pcf%leaf_mr )
    deallocate(pcf%froot_mr )
    deallocate(pcf%livestem_mr )
    deallocate(pcf%livecroot_mr )
    deallocate(pcf%leaf_curmr )
    deallocate(pcf%froot_curmr )
    deallocate(pcf%livestem_curmr )
    deallocate(pcf%livecroot_curmr )
    deallocate(pcf%leaf_xsmr )
    deallocate(pcf%froot_xsmr )
    deallocate(pcf%livestem_xsmr )
    deallocate(pcf%livecroot_xsmr )
    deallocate(pcf%psnsun_to_cpool )
    deallocate(pcf%psnshade_to_cpool )
    deallocate(pcf%cpool_to_xsmrpool )
    deallocate(pcf%cpool_to_leafc )
    deallocate(pcf%cpool_to_leafc_storage )
    deallocate(pcf%cpool_to_frootc )
    deallocate(pcf%cpool_to_frootc_storage )
    deallocate(pcf%cpool_to_livestemc )
    deallocate(pcf%cpool_to_livestemc_storage )
    deallocate(pcf%cpool_to_deadstemc )
    deallocate(pcf%cpool_to_deadstemc_storage )
    deallocate(pcf%cpool_to_livecrootc )
    deallocate(pcf%cpool_to_livecrootc_storage )
    deallocate(pcf%cpool_to_deadcrootc )
    deallocate(pcf%cpool_to_deadcrootc_storage )
    deallocate(pcf%cpool_to_gresp_storage )
    deallocate(pcf%cpool_leaf_gr )
    deallocate(pcf%cpool_leaf_storage_gr )
    deallocate(pcf%transfer_leaf_gr )
    deallocate(pcf%cpool_froot_gr )
    deallocate(pcf%cpool_froot_storage_gr )
    deallocate(pcf%transfer_froot_gr )
    deallocate(pcf%cpool_livestem_gr )
    deallocate(pcf%cpool_livestem_storage_gr )
    deallocate(pcf%transfer_livestem_gr )
    deallocate(pcf%cpool_deadstem_gr )
    deallocate(pcf%cpool_deadstem_storage_gr )
    deallocate(pcf%transfer_deadstem_gr )
    deallocate(pcf%cpool_livecroot_gr )
    deallocate(pcf%cpool_livecroot_storage_gr )
    deallocate(pcf%transfer_livecroot_gr )
    deallocate(pcf%cpool_deadcroot_gr )
    deallocate(pcf%cpool_deadcroot_storage_gr )
    deallocate(pcf%transfer_deadcroot_gr )
    deallocate(pcf%leafc_storage_to_xfer )
    deallocate(pcf%frootc_storage_to_xfer )
    deallocate(pcf%livestemc_storage_to_xfer )
    deallocate(pcf%deadstemc_storage_to_xfer )
    deallocate(pcf%livecrootc_storage_to_xfer )
    deallocate(pcf%deadcrootc_storage_to_xfer )
    deallocate(pcf%gresp_storage_to_xfer )
    deallocate(pcf%livestemc_to_deadstemc )
    deallocate(pcf%livecrootc_to_deadcrootc )
    deallocate(pcf%gpp )
    deallocate(pcf%mr )
    deallocate(pcf%current_gr )
    deallocate(pcf%transfer_gr )
    deallocate(pcf%storage_gr )
    deallocate(pcf%gr )
    deallocate(pcf%ar )
    deallocate(pcf%rr )
    deallocate(pcf%npp )
    deallocate(pcf%agnpp )
    deallocate(pcf%bgnpp )
    deallocate(pcf%litfall )
    deallocate(pcf%vegfire )
    deallocate(pcf%wood_harvestc )
    deallocate(pcf%pft_cinputs )
    deallocate(pcf%pft_coutputs )
    deallocate(pcf%pft_fire_closs )
  end subroutine dealloc_pft_cflux_type
  subroutine dealloc_pft_nflux_type( pnf)
    implicit none
    type (pft_nflux_type), intent(inout) :: pnf
    deallocate(pnf%m_leafn_to_litter )
    deallocate(pnf%m_frootn_to_litter )
    deallocate(pnf%m_leafn_storage_to_litter )
    deallocate(pnf%m_frootn_storage_to_litter )
    deallocate(pnf%m_livestemn_storage_to_litter )
    deallocate(pnf%m_deadstemn_storage_to_litter )
    deallocate(pnf%m_livecrootn_storage_to_litter )
    deallocate(pnf%m_deadcrootn_storage_to_litter )
    deallocate(pnf%m_leafn_xfer_to_litter )
    deallocate(pnf%m_frootn_xfer_to_litter )
    deallocate(pnf%m_livestemn_xfer_to_litter )
    deallocate(pnf%m_deadstemn_xfer_to_litter )
    deallocate(pnf%m_livecrootn_xfer_to_litter )
    deallocate(pnf%m_deadcrootn_xfer_to_litter )
    deallocate(pnf%m_livestemn_to_litter )
    deallocate(pnf%m_deadstemn_to_litter )
    deallocate(pnf%m_livecrootn_to_litter )
    deallocate(pnf%m_deadcrootn_to_litter )
    deallocate(pnf%m_retransn_to_litter )
    deallocate(pnf%hrv_leafn_to_litter )
    deallocate(pnf%hrv_frootn_to_litter )
    deallocate(pnf%hrv_leafn_storage_to_litter )
    deallocate(pnf%hrv_frootn_storage_to_litter )
    deallocate(pnf%hrv_livestemn_storage_to_litter )
    deallocate(pnf%hrv_deadstemn_storage_to_litter )
    deallocate(pnf%hrv_livecrootn_storage_to_litter )
    deallocate(pnf%hrv_deadcrootn_storage_to_litter )
    deallocate(pnf%hrv_leafn_xfer_to_litter )
    deallocate(pnf%hrv_frootn_xfer_to_litter )
    deallocate(pnf%hrv_livestemn_xfer_to_litter )
    deallocate(pnf%hrv_deadstemn_xfer_to_litter )
    deallocate(pnf%hrv_livecrootn_xfer_to_litter )
    deallocate(pnf%hrv_deadcrootn_xfer_to_litter )
    deallocate(pnf%hrv_livestemn_to_litter )
    deallocate(pnf%hrv_deadstemn_to_prod10n )
    deallocate(pnf%hrv_deadstemn_to_prod100n )
    deallocate(pnf%hrv_livecrootn_to_litter )
    deallocate(pnf%hrv_deadcrootn_to_litter )
    deallocate(pnf%hrv_retransn_to_litter )
    deallocate(pnf%m_leafn_to_fire )
    deallocate(pnf%m_frootn_to_fire )
    deallocate(pnf%m_leafn_storage_to_fire )
    deallocate(pnf%m_frootn_storage_to_fire )
    deallocate(pnf%m_livestemn_storage_to_fire )
    deallocate(pnf%m_deadstemn_storage_to_fire )
    deallocate(pnf%m_livecrootn_storage_to_fire )
    deallocate(pnf%m_deadcrootn_storage_to_fire )
    deallocate(pnf%m_leafn_xfer_to_fire )
    deallocate(pnf%m_frootn_xfer_to_fire )
    deallocate(pnf%m_livestemn_xfer_to_fire )
    deallocate(pnf%m_deadstemn_xfer_to_fire )
    deallocate(pnf%m_livecrootn_xfer_to_fire )
    deallocate(pnf%m_deadcrootn_xfer_to_fire )
    deallocate(pnf%m_livestemn_to_fire )
    deallocate(pnf%m_deadstemn_to_fire )
    deallocate(pnf%m_deadstemn_to_litter_fire )
    deallocate(pnf%m_livecrootn_to_fire )
    deallocate(pnf%m_deadcrootn_to_fire )
    deallocate(pnf%m_deadcrootn_to_litter_fire )
    deallocate(pnf%m_retransn_to_fire )
    deallocate(pnf%leafn_xfer_to_leafn )
    deallocate(pnf%frootn_xfer_to_frootn )
    deallocate(pnf%livestemn_xfer_to_livestemn )
    deallocate(pnf%deadstemn_xfer_to_deadstemn )
    deallocate(pnf%livecrootn_xfer_to_livecrootn )
    deallocate(pnf%deadcrootn_xfer_to_deadcrootn )
    deallocate(pnf%leafn_to_litter )
    deallocate(pnf%leafn_to_retransn )
    deallocate(pnf%frootn_to_litter )
    deallocate(pnf%retransn_to_npool )
    deallocate(pnf%sminn_to_npool )
    deallocate(pnf%npool_to_leafn )
    deallocate(pnf%npool_to_leafn_storage )
    deallocate(pnf%npool_to_frootn )
    deallocate(pnf%npool_to_frootn_storage )
    deallocate(pnf%npool_to_livestemn )
    deallocate(pnf%npool_to_livestemn_storage )
    deallocate(pnf%npool_to_deadstemn )
    deallocate(pnf%npool_to_deadstemn_storage )
    deallocate(pnf%npool_to_livecrootn )
    deallocate(pnf%npool_to_livecrootn_storage )
    deallocate(pnf%npool_to_deadcrootn )
    deallocate(pnf%npool_to_deadcrootn_storage )
    deallocate(pnf%leafn_storage_to_xfer )
    deallocate(pnf%frootn_storage_to_xfer )
    deallocate(pnf%livestemn_storage_to_xfer )
    deallocate(pnf%deadstemn_storage_to_xfer )
    deallocate(pnf%livecrootn_storage_to_xfer )
    deallocate(pnf%deadcrootn_storage_to_xfer )
    deallocate(pnf%livestemn_to_deadstemn )
    deallocate(pnf%livestemn_to_retransn )
    deallocate(pnf%livecrootn_to_deadcrootn )
    deallocate(pnf%livecrootn_to_retransn )
    deallocate(pnf%ndeploy )
    deallocate(pnf%pft_ninputs )
    deallocate(pnf%pft_noutputs )
    deallocate(pnf%wood_harvestn )
    deallocate(pnf%pft_fire_nloss )
  end subroutine dealloc_pft_nflux_type
  subroutine dealloc_pft_vflux_type( pvf)
    implicit none
    type (pft_vflux_type), intent(inout) :: pvf
    deallocate(pvf%vocflx_tot )
    deallocate(pvf%vocflx)
    deallocate(pvf%vocflx_1 )
    deallocate(pvf%vocflx_2 )
    deallocate(pvf%vocflx_3 )
    deallocate(pvf%vocflx_4 )
    deallocate(pvf%vocflx_5 )
    deallocate(pvf%Eopt_out )
    deallocate(pvf%topt_out )
    deallocate(pvf%alpha_out )
    deallocate(pvf%cp_out )
    deallocate(pvf%para_out )
    deallocate(pvf%par24a_out )
    deallocate(pvf%par240a_out )
    deallocate(pvf%paru_out )
    deallocate(pvf%par24u_out )
    deallocate(pvf%par240u_out )
    deallocate(pvf%gamma_out )
    deallocate(pvf%gammaL_out )
    deallocate(pvf%gammaT_out )
    deallocate(pvf%gammaP_out )
    deallocate(pvf%gammaA_out )
    deallocate(pvf%gammaS_out )
  end subroutine dealloc_pft_vflux_type
  subroutine dealloc_pft_dflux_type( pdf)
    implicit none
    type (pft_dflux_type), intent(inout):: pdf
    deallocate(pdf%flx_mss_vrt_dst)
    deallocate(pdf%flx_mss_vrt_dst_tot )
    deallocate(pdf%vlc_trb)
    deallocate(pdf%vlc_trb_1 )
    deallocate(pdf%vlc_trb_2 )
    deallocate(pdf%vlc_trb_3 )
    deallocate(pdf%vlc_trb_4 )
  end subroutine dealloc_pft_dflux_type
  subroutine dealloc_pft_depvd_type( pdd)
    implicit none
    type (pft_depvd_type), intent(inout):: pdd
  end subroutine dealloc_pft_depvd_type
  subroutine dealloc_column_pstate_type( cps)
    implicit none
    type (column_pstate_type), intent(inout):: cps
    deallocate(cps%snl )
    deallocate(cps%isoicol )
    deallocate(cps%bsw)
    deallocate(cps%watsat)
    deallocate(cps%watfc)
    deallocate(cps%watdry)
    deallocate(cps%watopt)
    deallocate(cps%hksat)
    deallocate(cps%sucsat)
    deallocate(cps%csol)
    deallocate(cps%tkmg)
    deallocate(cps%tkdry)
    deallocate(cps%tksatu)
    deallocate(cps%smpmin )
    deallocate(cps%hkdepth )
    deallocate(cps%wtfact )
    deallocate(cps%fracice)
    deallocate(cps%gwc_thr )
    deallocate(cps%mss_frc_cly_vld )
    deallocate(cps%mbl_bsn_fct )
    deallocate(cps%do_capsnow )
    deallocate(cps%snowdp )
    deallocate(cps%frac_sno )
    deallocate(cps%zi)
    deallocate(cps%dz)
    deallocate(cps%z )
    deallocate(cps%frac_iceold)
    deallocate(cps%imelt)
    deallocate(cps%eff_porosity)
    deallocate(cps%emg )
    deallocate(cps%z0mg )
    deallocate(cps%z0hg )
    deallocate(cps%z0qg )
    deallocate(cps%htvp )
    deallocate(cps%beta )
    deallocate(cps%zii )
    deallocate(cps%albgrd)
    deallocate(cps%albgri)
    deallocate(cps%rootr_column)
    deallocate(cps%rootfr_road_perv)
    deallocate(cps%rootr_road_perv)
    deallocate(cps%wf )
    deallocate(cps%max_dayl )
    deallocate(cps%bsw2)
    deallocate(cps%psisat)
    deallocate(cps%vwcsat)
    deallocate(cps%soilpsi)
    deallocate(cps%decl )
    deallocate(cps%coszen )
    deallocate(cps%fpi )
    deallocate(cps%fpg )
    deallocate(cps%annsum_counter )
    deallocate(cps%cannsum_npp )
    deallocate(cps%cannavg_t2m )
    deallocate(cps%me )
    deallocate(cps%fire_prob )
    deallocate(cps%mean_fire_prob )
    deallocate(cps%fireseasonl )
    deallocate(cps%farea_burned )
    deallocate(cps%ann_farea_burned )
    deallocate(cps%albsnd_hst)
    deallocate(cps%albsni_hst)
    deallocate(cps%albsod)
    deallocate(cps%albsoi)
    deallocate(cps%flx_absdv)
    deallocate(cps%flx_absdn)
    deallocate(cps%flx_absiv )
    deallocate(cps%flx_absin )
    deallocate(cps%snw_rds )
    deallocate(cps%snw_rds_top )
    deallocate(cps%sno_liq_top )
    deallocate(cps%mss_bcpho )
    deallocate(cps%mss_bcphi )
    deallocate(cps%mss_bctot )
    deallocate(cps%mss_bc_col )
    deallocate(cps%mss_bc_top )
    deallocate(cps%mss_ocpho )
    deallocate(cps%mss_ocphi )
    deallocate(cps%mss_octot )
    deallocate(cps%mss_oc_col )
    deallocate(cps%mss_oc_top )
    deallocate(cps%mss_dst1 )
    deallocate(cps%mss_dst2 )
    deallocate(cps%mss_dst3 )
    deallocate(cps%mss_dst4 )
    deallocate(cps%mss_dsttot )
    deallocate(cps%mss_dst_col )
    deallocate(cps%mss_dst_top )
    deallocate(cps%h2osno_top )
    deallocate(cps%mss_cnc_bcphi )
    deallocate(cps%mss_cnc_bcpho )
    deallocate(cps%mss_cnc_ocphi )
    deallocate(cps%mss_cnc_ocpho )
    deallocate(cps%mss_cnc_dst1 )
    deallocate(cps%mss_cnc_dst2 )
    deallocate(cps%mss_cnc_dst3 )
    deallocate(cps%mss_cnc_dst4 )
    deallocate(cps%albgrd_pur )
    deallocate(cps%albgri_pur )
    deallocate(cps%albgrd_bc )
    deallocate(cps%albgri_bc )
    deallocate(cps%albgrd_oc )
    deallocate(cps%albgri_oc )
    deallocate(cps%albgrd_dst )
    deallocate(cps%albgri_dst )
    deallocate(cps%dTdz_top )
    deallocate(cps%snot_top )
  end subroutine dealloc_column_pstate_type
  subroutine dealloc_column_estate_type( ces)
    implicit none
    type (column_estate_type), intent(inout):: ces
    deallocate(ces%t_grnd )
    deallocate(ces%t_grnd_u )
    deallocate(ces%t_grnd_r )
    deallocate(ces%dt_grnd )
    deallocate(ces%t_soisno)
    deallocate(ces%t_soi_10cm )
    deallocate(ces%t_lake)
    deallocate(ces%tssbef)
    deallocate(ces%thv )
    deallocate(ces%hc_soi )
    deallocate(ces%hc_soisno )
  end subroutine dealloc_column_estate_type
  subroutine dealloc_column_wstate_type( cws)
    implicit none
    type (column_wstate_type), intent(inout):: cws
    deallocate(cws%h2osno )
    deallocate(cws%h2osoi_liq)
    deallocate(cws%h2osoi_ice)
    deallocate(cws%h2osoi_liqice_10cm )
    deallocate(cws%h2osoi_vol )
    deallocate(cws%h2osno_old )
    deallocate(cws%qg )
    deallocate(cws%dqgdT )
    deallocate(cws%snowice )
    deallocate(cws%snowliq )
    deallocate(cws%soilalpha )
    deallocate(cws%soilbeta )
    deallocate(cws%soilalpha_u )
    deallocate(cws%zwt )
    deallocate(cws%fcov )
    deallocate(cws%fsat )
    deallocate(cws%wa )
    deallocate(cws%wt )
    deallocate(cws%qcharge )
    deallocate(cws%smp_l )
    deallocate(cws%hk_l )
  end subroutine dealloc_column_wstate_type
  subroutine dealloc_column_cstate_type( ccs)
    implicit none
    type (column_cstate_type), intent(inout):: ccs
    deallocate(ccs%soilc )
    deallocate(ccs%cwdc )
    deallocate(ccs%litr1c )
    deallocate(ccs%litr2c )
    deallocate(ccs%litr3c )
    deallocate(ccs%soil1c )
    deallocate(ccs%soil2c )
    deallocate(ccs%soil3c )
    deallocate(ccs%soil4c )
    deallocate(ccs%seedc )
    deallocate(ccs%col_ctrunc )
    deallocate(ccs%prod10c )
    deallocate(ccs%prod100c )
    deallocate(ccs%totprodc )
    deallocate(ccs%totlitc )
    deallocate(ccs%totsomc )
    deallocate(ccs%totecosysc )
    deallocate(ccs%totcolc )
  end subroutine dealloc_column_cstate_type
  subroutine dealloc_column_nstate_type( cns)
    implicit none
    type (column_nstate_type), intent(inout):: cns
    deallocate(cns%cwdn )
    deallocate(cns%litr1n )
    deallocate(cns%litr2n )
    deallocate(cns%litr3n )
    deallocate(cns%soil1n )
    deallocate(cns%soil2n )
    deallocate(cns%soil3n )
    deallocate(cns%soil4n )
    deallocate(cns%sminn )
    deallocate(cns%col_ntrunc )
    deallocate(cns%seedn )
    deallocate(cns%prod10n )
    deallocate(cns%prod100n )
    deallocate(cns%totprodn )
    deallocate(cns%totlitn )
    deallocate(cns%totsomn )
    deallocate(cns%totecosysn )
    deallocate(cns%totcoln )
  end subroutine dealloc_column_nstate_type
  subroutine dealloc_column_eflux_type( cef)
    implicit none
    type (column_eflux_type), intent(inout):: cef
    deallocate(cef%eflx_snomelt )
    deallocate(cef%eflx_snomelt_u )
    deallocate(cef%eflx_snomelt_r )
    deallocate(cef%eflx_impsoil )
    deallocate(cef%eflx_fgr12 )
    deallocate(cef%eflx_building_heat )
    deallocate(cef%eflx_urban_ac )
    deallocate(cef%eflx_urban_heat )
  end subroutine dealloc_column_eflux_type
  subroutine dealloc_column_wflux_type( cwf)
    implicit none
    type (column_wflux_type), intent(inout):: cwf
    deallocate(cwf%qflx_infl )
    deallocate(cwf%qflx_surf )
    deallocate(cwf%qflx_drain )
    deallocate(cwf%qflx_top_soil )
    deallocate(cwf%qflx_snomelt )
    deallocate(cwf%qflx_qrgwl )
    deallocate(cwf%qflx_runoff )
    deallocate(cwf%qflx_runoff_u )
    deallocate(cwf%qflx_runoff_r )
    deallocate(cwf%qmelt )
    deallocate(cwf%h2ocan_loss )
    deallocate(cwf%qflx_rsub_sat )
    deallocate(cwf%flx_bc_dep_dry )
    deallocate(cwf%flx_bc_dep_wet )
    deallocate(cwf%flx_bc_dep_pho )
    deallocate(cwf%flx_bc_dep_phi )
    deallocate(cwf%flx_bc_dep )
    deallocate(cwf%flx_oc_dep_dry )
    deallocate(cwf%flx_oc_dep_wet )
    deallocate(cwf%flx_oc_dep_pho )
    deallocate(cwf%flx_oc_dep_phi )
    deallocate(cwf%flx_oc_dep )
    deallocate(cwf%flx_dst_dep_dry1 )
    deallocate(cwf%flx_dst_dep_wet1 )
    deallocate(cwf%flx_dst_dep_dry2 )
    deallocate(cwf%flx_dst_dep_wet2 )
    deallocate(cwf%flx_dst_dep_dry3 )
    deallocate(cwf%flx_dst_dep_wet3 )
    deallocate(cwf%flx_dst_dep_dry4 )
    deallocate(cwf%flx_dst_dep_wet4 )
    deallocate(cwf%flx_dst_dep )
    deallocate(cwf%qflx_snofrz_lyr)
  end subroutine dealloc_column_wflux_type
  subroutine dealloc_column_cflux_type( ccf)
    implicit none
    type (column_cflux_type), intent(inout):: ccf
    deallocate(ccf%m_leafc_to_litr1c )
    deallocate(ccf%m_leafc_to_litr2c )
    deallocate(ccf%m_leafc_to_litr3c )
    deallocate(ccf%m_frootc_to_litr1c )
    deallocate(ccf%m_frootc_to_litr2c )
    deallocate(ccf%m_frootc_to_litr3c )
    deallocate(ccf%m_leafc_storage_to_litr1c )
    deallocate(ccf%m_frootc_storage_to_litr1c )
    deallocate(ccf%m_livestemc_storage_to_litr1c )
    deallocate(ccf%m_deadstemc_storage_to_litr1c )
    deallocate(ccf%m_livecrootc_storage_to_litr1c )
    deallocate(ccf%m_deadcrootc_storage_to_litr1c )
    deallocate(ccf%m_leafc_xfer_to_litr1c )
    deallocate(ccf%m_frootc_xfer_to_litr1c )
    deallocate(ccf%m_livestemc_xfer_to_litr1c )
    deallocate(ccf%m_deadstemc_xfer_to_litr1c )
    deallocate(ccf%m_livecrootc_xfer_to_litr1c )
    deallocate(ccf%m_deadcrootc_xfer_to_litr1c )
    deallocate(ccf%m_livestemc_to_cwdc )
    deallocate(ccf%m_deadstemc_to_cwdc )
    deallocate(ccf%m_livecrootc_to_cwdc )
    deallocate(ccf%m_deadcrootc_to_cwdc )
    deallocate(ccf%m_gresp_storage_to_litr1c )
    deallocate(ccf%m_gresp_xfer_to_litr1c )
    deallocate(ccf%m_deadstemc_to_cwdc_fire )
    deallocate(ccf%m_deadcrootc_to_cwdc_fire )
    deallocate(ccf%hrv_leafc_to_litr1c )
    deallocate(ccf%hrv_leafc_to_litr2c )
    deallocate(ccf%hrv_leafc_to_litr3c )
    deallocate(ccf%hrv_frootc_to_litr1c )
    deallocate(ccf%hrv_frootc_to_litr2c )
    deallocate(ccf%hrv_frootc_to_litr3c )
    deallocate(ccf%hrv_livestemc_to_cwdc )
    deallocate(ccf%hrv_deadstemc_to_prod10c )
    deallocate(ccf%hrv_deadstemc_to_prod100c )
    deallocate(ccf%hrv_livecrootc_to_cwdc )
    deallocate(ccf%hrv_deadcrootc_to_cwdc )
    deallocate(ccf%hrv_leafc_storage_to_litr1c )
    deallocate(ccf%hrv_frootc_storage_to_litr1c )
    deallocate(ccf%hrv_livestemc_storage_to_litr1c )
    deallocate(ccf%hrv_deadstemc_storage_to_litr1c )
    deallocate(ccf%hrv_livecrootc_storage_to_litr1c )
    deallocate(ccf%hrv_deadcrootc_storage_to_litr1c )
    deallocate(ccf%hrv_gresp_storage_to_litr1c )
    deallocate(ccf%hrv_leafc_xfer_to_litr1c )
    deallocate(ccf%hrv_frootc_xfer_to_litr1c )
    deallocate(ccf%hrv_livestemc_xfer_to_litr1c )
    deallocate(ccf%hrv_deadstemc_xfer_to_litr1c )
    deallocate(ccf%hrv_livecrootc_xfer_to_litr1c )
    deallocate(ccf%hrv_deadcrootc_xfer_to_litr1c )
    deallocate(ccf%hrv_gresp_xfer_to_litr1c )
    deallocate(ccf%m_litr1c_to_fire )
    deallocate(ccf%m_litr2c_to_fire )
    deallocate(ccf%m_litr3c_to_fire )
    deallocate(ccf%m_cwdc_to_fire )
    deallocate(ccf%leafc_to_litr1c )
    deallocate(ccf%leafc_to_litr2c )
    deallocate(ccf%leafc_to_litr3c )
    deallocate(ccf%frootc_to_litr1c )
    deallocate(ccf%frootc_to_litr2c )
    deallocate(ccf%frootc_to_litr3c )
    deallocate(ccf%cwdc_to_litr2c )
    deallocate(ccf%cwdc_to_litr3c )
    deallocate(ccf%litr1_hr )
    deallocate(ccf%litr1c_to_soil1c )
    deallocate(ccf%litr2_hr )
    deallocate(ccf%litr2c_to_soil2c )
    deallocate(ccf%litr3_hr )
    deallocate(ccf%litr3c_to_soil3c )
    deallocate(ccf%soil1_hr )
    deallocate(ccf%soil1c_to_soil2c )
    deallocate(ccf%soil2_hr )
    deallocate(ccf%soil2c_to_soil3c )
    deallocate(ccf%soil3_hr )
    deallocate(ccf%soil3c_to_soil4c )
    deallocate(ccf%soil4_hr )
    deallocate(ccf%lithr )
    deallocate(ccf%somhr )
    deallocate(ccf%hr )
    deallocate(ccf%sr )
    deallocate(ccf%er )
    deallocate(ccf%litfire )
    deallocate(ccf%somfire )
    deallocate(ccf%totfire )
    deallocate(ccf%nep )
    deallocate(ccf%nbp )
    deallocate(ccf%nee )
    deallocate(ccf%col_cinputs )
    deallocate(ccf%col_coutputs )
    deallocate(ccf%col_fire_closs )
 end subroutine dealloc_column_cflux_type
  subroutine dealloc_column_nflux_type( cnf)
    implicit none
    type (column_nflux_type), intent(inout):: cnf
    deallocate(cnf%ndep_to_sminn )
    deallocate(cnf%nfix_to_sminn )
    deallocate(cnf%m_leafn_to_litr1n )
    deallocate(cnf%m_leafn_to_litr2n )
    deallocate(cnf%m_leafn_to_litr3n )
    deallocate(cnf%m_frootn_to_litr1n )
    deallocate(cnf%m_frootn_to_litr2n )
    deallocate(cnf%m_frootn_to_litr3n )
    deallocate(cnf%m_leafn_storage_to_litr1n )
    deallocate(cnf%m_frootn_storage_to_litr1n )
    deallocate(cnf%m_livestemn_storage_to_litr1n )
    deallocate(cnf%m_deadstemn_storage_to_litr1n )
    deallocate(cnf%m_livecrootn_storage_to_litr1n )
    deallocate(cnf%m_deadcrootn_storage_to_litr1n )
    deallocate(cnf%m_leafn_xfer_to_litr1n )
    deallocate(cnf%m_frootn_xfer_to_litr1n )
    deallocate(cnf%m_livestemn_xfer_to_litr1n )
    deallocate(cnf%m_deadstemn_xfer_to_litr1n )
    deallocate(cnf%m_livecrootn_xfer_to_litr1n )
    deallocate(cnf%m_deadcrootn_xfer_to_litr1n )
    deallocate(cnf%m_livestemn_to_cwdn )
    deallocate(cnf%m_deadstemn_to_cwdn )
    deallocate(cnf%m_livecrootn_to_cwdn )
    deallocate(cnf%m_deadcrootn_to_cwdn )
    deallocate(cnf%m_retransn_to_litr1n )
    deallocate(cnf%hrv_leafn_to_litr1n )
    deallocate(cnf%hrv_leafn_to_litr2n )
    deallocate(cnf%hrv_leafn_to_litr3n )
    deallocate(cnf%hrv_frootn_to_litr1n )
    deallocate(cnf%hrv_frootn_to_litr2n )
    deallocate(cnf%hrv_frootn_to_litr3n )
    deallocate(cnf%hrv_livestemn_to_cwdn )
    deallocate(cnf%hrv_deadstemn_to_prod10n )
    deallocate(cnf%hrv_deadstemn_to_prod100n )
    deallocate(cnf%hrv_livecrootn_to_cwdn )
    deallocate(cnf%hrv_deadcrootn_to_cwdn )
    deallocate(cnf%hrv_retransn_to_litr1n )
    deallocate(cnf%hrv_leafn_storage_to_litr1n )
    deallocate(cnf%hrv_frootn_storage_to_litr1n )
    deallocate(cnf%hrv_livestemn_storage_to_litr1n )
    deallocate(cnf%hrv_deadstemn_storage_to_litr1n )
    deallocate(cnf%hrv_livecrootn_storage_to_litr1n )
    deallocate(cnf%hrv_deadcrootn_storage_to_litr1n )
    deallocate(cnf%hrv_leafn_xfer_to_litr1n )
    deallocate(cnf%hrv_frootn_xfer_to_litr1n )
    deallocate(cnf%hrv_livestemn_xfer_to_litr1n )
    deallocate(cnf%hrv_deadstemn_xfer_to_litr1n )
    deallocate(cnf%hrv_livecrootn_xfer_to_litr1n )
    deallocate(cnf%hrv_deadcrootn_xfer_to_litr1n )
    deallocate(cnf%m_deadstemn_to_cwdn_fire )
    deallocate(cnf%m_deadcrootn_to_cwdn_fire )
    deallocate(cnf%m_litr1n_to_fire )
    deallocate(cnf%m_litr2n_to_fire )
    deallocate(cnf%m_litr3n_to_fire )
    deallocate(cnf%m_cwdn_to_fire )
    deallocate(cnf%leafn_to_litr1n )
    deallocate(cnf%leafn_to_litr2n )
    deallocate(cnf%leafn_to_litr3n )
    deallocate(cnf%frootn_to_litr1n )
    deallocate(cnf%frootn_to_litr2n )
    deallocate(cnf%frootn_to_litr3n )
    deallocate(cnf%cwdn_to_litr2n )
    deallocate(cnf%cwdn_to_litr3n )
    deallocate(cnf%litr1n_to_soil1n )
    deallocate(cnf%sminn_to_soil1n_l1 )
    deallocate(cnf%litr2n_to_soil2n )
    deallocate(cnf%sminn_to_soil2n_l2 )
    deallocate(cnf%litr3n_to_soil3n )
    deallocate(cnf%sminn_to_soil3n_l3 )
    deallocate(cnf%soil1n_to_soil2n )
    deallocate(cnf%sminn_to_soil2n_s1 )
    deallocate(cnf%soil2n_to_soil3n )
    deallocate(cnf%sminn_to_soil3n_s2 )
    deallocate(cnf%soil3n_to_soil4n )
    deallocate(cnf%sminn_to_soil4n_s3 )
    deallocate(cnf%soil4n_to_sminn )
    deallocate(cnf%sminn_to_denit_l1s1 )
    deallocate(cnf%sminn_to_denit_l2s2 )
    deallocate(cnf%sminn_to_denit_l3s3 )
    deallocate(cnf%sminn_to_denit_s1s2 )
    deallocate(cnf%sminn_to_denit_s2s3 )
    deallocate(cnf%sminn_to_denit_s3s4 )
    deallocate(cnf%sminn_to_denit_s4 )
    deallocate(cnf%sminn_to_denit_excess )
    deallocate(cnf%sminn_leached )
    deallocate(cnf%dwt_seedn_to_leaf )
    deallocate(cnf%dwt_seedn_to_deadstem )
    deallocate(cnf%dwt_conv_nflux )
    deallocate(cnf%dwt_prod10n_gain )
    deallocate(cnf%dwt_prod100n_gain )
    deallocate(cnf%dwt_frootn_to_litr1n )
    deallocate(cnf%dwt_frootn_to_litr2n )
    deallocate(cnf%dwt_frootn_to_litr3n )
    deallocate(cnf%dwt_livecrootn_to_cwdn )
    deallocate(cnf%dwt_deadcrootn_to_cwdn )
    deallocate(cnf%dwt_nloss )
    deallocate(cnf%prod10n_loss )
    deallocate(cnf%prod100n_loss )
    deallocate(cnf%product_nloss )
    deallocate(cnf%potential_immob )
    deallocate(cnf%actual_immob )
    deallocate(cnf%sminn_to_plant )
    deallocate(cnf%supplement_to_sminn )
    deallocate(cnf%gross_nmin )
    deallocate(cnf%net_nmin )
    deallocate(cnf%denit )
    deallocate(cnf%col_ninputs )
    deallocate(cnf%col_noutputs )
   deallocate(cnf%col_fire_nloss )
  end subroutine dealloc_column_nflux_type
  subroutine dealloc_landunit_pstate_type( lps)
    implicit none
    type (landunit_pstate_type), intent(inout):: lps
    deallocate(lps%t_building )
    deallocate(lps%t_building_max )
    deallocate(lps%t_building_min )
    deallocate(lps%tk_wall)
    deallocate(lps%tk_roof)
    deallocate(lps%tk_improad)
    deallocate(lps%cv_wall)
    deallocate(lps%cv_roof)
    deallocate(lps%cv_improad)
    deallocate(lps%thick_wall )
    deallocate(lps%thick_roof )
    deallocate(lps%nlev_improad )
    deallocate(lps%vf_sr )
    deallocate(lps%vf_wr )
    deallocate(lps%vf_sw )
    deallocate(lps%vf_rw )
    deallocate(lps%vf_ww )
    deallocate(lps%taf )
    deallocate(lps%qaf )
    deallocate(lps%sabs_roof_dir )
    deallocate(lps%sabs_roof_dif )
    deallocate(lps%sabs_sunwall_dir )
    deallocate(lps%sabs_sunwall_dif )
    deallocate(lps%sabs_shadewall_dir )
    deallocate(lps%sabs_shadewall_dif )
    deallocate(lps%sabs_improad_dir )
    deallocate(lps%sabs_improad_dif )
    deallocate(lps%sabs_perroad_dir )
    deallocate(lps%sabs_perroad_dif )
  end subroutine dealloc_landunit_pstate_type
  subroutine dealloc_landunit_eflux_type( lef)
    implicit none
    type (landunit_eflux_type), intent(inout):: lef
    deallocate(lef%eflx_traffic )
    deallocate(lef%eflx_traffic_factor )
    deallocate(lef%eflx_wasteheat )
    deallocate(lef%eflx_heat_from_ac )
  end subroutine dealloc_landunit_eflux_type
  subroutine dealloc_gridcell_pstate_type( gps)
    implicit none
    type (gridcell_pstate_type), intent(inout):: gps
  end subroutine dealloc_gridcell_pstate_type
 subroutine dealloc_gridcell_efstate_type( gve)
    implicit none
    type (gridcell_efstate_type), intent(inout) :: gve
    deallocate(gve%efisop)
  end subroutine dealloc_gridcell_efstate_type
  subroutine dealloc_gridcell_wflux_type( gwf)
    implicit none
    type (gridcell_wflux_type), intent(inout):: gwf
    deallocate(gwf%qflx_runoffg )
    deallocate(gwf%qflx_snwcp_iceg )
    deallocate(gwf%qflx_liq_dynbal )
    deallocate(gwf%qflx_ice_dynbal )
  end subroutine dealloc_gridcell_wflux_type
  subroutine dealloc_gridcell_eflux_type( gef)
    implicit none
    type (gridcell_eflux_type), intent(inout):: gef
    deallocate(gef%eflx_sh_totg )
    deallocate(gef%eflx_dynbal )
  end subroutine dealloc_gridcell_eflux_type
  subroutine dealloc_gridcell_wstate_type( gws)
    implicit none
    type (gridcell_wstate_type), intent(inout):: gws
    deallocate(gws%gc_liq1 )
    deallocate(gws%gc_liq2 )
    deallocate(gws%gc_ice1 )
    deallocate(gws%gc_ice2 )
  end subroutine dealloc_gridcell_wstate_type
  subroutine dealloc_gridcell_estate_type( ges)
    implicit none
    type (gridcell_estate_type), intent(inout):: ges
    deallocate(ges%gc_heat1 )
    deallocate(ges%gc_heat2 )
  end subroutine dealloc_gridcell_estate_type
   subroutine dealloc_atm2lnd_type(a2l)
    implicit none
  type (atm2lnd_type), intent(inout):: a2l
  deallocate(a2l%forc_t)
  deallocate(a2l%forc_u)
  deallocate(a2l%forc_v)
  deallocate(a2l%forc_wind)
  deallocate(a2l%forc_q)
  deallocate(a2l%forc_rh)
  deallocate(a2l%forc_hgt)
  deallocate(a2l%forc_hgt_u)
  deallocate(a2l%forc_hgt_t)
  deallocate(a2l%forc_hgt_q)
  deallocate(a2l%forc_pbot)
  deallocate(a2l%forc_th)
  deallocate(a2l%forc_vp)
  deallocate(a2l%forc_rho)
  deallocate(a2l%forc_psrf)
  deallocate(a2l%forc_pco2)
  deallocate(a2l%forc_lwrad)
  deallocate(a2l%forc_solad)
  deallocate(a2l%forc_solai)
  deallocate(a2l%forc_solar)
  deallocate(a2l%forc_rain)
  deallocate(a2l%forc_snow)
  deallocate(a2l%forc_ndep)
  deallocate(a2l%rainf)
  deallocate(a2l%forc_po2)
  deallocate(a2l%forc_aer)
 end subroutine dealloc_atm2lnd_type
end module clmtypeInitMod
module pftvarcon
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar,only : numpft, numrad
  implicit none
  save
  integer,parameter :: noveg = 0
  integer,parameter :: ndllf_evr_tmp_tree = 1
  integer,parameter :: ndllf_evr_brl_tree = 2
  integer,parameter :: ndllf_dcd_brl_tree = 3
  integer,parameter :: nbrdlf_evr_trp_tree = 4
  integer,parameter :: nbrdlf_evr_tmp_tree = 5
  integer,parameter :: nbrdlf_dcd_trp_tree = 6
  integer,parameter :: nbrdlf_dcd_tmp_tree = 7
  integer,parameter :: nbrdlf_dcd_brl_tree = 8
  integer :: ntree
  integer,parameter :: nbrdlf_evr_shrub = 9
  integer,parameter :: nbrdlf_dcd_tmp_shrub = 10
  integer,parameter :: nbrdlf_dcd_brl_shrub = 11
  integer,parameter :: nc3_arctic_grass = 12
  integer,parameter :: nc3_nonarctic_grass = 13
  integer,parameter :: nc4_grass = 14
  integer,parameter :: nc3crop = 15
  integer,parameter :: nc4crop = 16
  real(r8):: crop(0:numpft)
  character(len=40) pftname(0:numpft)
  real(r8):: dleaf(0:numpft)
  real(r8):: c3psn(0:numpft)
  real(r8):: vcmx25(0:numpft)
  real(r8):: mp(0:numpft)
  real(r8):: qe25(0:numpft)
  real(r8):: xl(0:numpft)
  real(r8):: rhol(0:numpft,numrad)
  real(r8):: rhos(0:numpft,numrad)
  real(r8):: taul(0:numpft,numrad)
  real(r8):: taus(0:numpft,numrad)
  real(r8):: z0mr(0:numpft)
  real(r8):: displar(0:numpft)
  real(r8):: roota_par(0:numpft)
  real(r8):: rootb_par(0:numpft)
  real(r8):: slatop(0:numpft)
  real(r8):: dsladlai(0:numpft)
  real(r8):: leafcn(0:numpft)
  real(r8):: flnr(0:numpft)
  real(r8):: smpso(0:numpft)
  real(r8):: smpsc(0:numpft)
  real(r8):: fnitr(0:numpft)
  real(r8):: woody(0:numpft)
  real(r8):: lflitcn(0:numpft)
  real(r8):: frootcn(0:numpft)
  real(r8):: livewdcn(0:numpft)
  real(r8):: deadwdcn(0:numpft)
  real(r8):: froot_leaf(0:numpft)
  real(r8):: stem_leaf(0:numpft)
  real(r8):: croot_stem(0:numpft)
  real(r8):: flivewd(0:numpft)
  real(r8):: fcur(0:numpft)
  real(r8):: lf_flab(0:numpft)
  real(r8):: lf_fcel(0:numpft)
  real(r8):: lf_flig(0:numpft)
  real(r8):: fr_flab(0:numpft)
  real(r8):: fr_fcel(0:numpft)
  real(r8):: fr_flig(0:numpft)
  real(r8):: dw_fcel(0:numpft)
  real(r8):: dw_flig(0:numpft)
  real(r8):: leaf_long(0:numpft)
  real(r8):: evergreen(0:numpft)
  real(r8):: stress_decid(0:numpft)
  real(r8):: season_decid(0:numpft)
  real(r8):: resist(0:numpft)
  real(r8):: pftpar20(0:numpft)
  real(r8):: pftpar28(0:numpft)
  real(r8):: pftpar29(0:numpft)
  real(r8):: pftpar30(0:numpft)
  real(r8):: pftpar31(0:numpft)
  real(r8):: graincn(0:numpft)
  real(r8), parameter :: reinickerp = 1.6_r8
  real(r8), parameter :: dwood = 2.5e5_r8
  real(r8), parameter :: allom1 = 100.0_r8
  real(r8), parameter :: allom2 = 40.0_r8
  real(r8), parameter :: allom3 = 0.5_r8
  real(r8), parameter :: allom1s = 250.0_r8
  real(r8), parameter :: allom2s = 8.0_r8
    character(len=40) expected_pftnames(0:numpft)
    integer, private :: i
   data (expected_pftnames(i),i=1,numpft) / &
                 'needleleaf_evergreen_temperate_tree' &
               , 'needleleaf_evergreen_boreal_tree   ' &
               , 'needleleaf_deciduous_boreal_tree   ' &
               , 'broadleaf_evergreen_tropical_tree  ' &
               , 'broadleaf_evergreen_temperate_tree ' &
               , 'broadleaf_deciduous_tropical_tree  ' &
               , 'broadleaf_deciduous_temperate_tree ' &
               , 'broadleaf_deciduous_boreal_tree    ' &
               , 'broadleaf_evergreen_shrub          ' &
               , 'broadleaf_deciduous_temperate_shrub' &
               , 'broadleaf_deciduous_boreal_shrub   ' &
               , 'c3_arctic_grass                    ' &
               , 'c3_non-arctic_grass                ' &
               , 'c4_grass                           ' &
               , 'c3_crop                            ' &
               , 'c4_crop                            ' &
    /
data (pftname(i),i=1,numpft)/'needleleaf_evergreen_temperate_tree'&
               , 'needleleaf_evergreen_boreal_tree   ' &
               , 'needleleaf_deciduous_boreal_tree   ' &
               , 'broadleaf_evergreen_tropical_tree  ' &
               , 'broadleaf_evergreen_temperate_tree ' &
               , 'broadleaf_deciduous_tropical_tree  ' &
               , 'broadleaf_deciduous_temperate_tree ' &
               , 'broadleaf_deciduous_boreal_tree    ' &
               , 'broadleaf_evergreen_shrub          ' &
               , 'broadleaf_deciduous_temperate_shrub' &
               , 'broadleaf_deciduous_boreal_shrub   ' &
               , 'c3_arctic_grass                    ' &
               , 'c3_non-arctic_grass                ' &
               , 'c4_grass                           ' &
               , 'c3_crop                            ' &
               , 'c4_crop                            ' &
    /
  data (z0mr(i),i=1,numpft)/ 0.055,0.055,0.055,0.075,0.075,&
         0.055,0.055,0.055,0.120,0.120,0.120,0.120,0.120,&
         0.120,0.120,0.120&
         /
  data (displar(i),i=1,numpft)/0.67,0.67,0.67,0.67,0.67,0.67,&
         0.67,0.67,0.68,0.68,0.68,0.68,0.68,0.68,0.68,0.68&
         /
  data (dleaf(i),i=1,numpft)/ 0.04,0.04,0.04,0.04,0.04,0.04,&
         0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04&
         /
  data (c3psn(i),i=1,numpft)/1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1&
         /
  data (vcmx25(i),i=1,numpft)/51,43,51,75,69,40,51,51,17,17,&
         33,43,43,24,50,50&
         /
  data (mp(i),i=1,numpft)/6,6,6,9,9,9,9,9,9,9,9,9,9,5,9,9&
         /
  data (qe25(i),i=1,numpft)/ 0.06,0.06,0.06,0.06,0.06,0.06,&
       0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.04,0.06,0.06&
         /
  data (rhol(i,1),i=1,numpft)/0.07,0.07,0.07,0.10,0.10,0.10,&
       0.10,0.10,0.07,0.10,0.10,0.11,0.11,0.11,0.11,0.11&
         /
  data (rhol(i,2),i=1,numpft)/ 0.35,0.35,0.35,0.45,0.45,0.45,&
        0.45,0.45,0.35,0.45,0.45,0.35,0.35,0.35,0.35,0.35&
         /
  data (rhos(i,1),i=1,numpft) /0.16,0.16,0.16,0.16,0.16,0.16,&
       0.16,0.16,0.16,0.16,0.16,0.31,0.31,0.31,0.31,0.31&
         /
  data (rhos(i,2),i=1,numpft)/0.39,0.39,0.39,0.39,0.39,0.39,&
       0.39,0.39,0.39,0.39,0.39,0.53,0.53,0.53,0.53,0.53&
         /
  data (taul(i,1),i=1,numpft)/0.05,0.05,0.05,0.05,0.05,0.05,&
       0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05&
         /
  data (taul(i,2),i=1,numpft)/0.10,0.10,0.10,0.25,0.25,0.25,&
      0.25,0.25,0.10,0.25,0.25,0.34,0.34,0.34,0.34,0.34&
         /
  data (taus(i,1),i=1,numpft)/0.001,0.001,0.001,0.001,0.001,&
      0.001,0.001,0.001,0.001,0.001,0.001,0.120,0.120,0.120,0.120,0.120&
         /
  data (taus(i,2),i=1,numpft)/ 0.001,0.001,0.001,0.001,0.001,0.001,&
     0.001,0.001,0.001,0.001,0.001,0.250,0.250,0.250,0.250,0.250&
         /
  data (xl(i),i=1,numpft)/0.01, 0.01, 0.01, 0.10, 0.10, 0.01,&
     0.25, 0.25, 0.01, 0.25, 0.25,-0.30,-0.30,-0.30,-0.30,-0.30&
         /
  data (roota_par(i),i=1,numpft)/ 7, 7, 7, 7, 7, 6, 6, 6, 7,&
       7, 7,11,11,11, 6, 6&
         /
  data (rootb_par(i),i=1,numpft)/ 2.0,2.0,2.0,1.0,1.0,2.0,2.0,&
       2.0,1.5,1.5,1.5,2.0,2.0,2.0,3.0,3.0&
         /
  data (slatop(i),i=1,numpft)/0.010,0.008,0.024,0.012,0.012,0.030,&
       0.030,0.030,0.012,0.030,0.030,0.030,0.030,0.030,0.030,0.030&
         /
  data (dsladlai(i),i=1,numpft)/0.00125,0.00100,0.00300,0.00150,0.00150,&
       0.00400,0.00400,0.00400,0.00000,0.00000,0.00000,0.00000,0.00000,&
       0.00000,0.00000,0.00000&
         /
  data (leafcn(i),i=1,numpft)/35,40,25,30,30,25,25,25,30,25,25,&
       25,25,25,25,25&
         /
 data (flnr(i),i=1,numpft)/0.05,0.04,0.08,0.06,0.06,0.09,0.09,0.09,&
      0.06,0.09,0.09,0.09,0.09,0.09,0.10,0.10&
         /
 data (smpso(i),i=1,numpft)/-66000,-66000,-66000,-66000,-66000,-35000,&
     -35000,-35000,-83000,-83000,-83000,-74000,-74000,-74000,-74000,-74000&
         /
 data (smpsc(i),i=1,numpft)/-255000,-255000,-255000,-255000,-255000,-224000,&
     -224000,-224000,-428000,-428000,-428000,-275000,-275000,-275000,-275000,-275000&
         /
 data(fnitr(i),i=1,numpft)/0.72,0.78,0.79,0.83,0.71,0.66,0.64,0.70,0.62,&
     0.60,0.76,0.68,0.61,0.64,0.61,0.61&
         /
 data(woody(i),i=1,numpft)/1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0&
         /
 data(lflitcn(i),i=1,numpft)/70,80,50,60,60,50,50,50,60,50,50,50,50,50,50,50&
         /
 data(frootcn(i),i=1,numpft)/42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42&
         /
 data(livewdcn(i),i=1,numpft)/50,50,50,50,50,50,50,50,50,50,50, 50, 50, 50, 50, 50&
         /
 data(deadwdcn(i),i=1,numpft)/500,500,500,500,500,500,500,500,500,500,500,&
       500, 500, 500, 500, 500&
         /
 data(froot_leaf(i),i=1,numpft)/1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2&
         /
 data(stem_leaf(i),i=1,numpft)/-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,&
       0.2, 0.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0&
         /
 data(croot_stem(i),i=1,numpft)/0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,&
       0.3,0.0,0.0,0.0,0.0,0.0&
         /
 data(flivewd(i),i=1,numpft)/0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.5,0.5,0.1,&
       0.0,0.0,0.0,0.0,0.0&
         /
 data(fcur(i),i=1,numpft)/1,1,0,1,1,0,0,0,1,0,0,0,0,0,0,0&
         /
 data(lf_flab(i),i=1,numpft)/0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,&
      0.25,0.25,0.25,0.25,0.25,0.25,0.25&
         /
 data(lf_fcel(i),i=1,numpft)/0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,&
      0.5,0.5,0.5,0.5,0.5&
         /
 data(lf_flig(i),i=1,numpft)/0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,&
      0.25,0.25,0.25,0.25,0.25,0.25,0.25&
         /
 data(fr_flab(i),i=1,numpft)/0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,&
      0.25,0.25,0.25,0.25,0.25,0.25,0.25&
         /
 data(fr_fcel(i),i=1,numpft)/0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,&
      0.5,0.5,0.5,0.5,0.5&
         /
 data(fr_flig(i),i=1,numpft)/0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,&
      0.25,0.25,0.25,0.25,0.25,0.25,0.25&
         /
 data(dw_fcel(i),i=1,numpft)/0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,&
      0.75,0.75,0.75,0.75,0.75,0.75,0.75&
         /
 data(dw_flig(i),i=1,numpft)/0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,&
      0.25,0.25,0.25,0.25,0.25,0.25,0.25&
         /
 data(leaf_long(i),i=1,numpft)/3.0,6.0,1.0,1.5,1.5,1.0,1.0,1.0,1.5,1.0,1.0,&
       1.0,1.0,1.0,1.0,1.0&
         /
 data(evergreen(i),i=1,numpft)/1,1,0,1,1,0,0,0,1,0,0,0,0,0,0,0&
         /
 data(stress_decid(i),i=1,numpft)/0,0,0,0,0,1,0,0,0,1,1,1,1,1,1,1&
         /
 data(season_decid(i),i=1,numpft)/0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0&
         /
 data(resist(i),i=1,numpft)/0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,&
     0.12,0.12,0.12,0.12,0.12,0.12,1.00,1.00&
         /
 data(pftpar20(i),i=1,numpft)/15,15,15,15,15,15,15,15, 5, 5, 5, 0, 0, 0, 0, 0&
         /
 data(pftpar28(i),i=1,numpft)/ -2.0, -32.5, 9999.9, 15.5, 3.0, 15.5,&
       -17.0,-1000.0, 9999.9, -17.0,-1000.0,-1000.0, -17.0,15.5, 9999.9, 9999.9&
         /
 data(pftpar29(i),i=1,numpft)/ 22.0, -2.0, -2.0,1000.0, 18.8,1000.0, 15.5,&
      -2.0,1000.0,1000.0, -2.0, -17.0, 15.5,1000.0,1000.0,1000.0&
         /
 data(pftpar30(i),i=1,numpft)/900, 600, 350, 0,1200, 0,1200, 350, 0,1200,&
       350, 0, 0, 0, 0, 0&
         /
 data(pftpar31(i),i=1,numpft)/1000, 23, 23,1000,1000,1000,1000, 23,1000,1000,&
       23,1000,1000,1000,1000,1000&
         /
 data(graincn(i),i=1,numpft)/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0&
         /
  public :: pftconrd
contains
  subroutine pftconrd
    use nanMod , only : inf
    implicit none
    integer :: i,n
    integer :: ier
    ntree = nbrdlf_dcd_brl_tree
    crop(:) = 0
    crop(nc3crop:numpft) = 1
    pftname(noveg) = 'not_vegetated'
       z0mr(noveg) = 0._r8
       displar(noveg) = 0._r8
       dleaf(noveg) = 0._r8
       c3psn(noveg) = 1._r8
       vcmx25(noveg) = 0._r8
       mp(noveg) = 9._r8
       qe25(noveg) = 0._r8
       rhol(noveg,1) = 0._r8
       rhol(noveg,2) = 0._r8
       rhos(noveg,1) = 0._r8
       rhos(noveg,2) = 0._r8
       taul(noveg,1) = 0._r8
       taul(noveg,2) = 0._r8
       taus(noveg,1) = 0._r8
       taus(noveg,2) = 0._r8
       xl(noveg) = 0._r8
       roota_par(noveg) = 0._r8
       rootb_par(noveg) = 0._r8
       crop(noveg) = 0._r8
       smpso(noveg) = 0._r8
       smpsc(noveg) = 0._r8
       fnitr(noveg) = 0._r8
       slatop(noveg) = 0._r8
       dsladlai(noveg) = 0._r8
       leafcn(noveg) = 1._r8
       flnr(noveg) = 0._r8
       woody(noveg) = 0._r8
       lflitcn(noveg) = 1._r8
       frootcn(noveg) = 1._r8
       livewdcn(noveg) = 1._r8
       deadwdcn(noveg) = 1._r8
       froot_leaf(noveg) = 0._r8
       stem_leaf(noveg) = 0._r8
       croot_stem(noveg) = 0._r8
       flivewd(noveg) = 0._r8
       fcur(noveg) = 0._r8
       lf_flab(noveg) = 0._r8
       lf_fcel(noveg) = 0._r8
       lf_flig(noveg) = 0._r8
       fr_flab(noveg) = 0._r8
       fr_fcel(noveg) = 0._r8
       fr_flig(noveg) = 0._r8
       dw_fcel(noveg) = 0._r8
       dw_flig(noveg) = 0._r8
       leaf_long(noveg) = 0._r8
       evergreen(noveg) = 0._r8
       stress_decid(noveg) = 0._r8
       season_decid(noveg) = 0._r8
       resist(noveg) = 1._r8
       pftpar20(noveg) = inf
       pftpar28(noveg) = 9999.9_r8
       pftpar29(noveg) = 1000.0_r8
       pftpar30(noveg) = 0.0_r8
       pftpar31(noveg) = 1000.0_r8
  end subroutine pftconrd
end module pftvarcon
module pftdynMod
  use clmtype
  use decompMod , only : get_proc_bounds
  use clm_varpar , only : max_pft_per_col
  use shr_kind_mod, only : r8 => shr_kind_r8
  use module_cam_support, only: endrun
  private
  save
  public :: pftdyn_init
  public :: pftdyn_interp
  public :: pftdyn_wbal_init
  public :: pftdyn_wbal
  real(r8), parameter :: days_per_year = 365._r8
  integer , pointer :: yearspft(:)
  real(r8), pointer :: wtpft1(:,:)
  real(r8), pointer :: wtpft2(:,:)
  real(r8), pointer :: harvest(:)
  real(r8), pointer :: wtcol_old(:)
  integer :: nt1
  integer :: nt2
  integer :: ntimes
  logical :: do_harvest
  integer :: ncid
contains
subroutine CNHarvest (num_soilc, filter_soilc, num_soilp, filter_soilp)
   use clmtype
   use pftvarcon, only : noveg, nbrdlf_evr_shrub
   implicit none
   integer, intent(in) :: num_soilc
   integer, intent(in) :: filter_soilc(:)
   integer, intent(in) :: num_soilp
   integer, intent(in) :: filter_soilp(:)
   integer , pointer :: pgridcell(:)
   integer , pointer :: ivt(:)
   real(r8), pointer :: leafc(:)
   real(r8), pointer :: frootc(:)
   real(r8), pointer :: livestemc(:)
   real(r8), pointer :: deadstemc(:)
   real(r8), pointer :: livecrootc(:)
   real(r8), pointer :: deadcrootc(:)
   real(r8), pointer :: xsmrpool(:)
   real(r8), pointer :: leafc_storage(:)
   real(r8), pointer :: frootc_storage(:)
   real(r8), pointer :: livestemc_storage(:)
   real(r8), pointer :: deadstemc_storage(:)
   real(r8), pointer :: livecrootc_storage(:)
   real(r8), pointer :: deadcrootc_storage(:)
   real(r8), pointer :: gresp_storage(:)
   real(r8), pointer :: leafc_xfer(:)
   real(r8), pointer :: frootc_xfer(:)
   real(r8), pointer :: livestemc_xfer(:)
   real(r8), pointer :: deadstemc_xfer(:)
   real(r8), pointer :: livecrootc_xfer(:)
   real(r8), pointer :: deadcrootc_xfer(:)
   real(r8), pointer :: gresp_xfer(:)
   real(r8), pointer :: leafn(:)
   real(r8), pointer :: frootn(:)
   real(r8), pointer :: livestemn(:)
   real(r8), pointer :: deadstemn(:)
   real(r8), pointer :: livecrootn(:)
   real(r8), pointer :: deadcrootn(:)
   real(r8), pointer :: retransn(:)
   real(r8), pointer :: leafn_storage(:)
   real(r8), pointer :: frootn_storage(:)
   real(r8), pointer :: livestemn_storage(:)
   real(r8), pointer :: deadstemn_storage(:)
   real(r8), pointer :: livecrootn_storage(:)
   real(r8), pointer :: deadcrootn_storage(:)
   real(r8), pointer :: leafn_xfer(:)
   real(r8), pointer :: frootn_xfer(:)
   real(r8), pointer :: livestemn_xfer(:)
   real(r8), pointer :: deadstemn_xfer(:)
   real(r8), pointer :: livecrootn_xfer(:)
   real(r8), pointer :: deadcrootn_xfer(:)
   real(r8), pointer :: hrv_leafc_to_litter(:)
   real(r8), pointer :: hrv_frootc_to_litter(:)
   real(r8), pointer :: hrv_livestemc_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_to_prod10c(:)
   real(r8), pointer :: hrv_deadstemc_to_prod100c(:)
   real(r8), pointer :: hrv_livecrootc_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_to_litter(:)
   real(r8), pointer :: hrv_xsmrpool_to_atm(:)
   real(r8), pointer :: hrv_leafc_storage_to_litter(:)
   real(r8), pointer :: hrv_frootc_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_gresp_storage_to_litter(:)
   real(r8), pointer :: hrv_leafc_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_gresp_xfer_to_litter(:)
   real(r8), pointer :: hrv_leafn_to_litter(:)
   real(r8), pointer :: hrv_frootn_to_litter(:)
   real(r8), pointer :: hrv_livestemn_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_to_prod10n(:)
   real(r8), pointer :: hrv_deadstemn_to_prod100n(:)
   real(r8), pointer :: hrv_livecrootn_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_to_litter(:)
   real(r8), pointer :: hrv_retransn_to_litter(:)
   real(r8), pointer :: hrv_leafn_storage_to_litter(:)
   real(r8), pointer :: hrv_frootn_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_leafn_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_xfer_to_litter(:)
   integer :: p
   integer :: g
   integer :: fp
   real(r8):: am
   real(r8):: m
   real(r8) :: pprod10(1:8)
   pgridcell => clm3%g%l%c%p%gridcell
   ivt => clm3%g%l%c%p%itype
   leafc => clm3%g%l%c%p%pcs%leafc
   frootc => clm3%g%l%c%p%pcs%frootc
   livestemc => clm3%g%l%c%p%pcs%livestemc
   deadstemc => clm3%g%l%c%p%pcs%deadstemc
   livecrootc => clm3%g%l%c%p%pcs%livecrootc
   deadcrootc => clm3%g%l%c%p%pcs%deadcrootc
   xsmrpool => clm3%g%l%c%p%pcs%xsmrpool
   leafc_storage => clm3%g%l%c%p%pcs%leafc_storage
   frootc_storage => clm3%g%l%c%p%pcs%frootc_storage
   livestemc_storage => clm3%g%l%c%p%pcs%livestemc_storage
   deadstemc_storage => clm3%g%l%c%p%pcs%deadstemc_storage
   livecrootc_storage => clm3%g%l%c%p%pcs%livecrootc_storage
   deadcrootc_storage => clm3%g%l%c%p%pcs%deadcrootc_storage
   gresp_storage => clm3%g%l%c%p%pcs%gresp_storage
   leafc_xfer => clm3%g%l%c%p%pcs%leafc_xfer
   frootc_xfer => clm3%g%l%c%p%pcs%frootc_xfer
   livestemc_xfer => clm3%g%l%c%p%pcs%livestemc_xfer
   deadstemc_xfer => clm3%g%l%c%p%pcs%deadstemc_xfer
   livecrootc_xfer => clm3%g%l%c%p%pcs%livecrootc_xfer
   deadcrootc_xfer => clm3%g%l%c%p%pcs%deadcrootc_xfer
   gresp_xfer => clm3%g%l%c%p%pcs%gresp_xfer
   leafn => clm3%g%l%c%p%pns%leafn
   frootn => clm3%g%l%c%p%pns%frootn
   livestemn => clm3%g%l%c%p%pns%livestemn
   deadstemn => clm3%g%l%c%p%pns%deadstemn
   livecrootn => clm3%g%l%c%p%pns%livecrootn
   deadcrootn => clm3%g%l%c%p%pns%deadcrootn
   retransn => clm3%g%l%c%p%pns%retransn
   leafn_storage => clm3%g%l%c%p%pns%leafn_storage
   frootn_storage => clm3%g%l%c%p%pns%frootn_storage
   livestemn_storage => clm3%g%l%c%p%pns%livestemn_storage
   deadstemn_storage => clm3%g%l%c%p%pns%deadstemn_storage
   livecrootn_storage => clm3%g%l%c%p%pns%livecrootn_storage
   deadcrootn_storage => clm3%g%l%c%p%pns%deadcrootn_storage
   leafn_xfer => clm3%g%l%c%p%pns%leafn_xfer
   frootn_xfer => clm3%g%l%c%p%pns%frootn_xfer
   livestemn_xfer => clm3%g%l%c%p%pns%livestemn_xfer
   deadstemn_xfer => clm3%g%l%c%p%pns%deadstemn_xfer
   livecrootn_xfer => clm3%g%l%c%p%pns%livecrootn_xfer
   deadcrootn_xfer => clm3%g%l%c%p%pns%deadcrootn_xfer
   hrv_leafc_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_to_litter
   hrv_frootc_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_to_litter
   hrv_livestemc_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_to_litter
   hrv_deadstemc_to_prod10c => clm3%g%l%c%p%pcf%hrv_deadstemc_to_prod10c
   hrv_deadstemc_to_prod100c => clm3%g%l%c%p%pcf%hrv_deadstemc_to_prod100c
   hrv_livecrootc_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_to_litter
   hrv_deadcrootc_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_to_litter
   hrv_xsmrpool_to_atm => clm3%g%l%c%p%pcf%hrv_xsmrpool_to_atm
   hrv_leafc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_storage_to_litter
   hrv_frootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_storage_to_litter
   hrv_livestemc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_storage_to_litter
   hrv_deadstemc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_deadstemc_storage_to_litter
   hrv_livecrootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_storage_to_litter
   hrv_deadcrootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_storage_to_litter
   hrv_gresp_storage_to_litter => clm3%g%l%c%p%pcf%hrv_gresp_storage_to_litter
   hrv_leafc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_xfer_to_litter
   hrv_frootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_xfer_to_litter
   hrv_livestemc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_xfer_to_litter
   hrv_deadstemc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_deadstemc_xfer_to_litter
   hrv_livecrootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_xfer_to_litter
   hrv_deadcrootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_xfer_to_litter
   hrv_gresp_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_gresp_xfer_to_litter
   hrv_leafn_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_to_litter
   hrv_frootn_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_to_litter
   hrv_livestemn_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_to_litter
   hrv_deadstemn_to_prod10n => clm3%g%l%c%p%pnf%hrv_deadstemn_to_prod10n
   hrv_deadstemn_to_prod100n => clm3%g%l%c%p%pnf%hrv_deadstemn_to_prod100n
   hrv_livecrootn_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_to_litter
   hrv_deadcrootn_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_to_litter
   hrv_retransn_to_litter => clm3%g%l%c%p%pnf%hrv_retransn_to_litter
   hrv_leafn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_storage_to_litter
   hrv_frootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_storage_to_litter
   hrv_livestemn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_storage_to_litter
   hrv_deadstemn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_deadstemn_storage_to_litter
   hrv_livecrootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_storage_to_litter
   hrv_deadcrootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_storage_to_litter
   hrv_leafn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_xfer_to_litter
   hrv_frootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_xfer_to_litter
   hrv_livestemn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_xfer_to_litter
   hrv_deadstemn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_deadstemn_xfer_to_litter
   hrv_livecrootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_xfer_to_litter
   hrv_deadcrootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_xfer_to_litter
   pprod10 = (/0.75_r8, 0.75_r8, 0.75_r8, 1.0_r8, 0.75_r8, 1.0_r8, 0.75_r8, 0.75_r8/)
   do fp = 1,num_soilp
      p = filter_soilp(fp)
      g = pgridcell(p)
      if (ivt(p) > noveg .and. ivt(p) < nbrdlf_evr_shrub) then
         if (do_harvest) then
            am = harvest(g)
            m = am/(365._r8 * 86400._r8)
         else
            m = 0._r8
         end if
         hrv_leafc_to_litter(p) = leafc(p) * m
         hrv_frootc_to_litter(p) = frootc(p) * m
         hrv_livestemc_to_litter(p) = livestemc(p) * m
         hrv_deadstemc_to_prod10c(p) = deadstemc(p) * m * pprod10(ivt(p))
         hrv_deadstemc_to_prod100c(p) = deadstemc(p) * m * (1.0_r8 - pprod10(ivt(p)))
         hrv_livecrootc_to_litter(p) = livecrootc(p) * m
         hrv_deadcrootc_to_litter(p) = deadcrootc(p) * m
         hrv_xsmrpool_to_atm(p) = xsmrpool(p) * m
         hrv_leafc_storage_to_litter(p) = leafc_storage(p) * m
         hrv_frootc_storage_to_litter(p) = frootc_storage(p) * m
         hrv_livestemc_storage_to_litter(p) = livestemc_storage(p) * m
         hrv_deadstemc_storage_to_litter(p) = deadstemc_storage(p) * m
         hrv_livecrootc_storage_to_litter(p) = livecrootc_storage(p) * m
         hrv_deadcrootc_storage_to_litter(p) = deadcrootc_storage(p) * m
         hrv_gresp_storage_to_litter(p) = gresp_storage(p) * m
         hrv_leafc_xfer_to_litter(p) = leafc_xfer(p) * m
         hrv_frootc_xfer_to_litter(p) = frootc_xfer(p) * m
         hrv_livestemc_xfer_to_litter(p) = livestemc_xfer(p) * m
         hrv_deadstemc_xfer_to_litter(p) = deadstemc_xfer(p) * m
         hrv_livecrootc_xfer_to_litter(p) = livecrootc_xfer(p) * m
         hrv_deadcrootc_xfer_to_litter(p) = deadcrootc_xfer(p) * m
         hrv_gresp_xfer_to_litter(p) = gresp_xfer(p) * m
         hrv_leafn_to_litter(p) = leafn(p) * m
         hrv_frootn_to_litter(p) = frootn(p) * m
         hrv_livestemn_to_litter(p) = livestemn(p) * m
         hrv_deadstemn_to_prod10n(p) = deadstemn(p) * m * pprod10(ivt(p))
         hrv_deadstemn_to_prod100n(p) = deadstemn(p) * m * (1.0_r8 - pprod10(ivt(p)))
         hrv_livecrootn_to_litter(p) = livecrootn(p) * m
         hrv_deadcrootn_to_litter(p) = deadcrootn(p) * m
         hrv_retransn_to_litter(p) = retransn(p) * m
         hrv_leafn_storage_to_litter(p) = leafn_storage(p) * m
         hrv_frootn_storage_to_litter(p) = frootn_storage(p) * m
         hrv_livestemn_storage_to_litter(p) = livestemn_storage(p) * m
         hrv_deadstemn_storage_to_litter(p) = deadstemn_storage(p) * m
         hrv_livecrootn_storage_to_litter(p) = livecrootn_storage(p) * m
         hrv_deadcrootn_storage_to_litter(p) = deadcrootn_storage(p) * m
         hrv_leafn_xfer_to_litter(p) = leafn_xfer(p) * m
         hrv_frootn_xfer_to_litter(p) = frootn_xfer(p) * m
         hrv_livestemn_xfer_to_litter(p) = livestemn_xfer(p) * m
         hrv_deadstemn_xfer_to_litter(p) = deadstemn_xfer(p) * m
         hrv_livecrootn_xfer_to_litter(p) = livecrootn_xfer(p) * m
         hrv_deadcrootn_xfer_to_litter(p) = deadcrootn_xfer(p) * m
      end if
   end do
   call CNHarvestPftToColumn(num_soilc, filter_soilc)
end subroutine CNHarvest
subroutine CNHarvestPftToColumn (num_soilc, filter_soilc)
  use clmtype
  use clm_varpar, only : max_pft_per_col, maxpatch_pft
  implicit none
  integer, intent(in) :: num_soilc
  integer, intent(in) :: filter_soilc(:)
   integer , pointer :: ivt(:)
   real(r8), pointer :: wtcol(:)
   real(r8), pointer :: pwtgcell(:)
   real(r8), pointer :: lf_flab(:)
   real(r8), pointer :: lf_fcel(:)
   real(r8), pointer :: lf_flig(:)
   real(r8), pointer :: fr_flab(:)
   real(r8), pointer :: fr_fcel(:)
   real(r8), pointer :: fr_flig(:)
   integer , pointer :: npfts(:)
   integer , pointer :: pfti(:)
   real(r8), pointer :: hrv_leafc_to_litter(:)
   real(r8), pointer :: hrv_frootc_to_litter(:)
   real(r8), pointer :: hrv_livestemc_to_litter(:)
   real(r8), pointer :: phrv_deadstemc_to_prod10c(:)
   real(r8), pointer :: phrv_deadstemc_to_prod100c(:)
   real(r8), pointer :: hrv_livecrootc_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_to_litter(:)
   real(r8), pointer :: hrv_leafc_storage_to_litter(:)
   real(r8), pointer :: hrv_frootc_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_storage_to_litter(:)
   real(r8), pointer :: hrv_gresp_storage_to_litter(:)
   real(r8), pointer :: hrv_leafc_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemc_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootc_xfer_to_litter(:)
   real(r8), pointer :: hrv_gresp_xfer_to_litter(:)
   real(r8), pointer :: hrv_leafn_to_litter(:)
   real(r8), pointer :: hrv_frootn_to_litter(:)
   real(r8), pointer :: hrv_livestemn_to_litter(:)
   real(r8), pointer :: phrv_deadstemn_to_prod10n(:)
   real(r8), pointer :: phrv_deadstemn_to_prod100n(:)
   real(r8), pointer :: hrv_livecrootn_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_to_litter(:)
   real(r8), pointer :: hrv_retransn_to_litter(:)
   real(r8), pointer :: hrv_leafn_storage_to_litter(:)
   real(r8), pointer :: hrv_frootn_storage_to_litter(:)
   real(r8), pointer :: hrv_livestemn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_storage_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_storage_to_litter(:)
   real(r8), pointer :: hrv_leafn_xfer_to_litter(:)
   real(r8), pointer :: hrv_frootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livestemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadstemn_xfer_to_litter(:)
   real(r8), pointer :: hrv_livecrootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_deadcrootn_xfer_to_litter(:)
   real(r8), pointer :: hrv_leafc_to_litr1c(:)
   real(r8), pointer :: hrv_leafc_to_litr2c(:)
   real(r8), pointer :: hrv_leafc_to_litr3c(:)
   real(r8), pointer :: hrv_frootc_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_to_litr2c(:)
   real(r8), pointer :: hrv_frootc_to_litr3c(:)
   real(r8), pointer :: hrv_livestemc_to_cwdc(:)
   real(r8), pointer :: chrv_deadstemc_to_prod10c(:)
   real(r8), pointer :: chrv_deadstemc_to_prod100c(:)
   real(r8), pointer :: hrv_livecrootc_to_cwdc(:)
   real(r8), pointer :: hrv_deadcrootc_to_cwdc(:)
   real(r8), pointer :: hrv_leafc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_livestemc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_deadstemc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_livecrootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_deadcrootc_storage_to_litr1c(:)
   real(r8), pointer :: hrv_gresp_storage_to_litr1c(:)
   real(r8), pointer :: hrv_leafc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_frootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_livestemc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_deadstemc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_livecrootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_deadcrootc_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_gresp_xfer_to_litr1c(:)
   real(r8), pointer :: hrv_leafn_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_to_litr2n(:)
   real(r8), pointer :: hrv_leafn_to_litr3n(:)
   real(r8), pointer :: hrv_frootn_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_to_litr2n(:)
   real(r8), pointer :: hrv_frootn_to_litr3n(:)
   real(r8), pointer :: hrv_livestemn_to_cwdn(:)
   real(r8), pointer :: chrv_deadstemn_to_prod10n(:)
   real(r8), pointer :: chrv_deadstemn_to_prod100n(:)
   real(r8), pointer :: hrv_livecrootn_to_cwdn(:)
   real(r8), pointer :: hrv_deadcrootn_to_cwdn(:)
   real(r8), pointer :: hrv_retransn_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_livestemn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_deadstemn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_livecrootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_deadcrootn_storage_to_litr1n(:)
   real(r8), pointer :: hrv_leafn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_frootn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_livestemn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_deadstemn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_livecrootn_xfer_to_litr1n(:)
   real(r8), pointer :: hrv_deadcrootn_xfer_to_litr1n(:)
   integer :: fc,c,pi,p
   lf_flab => pftcon%lf_flab
   lf_fcel => pftcon%lf_fcel
   lf_flig => pftcon%lf_flig
   fr_flab => pftcon%fr_flab
   fr_fcel => pftcon%fr_fcel
   fr_flig => pftcon%fr_flig
   npfts => clm3%g%l%c%npfts
   pfti => clm3%g%l%c%pfti
   hrv_leafc_to_litr1c => clm3%g%l%c%ccf%hrv_leafc_to_litr1c
   hrv_leafc_to_litr2c => clm3%g%l%c%ccf%hrv_leafc_to_litr2c
   hrv_leafc_to_litr3c => clm3%g%l%c%ccf%hrv_leafc_to_litr3c
   hrv_frootc_to_litr1c => clm3%g%l%c%ccf%hrv_frootc_to_litr1c
   hrv_frootc_to_litr2c => clm3%g%l%c%ccf%hrv_frootc_to_litr2c
   hrv_frootc_to_litr3c => clm3%g%l%c%ccf%hrv_frootc_to_litr3c
   hrv_livestemc_to_cwdc => clm3%g%l%c%ccf%hrv_livestemc_to_cwdc
   chrv_deadstemc_to_prod10c => clm3%g%l%c%ccf%hrv_deadstemc_to_prod10c
   chrv_deadstemc_to_prod100c => clm3%g%l%c%ccf%hrv_deadstemc_to_prod100c
   hrv_livecrootc_to_cwdc => clm3%g%l%c%ccf%hrv_livecrootc_to_cwdc
   hrv_deadcrootc_to_cwdc => clm3%g%l%c%ccf%hrv_deadcrootc_to_cwdc
   hrv_leafc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_leafc_storage_to_litr1c
   hrv_frootc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_frootc_storage_to_litr1c
   hrv_livestemc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_livestemc_storage_to_litr1c
   hrv_deadstemc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_deadstemc_storage_to_litr1c
   hrv_livecrootc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_livecrootc_storage_to_litr1c
   hrv_deadcrootc_storage_to_litr1c => clm3%g%l%c%ccf%hrv_deadcrootc_storage_to_litr1c
   hrv_gresp_storage_to_litr1c => clm3%g%l%c%ccf%hrv_gresp_storage_to_litr1c
   hrv_leafc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_leafc_xfer_to_litr1c
   hrv_frootc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_frootc_xfer_to_litr1c
   hrv_livestemc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_livestemc_xfer_to_litr1c
   hrv_deadstemc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_deadstemc_xfer_to_litr1c
   hrv_livecrootc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_livecrootc_xfer_to_litr1c
   hrv_deadcrootc_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_deadcrootc_xfer_to_litr1c
   hrv_gresp_xfer_to_litr1c => clm3%g%l%c%ccf%hrv_gresp_xfer_to_litr1c
   hrv_leafn_to_litr1n => clm3%g%l%c%cnf%hrv_leafn_to_litr1n
   hrv_leafn_to_litr2n => clm3%g%l%c%cnf%hrv_leafn_to_litr2n
   hrv_leafn_to_litr3n => clm3%g%l%c%cnf%hrv_leafn_to_litr3n
   hrv_frootn_to_litr1n => clm3%g%l%c%cnf%hrv_frootn_to_litr1n
   hrv_frootn_to_litr2n => clm3%g%l%c%cnf%hrv_frootn_to_litr2n
   hrv_frootn_to_litr3n => clm3%g%l%c%cnf%hrv_frootn_to_litr3n
   hrv_livestemn_to_cwdn => clm3%g%l%c%cnf%hrv_livestemn_to_cwdn
   chrv_deadstemn_to_prod10n => clm3%g%l%c%cnf%hrv_deadstemn_to_prod10n
   chrv_deadstemn_to_prod100n => clm3%g%l%c%cnf%hrv_deadstemn_to_prod100n
   hrv_livecrootn_to_cwdn => clm3%g%l%c%cnf%hrv_livecrootn_to_cwdn
   hrv_deadcrootn_to_cwdn => clm3%g%l%c%cnf%hrv_deadcrootn_to_cwdn
   hrv_retransn_to_litr1n => clm3%g%l%c%cnf%hrv_retransn_to_litr1n
   hrv_leafn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_leafn_storage_to_litr1n
   hrv_frootn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_frootn_storage_to_litr1n
   hrv_livestemn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_livestemn_storage_to_litr1n
   hrv_deadstemn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_deadstemn_storage_to_litr1n
   hrv_livecrootn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_livecrootn_storage_to_litr1n
   hrv_deadcrootn_storage_to_litr1n => clm3%g%l%c%cnf%hrv_deadcrootn_storage_to_litr1n
   hrv_leafn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_leafn_xfer_to_litr1n
   hrv_frootn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_frootn_xfer_to_litr1n
   hrv_livestemn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_livestemn_xfer_to_litr1n
   hrv_deadstemn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_deadstemn_xfer_to_litr1n
   hrv_livecrootn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_livecrootn_xfer_to_litr1n
   hrv_deadcrootn_xfer_to_litr1n => clm3%g%l%c%cnf%hrv_deadcrootn_xfer_to_litr1n
   ivt => clm3%g%l%c%p%itype
   wtcol => clm3%g%l%c%p%wtcol
   pwtgcell => clm3%g%l%c%p%wtgcell
   hrv_leafc_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_to_litter
   hrv_frootc_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_to_litter
   hrv_livestemc_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_to_litter
   phrv_deadstemc_to_prod10c => clm3%g%l%c%p%pcf%hrv_deadstemc_to_prod10c
   phrv_deadstemc_to_prod100c => clm3%g%l%c%p%pcf%hrv_deadstemc_to_prod100c
   hrv_livecrootc_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_to_litter
   hrv_deadcrootc_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_to_litter
   hrv_leafc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_storage_to_litter
   hrv_frootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_storage_to_litter
   hrv_livestemc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_storage_to_litter
   hrv_deadstemc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_deadstemc_storage_to_litter
   hrv_livecrootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_storage_to_litter
   hrv_deadcrootc_storage_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_storage_to_litter
   hrv_gresp_storage_to_litter => clm3%g%l%c%p%pcf%hrv_gresp_storage_to_litter
   hrv_leafc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_leafc_xfer_to_litter
   hrv_frootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_frootc_xfer_to_litter
   hrv_livestemc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_livestemc_xfer_to_litter
   hrv_deadstemc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_deadstemc_xfer_to_litter
   hrv_livecrootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_livecrootc_xfer_to_litter
   hrv_deadcrootc_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_deadcrootc_xfer_to_litter
   hrv_gresp_xfer_to_litter => clm3%g%l%c%p%pcf%hrv_gresp_xfer_to_litter
   hrv_leafn_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_to_litter
   hrv_frootn_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_to_litter
   hrv_livestemn_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_to_litter
   phrv_deadstemn_to_prod10n => clm3%g%l%c%p%pnf%hrv_deadstemn_to_prod10n
   phrv_deadstemn_to_prod100n => clm3%g%l%c%p%pnf%hrv_deadstemn_to_prod100n
   hrv_livecrootn_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_to_litter
   hrv_deadcrootn_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_to_litter
   hrv_retransn_to_litter => clm3%g%l%c%p%pnf%hrv_retransn_to_litter
   hrv_leafn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_storage_to_litter
   hrv_frootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_storage_to_litter
   hrv_livestemn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_storage_to_litter
   hrv_deadstemn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_deadstemn_storage_to_litter
   hrv_livecrootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_storage_to_litter
   hrv_deadcrootn_storage_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_storage_to_litter
   hrv_leafn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_leafn_xfer_to_litter
   hrv_frootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_frootn_xfer_to_litter
   hrv_livestemn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_livestemn_xfer_to_litter
   hrv_deadstemn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_deadstemn_xfer_to_litter
   hrv_livecrootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_livecrootn_xfer_to_litter
   hrv_deadcrootn_xfer_to_litter => clm3%g%l%c%p%pnf%hrv_deadcrootn_xfer_to_litter
   do pi = 1,maxpatch_pft
      do fc = 1,num_soilc
         c = filter_soilc(fc)
         if (pi <= npfts(c)) then
            p = pfti(c) + pi - 1
            if (pwtgcell(p)>0._r8) then
               hrv_leafc_to_litr1c(c) = hrv_leafc_to_litr1c(c) + &
                  hrv_leafc_to_litter(p) * lf_flab(ivt(p)) * wtcol(p)
               hrv_leafc_to_litr2c(c) = hrv_leafc_to_litr2c(c) + &
                  hrv_leafc_to_litter(p) * lf_fcel(ivt(p)) * wtcol(p)
               hrv_leafc_to_litr3c(c) = hrv_leafc_to_litr3c(c) + &
                  hrv_leafc_to_litter(p) * lf_flig(ivt(p)) * wtcol(p)
               hrv_frootc_to_litr1c(c) = hrv_frootc_to_litr1c(c) + &
                  hrv_frootc_to_litter(p) * fr_flab(ivt(p)) * wtcol(p)
               hrv_frootc_to_litr2c(c) = hrv_frootc_to_litr2c(c) + &
                  hrv_frootc_to_litter(p) * fr_fcel(ivt(p)) * wtcol(p)
               hrv_frootc_to_litr3c(c) = hrv_frootc_to_litr3c(c) + &
                  hrv_frootc_to_litter(p) * fr_flig(ivt(p)) * wtcol(p)
               hrv_livestemc_to_cwdc(c) = hrv_livestemc_to_cwdc(c) + &
                  hrv_livestemc_to_litter(p) * wtcol(p)
               chrv_deadstemc_to_prod10c(c) = chrv_deadstemc_to_prod10c(c) + &
                  phrv_deadstemc_to_prod10c(p) * wtcol(p)
               chrv_deadstemc_to_prod100c(c) = chrv_deadstemc_to_prod100c(c) + &
                  phrv_deadstemc_to_prod100c(p) * wtcol(p)
               hrv_livecrootc_to_cwdc(c) = hrv_livecrootc_to_cwdc(c) + &
                  hrv_livecrootc_to_litter(p) * wtcol(p)
               hrv_deadcrootc_to_cwdc(c) = hrv_deadcrootc_to_cwdc(c) + &
                  hrv_deadcrootc_to_litter(p) * wtcol(p)
               hrv_leafc_storage_to_litr1c(c) = hrv_leafc_storage_to_litr1c(c) + &
                  hrv_leafc_storage_to_litter(p) * wtcol(p)
               hrv_frootc_storage_to_litr1c(c) = hrv_frootc_storage_to_litr1c(c) + &
                  hrv_frootc_storage_to_litter(p) * wtcol(p)
               hrv_livestemc_storage_to_litr1c(c) = hrv_livestemc_storage_to_litr1c(c) + &
                  hrv_livestemc_storage_to_litter(p) * wtcol(p)
               hrv_deadstemc_storage_to_litr1c(c) = hrv_deadstemc_storage_to_litr1c(c) + &
                  hrv_deadstemc_storage_to_litter(p) * wtcol(p)
               hrv_livecrootc_storage_to_litr1c(c) = hrv_livecrootc_storage_to_litr1c(c) + &
                  hrv_livecrootc_storage_to_litter(p) * wtcol(p)
               hrv_deadcrootc_storage_to_litr1c(c) = hrv_deadcrootc_storage_to_litr1c(c) + &
                  hrv_deadcrootc_storage_to_litter(p) * wtcol(p)
               hrv_gresp_storage_to_litr1c(c) = hrv_gresp_storage_to_litr1c(c) + &
                  hrv_gresp_storage_to_litter(p) * wtcol(p)
               hrv_leafc_xfer_to_litr1c(c) = hrv_leafc_xfer_to_litr1c(c) + &
                  hrv_leafc_xfer_to_litter(p) * wtcol(p)
               hrv_frootc_xfer_to_litr1c(c) = hrv_frootc_xfer_to_litr1c(c) + &
                  hrv_frootc_xfer_to_litter(p) * wtcol(p)
               hrv_livestemc_xfer_to_litr1c(c) = hrv_livestemc_xfer_to_litr1c(c) + &
                  hrv_livestemc_xfer_to_litter(p) * wtcol(p)
               hrv_deadstemc_xfer_to_litr1c(c) = hrv_deadstemc_xfer_to_litr1c(c) + &
                  hrv_deadstemc_xfer_to_litter(p) * wtcol(p)
               hrv_livecrootc_xfer_to_litr1c(c) = hrv_livecrootc_xfer_to_litr1c(c) + &
                  hrv_livecrootc_xfer_to_litter(p) * wtcol(p)
               hrv_deadcrootc_xfer_to_litr1c(c) = hrv_deadcrootc_xfer_to_litr1c(c) + &
                  hrv_deadcrootc_xfer_to_litter(p) * wtcol(p)
               hrv_gresp_xfer_to_litr1c(c) = hrv_gresp_xfer_to_litr1c(c) + &
                  hrv_gresp_xfer_to_litter(p) * wtcol(p)
               hrv_leafn_to_litr1n(c) = hrv_leafn_to_litr1n(c) + &
                  hrv_leafn_to_litter(p) * lf_flab(ivt(p)) * wtcol(p)
               hrv_leafn_to_litr2n(c) = hrv_leafn_to_litr2n(c) + &
                  hrv_leafn_to_litter(p) * lf_fcel(ivt(p)) * wtcol(p)
               hrv_leafn_to_litr3n(c) = hrv_leafn_to_litr3n(c) + &
                  hrv_leafn_to_litter(p) * lf_flig(ivt(p)) * wtcol(p)
               hrv_frootn_to_litr1n(c) = hrv_frootn_to_litr1n(c) + &
                  hrv_frootn_to_litter(p) * fr_flab(ivt(p)) * wtcol(p)
               hrv_frootn_to_litr2n(c) = hrv_frootn_to_litr2n(c) + &
                  hrv_frootn_to_litter(p) * fr_fcel(ivt(p)) * wtcol(p)
               hrv_frootn_to_litr3n(c) = hrv_frootn_to_litr3n(c) + &
                  hrv_frootn_to_litter(p) * fr_flig(ivt(p)) * wtcol(p)
               hrv_livestemn_to_cwdn(c) = hrv_livestemn_to_cwdn(c) + &
                  hrv_livestemn_to_litter(p) * wtcol(p)
               chrv_deadstemn_to_prod10n(c) = chrv_deadstemn_to_prod10n(c) + &
                  phrv_deadstemn_to_prod10n(p) * wtcol(p)
               chrv_deadstemn_to_prod100n(c) = chrv_deadstemn_to_prod100n(c) + &
                  phrv_deadstemn_to_prod100n(p) * wtcol(p)
               hrv_livecrootn_to_cwdn(c) = hrv_livecrootn_to_cwdn(c) + &
                  hrv_livecrootn_to_litter(p) * wtcol(p)
               hrv_deadcrootn_to_cwdn(c) = hrv_deadcrootn_to_cwdn(c) + &
                  hrv_deadcrootn_to_litter(p) * wtcol(p)
               hrv_retransn_to_litr1n(c) = hrv_retransn_to_litr1n(c) + &
                  hrv_retransn_to_litter(p) * wtcol(p)
               hrv_leafn_storage_to_litr1n(c) = hrv_leafn_storage_to_litr1n(c) + &
                  hrv_leafn_storage_to_litter(p) * wtcol(p)
               hrv_frootn_storage_to_litr1n(c) = hrv_frootn_storage_to_litr1n(c) + &
                  hrv_frootn_storage_to_litter(p) * wtcol(p)
               hrv_livestemn_storage_to_litr1n(c) = hrv_livestemn_storage_to_litr1n(c) + &
                  hrv_livestemn_storage_to_litter(p) * wtcol(p)
               hrv_deadstemn_storage_to_litr1n(c) = hrv_deadstemn_storage_to_litr1n(c) + &
                  hrv_deadstemn_storage_to_litter(p) * wtcol(p)
               hrv_livecrootn_storage_to_litr1n(c) = hrv_livecrootn_storage_to_litr1n(c) + &
                  hrv_livecrootn_storage_to_litter(p) * wtcol(p)
               hrv_deadcrootn_storage_to_litr1n(c) = hrv_deadcrootn_storage_to_litr1n(c) + &
                  hrv_deadcrootn_storage_to_litter(p) * wtcol(p)
               hrv_leafn_xfer_to_litr1n(c) = hrv_leafn_xfer_to_litr1n(c) + &
                  hrv_leafn_xfer_to_litter(p) * wtcol(p)
               hrv_frootn_xfer_to_litr1n(c) = hrv_frootn_xfer_to_litr1n(c) + &
                  hrv_frootn_xfer_to_litter(p) * wtcol(p)
               hrv_livestemn_xfer_to_litr1n(c) = hrv_livestemn_xfer_to_litr1n(c) + &
                  hrv_livestemn_xfer_to_litter(p) * wtcol(p)
               hrv_deadstemn_xfer_to_litr1n(c) = hrv_deadstemn_xfer_to_litr1n(c) + &
                  hrv_deadstemn_xfer_to_litter(p) * wtcol(p)
               hrv_livecrootn_xfer_to_litr1n(c) = hrv_livecrootn_xfer_to_litr1n(c) + &
                  hrv_livecrootn_xfer_to_litter(p) * wtcol(p)
               hrv_deadcrootn_xfer_to_litr1n(c) = hrv_deadcrootn_xfer_to_litr1n(c) + &
                  hrv_deadcrootn_xfer_to_litter(p) * wtcol(p)
            end if
         end if
      end do
   end do
end subroutine CNHarvestPftToColumn
  subroutine pftdyn_wbal_init()
    implicit none
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    integer :: c
    type(column_type), pointer :: cptr
    cptr => clm3%g%l%c
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    do c = begc,endc
       cptr%cwf%h2ocan_loss(c) = 0._r8
    end do
  end subroutine pftdyn_wbal_init
end module pftdynMod
module filterMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  save
  type clumpfilter
     integer, pointer :: lakep(:)
     integer :: num_lakep
     integer, pointer :: nolakep(:)
     integer :: num_nolakep
     integer, pointer :: lakec(:)
     integer :: num_lakec
     integer, pointer :: nolakec(:)
     integer :: num_nolakec
     integer, pointer :: soilc(:)
     integer :: num_soilc
     integer, pointer :: soilp(:)
     integer :: num_soilp
     integer, pointer :: snowc(:)
     integer :: num_snowc
     integer, pointer :: nosnowc(:)
     integer :: num_nosnowc
     integer, pointer :: hydrologyc(:)
     integer :: num_hydrologyc
     integer, pointer :: urbanl(:)
     integer :: num_urbanl
     integer, pointer :: nourbanl(:)
     integer :: num_nourbanl
     integer, pointer :: urbanc(:)
     integer :: num_urbanc
     integer, pointer :: nourbanc(:)
     integer :: num_nourbanc
     integer, pointer :: urbanp(:)
     integer :: num_urbanp
     integer, pointer :: nourbanp(:)
     integer :: num_nourbanp
     integer, pointer :: nolakeurbanp(:)
     integer :: num_nolakeurbanp
  end type clumpfilter
  public clumpfilter
  type(clumpfilter), public :: filter
  public allocFilters
  public setFilters
  public filters_dealloc
contains
  subroutine allocFilters()
    use clmtype
    use decompMod , only : get_proc_bounds
    implicit none
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
       call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
       allocate(filter%lakep(endp-begp+1))
       allocate(filter%nolakep(endp-begp+1))
       allocate(filter%nolakeurbanp(endp-begp+1))
       allocate(filter%lakec(endc-begc+1))
       allocate(filter%nolakec(endc-begc+1))
       allocate(filter%soilc(endc-begc+1))
       allocate(filter%soilp(endp-begp+1))
       allocate(filter%snowc(endc-begc+1))
       allocate(filter%nosnowc(endc-begc+1))
       allocate(filter%hydrologyc(endc-begc+1))
       allocate(filter%urbanp(endp-begp+1))
       allocate(filter%nourbanp(endp-begp+1))
       allocate(filter%urbanc(endc-begc+1))
       allocate(filter%nourbanc(endc-begc+1))
       allocate(filter%urbanl(endl-begl+1))
       allocate(filter%nourbanl(endl-begl+1))
  end subroutine allocFilters
  subroutine setFilters()
    use clmtype
    use decompMod , only : get_proc_bounds
    use clm_varcon, only : istsoil, isturb, icol_road_perv
    implicit none
    integer , pointer :: ctype(:)
    integer :: c,l,p
    integer :: fl
    integer :: fnl,fnlu
    integer :: fs
    integer :: f, fn
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    ctype => clm3%g%l%c%itype
       call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
       fl = 0
       fnl = 0
       do c = begc,endc
          l = clm3%g%l%c%landunit(c)
          if (clm3%g%l%lakpoi(l)) then
             fl = fl + 1
             filter%lakec(fl) = c
          else
             fnl = fnl + 1
             filter%nolakec(fnl) = c
          end if
       end do
       filter%num_lakec = fl
       filter%num_nolakec = fnl
       fl = 0
       fnl = 0
       fnlu = 0
       do p = begp,endp
          if (clm3%g%l%c%p%wtgcell(p) > 0._r8) then
             l = clm3%g%l%c%p%landunit(p)
             if (clm3%g%l%lakpoi(l) ) then
                fl = fl + 1
                filter%lakep(fl) = p
             else
                fnl = fnl + 1
                filter%nolakep(fnl) = p
                if (clm3%g%l%itype(l) /= isturb) then
                   fnlu = fnlu + 1
                   filter%nolakeurbanp(fnlu) = p
                end if
             end if
          end if
       end do
       filter%num_lakep = fl
       filter%num_nolakep = fnl
       filter%num_nolakeurbanp = fnlu
       fs = 0
       do c = begc,endc
          l = clm3%g%l%c%landunit(c)
          if (clm3%g%l%itype(l) == istsoil) then
             fs = fs + 1
             filter%soilc(fs) = c
          end if
       end do
       filter%num_soilc = fs
       fs = 0
       do p = begp,endp
          if (clm3%g%l%c%p%wtgcell(p) > 0._r8) then
             l = clm3%g%l%c%p%landunit(p)
             if (clm3%g%l%itype(l) == istsoil) then
                fs = fs + 1
                filter%soilp(fs) = p
             end if
          end if
       end do
       filter%num_soilp = fs
       f = 0
       do c = begc,endc
          l = clm3%g%l%c%landunit(c)
          if (clm3%g%l%itype(l) == istsoil .or. ctype(c) == icol_road_perv ) then
             f = f + 1
             filter%hydrologyc(f) = c
          end if
       end do
       filter%num_hydrologyc = f
       f = 0
       fn = 0
       do l = begl,endl
          if (clm3%g%l%itype(l) == isturb) then
             f = f + 1
             filter%urbanl(f) = l
          else
             fn = fn + 1
             filter%nourbanl(fn) = l
          end if
       end do
       filter%num_urbanl = f
       filter%num_nourbanl = fn
       f = 0
       fn = 0
       do c = begc,endc
          l = clm3%g%l%c%landunit(c)
          if (clm3%g%l%itype(l) == isturb) then
             f = f + 1
             filter%urbanc(f) = c
          else
             fn = fn + 1
             filter%nourbanc(fn) = c
          end if
       end do
       filter%num_urbanc = f
       filter%num_nourbanc = fn
       f = 0
       fn = 0
       do p = begp,endp
          l = clm3%g%l%c%p%landunit(p)
          if (clm3%g%l%itype(l) == isturb .and. clm3%g%l%c%p%wtgcell(p) > 0._r8) then
             f = f + 1
             filter%urbanp(f) = p
          else
             fn = fn + 1
             filter%nourbanp(fn) = p
          end if
       end do
       filter%num_urbanp = f
       filter%num_nourbanp = fn
  end subroutine setFilters
  subroutine filters_dealloc
    implicit none
       deallocate(filter%lakep)
   call CLMDebug('mark1')
       deallocate(filter%nolakep)
       deallocate(filter%nolakeurbanp)
       deallocate(filter%lakec)
       deallocate(filter%nolakec)
       deallocate(filter%soilc)
       deallocate(filter%soilp)
       deallocate(filter%snowc)
       deallocate(filter%nosnowc)
       deallocate(filter%hydrologyc)
       deallocate(filter%urbanp)
       deallocate(filter%nourbanp)
       deallocate(filter%urbanc)
       deallocate(filter%nourbanc)
       deallocate(filter%urbanl)
       deallocate(filter%nourbanl)
  call CLMDebug('done  filters_dealloc')
  end subroutine filters_dealloc
end module filterMod
subroutine iniTimeConst
  use shr_kind_mod, only : r8 => shr_kind_r8
  use nanMod , only : nan
  use clmtype
  use decompMod , only : get_proc_bounds
  use clm_varpar , only : nlevsoi, nlevgrnd, nlevlak, lsmlon, lsmlat, numpft, numrad, nlevurb
  use clm_varcon , only : istice, istdlak, istwet, isturb, &
                           icol_roof, icol_sunwall, icol_shadewall, icol_road_perv, icol_road_imperv, &
                           zlak, dzlak, zsoi, dzsoi, zisoi, spval
  use pftvarcon , only : noveg, ntree, roota_par, rootb_par, &
                           smpso, smpsc, fnitr, nbrdlf_dcd_brl_shrub, &
                           z0mr, displar, dleaf, rhol, rhos, taul, taus, xl, &
                           qe25, vcmx25, mp, c3psn, slatop, dsladlai, leafcn, flnr, woody, &
                           lflitcn, frootcn, livewdcn, deadwdcn, froot_leaf, stem_leaf, croot_stem, &
                           flivewd, fcur, lf_flab, lf_fcel, lf_flig, fr_flab, fr_fcel, fr_flig, &
                           dw_fcel, dw_flig, leaf_long, evergreen, stress_decid, season_decid, &
                           resist, pftpar20, pftpar28, pftpar29, pftpar30, pftpar31, &
                           allom1s, allom2s, &
                           allom1 , allom2 , allom3 , reinickerp, dwood
  use module_cam_support, only: endrun
  use globals , only : nstep
  use clm_varsur , only : gti,soic2d,efisop2d,sand3d,clay3d,organic3d
  implicit none
  integer , pointer :: ivt(:)
  integer , pointer :: pcolumn(:)
  integer , pointer :: pgridcell(:)
  integer , pointer :: clandunit(:)
  integer , pointer :: cgridcell(:)
  integer , pointer :: ctype(:)
  integer , pointer :: ltype(:)
  real(r8), pointer :: thick_wall(:)
  real(r8), pointer :: thick_roof(:)
  real(r8), pointer :: lat(:)
  real(r8), pointer :: z(:,:)
  real(r8), pointer :: zi(:,:)
  real(r8), pointer :: dz(:,:)
  real(r8), pointer :: rootfr(:,:)
  real(r8), pointer :: rootfr_road_perv(:,:)
  real(r8), pointer :: rresis(:,:)
  real(r8), pointer :: dewmx(:)
  real(r8), pointer :: bsw(:,:)
  real(r8), pointer :: bsw2(:,:)
  real(r8), pointer :: psisat(:,:)
  real(r8), pointer :: vwcsat(:,:)
  real(r8), pointer :: watsat(:,:)
  real(r8), pointer :: watfc(:,:)
  real(r8), pointer :: watdry(:,:)
  real(r8), pointer :: watopt(:,:)
  real(r8), pointer :: hksat(:,:)
  real(r8), pointer :: sucsat(:,:)
  real(r8), pointer :: csol(:,:)
  real(r8), pointer :: tkmg(:,:)
  real(r8), pointer :: tkdry(:,:)
  real(r8), pointer :: tksatu(:,:)
  real(r8), pointer :: wtfact(:)
  real(r8), pointer :: smpmin(:)
  real(r8), pointer :: hkdepth(:)
  integer , pointer :: isoicol(:)
  real(r8), pointer :: gwc_thr(:)
  real(r8), pointer :: mss_frc_cly_vld(:)
  real(r8), pointer :: forc_ndep(:)
  real(r8), pointer :: efisop(:,:)
  real(r8), pointer :: max_dayl(:)
  real(r8), pointer :: sandfrac(:)
  real(r8), pointer :: clayfrac(:)
  integer :: ncid
  integer :: n,j,ib,lev,bottom
  integer :: g,l,c,p
  integer :: m
  real(r8) :: bd
  real(r8) :: tkm
  real(r8) :: xksat
  real(r8) :: scalez = 0.025_r8
  real(r8) :: clay,sand
  real(r8) :: slope,intercept
  real(r8) :: temp, max_decl
  integer :: begp, endp
  integer :: begc, endc
  integer :: begl, endl
  integer :: begg, endg
  real(r8) :: om_frac
  real(r8) :: om_watsat = 0.9_r8
  real(r8) :: om_hksat = 0.1_r8
  real(r8) :: om_tkm = 0.25_r8
  real(r8) :: om_sucsat = 10.3_r8
  real(r8) :: om_csol = 2.5_r8
  real(r8) :: om_tkd = 0.05_r8
  real(r8) :: om_b = 2.7_r8
  real(r8) :: organic_max = 130._r8
  real(r8) :: csol_bedrock = 2.0e6_r8
  real(r8) :: pc = 0.5_r8
  real(r8) :: pcbeta = 0.139_r8
  real(r8) :: perc_frac
  real(r8) :: perc_norm
  real(r8) :: uncon_hksat
  real(r8) :: uncon_frac
  integer :: start(3),count(3)
  integer :: varid
  integer :: ret
  integer :: ier
  character(len=256) :: locfn
  character(len= 32) :: subname = 'iniTimeConst'
  integer :: mxsoil_color
  real(r8), allocatable :: zurb_wall(:,:)
  real(r8), allocatable :: zurb_roof(:,:)
  real(r8), allocatable :: dzurb_wall(:,:)
  real(r8), allocatable :: dzurb_roof(:,:)
  real(r8), allocatable :: ziurb_wall(:,:)
  real(r8), allocatable :: ziurb_roof(:,:)
  integer :: closelatidx,closelonidx
  real(r8):: closelat,closelon
  integer :: iostat
  call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
  efisop => clm3%g%gve%efisop
  lat => clm3%g%lat
  ltype => clm3%g%l%itype
  thick_wall => clm3%g%l%lps%thick_wall
  thick_roof => clm3%g%l%lps%thick_roof
  ctype => clm3%g%l%c%itype
  clandunit => clm3%g%l%c%landunit
  cgridcell => clm3%g%l%c%gridcell
  z => clm3%g%l%c%cps%z
  dz => clm3%g%l%c%cps%dz
  zi => clm3%g%l%c%cps%zi
  bsw => clm3%g%l%c%cps%bsw
  bsw2 => clm3%g%l%c%cps%bsw2
  psisat => clm3%g%l%c%cps%psisat
  vwcsat => clm3%g%l%c%cps%vwcsat
  watsat => clm3%g%l%c%cps%watsat
  watfc => clm3%g%l%c%cps%watfc
  watdry => clm3%g%l%c%cps%watdry
  watopt => clm3%g%l%c%cps%watopt
  rootfr_road_perv => clm3%g%l%c%cps%rootfr_road_perv
  hksat => clm3%g%l%c%cps%hksat
  sucsat => clm3%g%l%c%cps%sucsat
  tkmg => clm3%g%l%c%cps%tkmg
  tksatu => clm3%g%l%c%cps%tksatu
  tkdry => clm3%g%l%c%cps%tkdry
  csol => clm3%g%l%c%cps%csol
  smpmin => clm3%g%l%c%cps%smpmin
  hkdepth => clm3%g%l%c%cps%hkdepth
  wtfact => clm3%g%l%c%cps%wtfact
  isoicol => clm3%g%l%c%cps%isoicol
  gwc_thr => clm3%g%l%c%cps%gwc_thr
  mss_frc_cly_vld => clm3%g%l%c%cps%mss_frc_cly_vld
  max_dayl => clm3%g%l%c%cps%max_dayl
  forc_ndep => clm_a2l%forc_ndep
  ivt => clm3%g%l%c%p%itype
  pgridcell => clm3%g%l%c%p%gridcell
  pcolumn => clm3%g%l%c%p%column
  dewmx => clm3%g%l%c%p%pps%dewmx
  rootfr => clm3%g%l%c%p%pps%rootfr
  rresis => clm3%g%l%c%p%pps%rresis
  sandfrac => clm3%g%l%c%p%pps%sandfrac
  clayfrac => clm3%g%l%c%p%pps%clayfrac
  allocate(zurb_wall(begl:endl,nlevurb), zurb_roof(begl:endl,nlevurb), &
           dzurb_wall(begl:endl,nlevurb), dzurb_roof(begl:endl,nlevurb), &
           ziurb_wall(begl:endl,0:nlevurb), ziurb_roof(begl:endl,0:nlevurb), stat=ier)
  if (ier /= 0) then
     call endrun( 'iniTimeConst: allocation error for zurb_wall,zurb_roof,dzurb_wall,dzurb_roof,ziurb_wall,ziurb_roof' )
  end if
  call CLMDebug('TimeConst mark1')
  do p = begp,endp
     g = pgridcell(p)
     sandfrac(p) = sand3d(g,1)/100.0_r8
     clayfrac(p) = clay3d(g,1)/100.0_r8
  end do
   do m = 0,numpft
      if (m <= ntree) then
         pftcon%tree(m) = 1
      else
         pftcon%tree(m) = 0
      end if
      pftcon%z0mr(m) = z0mr(m)
      pftcon%displar(m) = displar(m)
      pftcon%dleaf(m) = dleaf(m)
      pftcon%xl(m) = xl(m)
      do ib = 1,numrad
         pftcon%rhol(m,ib) = rhol(m,ib)
         pftcon%rhos(m,ib) = rhos(m,ib)
         pftcon%taul(m,ib) = taul(m,ib)
         pftcon%taus(m,ib) = taus(m,ib)
      end do
      pftcon%qe25(m) = qe25(m)
      pftcon%vcmx25(m) = vcmx25(m)
      pftcon%mp(m) = mp(m)
      pftcon%c3psn(m) = c3psn(m)
      pftcon%slatop(m) = slatop(m)
      pftcon%dsladlai(m) = dsladlai(m)
      pftcon%leafcn(m) = leafcn(m)
      pftcon%flnr(m) = flnr(m)
      pftcon%smpso(m) = smpso(m)
      pftcon%smpsc(m) = smpsc(m)
      pftcon%fnitr(m) = fnitr(m)
      pftcon%woody(m) = woody(m)
      pftcon%lflitcn(m) = lflitcn(m)
      pftcon%frootcn(m) = frootcn(m)
      pftcon%livewdcn(m) = livewdcn(m)
      pftcon%deadwdcn(m) = deadwdcn(m)
      pftcon%froot_leaf(m) = froot_leaf(m)
      pftcon%stem_leaf(m) = stem_leaf(m)
      pftcon%croot_stem(m) = croot_stem(m)
      pftcon%flivewd(m) = flivewd(m)
      pftcon%fcur(m) = fcur(m)
      pftcon%lf_flab(m) = lf_flab(m)
      pftcon%lf_fcel(m) = lf_fcel(m)
      pftcon%lf_flig(m) = lf_flig(m)
      pftcon%fr_flab(m) = fr_flab(m)
      pftcon%fr_fcel(m) = fr_fcel(m)
      pftcon%fr_flig(m) = fr_flig(m)
      pftcon%dw_fcel(m) = dw_fcel(m)
      pftcon%dw_flig(m) = dw_flig(m)
      pftcon%leaf_long(m) = leaf_long(m)
      pftcon%evergreen(m) = evergreen(m)
      pftcon%stress_decid(m) = stress_decid(m)
      pftcon%season_decid(m) = season_decid(m)
      pftcon%resist(m) = resist(m)
      pftcon%dwood(m) = dwood
   end do
   dzlak(1) = 0.1_r8
   dzlak(2) = 1._r8
   dzlak(3) = 2._r8
   dzlak(4) = 3._r8
   dzlak(5) = 4._r8
   dzlak(6) = 5._r8
   dzlak(7) = 7._r8
   dzlak(8) = 7._r8
   dzlak(9) = 10.45_r8
   dzlak(10)= 10.45_r8
   zlak(1) = 0.05_r8
   zlak(2) = 0.6_r8
   zlak(3) = 2.1_r8
   zlak(4) = 4.6_r8
   zlak(5) = 8.1_r8
   zlak(6) = 12.6_r8
   zlak(7) = 18.6_r8
   zlak(8) = 25.6_r8
   zlak(9) = 34.325_r8
   zlak(10)= 44.775_r8
   do j = 1, nlevgrnd
      zsoi(j) = scalez*(exp(0.5_r8*(j-0.5_r8))-1._r8)
   enddo
   dzsoi(1) = 0.5_r8*(zsoi(1)+zsoi(2))
   do j = 2,nlevgrnd-1
      dzsoi(j)= 0.5_r8*(zsoi(j+1)-zsoi(j-1))
   enddo
   dzsoi(nlevgrnd) = zsoi(nlevgrnd)-zsoi(nlevgrnd-1)
   zisoi(0) = 0._r8
   do j = 1, nlevgrnd-1
      zisoi(j) = 0.5_r8*(zsoi(j)+zsoi(j+1))
   enddo
   zisoi(nlevgrnd) = zsoi(nlevgrnd) + 0.5_r8*dzsoi(nlevgrnd)
   do l = begl, endl
    if (ltype(l)==isturb) then
      do j = 1, nlevurb
        zurb_wall(l,j) = (j-0.5)*(thick_wall(l)/float(nlevurb))
      end do
      do j = 1, nlevurb
        zurb_roof(l,j) = (j-0.5)*(thick_roof(l)/float(nlevurb))
      end do
      dzurb_wall(l,1) = 0.5*(zurb_wall(l,1)+zurb_wall(l,2))
      do j = 2,nlevurb-1
        dzurb_wall(l,j)= 0.5*(zurb_wall(l,j+1)-zurb_wall(l,j-1))
      enddo
      dzurb_wall(l,nlevurb) = zurb_wall(l,nlevurb)-zurb_wall(l,nlevurb-1)
      dzurb_roof(l,1) = 0.5*(zurb_roof(l,1)+zurb_roof(l,2))
      do j = 2,nlevurb-1
        dzurb_roof(l,j)= 0.5*(zurb_roof(l,j+1)-zurb_roof(l,j-1))
      enddo
      dzurb_roof(l,nlevurb) = zurb_roof(l,nlevurb)-zurb_roof(l,nlevurb-1)
      ziurb_wall(l,0) = 0.
      do j = 1, nlevurb-1
        ziurb_wall(l,j) = 0.5*(zurb_wall(l,j)+zurb_wall(l,j+1))
      enddo
      ziurb_wall(l,nlevurb) = zurb_wall(l,nlevurb) + 0.5*dzurb_wall(l,nlevurb)
      ziurb_roof(l,0) = 0.
      do j = 1, nlevurb-1
        ziurb_roof(l,j) = 0.5*(zurb_roof(l,j)+zurb_roof(l,j+1))
      enddo
      ziurb_roof(l,nlevurb) = zurb_roof(l,nlevurb) + 0.5*dzurb_roof(l,nlevurb)
    end if
   end do
   do g = begg, endg
      efisop(:,g)=efisop2d(:,g)
   end do
   call CLMDebug('mark2')
   do c = begc, endc
      g = cgridcell(c)
      l = clandunit(c)
      call CLMDebug('mark21')
      max_decl = 0.409571
      if (lat(g) .lt. 0._r8) max_decl = -max_decl
      temp = -(sin(lat(g))*sin(max_decl))/(cos(lat(g)) * cos(max_decl))
      temp = min(1._r8,max(-1._r8,temp))
      max_dayl(c) = 2.0_r8 * 13750.9871_r8 * acos(temp)
      smpmin(c) = -1.e8_r8
      hkdepth(c) = 1._r8/2.5_r8
  call CLMDebug('mark22')
      wtfact(c) = gti(g)
   call CLMDebug('mark23')
      isoicol(c) = soic2d(g)
      if (ltype(l)==istdlak .or. ltype(l)==istwet .or. ltype(l)==istice) then
         do lev = 1,nlevgrnd
            bsw(c,lev) = spval
            bsw2(c,lev) = spval
            psisat(c,lev) = spval
            vwcsat(c,lev) = spval
            watsat(c,lev) = spval
            watfc(c,lev) = spval
            hksat(c,lev) = spval
            sucsat(c,lev) = spval
            tkmg(c,lev) = spval
            tksatu(c,lev) = spval
            tkdry(c,lev) = spval
            if (ltype(l)==istwet .and. lev > nlevsoi) then
               csol(c,lev) = csol_bedrock
            else
               csol(c,lev)= spval
            endif
            watdry(c,lev) = spval
            watopt(c,lev) = spval
         end do
      else if (ltype(l)==isturb .and. (ctype(c) /= icol_road_perv) .and. (ctype(c) /= icol_road_imperv) )then
         do lev = 1,nlevurb
            watsat(c,lev) = spval
            watfc(c,lev) = spval
            bsw(c,lev) = spval
            bsw2(c,lev) = spval
            psisat(c,lev) = spval
            vwcsat(c,lev) = spval
            hksat(c,lev) = spval
            sucsat(c,lev) = spval
            tkmg(c,lev) = spval
            tksatu(c,lev) = spval
            tkdry(c,lev) = spval
            csol(c,lev) = spval
            watdry(c,lev) = spval
            watopt(c,lev) = spval
         end do
      else
         do lev = 1,nlevgrnd
            if (lev .le. nlevsoi) then
               clay = clay3d(g,lev)
               sand = sand3d(g,lev)
               om_frac = (organic3d(g,lev)/organic_max)**2._r8
            else
               clay = clay3d(g,nlevsoi)
               sand = sand3d(g,nlevsoi)
               om_frac = 0._r8
            endif
            if (ltype(l)==isturb) then
              om_frac = 0._r8
            end if
            watsat(c,lev) = 0.489_r8 - 0.00126_r8*sand
            bsw(c,lev) = 2.91 + 0.159*clay
            sucsat(c,lev) = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand) )
            bd = (1._r8-watsat(c,lev))*2.7e3_r8
            watsat(c,lev) = (1._r8 - om_frac)*watsat(c,lev) + om_watsat*om_frac
            tkm = (1._r8-om_frac)*(8.80_r8*sand+2.92_r8*clay)/(sand+clay)+om_tkm*om_frac
            bsw(c,lev) = (1._r8-om_frac)*(2.91_r8 + 0.159_r8*clay) + om_frac*om_b
            bsw2(c,lev) = -(3.10_r8 + 0.157_r8*clay - 0.003_r8*sand)
            psisat(c,lev) = -(exp((1.54_r8 - 0.0095_r8*sand + 0.0063_r8*(100.0_r8-sand-clay))*log(10.0_r8))*9.8e-5_r8)
            vwcsat(c,lev) = (50.5_r8 - 0.142_r8*sand - 0.037_r8*clay)/100.0_r8
            sucsat(c,lev) = (1._r8-om_frac)*sucsat(c,lev) + om_sucsat*om_frac
            xksat = 0.0070556 *( 10.**(-0.884+0.0153*sand) )
            if (om_frac > pc) then
               perc_norm=(1._r8 - pc)**(-pcbeta)
               perc_frac=perc_norm*(om_frac - pc)**pcbeta
            else
               perc_frac=0._r8
            endif
            uncon_frac=(1._r8-om_frac)+(1._r8-perc_frac)*om_frac
            if (om_frac .lt. 1._r8) then
              uncon_hksat=uncon_frac/((1._r8-om_frac)/xksat &
                   +((1._r8-perc_frac)*om_frac)/om_hksat)
            else
              uncon_hksat = 0._r8
            end if
            hksat(c,lev) = uncon_frac*uncon_hksat + (perc_frac*om_frac)*om_hksat
            tkmg(c,lev) = tkm ** (1._r8- watsat(c,lev))
            tksatu(c,lev) = tkmg(c,lev)*0.57_r8**watsat(c,lev)
            tkdry(c,lev) = ((0.135_r8*bd + 64.7_r8) / (2.7e3_r8 - 0.947_r8*bd))*(1._r8-om_frac) + &
                            om_tkd*om_frac
            csol(c,lev) = ((1._r8-om_frac)*(2.128_r8*sand+2.385_r8*clay) / (sand+clay) + &
                           om_csol*om_frac)*1.e6_r8
            if (lev .gt. nlevsoi) then
               csol(c,lev) = csol_bedrock
            endif
            watdry(c,lev) = watsat(c,lev) * (316230._r8/sucsat(c,lev)) ** (-1._r8/bsw(c,lev))
            watopt(c,lev) = watsat(c,lev) * (158490._r8/sucsat(c,lev)) ** (-1._r8/bsw(c,lev))
            watfc(c,lev) = watsat(c,lev) * (0.1_r8 / (hksat(c,lev)*86400._r8))**(1._r8/(2._r8*bsw(c,lev)+3._r8))
         end do
         if (ctype(c) == icol_road_imperv) then
            do lev = 1,nlevgrnd
               watdry(c,lev) = spval
               watopt(c,lev) = spval
            end do
         else if (ctype(c) == icol_road_perv) then
            do lev = 1, nlevgrnd
               rootfr_road_perv(c,lev) = 0._r8
            enddo
            do lev = 1,nlevsoi
               rootfr_road_perv(c,lev) = 0.1_r8
            end do
         end if
      endif
      if (ltype(l) == istdlak) then
         z(c,1:nlevlak) = zlak(1:nlevlak)
         dz(c,1:nlevlak) = dzlak(1:nlevlak)
      else if (ltype(l) == isturb) then
         if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall) then
            z(c,1:nlevurb) = zurb_wall(l,1:nlevurb)
            zi(c,0:nlevurb) = ziurb_wall(l,0:nlevurb)
            dz(c,1:nlevurb) = dzurb_wall(l,1:nlevurb)
         else if (ctype(c)==icol_roof) then
            z(c,1:nlevurb) = zurb_roof(l,1:nlevurb)
            zi(c,0:nlevurb) = ziurb_roof(l,0:nlevurb)
            dz(c,1:nlevurb) = dzurb_roof(l,1:nlevurb)
         else
            z(c,1:nlevurb) = zsoi(1:nlevurb)
            zi(c,0:nlevurb) = zisoi(0:nlevurb)
            dz(c,1:nlevurb) = dzsoi(1:nlevurb)
         end if
      else
         z(c,1:nlevgrnd) = zsoi(1:nlevgrnd)
         zi(c,0:nlevgrnd) = zisoi(0:nlevgrnd)
         dz(c,1:nlevgrnd) = dzsoi(1:nlevgrnd)
      end if
      clay = clay3d(g,1)
      gwc_thr(c) = 0.17_r8 + 0.14_r8*clay*0.01_r8
      mss_frc_cly_vld(c) = min(clay*0.01_r8, 0.20_r8)
   end do
    call CLMDebug('mark3')
   do p = begp, endp
      dewmx(p) = 0.1_r8
      c = pcolumn(p)
      if (ivt(p) /= noveg) then
         do lev = 1, nlevgrnd
            rootfr(p,lev) = 0._r8
         enddo
         do lev = 1, nlevsoi-1
            rootfr(p,lev) = .5_r8*( exp(-roota_par(ivt(p)) * zi(c,lev-1)) &
                               + exp(-rootb_par(ivt(p)) * zi(c,lev-1)) &
                               - exp(-roota_par(ivt(p)) * zi(c,lev )) &
                               - exp(-rootb_par(ivt(p)) * zi(c,lev )) )
         end do
         rootfr(p,nlevsoi) = .5_r8*( exp(-roota_par(ivt(p)) * zi(c,nlevsoi-1)) &
                                + exp(-rootb_par(ivt(p)) * zi(c,nlevsoi-1)) )
         rootfr(p,nlevsoi+1:nlevgrnd) = 0.0_r8
      else
         rootfr(p,1:nlevsoi) = 0._r8
      endif
      do lev = 1,nlevgrnd
         rresis(p,lev) = 0._r8
      end do
   end do
   call CLMDebug('Successfully initialized time invariant variables')
  deallocate(zurb_wall)
  deallocate(zurb_roof)
  deallocate(dzurb_wall)
  deallocate(dzurb_roof)
  deallocate(ziurb_wall)
  deallocate(ziurb_roof)
end subroutine iniTimeConst
module QSatMod
  use module_cam_support, only: endrun
  implicit none
  save
  public :: QSat
contains
  subroutine QSat (T, p, es, esdT, qs, qsdT)
    use shr_kind_mod , only: r8 => shr_kind_r8
    use shr_const_mod, only: SHR_CONST_TKFRZ
    implicit none
    real(r8), intent(in) :: T
    real(r8), intent(in) :: p
    real(r8), intent(out) :: es
    real(r8), intent(out) :: esdT
    real(r8), intent(out) :: qs
    real(r8), intent(out) :: qsdT
    real(r8) :: T_limit
    real(r8) :: td,vp,vp1,vp2
    real(r8), parameter :: a0 = 6.11213476_r8
    real(r8), parameter :: a1 = 0.444007856_r8
    real(r8), parameter :: a2 = 0.143064234e-01_r8
    real(r8), parameter :: a3 = 0.264461437e-03_r8
    real(r8), parameter :: a4 = 0.305903558e-05_r8
    real(r8), parameter :: a5 = 0.196237241e-07_r8
    real(r8), parameter :: a6 = 0.892344772e-10_r8
    real(r8), parameter :: a7 = -0.373208410e-12_r8
    real(r8), parameter :: a8 = 0.209339997e-15_r8
    real(r8), parameter :: b0 = 0.444017302_r8
    real(r8), parameter :: b1 = 0.286064092e-01_r8
    real(r8), parameter :: b2 = 0.794683137e-03_r8
    real(r8), parameter :: b3 = 0.121211669e-04_r8
    real(r8), parameter :: b4 = 0.103354611e-06_r8
    real(r8), parameter :: b5 = 0.404125005e-09_r8
    real(r8), parameter :: b6 = -0.788037859e-12_r8
    real(r8), parameter :: b7 = -0.114596802e-13_r8
    real(r8), parameter :: b8 = 0.381294516e-16_r8
    real(r8), parameter :: c0 = 6.11123516_r8
    real(r8), parameter :: c1 = 0.503109514_r8
    real(r8), parameter :: c2 = 0.188369801e-01_r8
    real(r8), parameter :: c3 = 0.420547422e-03_r8
    real(r8), parameter :: c4 = 0.614396778e-05_r8
    real(r8), parameter :: c5 = 0.602780717e-07_r8
    real(r8), parameter :: c6 = 0.387940929e-09_r8
    real(r8), parameter :: c7 = 0.149436277e-11_r8
    real(r8), parameter :: c8 = 0.262655803e-14_r8
    real(r8), parameter :: d0 = 0.503277922_r8
    real(r8), parameter :: d1 = 0.377289173e-01_r8
    real(r8), parameter :: d2 = 0.126801703e-02_r8
    real(r8), parameter :: d3 = 0.249468427e-04_r8
    real(r8), parameter :: d4 = 0.313703411e-06_r8
    real(r8), parameter :: d5 = 0.257180651e-08_r8
    real(r8), parameter :: d6 = 0.133268878e-10_r8
    real(r8), parameter :: d7 = 0.394116744e-13_r8
    real(r8), parameter :: d8 = 0.498070196e-16_r8
    T_limit = T - SHR_CONST_TKFRZ
    if (T_limit > 100.0_r8) T_limit=100.0_r8
    if (T_limit < -75.0_r8) T_limit=-75.0_r8
    td = T_limit
    if (td >= 0.0_r8) then
       es = a0 + td*(a1 + td*(a2 + td*(a3 + td*(a4 &
            + td*(a5 + td*(a6 + td*(a7 + td*a8)))))))
       esdT = b0 + td*(b1 + td*(b2 + td*(b3 + td*(b4 &
            + td*(b5 + td*(b6 + td*(b7 + td*b8)))))))
    else
       es = c0 + td*(c1 + td*(c2 + td*(c3 + td*(c4 &
            + td*(c5 + td*(c6 + td*(c7 + td*c8)))))))
       esdT = d0 + td*(d1 + td*(d2 + td*(d3 + td*(d4 &
            + td*(d5 + td*(d6 + td*(d7 + td*d8)))))))
    endif
    es = es * 100._r8
    esdT = esdT * 100._r8
    vp = 1.0_r8 / (p - 0.378_r8*es)
    vp1 = 0.622_r8 * vp
    vp2 = vp1 * vp
    qs = es * vp1
    qsdT = esdT * vp2 * p
  end subroutine QSat
end module QSatMod
module initGridcellsMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clmtype
  use clm_varpar, only : lsmlon, lsmlat,npatch_urban,npatch_glacier,npatch_crop, maxpatch, maxpatch_pft
  use clm_varsur, only : wtxy,vegxy,numlon, area, latixy, longxy
  use module_cam_support, only: endrun
  implicit none
  private
  save
  public initGridcells
  private landunit_veg_compete
  private landunit_veg_noncompete
  private landunit_special
  private landunit_crop_noncompete
  type(gridcell_type), pointer :: gptr
  type(landunit_type), pointer :: lptr
  type(column_type) , pointer :: cptr
  type(pft_type) , pointer :: pptr
contains
  subroutine initGridcells
    use decompMod , only : get_proc_bounds, get_gcell_xyind, &
                           get_gcell_info
    use clm_varcon, only : pie
    implicit none
    integer :: g,i,j,m,n,gi,li,ci,pi
    integer :: ngcells
    integer :: nlunits
    integer :: ncols
    integer :: npfts
    integer :: nveg
    real(r8):: wtveg
    integer :: ncrop
    real(r8):: wtcrop
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    integer :: ier
    integer :: ilunits, icols, ipfts
    gptr => clm3%g
    lptr => clm3%g%l
    cptr => clm3%g%l%c
    pptr => clm3%g%l%c%p
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    call get_gcell_xyind(begg, endg)
    clm3%ngridcells = endg - begg + 1
    ngcells = begg-1
    nlunits = begl-1
    ncols = begc-1
    npfts = begp-1
    do gi = begg, endg
       gptr%area(gi) = area(gi)
  call CLMDebug('mark0')
       gptr%lat(gi) = latixy(gi) * pie/180.
       gptr%lon(gi) = longxy(gi) * pie/180.
       gptr%latdeg(gi) = latixy(gi)
       gptr%londeg(gi) = longxy(gi)
       gptr%luni(gi) = nlunits + 1
       gptr%coli(gi) = ncols + 1
       gptr%pfti(gi) = npfts + 1
       call get_gcell_info(gi, nlunits=ilunits, ncols=icols, npfts=ipfts)
       ngcells = ngcells + 1
       nlunits = nlunits + ilunits
       ncols = ncols + icols
       npfts = npfts + ipfts
       gptr%lunf(gi) = nlunits
       gptr%colf(gi) = ncols
       gptr%pftf(gi) = npfts
       gptr%nlandunits(gi) = gptr%lunf(gi) - gptr%luni(gi) + 1
       gptr%ncolumns(gi) = gptr%colf(gi) - gptr%coli(gi) + 1
       gptr%npfts(gi) = gptr%pftf(gi) - gptr%pfti(gi) + 1
    end do
     call CLMDebug('mark1')
    ngcells = 0
    nlunits = 0
    ncols = 0
    npfts = 0
    li = begl - 1
    ci = begc - 1
    pi = begp - 1
    do gi = begg,endg
       call get_gcell_info(gi, nveg=nveg, wtveg=wtveg, ncrop=ncrop, wtcrop=wtcrop)
       if (nveg > 0) call landunit_veg_compete(nveg, wtveg, i, j, gi, li, ci, pi)
       if (ncrop > 0) call landunit_crop_noncompete(ncrop, wtcrop, i, j, gi, li, ci, pi)
       do m = npatch_urban, npatch_glacier
          if (wtxy(gi,m) > 0.) call landunit_special(i, j, m, gi, li, ci, pi)
       end do
    end do
  end subroutine initGridcells
  subroutine landunit_veg_compete (nveg, wtveg, i, j, &
                                   gi, li, ci, pi)
    use clm_varcon, only : istsoil
    implicit none
    integer , intent(in) :: nveg
    real(r8), intent(in) :: wtveg
    integer , intent(in) :: i
    integer , intent(in) :: j
    integer , intent(in) :: gi
    integer , intent(inout) :: li
    integer , intent(inout) :: ci
    integer , intent(inout) :: pi
    integer :: m
    li = li + 1
    lptr%ncolumns(li) = 1
    lptr%coli(li) = ci + 1
    lptr%colf(li) = ci + 1
    lptr%npfts(li) = nveg
    lptr%pfti(li) = pi + 1
    lptr%pftf(li) = pi + nveg
    lptr%area(li) = gptr%area(gi) * wtveg
    lptr%gridcell(li) = gi
    lptr%wtgcell(li) = wtveg
    lptr%ifspecial(li) = .false.
    lptr%lakpoi(li) = .false.
    lptr%urbpoi(li) = .false.
    lptr%itype(li) = istsoil
    ci = ci + 1
    cptr%npfts(ci) = nveg
    cptr%pfti(ci) = pi + 1
    cptr%pftf(ci) = pi + nveg
    cptr%area(ci) = lptr%area(li)
    cptr%landunit(ci) = li
    cptr%gridcell(ci) = gi
    cptr%wtlunit(ci) = 1.0
    cptr%wtgcell(ci) = wtveg
    cptr%itype(ci) = 1
!dir$ concurrent
    do m = 1,maxpatch_pft
       if (wtxy(gi,m) > 0.) then
          pi = pi+1
          pptr%column(pi) = ci
          pptr%landunit(pi) = li
          pptr%gridcell(pi) = gi
          pptr%wtcol(pi) = wtxy(gi,m) / wtveg
          pptr%wtlunit(pi) = wtxy(gi,m) / wtveg
          pptr%wtgcell(pi) = wtxy(gi,m)
          pptr%area(pi) = cptr%area(ci) * pptr%wtcol(pi)
          pptr%mxy(pi) = m
          pptr%itype(pi) = vegxy(gi,m)
       end if
    end do
  end subroutine landunit_veg_compete
  subroutine landunit_veg_noncompete (nveg, wtveg, i, j, &
                                      gi, li, ci, pi)
    use clm_varcon, only : istsoil
    implicit none
    integer , intent(in) :: nveg
    real(r8), intent(in) :: wtveg
    integer , intent(in) :: i
    integer , intent(in) :: j
    integer , intent(in) :: gi
    integer , intent(inout) :: li
    integer , intent(inout) :: ci
    integer , intent(inout) :: pi
    integer :: m
    real(r8) :: wtlunit
    li = li + 1
    lptr%ncolumns(li) = nveg
    lptr%coli(li) = ci + 1
    lptr%colf(li) = ci + nveg
    lptr%npfts(li) = nveg
    lptr%pfti(li) = pi + 1
    lptr%pftf(li) = pi + nveg
    lptr%area(li) = gptr%area(gi) * wtveg
    lptr%gridcell(li) = gi
    lptr%wtgcell(li) = wtveg
    lptr%ifspecial(li) = .false.
    lptr%lakpoi(li) = .false.
    lptr%itype(li) = istsoil
!dir$ concurrent
    do m = 1, maxpatch_pft
       if (wtxy(gi,m) > 0.) then
          wtlunit = wtxy(gi,m) / wtveg
          ci = ci + 1
          cptr%npfts(ci) = 1
          cptr%pfti(ci) = ci
          cptr%pftf(ci) = ci
          cptr%area(ci) = lptr%area(li) * wtlunit
          cptr%landunit(ci) = li
          cptr%gridcell(ci) = gi
          cptr%wtlunit(ci) = wtlunit
          cptr%wtgcell(ci) = cptr%area(ci) / gptr%area(gi)
          cptr%itype(ci) = 1
          pi = pi + 1
          pptr%column(pi) = ci
          pptr%landunit(pi) = li
          pptr%gridcell(pi) = gi
          pptr%wtcol(pi) = 1.0
          pptr%wtlunit(pi) = cptr%wtlunit(ci)
          pptr%area(pi) = cptr%area(ci)
          pptr%wtgcell(pi) = pptr%area(pi) / gptr%area(gi)
          pptr%mxy(pi) = m
          pptr%itype(pi) = vegxy(gi,m)
       end if
    end do
  end subroutine landunit_veg_noncompete
  subroutine landunit_special ( i, j, m, gi, li, ci, pi)
    use pftvarcon, only : noveg
    use clm_varcon, only : istice, istwet, istdlak, isturb
    use clm_varpar, only : npatch_lake, npatch_wet
    implicit none
    integer, intent(in) :: i
    integer, intent(in) :: j
    integer, intent(in) :: m
    integer, intent(in) :: gi
    integer, intent(inout) :: li
    integer, intent(inout) :: ci
    integer, intent(inout) :: pi
    integer :: c
    integer :: ncols
    integer :: npfts
    integer :: ier
    real(r8) :: weight
    integer :: itype
    if (m == npatch_lake) then
       itype = istdlak
    else if (m == npatch_wet) then
       itype = istwet
    else if (m == npatch_glacier) then
       itype = istice
    else if (m == npatch_urban) then
       itype = isturb
    else
       write(6,*)'special landunit are currently only:', &
            ' deep lake, wetland, glacier or urban)'
       call endrun()
    endif
    li = li + 1
    lptr%ncolumns(li) = 1
    lptr%coli(li) = ci + 1
    lptr%colf(li) = ci + 1
    lptr%npfts(li) = 1
    lptr%pfti(li) = pi + 1
    lptr%pftf(li) = pi + 1
    lptr%area(li) = gptr%area(gi) * wtxy(gi,m)
    lptr%gridcell(li) = gi
    lptr%wtgcell(li) = lptr%area(li) / gptr%area(gi)
    lptr%ifspecial(li) = .true.
    if (itype == istdlak) then
       lptr%lakpoi(li) = .true.
    else
       lptr%lakpoi(li) = .false.
    end if
    lptr%itype(li) = itype
    ncols = 1
    do c = 1,ncols
       weight = 1.0/ncols
       ci = ci + c
       cptr%npfts(ci) = 1
       cptr%pfti(ci) = pi + 1
       cptr%pftf(ci) = pi + 1
       cptr%area(ci) = lptr%area(li) * weight
       cptr%landunit(ci) = li
       cptr%gridcell(ci) = gi
       cptr%wtlunit(ci) = weight
       cptr%wtgcell(ci) = cptr%area(ci) / gptr%area(gi)
       cptr%itype(ci) = 1
       npfts = 1
       weight = 1.0/npfts
       pi = pi + 1
       pptr%column(pi) = ci
       pptr%landunit(pi) = li
       pptr%gridcell(pi) = gi
       pptr%area(pi) = lptr%area(li) * weight
       pptr%wtcol(pi) = weight
       pptr%wtlunit(pi) = cptr%wtlunit(ci)
       pptr%wtgcell(pi) = pptr%area(pi) / gptr%area(gi)
       pptr%mxy(pi) = m
       pptr%itype(pi) = noveg
    end do
  end subroutine landunit_special
  subroutine landunit_crop_noncompete (ncrop, wtcrop, i, j, &
                                       gi, li, ci, pi)
    use clm_varcon, only : istsoil
    use clm_varpar, only : npatch_crop
    implicit none
    integer , intent(in) :: ncrop
    real(r8), intent(in) :: wtcrop
    integer , intent(in) :: i
    integer , intent(in) :: j
    integer , intent(in) :: gi
    integer , intent(inout) :: li
    integer , intent(inout) :: ci
    integer , intent(inout) :: pi
    integer :: m
    real(r8) :: wtlunit
    li = li + 1
    lptr%ncolumns(li) = ncrop
    lptr%coli(li) = ci + 1
    lptr%colf(li) = ci + ncrop
    lptr%npfts(li) = ncrop
    lptr%pfti(li) = pi + 1
    lptr%pftf(li) = pi + ncrop
    lptr%area(li) = gptr%area(gi) * wtcrop
    lptr%gridcell(li) = gi
    lptr%wtgcell(li) = wtcrop
    lptr%ifspecial(li) = .false.
    lptr%lakpoi(li) = .false.
    lptr%urbpoi(li) = .false.
    lptr%itype(li) = istsoil
!dir$ concurrent
    do m = npatch_glacier+1, npatch_crop
       if (wtxy(gi,m) > 0.) then
          wtlunit = wtxy(gi,m) / wtcrop
          ci = ci + 1
          cptr%npfts(ci) = 1
          cptr%area(ci) = lptr%area(li) * wtlunit
          cptr%landunit(ci) = li
          cptr%gridcell(ci) = gi
          cptr%wtlunit(ci) = wtlunit
          cptr%wtgcell(ci) = cptr%area(ci) / gptr%area(gi)
          cptr%itype(ci) = 1
          pi = pi + 1
          pptr%column(pi) = ci
          pptr%landunit(pi) = li
          pptr%gridcell(pi) = gi
          pptr%wtcol(pi) = 1.0
          pptr%wtlunit(pi) = cptr%wtlunit(ci)
          pptr%area(pi) = cptr%area(ci)
          pptr%wtgcell(pi) = pptr%area(pi) / gptr%area(gi)
          pptr%mxy(pi) = m
          pptr%itype(pi) = vegxy(gi,m)
          cptr%pfti(ci) = pi
          cptr%pftf(ci) = pi
       end if
    end do
  end subroutine landunit_crop_noncompete
end module initGridcellsMod
module FracWetMod
  implicit none
  save
  public :: FracWet
contains
  subroutine FracWet(numf, filter)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    implicit none
    integer, intent(in) :: numf
    integer, intent(in) :: filter(numf)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: dewmx(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: esai(:)
    real(r8), pointer :: h2ocan(:)
    real(r8), pointer :: fwet(:)
    real(r8), pointer :: fdry(:)
    integer :: fp,p
    real(r8) :: vegt
    real(r8) :: dewmxi
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    dewmx => clm3%g%l%c%p%pps%dewmx
    elai => clm3%g%l%c%p%pps%elai
    esai => clm3%g%l%c%p%pps%esai
    h2ocan => clm3%g%l%c%p%pws%h2ocan
    fwet => clm3%g%l%c%p%pps%fwet
    fdry => clm3%g%l%c%p%pps%fdry
!dir$ concurrent
    do fp = 1,numf
       p = filter(fp)
       if (frac_veg_nosno(p) == 1) then
          if (h2ocan(p) > 0._r8) then
             vegt = frac_veg_nosno(p)*(elai(p) + esai(p))
             dewmxi = 1.0_r8/dewmx(p)
             fwet(p) = ((dewmxi/vegt)*h2ocan(p))**0.666666666666_r8
             fwet(p) = min (fwet(p),1.0_r8)
          else
             fwet(p) = 0._r8
          end if
          fdry(p) = (1._r8-fwet(p))*elai(p)/(elai(p)+esai(p))
       else
          fwet(p) = 0._r8
          fdry(p) = 0._r8
       end if
    end do
  end subroutine FracWet
end module FracWetMod
module FrictionVelocityMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  save
  public :: FrictionVelocity
  public :: MoninObukIni
  private :: StabilityFunc1
  private :: StabilityFunc2
contains
  subroutine FrictionVelocity(lbn, ubn, fn, filtern, &
                              displa, z0m, z0h, z0q, &
                              obu, iter, ur, um, ustar, &
                              temp1, temp2, temp12m, temp22m, fm, landunit_index)
   use clmtype
   use clm_varcon, only : vkc
   implicit none
   integer , intent(in) :: lbn, ubn
   integer , intent(in) :: fn
   integer , intent(in) :: filtern(fn)
   real(r8), intent(in) :: displa(lbn:ubn)
   real(r8), intent(in) :: z0m(lbn:ubn)
   real(r8), intent(in) :: z0h(lbn:ubn)
   real(r8), intent(in) :: z0q(lbn:ubn)
   real(r8), intent(in) :: obu(lbn:ubn)
   integer, intent(in) :: iter
   real(r8), intent(in) :: ur(lbn:ubn)
   real(r8), intent(in) :: um(lbn:ubn)
   logical, optional, intent(in) :: landunit_index
   real(r8), intent(out) :: ustar(lbn:ubn)
   real(r8), intent(out) :: temp1(lbn:ubn)
   real(r8), intent(out) :: temp12m(lbn:ubn)
   real(r8), intent(out) :: temp2(lbn:ubn)
   real(r8), intent(out) :: temp22m(lbn:ubn)
   real(r8), intent(inout) :: fm(lbn:ubn)
   integer , pointer :: ngridcell(:)
   real(r8), pointer :: forc_hgt_u_pft(:)
   real(r8), pointer :: forc_hgt_t_pft(:)
   real(r8), pointer :: forc_hgt_q_pft(:)
   integer , pointer :: pfti(:)
   integer , pointer :: pftf(:)
   real(r8), pointer :: u10(:)
   real(r8), pointer :: fv(:)
   real(r8), pointer :: vds(:)
   real(r8), parameter :: zetam = 1.574_r8
   real(r8), parameter :: zetat = 0.465_r8
   integer :: f
   integer :: n
   integer :: g
   integer :: pp
   real(r8):: zldis(lbn:ubn)
   real(r8):: zeta(lbn:ubn)
   real(r8) :: vds_tmp
   if (present(landunit_index)) then
     ngridcell => clm3%g%l%gridcell
   else
     ngridcell => clm3%g%l%c%p%gridcell
   end if
   vds => clm3%g%l%c%p%pps%vds
   u10 => clm3%g%l%c%p%pps%u10
   fv => clm3%g%l%c%p%pps%fv
   pfti => clm3%g%l%pfti
   pftf => clm3%g%l%pftf
   forc_hgt_u_pft => clm3%g%l%c%p%pps%forc_hgt_u_pft
   forc_hgt_t_pft => clm3%g%l%c%p%pps%forc_hgt_t_pft
   forc_hgt_q_pft => clm3%g%l%c%p%pps%forc_hgt_q_pft
!dir$ concurrent
   do f = 1, fn
      n = filtern(f)
      g = ngridcell(n)
      if (present(landunit_index)) then
        zldis(n) = forc_hgt_u_pft(pfti(n))-displa(n)
      else
        zldis(n) = forc_hgt_u_pft(n)-displa(n)
      end if
      zeta(n) = zldis(n)/obu(n)
      if (zeta(n) < -zetam) then
         ustar(n) = vkc*um(n)/(log(-zetam*obu(n)/z0m(n))&
              - StabilityFunc1(-zetam) &
              + StabilityFunc1(z0m(n)/obu(n)) &
              + 1.14_r8*((-zeta(n))**0.333_r8-(zetam)**0.333_r8))
      else if (zeta(n) < 0._r8) then
         ustar(n) = vkc*um(n)/(log(zldis(n)/z0m(n))&
              - StabilityFunc1(zeta(n))&
              + StabilityFunc1(z0m(n)/obu(n)))
      else if (zeta(n) <= 1._r8) then
         ustar(n) = vkc*um(n)/(log(zldis(n)/z0m(n)) + 5._r8*zeta(n) -5._r8*z0m(n)/obu(n))
      else
         ustar(n) = vkc*um(n)/(log(obu(n)/z0m(n))+5._r8-5._r8*z0m(n)/obu(n) &
              +(5._r8*log(zeta(n))+zeta(n)-1._r8))
      end if
      if (zeta(n) < 0._r8) then
         vds_tmp = 2.e-3_r8*ustar(n) * ( 1._r8 + (300._r8/(-obu(n)))**0.666_r8)
      else
         vds_tmp = 2.e-3_r8*ustar(n)
      endif
      if (present(landunit_index)) then
         do pp = pfti(n),pftf(n)
            vds(pp) = vds_tmp
         end do
      else
         vds(n) = vds_tmp
      end if
      if (present(landunit_index)) then
        zldis(n) = forc_hgt_t_pft(pfti(n))-displa(n)
      else
        zldis(n) = forc_hgt_t_pft(n)-displa(n)
      end if
      zeta(n) = zldis(n)/obu(n)
      if (zeta(n) < -zetat) then
         temp1(n) = vkc/(log(-zetat*obu(n)/z0h(n))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(n)/obu(n)) &
              + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(n))**(-0.333_r8)))
      else if (zeta(n) < 0._r8) then
         temp1(n) = vkc/(log(zldis(n)/z0h(n)) &
              - StabilityFunc2(zeta(n)) &
              + StabilityFunc2(z0h(n)/obu(n)))
      else if (zeta(n) <= 1._r8) then
         temp1(n) = vkc/(log(zldis(n)/z0h(n)) + 5._r8*zeta(n) - 5._r8*z0h(n)/obu(n))
      else
         temp1(n) = vkc/(log(obu(n)/z0h(n)) + 5._r8 - 5._r8*z0h(n)/obu(n) &
              + (5._r8*log(zeta(n))+zeta(n)-1._r8))
      end if
      if (present(landunit_index)) then
       if (forc_hgt_q_pft(pfti(n)) == forc_hgt_t_pft(pfti(n)) .and. z0q(n) == z0h(n)) then
         temp2(n) = temp1(n)
       else
         zldis(n) = forc_hgt_q_pft(pfti(n))-displa(n)
         zeta(n) = zldis(n)/obu(n)
         if (zeta(n) < -zetat) then
            temp2(n) = vkc/(log(-zetat*obu(n)/z0q(n)) &
                 - StabilityFunc2(-zetat) &
                 + StabilityFunc2(z0q(n)/obu(n)) &
                 + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(n))**(-0.333_r8)))
         else if (zeta(n) < 0._r8) then
            temp2(n) = vkc/(log(zldis(n)/z0q(n)) &
                 - StabilityFunc2(zeta(n)) &
                 + StabilityFunc2(z0q(n)/obu(n)))
         else if (zeta(n) <= 1._r8) then
            temp2(n) = vkc/(log(zldis(n)/z0q(n)) + 5._r8*zeta(n)-5._r8*z0q(n)/obu(n))
         else
            temp2(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                 + (5._r8*log(zeta(n))+zeta(n)-1._r8))
         end if
       end if
      else
       if (forc_hgt_q_pft(n) == forc_hgt_t_pft(n) .and. z0q(n) == z0h(n)) then
         temp2(n) = temp1(n)
       else
         zldis(n) = forc_hgt_q_pft(n)-displa(n)
         zeta(n) = zldis(n)/obu(n)
         if (zeta(n) < -zetat) then
            temp2(n) = vkc/(log(-zetat*obu(n)/z0q(n)) &
                 - StabilityFunc2(-zetat) &
                 + StabilityFunc2(z0q(n)/obu(n)) &
                 + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(n))**(-0.333_r8)))
         else if (zeta(n) < 0._r8) then
            temp2(n) = vkc/(log(zldis(n)/z0q(n)) &
                 - StabilityFunc2(zeta(n)) &
                 + StabilityFunc2(z0q(n)/obu(n)))
         else if (zeta(n) <= 1._r8) then
            temp2(n) = vkc/(log(zldis(n)/z0q(n)) + 5._r8*zeta(n)-5._r8*z0q(n)/obu(n))
         else
            temp2(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                 + (5._r8*log(zeta(n))+zeta(n)-1._r8))
         end if
       endif
      endif
      zldis(n) = 2.0_r8 + z0h(n)
      zeta(n) = zldis(n)/obu(n)
      if (zeta(n) < -zetat) then
         temp12m(n) = vkc/(log(-zetat*obu(n)/z0h(n))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(n)/obu(n)) &
              + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(n))**(-0.333_r8)))
      else if (zeta(n) < 0._r8) then
         temp12m(n) = vkc/(log(zldis(n)/z0h(n)) &
              - StabilityFunc2(zeta(n)) &
              + StabilityFunc2(z0h(n)/obu(n)))
      else if (zeta(n) <= 1._r8) then
         temp12m(n) = vkc/(log(zldis(n)/z0h(n)) + 5._r8*zeta(n) - 5._r8*z0h(n)/obu(n))
      else
         temp12m(n) = vkc/(log(obu(n)/z0h(n)) + 5._r8 - 5._r8*z0h(n)/obu(n) &
              + (5._r8*log(zeta(n))+zeta(n)-1._r8))
      end if
      if (z0q(n) == z0h(n)) then
         temp22m(n) = temp12m(n)
      else
         zldis(n) = 2.0_r8 + z0q(n)
         zeta(n) = zldis(n)/obu(n)
         if (zeta(n) < -zetat) then
            temp22m(n) = vkc/(log(-zetat*obu(n)/z0q(n)) - &
                 StabilityFunc2(-zetat) + StabilityFunc2(z0q(n)/obu(n)) &
                 + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(n))**(-0.333_r8)))
         else if (zeta(n) < 0._r8) then
            temp22m(n) = vkc/(log(zldis(n)/z0q(n)) - &
                 StabilityFunc2(zeta(n))+StabilityFunc2(z0q(n)/obu(n)))
         else if (zeta(n) <= 1._r8) then
            temp22m(n) = vkc/(log(zldis(n)/z0q(n)) + 5._r8*zeta(n)-5._r8*z0q(n)/obu(n))
         else
            temp22m(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                 + (5._r8*log(zeta(n))+zeta(n)-1._r8))
         end if
      end if
   end do
   end subroutine FrictionVelocity
   real(r8) function StabilityFunc1(zeta)
      use shr_const_mod, only: SHR_CONST_PI
      implicit none
      real(r8), intent(in) :: zeta
      real(r8) :: chik, chik2
      chik2 = sqrt(1._r8-16._r8*zeta)
      chik = sqrt(chik2)
      StabilityFunc1 = 2._r8*log((1._r8+chik)*0.5_r8) &
           + log((1._r8+chik2)*0.5_r8)-2._r8*atan(chik)+SHR_CONST_PI*0.5_r8
    end function StabilityFunc1
   real(r8) function StabilityFunc2(zeta)
     use shr_const_mod, only: SHR_CONST_PI
     implicit none
     real(r8), intent(in) :: zeta
     real(r8) :: chik2
     chik2 = sqrt(1._r8-16._r8*zeta)
     StabilityFunc2 = 2._r8*log((1._r8+chik2)*0.5_r8)
   end function StabilityFunc2
  subroutine MoninObukIni (ur, thv, dthv, zldis, z0m, um, obu)
    use clm_varcon, only : grav
    implicit none
    real(r8), intent(in) :: ur
    real(r8), intent(in) :: thv
    real(r8), intent(in) :: dthv
    real(r8), intent(in) :: zldis
    real(r8), intent(in) :: z0m
    real(r8), intent(out) :: um
    real(r8), intent(out) :: obu
    real(r8) :: wc
    real(r8) :: rib
    real(r8) :: zeta
    real(r8) :: ustar
    ustar=0.06_r8
    wc=0.5_r8
    if (dthv >= 0._r8) then
       um=max(ur,0.1_r8)
    else
       um=sqrt(ur*ur+wc*wc)
    endif
    rib=grav*zldis*dthv/(thv*um*um)
    if (rib >= 0._r8) then
       zeta = rib*log(zldis/z0m)/(1._r8-5._r8*min(rib,0.19_r8))
       zeta = min(2._r8,max(zeta,0.01_r8 ))
    else
       zeta=rib*log(zldis/z0m)
       zeta = max(-100._r8,min(zeta,-0.01_r8 ))
    endif
    obu=zldis/zeta
  end subroutine MoninObukIni
end module FrictionVelocityMod
module VOCEmissionMod
  use module_cam_support, only: endrun
  implicit none
  save
  public :: VOCEmission
contains
  subroutine VOCEmission (lbp, ubp, num_soilp, filter_soilp )
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varpar , only : nvoc, numpft
    use shr_const_mod, only : SHR_CONST_RGAS
    use clm_varcon , only : denice
    use clm_varpar , only : nlevsoi
    use pftvarcon , only : ndllf_evr_tmp_tree, ndllf_evr_brl_tree, &
                              ndllf_dcd_brl_tree, nbrdlf_evr_trp_tree, &
                              nbrdlf_evr_tmp_tree, nbrdlf_dcd_brl_shrub, &
                              nbrdlf_dcd_trp_tree, nbrdlf_dcd_tmp_tree, &
                              nbrdlf_dcd_brl_tree, nbrdlf_evr_shrub, &
                              nc3_arctic_grass, nc4_grass, noveg
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_soilp
    integer, intent(in) :: filter_soilp(num_soilp)
    integer , pointer :: pgridcell(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: ivt(:)
    real(r8), pointer :: t_veg(:)
    real(r8), pointer :: fsun(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: clayfrac(:)
    real(r8), pointer :: sandfrac(:)
    real(r8), pointer :: forc_solad(:,:)
    real(r8), pointer :: forc_solai(:,:)
    real(r8), pointer :: sla(:)
    real(r8), pointer :: h2osoi_vol(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: coszen(:)
    real(r8), pointer :: efisop(:,:)
    real(r8), pointer :: elai_p(:)
    real(r8), pointer :: t_veg24(:)
    real(r8), pointer :: t_veg240(:)
    real(r8), pointer :: fsun24(:)
    real(r8), pointer :: fsun240(:)
    real(r8), pointer :: forc_solad24(:)
    real(r8), pointer :: forc_solai24(:)
    real(r8), pointer :: forc_solad240(:)
    real(r8), pointer :: forc_solai240(:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), parameter :: smpmax = 2.57e5_r8
    real(r8), pointer :: vocflx(:,:)
    real(r8), pointer :: vocflx_tot(:)
    real(r8), pointer :: vocflx_1(:)
    real(r8), pointer :: vocflx_2(:)
    real(r8), pointer :: vocflx_3(:)
    real(r8), pointer :: vocflx_4(:)
    real(r8), pointer :: vocflx_5(:)
    real(r8), pointer :: Eopt_out(:)
    real(r8), pointer :: topt_out(:)
    real(r8), pointer :: alpha_out(:)
    real(r8), pointer :: cp_out(:)
    real(r8), pointer :: paru_out(:)
    real(r8), pointer :: par24u_out(:)
    real(r8), pointer :: par240u_out(:)
    real(r8), pointer :: para_out(:)
    real(r8), pointer :: par24a_out(:)
    real(r8), pointer :: par240a_out(:)
    real(r8), pointer :: gamma_out(:)
    real(r8), pointer :: gammaT_out(:)
    real(r8), pointer :: gammaP_out(:)
    real(r8), pointer :: gammaL_out(:)
    real(r8), pointer :: gammaA_out(:)
    real(r8), pointer :: gammaS_out(:)
    integer :: fp,p,g,c,n,j
    integer :: ct_bad
    real(r8) :: epsilon(lbp:ubp)
    real(r8) :: par
    real(r8) :: par24
    real(r8) :: par240
    real(r8) :: density
    real(r8) :: gamma(lbp:ubp)
    real(r8) :: gamma_p
    real(r8) :: gamma_l
    real(r8) :: gamma_t
    real(r8) :: gamma_a
    real(r8) :: gamma_sm
    real(r8) :: x
    real(r8) :: Eopt
    real(r8) :: topt
    real(r8) :: cp
    real(r8) :: alpha
    real(r8) :: elai_prev
    real(r8) :: fnew, fgro, fmat, fsen
    real(r8) :: nl
    real(r8) :: theta_ice
    real(r8) :: wilt
    real(r8) :: theta1
    real(r8), parameter :: R = SHR_CONST_RGAS*0.001_r8
    real(r8), parameter :: scale_mw =0.882_r8
    real(r8), parameter :: alpha_fix = 0.001_r8
    real(r8), parameter :: cp_fix = 1.21_r8
    real(r8), parameter :: ct1 = 95.0_r8
    real(r8), parameter :: ct2 = 230.0_r8
    real(r8), parameter :: ct3 = 0.00831_r8
    real(r8), parameter :: topt_fix = 317._r8
    real(r8), parameter :: Eopt_fix = 2.26_r8
    real(r8), parameter :: tstd = 303.15_r8
    real(r8), parameter :: bet = 0.09_r8
    real(r8), parameter :: clai1 = 0.49_r8
    real(r8), parameter :: clai2 = 0.2_r8
    real(r8), parameter :: clai3 = 5.0_r8
    real(r8), parameter :: Anew = 0.01_r8
    real(r8), parameter :: Agro = 0.5_r8
    real(r8), parameter :: Amat = 1.0_r8
    real(r8), parameter :: Asen = 0.33_r8
    real(r8), parameter :: cce = 0.40_r8
    real(r8), parameter :: cce1 = 0.47_r8
    real(r8), parameter :: ca1 = 0.004_r8
    real(r8), parameter :: ca2 = 0.0005_r8
    real(r8), parameter :: ca3 = 0.0468_r8
    real(r8), parameter :: par0_sun = 200._r8
    real(r8), parameter :: par0_shade = 50._r8
    real(r8), parameter :: co1 = 313._r8
    real(r8), parameter :: co2 = 0.6_r8
    real(r8), parameter :: co3 = 2.034_r8
    real(r8), parameter :: co4 = 0.05_r8
    real(r8), parameter :: tstd0 = 297_r8
    real(r8), parameter :: deltheta1=0.06_r8
    real(r8) :: hardwire_sla(0:numpft)
    real(r8) :: slarea(lbp:ubp)
    real(r8) :: hardwire_droot(0:numpft)
    forc_solad => clm_a2l%forc_solad
    forc_solai => clm_a2l%forc_solai
    efisop => clm3%g%gve%efisop
    h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    dz => clm3%g%l%c%cps%dz
    bsw => clm3%g%l%c%cps%bsw
    watsat => clm3%g%l%c%cps%watsat
    sucsat => clm3%g%l%c%cps%sucsat
    pgridcell => clm3%g%l%c%p%gridcell
    pcolumn => clm3%g%l%c%p%column
    ivt => clm3%g%l%c%p%itype
    t_veg => clm3%g%l%c%p%pes%t_veg
    fsun => clm3%g%l%c%p%pps%fsun
    elai => clm3%g%l%c%p%pps%elai
    clayfrac => clm3%g%l%c%p%pps%clayfrac
    sandfrac => clm3%g%l%c%p%pps%sandfrac
    vocflx => clm3%g%l%c%p%pvf%vocflx
    vocflx_tot => clm3%g%l%c%p%pvf%vocflx_tot
    vocflx_1 => clm3%g%l%c%p%pvf%vocflx_1
    vocflx_2 => clm3%g%l%c%p%pvf%vocflx_2
    vocflx_3 => clm3%g%l%c%p%pvf%vocflx_3
    vocflx_4 => clm3%g%l%c%p%pvf%vocflx_4
    vocflx_5 => clm3%g%l%c%p%pvf%vocflx_5
    Eopt_out => clm3%g%l%c%p%pvf%Eopt_out
    topt_out => clm3%g%l%c%p%pvf%topt_out
    alpha_out => clm3%g%l%c%p%pvf%alpha_out
    cp_out => clm3%g%l%c%p%pvf%cp_out
    paru_out => clm3%g%l%c%p%pvf%paru_out
    par24u_out => clm3%g%l%c%p%pvf%par24u_out
    par240u_out => clm3%g%l%c%p%pvf%par240u_out
    para_out => clm3%g%l%c%p%pvf%para_out
    par24a_out => clm3%g%l%c%p%pvf%par24a_out
    par240a_out => clm3%g%l%c%p%pvf%par240a_out
    gammaL_out => clm3%g%l%c%p%pvf%gammaL_out
    gammaT_out => clm3%g%l%c%p%pvf%gammaT_out
    gammaP_out => clm3%g%l%c%p%pvf%gammaP_out
    gammaA_out => clm3%g%l%c%p%pvf%gammaA_out
    gammaS_out => clm3%g%l%c%p%pvf%gammaS_out
    gamma_out => clm3%g%l%c%p%pvf%gamma_out
    sla => clm3%g%l%c%p%pps%slasha
    t_veg24 => clm3%g%l%c%p%pvs%t_veg24
    t_veg240 => clm3%g%l%c%p%pvs%t_veg240
    forc_solad24 => clm3%g%l%c%p%pvs%fsd24
    forc_solad240 => clm3%g%l%c%p%pvs%fsd240
    forc_solai24 => clm3%g%l%c%p%pvs%fsi24
    forc_solai240 => clm3%g%l%c%p%pvs%fsi240
    fsun24 => clm3%g%l%c%p%pvs%fsun24
    fsun240 => clm3%g%l%c%p%pvs%fsun240
    elai_p => clm3%g%l%c%p%pvs%elai_p
    hardwire_sla(noveg) = 0._r8
    hardwire_sla(ndllf_evr_tmp_tree) = 0.0125_r8
    hardwire_sla(ndllf_evr_brl_tree) = 0.0125_r8
    hardwire_sla(ndllf_dcd_brl_tree) = 0.0125_r8
    hardwire_sla(nbrdlf_evr_trp_tree) = 0.0250_r8
    hardwire_sla(nbrdlf_evr_tmp_tree) = 0.0250_r8
    hardwire_sla(nbrdlf_dcd_trp_tree) = 0.0250_r8
    hardwire_sla(nbrdlf_dcd_tmp_tree:nbrdlf_dcd_brl_shrub) = 0.0250_r8
    hardwire_sla(nc3_arctic_grass:numpft) = 0.0200_r8
    hardwire_droot(noveg) = 0._r8
    hardwire_droot(ndllf_evr_tmp_tree:ndllf_evr_brl_tree) = 1.8_r8
    hardwire_droot(ndllf_dcd_brl_tree) = 2.0_r8
    hardwire_droot(nbrdlf_evr_trp_tree:nbrdlf_evr_tmp_tree) = 3.0_r8
    hardwire_droot(nbrdlf_dcd_trp_tree:nbrdlf_dcd_brl_tree) = 2.0_r8
    hardwire_droot(nbrdlf_evr_shrub:nbrdlf_dcd_brl_shrub) = 2.5_r8
    hardwire_droot(nc3_arctic_grass:numpft) = 1.5_r8
    vocflx(lbp:ubp, :)=0._r8
    do fp = 1,num_soilp
       p = filter_soilp(fp)
       slarea(p) = hardwire_sla(ivt(p))
    end do
    do n = 1, nvoc
       select case (n)
       case(1)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
          epsilon(p) = 0._r8
             if ( ivt(p) == ndllf_evr_tmp_tree &
             .or. ivt(p) == ndllf_evr_brl_tree) then
                 epsilon(p) = efisop(2,g)*scale_mw
             else if (ivt(p) == ndllf_dcd_brl_tree) then
                 epsilon(p) = efisop(3,g)*scale_mw
             else if (ivt(p) >= nbrdlf_evr_trp_tree &
             .and. ivt(p) <= nbrdlf_dcd_brl_tree) then
                 epsilon(p) = efisop(1,g)*scale_mw
             else if (ivt(p) >= nbrdlf_evr_shrub &
             .and. ivt(p) <= nbrdlf_dcd_brl_shrub) then
                 epsilon(p) = efisop(4,g)*scale_mw
             else if (ivt(p) >= nc3_arctic_grass &
             .and. ivt(p) <= nc4_grass) then
                 epsilon(p) = efisop(5,g)*scale_mw
             else if (ivt(p) > nc4_grass) then
                 epsilon(p) =efisop(6,g)*scale_mw
             end if
          end do
       case(2)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             epsilon(p) = 0._r8
             if ( ivt(p) >= ndllf_evr_tmp_tree &
             .and. ivt(p) <= ndllf_evr_brl_tree) then
                epsilon(p) = 2.0_r8
             else if (ivt(p) == ndllf_dcd_brl_tree) then
                epsilon(p) = 1.6_r8
             else if (ivt(p) >= nbrdlf_evr_trp_tree &
             .and. ivt(p) <= nbrdlf_dcd_brl_tree) then
                epsilon(p) = 0.4_r8
             else if (ivt(p) >= nbrdlf_evr_shrub &
             .and. ivt(p) <= nbrdlf_dcd_brl_shrub) then
                epsilon(p) = 0.8_r8
             else if (ivt(p) >= nc3_arctic_grass &
             .and. ivt(p) <= numpft) then
                epsilon(p) = 0.1_r8
             end if
          end do
       case (3)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             epsilon(p) = 1.0_r8
          end do
       case (4)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             epsilon(p) = 1.0_r8
          end do
       case (5)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             epsilon(p) = 0.3_r8
          end do
       case default
          write(6,*)'only nvocs up to index 5 are currently supported'
          call endrun()
       end select
       ct_bad=0
       select case (n)
       case (1)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             c = pcolumn(p)
             if ( (fsun240(p) > 0.0_r8) .and. (fsun240(p) < 1.e30_r8) ) then
                 gamma_l = cce * elai(p)
             else
                 gamma_l = cce1 * elai(p)
             end if
      gammaL_out(p)=gamma_l
             if ( (fsun240(p) > 0._r8) .and. (fsun240(p) < 1._r8) .and. (forc_solad240(p) > 0._r8) &
             .and. (forc_solai240(p) > 0._r8)) then
                par = (forc_solad(g,1) + fsun(p) * forc_solai(g,1)) * 4.6_r8
                par24 = (forc_solad24(p) + fsun24(p) * forc_solai24(p)) * 4.6_r8
                par240 = (forc_solad240(p) + fsun240(p) * forc_solai240(p)) * 4.6_r8
                alpha = ca1 - ca2 * log(par240)
                cp = ca3 * exp(ca2 * (par24-par0_sun))*par240**(0.6_r8)
                gamma_p = fsun(p) * ( cp * alpha*par * (1._r8 + alpha*alpha*par*par)**(-0.5_r8) )
         paru_out(p)=par
  par24u_out(p)=par24
                par240u_out(p)=par240
                par = ((1._r8 - fsun(p)) * forc_solai(g,1)) * 4.6_r8
                par24 = ((1._r8 - fsun24(p)) * forc_solai24(p)) * 4.6_r8
                par240 = ((1._r8 - fsun240(p)) * forc_solai240(p)) * 4.6_r8
                alpha = ca1 - ca2 * log(par240)
                cp = ca3 * exp(ca2 * (par24-par0_shade))*par240**(0.6_r8)
                par = ((1._r8 - fsun(p)) * forc_solai(g,1)) * 4.6_r8
                gamma_p = gamma_p + (1-fsun(p)) * (cp*alpha*par*(1._r8 + alpha*alpha*par*par)**(-0.5_r8))
                para_out(p)=par
  par24a_out(p)=par24
   par240a_out(p)=par240
             else
                par = (forc_solad(g,1) + fsun(p) * forc_solai(g,1)) * 4.6_r8
                alpha = alpha_fix
                cp = cp_fix
                gamma_p = fsun(p) * ( cp * alpha*par * (1._r8 + alpha*alpha*par*par)**(-0.5_r8) )
  paru_out(p)=par
         par24u_out(p)=-999
         par240u_out(p)=-999
                par = ((1._r8 - fsun(p)) * forc_solai(g,1)) * 4.6_r8
                gamma_p = gamma_p + (1-fsun(p)) * (cp*alpha*par*(1._r8 + alpha*alpha*par*par)**(-0.5_r8))
  para_out(p)=par
                par24a_out(p)=-999
                par240a_out(p)=-999
             end if
             alpha_out(p)=alpha
             cp_out(p)=cp
             gammaP_out(p)=gamma_p
             if ( (t_veg240(p) > 0.0_r8) .and. (t_veg240(p) < 1.e30_r8) ) then
                topt = co1 + (co2 * (t_veg240(p)-tstd0))
                Eopt = co3 * exp (co4 * (t_veg24(p)-tstd0)) * exp(co4 * (t_veg240(p) -tstd0))
      else
                topt = topt_fix
                Eopt = Eopt_fix
             endif
             x = ( (1._r8/topt) - (1._r8/(t_veg(p))) ) / ct3
             gamma_t = Eopt * ( ct2 * exp(ct1 * x)/(ct2 - ct1 * (1._r8 - exp(ct2 * x))) )
             topt_out(p)=topt
             Eopt_out(p)=Eopt
             gammaT_out(p)=gamma_t
      if ( (ivt(p) == ndllf_dcd_brl_tree) .or. (ivt(p) >= nbrdlf_dcd_trp_tree) ) then
                if ( (elai_p(p) > 0.0_r8) .and. (elai_p(p) < 1.e30_r8) )then
                   elai_prev = 2._r8*elai_p(p)-elai(p)
                   if (elai_prev == elai(p)) then
                      fnew = 0.0_r8
                      fgro = 0.0_r8
                      fmat = 1.0_r8
                      fsen = 0.0_r8
                   else if (elai_prev > elai(p)) then
                      fnew = 0.0_r8
                      fgro = 0.0_r8
                      fmat = 1.0_r8 - (elai_prev - elai(p))/elai_prev
                      fsen = (elai_prev - elai(p))/elai_prev
                   else if (elai_prev < elai(p)) then
                      fnew = 1 - (elai_prev / elai(p))
                      fgro = 0.0_r8
                      fmat = (elai_prev / elai(p))
                      fsen = 0.0_r8
                   end if
                   gamma_a = fnew * Anew + fgro * Agro + fmat * Amat + fsen * Asen
         else
                   gamma_a = 1.0_r8
                end if
             else
                gamma_a = 1.0_r8
             end if
             gammaA_out(p)=gamma_a
             if ((clayfrac(p) > 0) .and. (sandfrac(p) > 0)) then
               gamma_sm = 0._r8
        nl=0._r8
               do j = 1,nlevsoi
          if (sum(dz(c,1:j)) < hardwire_droot(ivt(p))) then
                   theta_ice = h2osoi_ice(c,j)/(dz(c,j)*denice)
                   wilt = ((smpmax/sucsat(c,j))**(-1._r8/bsw(c,j))) * (watsat(c,j) - theta_ice)
                   theta1 = wilt + deltheta1
                   if (h2osoi_vol(c,j) >= theta1) then
                    gamma_sm = gamma_sm + 1._r8
                   else if ( (h2osoi_vol(c,j) > wilt) .and. (h2osoi_vol(c,j) < theta1) ) then
        gamma_sm = gamma_sm + ( h2osoi_vol(c,j) - wilt ) / deltheta1
                   else
        gamma_sm = gamma_sm + 0._r8
                   end if
     nl=nl+1._r8
                 end if
         end do
        if (nl > 0) then
          gamma_sm = gamma_sm/nl
        endif
             else
        gamma_sm = 1.0_r8
             end if
             gammaS_out(p)=gamma_sm
      gamma(p) = gamma_l * gamma_p * gamma_t * gamma_a * gamma_sm
             if ( (gamma(p) >=0.0_r8) .and. (gamma(p)< 100._r8) ) then
                gamma_out(p)=gamma(p)
      else
                gamma_out(p)=gamma(p)
                write(6,*) 'clh GAMMA: ',gamma(p),gamma_l,gamma_p,gamma_t,gamma_a,gamma_sm
             end if
          end do
       case (2,3,4,5)
          do fp = 1,num_soilp
             p = filter_soilp(fp)
             g = pgridcell(p)
             gamma_t = exp(bet * (t_veg(p) - tstd))
      gamma(p)=gamma_t
          end do
       end select
       do fp = 1,num_soilp
          p = filter_soilp(fp)
          g = pgridcell(p)
          if (ivt(p) > noveg) then
             density = elai(p) / (slarea(p) * 0.5_r8)
          else
             density = 0._r8
          end if
   select case (n)
          case(1)
              vocflx(p,n) = epsilon(p) * gamma(p)
          case(2,3,4,5)
              vocflx(p,n) = epsilon(p) * gamma(p) * density
          end select
       end do
    end do
    do fp = 1,num_soilp
       p = filter_soilp(fp)
       vocflx_tot(p) = 0._r8
    end do
    do n = 1, nvoc
       do fp = 1,num_soilp
          p = filter_soilp(fp)
          vocflx_tot(p) = vocflx_tot(p) + vocflx(p,n)
       end do
    end do
    do fp = 1,num_soilp
       p = filter_soilp(fp)
       g = pgridcell(p)
       vocflx_1(p) = vocflx(p,1)
       vocflx_2(p) = vocflx(p,2)
       vocflx_3(p) = vocflx(p,3)
       vocflx_4(p) = vocflx(p,4)
       vocflx_5(p) = vocflx(p,5)
    end do
  end subroutine VOCEmission
end module VOCEmissionMod
module dynlandMod
   use clmtype
   use decompMod , only : get_proc_bounds
   use shr_kind_mod, only : r8 => shr_kind_r8
   implicit none
   private
   save
   public :: dynland_hwcontent
contains
   subroutine dynland_hwcontent(begg,endg,gcell_liq,gcell_ice,gcell_heat)
   use clm_varcon, only : istsoil,istice,istwet, istdlak,istslak,isturb
   use clm_varcon, only : icol_road_perv,icol_road_imperv,icol_roof
   use clm_varcon, only : icol_sunwall,icol_shadewall
   use clm_varcon, only : cpice, cpliq
   use clm_varpar, only : nlevsno, nlevgrnd
   implicit none
   integer , intent(in) :: begg, endg
   real(r8), intent(out) :: gcell_liq(begg:endg)
   real(r8), intent(out) :: gcell_ice (begg:endg)
   real(r8), intent(out) :: gcell_heat (begg:endg)
   integer :: li,lf
   integer :: ci,cf
   integer :: pi,pf
   integer :: g,l,c,p,k
   real(r8) :: wtgcell
   real(r8) :: wtcol
   real(r8) :: liq
   real(r8) :: ice
   real(r8) :: heat
   real(r8) :: cv
   integer ,pointer :: ltype(:)
   integer ,pointer :: ctype(:)
   integer ,pointer :: ptype(:)
   integer, pointer :: nlev_improad(:)
   real(r8), pointer :: cv_wall(:,:)
   real(r8), pointer :: cv_roof(:,:)
   real(r8), pointer :: cv_improad(:,:)
   integer , pointer :: snl(:)
   real(r8), pointer :: t_soisno(:,:)
   real(r8), pointer :: h2osno(:)
   real(r8), pointer :: h2osoi_liq(:,:)
   real(r8), pointer :: h2osoi_ice(:,:)
   real(r8), pointer :: watsat(:,:)
   real(r8), pointer :: csol(:,:)
   real(r8), pointer :: dz(:,:)
   real(r8), pointer :: wa(:,:)
   type(gridcell_type), pointer :: gptr
   type(landunit_type), pointer :: lptr
   type(column_type) , pointer :: cptr
   type(pft_type) , pointer :: pptr
   gptr => clm3%g
   lptr => clm3%g%l
   cptr => clm3%g%l%c
   pptr => clm3%g%l%c%p
   ltype => clm3%g%l%itype
   ctype => clm3%g%l%c%itype
   ptype => clm3%g%l%c%p%itype
   nlev_improad => clm3%g%l%lps%nlev_improad
   cv_wall => clm3%g%l%lps%cv_wall
   cv_roof => clm3%g%l%lps%cv_roof
   cv_improad => clm3%g%l%lps%cv_improad
   snl => clm3%g%l%c%cps%snl
   watsat => clm3%g%l%c%cps%watsat
   csol => clm3%g%l%c%cps%csol
   dz => clm3%g%l%c%cps%dz
   t_soisno => clm3%g%l%c%ces%t_soisno
   h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
   h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
   h2osno => clm3%g%l%c%cws%h2osno
   do g = begg,endg
      gcell_liq (g) = 0.0_r8
      gcell_ice (g) = 0.0_r8
      gcell_heat (g) = 0.0_r8
      li = gptr%luni(g)
      lf = gptr%lunf(g)
      do l = li,lf
         ci = lptr%coli(l)
         cf = lptr%colf(l)
         do c = ci,cf
            liq = 0.0_r8
            ice = 0.0_r8
            heat = 0.0_r8
            if ( (ltype(l) == istsoil ) &
            .or. (ltype(l) == istwet ) &
            .or. (ltype(l) == istice ) &
            .or. (ltype(l) == isturb .and. ctype(c) == icol_roof ) &
            .or. (ltype(l) == isturb .and. ctype(c) == icol_road_imperv) &
            .or. (ltype(l) == isturb .and. ctype(c) == icol_road_perv )) then
               if ( snl(c) < 0 ) then
                  do k = snl(c)+1,0
                     liq = liq + clm3%g%l%c%cws%h2osoi_liq(c,k)
                     ice = ice + clm3%g%l%c%cws%h2osoi_ice(c,k)
                  end do
               else
                  ice = ice + cptr%cws%h2osno(c)
               end if
            end if
            if ( (ltype(l) == istsoil ) &
            .or. (ltype(l) == istwet ) &
            .or. (ltype(l) == istice ) &
            .or. (ltype(l) == isturb .and. ctype(c) == icol_road_perv )) then
               do k = 1,nlevgrnd
                  liq = liq + cptr%cws%h2osoi_liq(c,k)
                  ice = ice + cptr%cws%h2osoi_ice(c,k)
               end do
            end if
            if ( (ltype(l) == istsoil ) &
            .or. (ltype(l) == istwet ) &
            .or. (ltype(l) == istice ) &
            .or. (ltype(l) == isturb .and. ctype(c) == icol_road_perv )) then
               liq = liq + cptr%cws%wa(c)
            end if
            if (ltype(l) == istsoil ) then
               pi = cptr%pfti(c)
               pf = cptr%pftf(c)
               do p = pi,pf
                  wtcol = pptr%wtcol(p)
                  liq = liq + pptr%pws%h2ocan(p) * wtcol
               end do
            end if
            if ( (ltype(l) /= istslak) .and. ltype(l) /= istdlak) then
               do k = 1,nlevgrnd
                  if (ctype(c)==icol_sunwall .OR. ctype(c)==icol_shadewall) then
                      cv = cv_wall(l,k) * dz(c,k)
                   else if (ctype(c) == icol_roof) then
                      cv = cv_roof(l,k) * dz(c,k)
                   else if (ctype(c) == icol_road_imperv .and. k >= 1 .and. k <= nlev_improad(l)) then
                      cv = cv_improad(l,k) * dz(c,k)
                   else if (ltype(l) /= istwet .AND. ltype(l) /= istice) then
                      cv = csol(c,k)*(1-watsat(c,k))*dz(c,k) + (h2osoi_ice(c,k)*cpice + h2osoi_liq(c,k)*cpliq)
                   else
                      cv = (h2osoi_ice(c,k)*cpice + h2osoi_liq(c,k)*cpliq)
                   endif
                   heat = heat + cv*t_soisno(c,k) / 1.e6_r8
                end do
               if ( snl(c) < 0 ) then
                  do k = snl(c)+1,0
                     cv = cpliq*h2osoi_liq(c,k) + cpice*h2osoi_ice(c,k)
                     heat = heat + cv*t_soisno(c,k) / 1.e6_r8
                  end do
               else if ( h2osno(c) > 0.0_r8) then
                  k = 1
                  cv = cpice*h2osno(c)
                  heat = heat + cv*t_soisno(c,k) / 1.e6_r8
               end if
            end if
            wtgcell = cptr%wtgcell(c)
            gcell_liq (g) = gcell_liq (g) + liq * wtgcell
            gcell_ice (g) = gcell_ice (g) + ice * wtgcell
            gcell_heat (g) = gcell_heat (g) + heat * wtgcell
         end do
      end do
   end do
   end subroutine dynland_hwcontent
end module dynlandMod
module subgridAveMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clmtype , only : clm3
  use clm_varcon, only : spval, isturb, icol_roof, icol_sunwall, icol_shadewall, &
                         icol_road_perv, icol_road_imperv
  use module_cam_support, only: endrun
  implicit none
  save
  public :: p2c
  public :: p2l
  public :: p2g
  public :: c2l
  public :: c2g
  public :: l2g
  interface p2c
     module procedure p2c_1d
     module procedure p2c_2d
     module procedure p2c_1d_filter
     module procedure p2c_2d_filter
  end interface
  interface p2l
     module procedure p2l_1d
     module procedure p2l_2d
  end interface
  interface p2g
     module procedure p2g_1d
     module procedure p2g_2d
  end interface
  interface c2l
     module procedure c2l_1d
     module procedure c2l_2d
  end interface
  interface c2g
     module procedure c2g_1d
     module procedure c2g_2d
  end interface
  interface l2g
     module procedure l2g_1d
     module procedure l2g_2d
  end interface
contains
  subroutine p2c_1d (lbp, ubp, lbc, ubc, parr, carr, p2c_scale_type)
    use clm_varpar, only : max_pft_per_col
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    real(r8), intent(in) :: parr(lbp:ubp)
    real(r8), intent(out) :: carr(lbc:ubc)
    character(len=*), intent(in) :: p2c_scale_type
    integer :: pi,p,c,index
    real(r8) :: scale_p2c(lbp:ubp)
    logical :: found
    real(r8) :: sumwt(lbc:ubc)
    real(r8), pointer :: wtcol(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    wtcol => clm3%g%l%c%p%wtcol
    pcolumn => clm3%g%l%c%p%column
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2c_1d error: scale type ',p2c_scale_type,' not supported'
       call endrun()
    end if
    carr(lbc:ubc) = spval
    sumwt(lbc:ubc) = 0._r8
    do p = lbp,ubp
       if (wtcol(p) /= 0._r8) then
          if (parr(p) /= spval) then
             c = pcolumn(p)
             if (sumwt(c) == 0._r8) carr(c) = 0._r8
             carr(c) = carr(c) + parr(p) * scale_p2c(p) * wtcol(p)
             sumwt(c) = sumwt(c) + wtcol(p)
          end if
       end if
    end do
    found = .false.
    do c = lbc,ubc
       if (sumwt(c) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = c
       else if (sumwt(c) /= 0._r8) then
          carr(c) = carr(c)/sumwt(c)
       end if
    end do
    if (found) then
       write(6,*)'p2c error: sumwt is greater than 1.0 at c= ',index
       call endrun()
    end if
  end subroutine p2c_1d
  subroutine p2c_2d (lbp, ubp, lbc, ubc, num2d, parr, carr, p2c_scale_type)
    use clm_varpar, only : max_pft_per_col
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num2d
    real(r8), intent(in) :: parr(lbp:ubp,num2d)
    real(r8), intent(out) :: carr(lbc:ubc,num2d)
    character(len=*), intent(in) :: p2c_scale_type
    integer :: j,pi,p,c,index
    real(r8) :: scale_p2c(lbp:ubp)
    logical :: found
    real(r8) :: sumwt(lbc:ubc)
    real(r8), pointer :: wtcol(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    wtcol => clm3%g%l%c%p%wtcol
    pcolumn => clm3%g%l%c%p%column
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2c_2d error: scale type ',p2c_scale_type,' not supported'
       call endrun()
    end if
    carr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do p = lbp,ubp
          if (wtcol(p) /= 0._r8) then
             if (parr(p,j) /= spval) then
                c = pcolumn(p)
                if (sumwt(c) == 0._r8) carr(c,j) = 0._r8
                carr(c,j) = carr(c,j) + parr(p,j) * scale_p2c(p) * wtcol(p)
                sumwt(c) = sumwt(c) + wtcol(p)
             end if
          end if
       end do
       found = .false.
       do c = lbc,ubc
          if (sumwt(c) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index = c
          else if (sumwt(c) /= 0._r8) then
             carr(c,j) = carr(c,j)/sumwt(c)
          end if
       end do
       if (found) then
          write(6,*)'p2c_2d error: sumwt is greater than 1.0 at c= ',index,' lev= ',j
          call endrun()
       end if
    end do
  end subroutine p2c_2d
  subroutine p2c_1d_filter (numfc, filterc, pftarr, colarr)
    use clm_varpar, only : max_pft_per_col
    implicit none
    integer , intent(in) :: numfc
    integer , intent(in) :: filterc(numfc)
    real(r8), pointer :: pftarr(:)
    real(r8), pointer :: colarr(:)
    integer :: fc,c,pi,p
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: pftf(:)
    real(r8), pointer :: wtcol(:)
    real(r8), pointer :: wtgcell(:)
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    pftf => clm3%g%l%c%pftf
    wtcol => clm3%g%l%c%p%wtcol
    wtgcell => clm3%g%l%c%p%wtgcell
    do fc = 1,numfc
       c = filterc(fc)
       colarr(c) = 0._r8
       do p = pfti(c), pftf(c)
          if (wtgcell(p) > 0._r8) colarr(c) = colarr(c) + pftarr(p) * wtcol(p)
       end do
    end do
  end subroutine p2c_1d_filter
  subroutine p2c_2d_filter (lev, numfc, filterc, pftarr, colarr)
    use clm_varpar, only : max_pft_per_col
    implicit none
    integer , intent(in) :: lev
    integer , intent(in) :: numfc
    integer , intent(in) :: filterc(numfc)
    real(r8), pointer :: pftarr(:,:)
    real(r8), pointer :: colarr(:,:)
    integer :: fc,c,pi,p,j
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: pftf(:)
    real(r8), pointer :: wtcol(:)
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    pftf => clm3%g%l%c%pftf
    wtcol => clm3%g%l%c%p%wtcol
    do j = 1,lev
       do fc = 1,numfc
          c = filterc(fc)
          colarr(c,j) = 0._r8
          do p = pfti(c), pftf(c)
             colarr(c,j) = colarr(c,j) + pftarr(p,j) * wtcol(p)
          end do
       end do
    end do
  end subroutine p2c_2d_filter
  subroutine p2l_1d (lbp, ubp, lbc, ubc, lbl, ubl, parr, larr, &
       p2c_scale_type, c2l_scale_type)
    use clm_varpar, only : max_pft_per_lu
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    real(r8), intent(in) :: parr(lbp:ubp)
    real(r8), intent(out) :: larr(lbl:ubl)
    character(len=*), intent(in) :: p2c_scale_type
    character(len=*), intent(in) :: c2l_scale_type
    integer :: pi,p,c,l,index
    logical :: found
    real(r8) :: sumwt(lbl:ubl)
    real(r8) :: scale_p2c(lbc:ubc)
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8), pointer :: wtlunit(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    canyon_hwr => clm3%g%l%canyon_hwr
    ltype => clm3%g%l%itype
    ctype => clm3%g%l%c%itype
    clandunit => clm3%g%l%c%landunit
    wtlunit => clm3%g%l%c%p%wtlunit
    pcolumn => clm3%g%l%c%p%column
    plandunit => clm3%g%l%c%p%landunit
    npfts => clm3%g%l%npfts
    pfti => clm3%g%l%pfti
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'p2l_1d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2l_1d error: scale type ',p2c_scale_type,' not supported'
       call endrun()
    end if
    larr(:) = spval
    sumwt(:) = 0._r8
    do p = lbp,ubp
       if (wtlunit(p) /= 0._r8) then
          c = pcolumn(p)
          if (parr(p) /= spval .and. scale_c2l(c) /= spval) then
             l = plandunit(p)
             if (sumwt(l) == 0._r8) larr(l) = 0._r8
             larr(l) = larr(l) + parr(p) * scale_p2c(p) * scale_c2l(c) * wtlunit(p)
             sumwt(l) = sumwt(l) + wtlunit(p)
          end if
       end if
    end do
    found = .false.
    do l = lbl,ubl
       if (sumwt(l) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = l
       else if (sumwt(l) /= 0._r8) then
          larr(l) = larr(l)/sumwt(l)
       end if
    end do
    if (found) then
       write(6,*)'p2l_1d error: sumwt is greater than 1.0 at l= ',index
       call endrun()
    end if
  end subroutine p2l_1d
  subroutine p2l_2d(lbp, ubp, lbc, ubc, lbl, ubl, num2d, parr, larr, &
       p2c_scale_type, c2l_scale_type)
    use clm_varpar, only : max_pft_per_lu
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: num2d
    real(r8), intent(in) :: parr(lbp:ubp,num2d)
    real(r8), intent(out) :: larr(lbl:ubl,num2d)
    character(len=*), intent(in) :: p2c_scale_type
    character(len=*), intent(in) :: c2l_scale_type
    integer :: j,pi,p,c,l,index
    logical :: found
    real(r8) :: sumwt(lbl:ubl)
    real(r8) :: scale_p2c(lbc:ubc)
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8), pointer :: wtlunit(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    canyon_hwr => clm3%g%l%canyon_hwr
    ltype => clm3%g%l%itype
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    wtlunit => clm3%g%l%c%p%wtlunit
    pcolumn => clm3%g%l%c%p%column
    plandunit => clm3%g%l%c%p%landunit
    npfts => clm3%g%l%npfts
    pfti => clm3%g%l%pfti
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'p2l_2d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2l_2d error: scale type ',p2c_scale_type,' not supported'
       call endrun()
    end if
    larr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do p = lbp,ubp
          if (wtlunit(p) /= 0._r8) then
             c = pcolumn(p)
             if (parr(p,j) /= spval .and. scale_c2l(c) /= spval) then
                l = plandunit(p)
                if (sumwt(l) == 0._r8) larr(l,j) = 0._r8
                larr(l,j) = larr(l,j) + parr(p,j) * scale_p2c(p) * scale_c2l(c) * wtlunit(p)
                sumwt(l) = sumwt(l) + wtlunit(p)
             end if
          end if
       end do
       found = .false.
       do l = lbl,ubl
          if (sumwt(l) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index = l
          else if (sumwt(l) /= 0._r8) then
             larr(l,j) = larr(l,j)/sumwt(l)
          end if
       end do
       if (found) then
          write(6,*)'p2l_2d error: sumwt is greater than 1.0 at l= ',index,' j= ',j
          call endrun()
       end if
    end do
  end subroutine p2l_2d
  subroutine p2g_1d(lbp, ubp, lbc, ubc, lbl, ubl, lbg, ubg, parr, garr, &
       p2c_scale_type, c2l_scale_type, l2g_scale_type)
    use clm_varpar, only : max_pft_per_gcell
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    real(r8), intent(in) :: parr(lbp:ubp)
    real(r8), intent(out) :: garr(lbg:ubg)
    character(len=*), intent(in) :: p2c_scale_type
    character(len=*), intent(in) :: c2l_scale_type
    character(len=*), intent(in) :: l2g_scale_type
    integer :: pi,p,c,l,g,index
    logical :: found
    real(r8) :: scale_p2c(lbp:ubp)
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: ctype(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    canyon_hwr => clm3%g%l%canyon_hwr
    ltype => clm3%g%l%itype
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    wtgcell => clm3%g%l%c%p%wtgcell
    pcolumn => clm3%g%l%c%p%column
    pgridcell => clm3%g%l%c%p%gridcell
    plandunit => clm3%g%l%c%p%landunit
    npfts => clm3%g%npfts
    pfti => clm3%g%pfti
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'p2g_1d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'p2g_1d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2g_1d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    garr(:) = spval
    sumwt(:) = 0._r8
    do p = lbp,ubp
       if (wtgcell(p) /= 0._r8) then
          c = pcolumn(p)
          if (parr(p) /= spval .and. scale_c2l(c) /= spval) then
             l = plandunit(p)
             g = pgridcell(p)
             if (sumwt(g) == 0._r8) garr(g) = 0._r8
             garr(g) = garr(g) + parr(p) * scale_p2c(p) * scale_c2l(c) * scale_l2g(l) * wtgcell(p)
             sumwt(g) = sumwt(g) + wtgcell(p)
          end if
       end if
    end do
    found = .false.
    do g = lbg, ubg
       if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = g
       else if (sumwt(g) /= 0._r8) then
          garr(g) = garr(g)/sumwt(g)
       end if
    end do
    if (found) then
       write(6,*)'p2g_1d error: sumwt is greater than 1.0 at g= ',index
       call endrun()
    end if
  end subroutine p2g_1d
  subroutine p2g_2d(lbp, ubp, lbc, ubc, lbl, ubl, lbg, ubg, num2d, &
       parr, garr, p2c_scale_type, c2l_scale_type, l2g_scale_type)
    use clm_varpar, only : max_pft_per_gcell
    implicit none
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    integer , intent(in) :: num2d
    real(r8), intent(in) :: parr(lbp:ubp,num2d)
    real(r8), intent(out) :: garr(lbg:ubg,num2d)
    character(len=*), intent(in) :: p2c_scale_type
    character(len=*), intent(in) :: c2l_scale_type
    character(len=*), intent(in) :: l2g_scale_type
    integer :: j,pi,p,c,l,g,index
    logical :: found
    real(r8) :: scale_p2c(lbp:ubp)
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    canyon_hwr => clm3%g%l%canyon_hwr
    ltype => clm3%g%l%itype
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    wtgcell => clm3%g%l%c%p%wtgcell
    pcolumn => clm3%g%l%c%p%column
    pgridcell => clm3%g%l%c%p%gridcell
    plandunit => clm3%g%l%c%p%landunit
    npfts => clm3%g%npfts
    pfti => clm3%g%pfti
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'p2g_2d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'p2g_2d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    if (p2c_scale_type == 'unity') then
       do p = lbp,ubp
          scale_p2c(p) = 1.0_r8
       end do
    else
       write(6,*)'p2g_2d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    garr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do p = lbp,ubp
          if (wtgcell(p) /= 0._r8) then
             c = pcolumn(p)
             if (parr(p,j) /= spval .and. scale_c2l(c) /= spval) then
                l = plandunit(p)
                g = pgridcell(p)
                if (sumwt(g) == 0._r8) garr(g,j) = 0._r8
                garr(g,j) = garr(g,j) + parr(p,j) * scale_p2c(p) * scale_c2l(c) * scale_l2g(l) * wtgcell(p)
                sumwt(g) = sumwt(g) + wtgcell(p)
             end if
          end if
       end do
       found = .false.
       do g = lbg, ubg
          if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index = g
          else if (sumwt(g) /= 0._r8) then
             garr(g,j) = garr(g,j)/sumwt(g)
          end if
       end do
       if (found) then
          write(6,*)'p2g_2d error: sumwt gt 1.0 at g/sumwt = ',index,sumwt(index)
          call endrun()
       end if
    end do
  end subroutine p2g_2d
  subroutine c2l_1d (lbc, ubc, lbl, ubl, carr, larr, c2l_scale_type)
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    real(r8), intent(in) :: carr(lbc:ubc)
    real(r8), intent(out) :: larr(lbl:ubl)
    character(len=*), intent(in) :: c2l_scale_type
    integer :: ci,c,l,index
    integer :: max_col_per_lu
    logical :: found
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: sumwt(lbl:ubl)
    real(r8), pointer :: wtlunit(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ncolumns(:)
    integer , pointer :: coli(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    ctype => clm3%g%l%c%itype
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtlunit => clm3%g%l%c%wtlunit
    clandunit => clm3%g%l%c%landunit
    ncolumns => clm3%g%l%ncolumns
    coli => clm3%g%l%coli
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'c2l_1d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    larr(:) = spval
    sumwt(:) = 0._r8
    do c = lbc,ubc
       if (wtlunit(c) /= 0._r8) then
          if (carr(c) /= spval .and. scale_c2l(c) /= spval) then
             l = clandunit(c)
             if (sumwt(l) == 0._r8) larr(l) = 0._r8
             larr(l) = larr(l) + carr(c) * scale_c2l(c) * wtlunit(c)
             sumwt(l) = sumwt(l) + wtlunit(c)
          end if
       end if
    end do
    found = .false.
    do l = lbl,ubl
       if (sumwt(l) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = l
       else if (sumwt(l) /= 0._r8) then
          larr(l) = larr(l)/sumwt(l)
       end if
    end do
    if (found) then
       write(6,*)'c2l_1d error: sumwt is greater than 1.0 at l= ',index
       call endrun()
    end if
  end subroutine c2l_1d
  subroutine c2l_2d (lbc, ubc, lbl, ubl, num2d, carr, larr, c2l_scale_type)
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: num2d
    real(r8), intent(in) :: carr(lbc:ubc,num2d)
    real(r8), intent(out) :: larr(lbl:ubl,num2d)
    character(len=*), intent(in) :: c2l_scale_type
    integer :: j,l,ci,c,index
    integer :: max_col_per_lu
    logical :: found
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: sumwt(lbl:ubl)
    real(r8), pointer :: wtlunit(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ncolumns(:)
    integer , pointer :: coli(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    ctype => clm3%g%l%c%itype
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtlunit => clm3%g%l%c%wtlunit
    clandunit => clm3%g%l%c%landunit
    ncolumns => clm3%g%l%ncolumns
    coli => clm3%g%l%coli
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'c2l_2d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    larr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do c = lbc,ubc
          if (wtlunit(c) /= 0._r8) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                l = clandunit(c)
                if (sumwt(l) == 0._r8) larr(l,j) = 0._r8
                larr(l,j) = larr(l,j) + carr(c,j) * scale_c2l(c) * wtlunit(c)
                sumwt(l) = sumwt(l) + wtlunit(c)
             end if
          end if
       end do
       found = .false.
       do l = lbl,ubl
          if (sumwt(l) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index = l
          else if (sumwt(l) /= 0._r8) then
             larr(l,j) = larr(l,j)/sumwt(l)
          end if
       end do
       if (found) then
          write(6,*)'c2l_2d error: sumwt is greater than 1.0 at l= ',index,' lev= ',j
          call endrun()
       end if
    end do
  end subroutine c2l_2d
  subroutine c2g_1d(lbc, ubc, lbl, ubl, lbg, ubg, carr, garr, &
       c2l_scale_type, l2g_scale_type)
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    real(r8), intent(in) :: carr(lbc:ubc)
    real(r8), intent(out) :: garr(lbg:ubg)
    character(len=*), intent(in) :: c2l_scale_type
    character(len=*), intent(in) :: l2g_scale_type
    integer :: ci,c,l,g,index
    integer :: max_col_per_gcell
    logical :: found
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: cgridcell(:)
    integer , pointer :: ncolumns(:)
    integer , pointer :: coli(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    ctype => clm3%g%l%c%itype
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtgcell => clm3%g%l%c%wtgcell
    clandunit => clm3%g%l%c%landunit
    cgridcell => clm3%g%l%c%gridcell
    ncolumns => clm3%g%ncolumns
    coli => clm3%g%coli
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'c2l_1d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'c2l_1d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    garr(:) = spval
    sumwt(:) = 0._r8
    do c = lbc,ubc
       if ( wtgcell(c) /= 0._r8) then
          if (carr(c) /= spval .and. scale_c2l(c) /= spval) then
             l = clandunit(c)
             g = cgridcell(c)
             if (sumwt(g) == 0._r8) garr(g) = 0._r8
             garr(g) = garr(g) + carr(c) * scale_c2l(c) * scale_l2g(l) * wtgcell(c)
             sumwt(g) = sumwt(g) + wtgcell(c)
          end if
       end if
    end do
    found = .false.
    do g = lbg, ubg
       if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = g
       else if (sumwt(g) /= 0._r8) then
          garr(g) = garr(g)/sumwt(g)
       end if
    end do
    if (found) then
       write(6,*)'c2g_1d error: sumwt is greater than 1.0 at g= ',index
       call endrun()
    end if
  end subroutine c2g_1d
  subroutine c2g_2d(lbc, ubc, lbl, ubl, lbg, ubg, num2d, carr, garr, &
       c2l_scale_type, l2g_scale_type)
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    integer , intent(in) :: num2d
    real(r8), intent(in) :: carr(lbc:ubc,num2d)
    real(r8), intent(out) :: garr(lbg:ubg,num2d)
    character(len=*), intent(in) :: c2l_scale_type
    character(len=*), intent(in) :: l2g_scale_type
    integer :: j,ci,c,g,l,index
    integer :: max_col_per_gcell
    logical :: found
    real(r8) :: scale_c2l(lbc:ubc)
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: cgridcell(:)
    integer , pointer :: ncolumns(:)
    integer , pointer :: coli(:)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    real(r8), pointer :: canyon_hwr(:)
    ctype => clm3%g%l%c%itype
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtgcell => clm3%g%l%c%wtgcell
    clandunit => clm3%g%l%c%landunit
    cgridcell => clm3%g%l%c%gridcell
    ncolumns => clm3%g%ncolumns
    coli => clm3%g%coli
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'c2g_2d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0_r8
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0_r8
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0_r8
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c)
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0_r8
          end if
       end do
    else
       write(6,*)'c2g_2d error: scale type ',c2l_scale_type,' not supported'
       call endrun()
    end if
    garr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do c = lbc,ubc
          if (wtgcell(c) /= 0._r8) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                l = clandunit(c)
                g = cgridcell(c)
                if (sumwt(g) == 0._r8) garr(g,j) = 0._r8
                garr(g,j) = garr(g,j) + carr(c,j) * scale_c2l(c) * scale_l2g(l) * wtgcell(c)
                sumwt(g) = sumwt(g) + wtgcell(c)
             end if
          end if
       end do
       found = .false.
       do g = lbg, ubg
          if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index = g
          else if (sumwt(g) /= 0._r8) then
             garr(g,j) = garr(g,j)/sumwt(g)
          end if
       end do
       if (found) then
          write(6,*)'c2g_2d error: sumwt is greater than 1.0 at g= ',index
          call endrun()
       end if
    end do
  end subroutine c2g_2d
  subroutine l2g_1d(lbl, ubl, lbg, ubg, larr, garr, l2g_scale_type)
    implicit none
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    real(r8), intent(in) :: larr(lbl:ubl)
    real(r8), intent(out) :: garr(lbg:ubg)
    character(len=*), intent(in) :: l2g_scale_type
    integer :: li,l,g,index
    integer :: max_lu_per_gcell
    logical :: found
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: lgridcell(:)
    integer , pointer :: nlandunits(:)
    integer , pointer :: luni(:)
    wtgcell => clm3%g%l%wtgcell
    lgridcell => clm3%g%l%gridcell
    nlandunits => clm3%g%nlandunits
    luni => clm3%g%luni
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'l2g_1d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    garr(:) = spval
    sumwt(:) = 0._r8
    do l = lbl,ubl
       if (wtgcell(l) /= 0._r8) then
          if (larr(l) /= spval) then
             g = lgridcell(l)
             if (sumwt(g) == 0._r8) garr(g) = 0._r8
             garr(g) = garr(g) + larr(l) * scale_l2g(l) * wtgcell(l)
             sumwt(g) = sumwt(g) + wtgcell(l)
          end if
       end if
    end do
    found = .false.
    do g = lbg, ubg
       if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
          found = .true.
          index = g
       else if (sumwt(g) /= 0._r8) then
          garr(g) = garr(g)/sumwt(g)
       end if
    end do
    if (found) then
       write(6,*)'l2g_1d error: sumwt is greater than 1.0 at g= ',index
       call endrun()
    end if
  end subroutine l2g_1d
  subroutine l2g_2d(lbl, ubl, lbg, ubg, num2d, larr, garr, l2g_scale_type)
    implicit none
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: lbg, ubg
    integer , intent(in) :: num2d
    real(r8), intent(in) :: larr(lbl:ubl,num2d)
    real(r8), intent(out) :: garr(lbg:ubg,num2d)
    character(len=*), intent(in) :: l2g_scale_type
    integer :: j,g,li,l,index
    integer :: max_lu_per_gcell
    logical :: found
    real(r8) :: scale_l2g(lbl:ubl)
    real(r8) :: sumwt(lbg:ubg)
    real(r8), pointer :: wtgcell(:)
    integer , pointer :: lgridcell(:)
    integer , pointer :: nlandunits(:)
    integer , pointer :: luni(:)
    wtgcell => clm3%g%l%wtgcell
    lgridcell => clm3%g%l%gridcell
    nlandunits => clm3%g%nlandunits
    luni => clm3%g%luni
    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0_r8
       end do
    else
       write(6,*)'l2g_2d error: scale type ',l2g_scale_type,' not supported'
       call endrun()
    end if
    garr(:,:) = spval
    do j = 1,num2d
       sumwt(:) = 0._r8
       do l = lbl,ubl
          if (wtgcell(l) /= 0._r8) then
             if (larr(l,j) /= spval) then
                g = lgridcell(l)
                if (sumwt(g) == 0._r8) garr(g,j) = 0._r8
                garr(g,j) = garr(g,j) + larr(l,j) * scale_l2g(l) * wtgcell(l)
                sumwt(g) = sumwt(g) + wtgcell(l)
             end if
          end if
       end do
       found = .false.
       do g = lbg,ubg
          if (sumwt(g) > 1.0_r8 + 1.e-6_r8) then
             found = .true.
             index= g
          else if (sumwt(g) /= 0._r8) then
             garr(g,j) = garr(g,j)/sumwt(g)
          end if
       end do
       if (found) then
          write(6,*)'l2g_2d error: sumwt is greater than 1.0 at g= ',index,' lev= ',j
          call endrun()
       end if
    end do
  end subroutine l2g_2d
end module subgridAveMod
module pft2colMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use subgridAveMod
  use clmtype
  implicit none
  save
  public :: p2c
contains
  subroutine pft2col (lbc, ubc, num_nolakec, filter_nolakec)
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer :: c,fc
    integer :: num_allc
    integer :: filter_allc(ubc-lbc+1)
    real(r8), pointer :: ptrp(:)
    real(r8), pointer :: ptrc(:)
    num_allc = ubc-lbc+1
    fc = 0
    do c = lbc,ubc
       fc = fc + 1
       filter_allc(fc) = c
    end do
    ptrp => clm3%g%l%c%p%pws%h2ocan
    ptrc => clm3%g%l%c%cws%pws_a%h2ocan
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_evap_tot
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_evap_tot
    call p2c (num_allc, filter_allc, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_rain_grnd
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_rain_grnd
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_snow_grnd
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_snow_grnd
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_snwcp_liq
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_liq
    call p2c (num_allc, filter_allc, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_snwcp_ice
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_ice
    call p2c (num_allc, filter_allc, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_tran_veg
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_tran_veg
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_evap_grnd
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_evap_grnd
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_dew_grnd
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_dew_grnd
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_sub_snow
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_sub_snow
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
    ptrp => clm3%g%l%c%p%pwf%qflx_dew_snow
    ptrc => clm3%g%l%c%cwf%pwf_a%qflx_dew_snow
    call p2c (num_nolakec, filter_nolakec, ptrp, ptrc)
  end subroutine pft2col
end module pft2colMod
MODULE shr_orb_mod
   use shr_kind_mod
   use shr_const_mod
   use module_cam_support, only: endrun
   IMPLICIT none
   public :: shr_orb_cosz
   public :: shr_orb_params
   public :: shr_orb_decl
   real (SHR_KIND_R8),public,parameter :: SHR_ORB_UNDEF_REAL = 1.e36_SHR_KIND_R8
   integer,public,parameter :: SHR_ORB_UNDEF_INT = 2000000000
   private
   real (SHR_KIND_R8),parameter :: pi = SHR_CONST_PI
   real (SHR_KIND_R8),parameter :: SHR_ORB_ECCEN_MIN = 0.0_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: SHR_ORB_ECCEN_MAX = 0.1_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: SHR_ORB_OBLIQ_MIN = -90.0_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: SHR_ORB_OBLIQ_MAX = +90.0_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: SHR_ORB_MVELP_MIN = 0.0_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: SHR_ORB_MVELP_MAX = 360.0_SHR_KIND_R8
CONTAINS
real(SHR_KIND_R8) FUNCTION shr_orb_cosz(jday,lat,lon,declin)
   real (SHR_KIND_R8),intent(in) :: jday
   real (SHR_KIND_R8),intent(in) :: lat
   real (SHR_KIND_R8),intent(in) :: lon
   real (SHR_KIND_R8),intent(in) :: declin
   shr_orb_cosz = sin(lat)*sin(declin) - &
   & cos(lat)*cos(declin)*cos(jday*2.0_SHR_KIND_R8*pi + lon)
END FUNCTION shr_orb_cosz
SUBROUTINE shr_orb_params( iyear_AD , eccen , obliq , mvelp , &
           & obliqr , lambm0 , mvelpp)
   real (SHR_KIND_R8),intent(inout) :: eccen
   real (SHR_KIND_R8),intent(inout) :: obliq
   real (SHR_KIND_R8),intent(inout) :: mvelp
   integer,intent(in) :: iyear_AD
   real (SHR_KIND_R8),intent(out) :: obliqr
   real (SHR_KIND_R8),intent(out) :: lambm0
   real (SHR_KIND_R8),intent(out) :: mvelpp
   integer,parameter :: poblen =47
   integer,parameter :: pecclen=19
   integer,parameter :: pmvelen=78
   real (SHR_KIND_R8),parameter :: psecdeg = 1.0_SHR_KIND_R8/3600.0_SHR_KIND_R8
   real (SHR_KIND_R8) :: degrad = pi/180._SHR_KIND_R8
   real (SHR_KIND_R8) :: yb4_1950AD
   real (SHR_KIND_R8), parameter :: obamp(poblen) = &
   & (/ -2462.2214466_SHR_KIND_R8, -857.3232075_SHR_KIND_R8, -629.3231835_SHR_KIND_R8, &
   & -414.2804924_SHR_KIND_R8, -311.7632587_SHR_KIND_R8, 308.9408604_SHR_KIND_R8, &
   & -162.5533601_SHR_KIND_R8, -116.1077911_SHR_KIND_R8, 101.1189923_SHR_KIND_R8, &
   & -67.6856209_SHR_KIND_R8, 24.9079067_SHR_KIND_R8, 22.5811241_SHR_KIND_R8, &
   & -21.1648355_SHR_KIND_R8, -15.6549876_SHR_KIND_R8, 15.3936813_SHR_KIND_R8, &
   & 14.6660938_SHR_KIND_R8, -11.7273029_SHR_KIND_R8, 10.2742696_SHR_KIND_R8, &
   & 6.4914588_SHR_KIND_R8, 5.8539148_SHR_KIND_R8, -5.4872205_SHR_KIND_R8, &
   & -5.4290191_SHR_KIND_R8, 5.1609570_SHR_KIND_R8, 5.0786314_SHR_KIND_R8, &
   & -4.0735782_SHR_KIND_R8, 3.7227167_SHR_KIND_R8, 3.3971932_SHR_KIND_R8, &
   & -2.8347004_SHR_KIND_R8, -2.6550721_SHR_KIND_R8, -2.5717867_SHR_KIND_R8, &
   & -2.4712188_SHR_KIND_R8, 2.4625410_SHR_KIND_R8, 2.2464112_SHR_KIND_R8, &
   & -2.0755511_SHR_KIND_R8, -1.9713669_SHR_KIND_R8, -1.8813061_SHR_KIND_R8, &
   & -1.8468785_SHR_KIND_R8, 1.8186742_SHR_KIND_R8, 1.7601888_SHR_KIND_R8, &
   & -1.5428851_SHR_KIND_R8, 1.4738838_SHR_KIND_R8, -1.4593669_SHR_KIND_R8, &
   & 1.4192259_SHR_KIND_R8, -1.1818980_SHR_KIND_R8, 1.1756474_SHR_KIND_R8, &
   & -1.1316126_SHR_KIND_R8, 1.0896928_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: obrate(poblen) = &
   & (/ 31.609974_SHR_KIND_R8, 32.620504_SHR_KIND_R8, 24.172203_SHR_KIND_R8, &
   & 31.983787_SHR_KIND_R8, 44.828336_SHR_KIND_R8, 30.973257_SHR_KIND_R8, &
   & 43.668246_SHR_KIND_R8, 32.246691_SHR_KIND_R8, 30.599444_SHR_KIND_R8, &
   & 42.681324_SHR_KIND_R8, 43.836462_SHR_KIND_R8, 47.439436_SHR_KIND_R8, &
   & 63.219948_SHR_KIND_R8, 64.230478_SHR_KIND_R8, 1.010530_SHR_KIND_R8, &
   & 7.437771_SHR_KIND_R8, 55.782177_SHR_KIND_R8, 0.373813_SHR_KIND_R8, &
   & 13.218362_SHR_KIND_R8, 62.583231_SHR_KIND_R8, 63.593761_SHR_KIND_R8, &
   & 76.438310_SHR_KIND_R8, 45.815258_SHR_KIND_R8, 8.448301_SHR_KIND_R8, &
   & 56.792707_SHR_KIND_R8, 49.747842_SHR_KIND_R8, 12.058272_SHR_KIND_R8, &
   & 75.278220_SHR_KIND_R8, 65.241008_SHR_KIND_R8, 64.604291_SHR_KIND_R8, &
   & 1.647247_SHR_KIND_R8, 7.811584_SHR_KIND_R8, 12.207832_SHR_KIND_R8, &
   & 63.856665_SHR_KIND_R8, 56.155990_SHR_KIND_R8, 77.448840_SHR_KIND_R8, &
   & 6.801054_SHR_KIND_R8, 62.209418_SHR_KIND_R8, 20.656133_SHR_KIND_R8, &
   & 48.344406_SHR_KIND_R8, 55.145460_SHR_KIND_R8, 69.000539_SHR_KIND_R8, &
   & 11.071350_SHR_KIND_R8, 74.291298_SHR_KIND_R8, 11.047742_SHR_KIND_R8, &
   & 0.636717_SHR_KIND_R8, 12.844549_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: obphas(poblen) = &
   & (/ 251.9025_SHR_KIND_R8, 280.8325_SHR_KIND_R8, 128.3057_SHR_KIND_R8, &
   & 292.7252_SHR_KIND_R8, 15.3747_SHR_KIND_R8, 263.7951_SHR_KIND_R8, &
   & 308.4258_SHR_KIND_R8, 240.0099_SHR_KIND_R8, 222.9725_SHR_KIND_R8, &
   & 268.7809_SHR_KIND_R8, 316.7998_SHR_KIND_R8, 319.6024_SHR_KIND_R8, &
   & 143.8050_SHR_KIND_R8, 172.7351_SHR_KIND_R8, 28.9300_SHR_KIND_R8, &
   & 123.5968_SHR_KIND_R8, 20.2082_SHR_KIND_R8, 40.8226_SHR_KIND_R8, &
   & 123.4722_SHR_KIND_R8, 155.6977_SHR_KIND_R8, 184.6277_SHR_KIND_R8, &
   & 267.2772_SHR_KIND_R8, 55.0196_SHR_KIND_R8, 152.5268_SHR_KIND_R8, &
   & 49.1382_SHR_KIND_R8, 204.6609_SHR_KIND_R8, 56.5233_SHR_KIND_R8, &
   & 200.3284_SHR_KIND_R8, 201.6651_SHR_KIND_R8, 213.5577_SHR_KIND_R8, &
   & 17.0374_SHR_KIND_R8, 164.4194_SHR_KIND_R8, 94.5422_SHR_KIND_R8, &
   & 131.9124_SHR_KIND_R8, 61.0309_SHR_KIND_R8, 296.2073_SHR_KIND_R8, &
   & 135.4894_SHR_KIND_R8, 114.8750_SHR_KIND_R8, 247.0691_SHR_KIND_R8, &
   & 256.6114_SHR_KIND_R8, 32.1008_SHR_KIND_R8, 143.6804_SHR_KIND_R8, &
   & 16.8784_SHR_KIND_R8, 160.6835_SHR_KIND_R8, 27.5932_SHR_KIND_R8, &
   & 348.1074_SHR_KIND_R8, 82.6496_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: ecamp (pecclen) = &
   & (/ 0.01860798_SHR_KIND_R8, 0.01627522_SHR_KIND_R8, -0.01300660_SHR_KIND_R8, &
   & 0.00988829_SHR_KIND_R8, -0.00336700_SHR_KIND_R8, 0.00333077_SHR_KIND_R8, &
   & -0.00235400_SHR_KIND_R8, 0.00140015_SHR_KIND_R8, 0.00100700_SHR_KIND_R8, &
   & 0.00085700_SHR_KIND_R8, 0.00064990_SHR_KIND_R8, 0.00059900_SHR_KIND_R8, &
   & 0.00037800_SHR_KIND_R8, -0.00033700_SHR_KIND_R8, 0.00027600_SHR_KIND_R8, &
   & 0.00018200_SHR_KIND_R8, -0.00017400_SHR_KIND_R8, -0.00012400_SHR_KIND_R8, &
   & 0.00001250_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: ecrate(pecclen) = &
   & (/ 4.2072050_SHR_KIND_R8, 7.3460910_SHR_KIND_R8, 17.8572630_SHR_KIND_R8, &
   & 17.2205460_SHR_KIND_R8, 16.8467330_SHR_KIND_R8, 5.1990790_SHR_KIND_R8, &
   & 18.2310760_SHR_KIND_R8, 26.2167580_SHR_KIND_R8, 6.3591690_SHR_KIND_R8, &
   & 16.2100160_SHR_KIND_R8, 3.0651810_SHR_KIND_R8, 16.5838290_SHR_KIND_R8, &
   & 18.4939800_SHR_KIND_R8, 6.1909530_SHR_KIND_R8, 18.8677930_SHR_KIND_R8, &
   & 17.4255670_SHR_KIND_R8, 6.1860010_SHR_KIND_R8, 18.4174410_SHR_KIND_R8, &
   & 0.6678630_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: ecphas(pecclen) = &
   & (/ 28.620089_SHR_KIND_R8, 193.788772_SHR_KIND_R8, 308.307024_SHR_KIND_R8, &
   & 320.199637_SHR_KIND_R8, 279.376984_SHR_KIND_R8, 87.195000_SHR_KIND_R8, &
   & 349.129677_SHR_KIND_R8, 128.443387_SHR_KIND_R8, 154.143880_SHR_KIND_R8, &
   & 291.269597_SHR_KIND_R8, 114.860583_SHR_KIND_R8, 332.092251_SHR_KIND_R8, &
   & 296.414411_SHR_KIND_R8, 145.769910_SHR_KIND_R8, 337.237063_SHR_KIND_R8, &
   & 152.092288_SHR_KIND_R8, 126.839891_SHR_KIND_R8, 210.667199_SHR_KIND_R8, &
   & 72.108838_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: mvamp (pmvelen) = &
   & (/ 7391.0225890_SHR_KIND_R8, 2555.1526947_SHR_KIND_R8, 2022.7629188_SHR_KIND_R8, &
   & -1973.6517951_SHR_KIND_R8, 1240.2321818_SHR_KIND_R8, 953.8679112_SHR_KIND_R8, &
   & -931.7537108_SHR_KIND_R8, 872.3795383_SHR_KIND_R8, 606.3544732_SHR_KIND_R8, &
   & -496.0274038_SHR_KIND_R8, 456.9608039_SHR_KIND_R8, 346.9462320_SHR_KIND_R8, &
   & -305.8412902_SHR_KIND_R8, 249.6173246_SHR_KIND_R8, -199.1027200_SHR_KIND_R8, &
   & 191.0560889_SHR_KIND_R8, -175.2936572_SHR_KIND_R8, 165.9068833_SHR_KIND_R8, &
   & 161.1285917_SHR_KIND_R8, 139.7878093_SHR_KIND_R8, -133.5228399_SHR_KIND_R8, &
   & 117.0673811_SHR_KIND_R8, 104.6907281_SHR_KIND_R8, 95.3227476_SHR_KIND_R8, &
   & 86.7824524_SHR_KIND_R8, 86.0857729_SHR_KIND_R8, 70.5893698_SHR_KIND_R8, &
   & -69.9719343_SHR_KIND_R8, -62.5817473_SHR_KIND_R8, 61.5450059_SHR_KIND_R8, &
   & -57.9364011_SHR_KIND_R8, 57.1899832_SHR_KIND_R8, -57.0236109_SHR_KIND_R8, &
   & -54.2119253_SHR_KIND_R8, 53.2834147_SHR_KIND_R8, 52.1223575_SHR_KIND_R8, &
   & -49.0059908_SHR_KIND_R8, -48.3118757_SHR_KIND_R8, -45.4191685_SHR_KIND_R8, &
   & -42.2357920_SHR_KIND_R8, -34.7971099_SHR_KIND_R8, 34.4623613_SHR_KIND_R8, &
   & -33.8356643_SHR_KIND_R8, 33.6689362_SHR_KIND_R8, -31.2521586_SHR_KIND_R8, &
   & -30.8798701_SHR_KIND_R8, 28.4640769_SHR_KIND_R8, -27.1960802_SHR_KIND_R8, &
   & 27.0860736_SHR_KIND_R8, -26.3437456_SHR_KIND_R8, 24.7253740_SHR_KIND_R8, &
   & 24.6732126_SHR_KIND_R8, 24.4272733_SHR_KIND_R8, 24.0127327_SHR_KIND_R8, &
   & 21.7150294_SHR_KIND_R8, -21.5375347_SHR_KIND_R8, 18.1148363_SHR_KIND_R8, &
   & -16.9603104_SHR_KIND_R8, -16.1765215_SHR_KIND_R8, 15.5567653_SHR_KIND_R8, &
   & 15.4846529_SHR_KIND_R8, 15.2150632_SHR_KIND_R8, 14.5047426_SHR_KIND_R8, &
   & -14.3873316_SHR_KIND_R8, 13.1351419_SHR_KIND_R8, 12.8776311_SHR_KIND_R8, &
   & 11.9867234_SHR_KIND_R8, 11.9385578_SHR_KIND_R8, 11.7030822_SHR_KIND_R8, &
   & 11.6018181_SHR_KIND_R8, -11.2617293_SHR_KIND_R8, -10.4664199_SHR_KIND_R8, &
   & 10.4333970_SHR_KIND_R8, -10.2377466_SHR_KIND_R8, 10.1934446_SHR_KIND_R8, &
   & -10.1280191_SHR_KIND_R8, 10.0289441_SHR_KIND_R8, -10.0034259_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: mvrate(pmvelen) = &
   & (/ 31.609974_SHR_KIND_R8, 32.620504_SHR_KIND_R8, 24.172203_SHR_KIND_R8, &
   & 0.636717_SHR_KIND_R8, 31.983787_SHR_KIND_R8, 3.138886_SHR_KIND_R8, &
   & 30.973257_SHR_KIND_R8, 44.828336_SHR_KIND_R8, 0.991874_SHR_KIND_R8, &
   & 0.373813_SHR_KIND_R8, 43.668246_SHR_KIND_R8, 32.246691_SHR_KIND_R8, &
   & 30.599444_SHR_KIND_R8, 2.147012_SHR_KIND_R8, 10.511172_SHR_KIND_R8, &
   & 42.681324_SHR_KIND_R8, 13.650058_SHR_KIND_R8, 0.986922_SHR_KIND_R8, &
   & 9.874455_SHR_KIND_R8, 13.013341_SHR_KIND_R8, 0.262904_SHR_KIND_R8, &
   & 0.004952_SHR_KIND_R8, 1.142024_SHR_KIND_R8, 63.219948_SHR_KIND_R8, &
   & 0.205021_SHR_KIND_R8, 2.151964_SHR_KIND_R8, 64.230478_SHR_KIND_R8, &
   & 43.836462_SHR_KIND_R8, 47.439436_SHR_KIND_R8, 1.384343_SHR_KIND_R8, &
   & 7.437771_SHR_KIND_R8, 18.829299_SHR_KIND_R8, 9.500642_SHR_KIND_R8, &
   & 0.431696_SHR_KIND_R8, 1.160090_SHR_KIND_R8, 55.782177_SHR_KIND_R8, &
   & 12.639528_SHR_KIND_R8, 1.155138_SHR_KIND_R8, 0.168216_SHR_KIND_R8, &
   & 1.647247_SHR_KIND_R8, 10.884985_SHR_KIND_R8, 5.610937_SHR_KIND_R8, &
   & 12.658184_SHR_KIND_R8, 1.010530_SHR_KIND_R8, 1.983748_SHR_KIND_R8, &
   & 14.023871_SHR_KIND_R8, 0.560178_SHR_KIND_R8, 1.273434_SHR_KIND_R8, &
   & 12.021467_SHR_KIND_R8, 62.583231_SHR_KIND_R8, 63.593761_SHR_KIND_R8, &
   & 76.438310_SHR_KIND_R8, 4.280910_SHR_KIND_R8, 13.218362_SHR_KIND_R8, &
   & 17.818769_SHR_KIND_R8, 8.359495_SHR_KIND_R8, 56.792707_SHR_KIND_R8, &
   & 8.448301_SHR_KIND_R8, 1.978796_SHR_KIND_R8, 8.863925_SHR_KIND_R8, &
   & 0.186365_SHR_KIND_R8, 8.996212_SHR_KIND_R8, 6.771027_SHR_KIND_R8, &
   & 45.815258_SHR_KIND_R8, 12.002811_SHR_KIND_R8, 75.278220_SHR_KIND_R8, &
   & 65.241008_SHR_KIND_R8, 18.870667_SHR_KIND_R8, 22.009553_SHR_KIND_R8, &
   & 64.604291_SHR_KIND_R8, 11.498094_SHR_KIND_R8, 0.578834_SHR_KIND_R8, &
   & 9.237738_SHR_KIND_R8, 49.747842_SHR_KIND_R8, 2.147012_SHR_KIND_R8, &
   & 1.196895_SHR_KIND_R8, 2.133898_SHR_KIND_R8, 0.173168_SHR_KIND_R8/)
   real (SHR_KIND_R8), parameter :: mvphas(pmvelen) = &
   & (/ 251.9025_SHR_KIND_R8, 280.8325_SHR_KIND_R8, 128.3057_SHR_KIND_R8, &
   & 348.1074_SHR_KIND_R8, 292.7252_SHR_KIND_R8, 165.1686_SHR_KIND_R8, &
   & 263.7951_SHR_KIND_R8, 15.3747_SHR_KIND_R8, 58.5749_SHR_KIND_R8, &
   & 40.8226_SHR_KIND_R8, 308.4258_SHR_KIND_R8, 240.0099_SHR_KIND_R8, &
   & 222.9725_SHR_KIND_R8, 106.5937_SHR_KIND_R8, 114.5182_SHR_KIND_R8, &
   & 268.7809_SHR_KIND_R8, 279.6869_SHR_KIND_R8, 39.6448_SHR_KIND_R8, &
   & 126.4108_SHR_KIND_R8, 291.5795_SHR_KIND_R8, 307.2848_SHR_KIND_R8, &
   & 18.9300_SHR_KIND_R8, 273.7596_SHR_KIND_R8, 143.8050_SHR_KIND_R8, &
   & 191.8927_SHR_KIND_R8, 125.5237_SHR_KIND_R8, 172.7351_SHR_KIND_R8, &
   & 316.7998_SHR_KIND_R8, 319.6024_SHR_KIND_R8, 69.7526_SHR_KIND_R8, &
   & 123.5968_SHR_KIND_R8, 217.6432_SHR_KIND_R8, 85.5882_SHR_KIND_R8, &
   & 156.2147_SHR_KIND_R8, 66.9489_SHR_KIND_R8, 20.2082_SHR_KIND_R8, &
   & 250.7568_SHR_KIND_R8, 48.0188_SHR_KIND_R8, 8.3739_SHR_KIND_R8, &
   & 17.0374_SHR_KIND_R8, 155.3409_SHR_KIND_R8, 94.1709_SHR_KIND_R8, &
   & 221.1120_SHR_KIND_R8, 28.9300_SHR_KIND_R8, 117.1498_SHR_KIND_R8, &
   & 320.5095_SHR_KIND_R8, 262.3602_SHR_KIND_R8, 336.2148_SHR_KIND_R8, &
   & 233.0046_SHR_KIND_R8, 155.6977_SHR_KIND_R8, 184.6277_SHR_KIND_R8, &
   & 267.2772_SHR_KIND_R8, 78.9281_SHR_KIND_R8, 123.4722_SHR_KIND_R8, &
   & 188.7132_SHR_KIND_R8, 180.1364_SHR_KIND_R8, 49.1382_SHR_KIND_R8, &
   & 152.5268_SHR_KIND_R8, 98.2198_SHR_KIND_R8, 97.4808_SHR_KIND_R8, &
   & 221.5376_SHR_KIND_R8, 168.2438_SHR_KIND_R8, 161.1199_SHR_KIND_R8, &
   & 55.0196_SHR_KIND_R8, 262.6495_SHR_KIND_R8, 200.3284_SHR_KIND_R8, &
   & 201.6651_SHR_KIND_R8, 294.6547_SHR_KIND_R8, 99.8233_SHR_KIND_R8, &
   & 213.5577_SHR_KIND_R8, 154.1631_SHR_KIND_R8, 232.7153_SHR_KIND_R8, &
   & 138.3034_SHR_KIND_R8, 204.6609_SHR_KIND_R8, 106.5938_SHR_KIND_R8, &
   & 250.4676_SHR_KIND_R8, 332.3345_SHR_KIND_R8, 27.3039_SHR_KIND_R8/)
   integer :: i
   real (SHR_KIND_R8) :: obsum
   real (SHR_KIND_R8) :: cossum
   real (SHR_KIND_R8) :: sinsum
   real (SHR_KIND_R8) :: fvelp
   real (SHR_KIND_R8) :: mvsum
   real (SHR_KIND_R8) :: beta
   real (SHR_KIND_R8) :: years
   real (SHR_KIND_R8) :: eccen2
   real (SHR_KIND_R8) :: eccen3
      yb4_1950AD = 1950.0_SHR_KIND_R8 - real(iyear_AD,SHR_KIND_R8)
      if ( abs(yb4_1950AD) .gt. 1000000.0_SHR_KIND_R8 )then
          write(6,*) 'Error in shr_orb,  abs(yb4_1950AD) .gt. 1000000.0_SHR_KIND_R8'
          call endrun()
      end if
      years = - yb4_1950AD
      obsum = 0.0_SHR_KIND_R8
      do i = 1, poblen
         obsum = obsum + obamp(i)*psecdeg*cos((obrate(i)*psecdeg*years + &
         & obphas(i))*degrad)
      end do
      obliq = 23.320556_SHR_KIND_R8 + obsum
      cossum = 0.0_SHR_KIND_R8
      do i = 1, pecclen
        cossum = cossum+ecamp(i)*cos((ecrate(i)*psecdeg*years+ecphas(i))*degrad)
      end do
      sinsum = 0.0_SHR_KIND_R8
      do i = 1, pecclen
        sinsum = sinsum+ecamp(i)*sin((ecrate(i)*psecdeg*years+ecphas(i))*degrad)
      end do
      eccen2 = cossum*cossum + sinsum*sinsum
      eccen = sqrt(eccen2)
      eccen3 = eccen2*eccen
      if (abs(cossum) .le. 1.0E-8_SHR_KIND_R8) then
        if (sinsum .eq. 0.0_SHR_KIND_R8) then
          fvelp = 0.0_SHR_KIND_R8
        else if (sinsum .lt. 0.0_SHR_KIND_R8) then
          fvelp = 1.5_SHR_KIND_R8*pi
        else if (sinsum .gt. 0.0_SHR_KIND_R8) then
          fvelp = .5_SHR_KIND_R8*pi
        endif
      else if (cossum .lt. 0.0_SHR_KIND_R8) then
        fvelp = atan(sinsum/cossum) + pi
      else if (cossum .gt. 0.0_SHR_KIND_R8) then
        if (sinsum .lt. 0.0_SHR_KIND_R8) then
          fvelp = atan(sinsum/cossum) + 2.0_SHR_KIND_R8*pi
        else
          fvelp = atan(sinsum/cossum)
        endif
      endif
      mvsum = 0.0_SHR_KIND_R8
      do i = 1, pmvelen
        mvsum = mvsum + mvamp(i)*psecdeg*sin((mvrate(i)*psecdeg*years + &
        & mvphas(i))*degrad)
      end do
      mvelp = fvelp/degrad + 50.439273_SHR_KIND_R8*psecdeg*years + 3.392506_SHR_KIND_R8 + mvsum
      do while (mvelp .lt. 0.0_SHR_KIND_R8)
        mvelp = mvelp + 360.0_SHR_KIND_R8
      end do
      do while (mvelp .ge. 360.0_SHR_KIND_R8)
        mvelp = mvelp - 360.0_SHR_KIND_R8
      end do
   obliqr = obliq*degrad
   mvelpp = (mvelp + 180._SHR_KIND_R8)*degrad
   beta = sqrt(1._SHR_KIND_R8 - eccen2)
   lambm0 = 2._SHR_KIND_R8*((.5_SHR_KIND_R8*eccen + .125_SHR_KIND_R8*eccen3)*(1._SHR_KIND_R8 + beta)*sin(mvelpp) &
   & - .250_SHR_KIND_R8*eccen2*(.5_SHR_KIND_R8 + beta)*sin(2._SHR_KIND_R8*mvelpp) &
   & + .125_SHR_KIND_R8*eccen3*(1._SHR_KIND_R8/3._SHR_KIND_R8 + beta)*sin(3._SHR_KIND_R8*mvelpp))
END SUBROUTINE shr_orb_params
SUBROUTINE shr_orb_decl(calday ,eccen ,mvelpp ,lambm0 ,obliqr ,delta ,eccf)
   real (SHR_KIND_R8),intent(in) :: calday
   real (SHR_KIND_R8),intent(in) :: eccen
   real (SHR_KIND_R8),intent(in) :: obliqr
   real (SHR_KIND_R8),intent(in) :: lambm0
   real (SHR_KIND_R8),intent(in) :: mvelpp
   real (SHR_KIND_R8),intent(out) :: delta
   real (SHR_KIND_R8),intent(out) :: eccf
   real (SHR_KIND_R8),parameter :: dayspy = 365.0_SHR_KIND_R8
   real (SHR_KIND_R8),parameter :: ve = 80.5_SHR_KIND_R8
   real (SHR_KIND_R8) :: lambm
   real (SHR_KIND_R8) :: lmm
   real (SHR_KIND_R8) :: lamb
   real (SHR_KIND_R8) :: invrho
   real (SHR_KIND_R8) :: sinl
   lambm = lambm0 + (calday - ve)*2._SHR_KIND_R8*pi/dayspy
   lmm = lambm - mvelpp
   sinl = sin(lmm)
   lamb = lambm + eccen*(2._SHR_KIND_R8*sinl + eccen*(1.25_SHR_KIND_R8*sin(2._SHR_KIND_R8*lmm) &
   & + eccen*((13.0_SHR_KIND_R8/12.0_SHR_KIND_R8)*sin(3._SHR_KIND_R8*lmm) - 0.25_SHR_KIND_R8*sinl)))
   invrho = (1._SHR_KIND_R8 + eccen*cos(lamb - mvelpp)) / (1._SHR_KIND_R8 - eccen*eccen)
   delta = asin(sin(obliqr)*sin(lamb))
   eccf = invrho*invrho
   return
END SUBROUTINE shr_orb_decl
END MODULE shr_orb_mod
module surfFileMod
  implicit none
  save
  public :: surfrd
contains
  subroutine surfrd(organicxy,efisopxy,gtixy,ilx,jlx,iveg,isl,lndmsk)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clm_varpar
    use pftvarcon, only : noveg, crop
    use clm_varcon,only : sand,clay,soic,plant,cover
    use clm_varsur , only :gti, wtxy,vegxy,soic2d,sand3d,clay3d,organic3d,efisop2d &
                                 ,pctgla,pctlak,pctwet,pcturb
    use decompMod , only: get_proc_bounds
    use module_cam_support, only: endrun
    implicit none
    integer :: ilx,jlx
    real(r8) :: organicxy(maxpatch)
    real(r8) :: efisopxy(6)
    real(r8) :: gtixy
    integer :: iveg,isl,lndmsk
    integer :: g,k,m,k1,k2,begg,endg
    integer :: ncid,dimid,varid
    integer :: ier
    integer ,allocatable :: pft(:,:)
    integer ,allocatable :: cft(:,:)
    real(r8),allocatable :: pctcft_lunit(:,:)
    real(r8),allocatable :: pctpft_lunit(:,:)
    real(r8), allocatable :: pctpft(:,:)
    real(r8) :: pctspec1
    integer :: cropcount
    real(r8) :: sumscl
    real(r8),allocatable :: sumvec(:)
    logical :: found
    integer :: iindx, jindx
    integer :: miss = 99999
    real(r8) :: wst(0:numpft)
    integer :: wsti(maxpatch_pft)
    real(r8) :: wst_sum
    real(r8) :: sumpct
    real(r8) :: diff
    real(r8) :: rmax
    integer :: pftid
  call CLMDebug('surfrd-mark1')
    call get_proc_bounds(begg=begg, endg=endg)
   call CLMDebug('get begg,endg')
       soic2d(:) = -999
       sand3d(:,:) = -999.
       clay3d(:,:) = -999.
       pctlak(:) = 0.0
       pctwet(:) = 0.0
       pcturb(:) = 0.0
       pctgla(:) = 0.0
      pftid = 0
    call CLMDebug('allocate sumvec')
    allocate(sumvec(begg:endg))
    call CLMDebug('allocate cft')
    allocate(cft(begg:endg,numcft))
    call CLMDebug('allocate pft')
    allocate(pft(begg:endg,maxpatch_pft))
    call CLMDebug('allocate pctpft_lunit')
    allocate(pctcft_lunit(begg:endg,numcft))
    call CLMDebug('allocate pctpft_lunit')
    allocate(pctpft_lunit(begg:endg,maxpatch_pft))
    call CLMDebug('allocate pctpft')
    allocate(pctpft(begg:endg,0:numpft))
       pctpft(:,:) = 0.0
      pft(:,:) = 0
      call CLMDebug('surfrd-mark')
       do g=begg,endg
           soic2d(g) = soic(isl)
           efisop2d(:,g) = efisopxy(:)
          gti(g) = gtixy
           do k=1,nlevsoi
             sand3d(g,k) = sand(isl)
             clay3d(g,k) = clay(isl)
             organic3d(g,k) = organicxy(k)
          end do
          call CLMDebug('surfrd-mark2')
           do m=1,maxpatch_pft
               pft(g,m) = plant(iveg,m)
               if(cover(iveg,m).ne.0.0) then
                pctpft(g,pft(g,m)) = cover(iveg,m)
               end if
           end do
         end do
           call CLMDebug('surfrd--mark3')
       sumvec(:) = abs(sum(pctpft,dim=2)-100.)
          do g=begg,endg
             do m = 1, maxpatch_pft
              if (pft(g,m)<0 .or. pft(g,m)>numpft) then
                   write(6,*)'SURFRD error: invalid PFT for g,m=',ilx,jlx,m,pft(g,m)
                   call endrun
                end if
             end do
              if (sumvec(g)>1.e-04 .and. pftid == 0) then
               write(6,*)'SURFRD error: PFT cover ne 100 for g=',ilx,jlx
                do m=1,maxpatch_pft
                   write(6,*)'m= ',m,' pft= ',pft(g,m)
                end do
                write(6,*)'sumvec= ',sumvec(g)
                call endrun
             end if
          end do
          call CLMDebug('surfrd--mark4')
           do g=begg,endg
               cft(g,:) = 0
                pctcft_lunit(g,:) = 0.
                cropcount = 0
                pctspec1 = pcturb(g) + pctgla(g) + pctlak(g) + pctwet(g)
                if (pctspec1 < 100.) then
                   do m = 0, numpft
                      if (crop(m) == 1. .and. pctpft(g,m) > 0.) then
                         cropcount = cropcount + 1
                         if (cropcount > maxpatch_cft) then
                            write(6,*) 'ERROR surfFileMod: cropcount>maxpatch_cft'
                            call endrun()
                         end if
                         cft(g,cropcount) = m
                         pctcft_lunit(g,cropcount) = pctpft(g,m)
                         pctpft(g,m) = 0.0
                      else if (crop(m) == 0.) then
                         pctpft(g,m) = pctpft(g,m)
                       end if
                   end do
                else if (pctspec1 == 100.) then
                   pctpft(g,0) = 100.
                   pctpft(g,1:numpft) = 0.
                end if
             end do
          call CLMDebug('surfrd-mark5')
             do g=begg,endg
                wst_sum = 0.
                sumpct = 0
                do m = 0, numpft
                   wst(m) = pctpft(g,m)
                   wst_sum = wst_sum + pctpft(g,m)
                end do
                if (pftid .eq. 0) call mkrank (numpft, wst, miss, wsti, maxpatch_pft)
                 if (pftid .eq. 0) then
                   do m = 1, maxpatch_pft
                      if(wsti(m) /= miss) then
                          pft(g,m) = wsti(m)
                          pctpft_lunit(g,m) = wst(wsti(m))
                       else
                         pft(g,m) = noveg
                         pctpft_lunit(g,m) = 0.
                       end if
                      sumpct = sumpct + pctpft_lunit(g,m)
                   end do
                else
                   do m = 1, maxpatch_pft
                      pft(g,m) = 0
                      pctpft_lunit(g,m) = 0.
                   end do
                end if
          call CLMDebug('surfrd--mark6')
                if (sumpct < wst_sum) then
                   diff = wst_sum - sumpct
                   sumpct = 0.
                   do m = 1, maxpatch_pft
                      pctpft_lunit(g,m) = pctpft_lunit(g,m) + diff/maxpatch_pft
                      sumpct = sumpct + pctpft_lunit(g,m)
                   end do
                end if
                do m = 1,maxpatch_pft
                   if (pft(g,m) < 0 .or. pft(g,m) > numpft) then
                      write (6,*)'surfrd error: invalid PFT at gridcell g=',ilx,jlx,pft(g,m)
                      call endrun()
                   end if
                end do
                do m=1,maxpatch_pft
                   pctpft_lunit(g,m) = float(nint(pctpft_lunit(g,m)))
                end do
                do m=1,maxpatch_cft
                   pctcft_lunit(g,m) = float(nint(pctcft_lunit(g,m)))
                end do
          call CLMDebug('surfrd--mark7')
                rmax = -9999.
                k1 = -9999
                k2 = -9999
                sumpct = 0.
                do m = 1, maxpatch_pft
                   sumpct = sumpct + pctpft_lunit(g,m)
                   if (pctpft_lunit(g,m) > rmax) then
                      k1 = m
                      rmax = pctpft_lunit(g,m)
                   end if
                end do
                do m = 1, maxpatch_cft
                   sumpct = sumpct + pctcft_lunit(g,m)
                   if (pctcft_lunit(g,m) > rmax) then
                      k2 = m
                      rmax = pctcft_lunit(g,m)
                   end if
                end do
                if (k1 == -9999 .and. k2 == -9999) then
                   write(6,*)'surfrd error: largest PFT patch not found'
                   call endrun()
                 else if(pftid /=1) then
                   if (sumpct < 95 .or. sumpct > 105.) then
                      write(6,*)'surfrd error: sum of PFT cover =',sumpct,' at g=',ilx,jlx
                      call endrun()
                   else if (sumpct /= 100. .and. k2 /= -9999) then
                      pctcft_lunit(g,k2) = pctcft_lunit(g,k2) - (sumpct-100.)
                   else if (sumpct /= 100.) then
                      pctpft_lunit(g,k1) = pctpft_lunit(g,k1) - (sumpct-100.)
                   end if
                end if
                sumpct = 0.
                do m = 1, maxpatch_pft
                   sumpct = sumpct + pctpft_lunit(g,m)
                end do
                do m = 1, maxpatch_cft
                   sumpct = sumpct + pctcft_lunit(g,m)
                end do
              if (pftid == 0) then
                   if (abs(sumpct - 100.) > 0.000001) then
                      write(6,*)'surfFileMod error: sum(pct) over maxpatch_pft is not = 100.'
                      write(6,*)sumpct, g
                      call endrun()
                   end if
                   if (sumpct < -0.000001) then
                      write(6,*)'surfFileMod error: sum(pct) over maxpatch_pft is < 0.'
                      write(6,*)sumpct, g
                      call endrun()
                   end if
                end if
             end do
          call CLMDebug('surfrd--mark8')
       found = .false.
          do g=begg,endg
             sumscl = pctlak(g)+pctwet(g)+pcturb(g)+pctgla(g)
             if (sumscl > 100.+1.e-04) then
                found = .true.
                iindx = ilx
                jindx = jlx
                exit
             end if
          if (found) exit
       end do
       if ( found ) then
          write(6,*)'surfrd error: PFT cover>100 for g=',ilx,jlx
          call endrun()
       end if
       found = .false.
          do g=begg,endg
             if (pcturb(g) /= 0.) then
                found = .true.
                iindx = ilx
                jindx = jlx
                exit
             end if
          if (found) exit
       end do
       if ( found ) then
          write (6,*)'surfrd error: urban parameterization not implemented at g= ',ilx,jlx
          call endrun()
       end if
    vegxy(:,:) = 0
    wtxy(:,:) = 0.
       do g=begg,endg
          if (lndmsk == 1) then
             sumscl = pcturb(g)+pctlak(g)+pctwet(g)+pctgla(g)
             do m = 1, maxpatch_pft
                vegxy(g,m) = pft(g,m)
                wtxy(g,m) = pctpft_lunit(g,m) * (100.-sumscl)/10000.
             end do
             vegxy(g,npatch_urban) = noveg
             wtxy(g,npatch_urban) = pcturb(g)/100.
             vegxy(g,npatch_lake) = noveg
             wtxy(g,npatch_lake) = pctlak(g)/100.
             vegxy(g,npatch_wet) = noveg
             wtxy(g,npatch_wet) = pctwet(g)/100.
             vegxy(g,npatch_glacier) = noveg
             wtxy(g,npatch_glacier) = pctgla(g)/100.
             do m = 1,maxpatch_cft
                   vegxy(g,npatch_glacier+m) = cft(g,m)
                   wtxy(g,npatch_glacier+m)= pctcft_lunit(g,m) * (100.-sumscl)/10000.
            end do
          end if
    end do
    found = .false.
    sumvec(:) = abs(sum(wtxy,dim=2)-1.)
      do g=begg,endg
          if (sumvec(g) > 1.e-06 .and. lndmsk==1) then
             found = .true.
             iindx = ilx
             jindx = jlx
             exit
          endif
       if (found) exit
    end do
    if ( found ) then
       write (6,*)'surfrd error: WT > 1 or <1  occurs at g= ',iindx,jindx
       call endrun()
    end if
          call CLMDebug('surfrd done')
    deallocate(sumvec)
    deallocate(cft)
    deallocate(pft)
    deallocate(pctcft_lunit)
    deallocate(pctpft_lunit)
    deallocate(pctpft)
    deallocate(plant)
    deallocate(cover)
  end subroutine surfrd
end module surfFileMod
module SNICARMod
  use shr_kind_mod , only : r8 => shr_kind_r8
  use shr_const_mod , only : SHR_CONST_RHOICE
  use clm_varcon, only: ss_alb_bc1,asm_prm_bc1,ext_cff_mss_bc1,ss_alb_bc2,asm_prm_bc2,ext_cff_mss_bc2&
                        ,ss_alb_oc1,asm_prm_oc1,ext_cff_mss_oc1,ss_alb_oc2,asm_prm_oc2,ext_cff_mss_oc2&
                        ,ss_alb_dst1,asm_prm_dst1,ext_cff_mss_dst1,ss_alb_dst2,asm_prm_dst2,ext_cff_mss_dst2 &
                        ,ss_alb_dst3,asm_prm_dst3,ext_cff_mss_dst3,ss_alb_dst4,asm_prm_dst4,ext_cff_mss_dst4 &
                        ,ss_alb_snw_drc,asm_prm_snw_drc,ext_cff_mss_snw_drc,ss_alb_snw_dfs,asm_prm_snw_dfs &
                        ,ext_cff_mss_snw_dfs,snowage_tau,snowage_kappa,snowage_drdt0 &
                        ,xx_ss_alb_snw_drc &
                        ,xx_asm_prm_snw_drc &
                        ,xx_ext_cff_mss_snw_drc &
                        ,xx_ss_alb_snw_dfs &
                        ,xx_asm_prm_snw_dfs &
                        ,xx_ext_cff_mss_snw_dfs &
                        ,xx_snowage_tau &
                        ,xx_snowage_kappa &
                        ,xx_snowage_drdt0 &
                        ,idx_Mie_snw_mx &
                        ,idx_T_max &
                        ,idx_Tgrd_max &
                        ,idx_rhos_max &
                        ,numrad_snw
  use module_cam_support, only: endrun
  implicit none
  save
  public :: SNICAR_RT
  public :: SnowAge_grain
  real(r8), public, parameter :: snw_rds_min = 54.526_r8
  integer, public, parameter :: sno_nbr_aer = 8
  logical, public, parameter :: DO_SNO_OC = .false.
  logical, public, parameter :: DO_SNO_AER = .true.
  real(r8), public, parameter :: scvng_fct_mlt_bcphi = 0.20_r8
  real(r8), public, parameter :: scvng_fct_mlt_bcpho = 0.03_r8
  real(r8), public, parameter :: scvng_fct_mlt_ocphi = 0.20_r8
  real(r8), public, parameter :: scvng_fct_mlt_ocpho = 0.03_r8
  real(r8), public, parameter :: scvng_fct_mlt_dst1 = 0.02_r8
  real(r8), public, parameter :: scvng_fct_mlt_dst2 = 0.02_r8
  real(r8), public, parameter :: scvng_fct_mlt_dst3 = 0.01_r8
  real(r8), public, parameter :: scvng_fct_mlt_dst4 = 0.01_r8
  integer, parameter :: nir_bnd_bgn = 2
  integer, parameter :: nir_bnd_end = 5
  integer, parameter :: idx_T_min = 1
  integer, parameter :: idx_Tgrd_min = 1
  integer, parameter :: idx_rhos_min = 1
  integer, parameter :: snw_rds_max_tbl = 1500
  integer, parameter :: snw_rds_min_tbl = 30
  real(r8), parameter :: snw_rds_max = 1500._r8
  real(r8), parameter :: snw_rds_refrz = 1000._r8
  real(r8), parameter :: min_snw = 1.0E-30_r8
  real(r8), parameter :: C1_liq_Brun89 = 0._r8
  real(r8), parameter :: C2_liq_Brun89 = 4.22E-13_r8
  real(r8), parameter :: tim_cns_bc_rmv = 2.2E-8_r8
  real(r8), parameter :: tim_cns_oc_rmv = 2.2E-8_r8
  real(r8), parameter :: tim_cns_dst_rmv = 2.2E-8_r8
  logical :: flg_snoage_scl = .false.
  real(r8), parameter :: xdrdt = 1.0_r8
contains
  subroutine SNICAR_RT (flg_snw_ice, lbc, ubc, num_nourbanc, filter_nourbanc, &
                        coszen, flg_slr_in, h2osno_liq, h2osno_ice, snw_rds, &
                        mss_cnc_aer_in, albsfc, albout, flx_abs)
    use clmtype
    use clm_varpar , only : nlevsno, numrad
    use shr_const_mod , only : SHR_CONST_PI
    use globals , only : nstep
    implicit none
    integer , intent(in) :: flg_snw_ice
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_nourbanc
    integer , intent(in) :: filter_nourbanc(ubc-lbc+1)
    real(r8), intent(in) :: coszen(lbc:ubc)
    integer , intent(in) :: flg_slr_in
    real(r8), intent(in) :: h2osno_liq(lbc:ubc,-nlevsno+1:0)
    real(r8), intent(in) :: h2osno_ice(lbc:ubc,-nlevsno+1:0)
    integer, intent(in) :: snw_rds(lbc:ubc,-nlevsno+1:0)
    real(r8), intent(in) :: mss_cnc_aer_in(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    real(r8), intent(in) :: albsfc(lbc:ubc,numrad)
    real(r8), intent(out) :: albout(lbc:ubc,numrad)
    real(r8), intent(out) :: flx_abs(lbc:ubc,-nlevsno+1:1,numrad)
    integer, pointer :: snl(:)
    real(r8), pointer :: h2osno(:)
    integer, pointer :: clandunit(:)
    integer, pointer :: cgridcell(:)
    integer, pointer :: ltype(:)
    real(r8), pointer :: londeg(:)
    real(r8), pointer :: latdeg(:)
    integer :: snl_lcl
    integer :: snw_rds_lcl(-nlevsno+1:0)
    real(r8):: flx_slrd_lcl(1:numrad_snw)
    real(r8):: flx_slri_lcl(1:numrad_snw)
    real(r8):: mss_cnc_aer_lcl(-nlevsno+1:0,1:sno_nbr_aer)
    real(r8):: h2osno_lcl
    real(r8):: h2osno_liq_lcl(-nlevsno+1:0)
    real(r8):: h2osno_ice_lcl(-nlevsno+1:0)
    real(r8):: albsfc_lcl(1:numrad_snw)
    real(r8):: ss_alb_snw_lcl(-nlevsno+1:0)
    real(r8):: asm_prm_snw_lcl(-nlevsno+1:0)
    real(r8):: ext_cff_mss_snw_lcl(-nlevsno+1:0)
    real(r8):: ss_alb_aer_lcl(sno_nbr_aer)
    real(r8):: asm_prm_aer_lcl(sno_nbr_aer)
    real(r8):: ext_cff_mss_aer_lcl(sno_nbr_aer)
    integer :: APRX_TYP
    integer :: DELTA
    real(r8):: flx_wgt(1:numrad_snw)
    integer :: flg_nosnl
    integer :: trip
    integer :: flg_dover
    real(r8):: albedo
    real(r8):: flx_sum
    real(r8):: albout_lcl(numrad_snw)
    real(r8):: flx_abs_lcl(-nlevsno+1:1,numrad_snw)
    real(r8):: L_snw(-nlevsno+1:0)
    real(r8):: tau_snw(-nlevsno+1:0)
    real(r8):: L_aer(-nlevsno+1:0,sno_nbr_aer)
    real(r8):: tau_aer(-nlevsno+1:0,sno_nbr_aer)
    real(r8):: tau_sum
    real(r8):: tau_clm(-nlevsno+1:0)
    real(r8):: omega_sum
    real(r8):: g_sum
    real(r8):: tau(-nlevsno+1:0)
    real(r8):: omega(-nlevsno+1:0)
    real(r8):: g(-nlevsno+1:0)
    real(r8):: tau_star(-nlevsno+1:0)
    real(r8):: omega_star(-nlevsno+1:0)
    real(r8):: g_star(-nlevsno+1:0)
    integer :: g_idx, c_idx, l_idx
    integer :: bnd_idx
    integer :: rds_idx
    integer :: snl_btm
    integer :: snl_top
    integer :: fc
    integer :: i
    integer :: j
    integer :: n
    integer :: m
    integer :: ix,k
    real(r8):: F_direct(-nlevsno+1:0)
    real(r8):: F_net(-nlevsno+1:0)
    real(r8):: F_abs(-nlevsno+1:0)
    real(r8):: F_abs_sum
    real(r8):: F_sfc_pls
    real(r8):: F_btm_net
    real(r8):: F_sfc_net
    real(r8):: energy_sum
    real(r8):: F_direct_btm
    real(r8):: mu_not
    integer :: err_idx
    real(r8):: lat_coord
    real(r8):: lon_coord
    integer :: sfctype
    real(r8):: pi
    real(r8):: gamma1(-nlevsno+1:0)
    real(r8):: gamma2(-nlevsno+1:0)
    real(r8):: gamma3(-nlevsno+1:0)
    real(r8):: gamma4(-nlevsno+1:0)
    real(r8):: lambda(-nlevsno+1:0)
    real(r8):: GAMMA(-nlevsno+1:0)
    real(r8):: mu_one
    real(r8):: e1(-nlevsno+1:0)
    real(r8):: e2(-nlevsno+1:0)
    real(r8):: e3(-nlevsno+1:0)
    real(r8):: e4(-nlevsno+1:0)
    real(r8):: C_pls_btm(-nlevsno+1:0)
    real(r8):: C_mns_btm(-nlevsno+1:0)
    real(r8):: C_pls_top(-nlevsno+1:0)
    real(r8):: C_mns_top(-nlevsno+1:0)
    real(r8):: A(-2*nlevsno+1:0)
    real(r8):: B(-2*nlevsno+1:0)
    real(r8):: D(-2*nlevsno+1:0)
    real(r8):: E(-2*nlevsno+1:0)
    real(r8):: AS(-2*nlevsno+1:0)
    real(r8):: DS(-2*nlevsno+1:0)
    real(r8):: X(-2*nlevsno+1:0)
    real(r8):: Y(-2*nlevsno+1:0)
    if (flg_snw_ice == 1) then
       snl => clm3%g%l%c%cps%snl
       h2osno => clm3%g%l%c%cws%h2osno
       clandunit => clm3%g%l%c%landunit
       cgridcell => clm3%g%l%c%gridcell
       ltype => clm3%g%l%itype
       londeg => clm3%g%londeg
       latdeg => clm3%g%latdeg
    endif
  ix = 0
  do i=1, idx_Mie_snw_mx
  do j=1, numrad_snw
    ix = ix+1
    ss_alb_snw_drc(i,j) = xx_ss_alb_snw_drc(ix)
    asm_prm_snw_drc(i,j) = xx_asm_prm_snw_drc(ix)
    ext_cff_mss_snw_drc(i,j) = xx_ext_cff_mss_snw_drc(ix)
    ss_alb_snw_dfs(i,j) = xx_ss_alb_snw_dfs(ix)
    asm_prm_snw_dfs(i,j) = xx_asm_prm_snw_dfs(ix)
    ext_cff_mss_snw_dfs(i,j) = xx_ext_cff_mss_snw_dfs(ix)
  end do
  end do
 ix = 0
 do i=1,idx_T_max
 do j=1,idx_Tgrd_max
 do k=1,idx_rhos_max
    ix = ix + 1
    snowage_tau(i,j,k) = xx_snowage_tau(ix)
    snowage_kappa(i,j,k) = xx_snowage_kappa(ix)
    snowage_drdt0(i,j,k) = xx_snowage_drdt0(ix)
 end do
 end do
 end do
    pi = SHR_CONST_PI
    DELTA = 1
    do fc = 1,num_nourbanc
       c_idx = filter_nourbanc(fc)
       do i=-nlevsno+1,1,1
          flx_abs_lcl(:,:) = 0._r8
          flx_abs(c_idx,i,:) = 0._r8
       enddo
       if (flg_snw_ice == 1) then
          h2osno_lcl = h2osno(c_idx)
       else
          h2osno_lcl = h2osno_ice(c_idx,0)
       endif
       if ((coszen(c_idx) > 0._r8) .and. (h2osno_lcl > min_snw)) then
          if (flg_snw_ice == 1) then
             if (snl(c_idx) > -1) then
                flg_nosnl = 1
                snl_lcl = -1
                h2osno_ice_lcl(0) = h2osno_lcl
                h2osno_liq_lcl(0) = 0._r8
                snw_rds_lcl(0) = nint(snw_rds_min)
             else
                flg_nosnl = 0
                snl_lcl = snl(c_idx)
                h2osno_liq_lcl(:) = h2osno_liq(c_idx,:)
                h2osno_ice_lcl(:) = h2osno_ice(c_idx,:)
                snw_rds_lcl(:) = snw_rds(c_idx,:)
             endif
             snl_btm = 0
             snl_top = snl_lcl+1
             l_idx = clandunit(c_idx)
             g_idx = cgridcell(c_idx)
             sfctype = ltype(l_idx)
             lat_coord = latdeg(g_idx)
             lon_coord = londeg(g_idx)
          else
             flg_nosnl = 0
             snl_lcl = -1
             h2osno_liq_lcl(:) = h2osno_liq(c_idx,:)
             h2osno_ice_lcl(:) = h2osno_ice(c_idx,:)
             snw_rds_lcl(:) = snw_rds(c_idx,:)
             snl_btm = 0
             snl_top = 0
             sfctype = -1
             lat_coord = -90
             lon_coord = 0
          endif
          do j=1,sno_nbr_aer
             mss_cnc_aer_lcl(:,j) = mss_cnc_aer_in(c_idx,:,j)
          enddo
          albsfc_lcl(1) = albsfc(c_idx,1)
          albsfc_lcl(nir_bnd_bgn:nir_bnd_end) = albsfc(c_idx,2)
          do i=snl_top,snl_btm,1
             if ((snw_rds_lcl(i) < snw_rds_min_tbl) .or. (snw_rds_lcl(i) > snw_rds_max_tbl)) then
                write (6,*) "SNICAR ERROR: snow grain radius of ", snw_rds_lcl(i), " out of bounds."
                write (6,*) "NSTEP= ", nstep
                write (6,*) "flg_snw_ice= ", flg_snw_ice
                write (6,*) "column: ", c_idx, " level: ", i, " snl(c)= ", snl_lcl
                write (6,*) "lat= ", lat_coord, " lon= ", lon_coord
                write (6,*) "h2osno(c)= ", h2osno_lcl
                call endrun()
             endif
          enddo
          if (numrad_snw==3) then
             if (flg_slr_in == 1) then
                flx_wgt(1) = 1._r8
                flx_wgt(2) = 0.66628670195247_r8
                flx_wgt(3) = 0.33371329804753_r8
             elseif (flg_slr_in == 2) then
                flx_wgt(1) = 1._r8
                flx_wgt(2) = 0.77887652162877_r8
                flx_wgt(3) = 0.22112347837123_r8
             endif
          elseif(numrad_snw==5) then
             if (flg_slr_in == 1) then
                flx_wgt(1) = 1._r8
                flx_wgt(2) = 0.49352158521175_r8
                flx_wgt(3) = 0.18099494230665_r8
                flx_wgt(4) = 0.12094898498813_r8
                flx_wgt(5) = 0.20453448749347_r8
             elseif (flg_slr_in == 2) then
                flx_wgt(1) = 1._r8
                flx_wgt(2) = 0.58581507618433_r8
                flx_wgt(3) = 0.20156903770812_r8
                flx_wgt(4) = 0.10917889346386_r8
                flx_wgt(5) = 0.10343699264369_r8
             endif
          endif
          do bnd_idx = 1,numrad_snw
             mu_not = coszen(c_idx)
             flg_dover = 1
             err_idx = 0
             do while (flg_dover > 0)
                if (bnd_idx == 1) then
                   if (flg_dover == 2) then
                      APRX_TYP = 3
                   elseif (flg_dover == 3) then
                      APRX_TYP = 1
                      if (coszen(c_idx) > 0.5_r8) then
                         mu_not = mu_not - 0.02_r8
                      else
                         mu_not = mu_not + 0.02_r8
                      endif
                   elseif (flg_dover == 4) then
                      APRX_TYP = 3
                   else
                      APRX_TYP = 1
                   endif
                else
                   if (flg_dover == 2) then
                      APRX_TYP = 1
                   elseif (flg_dover == 3) then
                      APRX_TYP = 3
                      if (coszen(c_idx) > 0.5_r8) then
                         mu_not = mu_not - 0.02_r8
                      else
                         mu_not = mu_not + 0.02_r8
                      endif
                   elseif (flg_dover == 4) then
                      APRX_TYP = 1
                   else
                      APRX_TYP = 3
                   endif
                endif
                if (flg_slr_in == 1) then
                   flx_slrd_lcl(bnd_idx) = 1._r8/(mu_not*pi)
                   flx_slri_lcl(bnd_idx) = 0._r8
                else
                   flx_slrd_lcl(bnd_idx) = 0._r8
                   flx_slri_lcl(bnd_idx) = 1._r8
                endif
                if ( (numrad_snw == 5).and.((bnd_idx == 5).or.(bnd_idx == 4)) ) then
                   mss_cnc_aer_lcl(:,:) = 0._r8
                endif
                if ( (numrad_snw == 3).and.(bnd_idx == 3) ) then
                   mss_cnc_aer_lcl(:,:) = 0._r8
                endif
                if (flg_slr_in == 1) then
                   do i=snl_top,snl_btm,1
                      rds_idx = snw_rds_lcl(i) - snw_rds_min_tbl + 1
                      ss_alb_snw_lcl(i) = ss_alb_snw_drc(rds_idx,bnd_idx)
                      asm_prm_snw_lcl(i) = asm_prm_snw_drc(rds_idx,bnd_idx)
                      ext_cff_mss_snw_lcl(i) = ext_cff_mss_snw_drc(rds_idx,bnd_idx)
                   enddo
                elseif (flg_slr_in == 2) then
                   do i=snl_top,snl_btm,1
                      rds_idx = snw_rds_lcl(i) - snw_rds_min_tbl + 1
                      ss_alb_snw_lcl(i) = ss_alb_snw_dfs(rds_idx,bnd_idx)
                      asm_prm_snw_lcl(i) = asm_prm_snw_dfs(rds_idx,bnd_idx)
                      ext_cff_mss_snw_lcl(i) = ext_cff_mss_snw_dfs(rds_idx,bnd_idx)
                   enddo
                endif
                ss_alb_aer_lcl(1) = ss_alb_bc1(1,bnd_idx)
                asm_prm_aer_lcl(1) = asm_prm_bc1(1,bnd_idx)
                ext_cff_mss_aer_lcl(1) = ext_cff_mss_bc1(1,bnd_idx)
                ss_alb_aer_lcl(2) = ss_alb_bc2(1,bnd_idx)
                asm_prm_aer_lcl(2) = asm_prm_bc2(1,bnd_idx)
                ext_cff_mss_aer_lcl(2) = ext_cff_mss_bc2(1,bnd_idx)
                ss_alb_aer_lcl(3) = ss_alb_oc1(1,bnd_idx)
                asm_prm_aer_lcl(3) = asm_prm_oc1(1,bnd_idx)
                ext_cff_mss_aer_lcl(3) = ext_cff_mss_oc1(1,bnd_idx)
                ss_alb_aer_lcl(4) = ss_alb_oc2(1,bnd_idx)
                asm_prm_aer_lcl(4) = asm_prm_oc2(1,bnd_idx)
                ext_cff_mss_aer_lcl(4) = ext_cff_mss_oc2(1,bnd_idx)
                ss_alb_aer_lcl(5) = ss_alb_dst1(1,bnd_idx)
                asm_prm_aer_lcl(5) = asm_prm_dst1(1,bnd_idx)
                ext_cff_mss_aer_lcl(5) = ext_cff_mss_dst1(1,bnd_idx)
                ss_alb_aer_lcl(6) = ss_alb_dst2(1,bnd_idx)
                asm_prm_aer_lcl(6) = asm_prm_dst2(1,bnd_idx)
                ext_cff_mss_aer_lcl(6) = ext_cff_mss_dst2(1,bnd_idx)
                ss_alb_aer_lcl(7) = ss_alb_dst3(1,bnd_idx)
                asm_prm_aer_lcl(7) = asm_prm_dst3(1,bnd_idx)
                ext_cff_mss_aer_lcl(7) = ext_cff_mss_dst3(1,bnd_idx)
                ss_alb_aer_lcl(8) = ss_alb_dst4(1,bnd_idx)
                asm_prm_aer_lcl(8) = asm_prm_dst4(1,bnd_idx)
                ext_cff_mss_aer_lcl(8) = ext_cff_mss_dst4(1,bnd_idx)
                do i=snl_top,snl_btm,1
                   L_snw(i) = h2osno_ice_lcl(i)+h2osno_liq_lcl(i)
                   tau_snw(i) = L_snw(i)*ext_cff_mss_snw_lcl(i)
                   do j=1,sno_nbr_aer
                      L_aer(i,j) = L_snw(i)*mss_cnc_aer_lcl(i,j)
                      tau_aer(i,j) = L_aer(i,j)*ext_cff_mss_aer_lcl(j)
                   enddo
                   tau_sum = 0._r8
                   omega_sum = 0._r8
                   g_sum = 0._r8
                   do j=1,sno_nbr_aer
                      tau_sum = tau_sum + tau_aer(i,j)
                      omega_sum = omega_sum + (tau_aer(i,j)*ss_alb_aer_lcl(j))
                      g_sum = g_sum + (tau_aer(i,j)*ss_alb_aer_lcl(j)*asm_prm_aer_lcl(j))
                   enddo
                   tau(i) = tau_sum + tau_snw(i)
                   if(tau(i) == 0) then
                      write(6,*) 'FATAL ERROR in SNICAR RT, tau(',i,') is the denominatoer can not equal to ',tau(i)
                       call endrun()
                   end if
                   omega(i) = (1/tau(i))*(omega_sum+(ss_alb_snw_lcl(i)*tau_snw(i)))
                   g(i) = (1/(tau(i)*omega(i)))*(g_sum+ (asm_prm_snw_lcl(i)*ss_alb_snw_lcl(i)*tau_snw(i)))
                enddo
                if (DELTA == 1) then
                   do i=snl_top,snl_btm,1
                      g_star(i) = g(i)/(1+g(i))
                      omega_star(i) = ((1-(g(i)**2))*omega(i)) / (1-(omega(i)*(g(i)**2)))
                      tau_star(i) = (1-(omega(i)*(g(i)**2)))*tau(i)
                   enddo
                else
                   do i=snl_top,snl_btm,1
                      g_star(i) = g(i)
                      omega_star(i) = omega(i)
                      tau_star(i) = tau(i)
                   enddo
                endif
                tau_clm(snl_top) = 0._r8
                do i=snl_top+1,snl_btm,1
                   tau_clm(i) = tau_clm(i-1)+tau_star(i-1)
                enddo
                F_direct_btm = albsfc_lcl(bnd_idx)*mu_not*exp(-(tau_clm(snl_btm)+tau_star(snl_btm))/mu_not)*pi*flx_slrd_lcl(bnd_idx)
                if (APRX_TYP==1) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = (7-(omega_star(i)*(4+(3*g_star(i)))))/4
                      gamma2(i) = -(1-(omega_star(i)*(4-(3*g_star(i)))))/4
                      gamma3(i) = (2-(3*g_star(i)*mu_not))/4
                      gamma4(i) = 1-gamma3(i)
                      mu_one = 0.5
                   enddo
                elseif (APRX_TYP==2) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = (3**0.5)*(2-(omega_star(i)*(1+g_star(i))))/2
                      gamma2(i) = omega_star(i)*(3**0.5)*(1-g_star(i))/2
                      gamma3(i) = (1-((3**0.5)*g_star(i)*mu_not))/2
                      gamma4(i) = 1-gamma3(i)
                      mu_one = 1/(3**0.5)
                   enddo
                elseif (APRX_TYP==3) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = 2 - (omega_star(i)*(1+g_star(i)))
                      gamma2(i) = omega_star(i)*(1-g_star(i))
                      gamma3(i) = (1-((3**0.5)*g_star(i)*mu_not))/2
                      gamma4(i) = 1-gamma3(i)
                      mu_one = 0.5
                   enddo
                endif
                do i=snl_top,snl_btm,1
                   lambda(i) = sqrt(abs((gamma1(i)**2) - (gamma2(i)**2)))
                   GAMMA(i) = gamma2(i)/(gamma1(i)+lambda(i))
                   e1(i) = 1+(GAMMA(i)*exp(-lambda(i)*tau_star(i)))
                   e2(i) = 1-(GAMMA(i)*exp(-lambda(i)*tau_star(i)))
                   e3(i) = GAMMA(i) + exp(-lambda(i)*tau_star(i))
                   e4(i) = GAMMA(i) - exp(-lambda(i)*tau_star(i))
                enddo
                do i=snl_top,snl_btm,1
                   if (flg_slr_in == 1) then
                      C_pls_btm(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-(tau_clm(i)+tau_star(i))/mu_not)* &
                                     (((gamma1(i)-(1/mu_not))*gamma3(i))+ &
                                     (gamma4(i)*gamma2(i))))/((lambda(i)**2)-(1/(mu_not**2)))
                      C_mns_btm(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-(tau_clm(i)+tau_star(i))/mu_not)* &
                                     (((gamma1(i)+(1/mu_not))*gamma4(i))+ &
                                     (gamma2(i)*gamma3(i))))/((lambda(i)**2)-(1/(mu_not**2)))
                      C_pls_top(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-tau_clm(i)/mu_not)*(((gamma1(i)-(1/mu_not))* &
                                     gamma3(i))+(gamma4(i)*gamma2(i))))/((lambda(i)**2)-(1/(mu_not**2)))
                      C_mns_top(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-tau_clm(i)/mu_not)*(((gamma1(i)+(1/mu_not))* &
                                     gamma4(i))+(gamma2(i)*gamma3(i))))/((lambda(i)**2)-(1/(mu_not**2)))
                   else
                      C_pls_btm(i) = 0._r8
                      C_mns_btm(i) = 0._r8
                      C_pls_top(i) = 0._r8
                      C_mns_top(i) = 0._r8
                   endif
                enddo
                do i=2*snl_lcl+1,0,1
                   if (i==(2*snl_lcl+1)) then
                      A(i) = 0
                      B(i) = e1(snl_top)
                      D(i) = -e2(snl_top)
                      E(i) = flx_slri_lcl(bnd_idx)-C_mns_top(snl_top)
                   elseif(i==0) then
                      A(i) = e1(snl_btm)-(albsfc_lcl(bnd_idx)*e3(snl_btm))
                      B(i) = e2(snl_btm)-(albsfc_lcl(bnd_idx)*e4(snl_btm))
                      D(i) = 0
                      E(i) = F_direct_btm-C_pls_btm(snl_btm)+(albsfc_lcl(bnd_idx)*C_mns_btm(snl_btm))
                   elseif(mod(i,2)==-1) then
                      n=floor(i/2.0)
                      A(i) = (e2(n)*e3(n))-(e4(n)*e1(n))
                      B(i) = (e1(n)*e1(n+1))-(e3(n)*e3(n+1))
                      D(i) = (e3(n)*e4(n+1))-(e1(n)*e2(n+1))
                      E(i) = (e3(n)*(C_pls_top(n+1)-C_pls_btm(n)))+(e1(n)*(C_mns_btm(n)-C_mns_top(n+1)))
                   elseif(mod(i,2)==0) then
                      n=(i/2)
                      A(i) = (e2(n+1)*e1(n))-(e3(n)*e4(n+1))
                      B(i) = (e2(n)*e2(n+1))-(e4(n)*e4(n+1))
                      D(i) = (e1(n+1)*e4(n+1))-(e2(n+1)*e3(n+1))
                      E(i) = (e2(n+1)*(C_pls_top(n+1)-C_pls_btm(n)))+(e4(n+1)*(C_mns_top(n+1)-C_mns_btm(n)))
                   endif
                enddo
                AS(0) = A(0)/B(0)
                DS(0) = E(0)/B(0)
                do i=-1,(2*snl_lcl+1),-1
                   X(i) = 1/(B(i)-(D(i)*AS(i+1)))
                   AS(i) = A(i)*X(i)
                   DS(i) = (E(i)-(D(i)*DS(i+1)))*X(i)
                enddo
                Y(2*snl_lcl+1) = DS(2*snl_lcl+1)
                do i=(2*snl_lcl+2),0,1
                   Y(i) = DS(i)-(AS(i)*Y(i-1))
                enddo
                do i=snl_top,snl_btm,1
                   F_direct(i) = mu_not*pi*flx_slrd_lcl(bnd_idx)*exp(-(tau_clm(i)+tau_star(i))/mu_not)
                   F_net(i) = (Y(2*i-1)*(e1(i)-e3(i))) + (Y(2*i)*(e2(i)-e4(i))) + &
                                 C_pls_btm(i) - C_mns_btm(i) - F_direct(i)
                enddo
                F_sfc_pls = (Y(2*snl_lcl+1)*(exp(-lambda(snl_top)*tau_star(snl_top))+ &
                            GAMMA(snl_top))) + (Y(2*snl_lcl+2)*(exp(-lambda(snl_top)* &
                            tau_star(snl_top))-GAMMA(snl_top))) + C_pls_top(snl_top)
                F_btm_net = -F_net(snl_btm)
                albedo = F_sfc_pls/((mu_not*pi*flx_slrd_lcl(bnd_idx))+flx_slri_lcl(bnd_idx))
                F_sfc_net = F_sfc_pls - ((mu_not*pi*flx_slrd_lcl(bnd_idx))+flx_slri_lcl(bnd_idx))
                trip = 0
                do i=snl_top,snl_btm,1
                   if(i==snl_top) then
                      F_abs(i) = F_net(i)-F_sfc_net
                   else
                      F_abs(i) = F_net(i)-F_net(i-1)
                   endif
                   flx_abs_lcl(i,bnd_idx) = F_abs(i)
                   if (flx_abs_lcl(i,bnd_idx) < -0.00001) then
                      trip = 1
                   endif
                enddo
                flx_abs_lcl(1,bnd_idx) = F_btm_net
                if (flg_nosnl == 1) then
                   flx_abs_lcl(0,bnd_idx) = F_abs(0)
                   flx_abs_lcl(1,bnd_idx) = F_btm_net
                endif
                do i=snl_top,1,1
                   if (flx_abs_lcl(i,bnd_idx) < 0._r8) then
                      flx_abs_lcl(i,bnd_idx) = 0._r8
                   endif
                enddo
                F_abs_sum = 0._r8
                do i=snl_top,snl_btm,1
                   F_abs_sum = F_abs_sum + F_abs(i)
                enddo
                if (F_abs_sum > 1._r8) then
                   trip = 1
                endif
                if ((albedo < 0._r8).and.(trip==0)) then
                   write(6,*) 'ERROR: albedo <0 = ', albedo
                   trip = 1
                endif
                if ((trip == 1).and.(flg_dover == 1)) then
                   flg_dover = 2
                elseif ((trip == 1).and.(flg_dover == 2)) then
                   flg_dover = 3
                elseif ((trip == 1).and.(flg_dover == 3)) then
                   flg_dover = 4
                elseif((trip == 1).and.(flg_dover == 4).and.(err_idx < 20)) then
                   flg_dover = 3
                   err_idx = err_idx + 1
                   write(6,*) "SNICAR WARNING: Both approximations failed with new zenith angle :(. Zenith= ", mu_not, &
                                  " called from: ", flg_snw_ice, " flg_slr= ", flg_slr_in, " bnd= ", bnd_idx, " Moving the sun..."
                elseif((trip == 1).and.(flg_dover == 4).and.(err_idx >= 20)) then
                   flg_dover = 0
                   write(6,*) "SNICAR ERROR: FOUND A WORMHOLE. STUCK IN INFINITE LOOP! Called from: ", flg_snw_ice
                   write(6,*) "SNICAR STATS: snw_rds(0)= ", snw_rds(c_idx,0)
                   write(6,*) "SNICAR STATS: L_snw(0)= ", L_snw(0)
                   write(6,*) "SNICAR STATS: h2osno= ", h2osno_lcl, " snl= ", snl_lcl
                   write(6,*) "SNICAR STATS: soot1(0)= ", mss_cnc_aer_lcl(0,1)
                   write(6,*) "SNICAR STATS: soot2(0)= ", mss_cnc_aer_lcl(0,2)
                   write(6,*) "SNICAR STATS: dust1(0)= ", mss_cnc_aer_lcl(0,3)
                   write(6,*) "SNICAR STATS: dust2(0)= ", mss_cnc_aer_lcl(0,4)
                   write(6,*) "SNICAR STATS: dust3(0)= ", mss_cnc_aer_lcl(0,5)
                   write(6,*) "SNICAR STATS: dust4(0)= ", mss_cnc_aer_lcl(0,6)
                   call endrun()
                else
                   flg_dover = 0
                endif
             enddo
             energy_sum = (mu_not*pi*flx_slrd_lcl(bnd_idx)) + flx_slri_lcl(bnd_idx) - (F_abs_sum + F_btm_net + F_sfc_pls)
             if (abs(energy_sum) > 0.00001_r8) then
                write (6,"(a,e12.6,a,i6,a,i6)") "SNICAR ERROR: Energy conservation error of : ", energy_sum, &
                             " at timestep: ", nstep, " at column: ", c_idx
                call endrun()
             endif
             albout_lcl(bnd_idx) = albedo
             if (albout_lcl(bnd_idx) > 1.0) then
                write (6,*) "SNICAR ERROR: Albedo > 1.0 at c: ", c_idx, " NSTEP= ",nstep
                write (6,*) "SNICAR STATS: bnd_idx= ",bnd_idx
                write (6,*) "SNICAR STATS: albout_lcl(bnd)= ",albout_lcl(bnd_idx), " albsfc_lcl(bnd_idx)= ",albsfc_lcl(bnd_idx)
                write (6,*) "SNICAR STATS: landtype= ", sfctype
                write (6,*) "SNICAR STATS: h2osno= ", h2osno_lcl, " snl= ", snl_lcl
                write (6,*) "SNICAR STATS: coszen= ", coszen(c_idx), " flg_slr= ", flg_slr_in
                write (6,*) "SNICAR STATS: soot(-4)= ", mss_cnc_aer_lcl(-4,1)
                write (6,*) "SNICAR STATS: soot(-3)= ", mss_cnc_aer_lcl(-3,1)
                write (6,*) "SNICAR STATS: soot(-2)= ", mss_cnc_aer_lcl(-2,1)
                write (6,*) "SNICAR STATS: soot(-1)= ", mss_cnc_aer_lcl(-1,1)
                write (6,*) "SNICAR STATS: soot(0)= ", mss_cnc_aer_lcl(0,1)
                write (6,*) "SNICAR STATS: L_snw(-4)= ", L_snw(-4)
                write (6,*) "SNICAR STATS: L_snw(-3)= ", L_snw(-3)
                write (6,*) "SNICAR STATS: L_snw(-2)= ", L_snw(-2)
                write (6,*) "SNICAR STATS: L_snw(-1)= ", L_snw(-1)
                write (6,*) "SNICAR STATS: L_snw(0)= ", L_snw(0)
                write (6,*) "SNICAR STATS: snw_rds(-4)= ", snw_rds(c_idx,-4)
                write (6,*) "SNICAR STATS: snw_rds(-3)= ", snw_rds(c_idx,-3)
                write (6,*) "SNICAR STATS: snw_rds(-2)= ", snw_rds(c_idx,-2)
                write (6,*) "SNICAR STATS: snw_rds(-1)= ", snw_rds(c_idx,-1)
                write (6,*) "SNICAR STATS: snw_rds(0)= ", snw_rds(c_idx,0)
                call endrun()
             endif
          enddo
          albout(c_idx,1) = albout_lcl(1)
          flx_sum = 0._r8
          do bnd_idx= nir_bnd_bgn,nir_bnd_end
             flx_sum = flx_sum + flx_wgt(bnd_idx)*albout_lcl(bnd_idx)
          end do
          albout(c_idx,2) = flx_sum / sum(flx_wgt(nir_bnd_bgn:nir_bnd_end))
          flx_abs(c_idx,:,1) = flx_abs_lcl(:,1)
          do i=snl_top,1,1
             flx_sum = 0._r8
             do bnd_idx= nir_bnd_bgn,nir_bnd_end
                flx_sum = flx_sum + flx_wgt(bnd_idx)*flx_abs_lcl(i,bnd_idx)
             enddo
             flx_abs(c_idx,i,2) = flx_sum / sum(flx_wgt(nir_bnd_bgn:nir_bnd_end))
          end do
       elseif ( (coszen(c_idx) > 0._r8) .and. (h2osno_lcl < min_snw) .and. (h2osno_lcl > 0._r8) ) then
          albout(c_idx,1) = albsfc(c_idx,1)
          albout(c_idx,2) = albsfc(c_idx,2)
       else
          albout(c_idx,1) = 0._r8
          albout(c_idx,2) = 0._r8
       endif
    enddo
  end subroutine SNICAR_RT
  subroutine SnowAge_grain(lbc, ubc, num_snowc, filter_snowc, num_nosnowc, filter_nosnowc)
    use clmtype
    use clm_varpar , only : nlevsno
    use clm_varcon , only : spval
    use shr_const_mod , only : SHR_CONST_RHOICE, SHR_CONST_PI
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_snowc
    integer, intent(in) :: filter_snowc(ubc-lbc+1)
    integer, intent(in) :: num_nosnowc
    integer, intent(in) :: filter_nosnowc(ubc-lbc+1)
    real(r8), pointer :: t_soisno(:,:)
    integer, pointer :: snl(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: snw_rds(:,:)
    real(r8), pointer :: snw_rds_top(:)
    real(r8), pointer :: sno_liq_top(:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: snot_top(:)
    real(r8), pointer :: dTdz_top(:)
    real(r8), pointer :: qflx_snow_grnd_col(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: qflx_snofrz_lyr(:,:)
    logical , pointer :: do_capsnow(:)
    integer :: snl_top
    integer :: snl_btm
    integer :: i
    integer :: c_idx
    integer :: fc
    integer :: T_idx
    integer :: Tgrd_idx
    integer :: rhos_idx
    real(r8) :: t_snotop
    real(r8) :: t_snobtm
    real(r8) :: dTdz(lbc:ubc,-nlevsno:0)
    real(r8) :: bst_tau
    real(r8) :: bst_kappa
    real(r8) :: bst_drdt0
    real(r8) :: dr
    real(r8) :: dr_wet
    real(r8) :: dr_fresh
    real(r8) :: newsnow
    real(r8) :: refrzsnow
    real(r8) :: frc_newsnow
    real(r8) :: frc_oldsnow
    real(r8) :: frc_refrz
    real(r8) :: frc_liq
    real(r8) :: rhos
    real(r8) :: h2osno_lyr
    t_soisno => clm3%g%l%c%ces%t_soisno
    snl => clm3%g%l%c%cps%snl
    t_grnd => clm3%g%l%c%ces%t_grnd
    dz => clm3%g%l%c%cps%dz
    h2osno => clm3%g%l%c%cws%h2osno
    snw_rds => clm3%g%l%c%cps%snw_rds
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    snot_top => clm3%g%l%c%cps%snot_top
    dTdz_top => clm3%g%l%c%cps%dTdz_top
    snw_rds_top => clm3%g%l%c%cps%snw_rds_top
    sno_liq_top => clm3%g%l%c%cps%sno_liq_top
    qflx_snow_grnd_col => clm3%g%l%c%cwf%pwf_a%qflx_snow_grnd
    qflx_snwcp_ice => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_ice
    qflx_snofrz_lyr => clm3%g%l%c%cwf%qflx_snofrz_lyr
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    do fc = 1, num_snowc
       c_idx = filter_snowc(fc)
       snl_btm = 0
       snl_top = snl(c_idx) + 1
       do i=snl_top,snl_btm,1
          h2osno_lyr = h2osoi_liq(c_idx,i) + h2osoi_ice(c_idx,i)
          if (i == snl_top) then
             t_snotop = t_grnd(c_idx)
             t_snobtm = (t_soisno(c_idx,i+1)*dz(c_idx,i) + t_soisno(c_idx,i)*dz(c_idx,i+1)) / (dz(c_idx,i)+dz(c_idx,i+1))
          else
             t_snotop = (t_soisno(c_idx,i-1)*dz(c_idx,i) + t_soisno(c_idx,i)*dz(c_idx,i-1)) / (dz(c_idx,i)+dz(c_idx,i-1))
             t_snobtm = (t_soisno(c_idx,i+1)*dz(c_idx,i) + t_soisno(c_idx,i)*dz(c_idx,i+1)) / (dz(c_idx,i)+dz(c_idx,i+1))
          endif
          dTdz(c_idx,i) = abs((t_snotop - t_snobtm) / dz(c_idx,i))
          rhos = (h2osoi_liq(c_idx,i)+h2osoi_ice(c_idx,i)) / dz(c_idx,i)
          T_idx = nint((t_soisno(c_idx,i)-223) / 5) + 1
          Tgrd_idx = nint(dTdz(c_idx,i) / 10) + 1
          rhos_idx = nint((rhos-50) / 50) + 1
          if (T_idx < idx_T_min) then
             T_idx = idx_T_min
          endif
          if (T_idx > idx_T_max) then
             T_idx = idx_T_max
          endif
          if (Tgrd_idx < idx_Tgrd_min) then
             Tgrd_idx = idx_Tgrd_min
          endif
          if (Tgrd_idx > idx_Tgrd_max) then
             Tgrd_idx = idx_Tgrd_max
          endif
          if (rhos_idx < idx_rhos_min) then
             rhos_idx = idx_rhos_min
          endif
          if (rhos_idx > idx_rhos_max) then
             rhos_idx = idx_rhos_max
          endif
          bst_tau = snowage_tau(T_idx,Tgrd_idx,rhos_idx)
          bst_kappa = snowage_kappa(T_idx,Tgrd_idx,rhos_idx)
          bst_drdt0 = snowage_drdt0(T_idx,Tgrd_idx,rhos_idx)
          dr_fresh = snw_rds(c_idx,i)-snw_rds_min
          dr = (bst_drdt0*(bst_tau/(dr_fresh+bst_tau))**(1/bst_kappa)) * (dtime/3600)
          frc_liq = min(0.1_r8, (h2osoi_liq(c_idx,i) / (h2osoi_liq(c_idx,i)+h2osoi_ice(c_idx,i))))
          dr_wet = 1E18_r8*(dtime*(C2_liq_Brun89*(frc_liq**(3))) / (4*SHR_CONST_PI*snw_rds(c_idx,i)**(2)))
          dr = dr + dr_wet
          if (flg_snoage_scl) then
             dr = dr*xdrdt
          endif
          if (do_capsnow(c_idx)) then
             newsnow = max(0._r8, (qflx_snwcp_ice(c_idx)*dtime))
          else
             newsnow = max(0._r8, (qflx_snow_grnd_col(c_idx)*dtime))
          endif
          refrzsnow = max(0._r8, (qflx_snofrz_lyr(c_idx,i)*dtime))
          frc_refrz = refrzsnow / h2osno_lyr
          if (i == snl_top) then
             frc_newsnow = newsnow / h2osno_lyr
          else
             frc_newsnow = 0._r8
          endif
          if ((frc_refrz + frc_newsnow) > 1._r8) then
             frc_refrz = frc_refrz / (frc_refrz + frc_newsnow)
             frc_newsnow = 1._r8 - frc_refrz
             frc_oldsnow = 0._r8
          else
             frc_oldsnow = 1._r8 - frc_refrz - frc_newsnow
          endif
          snw_rds(c_idx,i) = (snw_rds(c_idx,i)+dr)*frc_oldsnow + snw_rds_min*frc_newsnow + snw_rds_refrz*frc_refrz
          if (snw_rds(c_idx,i) < snw_rds_min) then
             snw_rds(c_idx,i) = snw_rds_min
          endif
          if (snw_rds(c_idx,i) > snw_rds_max) then
             snw_rds(c_idx,i) = snw_rds_max
          end if
          if (i == snl_top) then
             snot_top(c_idx) = t_soisno(c_idx,i)
             dTdz_top(c_idx) = dTdz(c_idx,i)
             snw_rds_top(c_idx) = snw_rds(c_idx,i)
             sno_liq_top(c_idx) = h2osoi_liq(c_idx,i) / (h2osoi_liq(c_idx,i)+h2osoi_ice(c_idx,i))
          endif
       enddo
    enddo
    do fc = 1, num_nosnowc
       c_idx = filter_nosnowc(fc)
       if (h2osno(c_idx) > 0._r8) then
          snw_rds(c_idx,0) = snw_rds_min
       endif
    enddo
  end subroutine SnowAge_grain
end module SNICARMod
subroutine mkarbinit(snlx ,snowdpx , dzclmx ,zclmx ,&
                  ziclmx ,h2osnox ,h2osoi_liqx,h2osoi_icex,t_grndx,&
                  t_soisnox ,t_lakex ,t_vegx ,h2ocanx ,h2ocan_colx,&
                  h2osoi_volx,t_ref2mx,snw_rdsx &
)
  use shr_kind_mod , only : r8 => shr_kind_r8
  use clmtype
  use decompMod , only : get_proc_bounds
  use clm_varpar , only : nlevgrnd,nlevsoi, nlevsno, nlevlak,maxpatch
  use clm_varcon , only : bdsno, istice, istwet, istsoil, &
                            denice, denh2o, spval, sb, tfrz
  use SNICARMod , only : snw_rds_min
  use globals , only : nstep
  implicit none
  integer , pointer :: pcolumn(:)
  integer , pointer :: clandunit(:)
  integer , pointer :: ltype(:)
  logical , pointer :: lakpoi(:)
  real(r8), pointer :: dz(:,:)
  real(r8), pointer :: zi(:,:)
  real(r8), pointer :: z(:,:)
  real(r8), pointer :: watsat(:,:)
  real(r8), pointer :: h2osoi_ice(:,:)
  real(r8), pointer :: h2osoi_liq(:,:)
  real(r8), pointer :: bsw2(:,:)
  real(r8), pointer :: psisat(:,:)
  real(r8), pointer :: vwcsat(:,:)
  real(r8), pointer :: wa(:)
  real(r8), pointer :: wt(:)
  real(r8), pointer :: zwt(:)
  real(r8), pointer :: h2ocan_loss(:)
  integer , pointer :: snl(:)
  real(r8), pointer :: t_soisno(:,:)
  real(r8), pointer :: t_lake(:,:)
  real(r8), pointer :: t_grnd(:)
  real(r8), pointer :: t_veg(:)
  real(r8), pointer :: h2osoi_vol(:,:)
  real(r8), pointer :: h2ocan_col(:)
  real(r8), pointer :: h2ocan_pft(:)
  real(r8), pointer :: h2osno(:)
  real(r8), pointer :: snowdp(:)
   real(r8), pointer :: t_ref2m(:)
    real(r8) :: t_ref2mx(maxpatch)
  real(r8), pointer :: eflx_lwrad_out(:)
  real(r8), pointer :: soilpsi(:,:)
    integer :: snlx(maxpatch)
    real(r8) :: snowdpx(maxpatch)
    real(r8) :: h2osnox(maxpatch)
    real(r8) :: t_grndx(maxpatch)
    real(r8) :: t_vegx(maxpatch)
    real(r8) :: h2ocanx(maxpatch)
    real(r8) :: h2ocan_colx(maxpatch)
    real(r8) :: snw_rdsx(maxpatch,-nlevsno+1:0)
    real(r8) :: t_lakex(maxpatch,nlevlak)
    real(r8) :: t_soisnox(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_liqx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_icex(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: dzclmx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: zclmx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: ziclmx(maxpatch,-nlevsno:nlevgrnd)
    real(r8) :: h2osoi_volx(maxpatch,nlevgrnd)
    real(r8), pointer :: snw_rds(:,:)
    real(r8), pointer :: snw_rds_top(:)
    real(r8), pointer :: sno_liq_top(:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bctot(:,:)
    real(r8), pointer :: mss_bc_col(:)
    real(r8), pointer :: mss_bc_top(:)
    real(r8), pointer :: mss_cnc_bcphi(:,:)
    real(r8), pointer :: mss_cnc_bcpho(:,:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_octot(:,:)
    real(r8), pointer :: mss_oc_col(:)
    real(r8), pointer :: mss_oc_top(:)
    real(r8), pointer :: mss_cnc_ocphi(:,:)
    real(r8), pointer :: mss_cnc_ocpho(:,:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: mss_dsttot(:,:)
    real(r8), pointer :: mss_dst_col(:)
    real(r8), pointer :: mss_dst_top(:)
    real(r8), pointer :: mss_cnc_dst1(:,:)
    real(r8), pointer :: mss_cnc_dst2(:,:)
    real(r8), pointer :: mss_cnc_dst3(:,:)
    real(r8), pointer :: mss_cnc_dst4(:,:)
  integer :: j,l,c,p
  integer :: begp, endp
  integer :: begc, endc
  integer :: begl, endl
  integer :: begg, endg
  real(r8):: vwc,psi
  ltype => clm3%g%l%itype
  lakpoi => clm3%g%l%lakpoi
  clandunit => clm3%g%l%c%landunit
  snl => clm3%g%l%c%cps%snl
  dz => clm3%g%l%c%cps%dz
  zi => clm3%g%l%c%cps%zi
  z => clm3%g%l%c%cps%z
  watsat => clm3%g%l%c%cps%watsat
  h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
  h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
  h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
  h2ocan_col => clm3%g%l%c%cws%pws_a%h2ocan
  snowdp => clm3%g%l%c%cps%snowdp
   h2osno => clm3%g%l%c%cws%h2osno
  t_soisno => clm3%g%l%c%ces%t_soisno
  t_lake => clm3%g%l%c%ces%t_lake
  t_grnd => clm3%g%l%c%ces%t_grnd
    bsw2 => clm3%g%l%c%cps%bsw2
    vwcsat => clm3%g%l%c%cps%vwcsat
    psisat => clm3%g%l%c%cps%psisat
    soilpsi => clm3%g%l%c%cps%soilpsi
    wa => clm3%g%l%c%cws%wa
    wt => clm3%g%l%c%cws%wt
    zwt => clm3%g%l%c%cws%zwt
    h2ocan_loss => clm3%g%l%c%cwf%h2ocan_loss
    snw_rds => clm3%g%l%c%cps%snw_rds
    snw_rds_top => clm3%g%l%c%cps%snw_rds_top
    sno_liq_top => clm3%g%l%c%cps%sno_liq_top
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bctot => clm3%g%l%c%cps%mss_bctot
    mss_bc_col => clm3%g%l%c%cps%mss_bc_col
    mss_bc_top => clm3%g%l%c%cps%mss_bc_top
    mss_cnc_bcphi => clm3%g%l%c%cps%mss_cnc_bcphi
    mss_cnc_bcpho => clm3%g%l%c%cps%mss_cnc_bcpho
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_octot => clm3%g%l%c%cps%mss_octot
    mss_oc_col => clm3%g%l%c%cps%mss_oc_col
    mss_oc_top => clm3%g%l%c%cps%mss_oc_top
    mss_cnc_ocphi => clm3%g%l%c%cps%mss_cnc_ocphi
    mss_cnc_ocpho => clm3%g%l%c%cps%mss_cnc_ocpho
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    mss_dsttot => clm3%g%l%c%cps%mss_dsttot
    mss_dst_col => clm3%g%l%c%cps%mss_dst_col
    mss_dst_top => clm3%g%l%c%cps%mss_dst_top
    mss_cnc_dst1 => clm3%g%l%c%cps%mss_cnc_dst1
    mss_cnc_dst2 => clm3%g%l%c%cps%mss_cnc_dst2
    mss_cnc_dst3 => clm3%g%l%c%cps%mss_cnc_dst3
    mss_cnc_dst4 => clm3%g%l%c%cps%mss_cnc_dst4
  t_ref2m => clm3%g%l%c%p%pes%t_ref2m
  pcolumn => clm3%g%l%c%p%column
  h2ocan_pft => clm3%g%l%c%p%pws%h2ocan
  t_veg => clm3%g%l%c%p%pes%t_veg
  eflx_lwrad_out => clm3%g%l%c%p%pef%eflx_lwrad_out
  call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
  do p = begp, endp
     h2ocan_pft(p) = h2ocanx(p)
     t_ref2m(p) = t_ref2mx(p)
  end do
!dir$ concurrent
  do c = begc,endc
     h2ocan_col(c) = h2ocan_colx(c)
     h2ocan_loss(c) = 0._r8
        h2osno(c) = h2osnox(c)
     snowdp(c) = snowdpx(c)
  end do
  do c = begc,endc
   dz(c,-nlevsno+1:0) = dzclmx(c,-nlevsno+1:0)
   z(c,-nlevsno+1:0) = zclmx(c,-nlevsno+1:0)
   zi(c,-nlevsno+0:0) = ziclmx(c,-nlevsno+0:0)
  end do
  do c = begc,endc
   snl(c) = snlx(c)
   dz(c,1:nlevgrnd) = dzclmx(c,1:nlevgrnd)
   z(c,1:nlevgrnd) = zclmx(c,1:nlevgrnd)
   zi(c,1:nlevgrnd) = ziclmx(c,1:nlevgrnd)
  end do
!dir$ concurrent
  do c = begc,endc
     t_soisno(c,-nlevsno+1:nlevgrnd) = t_soisnox(c,-nlevsno+1:nlevgrnd)
     t_lake(c,1:nlevlak) = t_lakex(c,1:nlevlak)
     t_grnd(c) = t_grndx(c)
  end do
!dir$ concurrent
  do p = begp, endp
     c = pcolumn(p)
     t_veg(p) = t_vegx(c)
     eflx_lwrad_out(p) = sb * (t_grnd(c))**4
  end do
  do c = begc,endc
      do j=1,nlevgrnd
          h2osoi_vol(c,j) = h2osoi_volx(c,j)
      end do
      do j=-nlevsno+1,nlevgrnd
          h2osoi_liq(c,j) = h2osoi_liqx(c,j)
          h2osoi_ice(c,j) = h2osoi_icex(c,j)
      end do
  end do
      call CLMDebug('initialize SNICAR')
    do c = begc,endc
       mss_bctot(c,:) = 0._r8
       mss_bcpho(c,:) = 0._r8
       mss_bcphi(c,:) = 0._r8
       mss_cnc_bcphi(c,:)=0._r8
       mss_cnc_bcpho(c,:)=0._r8
       mss_octot(c,:) = 0._r8
       mss_ocpho(c,:) = 0._r8
       mss_ocphi(c,:) = 0._r8
       mss_cnc_ocphi(c,:)=0._r8
       mss_cnc_ocpho(c,:)=0._r8
       mss_dst1(c,:) = 0._r8
       mss_dst2(c,:) = 0._r8
       mss_dst3(c,:) = 0._r8
       mss_dst4(c,:) = 0._r8
       mss_dsttot(c,:) = 0._r8
       mss_cnc_dst1(c,:)=0._r8
       mss_cnc_dst2(c,:)=0._r8
       mss_cnc_dst3(c,:)=0._r8
       mss_cnc_dst4(c,:)=0._r8
     snw_rds(c,-nlevsno+1:0) = snw_rdsx(c,-nlevsno+1:0)
    enddo
        call CLMDebug('mark1')
    wa(begc:endc) = 5000._r8
    wt(begc:endc) = 5000._r8
    zwt(begc:endc) = 0._r8
  do c = begc,endc
        l = clandunit(c)
        if (.not. lakpoi(l)) then
          wa(c) = 4800._r8
          wt(c) = wa(c)
          zwt(c) = (25._r8 + zi(c,nlevsoi)) - wa(c)/0.2_r8 /1000._r8
       end if
     do j = 1,nlevgrnd
        l = clandunit(c)
        if (.not. lakpoi(l)) then
           if(h2osoi_vol(c,j) > watsat(c,j)) then
              h2osoi_vol(c,j) = watsat(c,j)
              if(h2osoi_liq(c,j)/(dz(c,j)*denh2o)+ &
                 h2osoi_ice(c,j)/(dz(c,j)*denice)> &
                 h2osoi_vol(c,j) ) then
                 if(t_soisno(c,j) > tfrz) then
                    h2osoi_liq(c,j) = dz(c,j)*denh2o*watsat(c,j)
                    h2osoi_ice(c,j) = 0.0
                 else
                    h2osoi_liq(c,j) = 0.0
                    h2osoi_ice(c,j) = dz(c,j)*denice*watsat(c,j)
                 end if
              end if
           endif
        end if
     end do
  end do
   call CLMDebug('done mkarbinit')
end subroutine mkarbinit
module aerdepMOD
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon , only : secspday,set_caerdep_from_file, set_dustdep_from_file
  use decompMod , only : get_proc_bounds
  use module_cam_support, only: endrun
  implicit none
  private
  public :: interpMonthlyAerdep
  public :: aerdepini
  private :: readMonthlyAerdep
  real(r8), save, private, allocatable :: bcphiwet2t(:,:)
  real(r8), save, private, allocatable :: bcphidry2t(:,:)
  real(r8), save, private, allocatable :: bcphodry2t(:,:)
  real(r8), save, private, allocatable :: ocphiwet2t(:,:)
  real(r8), save, private, allocatable :: ocphidry2t(:,:)
  real(r8), save, private, allocatable :: ocphodry2t(:,:)
  real(r8), save, private, allocatable :: dstx01wd2t(:,:)
  real(r8), save, private, allocatable :: dstx01dd2t(:,:)
  real(r8), save, private, allocatable :: dstx02wd2t(:,:)
  real(r8), save, private, allocatable :: dstx02dd2t(:,:)
  real(r8), save, private, allocatable :: dstx03wd2t(:,:)
  real(r8), save, private, allocatable :: dstx03dd2t(:,:)
  real(r8), save, private, allocatable :: dstx04wd2t(:,:)
  real(r8), save, private, allocatable :: dstx04dd2t(:,:)
  integer,parameter :: nt =12
  real(r8) :: time(12)
  real(r8),parameter :: daysPerYear = 365.0_r8
  integer,parameter :: debug = 1
contains
  subroutine aerdepini()
    use nanMod , only : nan
    implicit none
    integer :: ier
    integer :: begg,endg
    character(256) :: locfn
    integer :: ncid,dimid,varid
    integer :: n
    integer :: m1,m2
    integer, parameter :: ndaypm(12) = &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    character(*),parameter :: subName = '(aerdepini) '
    character(*),parameter :: F00 = "('(aerdepini) ',4a)"
    character(*),parameter :: F01 = "('(aerdepini) ',a,4f13.3)"
    call get_proc_bounds(begg=begg,endg=endg)
    if ( set_caerdep_from_file )then
       ier = 0
       if(.not.allocated(bcphiwet2t)) then
          allocate(bcphiwet2t(begg:endg,2))
          allocate(bcphidry2t(begg:endg,2))
          allocate(bcphodry2t(begg:endg,2))
          allocate(ocphiwet2t(begg:endg,2))
          allocate(ocphidry2t(begg:endg,2))
          allocate(ocphodry2t(begg:endg,2))
       endif
       if (ier /= 0) then
          write(6,*) 'aerdepini allocation error'
          call endrun()
       end if
        bcphiwet2t(begg:endg,1:2) = nan
        bcphidry2t(begg:endg,1:2) = nan
        bcphodry2t(begg:endg,1:2) = nan
        ocphiwet2t(begg:endg,1:2) = nan
        ocphidry2t(begg:endg,1:2) = nan
        ocphodry2t(begg:endg,1:2) = nan
    end if
    if ( set_dustdep_from_file )then
       allocate(dstx01wd2t(begg:endg,2))
       allocate(dstx01dd2t(begg:endg,2))
       allocate(dstx02wd2t(begg:endg,2))
       allocate(dstx02dd2t(begg:endg,2))
       allocate(dstx03wd2t(begg:endg,2))
       allocate(dstx03dd2t(begg:endg,2))
       allocate(dstx04wd2t(begg:endg,2))
       allocate(dstx04dd2t(begg:endg,2))
       if (ier /= 0) then
         write(6,*) 'aerdepini allocation error'
         call endrun()
       end if
       dstx01wd2t(begg:endg,1:2) = nan
       dstx01dd2t(begg:endg,1:2) = nan
       dstx02wd2t(begg:endg,1:2) = nan
       dstx02dd2t(begg:endg,1:2) = nan
       dstx03wd2t(begg:endg,1:2) = nan
       dstx03dd2t(begg:endg,1:2) = nan
       dstx04wd2t(begg:endg,1:2) = nan
       dstx04dd2t(begg:endg,1:2) = nan
    end if
      n = 365
      time( 1) = n + float(ndaypm( 1))/2.0_r8 ; n = n + ndaypm( 1)
      time( 2) = n + float(ndaypm( 2))/2.0_r8 ; n = n + ndaypm( 2)
      time( 3) = n + float(ndaypm( 3))/2.0_r8 ; n = n + ndaypm( 3)
      time( 4) = n + float(ndaypm( 4))/2.0_r8 ; n = n + ndaypm( 4)
      time( 5) = n + float(ndaypm( 5))/2.0_r8 ; n = n + ndaypm( 5)
      time( 6) = n + float(ndaypm( 6))/2.0_r8 ; n = n + ndaypm( 6)
      time( 7) = n + float(ndaypm( 7))/2.0_r8 ; n = n + ndaypm( 7)
      time( 8) = n + float(ndaypm( 8))/2.0_r8 ; n = n + ndaypm( 8)
      time( 9) = n + float(ndaypm( 9))/2.0_r8 ; n = n + ndaypm( 9)
      time(10) = n + float(ndaypm(10))/2.0_r8 ; n = n + ndaypm(10)
      time(11) = n + float(ndaypm(11))/2.0_r8 ; n = n + ndaypm(11)
      time(12) = n + float(ndaypm(12))/2.0_r8
  end subroutine aerdepini
  subroutine interpMonthlyAerdep (kmo, kda)
    use clmtype
    use globals , only : dtime
    implicit none
    real(r8), pointer :: forc_aer(:,:)
    real(r8):: timwt_aer(2)
    integer :: kmo
    integer :: kda
    integer :: g
    integer :: begg,endg
    integer :: n
    integer :: edays
    real(r8) :: t
    integer ,save :: nLB = 0
    integer ,save :: nUB = 1
    real(r8),save :: tLB =-1.0
    real(r8),save :: tUB =-2.0
    real(r8) :: fUB,fLB
    logical ,save :: firstCallA = .true.
    logical ,save :: firstCallB = .true.
    logical ,save :: firstCallC = .true.
    character(1) :: case
    logical :: readNewData
    integer, parameter :: ndaypm(12) = &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    character(*),parameter :: subName = '(interpMonthlyAerdep) '
    character(*),parameter :: F00 = "('(interpMonthlyAerdep) ',4a)"
    character(*),parameter :: F01 = "('(interpMonthlyAerdep) ',a,i4.4,2('-',i2.2),3f11.2,2i6,2x,2f6.3)"
    character(*),parameter :: F02 = "('(interpMonthlyAerdep) ',a,i4.4,2('-',i2.2),i7,'s ',f12.3)"
    call get_proc_bounds(begg=begg,endg=endg)
    forc_aer => clm_a2l%forc_aer
    t = (kda-0.5) / ndaypm(kmo)
   CASE = "B"
   if (t < time( 1) ) CASE = "A"
   if (t > time(nt) ) CASE = "C"
   if ( case == "A" ) then
      if ( firstCallA ) then
         nLB = 0 ; tLB = -2.0
         nUB = 1 ; tUB = -1.0
         firstCallA = .false.
      end if
      t = mod(t,daysPerYear) + daysPerYear
      n = 0
      readNewData = .false.
      do while (t < tLB .or. tUB < t)
         readNewData = .true.
         nLB = nLB + 1 ; if (nLB > 12) nLB = 1
         nUB = nLB + 1 ; if (nUB > 12) nUB = 1
         tLB = mod(time(nLB),daysPerYear) + daysPerYear
         tUB = mod(time(nUB),daysPerYear) + daysPerYear
         if (nLB == 12) then
            if (tLB <= t ) then
               tUB = tUB + daysPerYear
            else if (t < tUB ) then
               tLB = tLB - daysPerYear
            else
               write(6,*) "ERROR: in case A aerinterp"
               call endrun()
            end if
         end if
         n = n + 1
         if (n > 12) then
             write(6,F01) "ERROR: date,tLB,t,tUB = ",kmo,kda,tLB,t,tUB
             call endrun()
         end if
      end do
   else if ( case == "C" ) then
      if ( firstCallC ) then
         nLB = nt-12 ; tLB = -2.0
         nUB = nt-11 ; tUB = -1.0
         firstCallC = .false.
      end if
      t = mod(t,daysPerYear) + daysPerYear
      n = 0
      readNewData = .false.
      do while (t < tLB .or. tUB < t)
         readNewData = .true.
         nLB = nLB + 1 ; if (nLB > nt) nLB = nt - 11
         nUB = nLB + 1 ; if (nUB > nt) nUB = nt - 11
         tLB = mod(time(nLB),daysPerYear) + daysPerYear
         tUB = mod(time(nUB),daysPerYear) + daysPerYear
         if (nLB == nt) then
            if (tLB <= t ) then
               tUB = tUB + daysPerYear
               else if (t < tUB ) then
               tLB = tLB - daysPerYear
            else
               write(6,*) "ERROR: in case A second aerinterp"
               call endrun()
            end if
         end if
         n = n + 1
         if (n > 12) then
             write(6,F01) "ERROR: date,tLB,t,tUB = ",kmo,kda,tLB,t,tUB
             call endrun()
         end if
      end do
   else
      if ( firstCallB ) then
         nLB = 0 ; tLB = -2.0
         nUB = 1 ; tUB = -1.0
         firstCallB = .false.
      end if
      readNewData = .false.
      do while (tUB < t)
         readNewData = .true.
         nLB = nLB + 1
         nUB = nLB + 1
         tLB = time(nLB)
         tUB = time(nUB)
         if (nUB > nt) then
          write(6,*) "ERROR: nt < nUB aerinterp"
          call endrun()
         end if
      end do
   end if
      call readMonthlyAerdep (kmo,kda)
   fLB = (tUB - t)/(tUB - tLB)
   fUB = 1.0_r8 - fLB
    do g = begg, endg
       if ( set_caerdep_from_file )then
          forc_aer(g, 1) = fLB*bcphidry2t(g,1) + fUB*bcphidry2t(g,2)
          forc_aer(g, 2) = fLB*bcphodry2t(g,1) + fUB*bcphodry2t(g,2)
          forc_aer(g, 3) = fLB*bcphiwet2t(g,1) + fUB*bcphiwet2t(g,2)
          forc_aer(g, 4) = fLB*ocphidry2t(g,1) + fUB*ocphidry2t(g,2)
          forc_aer(g, 5) = fLB*ocphodry2t(g,1) + fUB*ocphodry2t(g,2)
          forc_aer(g, 6) = fLB*ocphiwet2t(g,1) + fUB*ocphiwet2t(g,2)
       end if
       if ( set_dustdep_from_file )then
          forc_aer(g, 7) = fLB*dstx01wd2t(g,1) + fUB*dstx01wd2t(g,2)
          forc_aer(g, 8) = fLB*dstx01dd2t(g,1) + fUB*dstx01dd2t(g,2)
          forc_aer(g, 9) = fLB*dstx02wd2t(g,1) + fUB*dstx02wd2t(g,2)
          forc_aer(g,10) = fLB*dstx02dd2t(g,1) + fUB*dstx02dd2t(g,2)
          forc_aer(g,11) = fLB*dstx03wd2t(g,1) + fUB*dstx03wd2t(g,2)
          forc_aer(g,12) = fLB*dstx03dd2t(g,1) + fUB*dstx03dd2t(g,2)
          forc_aer(g,13) = fLB*dstx04wd2t(g,1) + fUB*dstx04wd2t(g,2)
          forc_aer(g,14) = fLB*dstx04dd2t(g,1) + fUB*dstx04dd2t(g,2)
       end if
    enddo
  call aerdealloc()
  end subroutine interpMonthlyAerdep
  subroutine readMonthlyAerdep(kmo, kda)
   use clm_varcon , only :bcphidry,bcphodry,bcphiwet,ocphidry,ocphodry,ocphiwet,dstx01wd,dstx01dd,dstx02wd,&
                          dstx02dd,dstx03wd,dstx03dd,dstx04wd,dstx04dd
    implicit none
    integer, intent(in) :: kmo
    integer, intent(in) :: kda
    integer :: k,g
    integer :: begg
    integer :: endg
    integer :: begl
    integer :: endl
    integer :: begc
    integer :: endc
    integer :: begp
    integer :: endp
    real(r8):: t
    integer :: it(2)
    integer :: months(2)
    integer, dimension(12) :: ndaypm= &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    t = (kda-0.5) / ndaypm(kmo)
    it(1) = t + 0.5
    it(2) = it(1) + 1
    months(1) = kmo + it(1) - 1
    months(2) = kmo + it(2) - 1
    if (months(1) < 1) months(1) = 12
    if (months(2) > 12) months(2) = 1
       call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
  do g = begg,endg
    do k=1,2
      bcphidry2t(g,k) = bcphidry(months(k))
      bcphodry2t(g,k) = bcphodry(months(k))
      bcphiwet2t(g,k) = bcphiwet(months(k))
      ocphidry2t(g,k) = ocphidry(months(k))
      ocphodry2t(g,k) = ocphodry(months(k))
      ocphiwet2t(g,k) = ocphiwet(months(k))
      dstx01wd2t(g,k) = dstx01wd(months(k))
      dstx01dd2t(g,k) = dstx01dd(months(k))
      dstx02wd2t(g,k) = dstx02wd(months(k))
      dstx02dd2t(g,k) = dstx02dd(months(k))
      dstx03wd2t(g,k) = dstx03wd(months(k))
      dstx03dd2t(g,k) = dstx03dd(months(k))
      dstx04wd2t(g,k) = dstx04wd(months(k))
      dstx04dd2t(g,k) = dstx04dd(months(k))
    end do
  end do
  end subroutine readMonthlyAerdep
  subroutine aerdealloc
  implicit none
     if(allocated(bcphidry2t)) deallocate(bcphidry2t)
     if(allocated(bcphodry2t)) deallocate(bcphodry2t)
     if(allocated(bcphiwet2t)) deallocate(bcphiwet2t)
     if(allocated(ocphidry2t)) deallocate(ocphidry2t)
     if(allocated(ocphodry2t)) deallocate(ocphodry2t)
     if(allocated(ocphiwet2t)) deallocate(ocphiwet2t)
     if(allocated(dstx01wd2t)) deallocate(dstx01wd2t)
     if(allocated(dstx01dd2t)) deallocate(dstx01dd2t)
     if(allocated(dstx02wd2t)) deallocate(dstx02wd2t)
     if(allocated(dstx02dd2t)) deallocate(dstx02dd2t)
     if(allocated(dstx03wd2t)) deallocate(dstx03wd2t)
     if(allocated(dstx03dd2t)) deallocate(dstx03dd2t)
     if(allocated(dstx04wd2t)) deallocate(dstx04wd2t)
     if(allocated(dstx04dd2t)) deallocate(dstx04dd2t)
  end subroutine aerdealloc
end module aerdepMod
module accumulMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar , only: maxpatch
  use module_cam_support, only: endrun
  implicit none
  save
  public :: init_accum_field
  public :: accum_dealloc
  public :: extract_accum_field
  interface extract_accum_field
     module procedure extract_accum_field_sl
     module procedure extract_accum_field_ml
  end interface
  public :: update_accum_field
  interface update_accum_field
     module procedure update_accum_field_sl
     module procedure update_accum_field_ml
  end interface
  private
  type accum_field
     character(len= 8) :: name
     character(len=128) :: desc
     character(len= 8) :: units
     character(len= 8) :: acctype
     character(len= 8) :: type1d
     character(len= 8) :: type2d
     integer :: beg1d
     integer :: end1d
     integer :: num1d
     integer :: numlev
     real(r8):: initval
     real(r8), pointer :: val(:,:)
     integer :: period
  end type accum_field
  integer, parameter :: max_accum = 100
  type (accum_field) :: accum(max_accum)
  integer :: naccflds = 0
contains
  subroutine init_accum_field (name, units, desc, &
       accum_type, accum_period, numlev, subgrid_type, init_value,type2d)
    use clm_varcon, only : cday
    use globals, only : dtime
    use decompMod, only : get_proc_bounds
    implicit none
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: desc
    character(len=*), intent(in) :: accum_type
    integer , intent(in) :: accum_period
    character(len=*), intent(in) :: subgrid_type
    integer , intent(in) :: numlev
    real(r8), intent(in) :: init_value
    character(len=*), intent(in), optional :: type2d
    integer :: nf
    integer :: beg1d,end1d
    integer :: num1d
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    if (naccflds > max_accum) then
       write (6,*) 'INIT_ACCUM_FIELD error: user-defined accumulation fields ', &
            'equal to ',naccflds,' exceeds max_accum'
       call endrun
    end if
    nf = naccflds
    accum(nf)%name = trim(name)
    accum(nf)%units = trim(units)
    accum(nf)%desc = trim(desc)
    accum(nf)%acctype = trim(accum_type)
    accum(nf)%initval = init_value
    accum(nf)%period = accum_period
    if (accum(nf)%period < 0) then
       accum(nf)%period = -accum(nf)%period * nint(cday) / dtime
    end if
    select case (trim(subgrid_type))
    case ('gridcell')
       beg1d = begg
       end1d = endg
       num1d = endg - begg + 1
    case ('landunit')
       beg1d = begl
       end1d = endl
       num1d = endl - begl + 1
    case ('column')
       beg1d = begc
       end1d = endc
       num1d = endc - begc + 1
    case ('pft')
       beg1d = begp
       end1d = endp
       num1d = endp - begp + 1
    case default
       write(6,*)'INIT_ACCUM_FIELD: unknown subgrid type ',subgrid_type
       call endrun ()
    end select
    accum(nf)%type1d = trim(subgrid_type)
    accum(nf)%beg1d = beg1d
    accum(nf)%end1d = end1d
    accum(nf)%num1d = num1d
    accum(nf)%numlev = numlev
    if (present(type2d)) then
       accum(nf)%type2d = type2d
    else
       accum(nf)%type2d = ' '
    end if
    allocate(accum(nf)%val(beg1d:end1d,numlev))
    accum(nf)%val(beg1d:end1d,1) = init_value
  end subroutine init_accum_field
  subroutine extract_accum_field_sl (name, field, nstep)
    use clm_varcon, only : spval
    implicit none
    character(len=*), intent(in) :: name
    real(r8), pointer, dimension(:) :: field
    integer , intent(in) :: nstep
    integer :: i,k,nf
    integer :: beg,end
    nf = 0
!dir$ concurrent
    do i = 1, naccflds
       if (name == accum(i)%name) nf = i
    end do
    if (nf == 0) then
       write(6,*) 'EXTRACT_ACCUM_FIELD_SL error: field name ',name,' not found'
       call endrun
    endif
    beg = accum(nf)%beg1d
    end = accum(nf)%end1d
    if (size(field,dim=1) /= end-beg+1) then
       write(6,*)'ERROR in extract_accum_field for field ',accum(nf)%name
       write(6,*)'size of first dimension of field is ',&
            size(field,dim=1),' and should be ',end-beg+1
       call endrun
    endif
    if (accum(nf)%acctype == 'timeavg' .and. &
         mod(nstep,accum(nf)%period) /= 0) then
!dir$ concurrent
       do k = beg,end
          field(k) = spval
       end do
    else
!dir$ concurrent
       do k = beg,end
          field(k) = accum(nf)%val(k,1)
       end do
    end if
  end subroutine extract_accum_field_sl
  subroutine extract_accum_field_ml (name, field, nstep)
    use clm_varcon, only : spval
    implicit none
    character(len=*), intent(in) :: name
    real(r8), pointer, dimension(:,:) :: field
    integer, intent(in) :: nstep
    integer :: i,j,k,nf
    integer :: beg,end
    integer :: numlev
    nf = 0
    do i = 1, naccflds
       if (name == accum(i)%name) nf = i
    end do
    if (nf == 0) then
       write(6,*) 'EXTRACT_ACCUM_FIELD_ML error: field name ',name,' not found'
       call endrun
    endif
    numlev = accum(nf)%numlev
    beg = accum(nf)%beg1d
    end = accum(nf)%end1d
    if (size(field,dim=1) /= end-beg+1) then
       write(6,*)'ERROR in extract_accum_field for field ',accum(nf)%name
       write(6,*)'size of first dimension of field is ',&
            size(field,dim=1),' and should be ',end-beg+1
       call endrun
    else if (size(field,dim=2) /= numlev) then
       write(6,*)'ERROR in extract_accum_field for field ',accum(nf)%name
       write(6,*)'size of second dimension of field iis ',&
            size(field,dim=2),' and should be ',numlev
       call endrun
    endif
    if (accum(nf)%acctype == 'timeavg' .and. &
         mod(nstep,accum(nf)%period) /= 0) then
       do j = 1,numlev
!dir$ concurrent
          do k = beg,end
             field(k,j) = spval
          end do
       end do
    else
       do j = 1,numlev
!dir$ concurrent
          do k = beg,end
             field(k,j) = accum(nf)%val(k,j)
          end do
       end do
    end if
  end subroutine extract_accum_field_ml
  subroutine update_accum_field_sl (name, field, nstep)
    implicit none
    character(len=*), intent(in) :: name
    real(r8), pointer, dimension(:) :: field
    integer , intent(in) :: nstep
    integer :: i,k,nf
    integer :: accper
    integer :: beg,end
    nf = 0
    do i = 1, naccflds
       if (name == accum(i)%name) nf = i
    end do
    if (nf == 0) then
       write(6,*) 'UPDATE_ACCUM_FIELD_SL error: field name ',name,' not found'
       call endrun
    endif
    beg = accum(nf)%beg1d
    end = accum(nf)%end1d
    if (size(field,dim=1) /= end-beg+1) then
       write(6,*)'ERROR in UPDATE_ACCUM_FIELD_SL for field ',accum(nf)%name
       write(6,*)'size of first dimension of field is ',size(field,dim=1),&
            ' and should be ',end-beg+1
       call endrun
    endif
    if (accum(nf)%acctype /= 'timeavg' .AND. &
        accum(nf)%acctype /= 'runmean' .AND. &
        accum(nf)%acctype /= 'runaccum') then
       write(6,*) 'UPDATE_ACCUM_FIELD_SL error: incorrect accumulation type'
       write(6,*) ' was specified for field ',name
       write(6,*)' accumulation type specified is ',accum(nf)%acctype
       write(6,*)' only [timeavg, runmean, runaccum] are currently acceptable'
       call endrun()
    end if
    if (accum(nf)%acctype == 'timeavg') then
       if ((mod(nstep,accum(nf)%period) == 1) .and. (nstep /= 0)) then
          accum(nf)%val(beg:end,1) = 0._r8
       end if
       accum(nf)%val(beg:end,1) = accum(nf)%val(beg:end,1) + field(beg:end)
       if (mod(nstep,accum(nf)%period) == 0) then
          accum(nf)%val(beg:end,1) = accum(nf)%val(beg:end,1) / accum(nf)%period
       endif
    else if (accum(nf)%acctype == 'runmean') then
       accper = min (nstep,accum(nf)%period)
       accum(nf)%val(beg:end,1) = ((accper-1)*accum(nf)%val(beg:end,1) + field(beg:end)) / accper
    else if (accum(nf)%acctype == 'runaccum') then
!dir$ concurrent
       do k = beg,end
          if (nint(field(k)) == -99999) then
             accum(nf)%val(k,1) = 0._r8
          end if
       end do
       accum(nf)%val(beg:end,1) = min(max(accum(nf)%val(beg:end,1) + field(beg:end), 0._r8), 99999._r8)
    end if
  end subroutine update_accum_field_sl
  subroutine update_accum_field_ml (name, field, nstep)
    implicit none
    character(len=*), intent(in) :: name
    real(r8), pointer, dimension(:,:) :: field
    integer , intent(in) :: nstep
    integer :: i,j,k,nf
    integer :: accper
    integer :: beg,end
    integer :: numlev
    nf = 0
    do i = 1, naccflds
       if (name == accum(i)%name) nf = i
    end do
    if (nf == 0) then
       write(6,*) 'UPDATE_ACCUM_FIELD_ML error: field name ',name,' not found'
       call endrun
    endif
    numlev = accum(nf)%numlev
    beg = accum(nf)%beg1d
    end = accum(nf)%end1d
    if (size(field,dim=1) /= end-beg+1) then
       write(6,*)'ERROR in UPDATE_ACCUM_FIELD_ML for field ',accum(nf)%name
       write(6,*)'size of first dimension of field is ',size(field,dim=1),&
            ' and should be ',end-beg+1
       call endrun
    else if (size(field,dim=2) /= numlev) then
       write(6,*)'ERROR in UPDATE_ACCUM_FIELD_ML for field ',accum(nf)%name
       write(6,*)'size of second dimension of field is ',size(field,dim=2),&
            ' and should be ',numlev
       call endrun
    endif
    if (accum(nf)%acctype /= 'timeavg' .AND. &
        accum(nf)%acctype /= 'runmean' .AND. &
        accum(nf)%acctype /= 'runaccum') then
       write(6,*) 'UPDATE_ACCUM_FIELD_ML error: incorrect accumulation type'
       write(6,*) ' was specified for field ',name
       write(6,*)' accumulation type specified is ',accum(nf)%acctype
       write(6,*)' only [timeavg, runmean, runaccum] are currently acceptable'
       call endrun()
    end if
    if (accum(nf)%acctype == 'timeavg') then
       if ((mod(nstep,accum(nf)%period) == 1) .and. (nstep /= 0)) then
          accum(nf)%val(beg:end,1:numlev) = 0._r8
       endif
       accum(nf)%val(beg:end,1:numlev) = accum(nf)%val(beg:end,1:numlev) + field(beg:end,1:numlev)
       if (mod(nstep,accum(nf)%period) == 0) then
          accum(nf)%val(beg:end,1:numlev) = accum(nf)%val(beg:end,1:numlev) / accum(nf)%period
       endif
    else if (accum(nf)%acctype == 'runmean') then
       accper = min (nstep,accum(nf)%period)
       accum(nf)%val(beg:end,1:numlev) = &
            ((accper-1)*accum(nf)%val(beg:end,1:numlev) + field(beg:end,1:numlev)) / accper
    else if (accum(nf)%acctype == 'runaccum') then
       do j = 1,numlev
!dir$ concurrent
          do k = beg,end
             if (nint(field(k,j)) == -99999) then
                accum(nf)%val(k,j) = 0._r8
             end if
          end do
       end do
       accum(nf)%val(beg:end,1:numlev) = &
            min(max(accum(nf)%val(beg:end,1:numlev) + field(beg:end,1:numlev), 0._r8), 99999._r8)
    end if
  end subroutine update_accum_field_ml
  subroutine accum_dealloc
    implicit none
    integer :: i
    do i = 1,naccflds
      deallocate (accum(i)%val)
    end do
  end subroutine accum_dealloc
end module accumulMod
module accFldsMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use module_cam_support, only: endrun
  implicit none
  save
  public :: initAccFlds
  public :: initAccClmtype
  public :: updateAccFlds
contains
  subroutine initAccFlds()
    use decompMod , only : get_proc_bounds
    use accumulMod , only : init_accum_field
    use globals , only : dtime, nstep
    use clm_varcon , only : cday, tfrz
    use nanMod , only : bigint
    use clm_varpar , only : maxpatch
    use shr_const_mod, only : SHR_CONST_TKFRZ
    implicit none
    integer, parameter :: not_used = bigint
   call init_accum_field(name='TREFAV', units='K', &
         desc='average over an hour of 2-m temperature', &
         accum_type='timeavg', accum_period=nint(3600._r8/dtime), &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field(name='TREFAV_U', units='K', &
         desc='average over an hour of urban 2-m temperature', &
         accum_type='timeavg', accum_period=nint(3600._r8/dtime), &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field(name='TREFAV_R', units='K', &
         desc='average over an hour of rural 2-m temperature', &
         accum_type='timeavg', accum_period=nint(3600._r8/dtime), &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='T_VEG24', units='K', &
         desc='24hr average of vegetation temperature', &
         accum_type='runmean', accum_period=-1, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='T_VEG240', units='K', &
         desc='240hr average of vegetation temperature', &
         accum_type='runmean', accum_period=-10, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSD24', units='W/m2', &
         desc='24hr average of direct solar radiation', &
         accum_type='runmean', accum_period=-1, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSD240', units='W/m2', &
         desc='240hr average of direct solar radiation', &
         accum_type='runmean', accum_period=-10, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSI24', units='W/m2', &
         desc='24hr average of diffuse solar radiation', &
         accum_type='runmean', accum_period=-1, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSI240', units='W/m2', &
         desc='240hr average of diffuse solar radiation', &
         accum_type='runmean', accum_period=-10, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSUN24', units='fraction', &
         desc='24hr average of diffuse solar radiation', &
         accum_type='runmean', accum_period=-1, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='FSUN240', units='fraction', &
         desc='240hr average of diffuse solar radiation', &
         accum_type='runmean', accum_period=-10, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
    call init_accum_field (name='LAIP', units='m2/m2', &
         desc='leaf area index average over timestep', &
         accum_type='runmean', accum_period=1, &
         subgrid_type='pft', numlev=1, init_value=0._r8)
  end subroutine initAccFlds
  subroutine updateAccFlds()
    use clmtype
    use decompMod , only : get_proc_bounds
    use clm_varcon , only : spval
    use shr_const_mod, only : SHR_CONST_CDAY, SHR_CONST_TKFRZ
    use pftvarcon , only : ndllf_dcd_brl_tree
    use globals , only : dtime, nstep, secs,day,dayp1,month
    use accumulMod , only : update_accum_field, extract_accum_field
    implicit none
    integer , pointer :: itype(:)
    integer , pointer :: pgridcell(:)
    real(r8), pointer :: forc_t(:)
    real(r8), pointer :: forc_rain(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: t_ref2m(:)
    real(r8), pointer :: t_ref2m_u(:)
    real(r8), pointer :: t_ref2m_r(:)
    logical , pointer :: urbpoi(:)
   logical , pointer :: ifspecial(:)
    integer , pointer :: plandunit(:)
    real(r8), pointer :: t_veg(:)
    real(r8), pointer :: forc_solad(:,:)
    real(r8), pointer :: forc_solai(:,:)
    real(r8), pointer :: fsun(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: t_veg24(:)
    real(r8), pointer :: t_veg240(:)
    real(r8), pointer :: fsd24(:)
    real(r8), pointer :: fsd240(:)
    real(r8), pointer :: fsi24(:)
    real(r8), pointer :: fsi240(:)
    real(r8), pointer :: fsun24(:)
    real(r8), pointer :: fsun240(:)
    real(r8), pointer :: elai_p(:)
    real(r8), pointer :: t_ref2m_min(:)
    real(r8), pointer :: t_ref2m_max(:)
    real(r8), pointer :: t_ref2m_min_inst(:)
    real(r8), pointer :: t_ref2m_max_inst(:)
    real(r8), pointer :: t_ref2m_min_u(:)
    real(r8), pointer :: t_ref2m_min_r(:)
    real(r8), pointer :: t_ref2m_max_u(:)
    real(r8), pointer :: t_ref2m_max_r(:)
    real(r8), pointer :: t_ref2m_min_inst_u(:)
    real(r8), pointer :: t_ref2m_min_inst_r(:)
    real(r8), pointer :: t_ref2m_max_inst_u(:)
    real(r8), pointer :: t_ref2m_max_inst_r(:)
    integer :: g,l,c,p
    integer :: itypveg
    logical :: end_cd
    integer :: ier
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    real(r8), pointer :: rbufslp(:)
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    forc_t => clm_a2l%forc_t
    forc_rain => clm_a2l%forc_rain
    forc_snow => clm_a2l%forc_snow
    forc_solad => clm_a2l%forc_solad
    forc_solai => clm_a2l%forc_solai
    ifspecial => clm3%g%l%ifspecial
    urbpoi => clm3%g%l%urbpoi
    itype => clm3%g%l%c%p%itype
    pgridcell => clm3%g%l%c%p%gridcell
    t_ref2m => clm3%g%l%c%p%pes%t_ref2m
    t_ref2m_max_inst => clm3%g%l%c%p%pes%t_ref2m_max_inst
    t_ref2m_min_inst => clm3%g%l%c%p%pes%t_ref2m_min_inst
    t_ref2m_max => clm3%g%l%c%p%pes%t_ref2m_max
    t_ref2m_min => clm3%g%l%c%p%pes%t_ref2m_min
    t_ref2m_u => clm3%g%l%c%p%pes%t_ref2m_u
    t_ref2m_r => clm3%g%l%c%p%pes%t_ref2m_r
    t_ref2m_max_u => clm3%g%l%c%p%pes%t_ref2m_max_u
    t_ref2m_max_r => clm3%g%l%c%p%pes%t_ref2m_max_r
    t_ref2m_min_u => clm3%g%l%c%p%pes%t_ref2m_min_u
    t_ref2m_min_r => clm3%g%l%c%p%pes%t_ref2m_min_r
    t_ref2m_max_inst_u => clm3%g%l%c%p%pes%t_ref2m_max_inst_u
    t_ref2m_max_inst_r => clm3%g%l%c%p%pes%t_ref2m_max_inst_r
    t_ref2m_min_inst_u => clm3%g%l%c%p%pes%t_ref2m_min_inst_u
    t_ref2m_min_inst_r => clm3%g%l%c%p%pes%t_ref2m_min_inst_r
    plandunit => clm3%g%l%c%p%landunit
    t_veg24 => clm3%g%l%c%p%pvs%t_veg24
    t_veg240 => clm3%g%l%c%p%pvs%t_veg240
    fsd24 => clm3%g%l%c%p%pvs%fsd24
    fsd240 => clm3%g%l%c%p%pvs%fsd240
    fsi24 => clm3%g%l%c%p%pvs%fsi24
    fsi240 => clm3%g%l%c%p%pvs%fsi240
    fsun24 => clm3%g%l%c%p%pvs%fsun24
    fsun240 => clm3%g%l%c%p%pvs%fsun240
    elai_p => clm3%g%l%c%p%pvs%elai_p
    t_veg => clm3%g%l%c%p%pes%t_veg
    fsun => clm3%g%l%c%p%pps%fsun
    elai => clm3%g%l%c%p%pps%elai
    if (nstep == 0) return
    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(6,*)'update_accum_hist allocation error for rbuf1dp'
       call endrun
    endif
    call update_accum_field ('TREFAV', t_ref2m, nstep)
    call extract_accum_field ('TREFAV', rbufslp, nstep)
    if(dayp1-day.eq.1) then
      end_cd = .true.
    else
      end_cd = .false.
    end if
!dir$ concurrent
    do p = begp,endp
       if (rbufslp(p) /= spval) then
          t_ref2m_max_inst(p) = max(rbufslp(p), t_ref2m_max_inst(p))
          t_ref2m_min_inst(p) = min(rbufslp(p), t_ref2m_min_inst(p))
       endif
       if (end_cd) then
          t_ref2m_max(p) = t_ref2m_max_inst(p)
          t_ref2m_min(p) = t_ref2m_min_inst(p)
          t_ref2m_max_inst(p) = -spval
          t_ref2m_min_inst(p) = spval
       else if (secs == int(dtime)) then
          t_ref2m_max(p) = spval
          t_ref2m_min(p) = spval
       endif
    end do
    call update_accum_field ('TREFAV_U', t_ref2m_u, nstep)
    call extract_accum_field ('TREFAV_U', rbufslp, nstep)
    do p = begp,endp
       l = plandunit(p)
       if (rbufslp(p) /= spval) then
          t_ref2m_max_inst_u(p) = max(rbufslp(p), t_ref2m_max_inst_u(p))
          t_ref2m_min_inst_u(p) = min(rbufslp(p), t_ref2m_min_inst_u(p))
       endif
       if (end_cd) then
         if (urbpoi(l)) then
          t_ref2m_max_u(p) = t_ref2m_max_inst_u(p)
          t_ref2m_min_u(p) = t_ref2m_min_inst_u(p)
          t_ref2m_max_inst_u(p) = -spval
          t_ref2m_min_inst_u(p) = spval
         end if
       else if (secs == int(dtime)) then
          t_ref2m_max_u(p) = spval
          t_ref2m_min_u(p) = spval
       endif
    end do
    call update_accum_field ('TREFAV_R', t_ref2m_r, nstep)
    call extract_accum_field ('TREFAV_R', rbufslp, nstep)
    do p = begp,endp
       l = plandunit(p)
       if (rbufslp(p) /= spval) then
          t_ref2m_max_inst_r(p) = max(rbufslp(p), t_ref2m_max_inst_r(p))
          t_ref2m_min_inst_r(p) = min(rbufslp(p), t_ref2m_min_inst_r(p))
       endif
       if (end_cd) then
         if (.not.(ifspecial(l))) then
          t_ref2m_max_r(p) = t_ref2m_max_inst_r(p)
          t_ref2m_min_r(p) = t_ref2m_min_inst_r(p)
          t_ref2m_max_inst_r(p) = -spval
          t_ref2m_min_inst_r(p) = spval
        end if
       else if (secs == int(dtime)) then
          t_ref2m_max_r(p) = spval
          t_ref2m_min_r(p) = spval
       endif
    end do
    do p = begp,endp
       rbufslp(p) = t_veg(p)
    end do
    call update_accum_field ('T_VEG24', rbufslp, nstep)
    call extract_accum_field ('T_VEG24', t_veg24, nstep)
    call update_accum_field ('T_VEG240', rbufslp, nstep)
    call extract_accum_field ('T_VEG240', t_veg240, nstep)
    do p = begp,endp
       g = pgridcell(p)
       rbufslp(p) = forc_solad(g,1)
    end do
    call update_accum_field ('FSD240', rbufslp, nstep)
    call extract_accum_field ('FSD240', fsd240, nstep)
    call update_accum_field ('FSD24', rbufslp, nstep)
    call extract_accum_field ('FSD24', fsd24, nstep)
    do p = begp,endp
       g = pgridcell(p)
       rbufslp(p) = forc_solai(g,1)
    end do
    call update_accum_field ('FSI24', rbufslp, nstep)
    call extract_accum_field ('FSI24', fsi24, nstep)
    call update_accum_field ('FSI240', rbufslp, nstep)
    call extract_accum_field ('FSI240', fsi240, nstep)
    do p = begp,endp
       rbufslp(p) = fsun(p)
    end do
    call update_accum_field ('FSUN24', rbufslp, nstep)
    call extract_accum_field ('FSUN24', fsun24, nstep)
    call update_accum_field ('FSUN240', rbufslp, nstep)
    call extract_accum_field ('FSUN240', fsun240, nstep)
    do p = begp,endp
       rbufslp(p) = elai(p)
    end do
    call update_accum_field ('LAIP', rbufslp, nstep)
    call extract_accum_field ('LAIP', elai_p, nstep)
    deallocate(rbufslp)
  end subroutine updateAccFlds
  subroutine initAccClmtype
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use decompMod , only : get_proc_bounds
    use accumulMod , only : extract_accum_field
    use clm_varcon , only : spval
    use globals , only : nstep
    implicit none
    real(r8), pointer :: t_ref2m_min(:)
    real(r8), pointer :: t_ref2m_max(:)
    real(r8), pointer :: t_ref2m_min_inst(:)
    real(r8), pointer :: t_ref2m_max_inst(:)
    real(r8), pointer :: t_ref2m_min_u(:)
    real(r8), pointer :: t_ref2m_min_r(:)
    real(r8), pointer :: t_ref2m_max_u(:)
    real(r8), pointer :: t_ref2m_max_r(:)
    real(r8), pointer :: t_ref2m_min_inst_u(:)
    real(r8), pointer :: t_ref2m_min_inst_r(:)
    real(r8), pointer :: t_ref2m_max_inst_u(:)
    real(r8), pointer :: t_ref2m_max_inst_r(:)
    real(r8), pointer :: t_veg24(:)
    real(r8), pointer :: t_veg240(:)
    real(r8), pointer :: fsd24(:)
    real(r8), pointer :: fsd240(:)
    real(r8), pointer :: fsi24(:)
    real(r8), pointer :: fsi240(:)
    real(r8), pointer :: fsun24(:)
    real(r8), pointer :: fsun240(:)
    real(r8), pointer :: elai_p(:)
    integer :: p
    integer :: ier
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    real(r8), pointer :: rbufslp(:)
    character(len=32) :: subname = 'initAccClmtype'
    t_ref2m_max_inst => clm3%g%l%c%p%pes%t_ref2m_max_inst
    t_ref2m_min_inst => clm3%g%l%c%p%pes%t_ref2m_min_inst
    t_ref2m_max => clm3%g%l%c%p%pes%t_ref2m_max
    t_ref2m_min => clm3%g%l%c%p%pes%t_ref2m_min
    t_ref2m_max_inst_u => clm3%g%l%c%p%pes%t_ref2m_max_inst_u
    t_ref2m_max_inst_r => clm3%g%l%c%p%pes%t_ref2m_max_inst_r
    t_ref2m_min_inst_u => clm3%g%l%c%p%pes%t_ref2m_min_inst_u
    t_ref2m_min_inst_r => clm3%g%l%c%p%pes%t_ref2m_min_inst_r
    t_ref2m_max_u => clm3%g%l%c%p%pes%t_ref2m_max_u
    t_ref2m_max_r => clm3%g%l%c%p%pes%t_ref2m_max_r
    t_ref2m_min_u => clm3%g%l%c%p%pes%t_ref2m_min_u
    t_ref2m_min_r => clm3%g%l%c%p%pes%t_ref2m_min_r
    t_veg24 => clm3%g%l%c%p%pvs%t_veg24
    t_veg240 => clm3%g%l%c%p%pvs%t_veg240
    fsd24 => clm3%g%l%c%p%pvs%fsd24
    fsd240 => clm3%g%l%c%p%pvs%fsd240
    fsi24 => clm3%g%l%c%p%pvs%fsi24
    fsi240 => clm3%g%l%c%p%pvs%fsi240
    fsun24 => clm3%g%l%c%p%pvs%fsun24
    fsun240 => clm3%g%l%c%p%pvs%fsun240
    elai_p => clm3%g%l%c%p%pvs%elai_p
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    do p = begp,endp
          t_ref2m_max(p) = spval
          t_ref2m_min(p) = spval
          t_ref2m_max_inst(p) = -spval
          t_ref2m_min_inst(p) = spval
          t_ref2m_max_u(p) = spval
          t_ref2m_max_r(p) = spval
          t_ref2m_min_u(p) = spval
          t_ref2m_min_r(p) = spval
          t_ref2m_max_inst_u(p) = -spval
          t_ref2m_max_inst_r(p) = -spval
          t_ref2m_min_inst_u(p) = spval
          t_ref2m_min_inst_r(p) = spval
    end do
    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(6,*)'update_accum_hist allocation error for rbufslp'
       call endrun
    endif
    call extract_accum_field ('T_VEG24', rbufslp, nstep)
    do p = begp,endp
       t_veg24(p) = rbufslp(p)
    end do
    call extract_accum_field ('T_VEG240', rbufslp, nstep)
    do p = begp,endp
       t_veg240(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSD24', rbufslp, nstep)
    do p = begp,endp
       fsd24(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSD240', rbufslp, nstep)
    do p = begp,endp
       fsd240(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSI24', rbufslp, nstep)
    do p = begp,endp
       fsi24(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSI240', rbufslp, nstep)
    do p = begp,endp
       fsi240(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSUN24', rbufslp, nstep)
    do p = begp,endp
       fsun24(p) = rbufslp(p)
    end do
    call extract_accum_field ('FSUN240', rbufslp, nstep)
    do p = begp,endp
       fsun240(p) = rbufslp(p)
    end do
    call extract_accum_field ('LAIP', rbufslp, nstep)
    do p = begp,endp
       elai_p(p) = rbufslp(p)
    end do
    deallocate(rbufslp)
  end subroutine initAccClmtype
end module accFldsMod
module SurfaceRadiationMod
   use shr_kind_mod, only: r8 => shr_kind_r8
   use globals, only : nstep
   use module_cam_support, only: endrun
  implicit none
  save
  public :: SurfaceRadiation
contains
   subroutine SurfaceRadiation(lbp, ubp, num_nourbanp, filter_nourbanp)
     use clmtype
     use clm_varpar , only : numrad
     use clm_varcon , only : spval, istsoil
     use clm_varpar , only : nlevsno
     use SNICARMod , only : DO_SNO_OC
     use globals , only : dtime, secs
     implicit none
     integer, intent(in) :: lbp, ubp
     integer, intent(in) :: num_nourbanp
     integer, intent(in) :: filter_nourbanp(ubp-lbp+1)
     integer , pointer :: ivt(:)
     integer , pointer :: pcolumn(:)
     integer , pointer :: pgridcell(:)
     real(r8), pointer :: pwtgcell(:)
     real(r8), pointer :: elai(:)
     real(r8), pointer :: esai(:)
     real(r8), pointer :: londeg(:)
     real(r8), pointer :: latdeg(:)
     real(r8), pointer :: slasun(:)
     real(r8), pointer :: slasha(:)
     real(r8), pointer :: gdir(:)
     real(r8), pointer :: omega(:,:)
     real(r8), pointer :: coszen(:)
     real(r8), pointer :: forc_solad(:,:)
     real(r8), pointer :: forc_solai(:,:)
     real(r8), pointer :: fabd(:,:)
     real(r8), pointer :: fabi(:,:)
     real(r8), pointer :: ftdd(:,:)
     real(r8), pointer :: ftid(:,:)
     real(r8), pointer :: ftii(:,:)
     real(r8), pointer :: albgrd(:,:)
     real(r8), pointer :: albgri(:,:)
     real(r8), pointer :: albd(:,:)
     real(r8), pointer :: albi(:,:)
     real(r8), pointer :: slatop(:)
     real(r8), pointer :: dsladlai(:)
     real(r8), pointer :: fsun(:)
     real(r8), pointer :: laisun(:)
     real(r8), pointer :: laisha(:)
     real(r8), pointer :: sabg(:)
     real(r8), pointer :: sabv(:)
     real(r8), pointer :: fsa(:)
     real(r8), pointer :: fsa_r(:)
     integer , pointer :: ityplun(:)
     integer , pointer :: plandunit(:)
     real(r8), pointer :: parsun(:)
     real(r8), pointer :: parsha(:)
     real(r8), pointer :: fsr(:)
     real(r8), pointer :: fsds_vis_d(:)
     real(r8), pointer :: fsds_nir_d(:)
     real(r8), pointer :: fsds_vis_i(:)
     real(r8), pointer :: fsds_nir_i(:)
     real(r8), pointer :: fsr_vis_d(:)
     real(r8), pointer :: fsr_nir_d(:)
     real(r8), pointer :: fsr_vis_i(:)
     real(r8), pointer :: fsr_nir_i(:)
     real(r8), pointer :: fsds_vis_d_ln(:)
     real(r8), pointer :: fsds_nir_d_ln(:)
     real(r8), pointer :: fsr_vis_d_ln(:)
     real(r8), pointer :: fsr_nir_d_ln(:)
     real(r8), pointer :: eff_kid(:,:)
     real(r8), pointer :: eff_kii(:,:)
     real(r8), pointer :: sun_faid(:,:)
     real(r8), pointer :: sun_faii(:,:)
     real(r8), pointer :: sha_faid(:,:)
     real(r8), pointer :: sha_faii(:,:)
     real(r8), pointer :: sun_add(:,:)
     real(r8), pointer :: tot_aid(:,:)
     real(r8), pointer :: sun_aid(:,:)
     real(r8), pointer :: sun_aii(:,:)
     real(r8), pointer :: sha_aid(:,:)
     real(r8), pointer :: sha_aii(:,:)
     real(r8), pointer :: sun_atot(:,:)
     real(r8), pointer :: sha_atot(:,:)
     real(r8), pointer :: sun_alf(:,:)
     real(r8), pointer :: sha_alf(:,:)
     real(r8), pointer :: sun_aperlai(:,:)
     real(r8), pointer :: sha_aperlai(:,:)
     real(r8), pointer :: flx_absdv(:,:)
     real(r8), pointer :: flx_absdn(:,:)
     real(r8), pointer :: flx_absiv(:,:)
     real(r8), pointer :: flx_absin(:,:)
     integer , pointer :: snl(:)
     real(r8), pointer :: albgrd_pur(:,:)
     real(r8), pointer :: albgri_pur(:,:)
     real(r8), pointer :: albgrd_bc(:,:)
     real(r8), pointer :: albgri_bc(:,:)
     real(r8), pointer :: albgrd_oc(:,:)
     real(r8), pointer :: albgri_oc(:,:)
     real(r8), pointer :: albgrd_dst(:,:)
     real(r8), pointer :: albgri_dst(:,:)
     real(r8), pointer :: albsnd_hst(:,:)
     real(r8), pointer :: albsni_hst(:,:)
     real(r8), pointer :: sabg_lyr(:,:)
     real(r8), pointer :: sfc_frc_aer(:)
     real(r8), pointer :: sfc_frc_bc(:)
     real(r8), pointer :: sfc_frc_oc(:)
     real(r8), pointer :: sfc_frc_dst(:)
     real(r8), pointer :: sfc_frc_aer_sno(:)
     real(r8), pointer :: sfc_frc_bc_sno(:)
     real(r8), pointer :: sfc_frc_oc_sno(:)
     real(r8), pointer :: sfc_frc_dst_sno(:)
     real(r8), pointer :: frac_sno(:)
     real(r8), pointer :: fsr_sno_vd(:)
     real(r8), pointer :: fsr_sno_nd(:)
     real(r8), pointer :: fsr_sno_vi(:)
     real(r8), pointer :: fsr_sno_ni(:)
     real(r8), pointer :: fsds_sno_vd(:)
     real(r8), pointer :: fsds_sno_nd(:)
     real(r8), pointer :: fsds_sno_vi(:)
     real(r8), pointer :: fsds_sno_ni(:)
     real(r8), pointer :: snowdp(:)
     integer , parameter :: nband = numrad
     real(r8), parameter :: mpe = 1.e-06_r8
     integer :: fp
     integer :: p
     integer :: c
     integer :: l
     integer :: g
     integer :: ib
     real(r8) :: absrad
     real(r8) :: rnir
     real(r8) :: rvis
     real(r8) :: laifra
     real(r8) :: trd(lbp:ubp,numrad)
     real(r8) :: tri(lbp:ubp,numrad)
     real(r8) :: cad(lbp:ubp,numrad)
     real(r8) :: cai(lbp:ubp,numrad)
     real(r8) :: vai(lbp:ubp)
     real(r8) :: ext
     real(r8) :: t1, t2
     real(r8) :: cosz
     integer :: local_secp1
     integer :: i
     real(r8) :: sabg_snl_sum
     real(r8) :: absrad_pur
     real(r8) :: absrad_bc
     real(r8) :: absrad_oc
     real(r8) :: absrad_dst
     real(r8) :: sabg_pur(lbp:ubp)
     real(r8) :: sabg_bc(lbp:ubp)
     real(r8) :: sabg_oc(lbp:ubp)
     real(r8) :: sabg_dst(lbp:ubp)
     londeg => clm3%g%londeg
     latdeg => clm3%g%latdeg
     forc_solad => clm_a2l%forc_solad
     forc_solai => clm_a2l%forc_solai
     ityplun => clm3%g%l%itype
     albgrd => clm3%g%l%c%cps%albgrd
     albgri => clm3%g%l%c%cps%albgri
     coszen => clm3%g%l%c%cps%coszen
     plandunit => clm3%g%l%c%p%landunit
     ivt => clm3%g%l%c%p%itype
     pcolumn => clm3%g%l%c%p%column
     pgridcell => clm3%g%l%c%p%gridcell
     pwtgcell => clm3%g%l%c%p%wtgcell
     elai => clm3%g%l%c%p%pps%elai
     esai => clm3%g%l%c%p%pps%esai
     slasun => clm3%g%l%c%p%pps%slasun
     slasha => clm3%g%l%c%p%pps%slasha
     gdir => clm3%g%l%c%p%pps%gdir
     omega => clm3%g%l%c%p%pps%omega
     laisun => clm3%g%l%c%p%pps%laisun
     laisha => clm3%g%l%c%p%pps%laisha
     fabd => clm3%g%l%c%p%pps%fabd
     fabi => clm3%g%l%c%p%pps%fabi
     ftdd => clm3%g%l%c%p%pps%ftdd
     ftid => clm3%g%l%c%p%pps%ftid
     ftii => clm3%g%l%c%p%pps%ftii
     albd => clm3%g%l%c%p%pps%albd
     albi => clm3%g%l%c%p%pps%albi
     fsun => clm3%g%l%c%p%pps%fsun
     sabg => clm3%g%l%c%p%pef%sabg
     sabv => clm3%g%l%c%p%pef%sabv
     snowdp => clm3%g%l%c%cps%snowdp
     fsa => clm3%g%l%c%p%pef%fsa
     fsa_r => clm3%g%l%c%p%pef%fsa_r
     fsr => clm3%g%l%c%p%pef%fsr
     parsun => clm3%g%l%c%p%pef%parsun
     parsha => clm3%g%l%c%p%pef%parsha
     fsds_vis_d => clm3%g%l%c%p%pef%fsds_vis_d
     fsds_nir_d => clm3%g%l%c%p%pef%fsds_nir_d
     fsds_vis_i => clm3%g%l%c%p%pef%fsds_vis_i
     fsds_nir_i => clm3%g%l%c%p%pef%fsds_nir_i
     fsr_vis_d => clm3%g%l%c%p%pef%fsr_vis_d
     fsr_nir_d => clm3%g%l%c%p%pef%fsr_nir_d
     fsr_vis_i => clm3%g%l%c%p%pef%fsr_vis_i
     fsr_nir_i => clm3%g%l%c%p%pef%fsr_nir_i
     fsds_vis_d_ln => clm3%g%l%c%p%pef%fsds_vis_d_ln
     fsds_nir_d_ln => clm3%g%l%c%p%pef%fsds_nir_d_ln
     fsr_vis_d_ln => clm3%g%l%c%p%pef%fsr_vis_d_ln
     fsr_nir_d_ln => clm3%g%l%c%p%pef%fsr_nir_d_ln
     eff_kid => clm3%g%l%c%p%pps%eff_kid
     eff_kii => clm3%g%l%c%p%pps%eff_kii
     sun_faid => clm3%g%l%c%p%pps%sun_faid
     sun_faii => clm3%g%l%c%p%pps%sun_faii
     sha_faid => clm3%g%l%c%p%pps%sha_faid
     sha_faii => clm3%g%l%c%p%pps%sha_faii
     sun_add => clm3%g%l%c%p%pef%sun_add
     tot_aid => clm3%g%l%c%p%pef%tot_aid
     sun_aid => clm3%g%l%c%p%pef%sun_aid
     sun_aii => clm3%g%l%c%p%pef%sun_aii
     sha_aid => clm3%g%l%c%p%pef%sha_aid
     sha_aii => clm3%g%l%c%p%pef%sha_aii
     sun_atot => clm3%g%l%c%p%pef%sun_atot
     sha_atot => clm3%g%l%c%p%pef%sha_atot
     sun_alf => clm3%g%l%c%p%pef%sun_alf
     sha_alf => clm3%g%l%c%p%pef%sha_alf
     sun_aperlai => clm3%g%l%c%p%pef%sun_aperlai
     sha_aperlai => clm3%g%l%c%p%pef%sha_aperlai
     slatop => pftcon%slatop
     dsladlai => pftcon%dsladlai
     frac_sno => clm3%g%l%c%cps%frac_sno
     flx_absdv => clm3%g%l%c%cps%flx_absdv
     flx_absdn => clm3%g%l%c%cps%flx_absdn
     flx_absiv => clm3%g%l%c%cps%flx_absiv
     flx_absin => clm3%g%l%c%cps%flx_absin
     sabg_lyr => clm3%g%l%c%p%pef%sabg_lyr
     snl => clm3%g%l%c%cps%snl
     sfc_frc_aer => clm3%g%l%c%p%pef%sfc_frc_aer
     sfc_frc_aer_sno => clm3%g%l%c%p%pef%sfc_frc_aer_sno
     albgrd_pur => clm3%g%l%c%cps%albgrd_pur
     albgri_pur => clm3%g%l%c%cps%albgri_pur
     sfc_frc_bc => clm3%g%l%c%p%pef%sfc_frc_bc
     sfc_frc_bc_sno => clm3%g%l%c%p%pef%sfc_frc_bc_sno
     albgrd_bc => clm3%g%l%c%cps%albgrd_bc
     albgri_bc => clm3%g%l%c%cps%albgri_bc
     sfc_frc_oc => clm3%g%l%c%p%pef%sfc_frc_oc
     sfc_frc_oc_sno => clm3%g%l%c%p%pef%sfc_frc_oc_sno
     albgrd_oc => clm3%g%l%c%cps%albgrd_oc
     albgri_oc => clm3%g%l%c%cps%albgri_oc
     sfc_frc_dst => clm3%g%l%c%p%pef%sfc_frc_dst
     sfc_frc_dst_sno => clm3%g%l%c%p%pef%sfc_frc_dst_sno
     albgrd_dst => clm3%g%l%c%cps%albgrd_dst
     albgri_dst => clm3%g%l%c%cps%albgri_dst
     albsnd_hst => clm3%g%l%c%cps%albsnd_hst
     albsni_hst => clm3%g%l%c%cps%albsni_hst
     fsr_sno_vd => clm3%g%l%c%p%pef%fsr_sno_vd
     fsr_sno_nd => clm3%g%l%c%p%pef%fsr_sno_nd
     fsr_sno_vi => clm3%g%l%c%p%pef%fsr_sno_vi
     fsr_sno_ni => clm3%g%l%c%p%pef%fsr_sno_ni
     fsds_sno_vd => clm3%g%l%c%p%pef%fsds_sno_vd
     fsds_sno_nd => clm3%g%l%c%p%pef%fsds_sno_nd
     fsds_sno_vi => clm3%g%l%c%p%pef%fsds_sno_vi
     fsds_sno_ni => clm3%g%l%c%p%pef%fsds_sno_ni
!dir$ concurrent
     do fp = 1,num_nourbanp
        p = filter_nourbanp(fp)
        if (pwtgcell(p)>0._r8) then
           sabg(p) = 0._r8
           sabv(p) = 0._r8
           fsa(p) = 0._r8
           l = plandunit(p)
           if (ityplun(l)==istsoil) then
             fsa_r(p) = 0._r8
           end if
           sabg_lyr(p,:) = 0._r8
           sabg_pur(p) = 0._r8
           sabg_bc(p) = 0._r8
           sabg_oc(p) = 0._r8
           sabg_dst(p) = 0._r8
        end if
     end do
!dir$ concurrent
     do fp = 1,num_nourbanp
        p = filter_nourbanp(fp)
        if (pwtgcell(p)>0._r8) then
           c = pcolumn(p)
           g = pgridcell(p)
           vai(p) = elai(p) + esai(p)
           if (coszen(c) > 0._r8 .and. elai(p) > 0._r8 .and. gdir(p) > 0._r8) then
              cosz = max(0.001_r8, coszen(c))
              ext = gdir(p)/cosz
              t1 = min(ext*elai(p), 40.0_r8)
              t2 = exp(-t1)
              fsun(p) = (1._r8-t2)/t1
              if (elai(p) > 0.01_r8) then
                 laisun(p) = elai(p)*fsun(p)
                 laisha(p) = elai(p)*(1._r8-fsun(p))
                 slasun(p) = (t2*dsladlai(ivt(p))*ext*elai(p) + &
                              t2*dsladlai(ivt(p)) + &
                              t2*slatop(ivt(p))*ext - &
                              dsladlai(ivt(p)) - &
                              slatop(ivt(p))*ext) / &
                              (ext*(t2-1._r8))
                 slasha(p) = ((slatop(ivt(p)) + &
                             (dsladlai(ivt(p)) * elai(p)/2.0_r8)) * elai(p) - &
                             laisun(p)*slasun(p)) / laisha(p)
              else
                 fsun(p) = 1._r8
                 laisun(p) = elai(p)
                 laisha(p) = 0._r8
                 slasun(p) = slatop(ivt(p))
                 slasha(p) = 0._r8
              end if
           else
              fsun(p) = 0._r8
              laisun(p) = 0._r8
              laisha(p) = elai(p)
              slasun(p) = 0._r8
              slasha(p) = 0._r8
           end if
        end if
     end do
     do ib = 1, nband
!dir$ concurrent
        do fp = 1,num_nourbanp
           p = filter_nourbanp(fp)
           if (pwtgcell(p)>0._r8) then
              c = pcolumn(p)
              g = pgridcell(p)
              cad(p,ib) = forc_solad(g,ib)*fabd(p,ib)
              cai(p,ib) = forc_solai(g,ib)*fabi(p,ib)
              sabv(p) = sabv(p) + cad(p,ib) + cai(p,ib)
              fsa(p) = fsa(p) + cad(p,ib) + cai(p,ib)
              l = plandunit(p)
              if (ityplun(l)==istsoil) then
                fsa_r(p) = fsa_r(p) + cad(p,ib) + cai(p,ib)
              end if
              trd(p,ib) = forc_solad(g,ib)*ftdd(p,ib)
              tri(p,ib) = forc_solad(g,ib)*ftid(p,ib) + forc_solai(g,ib)*ftii(p,ib)
              absrad = trd(p,ib)*(1._r8-albgrd(c,ib)) + tri(p,ib)*(1._r8-albgri(c,ib))
              sabg(p) = sabg(p) + absrad
              fsa(p) = fsa(p) + absrad
              if (ityplun(l)==istsoil) then
                fsa_r(p) = fsa_r(p) + absrad
              end if
              if (coszen(c) > 0._r8 .and. elai(p) > 0._r8 .and. gdir(p) > 0._r8 ) then
                 sun_add(p,ib) = forc_solad(g,ib) * (1._r8-ftdd(p,ib)) * (1._r8-omega(p,ib))
                 tot_aid(p,ib) = (forc_solad(g,ib) * fabd(p,ib)) - sun_add(p,ib)
                 tot_aid(p,ib) = max(tot_aid(p,ib), 0._r8)
                 sun_faid(p,ib) = fsun(p)
                 sun_faii(p,ib) = fsun(p)
                 sha_faid(p,ib) = 1._r8-sun_faid(p,ib)
                 sha_faii(p,ib) = 1._r8-sun_faii(p,ib)
                 sun_aid(p,ib) = tot_aid(p,ib) * sun_faid(p,ib)
                 sun_aii(p,ib) = forc_solai(g,ib)*fabi(p,ib)*sun_faii(p,ib)
                 sha_aid(p,ib) = tot_aid(p,ib) * sha_faid(p,ib)
                 sha_aii(p,ib) = forc_solai(g,ib)*fabi(p,ib)*sha_faii(p,ib)
                 sun_atot(p,ib) = sun_add(p,ib) + sun_aid(p,ib) + sun_aii(p,ib)
                 sha_atot(p,ib) = sha_aid(p,ib) + sha_aii(p,ib)
                 laifra = elai(p)/vai(p)
                 sun_alf(p,ib) = sun_atot(p,ib) * laifra
                 sha_alf(p,ib) = sha_atot(p,ib) * laifra
                 if (laisun(p) > 0._r8) then
                    sun_aperlai(p,ib) = sun_alf(p,ib)/laisun(p)
                 else
                    sun_aperlai(p,ib) = 0._r8
                 endif
                 if (laisha(p) > 0._r8) then
                    sha_aperlai(p,ib) = sha_alf(p,ib)/laisha(p)
                 else
                    sha_aperlai(p,ib) = 0._r8
                 endif
              else
                 sun_add(p,ib) = 0._r8
                 tot_aid(p,ib) = 0._r8
                 eff_kid(p,ib) = 0._r8
                 eff_kii(p,ib) = 0._r8
                 sun_faid(p,ib) = 0._r8
                 sun_faii(p,ib) = 0._r8
                 sha_faid(p,ib) = 0._r8
                 sha_faii(p,ib) = 0._r8
                 sun_aid(p,ib) = 0._r8
                 sun_aii(p,ib) = 0._r8
                 sha_aid(p,ib) = 0._r8
                 sha_aii(p,ib) = 0._r8
                 sun_atot(p,ib) = 0._r8
                 sha_atot(p,ib) = 0._r8
                 sun_alf(p,ib) = 0._r8
                 sha_alf(p,ib) = 0._r8
                 sun_aperlai(p,ib) = 0._r8
                 sha_aperlai(p,ib) = 0._r8
              end if
           end if
        end do
     end do
     do fp = 1,num_nourbanp
        p = filter_nourbanp(fp)
        if (pwtgcell(p)>0._r8) then
           c = pcolumn(p)
           sabg_snl_sum = 0._r8
           if (snl(c) == 0) then
              sabg_lyr(p,:) = 0._r8
              sabg_lyr(p,1) = sabg(p)
              sabg_snl_sum = sabg_lyr(p,1)
           else
              do i = -nlevsno+1,1,1
                 sabg_lyr(p,i) = flx_absdv(c,i)*trd(p,1) + flx_absdn(c,i)*trd(p,2) + &
                                 flx_absiv(c,i)*tri(p,1) + flx_absin(c,i)*tri(p,2)
                 if (i >= snl(c)+1) then
                    sabg_snl_sum = sabg_snl_sum + sabg_lyr(p,i)
                 endif
              enddo
              if (abs(sabg_snl_sum-sabg(p)) > 0.00001_r8) then
                 if (snl(c) == 0) then
                    sabg_lyr(p,-4:0) = 0._r8
                    sabg_lyr(p,1) = sabg(p)
                 elseif (snl(c) == -1) then
                    sabg_lyr(p,-4:-1) = 0._r8
                    sabg_lyr(p,0) = sabg(p)*0.6_r8
                    sabg_lyr(p,1) = sabg(p)*0.4_r8
                 else
                    sabg_lyr(p,:) = 0._r8
                    sabg_lyr(p,snl(c)+1) = sabg(p)*0.75_r8
                    sabg_lyr(p,snl(c)+2) = sabg(p)*0.25_r8
                 endif
              endif
              if (snowdp(c) < 0.10_r8) then
                 if (snl(c) == 0) then
                    sabg_lyr(p,-4:0) = 0._r8
                    sabg_lyr(p,1) = sabg(p)
                 elseif (snl(c) == -1) then
                    sabg_lyr(p,-4:-1) = 0._r8
                    sabg_lyr(p,0) = sabg(p)
                    sabg_lyr(p,1) = 0._r8
                 else
                    sabg_lyr(p,:) = 0._r8
                    sabg_lyr(p,snl(c)+1) = sabg(p)*0.75_r8
                    sabg_lyr(p,snl(c)+2) = sabg(p)*0.25_r8
                 endif
              endif
           endif
           if (abs(sum(sabg_lyr(p,:))-sabg(p)) > 0.00001_r8) then
              write(6,*) "SNICAR ERROR: Absorbed ground radiation not equal to summed snow layer radiation. pft = ", &
                             p," Col= ", c, " Diff= ",sum(sabg_lyr(p,:))-sabg(p), " sabg(p)= ", sabg(p), " sabg_sum(p)= ", &
                             sum(sabg_lyr(p,:)), " snl(c)= ", snl(c)
              write(6,*) "flx_absdv1= ", trd(p,1)*(1.-albgrd(c,1)), "flx_absdv2= ", sum(flx_absdv(c,:))*trd(p,1)
              write(6,*) "flx_absiv1= ", tri(p,1)*(1.-albgri(c,1))," flx_absiv2= ", sum(flx_absiv(c,:))*tri(p,1)
              write(6,*) "flx_absdn1= ", trd(p,2)*(1.-albgrd(c,2))," flx_absdn2= ", sum(flx_absdn(c,:))*trd(p,2)
              write(6,*) "flx_absin1= ", tri(p,2)*(1.-albgri(c,2))," flx_absin2= ", sum(flx_absin(c,:))*tri(p,2)
              write(6,*) "albgrd_nir= ", albgrd(c,2)
              write(6,*) "coszen= ", coszen(c)
              call endrun()
           endif
        endif
     enddo
!dir$ concurrent
     do fp = 1,num_nourbanp
        p = filter_nourbanp(fp)
        if (pwtgcell(p)>0._r8) then
           g = pgridcell(p)
           parsun(p) = sun_aperlai(p,1)
           parsha(p) = sha_aperlai(p,1)
           rvis = albd(p,1)*forc_solad(g,1) + albi(p,1)*forc_solai(g,1)
           rnir = albd(p,2)*forc_solad(g,2) + albi(p,2)*forc_solai(g,2)
           fsr(p) = rvis + rnir
           fsds_vis_d(p) = forc_solad(g,1)
           fsds_nir_d(p) = forc_solad(g,2)
           fsds_vis_i(p) = forc_solai(g,1)
           fsds_nir_i(p) = forc_solai(g,2)
           fsr_vis_d(p) = albd(p,1)*forc_solad(g,1)
           fsr_nir_d(p) = albd(p,2)*forc_solad(g,2)
           fsr_vis_i(p) = albi(p,1)*forc_solai(g,1)
           fsr_nir_i(p) = albi(p,2)*forc_solai(g,2)
           local_secp1 = secs + nint((londeg(g)/15._r8*3600._r8)/dtime)*dtime
           local_secp1 = mod(local_secp1,86400)
           if (local_secp1 == 43200) then
              fsds_vis_d_ln(p) = forc_solad(g,1)
              fsds_nir_d_ln(p) = forc_solad(g,2)
              fsr_vis_d_ln(p) = albd(p,1)*forc_solad(g,1)
              fsr_nir_d_ln(p) = albd(p,2)*forc_solad(g,2)
           else
              fsds_vis_d_ln(p) = spval
              fsds_nir_d_ln(p) = spval
              fsr_vis_d_ln(p) = spval
              fsr_nir_d_ln(p) = spval
           end if
           c = pcolumn(p)
           if (snl(c) < 0) then
              fsds_sno_vd(p) = forc_solad(g,1)
              fsds_sno_nd(p) = forc_solad(g,2)
              fsds_sno_vi(p) = forc_solai(g,1)
              fsds_sno_ni(p) = forc_solai(g,2)
              fsr_sno_vd(p) = fsds_vis_d(p)*albsnd_hst(c,1)
              fsr_sno_nd(p) = fsds_nir_d(p)*albsnd_hst(c,2)
              fsr_sno_vi(p) = fsds_vis_i(p)*albsni_hst(c,1)
              fsr_sno_ni(p) = fsds_nir_i(p)*albsni_hst(c,2)
           else
              fsds_sno_vd(p) = spval
              fsds_sno_nd(p) = spval
              fsds_sno_vi(p) = spval
              fsds_sno_ni(p) = spval
              fsr_sno_vd(p) = spval
              fsr_sno_nd(p) = spval
              fsr_sno_vi(p) = spval
              fsr_sno_ni(p) = spval
           endif
        end if
     end do
   end subroutine SurfaceRadiation
end module SurfaceRadiationMod
module SurfaceAlbedoMod
  use clm_varcon , only : istsoil
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar , only : nlevsno
  use SNICARMod , only : sno_nbr_aer, SNICAR_RT, DO_SNO_AER, DO_SNO_OC
  use globals, only : nstep
  implicit none
  save
  public :: SurfaceAlbedo
  private :: SoilAlbedo
  private :: TwoStream
contains
  subroutine SurfaceAlbedo(lbg, ubg, lbc, ubc, lbp, ubp, &
                           num_nourbanc, filter_nourbanc, &
                           num_nourbanp, filter_nourbanp, &
                           nextsw_cday, declinp1)
    use clmtype
    use clm_varpar , only : numrad
    use shr_orb_mod
    implicit none
    integer , intent(in) :: lbg, ubg
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: num_nourbanc
    integer , intent(in) :: filter_nourbanc(ubc-lbc+1)
    integer , intent(in) :: num_nourbanp
    integer , intent(in) :: filter_nourbanp(ubp-lbp+1)
    real(r8), intent(in) :: nextsw_cday
    real(r8), intent(in) :: declinp1
    integer , pointer :: pgridcell(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: itypelun(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: cgridcell(:)
    real(r8), pointer :: pwtgcell(:)
    real(r8), pointer :: lat(:)
    real(r8), pointer :: lon(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: esai(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: rhol(:,:)
    real(r8), pointer :: rhos(:,:)
    real(r8), pointer :: taul(:,:)
    real(r8), pointer :: taus(:,:)
    integer , pointer :: ivt(:)
    real(r8), pointer :: coszen(:)
    real(r8), pointer :: fsun(:)
    real(r8), pointer :: albgrd(:,:)
    real(r8), pointer :: albgri(:,:)
    real(r8), pointer :: albd(:,:)
    real(r8), pointer :: albi(:,:)
    real(r8), pointer :: fabd(:,:)
    real(r8), pointer :: fabi(:,:)
    real(r8), pointer :: ftdd(:,:)
    real(r8), pointer :: ftid(:,:)
    real(r8), pointer :: ftii(:,:)
    real(r8), pointer :: decl(:)
    real(r8), pointer :: gdir(:)
    real(r8), pointer :: omega(:,:)
    real(r8), pointer :: frac_sno(:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: mss_cnc_bcphi(:,:)
    real(r8), pointer :: mss_cnc_bcpho(:,:)
    real(r8), pointer :: mss_cnc_ocphi(:,:)
    real(r8), pointer :: mss_cnc_ocpho(:,:)
    real(r8), pointer :: mss_cnc_dst1(:,:)
    real(r8), pointer :: mss_cnc_dst2(:,:)
    real(r8), pointer :: mss_cnc_dst3(:,:)
    real(r8), pointer :: mss_cnc_dst4(:,:)
    real(r8), pointer :: albsod(:,:)
    real(r8), pointer :: albsoi(:,:)
    real(r8), pointer :: flx_absdv(:,:)
    real(r8), pointer :: flx_absdn(:,:)
    real(r8), pointer :: flx_absiv(:,:)
    real(r8), pointer :: flx_absin(:,:)
    real(r8), pointer :: snw_rds(:,:)
    real(r8), pointer :: albgrd_pur(:,:)
    real(r8), pointer :: albgri_pur(:,:)
    real(r8), pointer :: albgrd_bc(:,:)
    real(r8), pointer :: albgri_bc(:,:)
    real(r8), pointer :: albgrd_oc(:,:)
    real(r8), pointer :: albgri_oc(:,:)
    real(r8), pointer :: albgrd_dst(:,:)
    real(r8), pointer :: albgri_dst(:,:)
    real(r8), pointer :: albsnd_hst(:,:)
    real(r8), pointer :: albsni_hst(:,:)
    real(r8), parameter :: mpe = 1.e-06_r8
    integer :: fp,fc,g,c,p
    integer :: ib
    integer :: ic
    real(r8) :: wl(lbp:ubp)
    real(r8) :: ws(lbp:ubp)
    real(r8) :: vai(lbp:ubp)
    real(r8) :: rho(lbp:ubp,numrad)
    real(r8) :: tau(lbp:ubp,numrad)
    real(r8) :: ftdi(lbp:ubp,numrad)
    real(r8) :: albsnd(lbc:ubc,numrad)
    real(r8) :: albsni(lbc:ubc,numrad)
    real(r8) :: ext(lbp:ubp)
    real(r8) :: coszen_gcell(lbg:ubg)
    real(r8) :: coszen_col(lbc:ubc)
    real(r8) :: coszen_pft(lbp:ubp)
    integer :: num_vegsol
    integer :: filter_vegsol(ubp-lbp+1)
    integer :: num_novegsol
    integer :: filter_novegsol(ubp-lbp+1)
    integer, parameter :: nband =numrad
    integer :: flg_slr
    integer :: flg_snw_ice
    real(r8) :: albsnd_pur(lbc:ubc,numrad)
    real(r8) :: albsni_pur(lbc:ubc,numrad)
    real(r8) :: albsnd_bc(lbc:ubc,numrad)
    real(r8) :: albsni_bc(lbc:ubc,numrad)
    real(r8) :: albsnd_oc(lbc:ubc,numrad)
    real(r8) :: albsni_oc(lbc:ubc,numrad)
    real(r8) :: albsnd_dst(lbc:ubc,numrad)
    real(r8) :: albsni_dst(lbc:ubc,numrad)
    integer :: i
    real(r8) :: flx_absd_snw(lbc:ubc,-nlevsno+1:1,numrad)
    real(r8) :: flx_absi_snw(lbc:ubc,-nlevsno+1:1,numrad)
    real(r8) :: foo_snw(lbc:ubc,-nlevsno+1:1,numrad)
    real(r8) :: albsfc(lbc:ubc,numrad)
    real(r8) :: h2osno_liq(lbc:ubc,-nlevsno+1:0)
    real(r8) :: h2osno_ice(lbc:ubc,-nlevsno+1:0)
    integer :: snw_rds_in(lbc:ubc,-nlevsno+1:0)
    real(r8) :: mss_cnc_aer_in_frc_pur(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    real(r8) :: mss_cnc_aer_in_frc_bc(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    real(r8) :: mss_cnc_aer_in_frc_oc(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    real(r8) :: mss_cnc_aer_in_frc_dst(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    real(r8) :: mss_cnc_aer_in_fdb(lbc:ubc,-nlevsno+1:0,sno_nbr_aer)
    lat => clm3%g%lat_a
    lon => clm3%g%lon_a
    itypelun => clm3%g%l%itype
    cgridcell => clm3%g%l%c%gridcell
    h2osno => clm3%g%l%c%cws%h2osno
    albgrd => clm3%g%l%c%cps%albgrd
    albgri => clm3%g%l%c%cps%albgri
    decl => clm3%g%l%c%cps%decl
    coszen => clm3%g%l%c%cps%coszen
    albsod => clm3%g%l%c%cps%albsod
    albsoi => clm3%g%l%c%cps%albsoi
    frac_sno => clm3%g%l%c%cps%frac_sno
    flx_absdv => clm3%g%l%c%cps%flx_absdv
    flx_absdn => clm3%g%l%c%cps%flx_absdn
    flx_absiv => clm3%g%l%c%cps%flx_absiv
    flx_absin => clm3%g%l%c%cps%flx_absin
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    snw_rds => clm3%g%l%c%cps%snw_rds
    albgrd_pur => clm3%g%l%c%cps%albgrd_pur
    albgri_pur => clm3%g%l%c%cps%albgri_pur
    albgrd_bc => clm3%g%l%c%cps%albgrd_bc
    albgri_bc => clm3%g%l%c%cps%albgri_bc
    albgrd_oc => clm3%g%l%c%cps%albgrd_oc
    albgri_oc => clm3%g%l%c%cps%albgri_oc
    albgrd_dst => clm3%g%l%c%cps%albgrd_dst
    albgri_dst => clm3%g%l%c%cps%albgri_dst
    mss_cnc_bcphi => clm3%g%l%c%cps%mss_cnc_bcphi
    mss_cnc_bcpho => clm3%g%l%c%cps%mss_cnc_bcpho
    mss_cnc_ocphi => clm3%g%l%c%cps%mss_cnc_ocphi
    mss_cnc_ocpho => clm3%g%l%c%cps%mss_cnc_ocpho
    mss_cnc_dst1 => clm3%g%l%c%cps%mss_cnc_dst1
    mss_cnc_dst2 => clm3%g%l%c%cps%mss_cnc_dst2
    mss_cnc_dst3 => clm3%g%l%c%cps%mss_cnc_dst3
    mss_cnc_dst4 => clm3%g%l%c%cps%mss_cnc_dst4
    albsnd_hst => clm3%g%l%c%cps%albsnd_hst
    albsni_hst => clm3%g%l%c%cps%albsni_hst
    plandunit => clm3%g%l%c%p%landunit
    pgridcell => clm3%g%l%c%p%gridcell
    pcolumn => clm3%g%l%c%p%column
    pwtgcell => clm3%g%l%c%p%wtgcell
    albd => clm3%g%l%c%p%pps%albd
    albi => clm3%g%l%c%p%pps%albi
    fabd => clm3%g%l%c%p%pps%fabd
    fabi => clm3%g%l%c%p%pps%fabi
    ftdd => clm3%g%l%c%p%pps%ftdd
    ftid => clm3%g%l%c%p%pps%ftid
    ftii => clm3%g%l%c%p%pps%ftii
    fsun => clm3%g%l%c%p%pps%fsun
    elai => clm3%g%l%c%p%pps%elai
    esai => clm3%g%l%c%p%pps%esai
    gdir => clm3%g%l%c%p%pps%gdir
    omega => clm3%g%l%c%p%pps%omega
    ivt => clm3%g%l%c%p%itype
    rhol => pftcon%rhol
    rhos => pftcon%rhos
    taul => pftcon%taul
    taus => pftcon%taus
    do g = lbg, ubg
       coszen_gcell(g) = shr_orb_cosz (nextsw_cday, lat(g), lon(g), declinp1)
    end do
    do c = lbc,ubc
       g = cgridcell(c)
       coszen_col(c) = coszen_gcell(g)
       coszen(c) = coszen_col(c)
       decl(c) = declinp1
    end do
    do fp = 1,num_nourbanp
       p = filter_nourbanp(fp)
       g = pgridcell(p)
       coszen_pft(p) = coszen_gcell(g)
    end do
    do ib = 1, numrad
       do fc = 1,num_nourbanc
          c = filter_nourbanc(fc)
          albgrd(c,ib) = 0._r8
          albgri(c,ib) = 0._r8
          albgrd_pur(c,ib) = 0._r8
          albgri_pur(c,ib) = 0._r8
          albgrd_bc(c,ib) = 0._r8
          albgri_bc(c,ib) = 0._r8
          albgrd_oc(c,ib) = 0._r8
          albgri_oc(c,ib) = 0._r8
          albgrd_dst(c,ib) = 0._r8
          albgri_dst(c,ib) = 0._r8
          do i=-nlevsno+1,1,1
             flx_absdv(c,i) = 0._r8
             flx_absdn(c,i) = 0._r8
             flx_absiv(c,i) = 0._r8
             flx_absin(c,i) = 0._r8
          enddo
       end do
       do fp = 1,num_nourbanp
          p = filter_nourbanp(fp)
          albd(p,ib) = 0.999_r8
          albi(p,ib) = 0.999_r8
          fabd(p,ib) = 0._r8
          fabi(p,ib) = 0._r8
          ftdd(p,ib) = 0._r8
          ftid(p,ib) = 0._r8
          ftii(p,ib) = 0._r8
          omega(p,ib)= 0._r8
          if (ib==1) then
             gdir(p) = 0._r8
          end if
       end do
    end do
    call SoilAlbedo(lbc, ubc, num_nourbanc, filter_nourbanc, &
                    coszen_col, albsnd, albsni)
    flg_snw_ice = 1
    do c=lbc,ubc
       albsfc(c,:) = albsoi(c,:)
       h2osno_liq(c,:) = h2osoi_liq(c,-nlevsno+1:0)
       h2osno_ice(c,:) = h2osoi_ice(c,-nlevsno+1:0)
       snw_rds_in(c,:) = nint(snw_rds(c,:))
       mss_cnc_aer_in_frc_pur(c,:,:) = 0._r8
       mss_cnc_aer_in_frc_bc(c,:,:) = 0._r8
       mss_cnc_aer_in_frc_oc(c,:,:) = 0._r8
       mss_cnc_aer_in_frc_dst(c,:,:) = 0._r8
       mss_cnc_aer_in_fdb(c,:,:) = 0._r8
    end do
    if (DO_SNO_AER) then
       mss_cnc_aer_in_fdb(lbc:ubc,:,1) = mss_cnc_bcphi(lbc:ubc,:)
       mss_cnc_aer_in_fdb(lbc:ubc,:,2) = mss_cnc_bcpho(lbc:ubc,:)
       if (DO_SNO_OC) then
          mss_cnc_aer_in_fdb(lbc:ubc,:,3) = mss_cnc_ocphi(lbc:ubc,:)
          mss_cnc_aer_in_fdb(lbc:ubc,:,4) = mss_cnc_ocpho(lbc:ubc,:)
       endif
       mss_cnc_aer_in_fdb(lbc:ubc,:,5) = mss_cnc_dst1(lbc:ubc,:)
       mss_cnc_aer_in_fdb(lbc:ubc,:,6) = mss_cnc_dst2(lbc:ubc,:)
       mss_cnc_aer_in_fdb(lbc:ubc,:,7) = mss_cnc_dst3(lbc:ubc,:)
       mss_cnc_aer_in_fdb(lbc:ubc,:,8) = mss_cnc_dst4(lbc:ubc,:)
    endif
    flg_slr = 1;
    call SNICAR_RT(flg_snw_ice, lbc, ubc, num_nourbanc, filter_nourbanc, &
                   coszen_col, flg_slr, h2osno_liq, h2osno_ice, snw_rds_in, &
                   mss_cnc_aer_in_fdb, albsfc, albsnd, flx_absd_snw)
    flg_slr = 2;
    call SNICAR_RT(flg_snw_ice, lbc, ubc, num_nourbanc, filter_nourbanc, &
                   coszen_col, flg_slr, h2osno_liq, h2osno_ice, snw_rds_in, &
                   mss_cnc_aer_in_fdb, albsfc, albsni, flx_absi_snw)
    do ib = 1, nband
       do fc = 1,num_nourbanc
          c = filter_nourbanc(fc)
          if (coszen(c) > 0._r8) then
             albgrd(c,ib) = albsod(c,ib)*(1._r8-frac_sno(c)) + albsnd(c,ib)*frac_sno(c)
             albgri(c,ib) = albsoi(c,ib)*(1._r8-frac_sno(c)) + albsni(c,ib)*frac_sno(c)
             do i = -nlevsno+1,1,1
                if (ib == 1) then
                   flx_absdv(c,i) = flx_absd_snw(c,i,ib)*frac_sno(c) + &
                        ((1.-frac_sno(c))*(1-albsod(c,ib))*(flx_absd_snw(c,i,ib)/(1.-albsnd(c,ib))))
                   flx_absiv(c,i) = flx_absi_snw(c,i,ib)*frac_sno(c) + &
                        ((1.-frac_sno(c))*(1-albsoi(c,ib))*(flx_absi_snw(c,i,ib)/(1.-albsni(c,ib))))
                elseif (ib == 2) then
                   flx_absdn(c,i) = flx_absd_snw(c,i,ib)*frac_sno(c) + &
                        ((1.-frac_sno(c))*(1-albsod(c,ib))*(flx_absd_snw(c,i,ib)/(1.-albsnd(c,ib))))
                   flx_absin(c,i) = flx_absi_snw(c,i,ib)*frac_sno(c) + &
                        ((1.-frac_sno(c))*(1-albsoi(c,ib))*(flx_absi_snw(c,i,ib)/(1.-albsni(c,ib))))
                endif
             enddo
          endif
       enddo
    enddo
    do ib = 1, nband
       do fc = 1,num_nourbanc
          c = filter_nourbanc(fc)
          if ((coszen(c) > 0._r8) .and. (h2osno(c) > 0._r8)) then
             albsnd_hst(c,ib) = albsnd(c,ib)
             albsni_hst(c,ib) = albsni(c,ib)
          else
             albsnd_hst(c,ib) = 0._r8
             albsni_hst(c,ib) = 0._r8
          endif
       enddo
    enddo
    num_vegsol = 0
    num_novegsol = 0
    do fp = 1,num_nourbanp
       p = filter_nourbanp(fp)
         if (coszen_pft(p) > 0._r8) then
             if (itypelun(plandunit(p)) == istsoil &
                 .and. (elai(p) + esai(p)) > 0._r8 &
                 .and. pwtgcell(p) > 0._r8) then
                num_vegsol = num_vegsol + 1
                filter_vegsol(num_vegsol) = p
             else
                num_novegsol = num_novegsol + 1
                filter_novegsol(num_novegsol) = p
             end if
          end if
    end do
    do fp = 1,num_vegsol
       p = filter_vegsol(fp)
       vai(p) = elai(p) + esai(p)
       wl(p) = elai(p) / max( vai(p), mpe )
       ws(p) = esai(p) / max( vai(p), mpe )
    end do
    do ib = 1, numrad
       do fp = 1,num_vegsol
          p = filter_vegsol(fp)
          rho(p,ib) = max( rhol(ivt(p),ib)*wl(p) + rhos(ivt(p),ib)*ws(p), mpe )
          tau(p,ib) = max( taul(ivt(p),ib)*wl(p) + taus(ivt(p),ib)*ws(p), mpe )
       end do
    end do
    call TwoStream (lbc, ubc, lbp, ubp, filter_vegsol, num_vegsol, &
                    coszen_pft, vai, rho, tau)
    do ib = 1,numrad
       do fp = 1,num_novegsol
          p = filter_novegsol(fp)
          c = pcolumn(p)
          fabd(p,ib) = 0._r8
          fabi(p,ib) = 0._r8
          ftdd(p,ib) = 1._r8
          ftid(p,ib) = 0._r8
          ftii(p,ib) = 1._r8
          albd(p,ib) = albgrd(c,ib)
          albi(p,ib) = albgri(c,ib)
          gdir(p) = 0._r8
       end do
    end do
  end subroutine SurfaceAlbedo
  subroutine SoilAlbedo (lbc, ubc, num_nourbanc, filter_nourbanc, coszen, albsnd, albsni)
    use clmtype
    use clm_varpar, only : numrad
    use clm_varcon, only : albsat, albdry, alblak, albice, tfrz, istice
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_nourbanc
    integer , intent(in) :: filter_nourbanc(ubc-lbc+1)
    real(r8), intent(in) :: coszen(lbc:ubc)
    real(r8), intent(in) :: albsnd(lbc:ubc,numrad)
    real(r8), intent(in) :: albsni(lbc:ubc,numrad)
    integer , pointer :: clandunit(:)
    integer , pointer :: ltype(:)
    integer , pointer :: isoicol(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: frac_sno(:)
    real(r8), pointer :: h2osoi_vol(:,:)
    real(r8), pointer:: albgrd(:,:)
    real(r8), pointer:: albgri(:,:)
    real(r8), pointer :: albsod(:,:)
    real(r8), pointer :: albsoi(:,:)
    integer, parameter :: nband =numrad
    integer :: fc
    integer :: c,l
    integer :: ib
    real(r8) :: inc
    integer :: soilcol
!dir$ inlinenever SoilAlbedo
    clandunit => clm3%g%l%c%landunit
    isoicol => clm3%g%l%c%cps%isoicol
    t_grnd => clm3%g%l%c%ces%t_grnd
    frac_sno => clm3%g%l%c%cps%frac_sno
    h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
    albgrd => clm3%g%l%c%cps%albgrd
    albgri => clm3%g%l%c%cps%albgri
    albsod => clm3%g%l%c%cps%albsod
    albsoi => clm3%g%l%c%cps%albsoi
    ltype => clm3%g%l%itype
    do ib = 1, nband
       do fc = 1,num_nourbanc
          c = filter_nourbanc(fc)
          if (coszen(c) > 0._r8) then
             l = clandunit(c)
             if (ltype(l) == istsoil) then
                inc = max(0.11_r8-0.40_r8*h2osoi_vol(c,1), 0._r8)
                soilcol = isoicol(c)
                albsod(c,ib) = min(albsat(soilcol,ib)+inc, albdry(soilcol,ib))
                albsoi(c,ib) = albsod(c,ib)
             else if (ltype(l) == istice) then
                albsod(c,ib) = albice(ib)
                albsoi(c,ib) = albsod(c,ib)
             else if (t_grnd(c) > tfrz) then
                albsod(c,ib) = 0.05_r8/(max(0.001_r8,coszen(c)) + 0.15_r8)
                albsoi(c,ib) = albsod(c,ib)
             else
                albsod(c,ib) = alblak(ib)
                albsoi(c,ib) = albsod(c,ib)
             end if
          end if
       end do
    end do
  end subroutine SoilAlbedo
  subroutine TwoStream (lbc, ubc, lbp, ubp, filter_vegsol, num_vegsol, &
                        coszen, vai, rho, tau)
    use clmtype
    use clm_varpar, only : numrad
    use clm_varcon, only : omegas, tfrz, betads, betais
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: filter_vegsol(ubp-lbp+1)
    integer , intent(in) :: num_vegsol
    real(r8), intent(in) :: coszen(lbp:ubp)
    real(r8), intent(in) :: vai(lbp:ubp)
    real(r8), intent(in) :: rho(lbp:ubp,numrad)
    real(r8), intent(in) :: tau(lbp:ubp,numrad)
    integer , pointer :: pcolumn(:)
    real(r8), pointer :: albgrd(:,:)
    real(r8), pointer :: albgri(:,:)
    real(r8), pointer :: t_veg(:)
    real(r8), pointer :: fwet(:)
    integer , pointer :: ivt(:)
    real(r8), pointer :: xl(:)
    real(r8), pointer :: albd(:,:)
    real(r8), pointer :: albi(:,:)
    real(r8), pointer :: fabd(:,:)
    real(r8), pointer :: fabi(:,:)
    real(r8), pointer :: ftdd(:,:)
    real(r8), pointer :: ftid(:,:)
    real(r8), pointer :: ftii(:,:)
    real(r8), pointer :: gdir(:)
  real(r8), pointer :: omega(:,:)
    integer :: fp,p,c
    integer :: ib
    real(r8) :: cosz
    real(r8) :: asu
    real(r8) :: chil(lbp:ubp)
    real(r8) :: twostext(lbp:ubp)
    real(r8) :: avmu(lbp:ubp)
    real(r8) :: omegal
    real(r8) :: betai
    real(r8) :: betail
    real(r8) :: betad
    real(r8) :: betadl
    real(r8) :: tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9
    real(r8) :: p1,p2,p3,p4,s1,s2,u1,u2,u3
    real(r8) :: b,c1,d,d1,d2,f,h,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10
    real(r8) :: phi1,phi2,sigma
    real(r8) :: temp0(lbp:ubp),temp1,temp2(lbp:ubp)
    real(r8) :: t1
    albgrd => clm3%g%l%c%cps%albgrd
    albgri => clm3%g%l%c%cps%albgri
    pcolumn => clm3%g%l%c%p%column
    fwet => clm3%g%l%c%p%pps%fwet
    t_veg => clm3%g%l%c%p%pes%t_veg
    ivt => clm3%g%l%c%p%itype
    albd => clm3%g%l%c%p%pps%albd
    albi => clm3%g%l%c%p%pps%albi
    fabd => clm3%g%l%c%p%pps%fabd
    fabi => clm3%g%l%c%p%pps%fabi
    ftdd => clm3%g%l%c%p%pps%ftdd
    ftid => clm3%g%l%c%p%pps%ftid
    ftii => clm3%g%l%c%p%pps%ftii
    gdir => clm3%g%l%c%p%pps%gdir
    omega => clm3%g%l%c%p%pps%omega
    xl => pftcon%xl
    do fp = 1,num_vegsol
       p = filter_vegsol(fp)
       cosz = max(0.001_r8, coszen(p))
       chil(p) = min( max(xl(ivt(p)), -0.4_r8), 0.6_r8 )
       if (abs(chil(p)) <= 0.01_r8) chil(p) = 0.01_r8
       phi1 = 0.5_r8 - 0.633_r8*chil(p) - 0.330_r8*chil(p)*chil(p)
       phi2 = 0.877_r8 * (1._r8-2._r8*phi1)
       gdir(p) = phi1 + phi2*cosz
       twostext(p) = gdir(p)/cosz
       avmu(p) = ( 1._r8 - phi1/phi2 * log((phi1+phi2)/phi1) ) / phi2
       temp0(p) = gdir(p) + phi2*cosz
       temp1 = phi1*cosz
       temp2(p) = ( 1._r8 - temp1/temp0(p) * log((temp1+temp0(p))/temp1) )
    end do
    do ib = 1, numrad
       do fp = 1,num_vegsol
          p = filter_vegsol(fp)
          c = pcolumn(p)
          omegal = rho(p,ib) + tau(p,ib)
          asu = 0.5_r8*omegal*gdir(p)/temp0(p) *temp2(p)
          betadl = (1._r8+avmu(p)*twostext(p))/(omegal*avmu(p)*twostext(p))*asu
          betail = 0.5_r8 * ((rho(p,ib)+tau(p,ib)) + (rho(p,ib)-tau(p,ib)) &
               * ((1._r8+chil(p))/2._r8)**2) / omegal
          if (t_veg(p) > tfrz) then
             tmp0 = omegal
             tmp1 = betadl
             tmp2 = betail
          else
             tmp0 = (1._r8-fwet(p))*omegal + fwet(p)*omegas(ib)
             tmp1 = ( (1._r8-fwet(p))*omegal*betadl + fwet(p)*omegas(ib)*betads ) / tmp0
             tmp2 = ( (1._r8-fwet(p))*omegal*betail + fwet(p)*omegas(ib)*betais ) / tmp0
          end if
          omega(p,ib) = tmp0
          betad = tmp1
          betai = tmp2
          b = 1._r8 - omega(p,ib) + omega(p,ib)*betai
          c1 = omega(p,ib)*betai
          tmp0 = avmu(p)*twostext(p)
          d = tmp0 * omega(p,ib)*betad
          f = tmp0 * omega(p,ib)*(1._r8-betad)
          tmp1 = b*b - c1*c1
          h = sqrt(tmp1) / avmu(p)
          sigma = tmp0*tmp0 - tmp1
          p1 = b + avmu(p)*h
          p2 = b - avmu(p)*h
          p3 = b + tmp0
          p4 = b - tmp0
          t1 = min(h*vai(p), 40._r8)
          s1 = exp(-t1)
          t1 = min(twostext(p)*vai(p), 40._r8)
          s2 = exp(-t1)
          u1 = b - c1/albgrd(c,ib)
          u2 = b - c1*albgrd(c,ib)
          u3 = f + c1*albgrd(c,ib)
          tmp2 = u1 - avmu(p)*h
          tmp3 = u1 + avmu(p)*h
          d1 = p1*tmp2/s1 - p2*tmp3*s1
          tmp4 = u2 + avmu(p)*h
          tmp5 = u2 - avmu(p)*h
          d2 = tmp4/s1 - tmp5*s1
          h1 = -d*p4 - c1*f
          tmp6 = d - h1*p3/sigma
          tmp7 = ( d - c1 - h1/sigma*(u1+tmp0) ) * s2
          h2 = ( tmp6*tmp2/s1 - p2*tmp7 ) / d1
          h3 = - ( tmp6*tmp3*s1 - p1*tmp7 ) / d1
          h4 = -f*p3 - c1*d
          tmp8 = h4/sigma
          tmp9 = ( u3 - tmp8*(u2-tmp0) ) * s2
          h5 = - ( tmp8*tmp4/s1 + tmp9 ) / d2
          h6 = ( tmp8*tmp5*s1 + tmp9 ) / d2
          h7 = (c1*tmp2) / (d1*s1)
          h8 = (-c1*tmp3*s1) / d1
          h9 = tmp4 / (d2*s1)
          h10 = (-tmp5*s1) / d2
          ftdd(p,ib) = s2
          ftid(p,ib) = h4*s2/sigma + h5*s1 + h6/s1
          albd(p,ib) = h1/sigma + h2 + h3
          fabd(p,ib) = 1._r8 - albd(p,ib) &
               - (1._r8-albgrd(c,ib))*ftdd(p,ib) - (1._r8-albgri(c,ib))*ftid(p,ib)
          u1 = b - c1/albgri(c,ib)
          u2 = b - c1*albgri(c,ib)
          u3 = f + c1*albgri(c,ib)
          tmp2 = u1 - avmu(p)*h
          tmp3 = u1 + avmu(p)*h
          d1 = p1*tmp2/s1 - p2*tmp3*s1
          tmp4 = u2 + avmu(p)*h
          tmp5 = u2 - avmu(p)*h
          d2 = tmp4/s1 - tmp5*s1
          h1 = -d*p4 - c1*f
          tmp6 = d - h1*p3/sigma
          tmp7 = ( d - c1 - h1/sigma*(u1+tmp0) ) * s2
          h2 = ( tmp6*tmp2/s1 - p2*tmp7 ) / d1
          h3 = - ( tmp6*tmp3*s1 - p1*tmp7 ) / d1
          h4 = -f*p3 - c1*d
          tmp8 = h4/sigma
          tmp9 = ( u3 - tmp8*(u2-tmp0) ) * s2
          h5 = - ( tmp8*tmp4/s1 + tmp9 ) / d2
          h6 = ( tmp8*tmp5*s1 + tmp9 ) / d2
          h7 = (c1*tmp2) / (d1*s1)
          h8 = (-c1*tmp3*s1) / d1
          h9 = tmp4 / (d2*s1)
          h10 = (-tmp5*s1) / d2
          ftii(p,ib) = h9*s1 + h10/s1
          albi(p,ib) = h7 + h8
          fabi(p,ib) = 1._r8 - albi(p,ib) - (1._r8-albgri(c,ib))*ftii(p,ib)
       end do
    end do
  end subroutine TwoStream
end module SurfaceAlbedoMod
module SoilTemperatureMod
  implicit none
  save
  public :: SoilTemperature
  private :: SoilThermProp
  private :: PhaseChange
contains
  subroutine SoilTemperature(lbl, ubl, lbc, ubc, num_urbanl, filter_urbanl, &
                             num_nolakec, filter_nolakec, xmf, fact)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : sb, capr, cnfac, hvap, isturb, &
                               icol_roof, icol_sunwall, icol_shadewall, &
                               icol_road_perv, icol_road_imperv, istwet
    use clm_varpar , only : nlevsno, nlevgrnd, max_pft_per_col, nlevurb
    use TridiagonalMod, only : Tridiagonal
    use globals , only : dtime
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_nolakec
    integer , intent(in) :: filter_nolakec(ubc-lbc+1)
    integer , intent(in) :: lbl, ubl
    integer , intent(in) :: num_urbanl
    integer , intent(in) :: filter_urbanl(ubl-lbl+1)
    real(r8), intent(out) :: xmf(lbc:ubc)
    real(r8), intent(out) :: fact(lbc:ubc, -nlevsno+1:nlevgrnd)
    integer , pointer :: pgridcell(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ltype(:)
    integer , pointer :: ctype(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    real(r8), pointer :: pwtcol(:)
    real(r8), pointer :: pwtgcell(:)
    real(r8), pointer :: forc_lwrad(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: htvp(:)
    real(r8), pointer :: emg(:)
    real(r8), pointer :: cgrnd(:)
    real(r8), pointer :: dlrad(:)
    real(r8), pointer :: sabg(:)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: eflx_sh_grnd(:)
    real(r8), pointer :: qflx_evap_soi(:)
    real(r8), pointer :: qflx_tran_veg(:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: eflx_lwrad_net(:)
    real(r8), pointer :: tssbef(:,:)
    real(r8), pointer :: t_building(:)
    real(r8), pointer :: t_building_max(:)
    real(r8), pointer :: t_building_min(:)
    real(r8), pointer :: hc_soi(:)
    real(r8), pointer :: hc_soisno(:)
    real(r8), pointer :: eflx_fgr12(:)
    real(r8), pointer :: eflx_traffic(:)
    real(r8), pointer :: eflx_wasteheat(:)
    real(r8), pointer :: eflx_wasteheat_pft(:)
    real(r8), pointer :: eflx_heat_from_ac(:)
    real(r8), pointer :: eflx_heat_from_ac_pft(:)
    real(r8), pointer :: eflx_traffic_pft(:)
    real(r8), pointer :: eflx_anthro(:)
    real(r8), pointer :: canyon_hwr(:)
    real(r8), pointer :: wtlunit_roof(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: eflx_gnet(:)
    real(r8), pointer :: dgnetdT(:)
    real(r8), pointer :: eflx_building_heat(:)
    real(r8), pointer :: sabg_lyr(:,:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: eflx_urban_ac(:)
    real(r8), pointer :: eflx_urban_heat(:)
    integer :: j,c,p,l,g,pi
    integer :: fc
    integer :: fl
    integer :: jtop(lbc:ubc)
    real(r8) :: at (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: bt (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: ct (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: rt (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: cv (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: tk (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: fn (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: fn1(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: brr(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: dzm
    real(r8) :: dzp
    real(r8) :: hs(lbc:ubc)
    real(r8) :: dhsdT(lbc:ubc)
    real(r8) :: lwrad_emit(lbc:ubc)
    real(r8) :: dlwrad_emit(lbc:ubc)
    integer :: lyr_top
    real(r8) :: sabg_lyr_col(lbc:ubc,-nlevsno+1:1)
    real(r8) :: eflx_gnet_top
    real(r8) :: hs_top(lbc:ubc)
    logical :: cool_on(lbl:ubl)
    logical :: heat_on(lbl:ubl)
    forc_lwrad => clm_a2l%forc_lwrad
    ltype => clm3%g%l%itype
    t_building => clm3%g%l%lps%t_building
    t_building_max => clm3%g%l%lps%t_building_max
    t_building_min => clm3%g%l%lps%t_building_min
    eflx_traffic => clm3%g%l%lef%eflx_traffic
    canyon_hwr => clm3%g%l%canyon_hwr
    eflx_wasteheat => clm3%g%l%lef%eflx_wasteheat
    eflx_heat_from_ac => clm3%g%l%lef%eflx_heat_from_ac
    wtlunit_roof => clm3%g%l%wtlunit_roof
    ctype => clm3%g%l%c%itype
    clandunit => clm3%g%l%c%landunit
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    snl => clm3%g%l%c%cps%snl
    htvp => clm3%g%l%c%cps%htvp
    emg => clm3%g%l%c%cps%emg
    t_grnd => clm3%g%l%c%ces%t_grnd
    hc_soi => clm3%g%l%c%ces%hc_soi
    hc_soisno => clm3%g%l%c%ces%hc_soisno
    eflx_fgr12 => clm3%g%l%c%cef%eflx_fgr12
    zi => clm3%g%l%c%cps%zi
    dz => clm3%g%l%c%cps%dz
    z => clm3%g%l%c%cps%z
    t_soisno => clm3%g%l%c%ces%t_soisno
    eflx_building_heat => clm3%g%l%c%cef%eflx_building_heat
    tssbef => clm3%g%l%c%ces%tssbef
    eflx_urban_ac => clm3%g%l%c%cef%eflx_urban_ac
    eflx_urban_heat => clm3%g%l%c%cef%eflx_urban_heat
    pgridcell => clm3%g%l%c%p%gridcell
    plandunit => clm3%g%l%c%p%landunit
    pwtcol => clm3%g%l%c%p%wtcol
    pwtgcell => clm3%g%l%c%p%wtgcell
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    cgrnd => clm3%g%l%c%p%pef%cgrnd
    dlrad => clm3%g%l%c%p%pef%dlrad
    sabg => clm3%g%l%c%p%pef%sabg
    eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    qflx_tran_veg => clm3%g%l%c%p%pwf%qflx_tran_veg
    eflx_gnet => clm3%g%l%c%p%pef%eflx_gnet
    dgnetdT => clm3%g%l%c%p%pef%dgnetdT
    eflx_lwrad_net => clm3%g%l%c%p%pef%eflx_lwrad_net
    eflx_wasteheat_pft => clm3%g%l%c%p%pef%eflx_wasteheat_pft
    eflx_heat_from_ac_pft => clm3%g%l%c%p%pef%eflx_heat_from_ac_pft
    eflx_traffic_pft => clm3%g%l%c%p%pef%eflx_traffic_pft
    eflx_anthro => clm3%g%l%c%p%pef%eflx_anthro
    sabg_lyr => clm3%g%l%c%p%pef%sabg_lyr
    h2osno => clm3%g%l%c%cws%h2osno
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    call SoilThermProp(lbc, ubc, num_nolakec, filter_nolakec, tk, cv)
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       lwrad_emit(c) = emg(c) * sb * t_grnd(c)**4
       dlwrad_emit(c) = 4._r8*emg(c) * sb * t_grnd(c)**3
    end do
    hs(lbc:ubc) = 0._r8
    dhsdT(lbc:ubc) = 0._r8
    do pi = 1,max_pft_per_col
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if ( pi <= npfts(c) ) then
             p = pfti(c) + pi - 1
             l = plandunit(p)
             g = pgridcell(p)
             if (pwtgcell(p)>0._r8) then
                if (ltype(l) /= isturb) then
                   eflx_gnet(p) = sabg(p) + dlrad(p) &
                                  + (1-frac_veg_nosno(p))*emg(c)*forc_lwrad(g) - lwrad_emit(c) &
                                  - (eflx_sh_grnd(p)+qflx_evap_soi(p)*htvp(c))
                else
                   if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                      eflx_wasteheat_pft(p) = eflx_wasteheat(l)/(1._r8-wtlunit_roof(l))
                      eflx_heat_from_ac_pft(p) = eflx_heat_from_ac(l)/(1._r8-wtlunit_roof(l))
                      eflx_traffic_pft(p) = eflx_traffic(l)/(1._r8-wtlunit_roof(l))
                   else
                      eflx_wasteheat_pft(p) = 0._r8
                      eflx_heat_from_ac_pft(p) = 0._r8
                      eflx_traffic_pft(p) = 0._r8
                   end if
                   eflx_gnet(p) = sabg(p) + dlrad(p) &
                                  - eflx_lwrad_net(p) &
                                  - (eflx_sh_grnd(p) + qflx_evap_soi(p)*htvp(c) + qflx_tran_veg(p)*hvap) &
                                  + eflx_wasteheat_pft(p) + eflx_heat_from_ac_pft(p) + eflx_traffic_pft(p)
                   eflx_anthro(p) = eflx_wasteheat_pft(p) + eflx_traffic_pft(p)
                end if
                dgnetdT(p) = - cgrnd(p) - dlwrad_emit(c)
                hs(c) = hs(c) + eflx_gnet(p) * pwtcol(p)
                dhsdT(c) = dhsdT(c) + dgnetdT(p) * pwtcol(p)
             end if
          end if
       end do
    end do
    sabg_lyr_col(lbc:ubc,-nlevsno+1:1) = 0._r8
    hs_top(lbc:ubc) = 0._r8
    do pi = 1,max_pft_per_col
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          lyr_top = snl(c) + 1
          if ( pi <= npfts(c) ) then
             p = pfti(c) + pi - 1
             l = plandunit(p)
             if (pwtgcell(p)>0._r8) then
                g = pgridcell(p)
                if (ltype(l) /= isturb )then
                   eflx_gnet_top = sabg_lyr(p,lyr_top) + dlrad(p) + (1-frac_veg_nosno(p))*emg(c)*forc_lwrad(g) &
                        - lwrad_emit(c) - (eflx_sh_grnd(p)+qflx_evap_soi(p)*htvp(c))
                   hs_top(c) = hs_top(c) + eflx_gnet_top*pwtcol(p)
                   do j = lyr_top,1,1
                      sabg_lyr_col(c,j) = sabg_lyr_col(c,j) + sabg_lyr(p,j) * pwtcol(p)
                   enddo
                else
                   hs_top(c) = hs_top(c) + eflx_gnet(p)*pwtcol(p)
                   sabg_lyr_col(c,lyr_top) = sabg_lyr_col(c,lyr_top) + sabg(p) * pwtcol(p)
                endif
             endif
          endif
       enddo
    enddo
    do fl = 1,num_urbanl
       l = filter_urbanl(fl)
       if (ltype(l) == isturb) then
          cool_on(l) = .false.
          heat_on(l) = .false.
          if (t_building(l) > t_building_max(l)) then
            t_building(l) = t_building_max(l)
            cool_on(l) = .true.
            heat_on(l) = .false.
          else if (t_building(l) < t_building_min(l)) then
            t_building(l) = t_building_min(l)
            cool_on(l) = .false.
            heat_on(l) = .true.
          end if
       end if
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (j >= snl(c)+1) then
             if (j == snl(c)+1) then
                if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall .or. ctype(c)==icol_roof) then
                  fact(c,j) = dtime/cv(c,j)
                else
                  fact(c,j) = dtime/cv(c,j) * dz(c,j) / (0.5_r8*(z(c,j)-zi(c,j-1)+capr*(z(c,j+1)-zi(c,j-1))))
             end if
                fn(c,j) = tk(c,j)*(t_soisno(c,j+1)-t_soisno(c,j))/(z(c,j+1)-z(c,j))
             else if (j <= nlevgrnd-1) then
                fact(c,j) = dtime/cv(c,j)
                fn(c,j) = tk(c,j)*(t_soisno(c,j+1)-t_soisno(c,j))/(z(c,j+1)-z(c,j))
                dzm = (z(c,j)-z(c,j-1))
             else if (j == nlevgrnd) then
                fact(c,j) = dtime/cv(c,j)
                if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall .or. ctype(c)==icol_roof) then
                   fn(c,j) = tk(c,j) * (t_building(l) - cnfac*t_soisno(c,j))/(zi(c,j) - z(c,j))
                else
                   fn(c,j) = 0._r8
                end if
             end if
          end if
       enddo
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (j >= snl(c)+1) then
             if (j == snl(c)+1) then
                dzp = z(c,j+1)-z(c,j)
                at(c,j) = 0._r8
                bt(c,j) = 1+(1._r8-cnfac)*fact(c,j)*tk(c,j)/dzp-fact(c,j)*dhsdT(c)
                ct(c,j) = -(1._r8-cnfac)*fact(c,j)*tk(c,j)/dzp
                rt(c,j) = t_soisno(c,j) + fact(c,j)*( hs_top(c) - dhsdT(c)*t_soisno(c,j) + cnfac*fn(c,j) )
             else if (j <= nlevgrnd-1) then
                dzm = (z(c,j)-z(c,j-1))
                dzp = (z(c,j+1)-z(c,j))
                at(c,j) = - (1._r8-cnfac)*fact(c,j)* tk(c,j-1)/dzm
                bt(c,j) = 1._r8+ (1._r8-cnfac)*fact(c,j)*(tk(c,j)/dzp + tk(c,j-1)/dzm)
                ct(c,j) = - (1._r8-cnfac)*fact(c,j)* tk(c,j)/dzp
                if (j <= 1) then
                   rt(c,j) = t_soisno(c,j) + cnfac*fact(c,j)*( fn(c,j) - fn(c,j-1) ) + (fact(c,j)*sabg_lyr_col(c,j))
                else
                   rt(c,j) = t_soisno(c,j) + cnfac*fact(c,j)*( fn(c,j) - fn(c,j-1) )
                endif
             else if (j == nlevgrnd) then
                if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall .or. ctype(c)==icol_roof) then
                   dzm = ( z(c,j)-z(c,j-1))
                   dzp = (zi(c,j)-z(c,j))
                   at(c,j) = - (1._r8-cnfac)*fact(c,j)*(tk(c,j-1)/dzm)
                   bt(c,j) = 1._r8+ (1._r8-cnfac)*fact(c,j)*(tk(c,j-1)/dzm + tk(c,j)/dzp)
                   ct(c,j) = 0._r8
                   rt(c,j) = t_soisno(c,j) + fact(c,j)*( fn(c,j) - cnfac*fn(c,j-1) )
                else
                   dzm = (z(c,j)-z(c,j-1))
                   at(c,j) = - (1._r8-cnfac)*fact(c,j)*tk(c,j-1)/dzm
                   bt(c,j) = 1._r8+ (1._r8-cnfac)*fact(c,j)*tk(c,j-1)/dzm
                   ct(c,j) = 0._r8
                   rt(c,j) = t_soisno(c,j) - cnfac*fact(c,j)*fn(c,j-1)
                end if
             end if
          end if
       enddo
    end do
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       jtop(c) = snl(c) + 1
    end do
    call Tridiagonal(lbc, ubc, -nlevsno+1, nlevgrnd, jtop, num_nolakec, filter_nolakec, &
                     at, bt, ct, rt, t_soisno(lbc:ubc,-nlevsno+1:nlevgrnd))
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (j >= snl(c)+1) then
             if (j <= nlevgrnd-1) then
                fn1(c,j) = tk(c,j)*(t_soisno(c,j+1)-t_soisno(c,j))/(z(c,j+1)-z(c,j))
             else if (j == nlevgrnd) then
                if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall .or. ctype(c)==icol_roof) then
                   fn1(c,j) = tk(c,j) * (t_building(l) - t_soisno(c,j))/(zi(c,j) - z(c,j))
                   fn(c,j) = tk(c,j) * (t_building(l) - tssbef(c,j))/(zi(c,j) - z(c,j))
                else
                   fn1(c,j) = 0._r8
                end if
             end if
          end if
       end do
    end do
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       if (ltype(l) == isturb) then
         eflx_building_heat(c) = cnfac*fn(c,nlevurb) + (1-cnfac)*fn1(c,nlevurb)
         if (cool_on(l)) then
           eflx_urban_ac(c) = abs(eflx_building_heat(c))
           eflx_urban_heat(c) = 0._r8
         else if (heat_on(l)) then
           eflx_urban_ac(c) = 0._r8
           eflx_urban_heat(c) = abs(eflx_building_heat(c))
         else
           eflx_urban_ac(c) = 0._r8
           eflx_urban_heat(c) = 0._r8
         end if
       end if
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ prefervector
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (j >= snl(c)+1) then
             if (j == snl(c)+1) then
                brr(c,j) = cnfac*fn(c,j) + (1._r8-cnfac)*fn1(c,j)
             else
                brr(c,j) = cnfac*(fn(c,j)-fn(c,j-1)) + (1._r8-cnfac)*(fn1(c,j)-fn1(c,j-1))
             end if
          end if
       end do
    end do
    call PhaseChange (lbc, ubc, num_nolakec, filter_nolakec, fact, brr, hs, dhsdT, xmf, hs_top, sabg_lyr_col)
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       t_grnd(c) = t_soisno(c,snl(c)+1)
    end do
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       if (ltype(l) /= isturb) then
         hc_soisno(c) = 0._r8
         hc_soi(c) = 0._r8
       end if
       eflx_fgr12(c)= 0._r8
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ prefervector
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          eflx_fgr12(c) = -cnfac*fn(c,1) - (1._r8-cnfac)*fn1(c,1)
          if (ltype(l) /= isturb) then
            if (j >= snl(c)+1) then
               hc_soisno(c) = hc_soisno(c) + cv(c,j)*t_soisno(c,j) / 1.e6_r8
            endif
            if (j >= 1) then
               hc_soi(c) = hc_soi(c) + cv(c,j)*t_soisno(c,j) / 1.e6_r8
            end if
          end if
       end do
    end do
  end subroutine SoilTemperature
  subroutine SoilThermProp (lbc, ubc, num_nolakec, filter_nolakec, tk, cv)
    use shr_kind_mod, only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : denh2o, denice, tfrz, tkwat, tkice, tkair, &
                             cpice, cpliq, istice, istwet, &
                             icol_roof, icol_sunwall, icol_shadewall, &
                             icol_road_perv, icol_road_imperv
    use clm_varpar , only : nlevsno, nlevgrnd, nlevurb, nlevsoi
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_nolakec
    integer , intent(in) :: filter_nolakec(ubc-lbc+1)
    real(r8), intent(out) :: cv(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8), intent(out) :: tk(lbc:ubc,-nlevsno+1:nlevgrnd)
    integer , pointer :: ctype(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ltype(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: tksatu(:,:)
    real(r8), pointer :: tkmg(:,:)
    real(r8), pointer :: tkdry(:,:)
    real(r8), pointer :: csol(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: tk_wall(:,:)
    real(r8), pointer :: tk_roof(:,:)
    real(r8), pointer :: tk_improad(:,:)
    real(r8), pointer :: cv_wall(:,:)
    real(r8), pointer :: cv_roof(:,:)
    real(r8), pointer :: cv_improad(:,:)
    integer, pointer :: nlev_improad(:)
    integer :: l,c,j
    integer :: fc
    real(r8) :: bw
    real(r8) :: dksat
    real(r8) :: dke
    real(r8) :: fl
    real(r8) :: satw
    real(r8) :: thk(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: thk_bedrock = 3.0_r8
    ltype => clm3%g%l%itype
    ctype => clm3%g%l%c%itype
    clandunit => clm3%g%l%c%landunit
    snl => clm3%g%l%c%cps%snl
    h2osno => clm3%g%l%c%cws%h2osno
    watsat => clm3%g%l%c%cps%watsat
    tksatu => clm3%g%l%c%cps%tksatu
    tkmg => clm3%g%l%c%cps%tkmg
    tkdry => clm3%g%l%c%cps%tkdry
    csol => clm3%g%l%c%cps%csol
    dz => clm3%g%l%c%cps%dz
    zi => clm3%g%l%c%cps%zi
    z => clm3%g%l%c%cps%z
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    tk_wall => clm3%g%l%lps%tk_wall
    tk_roof => clm3%g%l%lps%tk_roof
    tk_improad => clm3%g%l%lps%tk_improad
    cv_wall => clm3%g%l%lps%cv_wall
    cv_roof => clm3%g%l%lps%cv_roof
    cv_improad => clm3%g%l%lps%cv_improad
    nlev_improad => clm3%g%l%lps%nlev_improad
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          if (j >= 1) then
             l = clandunit(c)
             if (ctype(c) == icol_sunwall .OR. ctype(c) == icol_shadewall) then
                thk(c,j) = tk_wall(l,j)
             else if (ctype(c) == icol_roof) then
                thk(c,j) = tk_roof(l,j)
             else if (ctype(c) == icol_road_imperv .and. j >= 1 .and. j <= nlev_improad(l)) then
                thk(c,j) = tk_improad(l,j)
             else if (ltype(l) /= istwet .AND. ltype(l) /= istice) then
                satw = (h2osoi_liq(c,j)/denh2o + h2osoi_ice(c,j)/denice)/(dz(c,j)*watsat(c,j))
                satw = min(1._r8, satw)
                if (satw > .1e-6_r8) then
                   fl = h2osoi_liq(c,j)/(h2osoi_ice(c,j)+h2osoi_liq(c,j))
                   if (t_soisno(c,j) >= tfrz) then
                      dke = max(0._r8, log10(satw) + 1.0_r8)
                      dksat = tksatu(c,j)
                   else
                      dke = satw
                      dksat = tkmg(c,j)*0.249_r8**(fl*watsat(c,j))*2.29_r8**watsat(c,j)
                   endif
                   thk(c,j) = dke*dksat + (1._r8-dke)*tkdry(c,j)
                else
                   thk(c,j) = tkdry(c,j)
                endif
                if (j > nlevsoi) thk(c,j) = thk_bedrock
             else if (ltype(l) == istice) then
                thk(c,j) = tkwat
                if (t_soisno(c,j) < tfrz) thk(c,j) = tkice
             else if (ltype(l) == istwet) then
                if (j > nlevsoi) then
                   thk(c,j) = thk_bedrock
                else
                   thk(c,j) = tkwat
                   if (t_soisno(c,j) < tfrz) thk(c,j) = tkice
                endif
             endif
          endif
          if (snl(c)+1 < 1 .AND. (j >= snl(c)+1) .AND. (j <= 0)) then
             bw = (h2osoi_ice(c,j)+h2osoi_liq(c,j))/dz(c,j)
             thk(c,j) = tkair + (7.75e-5_r8 *bw + 1.105e-6_r8*bw*bw)*(tkice-tkair)
          end if
       end do
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if (j >= snl(c)+1 .AND. j <= nlevgrnd-1) then
             tk(c,j) = thk(c,j)*thk(c,j+1)*(z(c,j+1)-z(c,j)) &
                       /(thk(c,j)*(z(c,j+1)-zi(c,j))+thk(c,j+1)*(zi(c,j)-z(c,j)))
          else if (j == nlevgrnd) then
             if (ctype(c)==icol_sunwall .OR. ctype(c)==icol_shadewall .OR. ctype(c)==icol_roof) then
                tk(c,j) = thk(c,j)
             else
                tk(c,j) = 0._r8
             end if
          end if
       end do
    end do
    do j = 1, nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (ctype(c)==icol_sunwall .OR. ctype(c)==icol_shadewall) then
             cv(c,j) = cv_wall(l,j) * dz(c,j)
          else if (ctype(c) == icol_roof) then
             cv(c,j) = cv_roof(l,j) * dz(c,j)
          else if (ctype(c) == icol_road_imperv .and. j >= 1 .and. j <= nlev_improad(l)) then
             cv(c,j) = cv_improad(l,j) * dz(c,j)
          else if (ltype(l) /= istwet .AND. ltype(l) /= istice) then
             cv(c,j) = csol(c,j)*(1-watsat(c,j))*dz(c,j) + (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
          else if (ltype(l) == istwet) then
             cv(c,j) = (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
             if (j > nlevsoi) cv(c,j) = csol(c,j)*dz(c,j)
          else if (ltype(l) == istice) then
             cv(c,j) = (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
          endif
          if (j == 1) then
             if (snl(c)+1 == 1 .AND. h2osno(c) > 0._r8) then
                cv(c,j) = cv(c,j) + cpice*h2osno(c)
             end if
          end if
       enddo
    end do
    do j = -nlevsno+1,0
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if (snl(c)+1 < 1 .and. j >= snl(c)+1) then
             cv(c,j) = cpliq*h2osoi_liq(c,j) + cpice*h2osoi_ice(c,j)
          end if
       end do
    end do
  end subroutine SoilThermProp
  subroutine PhaseChange (lbc, ubc, num_nolakec, filter_nolakec, fact, &
                          brr, hs, dhsdT, xmf, hs_top, sabg_lyr_col)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : tfrz, hfus, grav, istsoil, isturb, icol_road_perv
    use clm_varpar , only : nlevsno, nlevgrnd
    use globals , only : dtime
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_nolakec
    integer , intent(in) :: filter_nolakec(ubc-lbc+1)
    real(r8), intent(in) :: brr (lbc:ubc, -nlevsno+1:nlevgrnd)
    real(r8), intent(in) :: fact (lbc:ubc, -nlevsno+1:nlevgrnd)
    real(r8), intent(in) :: hs (lbc:ubc)
    real(r8), intent(in) :: dhsdT (lbc:ubc)
    real(r8), intent(out):: xmf (lbc:ubc)
    real(r8), intent(in) :: hs_top(lbc:ubc)
    real(r8), intent(in) :: sabg_lyr_col(lbc:ubc,-nlevsno+1:1)
    integer , pointer :: snl(:)
    real(r8), pointer :: h2osno(:)
    integer , pointer :: ltype(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: qflx_snomelt(:)
    real(r8), pointer :: eflx_snomelt(:)
    real(r8), pointer :: eflx_snomelt_u(:)
    real(r8), pointer :: eflx_snomelt_r(:)
    real(r8), pointer :: qflx_snofrz_lyr(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: tssbef(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: t_soisno(:,:)
    integer, pointer :: imelt(:,:)
    integer :: j,c,g,l
    integer :: fc
    real(r8) :: heatr
    real(r8) :: temp1
    real(r8) :: hm(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: xm(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: wmass0(lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: wice0 (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: wliq0 (lbc:ubc,-nlevsno+1:nlevgrnd)
    real(r8) :: supercool(lbc:ubc,nlevgrnd)
    real(r8) :: propor
    real(r8) :: tinc
    real(r8) :: smp
    snl => clm3%g%l%c%cps%snl
    h2osno => clm3%g%l%c%cws%h2osno
    snowdp => clm3%g%l%c%cps%snowdp
    qflx_snomelt => clm3%g%l%c%cwf%qflx_snomelt
    eflx_snomelt => clm3%g%l%c%cef%eflx_snomelt
    eflx_snomelt_u => clm3%g%l%c%cef%eflx_snomelt_u
    eflx_snomelt_r => clm3%g%l%c%cef%eflx_snomelt_r
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    imelt => clm3%g%l%c%cps%imelt
    t_soisno => clm3%g%l%c%ces%t_soisno
    tssbef => clm3%g%l%c%ces%tssbef
    bsw => clm3%g%l%c%cps%bsw
    sucsat => clm3%g%l%c%cps%sucsat
    watsat => clm3%g%l%c%cps%watsat
    dz => clm3%g%l%c%cps%dz
    ctype => clm3%g%l%c%itype
    clandunit => clm3%g%l%c%landunit
    ltype => clm3%g%l%itype
    qflx_snofrz_lyr => clm3%g%l%c%cwf%qflx_snofrz_lyr
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       qflx_snomelt(c) = 0._r8
       xmf(c) = 0._r8
       qflx_snofrz_lyr(c,-nlevsno+1:0) = 0._r8
    end do
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if (j >= snl(c)+1) then
             imelt(c,j) = 0
             hm(c,j) = 0._r8
             xm(c,j) = 0._r8
             wice0(c,j) = h2osoi_ice(c,j)
             wliq0(c,j) = h2osoi_liq(c,j)
             wmass0(c,j) = h2osoi_ice(c,j) + h2osoi_liq(c,j)
          endif
       end do
    enddo
    do j = -nlevsno+1,0
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if (j >= snl(c)+1) then
             if (h2osoi_ice(c,j) > 0._r8 .AND. t_soisno(c,j) > tfrz) then
                imelt(c,j) = 1
                t_soisno(c,j) = tfrz
             endif
             if (h2osoi_liq(c,j) > 0._r8 .AND. t_soisno(c,j) < tfrz) then
                imelt(c,j) = 2
                t_soisno(c,j) = tfrz
             endif
          endif
       end do
    enddo
    do j = 1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (h2osoi_ice(c,j) > 0. .AND. t_soisno(c,j) > tfrz) then
             imelt(c,j) = 1
             t_soisno(c,j) = tfrz
          endif
          supercool(c,j) = 0.0_r8
          if (ltype(l) == istsoil .or. ctype(c) == icol_road_perv) then
             if(t_soisno(c,j) < tfrz) then
                smp = hfus*(tfrz-t_soisno(c,j))/(grav*t_soisno(c,j)) * 1000._r8
                supercool(c,j) = watsat(c,j)*(smp/sucsat(c,j))**(-1._r8/bsw(c,j))
                supercool(c,j) = supercool(c,j)*dz(c,j)*1000._r8
             endif
          endif
          if (h2osoi_liq(c,j) > supercool(c,j) .AND. t_soisno(c,j) < tfrz) then
             imelt(c,j) = 2
             t_soisno(c,j) = tfrz
          endif
          if (snl(c)+1 == 1 .AND. h2osno(c) > 0._r8 .AND. j == 1) then
             if (t_soisno(c,j) > tfrz) then
                imelt(c,j) = 1
                t_soisno(c,j) = tfrz
             endif
          endif
       end do
    enddo
    do j = -nlevsno+1,nlevgrnd
!dir$ concurrent
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if (j >= snl(c)+1) then
             if (imelt(c,j) > 0) then
                tinc = t_soisno(c,j)-tssbef(c,j)
                if (j == snl(c)+1) then
                   hm(c,j) = hs_top(c) + dhsdT(c)*tinc + brr(c,j) - tinc/fact(c,j)
                elseif (j <= 1) then
                   hm(c,j) = brr(c,j) - tinc/fact(c,j) + sabg_lyr_col(c,j)
                else
                   hm(c,j) = brr(c,j) - tinc/fact(c,j)
                endif
             endif
             if (imelt(c,j) == 1 .AND. hm(c,j) < 0._r8) then
                hm(c,j) = 0._r8
                imelt(c,j) = 0
             endif
             if (imelt(c,j) == 2 .AND. hm(c,j) > 0._r8) then
                hm(c,j) = 0._r8
                imelt(c,j) = 0
             endif
             if (imelt(c,j) > 0 .and. abs(hm(c,j)) > 0._r8) then
                xm(c,j) = hm(c,j)*dtime/hfus
                if (j == 1) then
                   if (snl(c)+1 == 1 .AND. h2osno(c) > 0._r8 .AND. xm(c,j) > 0._r8) then
                      temp1 = h2osno(c)
                      h2osno(c) = max(0._r8,temp1-xm(c,j))
                      propor = h2osno(c)/temp1
                      snowdp(c) = propor * snowdp(c)
                      heatr = hm(c,j) - hfus*(temp1-h2osno(c))/dtime
                      if (heatr > 0._r8) then
                         xm(c,j) = heatr*dtime/hfus
                         hm(c,j) = heatr
                      else
                         xm(c,j) = 0._r8
                         hm(c,j) = 0._r8
                      endif
                      qflx_snomelt(c) = max(0._r8,(temp1-h2osno(c)))/dtime
                      xmf(c) = hfus*qflx_snomelt(c)
                   endif
                endif
                heatr = 0._r8
                if (xm(c,j) > 0._r8) then
                   h2osoi_ice(c,j) = max(0._r8, wice0(c,j)-xm(c,j))
                   heatr = hm(c,j) - hfus*(wice0(c,j)-h2osoi_ice(c,j))/dtime
                else if (xm(c,j) < 0._r8) then
                   if (j <= 0) then
                      h2osoi_ice(c,j) = min(wmass0(c,j), wice0(c,j)-xm(c,j))
                   else
                      if (wmass0(c,j) < supercool(c,j)) then
                         h2osoi_ice(c,j) = 0._r8
                      else
                         h2osoi_ice(c,j) = min(wmass0(c,j) - supercool(c,j),wice0(c,j)-xm(c,j))
                      endif
                   endif
                   heatr = hm(c,j) - hfus*(wice0(c,j)-h2osoi_ice(c,j))/dtime
                endif
                h2osoi_liq(c,j) = max(0._r8,wmass0(c,j)-h2osoi_ice(c,j))
                if (abs(heatr) > 0._r8) then
                   if (j > snl(c)+1) then
                      t_soisno(c,j) = t_soisno(c,j) + fact(c,j)*heatr
                   else
                      t_soisno(c,j) = t_soisno(c,j) + fact(c,j)*heatr/(1._r8-fact(c,j)*dhsdT(c))
                   endif
                   if (j <= 0) then
                      if (h2osoi_liq(c,j)*h2osoi_ice(c,j)>0._r8) t_soisno(c,j) = tfrz
                   end if
                endif
                xmf(c) = xmf(c) + hfus * (wice0(c,j)-h2osoi_ice(c,j))/dtime
                if (imelt(c,j) == 1 .AND. j < 1) then
                   qflx_snomelt(c) = qflx_snomelt(c) + max(0._r8,(wice0(c,j)-h2osoi_ice(c,j)))/dtime
                endif
                if (imelt(c,j) == 2 .AND. j < 1) then
                   qflx_snofrz_lyr(c,j) = max(0._r8,(h2osoi_ice(c,j)-wice0(c,j)))/dtime
                endif
             endif
          endif
       end do
    enddo
!dir$ concurrent
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       eflx_snomelt(c) = qflx_snomelt(c) * hfus
       l = clandunit(c)
       if (ltype(l) == isturb) then
         eflx_snomelt_u(c) = eflx_snomelt(c)
       else if (ltype(l) == istsoil) then
         eflx_snomelt_r(c) = eflx_snomelt(c)
       end if
    end do
  end subroutine PhaseChange
end module SoilTemperatureMod
module SoilHydrologyMod
  implicit none
  save
  public :: SurfaceRunoff
  public :: Infiltration
  public :: SoilWater
  public :: Drainage
contains
  subroutine SurfaceRunoff (lbc, ubc, lbp, ubp, num_hydrologyc, filter_hydrologyc, &
                            num_urbanc, filter_urbanc, vol_liq, icefrac)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : denice, denh2o, wimp, pondmx_urban, &
                                 icol_roof, icol_sunwall, icol_shadewall, &
                                 icol_road_imperv, icol_road_perv
    use clm_varpar , only : nlevsoi, maxpatch_pft
    use globals , only : dtime
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: lbp, ubp
    integer , intent(in) :: num_hydrologyc
    integer , intent(in) :: filter_hydrologyc(ubc-lbc+1)
    integer , intent(in) :: num_urbanc
    integer , intent(in) :: filter_urbanc(ubc-lbc+1)
    real(r8), intent(out) :: vol_liq(lbc:ubc,1:nlevsoi)
    real(r8), intent(out) :: icefrac(lbc:ubc,1:nlevsoi)
    integer , pointer :: cgridcell(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: qflx_top_soil(:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: hkdepth(:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: fcov(:)
    real(r8), pointer :: fsat(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: wtfact(:)
    real(r8), pointer :: hksat(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: sucsat(:,:)
    integer , pointer :: snl(:)
    real(r8), pointer :: qflx_evap_grnd(:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: qflx_surf(:)
    real(r8), pointer :: eff_porosity(:,:)
    real(r8), pointer :: fracice(:,:)
    integer :: c,j,fc,g
    real(r8) :: xs(lbc:ubc)
    real(r8) :: vol_ice(lbc:ubc,1:nlevsoi)
    real(r8) :: fff(lbc:ubc)
    real(r8) :: s1
    real(r8) :: su
    real(r8) :: v
    real(r8) :: qinmax
    ctype => clm3%g%l%c%itype
    qflx_top_soil => clm3%g%l%c%cwf%qflx_top_soil
    qflx_surf => clm3%g%l%c%cwf%qflx_surf
    watsat => clm3%g%l%c%cps%watsat
    hkdepth => clm3%g%l%c%cps%hkdepth
    dz => clm3%g%l%c%cps%dz
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    fcov => clm3%g%l%c%cws%fcov
    fsat => clm3%g%l%c%cws%fsat
    eff_porosity => clm3%g%l%c%cps%eff_porosity
    wtfact => clm3%g%l%c%cps%wtfact
    zwt => clm3%g%l%c%cws%zwt
    fracice => clm3%g%l%c%cps%fracice
    hksat => clm3%g%l%c%cps%hksat
    bsw => clm3%g%l%c%cps%bsw
    sucsat => clm3%g%l%c%cps%sucsat
    snl => clm3%g%l%c%cps%snl
    qflx_evap_grnd => clm3%g%l%c%cwf%pwf_a%qflx_evap_grnd
    zi => clm3%g%l%c%cps%zi
    do j = 1,nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          vol_ice(c,j) = min(watsat(c,j), h2osoi_ice(c,j)/(dz(c,j)*denice))
          eff_porosity(c,j) = max(0.01_r8,watsat(c,j)-vol_ice(c,j))
          vol_liq(c,j) = min(eff_porosity(c,j), h2osoi_liq(c,j)/(dz(c,j)*denh2o))
          icefrac(c,j) = min(1._r8,h2osoi_ice(c,j)/(h2osoi_ice(c,j)+h2osoi_liq(c,j)))
          fracice(c,j) = max(0._r8,exp(-3._r8*(1._r8-icefrac(c,j)))- exp(-3._r8))/(1.0_r8-exp(-3._r8))
       end do
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       fff(c) = 0.5_r8
       fsat(c) = wtfact(c) * exp(-0.5_r8*fff(c)*zwt(c))
       fcov(c) = (1._r8 - fracice(c,1)) * fsat(c) + fracice(c,1)
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       s1 = max(0.01_r8,vol_liq(c,1)/max(wimp,eff_porosity(c,1)))
       su = max(0._r8,(s1-fcov(c)) / (max(0.01_r8,1._r8-fcov(c))))
       v = -bsw(c,1)*sucsat(c,1)/(0.5_r8*dz(c,1)*1000._r8)
       qinmax = (1._r8+v*(su-1._r8))*hksat(c,1)
       qflx_surf(c) = fcov(c) * qflx_top_soil(c) + &
                       (1._r8-fcov(c)) * max(0._r8, qflx_top_soil(c)-qinmax)
    end do
!dir$ concurrent
    do fc = 1, num_urbanc
       c = filter_urbanc(fc)
       if (ctype(c) == icol_roof .or. ctype(c) == icol_road_imperv) then
          if (snl(c) < 0) then
             qflx_surf(c) = max(0._r8,qflx_top_soil(c))
          else
             xs(c) = max(0._r8, &
                         h2osoi_liq(c,1)/dtime + qflx_top_soil(c) - qflx_evap_grnd(c) - &
                         pondmx_urban/dtime)
             if (xs(c) > 0.) then
                h2osoi_liq(c,1) = pondmx_urban
             else
                h2osoi_liq(c,1) = max(0._r8,h2osoi_liq(c,1)+ &
                                     (qflx_top_soil(c)-qflx_evap_grnd(c))*dtime)
             end if
             qflx_surf(c) = xs(c)
          end if
       else if (ctype(c) == icol_sunwall .or. ctype(c) == icol_shadewall) then
         qflx_surf(c) = 0._r8
       end if
    end do
  end subroutine SurfaceRunoff
  subroutine Infiltration(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                          num_urbanc, filter_urbanc)
    use shr_kind_mod, only : r8 => shr_kind_r8
    use clm_varcon , only : icol_roof, icol_road_imperv, icol_sunwall, icol_shadewall, &
                             icol_road_perv
    use clmtype
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_hydrologyc
    integer, intent(in) :: filter_hydrologyc(ubc-lbc+1)
    integer, intent(in) :: num_urbanc
    integer, intent(in) :: filter_urbanc(ubc-lbc+1)
    integer , pointer :: ctype(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: qflx_top_soil(:)
    real(r8), pointer :: qflx_surf(:)
    real(r8), pointer :: qflx_evap_grnd(:)
    real(r8), pointer :: qflx_infl(:)
    integer :: c, fc
    ctype => clm3%g%l%c%itype
    snl => clm3%g%l%c%cps%snl
    qflx_top_soil => clm3%g%l%c%cwf%qflx_top_soil
    qflx_surf => clm3%g%l%c%cwf%qflx_surf
    qflx_infl => clm3%g%l%c%cwf%qflx_infl
    qflx_evap_grnd => clm3%g%l%c%cwf%pwf_a%qflx_evap_grnd
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       if (snl(c) >= 0) then
          qflx_infl(c) = qflx_top_soil(c) - qflx_surf(c) - qflx_evap_grnd(c)
       else
          qflx_infl(c) = qflx_top_soil(c) - qflx_surf(c)
       end if
    end do
!dir$ concurrent
    do fc = 1, num_urbanc
       c = filter_urbanc(fc)
       if (ctype(c) /= icol_road_perv) then
          qflx_infl(c) = 0._r8
       end if
    end do
  end subroutine Infiltration
  subroutine SoilWater(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                       num_urbanc, filter_urbanc, &
                       vol_liq, dwat, hk, dhkdw)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : wimp, icol_roof, icol_road_imperv
    use clm_varpar , only : nlevsoi, max_pft_per_col
    use shr_const_mod , only : SHR_CONST_TKFRZ, SHR_CONST_LATICE, SHR_CONST_G
    use TridiagonalMod, only : Tridiagonal
    use globals , only : dtime
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_hydrologyc
    integer , intent(in) :: filter_hydrologyc(ubc-lbc+1)
    integer , intent(in) :: num_urbanc
    integer , intent(in) :: filter_urbanc(ubc-lbc+1)
    real(r8), intent(in) :: vol_liq(lbc:ubc,1:nlevsoi)
    real(r8), intent(out) :: dwat(lbc:ubc,1:nlevsoi)
    real(r8), intent(out) :: hk(lbc:ubc,1:nlevsoi)
    real(r8), intent(out) :: dhkdw(lbc:ubc,1:nlevsoi)
    integer , pointer :: ctype(:)
    integer , pointer :: npfts(:)
    real(r8), pointer :: pwtcol(:)
    real(r8), pointer :: pwtgcell(:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: smpmin(:)
    real(r8), pointer :: qflx_infl(:)
    real(r8), pointer :: qflx_tran_veg_pft(:)
    real(r8), pointer :: qflx_tran_veg_col(:)
    real(r8), pointer :: eff_porosity(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: hksat(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: rootr_pft(:,:)
    integer , pointer :: pfti(:)
    real(r8), pointer :: fracice(:,:)
    real(r8), pointer :: h2osoi_vol(:,:)
    real(r8), pointer :: qcharge(:)
    real(r8), pointer :: hkdepth(:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: rootr_col(:,:)
    real(r8), pointer :: smp_l(:,:)
    real(r8), pointer :: hk_l(:,:)
    integer :: p,c,fc,j
    integer :: jtop(lbc:ubc)
    real(r8) :: amx(lbc:ubc,1:nlevsoi+1)
    real(r8) :: bmx(lbc:ubc,1:nlevsoi+1)
    real(r8) :: cmx(lbc:ubc,1:nlevsoi+1)
    real(r8) :: rmx(lbc:ubc,1:nlevsoi+1)
    real(r8) :: zmm(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dzmm(lbc:ubc,1:nlevsoi+1)
    real(r8) :: den
    real(r8) :: dqidw0(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dqidw1(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dqodw1(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dqodw2(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dsmpdw(lbc:ubc,1:nlevsoi+1)
    real(r8) :: num
    real(r8) :: qin(lbc:ubc,1:nlevsoi+1)
    real(r8) :: qout(lbc:ubc,1:nlevsoi+1)
    real(r8) :: s_node
    real(r8) :: s1
    real(r8) :: s2
    real(r8) :: smp(lbc:ubc,1:nlevsoi)
    real(r8) :: sdamp
    integer :: pi
    real(r8) :: temp(lbc:ubc)
    integer :: jwt(lbc:ubc)
    real(r8) :: smp1,dsmpdw1,wh,wh_zwt,ka
    real(r8) :: dwat2(lbc:ubc,1:nlevsoi+1)
    real(r8) :: dzq
    real(r8) :: zimm(lbc:ubc,0:nlevsoi)
    real(r8) :: zq(lbc:ubc,1:nlevsoi+1)
    real(r8) :: vol_eq(lbc:ubc,1:nlevsoi+1)
    real(r8) :: tempi
    real(r8) :: temp0
    real(r8) :: voleq1
    real(r8) :: zwtmm(lbc:ubc)
    qcharge => clm3%g%l%c%cws%qcharge
    hkdepth => clm3%g%l%c%cps%hkdepth
    zi => clm3%g%l%c%cps%zi
    zwt => clm3%g%l%c%cws%zwt
    ctype => clm3%g%l%c%itype
    npfts => clm3%g%l%c%npfts
    z => clm3%g%l%c%cps%z
    dz => clm3%g%l%c%cps%dz
    smpmin => clm3%g%l%c%cps%smpmin
    watsat => clm3%g%l%c%cps%watsat
    hksat => clm3%g%l%c%cps%hksat
    bsw => clm3%g%l%c%cps%bsw
    sucsat => clm3%g%l%c%cps%sucsat
    eff_porosity => clm3%g%l%c%cps%eff_porosity
    rootr_col => clm3%g%l%c%cps%rootr_column
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
    qflx_infl => clm3%g%l%c%cwf%qflx_infl
    fracice => clm3%g%l%c%cps%fracice
    qflx_tran_veg_col => clm3%g%l%c%cwf%pwf_a%qflx_tran_veg
    pfti => clm3%g%l%c%pfti
    smp_l => clm3%g%l%c%cws%smp_l
    hk_l => clm3%g%l%c%cws%hk_l
    qflx_tran_veg_pft => clm3%g%l%c%p%pwf%qflx_tran_veg
    rootr_pft => clm3%g%l%c%p%pps%rootr
    pwtcol => clm3%g%l%c%p%wtcol
    pwtgcell => clm3%g%l%c%p%wtgcell
    do j = 1, nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          zmm(c,j) = z(c,j)*1.e3_r8
          dzmm(c,j) = dz(c,j)*1.e3_r8
          zimm(c,j) = zi(c,j)*1.e3_r8
       end do
    end do
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       zimm(c,0) = 0.0_r8
       zwtmm(c) = zwt(c)*1.e3_r8
    end do
    temp(:) = 0._r8
    do j = 1, nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          rootr_col(c,j) = 0._r8
       end do
    end do
    do pi = 1,max_pft_per_col
       do j = 1,nlevsoi
!dir$ concurrent
          do fc = 1, num_hydrologyc
             c = filter_hydrologyc(fc)
             if (pi <= npfts(c)) then
                p = pfti(c) + pi - 1
                if (pwtgcell(p)>0._r8) then
                   rootr_col(c,j) = rootr_col(c,j) + rootr_pft(p,j) * qflx_tran_veg_pft(p) * pwtcol(p)
                end if
             end if
          end do
       end do
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          if (pi <= npfts(c)) then
             p = pfti(c) + pi - 1
             if (pwtgcell(p)>0._r8) then
                temp(c) = temp(c) + qflx_tran_veg_pft(p) * pwtcol(p)
             end if
          end if
       end do
    end do
    do j = 1, nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          if (temp(c) /= 0._r8) then
             rootr_col(c,j) = rootr_col(c,j)/temp(c)
          end if
       end do
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       jwt(c) = nlevsoi
       do j = 2,nlevsoi
          if(zwt(c) <= zi(c,j)) then
             jwt(c) = j-1
             exit
          end if
       enddo
    end do
    do j=1,nlevsoi
       do fc=1, num_hydrologyc
          c = filter_hydrologyc(fc)
          if ((zwtmm(c) .lt. zimm(c,j-1))) then
             vol_eq(c,j) = watsat(c,j)
          else if ((zwtmm(c) .lt. zimm(c,j)) .and. (zwtmm(c) .gt. zimm(c,j-1))) then
             tempi = 1.0_r8
             temp0 = (((sucsat(c,j)+zwtmm(c)-zimm(c,j-1))/sucsat(c,j)))**(1._r8-1._r8/bsw(c,j))
             voleq1 = -sucsat(c,j)*watsat(c,j)/(1._r8-1._r8/bsw(c,j))/(zwtmm(c)-zimm(c,j-1))*(tempi-temp0)
             vol_eq(c,j) = (voleq1*(zwtmm(c)-zimm(c,j-1)) + watsat(c,j)*(zimm(c,j)-zwtmm(c)))/(zimm(c,j)-zimm(c,j-1))
             vol_eq(c,j) = min(watsat(c,j),vol_eq(c,j))
             vol_eq(c,j) = max(vol_eq(c,j),0.0_r8)
          else
             tempi = (((sucsat(c,j)+zwtmm(c)-zimm(c,j))/sucsat(c,j)))**(1._r8-1._r8/bsw(c,j))
             temp0 = (((sucsat(c,j)+zwtmm(c)-zimm(c,j-1))/sucsat(c,j)))**(1._r8-1._r8/bsw(c,j))
             vol_eq(c,j) = -sucsat(c,j)*watsat(c,j)/(1._r8-1._r8/bsw(c,j))/(zimm(c,j)-zimm(c,j-1))*(tempi-temp0)
             vol_eq(c,j) = max(vol_eq(c,j),0.0_r8)
             vol_eq(c,j) = min(watsat(c,j),vol_eq(c,j))
          endif
          zq(c,j) = -sucsat(c,j)*(max(vol_eq(c,j)/watsat(c,j),0.01_r8))**(-bsw(c,j))
          zq(c,j) = max(smpmin(c), zq(c,j))
       end do
    end do
    j = nlevsoi
    do fc=1, num_hydrologyc
       c = filter_hydrologyc(fc)
       if(jwt(c) == nlevsoi) then
          tempi = 1._r8
          temp0 = (((sucsat(c,j)+zwtmm(c)-zimm(c,j))/sucsat(c,j)))**(1._r8-1._r8/bsw(c,j))
          vol_eq(c,j+1) = -sucsat(c,j)*watsat(c,j)/(1._r8-1._r8/bsw(c,j))/(zwtmm(c)-zimm(c,j))*(tempi-temp0)
          vol_eq(c,j+1) = max(vol_eq(c,j+1),0.0_r8)
          vol_eq(c,j+1) = min(watsat(c,j),vol_eq(c,j+1))
          zq(c,j+1) = -sucsat(c,j)*(max(vol_eq(c,j+1)/watsat(c,j),0.01_r8))**(-bsw(c,j))
          zq(c,j+1) = max(smpmin(c), zq(c,j+1))
       end if
    end do
    sdamp = 0._r8
    do j = 1, nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          s1 = 0.5_r8*(h2osoi_vol(c,j) + h2osoi_vol(c,min(nlevsoi, j+1))) / &
               (0.5_r8*(watsat(c,j)+watsat(c,min(nlevsoi, j+1))))
          s1 = min(1._r8, s1)
          s2 = hksat(c,j)*s1**(2._r8*bsw(c,j)+2._r8)
          hk(c,j) = (1._r8-0.5_r8*(fracice(c,j)+fracice(c,min(nlevsoi, j+1))))*s1*s2
          dhkdw(c,j) = (1._r8-0.5_r8*(fracice(c,j)+fracice(c,min(nlevsoi, j+1))))* &
                       (2._r8*bsw(c,j)+3._r8)*s2*0.5_r8/watsat(c,j)
          s_node = max(h2osoi_vol(c,j)/watsat(c,j), 0.01_r8)
          s_node = min(1.0_r8, s_node)
          smp(c,j) = -sucsat(c,j)*s_node**(-bsw(c,j))
          smp(c,j) = max(smpmin(c), smp(c,j))
          dsmpdw(c,j) = -bsw(c,j)*smp(c,j)/(s_node*watsat(c,j))
          smp_l(c,j) = smp(c,j)
          hk_l(c,j) = hk(c,j)
       end do
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       zmm(c,nlevsoi+1) = 0.5*(1.e3_r8*zwt(c) + zmm(c,nlevsoi))
       if(jwt(c) < nlevsoi) then
         dzmm(c,nlevsoi+1) = dzmm(c,nlevsoi)
       else
         dzmm(c,nlevsoi+1) = (1.e3_r8*zwt(c) - zmm(c,nlevsoi))
       end if
    end do
    j = 1
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       qin(c,j) = qflx_infl(c)
       den = (zmm(c,j+1)-zmm(c,j))
       dzq = (zq(c,j+1)-zq(c,j))
       num = (smp(c,j+1)-smp(c,j)) - dzq
       qout(c,j) = -hk(c,j)*num/den
       dqodw1(c,j) = -(-hk(c,j)*dsmpdw(c,j) + num*dhkdw(c,j))/den
       dqodw2(c,j) = -( hk(c,j)*dsmpdw(c,j+1) + num*dhkdw(c,j))/den
       rmx(c,j) = qin(c,j) - qout(c,j) - qflx_tran_veg_col(c) * rootr_col(c,j)
       amx(c,j) = 0._r8
       bmx(c,j) = dzmm(c,j)*(sdamp+1._r8/dtime) + dqodw1(c,j)
       cmx(c,j) = dqodw2(c,j)
    end do
    do j = 2, nlevsoi - 1
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          den = (zmm(c,j) - zmm(c,j-1))
          dzq = (zq(c,j)-zq(c,j-1))
          num = (smp(c,j)-smp(c,j-1)) - dzq
          qin(c,j) = -hk(c,j-1)*num/den
          dqidw0(c,j) = -(-hk(c,j-1)*dsmpdw(c,j-1) + num*dhkdw(c,j-1))/den
          dqidw1(c,j) = -( hk(c,j-1)*dsmpdw(c,j) + num*dhkdw(c,j-1))/den
          den = (zmm(c,j+1)-zmm(c,j))
          dzq = (zq(c,j+1)-zq(c,j))
          num = (smp(c,j+1)-smp(c,j)) - dzq
          qout(c,j) = -hk(c,j)*num/den
          dqodw1(c,j) = -(-hk(c,j)*dsmpdw(c,j) + num*dhkdw(c,j))/den
          dqodw2(c,j) = -( hk(c,j)*dsmpdw(c,j+1) + num*dhkdw(c,j))/den
          rmx(c,j) = qin(c,j) - qout(c,j) - qflx_tran_veg_col(c)*rootr_col(c,j)
          amx(c,j) = -dqidw0(c,j)
          bmx(c,j) = dzmm(c,j)/dtime - dqidw1(c,j) + dqodw1(c,j)
          cmx(c,j) = dqodw2(c,j)
       end do
    end do
    j = nlevsoi
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       if(j > jwt(c)) then
         den = (zmm(c,j) - zmm(c,j-1))
         dzq = (zq(c,j)-zq(c,j-1))
         num = (smp(c,j)-smp(c,j-1)) - dzq
         qin(c,j) = -hk(c,j-1)*num/den
         dqidw0(c,j) = -(-hk(c,j-1)*dsmpdw(c,j-1) + num*dhkdw(c,j-1))/den
         dqidw1(c,j) = -( hk(c,j-1)*dsmpdw(c,j) + num*dhkdw(c,j-1))/den
         qout(c,j) = 0._r8
         dqodw1(c,j) = 0._r8
         rmx(c,j) = qin(c,j) - qout(c,j) - qflx_tran_veg_col(c)*rootr_col(c,j)
         amx(c,j) = -dqidw0(c,j)
         bmx(c,j) = dzmm(c,j)/dtime - dqidw1(c,j) + dqodw1(c,j)
         cmx(c,j) = 0._r8
         rmx(c,j+1) = 0._r8
         amx(c,j+1) = 0._r8
         bmx(c,j+1) = dzmm(c,j+1)/dtime
         cmx(c,j+1) = 0._r8
       else
         s_node = max(0.5*(1.0_r8+h2osoi_vol(c,j)/watsat(c,j)), 0.01_r8)
         s_node = min(1.0_r8, s_node)
         smp1 = -sucsat(c,j)*s_node**(-bsw(c,j))
         smp1 = max(smpmin(c), smp1)
         dsmpdw1 = -bsw(c,j)*smp1/(s_node*watsat(c,j))
         den = (zmm(c,j) - zmm(c,j-1))
         dzq = (zq(c,j)-zq(c,j-1))
         num = (smp(c,j)-smp(c,j-1)) - dzq
         qin(c,j) = -hk(c,j-1)*num/den
         dqidw0(c,j) = -(-hk(c,j-1)*dsmpdw(c,j-1) + num*dhkdw(c,j-1))/den
         dqidw1(c,j) = -( hk(c,j-1)*dsmpdw(c,j) + num*dhkdw(c,j-1))/den
         den = (zmm(c,j+1)-zmm(c,j))
         dzq = (zq(c,j+1)-zq(c,j))
         num = (smp1-smp(c,j)) - dzq
         qout(c,j) = -hk(c,j)*num/den
         dqodw1(c,j) = -(-hk(c,j)*dsmpdw(c,j) + num*dhkdw(c,j))/den
         dqodw2(c,j) = -( hk(c,j)*dsmpdw1 + num*dhkdw(c,j))/den
         rmx(c,j) = qin(c,j) - qout(c,j) - qflx_tran_veg_col(c)*rootr_col(c,j)
         amx(c,j) = -dqidw0(c,j)
         bmx(c,j) = dzmm(c,j)/dtime - dqidw1(c,j) + dqodw1(c,j)
         cmx(c,j) = dqodw2(c,j)
         qin(c,j+1) = qout(c,j)
         dqidw0(c,j+1) = -(-hk(c,j)*dsmpdw(c,j) + num*dhkdw(c,j))/den
         dqidw1(c,j+1) = -( hk(c,j)*dsmpdw1 + num*dhkdw(c,j))/den
         qout(c,j+1) = 0._r8
         dqodw1(c,j+1) = 0._r8
         rmx(c,j+1) = qin(c,j+1) - qout(c,j+1)
         amx(c,j+1) = -dqidw0(c,j+1)
         bmx(c,j+1) = dzmm(c,j+1)/dtime - dqidw1(c,j+1) + dqodw1(c,j+1)
         cmx(c,j+1) = 0._r8
       endif
    end do
    jtop(:) = 1
    call Tridiagonal(lbc, ubc, 1, nlevsoi+1, jtop, num_hydrologyc, filter_hydrologyc, &
                     amx, bmx, cmx, rmx, dwat2 )
    do fc = 1,num_hydrologyc
       c = filter_hydrologyc(fc)
       do j = 1, nlevsoi
          dwat(c,j)=dwat2(c,j)
       end do
    end do
!dir$ concurrent
    do fc = 1,num_hydrologyc
       c = filter_hydrologyc(fc)
       do j = 1, nlevsoi
          h2osoi_liq(c,j) = h2osoi_liq(c,j) + dwat2(c,j)*dzmm(c,j)
       end do
       if(jwt(c) < nlevsoi) then
          wh_zwt = 0._r8
          s_node = max(h2osoi_vol(c,jwt(c))/watsat(c,jwt(c)), 0.01_r8)
          s_node = min(1.0_r8, s_node)
          s1 = 0.5_r8*(1.0+s_node)
          s1 = min(1._r8, s1)
          ka = hksat(c,jwt(c))*s1**(2._r8*bsw(c,jwt(c))+3._r8)
          smp1 = -sucsat(c,jwt(c))*s_node**(-bsw(c,jwt(c)))
          smp1 = max(smpmin(c), smp(c,jwt(c)))
          wh = smp1 - zq(c,jwt(c))
          qcharge(c) = -ka * (wh_zwt-wh) /((zwt(c)-z(c,jwt(c)))*1000._r8)
          qcharge(c) = max(-10.0_r8/dtime,qcharge(c))
          qcharge(c) = min( 10.0_r8/dtime,qcharge(c))
       else
          qcharge(c) = dwat2(c,nlevsoi+1)*dzmm(c,nlevsoi+1)/dtime
       endif
    end do
  end subroutine SoilWater
  subroutine Drainage(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                      num_urbanc, filter_urbanc, vol_liq, hk, &
                      icefrac)
    use shr_kind_mod, only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : pondmx, tfrz, icol_roof, icol_road_imperv, icol_road_perv, watmin
    use clm_varpar , only : nlevsoi
    use globals , only : dtime
    implicit none
    integer , intent(in) :: lbc, ubc
    integer , intent(in) :: num_hydrologyc
    integer , intent(in) :: num_urbanc
    integer , intent(in) :: filter_urbanc(ubc-lbc+1)
    integer , intent(in) :: filter_hydrologyc(ubc-lbc+1)
    real(r8), intent(in) :: vol_liq(lbc:ubc,1:nlevsoi)
    real(r8), intent(in) :: hk(lbc:ubc,1:nlevsoi)
    real(r8), intent(in) :: icefrac(lbc:ubc,1:nlevsoi)
    integer , pointer :: ctype(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: qflx_snwcp_liq(:)
    real(r8), pointer :: qflx_dew_grnd(:)
    real(r8), pointer :: qflx_dew_snow(:)
    real(r8), pointer :: qflx_sub_snow(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: eff_porosity(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: hksat(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: hkdepth(:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: wa(:)
    real(r8), pointer :: wt(:)
    real(r8), pointer :: qcharge(:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: qflx_drain(:)
    real(r8), pointer :: qflx_qrgwl(:)
    real(r8), pointer :: eflx_impsoil(:)
    real(r8), pointer :: qflx_rsub_sat(:)
    integer :: c,j,fc,i
    real(r8) :: xs(lbc:ubc)
    real(r8) :: dzmm(lbc:ubc,1:nlevsoi)
    integer :: jwt(lbc:ubc)
    real(r8) :: rsub_bot(lbc:ubc)
    real(r8) :: rsub_top(lbc:ubc)
    real(r8) :: fff(lbc:ubc)
    real(r8) :: xsi(lbc:ubc)
    real(r8) :: xsia(lbc:ubc)
    real(r8) :: xs1(lbc:ubc)
    real(r8) :: smpfz(1:nlevsoi)
    real(r8) :: wtsub
    real(r8) :: rous
    real(r8) :: wh
    real(r8) :: wh_zwt
    real(r8) :: ws
    real(r8) :: s_node
    real(r8) :: dzsum
    real(r8) :: icefracsum
    real(r8) :: fracice_rsub(lbc:ubc)
    real(r8) :: ka
    real(r8) :: dza
    real(r8) :: available_h2osoi_liq
    ctype => clm3%g%l%c%itype
    snl => clm3%g%l%c%cps%snl
    dz => clm3%g%l%c%cps%dz
    bsw => clm3%g%l%c%cps%bsw
    t_soisno => clm3%g%l%c%ces%t_soisno
    hksat => clm3%g%l%c%cps%hksat
    sucsat => clm3%g%l%c%cps%sucsat
    z => clm3%g%l%c%cps%z
    zi => clm3%g%l%c%cps%zi
    watsat => clm3%g%l%c%cps%watsat
    hkdepth => clm3%g%l%c%cps%hkdepth
    zwt => clm3%g%l%c%cws%zwt
    wa => clm3%g%l%c%cws%wa
    wt => clm3%g%l%c%cws%wt
    qcharge => clm3%g%l%c%cws%qcharge
    eff_porosity => clm3%g%l%c%cps%eff_porosity
    qflx_snwcp_liq => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_liq
    qflx_dew_grnd => clm3%g%l%c%cwf%pwf_a%qflx_dew_grnd
    qflx_dew_snow => clm3%g%l%c%cwf%pwf_a%qflx_dew_snow
    qflx_sub_snow => clm3%g%l%c%cwf%pwf_a%qflx_sub_snow
    qflx_drain => clm3%g%l%c%cwf%qflx_drain
    qflx_qrgwl => clm3%g%l%c%cwf%qflx_qrgwl
    qflx_rsub_sat => clm3%g%l%c%cwf%qflx_rsub_sat
    eflx_impsoil => clm3%g%l%c%cef%eflx_impsoil
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    do j = 1,nlevsoi
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          dzmm(c,j) = dz(c,j)*1.e3_r8
       end do
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       qflx_drain(c) = 0._r8
       rsub_bot(c) = 0._r8
       qflx_rsub_sat(c) = 0._r8
       rsub_top(c) = 0._r8
       fracice_rsub(c) = 0._r8
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       jwt(c) = nlevsoi
       do j = 2,nlevsoi
          if(zwt(c) <= zi(c,j)) then
             jwt(c) = j-1
             exit
          end if
       enddo
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       fff(c) = 1._r8/ hkdepth(c)
       dzsum = 0._r8
       icefracsum = 0._r8
       do j = jwt(c), nlevsoi
          dzsum = dzsum + dzmm(c,j)
          icefracsum = icefracsum + icefrac(c,j) * dzmm(c,j)
       end do
       fracice_rsub(c) = max(0._r8,exp(-3._r8*(1._r8-(icefracsum/dzsum)))- exp(-3._r8))/(1.0_r8-exp(-3._r8))
       rsub_top(c) = (1._r8 - fracice_rsub(c)) * 5.5e-3_r8 * exp(-fff(c)*zwt(c))
    end do
    rous = 0.2_r8
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       wt(c) = wt(c) + (qcharge(c) - rsub_top(c)) * dtime
       if(jwt(c) == nlevsoi) then
          wa(c) = wa(c) + (qcharge(c) -rsub_top(c)) * dtime
          wt(c) = wa(c)
          zwt(c) = (zi(c,nlevsoi) + 25._r8) - wa(c)/1000._r8/rous
          h2osoi_liq(c,nlevsoi) = h2osoi_liq(c,nlevsoi) + max(0._r8,(wa(c)-5000._r8))
          wa(c) = min(wa(c), 5000._r8)
       else
          if (jwt(c) == nlevsoi-1) then
             zwt(c) = zi(c,nlevsoi)- (wt(c)-rous*1000._r8*25._r8) /eff_porosity(c,nlevsoi)/1000._r8
          else
             ws = 0._r8
             do j = jwt(c)+2,nlevsoi
               ws = ws + eff_porosity(c,j) * 1000._r8 * dz(c,j)
             enddo
             zwt(c) = zi(c,jwt(c)+1)-(wt(c)-rous*1000_r8*25._r8-ws) /eff_porosity(c,jwt(c)+1)/1000._r8
          endif
          wtsub = 0._r8
          do j = jwt(c)+1, nlevsoi
             wtsub = wtsub + hk(c,j)*dzmm(c,j)
          end do
          do j = jwt(c)+1, nlevsoi
             h2osoi_liq(c,j) = h2osoi_liq(c,j) - rsub_top(c)*dtime*hk(c,j)*dzmm(c,j)/wtsub
          end do
       end if
       zwt(c) = max(0.05_r8,zwt(c))
       zwt(c) = min(80._r8,zwt(c))
    end do
    do j = nlevsoi,2,-1
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          xsi(c) = max(h2osoi_liq(c,j)-eff_porosity(c,j)*dzmm(c,j),0._r8)
          h2osoi_liq(c,j) = min(eff_porosity(c,j)*dzmm(c,j), h2osoi_liq(c,j))
          h2osoi_liq(c,j-1) = h2osoi_liq(c,j-1) + xsi(c)
       end do
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       xs1(c) = max(max(h2osoi_liq(c,1),0._r8)-max(0._r8,(pondmx+watsat(c,1)*dzmm(c,1)-h2osoi_ice(c,1))),0._r8)
       h2osoi_liq(c,1) = min(max(0._r8,pondmx+watsat(c,1)*dzmm(c,1)-h2osoi_ice(c,1)), h2osoi_liq(c,1))
       qflx_rsub_sat(c) = xs1(c) / dtime
    end do
    do j = 1, nlevsoi-1
!dir$ concurrent
       do fc = 1, num_hydrologyc
          c = filter_hydrologyc(fc)
          if (h2osoi_liq(c,j) < watmin) then
             xs(c) = watmin - h2osoi_liq(c,j)
          else
             xs(c) = 0._r8
          end if
          h2osoi_liq(c,j ) = h2osoi_liq(c,j ) + xs(c)
          h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) - xs(c)
       end do
    end do
!KO!dir$ concurrent
    j = nlevsoi
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       if (h2osoi_liq(c,j) < watmin) then
          xs(c) = watmin-h2osoi_liq(c,j)
          searchforwater: do i = nlevsoi-1, 1, -1
             available_h2osoi_liq = max(h2osoi_liq(c,i)-watmin-xs(c),0._r8)
             if (available_h2osoi_liq .ge. xs(c)) then
               h2osoi_liq(c,j) = h2osoi_liq(c,j) + xs(c)
               h2osoi_liq(c,i) = h2osoi_liq(c,i) - xs(c)
               xs(c) = 0._r8
               exit searchforwater
             else
               h2osoi_liq(c,j) = h2osoi_liq(c,j) + available_h2osoi_liq
               h2osoi_liq(c,i) = h2osoi_liq(c,i) - available_h2osoi_liq
               xs(c) = xs(c) - available_h2osoi_liq
             end if
          end do searchforwater
       else
          xs(c) = 0._r8
       end if
       h2osoi_liq(c,j) = h2osoi_liq(c,j) + xs(c)
       wt(c) = wt(c) - xs(c)
       rsub_top(c) = rsub_top(c) - xs(c)/dtime
    end do
!dir$ concurrent
    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)
       qflx_drain(c) = qflx_rsub_sat(c) + rsub_top(c)
       qflx_qrgwl(c) = qflx_snwcp_liq(c)
       eflx_impsoil(c) = 0._r8
       if (snl(c)+1 >= 1) then
          h2osoi_liq(c,1) = h2osoi_liq(c,1) + qflx_dew_grnd(c) * dtime
          h2osoi_ice(c,1) = h2osoi_ice(c,1) + (qflx_dew_snow(c) * dtime)
          if (qflx_sub_snow(c)*dtime > h2osoi_ice(c,1)) then
             qflx_sub_snow(c) = h2osoi_ice(c,1)/dtime
             h2osoi_ice(c,1) = 0._r8
          else
             h2osoi_ice(c,1) = h2osoi_ice(c,1) - (qflx_sub_snow(c) * dtime)
          end if
       end if
    end do
!dir$ concurrent
    do fc = 1, num_urbanc
       c = filter_urbanc(fc)
       if (ctype(c) /= icol_road_perv) then
         qflx_drain(c) = 0._r8
         qflx_qrgwl(c) = qflx_snwcp_liq(c)
         eflx_impsoil(c) = 0._r8
       end if
       if (ctype(c) == icol_roof .or. ctype(c) == icol_road_imperv) then
         if (snl(c)+1 >= 1) then
            h2osoi_liq(c,1) = h2osoi_liq(c,1) + qflx_dew_grnd(c) * dtime
            h2osoi_ice(c,1) = h2osoi_ice(c,1) + (qflx_dew_snow(c) * dtime)
            if (qflx_sub_snow(c)*dtime > h2osoi_ice(c,1)) then
               qflx_sub_snow(c) = h2osoi_ice(c,1)/dtime
               h2osoi_ice(c,1) = 0._r8
            else
               h2osoi_ice(c,1) = h2osoi_ice(c,1) - (qflx_sub_snow(c) * dtime)
            end if
         end if
       end if
    end do
  end subroutine Drainage
end module SoilHydrologyMod
module SnowHydrologyMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar , only : nlevsno
  implicit none
  save
  public :: SnowWater
  public :: SnowCompaction
  public :: CombineSnowLayers
  public :: DivideSnowLayers
  public :: BuildSnowFilter
  private :: Combo
contains
  subroutine SnowWater(lbc, ubc, num_snowc, filter_snowc, &
                       num_nosnowc, filter_nosnowc)
    use clmtype
    use clm_varcon , only : denh2o, denice, wimp, ssi
    use SNICARMod , only : scvng_fct_mlt_bcphi, scvng_fct_mlt_bcpho, &
                                   scvng_fct_mlt_ocphi, scvng_fct_mlt_ocpho, &
                                   scvng_fct_mlt_dst1, scvng_fct_mlt_dst2, &
                                   scvng_fct_mlt_dst3, scvng_fct_mlt_dst4
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_snowc
    integer, intent(in) :: filter_snowc(ubc-lbc+1)
    integer, intent(in) :: num_nosnowc
    integer, intent(in) :: filter_nosnowc(ubc-lbc+1)
    integer , pointer :: snl(:)
    logical , pointer :: do_capsnow(:)
    real(r8), pointer :: qflx_snomelt(:)
    real(r8), pointer :: qflx_rain_grnd(:)
    real(r8), pointer :: qflx_sub_snow(:)
    real(r8), pointer :: qflx_evap_grnd(:)
    real(r8), pointer :: qflx_dew_snow(:)
    real(r8), pointer :: qflx_dew_grnd(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: qflx_top_soil(:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    integer , pointer :: cgridcell(:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: flx_bc_dep_dry(:)
    real(r8), pointer :: flx_bc_dep_wet(:)
    real(r8), pointer :: flx_bc_dep(:)
    real(r8), pointer :: flx_bc_dep_pho(:)
    real(r8), pointer :: flx_bc_dep_phi(:)
    real(r8), pointer :: flx_oc_dep_dry(:)
    real(r8), pointer :: flx_oc_dep_wet(:)
    real(r8), pointer :: flx_oc_dep(:)
    real(r8), pointer :: flx_oc_dep_pho(:)
    real(r8), pointer :: flx_oc_dep_phi(:)
    real(r8), pointer :: flx_dst_dep_dry1(:)
    real(r8), pointer :: flx_dst_dep_wet1(:)
    real(r8), pointer :: flx_dst_dep_dry2(:)
    real(r8), pointer :: flx_dst_dep_wet2(:)
    real(r8), pointer :: flx_dst_dep_dry3(:)
    real(r8), pointer :: flx_dst_dep_wet3(:)
    real(r8), pointer :: flx_dst_dep_dry4(:)
    real(r8), pointer :: flx_dst_dep_wet4(:)
    real(r8), pointer :: flx_dst_dep(:)
    real(r8), pointer :: forc_aer(:,:)
    integer :: c, j, fc
    real(r8) :: qin(lbc:ubc)
    real(r8) :: qout(lbc:ubc)
    real(r8) :: wgdif
    real(r8) :: vol_liq(lbc:ubc,-nlevsno+1:0)
    real(r8) :: vol_ice(lbc:ubc,-nlevsno+1:0)
    real(r8) :: eff_porosity(lbc:ubc,-nlevsno+1:0)
    integer :: g
    real(r8) :: qin_bc_phi(lbc:ubc)
    real(r8) :: qout_bc_phi(lbc:ubc)
    real(r8) :: qin_bc_pho(lbc:ubc)
    real(r8) :: qout_bc_pho(lbc:ubc)
    real(r8) :: qin_oc_phi(lbc:ubc)
    real(r8) :: qout_oc_phi(lbc:ubc)
    real(r8) :: qin_oc_pho(lbc:ubc)
    real(r8) :: qout_oc_pho(lbc:ubc)
    real(r8) :: qin_dst1(lbc:ubc)
    real(r8) :: qout_dst1(lbc:ubc)
    real(r8) :: qin_dst2(lbc:ubc)
    real(r8) :: qout_dst2(lbc:ubc)
    real(r8) :: qin_dst3(lbc:ubc)
    real(r8) :: qout_dst3(lbc:ubc)
    real(r8) :: qin_dst4(lbc:ubc)
    real(r8) :: qout_dst4(lbc:ubc)
    real(r8) :: mss_liqice
    snl => clm3%g%l%c%cps%snl
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    qflx_snomelt => clm3%g%l%c%cwf%qflx_snomelt
    qflx_rain_grnd => clm3%g%l%c%cwf%pwf_a%qflx_rain_grnd
    qflx_sub_snow => clm3%g%l%c%cwf%pwf_a%qflx_sub_snow
    qflx_evap_grnd => clm3%g%l%c%cwf%pwf_a%qflx_evap_grnd
    qflx_dew_snow => clm3%g%l%c%cwf%pwf_a%qflx_dew_snow
    qflx_dew_grnd => clm3%g%l%c%cwf%pwf_a%qflx_dew_grnd
    qflx_top_soil => clm3%g%l%c%cwf%qflx_top_soil
    dz => clm3%g%l%c%cps%dz
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    cgridcell => clm3%g%l%c%gridcell
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    flx_bc_dep => clm3%g%l%c%cwf%flx_bc_dep
    flx_bc_dep_wet => clm3%g%l%c%cwf%flx_bc_dep_wet
    flx_bc_dep_dry => clm3%g%l%c%cwf%flx_bc_dep_dry
    flx_bc_dep_phi => clm3%g%l%c%cwf%flx_bc_dep_phi
    flx_bc_dep_pho => clm3%g%l%c%cwf%flx_bc_dep_pho
    flx_oc_dep => clm3%g%l%c%cwf%flx_oc_dep
    flx_oc_dep_wet => clm3%g%l%c%cwf%flx_oc_dep_wet
    flx_oc_dep_dry => clm3%g%l%c%cwf%flx_oc_dep_dry
    flx_oc_dep_phi => clm3%g%l%c%cwf%flx_oc_dep_phi
    flx_oc_dep_pho => clm3%g%l%c%cwf%flx_oc_dep_pho
    flx_dst_dep => clm3%g%l%c%cwf%flx_dst_dep
    flx_dst_dep_wet1 => clm3%g%l%c%cwf%flx_dst_dep_wet1
    flx_dst_dep_dry1 => clm3%g%l%c%cwf%flx_dst_dep_dry1
    flx_dst_dep_wet2 => clm3%g%l%c%cwf%flx_dst_dep_wet2
    flx_dst_dep_dry2 => clm3%g%l%c%cwf%flx_dst_dep_dry2
    flx_dst_dep_wet3 => clm3%g%l%c%cwf%flx_dst_dep_wet3
    flx_dst_dep_dry3 => clm3%g%l%c%cwf%flx_dst_dep_dry3
    flx_dst_dep_wet4 => clm3%g%l%c%cwf%flx_dst_dep_wet4
    flx_dst_dep_dry4 => clm3%g%l%c%cwf%flx_dst_dep_dry4
    forc_aer => clm_a2l%forc_aer
!dir$ concurrent
    do fc = 1,num_snowc
       c = filter_snowc(fc)
       if (do_capsnow(c)) then
          wgdif = h2osoi_ice(c,snl(c)+1) - qflx_sub_snow(c)*dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0._r8) then
             h2osoi_ice(c,snl(c)+1) = 0._r8
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) - qflx_evap_grnd(c) * dtime
       else
          wgdif = h2osoi_ice(c,snl(c)+1) + (qflx_dew_snow(c) - qflx_sub_snow(c)) * dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0._r8) then
             h2osoi_ice(c,snl(c)+1) = 0._r8
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + &
               (qflx_rain_grnd(c) + qflx_dew_grnd(c) - qflx_evap_grnd(c)) * dtime
       end if
       h2osoi_liq(c,snl(c)+1) = max(0._r8, h2osoi_liq(c,snl(c)+1))
    end do
    do j = -nlevsno+1, 0
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             vol_ice(c,j) = min(1._r8, h2osoi_ice(c,j)/(dz(c,j)*denice))
             eff_porosity(c,j) = 1._r8 - vol_ice(c,j)
             vol_liq(c,j) = min(eff_porosity(c,j),h2osoi_liq(c,j)/(dz(c,j)*denh2o))
          end if
       end do
    end do
    qin(:) = 0._r8
    qin_bc_phi(:) = 0._r8
    qin_bc_pho(:) = 0._r8
    qin_oc_phi(:) = 0._r8
    qin_oc_pho(:) = 0._r8
    qin_dst1(:) = 0._r8
    qin_dst2(:) = 0._r8
    qin_dst3(:) = 0._r8
    qin_dst4(:) = 0._r8
    do j = -nlevsno+1, 0
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osoi_liq(c,j) = h2osoi_liq(c,j) + qin(c)
             mss_bcphi(c,j) = mss_bcphi(c,j) + qin_bc_phi(c)
             mss_bcpho(c,j) = mss_bcpho(c,j) + qin_bc_pho(c)
             mss_ocphi(c,j) = mss_ocphi(c,j) + qin_oc_phi(c)
             mss_ocpho(c,j) = mss_ocpho(c,j) + qin_oc_pho(c)
             mss_dst1(c,j) = mss_dst1(c,j) + qin_dst1(c)
             mss_dst2(c,j) = mss_dst2(c,j) + qin_dst2(c)
             mss_dst3(c,j) = mss_dst3(c,j) + qin_dst3(c)
             mss_dst4(c,j) = mss_dst4(c,j) + qin_dst4(c)
             if (j <= -1) then
                if (eff_porosity(c,j) < wimp .OR. eff_porosity(c,j+1) < wimp) then
                   qout(c) = 0._r8
                else
                   qout(c) = max(0._r8,(vol_liq(c,j)-ssi*eff_porosity(c,j))*dz(c,j))
                   qout(c) = min(qout(c),(1._r8-vol_ice(c,j+1)-vol_liq(c,j+1))*dz(c,j+1))
                end if
             else
                qout(c) = max(0._r8,(vol_liq(c,j) - ssi*eff_porosity(c,j))*dz(c,j))
             end if
             qout(c) = qout(c)*1000._r8
             h2osoi_liq(c,j) = h2osoi_liq(c,j) - qout(c)
             qin(c) = qout(c)
             mss_liqice = h2osoi_liq(c,j)+h2osoi_ice(c,j)
             if (mss_liqice < 1E-30_r8) then
                mss_liqice = 1E-30_r8
             endif
             qout_bc_phi(c) = qout(c)*scvng_fct_mlt_bcphi*(mss_bcphi(c,j)/mss_liqice)
             if (qout_bc_phi(c) > mss_bcphi(c,j)) then
                qout_bc_phi(c) = mss_bcphi(c,j)
             endif
             mss_bcphi(c,j) = mss_bcphi(c,j) - qout_bc_phi(c)
             qin_bc_phi(c) = qout_bc_phi(c)
             qout_bc_pho(c) = qout(c)*scvng_fct_mlt_bcpho*(mss_bcpho(c,j)/mss_liqice)
             if (qout_bc_pho(c) > mss_bcpho(c,j)) then
                qout_bc_pho(c) = mss_bcpho(c,j)
             endif
             mss_bcpho(c,j) = mss_bcpho(c,j) - qout_bc_pho(c)
             qin_bc_pho(c) = qout_bc_pho(c)
             qout_oc_phi(c) = qout(c)*scvng_fct_mlt_ocphi*(mss_ocphi(c,j)/mss_liqice)
             if (qout_oc_phi(c) > mss_ocphi(c,j)) then
                qout_oc_phi(c) = mss_ocphi(c,j)
             endif
             mss_ocphi(c,j) = mss_ocphi(c,j) - qout_oc_phi(c)
             qin_oc_phi(c) = qout_oc_phi(c)
             qout_oc_pho(c) = qout(c)*scvng_fct_mlt_ocpho*(mss_ocpho(c,j)/mss_liqice)
             if (qout_oc_pho(c) > mss_ocpho(c,j)) then
                qout_oc_pho(c) = mss_ocpho(c,j)
             endif
             mss_ocpho(c,j) = mss_ocpho(c,j) - qout_oc_pho(c)
             qin_oc_pho(c) = qout_oc_pho(c)
             qout_dst1(c) = qout(c)*scvng_fct_mlt_dst1*(mss_dst1(c,j)/mss_liqice)
             if (qout_dst1(c) > mss_dst1(c,j)) then
                qout_dst1(c) = mss_dst1(c,j)
             endif
             mss_dst1(c,j) = mss_dst1(c,j) - qout_dst1(c)
             qin_dst1(c) = qout_dst1(c)
             qout_dst2(c) = qout(c)*scvng_fct_mlt_dst2*(mss_dst2(c,j)/mss_liqice)
             if (qout_dst2(c) > mss_dst2(c,j)) then
                qout_dst2(c) = mss_dst2(c,j)
             endif
             mss_dst2(c,j) = mss_dst2(c,j) - qout_dst2(c)
             qin_dst2(c) = qout_dst2(c)
             qout_dst3(c) = qout(c)*scvng_fct_mlt_dst3*(mss_dst3(c,j)/mss_liqice)
             if (qout_dst3(c) > mss_dst3(c,j)) then
                qout_dst3(c) = mss_dst3(c,j)
             endif
             mss_dst3(c,j) = mss_dst3(c,j) - qout_dst3(c)
             qin_dst3(c) = qout_dst3(c)
             qout_dst4(c) = qout(c)*scvng_fct_mlt_dst4*(mss_dst4(c,j)/mss_liqice)
             if (qout_dst4(c) > mss_dst4(c,j)) then
                qout_dst4(c) = mss_dst4(c,j)
             endif
             mss_dst4(c,j) = mss_dst4(c,j) - qout_dst4(c)
             qin_dst4(c) = qout_dst4(c)
          end if
       end do
    end do
!dir$ concurrent
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       qflx_top_soil(c) = qout(c) / dtime
    end do
!dir$ concurrent
    do fc = 1, num_nosnowc
       c = filter_nosnowc(fc)
       qflx_top_soil(c) = qflx_rain_grnd(c) + qflx_snomelt(c)
    end do
    do c = lbc,ubc
       g = cgridcell(c)
       flx_bc_dep_dry(c) = forc_aer(g,1) + forc_aer(g,2)
       flx_bc_dep_wet(c) = forc_aer(g,3)
       flx_bc_dep_phi(c) = forc_aer(g,1) + forc_aer(g,3)
       flx_bc_dep_pho(c) = forc_aer(g,2)
       flx_bc_dep(c) = forc_aer(g,1) + forc_aer(g,2) + forc_aer(g,3)
       flx_oc_dep_dry(c) = forc_aer(g,4) + forc_aer(g,5)
       flx_oc_dep_wet(c) = forc_aer(g,6)
       flx_oc_dep_phi(c) = forc_aer(g,4) + forc_aer(g,6)
       flx_oc_dep_pho(c) = forc_aer(g,5)
       flx_oc_dep(c) = forc_aer(g,4) + forc_aer(g,5) + forc_aer(g,6)
       flx_dst_dep_wet1(c) = forc_aer(g,7)
       flx_dst_dep_dry1(c) = forc_aer(g,8)
       flx_dst_dep_wet2(c) = forc_aer(g,9)
       flx_dst_dep_dry2(c) = forc_aer(g,10)
       flx_dst_dep_wet3(c) = forc_aer(g,11)
       flx_dst_dep_dry3(c) = forc_aer(g,12)
       flx_dst_dep_wet4(c) = forc_aer(g,13)
       flx_dst_dep_dry4(c) = forc_aer(g,14)
       flx_dst_dep(c) = forc_aer(g,7) + forc_aer(g,8) + forc_aer(g,9) + &
                             forc_aer(g,10) + forc_aer(g,11) + forc_aer(g,12) + &
                             forc_aer(g,13) + forc_aer(g,14)
    end do
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       mss_bcphi(c,snl(c)+1) = mss_bcphi(c,snl(c)+1) + (flx_bc_dep_phi(c)*dtime)
       mss_bcpho(c,snl(c)+1) = mss_bcpho(c,snl(c)+1) + (flx_bc_dep_pho(c)*dtime)
       mss_ocphi(c,snl(c)+1) = mss_ocphi(c,snl(c)+1) + (flx_oc_dep_phi(c)*dtime)
       mss_ocpho(c,snl(c)+1) = mss_ocpho(c,snl(c)+1) + (flx_oc_dep_pho(c)*dtime)
       mss_dst1(c,snl(c)+1) = mss_dst1(c,snl(c)+1) + (flx_dst_dep_dry1(c) + flx_dst_dep_wet1(c))*dtime
       mss_dst2(c,snl(c)+1) = mss_dst2(c,snl(c)+1) + (flx_dst_dep_dry2(c) + flx_dst_dep_wet2(c))*dtime
       mss_dst3(c,snl(c)+1) = mss_dst3(c,snl(c)+1) + (flx_dst_dep_dry3(c) + flx_dst_dep_wet3(c))*dtime
       mss_dst4(c,snl(c)+1) = mss_dst4(c,snl(c)+1) + (flx_dst_dep_dry4(c) + flx_dst_dep_wet4(c))*dtime
    end do
  end subroutine SnowWater
  subroutine SnowCompaction(lbc, ubc, num_snowc, filter_snowc)
    use clmtype
    use clm_varcon , only : denice, denh2o, tfrz
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_snowc
    integer, intent(in) :: filter_snowc(ubc-lbc+1)
    integer, pointer :: snl(:)
    integer, pointer :: imelt(:,:)
    real(r8), pointer :: frac_iceold(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: dz(:,:)
    integer :: j, c, fc
    real(r8), parameter :: c2 = 23.e-3_r8
    real(r8), parameter :: c3 = 2.777e-6_r8
    real(r8), parameter :: c4 = 0.04_r8
    real(r8), parameter :: c5 = 2.0_r8
    real(r8), parameter :: dm = 100.0_r8
    real(r8), parameter :: eta0 = 9.e+5_r8
    real(r8) :: burden(lbc:ubc)
    real(r8) :: ddz1
    real(r8) :: ddz2
    real(r8) :: ddz3
    real(r8) :: dexpf
    real(r8) :: fi
    real(r8) :: td
    real(r8) :: pdzdtc
    real(r8) :: void
    real(r8) :: wx
    real(r8) :: bi
    snl => clm3%g%l%c%cps%snl
    dz => clm3%g%l%c%cps%dz
    imelt => clm3%g%l%c%cps%imelt
    frac_iceold => clm3%g%l%c%cps%frac_iceold
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    burden(:) = 0._r8
    do j = -nlevsno+1, 0
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             wx = h2osoi_ice(c,j) + h2osoi_liq(c,j)
             void = 1._r8 - (h2osoi_ice(c,j)/denice + h2osoi_liq(c,j)/denh2o) / dz(c,j)
             if (void > 0.001_r8 .and. h2osoi_ice(c,j) > .1_r8) then
                bi = h2osoi_ice(c,j) / dz(c,j)
                fi = h2osoi_ice(c,j) / wx
                td = tfrz-t_soisno(c,j)
                dexpf = exp(-c4*td)
                ddz1 = -c3*dexpf
                if (bi > dm) ddz1 = ddz1*exp(-46.0e-3_r8*(bi-dm))
                if (h2osoi_liq(c,j) > 0.01_r8*dz(c,j)) ddz1=ddz1*c5
                ddz2 = -(burden(c)+wx/2._r8)*exp(-0.08_r8*td - c2*bi)/eta0
                if (imelt(c,j) == 1) then
                   ddz3 = - 1._r8/dtime * max(0._r8,(frac_iceold(c,j) - fi)/frac_iceold(c,j))
                else
                   ddz3 = 0._r8
                end if
                pdzdtc = ddz1 + ddz2 + ddz3
                dz(c,j) = dz(c,j) * (1._r8+pdzdtc*dtime)
             end if
             burden(c) = burden(c) + wx
          end if
       end do
    end do
  end subroutine SnowCompaction
  subroutine CombineSnowLayers(lbc, ubc, num_snowc, filter_snowc)
    use clmtype
    use clm_varcon, only : istsoil, isturb
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(inout) :: num_snowc
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)
    integer, pointer :: clandunit(:)
    integer, pointer :: ltype(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: snw_rds(:,:)
    integer :: c, fc
    integer :: i,k
    integer :: j,l
    integer :: msn_old(lbc:ubc)
    integer :: mssi(lbc:ubc)
    integer :: neibor
    real(r8):: zwice(lbc:ubc)
    real(r8):: zwliq (lbc:ubc)
    real(r8):: dzmin(5)
    data dzmin /0.010_r8, 0.015_r8, 0.025_r8, 0.055_r8, 0.115_r8/
    ltype => clm3%g%l%itype
    clandunit => clm3%g%l%c%landunit
    snl => clm3%g%l%c%cps%snl
    snowdp => clm3%g%l%c%cps%snowdp
    h2osno => clm3%g%l%c%cws%h2osno
    dz => clm3%g%l%c%cps%dz
    zi => clm3%g%l%c%cps%zi
    z => clm3%g%l%c%cps%z
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    snw_rds => clm3%g%l%c%cps%snw_rds
!dir$ concurrent
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       msn_old(c) = snl(c)
    end do
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       l = clandunit(c)
       do j = msn_old(c)+1,0
          if (h2osoi_ice(c,j) <= .1_r8) then
             if (ltype(l) == istsoil .or. ltype(l)==isturb) then
                h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) + h2osoi_liq(c,j)
                h2osoi_ice(c,j+1) = h2osoi_ice(c,j+1) + h2osoi_ice(c,j)
                if (j /= 0) then
                   mss_bcphi(c,j+1) = mss_bcphi(c,j+1) + mss_bcphi(c,j)
                   mss_bcpho(c,j+1) = mss_bcpho(c,j+1) + mss_bcpho(c,j)
                   mss_ocphi(c,j+1) = mss_ocphi(c,j+1) + mss_ocphi(c,j)
                   mss_ocpho(c,j+1) = mss_ocpho(c,j+1) + mss_ocpho(c,j)
                   mss_dst1(c,j+1) = mss_dst1(c,j+1) + mss_dst1(c,j)
                   mss_dst2(c,j+1) = mss_dst2(c,j+1) + mss_dst2(c,j)
                   mss_dst3(c,j+1) = mss_dst3(c,j+1) + mss_dst3(c,j)
                   mss_dst4(c,j+1) = mss_dst4(c,j+1) + mss_dst4(c,j)
                end if
             else if (ltype(l) /= istsoil .and. ltype(l) /= isturb .and. j /= 0) then
                h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) + h2osoi_liq(c,j)
                h2osoi_ice(c,j+1) = h2osoi_ice(c,j+1) + h2osoi_ice(c,j)
                mss_bcphi(c,j+1) = mss_bcphi(c,j+1) + mss_bcphi(c,j)
                mss_bcpho(c,j+1) = mss_bcpho(c,j+1) + mss_bcpho(c,j)
                mss_ocphi(c,j+1) = mss_ocphi(c,j+1) + mss_ocphi(c,j)
                mss_ocpho(c,j+1) = mss_ocpho(c,j+1) + mss_ocpho(c,j)
                mss_dst1(c,j+1) = mss_dst1(c,j+1) + mss_dst1(c,j)
                mss_dst2(c,j+1) = mss_dst2(c,j+1) + mss_dst2(c,j)
                mss_dst3(c,j+1) = mss_dst3(c,j+1) + mss_dst3(c,j)
                mss_dst4(c,j+1) = mss_dst4(c,j+1) + mss_dst4(c,j)
             end if
             if (j > snl(c)+1 .and. snl(c) < -1) then
                do i = j, snl(c)+2, -1
                   t_soisno(c,i) = t_soisno(c,i-1)
                   h2osoi_liq(c,i) = h2osoi_liq(c,i-1)
                   h2osoi_ice(c,i) = h2osoi_ice(c,i-1)
                   mss_bcphi(c,i) = mss_bcphi(c,i-1)
                   mss_bcpho(c,i) = mss_bcpho(c,i-1)
                   mss_ocphi(c,i) = mss_ocphi(c,i-1)
                   mss_ocpho(c,i) = mss_ocpho(c,i-1)
                   mss_dst1(c,i) = mss_dst1(c,i-1)
                   mss_dst2(c,i) = mss_dst2(c,i-1)
                   mss_dst3(c,i) = mss_dst3(c,i-1)
                   mss_dst4(c,i) = mss_dst4(c,i-1)
                   snw_rds(c,i) = snw_rds(c,i-1)
                   dz(c,i) = dz(c,i-1)
                end do
             end if
             snl(c) = snl(c) + 1
          end if
       end do
    end do
!dir$ concurrent
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       h2osno(c) = 0._r8
       snowdp(c) = 0._r8
       zwice(c) = 0._r8
       zwliq(c) = 0._r8
    end do
    do j = -nlevsno+1,0
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             snowdp(c) = snowdp(c) + dz(c,j)
             zwice(c) = zwice(c) + h2osoi_ice(c,j)
             zwliq(c) = zwliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do
!dir$ concurrent
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       l = clandunit(c)
       if (snowdp(c) < 0.01_r8 .and. snowdp(c) > 0._r8) then
          snl(c) = 0
          h2osno(c) = zwice(c)
          mss_bcphi(c,:) = 0._r8
          mss_bcpho(c,:) = 0._r8
          mss_ocphi(c,:) = 0._r8
          mss_ocpho(c,:) = 0._r8
          mss_dst1(c,:) = 0._r8
          mss_dst2(c,:) = 0._r8
          mss_dst3(c,:) = 0._r8
          mss_dst4(c,:) = 0._r8
          if (h2osno(c) <= 0._r8) snowdp(c) = 0._r8
          if (ltype(l) == istsoil .or. ltype(l) == isturb) then
             h2osoi_liq(c,1) = h2osoi_liq(c,1) + zwliq(c)
          end if
       end if
    end do
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       if (snl(c) < -1) then
          msn_old(c) = snl(c)
          mssi(c) = 1
          do i = msn_old(c)+1,0
             if (dz(c,i) < dzmin(mssi(c))) then
                if (i == snl(c)+1) then
                   neibor = i + 1
                else if (i == 0) then
                   neibor = i - 1
                else
                   neibor = i + 1
                   if ((dz(c,i-1)+dz(c,i)) < (dz(c,i+1)+dz(c,i))) neibor = i-1
                end if
                if (neibor > i) then
                   j = neibor
                   l = i
                else
                   j = i
                   l = neibor
                end if
                mss_bcphi(c,j)=mss_bcphi(c,j)+mss_bcphi(c,l)
                mss_bcpho(c,j)=mss_bcpho(c,j)+mss_bcpho(c,l)
                mss_ocphi(c,j)=mss_ocphi(c,j)+mss_ocphi(c,l)
                mss_ocpho(c,j)=mss_ocpho(c,j)+mss_ocpho(c,l)
                mss_dst1(c,j)=mss_dst1(c,j)+mss_dst1(c,l)
                mss_dst2(c,j)=mss_dst2(c,j)+mss_dst2(c,l)
                mss_dst3(c,j)=mss_dst3(c,j)+mss_dst3(c,l)
                mss_dst4(c,j)=mss_dst4(c,j)+mss_dst4(c,l)
                snw_rds(c,j) = (snw_rds(c,j)*(h2osoi_liq(c,j)+h2osoi_ice(c,j)) + &
                               snw_rds(c,l)*(h2osoi_liq(c,l)+h2osoi_ice(c,l))) / &
                               (h2osoi_liq(c,j)+h2osoi_ice(c,j)+h2osoi_liq(c,l)+h2osoi_ice(c,l))
                call Combo (dz(c,j), h2osoi_liq(c,j), h2osoi_ice(c,j), &
                   t_soisno(c,j), dz(c,l), h2osoi_liq(c,l), h2osoi_ice(c,l), t_soisno(c,l) )
                if (j-1 > snl(c)+1) then
                   do k = j-1, snl(c)+2, -1
                      t_soisno(c,k) = t_soisno(c,k-1)
                      h2osoi_ice(c,k) = h2osoi_ice(c,k-1)
                      h2osoi_liq(c,k) = h2osoi_liq(c,k-1)
                      mss_bcphi(c,k) = mss_bcphi(c,k-1)
                      mss_bcpho(c,k) = mss_bcpho(c,k-1)
                      mss_ocphi(c,k) = mss_ocphi(c,k-1)
                      mss_ocpho(c,k) = mss_ocpho(c,k-1)
                      mss_dst1(c,k) = mss_dst1(c,k-1)
                      mss_dst2(c,k) = mss_dst2(c,k-1)
                      mss_dst3(c,k) = mss_dst3(c,k-1)
                      mss_dst4(c,k) = mss_dst4(c,k-1)
                      snw_rds(c,k) = snw_rds(c,k-1)
                      dz(c,k) = dz(c,k-1)
                   end do
                end if
                snl(c) = snl(c) + 1
                if (snl(c) >= -1) EXIT
             else
                mssi(c) = mssi(c) + 1
             end if
          end do
       end if
    end do
    do j = 0, -nlevsno+1, -1
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c) + 1) then
             z(c,j) = zi(c,j) - 0.5_r8*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do
  end subroutine CombineSnowLayers
  subroutine DivideSnowLayers(lbc, ubc, num_snowc, filter_snowc)
    use clmtype
    use clm_varcon, only : tfrz
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(inout) :: num_snowc
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)
    integer , pointer :: snl(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: snw_rds(:,:)
    integer :: j, c, fc
    real(r8) :: drr
    integer :: msno
    real(r8) :: dzsno(lbc:ubc,nlevsno)
    real(r8) :: swice(lbc:ubc,nlevsno)
    real(r8) :: swliq(lbc:ubc,nlevsno)
    real(r8) :: tsno(lbc:ubc ,nlevsno)
    real(r8) :: zwice
    real(r8) :: zwliq
    real(r8) :: propor
    real(r8) :: dtdz
    real(r8) :: mbc_phi(lbc:ubc,nlevsno)
    real(r8) :: zmbc_phi
    real(r8) :: mbc_pho(lbc:ubc,nlevsno)
    real(r8) :: zmbc_pho
    real(r8) :: moc_phi(lbc:ubc,nlevsno)
    real(r8) :: zmoc_phi
    real(r8) :: moc_pho(lbc:ubc,nlevsno)
    real(r8) :: zmoc_pho
    real(r8) :: mdst1(lbc:ubc,nlevsno)
    real(r8) :: zmdst1
    real(r8) :: mdst2(lbc:ubc,nlevsno)
    real(r8) :: zmdst2
    real(r8) :: mdst3(lbc:ubc,nlevsno)
    real(r8) :: zmdst3
    real(r8) :: mdst4(lbc:ubc,nlevsno)
    real(r8) :: zmdst4
    real(r8) :: rds(lbc:ubc,nlevsno)
    snl => clm3%g%l%c%cps%snl
    dz => clm3%g%l%c%cps%dz
    zi => clm3%g%l%c%cps%zi
    z => clm3%g%l%c%cps%z
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    snw_rds => clm3%g%l%c%cps%snw_rds
    do j = 1,nlevsno
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j <= abs(snl(c))) then
             dzsno(c,j) = dz(c,j+snl(c))
             swice(c,j) = h2osoi_ice(c,j+snl(c))
             swliq(c,j) = h2osoi_liq(c,j+snl(c))
             tsno(c,j) = t_soisno(c,j+snl(c))
             mbc_phi(c,j) = mss_bcphi(c,j+snl(c))
             mbc_pho(c,j) = mss_bcpho(c,j+snl(c))
             moc_phi(c,j) = mss_ocphi(c,j+snl(c))
             moc_pho(c,j) = mss_ocpho(c,j+snl(c))
             mdst1(c,j) = mss_dst1(c,j+snl(c))
             mdst2(c,j) = mss_dst2(c,j+snl(c))
             mdst3(c,j) = mss_dst3(c,j+snl(c))
             mdst4(c,j) = mss_dst4(c,j+snl(c))
             rds(c,j) = snw_rds(c,j+snl(c))
          end if
       end do
    end do
!dir$ concurrent
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       msno = abs(snl(c))
       if (msno == 1) then
          if (dzsno(c,1) > 0.03_r8) then
             msno = 2
             dzsno(c,1) = dzsno(c,1)/2._r8
             swice(c,1) = swice(c,1)/2._r8
             swliq(c,1) = swliq(c,1)/2._r8
             dzsno(c,2) = dzsno(c,1)
             swice(c,2) = swice(c,1)
             swliq(c,2) = swliq(c,1)
             tsno(c,2) = tsno(c,1)
             mbc_phi(c,1) = mbc_phi(c,1)/2._r8
             mbc_phi(c,2) = mbc_phi(c,1)
             mbc_pho(c,1) = mbc_pho(c,1)/2._r8
             mbc_pho(c,2) = mbc_pho(c,1)
             moc_phi(c,1) = moc_phi(c,1)/2._r8
             moc_phi(c,2) = moc_phi(c,1)
             moc_pho(c,1) = moc_pho(c,1)/2._r8
             moc_pho(c,2) = moc_pho(c,1)
             mdst1(c,1) = mdst1(c,1)/2._r8
             mdst1(c,2) = mdst1(c,1)
             mdst2(c,1) = mdst2(c,1)/2._r8
             mdst2(c,2) = mdst2(c,1)
             mdst3(c,1) = mdst3(c,1)/2._r8
             mdst3(c,2) = mdst3(c,1)
             mdst4(c,1) = mdst4(c,1)/2._r8
             mdst4(c,2) = mdst4(c,1)
             rds(c,2) = rds(c,1)
          end if
       end if
       if (msno > 1) then
          if (dzsno(c,1) > 0.02_r8) then
             drr = dzsno(c,1) - 0.02_r8
             propor = drr/dzsno(c,1)
             zwice = propor*swice(c,1)
             zwliq = propor*swliq(c,1)
             zmbc_phi = propor*mbc_phi(c,1)
             zmbc_pho = propor*mbc_pho(c,1)
             zmoc_phi = propor*moc_phi(c,1)
             zmoc_pho = propor*moc_pho(c,1)
             zmdst1 = propor*mdst1(c,1)
             zmdst2 = propor*mdst2(c,1)
             zmdst3 = propor*mdst3(c,1)
             zmdst4 = propor*mdst4(c,1)
             propor = 0.02_r8/dzsno(c,1)
             swice(c,1) = propor*swice(c,1)
             swliq(c,1) = propor*swliq(c,1)
             mbc_phi(c,1) = propor*mbc_phi(c,1)
             mbc_pho(c,1) = propor*mbc_pho(c,1)
             moc_phi(c,1) = propor*moc_phi(c,1)
             moc_pho(c,1) = propor*moc_pho(c,1)
             mdst1(c,1) = propor*mdst1(c,1)
             mdst2(c,1) = propor*mdst2(c,1)
             mdst3(c,1) = propor*mdst3(c,1)
             mdst4(c,1) = propor*mdst4(c,1)
             dzsno(c,1) = 0.02_r8
             mbc_phi(c,2) = mbc_phi(c,2)+zmbc_phi
             mbc_pho(c,2) = mbc_pho(c,2)+zmbc_pho
             moc_phi(c,2) = moc_phi(c,2)+zmoc_phi
             moc_pho(c,2) = moc_pho(c,2)+zmoc_pho
             mdst1(c,2) = mdst1(c,2)+zmdst1
             mdst2(c,2) = mdst2(c,2)+zmdst2
             mdst3(c,2) = mdst3(c,2)+zmdst3
             mdst4(c,2) = mdst4(c,2)+zmdst4
             rds(c,2) = rds(c,1)
             call Combo (dzsno(c,2), swliq(c,2), swice(c,2), tsno(c,2), drr, &
                  zwliq, zwice, tsno(c,1))
             if (msno <= 2 .and. dzsno(c,2) > 0.07_r8) then
                msno = 3
                dtdz = (tsno(c,1) - tsno(c,2))/((dzsno(c,1)+dzsno(c,2))/2._r8)
                dzsno(c,2) = dzsno(c,2)/2._r8
                swice(c,2) = swice(c,2)/2._r8
                swliq(c,2) = swliq(c,2)/2._r8
                dzsno(c,3) = dzsno(c,2)
                swice(c,3) = swice(c,2)
                swliq(c,3) = swliq(c,2)
                tsno(c,3) = tsno(c,2) - dtdz*dzsno(c,2)/2._r8
                if (tsno(c,3) >= tfrz) then
                   tsno(c,3) = tsno(c,2)
                else
                   tsno(c,2) = tsno(c,2) + dtdz*dzsno(c,2)/2._r8
                endif
                mbc_phi(c,2) = mbc_phi(c,2)/2._r8
                mbc_phi(c,3) = mbc_phi(c,2)
                mbc_pho(c,2) = mbc_pho(c,2)/2._r8
                mbc_pho(c,3) = mbc_pho(c,2)
                moc_phi(c,2) = moc_phi(c,2)/2._r8
                moc_phi(c,3) = moc_phi(c,2)
                moc_pho(c,2) = moc_pho(c,2)/2._r8
                moc_pho(c,3) = moc_pho(c,2)
                mdst1(c,2) = mdst1(c,2)/2._r8
                mdst1(c,3) = mdst1(c,2)
                mdst2(c,2) = mdst2(c,2)/2._r8
                mdst2(c,3) = mdst2(c,2)
                mdst3(c,2) = mdst3(c,2)/2._r8
                mdst3(c,3) = mdst3(c,2)
                mdst4(c,2) = mdst4(c,2)/2._r8
                mdst4(c,3) = mdst4(c,2)
                rds(c,3) = rds(c,2)
             end if
          end if
       end if
       if (msno > 2) then
          if (dzsno(c,2) > 0.05_r8) then
             drr = dzsno(c,2) - 0.05_r8
             propor = drr/dzsno(c,2)
             zwice = propor*swice(c,2)
             zwliq = propor*swliq(c,2)
             zmbc_phi = propor*mbc_phi(c,2)
             zmbc_pho = propor*mbc_pho(c,2)
             zmoc_phi = propor*moc_phi(c,2)
             zmoc_pho = propor*moc_pho(c,2)
             zmdst1 = propor*mdst1(c,2)
             zmdst2 = propor*mdst2(c,2)
             zmdst3 = propor*mdst3(c,2)
             zmdst4 = propor*mdst4(c,2)
             propor = 0.05_r8/dzsno(c,2)
             swice(c,2) = propor*swice(c,2)
             swliq(c,2) = propor*swliq(c,2)
             mbc_phi(c,2) = propor*mbc_phi(c,2)
             mbc_pho(c,2) = propor*mbc_pho(c,2)
             moc_phi(c,2) = propor*moc_phi(c,2)
             moc_pho(c,2) = propor*moc_pho(c,2)
             mdst1(c,2) = propor*mdst1(c,2)
             mdst2(c,2) = propor*mdst2(c,2)
             mdst3(c,2) = propor*mdst3(c,2)
             mdst4(c,2) = propor*mdst4(c,2)
             dzsno(c,2) = 0.05_r8
             mbc_phi(c,3) = mbc_phi(c,3)+zmbc_phi
             mbc_pho(c,3) = mbc_pho(c,3)+zmbc_pho
             moc_phi(c,3) = moc_phi(c,3)+zmoc_phi
             moc_pho(c,3) = moc_pho(c,3)+zmoc_pho
             mdst1(c,3) = mdst1(c,3)+zmdst1
             mdst2(c,3) = mdst2(c,3)+zmdst2
             mdst3(c,3) = mdst3(c,3)+zmdst3
             mdst4(c,3) = mdst4(c,3)+zmdst4
             rds(c,3) = rds(c,2)
             call Combo (dzsno(c,3), swliq(c,3), swice(c,3), tsno(c,3), drr, &
                  zwliq, zwice, tsno(c,2))
             if (msno <= 3 .and. dzsno(c,3) > 0.18_r8) then
                msno = 4
                dtdz = (tsno(c,2) - tsno(c,3))/((dzsno(c,2)+dzsno(c,3))/2._r8)
                dzsno(c,3) = dzsno(c,3)/2._r8
                swice(c,3) = swice(c,3)/2._r8
                swliq(c,3) = swliq(c,3)/2._r8
                dzsno(c,4) = dzsno(c,3)
                swice(c,4) = swice(c,3)
                swliq(c,4) = swliq(c,3)
                tsno(c,4) = tsno(c,3) - dtdz*dzsno(c,3)/2._r8
                if (tsno(c,4) >= tfrz) then
                   tsno(c,4) = tsno(c,3)
                else
                   tsno(c,3) = tsno(c,3) + dtdz*dzsno(c,3)/2._r8
                endif
                mbc_phi(c,3) = mbc_phi(c,3)/2._r8
                mbc_phi(c,4) = mbc_phi(c,3)
                mbc_pho(c,3) = mbc_pho(c,3)/2._r8
                mbc_pho(c,4) = mbc_pho(c,3)
                moc_phi(c,3) = moc_phi(c,3)/2._r8
                moc_phi(c,4) = moc_phi(c,3)
                moc_pho(c,3) = moc_pho(c,3)/2._r8
                moc_pho(c,4) = moc_pho(c,3)
                mdst1(c,3) = mdst1(c,3)/2._r8
                mdst1(c,4) = mdst1(c,3)
                mdst2(c,3) = mdst2(c,3)/2._r8
                mdst2(c,4) = mdst2(c,3)
                mdst3(c,3) = mdst3(c,3)/2._r8
                mdst3(c,4) = mdst3(c,3)
                mdst4(c,3) = mdst4(c,3)/2._r8
                mdst4(c,4) = mdst4(c,3)
                rds(c,4) = rds(c,3)
             end if
          end if
       end if
       if (msno > 3) then
          if (dzsno(c,3) > 0.11_r8) then
             drr = dzsno(c,3) - 0.11_r8
             propor = drr/dzsno(c,3)
             zwice = propor*swice(c,3)
             zwliq = propor*swliq(c,3)
             zmbc_phi = propor*mbc_phi(c,3)
             zmbc_pho = propor*mbc_pho(c,3)
             zmoc_phi = propor*moc_phi(c,3)
             zmoc_pho = propor*moc_pho(c,3)
             zmdst1 = propor*mdst1(c,3)
             zmdst2 = propor*mdst2(c,3)
             zmdst3 = propor*mdst3(c,3)
             zmdst4 = propor*mdst4(c,3)
             propor = 0.11_r8/dzsno(c,3)
             swice(c,3) = propor*swice(c,3)
             swliq(c,3) = propor*swliq(c,3)
             mbc_phi(c,3) = propor*mbc_phi(c,3)
             mbc_pho(c,3) = propor*mbc_pho(c,3)
             moc_phi(c,3) = propor*moc_phi(c,3)
             moc_pho(c,3) = propor*moc_pho(c,3)
             mdst1(c,3) = propor*mdst1(c,3)
             mdst2(c,3) = propor*mdst2(c,3)
             mdst3(c,3) = propor*mdst3(c,3)
             mdst4(c,3) = propor*mdst4(c,3)
             dzsno(c,3) = 0.11_r8
             mbc_phi(c,4) = mbc_phi(c,4)+zmbc_phi
             mbc_pho(c,4) = mbc_pho(c,4)+zmbc_pho
             moc_phi(c,4) = moc_phi(c,4)+zmoc_phi
             moc_pho(c,4) = moc_pho(c,4)+zmoc_pho
             mdst1(c,4) = mdst1(c,4)+zmdst1
             mdst2(c,4) = mdst2(c,4)+zmdst2
             mdst3(c,4) = mdst3(c,4)+zmdst3
             mdst4(c,4) = mdst4(c,4)+zmdst4
             rds(c,4) = rds(c,3)
             call Combo (dzsno(c,4), swliq(c,4), swice(c,4), tsno(c,4), drr, &
                  zwliq, zwice, tsno(c,3))
             if (msno <= 4 .and. dzsno(c,4) > 0.41_r8) then
                msno = 5
                dtdz = (tsno(c,3) - tsno(c,4))/((dzsno(c,3)+dzsno(c,4))/2._r8)
                dzsno(c,4) = dzsno(c,4)/2._r8
                swice(c,4) = swice(c,4)/2._r8
                swliq(c,4) = swliq(c,4)/2._r8
                dzsno(c,5) = dzsno(c,4)
                swice(c,5) = swice(c,4)
                swliq(c,5) = swliq(c,4)
                tsno(c,5) = tsno(c,4) - dtdz*dzsno(c,4)/2._r8
                if (tsno(c,5) >= tfrz) then
                   tsno(c,5) = tsno(c,4)
                else
                   tsno(c,4) = tsno(c,4) + dtdz*dzsno(c,4)/2._r8
                endif
                mbc_phi(c,4) = mbc_phi(c,4)/2._r8
                mbc_phi(c,5) = mbc_phi(c,4)
                mbc_pho(c,4) = mbc_pho(c,4)/2._r8
                mbc_pho(c,5) = mbc_pho(c,4)
                moc_phi(c,4) = moc_phi(c,4)/2._r8
                moc_phi(c,5) = moc_phi(c,4)
                moc_pho(c,4) = moc_pho(c,4)/2._r8
                moc_pho(c,5) = moc_pho(c,4)
                mdst1(c,4) = mdst1(c,4)/2._r8
                mdst1(c,5) = mdst1(c,4)
                mdst2(c,4) = mdst2(c,4)/2._r8
                mdst2(c,5) = mdst2(c,4)
                mdst3(c,4) = mdst3(c,4)/2._r8
                mdst3(c,5) = mdst3(c,4)
                mdst4(c,4) = mdst4(c,4)/2._r8
                mdst4(c,5) = mdst4(c,4)
                rds(c,5) = rds(c,4)
             end if
          end if
       end if
       if (msno > 4) then
          if (dzsno(c,4) > 0.23_r8) then
             drr = dzsno(c,4) - 0.23_r8
             propor = drr/dzsno(c,4)
             zwice = propor*swice(c,4)
             zwliq = propor*swliq(c,4)
             zmbc_phi = propor*mbc_phi(c,4)
             zmbc_pho = propor*mbc_pho(c,4)
             zmoc_phi = propor*moc_phi(c,4)
             zmoc_pho = propor*moc_pho(c,4)
             zmdst1 = propor*mdst1(c,4)
             zmdst2 = propor*mdst2(c,4)
             zmdst3 = propor*mdst3(c,4)
             zmdst4 = propor*mdst4(c,4)
             propor = 0.23_r8/dzsno(c,4)
             swice(c,4) = propor*swice(c,4)
             swliq(c,4) = propor*swliq(c,4)
             mbc_phi(c,4) = propor*mbc_phi(c,4)
             mbc_pho(c,4) = propor*mbc_pho(c,4)
             moc_phi(c,4) = propor*moc_phi(c,4)
             moc_pho(c,4) = propor*moc_pho(c,4)
             mdst1(c,4) = propor*mdst1(c,4)
             mdst2(c,4) = propor*mdst2(c,4)
             mdst3(c,4) = propor*mdst3(c,4)
             mdst4(c,4) = propor*mdst4(c,4)
             dzsno(c,4) = 0.23_r8
             mbc_phi(c,5) = mbc_phi(c,5)+zmbc_phi
             mbc_pho(c,5) = mbc_pho(c,5)+zmbc_pho
             moc_phi(c,5) = moc_phi(c,5)+zmoc_phi
             moc_pho(c,5) = moc_pho(c,5)+zmoc_pho
             mdst1(c,5) = mdst1(c,5)+zmdst1
             mdst2(c,5) = mdst2(c,5)+zmdst2
             mdst3(c,5) = mdst3(c,5)+zmdst3
             mdst4(c,5) = mdst4(c,5)+zmdst4
             rds(c,5) = rds(c,4)
             call Combo (dzsno(c,5), swliq(c,5), swice(c,5), tsno(c,5), drr, &
                  zwliq, zwice, tsno(c,4))
          end if
       end if
       snl(c) = -msno
    end do
    do j = -nlevsno+1,0
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             dz(c,j) = dzsno(c,j-snl(c))
             h2osoi_ice(c,j) = swice(c,j-snl(c))
             h2osoi_liq(c,j) = swliq(c,j-snl(c))
             t_soisno(c,j) = tsno(c,j-snl(c))
             mss_bcphi(c,j) = mbc_phi(c,j-snl(c))
             mss_bcpho(c,j) = mbc_pho(c,j-snl(c))
             mss_ocphi(c,j) = moc_phi(c,j-snl(c))
             mss_ocpho(c,j) = moc_pho(c,j-snl(c))
             mss_dst1(c,j) = mdst1(c,j-snl(c))
             mss_dst2(c,j) = mdst2(c,j-snl(c))
             mss_dst3(c,j) = mdst3(c,j-snl(c))
             mss_dst4(c,j) = mdst4(c,j-snl(c))
             snw_rds(c,j) = rds(c,j-snl(c))
          end if
       end do
    end do
    do j = 0, -nlevsno+1, -1
!dir$ concurrent
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             z(c,j) = zi(c,j) - 0.5_r8*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do
  end subroutine DivideSnowLayers
  subroutine Combo(dz, wliq, wice, t, dz2, wliq2, wice2, t2)
    use clm_varcon, only : cpice, cpliq, tfrz, hfus
    implicit none
    real(r8), intent(in) :: dz2
    real(r8), intent(in) :: wliq2
    real(r8), intent(in) :: wice2
    real(r8), intent(in) :: t2
    real(r8), intent(inout) :: dz
    real(r8), intent(inout) :: wliq
    real(r8), intent(inout) :: wice
    real(r8), intent(inout) :: t
    real(r8) :: dzc
    real(r8) :: wliqc
    real(r8) :: wicec
    real(r8) :: tc
    real(r8) :: h
    real(r8) :: h2
    real(r8) :: hc
    dzc = dz+dz2
    wicec = (wice+wice2)
    wliqc = (wliq+wliq2)
    h = (cpice*wice+cpliq*wliq) * (t-tfrz)+hfus*wliq
    h2= (cpice*wice2+cpliq*wliq2) * (t2-tfrz)+hfus*wliq2
    hc = h + h2
    tc = tfrz + (hc - hfus*wliqc) / (cpice*wicec + cpliq*wliqc)
    dz = dzc
    wice = wicec
    wliq = wliqc
    t = tc
  end subroutine Combo
  subroutine BuildSnowFilter(lbc, ubc, num_nolakec, filter_nolakec, &
                             num_snowc, filter_snowc, &
                             num_nosnowc, filter_nosnowc)
    use clmtype
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(out) :: num_snowc
    integer, intent(out) :: filter_snowc(ubc-lbc+1)
    integer, intent(out) :: num_nosnowc
    integer, intent(out) :: filter_nosnowc(ubc-lbc+1)
    integer , pointer :: snl(:)
    integer :: fc, c
    snl => clm3%g%l%c%cps%snl
    num_snowc = 0
    num_nosnowc = 0
    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       if (snl(c) < 0) then
          num_snowc = num_snowc + 1
          filter_snowc(num_snowc) = c
       else
          num_nosnowc = num_nosnowc + 1
          filter_nosnowc(num_nosnowc) = c
       end if
    end do
  end subroutine BuildSnowFilter
end module SnowHydrologyMod
module STATICEcosysdynMOD
  use shr_kind_mod, only: r8 => shr_kind_r8
  use decompMod , only : get_proc_bounds
  use module_cam_support, only: endrun
  implicit none
  save
  public :: EcosystemDyn
  public :: EcosystemDynini
  public :: interpMonthlyVeg
  private :: readMonthlyVegetation
  integer , private :: InterpMonths1
  real(r8), private :: timwt(2)
  real(r8), allocatable :: mlai1(:)
  real(r8), allocatable :: mlai2(:)
  real(r8), allocatable :: msai1(:)
  real(r8), allocatable :: msai2(:)
  real(r8), allocatable :: mhvt1(:)
  real(r8), allocatable :: mhvt2(:)
  real(r8), allocatable :: mhvb1(:)
  real(r8), allocatable :: mhvb2(:)
contains
  subroutine EcosystemDynini ()
    use nanMod
    implicit none
    integer :: ier
    integer :: begg
    integer :: endg
    integer :: begl
    integer :: endl
    integer :: begc
    integer :: endc
    integer :: begp
    integer :: endp
    InterpMonths1 = -999
    call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
    ier = 0
    if(.not.allocated(mlai1)) allocate (mlai1(begp:endp))
    if(.not.allocated(mlai2)) allocate (mlai2(begp:endp))
    if(.not.allocated(msai1)) allocate (msai1(begp:endp))
    if(.not.allocated(msai2)) allocate (msai2(begp:endp))
    if(.not.allocated(mhvt1)) allocate (mhvt1(begp:endp))
    if(.not.allocated(mhvt2)) allocate (mhvt2(begp:endp))
    if(.not.allocated(mhvb1)) allocate (mhvb1(begp:endp))
    if(.not.allocated(mhvb2)) allocate (mhvb2(begp:endp))
    if (ier /= 0) then
       write (6,*) 'EcosystemDynini allocation error'
       call endrun
    end if
   call CLMDebug('EcosystemDynini mark1')
    mlai1(:) = nan
    mlai2(:) = nan
    msai1(:) = nan
    msai2(:) = nan
    mhvt1(:) = nan
    mhvt2(:) = nan
    mhvb1(:) = nan
    mhvb2(:) = nan
  end subroutine EcosystemDynini
  subroutine EcosystemDyn(lbp, ubp, num_nolakep, filter_nolakep, doalb)
    use clmtype
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
    logical, intent(in) :: doalb
    integer , pointer :: pcolumn(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: tlai(:)
    real(r8), pointer :: tsai(:)
    real(r8), pointer :: htop(:)
    real(r8), pointer :: hbot(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: esai(:)
    integer , pointer :: frac_veg_nosno_alb(:)
    integer :: fp,p,c
    real(r8) :: ol
    real(r8) :: fb
    if (doalb) then
       snowdp => clm3%g%l%c%cps%snowdp
       pcolumn => clm3%g%l%c%p%column
       tlai => clm3%g%l%c%p%pps%tlai
       tsai => clm3%g%l%c%p%pps%tsai
       elai => clm3%g%l%c%p%pps%elai
       esai => clm3%g%l%c%p%pps%esai
       htop => clm3%g%l%c%p%pps%htop
       hbot => clm3%g%l%c%p%pps%hbot
       frac_veg_nosno_alb => clm3%g%l%c%p%pps%frac_veg_nosno_alb
!dir$ concurrent
       do fp = 1, num_nolakep
          p = filter_nolakep(fp)
          c = pcolumn(p)
          tlai(p) = timwt(1)*mlai1(p) + timwt(2)*mlai2(p)
          tsai(p) = timwt(1)*msai1(p) + timwt(2)*msai2(p)
          htop(p) = timwt(1)*mhvt1(p) + timwt(2)*mhvt2(p)
          hbot(p) = timwt(1)*mhvb1(p) + timwt(2)*mhvb2(p)
          ol = min( max(snowdp(c)-hbot(p), 0._r8), htop(p)-hbot(p))
          fb = 1. - ol / max(1.e-06_r8, htop(p)-hbot(p))
          elai(p) = max(tlai(p)*fb, 0.0_r8)
          esai(p) = max(tsai(p)*fb, 0.0_r8)
          if (elai(p) < 0.05) elai(p) = 0._r8
          if (esai(p) < 0.05) esai(p) = 0._r8
          if ((elai(p) + esai(p)) >= 0.05) then
             frac_veg_nosno_alb(p) = 1
          else
             frac_veg_nosno_alb(p) = 0
          end if
       end do
    end if
  end subroutine EcosystemDyn
  subroutine interpMonthlyVeg (kmo, kda)
    implicit none
    integer :: kyr
    integer :: kmo
    integer :: kda
    integer :: ksec
    real(r8):: dtime
    real(r8):: t
    integer :: it(2)
    integer :: months(2)
    integer, dimension(12) :: ndaypm= &
         (/31,28,31,30,31,30,31,31,30,31,30,31/)
    t = (kda-0.5) / ndaypm(kmo)
    it(1) = t + 0.5
    it(2) = it(1) + 1
    months(1) = kmo + it(1) - 1
    months(2) = kmo + it(2) - 1
    if (months(1) < 1) months(1) = 12
    if (months(2) > 12) months(2) = 1
    timwt(1) = (it(1)+0.5) - t
    timwt(2) = 1.-timwt(1)
       call CLMDebug(' call readMonthlyVegetation')
       call readMonthlyVegetation(kmo, kda, months)
  end subroutine interpMonthlyVeg
  subroutine readMonthlyVegetation(kmo, kda, months)
    use clmtype
    use clm_varpar , only : lsmlon, lsmlat, maxpatch_pft, maxpatch, numpft
    use clm_varcon , only: hvt, hvb ,lai,sai
    implicit none
    integer, intent(in) :: kmo
    integer, intent(in) :: kda
    integer, intent(in) :: months(2)
    integer :: i,j,k,l,m,p,ivt
    integer :: begg
    integer :: endg
    integer :: begl
    integer :: endl
    integer :: begc
    integer :: endc
    integer :: begp
    integer :: endp
    integer :: ier
      call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
      do k=1,2
          do p = begp, endp
             m = clm3%g%l%c%p%mxy(p)
             ivt = clm3%g%l%c%p%itype(p)
          call CLMDebug('mark1')
                if((m <= maxpatch_pft.and.ivt/=0).or.ivt==15.or.ivt==16)then
                   if (k == 1) then
                   call CLMDebug('if (k == 1) m1')
                      mlai1(p) = lai(ivt,months(k))
                      msai1(p) = sai(ivt,months(k))
                      mhvt1(p) = hvt(ivt)
                      mhvb1(p) = hvb(ivt)
                   else
                   call CLMDebug('else m1')
                      mlai2(p) = lai(ivt,months(k))
                      msai2(p) = sai(ivt,months(k))
                      mhvt2(p) = hvt(ivt)
                      mhvb2(p) = hvb(ivt)
                   end if
              else
                 call CLMDebug('non vegetated')
                    if (k == 1) then
                     call CLMDebug('if (k == 1) m2')
                     call CLMDebug('test')
                      mlai1(p) = 0_r8
                      msai1(p) = 0_r8
                      mhvt1(p) = 0_r8
                      call CLMDebug('mhvb1(p)')
                       mhvb1(p) = 0_r8
                   else
                     call CLMDebug('else m2')
                      mlai2(p) = 0.
                      msai2(p) = 0.
                      mhvt2(p) = 0.
                      mhvb2(p) = 0.
                   end if
                end if
          end do
       end do
   call CLMDebug('done readMonthlyVegetation')
  end subroutine readMonthlyVegetation
  subroutine EcosystemDyn_dealloc ()
    implicit none
    if(allocated(mlai1)) deallocate (mlai1)
    if(allocated(mlai2)) deallocate (mlai2)
    if(allocated(msai1)) deallocate (msai1)
    if(allocated(msai2)) deallocate (msai2)
    if(allocated(mhvt1)) deallocate (mhvt1)
    if(allocated(mhvt2)) deallocate (mhvt2)
    if(allocated(mhvb1)) deallocate (mhvb1)
    if(allocated(mhvb2)) deallocate (mhvb2)
  end subroutine EcosystemDyn_dealloc
end module STATICEcosysDynMod
module HydrologyLakeMod
  implicit none
  save
  public :: HydrologyLake
contains
  subroutine HydrologyLake(lbp, ubp, num_lakep, filter_lakep)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : hfus, tfrz, spval
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_lakep
    integer, intent(in) :: filter_lakep(ubp-lbp+1)
    integer , pointer :: pcolumn(:)
    integer , pointer :: pgridcell(:)
    real(r8), pointer :: begwb(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: forc_rain(:)
    logical , pointer :: do_capsnow(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: qmelt(:)
    real(r8), pointer :: qflx_evap_soi(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: endwb(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: snowice(:)
    real(r8), pointer :: snowliq(:)
    real(r8), pointer :: eflx_snomelt(:)
    real(r8), pointer :: qflx_infl(:)
    real(r8), pointer :: qflx_snomelt(:)
    real(r8), pointer :: qflx_surf(:)
    real(r8), pointer :: qflx_drain(:)
    real(r8), pointer :: qflx_qrgwl(:)
    real(r8), pointer :: qflx_runoff(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: qflx_evap_tot_col(:)
    real(r8) ,pointer :: soilalpha(:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: fcov(:)
    real(r8), pointer :: fsat(:)
    real(r8), pointer :: qcharge(:)
    real(r8), pointer :: rootr_column(:,:)
    real(r8), pointer :: h2osoi_vol(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), parameter :: snow_bd = 250._r8
    integer :: fp, p, c, g
    real(r8) :: qflx_evap_grnd
    real(r8) :: qflx_dew_grnd
    real(r8) :: qflx_sub_snow
    real(r8) :: qflx_dew_snow
    forc_snow => clm_a2l%forc_snow
    forc_rain => clm_a2l%forc_rain
    begwb => clm3%g%l%c%cwbal%begwb
    endwb => clm3%g%l%c%cwbal%endwb
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    snowdp => clm3%g%l%c%cps%snowdp
    t_grnd => clm3%g%l%c%ces%t_grnd
    h2osno => clm3%g%l%c%cws%h2osno
    snowice => clm3%g%l%c%cws%snowice
    snowliq => clm3%g%l%c%cws%snowliq
    eflx_snomelt => clm3%g%l%c%cef%eflx_snomelt
    qmelt => clm3%g%l%c%cwf%qmelt
    qflx_snomelt => clm3%g%l%c%cwf%qflx_snomelt
    qflx_surf => clm3%g%l%c%cwf%qflx_surf
    qflx_qrgwl => clm3%g%l%c%cwf%qflx_qrgwl
    qflx_runoff => clm3%g%l%c%cwf%qflx_runoff
    qflx_snwcp_ice => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_ice
    qflx_drain => clm3%g%l%c%cwf%qflx_drain
    qflx_infl => clm3%g%l%c%cwf%qflx_infl
    rootr_column => clm3%g%l%c%cps%rootr_column
    h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    qflx_evap_tot_col => clm3%g%l%c%cwf%pwf_a%qflx_evap_tot
    soilalpha => clm3%g%l%c%cws%soilalpha
    zwt => clm3%g%l%c%cws%zwt
    fcov => clm3%g%l%c%cws%fcov
    fsat => clm3%g%l%c%cws%fsat
    qcharge => clm3%g%l%c%cws%qcharge
    pcolumn => clm3%g%l%c%p%column
    pgridcell => clm3%g%l%c%p%gridcell
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    qflx_evap_tot => clm3%g%l%c%p%pwf%qflx_evap_tot
    do fp = 1, num_lakep
       p = filter_lakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)
       qflx_evap_grnd = 0._r8
       qflx_sub_snow = 0._r8
       qflx_dew_snow = 0._r8
       qflx_dew_grnd = 0._r8
       if (qflx_evap_soi(p) >= 0._r8) then
          qflx_sub_snow = min(qflx_evap_soi(p), h2osno(c)/dtime-qmelt(c))
          qflx_evap_grnd = qflx_evap_soi(p) - qflx_sub_snow
       else
          if (t_grnd(c) < tfrz-0.1_r8) then
             qflx_dew_snow = abs(qflx_evap_soi(p))
          else
             qflx_dew_grnd = abs(qflx_evap_soi(p))
          end if
       end if
       if (do_capsnow(c)) then
          h2osno(c) = h2osno(c) - (qmelt(c) + qflx_sub_snow)*dtime
          qflx_snwcp_ice(c) = forc_snow(g) + qflx_dew_snow
       else
          h2osno(c) = h2osno(c) + (forc_snow(g)-qmelt(c)-qflx_sub_snow+qflx_dew_snow)*dtime
          qflx_snwcp_ice(c) = 0._r8
       end if
       h2osno(c) = max(h2osno(c), 0._r8)
       h2osno(c) = max(h2osno(c), 0._r8)
       if (t_grnd(c) > tfrz) h2osno(c) = 0._r8
       snowdp(c) = h2osno(c)/snow_bd
       endwb(c) = h2osno(c)
       eflx_snomelt(c) = qmelt(c)*hfus
       qflx_infl(c) = 0._r8
       qflx_snomelt(c) = qmelt(c)
       qflx_surf(c) = 0._r8
       qflx_drain(c) = 0._r8
       rootr_column(c,:) = spval
       snowice(c) = spval
       snowliq(c) = spval
       soilalpha(c) = spval
       zwt(c) = spval
       fcov(c) = spval
       fsat(c) = spval
       qcharge(c) = spval
       h2osoi_vol(c,:) = spval
       h2osoi_ice(c,:) = spval
       h2osoi_liq(c,:) = spval
       qflx_qrgwl(c) = forc_rain(g) + forc_snow(g) - qflx_evap_tot(p) - qflx_snwcp_ice(c) - &
                           (endwb(c)-begwb(c))/dtime
       qflx_runoff(c) = qflx_drain(c) + qflx_surf(c) + qflx_qrgwl(c)
       qflx_evap_tot_col(c) = qflx_evap_tot(p)
    end do
  end subroutine HydrologyLake
end module HydrologyLakeMod
module Hydrology1Mod
   implicit none
   save
   public :: Hydrology1
contains
   subroutine Hydrology1(lbc, ubc, lbp, ubp, num_nolakec, filter_nolakec, &
                         num_nolakep, filter_nolakep)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : tfrz, istice, istwet, istsoil, isturb, &
                              icol_roof, icol_sunwall, icol_shadewall
    use FracWetMod , only : FracWet
    use subgridAveMod, only : p2c
    use SNICARMod , only : snw_rds_min
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
    integer , pointer :: cgridcell(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: ltype(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: forc_rain(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: forc_t(:)
    logical , pointer :: do_capsnow(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: dewmx(:)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: esai(:)
    real(r8), pointer :: h2ocan_loss(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: h2ocan(:)
    real(r8), pointer :: qflx_prec_intr(:)
    real(r8), pointer :: qflx_prec_grnd(:)
    real(r8), pointer :: qflx_snwcp_liq(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: qflx_snow_grnd_pft(:)
    real(r8), pointer :: qflx_snow_grnd_col(:)
    real(r8), pointer :: qflx_rain_grnd(:)
    real(r8), pointer :: fwet(:)
    real(r8), pointer :: fdry(:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: frac_iceold(:,:)
    real(r8), pointer :: snw_rds(:,:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bctot(:,:)
    real(r8), pointer :: mss_bc_col(:)
    real(r8), pointer :: mss_bc_top(:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_octot(:,:)
    real(r8), pointer :: mss_oc_col(:)
    real(r8), pointer :: mss_oc_top(:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: mss_dsttot(:,:)
    real(r8), pointer :: mss_dst_col(:)
    real(r8), pointer :: mss_dst_top(:)
    integer :: f
    integer :: pi
    integer :: p
    integer :: c
    integer :: l
    integer :: g
    integer :: newnode
    real(r8) :: h2ocanmx
    real(r8) :: fpi
    real(r8) :: xrun
    real(r8) :: dz_snowf
    real(r8) :: bifall
    real(r8) :: fracsnow(lbp:ubp)
    real(r8) :: fracrain(lbp:ubp)
    real(r8) :: qflx_candrip(lbp:ubp)
    real(r8) :: qflx_through_rain(lbp:ubp)
    real(r8) :: qflx_through_snow(lbp:ubp)
    real(r8) :: qflx_prec_grnd_snow(lbp:ubp)
    real(r8) :: qflx_prec_grnd_rain(lbp:ubp)
    pgridcell => clm3%g%l%c%p%gridcell
    forc_rain => clm_a2l%forc_rain
    forc_snow => clm_a2l%forc_snow
    forc_t => clm_a2l%forc_t
    ltype => clm3%g%l%itype
    cgridcell => clm3%g%l%c%gridcell
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    pfti => clm3%g%l%c%pfti
    npfts => clm3%g%l%c%npfts
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    t_grnd => clm3%g%l%c%ces%t_grnd
    snl => clm3%g%l%c%cps%snl
    snowdp => clm3%g%l%c%cps%snowdp
    h2osno => clm3%g%l%c%cws%h2osno
    zi => clm3%g%l%c%cps%zi
    dz => clm3%g%l%c%cps%dz
    z => clm3%g%l%c%cps%z
    frac_iceold => clm3%g%l%c%cps%frac_iceold
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    qflx_snow_grnd_col => clm3%g%l%c%cwf%pwf_a%qflx_snow_grnd
    h2ocan_loss => clm3%g%l%c%cwf%h2ocan_loss
    snw_rds => clm3%g%l%c%cps%snw_rds
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bctot => clm3%g%l%c%cps%mss_bctot
    mss_bc_col => clm3%g%l%c%cps%mss_bc_col
    mss_bc_top => clm3%g%l%c%cps%mss_bc_top
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_octot => clm3%g%l%c%cps%mss_octot
    mss_oc_col => clm3%g%l%c%cps%mss_oc_col
    mss_oc_top => clm3%g%l%c%cps%mss_oc_top
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    mss_dsttot => clm3%g%l%c%cps%mss_dsttot
    mss_dst_col => clm3%g%l%c%cps%mss_dst_col
    mss_dst_top => clm3%g%l%c%cps%mss_dst_top
    plandunit => clm3%g%l%c%p%landunit
    pcolumn => clm3%g%l%c%p%column
    dewmx => clm3%g%l%c%p%pps%dewmx
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    elai => clm3%g%l%c%p%pps%elai
    esai => clm3%g%l%c%p%pps%esai
    h2ocan => clm3%g%l%c%p%pws%h2ocan
    qflx_prec_intr => clm3%g%l%c%p%pwf%qflx_prec_intr
    qflx_prec_grnd => clm3%g%l%c%p%pwf%qflx_prec_grnd
    qflx_snwcp_liq => clm3%g%l%c%p%pwf%qflx_snwcp_liq
    qflx_snwcp_ice => clm3%g%l%c%p%pwf%qflx_snwcp_ice
    qflx_snow_grnd_pft => clm3%g%l%c%p%pwf%qflx_snow_grnd
    qflx_rain_grnd => clm3%g%l%c%p%pwf%qflx_rain_grnd
    fwet => clm3%g%l%c%p%pps%fwet
    fdry => clm3%g%l%c%p%pps%fdry
    do f = 1, num_nolakep
       p = filter_nolakep(f)
       g = pgridcell(p)
       l = plandunit(p)
       c = pcolumn(p)
       if (ltype(l)==istsoil .or. ltype(l)==istwet .or. ltype(l)==isturb)then
          qflx_candrip(p) = 0._r8
          qflx_through_snow(p) = 0._r8
          qflx_through_rain(p) = 0._r8
          qflx_prec_intr(p) = 0._r8
          fracsnow(p) = 0._r8
          fracrain(p) = 0._r8
          if (ctype(c) /= icol_sunwall .and. ctype(c) /= icol_shadewall) then
             if (frac_veg_nosno(p) == 1 .and. (forc_rain(g) + forc_snow(g)) > 0._r8) then
                fracsnow(p) = forc_snow(g)/(forc_snow(g) + forc_rain(g))
                fracrain(p) = forc_rain(g)/(forc_snow(g) + forc_rain(g))
                h2ocanmx = dewmx(p) * (elai(p) + esai(p))
                fpi = 0.25_r8*(1._r8 - exp(-0.5_r8*(elai(p) + esai(p))))
                qflx_through_snow(p) = forc_snow(g) * (1._r8-fpi)
                qflx_through_rain(p) = forc_rain(g) * (1._r8-fpi)
                qflx_prec_intr(p) = (forc_snow(g) + forc_rain(g)) * fpi
                h2ocan(p) = max(0._r8, h2ocan(p) + dtime*qflx_prec_intr(p))
                qflx_candrip(p) = 0._r8
                xrun = (h2ocan(p) - h2ocanmx)/dtime
                if (xrun > 0._r8) then
                   qflx_candrip(p) = xrun
                   h2ocan(p) = h2ocanmx
                end if
             end if
          end if
       else if (ltype(l) == istice) then
          h2ocan(p) = 0._r8
          qflx_candrip(p) = 0._r8
          qflx_through_snow(p) = 0._r8
          qflx_through_rain(p) = 0._r8
          qflx_prec_intr(p) = 0._r8
          fracsnow(p) = 0._r8
          fracrain(p) = 0._r8
       end if
       if (ctype(c) /= icol_sunwall .and. ctype(c) /= icol_shadewall) then
          if (frac_veg_nosno(p) == 0) then
             qflx_prec_grnd_snow(p) = forc_snow(g)
             qflx_prec_grnd_rain(p) = forc_rain(g) + h2ocan_loss(c)
          else
             qflx_prec_grnd_snow(p) = qflx_through_snow(p) + (qflx_candrip(p) * fracsnow(p))
             qflx_prec_grnd_rain(p) = qflx_through_rain(p) + (qflx_candrip(p) * fracrain(p)) + h2ocan_loss(c)
          end if
       else
          qflx_prec_grnd_snow(p) = 0.
          qflx_prec_grnd_rain(p) = 0.
       end if
       qflx_prec_grnd(p) = qflx_prec_grnd_snow(p) + qflx_prec_grnd_rain(p)
       if (do_capsnow(c)) then
          qflx_snwcp_liq(p) = qflx_prec_grnd_rain(p)
          qflx_snwcp_ice(p) = qflx_prec_grnd_snow(p)
          qflx_snow_grnd_pft(p) = 0._r8
          qflx_rain_grnd(p) = 0._r8
       else
          qflx_snwcp_liq(p) = 0._r8
          qflx_snwcp_ice(p) = 0._r8
          qflx_snow_grnd_pft(p) = qflx_prec_grnd_snow(p)
          qflx_rain_grnd(p) = qflx_prec_grnd_rain(p)
       end if
    end do
    call FracWet(num_nolakep, filter_nolakep)
    call p2c(num_nolakec, filter_nolakec, qflx_snow_grnd_pft, qflx_snow_grnd_col)
    do f = 1, num_nolakec
       c = filter_nolakec(f)
       l = clandunit(c)
       g = cgridcell(c)
       if (do_capsnow(c)) then
          dz_snowf = 0._r8
       else
          if (forc_t(g) > tfrz + 2._r8) then
             bifall=50._r8 + 1.7_r8*(17.0_r8)**1.5_r8
          else if (forc_t(g) > tfrz - 15._r8) then
             bifall=50._r8 + 1.7_r8*(forc_t(g) - tfrz + 15._r8)**1.5_r8
          else
             bifall=50._r8
          end if
          dz_snowf = qflx_snow_grnd_col(c)/bifall
          snowdp(c) = snowdp(c) + dz_snowf*dtime
          h2osno(c) = h2osno(c) + qflx_snow_grnd_col(c)*dtime
       end if
       if (ltype(l)==istwet .and. t_grnd(c)>tfrz) then
          h2osno(c)=0._r8
          snowdp(c)=0._r8
       end if
       newnode = 0
       if (snl(c) == 0 .and. qflx_snow_grnd_col(c) > 0.0_r8 .and. snowdp(c) >= 0.01_r8) then
          newnode = 1
          snl(c) = -1
          dz(c,0) = snowdp(c)
          z(c,0) = -0.5_r8*dz(c,0)
          zi(c,-1) = -dz(c,0)
          t_soisno(c,0) = min(tfrz, forc_t(g))
          h2osoi_ice(c,0) = h2osno(c)
          h2osoi_liq(c,0) = 0._r8
          frac_iceold(c,0) = 1._r8
          snw_rds(c,0) = snw_rds_min
          mss_bcpho(c,:) = 0._r8
          mss_bcphi(c,:) = 0._r8
          mss_bctot(c,:) = 0._r8
          mss_bc_col(c) = 0._r8
          mss_bc_top(c) = 0._r8
          mss_ocpho(c,:) = 0._r8
          mss_ocphi(c,:) = 0._r8
          mss_octot(c,:) = 0._r8
          mss_oc_col(c) = 0._r8
          mss_oc_top(c) = 0._r8
          mss_dst1(c,:) = 0._r8
          mss_dst2(c,:) = 0._r8
          mss_dst3(c,:) = 0._r8
          mss_dst4(c,:) = 0._r8
          mss_dsttot(c,:) = 0._r8
          mss_dst_col(c) = 0._r8
          mss_dst_top(c) = 0._r8
       end if
       if (snl(c) < 0 .and. newnode == 0) then
          h2osoi_ice(c,snl(c)+1) = h2osoi_ice(c,snl(c)+1)+dtime*qflx_snow_grnd_col(c)
          dz(c,snl(c)+1) = dz(c,snl(c)+1)+dz_snowf*dtime
       end if
    end do
  end subroutine Hydrology1
end module Hydrology1Mod
module Hydrology2Mod
  implicit none
  save
  public :: Hydrology2
contains
  subroutine Hydrology2(lbc, ubc, lbp, ubp, &
                        num_nolakec, filter_nolakec, &
                        num_hydrologyc, filter_hydrologyc, &
                        num_urbanc, filter_urbanc, &
                        num_snowc, filter_snowc, &
                        num_nosnowc, filter_nosnowc)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use clm_varcon , only : denh2o, denice, spval, &
                                 istice, istwet, istsoil, isturb, &
                                 icol_roof, icol_road_imperv, icol_road_perv, icol_sunwall, &
                                 icol_shadewall
    use clm_varpar , only : nlevgrnd, nlevsno, nlevsoi
    use SnowHydrologyMod, only : SnowCompaction, CombineSnowLayers, DivideSnowLayers, &
                                 SnowWater, BuildSnowFilter
    use SoilHydrologyMod, only : Infiltration, SoilWater, Drainage, SurfaceRunoff
    use globals , only : nstep,dtime,is_perpetual
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_hydrologyc
    integer, intent(in) :: filter_hydrologyc(ubc-lbc+1)
    integer, intent(in) :: num_urbanc
    integer, intent(in) :: filter_urbanc(ubc-lbc+1)
    integer :: num_snowc
    integer :: filter_snowc(ubc-lbc+1)
    integer :: num_nosnowc
    integer :: filter_nosnowc(ubc-lbc+1)
    integer , pointer :: cgridcell(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: ityplun(:)
    integer , pointer :: ctype(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: h2ocan(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: forc_rain(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: begwb(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: bsw2(:,:)
    real(r8), pointer :: psisat(:,:)
    real(r8), pointer :: vwcsat(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: fcov(:)
    real(r8), pointer :: fsat(:)
    real(r8), pointer :: wa(:)
    real(r8), pointer :: qcharge(:)
    real(r8), pointer :: smp_l(:,:)
    real(r8), pointer :: hk_l(:,:)
    real(r8), pointer :: qflx_rsub_sat(:)
    real(r8), pointer :: endwb(:)
    real(r8), pointer :: wf(:)
    real(r8), pointer :: snowice(:)
    real(r8), pointer :: snowliq(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: t_soi_10cm(:)
    real(r8), pointer :: h2osoi_liqice_10cm(:)
    real(r8), pointer :: h2osoi_vol(:,:)
    real(r8), pointer :: qflx_drain(:)
    real(r8), pointer :: qflx_surf(:)
    real(r8), pointer :: qflx_infl(:)
    real(r8), pointer :: qflx_qrgwl(:)
    real(r8), pointer :: qflx_runoff(:)
    real(r8), pointer :: qflx_runoff_u(:)
    real(r8), pointer :: qflx_runoff_r(:)
    real(r8), pointer :: t_grnd_u(:)
    real(r8), pointer :: t_grnd_r(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: soilpsi(:,:)
    real(r8), pointer :: snot_top(:)
    real(r8), pointer :: dTdz_top(:)
    real(r8), pointer :: snw_rds(:,:)
    real(r8), pointer :: snw_rds_top(:)
    real(r8), pointer :: sno_liq_top(:)
    real(r8), pointer :: frac_sno(:)
    real(r8), pointer :: h2osno_top(:)
    real(r8), pointer :: mss_bcpho(:,:)
    real(r8), pointer :: mss_bcphi(:,:)
    real(r8), pointer :: mss_bctot(:,:)
    real(r8), pointer :: mss_bc_col(:)
    real(r8), pointer :: mss_bc_top(:)
    real(r8), pointer :: mss_cnc_bcphi(:,:)
    real(r8), pointer :: mss_cnc_bcpho(:,:)
    real(r8), pointer :: mss_ocpho(:,:)
    real(r8), pointer :: mss_ocphi(:,:)
    real(r8), pointer :: mss_octot(:,:)
    real(r8), pointer :: mss_oc_col(:)
    real(r8), pointer :: mss_oc_top(:)
    real(r8), pointer :: mss_cnc_ocphi(:,:)
    real(r8), pointer :: mss_cnc_ocpho(:,:)
    real(r8), pointer :: mss_dst1(:,:)
    real(r8), pointer :: mss_dst2(:,:)
    real(r8), pointer :: mss_dst3(:,:)
    real(r8), pointer :: mss_dst4(:,:)
    real(r8), pointer :: mss_dsttot(:,:)
    real(r8), pointer :: mss_dst_col(:)
    real(r8), pointer :: mss_dst_top(:)
    real(r8), pointer :: mss_cnc_dst1(:,:)
    real(r8), pointer :: mss_cnc_dst2(:,:)
    real(r8), pointer :: mss_cnc_dst3(:,:)
    real(r8), pointer :: mss_cnc_dst4(:,:)
    logical , pointer :: do_capsnow(:)
    integer :: g,l,c,j,fc
    real(r8) :: vol_liq(lbc:ubc,1:nlevgrnd)
    real(r8) :: icefrac(lbc:ubc,1:nlevgrnd)
    real(r8) :: dwat(lbc:ubc,1:nlevgrnd)
    real(r8) :: hk(lbc:ubc,1:nlevgrnd)
    real(r8) :: dhkdw(lbc:ubc,1:nlevgrnd)
    real(r8) :: psi,vwc,fsattmp
    real(r8) :: snowmass
    real(r8) :: snowcap_scl_fct
    real(r8) :: fracl
    forc_rain => clm_a2l%forc_rain
    forc_snow => clm_a2l%forc_snow
    ityplun => clm3%g%l%itype
    cgridcell => clm3%g%l%c%gridcell
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    snl => clm3%g%l%c%cps%snl
    t_grnd => clm3%g%l%c%ces%t_grnd
    h2ocan => clm3%g%l%c%cws%pws_a%h2ocan
    h2osno => clm3%g%l%c%cws%h2osno
    wf => clm3%g%l%c%cps%wf
    snowice => clm3%g%l%c%cws%snowice
    snowliq => clm3%g%l%c%cws%snowliq
    zwt => clm3%g%l%c%cws%zwt
    fcov => clm3%g%l%c%cws%fcov
    fsat => clm3%g%l%c%cws%fsat
    wa => clm3%g%l%c%cws%wa
    qcharge => clm3%g%l%c%cws%qcharge
    watsat => clm3%g%l%c%cps%watsat
    sucsat => clm3%g%l%c%cps%sucsat
    bsw => clm3%g%l%c%cps%bsw
    z => clm3%g%l%c%cps%z
    dz => clm3%g%l%c%cps%dz
    zi => clm3%g%l%c%cps%zi
    t_soisno => clm3%g%l%c%ces%t_soisno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
    t_soi_10cm => clm3%g%l%c%ces%t_soi_10cm
    h2osoi_liqice_10cm => clm3%g%l%c%cws%h2osoi_liqice_10cm
    qflx_evap_tot => clm3%g%l%c%cwf%pwf_a%qflx_evap_tot
    qflx_drain => clm3%g%l%c%cwf%qflx_drain
    qflx_surf => clm3%g%l%c%cwf%qflx_surf
    qflx_infl => clm3%g%l%c%cwf%qflx_infl
    qflx_qrgwl => clm3%g%l%c%cwf%qflx_qrgwl
    endwb => clm3%g%l%c%cwbal%endwb
    begwb => clm3%g%l%c%cwbal%begwb
    bsw2 => clm3%g%l%c%cps%bsw2
    psisat => clm3%g%l%c%cps%psisat
    vwcsat => clm3%g%l%c%cps%vwcsat
    soilpsi => clm3%g%l%c%cps%soilpsi
    smp_l => clm3%g%l%c%cws%smp_l
    hk_l => clm3%g%l%c%cws%hk_l
    qflx_rsub_sat => clm3%g%l%c%cwf%qflx_rsub_sat
    qflx_runoff => clm3%g%l%c%cwf%qflx_runoff
    qflx_runoff_u => clm3%g%l%c%cwf%qflx_runoff_u
    qflx_runoff_r => clm3%g%l%c%cwf%qflx_runoff_r
    t_grnd_u => clm3%g%l%c%ces%t_grnd_u
    t_grnd_r => clm3%g%l%c%ces%t_grnd_r
    snot_top => clm3%g%l%c%cps%snot_top
    dTdz_top => clm3%g%l%c%cps%dTdz_top
    snw_rds => clm3%g%l%c%cps%snw_rds
    snw_rds_top => clm3%g%l%c%cps%snw_rds_top
    sno_liq_top => clm3%g%l%c%cps%sno_liq_top
    frac_sno => clm3%g%l%c%cps%frac_sno
    h2osno_top => clm3%g%l%c%cps%h2osno_top
    mss_bcpho => clm3%g%l%c%cps%mss_bcpho
    mss_bcphi => clm3%g%l%c%cps%mss_bcphi
    mss_bctot => clm3%g%l%c%cps%mss_bctot
    mss_bc_col => clm3%g%l%c%cps%mss_bc_col
    mss_bc_top => clm3%g%l%c%cps%mss_bc_top
    mss_cnc_bcphi => clm3%g%l%c%cps%mss_cnc_bcphi
    mss_cnc_bcpho => clm3%g%l%c%cps%mss_cnc_bcpho
    mss_ocpho => clm3%g%l%c%cps%mss_ocpho
    mss_ocphi => clm3%g%l%c%cps%mss_ocphi
    mss_octot => clm3%g%l%c%cps%mss_octot
    mss_oc_col => clm3%g%l%c%cps%mss_oc_col
    mss_oc_top => clm3%g%l%c%cps%mss_oc_top
    mss_cnc_ocphi => clm3%g%l%c%cps%mss_cnc_ocphi
    mss_cnc_ocpho => clm3%g%l%c%cps%mss_cnc_ocpho
    mss_dst1 => clm3%g%l%c%cps%mss_dst1
    mss_dst2 => clm3%g%l%c%cps%mss_dst2
    mss_dst3 => clm3%g%l%c%cps%mss_dst3
    mss_dst4 => clm3%g%l%c%cps%mss_dst4
    mss_dsttot => clm3%g%l%c%cps%mss_dsttot
    mss_dst_col => clm3%g%l%c%cps%mss_dst_col
    mss_dst_top => clm3%g%l%c%cps%mss_dst_top
    mss_cnc_dst1 => clm3%g%l%c%cps%mss_cnc_dst1
    mss_cnc_dst2 => clm3%g%l%c%cps%mss_cnc_dst2
    mss_cnc_dst3 => clm3%g%l%c%cps%mss_cnc_dst3
    mss_cnc_dst4 => clm3%g%l%c%cps%mss_cnc_dst4
    qflx_snwcp_ice => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_ice
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    call BuildSnowFilter(lbc, ubc, num_nolakec, filter_nolakec, &
         num_snowc, filter_snowc, num_nosnowc, filter_nosnowc)
    call SnowWater(lbc, ubc, num_snowc, filter_snowc, num_nosnowc, filter_nosnowc)
    call SurfaceRunoff(lbc, ubc, lbp, ubp, num_hydrologyc, filter_hydrologyc, &
                       num_urbanc, filter_urbanc, &
                       vol_liq, icefrac )
    call Infiltration(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                      num_urbanc, filter_urbanc)
    call SoilWater(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                   num_urbanc, filter_urbanc, &
                   vol_liq, dwat, hk, dhkdw)
    call Drainage(lbc, ubc, num_hydrologyc, filter_hydrologyc, &
                  num_urbanc, filter_urbanc, &
                  vol_liq, hk, icefrac)
    if (.not. is_perpetual) then
       call SnowCompaction(lbc, ubc, num_snowc, filter_snowc)
       call CombineSnowLayers(lbc, ubc, num_snowc, filter_snowc)
       call DivideSnowLayers(lbc, ubc, num_snowc, filter_snowc)
    else
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          h2osno(c) = 0._r8
       end do
       do j = -nlevsno+1,0
          do fc = 1, num_snowc
             c = filter_snowc(fc)
             if (j >= snl(c)+1) then
                h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             end if
          end do
       end do
    end if
    do j = -nlevsno+1,0
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j <= snl(c) .and. snl(c) > -nlevsno) then
             h2osoi_ice(c,j) = 0._r8
             h2osoi_liq(c,j) = 0._r8
             t_soisno(c,j) = 0._r8
             dz(c,j) = 0._r8
             z(c,j) = 0._r8
             zi(c,j-1) = 0._r8
          end if
       end do
    end do
    call BuildSnowFilter(lbc, ubc, num_nolakec, filter_nolakec, &
         num_snowc, filter_snowc, num_nosnowc, filter_nosnowc)
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       snowice(c) = 0._r8
       snowliq(c) = 0._r8
    end do
    do fc = 1, num_nosnowc
       c = filter_nosnowc(fc)
       snowice(c) = spval
       snowliq(c) = spval
    end do
    do j = -nlevsno+1, 0
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             snowice(c) = snowice(c) + h2osoi_ice(c,j)
             snowliq(c) = snowliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do
    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       if (ityplun(l) /= isturb) then
          t_soi_10cm(c) = 0._r8
          h2osoi_liqice_10cm(c) = 0._r8
       end if
    end do
    do j = 1, nlevsoi
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          l = clandunit(c)
          if (ityplun(l) /= isturb) then
            if (zi(c,j) <= 0.1_r8) then
              fracl = 1._r8
              t_soi_10cm(c) = t_soi_10cm(c) + t_soisno(c,j)*dz(c,j)*fracl
              h2osoi_liqice_10cm(c) = h2osoi_liqice_10cm(c) + (h2osoi_liq(c,j)+h2osoi_ice(c,j))* &
                                       fracl
            else
              if (zi(c,j) > 0.1_r8 .and. zi(c,j-1) .lt. 0.1_r8) then
                 fracl = (0.1_r8 - zi(c,j-1))/dz(c,j)
                 t_soi_10cm(c) = t_soi_10cm(c) + t_soisno(c,j)*dz(c,j)*fracl
                 h2osoi_liqice_10cm(c) = h2osoi_liqice_10cm(c) + (h2osoi_liq(c,j)+h2osoi_ice(c,j))* &
                                          fracl
              end if
            end if
          end if
       end do
    end do
    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       t_grnd(c) = t_soisno(c,snl(c)+1)
       if (ityplun(l) /= isturb) then
          t_soi_10cm(c) = t_soi_10cm(c)/0.1_r8
       end if
       if (ityplun(l)==isturb) then
         t_grnd_u(c) = t_soisno(c,snl(c)+1)
       end if
       if (ityplun(l)==istsoil) then
         t_grnd_r(c) = t_soisno(c,snl(c)+1)
       end if
       if (ctype(c) == icol_roof .or. ctype(c) == icol_sunwall &
          .or. ctype(c) == icol_shadewall .or. ctype(c) == icol_road_imperv) then
         endwb(c) = h2ocan(c) + h2osno(c)
       else
         endwb(c) = h2ocan(c) + h2osno(c) + wa(c)
       end if
    end do
    do j = 1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          endwb(c) = endwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
          h2osoi_vol(c,j) = h2osoi_liq(c,j)/(dz(c,j)*denh2o) + h2osoi_ice(c,j)/(dz(c,j)*denice)
       end do
    end do
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       g = cgridcell(c)
       if (ityplun(l)==istwet .or. ityplun(l)==istice) then
          qflx_drain(c) = 0._r8
          qflx_surf(c) = 0._r8
          qflx_infl(c) = 0._r8
          qflx_qrgwl(c) = forc_rain(g) + forc_snow(g) - qflx_evap_tot(c) - qflx_snwcp_ice(c) - &
                          (endwb(c)-begwb(c))/dtime
          fcov(c) = spval
          fsat(c) = spval
          qcharge(c) = spval
          qflx_rsub_sat(c) = spval
       else if (ityplun(l) == isturb .and. ctype(c) /= icol_road_perv) then
          fcov(c) = spval
          fsat(c) = spval
          qcharge(c) = spval
          qflx_rsub_sat(c) = spval
       end if
       qflx_runoff(c) = qflx_drain(c) + qflx_surf(c) + qflx_qrgwl(c)
       if (ityplun(l)==isturb) then
         qflx_runoff_u(c) = qflx_drain(c) + qflx_surf(c)
       else if (ityplun(l)==istsoil) then
         qflx_runoff_r(c) = qflx_drain(c) + qflx_surf(c) + qflx_qrgwl(c)
       end if
    end do
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       mss_bc_col(c) = 0._r8
       mss_oc_col(c) = 0._r8
       mss_dst_col(c) = 0._r8
       do j = -nlevsno+1, 0
          snowmass = h2osoi_ice(c,j)+h2osoi_liq(c,j)
          if (j == snl(c)+1) then
             if (do_capsnow(c)) then
                snowcap_scl_fct = snowmass / (snowmass+(qflx_snwcp_ice(c)*dtime))
                mss_bcpho(c,j) = mss_bcpho(c,j)*snowcap_scl_fct
                mss_bcphi(c,j) = mss_bcphi(c,j)*snowcap_scl_fct
                mss_ocpho(c,j) = mss_ocpho(c,j)*snowcap_scl_fct
                mss_ocphi(c,j) = mss_ocphi(c,j)*snowcap_scl_fct
                mss_dst1(c,j) = mss_dst1(c,j)*snowcap_scl_fct
                mss_dst2(c,j) = mss_dst2(c,j)*snowcap_scl_fct
                mss_dst3(c,j) = mss_dst3(c,j)*snowcap_scl_fct
                mss_dst4(c,j) = mss_dst4(c,j)*snowcap_scl_fct
             endif
          endif
          if (j >= snl(c)+1) then
             mss_bctot(c,j) = mss_bcpho(c,j) + mss_bcphi(c,j)
             mss_bc_col(c) = mss_bc_col(c) + mss_bctot(c,j)
             mss_cnc_bcphi(c,j) = mss_bcphi(c,j) / snowmass
             mss_cnc_bcpho(c,j) = mss_bcpho(c,j) / snowmass
             mss_octot(c,j) = mss_ocpho(c,j) + mss_ocphi(c,j)
             mss_oc_col(c) = mss_oc_col(c) + mss_octot(c,j)
             mss_cnc_ocphi(c,j) = mss_ocphi(c,j) / snowmass
             mss_cnc_ocpho(c,j) = mss_ocpho(c,j) / snowmass
             mss_dsttot(c,j) = mss_dst1(c,j) + mss_dst2(c,j) + mss_dst3(c,j) + mss_dst4(c,j)
             mss_dst_col(c) = mss_dst_col(c) + mss_dsttot(c,j)
             mss_cnc_dst1(c,j) = mss_dst1(c,j) / snowmass
             mss_cnc_dst2(c,j) = mss_dst2(c,j) / snowmass
             mss_cnc_dst3(c,j) = mss_dst3(c,j) / snowmass
             mss_cnc_dst4(c,j) = mss_dst4(c,j) / snowmass
          else
             snw_rds(c,j) = 0._r8
             mss_bcpho(c,j) = 0._r8
             mss_bcphi(c,j) = 0._r8
             mss_bctot(c,j) = 0._r8
             mss_cnc_bcphi(c,j) = 0._r8
             mss_cnc_bcpho(c,j) = 0._r8
             mss_ocpho(c,j) = 0._r8
             mss_ocphi(c,j) = 0._r8
             mss_octot(c,j) = 0._r8
             mss_cnc_ocphi(c,j) = 0._r8
             mss_cnc_ocpho(c,j) = 0._r8
             mss_dst1(c,j) = 0._r8
             mss_dst2(c,j) = 0._r8
             mss_dst3(c,j) = 0._r8
             mss_dst4(c,j) = 0._r8
             mss_dsttot(c,j) = 0._r8
             mss_cnc_dst1(c,j) = 0._r8
             mss_cnc_dst2(c,j) = 0._r8
             mss_cnc_dst3(c,j) = 0._r8
             mss_cnc_dst4(c,j) = 0._r8
          endif
       enddo
       h2osno_top(c) = h2osoi_ice(c,snl(c)+1) + h2osoi_liq(c,snl(c)+1)
       mss_bc_top(c) = mss_bctot(c,snl(c)+1)
       mss_oc_top(c) = mss_octot(c,snl(c)+1)
       mss_dst_top(c) = mss_dsttot(c,snl(c)+1)
    enddo
    do fc = 1, num_nosnowc
       c = filter_nosnowc(fc)
       h2osno_top(c) = 0._r8
       snw_rds(c,:) = 0._r8
       mss_bc_top(c) = 0._r8
       mss_bc_col(c) = 0._r8
       mss_bcpho(c,:) = 0._r8
       mss_bcphi(c,:) = 0._r8
       mss_bctot(c,:) = 0._r8
       mss_cnc_bcphi(c,:) = 0._r8
       mss_cnc_bcpho(c,:) = 0._r8
       mss_oc_top(c) = 0._r8
       mss_oc_col(c) = 0._r8
       mss_ocpho(c,:) = 0._r8
       mss_ocphi(c,:) = 0._r8
       mss_octot(c,:) = 0._r8
       mss_cnc_ocphi(c,:) = 0._r8
       mss_cnc_ocpho(c,:) = 0._r8
       mss_dst_top(c) = 0._r8
       mss_dst_col(c) = 0._r8
       mss_dst1(c,:) = 0._r8
       mss_dst2(c,:) = 0._r8
       mss_dst3(c,:) = 0._r8
       mss_dst4(c,:) = 0._r8
       mss_dsttot(c,:) = 0._r8
       mss_cnc_dst1(c,:) = 0._r8
       mss_cnc_dst2(c,:) = 0._r8
       mss_cnc_dst3(c,:) = 0._r8
       mss_cnc_dst4(c,:) = 0._r8
       snot_top(c) = spval
       dTdz_top(c) = spval
       snw_rds_top(c) = spval
       sno_liq_top(c) = spval
    enddo
  end subroutine Hydrology2
end module Hydrology2Mod
module DriverInitMod
  implicit none
  save
  public :: DriverInit
contains
  subroutine DriverInit(lbc, ubc, lbp, ubp, &
             num_nolakec, filter_nolakec, num_lakec, filter_lakec)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varpar , only : nlevsno
    use subgridAveMod, only : p2c
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_lakec
    integer, intent(in) :: filter_lakec(ubc-lbc+1)
    real(r8), pointer :: pwtgcell(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: h2osno(:)
    integer , pointer :: frac_veg_nosno_alb(:)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    logical , pointer :: do_capsnow(:)
    real(r8), pointer :: h2osno_old(:)
    real(r8), pointer :: frac_iceold(:,:)
    integer :: c, p, f, j, fc
    snl => clm3%g%l%c%cps%snl
    h2osno => clm3%g%l%c%cws%h2osno
    h2osno_old => clm3%g%l%c%cws%h2osno_old
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    frac_iceold => clm3%g%l%c%cps%frac_iceold
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    frac_veg_nosno_alb => clm3%g%l%c%p%pps%frac_veg_nosno_alb
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    pwtgcell => clm3%g%l%c%p%wtgcell
    do c = lbc, ubc
      h2osno_old(c) = h2osno(c)
      if (h2osno(c) > 1000._r8) then
         do_capsnow(c) = .true.
      else
         do_capsnow(c) = .false.
      end if
    end do
    do p = lbp,ubp
       if (pwtgcell(p)>0._r8) then
          frac_veg_nosno(p) = frac_veg_nosno_alb(p)
       else
          frac_veg_nosno(p) = 0._r8
       end if
    end do
    do j = -nlevsno+1,0
      do f = 1, num_nolakec
         c = filter_nolakec(f)
         if (j >= snl(c) + 1) then
            frac_iceold(c,j) = h2osoi_ice(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))
         end if
      end do
    end do
  end subroutine DriverInit
end module DriverInitMod
module CanopyFluxesMod
  use module_cam_support, only: endrun
   implicit none
   save
   public :: CanopyFluxes
   private :: Stomata
contains
  subroutine CanopyFluxes(lbg, ubg, lbc, ubc, lbp, ubp, &
                          num_nolakep, filter_nolakep)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varpar , only : nlevgrnd, nlevsno
    use clm_varcon , only : sb, cpair, hvap, vkc, grav, denice, &
                                    denh2o, tfrz, csoilc, tlsai_crit, alpha_aero
    use QSatMod , only : QSat
    use FrictionVelocityMod, only : FrictionVelocity, MoninObukIni
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbg, ubg
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
   integer , pointer :: frac_veg_nosno(:)
   integer , pointer :: ivt(:)
   integer , pointer :: pcolumn(:)
   integer , pointer :: plandunit(:)
   integer , pointer :: pgridcell(:)
   real(r8), pointer :: forc_th(:)
   real(r8), pointer :: t_grnd(:)
   real(r8), pointer :: thm(:)
   real(r8), pointer :: qg(:)
   real(r8), pointer :: thv(:)
   real(r8), pointer :: z0mv(:)
   real(r8), pointer :: z0hv(:)
   real(r8), pointer :: z0qv(:)
   real(r8), pointer :: z0mg(:)
   real(r8), pointer :: dqgdT(:)
   real(r8), pointer :: htvp(:)
   real(r8), pointer :: emv(:)
   real(r8), pointer :: emg(:)
   real(r8), pointer :: forc_pbot(:)
   real(r8), pointer :: forc_pco2(:)
   real(r8), pointer :: forc_po2(:)
   real(r8), pointer :: forc_q(:)
   real(r8), pointer :: forc_u(:)
   real(r8), pointer :: forc_v(:)
   real(r8), pointer :: forc_hgt_u_pft(:)
   real(r8), pointer :: forc_rho(:)
   real(r8), pointer :: forc_lwrad(:)
   real(r8), pointer :: displa(:)
   real(r8), pointer :: elai(:)
   real(r8), pointer :: esai(:)
   real(r8), pointer :: fdry(:)
   real(r8), pointer :: fwet(:)
   real(r8), pointer :: laisun(:)
   real(r8), pointer :: laisha(:)
   real(r8), pointer :: sabv(:)
   real(r8), pointer :: watsat(:,:)
   real(r8), pointer :: watdry(:,:)
   real(r8), pointer :: watopt(:,:)
   real(r8), pointer :: h2osoi_ice(:,:)
   real(r8), pointer :: h2osoi_liq(:,:)
   real(r8), pointer :: dz(:,:)
   real(r8), pointer :: t_soisno(:,:)
   real(r8), pointer :: sucsat(:,:)
   real(r8), pointer :: bsw(:,:)
   real(r8), pointer :: rootfr(:,:)
   real(r8), pointer :: dleaf(:)
   real(r8), pointer :: smpso(:)
   real(r8), pointer :: smpsc(:)
   real(r8), pointer :: frac_sno(:)
   real(r8), pointer :: htop(:)
   real(r8), pointer :: snowdp(:)
   real(r8), pointer :: soilbeta(:)
   real(r8), pointer :: lat(:)
   real(r8), pointer :: decl(:)
   real(r8), pointer :: max_dayl(:)
   real(r8), pointer :: cgrnds(:)
   real(r8), pointer :: cgrndl(:)
   real(r8), pointer :: t_veg(:)
   real(r8), pointer :: t_ref2m(:)
   real(r8), pointer :: q_ref2m(:)
   real(r8), pointer :: t_ref2m_r(:)
   real(r8), pointer :: rh_ref2m(:)
   real(r8), pointer :: rh_ref2m_r(:)
   real(r8), pointer :: h2ocan(:)
   real(r8), pointer :: cisun(:)
   real(r8), pointer :: cisha(:)
   real(r8), pointer :: rb1(:)
   real(r8), pointer :: cgrnd(:)
   real(r8), pointer :: dlrad(:)
   real(r8), pointer :: ulrad(:)
   real(r8), pointer :: ram1(:)
   real(r8), pointer :: btran(:)
   real(r8), pointer :: rssun(:)
   real(r8), pointer :: rssha(:)
   real(r8), pointer :: psnsun(:)
   real(r8), pointer :: psnsha(:)
   real(r8), pointer :: qflx_tran_veg(:)
   real(r8), pointer :: dt_veg(:)
   real(r8), pointer :: qflx_evap_veg(:)
   real(r8), pointer :: eflx_sh_veg(:)
   real(r8), pointer :: taux(:)
   real(r8), pointer :: tauy(:)
   real(r8), pointer :: eflx_sh_grnd(:)
   real(r8), pointer :: qflx_evap_soi(:)
   real(r8), pointer :: fpsn(:)
   real(r8), pointer :: rootr(:,:)
   real(r8), pointer :: rresis(:,:)
   real(r8), parameter :: btran0 = 0.0_r8
   real(r8), parameter :: zii = 1000.0_r8
   real(r8), parameter :: beta = 1.0_r8
   real(r8), parameter :: delmax = 1.0_r8
   real(r8), parameter :: dlemin = 0.1_r8
   real(r8), parameter :: dtmin = 0.01_r8
   integer , parameter :: itmax = 40
   integer , parameter :: itmin = 2
   real(r8), parameter :: lai_dl = 0.5_r8
   real(r8), parameter :: z_dl = 0.05_r8
   real(r8), parameter :: ria = 0.5_r8
   real(r8) :: zldis(lbp:ubp)
   real(r8) :: zeta
   real(r8) :: wc
   real(r8) :: dth(lbp:ubp)
   real(r8) :: dthv(lbp:ubp)
   real(r8) :: dqh(lbp:ubp)
   real(r8) :: obu(lbp:ubp)
   real(r8) :: um(lbp:ubp)
   real(r8) :: ur(lbp:ubp)
   real(r8) :: uaf(lbp:ubp)
   real(r8) :: temp1(lbp:ubp)
   real(r8) :: temp12m(lbp:ubp)
   real(r8) :: temp2(lbp:ubp)
   real(r8) :: temp22m(lbp:ubp)
   real(r8) :: ustar(lbp:ubp)
   real(r8) :: tstar
   real(r8) :: qstar
   real(r8) :: thvstar
   real(r8) :: taf(lbp:ubp)
   real(r8) :: qaf(lbp:ubp)
   real(r8) :: rpp
   real(r8) :: rppdry
   real(r8) :: cf
   real(r8) :: rb(lbp:ubp)
   real(r8) :: rah(lbp:ubp,2)
   real(r8) :: raw(lbp:ubp,2)
   real(r8) :: wta
   real(r8) :: wtg(lbp:ubp)
   real(r8) :: wtl
   real(r8) :: wta0(lbp:ubp)
   real(r8) :: wtl0(lbp:ubp)
   real(r8) :: wtg0
   real(r8) :: wtal(lbp:ubp)
   real(r8) :: wtga
   real(r8) :: wtaq
   real(r8) :: wtlq
   real(r8) :: wtgq(lbp:ubp)
   real(r8) :: wtaq0(lbp:ubp)
   real(r8) :: wtlq0(lbp:ubp)
   real(r8) :: wtgq0
   real(r8) :: wtalq(lbp:ubp)
   real(r8) :: wtgaq
   real(r8) :: el(lbp:ubp)
   real(r8) :: deldT
   real(r8) :: qsatl(lbp:ubp)
   real(r8) :: qsatldT(lbp:ubp)
   real(r8) :: e_ref2m
   real(r8) :: de2mdT
   real(r8) :: qsat_ref2m
   real(r8) :: dqsat2mdT
   real(r8) :: air(lbp:ubp),bir(lbp:ubp),cir(lbp:ubp)
   real(r8) :: dc1,dc2
   real(r8) :: delt
   real(r8) :: delq(lbp:ubp)
   real(r8) :: del(lbp:ubp)
   real(r8) :: del2(lbp:ubp)
   real(r8) :: dele(lbp:ubp)
   real(r8) :: dels
   real(r8) :: det(lbp:ubp)
   real(r8) :: efeb(lbp:ubp)
   real(r8) :: efeold
   real(r8) :: efpot
   real(r8) :: efe(lbp:ubp)
   real(r8) :: efsh
   real(r8) :: obuold(lbp:ubp)
   real(r8) :: tlbef(lbp:ubp)
   real(r8) :: ecidif
   real(r8) :: err(lbp:ubp)
   real(r8) :: erre
   real(r8) :: co2(lbp:ubp)
   real(r8) :: o2(lbp:ubp)
   real(r8) :: svpts(lbp:ubp)
   real(r8) :: eah(lbp:ubp)
   real(r8) :: s_node
   real(r8) :: smp_node
   real(r8) :: vol_ice
   real(r8) :: eff_porosity
   real(r8) :: vol_liq
   integer :: itlef
   integer :: nmozsgn(lbp:ubp)
   real(r8) :: w
   real(r8) :: csoilcn
   real(r8) :: fm(lbp:ubp)
   real(r8) :: wtshi
   real(r8) :: wtsqi
   integer :: j
   integer :: p
   integer :: c
   integer :: l
   integer :: g
   integer :: fp
   integer :: fn
   integer :: fnorig
   integer :: fnold
   integer :: f
   integer :: filterp(ubp-lbp+1)
   integer :: fporig(ubp-lbp+1)
   real(r8) :: displa_loc(lbp:ubp)
   real(r8) :: z0mv_loc(lbp:ubp)
   real(r8) :: z0hv_loc(lbp:ubp)
   real(r8) :: z0qv_loc(lbp:ubp)
   logical :: found
   integer :: index
   real(r8) :: egvf
   real(r8) :: lt
   real(r8) :: ri
   real(r8) :: csoilb
   real(r8) :: ricsoilc
   real(r8) :: snowdp_c
   real(r8) :: rdl
   real(r8) :: elai_dl
   real(r8) :: fsno_dl
   real(r8) :: dayl
   real(r8) :: temp
   real(r8) :: dayl_factor(lbp:ubp)
   forc_lwrad => clm_a2l%forc_lwrad
   forc_pco2 => clm_a2l%forc_pco2
   forc_po2 => clm_a2l%forc_po2
   forc_q => clm_a2l%forc_q
   forc_pbot => clm_a2l%forc_pbot
   forc_u => clm_a2l%forc_u
   forc_v => clm_a2l%forc_v
   forc_th => clm_a2l%forc_th
   forc_rho => clm_a2l%forc_rho
   lat => clm3%g%lat
   t_soisno => clm3%g%l%c%ces%t_soisno
   watsat => clm3%g%l%c%cps%watsat
   watdry => clm3%g%l%c%cps%watdry
   watopt => clm3%g%l%c%cps%watopt
   h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
   dz => clm3%g%l%c%cps%dz
   h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
   sucsat => clm3%g%l%c%cps%sucsat
   bsw => clm3%g%l%c%cps%bsw
   emg => clm3%g%l%c%cps%emg
   t_grnd => clm3%g%l%c%ces%t_grnd
   qg => clm3%g%l%c%cws%qg
   thv => clm3%g%l%c%ces%thv
   dqgdT => clm3%g%l%c%cws%dqgdT
   htvp => clm3%g%l%c%cps%htvp
   z0mg => clm3%g%l%c%cps%z0mg
   frac_sno => clm3%g%l%c%cps%frac_sno
   snowdp => clm3%g%l%c%cps%snowdp
   soilbeta => clm3%g%l%c%cws%soilbeta
   decl => clm3%g%l%c%cps%decl
   max_dayl => clm3%g%l%c%cps%max_dayl
   rb1 => clm3%g%l%c%p%pps%rb1
   ivt => clm3%g%l%c%p%itype
   pcolumn => clm3%g%l%c%p%column
   plandunit => clm3%g%l%c%p%landunit
   pgridcell => clm3%g%l%c%p%gridcell
   frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
   btran => clm3%g%l%c%p%pps%btran
   rootfr => clm3%g%l%c%p%pps%rootfr
   rootr => clm3%g%l%c%p%pps%rootr
   rresis => clm3%g%l%c%p%pps%rresis
   emv => clm3%g%l%c%p%pps%emv
   t_veg => clm3%g%l%c%p%pes%t_veg
   displa => clm3%g%l%c%p%pps%displa
   z0mv => clm3%g%l%c%p%pps%z0mv
   z0hv => clm3%g%l%c%p%pps%z0hv
   z0qv => clm3%g%l%c%p%pps%z0qv
   ram1 => clm3%g%l%c%p%pps%ram1
   htop => clm3%g%l%c%p%pps%htop
   rssun => clm3%g%l%c%p%pps%rssun
   rssha => clm3%g%l%c%p%pps%rssha
   cisun => clm3%g%l%c%p%pps%cisun
   cisha => clm3%g%l%c%p%pps%cisha
   psnsun => clm3%g%l%c%p%pcf%psnsun
   psnsha => clm3%g%l%c%p%pcf%psnsha
   elai => clm3%g%l%c%p%pps%elai
   esai => clm3%g%l%c%p%pps%esai
   fdry => clm3%g%l%c%p%pps%fdry
   laisun => clm3%g%l%c%p%pps%laisun
   laisha => clm3%g%l%c%p%pps%laisha
   qflx_tran_veg => clm3%g%l%c%p%pwf%qflx_tran_veg
   fwet => clm3%g%l%c%p%pps%fwet
   h2ocan => clm3%g%l%c%p%pws%h2ocan
   dt_veg => clm3%g%l%c%p%pps%dt_veg
   sabv => clm3%g%l%c%p%pef%sabv
   qflx_evap_veg => clm3%g%l%c%p%pwf%qflx_evap_veg
   eflx_sh_veg => clm3%g%l%c%p%pef%eflx_sh_veg
   taux => clm3%g%l%c%p%pmf%taux
   tauy => clm3%g%l%c%p%pmf%tauy
   eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
   qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
   t_ref2m => clm3%g%l%c%p%pes%t_ref2m
   q_ref2m => clm3%g%l%c%p%pes%q_ref2m
   t_ref2m_r => clm3%g%l%c%p%pes%t_ref2m_r
   rh_ref2m_r => clm3%g%l%c%p%pes%rh_ref2m_r
   rh_ref2m => clm3%g%l%c%p%pes%rh_ref2m
   dlrad => clm3%g%l%c%p%pef%dlrad
   ulrad => clm3%g%l%c%p%pef%ulrad
   cgrnds => clm3%g%l%c%p%pef%cgrnds
   cgrndl => clm3%g%l%c%p%pef%cgrndl
   cgrnd => clm3%g%l%c%p%pef%cgrnd
   fpsn => clm3%g%l%c%p%pcf%fpsn
   forc_hgt_u_pft => clm3%g%l%c%p%pps%forc_hgt_u_pft
   thm => clm3%g%l%c%p%pes%thm
   dleaf => pftcon%dleaf
   smpso => pftcon%smpso
   smpsc => pftcon%smpsc
   fn = 0
   do fp = 1,num_nolakep
      p = filter_nolakep(fp)
      if (frac_veg_nosno(p) /= 0) then
         fn = fn + 1
         filterp(fn) = p
      end if
   end do
   do f = 1, fn
      p = filterp(f)
      del(p) = 0._r8
      efeb(p) = 0._r8
      wtlq0(p) = 0._r8
      wtalq(p) = 0._r8
      wtgq(p) = 0._r8
      wtaq0(p) = 0._r8
      obuold(p) = 0._r8
      btran(p) = btran0
   end do
   do f = 1, fn
      p=filterp(f)
      c=pcolumn(p)
      g=pgridcell(p)
      temp = -(sin(lat(g))*sin(decl(c)))/(cos(lat(g)) * cos(decl(c)))
      temp = min(1._r8,max(-1._r8,temp))
      dayl = 2.0_r8 * 13750.9871_r8 * acos(temp)
      dayl_factor(p)=min(1._r8,max(0.01_r8,(dayl*dayl)/(max_dayl(c)*max_dayl(c))))
   end do
   rb1(lbp:ubp) = 0._r8
   do j = 1,nlevgrnd
!dir$ concurrent
      do f = 1, fn
         p = filterp(f)
         c = pcolumn(p)
         l = plandunit(p)
         vol_ice = min(watsat(c,j), h2osoi_ice(c,j)/(dz(c,j)*denice))
         eff_porosity = watsat(c,j)-vol_ice
         vol_liq = min(eff_porosity, h2osoi_liq(c,j)/(dz(c,j)*denh2o))
         if (vol_liq .le. 0._r8 .or. t_soisno(c,j) .le. tfrz-2._r8) then
            rootr(p,j) = 0._r8
         else
            s_node = max(vol_liq/eff_porosity,0.01_r8)
            smp_node = max(smpsc(ivt(p)), -sucsat(c,j)*s_node**(-bsw(c,j)))
            rresis(p,j) = min( (eff_porosity/watsat(c,j))* &
                          (smp_node - smpsc(ivt(p))) / (smpso(ivt(p)) - smpsc(ivt(p))), 1._r8)
            rootr(p,j) = rootfr(p,j)*rresis(p,j)
            btran(p) = btran(p) + rootr(p,j)
         endif
      end do
   end do
   do j = 1,nlevgrnd
!dir$ concurrent
      do f = 1, fn
         p = filterp(f)
         if (btran(p) .gt. btran0) then
           rootr(p,j) = rootr(p,j)/btran(p)
         else
           rootr(p,j) = 0._r8
         end if
      end do
   end do
   do f = 1, fn
      p = filterp(f)
      c = pcolumn(p)
      lt = min(elai(p)+esai(p), tlsai_crit)
      egvf =(1._r8 - alpha_aero * exp(-lt)) / (1._r8 - alpha_aero * exp(-tlsai_crit))
      displa(p) = egvf * displa(p)
      z0mv(p) = exp(egvf * log(z0mv(p)) + (1._r8 - egvf) * log(z0mg(c)))
      z0hv(p) = z0mv(p)
      z0qv(p) = z0mv(p)
  end do
   found = .false.
!dir$ concurrent
   do f = 1, fn
      p = filterp(f)
      c = pcolumn(p)
      g = pgridcell(p)
      air(p) = emv(p) * (1._r8+(1._r8-emv(p))*(1._r8-emg(c))) * forc_lwrad(g)
      bir(p) = - (2._r8-emv(p)*(1._r8-emg(c))) * emv(p) * sb
      cir(p) = emv(p)*emg(c)*sb
      call QSat (t_veg(p), forc_pbot(g), el(p), deldT, qsatl(p), qsatldT(p))
      co2(p) = forc_pco2(g)
      o2(p) = forc_po2(g)
      nmozsgn(p) = 0
      taf(p) = (t_grnd(c) + thm(p))/2._r8
      qaf(p) = (forc_q(g)+qg(c))/2._r8
      ur(p) = max(1.0_r8,sqrt(forc_u(g)*forc_u(g)+forc_v(g)*forc_v(g)))
      dth(p) = thm(p)-taf(p)
      dqh(p) = forc_q(g)-qaf(p)
      delq(p) = qg(c) - qaf(p)
      dthv(p) = dth(p)*(1._r8+0.61_r8*forc_q(g))+0.61_r8*forc_th(g)*dqh(p)
      zldis(p) = forc_hgt_u_pft(p) - displa(p)
      if (zldis(p) < 0._r8) then
         found = .true.
         index = p
      end if
   end do
   if (found) then
      write(6,*)'Error: Forcing height is below canopy height for pft index ',index
      call endrun()
   end if
!dir$ concurrent
   do f = 1, fn
      p = filterp(f)
      c = pcolumn(p)
      call MoninObukIni(ur(p), thv(c), dthv(p), zldis(p), z0mv(p), um(p), obu(p))
   end do
   itlef = 0
   fnorig = fn
   fporig(1:fn) = filterp(1:fn)
!dir$ concurrent
   do f = 1, fn
      p = filterp(f)
      displa_loc(p) = displa(p)
      z0mv_loc(p) = z0mv(p)
      z0hv_loc(p) = z0hv(p)
      z0qv_loc(p) = z0qv(p)
   end do
   ITERATION : do while (itlef <= itmax .and. fn > 0)
      call FrictionVelocity (lbp, ubp, fn, filterp, &
                             displa_loc, z0mv_loc, z0hv_loc, z0qv_loc, &
                             obu, itlef+1, ur, um, ustar, &
                             temp1, temp2, temp12m, temp22m, fm)
!dir$ concurrent
      do f = 1, fn
         p = filterp(f)
         c = pcolumn(p)
         g = pgridcell(p)
         tlbef(p) = t_veg(p)
         del2(p) = del(p)
         ram1(p) = 1._r8/(ustar(p)*ustar(p)/um(p))
         rah(p,1) = 1._r8/(temp1(p)*ustar(p))
         raw(p,1) = 1._r8/(temp2(p)*ustar(p))
         uaf(p) = um(p)*sqrt( 1._r8/(ram1(p)*um(p)) )
         cf = 0.01_r8/(sqrt(uaf(p))*sqrt(dleaf(ivt(p))))
         rb(p) = 1._r8/(cf*uaf(p))
         rb1(p) = rb(p)
         w = exp(-(elai(p)+esai(p)))
         csoilb = (vkc/(0.13_r8*(z0mg(c)*uaf(p)/1.5e-5_r8)**0.45_r8))
         ri = ( grav*htop(p) * (taf(p) - t_grnd(c)) ) / (taf(p) * uaf(p) **2.00_r8)
         if ( (taf(p) - t_grnd(c) ) > 0._r8) then
               ricsoilc = csoilc / (1.00_r8 + ria*min( ri, 10.0_r8) )
               csoilcn = csoilb*w + ricsoilc*(1._r8-w)
         else
              csoilcn = csoilb*w + csoilc*(1._r8-w)
         end if
         rah(p,2) = 1._r8/(csoilcn*uaf(p))
         raw(p,2) = rah(p,2)
         svpts(p) = el(p)
         eah(p) = forc_pbot(g) * qaf(p) / 0.622_r8
      end do
      call Stomata (fn, filterp, lbp, ubp, svpts, eah, o2, co2, rb, dayl_factor, phase='sun')
      call Stomata (fn, filterp, lbp, ubp, svpts, eah, o2, co2, rb, dayl_factor, phase='sha')
!dir$ concurrent
      do f = 1, fn
         p = filterp(f)
         c = pcolumn(p)
         g = pgridcell(p)
         wta = 1._r8/rah(p,1)
         wtl = (elai(p)+esai(p))/rb(p)
         wtg(p) = 1._r8/rah(p,2)
         wtshi = 1._r8/(wta+wtl+wtg(p))
         wtl0(p) = wtl*wtshi
         wtg0 = wtg(p)*wtshi
         wta0(p) = wta*wtshi
         wtga = wta0(p)+wtg0
         wtal(p) = wta0(p)+wtl0(p)
         if (fdry(p) .gt. 0._r8) then
            rppdry = fdry(p)*rb(p)*(laisun(p)/(rb(p)+rssun(p)) + &
                                     laisha(p)/(rb(p)+rssha(p)))/elai(p)
         else
            rppdry = 0._r8
         end if
         efpot = forc_rho(g)*wtl*(qsatl(p)-qaf(p))
         if (efpot > 0._r8) then
            if (btran(p) > btran0) then
               qflx_tran_veg(p) = efpot*rppdry
               rpp = rppdry + fwet(p)
            else
               rpp = fwet(p)
               qflx_tran_veg(p) = 0._r8
            end if
            rpp = min(rpp, (qflx_tran_veg(p)+h2ocan(p)/dtime)/efpot)
         else
            rpp = 1._r8
            qflx_tran_veg(p) = 0._r8
         end if
         wtaq = frac_veg_nosno(p)/raw(p,1)
         wtlq = frac_veg_nosno(p)*(elai(p)+esai(p))/rb(p) * rpp
         snowdp_c = z_dl
         fsno_dl = snowdp(c)/snowdp_c
         elai_dl = lai_dl*(1._r8 - min(fsno_dl,1._r8))
         rdl = ( 1._r8 - exp(-elai_dl) ) / ( 0.004_r8*uaf(p))
         if (delq(p) .lt. 0._r8) then
            wtgq(p) = frac_veg_nosno(p)/(raw(p,2)+rdl)
         else
            wtgq(p) = soilbeta(c)*frac_veg_nosno(p)/(raw(p,2)+rdl)
         end if
         wtsqi = 1._r8/(wtaq+wtlq+wtgq(p))
         wtgq0 = wtgq(p)*wtsqi
         wtlq0(p) = wtlq*wtsqi
         wtaq0(p) = wtaq*wtsqi
         wtgaq = wtaq0(p)+wtgq0
         wtalq(p) = wtaq0(p)+wtlq0(p)
         dc1 = forc_rho(g)*cpair*wtl
         dc2 = hvap*forc_rho(g)*wtlq
         efsh = dc1*(wtga*t_veg(p)-wtg0*t_grnd(c)-wta0(p)*thm(p))
         efe(p) = dc2*(wtgaq*qsatl(p)-wtgq0*qg(c)-wtaq0(p)*forc_q(g))
         erre = 0._r8
         if (efe(p)*efeb(p) < 0._r8) then
            efeold = efe(p)
            efe(p) = 0.1_r8*efeold
            erre = efe(p) - efeold
         end if
         dt_veg(p) = (sabv(p) + air(p) + bir(p)*t_veg(p)**4 + &
              cir(p)*t_grnd(c)**4 - efsh - efe(p)) / &
              (- 4._r8*bir(p)*t_veg(p)**3 +dc1*wtga +dc2*wtgaq*qsatldT(p))
         t_veg(p) = tlbef(p) + dt_veg(p)
         dels = dt_veg(p)
         del(p) = abs(dels)
         err(p) = 0._r8
         if (del(p) > delmax) then
            dt_veg(p) = delmax*dels/del(p)
            t_veg(p) = tlbef(p) + dt_veg(p)
            err(p) = sabv(p) + air(p) + bir(p)*tlbef(p)**3*(tlbef(p) + &
                 4._r8*dt_veg(p)) + cir(p)*t_grnd(c)**4 - &
                 (efsh + dc1*wtga*dt_veg(p)) - (efe(p) + &
                 dc2*wtgaq*qsatldT(p)*dt_veg(p))
         end if
         efpot = forc_rho(g)*wtl*(wtgaq*(qsatl(p)+qsatldT(p)*dt_veg(p)) &
            -wtgq0*qg(c)-wtaq0(p)*forc_q(g))
         qflx_evap_veg(p) = rpp*efpot
         ecidif = 0._r8
         if (efpot > 0._r8 .and. btran(p) > btran0) then
            qflx_tran_veg(p) = efpot*rppdry
         else
            qflx_tran_veg(p) = 0._r8
         end if
         ecidif = max(0._r8, qflx_evap_veg(p)-qflx_tran_veg(p)-h2ocan(p)/dtime)
         qflx_evap_veg(p) = min(qflx_evap_veg(p),qflx_tran_veg(p)+h2ocan(p)/dtime)
         eflx_sh_veg(p) = efsh + dc1*wtga*dt_veg(p) + err(p) + erre + hvap*ecidif
         call QSat(t_veg(p), forc_pbot(g), el(p), deldT, qsatl(p), qsatldT(p))
         taf(p) = wtg0*t_grnd(c) + wta0(p)*thm(p) + wtl0(p)*t_veg(p)
         qaf(p) = wtlq0(p)*qsatl(p) + wtgq0*qg(c) + forc_q(g)*wtaq0(p)
         dth(p) = thm(p)-taf(p)
         dqh(p) = forc_q(g)-qaf(p)
         delq(p) = wtalq(p)*qg(c)-wtlq0(p)*qsatl(p)-wtaq0(p)*forc_q(g)
         tstar = temp1(p)*dth(p)
         qstar = temp2(p)*dqh(p)
         thvstar = tstar*(1._r8+0.61_r8*forc_q(g)) + 0.61_r8*forc_th(g)*qstar
         zeta = zldis(p)*vkc*grav*thvstar/(ustar(p)**2*thv(c))
         if (zeta >= 0._r8) then
            zeta = min(2._r8,max(zeta,0.01_r8))
            um(p) = max(ur(p),0.1_r8)
         else
            zeta = max(-100._r8,min(zeta,-0.01_r8))
            wc = beta*(-grav*ustar(p)*thvstar*zii/thv(c))**0.333_r8
            um(p) = sqrt(ur(p)*ur(p)+wc*wc)
         end if
         obu(p) = zldis(p)/zeta
         if (obuold(p)*obu(p) < 0._r8) nmozsgn(p) = nmozsgn(p)+1
         if (nmozsgn(p) >= 4) obu(p) = zldis(p)/(-0.01_r8)
         obuold(p) = obu(p)
      end do
      itlef = itlef+1
      if (itlef > itmin) then
!dir$ concurrent
         do f = 1, fn
            p = filterp(f)
            dele(p) = abs(efe(p)-efeb(p))
            efeb(p) = efe(p)
            det(p) = max(del(p),del2(p))
         end do
         fnold = fn
         fn = 0
         do f = 1, fnold
            p = filterp(f)
            if (.not. (det(p) < dtmin .and. dele(p) < dlemin)) then
               fn = fn + 1
               filterp(fn) = p
            end if
         end do
      end if
   end do ITERATION
   fn = fnorig
   filterp(1:fn) = fporig(1:fn)
!dir$ concurrent
   do f = 1, fn
      p = filterp(f)
      c = pcolumn(p)
      g = pgridcell(p)
      err(p) = sabv(p) + air(p) + bir(p)*tlbef(p)**3*(tlbef(p) + 4._r8*dt_veg(p)) &
         + cir(p)*t_grnd(c)**4 - eflx_sh_veg(p) - hvap*qflx_evap_veg(p)
      delt = wtal(p)*t_grnd(c)-wtl0(p)*t_veg(p)-wta0(p)*thm(p)
      taux(p) = -forc_rho(g)*forc_u(g)/ram1(p)
      tauy(p) = -forc_rho(g)*forc_v(g)/ram1(p)
      eflx_sh_grnd(p) = cpair*forc_rho(g)*wtg(p)*delt
      qflx_evap_soi(p) = forc_rho(g)*wtgq(p)*delq(p)
      t_ref2m(p) = thm(p) + temp1(p)*dth(p)*(1._r8/temp12m(p) - 1._r8/temp1(p))
      t_ref2m_r(p) = t_ref2m(p)
      q_ref2m(p) = forc_q(g) + temp2(p)*dqh(p)*(1._r8/temp22m(p) - 1._r8/temp2(p))
      call QSat(t_ref2m(p), forc_pbot(g), e_ref2m, de2mdT, qsat_ref2m, dqsat2mdT)
      rh_ref2m(p) = min(100._r8, q_ref2m(p) / qsat_ref2m * 100._r8)
      rh_ref2m_r(p) = rh_ref2m(p)
      dlrad(p) = (1._r8-emv(p))*emg(c)*forc_lwrad(g) + &
         emv(p)*emg(c)*sb*tlbef(p)**3*(tlbef(p) + 4._r8*dt_veg(p))
      ulrad(p) = ((1._r8-emg(c))*(1._r8-emv(p))*(1._r8-emv(p))*forc_lwrad(g) &
         + emv(p)*(1._r8+(1._r8-emg(c))*(1._r8-emv(p)))*sb*tlbef(p)**3*(tlbef(p) + &
         4._r8*dt_veg(p)) + emg(c)*(1._r8-emv(p))*sb*t_grnd(c)**4)
      cgrnds(p) = cgrnds(p) + cpair*forc_rho(g)*wtg(p)*wtal(p)
      cgrndl(p) = cgrndl(p) + forc_rho(g)*wtgq(p)*wtalq(p)*dqgdT(c)
      cgrnd(p) = cgrnds(p) + cgrndl(p)*htvp(c)
      h2ocan(p) = max(0._r8,h2ocan(p)+(qflx_tran_veg(p)-qflx_evap_veg(p))*dtime)
      fpsn(p) = psnsun(p)*laisun(p) + psnsha(p)*laisha(p)
   end do
   fnold = fn
   fn = 0
   do f = 1, fnold
      p = filterp(f)
      if (abs(err(p)) > 0.1_r8) then
         fn = fn + 1
         filterp(fn) = p
      end if
   end do
   do f = 1, fn
      p = filterp(f)
      write(6,*) 'energy balance in canopy ',p,', err=',err(p)
   end do
   end subroutine CanopyFluxes
   subroutine Stomata (fn, filterp, lbp, ubp, ei, ea, o2, co2, rb, dayl_factor, phase)
     use shr_kind_mod , only : r8 => shr_kind_r8
     use shr_const_mod, only : SHR_CONST_TKFRZ, SHR_CONST_RGAS
     use clmtype
     use pftvarcon , only : nbrdlf_dcd_tmp_shrub
     implicit none
     integer , intent(in) :: fn
     integer , intent(in) :: filterp(fn)
     integer , intent(in) :: lbp, ubp
     real(r8), intent(in) :: ei(lbp:ubp)
     real(r8), intent(in) :: ea(lbp:ubp)
     real(r8), intent(in) :: o2(lbp:ubp)
     real(r8), intent(in) :: co2(lbp:ubp)
     real(r8), intent(inout) :: rb(lbp:ubp)
     real(r8), intent(in) :: dayl_factor(lbp:ubp)
     character(len=*), intent(in) :: phase
     integer , pointer :: pcolumn(:)
     integer , pointer :: pgridcell(:)
     integer , pointer :: ivt(:)
     real(r8), pointer :: qe25(:)
     real(r8), pointer :: vcmx25(:)
     real(r8), pointer :: c3psn(:)
     real(r8), pointer :: mp(:)
     real(r8), pointer :: tgcm(:)
     real(r8), pointer :: forc_pbot(:)
     real(r8), pointer :: tl(:)
     real(r8), pointer :: btran(:)
     real(r8), pointer :: apar(:)
     real(r8), pointer :: leafcn(:)
     real(r8), pointer :: flnr(:)
     real(r8), pointer :: sla(:)
     real(r8), pointer :: fnitr(:)
     real(r8), pointer :: rs(:)
     real(r8), pointer :: psn(:)
     real(r8), pointer :: ci(:)
     real(r8), pointer :: lnc(:)
     real(r8), pointer :: vcmx(:)
     real(r8), parameter :: mpe = 1.e-6_r8
     integer , parameter :: niter = 3
     integer :: f,p,c,g
     integer :: iter
     real(r8) :: ab
     real(r8) :: bc
     real(r8) :: f1
     real(r8) :: f2
     real(r8) :: tc
     real(r8) :: cs
     real(r8) :: kc
     real(r8) :: ko
     real(r8) :: atmp
     real(r8) :: btmp
     real(r8) :: ctmp
     real(r8) :: q
     real(r8) :: r1,r2
     real(r8) :: ppf
     real(r8) :: wc
     real(r8) :: wj
     real(r8) :: we
     real(r8) :: cp
     real(r8) :: awc
     real(r8) :: j
     real(r8) :: cea
     real(r8) :: cf
     real(r8) :: rsmax0
     real(r8) :: kc25
     real(r8) :: akc
     real(r8) :: ko25
     real(r8) :: ako
     real(r8) :: avcmx
     real(r8) :: bp
     real(r8) :: act25
     real(r8) :: act
     real(r8) :: q10act
     real(r8) :: fnr
     f1(ab,bc) = ab**((bc-25._r8)/10._r8)
     f2(ab) = 1._r8 + exp((-2.2e05_r8+710._r8*(ab+SHR_CONST_TKFRZ))/(SHR_CONST_RGAS*0.001_r8*(ab+SHR_CONST_TKFRZ)))
     pcolumn => clm3%g%l%c%p%column
     pgridcell => clm3%g%l%c%p%gridcell
     ivt => clm3%g%l%c%p%itype
     tl => clm3%g%l%c%p%pes%t_veg
     btran => clm3%g%l%c%p%pps%btran
     if (phase == 'sun') then
        apar => clm3%g%l%c%p%pef%parsun
        rs => clm3%g%l%c%p%pps%rssun
        psn => clm3%g%l%c%p%pcf%psnsun
        ci => clm3%g%l%c%p%pps%cisun
        sla => clm3%g%l%c%p%pps%slasun
        lnc => clm3%g%l%c%p%pps%lncsun
        vcmx => clm3%g%l%c%p%pps%vcmxsun
     else if (phase == 'sha') then
        apar => clm3%g%l%c%p%pef%parsha
        rs => clm3%g%l%c%p%pps%rssha
        psn => clm3%g%l%c%p%pcf%psnsha
        ci => clm3%g%l%c%p%pps%cisha
        sla => clm3%g%l%c%p%pps%slasha
        lnc => clm3%g%l%c%p%pps%lncsha
        vcmx => clm3%g%l%c%p%pps%vcmxsha
     end if
     forc_pbot => clm_a2l%forc_pbot
     tgcm => clm3%g%l%c%p%pes%thm
     qe25 => pftcon%qe25
     vcmx25 => pftcon%vcmx25
     c3psn => pftcon%c3psn
     mp => pftcon%mp
     leafcn => pftcon%leafcn
     flnr => pftcon%flnr
     fnitr => pftcon%fnitr
     kc25 = 30._r8
     akc = 2.1_r8
     ko25 = 30000._r8
     ako = 1.2_r8
     avcmx = 2.4_r8
     bp = 2000._r8
     act25 = 3.6_r8
     q10act = 2.4_r8
     fnr = 7.16_r8
     act25 = act25 * 1000.0_r8 / 60.0_r8
!dir$ concurrent
     do f = 1, fn
        p = filterp(f)
        c = pcolumn(p)
        g = pgridcell(p)
        rsmax0 = 2.e4_r8
        cf = forc_pbot(g)/(SHR_CONST_RGAS*0.001_r8*tgcm(p))*1.e06_r8
        if (apar(p) <= 0._r8) then
           rs(p) = min(rsmax0, 1._r8/bp * cf)
           psn(p) = 0._r8
           lnc(p) = 0._r8
           vcmx(p) = 0._r8
        else
           tc = tl(p) - SHR_CONST_TKFRZ
           ppf = 4.6_r8 * apar(p)
           j = ppf * qe25(ivt(p))
           kc = kc25 * f1(akc,tc)
           ko = ko25 * f1(ako,tc)
           awc = kc * (1._r8+o2(p)/ko)
           cp = 0.5_r8*kc/ko*o2(p)*0.21_r8
           lnc(p) = 1._r8 / (sla(p) * leafcn(ivt(p)))
     act = act25 * f1(q10act,tc)
           vcmx(p) = lnc(p) * flnr(ivt(p)) * fnr * act / f2(tc) * btran(p) * dayl_factor(p) * fnitr(ivt(p))
           ci(p) = 0.7_r8*co2(p)*c3psn(ivt(p)) + 0.4_r8*co2(p)*(1._r8-c3psn(ivt(p)))
           rb(p) = rb(p)/cf
           cea = max(0.25_r8*ei(p)*c3psn(ivt(p))+0.40_r8*ei(p)*(1._r8-c3psn(ivt(p))), min(ea(p),ei(p)) )
           do iter = 1, niter
              wj = max(ci(p)-cp,0._r8)*j/(ci(p)+2._r8*cp)*c3psn(ivt(p)) + j*(1._r8-c3psn(ivt(p)))
              wc = max(ci(p)-cp,0._r8)*vcmx(p)/(ci(p)+awc)*c3psn(ivt(p)) + vcmx(p)*(1._r8-c3psn(ivt(p)))
              we = 0.5_r8*vcmx(p)*c3psn(ivt(p)) + 4000._r8*vcmx(p)*ci(p)/forc_pbot(g)*(1._r8-c3psn(ivt(p)))
              psn(p) = min(wj,wc,we)
              cs = max( co2(p)-1.37_r8*rb(p)*forc_pbot(g)*psn(p), mpe )
              atmp = mp(ivt(p))*psn(p)*forc_pbot(g)*cea / (cs*ei(p)) + bp
              btmp = ( mp(ivt(p))*psn(p)*forc_pbot(g)/cs + bp ) * rb(p) - 1._r8
              ctmp = -rb(p)
              if (btmp >= 0._r8) then
                 q = -0.5_r8*( btmp + sqrt(btmp*btmp-4._r8*atmp*ctmp) )
              else
                 q = -0.5_r8*( btmp - sqrt(btmp*btmp-4._r8*atmp*ctmp) )
              end if
              r1 = q/atmp
              r2 = ctmp/q
              rs(p) = max(r1,r2)
              ci(p) = max( cs-psn(p)*forc_pbot(g)*1.65_r8*rs(p), 0._r8 )
           end do
           rs(p) = min(rsmax0, rs(p)*cf)
           rb(p) = rb(p) * cf
        end if
     end do
  end subroutine Stomata
end module CanopyFluxesMod
 subroutine biochem_to_wrf(htmx_buf,croplive_buf,gdd1020_buf,gdd820_buf,gdd020_buf,grainc_buf,grainc_storage_buf &
                ,grainc_xfer_buf,grainn_buf,grainn_storage_buf,grainn_xfer_buf,days_active_buf &
                ,onset_flag_buf,onset_counter_buf,onset_gddflag_buf,onset_fdd_buf,onset_gdd_buf &
                ,onset_swi_buf,offset_flag_buf,offset_counter_buf,offset_fdd_buf,offset_swi_buf &
                ,dayl_buf,annavg_t2m_buf,tempavg_t2m_buf,tempsum_potential_gpp_buf &
                ,annsum_potential_gpp_buf,tempmax_retransn_buf,annmax_retransn_buf &
                ,prev_leafc_to_litter_buf,prev_frootc_to_litter_buf,tempsum_npp_buf &
                ,annsum_npp_buf,leafc_buf,leafc_storage_buf,leafc_xfer_buf,frootc_buf &
                ,frootc_storage_buf,frootc_xfer_buf,livestemc_buf,livestemc_storage_buf &
                ,livestemc_xfer_buf,deadstemc_buf,deadstemc_storage_buf,deadstemc_xfer_buf &
                ,livecrootc_buf,livecrootc_storage_buf,livecrootc_xfer_buf,deadcrootc_buf &
                ,deadcrootc_storage_buf,deadcrootc_xfer_buf,cpool_buf,pft_ctrunc_buf &
                ,leafn_buf,leafn_storage_buf,leafn_xfer_buf,frootn_buf,frootn_storage_buf &
                ,frootn_xfer_buf,livestemn_buf,livestemn_storage_buf,livestemn_xfer_buf &
                ,deadstemn_buf,deadstemn_storage_buf,deadstemn_xfer_buf,livecrootn_buf &
                ,livecrootn_storage_buf,livecrootn_xfer_buf,deadcrootn_buf &
                ,deadcrootn_storage_buf,deadcrootn_xfer_buf,npool_buf,pft_ntrunc_buf &
                ,gresp_storage_buf,gresp_xfer_buf,xsmrpool_buf,annsum_counter_buf &
                ,cannsum_npp_buf,cannavg_t2m_buf,wf_buf,me_buf,mean_fire_prob_buf,cwdc_buf,litr1c_buf &
                ,litr2c_buf,litr3c_buf,soil1c_buf,soil2c_buf,soil3c_buf,soil4c_buf,seedc_buf,col_ctrunc_buf &
                ,prod10c_buf,prod100c_buf,cwdn_buf,litr1n_buf,litr2n_buf,litr3n_buf,soil1n_buf,soil2n_buf &
                ,soil3n_buf,soil4n_buf,seedn_buf,col_ntrunc_buf,prod10n_buf,prod100n_buf,sminn_buf &
               ,totlitc_buf,dwt_seedc_to_leaf_buf,dwt_seedc_to_deadstem_buf,dwt_conv_cflux_buf &
                ,dwt_prod10c_gain_buf,dwt_prod100c_gain_buf,prod100c_loss_buf,dwt_frootc_to_litr1c_buf &
                ,dwt_frootc_to_litr2c_buf,dwt_frootc_to_litr3c_buf,dwt_livecrootc_to_cwdc_buf &
                ,dwt_deadcrootc_to_cwdc_buf,dwt_seedn_to_leaf_buf,dwt_seedn_to_deadstem_buf &
                ,dwt_conv_nflux_buf,dwt_prod10n_gain_buf,dwt_prod100n_gain_buf,prod100n_loss_buf &
                ,dwt_frootn_to_litr1n_buf,dwt_frootn_to_litr2n_buf, dwt_frootn_to_litr3n_buf &
                , dwt_livecrootn_to_cwdn_buf,dwt_deadcrootn_to_cwdn_buf,retransn_buf &
                 )
  end subroutine biochem_to_wrf
subroutine biophy_to_wrf(snl ,snowdp ,dzclm ,zclm ,&
                      ziclm ,h2osno ,h2osoi_liq ,h2osoi_ice ,t_grnd ,&
                      t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col ,&
                      h2osoi_vol ,wtc ,wtp ,numc ,nump ,&
                      htop ,tsai &
                      ,t_ref2m ,znt ,q_ref2m,snw_rds)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use clm_varpar, only : nlevgrnd,numrad,maxpatch,nlevsno,nlevlak
    use clm_varcon, only : denice, denh2o
    use nanMod, only : nan
    use decompMod , only : get_proc_bounds
    use pftvarcon , only : noveg
    implicit none
    integer :: snl(maxpatch)
    integer :: frac_veg_nosno_alb(maxpatch)
    real(r8) :: snowdp(maxpatch)
    real(r8) :: frac_sno(maxpatch)
    real(r8) :: albd(numrad,maxpatch)
    real(r8) :: albi(numrad,maxpatch)
    real(r8) :: albgrd(numrad,maxpatch)
    real(r8) :: albgri(numrad,maxpatch)
    real(r8) :: h2osno(maxpatch)
    real(r8) :: t_grnd(maxpatch)
    real(r8) :: fwet(maxpatch)
    real(r8) :: tlai(maxpatch)
    real(r8) :: tsai(maxpatch)
    real(r8) :: elai(maxpatch)
    real(r8) :: esai(maxpatch)
    real(r8) :: fsun(maxpatch)
    real(r8) :: htop(maxpatch)
    real(r8) :: hbot(maxpatch)
    real(r8) :: fabd(numrad,maxpatch)
    real(r8) :: fabi(numrad,maxpatch)
    real(r8) :: ftdd(numrad,maxpatch)
    real(r8) :: ftid(numrad,maxpatch)
    real(r8) :: ftii(numrad,maxpatch)
    real(r8) :: t_veg(maxpatch)
    real(r8) :: h2ocan(maxpatch)
    real(r8) :: h2ocan_col(maxpatch)
    real(r8) :: wtc(maxpatch)
    real(r8) :: wtp(maxpatch)
  real(r8) :: snw_rds(maxpatch,-nlevsno+1:0)
  real(r8) :: t_lake(maxpatch,nlevlak)
  real(r8) :: t_soisno(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: h2osoi_liq(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: h2osoi_ice(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: dzclm(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: zclm(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: ziclm(maxpatch,-nlevsno:nlevgrnd)
  real(r8) :: h2osoi_vol(maxpatch,nlevgrnd)
    real(r8) :: t_ref2m(maxpatch)
    real(r8) :: q_ref2m(maxpatch)
    real(r8) :: znt(maxpatch)
    integer :: g,l,c,p,j
    real(r8):: pftsum
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    integer :: numc, nump
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    numc = endc-begc+1
    nump = endp-begp+1
    do c = begc,endc
       snl(c) = clm3%g%l%c%cps%snl(c)
    end do
    do c = begc,endc
       snowdp(c) = clm3%g%l%c%cps%snowdp(c)
    end do
    do j = -nlevsno+1,0
     do c = begc,endc
       snw_rds(c,j) = clm3%g%l%c%cps%snw_rds(c,j)
     end do
    end do
    do c = begc,endc
       frac_sno(c) = clm3%g%l%c%cps%frac_sno(c)
    end do
    do j = -nlevsno+1,0
       do c = begc,endc
          dzclm(c,j) = clm3%g%l%c%cps%dz(c,j)
       end do
    end do
    do j = -nlevsno+1,0
       do c = begc,endc
          zclm(c,j) = clm3%g%l%c%cps%z(c,j)
       end do
    end do
    do j = -nlevsno,0
       do c = begc,endc
          ziclm(c,j) = clm3%g%l%c%cps%zi(c,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          albd(j,p) = clm3%g%l%c%p%pps%albd(p,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          albi(j,p) = clm3%g%l%c%p%pps%albi(p,j)
       end do
    end do
    do j = 1,numrad
       do c = begc,endc
          albgrd(j,c) = clm3%g%l%c%cps%albgrd(c,j)
       end do
    end do
    do j = 1,numrad
       do c = begc,endc
          albgri(j,c) = clm3%g%l%c%cps%albgri(c,j)
       end do
    end do
    do c = begc,endc
       h2osno(c) = clm3%g%l%c%cws%h2osno(c)
    end do
    do j = -nlevsno+1,nlevgrnd
       do c = begc,endc
          h2osoi_liq(c,j) = clm3%g%l%c%cws%h2osoi_liq(c,j)
       end do
    end do
    do j = -nlevsno+1,nlevgrnd
       do c = begc,endc
          h2osoi_ice(c,j) = clm3%g%l%c%cws%h2osoi_ice(c,j)
       end do
    end do
    do c = begc,endc
       t_grnd(c) = clm3%g%l%c%ces%t_grnd(c)
    end do
    do p = begp,endp
       t_ref2m(p) = clm3%g%l%c%p%pes%t_ref2m(p)
    end do
    do p = begp,endp
       q_ref2m(p) = clm3%g%l%c%p%pes%q_ref2m(p)
    end do
    do j = -nlevsno+1,nlevgrnd
       do c = begc,endc
          t_soisno(c,j) = clm3%g%l%c%ces%t_soisno(c,j)
       end do
    end do
    do j = 1,nlevlak
       do c = begc,endc
          t_lake(c,j) = clm3%g%l%c%ces%t_lake(c,j)
       end do
    end do
    do p = begp,endp
       frac_veg_nosno_alb(p) = clm3%g%l%c%p%pps%frac_veg_nosno_alb(p)
    end do
    do p = begp,endp
       fwet(p) = clm3%g%l%c%p%pps%fwet(p)
    end do
    do p = begp,endp
       tlai(p) = clm3%g%l%c%p%pps%tlai(p)
    end do
    do p = begp,endp
       tsai(p) = clm3%g%l%c%p%pps%tsai(p)
    end do
    do p = begp,endp
       elai(p) = clm3%g%l%c%p%pps%elai(p)
    end do
    do p = begp,endp
       esai(p)= clm3%g%l%c%p%pps%esai(p)
    end do
    do p = begp,endp
       fsun(p)= clm3%g%l%c%p%pps%fsun(p)
    end do
    do p = begp,endp
       htop(p)= clm3%g%l%c%p%pps%htop(p)
    end do
    do p = begp,endp
       hbot(p)= clm3%g%l%c%p%pps%hbot(p)
    end do
    do j = 1,numrad
       do p = begp,endp
          fabd(j,p) = clm3%g%l%c%p%pps%fabd(p,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          fabi(j,p) = clm3%g%l%c%p%pps%fabi(p,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          ftdd(j,p) = clm3%g%l%c%p%pps%ftdd(p,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          ftid(j,p) = clm3%g%l%c%p%pps%ftid(p,j)
       end do
    end do
    do j = 1,numrad
       do p = begp,endp
          ftii(j,p) = clm3%g%l%c%p%pps%ftii(p,j)
       end do
    end do
    do p = begp,endp
       t_veg(p) = clm3%g%l%c%p%pes%t_veg(p)
    end do
    do p = begp,endp
       h2ocan(p) = clm3%g%l%c%p%pws%h2ocan(p)
    end do
    do p = begp,endp
        c = clm3%g%l%c%p%column(p)
       if(clm3%g%l%c%p%itype(p)/=noveg) then
          znt(p) = clm3%g%l%c%p%pps%z0mv(p)
       else
          znt(p) = clm3%g%l%c%cps%z0mg(c)
       end if
    end do
       do c = begc,endc
          clm3%g%l%c%cws%pws_a%h2ocan(c) = 0.
       end do
       do p = begp,endp
          c = clm3%g%l%c%p%column(p)
          clm3%g%l%c%cws%pws_a%h2ocan(c) = clm3%g%l%c%cws%pws_a%h2ocan(c) &
               + clm3%g%l%c%p%pws%h2ocan(p) * clm3%g%l%c%p%wtcol(p)
          h2ocan_col(c) = clm3%g%l%c%cws%pws_a%h2ocan(c)
       end do
    do j = 1,nlevgrnd
        do c = begc,endc
            clm3%g%l%c%cws%h2osoi_vol(c,j) = &
                clm3%g%l%c%cws%h2osoi_liq(c,j)/(clm3%g%l%c%cps%dz(c,j)*denh2o) &
              + clm3%g%l%c%cws%h2osoi_ice(c,j)/(clm3%g%l%c%cps%dz(c,j)*denice)
            h2osoi_vol(c,j) = clm3%g%l%c%cws%h2osoi_vol(c,j)
        end do
     end do
    do c = begc,endc
         wtc(c) = clm3%g%l%c%wtgcell(c)
    end do
    do p = begp,endp
         wtp(p) = clm3%g%l%c%p%wtgcell(p)
    end do
  end subroutine biophy_to_wrf
module BalanceCheckMod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use module_cam_support, only: endrun
  implicit none
  save
  public :: BeginWaterBalance
  public :: BalanceCheck
contains
  subroutine BeginWaterBalance(lbc, ubc, lbp, ubp, &
             num_nolakec, filter_nolakec, num_lakec, filter_lakec, &
             num_hydrologyc, filter_hydrologyc)
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtype
    use clm_varpar , only : nlevgrnd, nlevsoi
    use subgridAveMod, only : p2c
    use clm_varcon , only : icol_roof, icol_sunwall, icol_shadewall, icol_road_perv, &
                              icol_road_imperv
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_lakec
    integer, intent(in) :: filter_lakec(ubc-lbc+1)
    integer , intent(in) :: num_hydrologyc
    integer , intent(in) :: filter_hydrologyc(ubc-lbc+1)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2ocan_pft(:)
    real(r8), pointer :: wa(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: zwt(:)
    real(r8), pointer :: zi(:,:)
    real(r8), pointer :: h2ocan_col(:)
    real(r8), pointer :: begwb(:)
    integer :: c, p, f, j, fc
    h2osno => clm3%g%l%c%cws%h2osno
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    begwb => clm3%g%l%c%cwbal%begwb
    h2ocan_col => clm3%g%l%c%cws%pws_a%h2ocan
    wa => clm3%g%l%c%cws%wa
    ctype => clm3%g%l%c%itype
    zwt => clm3%g%l%c%cws%zwt
    zi => clm3%g%l%c%cps%zi
    h2ocan_pft => clm3%g%l%c%p%pws%h2ocan
    call p2c(num_nolakec, filter_nolakec, h2ocan_pft, h2ocan_col)
    do f = 1, num_hydrologyc
       c = filter_hydrologyc(f)
       if(zwt(c) <= zi(c,nlevsoi)) then
          wa(c) = 5000._r8
       end if
    end do
    do f = 1, num_nolakec
       c = filter_nolakec(f)
       if (ctype(c) == icol_roof .or. ctype(c) == icol_sunwall &
          .or. ctype(c) == icol_shadewall .or. ctype(c) == icol_road_imperv) then
         begwb(c) = h2ocan_col(c) + h2osno(c)
       else
         begwb(c) = h2ocan_col(c) + h2osno(c) + wa(c)
       end if
    end do
    do j = 1, nlevgrnd
      do f = 1, num_nolakec
         c = filter_nolakec(f)
         begwb(c) = begwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
      end do
    end do
    do f = 1, num_lakec
       c = filter_lakec(f)
       begwb(c) = h2osno(c)
    end do
  end subroutine BeginWaterBalance
  subroutine BalanceCheck(lbp, ubp, lbc, ubc, lbl, ubl, lbg, ubg)
    use clmtype
    use subgridAveMod
    use globals , only :nstep, dtime
    use clm_varcon , only : isturb, icol_roof, icol_sunwall, icol_shadewall, &
                              spval, icol_road_perv, icol_road_imperv
    implicit none
    integer :: lbp, ubp
    integer :: lbc, ubc
    integer :: lbl, ubl
    integer :: lbg, ubg
    integer , pointer :: pgridcell(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: cgridcell(:)
    integer , pointer :: ltype(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: pwtgcell(:)
    real(r8), pointer :: cwtgcell(:)
    real(r8), pointer :: forc_rain(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: forc_lwrad(:)
    real(r8), pointer :: endwb(:)
    real(r8), pointer :: begwb(:)
    real(r8), pointer :: fsa(:)
    real(r8), pointer :: fsr(:)
    real(r8), pointer :: eflx_lwrad_out(:)
    real(r8), pointer :: eflx_lwrad_net(:)
    real(r8), pointer :: sabv(:)
    real(r8), pointer :: sabg(:)
    real(r8), pointer :: eflx_sh_tot(:)
    real(r8), pointer :: eflx_sh_totg(:)
    real(r8), pointer :: eflx_dynbal(:)
    real(r8), pointer :: eflx_lh_tot(:)
    real(r8), pointer :: eflx_soil_grnd(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: qflx_surf(:)
    real(r8), pointer :: qflx_qrgwl(:)
    real(r8), pointer :: qflx_drain(:)
    real(r8), pointer :: qflx_runoff(:)
    real(r8), pointer :: qflx_runoffg(:)
    real(r8), pointer :: qflx_liq_dynbal(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: qflx_snwcp_iceg(:)
    real(r8), pointer :: qflx_ice_dynbal(:)
    real(r8), pointer :: forc_solad(:,:)
    real(r8), pointer :: forc_solai(:,:)
    real(r8), pointer :: eflx_traffic_pft(:)
    real(r8), pointer :: eflx_wasteheat_pft(:)
    real(r8), pointer :: canyon_hwr(:)
    real(r8), pointer :: eflx_heat_from_ac_pft(:)
    real(r8), pointer :: errh2o(:)
    real(r8), pointer :: errsol(:)
    real(r8), pointer :: errlon(:)
    real(r8), pointer :: errseb(:)
    real(r8), pointer :: netrad(:)
    real(r8), pointer :: errsoi_col(:)
    integer :: p,c,l,g
    logical :: found
    integer :: indexp,indexc,indexl,indexg
    real(r8) :: forc_rain_col(lbc:ubc)
    real(r8) :: forc_snow_col(lbc:ubc)
    forc_rain => clm_a2l%forc_rain
    forc_snow => clm_a2l%forc_snow
    forc_lwrad => clm_a2l%forc_lwrad
    forc_solad => clm_a2l%forc_solad
    forc_solai => clm_a2l%forc_solai
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    ctype => clm3%g%l%c%itype
    cgridcell => clm3%g%l%c%gridcell
    cwtgcell => clm3%g%l%c%wtgcell
    endwb => clm3%g%l%c%cwbal%endwb
    begwb => clm3%g%l%c%cwbal%begwb
    qflx_surf => clm3%g%l%c%cwf%qflx_surf
    qflx_qrgwl => clm3%g%l%c%cwf%qflx_qrgwl
    qflx_drain => clm3%g%l%c%cwf%qflx_drain
    qflx_runoff => clm3%g%l%c%cwf%qflx_runoff
    qflx_snwcp_ice => clm3%g%l%c%cwf%pwf_a%qflx_snwcp_ice
    qflx_evap_tot => clm3%g%l%c%cwf%pwf_a%qflx_evap_tot
    errh2o => clm3%g%l%c%cwbal%errh2o
    errsoi_col => clm3%g%l%c%cebal%errsoi
    pgridcell => clm3%g%l%c%p%gridcell
    plandunit => clm3%g%l%c%p%landunit
    pwtgcell => clm3%g%l%c%p%wtgcell
    fsa => clm3%g%l%c%p%pef%fsa
    fsr => clm3%g%l%c%p%pef%fsr
    eflx_lwrad_out => clm3%g%l%c%p%pef%eflx_lwrad_out
    eflx_lwrad_net => clm3%g%l%c%p%pef%eflx_lwrad_net
    sabv => clm3%g%l%c%p%pef%sabv
    sabg => clm3%g%l%c%p%pef%sabg
    eflx_sh_tot => clm3%g%l%c%p%pef%eflx_sh_tot
    eflx_lh_tot => clm3%g%l%c%p%pef%eflx_lh_tot
    eflx_soil_grnd => clm3%g%l%c%p%pef%eflx_soil_grnd
    errsol => clm3%g%l%c%p%pebal%errsol
    errseb => clm3%g%l%c%p%pebal%errseb
    errlon => clm3%g%l%c%p%pebal%errlon
    netrad => clm3%g%l%c%p%pef%netrad
    eflx_wasteheat_pft => clm3%g%l%c%p%pef%eflx_wasteheat_pft
    eflx_heat_from_ac_pft => clm3%g%l%c%p%pef%eflx_heat_from_ac_pft
    eflx_traffic_pft => clm3%g%l%c%p%pef%eflx_traffic_pft
    qflx_runoffg => clm3%g%gwf%qflx_runoffg
    qflx_liq_dynbal => clm3%g%gwf%qflx_liq_dynbal
    qflx_snwcp_iceg => clm3%g%gwf%qflx_snwcp_iceg
    qflx_ice_dynbal => clm3%g%gwf%qflx_ice_dynbal
    eflx_sh_totg => clm3%g%gef%eflx_sh_totg
    eflx_dynbal => clm3%g%gef%eflx_dynbal
    do c = lbc,ubc
       g = cgridcell(c)
       if (ctype(c) == icol_sunwall .or. ctype(c) == icol_shadewall) then
          forc_rain_col(c) = 0.
          forc_snow_col(c) = 0.
       else
          forc_rain_col(c) = forc_rain(g)
          forc_snow_col(c) = forc_snow(g)
       end if
    end do
    do c = lbc, ubc
       g = cgridcell(c)
       errh2o(c) = endwb(c) - begwb(c) &
            - (forc_rain_col(c) + forc_snow_col(c) - qflx_evap_tot(c) - qflx_surf(c) &
            - qflx_qrgwl(c) - qflx_drain(c) - qflx_snwcp_ice(c)) * dtime
    end do
    found = .false.
    do c = lbc, ubc
       if (cwtgcell(c) > 0._r8 .and. abs(errh2o(c)) > 1e-7_r8) then
          found = .true.
          indexc = c
       end if
    end do
    if ( found ) then
       if ((ctype(indexc) .eq. icol_roof .or. ctype(indexc) .eq. icol_road_imperv .or. &
            ctype(indexc) .eq. icol_road_perv) .and. abs(errh2o(indexc)) > 1.e-1 .and. (nstep > 2) ) then
          write(6,*)'clm urban model is stopping - error is greater than 1.e-1'
          write(6,*)'nstep = ',nstep,' indexc= ',indexc,' errh2o= ',errh2o(indexc)
          write(6,*)'ctype(indexc): ',ctype(indexc)
          write(6,*)'forc_rain    = ',forc_rain_col(indexc)
          write(6,*)'forc_snow    = ',forc_snow_col(indexc)
          write(6,*)'endwb        = ',endwb(indexc)
          write(6,*)'begwb        = ',begwb(indexc)
          write(6,*)'qflx_evap_tot= ',qflx_evap_tot(indexc)
          write(6,*)'qflx_surf    = ',qflx_surf(indexc)
          write(6,*)'qflx_qrgwl   = ',qflx_qrgwl(indexc)
          write(6,*)'qflx_drain   = ',qflx_drain(indexc)
          write(6,*)'qflx_snwcp_ice   = ',qflx_snwcp_ice(indexc)
          write(6,*)'clm model is stopping'
          call endrun()
       else if (abs(errh2o(indexc)) > .10_r8 .and. (nstep > 2) ) then
          write(6,*)'clm model is stopping - error is greater than .10'
          write(6,*)'nstep = ',nstep,' indexc= ',indexc,' errh2o= ',errh2o(indexc)
          write(6,*)'ctype(indexc): ',ctype(indexc)
          write(6,*)'forc_rain    = ',forc_rain_col(indexc)
          write(6,*)'forc_snow    = ',forc_snow_col(indexc)
          write(6,*)'endwb        = ',endwb(indexc)
          write(6,*)'begwb        = ',begwb(indexc)
          write(6,*)'qflx_evap_tot= ',qflx_evap_tot(indexc)
          write(6,*)'qflx_surf    = ',qflx_surf(indexc)
          write(6,*)'qflx_qrgwl   = ',qflx_qrgwl(indexc)
          write(6,*)'qflx_drain   = ',qflx_drain(indexc)
          write(6,*)'qflx_snwcp_ice   = ',qflx_snwcp_ice(indexc)
          write(6,*)'clm model is stopping'
          call endrun()
       end if
    end if
    do p = lbp, ubp
       if (pwtgcell(p)>0._r8) then
          g = pgridcell(p)
          l = plandunit(p)
          if (ltype(l) /= isturb) then
             errsol(p) = fsa(p) + fsr(p) &
                  - (forc_solad(g,1) + forc_solad(g,2) + forc_solai(g,1) + forc_solai(g,2))
          else
             errsol(p) = spval
          end if
          if (ltype(l) /= isturb) then
             errlon(p) = eflx_lwrad_out(p) - eflx_lwrad_net(p) - forc_lwrad(g)
          else
             errlon(p) = spval
          end if
          if (ltype(l) /= isturb) then
             errseb(p) = sabv(p) + sabg(p) + forc_lwrad(g) - eflx_lwrad_out(p) &
                         - eflx_sh_tot(p) - eflx_lh_tot(p) - eflx_soil_grnd(p)
          else
             errseb(p) = sabv(p) + sabg(p) &
                         - eflx_lwrad_net(p) &
                         - eflx_sh_tot(p) - eflx_lh_tot(p) - eflx_soil_grnd(p) &
                         + eflx_wasteheat_pft(p) + eflx_heat_from_ac_pft(p) + eflx_traffic_pft(p)
          end if
          netrad(p) = fsa(p) - eflx_lwrad_net(p)
       end if
    end do
    found = .false.
    do p = lbp, ubp
       if (pwtgcell(p)>0._r8) then
          if ( (errsol(p) /= spval) .and. (abs(errsol(p)) > .10_r8) ) then
             found = .true.
             indexp = p
             indexg = pgridcell(p)
          end if
       end if
    end do
    if ( found .and. (nstep > 2) ) then
       write(6,100)'BalanceCheck: solar radiation balance error', nstep, indexp, errsol(indexp)
       write(6,*)'fsa          = ',fsa(indexp)
       write(6,*)'fsr          = ',fsr(indexp)
       write(6,*)'forc_solad(1)= ',forc_solad(indexg,1)
       write(6,*)'forc_solad(2)= ',forc_solad(indexg,2)
       write(6,*)'forc_solai(1)= ',forc_solai(indexg,1)
       write(6,*)'forc_solai(2)= ',forc_solai(indexg,2)
       write(6,*)'forc_tot     = ',forc_solad(indexg,1)+forc_solad(indexg,2)&
                                  +forc_solai(indexg,1)+forc_solai(indexg,2)
       write(6,*)'clm model is stopping'
       call endrun()
    end if
    found = .false.
    do p = lbp, ubp
       if (pwtgcell(p)>0._r8) then
          if ( (errlon(p) /= spval) .and. (abs(errlon(p)) > .10_r8) ) then
             found = .true.
             indexp = p
          end if
       end if
    end do
    if ( found .and. (nstep > 2) ) then
       write(6,100)'BalanceCheck: longwave enery balance error',nstep,indexp,errlon(indexp)
       write(6,*)'clm model is stopping'
       call endrun()
    end if
    found = .false.
    do p = lbp, ubp
       if (pwtgcell(p)>0._r8) then
          if (abs(errseb(p)) > .10_r8 ) then
             found = .true.
             indexp = p
          end if
       end if
    end do
    if ( found .and. (nstep > 2) ) then
       write(6,100)'BalanceCheck: surface flux energy balance error',nstep,indexp,errseb(indexp)
       write(6,*)' sabv           = ',sabv(indexp)
       write(6,*)' sabg           = ',sabg(indexp)
       write(6,*)' eflx_lwrad_net = ',eflx_lwrad_net(indexp)
       write(6,*)' eflx_sh_tot    = ',eflx_sh_tot(indexp)
       write(6,*)' eflx_lh_tot    = ',eflx_lh_tot(indexp)
       write(6,*)' eflx_soil_grnd = ',eflx_soil_grnd(indexp)
       write(6,*)'clm model is stopping'
       call endrun()
    end if
    found = .false.
    do c = lbc, ubc
       if (abs(errsoi_col(c)) > 1.0e-2_r8 ) then
          found = .true.
          indexc = c
       end if
    end do
    if ( found ) then
       write(6,100)'BalanceCheck: soil balance error',nstep,indexc,errsoi_col(indexc)
       if (abs(errsoi_col(indexc)) > .10_r8 .and. (nstep > 2) ) then
          write(6,*)'clm model is stopping'
          call endrun()
       end if
    end if
    call c2g( lbc, ubc, lbl, ubl, lbg, ubg, &
              qflx_runoff(lbc:ubc), qflx_runoffg(lbg:ubg), &
              c2l_scale_type= 'urbanf', l2g_scale_type='unity' )
    do g = lbg, ubg
       qflx_runoffg(g) = qflx_runoffg(g) - qflx_liq_dynbal(g)
    enddo
    call c2g( lbc, ubc, lbl, ubl, lbg, ubg, &
              qflx_snwcp_ice(lbc:ubc), qflx_snwcp_iceg(lbg:ubg), &
              c2l_scale_type= 'urbanf', l2g_scale_type='unity' )
    do g = lbg, ubg
       qflx_snwcp_iceg(g) = qflx_snwcp_iceg(g) - qflx_ice_dynbal(g)
    enddo
    call p2g( lbp, ubp, lbc, ubc, lbl, ubl, lbg, ubg, &
              eflx_sh_tot(lbp:ubp), eflx_sh_totg(lbg:ubg), &
              p2c_scale_type='unity',c2l_scale_type='urbanf',l2g_scale_type='unity')
    do g = lbg, ubg
       eflx_sh_totg(g) = eflx_sh_totg(g) - eflx_dynbal(g)
    enddo
100 format (1x,a,' nstep =',i10,' point =',i6,' imbalance =',f12.6,' W/m2')
200 format (1x,a,' nstep =',i10,' point =',i6,' imbalance =',f12.6,' mm')
  end subroutine BalanceCheck
end module BalanceCheckMod
module BareGroundFluxesMod
   use shr_kind_mod, only: r8 => shr_kind_r8
   implicit none
   save
   public :: BareGroundFluxes
contains
  subroutine BareGroundFluxes(lbp, ubp, num_nolakep, filter_nolakep)
    use clmtype
    use clm_varpar , only : nlevgrnd
    use clm_varcon , only : cpair, vkc, grav, denice, denh2o, istsoil
    use shr_const_mod , only : SHR_CONST_RGAS
    use FrictionVelocityMod, only : FrictionVelocity, MoninObukIni
    use QSatMod , only : QSat
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
    integer , pointer :: pcolumn(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: ltype(:)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: thm(:)
    real(r8), pointer :: qg(:)
    real(r8), pointer :: thv(:)
    real(r8), pointer :: dqgdT(:)
    real(r8), pointer :: htvp(:)
    real(r8), pointer :: beta(:)
    real(r8), pointer :: zii(:)
    real(r8), pointer :: forc_u(:)
    real(r8), pointer :: forc_v(:)
    real(r8), pointer :: forc_t(:)
    real(r8), pointer :: forc_th(:)
    real(r8), pointer :: forc_q(:)
    real(r8), pointer :: forc_rho(:)
    real(r8), pointer :: forc_pbot(:)
    real(r8), pointer :: forc_hgt_u_pft(:)
    real(r8), pointer :: psnsun(:)
    real(r8), pointer :: psnsha(:)
    real(r8), pointer :: z0mg_col(:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: frac_sno(:)
    real(r8), pointer :: soilbeta(:)
    real(r8), pointer :: z0hg_col(:)
    real(r8), pointer :: z0qg_col(:)
    real(r8), pointer :: dlrad(:)
    real(r8), pointer :: ulrad(:)
    real(r8), pointer :: cgrnds(:)
    real(r8), pointer :: cgrndl(:)
    real(r8), pointer :: cgrnd(:)
    real(r8), pointer :: taux(:)
    real(r8), pointer :: tauy(:)
    real(r8), pointer :: eflx_sh_grnd(:)
    real(r8), pointer :: eflx_sh_tot(:)
    real(r8), pointer :: qflx_evap_soi(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: t_ref2m(:)
    real(r8), pointer :: q_ref2m(:)
    real(r8), pointer :: t_ref2m_r(:)
    real(r8), pointer :: rh_ref2m_r(:)
    real(r8), pointer :: rh_ref2m(:)
    real(r8), pointer :: t_veg(:)
    real(r8), pointer :: btran(:)
    real(r8), pointer :: rssun(:)
    real(r8), pointer :: rssha(:)
    real(r8), pointer :: ram1(:)
    real(r8), pointer :: fpsn(:)
    real(r8), pointer :: rootr(:,:)
    real(r8), pointer :: rresis(:,:)
    integer, parameter :: niters = 3
    integer :: p,c,g,f,j,l
    integer :: filterp(ubp-lbp+1)
    integer :: fn
    integer :: fp
    integer :: iter
    real(r8) :: zldis(lbp:ubp)
    real(r8) :: displa(lbp:ubp)
    real(r8) :: zeta
    real(r8) :: wc
    real(r8) :: dth(lbp:ubp)
    real(r8) :: dthv
    real(r8) :: dqh(lbp:ubp)
    real(r8) :: obu(lbp:ubp)
    real(r8) :: ur(lbp:ubp)
    real(r8) :: um(lbp:ubp)
    real(r8) :: temp1(lbp:ubp)
    real(r8) :: temp12m(lbp:ubp)
    real(r8) :: temp2(lbp:ubp)
    real(r8) :: temp22m(lbp:ubp)
    real(r8) :: ustar(lbp:ubp)
    real(r8) :: tstar
    real(r8) :: qstar
    real(r8) :: thvstar
    real(r8) :: cf
    real(r8) :: ram
    real(r8) :: rah
    real(r8) :: raw
    real(r8) :: raih
    real(r8) :: raiw
    real(r8) :: fm(lbp:ubp)
    real(r8) :: z0mg_pft(lbp:ubp)
    real(r8) :: z0hg_pft(lbp:ubp)
    real(r8) :: z0qg_pft(lbp:ubp)
    real(r8) :: e_ref2m
    real(r8) :: de2mdT
    real(r8) :: qsat_ref2m
    real(r8) :: dqsat2mdT
    real(r8) :: www
    forc_th => clm_a2l%forc_th
    forc_pbot => clm_a2l%forc_pbot
    forc_t => clm_a2l%forc_t
    forc_u => clm_a2l%forc_u
    forc_v => clm_a2l%forc_v
    forc_rho => clm_a2l%forc_rho
    forc_q => clm_a2l%forc_q
    ltype => clm3%g%l%itype
    pcolumn => clm3%g%l%c%p%column
    pgridcell => clm3%g%l%c%p%gridcell
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    dlrad => clm3%g%l%c%p%pef%dlrad
    ulrad => clm3%g%l%c%p%pef%ulrad
    t_grnd => clm3%g%l%c%ces%t_grnd
    qg => clm3%g%l%c%cws%qg
    z0mg_col => clm3%g%l%c%cps%z0mg
    z0hg_col => clm3%g%l%c%cps%z0hg
    z0qg_col => clm3%g%l%c%cps%z0qg
    thv => clm3%g%l%c%ces%thv
    beta => clm3%g%l%c%cps%beta
    zii => clm3%g%l%c%cps%zii
    ram1 => clm3%g%l%c%p%pps%ram1
    cgrnds => clm3%g%l%c%p%pef%cgrnds
    cgrndl => clm3%g%l%c%p%pef%cgrndl
    cgrnd => clm3%g%l%c%p%pef%cgrnd
    dqgdT => clm3%g%l%c%cws%dqgdT
    htvp => clm3%g%l%c%cps%htvp
    watsat => clm3%g%l%c%cps%watsat
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    dz => clm3%g%l%c%cps%dz
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    frac_sno => clm3%g%l%c%cps%frac_sno
    soilbeta => clm3%g%l%c%cws%soilbeta
    taux => clm3%g%l%c%p%pmf%taux
    tauy => clm3%g%l%c%p%pmf%tauy
    eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
    eflx_sh_tot => clm3%g%l%c%p%pef%eflx_sh_tot
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    qflx_evap_tot => clm3%g%l%c%p%pwf%qflx_evap_tot
    t_ref2m => clm3%g%l%c%p%pes%t_ref2m
    q_ref2m => clm3%g%l%c%p%pes%q_ref2m
    t_ref2m_r => clm3%g%l%c%p%pes%t_ref2m_r
    rh_ref2m_r => clm3%g%l%c%p%pes%rh_ref2m_r
    plandunit => clm3%g%l%c%p%landunit
    rh_ref2m => clm3%g%l%c%p%pes%rh_ref2m
    t_veg => clm3%g%l%c%p%pes%t_veg
    thm => clm3%g%l%c%p%pes%thm
    btran => clm3%g%l%c%p%pps%btran
    rssun => clm3%g%l%c%p%pps%rssun
    rssha => clm3%g%l%c%p%pps%rssha
    rootr => clm3%g%l%c%p%pps%rootr
    rresis => clm3%g%l%c%p%pps%rresis
    psnsun => clm3%g%l%c%p%pcf%psnsun
    psnsha => clm3%g%l%c%p%pcf%psnsha
    fpsn => clm3%g%l%c%p%pcf%fpsn
    forc_hgt_u_pft => clm3%g%l%c%p%pps%forc_hgt_u_pft
    fn = 0
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       if (frac_veg_nosno(p) == 0) then
          fn = fn + 1
          filterp(fn) = p
       end if
    end do
!dir$ concurrent
    do f = 1, fn
       p = filterp(f)
       c = pcolumn(p)
       g = pgridcell(p)
       displa(p) = 0._r8
       dlrad(p) = 0._r8
       ulrad(p) = 0._r8
       ur(p) = max(1.0_r8,sqrt(forc_u(g)*forc_u(g)+forc_v(g)*forc_v(g)))
       dth(p) = thm(p)-t_grnd(c)
       dqh(p) = forc_q(g)-qg(c)
       dthv = dth(p)*(1._r8+0.61_r8*forc_q(g))+0.61_r8*forc_th(g)*dqh(p)
       zldis(p) = forc_hgt_u_pft(p)
       z0mg_pft(p) = z0mg_col(c)
       z0hg_pft(p) = z0hg_col(c)
       z0qg_pft(p) = z0qg_col(c)
       call MoninObukIni(ur(p), thv(c), dthv, zldis(p), z0mg_pft(p), um(p), obu(p))
    end do
    do iter = 1, niters
       call FrictionVelocity(lbp, ubp, fn, filterp, &
                             displa, z0mg_pft, z0hg_pft, z0qg_pft, &
                             obu, iter, ur, um, ustar, &
                             temp1, temp2, temp12m, temp22m, fm)
!dir$ concurrent
       do f = 1, fn
          p = filterp(f)
          c = pcolumn(p)
          g = pgridcell(p)
          tstar = temp1(p)*dth(p)
          qstar = temp2(p)*dqh(p)
          z0hg_pft(p) = z0mg_pft(p)/exp(0.13_r8 * (ustar(p)*z0mg_pft(p)/1.5e-5_r8)**0.45_r8)
          z0qg_pft(p) = z0hg_pft(p)
          thvstar = tstar*(1._r8+0.61_r8*forc_q(g)) + 0.61_r8*forc_th(g)*qstar
          zeta = zldis(p)*vkc*grav*thvstar/(ustar(p)**2*thv(c))
          if (zeta >= 0._r8) then
             zeta = min(2._r8,max(zeta,0.01_r8))
             um(p) = max(ur(p),0.1_r8)
          else
             zeta = max(-100._r8,min(zeta,-0.01_r8))
             wc = beta(c)*(-grav*ustar(p)*thvstar*zii(c)/thv(c))**0.333_r8
             um(p) = sqrt(ur(p)*ur(p) + wc*wc)
          end if
          obu(p) = zldis(p)/zeta
       end do
    end do
     do j = 1, nlevgrnd
!dir$ concurrent
       do f = 1, fn
          p = filterp(f)
          rootr(p,j) = 0._r8
          rresis(p,j) = 0._r8
        end do
     end do
!dir$ prefervector
!dir$ concurrent
    do f = 1, fn
       p = filterp(f)
       c = pcolumn(p)
       g = pgridcell(p)
       l = plandunit(p)
       ram = 1._r8/(ustar(p)*ustar(p)/um(p))
       rah = 1._r8/(temp1(p)*ustar(p))
       raw = 1._r8/(temp2(p)*ustar(p))
       raih = forc_rho(g)*cpair/rah
       www = (h2osoi_liq(c,1)/denh2o+h2osoi_ice(c,1)/denice)/dz(c,1)/watsat(c,1)
       if (dqh(p) .gt. 0._r8) then
          raiw = forc_rho(g)/(raw)
       else
          raiw = soilbeta(c)*forc_rho(g)/(raw)
       end if
       ram1(p) = ram
       cgrnds(p) = raih
       cgrndl(p) = raiw*dqgdT(c)
       cgrnd(p) = cgrnds(p) + htvp(c)*cgrndl(p)
       taux(p) = -forc_rho(g)*forc_u(g)/ram
       tauy(p) = -forc_rho(g)*forc_v(g)/ram
       eflx_sh_grnd(p) = -raih*dth(p)
       eflx_sh_tot(p) = eflx_sh_grnd(p)
       qflx_evap_soi(p) = -raiw*dqh(p)
       qflx_evap_tot(p) = qflx_evap_soi(p)
       t_ref2m(p) = thm(p) + temp1(p)*dth(p)*(1._r8/temp12m(p) - 1._r8/temp1(p))
       q_ref2m(p) = forc_q(g) + temp2(p)*dqh(p)*(1._r8/temp22m(p) - 1._r8/temp2(p))
       call QSat(t_ref2m(p), forc_pbot(g), e_ref2m, de2mdT, qsat_ref2m, dqsat2mdT)
       rh_ref2m(p) = min(100._r8, q_ref2m(p) / qsat_ref2m * 100._r8)
       if (ltype(l) == istsoil) then
         rh_ref2m_r(p) = rh_ref2m(p)
         t_ref2m_r(p) = t_ref2m(p)
       end if
       t_veg(p) = forc_t(g)
       btran(p) = 0._r8
       cf = forc_pbot(g)/(SHR_CONST_RGAS*0.001_r8*thm(p))*1.e06_r8
       rssun(p) = 1._r8/1.e15_r8 * cf
       rssha(p) = 1._r8/1.e15_r8 * cf
       psnsun(p) = 0._r8
       psnsha(p) = 0._r8
       fpsn(p) = 0._r8
       clm3%g%l%c%p%pps%lncsun(p) = 0._r8
       clm3%g%l%c%p%pps%lncsha(p) = 0._r8
       clm3%g%l%c%p%pps%vcmxsun(p) = 0._r8
       clm3%g%l%c%p%pps%vcmxsha(p) = 0._r8
       clm3%g%l%c%p%pps%cisun(p) = 0._r8
       clm3%g%l%c%p%pps%cisha(p) = 0._r8
    end do
  end subroutine BareGroundFluxes
end module BareGroundFluxesMod
module Biogeophysics1Mod
   use shr_kind_mod, only: r8 => shr_kind_r8
   use globals , only:nstep
   implicit none
   save
   public :: Biogeophysics1
contains
  subroutine Biogeophysics1(lbg, ubg, lbc, ubc, lbp, ubp, &
       num_nolakec, filter_nolakec, num_nolakep, filter_nolakep)
    use clmtype
    use clm_varcon , only : denh2o, denice, roverg, hvap, hsub, &
                                    istice, istwet, istsoil, isturb, istdlak, &
                                    zlnd, zsno, tfrz, &
                                    icol_roof, icol_sunwall, icol_shadewall, &
                                    icol_road_imperv, icol_road_perv, tfrz, spval, istdlak
    use clm_varpar , only : nlevgrnd, nlevurb, nlevsno, max_pft_per_gcell, nlevsoi
    use QSatMod , only : QSat
    use shr_const_mod , only : SHR_CONST_PI
    implicit none
    integer, intent(in) :: lbg, ubg
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
    integer , pointer :: ivt(:)
    integer , pointer :: ityplun(:)
    integer , pointer :: clandunit(:)
    integer , pointer :: cgridcell(:)
    real(r8), pointer :: pwtgcell(:)
    integer , pointer :: ctype(:)
    real(r8), pointer :: forc_pbot(:)
    real(r8), pointer :: forc_q(:)
    real(r8), pointer :: forc_t(:)
    real(r8), pointer :: forc_hgt_t(:)
    real(r8), pointer :: forc_hgt_u(:)
    real(r8), pointer :: forc_hgt_q(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: plandunit(:)
    real(r8), pointer :: forc_hgt_u_pft(:)
    real(r8), pointer :: forc_hgt_t_pft(:)
    real(r8), pointer :: forc_hgt_q_pft(:)
    integer , pointer :: frac_veg_nosno(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: pcolumn(:)
    real(r8), pointer :: z_0_town(:)
    real(r8), pointer :: z_d_town(:)
    real(r8), pointer :: forc_th(:)
    real(r8), pointer :: forc_u(:)
    real(r8), pointer :: forc_v(:)
    real(r8), pointer :: smpmin(:)
    integer , pointer :: snl(:)
    real(r8), pointer :: frac_sno(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: elai(:)
    real(r8), pointer :: esai(:)
    real(r8), pointer :: z0mr(:)
    real(r8), pointer :: displar(:)
    real(r8), pointer :: htop(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: watsat(:,:)
    real(r8), pointer :: sucsat(:,:)
    real(r8), pointer :: bsw(:,:)
    real(r8), pointer :: watfc(:,:)
    real(r8), pointer :: watopt(:,:)
    real(r8), pointer :: watdry(:,:)
    real(r8), pointer :: rootfr_road_perv(:,:)
    real(r8), pointer :: rootr_road_perv(:,:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: qg(:)
    real(r8), pointer :: dqgdT(:)
    real(r8), pointer :: emg(:)
    real(r8), pointer :: htvp(:)
    real(r8), pointer :: beta(:)
    real(r8), pointer :: zii(:)
    real(r8), pointer :: thm(:)
    real(r8), pointer :: thv(:)
    real(r8), pointer :: z0mg(:)
    real(r8), pointer :: z0hg(:)
    real(r8), pointer :: z0qg(:)
    real(r8), pointer :: emv(:)
    real(r8), pointer :: z0m(:)
    real(r8), pointer :: displa(:)
    real(r8), pointer :: z0mv(:)
    real(r8), pointer :: z0hv(:)
    real(r8), pointer :: z0qv(:)
    real(r8), pointer :: eflx_sh_tot(:)
    real(r8), pointer :: eflx_sh_tot_u(:)
    real(r8), pointer :: eflx_sh_tot_r(:)
    real(r8), pointer :: eflx_lh_tot(:)
    real(r8), pointer :: eflx_lh_tot_u(:)
    real(r8), pointer :: eflx_lh_tot_r(:)
    real(r8), pointer :: eflx_sh_veg(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: qflx_evap_veg(:)
    real(r8), pointer :: qflx_tran_veg(:)
    real(r8), pointer :: cgrnd(:)
    real(r8), pointer :: cgrnds(:)
    real(r8), pointer :: cgrndl(:)
    real(r8) ,pointer :: tssbef(:,:)
    real(r8) ,pointer :: soilalpha(:)
    real(r8) ,pointer :: soilbeta(:)
    real(r8) ,pointer :: soilalpha_u(:)
    integer :: g,l,c,p
    integer :: j
    integer :: fp
    integer :: fc
    real(r8) :: qred
    real(r8) :: avmuir
    real(r8) :: eg
    real(r8) :: qsatg
    real(r8) :: degdT
    real(r8) :: qsatgdT
    real(r8) :: fac
    real(r8) :: psit
    real(r8) :: hr
    real(r8) :: hr_road_perv
    real(r8) :: wx
    real(r8) :: fac_fc
    real(r8) :: eff_porosity
    real(r8) :: vol_ice
    real(r8) :: vol_liq
    integer :: pi
    forc_hgt_t => clm_a2l%forc_hgt_t
    forc_pbot => clm_a2l%forc_pbot
    forc_q => clm_a2l%forc_q
    forc_t => clm_a2l%forc_t
    forc_th => clm_a2l%forc_th
    forc_u => clm_a2l%forc_u
    forc_v => clm_a2l%forc_v
    forc_hgt_u => clm_a2l%forc_hgt_u
    forc_hgt_q => clm_a2l%forc_hgt_q
    npfts => clm3%g%npfts
    pfti => clm3%g%pfti
    ityplun => clm3%g%l%itype
    z_0_town => clm3%g%l%z_0_town
    z_d_town => clm3%g%l%z_d_town
    cgridcell => clm3%g%l%c%gridcell
    clandunit => clm3%g%l%c%landunit
    ctype => clm3%g%l%c%itype
    beta => clm3%g%l%c%cps%beta
    dqgdT => clm3%g%l%c%cws%dqgdT
    emg => clm3%g%l%c%cps%emg
    frac_sno => clm3%g%l%c%cps%frac_sno
    h2osno => clm3%g%l%c%cws%h2osno
    htvp => clm3%g%l%c%cps%htvp
    qg => clm3%g%l%c%cws%qg
    smpmin => clm3%g%l%c%cps%smpmin
    snl => clm3%g%l%c%cps%snl
    t_grnd => clm3%g%l%c%ces%t_grnd
    thv => clm3%g%l%c%ces%thv
    z0hg => clm3%g%l%c%cps%z0hg
    z0mg => clm3%g%l%c%cps%z0mg
    z0qg => clm3%g%l%c%cps%z0qg
    zii => clm3%g%l%c%cps%zii
    bsw => clm3%g%l%c%cps%bsw
    dz => clm3%g%l%c%cps%dz
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    soilalpha => clm3%g%l%c%cws%soilalpha
    soilbeta => clm3%g%l%c%cws%soilbeta
    soilalpha_u => clm3%g%l%c%cws%soilalpha_u
    sucsat => clm3%g%l%c%cps%sucsat
    t_soisno => clm3%g%l%c%ces%t_soisno
    tssbef => clm3%g%l%c%ces%tssbef
    watsat => clm3%g%l%c%cps%watsat
    watfc => clm3%g%l%c%cps%watfc
    watdry => clm3%g%l%c%cps%watdry
    watopt => clm3%g%l%c%cps%watopt
    rootfr_road_perv => clm3%g%l%c%cps%rootfr_road_perv
    rootr_road_perv => clm3%g%l%c%cps%rootr_road_perv
    ivt => clm3%g%l%c%p%itype
    elai => clm3%g%l%c%p%pps%elai
    esai => clm3%g%l%c%p%pps%esai
    htop => clm3%g%l%c%p%pps%htop
    emv => clm3%g%l%c%p%pps%emv
    z0m => clm3%g%l%c%p%pps%z0m
    displa => clm3%g%l%c%p%pps%displa
    z0mv => clm3%g%l%c%p%pps%z0mv
    z0hv => clm3%g%l%c%p%pps%z0hv
    z0qv => clm3%g%l%c%p%pps%z0qv
    eflx_sh_tot => clm3%g%l%c%p%pef%eflx_sh_tot
    eflx_sh_tot_u => clm3%g%l%c%p%pef%eflx_sh_tot_u
    eflx_sh_tot_r => clm3%g%l%c%p%pef%eflx_sh_tot_r
    eflx_lh_tot => clm3%g%l%c%p%pef%eflx_lh_tot
    eflx_lh_tot_u => clm3%g%l%c%p%pef%eflx_lh_tot_u
    eflx_lh_tot_r => clm3%g%l%c%p%pef%eflx_lh_tot_r
    eflx_sh_veg => clm3%g%l%c%p%pef%eflx_sh_veg
    qflx_evap_tot => clm3%g%l%c%p%pwf%qflx_evap_tot
    qflx_evap_veg => clm3%g%l%c%p%pwf%qflx_evap_veg
    qflx_tran_veg => clm3%g%l%c%p%pwf%qflx_tran_veg
    cgrnd => clm3%g%l%c%p%pef%cgrnd
    cgrnds => clm3%g%l%c%p%pef%cgrnds
    cgrndl => clm3%g%l%c%p%pef%cgrndl
    forc_hgt_u_pft => clm3%g%l%c%p%pps%forc_hgt_u_pft
    forc_hgt_t_pft => clm3%g%l%c%p%pps%forc_hgt_t_pft
    forc_hgt_q_pft => clm3%g%l%c%p%pps%forc_hgt_q_pft
    plandunit => clm3%g%l%c%p%landunit
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    thm => clm3%g%l%c%p%pes%thm
    pgridcell => clm3%g%l%c%p%gridcell
    pcolumn => clm3%g%l%c%p%column
    pwtgcell => clm3%g%l%c%p%wtgcell
    z0mr => pftcon%z0mr
    displar => pftcon%displar
    do j = -nlevsno+1, nlevgrnd
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          tssbef(c,j) = t_soisno(c,j)
       end do
    end do
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       l = clandunit(c)
       g = cgridcell(c)
       if (ctype(c) == icol_road_perv) then
          hr_road_perv = 0._r8
       end if
       t_grnd(c) = t_soisno(c,snl(c)+1)
       qred = 1._r8
       if (ityplun(l)/=istwet .AND. ityplun(l)/=istice) then
          if (ityplun(l) == istsoil) then
             wx = (h2osoi_liq(c,1)/denh2o+h2osoi_ice(c,1)/denice)/dz(c,1)
             fac = min(1._r8, wx/watsat(c,1))
             fac = max( fac, 0.01_r8 )
             psit = -sucsat(c,1) * fac ** (-bsw(c,1))
             psit = max(smpmin(c), psit)
             hr = exp(psit/roverg/t_grnd(c))
             qred = (1.-frac_sno(c))*hr + frac_sno(c)
             if (wx < watfc(c,1) ) then
                fac_fc = min(1._r8, wx/watfc(c,1))
                fac_fc = max( fac_fc, 0.01_r8 )
                soilbeta(c) = (1._r8-frac_sno(c))*0.25_r8*(1._r8 - cos(SHR_CONST_PI*fac_fc))**2._r8 &
                              + frac_sno(c)
             else
                soilbeta(c) = 1._r8
             end if
             soilalpha(c) = qred
          else if (ctype(c) == icol_road_perv) then
             do j = 1, nlevsoi
                if (t_soisno(c,j) >= tfrz) then
                   vol_ice = min(watsat(c,j), h2osoi_ice(c,j)/(dz(c,j)*denice))
                   eff_porosity = watsat(c,j)-vol_ice
                   vol_liq = min(eff_porosity, h2osoi_liq(c,j)/(dz(c,j)*denh2o))
                   fac = min( max(vol_liq-watdry(c,j),0._r8) / (watopt(c,j)-watdry(c,j)), 1._r8 )
                else
                   fac = 0._r8
                end if
                rootr_road_perv(c,j) = rootfr_road_perv(c,j)*fac
                hr_road_perv = hr_road_perv + rootr_road_perv(c,j)
             end do
             qred = (1.-frac_sno(c))*hr_road_perv + frac_sno(c)
             if (hr_road_perv .gt. 0._r8) then
                do j = 1, nlevsoi
                   rootr_road_perv(c,j) = rootr_road_perv(c,j)/hr_road_perv
                end do
             end if
             soilalpha_u(c) = qred
             soilbeta(c) = 0._r8
          else if (ctype(c) == icol_sunwall .or. ctype(c) == icol_shadewall) then
             qred = 0._r8
             soilbeta(c) = 0._r8
             soilalpha_u(c) = spval
          else if (ctype(c) == icol_roof .or. ctype(c) == icol_road_imperv) then
             qred = 1._r8
             soilbeta(c) = 0._r8
             soilalpha_u(c) = spval
          end if
       else
          soilalpha(c) = spval
          soilbeta(c) = 1._r8
       end if
       call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg, qsatgdT)
       qg(c) = qred*qsatg
       dqgdT(c) = qred*qsatgdT
       if (qsatg > forc_q(g) .and. forc_q(g) > qred*qsatg) then
          qg(c) = forc_q(g)
          dqgdT(c) = 0._r8
       end if
       if (ityplun(l) /= isturb) then
          if (ityplun(l)==istice) then
             emg(c) = 0.97_r8
          else
             emg(c) = (1._r8-frac_sno(c))*0.96_r8 + frac_sno(c)*0.97_r8
          end if
       end if
       htvp(c) = hvap
       if (h2osoi_liq(c,snl(c)+1) <= 0._r8 .and. h2osoi_ice(c,snl(c)+1) > 0._r8) htvp(c) = hsub
       if (frac_sno(c) > 0._r8) then
          z0mg(c) = zsno
       else
          z0mg(c) = zlnd
       end if
       z0hg(c) = z0mg(c)
       z0qg(c) = z0mg(c)
       beta(c) = 1._r8
       zii(c) = 1000._r8
       thv(c) = forc_th(g)*(1._r8+0.61_r8*forc_q(g))
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       eflx_sh_tot(p) = 0._r8
       l = plandunit(p)
       if (ityplun(l) == isturb) then
         eflx_sh_tot_u(p) = 0._r8
       else if (ityplun(l) == istsoil) then
         eflx_sh_tot_r(p) = 0._r8
       end if
       eflx_lh_tot(p) = 0._r8
       if (ityplun(l) == isturb) then
         eflx_lh_tot_u(p) = 0._r8
       else if (ityplun(l) == istsoil) then
         eflx_lh_tot_r(p) = 0._r8
       end if
       eflx_sh_veg(p) = 0._r8
       qflx_evap_tot(p) = 0._r8
       qflx_evap_veg(p) = 0._r8
       qflx_tran_veg(p) = 0._r8
       cgrnd(p) = 0._r8
       cgrnds(p) = 0._r8
       cgrndl(p) = 0._r8
       avmuir = 1._r8
       emv(p) = 1._r8-exp(-(elai(p)+esai(p))/avmuir)
       z0m(p) = z0mr(ivt(p)) * htop(p)
       displa(p) = displar(ivt(p)) * htop(p)
       z0mv(p) = z0m(p)
       z0hv(p) = z0mv(p)
       z0qv(p) = z0mv(p)
    end do
    do pi = 1,max_pft_per_gcell
       do g = lbg, ubg
          if (pi <= npfts(g)) then
            p = pfti(g) + pi - 1
            if (pwtgcell(p) > 0._r8) then
              l = plandunit(p)
              c = pcolumn(p)
              if (ityplun(l) == istsoil) then
                if (frac_veg_nosno(p) == 0) then
                  forc_hgt_u_pft(p) = forc_hgt_u(g) + z0mg(c) + displa(p)
                  forc_hgt_t_pft(p) = forc_hgt_t(g) + z0mg(c) + displa(p)
                  forc_hgt_q_pft(p) = forc_hgt_q(g) + z0mg(c) + displa(p)
                else
                  forc_hgt_u_pft(p) = forc_hgt_u(g) + z0m(p) + displa(p)
                  forc_hgt_t_pft(p) = forc_hgt_t(g) + z0m(p) + displa(p)
                  forc_hgt_q_pft(p) = forc_hgt_q(g) + z0m(p) + displa(p)
                end if
              else if (ityplun(l) == istice .or. ityplun(l) == istwet) then
                forc_hgt_u_pft(p) = forc_hgt_u(g) + z0mg(c)
                forc_hgt_t_pft(p) = forc_hgt_t(g) + z0mg(c)
                forc_hgt_q_pft(p) = forc_hgt_q(g) + z0mg(c)
              else if (ityplun(l) == istdlak) then
                if (t_grnd(c) >= tfrz) then
                  forc_hgt_u_pft(p) = forc_hgt_u(g) + 0.01_r8
                  forc_hgt_t_pft(p) = forc_hgt_t(g) + 0.01_r8
                  forc_hgt_q_pft(p) = forc_hgt_q(g) + 0.01_r8
                else
                  forc_hgt_u_pft(p) = forc_hgt_u(g) + 0.04_r8
                  forc_hgt_t_pft(p) = forc_hgt_t(g) + 0.04_r8
                  forc_hgt_q_pft(p) = forc_hgt_q(g) + 0.04_r8
                end if
              else if (ityplun(l) == isturb) then
                forc_hgt_u_pft(p) = forc_hgt_u(g) + z_0_town(l) + z_d_town(l)
                forc_hgt_t_pft(p) = forc_hgt_t(g) + z_0_town(l) + z_d_town(l)
                forc_hgt_q_pft(p) = forc_hgt_q(g) + z_0_town(l) + z_d_town(l)
              end if
            end if
          end if
       end do
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       g = pgridcell(p)
       thm(p) = forc_t(g) + 0.0098_r8*forc_hgt_t_pft(p)
    end do
  end subroutine Biogeophysics1
end module Biogeophysics1Mod
module Biogeophysics2Mod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use globals, only: nstep
  implicit none
  save
  public :: Biogeophysics2
contains
  subroutine Biogeophysics2 (lbl, ubl, lbc, ubc, lbp, ubp, &
             num_urbanl, filter_urbanl, num_nolakec, filter_nolakec, &
             num_nolakep, filter_nolakep)
    use clmtype
    use clm_varcon , only : hvap, cpair, grav, vkc, tfrz, sb, &
                                   isturb, icol_roof, icol_sunwall, icol_shadewall, istsoil
    use clm_varpar , only : nlevsno, nlevgrnd, max_pft_per_col
    use SoilTemperatureMod, only : SoilTemperature
    use subgridAveMod , only : p2c
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbl, ubl
    integer, intent(in) :: num_nolakec
    integer, intent(in) :: filter_nolakec(ubc-lbc+1)
    integer, intent(in) :: num_urbanl
    integer, intent(in) :: filter_urbanl(ubl-lbl+1)
    integer, intent(in) :: num_nolakep
    integer, intent(in) :: filter_nolakep(ubp-lbp+1)
    integer , pointer :: ctype(:)
    integer , pointer :: ltype(:)
    integer , pointer :: pcolumn(:)
    integer , pointer :: plandunit(:)
    integer , pointer :: pgridcell(:)
    real(r8), pointer :: pwtgcell(:)
    integer , pointer :: npfts(:)
    integer , pointer :: pfti(:)
    integer , pointer :: snl(:)
    logical , pointer :: do_capsnow(:)
    real(r8), pointer :: forc_lwrad(:)
    real(r8), pointer :: emg(:)
    real(r8), pointer :: htvp(:)
    real(r8), pointer :: t_grnd(:)
    integer , pointer :: frac_veg_nosno(:)
    real(r8), pointer :: cgrnds(:)
    real(r8), pointer :: cgrndl(:)
    real(r8), pointer :: sabg(:)
    real(r8), pointer :: dlrad(:)
    real(r8), pointer :: ulrad(:)
    real(r8), pointer :: eflx_sh_veg(:)
    real(r8), pointer :: qflx_evap_veg(:)
    real(r8), pointer :: qflx_tran_veg(:)
    real(r8), pointer :: qflx_evap_can(:)
    real(r8), pointer :: wtcol(:)
    real(r8), pointer :: tssbef(:,:)
    real(r8), pointer :: t_soisno(:,:)
    real(r8), pointer :: h2osoi_ice(:,:)
    real(r8), pointer :: h2osoi_liq(:,:)
    real(r8), pointer :: eflx_building_heat(:)
    real(r8), pointer :: eflx_traffic_pft(:)
    real(r8), pointer :: eflx_wasteheat_pft(:)
    real(r8), pointer :: eflx_heat_from_ac_pft(:)
    real(r8), pointer :: canyon_hwr(:)
    real(r8), pointer :: eflx_sh_grnd(:)
    real(r8), pointer :: qflx_evap_soi(:)
    real(r8), pointer :: qflx_snwcp_liq(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: dt_grnd(:)
    real(r8), pointer :: eflx_soil_grnd(:)
    real(r8), pointer :: eflx_soil_grnd_u(:)
    real(r8), pointer :: eflx_soil_grnd_r(:)
    real(r8), pointer :: eflx_sh_tot(:)
    real(r8), pointer :: eflx_sh_tot_u(:)
    real(r8), pointer :: eflx_sh_tot_r(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: eflx_lh_tot(:)
    real(r8), pointer :: eflx_lh_tot_u(:)
    real(r8), pointer :: eflx_lh_tot_r(:)
    real(r8), pointer :: qflx_evap_grnd(:)
    real(r8), pointer :: qflx_sub_snow(:)
    real(r8), pointer :: qflx_dew_snow(:)
    real(r8), pointer :: qflx_dew_grnd(:)
    real(r8), pointer :: eflx_lwrad_out(:)
    real(r8), pointer :: eflx_lwrad_net(:)
    real(r8), pointer :: eflx_lwrad_net_u(:)
    real(r8), pointer :: eflx_lwrad_net_r(:)
    real(r8), pointer :: eflx_lh_vege(:)
    real(r8), pointer :: eflx_lh_vegt(:)
    real(r8), pointer :: eflx_lh_grnd(:)
    real(r8), pointer :: errsoi_pft(:)
    real(r8), pointer :: errsoi_col(:)
    integer :: p,c,g,j,pi,l
    integer :: fc,fp
    real(r8) :: egsmax(lbc:ubc)
    real(r8) :: egirat(lbc:ubc)
    real(r8) :: tinc(lbc:ubc)
    real(r8) :: xmf(lbc:ubc)
    real(r8) :: sumwt(lbc:ubc)
    real(r8) :: evaprat(lbp:ubp)
    real(r8) :: save_qflx_evap_soi
    real(r8) :: topsoil_evap_tot(lbc:ubc)
    real(r8) :: fact(lbc:ubc, -nlevsno+1:nlevgrnd)
    real(r8) :: eflx_lwrad_del(lbp:ubp)
    forc_lwrad => clm_a2l%forc_lwrad
    ltype => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    ctype => clm3%g%l%c%itype
    npfts => clm3%g%l%c%npfts
    pfti => clm3%g%l%c%pfti
    snl => clm3%g%l%c%cps%snl
    do_capsnow => clm3%g%l%c%cps%do_capsnow
    htvp => clm3%g%l%c%cps%htvp
    emg => clm3%g%l%c%cps%emg
    t_grnd => clm3%g%l%c%ces%t_grnd
    dt_grnd => clm3%g%l%c%ces%dt_grnd
    t_soisno => clm3%g%l%c%ces%t_soisno
    tssbef => clm3%g%l%c%ces%tssbef
    h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
    h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
    errsoi_col => clm3%g%l%c%cebal%errsoi
    eflx_building_heat => clm3%g%l%c%cef%eflx_building_heat
    pcolumn => clm3%g%l%c%p%column
    plandunit => clm3%g%l%c%p%landunit
    pgridcell => clm3%g%l%c%p%gridcell
    pwtgcell => clm3%g%l%c%p%wtgcell
    frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
    sabg => clm3%g%l%c%p%pef%sabg
    dlrad => clm3%g%l%c%p%pef%dlrad
    ulrad => clm3%g%l%c%p%pef%ulrad
    eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
    eflx_sh_veg => clm3%g%l%c%p%pef%eflx_sh_veg
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    qflx_evap_veg => clm3%g%l%c%p%pwf%qflx_evap_veg
    qflx_tran_veg => clm3%g%l%c%p%pwf%qflx_tran_veg
    qflx_evap_can => clm3%g%l%c%p%pwf%qflx_evap_can
    qflx_snwcp_liq => clm3%g%l%c%p%pwf%qflx_snwcp_liq
    qflx_snwcp_ice => clm3%g%l%c%p%pwf%qflx_snwcp_ice
    qflx_evap_tot => clm3%g%l%c%p%pwf%qflx_evap_tot
    qflx_evap_grnd => clm3%g%l%c%p%pwf%qflx_evap_grnd
    qflx_sub_snow => clm3%g%l%c%p%pwf%qflx_sub_snow
    qflx_dew_snow => clm3%g%l%c%p%pwf%qflx_dew_snow
    qflx_dew_grnd => clm3%g%l%c%p%pwf%qflx_dew_grnd
    eflx_soil_grnd => clm3%g%l%c%p%pef%eflx_soil_grnd
    eflx_soil_grnd_u => clm3%g%l%c%p%pef%eflx_soil_grnd_u
    eflx_soil_grnd_r => clm3%g%l%c%p%pef%eflx_soil_grnd_r
    eflx_sh_tot => clm3%g%l%c%p%pef%eflx_sh_tot
    eflx_sh_tot_u => clm3%g%l%c%p%pef%eflx_sh_tot_u
    eflx_sh_tot_r => clm3%g%l%c%p%pef%eflx_sh_tot_r
    eflx_lh_tot => clm3%g%l%c%p%pef%eflx_lh_tot
    eflx_lh_tot_u => clm3%g%l%c%p%pef%eflx_lh_tot_u
    eflx_lh_tot_r => clm3%g%l%c%p%pef%eflx_lh_tot_r
    eflx_lwrad_out => clm3%g%l%c%p%pef%eflx_lwrad_out
    eflx_lwrad_net => clm3%g%l%c%p%pef%eflx_lwrad_net
    eflx_lwrad_net_u => clm3%g%l%c%p%pef%eflx_lwrad_net_u
    eflx_lwrad_net_r => clm3%g%l%c%p%pef%eflx_lwrad_net_r
    eflx_lh_vege => clm3%g%l%c%p%pef%eflx_lh_vege
    eflx_lh_vegt => clm3%g%l%c%p%pef%eflx_lh_vegt
    eflx_lh_grnd => clm3%g%l%c%p%pef%eflx_lh_grnd
    cgrnds => clm3%g%l%c%p%pef%cgrnds
    cgrndl => clm3%g%l%c%p%pef%cgrndl
    eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    errsoi_pft => clm3%g%l%c%p%pebal%errsoi
    wtcol => clm3%g%l%c%p%wtcol
    eflx_wasteheat_pft => clm3%g%l%c%p%pef%eflx_wasteheat_pft
    eflx_heat_from_ac_pft => clm3%g%l%c%p%pef%eflx_heat_from_ac_pft
    eflx_traffic_pft => clm3%g%l%c%p%pef%eflx_traffic_pft
    call SoilTemperature(lbl, ubl, lbc, ubc, num_urbanl, filter_urbanl, &
                         num_nolakec, filter_nolakec, xmf , fact)
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       j = snl(c)+1
       tinc(c) = t_soisno(c,j) - tssbef(c,j)
       egsmax(c) = (h2osoi_ice(c,j)+h2osoi_liq(c,j)) / dtime
       if (egsmax(c) < 0._r8) then
          egsmax(c) = 0._r8
       end if
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       c = pcolumn(p)
       eflx_sh_grnd(p) = eflx_sh_grnd(p) + tinc(c)*cgrnds(p)
       qflx_evap_soi(p) = qflx_evap_soi(p) + tinc(c)*cgrndl(p)
    end do
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       topsoil_evap_tot(c) = 0._r8
       sumwt(c) = 0._r8
    end do
    do pi = 1,max_pft_per_col
       do fc = 1,num_nolakec
          c = filter_nolakec(fc)
          if ( pi <= npfts(c) ) then
             p = pfti(c) + pi - 1
             if (pwtgcell(p)>0._r8) then
                topsoil_evap_tot(c) = topsoil_evap_tot(c) + qflx_evap_soi(p) * wtcol(p)
             end if
          end if
       end do
    end do
    do fc = 1,num_nolakec
       c = filter_nolakec(fc)
       if (topsoil_evap_tot(c) > egsmax(c)) then
          egirat(c) = (egsmax(c)/topsoil_evap_tot(c))
       else
          egirat(c) = 1.0_r8
       end if
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       c = pcolumn(p)
       l = plandunit(p)
       g = pgridcell(p)
       j = snl(c)+1
       if (egirat(c) < 1.0_r8) then
          save_qflx_evap_soi = qflx_evap_soi(p)
          qflx_evap_soi(p) = qflx_evap_soi(p) * egirat(c)
          eflx_sh_grnd(p) = eflx_sh_grnd(p) + (save_qflx_evap_soi - qflx_evap_soi(p))*htvp(c)
       end if
       if (ltype(l) /= isturb) then
          eflx_soil_grnd(p) = sabg(p) + dlrad(p) &
                              + (1-frac_veg_nosno(p))*emg(c)*forc_lwrad(g) &
                              - emg(c)*sb*tssbef(c,j)**3*(tssbef(c,j) + 4._r8*tinc(c)) &
                              - (eflx_sh_grnd(p) + qflx_evap_soi(p)*htvp(c))
          if (ltype(l) == istsoil) then
            eflx_soil_grnd_r(p) = eflx_soil_grnd(p)
          end if
       else
          eflx_lwrad_del(p) = 4._r8*emg(c)*sb*tssbef(c,j)**3*tinc(c)
          eflx_soil_grnd(p) = sabg(p) + dlrad(p) &
                              - eflx_lwrad_net(p) - eflx_lwrad_del(p) &
                              - (eflx_sh_grnd(p) + qflx_evap_soi(p)*htvp(c) + qflx_tran_veg(p)*hvap) &
                              + eflx_wasteheat_pft(p) + eflx_heat_from_ac_pft(p) + eflx_traffic_pft(p)
          eflx_soil_grnd_u(p) = eflx_soil_grnd(p)
       end if
       eflx_sh_tot(p) = eflx_sh_veg(p) + eflx_sh_grnd(p)
       qflx_evap_tot(p) = qflx_evap_veg(p) + qflx_evap_soi(p)
       eflx_lh_tot(p)= hvap*qflx_evap_veg(p) + htvp(c)*qflx_evap_soi(p)
       if (ltype(l) == istsoil) then
         eflx_lh_tot_r(p)= eflx_lh_tot(p)
         eflx_sh_tot_r(p)= eflx_sh_tot(p)
       else if (ltype(l) == isturb) then
         eflx_lh_tot_u(p)= eflx_lh_tot(p)
         eflx_sh_tot_u(p)= eflx_sh_tot(p)
       end if
       qflx_evap_grnd(p) = 0._r8
       qflx_sub_snow(p) = 0._r8
       qflx_dew_snow(p) = 0._r8
       qflx_dew_grnd(p) = 0._r8
       if (qflx_evap_soi(p) >= 0._r8) then
   if ((h2osoi_liq(c,j)+h2osoi_ice(c,j)) > 0.) then
             qflx_evap_grnd(p) = max(qflx_evap_soi(p)*(h2osoi_liq(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))), 0._r8)
   else
      qflx_evap_grnd(p) = 0.
   end if
          qflx_sub_snow(p) = qflx_evap_soi(p) - qflx_evap_grnd(p)
       else
          if (t_grnd(c) < tfrz) then
             qflx_dew_snow(p) = abs(qflx_evap_soi(p))
          else
             qflx_dew_grnd(p) = abs(qflx_evap_soi(p))
          end if
       end if
       if (snl(c) < 0 .and. do_capsnow(c)) then
          qflx_snwcp_liq(p) = qflx_snwcp_liq(p) + qflx_dew_grnd(p)
          qflx_snwcp_ice(p) = qflx_snwcp_ice(p) + qflx_dew_snow(p)
       end if
       qflx_evap_can(p) = qflx_evap_veg(p) - qflx_tran_veg(p)
       eflx_lh_vege(p) = (qflx_evap_veg(p) - qflx_tran_veg(p)) * hvap
       eflx_lh_vegt(p) = qflx_tran_veg(p) * hvap
       eflx_lh_grnd(p) = qflx_evap_soi(p) * htvp(c)
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       c = pcolumn(p)
       errsoi_pft(p) = eflx_soil_grnd(p) - xmf(c)
       if (ctype(c)==icol_sunwall .or. ctype(c)==icol_shadewall .or. ctype(c)==icol_roof) then
          errsoi_pft(p) = errsoi_pft(p) + eflx_building_heat(c)
       end if
    end do
    do j = -nlevsno+1,nlevgrnd
       do fp = 1,num_nolakep
          p = filter_nolakep(fp)
          c = pcolumn(p)
          if (j >= snl(c)+1) then
             errsoi_pft(p) = errsoi_pft(p) - (t_soisno(c,j)-tssbef(c,j))/fact(c,j)
          end if
       end do
    end do
    do fp = 1,num_nolakep
       p = filter_nolakep(fp)
       c = pcolumn(p)
       l = plandunit(p)
       g = pgridcell(p)
       j = snl(c)+1
       if (ltype(l) /= isturb) then
          eflx_lwrad_out(p) = ulrad(p) &
                              + (1-frac_veg_nosno(p))*(1.-emg(c))*forc_lwrad(g) &
                              + (1-frac_veg_nosno(p))*emg(c)*sb*tssbef(c,j)**4 &
                              + 4.*emg(c)*sb*tssbef(c,j)**3*tinc(c)
          eflx_lwrad_net(p) = eflx_lwrad_out(p) - forc_lwrad(g)
          if (ltype(l) == istsoil) then
            eflx_lwrad_net_r(p) = eflx_lwrad_out(p) - forc_lwrad(g)
          end if
       else
          eflx_lwrad_out(p) = eflx_lwrad_out(p) + eflx_lwrad_del(p)
          eflx_lwrad_net(p) = eflx_lwrad_net(p) + eflx_lwrad_del(p)
          eflx_lwrad_net_u(p) = eflx_lwrad_net_u(p) + eflx_lwrad_del(p)
       end if
    end do
    call p2c(num_nolakec, filter_nolakec, errsoi_pft, errsoi_col)
  end subroutine Biogeophysics2
end module Biogeophysics2Mod
module BiogeophysicsLakeMod
  implicit none
  save
  public :: BiogeophysicsLake
contains
  subroutine BiogeophysicsLake(lbc, ubc, lbp, ubp, num_lakec, filter_lakec, &
                               num_lakep, filter_lakep)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use clmtype
    use clm_varpar , only : nlevlak
    use clm_varcon , only : hvap, hsub, hfus, cpair, cpliq, cpice, tkwat, tkice, &
                                    sb, vkc, grav, denh2o, tfrz, spval
    use QSatMod , only : QSat
    use FrictionVelocityMod, only : FrictionVelocity, MoninObukIni
    use TridiagonalMod , only : Tridiagonal
    use globals , only : dtime
    implicit none
    integer, intent(in) :: lbc, ubc
    integer, intent(in) :: lbp, ubp
    integer, intent(in) :: num_lakec
    integer, intent(in) :: filter_lakec(ubc-lbc+1)
    integer, intent(in) :: num_lakep
    integer, intent(in) :: filter_lakep(ubp-lbp+1)
    integer , pointer :: pcolumn(:)
    integer , pointer :: pgridcell(:)
    integer , pointer :: cgridcell(:)
    real(r8), pointer :: forc_t(:)
    real(r8), pointer :: forc_pbot(:)
    real(r8), pointer :: forc_hgt_u_pft(:)
    real(r8), pointer :: forc_hgt_t_pft(:)
    real(r8), pointer :: forc_hgt_q_pft(:)
    real(r8), pointer :: forc_th(:)
    real(r8), pointer :: forc_q(:)
    real(r8), pointer :: forc_u(:)
    real(r8), pointer :: forc_v(:)
    real(r8), pointer :: forc_lwrad(:)
    real(r8), pointer :: forc_rho(:)
    real(r8), pointer :: forc_snow(:)
    real(r8), pointer :: forc_rain(:)
    real(r8), pointer :: t_grnd(:)
    real(r8), pointer :: hc_soisno(:)
    real(r8), pointer :: h2osno(:)
    real(r8), pointer :: snowdp(:)
    real(r8), pointer :: sabg(:)
    real(r8), pointer :: lat(:)
    real(r8), pointer :: dz(:,:)
    real(r8), pointer :: z(:,:)
    real(r8), pointer :: qflx_prec_grnd(:)
    real(r8), pointer :: qflx_evap_soi(:)
    real(r8), pointer :: qflx_evap_tot(:)
    real(r8), pointer :: qflx_snwcp_liq(:)
    real(r8), pointer :: qflx_snwcp_ice(:)
    real(r8), pointer :: eflx_sh_grnd(:)
    real(r8), pointer :: eflx_lwrad_out(:)
    real(r8), pointer :: eflx_lwrad_net(:)
    real(r8), pointer :: eflx_soil_grnd(:)
    real(r8), pointer :: eflx_sh_tot(:)
    real(r8), pointer :: eflx_lh_tot(:)
    real(r8), pointer :: eflx_lh_grnd(:)
    real(r8), pointer :: t_veg(:)
    real(r8), pointer :: t_ref2m(:)
    real(r8), pointer :: q_ref2m(:)
    real(r8), pointer :: rh_ref2m(:)
    real(r8), pointer :: taux(:)
    real(r8), pointer :: tauy(:)
    real(r8), pointer :: qmelt(:)
    real(r8), pointer :: ram1(:)
    real(r8), pointer :: errsoi(:)
    real(r8), pointer :: t_lake(:,:)
    integer , parameter :: idlak = 1
    integer , parameter :: niters = 3
    real(r8), parameter :: beta1 = 1._r8
    real(r8), parameter :: emg = 0.97_r8
    real(r8), parameter :: zii = 1000._r8
    real(r8), parameter :: p0 = 1._r8
    integer :: i,j,fc,fp,g,c,p
    integer :: fncopy
    integer :: fnold
    integer :: fpcopy(num_lakep)
    integer :: num_unfrzc
    integer :: filter_unfrzc(ubc-lbc+1)
    integer :: iter
    integer :: nmozsgn(lbp:ubp)
    integer :: jtop(lbc:ubc)
    real(r8) :: ax
    real(r8) :: bx
    real(r8) :: degdT
    real(r8) :: dqh(lbp:ubp)
    real(r8) :: dth(lbp:ubp)
    real(r8) :: dthv
    real(r8) :: dzsur(lbc:ubc)
    real(r8) :: eg
    real(r8) :: hm
    real(r8) :: htvp(lbc:ubc)
    real(r8) :: obu(lbp:ubp)
    real(r8) :: obuold(lbp:ubp)
    real(r8) :: qsatg(lbc:ubc)
    real(r8) :: qsatgdT(lbc:ubc)
    real(r8) :: qstar
    real(r8) :: ram(lbp:ubp)
    real(r8) :: rah(lbp:ubp)
    real(r8) :: raw(lbp:ubp)
    real(r8) :: stftg3(lbp:ubp)
    real(r8) :: temp1(lbp:ubp)
    real(r8) :: temp12m(lbp:ubp)
    real(r8) :: temp2(lbp:ubp)
    real(r8) :: temp22m(lbp:ubp)
    real(r8) :: tgbef(lbc:ubc)
    real(r8) :: thm(lbp:ubp)
    real(r8) :: thv(lbc:ubc)
    real(r8) :: thvstar
    real(r8) :: tksur
    real(r8) :: tstar
    real(r8) :: um(lbp:ubp)
    real(r8) :: ur(lbp:ubp)
    real(r8) :: ustar(lbp:ubp)
    real(r8) :: wc
    real(r8) :: zeta
    real(r8) :: zldis(lbp:ubp)
    real(r8) :: displa(lbp:ubp)
    real(r8) :: z0mg(lbp:ubp)
    real(r8) :: z0hg(lbp:ubp)
    real(r8) :: z0qg(lbp:ubp)
    real(r8) :: beta(2)
    real(r8) :: za(2)
    real(r8) :: eta(2)
    real(r8) :: a(lbc:ubc,nlevlak)
    real(r8) :: b(lbc:ubc,nlevlak)
    real(r8) :: c1(lbc:ubc,nlevlak)
    real(r8) :: r(lbc:ubc,nlevlak)
    real(r8) :: rhow(lbc:ubc,nlevlak)
    real(r8) :: phi(lbc:ubc,nlevlak)
    real(r8) :: kme(lbc:ubc,nlevlak)
    real(r8) :: cwat
    real(r8) :: ws(lbc:ubc)
    real(r8) :: ks(lbc:ubc)
    real(r8) :: in
    real(r8) :: out
    real(r8) :: ri
    real(r8) :: fin(lbc:ubc)
    real(r8) :: ocvts(lbc:ubc)
    real(r8) :: ncvts(lbc:ubc)
    real(r8) :: m1
    real(r8) :: m2
    real(r8) :: m3
    real(r8) :: ke
    real(r8) :: km
    real(r8) :: zin
    real(r8) :: zout
    real(r8) :: drhodz
    real(r8) :: n2
    real(r8) :: num
    real(r8) :: den
    real(r8) :: tav(lbc:ubc)
    real(r8) :: nav(lbc:ubc)
    real(r8) :: phidum
    real(r8) :: u2m
    real(r8) :: fm(lbp:ubp)
    real(r8) :: e_ref2m
    real(r8) :: de2mdT
    real(r8) :: qsat_ref2m
    real(r8) :: dqsat2mdT
    data beta/0.4_r8, 0.4_r8/
    data za /0.6_r8, 0.5_r8/
    data eta /0.1_r8, 0.5_r8/
    forc_t => clm_a2l%forc_t
    forc_pbot => clm_a2l%forc_pbot
    forc_th => clm_a2l%forc_th
    forc_q => clm_a2l%forc_q
    forc_u => clm_a2l%forc_u
    forc_v => clm_a2l%forc_v
    forc_rho => clm_a2l%forc_rho
    forc_lwrad => clm_a2l%forc_lwrad
    forc_snow => clm_a2l%forc_snow
    forc_rain => clm_a2l%forc_rain
    lat => clm3%g%lat
    cgridcell => clm3%g%l%c%gridcell
    dz => clm3%g%l%c%cps%dz
    z => clm3%g%l%c%cps%z
    t_lake => clm3%g%l%c%ces%t_lake
    h2osno => clm3%g%l%c%cws%h2osno
    snowdp => clm3%g%l%c%cps%snowdp
    t_grnd => clm3%g%l%c%ces%t_grnd
    hc_soisno => clm3%g%l%c%ces%hc_soisno
    errsoi => clm3%g%l%c%cebal%errsoi
    qmelt => clm3%g%l%c%cwf%qmelt
    pcolumn => clm3%g%l%c%p%column
    pgridcell => clm3%g%l%c%p%gridcell
    sabg => clm3%g%l%c%p%pef%sabg
    t_ref2m => clm3%g%l%c%p%pes%t_ref2m
    q_ref2m => clm3%g%l%c%p%pes%q_ref2m
    rh_ref2m => clm3%g%l%c%p%pes%rh_ref2m
    t_veg => clm3%g%l%c%p%pes%t_veg
    eflx_lwrad_out => clm3%g%l%c%p%pef%eflx_lwrad_out
    eflx_lwrad_net => clm3%g%l%c%p%pef%eflx_lwrad_net
    eflx_soil_grnd => clm3%g%l%c%p%pef%eflx_soil_grnd
    eflx_lh_tot => clm3%g%l%c%p%pef%eflx_lh_tot
    eflx_lh_grnd => clm3%g%l%c%p%pef%eflx_lh_grnd
    eflx_sh_grnd => clm3%g%l%c%p%pef%eflx_sh_grnd
    eflx_sh_tot => clm3%g%l%c%p%pef%eflx_sh_tot
    ram1 => clm3%g%l%c%p%pps%ram1
    taux => clm3%g%l%c%p%pmf%taux
    tauy => clm3%g%l%c%p%pmf%tauy
    qflx_prec_grnd => clm3%g%l%c%p%pwf%qflx_prec_grnd
    qflx_evap_soi => clm3%g%l%c%p%pwf%qflx_evap_soi
    qflx_evap_tot => clm3%g%l%c%p%pwf%qflx_evap_tot
    forc_hgt_u_pft => clm3%g%l%c%p%pps%forc_hgt_u_pft
    forc_hgt_t_pft => clm3%g%l%c%p%pps%forc_hgt_t_pft
    forc_hgt_q_pft => clm3%g%l%c%p%pps%forc_hgt_q_pft
    qflx_snwcp_ice => clm3%g%l%c%p%pwf%qflx_snwcp_ice
    qflx_snwcp_liq => clm3%g%l%c%p%pwf%qflx_snwcp_liq
    do fc = 1, num_lakec
       c = filter_lakec(fc)
       g = cgridcell(c)
       ocvts(c) = 0._r8
       ncvts(c) = 0._r8
       hc_soisno(c) = 0._r8
       dzsur(c) = dz(c,1) + snowdp(c)
       call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))
       thv(c) = forc_th(g)*(1._r8+0.61_r8*forc_q(g))
    end do
    do fp = 1, num_lakep
       p = filter_lakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)
       nmozsgn(p) = 0
       obuold(p) = 0._r8
       displa(p) = 0._r8
       thm(p) = forc_t(g) + 0.0098_r8*forc_hgt_t_pft(p)
       if (t_grnd(c) >= tfrz) then
          z0mg(p) = 0.01_r8
       else
          z0mg(p) = 0.04_r8
       end if
       z0hg(p) = z0mg(p)
       z0qg(p) = z0mg(p)
       if (forc_t(g) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if
       ur(p) = max(1.0_r8,sqrt(forc_u(g)*forc_u(g)+forc_v(g)*forc_v(g)))
       dth(p) = thm(p)-t_grnd(c)
       dqh(p) = forc_q(g)-qsatg(c)
       dthv = dth(p)*(1._r8+0.61_r8*forc_q(g))+0.61_r8*forc_th(g)*dqh(p)
       zldis(p) = forc_hgt_u_pft(p) - 0._r8
       call MoninObukIni(ur(p), thv(c), dthv, zldis(p), z0mg(p), um(p), obu(p))
    end do
    iter = 1
    fncopy = num_lakep
    fpcopy(1:num_lakep) = filter_lakep(1:num_lakep)
    ITERATION : do while (iter <= niters .and. fncopy > 0)
       call FrictionVelocity(lbp, ubp, fncopy, fpcopy, &
                             displa, z0mg, z0hg, z0qg, &
                             obu, iter, ur, um, ustar, &
                             temp1, temp2, temp12m, temp22m, fm)
       do fp = 1, fncopy
          p = fpcopy(fp)
          c = pcolumn(p)
          g = pgridcell(p)
          tgbef(c) = t_grnd(c)
          if (t_grnd(c) > tfrz) then
             tksur = tkwat
          else
             tksur = tkice
          end if
          ram(p) = 1._r8/(ustar(p)*ustar(p)/um(p))
          rah(p) = 1._r8/(temp1(p)*ustar(p))
          raw(p) = 1._r8/(temp2(p)*ustar(p))
          ram1(p) = ram(p)
          stftg3(p) = emg*sb*tgbef(c)*tgbef(c)*tgbef(c)
          ax = sabg(p) + emg*forc_lwrad(g) + 3._r8*stftg3(p)*tgbef(c) &
               + forc_rho(g)*cpair/rah(p)*thm(p) &
               - htvp(c)*forc_rho(g)/raw(p)*(qsatg(c)-qsatgdT(c)*tgbef(c) - forc_q(g)) &
               + tksur*t_lake(c,1)/dzsur(c)
          bx = 4._r8*stftg3(p) + forc_rho(g)*cpair/rah(p) &
               + htvp(c)*forc_rho(g)/raw(p)*qsatgdT(c) + tksur/dzsur(c)
          t_grnd(c) = ax/bx
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(p))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-tgbef(c))-forc_q(g))/raw(p)
          call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))
          dth(p)=thm(p)-t_grnd(c)
          dqh(p)=forc_q(g)-qsatg(c)
          tstar = temp1(p)*dth(p)
          qstar = temp2(p)*dqh(p)
          thvstar=tstar*(1._r8+0.61_r8*forc_q(g)) + 0.61_r8*forc_th(g)*qstar
          zeta=zldis(p)*vkc * grav*thvstar/(ustar(p)**2*thv(c))
          if (zeta >= 0._r8) then
             zeta = min(2._r8,max(zeta,0.01_r8))
             um(p) = max(ur(p),0.1_r8)
          else
             zeta = max(-100._r8,min(zeta,-0.01_r8))
             wc = beta1*(-grav*ustar(p)*thvstar*zii/thv(c))**0.333_r8
             um(p) = sqrt(ur(p)*ur(p)+wc*wc)
          end if
          obu(p) = zldis(p)/zeta
          if (obuold(p)*obu(p) < 0._r8) nmozsgn(p) = nmozsgn(p)+1
          obuold(p) = obu(p)
       end do
       iter = iter + 1
       if (iter <= niters ) then
          fnold = fncopy
          fncopy = 0
          do fp = 1, fnold
             p = fpcopy(fp)
             if (nmozsgn(p) < 3) then
                fncopy = fncopy + 1
                fpcopy(fncopy) = p
             end if
          end do
       end if
    end do ITERATION
    do fp = 1, num_lakep
       p = filter_lakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)
       qflx_snwcp_ice(p) = 0._r8
       qflx_snwcp_liq(p) = 0._r8
       if (h2osno(c) > 0.5_r8 .AND. t_grnd(c) > tfrz) then
          t_grnd(c) = tfrz
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(p))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-tgbef(c)) - forc_q(g))/raw(p)
       end if
       eflx_lwrad_out(p) = (1._r8-emg)*forc_lwrad(g) + stftg3(p)*(-3._r8*tgbef(c)+4._r8*t_grnd(c))
       eflx_soil_grnd(p) = sabg(p) + forc_lwrad(g) - eflx_lwrad_out(p) - &
            eflx_sh_grnd(p) - htvp(c)*qflx_evap_soi(p)
       taux(p) = -forc_rho(g)*forc_u(g)/ram(p)
       tauy(p) = -forc_rho(g)*forc_v(g)/ram(p)
       eflx_sh_tot(p) = eflx_sh_grnd(p)
       qflx_evap_tot(p) = qflx_evap_soi(p)
       eflx_lh_tot(p) = htvp(c)*qflx_evap_soi(p)
       eflx_lh_grnd(p) = htvp(c)*qflx_evap_soi(p)
       t_ref2m(p) = thm(p) + temp1(p)*dth(p)*(1._r8/temp12m(p) - 1._r8/temp1(p))
       q_ref2m(p) = forc_q(g) + temp2(p)*dqh(p)*(1._r8/temp22m(p) - 1._r8/temp2(p))
       call QSat(t_ref2m(p), forc_pbot(g), e_ref2m, de2mdT, qsat_ref2m, dqsat2mdT)
       rh_ref2m(p) = min(100._r8, q_ref2m(p) / qsat_ref2m * 100._r8)
       if (h2osno(c) > 0._r8 .AND. t_grnd(c) >= tfrz) then
          hm = min(h2osno(c)*hfus/dtime, max(eflx_soil_grnd(p),0._r8))
       else
          hm = 0._r8
       end if
       qmelt(c) = hm/hfus
       fin(c) = beta(idlak) * sabg(p) + forc_lwrad(g) - (eflx_lwrad_out(p) + &
            eflx_sh_tot(p) + eflx_lh_tot(p) + hm)
       u2m = max(1.0_r8,ustar(p)/vkc*log(2._r8/z0mg(p)))
       ws(c) = 1.2e-03_r8 * u2m
       ks(c) = 6.6_r8*sqrt(abs(sin(lat(g))))*(u2m**(-1.84_r8))
    end do
    cwat = cpliq*denh2o
    km = tkwat/cwat
    do j = 1, nlevlak
       do fc = 1, num_lakec
          c = filter_lakec(fc)
          rhow(c,j) = 1000._r8*( 1.0_r8 - 1.9549e-05_r8*(abs(t_lake(c,j)-277._r8))**1.68_r8 )
       end do
    end do
    do j = 1, nlevlak-1
       do fc = 1, num_lakec
          c = filter_lakec(fc)
          drhodz = (rhow(c,j+1)-rhow(c,j)) / (z(c,j+1)-z(c,j))
          n2 = -grav / rhow(c,j) * drhodz
          num = 40._r8 * n2 * (vkc*z(c,j))**2
          den = max( (ws(c)**2) * exp(-2._r8*ks(c)*z(c,j)), 1.e-10_r8 )
          ri = ( -1._r8 + sqrt( max(1._r8+num/den, 0._r8) ) ) / 20._r8
          if (t_grnd(c) > tfrz) then
             ke = vkc*ws(c)*z(c,j)/p0 * exp(-ks(c)*z(c,j)) / (1._r8+37._r8*ri*ri)
          else
             ke = 0._r8
          end if
          kme(c,j) = km + ke
       end do
    end do
    do fc = 1, num_lakec
       c = filter_lakec(fc)
       kme(c,nlevlak) = kme(c,nlevlak-1)
       jtop(c) = 1
    end do
    do j = 1, nlevlak
       do fp = 1, num_lakep
          p = filter_lakep(fp)
          c = pcolumn(p)
          zin = z(c,j) - 0.5_r8*dz(c,j)
          zout = z(c,j) + 0.5_r8*dz(c,j)
          in = exp( -eta(idlak)*max( zin-za(idlak),0._r8 ) )
          out = exp( -eta(idlak)*max( zout-za(idlak),0._r8 ) )
          if (j == nlevlak) out = 0._r8
          if (t_grnd(c) > tfrz) then
             phidum = (in-out) * sabg(p) * (1._r8-beta(idlak))
          else if (j == 1) then
             phidum = sabg(p) * (1._r8-beta(idlak))
          else
             phidum = 0._r8
          end if
          phi(c,j) = phidum
       end do
    end do
    do j = 1, nlevlak
       do fc = 1, num_lakec
          c = filter_lakec(fc)
          ocvts(c) = ocvts(c) + cwat*t_lake(c,j)*dz(c,j)
       end do
    end do
    do fc = 1, num_lakec
       c = filter_lakec(fc)
       j = 1
       m2 = dz(c,j)/kme(c,j) + dz(c,j+1)/kme(c,j+1)
       m3 = dtime/dz(c,j)
       r(c,j) = t_lake(c,j) + (fin(c)+phi(c,j))*m3/cwat - (t_lake(c,j)-t_lake(c,j+1))*m3/m2
       a(c,j) = 0._r8
       b(c,j) = 1._r8 + m3/m2
       c1(c,j) = -m3/m2
       j = nlevlak
       m1 = dz(c,j-1)/kme(c,j-1) + dz(c,j)/kme(c,j)
       m3 = dtime/dz(c,j)
       r(c,j) = t_lake(c,j) + phi(c,j)*m3/cwat + (t_lake(c,j-1)-t_lake(c,j))*m3/m1
       a(c,j) = -m3/m1
       b(c,j) = 1._r8 + m3/m1
       c1(c,j) = 0._r8
    end do
    do j = 2, nlevlak-1
       do fc = 1, num_lakec
          c = filter_lakec(fc)
          m1 = dz(c,j-1)/kme(c,j-1) + dz(c,j )/kme(c,j )
          m2 = dz(c,j )/kme(c,j ) + dz(c,j+1)/kme(c,j+1)
          m3 = dtime/dz(c,j)
          r(c,j) = t_lake(c,j) + phi(c,j)*m3/cwat + &
             (t_lake(c,j-1) - t_lake(c,j ))*m3/m1 - &
             (t_lake(c,j ) - t_lake(c,j+1))*m3/m2
          a(c,j) = -m3/m1
          b(c,j) = 1._r8 + m3/m1 + m3/m2
          c1(c,j) = -m3/m2
       end do
    end do
    call Tridiagonal(lbc, ubc, 1, nlevlak, jtop, num_lakec, filter_lakec, &
                     a, b, c1, r, t_lake(lbc:ubc,1:nlevlak))
    num_unfrzc = 0
    do fc = 1, num_lakec
       c = filter_lakec(fc)
       if (t_grnd(c) > tfrz) then
          num_unfrzc = num_unfrzc + 1
          filter_unfrzc(num_unfrzc) = c
       end if
    end do
    do j = 1, nlevlak-1
       do fc = 1, num_unfrzc
          c = filter_unfrzc(fc)
          tav(c) = 0._r8
          nav(c) = 0._r8
       end do
       do i = 1, j+1
          do fc = 1, num_unfrzc
             c = filter_unfrzc(fc)
             if (rhow(c,j) > rhow(c,j+1)) then
                tav(c) = tav(c) + t_lake(c,i)*dz(c,i)
                nav(c) = nav(c) + dz(c,i)
             end if
          end do
       end do
       do fc = 1, num_unfrzc
          c = filter_unfrzc(fc)
          if (rhow(c,j) > rhow(c,j+1)) then
             tav(c) = tav(c)/nav(c)
          end if
       end do
       do i = 1, j+1
          do fc = 1, num_unfrzc
             c = filter_unfrzc(fc)
             if (nav(c) > 0._r8) then
                t_lake(c,i) = tav(c)
                rhow(c,i) = 1000._r8*( 1.0_r8 - 1.9549e-05_r8*(abs(t_lake(c,i)-277._r8))**1.68_r8 )
             end if
          end do
       end do
    end do
    do j = 1, nlevlak
       do fc = 1, num_lakec
          c = filter_lakec(fc)
          ncvts(c) = ncvts(c) + cwat*t_lake(c,j)*dz(c,j)
          hc_soisno(c) = hc_soisno(c) + cwat*t_lake(c,j)*dz(c,j) /1.e6_r8
          if (j == nlevlak) then
             hc_soisno(c) = hc_soisno(c) + &
                            cpice*h2osno(c)*t_grnd(c)*snowdp(c) /1.e6_r8
          endif
          fin(c) = fin(c) + phi(c,j)
       end do
    end do
    do fp = 1, num_lakep
       p = filter_lakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)
       errsoi(c) = (ncvts(c)-ocvts(c)) / dtime - fin(c)
       t_veg(p) = forc_t(g)
       eflx_lwrad_net(p) = eflx_lwrad_out(p) - forc_lwrad(g)
       qflx_prec_grnd(p) = forc_rain(g) + forc_snow(g)
    end do
  end subroutine BiogeophysicsLake
end module BiogeophysicsLakeMod
module CNAllocationMod
end module CNAllocationMod
module CNAnnualUpdateMod
end module CNAnnualUpdateMod
module CNBalanceCheckMod
end module CNBalanceCheckMod
module CNCStateUpdate1Mod
end module CNCStateUpdate1Mod
module CNCStateUpdate2Mod
end module CNCStateUpdate2Mod
module CNCStateUpdate3Mod
end module CNCStateUpdate3Mod
module CNDecompMod
end module CNDecompMod
module CNSetValueMod
end module CNSetValueMod
module CNFireMod
end module CNFireMod
module CNGRespMod
end module CNGRespMod
module CNGapMortalityMod
end module CNGapMortalityMod
module CNMRespMod
end module CNMRespMod
module CNNDynamicsMod
end module CNNDynamicsMod
module CNNStateUpdate1Mod
end module CNNStateUpdate1Mod
module CNNStateUpdate2Mod
end module CNNStateUpdate2Mod
module CNNStateUpdate3Mod
end module CNNStateUpdate3Mod
module CNPhenologyMod
end module CNPhenologyMod
module CNPrecisionControlMod
end module CNPrecisionControlMod
module CNSummaryMod
end module CNSummaryMod
module CNVegStructUpdateMod
end module CNVegStructUpdateMod
module CNWoodProductsMod
end module CNWoodProductsMod
subroutine CNiniSpecial ()
end subroutine CNiniSpecial
subroutine CNiniTimeVar(htmx_buf,croplive_buf,gdd1020_buf,gdd820_buf,gdd020_buf,grainc_buf,grainc_storage_buf &
                ,grainc_xfer_buf,grainn_buf,grainn_storage_buf,grainn_xfer_buf,days_active_buf &
                ,onset_flag_buf,onset_counter_buf,onset_gddflag_buf,onset_fdd_buf,onset_gdd_buf &
                ,onset_swi_buf,offset_flag_buf,offset_counter_buf,offset_fdd_buf,offset_swi_buf &
                ,dayl_buf,annavg_t2m_buf,tempavg_t2m_buf,tempsum_potential_gpp_buf &
                ,annsum_potential_gpp_buf,tempmax_retransn_buf,annmax_retransn_buf &
                ,prev_leafc_to_litter_buf,prev_frootc_to_litter_buf,tempsum_npp_buf &
                ,annsum_npp_buf,leafc_buf,leafc_storage_buf,leafc_xfer_buf,frootc_buf &
                ,frootc_storage_buf,frootc_xfer_buf,livestemc_buf,livestemc_storage_buf &
                ,livestemc_xfer_buf,deadstemc_buf,deadstemc_storage_buf,deadstemc_xfer_buf &
                ,livecrootc_buf,livecrootc_storage_buf,livecrootc_xfer_buf,deadcrootc_buf &
                ,deadcrootc_storage_buf,deadcrootc_xfer_buf,cpool_buf,pft_ctrunc_buf &
                ,leafn_buf,leafn_storage_buf,leafn_xfer_buf,frootn_buf,frootn_storage_buf &
                ,frootn_xfer_buf,livestemn_buf,livestemn_storage_buf,livestemn_xfer_buf &
                ,deadstemn_buf,deadstemn_storage_buf,deadstemn_xfer_buf,livecrootn_buf &
                ,livecrootn_storage_buf,livecrootn_xfer_buf,deadcrootn_buf &
                ,deadcrootn_storage_buf,deadcrootn_xfer_buf,npool_buf,pft_ntrunc_buf &
                ,gresp_storage_buf,gresp_xfer_buf,xsmrpool_buf,annsum_counter_buf &
                ,cannsum_npp_buf,cannavg_t2m_buf,wf_buf,me_buf,mean_fire_prob_buf,cwdc_buf,litr1c_buf &
                ,litr2c_buf,litr3c_buf,soil1c_buf,soil2c_buf,soil3c_buf,soil4c_buf,seedc_buf,col_ctrunc_buf &
                ,prod10c_buf,prod100c_buf,cwdn_buf,litr1n_buf,litr2n_buf,litr3n_buf,soil1n_buf,soil2n_buf &
                ,soil3n_buf,soil4n_buf,seedn_buf,col_ntrunc_buf,prod10n_buf,prod100n_buf,sminn_buf &
                ,totlitc_buf,dwt_seedc_to_leaf_buf,dwt_seedc_to_deadstem_buf,dwt_conv_cflux_buf &
                ,dwt_prod10c_gain_buf,dwt_prod100c_gain_buf,prod100c_loss_buf,dwt_frootc_to_litr1c_buf &
                ,dwt_frootc_to_litr2c_buf,dwt_frootc_to_litr3c_buf,dwt_livecrootc_to_cwdc_buf &
                ,dwt_deadcrootc_to_cwdc_buf,dwt_seedn_to_leaf_buf,dwt_seedn_to_deadstem_buf &
                ,dwt_conv_nflux_buf,dwt_prod10n_gain_buf,dwt_prod100n_gain_buf,prod100n_loss_buf &
                ,dwt_frootn_to_litr1n_buf,dwt_frootn_to_litr2n_buf, dwt_frootn_to_litr3n_buf &
                , dwt_livecrootn_to_cwdn_buf,dwt_deadcrootn_to_cwdn_buf,retransn_buf &
                        )
end subroutine CNiniTimeVar
module CNEcosystemDynMod
end module CNEcosystemDynMod
subroutine iniTimeVar(snlx ,snowdpx ,dzclmx ,zclmx ,&
                     ziclmx ,h2osnox ,h2osoi_liqx,h2osoi_icex,t_grndx,&
                     t_soisnox ,t_lakex ,t_vegx ,h2ocanx ,h2ocan_colx,&
                     h2osoi_volx,declin,t_ref2mx,xlat,xlon)
  use shr_kind_mod , only : r8 => shr_kind_r8
  use clmtype
  use decompMod , only : get_proc_bounds
  use filterMod , only : filter
  use clm_varpar , only : nlevsoi,nlevgrnd, nlevsno, nlevlak,maxpatch
  use clm_varcon , only : denice, denh2o, zlnd,istsoil,isturb
  use FracWetMod , only : FracWet
  use SurfaceAlbedoMod , only : SurfaceAlbedo
  use globals , only : month, day, calday
  use STATICEcosysDynMod, only : EcosystemDyn, interpMonthlyVeg
  use shr_const_mod, only : SHR_CONST_PI
  implicit none
    real(r8) :: xlon
    real(r8) :: xlat
    integer :: snlx(maxpatch)
    real(r8) :: snowdpx(maxpatch)
    real(r8) :: h2osnox(maxpatch)
    real(r8) :: t_grndx(maxpatch)
    real(r8) :: t_vegx(maxpatch)
    real(r8) :: h2ocanx(maxpatch)
    real(r8) :: h2ocan_colx(maxpatch)
    real(r8) :: t_ref2mx(maxpatch)
    real(r8) :: t_lakex(maxpatch,nlevlak)
    real(r8) :: t_soisnox(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_liqx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_icex(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: dzclmx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: zclmx(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: ziclmx(maxpatch,-nlevsno:nlevgrnd)
    real(r8) :: h2osoi_volx(maxpatch,nlevgrnd)
  integer , pointer :: plandunit(:)
  logical , pointer :: lakpoi(:)
  real(r8), pointer :: dz(:,:)
  real(r8), pointer :: h2osoi_ice(:,:)
  real(r8), pointer :: h2osoi_liq(:,:)
  integer , pointer :: frac_veg_nosno_alb(:)
  real(r8), pointer :: h2osoi_vol(:,:)
  real(r8), pointer :: snowdp(:)
  real(r8), pointer :: frac_sno(:)
  integer , pointer :: frac_veg_nosno(:)
  real(r8), pointer :: fwet(:)
  real(r8), pointer :: fdry(:)
  real(r8), pointer :: tlai(:)
  real(r8), pointer :: tsai(:)
  real(r8), pointer :: htop(:)
  real(r8), pointer :: hbot(:)
  real(r8), pointer :: elai(:)
  real(r8), pointer :: esai(:)
  real(r8) :: declin
    real(r8):: snowbd
    real(r8):: fmelt
   integer , pointer :: clandunit(:)
    integer , pointer :: itypelun(:)
  integer :: g,nc,j,l,c,p,fp,fc
  integer :: begp, endp
  integer :: begc, endc
  integer :: begl, endl
  integer :: begg, endg
  lakpoi => clm3%g%l%lakpoi
  itypelun => clm3%g%l%itype
  dz => clm3%g%l%c%cps%dz
  h2osoi_ice => clm3%g%l%c%cws%h2osoi_ice
  h2osoi_liq => clm3%g%l%c%cws%h2osoi_liq
  h2osoi_vol => clm3%g%l%c%cws%h2osoi_vol
  snowdp => clm3%g%l%c%cps%snowdp
  frac_sno => clm3%g%l%c%cps%frac_sno
    clandunit => clm3%g%l%c%landunit
  plandunit => clm3%g%l%c%p%landunit
  frac_veg_nosno_alb => clm3%g%l%c%p%pps%frac_veg_nosno_alb
  frac_veg_nosno => clm3%g%l%c%p%pps%frac_veg_nosno
  fwet => clm3%g%l%c%p%pps%fwet
  htop => clm3%g%l%c%p%pps%htop
  hbot => clm3%g%l%c%p%pps%hbot
  tlai => clm3%g%l%c%p%pps%tlai
  tsai => clm3%g%l%c%p%pps%tsai
  elai => clm3%g%l%c%p%pps%elai
  esai => clm3%g%l%c%p%pps%esai
  fdry => clm3%g%l%c%p%pps%fdry
  call CLMDebug('iniTimeVar mark0')
  call CLMDebug('call interpMonthlyVeg')
  call interpMonthlyVeg(month, day)
     call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
    do g = begg, endg
       clm3%g%lat_a(g) = xlat*(SHR_CONST_PI/180._r8)
       clm3%g%lon_a(g) = xlon*(SHR_CONST_PI/180._r8)
    end do
!dir$ concurrent
   do p = begp,endp
        l = plandunit(p)
        if (lakpoi(l)) then
           fwet(p) = 0.
           fdry(p) = 0.
           elai(p) = 0.
           esai(p) = 0.
           htop(p) = 0.
           hbot(p) = 0.
           tlai(p) = 0.
           tsai(p) = 0.
           frac_veg_nosno_alb(p) = 0.
           frac_veg_nosno(p) = 0.
        end if
     end do
    call CLMDebug('iniTimeVar mark1')
     call EcosystemDyn(begp, endp, filter%num_nolakep, filter%nolakep, &
                       doalb=.true.)
!dir$ concurrent
     do p = begp, endp
        l = plandunit(p)
        if (.not. lakpoi(l)) then
           frac_veg_nosno(p) = frac_veg_nosno_alb(p)
           fwet(p) = 0.
        end if
     end do
     call CLMDebug('call FracWet')
     call FracWet(filter%num_nolakep, filter%nolakep)
!dir$ concurrent
       do c = begc, endc
           snowdp(c) = snowdpx(c)
          l = clandunit(c)
          if (itypelun(l) == isturb) then
             frac_sno(c) = min( snowdp(c)/0.05_r8, 1._r8)
          else
             frac_sno(c) = 0._r8
             if(snowdp(c) .gt. 0.0 .and. h2osnox(c) .gt. 0.0) then
                snowbd = min(800._r8,h2osnox(c)/snowdp(c))
                fmelt = (snowbd/100.)**1.
                frac_sno(c) = tanh( snowdp(c) /(2.5 * zlnd * fmelt) )
             endif
          end if
       end do
     call SurfaceAlbedo(begg, endg, begc, endc, begp, endp,filter%num_nourbanc, filter%nourbanc, &
                           filter%num_nourbanp, filter%nourbanp, calday,declin)
end subroutine iniTimeVar
module initializeMod
  implicit none
  save
  public :: initialize
contains
  subroutine initialize(snl ,snowdp ,dzclm ,zclm &
                  ,ziclm ,h2osno ,h2osoi_liq,h2osoi_ice,t_grnd &
                  ,t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col &
                  ,h2osoi_vol ,xlat ,xlon ,areaxy ,iveg &
                  ,isl ,lndmsk &
                  ,t_ref2m ,ilx ,jlx,calday,declin,declinp1 &
                  , organicxy, efisopxy,gtixy ,snw_rdsx &
                                                         )
    use shr_kind_mod , only : r8 => shr_kind_r8
    use clmtypeInitMod , only : initClmtype
    use initGridCellsMod, only : initGridCells
    use clm_varpar , only : lsmlon, lsmlat, maxpatch,nlevgrnd,nlevsno,&
                                 nlevlak
    use clm_varsur , only : varsur_alloc, longxy,latixy,&
                                 area
    use filterMod , only : filter,allocFilters,setFilters
    use decompMod , only : initDecomp
    use surfFileMod , only : surfrd
    use pftvarcon , only : pftconrd
    use decompMod , only: get_proc_bounds
    use STATICEcosysDynMod , only : EcosystemDynini
 use clm_varcon , only : var_par
 use aerdepMOD , only : aerdepini
    implicit none
    integer :: i,j,k
    integer :: yr
    integer :: mon
    integer :: day
    integer :: ncsec
    logical :: readini
    integer :: ier
    real(r8),intent(in) :: gtixy
    real(r8), intent(in) :: calday
    real(r8), intent(in) :: declin
    real(r8), intent(in), optional :: declinp1
    real(r8) :: organicxy(maxpatch)
    real(r8) :: efisopxy(6)
    integer :: ilx,jlx
    integer :: begc,endc
    integer :: snl(maxpatch)
    real(r8) :: snowdp(maxpatch)
    real(r8) :: h2osno(maxpatch)
    real(r8) :: t_grnd(maxpatch)
    real(r8) :: t_veg(maxpatch)
    real(r8) :: h2ocan(maxpatch)
    real(r8) :: h2ocan_col(maxpatch)
    real(r8) :: t_lake(maxpatch,nlevlak)
    real(r8) :: t_soisno(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_liq(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: h2osoi_ice(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: dzclm(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: zclm(maxpatch,-nlevsno+1:nlevgrnd)
    real(r8) :: ziclm(maxpatch,-nlevsno:nlevgrnd)
    real(r8) :: h2osoi_vol(maxpatch,nlevgrnd)
    real(r8) :: snw_rdsx(maxpatch,-nlevsno+1:0)
    real(r8) :: xlon
    real(r8) :: xlat
    real(r8) :: areaxy
    integer :: iveg
    integer :: isl
    integer :: lndmsk
    real(r8) :: t_ref2m(maxpatch)
    call CLMDebug('Now in Initialize. Next call varsur_alloc.')
           longxy(1) = xlon
           latixy(1) = xlat
           area(1) = areaxy
     call varsur_alloc ()
     call CLMDebug('call pftconrd')
     call pftconrd ()
     call var_par()
     call CLMDebug('call surfrd')
     call surfrd (organicxy,efisopxy,gtixy,ilx,jlx, iveg, isl, lndmsk)
    call CLMDebug('call initDecomp')
    call initDecomp()
    call CLMDebug('initClmtype')
    call initClmtype()
    call CLMDebug('call initGridCells')
    call initGridCells()
    call CLMDebug('call allocFilters')
    call allocFilters()
    call CLMDebug('call setFilters')
    call setFilters()
    call CLMDebug('call iniTimeConst')
    call iniTimeConst()
    call mkarbinit(snl ,snowdp, dzclm ,zclm ,&
                  ziclm ,h2osno ,h2osoi_liq,h2osoi_ice,t_grnd,&
                  t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col,&
                  h2osoi_vol,t_ref2m ,snw_rdsx &
                 )
    call CLMDebug('init_ecosys')
    call EcosystemDynini()
     call aerdepini()
    call CLMDebug('call iniTimeVar')
    call iniTimeVar(snl ,snowdp ,dzclm ,zclm ,&
                   ziclm ,h2osno ,h2osoi_liq,h2osoi_ice,t_grnd ,&
                   t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col,&
                   h2osoi_vol,declin,t_ref2m,xlat,xlon)
  end subroutine initialize
end module initializeMod
    subroutine clm(forc_txy ,forc_uxy ,forc_vxy &
                  ,forc_qxy ,zgcmxy ,precxy &
                  ,flwdsxy ,forc_solsxy ,forc_sollxy &
                  ,forc_solsdxy ,forc_solldxy ,forc_pbotxy &
                  ,forc_psrfxy ,iveg ,isl &
                  ,lndmsk ,xlat ,xlon &
                  ,areaxy ,dt1 ,yr &
                  ,mnth ,dy ,nsec &
                  ,cxday ,yr1 ,mnp1 &
                  ,dyp1 ,nsec1 ,cxday1 &
                  ,mbdate ,qsfxy ,qdnxy &
                  ,snl ,snowdp ,snw_rdsxy &
                  ,dzclm ,zclm ,ziclm &
                  ,h2osno ,h2osoi_liq ,h2osoi_ice &
                  ,t_grnd ,t_soisno ,t_lake &
                  ,t_veg ,h2ocan ,h2ocan_col &
                  ,h2osoi_vol ,wtc ,wtp &
                  ,numc ,nump &
                  ,t_ref2m ,albxy , tsxy, trefxy &
                  ,shxy ,lhxy ,nstp &
                  ,inest ,ilx ,jlx &
                  ,soiflx ,sabv ,sabg &
                  ,lwupxy ,znt0 ,q_ref2m &
                  ,rhoxy &
                  ,ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid &
                  ,Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid &
                  ,SWUPsubgrid ,LHsoi,LHveg,LHtran,organicxy,efisopxy,gtixy &
                  ,alswnirdir ,alswnirdif, alswvisdir,alswvisdif &
                                                            )
  use shr_kind_mod , only : r8 => shr_kind_r8
  use clm_varpar, only : nlevgrnd,nlevsoi,numrad,maxpatch,&
                         nlevsno,nlevlak,lsmlon,lsmlat
  use initializeMod
  use nanMod
  use clmtype
  use clm_varcon , only : rair, cpair, po2, pco2, tcrit,tfrz,pstd,sb
  use globals
  use decompMod , only : get_proc_bounds
  use clmtypeInitMod
  use shr_orb_mod
  use shr_const_mod, only : SHR_CONST_PI
  use filterMod, only : filters_dealloc
  use clm_varsur , only :varsur_dealloc
  implicit none
  save
  real(r8) :: gtixy
  real(r8) :: forc_txy
  real(r8) :: forc_uxy
  real(r8) :: forc_vxy
  real(r8) :: forc_qxy
  real(r8) :: zgcmxy
  real(r8) :: precxy
  real(r8) :: flwdsxy
  real(r8) :: forc_solsxy
  real(r8) :: forc_sollxy
  real(r8) :: forc_solsdxy
  real(r8) :: forc_solldxy
  real(r8) :: forc_pbotxy
  real(r8) :: forc_psrfxy
  real(r8) :: forc_ndepxy
 real(r8) :: alswnirdir ,alswnirdif, alswvisdir,alswvisdif
 real(r8) :: swdall
    integer :: i,j,k,g,p,c
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    integer :: begg, endg
    type(gridcell_type), pointer :: gptr
  integer :: snl(maxpatch)
  real(r8) :: snowdp(maxpatch)
  real(r8) :: h2osno(maxpatch)
  real(r8) :: t_grnd(maxpatch)
  real(r8) :: t_veg(maxpatch)
  real(r8) :: h2ocan(maxpatch)
  real(r8) :: h2ocan_col(maxpatch)
  real(r8) :: wtc(maxpatch)
  real(r8) :: wtp(maxpatch)
  integer :: numc,nump
  real(r8) :: htop(maxpatch)
  real(r8) :: tsai(maxpatch)
    real(r8) :: efisopxy(6)
  real(r8) :: t_lake(maxpatch,nlevlak)
  real(r8),dimension(maxpatch,-nlevsno+1:nlevgrnd) :: t_soisno
  real(r8) :: h2osoi_liq(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: h2osoi_ice(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: dzclm(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: zclm(maxpatch,-nlevsno+1:nlevgrnd)
  real(r8) :: ziclm(maxpatch,-nlevsno:nlevgrnd)
  real(r8) :: h2osoi_vol(maxpatch,nlevgrnd)
  real(r8) :: snw_rdsxy(maxpatch,-nlevsno+1:0)
  real(r8) :: t_ref2m(maxpatch)
  real(r8), dimension(1:maxpatch), intent(out) :: ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid, &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,SWUPsubgrid,LHsoi,LHveg,LHtran
  real(r8) :: znt(maxpatch),organicxy(maxpatch)
  real(r8) :: q_ref2m(maxpatch)
  logical doalb
  real(r8) :: albxy,albixy(numrad),albdxy(numrad) &
              ,albedotemp(maxpatch, numrad)
  real(r8) :: trefxy,tsxy
  real(r8) :: shxy
  real(r8) :: lhxy
  real(r8) :: lwupxy
  real(r8) :: qsfxy
  real(r8) :: qdnxy
  real(r8) :: soiflx
  real(r8) :: sabv
  real(r8) :: sabg
  real(r8) :: znt0
  real(r8),intent(out) :: rhoxy
  integer :: nstp
 real(r8),dimension(1:numrad) :: cof_dir,cof_dif
  real(r8) :: areaxy
  real(r8) :: dt1
  real(r8) :: cxday
  real(r8) :: cxday1
  real(r8) :: xlat
  real(r8) :: xlon
  integer :: iveg
  integer :: isl
  integer :: lndmsk
  integer :: yr
  integer :: mnth
  integer :: dy
  integer :: nsec
  integer :: yr1
  integer :: mnp1
  integer :: dyp1
  integer :: nsec1
  integer :: mbdate
  integer :: inest
  integer :: ilx,jlx
  real(r8) :: t2m,dsq,dsqmin
  character*256 :: msg
   real(r8) :: eccen
   real(r8) :: obliq
   real(r8) :: mvelp
   integer :: orb_iyear_AD
   real(r8) :: obliqr
   real(r8) :: lambm0
   real(r8) :: mvelpp
   real(r8) :: declinp1
   real(r8) :: declin
   real(r8) :: eccf
       call CLMDebug('Starting clm3.F')
       msg= ''
       write(msg, *) 'At i,j = ', ilx, ', ', jlx, '.'
       call CLMDebug(msg)
       msg = ''
       write(msg, *) 't_grnd(1) = ', t_grnd(1), '.'
       call CLMDebug(msg)
       call clmtype_mod
       call globals_mod
       dtime = dt1
       dt = dt1
       year = yr
       month = mnth
       day = dy
       secs = nsec
       calday = cxday
       yrp1 = yr1
       monp1 = mnp1
       dayp1 = dyp1
       secp1 = nsec1
       caldayp1 = cxday1
       nbdate = mbdate
       nstep = nstp
       if(mod(year,4)==0) then
          day_per_year = 366
       else
          day_per_year = 365
       end if
       orb_iyear_AD = 1990
     swdall = forc_sollxy+forc_solsxy+forc_solsdxy+forc_solldxy
    if(swdall.ne. 0) then
      cof_dir(2) = forc_sollxy/swdall
      cof_dif(2) = forc_solldxy/swdall
      cof_dir(1) = forc_solsxy/swdall
      cof_dif(1) = forc_solsdxy/swdall
    else
      cof_dir(2) = 0.35
      cof_dif(2) = 0.15
      cof_dir(1) = 0.35
      cof_dif(1) = 0.15
   end if
    call CLMDebug('Start shr_orb_params')
     call shr_orb_params(orb_iyear_AD, eccen, obliq, mvelp, &
                           obliqr, lambm0, mvelpp)
     call shr_orb_decl(calday, eccen, mvelpp, lambm0, obliqr, declin, eccf )
     call shr_orb_decl(caldayp1, eccen, mvelpp, lambm0, obliqr, declinp1, eccf )
    call CLMDebug('End shr_orb_params & decl')
    call CLMDebug('Start initialize()')
    call initialize(snl ,snowdp ,dzclm ,zclm &
                  ,ziclm ,h2osno ,h2osoi_liq,h2osoi_ice,t_grnd &
                  ,t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col &
                  ,h2osoi_vol ,xlat ,xlon ,areaxy ,iveg &
                  ,isl ,lndmsk &
                  ,t_ref2m ,ilx,jlx,calday,declin,declinp1&
                  ,organicxy, efisopxy,gtixy, snw_rdsxy &
                                                                      )
    call CLMDebug('initialize done. Back in clm3')
    call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
       gptr => clm3%g
       do g = begg, endg
       clm3%g%latdeg(g) = xlat
       clm3%g%londeg(g) = xlon
       clm3%g%lat(g) = xlat*(SHR_CONST_PI/180._r8)
       clm3%g%lon(g) = xlon*(SHR_CONST_PI/180._r8)
       clm3%g%latdeg_a(g) = xlat
       clm3%g%londeg_a(g) = xlon
       clm3%g%lat_a(g) = xlat*(SHR_CONST_PI/180._r8)
       clm3%g%lon_a(g) = xlon*(SHR_CONST_PI/180._r8)
          clm_a2l%forc_t(g) = forc_txy
          clm_a2l%forc_u(g) = forc_uxy
          clm_a2l%forc_v(g) = forc_vxy
          clm_a2l%forc_wind(g) = sqrt(forc_uxy**2 + forc_vxy**2)
          clm_a2l%forc_q(g) = forc_qxy
          clm_a2l%forc_hgt(g) = zgcmxy
          clm_a2l%forc_hgt_u(g) = zgcmxy
          clm_a2l%forc_hgt_t(g) = zgcmxy
          clm_a2l%forc_hgt_q(g) = zgcmxy
          clm_a2l%forc_pbot(g) = forc_pbotxy
          clm_a2l%forc_psrf(g) = forc_psrfxy
          clm_a2l%forc_th(g) = clm_a2l%forc_t(g) * (clm_a2l%forc_psrf(g) &
               / clm_a2l%forc_pbot(g))**(rair/cpair)
          clm_a2l%forc_vp(g) = clm_a2l%forc_q(g) * clm_a2l%forc_pbot(g) &
               / (0.622 + 0.378 * clm_a2l%forc_q(g))
          clm_a2l%forc_rho(g) = (clm_a2l%forc_pbot(g) - 0.378 * clm_a2l%forc_vp(g)) &
               / (rair * clm_a2l%forc_t(g))
          clm_a2l%forc_pco2(g) = pco2 * clm_a2l%forc_pbot(g)
          clm_a2l%forc_po2(g) = po2 * clm_a2l%forc_pbot(g)
          clm_a2l%forc_lwrad(g) = flwdsxy
          clm_a2l%forc_solad(g,1) = forc_solsxy
          clm_a2l%forc_solad(g,2) = forc_sollxy
          clm_a2l%forc_solai(g,1) = forc_solsdxy
          clm_a2l%forc_solai(g,2) = forc_solldxy
          clm_a2l%forc_solar(g) = forc_solsxy + forc_sollxy &
               + forc_solsdxy + forc_solldxy
          if (precxy > 0.) then
             if (clm_a2l%forc_t(g) > (tfrz + tcrit)) then
                clm_a2l%forc_rain(g) = precxy
                clm_a2l%forc_snow(g) = 0.
             else
                clm_a2l%forc_rain(g) = 0.
                clm_a2l%forc_snow(g) = precxy
                if (clm_a2l%forc_t(g) <= tfrz) then
                else if (clm_a2l%forc_t(g) <= tfrz+2.) then
                else
                endif
             endif
          else
             clm_a2l%forc_rain(g) = 0.
             clm_a2l%forc_snow(g) = 0.
          endif
          rhoxy = clm_a2l%forc_rho(g)
          clm_a2l%rainf(g) = clm_a2l%forc_rain(g)+clm_a2l%forc_snow(g)
       end do
     doalb = .true.
     call CLMDebug('Calling Driver')
     call driver (doalb,ilx,jlx,caldayp1, declinp1, declin)
     call CLMDebug('Driver done, back to clm3.F')
     call CLMDebug('biophy_to_wrf')
     call biophy_to_wrf(snl ,snowdp ,dzclm ,zclm ,&
                     ziclm ,h2osno ,h2osoi_liq ,h2osoi_ice ,t_grnd ,&
                     t_soisno ,t_lake ,t_veg ,h2ocan ,h2ocan_col ,&
                     h2osoi_vol ,wtc ,wtp ,numc ,nump ,&
                     htop ,tsai &
                     ,t_ref2m ,znt ,q_ref2m, snw_rdsxy)
      call CLMDebug('start accumulate in clm3.F')
    albxy = 0._r8
    do j = 1,numrad
       do p = begp,endp
          albxy =albxy + clm3%g%l%c%p%pps%albd(p,j)*wtp(p)*cof_dir(j) + clm3%g%l%c%p%pps%albi(p,j)*wtp(p)*cof_dif(j)
          albedosubgrid(p) = clm3%g%l%c%p%pps%albd(p,j)*cof_dir(j)+clm3%g%l%c%p%pps%albi(p,j)*cof_dif(j)
       end do
    end do
    msg = ''
    write(msg,*) 'Calculated albedo is ', albxy, '.'
    call CLMDebug(msg)
    lwupxy= 0._r8
    shxy = 0._r8
    lhxy = 0._r8
    soiflx= 0._r8
    sabv = 0._r8
    sabg = 0._r8
    trefxy = 0._r8
    tsxy = 0._r8
    znt0 = 0._r8
    alswvisdir = 0._r8
    alswvisdif = 0._r8
    alswnirdir = 0._r8
    alswnirdif = 0._r8
    do p = begp,endp
       lwupxy= lwupxy+ clm3%g%l%c%p%pef%eflx_lwrad_out(p)*wtp(p)
       shxy = shxy + clm3%g%l%c%p%pef%eflx_sh_tot(p)*wtp(p)
       lhxy = lhxy + clm3%g%l%c%p%pef%eflx_lh_tot(p)*wtp(p)
       soiflx= soiflx+ clm3%g%l%c%p%pef%eflx_soil_grnd(p)*wtp(p)
       sabv = sabv + clm3%g%l%c%p%pef%sabv(p)*wtp(p)
       sabg = sabg + clm3%g%l%c%p%pef%sabg(p)*wtp(p)
       tsxy = tsxy + clm3%g%l%c%p%pes%t_veg(p)*wtp(p)
       trefxy = trefxy + clm3%g%l%c%p%pes%t_ref2m(p)*wtp(p)
       znt0 = znt0 + znt(p)*wtp(p)
       alswvisdir = alswvisdir + clm3%g%l%c%p%pps%albd(p,1)*wtp(p)
       alswvisdif = alswvisdif + clm3%g%l%c%p%pps%albi(p,1)*wtp(p)
       alswnirdir = alswnirdir + clm3%g%l%c%p%pps%albd(p,2)*wtp(p)
       alswnirdif = alswnirdif + clm3%g%l%c%p%pps%albi(p,2)*wtp(p)
       if ( wtp(p) > 0.001 ) then
          lhsubgrid(p) = clm3%g%l%c%p%pef%eflx_lh_tot(p)
          hfxsubgrid(p) = clm3%g%l%c%p%pef%eflx_sh_tot(p)
          lwupsubgrid(p) = clm3%g%l%c%p%pef%eflx_lwrad_out(p)
          q2subgrid(p) = q_ref2m(p)
          sabvsubgrid(p) = clm3%g%l%c%p%pef%sabv(p)
          sabgsubgrid(p) = clm3%g%l%c%p%pef%sabg(p)
          nrasubgrid(p) = clm3%g%l%c%p%pef%fsa(p)
          swupsubgrid(p) = clm3%g%l%c%p%pef%fsr(p)
          lhsoi(p) = clm3%g%l%c%p%pef%eflx_lh_grnd(p)
          lhveg(p) = clm3%g%l%c%p%pef%eflx_lh_vege(p)
          lhtran(p) = clm3%g%l%c%p%pef%eflx_lh_vegt(p)
       endif
    end do
    msg = ''
    write(msg,*) 'LWUP is', lwupxy, '.'
    call CLMDebug(msg)
    qsfxy = 0._r8
    qdnxy = 0._r8
    do c = begc,endc
       qsfxy = qsfxy + clm3%g%l%c%cwf%qflx_surf(c)*wtc(c)*dtime
       qdnxy = qdnxy + clm3%g%l%c%cwf%qflx_drain(c)*wtc(c)*dtime
    end do
    call CLMDebug('call clmtype_dealloc')
    call clmtype_dealloc()
    call CLMDebug('call filters_dealloc')
    call filters_dealloc()
    call CLMDebug('entering varsurdealloc')
    call varsur_dealloc()
 call CLMDebug('done clm()')
     return
  end subroutine clm
subroutine driver (doalb,ilx,jlx,nextsw_cday, declinp1, declin)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use globals
  use clmtype
  use decompMod , only : get_proc_bounds
  use filterMod , only : filter
  use pftdynMod , only : pftdyn_interp, pftdyn_wbal_init, pftdyn_wbal
  use dynlandMod , only : dynland_hwcontent
  use clm_varcon , only : set_caerdep_from_file,set_dustdep_from_file,zlnd, isturb, fpftdyn, fndepdyn
  use DriverInitMod , only : DriverInit
  use BalanceCheckMod , only : BalanceCheck, BeginWaterBalance
  use SurfaceRadiationMod , only : SurfaceRadiation
  use Hydrology1Mod , only : Hydrology1
  use Hydrology2Mod , only : Hydrology2
  use HydrologyLakeMod , only : HydrologyLake
  use Biogeophysics1Mod , only : Biogeophysics1
  use BareGroundFluxesMod , only : BareGroundFluxes
  use CanopyFluxesMod , only : CanopyFluxes
  use Biogeophysics2Mod , only : Biogeophysics2
  use BiogeophysicsLakeMod, only : BiogeophysicsLake
  use SurfaceAlbedoMod , only : SurfaceAlbedo
  use pft2colMod , only : pft2col
  use STATICEcosysDynMod , only : EcosystemDyn, interpMonthlyVeg, EcosystemDyn_dealloc
  use VOCEmissionMod , only : VOCEmission
  use SNICARMod , only : SnowAge_grain
  use aerdepMod , only : interpMonthlyAerdep
  implicit none
  logical , intent(in) :: doalb
  integer , pointer :: clandunit(:)
  integer , pointer :: itypelun(:)
  integer :: ilx,jlx
  integer :: c,g,l
  integer :: ncdate
  integer :: kyr
  integer :: begp, endp
  integer :: begc, endc
  integer :: begl, endl
  integer :: begg, endg
  type(column_type) , pointer :: cptr
  real(r8), intent(in) :: nextsw_cday
  real(r8), intent(in) :: declinp1
  real(r8), intent(in) :: declin
  real(r8), pointer :: t_soisno(:,:)
      t_soisno => clm3%g%l%c%ces%t_soisno
  itypelun => clm3%g%l%itype
  clandunit => clm3%g%l%c%landunit
  cptr => clm3%g%l%c
  if (doalb) call interpMonthlyVeg (monp1,dayp1)
  if ( (set_caerdep_from_file) .or. (set_dustdep_from_file) ) then
     call interpMonthlyAerdep(monp1,dayp1)
  endif
     call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
     do g = begg,endg
        clm3%g%gwf%qflx_liq_dynbal(g) = 0._r8
        clm3%g%gws%gc_liq2(g) = 0._r8
        clm3%g%gws%gc_liq1(g) = 0._r8
        clm3%g%gwf%qflx_ice_dynbal(g) = 0._r8
        clm3%g%gws%gc_ice2(g) = 0._r8
        clm3%g%gws%gc_ice1(g) = 0._r8
        clm3%g%gef%eflx_dynbal(g) = 0._r8
        clm3%g%ges%gc_heat2(g) = 0._r8
        clm3%g%ges%gc_heat1(g) = 0._r8
     enddo
      call dynland_hwcontent( begg, endg, clm3%g%gws%gc_liq1(begg:endg), &
                              clm3%g%gws%gc_ice1(begg:endg), clm3%g%ges%gc_heat1(begg:endg) )
     call CLMDebug('BeginWaterBalance')
     call BeginWaterBalance(begc, endc, begp, endp, &
          filter%num_nolakec, filter%nolakec, filter%num_lakec, filter%lakec, &
          filter%num_hydrologyc, filter%hydrologyc)
  call CLMDebug('pftdyn_wbal_init')
  call pftdyn_wbal_init()
   if (fpftdyn /= ' ') then
   end if
     do c = begc,endc
        clm3%g%l%c%cps%decl(c) = declin
     end do
     call CLMDebug('DriverInit')
     call DriverInit(begc, endc, begp, endp, &
          filter%num_nolakec, filter%nolakec, &
          filter%num_lakec, filter%lakec)
     call CLMDebug('Hydrology1')
     call Hydrology1(begc, endc, begp, endp, &
                     filter%num_nolakec, filter%nolakec, &
                     filter%num_nolakep, filter%nolakep)
     call CLMDebug('SurfaceRadiation')
     call SurfaceRadiation(begp, endp, filter%num_nourbanp, filter%nourbanp)
     call CLMDebug('Biogeophysics1')
     call Biogeophysics1(begg, endg, begc, endc, begp, endp, &
                         filter%num_nolakec, filter%nolakec, &
                         filter%num_nolakep, filter%nolakep)
     call CLMDebug('BareGroundFluxes')
     call BareGroundFluxes(begp, endp, &
                           filter%num_nolakeurbanp, filter%nolakeurbanp)
     call CLMDebug('CanopyFluxes')
     call CanopyFluxes(begg, endg, begc, endc, begp, endp, &
                       filter%num_nolakep, filter%nolakep)
     call CLMDebug('BiogeophysicsLake')
     call BiogeophysicsLake(begc, endc, begp, endp, &
                            filter%num_lakec, filter%lakec, &
                            filter%num_lakep, filter%lakep)
     call CLMDebug('Begin VOCEmission')
   call VOCEmission(begp, endp, &
                      filter%num_soilp, filter%soilp)
     call CLMDebug('Biogeophysics2')
   call Biogeophysics2(begl, endl, begc, endc, begp, endp, &
                         filter%num_urbanl, filter%urbanl, &
                         filter%num_nolakec, filter%nolakec, &
                         filter%num_nolakep, filter%nolakep)
     call CLMDebug('pft2col')
     call pft2col(begc, endc, filter%num_nolakec, filter%nolakec)
     call CLMDebug('Hydrology2')
     call Hydrology2(begc, endc, begp, endp, &
                     filter%num_nolakec, filter%nolakec, &
                     filter%num_hydrologyc, filter%hydrologyc, &
                     filter%num_urbanc, filter%urbanc, &
                     filter%num_snowc, filter%snowc, &
                     filter%num_nosnowc, filter%nosnowc)
     call CLMDebug('HydrologyLake')
     call HydrologyLake(begp, endp, &
                        filter%num_lakep, filter%lakep)
     do c = begc,endc
        l = clandunit(c)
        if (itypelun(l) == isturb) then
           cptr%cps%frac_sno(c) = min( cptr%cps%snowdp(c)/0.05_r8, 1._r8)
        else
           cptr%cps%frac_sno(c) = 0.0_r8
           if(cptr%cps%snowdp(c) .gt. 0.0_r8) then
             cptr%cps%frac_sno(c) = tanh(cptr%cps%snowdp(c)/(2.5_r8*zlnd* &
               (min(800._r8,cptr%cws%h2osno(c)/cptr%cps%snowdp(c))/100._r8)**1._r8) )
           endif
        end if
     end do
     call CLMDebug('SnowAge_grain')
     call SnowAge_grain(begc, endc, &
          filter%num_snowc, filter%snowc, &
          filter%num_nosnowc, filter%nosnowc)
     call CLMDebug('Begin EcosystemDyn')
     call EcosystemDyn(begp, endp, &
                       filter%num_nolakep, filter%nolakep, &
                       doalb)
     call CLMDebug('BalanceCheck')
     call BalanceCheck(begp, endp, begc, endc, begl, endl, begg, endg)
    if (doalb) then
        call CLMDebug('SurfaceAlbedo')
        call SurfaceAlbedo(begg, endg, begc, endc, begp, endp, &
                           filter%num_nourbanc, filter%nourbanc, &
                           filter%num_nourbanp, filter%nourbanp,nextsw_cday ,declinp1)
     end if
  call EcosystemDyn_dealloc
end subroutine driver
