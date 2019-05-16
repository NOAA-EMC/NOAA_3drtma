# CMake generated Testfile for 
# Source directory: /home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd
# Build directory: /home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(global_T62 "regression_driver.sh" "global_T62" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(global_T62_ozonly "regression_driver.sh" "global_T62_ozonly" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_T62_ozonly PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(global_4dvar_T62 "regression_driver.sh" "global_4dvar_T62" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_4dvar_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(global_4denvar_T126 "regression_driver.sh" "global_4denvar_T126" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_4denvar_T126 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(global_lanczos_T62 "regression_driver.sh" "global_lanczos_T62" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_lanczos_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(arw_netcdf "regression_driver.sh" "arw_netcdf" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(arw_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(
          arw_binary "regression_driver.sh" "
          arw_binary" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(
          arw_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(nmm_binary "regression_driver.sh" "nmm_binary" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(nmm_binary PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(nmm_netcdf "regression_driver.sh" "nmm_netcdf" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(nmm_netcdf PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(nmmb_nems_4denvar "regression_driver.sh" "nmmb_nems_4denvar" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(nmmb_nems_4denvar PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(hwrf_nmm_d2 "regression_driver.sh" "hwrf_nmm_d2" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(hwrf_nmm_d2 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(hwrf_nmm_d3 "regression_driver.sh" "hwrf_nmm_d3" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(hwrf_nmm_d3 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(rtma "regression_driver.sh" "rtma" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(rtma PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(global_enkf_T62 "regression_driver.sh" "global_enkf_T62" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(global_enkf_T62 PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
ADD_TEST(netcdf_fv3_regional "regression_driver.sh" "netcdf_fv3_regional" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi")
SET_TESTS_PROPERTIES(netcdf_fv3_regional PROPERTIES  TIMEOUT "86400" WORKING_DIRECTORY "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/regression")
SUBDIRS(libsrc/wrflib)
SUBDIRS(libsrc/bacio)
SUBDIRS(libsrc/w3nco)
SUBDIRS(libsrc/ncdiag)
SUBDIRS(libsrc/GSD/gsdcloud)
SUBDIRS(src)
SUBDIRS(src/enkf)
SUBDIRS(util/EnKF/gfs/src)
SUBDIRS(util/ndate)
