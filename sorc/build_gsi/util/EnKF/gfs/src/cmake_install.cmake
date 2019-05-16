# Install script for directory: /home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd/util/EnKF/gfs/src

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/home/Edward.Colon/EMC_noaa-3drtma/sorc")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "RELEASE")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "0")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/adderrspec_nmcmeth_spec.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/calc_increment_ens.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/getnstensmeanp.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/getsfcensmeanp.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/getsfcnstensupdp.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/getsigensmeanp_smooth_ncep.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/getsigensstatp.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/gribmean.fd/cmake_install.cmake")
  INCLUDE("/home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi/util/EnKF/gfs/src/recentersigp.fd/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

