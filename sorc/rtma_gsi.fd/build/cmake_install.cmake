# Install script for directory: /lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RELEASE")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "TRUE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/wrflib/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/bacio/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/bufr/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/sigio/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/nemsio/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/crtm/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/sp/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/sfcio/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/w3emc/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/ip/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/src/ncdiag/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/src/fv3gfs_ncio/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/libsrc/GSD/gsdcloud/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/src/gsi/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/src/enkf/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/util/ndate/cmake_install.cmake")
  include("/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/util/EnKF/arw/src/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/lfs/h2/emc/da/noscrub/edward.colon/3drtma_hrrr/sorc/rtma_gsi.fd/build/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
