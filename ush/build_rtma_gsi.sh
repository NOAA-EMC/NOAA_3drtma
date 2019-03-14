#!/bin/sh
date
set -x

#=========================================================================#
# User define the following variables:

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="feature/gsd_raphrrr_july2018"
#branch_gsi_gsd="master"
# branch_gsi_source: source branch  # the user-specified branch to build on.
                                    # if not specified by user, 
                                    #   it is branch_gsi_gsd by default.

# branch_gsi_source="<specify_your_source_branch_name_here>"
branch_gsi_source=${branch_gsi_source:-"$branch_gsi_gsd"}

build_corelibs="ON"   # OFF: using installed corelibs (bacio, bufr, etc.)
build_type=   # option: DEBUG, or PRODUCTION(default)

#=========================================================================#

echo "*==================================================================*"
echo " this script is going to build/make the GSI code for RTMA3D " 
echo "  building process is under sorc/build_gsi/ "
echo "   the branch is "
echo
echo "   ----> ${branch_gsi_source}"
echo
echo " please look at the branch name and make sure it is the branch you want to build GSI code on "
echo " if it is not, abort and change the definition of branch_gsi_source in this script ($0)  "
read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"
#
#--- detect the machine/platform
#
if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=cray
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=dell
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
elif [[ -d /mnt/lfs3/projects ]] ; then
    . /apps/lmod/lmod/init/sh
    target=jet
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."
#===================================================================#


cd ..
BASE=$(pwd)

dir_root=$BASE/sorc/rtma_gsi.fd
exec=$BASE/exec
build_type=
build_corelibs="ON"
dir_build=$BASE/sorc/build_gsi

echo
echo "machine is $target"
echo

dir_modules=$dir_root/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

rm -rf $dir_build
mkdir -p $dir_build/build_log
cd $dir_build

if [ $target = wcoss -o $target = cray ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = theia ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
elif [ $target = jet ]; then
    module purge
    if [ -f $dir_modules/modulefile.ProdGSI_LIBON.$target ] ; then
      source $dir_modules/modulefile.ProdGSI_LIBON.$target
    else
      echo "module file missing"
      exit 1
    fi
elif [ $target = dell ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
    export NETCDF_INCLUDE=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_CFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_LDFLAGS_CXX="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf -lnetcdf_c++"
    export NETCDF_LDFLAGS_CXX4="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf -lnetcdf_c++4"
    export NETCDF_CXXFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_FFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_ROOT=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0
    export NETCDF_LIB=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib
    export NETCDF_LDFLAGS_F="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdff"
    export NETCDF_LDFLAGS_C="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf"
    export NETCDF_LDFLAGS="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdff"
    export NETCDF=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0
    export NETCDF_INC=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_CXX4FLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
fi
module list >& log.modulelist 2>&1

#==================#
# compiling gsi

echo " cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ${dir_root}  >& $dir_build/build_log/log.cmake.rtma_gsi.${BUILD_TYPE}.txt "
cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ${dir_root}  >& $dir_build/build_log/log.cmake.rtma_gsi.${BUILD_TYPE}.txt

if [ $? -ne 0 ] ; then
  echo " ================ WARNING =============== " 
  echo " CMake step failed."
  echo " Check up with the log.cmake file under build_gsi/build_log/"
  echo "   ----> log.cmake.rtma_gsi.${BUILD_TYPE}.txt : "
  echo " ================ WARNING =============== " 
fi

echo " make VERBOSE=1 -j 8 >& ./build_log/log.make.rtma_gsi.${BUILD_TYPE}.txt "
make VERBOSE=1 -j 8 >& ./build_log/log.make.rtma_gsi.${BUILD_TYPE}.txt

if [ $? -eq 0 ] ; then
  echo " GSI code and utility codes were built successfully."
  echo " cp -p ${dir_build}/bin/gsi.x   ${exec}/rtma3d_gsi "
  cp -p ${dir_build}/bin/gsi.x   ${exec}/rtma3d_gsi
  ls -l ${exec}/rtma3d_gsi
else
  echo " ================ WARNING =============== " 
  echo " Compilation of GSI code was failed."
  echo " Check up with the log file under build_gsi/build_log/"
  echo "   ----> log.cmake.rtma_gsi.${BUILD_TYPE}.txt : "
  echo "   ----> log.make.rtma_gsi.${BUILD_TYPE}.txt  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0
