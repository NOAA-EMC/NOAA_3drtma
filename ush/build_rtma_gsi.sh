#!/bin/sh

date
# set -x

branch_gsdgsi="feature/gsd_raphrrr_july2018"
branch_wrking="feature/rtma3d_gsi_${USER}"

build_corelibs="OFF"
# build_type="DEBUG"   # option: DEBUG, or PRODUCTION(default)

echo " this script is going to clone a local repository of ProdGSI "
echo " to sub-directory of sorc/rtma_gsi.fd and "
echo " check out the branch of gsdgsi --> ${branch_gsdgsi}"
echo " to the user working/topic branch --> ${branch_wrking} "

#===================================================================#
# detect the machine/platform
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
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."
#===================================================================#

BASE=`pwd`;
echo " current directory is $BASE "

# detect existence of directory sorc/
i_max=4; i=0;
while [ "$i" -lt "$i_max" ]
do
  let "i=$i+1"
  if [ -d ./sorc ]
  then
    cd ./sorc
    TOP_SORC=`pwd`
    TOP_RTMA=`dirname $(readlink -f .)`
    echo " found sorc/ is under $TOP_RTMA"
    break
  else
    cd ..
  fi
done
if [ "$i" -ge "$i_max" ]
then
  echo ' directory sorc/ could not be found. Abort the task of compilation.'
  exit 1
fi

USH_DIR=${TOP_RTMA}/ush

cd $TOP_RTMA
EXEC=${TOP_RTMA}/exec
if [ ! -d ${EXEC} ]; then mkdir -p ${EXEC}; fi

cd ${TOP_SORC}
# all the building GSI jobs is to be done under sub-direvtory build_gsi
BUILD_GSI=${TOP_SORC}/build_gsi
BUILD_LOG=${BUILD_GSI}/build_log
DIRNAME_GSI="rtma_gsi.fd"
SORCDIR_GSI=${TOP_SORC}/${DIRNAME_GSI}
if [ ! -d ${BUILD_GSI} ] ; then mkdir -p ${BUILD_GSI} ; fi
if [ ! -d ${BUILD_LOG} ] ; then mkdir -p ${BUILD_LOG} ; fi
cd ${TOP_SORC}
if [ ! -d ${SORCDIR_GSI} ]
then
  echo " ====> WARNING: GSI source code directory: ${SORCDIR_GSI}  does NOT exist."
  echo " ====> WARNING: please check out a local copy of ProdGSI to ${SORCDIR_GSI}"
  echo " ====> Warning: abort compilation of GSI for RTMA3D."
  exit 2
fi

#===================================================================#
#                      compilation of GSI
# working branch
wrking_branch=${branch_wrking:-"$branch_gsdgsi"}
cd ${SORCDIR_GSI}
echo " ----> check out working branch (based on GSD-GSI branch)"
echo " ----> git checkout ${wrking_branch} "
git checkout ${wrking_branch}
echo " ----------------------------------------------- "
echo " ----> check up the working branch :"
git branch
echo " ----------------------------------------------- "
echo  " please make sure it is the branch you specified: "
read -p " Press [Enter] key to continue:"

#==================#
# load modules
modules_dir=${SORCDIR_GSI}/modulefiles
if [ $target = wcoss -o $target = cray ]; then
    module purge
    module load $modules_dir/modulefile.ProdGSI.$target
elif [ $target = theia ]; then
    module purge
    source $modules_dir/modulefile.ProdGSI.$target
    module list
elif [ $target = dell ]; then
    module purge
    source $modules_dir/modulefile.ProdGSI.$target
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
else
    echo " ----> WARNING: module file has not been configured for this machine: $target "
    echo " ----> warning: abort compilation "
    exit 9
fi
#==================#

#==================#
# compiling gsi
echo " ====>  compiling GSI under building directory: ${BUILD_GSI} "
cd ${BUILD_GSI}

echo " cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ../${DIRNAME_GSI}  >& ./build_log/log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt "
cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ../${DIRNAME_GSI}  >& ./build_log/log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt

echo " make VERBOSE=1 -j 8 >& ./build_log/log.make.${DIRNAME_GSI}.${BUILD_TYPE}.txt "
make VERBOSE=1 -j 8 >& ./build_log/log.make.${DIRNAME_GSI}.${BUILD_TYPE}.txt

if [ $? -eq 0 ] ; then
  echo " cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/rtma3d_gsi "
  cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/rtma3d_gsi
  ls -l ${EXEC}/rtma3d_gsi
fi

#===================================================================#


# set +x
date

exit 0
