#!/bin/bash

date
# set -x
#=========================================================================#
#
#--- detect the machine/platform
#
if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    target=wcoss
elif [[ -d /cm ]] ; then
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    conf_target=nco
    target=cray
elif [[ -d /ioddev_dell ]]; then
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    conf_target=nco
    target=dell
    if [[ -d /ioddev_dell/dell1/nco/ops/nwtest/wrf_shared.v1.1.0 ]]; then
    	MACHINENAME=venus
    else
    	MACHINENAME=mars
    fi 
elif [[ -d /scratch3 ]] ; then
    . /etc/profile
    . /etc/profile.d/modules.sh >/dev/null # Module Support
    target=theia
elif [[ -d /jetmon ]] ; then
    . /etc/profile
    . /etc/profile.d/modules.sh >/dev/null # Module Support
    target=jet
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."
#=========================================================================#
# User define the following variables:

dirname_source="rtma_process_cloud.fd"
exefile_name_cloud="rtma3d_process_cloud"
#=========================================================================#

echo "*==================================================================*"
echo " this script is going to build/make the executable code of observation pre-process " 
echo "   of NASA RC Cloud data used for RTMA3D " 
echo "*==================================================================*"
#===================================================================#

#
#--- Finding the RTMA ROOT DIRECTORY --- #
#
BASE=`pwd`;
echo " current directory is $BASE "

# detect existence of directory sorc/
i_max=5; i=0;
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
MODULEFILES_DIR=${TOP_RTMA}/modulefiles

cd $TOP_RTMA
EXEC=${TOP_RTMA}/exec
if [ ! -d ${EXEC} ]; then mkdir -p ${EXEC}; fi

cd ${TOP_SORC}
# all the building jobs is to be done under sub-direvtory BUILD_DIR
SOURCE_DIR=${TOP_SORC}/${dirname_source}
BUILD_DIR=${TOP_SORC}/${dirname_source}

echo "*==================================================================*"
echo "  building process is under $BUILD_DIR"
echo "   the source code is under "
echo
echo "   ----> ${SOURCE_DIR}"
echo
echo " please look at the source code directory name and make sure it is the name you want to build code on "
echo " if it is not, abort and change the definition of 'dirname_source' in this script ($0)  "
# read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"
#
#--- detecting the existence of the directory of source package
#
if [ ! -d ${SOURCE_DIR} ]
then
  echo " ====> WARNING: directory of source code of obs pre-process NASA RC Cloud : ${SOURCE_DIR}  does NOT exist."
  echo " ====> Warning: abort compilation of obs pre-process NASA RC Cloud for RTMA3D."
  exit 2
fi

#
#--- compilation of code
#

#==================#
# load modules (using module file under modulefiles/${target}/build)
#

modules_dir=${MODULEFILES_DIR}/${target}/build
modules_fname=modulefile.build.gsi.${target}

if [ $target = wcoss -o $target = cray ]; then
    module purge
    module load $modules_dir/${modules_fname}
elif [ $target = theia ]; then
    module purge
    source $modules_dir/${modules_fname}
    module list
elif [ $target = jet ]; then
#    modules_fname=modulefile.build.gsi.PreInstalledLibs.${target}
    module purge
    source $modules_dir/${modules_fname}
    module list
elif [ $target = dell ]; then
    module purge
    source $modules_dir/${modules_fname}
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
# compiling
cd ${SOURCE_DIR}
echo " ====>  compiling is under directory: ${SOURCE_DIR} "
if [ $target = dell ]; then
make clean  -f makefile_${target}_${MACHINENAME}
echo " make -f makefile_${target}_${MACHINENAME}  >& ./log.make.process_NASA_cloud "
make -f makefile_${target}_${MACHINENAME}  >& ./log.make.process_NASA_cloud 
else
make clean  -f makefile_${target}
echo " make -f makefile_${target}  >& ./log.make.process_NASA_cloud "
make -f makefile_${target} >& ./log.make.process_NASA_cloud
fi

if [ $? -eq 0 ] ; then
  echo " code was built successfully."
#  echo " cp -p ${BUILD_DIR}/rap_process_cloud.exe   ${EXEC}/GSI/${exefile_name_cloud} "
#  cp -p ${BUILD_DIR}/rap_process_cloud.exe   ${EXEC}/GSI/${exefile_name_cloud}
#  ls -l ${EXEC}/GSI/${exefile_name_cloud}
else
  echo " ================ WARNING =============== " 
  echo " Compilation of process_NASA_cloud code was failed."
  echo " Check up with the log file under ${SOURCE_DIR}"
  echo "   ----> log.make.process_NASA_cloud  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0
