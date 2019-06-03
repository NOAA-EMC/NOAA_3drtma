#!/bin/sh

date
# set -x
#
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

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="feature/gsd_raphrrr_April2019"
# branch_gsi_gsd="feature/gsd_raphrrr_july2018"
# branch_gsi_gsd="master"
# branch_gsi_source: source branch  # the user-specified branch to build on.
                                    # if not specified by user, 
                                    #   it is branch_gsi_gsd by default.

# branch_gsi_source="<specify_your_source_branch_name_here>"
branch_gsi_source=${branch_gsi_source:-"$branch_gsi_gsd"}

if [ ${target} = jet ] ; then
  build_corelibs="ON"   # ON: Not using installed corelibs, building all corelibs with GSI together
                        # OFF: using installed corelibs (bacio, bufr, etc.)
  build_type=""         # option: DEBUG, or PRODUCTION(default)
else
  build_corelibs="OFF"  # OFF: using installed corelibs (bacio, bufr, etc.)
  build_type=""         # option: DEBUG, or PRODUCTION(default)
fi

if [ ${target} = dell ] ; then
  build_corelibs="OFF"   # ON: Not using installed corelibs, building all corelibs with GSI together
                        # OFF: using installed corelibs (bacio, bufr, etc.)
  build_type=""         # option: DEBUG, or PRODUCTION(default)
else
  build_corelibs="OFF"  # OFF: using installed corelibs (bacio, bufr, etc.)
  build_type=""         # option: DEBUG, or PRODUCTION(default)
fi


echo "option of build_corelibs = ${build_corelibs}"
echo "option of build_type     = ${build_type}"

exefile_name_gsi="rtma3d_gsi"

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
# read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"
#
#=========================================================================#

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
# all the building GSI jobs is to be done under sub-direvtory build_gsi
BUILD_GSI=${TOP_SORC}/build_gsi
BUILD_LOG=${BUILD_GSI}/build_log
DIRNAME_GSI="rtma_gsi.fd"
SORCDIR_GSI=${TOP_SORC}/${DIRNAME_GSI}
if [ ! -d ${BUILD_GSI} ] ; then mkdir -p ${BUILD_GSI} ; fi
if [ ! -d ${BUILD_LOG} ] ; then mkdir -p ${BUILD_LOG} ; fi

#
#--- detecting the existence of the directory of GSI source package
#
cd ${TOP_SORC}
if [ ! -d ${SORCDIR_GSI} ]
then
  echo " ====> WARNING: GSI source code directory: ${SORCDIR_GSI}  does NOT exist."
  echo " ====> WARNING: please check out a local copy of ProdGSI to ${SORCDIR_GSI}"
  echo " ====> Warning: abort compilation of GSI for RTMA3D."
  exit 2
fi

#
#--- compilation of GSI
#
# working branch
wrking_branch=${branch_gsi_source}
cd ${SORCDIR_GSI}
echo " ----> check out working branch "
echo " ----> git checkout ${wrking_branch} "
git checkout ${wrking_branch}

if [ $? -ne 0 ] ; then
  echo " failed to check out the branch ${wrking_branch} and abort "
  exit 1
fi

#==================#
# load modules (using module file under modulefiles/${target}/build)
#

# modules_dir=${SORCDIR_GSI}/modulefiles
# modules_fname=modulefile.ProdGSI.$target
modules_dir=${MODULEFILES_DIR}/${target}/build
modules_fname=modulefile.build.gsi.${target}
#modules_fname=modulefile.build.gsi.No_PreInstalledLibs.${target}

if [ $target = wcoss -o $target = cray ]; then
    module purge
    module load $modules_dir/${modules_fname}
elif [ $target = theia ]; then
    module purge
    source $modules_dir/${modules_fname}
    module list
elif [ $target = jet ]; then
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
# compiling gsi
echo " ====>  compiling GSI under building directory: ${BUILD_GSI} "
cd ${BUILD_GSI}

echo " cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ../${DIRNAME_GSI}  >& ./build_log/log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt "
cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DBUILD_CORELIBS=${build_corelibs}  ../${DIRNAME_GSI}  >& ./build_log/log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt

if [ $? -ne 0 ] ; then
  echo " ================ WARNING =============== " 
  echo " CMake step failed."
  echo " Check up with the log.cmake file under build_gsi/build_log/"
  echo "   ----> log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt : "
  echo " ================ WARNING =============== " 
fi

echo " make VERBOSE=1 -j 8 >& ./build_log/log.make.${DIRNAME_GSI}.${BUILD_TYPE}.txt "
make VERBOSE=1 -j 8 >& ./build_log/log.make.${DIRNAME_GSI}.${BUILD_TYPE}.txt

if [ $? -eq 0 ] ; then
  echo " GSI code and utility codes were built successfully."

  echo " cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/${exefile_name_gsi} "
  cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/${exefile_name_gsi}
  cp -p ${BUILD_GSI}/bin/ndate.x ${EXEC}/ndate.x
  ls -l ${EXEC}/${exefile_name_gsi}

  cd ${BUILD_GSI}/bin
  exe_fnames=`ls *`
  cd ${BUILD_GSI}
  for fn in $exe_fnames
  do
    echo " cp -p  ${BUILD_GSI}/bin/$fn  ${EXEC}/$fn "
    cp -p  ${BUILD_GSI}/bin/$fn  ${EXEC}/$fn
  done

else
  echo " ================ WARNING =============== " 
  echo " Compilation of GSI code was failed."
  echo " Check up with the log file under build_gsi/build_log/"
  echo "   ----> log.cmake.${DIRNAME_GSI}.${BUILD_TYPE}.txt : "
  echo "   ----> log.make.${DIRNAME_GSI}.${BUILD_TYPE}.txt  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0
