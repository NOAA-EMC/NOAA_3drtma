#!/bin/sh

date
# set -x
#

#=========================================================================#
# User define the following variables:

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="gsd/i_lgt_data_type"
# branch_gsi_gsd="feature/gsd_raphrrr_july2018"
# branch_gsi_gsd="master"
# branch_gsi_source: source branch  # the user-specified branch to build on.
                                    # if not specified by user, 
                                    #   it is branch_gsi_gsd by default.

# branch_gsi_source="<specify_your_source_branch_name_here>"
branch_gsi_source=${branch_gsi_source:-"$branch_gsi_gsd"}
exefile_name_gsi="rtma3d_gsi"

#=========================================================================#

echo "*==================================================================*"
echo " this script is going to build/make the GSI code for RTMA3D " 
echo "  building process is under sorc/build/ "
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
# all the building GSI jobs is to be done under sub-direvtory build
DIRNAME_GSI="rtma_gsi.fd"
SORCDIR_GSI=${TOP_SORC}/${DIRNAME_GSI}
BUILD_GSI=${SORCDIR_GSI}/build
BUILD_LOG=${BUILD_GSI}/build_log
if [ ! -d ${BUILD_GSI} ] ; then mkdir -p ${BUILD_GSI} ; fi
if [ ! -d ${BUILD_LOG} ] ; then mkdir -p ${BUILD_LOG} ; fi

#
#--- detecting the existence of the directory of GSI source package
#
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


dir_root=$(pwd)

if [[ -d /scratch3 ]] ; then ### theia
source /etc/profile.d/modules.sh
    modulefile="/home/rtrr/PARM_EXEC/modulefiles/modulefile.theia.GSI_UPP_WRF"
elif [[ -d /jetmon ]] ; then ### jet
source /etc/profile.d/modules.sh
    modulefile="/home/rtrr/PARM_EXEC/modulefiles/modulefile.jet.GSI_UPP_WRF"
elif [[ -d /glade ]] ; then  ### cheyenne
source /etc/profile.d/modules.sh
    modulefile="/glade/p/ral/jntp/gge/modulefiles/modulefile.cheyenne.GSI_UPP_WRF"
elif [[ -d /ioddev_dell ]] ; then ### dell
source /usrx/local/prod/lmod/lmod/init/sh
    modulefile="/u/Edward.Colon/rtrr/PARM_EXEC/modulefiles/modulefile.dell.GSI_UPP_WRF"
else
    echo "unknown machine"
    exit 9
fi

if [ ! -f $modulefile ]; then
    echo "modulefiles $modulefile does not exist"
    exit 10
fi
source $modulefile

cd ${BUILD_GSI}

echo "compiled at the node:" >> ${USH_DIR}/log.build_rtma_gsi
hostname  >> ${USH_DIR}/log.build_rtma_gsi
module list >> ${USH_DIR}/log.build_rtma_gsi
echo -e "\nThe branch name:" >> ${USH_DIR}/log.build_rtma_gsi
git branch | grep "*"  >> ${USH_DIR}/log.build_rtma_gsi
echo -e "\nThe commit ID:" >> ${USH_DIR}/log.build_rtma_gsi
git log -1 | head -n1 >> ${USH_DIR}/log.build_rtma_gsi
echo -e "\ngit status:" >> ${USH_DIR}/log.build_rtma_gsi
git status >> ${USH_DIR}/log.build_rtma_gsi
echo -e "\nCompiling commands:" >> ${USH_DIR}/log.build_rtma_gsi
echo "  cmake -DENKF_MODE=WRF -DBUILD_CORELIBS=ON -DBUILD_GSDCLOUD_ARW=ON -DBUILD_UTIL_COM=ON .." >> ${USH_DIR}/log.build_rtma_gsi
echo "  make -j8" >> ${USH_DIR}/log.build_rtma_gsi
cat ${USH_DIR}/log.build_rtma_gsi


cmake -DENKF_MODE=WRF -DBUILD_CORELIBS=ON -DBUILD_GSDCLOUD_ARW=ON -DBUILD_UTIL_COM=ON ..  2>&1  | tee ${BUILD_LOG}/log.cmake.${DIRNAME_GSI}.txt
if [ $? -ne 0 ] ; then
  echo " ================ WARNING =============== " 
  echo " CMake step failed."
  echo " Check up with the log.cmake file under build/build_log/"
  echo "   ----> log.cmake.${DIRNAME_GSI}.txt : "
  echo " ================ WARNING =============== " 
fi

echo " make VERBOSE=1 -j 8 >& ${BUILD_LOG}/log.make.${DIRNAME_GSI}.txt "
make -j 8 2>&1 | tee ${BUILD_LOG}/log.make.${DIRNAME_GSI}.txt

if [ $? -eq 0 ] ; then
  echo " GSI code and utility codes were built successfully."

  echo " cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/GSI/${exefile_name_gsi} "
  cp -p ${BUILD_GSI}/bin/gsi.x   ${EXEC}/GSI/${exefile_name_gsi}
  cp -p ${BUILD_GSI}/bin/ndate.x ${EXEC}/ndate.x
  ls -l ${EXEC}/GSI/${exefile_name_gsi}

  cd ${BUILD_GSI}/bin
  exe_fnames=`ls *`
  cd ${BUILD_GSI}
  for fn in $exe_fnames
  do
    echo " cp -p  ${BUILD_GSI}/bin/$fn  ${EXEC}/GSI/$fn "
    cp -p  ${BUILD_GSI}/bin/$fn  ${EXEC}/GSI/$fn
  done

else
  echo " ================ WARNING =============== " 
  echo " Compilation of GSI code was failed."
  echo " Check up with the log file under "
  echo "   ----> log.cmake.${DIRNAME_GSI}.txt : "
  echo "   ----> log.make.${DIRNAME_GSI}.txt  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0

