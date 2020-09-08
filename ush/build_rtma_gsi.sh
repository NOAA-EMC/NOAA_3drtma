#!/bin/sh

date
set -x
#

#=========================================================================#
# User define the following variables:

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="gsd/develop_work"
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
PARM_DIR=${TOP_RTMA}/parm/gsi

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
#--- Building of GSILIBS
#
# working branch

export FC=ifort
export CXX=icpc
export CC=icc

cd ${TOP_SORC}/GSILIBS
mkdir build
cd build
module unload bufr
cmake -DBUILD_CORELIBS=ON ..
make -j8



#--- Building of NCEPLIBS
#
# working branch

cd ${TOP_SORC}/NCEPLIBS
cp ${PARM_DIR}/CMakeLists.txt .
mkdir build
cd build
cmake ..
make -j8 CXXFLAGS='icpc' CCFLAGS='ic' FCFLAGS='ifort'
make install

cd ${TOP_SORC}/NCEPLIBS/build/install/lib
${TOP_SORC}/GSILIBS/linklibs




#
#--- compilation of GSI
#
# working branch


wrking_branch=${branch_gsi_source}
cd ${SORCDIR_GSI}
#echo " ----> check out working branch "
#echo " ----> git checkout ${wrking_branch} "
#git checkout ${wrking_branch}

#if [ $? -ne 0 ] ; then
#  echo " failed to check out the branch ${wrking_branch} and abort "
#  exit 1
#fi

sROOT=ROOT

rm ${SORCDIR_GSI}/ush/build.comgsi

#cat ${PARM_DIR}/build.comgsi_tmp | sed "s/${sROOT//\\/\\\\}/${TOP_SORC//\\/\\\\}/g" > ${SORCDIR_GSI}/ush/build.comgsi

#sed "s!ROOT!${TOP_SORC}!ig" ${PARM_DIR}/build.comgsi_tmp > ${SORCDIR_GSI}/ush/build.comgsi

sed -e "s/${sROOT//\//\\/}/${TOP_RTMA//\//\\/}/g" ${PARM_DIR}/build.comgsi_tmp > ${SORCDIR_GSI}/ush/build.comgsi

chmod 755 ${SORCDIR_GSI}/ush/build.comgsi
#if [[ -d /scratch3 ]] ; then ### theia
#source /etc/profile.d/modules.sh
#    modulefile="${PARM_DIR}/modulefile.theia.GSI_UPP_WRF"
#elif [[ -d /jetmon ]] ; then ### jet
#source /etc/profile.d/modules.sh
#    modulefile="${PARM_DIR}/modulefile.jet.GSI_UPP_WRF"
#elif [[ -d /glade ]] ; then  ### cheyenne
#source /etc/profile.d/modules.sh
#    modulefile="${PARM_DIR}/modulefile.cheyenne.GSI_UPP_WRF"
#elif [[ -d /ioddev_dell ]] ; then ### dell
#source /usrx/local/prod/lmod/lmod/init/sh
#    modulefile="${PARM_DIR}/modulefile.dell.GSI_UPP_WRF"
#else
#    echo "unknown machine"
#    exit 9
#fi



#if [ ! -f $modulefile ]; then
#    echo "modulefiles $modulefile does not exist"
#    exit 10
#fi
#source $modulefile

cd ${SORCDIR_GSI}

echo "compiled at the node:" >> ${USH_DIR}/log.build_rtma_gsi
hostname  >> ${USH_DIR}/log.build_rtma_gsi
#module list >> ${USH_DIR}/log.build_rtma_gsi
#echo -e "\nThe branch name:" >> ${USH_DIR}/log.build_rtma_gsi
#git branch | grep "*"  >> ${USH_DIR}/log.build_rtma_gsi
#echo -e "\nThe commit ID:" >> ${USH_DIR}/log.build_rtma_gsi
#git log -1 | head -n1 >> ${USH_DIR}/log.build_rtma_gsi
#echo -e "\ngit status:" >> ${USH_DIR}/log.build_rtma_gsi
#git status >> ${USH_DIR}/log.build_rtma_gsi
#echo -e "\nCompiling commands:" >> ${USH_DIR}/log.build_rtma_gsi
echo "${SORCDIR_GSI}/ush/build.comgsi"  >> ${USH_DIR}/log.build_rtma_gsi
#cat ${USH_DIR}/log.build_rtma_gsi

./ush/build.comgsi >&  ${BUILD_LOG}/log.make.${DIRNAME_GSI}.txt

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
  echo "   ----> log.make.${DIRNAME_GSI}.txt  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0

