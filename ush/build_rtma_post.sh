#!/bin/sh

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_post_gsd: GSD RAP/HRRR-based POST branch in repository of POST
branch_post_gsd="master"

# branch_post_source: source branch  # the user-specified branch to build on.
                                    # if not specified by user, 
                                    #   it is branch_post_gsd by default.

# branch_post_source="<specify_your_source_branch_name_here>"
branch_post_source=${branch_post_source:-"$branch_post_gsd"}

build_corelibs="OFF"   # OFF: using installed corelibs (bacio, bufr, etc.)
build_type="DEBUG"   # option: DEBUG, or PRODUCTION(default)

#=========================================================================#

echo "*==================================================================*"
echo " this script is going to build/make the POST code for RTMA3D " 
echo "  building process is under sorc/build_post/ "
echo "   the branch is "
echo
echo "   ----> ${branch_post_gsd}"
echo
echo " please look at the branch name and make sure it is the branch you want to build POST code on "
echo " if it is not, abort and change the definition of branch_post_source in this script ($0)  "
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
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."
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

cd $TOP_RTMA
EXEC=${TOP_RTMA}/exec
if [ ! -d ${EXEC} ]; then mkdir -p ${EXEC}; fi

cd ${TOP_SORC}
# all the building POST jobs is to be done under sub-direvtory build_post
BUILD_POST=${TOP_SORC}/build_post
BUILD_LOG=${BUILD_POST}/build_log
DIRNAME_POST="rtma_post.fd"
SORCDIR_POST=${TOP_SORC}/${DIRNAME_POST}
if [ ! -d ${BUILD_POST} ] ; then mkdir -p ${BUILD_POST} ; fi
if [ ! -d ${BUILD_LOG} ] ; then mkdir -p ${BUILD_LOG} ; fi

#
#--- detecting the existence of the directory of POST source package
#
cd ${TOP_SORC}
if [ ! -d ${SORCDIR_POST} ]
then
  echo " ====> WARNING: POST source code directory: ${SORCDIR_POST}  does NOT exist."
  echo " ====> WARNING: please check out a local copy of POST to ${SORCDIR_POST}"
  echo " ====> Warning: abort compilation of POST for RTMA3D."
  exit 2
fi

#
#--- compilation of POST
#
# working branch
wrking_branch=${branch_post_source}
cd ${SORCDIR_POST}
echo " ----> check out working branch "
echo " ----> git checkout ${wrking_branch} "
git checkout ${wrking_branch}

if [ $? -ne 0 ] ; then
  echo " failed to check out the branch ${wrking_branch} and abort "
  exit 1
fi

#==================#
# compiling post
echo " ====>  compiling POST under building directory: ${SORCDIR_POST}/sorc/ "

cd ${SORCDIR_POST}/sorc/
build_ncep_post.sh >& ${BUILD_LOG}/log.make.${DIRNAME_POST}.txt

if [ $? -eq 0 ] ; then
  echo " POST code and utility codes were built successfully."
  echo " cp -p ${SORCDIR_POST}/exec/ncep_post   ${EXEC}/rtma3d_post "
  cp -p ${SORCDIR_POST}/exec/ncep_post  ${EXEC}/rtma3d_post
  ls -l ${EXEC}/rtma3d_post
else
  echo " ================ WARNING =============== " 
  echo " Compilation of POST code was failed."
  echo " Check up with the log file under build_post/build_log/"
  echo "   ----> log.cmake.${DIRNAME_POST}.txt : "
  echo "   ----> log.make.${DIRNAME_POST}.txt  : "
  echo " ================ WARNING =============== " 
fi

#===================================================================#

# set +x
date

exit 0
