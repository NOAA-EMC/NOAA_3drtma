#!/bin/bash

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="feature/gsd_raphrrr_july2018"
# branch_gsi_gsd="master"
# branch_gsi_source: source branch  # the user-specified branch to check out.
                                    # if not specified by user, 
                                    #   it is branch_gsi_gsd by default.

# branch_gsi_source="<specify_your_source_branch_name_here>"
branch_gsi_source=${branch_gsi_source:-"$branch_gsi_gsd"}

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
#===================================================================#

echo "*==================================================================*"
echo " this script is going to clone a local repository of ProdGSI "
echo " to sub-directory of sorc/rtma_gsi.fd for RTMA3D system and "
echo " check out the source branch  "
echo 
echo "     ----> ${branch_gsi_source}"
echo
echo " please look at the branch name and make sure it is the branch you want to check out "
echo " if it is not, abort and change the definition of branch_gsi_source in this script ($0)  "
# read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"

#
# --- Finding the RTMA ROOT DIRECTORY --- #
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

DIRNAME_GSI="rtma_gsi.fd"
SORCDIR_GSI=${TOP_SORC}/${DIRNAME_GSI}

#
#--- check out ProdGSi for RTMA3D and the specified branch
#
cd ${TOP_SORC}

if [ -d ${DIRNAME_GSI} ] ; then
    rm -rf ${DIRNAME_GSI}
fi

echo " make a local clone of the ProdGSI repository under ${TOP_SORC}/${DIRNAME_GSI} ... "
echo " ====> git clone gerrit:ProdGSI  ./${DIRNAME_GSI} "
git clone gerrit:ProdGSI  ./${DIRNAME_GSI}
cd ./${DIRNAME_GSI}
scp -p gerrit:hooks/commit-msg  .git/hooks

echo " check out the source branch (GSD RAP/HRRR-based) --> ${branch_gsi_source}"
echo " ====> git checkout ${branch_gsi_source} "
git checkout ${branch_gsi_source}

if [ $? -ne 0 ] ; then
  echo " WARNING: checking branch ${branch_gsi_source} failed!"
  echo " WARNING: user-specified branch ${branch_gsi_source} does NOT exist!"
  echo " please double check with the list of branches (git branch -a)"
  exit 1
fi

echo " check out the submodule libsrc (specific GSD-dev version)"
echo " ====> git submodule update --init libsrc "
git submodule update --init libsrc

echo

#
#--- If no modulefile specified for 3DRTMA on this machine to build GSI,
#--- then adopting the modulefile used in GSI.
#
MODULEFILES=${TOP_RTMA}/modulefiles
SORCDIR_GSI=${TOP_SORC}/rtma_gsi.fd
modules_fname=modulefile.build.gsi.${target}
# modules_fname=modulefile.build.gsi_NoPreInstalledLibs.${target}
if [ ! -f ${MODULEFILES}/${target}/build/${modules_fname} ] ; then
  echo " --> There is no pre-defined modulefile for building 3DRTMA on this ${target}.  "
  echo " --> Using modulefile in ProdGSI package to build 3DRTMA  "
  mfiles_gsi="modulefile.ProdGSI.${target}"
  for modfile in ${mfiles_gsi}
  do
    if [ ! -f ${SORCDIR_GSI}/modulefiles/${modfile} ] ; then
      echo " ----> ProdGSI also does NOT have modulefile for this ${target}. Abort! "
      exit 1
    else
      cp -p ${SORCDIR_GSI}/modulefiles/${modfile}   ${MODULEFILES}/${target}/build/${modules_fname}
    fi
  done
fi
# set +x

date

exit 0
