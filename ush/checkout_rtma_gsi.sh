#!/bin/sh

date
set -x

#=========================================================================#
# User define the following variables:

# branch_gsi_gsd: GSD RAP/HRRR-based GSI branch in repository of ProdGSI
branch_gsi_gsd="feature/gsd_raphrrr_july2018"
#branch_gsi_gsd="master"
# branch_gsi_source: source branch  # the user-specified branch to check out.
                                    # if not specified by user, 
                                    #   it is branch_gsi_gsd by default.

# branch_gsi_source="<specify_your_source_branch_name_here>"
branch_gsi_source=${branch_gsi_source:-"$branch_gsi_gsd"}

#=========================================================================#

echo "*==================================================================*"
echo " this script is going to clone a local repository of ProdGSI "
echo " to sub-directory of sorc/rtma_gsi.fd for RTMA3D system and "
echo " check out the source branch  "
echo 
echo "     ----> ${branch_gsi_source}"
echo
echo " please look at the branch name and make sure it is the branch you want to check out "
echo " if it is not, abort and change the definition of branch_gsi_source in this script ($0)  "
read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"

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
#--- link modulefiles used in GSI
#
MODULEFILES=${TOP_RTMA}/modulefiles
SORCDIR_GSI=${TOP_SORC}/rtma_gsi.fd
echo " --> linking GSI modulefiles to RTMA3D modulefiles (used for compilation of GSI)  "
echo $target

if [ $target = jet ] ; then
   cp ${MODULEFILES}/jet_files/modulefile.ProdGSI_LIBON.$target ${SORCDIR_GSI}/modulefiles
   cp ${MODULEFILES}/jet_files/modulefile.global_gsi.$target ${SORCDIR_GSI}/modulefiles
   cp ${MODULEFILES}/jet_files/modulefile.ProdGSI.$target ${MODULEFILES}
   mfiles="modulefile.ProdGSI_LIBON.$target modulefile.global_gsi.$target"
else 
   mfiles="modulefile.ProdGSI.$machine modulefile.global_gsi.$target"
fi

for modfile in $mfiles
do
  echo " ----> ln -sf ${MODULEFILES}/$modfile ${SORCDIR_GSI}/modulefiles/$modfile "
  ln -sf ${SORCDIR_GSI}/modulefiles/$modfile ${MODULEFILES}/$modfile 
done


date

exit 0
