#!/bin/sh

date
# set -x

branch_gsdgsi="feature/gsd_raphrrr_july2018"
branch_wrking="feature/rtma3d_gsi_${USER}"

echo "*==================================================================*"
echo " this script is going to clone a local repository of ProdGSI "
echo " to sub-directory of sorc/rtma_gsi.fd and "
echo " check out the branch of gsdgsi --> ${branch_gsdgsi}"
echo " to the user working/topic branch --> ${branch_wrking} "
echo "*==================================================================*"

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

cd ${TOP_SORC}
DIRNAME_GSI="rtma_gsi.fd"
SORCDIR_GSI=${TOP_SORC}/${DIRNAME_GSI}

echo " make a local clone of the ProdGSI repository under ${TOP_SORC}/${DIRNAME_GSI} ... "
echo " ====> git clone gerrit:EMC_noaa-3drtma  ./${DIRNAME_GSI} "
git clone gerrit:ProdGSI  ./${DIRNAME_GSI}
cd ./${DIRNAME_GSI}
scp -p gerrit:hooks/commit-msg  .git/hooks

echo " check out the GSD-GSI branch -->  ${branch_gsdgsi}"
echo " ====> git checkout ${branch_gsdgsi} "
git checkout ${branch_gsdgsi}

echo " check out the submodule libsrc (specific GSD-dev version)"
echo " ====> git submodule update --init libsrc "
git submodule update --init libsrc

echo " check out to the user working/topic branch --> ${branch_wrking} "
echo " ====> git checkout -b ${branch_wrking} "
git checkout -b ${branch_wrking}
echo " ====> git branch # (to make sure it is the working branch) "
git branch

echo ""

# link modulefiles used in GSI
MODULEFILES=${TOP_RTMA}/modulefiles
SORCDIR_GSI=${TOP_SORC}/rtma_gsi.fd
echo " --> linking modulefiles (used for compilation of GSI)  "
cd ${MODULEFILES}
mfiles="modulefile.ProdGSI.wcoss modulefile.ProdGSI.theia modulefile.global_gsi.theia"
for modfile in $mfiles
do
  echo " ----> ln -sf ${MODULEFILES}/$modfile ${SORCDIR_GSI}/modulefiles/$modfile "
  ln -sf ${SORCDIR_GSI}/modulefiles/$modfile ${MODULEFILES}/$modfile 
done

set +x

date

exit 0
