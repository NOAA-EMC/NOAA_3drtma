#!/bin/sh

# set -x

echo "*==================================================================*"
echo " this script is going to link the fixed data to directory fix/ "
echo "*==================================================================*"

#===================================================================#
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
elif [[ -d /jetmon ]] ; then
    . /apps/lmod/lmod/init/sh
    target=jet
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."

BASE=`pwd`;
echo " current directory is $BASE "

#
#--- detect existence of directory sorc/
#
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

TOP_FIX=${TOP_RTMA}/fix
if [ ! -d ${TOP_FIX} ] ; then mkdir -p ${TOP_FIX} ; fi

#
#--- link fix directories
#
cd ${TOP_FIX}
if [ $target = theia ]; then
  echo " linking fixed data on $target for GSI analysis"
  GSI_fix="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/GSI-fix_rtma3d_emc_test"
  CRTM_fix="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/CRTM-fix_rtma3d"
  ObsUseList="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/ObsUseList_rtma3d"
  WPS="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/WPS"

  echo " ln -sf ${GSI_fix}        ./GSI-fix"
  ln -sf ${GSI_fix}        ./GSI-fix
  echo " ln -sf ${CRTM_fix}       ./CRTM-fix"
  ln -sf ${CRTM_fix}       ./CRTM-fix
  echo " ln -sf ${ObsUseList}     ./ObsUseList"
  ln -sf ${ObsUseList}     ./ObsUseList
  echo " ln -sf ${WPS}            ./WPS"
  ln -sf ${WPS}            ./WPS

elif [ $target = jet ]; then
  echo " linking fixed data on $target for GSI analysis"
  GSI_fix="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/GSI-fix_rtma3d_emc_test"
  CRTM_fix="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/CRTM-fix_rtma3d"
  ObsUseList="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/ObsUseList_rtma3d"
  WPS="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/WPS"

  echo " ln -sf ${GSI_fix}        ./GSI-fix"
  ln -sf ${GSI_fix}        ./GSI-fix
  echo " ln -sf ${CRTM_fix}       ./CRTM-fix"
  ln -sf ${CRTM_fix}       ./CRTM-fix
  echo " ln -sf ${ObsUseList}     ./ObsUseList"
  ln -sf ${ObsUseList}     ./ObsUseList
  echo " ln -sf ${WPS}            ./WPS"
  ln -sf ${WPS}            ./WPS

else
  echo " the fixed data directories have not set up yet for machine $target."
  echo " Abort linking task."
  exit 9
fi
echo
ls -ltr $TOP_FIX
echo
echo

TOP_PARM=${TOP_RTMA}/parm
if [ ! -d ${TOP_PARM} ] ; then mkdir -p ${TOP_PARM} ; fi
cd ${TOP_PARM}
if [ $target = theia ]; then
  echo "linking parameters data (parm/) on $target for UPP (uni-post)"
  UPP="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/static_gsd_rtma3d_gge/UPP"
# UPP="${TOP_RTMA}/sorc/rtma_post.fd/parm"
  WRF="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/static_gsd_rtma3d_gge/WRF"
  VERIF="/scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/FixData/VERIF-fix"
  rm -f ./UPP ./WRF ./VERIF
  echo " ln -sf ${UPP}        ./UPP"
  ln -sf ${UPP}             ./UPP
  echo " ln -sf ${WRF}        ./WRF"
  ln -sf ${WRF}             ./WRF
  echo " ln -sf ${VERIF}      ./VERIF"
  ln -sf ${VERIF}            ./VERIF

elif [ $target = jet ]; then
  echo "linking parameters data (parm/) on $target for UPP (uni-post)"
  UPP="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/UPP"
# UPP="${TOP_RTMA}/sorc/rtma_post.fd/parm"
  WRF="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/WRF"
  VERIF="/mnt/lfs3/projects/hfv3gfs/Edward.Colon/3drtma_fixfiles/VERIF-fix"
  rm -f ./UPP ./WRF ./VERIF
  echo " ln -sf ${UPP}        ./UPP"
  ln -sf ${UPP}             ./UPP
  echo " ln -sf ${WRF}        ./WRF"
  ln -sf ${WRF}             ./WRF
  echo " ln -sf ${VERIF}      ./VERIF"
  ln -sf ${VERIF}            ./VERIF

else
  echo " the parm directories have not set up yet for machine $target."
  echo " Abort linking task."
  exit 9
fi
echo
ls -ltr $TOP_PARM
echo
echo

#
#--- copy/link executable files (GSI utility files, etc.)
#
GSI_BIN=${TOP_SORC}/build_gsi/bin
EXEC=${TOP_RTMA}/exec

echo " linking GSI executable files ... "
cd ${GSI_BIN}
exe_fnames=`ls *`
cd $EXEC
for fn in $exe_fnames
do
  echo " cp -p  $GSI_BIN/$fn  ./$fn "
# ln -sf $GSI_BIN/$fn  ./$fn
  cp -p  $GSI_BIN/$fn  ./$fn
done

POST_BIN=${TOP_SORC}/rtma_post.fd/exec
EXEC=${TOP_RTMA}/exec

echo " linking UNI-POST executable files ... "
cd ${POST_BIN}
exe_fnames=`ls *`
cd $EXEC
for fn in $exe_fnames
do
  echo " cp -p  $POST_BIN/$fn  ./$fn "
# ln -sf $POST_BIN/$fn  ./$fn
  cp -p  $POST_BIN/$fn  ./$fn
done

echo 
echo " ====> list out te executable files in exec/"
ls -ltr $EXEC

#
#--- set up the log directory for rocoto workflow running job
#
WORKFLOW_DIR=${TOP_RTMA}/workflow
cd $WORKFLOW_DIR
mkdir -p logs
mkdir -p logs/jlogfiles
mkdir -p logs/pgmout

#set +x

date

exit 0
