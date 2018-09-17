#/bin/sh

# set -x

echo "*==================================================================*"
echo " this script is going to link the fixed data to directory fix/ "
echo "*==================================================================*"

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

# link fix directories
cd ${FIX_DIR}
if [ $target = theia ]; then
  echo " linking fixed data on $target"
  GSI_fix="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/GSI-fix_rtma3d_emc_test"
  CRTM_fix="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/CRTM-fix_rtma3d"
  ObsUseList="/scratch4/NCEPDEV/meso/save/Gang.Zhao/FixData/ObsUseList_rtma3d"
  echo " ln -sf ${GSI_fix}        ./GSI-fix"
  ln -sf ${GSI_fix}        ./GSI-fix
  echo " ln -sf ${CRTM_fix}       ./CRTM-fix"
  ln -sf ${CRTM_fix}       ./CRTM-fix
  echo " ln -sf ${ObsUseList}     ./ObsUseList"
  ln -sf ${ObsUseList}     ./ObsUseList
else
  echo " this fix directories have not set up yet for machine $target."
  echo " Abort linking task."
  exit 9
fi

# copy/link executable files (GSI utility files, etc.)
GSI_BIN=${TOP_SORC}/build_gsi/bin
EXEC=${TOP_RTMA}/exec

echo " linking GSI executable files ... "
cd ${GSI_BIN}
exe_fnames=`ls *`
cd $EXEC
for fn in $exe_fnames
do
# ln -sf $BIN_GSI/$fn  ./$fn
  echo " cp -p  $BIN_GSI/$fn  ./$fn "
  cp -p  $BIN_GSI/$fn  ./$fn
done

#set +x

date

exit 0
