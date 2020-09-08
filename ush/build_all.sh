#!/bin/bash

date
#===================================================================#

if [ "$1" != "" ]; then
    echo "Interactive installation. It will pause for each step and ask user to confirm to continue."
    export INTBUILD=1
else
    echo "Non-interactive installation. Please check the log files after installation."
    export INTBUILD=0
fi

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
export USH_DIR=${TOP_RTMA}/ush 
export EXEC_DIR=${TOP_RTMA}/exec


if [ $target = 'jet' ] ; then

if [ ! -d ${EXEC_DIR} ]; then

mkdir -p ${EXEC_DIR}/GSI
mkdir -p ${EXEC_DIR}/UPP
mkdir -p ${EXEC_DIR}/smartinit

else

rm -r ${EXEC_DIR}/GSI/*
rm -r ${EXEC_DIR}/UPP/*
rm -r ${EXEC_DIR}/smartinit/*

fi

ln -s /home/rtrr/HRRR/exec/GSI/count_obs.exe ${EXEC_DIR}/GSI/rtma3d_count_obs
ln -s /home/rtrr/HRRR/exec/GSI/HRRR_gsi_hyb ${EXEC_DIR}/GSI/rtma3d_gsi_hyb
ln -s /home/rtrr/HRRR/exec/GSI/prepbufr_append_clamps.exe ${EXEC_DIR}/GSI/prepbufr_append_clamps.exe
ln -s /home/rtrr/HRRR/exec/GSI/prepbufr_append_sticknet.exe ${EXEC_DIR}/GSI/prepbufr_append_sticknet.exe
ln -s /home/rtrr/HRRR/exec/GSI/prepbufr_append_vessonndes.exe ${EXEC_DIR}/GSI/prepbufr_append_vessonndes.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_Lightning.exe ${EXEC_DIR}/GSI/process_Lightning.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_Lightning_bufr.exe ${EXEC_DIR}/GSI/process_Lightning_bufr.exe 
ln -s /home/rtrr/HRRR/exec/GSI/process_NASALaRC_cloud.exe ${EXEC_DIR}/GSI/process_NASALaRC_cloud.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_NSSL_mosaic.exe ${EXEC_DIR}/GSI/process_NSSL_mosaic.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_SST.exe  ${EXEC_DIR}/GSI/process_SST.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_sodardata_rt.exe ${EXEC_DIR}/GSI/process_sodardata_rt.exe
ln -s /home/rtrr/HRRR/exec/GSI/process_tamdar_netcdf.exe ${EXEC_DIR}/GSI/process_tamdar_netcdf.exe
ln -s /home/rtrr/HRRR/exec/GSI/rap_process_cloud ${EXEC_DIR}/GSI/rap_process_cloud
ln -s /home/rtrr/HRRR/exec/GSI/read_diag_conv.exe ${EXEC_DIR}/GSI/read_diag_conv.exe
ln -s /home/rtrr/HRRR/exec/GSI/read_diag_rad.exe ${EXEC_DIR}/GSI/read_diag_rad.exe
ln -s /home/rtrr/HRRR/exec/GSI/ssrc.exe ${EXEC_DIR}/GSI/ssrc.exe
ln -s /mnt/lfs3/projects/hfv3gfs/Edward.Colon/FixData/upp/ncep_post.exe ${EXEC_DIR}/UPP/ncep_post.exe
ln -s /home/rtrr/HRRR/exec/smartinit/wgrib2  ${EXEC_DIR}/smartinit/wgrib2

else

if [ ! -d ${EXEC_DIR} ]; then

mkdir -p ${EXEC_DIR}/GSI
mkdir -p ${EXEC_DIR}/UPP
mkdir -p ${EXEC_DIR}/smartinit

else

rm -r ${EXEC_DIR}/GSI/*
rm -r ${EXEC_DIR}/UPP/*
rm -r ${EXEC_DIR}/smartinit/*

fi

#
#--- check out GSI package
#
cd ${USH_DIR}
echo " running checkout_rtma_gsi.sh to check out GSI package...  "
# ./checkout_rtma_gsi.sh  >& log.checkout_rtma_gsi 2>&1
./checkout_rtma_gsi.sh

#
#--- build GSI
#
cd ${USH_DIR}
echo " running build_rtma_gsi.sh to build GSI code ..."
./build_rtma_gsi.sh > log.build_rtma_gsi 2>&1

#
#--- build obs pre-process code for MOSAIC Radar data
#
cd ${USH_DIR}
echo " running build_rtma_process_mosaic.sh to build obs process MOSAIC code ..."
./build_rtma_process_mosaic.sh > log.build_rtma_process_mosaic 2>&1

#
#--- build obs pre-process code for NASA Cloud data
#
cd ${USH_DIR}
echo " running build_rtma_process_cloud.sh to build obs process NASA Cloud code ..."
./build_rtma_process_cloud.sh > log.build_rtma_process_cloud 2>&1

#
#--- build obs pre-process code for ENLTN GLM lightning data
#
cd ${USH_DIR}
echo " running build_rtma_process_lightning.sh to build obs process ENLTN lightning code ..."
./build_rtma_process_lightning.sh > log.build_rtma_process_lightning 2>&1

#
#--- build updatevars executable (WRFV3.9) needed to generate updated reflectivities."
#
cd ${USH_DIR}
echo " running build_rtma_updatevars.sh to build  FV3.9 executable..."
./build_rtma_updatevars.sh > log.build_rtma_updatevars 2>&1

cd ${USH_DIR}
echo " running build_rtma_update_ncfields.sh to build  FV3.9 executable..."
./build_rtma_update_ncfields.sh > log.build_rtma_update_ncfields 2>&1

#
#--- check out uni-post package(UPP:EMC_post)
#
cd ${USH_DIR}
echo " running checkout_rtma_post.sh to check out uni-post package...  "
# ./checkout_rtma_post.sh >& log.checkout_rtma_post 2>&1
./checkout_rtma_post.sh

#
#--- build uni-post (UPP)
#
cd ${USH_DIR}
echo " running build_rtma_post.sh to build uni-post code ..."
./build_rtma_post.sh > log.build_rtma_post 2>&1
#
#===================================================================#
date

fi

exit
