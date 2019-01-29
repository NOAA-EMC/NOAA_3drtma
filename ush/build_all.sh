#!/bin/sh

date
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

#
#--- check out GSI package
#
cd ${USH_DIR}
echo " running checkout_rtma_gsi.sh to check out GSI package...  "
./checkout_rtma_gsi.sh

#
#--- build GSI
#
cd ${USH_DIR}
echo " running build_rtma_gsi.sh to build GSI code ..."
./build_rtma_gsi.sh

#
#--- build obs pre-process code for MOSAIC Radar data
#
cd ${USH_DIR}
echo " running build_rtma_process_mosaic.sh to build obs process MOSAIC code ..."
./build_rtma_process_mosaic.sh

#
#--- build obs pre-process code for NASA Cloud data
#
cd ${USH_DIR}
echo " running build_rtma_process_cloud.sh to build obs process NASA Cloud code ..."
./build_rtma_process_cloud.sh

#
#--- build obs pre-process code for ENLTN GLM lightning data
#
cd ${USH_DIR}
echo " running build_rtma_process_lightning.sh to build obs process ENLTN lightning code ..."
./build_rtma_process_lightning.sh

#
#--- check out uni-post package(UPP:EMC_post)
#
cd ${USH_DIR}
echo " running checkout_rtma_post.sh to check out uni-post package...  "
./checkout_rtma_post.sh

#
#--- build uni-post (UPP)
#
cd ${USH_DIR}
echo " running build_rtma_post.sh to build uni-post code ..."
./build_rtma_post.sh

#
#--- link exe, fixed data, etc.
#
# cd ${USH_DIR}
# echo " running link_rtma.sh to link fixed dta , executables, etc. ... "
# ./link_rtma.sh

#===================================================================#
date

exit
