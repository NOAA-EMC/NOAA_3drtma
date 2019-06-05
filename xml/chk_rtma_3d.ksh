#!/bin/ksh --login

# module load slurm
module load rocoto/1.3.0-RC5

subhr="00"
timewindow=$1
timewindow=${timewindow:-"3"}
date1=`date +%Y%m%d%H -d "now"`
date1="${date1}${subhr}"
date0=`date +%Y%m%d%H -d "${timewindow} hour ago"`
date0="${date0}${subhr}"

rocotostat -v 10 -w /home/Gang.Zhao/rtma3d_repo/feature_rt-reg_gz/xml/RTMA_3D.xml -d /home/Gang.Zhao/rtma3d_repo/feature_rt-reg_gz/xml/RTMA_3D.db -c ${date0}:${date1}

exit 0
