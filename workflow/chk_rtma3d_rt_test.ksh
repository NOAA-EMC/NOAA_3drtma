#!/bin/ksh

. /etc/profile
. /etc/profile.d/modules.sh >/dev/null # Module Support

module purge
module load intel
module load rocoto

subhr="00"
timewindow=$1
timewindow=${timewindow:-"6"}
date1=`date +%Y%m%d%H -d "now"`
date1="${date1}${subhr}"
date0=`date +%Y%m%d%H -d "${timewindow} hour ago"`
date0="${date0}${subhr}"

rocotostat -v 10 -w /misc/whome/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/workflow/rtma3d_rt_test.xml -d /misc/whome/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/workflow/rtma3d_rt_test.db -c ${date0}:${date1}
