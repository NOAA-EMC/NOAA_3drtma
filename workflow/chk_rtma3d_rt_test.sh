#!/bin/bash

. /etc/profile
. /etc/profile.d/modules.sh >/dev/null # Module Support

module purge
module load intel
module load rocoto

rocotostat -v 10 -w /misc/whome/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/workflow/rtma3d_rt_test.xml -d /misc/whome/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/workflow/rtma3d_rt_test.db
