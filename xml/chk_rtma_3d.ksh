#!/bin/ksh --login

module load intel
module load rocoto

/apps/rocoto/default/bin/rocotostat -v 10 -w /home/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/xml/RTMA_3D.xml -d /home/Gang.Zhao/rtma3d_repo/GSD_dev1_jjob/xml/RTMA_3D.db

exit 0
