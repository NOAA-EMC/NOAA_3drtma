#!/bin/ksh --login

module load rocoto/1.3.0-RC3
module load slurm

rocotorun -v 10 -w /home/Gang.Zhao/rtma3d_repo/rt_reg/xml/RTMA_3D.xml -d /home/Gang.Zhao/rtma3d_repo/rt_reg/xml/RTMA_3D.db

exit 0
