#!/bin/ksh --login

# module load slurm
module load rocoto/1.3.0-RC5

rocotorun -v 10 -w /home/Gang.Zhao/rtma3d_repo/feature_rt-reg_gz/xml/RTMA_3D.xml -d /home/Gang.Zhao/rtma3d_repo/feature_rt-reg_gz/xml/RTMA_3D.db

exit 0
