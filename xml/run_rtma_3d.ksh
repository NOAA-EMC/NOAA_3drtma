#!/bin/ksh --login

module load slurm
module load rocoto/1.3.0-RC3

rocotorun -w /home/rtrr/RTMA_3D/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D/xml/RTMA_3D.db

exit 0
