#!/bin/ksh --login

module load slurm
module load rocoto/1.3.0-RC5

rocotorun -w /home/rtrr/RTMA_3D_dev1/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D_dev1/xml/RTMA_3D.db

exit 0
