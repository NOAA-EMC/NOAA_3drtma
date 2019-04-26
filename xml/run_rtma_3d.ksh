#!/bin/ksh --login

module load slurm
module load rocoto/1.3.0-RC3

rocotorun -w /home/Edward.Colon/EMC_noaa-3drtma/xml/RTMA_3D.xml -d /home/Edward.Colon/EMC_noaa-3drtma/xml/RTMA_3D.db

exit 0
