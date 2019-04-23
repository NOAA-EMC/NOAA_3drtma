#!/bin/ksh --login

module load rocoto/1.3.0-RC3

rocotorun -w /home/rtrr/RTMA_3D_AK/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D_AK/xml/RTMA_3D.db

exit 0
