#!/bin/ksh --login

module load intel
module load rocoto

/apps/rocoto/default/bin/rocotorun -w /home/rtrr/RTMA_3D/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D/xml/RTMA_3D.db

exit 0
