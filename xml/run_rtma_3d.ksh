#!/bin/ksh --login

set -e
source /home/rtrr/PARM_EXEC/modulefiles/modulefile.jet.ROCOTO

rocotorun -w /home/rtrr/RTMA_3D_dev1/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D_dev1/xml/RTMA_3D.db

exit 0
