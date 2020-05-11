#!/bin/ksh --login

set -e
source /home/rtrr/FIX_EXEC_MODULE/modulefiles/modulefile.jet.ROCOTO

rocotorun -w /home/rtrr/RTMA_3D_RU/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D_RU/xml/RTMA_3D.db

exit 0
