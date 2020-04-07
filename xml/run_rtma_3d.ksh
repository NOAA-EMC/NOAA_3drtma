#!/bin/ksh --login

set -e
source /lfs1/BMC/nrtrr/FIX_EXEC_MODULE/modulefiles/modulefile.jet.ROCOTO

rocotorun -w /home/rtrr/RTMA_3D_AK/xml/RTMA_3D.xml -d /home/rtrr/RTMA_3D_AK/xml/RTMA_3D.db

exit 0
