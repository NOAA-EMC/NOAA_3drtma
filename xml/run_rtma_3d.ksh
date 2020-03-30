#!/bin/ksh --login

set -e
source /lfs1/BMC/nrtrr/FIX_EXEC_MODULE/modulefiles/modulefile.jet.ROCOTO

rocotorun -w /lfs1/BMC/nrtrr/workflow/RTMA_3D_RU/xml/RTMA_3D.xml -d /lfs1/BMC/nrtrr/workflow/RTMA_3D_RU/xml/RTMA_3D.db

exit 0
