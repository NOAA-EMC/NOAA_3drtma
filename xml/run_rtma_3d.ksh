#!/bin/ksh --login

set -e
source /lfs1/BMC/nrtrr/FIX_EXEC_MODULE/modulefiles/modulefile.jet.ROCOTO

dst=/lfs1/BMC/nrtrr/workflow/test_3drtma/xml

rocotorun -w ${dst}/RTMA_3D.xml -d ${dst}/RTMA_3D.db

exit 0
