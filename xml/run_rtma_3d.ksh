#!/bin/ksh --login

set -e
source /lfs1/projects/nrtrr/FIX_EXEC_MODULE/modulefiles/modulefile.jet.ROCOTO

rocotorun -w /lfs1/projects/nrtrr/workflow/RTMA_3D_RU/xml/RTMA_3D.xml -d /lfs1/projects/nrtrr/workflow/RTMA_3D_RU/xml/RTMA_3D.db

exit 0
