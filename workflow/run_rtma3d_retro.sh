#!/bin/sh -l

# . /etc/profile
# . /apps/lmod/lmod/init/ksh >/dev/null # Module Support

module load intel
module load rocoto
rocotorun -v 10 -w /scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/testrun/workflow/rtma3d_retro.xml -d /scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/testrun/workflow/rtma3d_retro.db 
