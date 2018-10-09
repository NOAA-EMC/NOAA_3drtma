#!/bin/sh -l

# . /etc/profile
# . /apps/lmod/lmod/init/ksh >/dev/null # Module Support

module load intel
module load rocoto
#rocotorun -v 10 -w /scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/EMC_noaa-3drtma/workflow/rtma3d_retro.xml -d /scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/EMC_noaa-3drtma/workflow/rtma3d_retro.db 

rocotoboot -v 10 -w /scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/EMC_noaa-3drtma/workflow/rtma3d_retro.xml -d /scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/EMC_noaa-3drtma/workflow/rtma3d_retro.db -c 201807110000 -t rtma3d_post
