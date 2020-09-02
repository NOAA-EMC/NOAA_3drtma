#!/bin/bash

  if [ -f /etc/bashrc ]; then
        . /etc/bashrc
  fi
  if [ ! -z /usrx/local/prod/lmod/lmod ]; then
    . /usrx/local/prod/lmod/lmod/init/bash
  else
    . /opt/modules/default/init/bash
  fi
  module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles/
  module load lsf/10.1 
  module load ruby/2.5.1 rocoto/complete

rocotorun -v 10 -w /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/NOAA_3drtma/xml/rtma3d_lsf_retro.xml -d /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/NOAA_3drtma/xml/rtma3d_lsf_retro.db 
