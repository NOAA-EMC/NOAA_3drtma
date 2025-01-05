#!/bin/bash

  if [ -f /etc/bashrc ]; then
        . /etc/bashrc
  fi
  if [ ! -z /usr/share/lmod/lmod ]; then
    . /usr/share/lmod/lmod/init/bash
  else
    . /opt/modules/default/init/bash
  fi
  module use /apps/ops/test/nco/modulefiles/core
  module load rocoto

rocotorun -v 10 -w /lfs/h2/emc/da/noscrub/$USER/NOAA_3drtma_new/workflow/rtma3d_pbspro_conus.xml -d /lfs/h2/emc/da/noscrub/$USER/NOAA_3drtma_new/workflow/rtma3d_pbspro_conus.db 

