#!/bin/ksh

set -xa

CYCLE=${PDYm1}00


  PDYtmp=`echo $CYCLE | cut -c 1-8`
  HH=`echo $CYCLE | cut -c 9-10`



  pb2nc ${urma_dir_ops}/rap.t${HH}z.prepbufr.tm00 \
        ${urma_dir}/rap.t${HH}z.prepbufr.tm00.nr.nc ${config_dir}/PB2NCConfig_DNG_new -v 2


