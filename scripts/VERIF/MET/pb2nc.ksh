#!/bin/ksh

set -xa

CYCLE=${PDYm1}00
CYCLE_LAST=$CYCLE
#CYCLE_LAST=${PDY}23
#CYCLE_LAST=${PDYm1}01

prepbufr=1

if [ $prepbufr -eq 1 ]; then

  while [ $CYCLE -le $CYCLE_LAST ] ; do

  PDYtmp=`echo $CYCLE | cut -c 1-8`
  HH=`echo $CYCLE | cut -c 9-10`

#  if [ $urma_dir/urma.$PDYtmp ]; then 
#    mkdir -p $urma_dir/urma.$PDYtmp
#  fi


  pb2nc ${urma_dir_ops}/rap.t${HH}z.prepbufr.tm00 \
        ${urma_dir}/rap.t${HH}z.prepbufr.tm00.nr.nc ${config_dir}/PB2NCConfig_DNG_new -v 2

  CYCLE=`$NDATE +1 $CYCLE`

  done

fi

