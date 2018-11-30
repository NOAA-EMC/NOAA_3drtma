#!/bin/ksh

set -xa

CYCLE=${PDYm1}00
PDYtmp=`echo $CYCLE | cut -c 1-8`
HH=`echo $CYCLE | cut -c 9-10`


pb2nc ${urma_dir_ops}/${mod}.t${HH}z.prepbufr.tm00 \
      ${urma_dir}/${mod}.t${HH}z.prepbufr.tm00.nr.nc ${config_dir}/PB2NCConfig_DNG_new -v 2

#flds="winds tmp spfh hgt"
fld="CandV"

for fld in $flds; do
  for exp in $exps; do
    mkdir -p $stat_dir/${dom_out}/${exp}/${fld}
        point_stat ${dng_dir}/wrfprs_hrconus_00.grib2 \
          ${urma_dir}/${mod}.t${HH}z.prepbufr.tm00.nr.nc \
          ${config_dir}/PointStatConfig_${dom_out}_ADPSFC_${mod}_${fld} -outdir $stat_dir/${dom_out}/${exp}${fld} -v 2
  done
done
exit
