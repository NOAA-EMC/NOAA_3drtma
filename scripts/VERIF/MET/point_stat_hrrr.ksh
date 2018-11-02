#!/bin/ksh

set -xa

CYCLE=${PDYm1}00
#CYCLE_LAST=${PDYm1}01

#CYCLE=$PDY$cyc

dng=1

flds="winds tmp spfh hgt"

if [ ${dng} -eq 1 ]; then

#while [ $CYCLE -le $CYCLE_LAST ] ; do
 for fld in $flds; do

  PDYtmp=`echo $CYCLE | cut -c 1-8`
  cyc=`echo $CYCLE | cut -c 9-10`

  for exp in $exps; do

    mkdir -p $stat_dir/${dom_out}/${exp}/${fld}
    export expnam=$exp

    for HH in $hrs; do

      export fcsthrs=$HH

      obsdate=`$NDATE $HH $PDYtmp$cyc`
      PDYobs=`echo $obsdate | cut -c 1-8`
      HHobs=`echo $obsdate | cut -c 9-10`

      if [ $exp = "nam" ];then
        rm NAMsfc.grb2
        if [ ! -e ${dng_dir}/${mod}.${PDYtmp}_${exp} ];then 
          mkdir -p ${dng_dir}/${mod}.${PDYtmp}_${exp}
        fi
        $WGRIB2 ${nam_ops_dir}/${mod}.${PDYm1}/${mod}.t${cyc}z.${bdom}.bsmart${HH}.tm00 | grep -F -f nam_sfc.fields | $WGRIB2 -i -grib NAMsfc.grb2 ${nam_ops_dir}/${mod}.${PDYm1}/${mod}.t${cyc}z.${bdom}.bsmart${HH}.tm00
        $WGRIB2 NAMsfc.grb2 -set_grib_type $compress \
          -new_grid_${fld} grid \
          -new_grid_interpolation bilinear \
          -if ":(VGTYP|LAND):" -new_grid_interpolation neighbor -fi \
          -new_grid ${wgrib2def} ${dng_dir}/${mod}.${PDYtmp}_${exp}/${mod}.t${cyc}z.NDFD${dom}f${HH}.grib2

        point_stat ${dng_dir}/${mod}.${PDYtmp}_${exp}/${mod}.t${cyc}z.NDFD${dom}f${HH}.grib2 \
          ${urma_dir}/rtma.t${HHobs}z.prepbufr.tm00.nr.nc \
          PointStatConfig_${dom_out}_ADPSFC_nam_${fld}_new -outdir $stat_dir/${dom_out}/${cyc}Z/${exp} -v 2
      elif [ $mod = "rap" ];then
        point_stat ${dng_dir}/wrfnat_hrconus_00.grib2 \
          ${urma_dir}/${mod}.t${HHobs}z.prepbufr.tm00.nr.nc \
          PointStatConfig_${dom_out}_ADPSFC_rap_tmp_new -outdir $stat_dir/${dom_out}/${cyc}Z/${exp} -v 2
      elif [ $mod = "hrrr" -a ${dom} = "conus" ];then
        point_stat ${dng_dir}/${dom}/${mod}.t${cyc}z.smart${mod}${dom}f${HH}.grib2 \
          ${urma_dir}/rtma.t${HHobs}z.prepbufr.tm00.nr.nc \
          ${config_dir}/PointStatConfig_${dom_out}_ADPSFC_${mod}_${fld} -outdir $stat_dir/${dom_out}/${exp}/${fld} -v 2
      elif [ $mod = "hrrr" -a ${dom} = "alaska" ];then
        point_stat ${dng_dir}/${mod}.${PDYtmp}_${exp}/${dom}/${mod}.t${cyc}z.smart${mod}${dom_out}f${HH}.grib2 \
          ${urma_dir}/rtma.t${HHobs}z.prepbufr.tm00.nr.nc \
          ${config_dir}/PointStatConfig_${dom_out}_ADPSFC_${mod}_${fld} -outdir $stat_dir/${dom_out}/${exp} -v 2
      else
        point_stat ${dng_dir}/${mod}.t${cyc}z.smart${dom}${HH}.tm00${ext}.grib2 \
          ${urma_dir}/rtma.t${HHobs}z.prepbufr.tm00.nr.nc \
          PointStatConfig_${dom_out}_ADPSFC_${fld}_new -outdir $stat_dir/${dom_out}/${cyc}Z/${exp} -v 2
      fi 

    done

  done
 
#  CYCLE=`/nwprod/util/exec/$NDATE +1 $CYCLE`

 done

#done

fi

exit
