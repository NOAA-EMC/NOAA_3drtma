#!/bin/bash

##########################################################################
####  UNIX Script Documentation Block                                    #
#                                                                        #
# Script name: exrtma3d_verif.sh                                         #
# Script description: - Computes verifification statistics               #
#                                                                        #
# Author:      Edward Colon        Org: EMC            Date: 2018-12-12  #
#                                                                        #
# Script history log:                                                    #
# 2012-12-12  colon                                                      #
##########################################################################

set -x



msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"
setpdy.sh
. PDY


cd $MET_DIR


CYCLE=${PDY}${cyc}
HH=`echo $CYCLE | cut -c 9-10`
export fcsthrs=$HH
export mod=rap
# uncomment if running in realtime

export MODEL=RAP
export exps="rap"
export expnam=$exps
export dom_check=$RUN

# NAM grid definitions; new RAP and HRRR definitions (can't use with the old RAP)
export wgrib2def_cs="lambert:265:25:25 233.723448:2345:2539.703 19.228976:1597:2539.703"
export wgrib2def_ak="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"
export wgrib2def_pr="mercator:20 291.804687:177:2500:296.027600 16.828685:129:2500:19.747399"
export wgrib2def_hi="mercator:20 198.474999:321:2500:206.130999 18.072699:225:2500:23.087799"
export wgrib2def_cs_hrrr="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"

# Do not need to do because the winds MET already does this.


for dom1 in $dom_check; do
  if [ $dom1 = rtma2p5 ];then
    export dom=conus
    export dom_out="cs"
    export ext="_wexp"
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_cs
  elif [ $dom1 = rtma3d ];then
    export dom=conus
    export dom_out="cs"
    export ext="_wexp"
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_cs
  elif [ $dom1 = akrtma ];then
    export dom=ak
    export dom_out="ak"
    export ext=""
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_ak
  elif [ $dom1 = prrtma ];then
    export dom=pr
    export dom_out=$dom
    export ext=""
    export compress="jpeg -set_bitmap 1"
    export wgrib2def=$wgrib2def_pr
  elif [ $dom1 = hirtma ];then
    export dom=hi
    export dom_out=$dom
    export ext=""
    export compress="jpeg -set_bitmap 1"
    export wgrib2def=$wgrib2def_hi
  fi


pb2nc ${urma_dir_ops}/${mod}.t${HH}z.prepbufr.tm00 \
      ${urma_dir}/${mod}.t${HH}z.prepbufr.tm00.nr.nc ${config_dir}/PB2NCConfig_DNG_new -v 2
      export err=$? ; err_chk
      if [ ${err} -ne 0 ]; then
            ${ECHO} "pb2nc crashed!  Exit status=${err}"
             exit ${err}
      fi


flds="CandV"
for fld in $flds; do
  for exp in $exps; do
    mkdir -p $stat_dir/${dom_out}/${exp}/${fld}
        point_stat ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_00.grib2 \
          ${urma_dir}/${mod}.t${HH}z.prepbufr.tm00.nr.nc \
          ${config_dir}/PointStatConfig_${dom_out}_ADPSFC_${mod}_${fld} -outdir $stat_dir/${dom_out}/${exp}/${fld} -v 2
          export err=$? ; err_chk
          if [ ${err} -ne 0 ]; then
              ${ECHO} "point_stat crashed! Occurred at exp=${exp} and fld=${fld}.  Exit status=${err}"
              exit ${err}
          fi
  done
done


# COPY statistics data file of verification to COM2 for archive
cp -p $stat_dir/${dom_out}/${exp}/${fld}/* ${COMOUTverif_rtma3d}/

done
exit ${err}
