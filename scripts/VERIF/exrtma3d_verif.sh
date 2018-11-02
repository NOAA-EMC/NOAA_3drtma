#!/bin/ksh

##########################################################################
####  UNIX Script Documentation Block                                    #
#                                                                        #
# Script name:          wrapup.sh                                        #
# Script description: - convert output to grads format.                  #
#                                                                        #
# Author:      Manuel Pondeca        Org: NP22         Date: 2005-10-30  #
#                                                                        #
# Script history log:                                                    #
# 2005-10-30  pondeca                                                    #
##########################################################################

set -x



msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"
export cycle=t${cyc}z
setpdy.sh
. PDY

CDATE=$PDY$cyc
YYYYMMDD=$PDY
YYYYMM=`echo $CDATE | cut -c 1-6`
YYYY=`echo $CDATE | cut -c 1-4`

cd $MET_DIR

# must run after URMA 06Z cycle from current day completes
# usually completed by 12:35Z; set cron to run at 13Z



export mod=rap
# uncomment if running in realtime

export hrs="01"
export MODEL=DNG
export exps="hrrr"
export dom_check=$RUN

# NAM grid definitions; new RAP and HRRR definitions (can't use with the old RAP)
export wgrib2def_cs="lambert:265:25:25 233.723448:2345:2539.703 19.228976:1597:2539.703"
export wgrib2def_ak="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"
export wgrib2def_pr="mercator:20 291.804687:177:2500:296.027600 16.828685:129:2500:19.747399"
export wgrib2def_hi="mercator:20 198.474999:321:2500:206.130999 18.072699:225:2500:23.087799"
export wgrib2def_cs_hrrr="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"

# Do not need to do because the winds MET already does this.

 $HOMErtma3d/scripts/VERIF/MET/pb2nc.ksh

for dom1 in $dom_check; do
  
  if [ $dom1 = rtma2p5 ];then
    export dom=conus
    export dom_out="cs"
    export ext="_wexp"
    export bdom="conusnest"
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_cs
  elif [ $dom1 = rtma3d ];then
    export dom=conus
    export dom_out="cs"
    export ext="_wexp"
    export bdom="conusnest"
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_cs
  elif [ $dom1 = akrtma ];then
    export dom=ak
    export dom_out="ak"
    export ext=""
    export bdom="alaskanest"
    export compress="c3 -set_bitmap 1"
    export wgrib2def=$wgrib2def_ak
  elif [ $dom1 = prrtma ];then
    export dom=pr
    export dom_out=$dom
    export ext=""
    export bdom="priconest"
    export compress="jpeg -set_bitmap 1"
    export wgrib2def=$wgrib2def_pr
  elif [ $dom1 = hirtma ];then
    export dom=hi
    export dom_out=$dom
    export ext=""
    export bdom="hawaiinest"
    export compress="jpeg -set_bitmap 1"
    export wgrib2def=$wgrib2def_hi
  fi

#  mkdir -p $stat_dir/${dom_out}
  export dom

  export dng_dir=$DATA_POST  

  $HOMErtma3d/scripts/VERIF/MET/point_stat_hrrr.ksh

# only need for WCOSS scripts

done

exit
