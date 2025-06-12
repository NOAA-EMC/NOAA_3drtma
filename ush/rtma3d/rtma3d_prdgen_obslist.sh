#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rtma_prdgen_obslist.sh
# Script description:  Run RTMA "obslist" product generation job
#
# Author: Manuel Pondeca  Org: NOAA/EMC         Date: 2024-10-18
#
# Abstract: This script runs the RTMA PRDGEN jobs
#
# Script history log:
# 2024-10-18  Annette Gibbs
#

set -x

cyc=$1
CDATE=$2
DATAobslist=$3
COMIN=$4
COMOUT=$5
USHrtma=$6
RUN=$7      #rtma or urma
EXECdir=$8
fixdir=$9
parmdir=${10}
NET=${11}

cd $DATAobslist

cat << EOF > do_steps_input
&do_steps
  gustadjust=.false.
  howvadjust=.false.
  sfcfld_donwnscale=.false.
  finalgrib2file=.false.
  sfcobs_lists=.true.
  airportlist=.true.
/
EOF

list="hrrr"    #
for item in $list ; do
  cp $fixdir/${RUN}/${RUN}_select_stnlist_${item}.dat select_stnlist_${item}.dat
  cp $fixdir/${RUN}/${RUN}_non_viable_stnlocation_list_${item}.dat non_viable_stnlocation_list_${item}.dat
done

##GZ:==> using the uncompressed tarball of obs-diag files with suffix tar (instead of tgz)
# tar -xzvf $COMIN/gsiprd.t${cyc}z/diag_${CDATE}${cyc}.tgz      # compressed tarball (tgz)
  tar -xvf  $COMIN/gsiprd.t${cyc}z/diag_${CDATE}${cyc}.tar      # uncompressed tarball (tar)
cp diag_conv_ges.${CDATE}${cyc} diag_conv_ges.dat
cp diag_conv_02.${CDATE}${cyc} diag_conv_02.dat
cp diag_conv_anl.${CDATE}${cyc} diag_conv_anl.dat
cp $COMIN/postprd.t${cyc}z/${RUN}.t${cyc}z.wrfsubhprs.grib2 anlfile_2.grb2

FAA_TIME_START=`$MDATE`
echo "FAA_TIME_START" $FAA_TIME_START
iyyyy0=`echo $FAA_TIME_START  |cut -c1-4`
imm0=`echo   $FAA_TIME_START  |cut -c5-6`
idd0=`echo   $FAA_TIME_START  |cut -c7-8`
ihh0=`echo   $FAA_TIME_START  |cut -c9-10`
imin0=`echo  $FAA_TIME_START  |cut -c11-12`

FAA_TIME_END=`$NDATE +01 $iyyyy0$imm0$idd0$ihh0`$imin0
echo "FAA_TIME_END" $FAA_TIME_END
iyyyyp1=`echo $FAA_TIME_END  |cut -c1-4`
immp1=`echo   $FAA_TIME_END  |cut -c5-6`
iddp1=`echo   $FAA_TIME_END  |cut -c7-8`
ihhp1=`echo   $FAA_TIME_END  |cut -c9-10`
iminp1=`echo  $FAA_TIME_END  |cut -c11-12`

cat << EOF > faa_related_input
&faa_grid_and_anlfile_info
    ngrids=1
    cgridset(1)='hrrr'
    anlfileset(1)='anlfile_2.grb2'
    stnlistset(1)='select_stnlist_hrrr.dat'
    non_viableset(1)='non_viable_stnlocation_list_hrrr.dat'
    outfileset(1)='stn_analysis_values_hrrr.dat'
    outsideset(1)='stn_outside_of_domain_hrrr.dat'
/
&faa_timerange
    iyyyy0=$iyyyy0
    imm0=$imm0
    idd0=$idd0
    ihh0=$ihh0
    imin0=$imin0
    iyyyy=$iyyyy0
    imm=$imm0
    idd=$idd0
    ihh=$ihh0
    imin=$imin0
    iyyyyp1=$iyyyyp1
    immp1=$immp1
    iddp1=$iddp1
    ihhp1=$ihhp1
    iminp1=$iminp1
    thiscdate=$CDATE$cyc
/
&faa_stninterp_method
/
EOF

mpiexec -n 1 -ppn 1 $EXECdir/rtma_obslist >>$pgmout 2>errfile
##GZ:==> adding error-trap of the running of rtma_obslist
export err=$?
if [ ${err} -ne 0 ] ; then
   echo "rtma_obslist (in rtma3d_prdgen_obslist.sh) failed, abort ..."
   exit ${err}       # return the non-zero code to upper-level script
fi

# Copy files to $COMOUT 

list="ps t q u v w spd gust howv vis"
  for type in $list
  do
    cp ${type}_obs.listing_iter_ges $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_01
    cp ${type}_obs.listing_iter_ges_aux $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_01_aux
    cp ${type}_obs.listing_iter_02 $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_02
    cp ${type}_obs.listing_iter_02_aux $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_02_aux
    cp ${type}_obs.listing_iter_anl $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_anl
    cp ${type}_obs.listing_iter_anl_aux $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_anl_aux

    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_01
    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_01_aux
    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_02
    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_02_aux
    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_anl
    chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${type}_obs.listing_iter_anl_aux
  done

list="hrrr"
  for type in $list
  do
    cp stn_analysis_values_${type}.dat $COMOUT/${RUN}.t${cyc}z.stn_analysis_values_${type}.dat
  done

