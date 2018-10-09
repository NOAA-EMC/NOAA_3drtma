#!/bin/ksh

set -x

postmsg $jlogfile "$0 of $job has begun on `hostname`"

if [ -z "$WGRIB2" ]; then
    err_exit "\$WGRIB2 must be set to the location of the wgrib2 executable!"
fi

echo "Start Time is `date` "

cd $DATA

rm -f fort.*

CDATE=$PDY$cyc
JDAY=`date2jday.sh $PDY` 

#YYYY=`echo ${CDATE} | cut -c 1-4`
#MM=`echo ${CDATE} | cut -c 5-6`
#DD=`echo ${CDATE} | cut -c 7-8`
#HH=`echo ${CDATE} | cut -c 9-10`


YYYY=2018
MM=10
DD=09
HH=11

timeform=${YYYY}"-"${MM}"-"${DD}"-"${HH}":00:00" 
#timeformalt=${YYYY}"-"${MM}"-"${DD}"_"${HH}"_00_00"


export XLFRTEOPTS="unit_vars=yes"
export MP_SHARED_MEMORY=yes
export RSTFNL=${DATA}/
export tmmark=tm00
#fullpath=$COMOUT/gsianl_wrf_inout_d01_$timeformalt
fullpath=$COMOUT/hrrr_${YYYY}${MM}${DD}${HH}f001
OUTTYP=netcdf
GTYPE=grib2
export CORE=RAPR
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2

export VALIDTIMEUNITS=FMIN

cp $PARMrtma/hrrr_post_avblflds.xml post_avblflds.xml
cp $PARMrtma/hrrr_params_grib2_tbl_new params_grib2_tbl_new

if [ $cyc -eq 0 -o $cyc -eq 1 ] ; then
cp $PARMrtma/hrrr_postcntrl_anl.xml postcntrl.xml
cp $PARMrtma//hrrr_postxconfig_anl-NT.txt postxconfig-NT.txt
else
cp $PARMrtma/hrrr_postcntrl_anl.xml  postcntrl.xml
cp $PARMrtma/hrrr_postxconfig-NT.txt postxconfig-NT.txt
fi

cp $PARMrtma/hrrr_run_ETAMPNEW_DATA eta_micro_lookup.dat
cp $PARMrtma/hrrr_imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin
cp $PARMrtma/hrrr_imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin
cp $PARMrtma/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
cp $PARMrtma/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
cp $PARMrtma/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
cp $PARMrtma/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
cp $PARMrtma/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
cp $PARMrtma/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
cp $PARMrtma/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
cp $PARMrtma/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
cp $PARMrtma/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
cp $PARMrtma/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
cp $PARMrtma/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
cp $PARMrtma/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
cp $PARMrtma/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
cp $PARMrtma/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
cp $PARMrtma/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
cp $PARMrtma/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
cp $PARMrtma/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
cp $PARMrtma/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
cp $PARMrtma/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
cp $PARMrtma/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
cp $PARMrtma/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
cp $PARMrtma/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
cp $PARMrtma/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
cp $PARMrtma/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
cp $PARMrtma/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
cp $PARMrtma/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
cp $PARMrtma/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
cp $PARMrtma/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
cp $PARMrtma/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
cp $PARMrtma/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
cp $PARMrtma/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
cp $PARMrtma/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
cp $PARMrtma/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
cp $PARMrtma/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
cp $PARMrtma/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
cp $PARMrtma/CloudCoeff.bin CloudCoeff.bin 
cp $PARMrtma/AerosolCoeff.bin AerosolCoeff.bin
cp $PARMrtma/EmisCoeff.bin EmisCoeff.bin


cat > itag <<EOF
$fullpath
$OUTTYP
$GTYPE
$timeform
$CORE
$SPLNUM
$SPL
${VALIDTIMEUNITS}
EOF

export pgm=${POST:-"${RUN}_post"}
runline="${MPIRUN} -np $np ${pgm}"

. prep_step

$runline >>$pgmout 2>errfile
export err=$?; err_chk
cat $pgmout

echo DONE > post_done



########################################################
postmsg $jlogfile "$0 of $job completed normally"
################## END OF SCRIPT #######################
