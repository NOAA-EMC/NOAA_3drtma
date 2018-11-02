#!/bin/sh -l
# Use the if clause so that this script stays portable
#

set -x

postmsg $jlogfile "$0 of $job has begun on `hostname`"

if [ -z "$WGRIB2" ]; then
    err_exit "\$WGRIB2 must be set to the location of the wgrib2 executable!"
fi

echo "Start Time is `date` "


cd $DATA

##########################################################################
#                                                                        # 
#  User Defined variables                                                #
#                                                                        #
##########################################################################

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
ANLS_TIME="$PDYHH"        # <-- define analysis date and time (YYYYMMDDHH)
FCST_TIME="$cyc"
FCST_TIME_fgs="$FCST_TIME"

START_TIME=$ANLS_TIME
# Make sure START_TIME is defined and in the correct format (YYYYMMDD HH)
if [ ! "${START_TIME}" ]; then
  echo "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `echo "${START_TIME}" | awk '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`echo "${START_TIME}" | awk '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    echo "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME} ${subcyc} minutes"`
fi

ANLS_CYC_TIME=`date --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`date +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`date +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`date +"%Y%m%d" -d "${START_TIME}"`
YYYY=`date +"%Y" -d "${START_TIME}"`
MM=`date +"%m" -d "${START_TIME}"`
DD=`date +"%d" -d "${START_TIME}"`
HH=`date +"%H" -d "${START_TIME}"`
mm=`date +"%M" -d "${START_TIME}"`

#------------------------------------------------------------------#

# Where the data is stored

# where the test is going to run
if [  "${DATA_POST}" ]; then
  ${RM} -rf ${DATA_POST}
  ${LN} -sf ${DATA} ${DATA_POST}
fi



# where the system (exe, static, script, etc.) is placed

export pgm=${POST:-"${RUN}_post"}

# using Guoqing Ge's static in branch of GSD (in 3DRTMA repo)

##########################################################################

# Print run parameters
echo
echo "unipost.ksh started at `date`"
echo
echo "DATAHOME = ${DATAHOME}"



# Check to make sure the post executable exists
if [ ! -x ${POST} ]; then
  echo "ERROR: ${POST} does not exist, or is not executable"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  echo "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Print out times
echo "   START TIME = "`date +%Y%m%d%H -d "${START_TIME}"`
echo "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`date +%Y%m%d%H -d "${START_TIME}"`


# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export RSTFNL=${DATA}/
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2
export VALIDTIMEUNITS=FMIN
export CORE=RAPR
timestr=`date +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`date +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

fullpath=$COMOUT/gsianl_wrf_inout_d01_$timestr

cat > itag <<EOF
$fullpath
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

${RM} -f fort.*
ln -s ${PARMrtma3d}/post_avblflds.xml post_avblflds.xml
ln -s ${PARMrtma3d}/params_grib2_tbl_new params_grib2_tbl_new
ln -s ${PARMrtma3d}/postcntrl.xml postcntrl.xml
ln -s ${PARMrtma3d}/postxconfig-NT.txt postxconfig-NT.txt
ln -s ${PARMrtma3d}/ETAMPNEW_DATA eta_micro_lookup.dat


#=============================================================================#
for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" ; do
    ln -s "$FIX_CRTM/$what.TauCoeff.bin" .
    ln -s "$FIX_CRTM/$what.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "$FIX_CRTM/${what}Coeff.bin" .
done

for what in  $FIX_CRTM/*Emis* ; do
   ln -s $what .
done

#=============================================================================#

# Run unipost
runline="${MPIRUN} -np $np ${pgm}"

. prep_step

$runline >>$pgmout 2>errfile
export err=$?; err_chk
if [ ${err} -ne 0 ]; then
  echo "${POST} crashed!  Exit status=${error}"
  exit ${err}
fi

cat $pgmout

echo DONE > post_done

# Append entire wrftwo to wrfprs
cat ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs} ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} > ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs}.new
mv ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs}.new ${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
cat WRFNAT.GrbF${FCST_TIME_fgs} WRFTWO.GrbF${FCST_TIME_fgs} > ${DATA}/WRFNAT.GrbF${FCST_TIME_fgs}.new
mv WRFNAT.GrbF${FCST_TIME_fgs}.new ${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2

# keep wrftwo
# mv ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2
cp -p ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2" ]; then
  echo "unipost crashed! wrfprs_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2" ]; then
  echo "unipost crashed! wrftwo_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2" ]; then
  echo "unipost crashed! wrfnat_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi

# Move the output files to postprd
mv ${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2
mv ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2
mv ${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2

# Create softlinks for transfer
basetime=`date +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00

echo "unipost.ksh completed at `date`"

exit 0
