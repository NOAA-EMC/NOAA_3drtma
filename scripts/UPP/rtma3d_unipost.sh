#!/bin/sh -l

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


##########################################################################
#                                                                        # 
#  User Defined variables                                                #
#                                                                        #
##########################################################################
sysname=$RUN
SYSNAME=`echo ${sysname} | tr '[:lower:]' '[:upper:]'`
expname=$RUN.$CDATE

#------------------------------------------------------------------#
#
# set up of Analysis Time 
FCST_TIME=$cyc

START_TIME=$CDATE
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

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
MM=`${DATE} +"%M" -d "${START_TIME}"`
JJJ=`${DATE} +"%j" -d "${START_TIME}"`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
echo " time_str = ${time_str}"
time_run=${time_str}

#------------------------------------------------------------------#

# Where the data is stored
DATAHOME_OBS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/obs
DATAHOME_FGS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/fgs
OBSDIR=${DATAHOME_OBS}/${YYYYMMDD}
FGSDIR=${DATAHOME_FGS}/${YYYYMMDD}

# where the test is going to run
PDATABASE=/scratch3/NCEPDEV/stmp2/${USER}
TESTROOT=${PDATABASE}/wrkdir_${sysname}/${cyc}
RUNDIR=${TESTROOT}/postprd_${expname}


# where the system (exe, static, script, etc.) is placed

# export CRTM="/lfs3/projects/nrtrr/alexander/code/contrib/nceplibs/nwprod/lib/sorc/crtm_v2.0.7/fix"
export CRTM="/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.3/fix_update"
FIXCRTM=$CRTM


export DATAHOME=$RUNDIR
export MODEL="RAP"
export DATAWRFHOME=$COMOUT
# export DATAWRFHOME="/scratch3/NCEPDEV/stmp1/Gang.Zhao/wrkdir_rtma3d/com2/rtma3d/retro6_111/rtma3d.20180501/fgsprd.t18z"
export STATIC_DIR="/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/testrun/static/UPP"
export STATICWRF_DIR="/scratch4/NCEPDEV/meso/save/Gang.Zhao/GSD_GSI_dev/rapid-refresh/WRFV3.7.1"
# ${LN} -sf ${DATAWRFHOME}/hrrr.t19z.wrfguess_rap   ${DATAWRFHOME}/wrf_inout

##########################################################################

# Print run parameters
echo
echo "unipost.ksh started at `${DATE}`"
echo
echo "DATAHOME = ${DATAHOME}"
echo "     EXE_ROOT = ${EXE_ROOT}"



# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  echo "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Print out times
echo "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
echo "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H -d "${START_TIME}"`

# Set up the work directory and cd into it
#DATA=${DATAHOME}/${FCST_TIME}
#${RM} -rf ${DATA}
#${MKDIR} -p ${DATA}
#cd ${DATA}

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export MP_SHARED_MEMORY=yes
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2


timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

cat > itag <<EOF
${CORE}
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

${RM} -f fort.*
ln -s ${STATIC_DIR}/post_avblflds.xml post_avblflds.xml
ln -s ${STATIC_DIR}/params_grib2_tbl_new params_grib2_tbl_new
ln -s ${STATIC_DIR}/postcntrl.xml postcntrl.xml
ln -s ${STATIC_DIR}/postxconfig-NT.txt postxconfig-NT.txt
ln -s ${STATICWRF_DIR}/run/ETAMPNEW_DATA eta_micro_lookup.dat

#=============================================================================#
for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" ; do
    ln -s "$FIXCRTM/$what.TauCoeff.bin" .
    ln -s "$FIXCRTM/$what.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "$FIXCRTM/${what}Coeff.bin" .
done

for what in  $FIXCRTM/*Emis* ; do
   ln -s $what .
done

#=============================================================================#

export pgm=${POST:-"${RUN}_post"}
runline="${MPIRUN} -np $np ${pgm}"

. prep_step

$runline >>$pgmout 2>errfile
export err=$?; err_chk
cat $pgmout

echo DONE > post_done

# Append entire wrftwo to wrfprs
cat ${DATA}/WRFPRS.GrbF${FCST_TIME} ${DATA}/WRFTWO.GrbF${FCST_TIME} > ${DATA}/WRFPRS.GrbF${FCST_TIME}.new
mv ${DATA}/WRFPRS.GrbF${FCST_TIME}.new ${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
cat WRFNAT.GrbF${FCST_TIME} WRFTWO.GrbF${FCST_TIME} > ${DATA}/WRFNAT.GrbF${FCST_TIME}.new
mv WRFNAT.GrbF${FCST_TIME}.new ${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2

mv ${DATA}/WRFTWO.GrbF${FCST_TIME} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2

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
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00

${ECHO} "unipost.ksh completed at `${DATE}`"

exit 0
