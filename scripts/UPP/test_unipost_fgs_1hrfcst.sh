#!/bin/sh -l
# Use the if clause so that this script stays portable
#
set -x

#np=`cat $PBS_NODEFILE | wc -l`
# np=$PBS_NP
#POST_PROC=${np}
#echo "number of total processes is ${np} $PBS_NP ${POST_PROC}"
#export OMP_NUM_THREADS=1

# module load newdefaults
# module list
#module load intel
#module load impi
#module load netcdf
#module load cnvgrib
#module list

# Make sure we are using GMT time zone for time computations
#export TZ="GMT"

# Set up paths to unix commands
DATE=/bin/date
ECHO=/bin/echo
AWK="/bin/gawk --posix"
SED=/bin/sed
WC=/usr/bin/wc
CUT=/bin/cut
MKDIR=/bin/mkdir
CAT=/bin/cat
LS=/bin/ls
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
RM=/bin/rm
TAIL=/usr/bin/tail
BC=/usr/bin/bc

CNVGRIB=/apps/cnvgrib/1.4.0/bin/cnvgrib
MPIRUN=mpirun
NCDUMP=ncdump
# WGRIB2=${EXE_ROOT}/wgrib2_new
cd $DATA
##########################################################################
#                                                                        # 
#  User Defined variables                                                #
#                                                                        #
##########################################################################
sysname="rtma3d"
MODEL="RAP"
SYSNAME=`echo ${sysname} | tr '[:lower:]' '[:upper:]'`
expname="test1_master_fgs_1hrfcst"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
ANLS_TIME=$PDY$cyc         # <-- define analysis date and time (YYYYMMDDHH)
subcyc=00                    # <-- define sub cycle time (minute)
cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"
FCST_TIME_fgs="00"

START_TIME=$ANLS_TIME
# Make sure START_TIME is defined and in the correct format (YYYYMMDD HH)
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME} ${subcyc} minutes"`
fi

ANLS_CYC_TIME=`${DATE} --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`
PREV_CYC_TIME=`${DATE} --date="${START_TIME} -${cyc_intvl} " +"%Y%m%d%H%M"`
NEXT_CYC_TIME=`${DATE} --date="${START_TIME} +${cyc_intvl} " +"%Y%m%d%H%M"`

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
mm=`${DATE} +"%M" -d "${START_TIME}"`
JJJ=`${DATE} +"%j" -d "${START_TIME}"`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}
timeform=${YYYY}"-"${MM}"-"${DD}"-"${HH}":00:00"
timeformalt=${YYYY}"-"${MM}"-"${DD}"_"${HH}"_00_00"

HH_cycm1=`${DATE} +"%H" -d "${PREV_CYC_TIME}"`
HH_cycp1=`${DATE} +"%H" -d "${NEXT_CYC_TIME}"`
#------------------------------------------------------------------#

# Where the data is stored
DATAHOME_OBS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/obs
DATAHOME_FGS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/fgs
OBSDIR=${DATAHOME_OBS}/${YYYYMMDD}
FGSDIR=${DATAHOME_FGS}/${YYYYMMDD}

# where the test is going to run
PDATABASE=/scratch3/NCEPDEV/stmp2/${USER}
TESTROOT=${PDATABASE}/wrkdir_${sysname}/${ANLS_CYC_TIME}
RUNDIR=${TESTROOT}/postprd_${expname}


# where the system (exe, static, script, etc.) is placed
SYSROOT=/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev             # <-- modify
RTMA3D_DIR=${SYSROOT}/rtma3d_workflow                              # <-- modify

export CRTM="/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.3/fix_update"
FIXCRTM=$CRTM

export pgm=${POST:-"${RUN}_post"}
export STATIC_DIR="/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/testrun/static/UPP"
export STATICWRF_DIR="/scratch4/NCEPDEV/meso/save/Gang.Zhao/GSD_GSI_dev/rapid-refresh/WRFV3.7.1"

# using Guoqing Ge's static in branch of GSD (in 3DRTMA repo)
export PARMhrrr="/scratch4/NCEPDEV/meso/save/Gang.Zhao/EMC_post_dev/test_post/parm"
export PARMhrrr="/scratch4/NCEPDEV/meso/save/Gang.Zhao/EMC_post_dev/test_post/parm"

##########################################################################

# Print run parameters
${ECHO}
${ECHO} "unipost.ksh started at `${DATE}`"
${ECHO}
${ECHO} "DATAHOME = ${DATAHOME}"

# Set up some constants
if [ "${MODEL}" == "RAP" ]; then
  export CORE=RAPR
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  export CORE=NMM
fi


# Check to make sure the post executable exists
if [ ! -x ${POST} ]; then
  ${ECHO} "ERROR: ${POST} does not exist, or is not executable"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H -d "${START_TIME}"`


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

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

fullpath=$COMOUT/gsianl_wrf_inout_d01_$timestr

${CAT} > itag <<EOF
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
# ln -s ${STATIC_DIR}/post_avblflds.xml       post_avblflds.xml
ln -s ${PARMhrrr}/hrrr_post_avblflds.xml      post_avblflds.xml

# ln -s ${STATIC_DIR}/params_grib2_tbl_new    params_grib2_tbl_new
ln -s ${PARMhrrr}/hrrr_params_grib2_tbl_new   params_grib2_tbl_new

# ln -s ${STATIC_DIR}/postcntrl.xml           postcntrl.xml
ln -s ${PARMhrrr}/hrrr_postcntrl.xml          postcntrl.xml

# ln -s ${STATIC_DIR}/postxconfig-NT.txt      postxconfig-NT.txt
# ln -s ${PARMhrrr}/hrrr_postxconfig-NT.txt   postxconfig-NT.txt
ln -s ${PARMhrrr}/postxconfig-NT-HRRR.txt     postxconfig-NT.txt


if [ "${MODEL}" == "RAP" ]; then
  ln -s ${PARMhrrr}/rap_micro_lookup.dat      eta_micro_lookup.dat
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  ln -s ${PARMhrrr}/nam_micro_lookup.dat      eta_micro_lookup.dat
fi

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

# Run unipost
runline="${MPIRUN} -np $np ${pgm}"

. prep_step

$runline >>$pgmout 2>errfile
export err=$?; err_chk
if [ ${err} -ne 0 ]; then
  ${ECHO} "${POST} crashed!  Exit status=${error}"
  exit ${err}
fi

${CAT} $pgmout

${ECHO} DONE > post_done

# Append entire wrftwo to wrfprs
${CAT} ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs} ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} > ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs}.new
${MV} ${DATA}/WRFPRS.GrbF${FCST_TIME_fgs}.new ${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
${CAT} WRFNAT.GrbF${FCST_TIME_fgs} WRFTWO.GrbF${FCST_TIME_fgs} > ${DATA}/WRFNAT.GrbF${FCST_TIME_fgs}.new
${MV} WRFNAT.GrbF${FCST_TIME_fgs}.new ${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2

# keep wrftwo
# ${MV} ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2
${CP} -p ${DATA}/WRFTWO.GrbF${FCST_TIME_fgs} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfprs_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrftwo_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfnat_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi

# Move the output files to postprd
${MV} ${DATA}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2
${MV} ${DATA}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2
${MV} ${DATA}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2
# ${RM} -rf ${DATA}

# Create softlinks for transfer
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00

${ECHO} "unipost.ksh completed at `${DATE}`"

exit 0
