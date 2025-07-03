#!/bin/ksh 
set -x

if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then

export OMP_NUM_THREADS=1

# Make sure we are using GMT time zone for time computations
export TZ="GMT"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
# ANLS_TIME=${PDY}' '${cyc}
ANLS_TIME=${ANLS_TIME:-"${PDY} ${cyc}"}            # YYYYMMDD HH
echo $PDY $cyc 
# cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"                 # <-- forecast time (hour) to provide fgs for rtma

# For RTMA there is no forecast, so START_TIME is ANLS_TIME
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
  START_TIME=`${DATE} -d "${START_TIME} ${FCST_TIME} minutes"`
fi

ANLS_CYC_TIME=`${DATE} --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`
FCST_INI_TIME=`${DATE} --date="${START_TIME} -${FCST_TIME} hour " +"%Y%m%d%H%M"`
export WGRIB2=/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.8_wmo/bin/wgrib2
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

HH_fcstinit=`${ECHO} ${FCST_INI_TIME} | cut -c9-10 `
#------------------------------------------------------------------#

export DATAHOME=$DATA
export CORE="RAPRRTMA"
export MODELNAME="RAPR"
export SUBMODELNAME="RTMA"
export fileNameFlux="DUMMY"
export fileNameFlat="DUMMY"
export DATAWRFHOME=${COMOUTgsi_rtma3d:-"$COMIN"}
export DATAWRFFILE=${ANLrtma3d_FNAME:-"${NET}.t${cyc}z.anl.wrf_inout.nc"}

##########################################################################

# Check to make sure the post executable exists
#if [ ! -x ${EXECrtma3d}/${exefile_name_post} ]; then
#  ${ECHO} "ERROR: ${EXECrtma3d}/${exefile_name_post} does not exist, or is not executable"
#  exit 1
#fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Check to make sure that the DATAWRFHOME is defined
if [ ! ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: DATAWRFHOME, \$DATAWRFHOME, is not defined."
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! -d ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME, does not exist."
  exit 1
fi

if [ ! -f ${DATAWRFHOME}/${DATAWRFFILE} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME/${DATAWRFFILE}, does not exist."
  exit 1
fi



# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

#
# Set up some constants for UPP namlist itag
#

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`

cat > itag <<EOF
&model_inputs
fileName='${DATAWRFHOME}/${DATAWRFFILE}'
IOFORM='netcdf'
grib='grib2'
DateStr='${timestr2}'
MODELNAME='RAPR'
SUBMODELNAME='RTMA'
/
&NAMPGB
KPO=47,PO=2.,5.,7.,10.,20.,30.,50.,70.,75.,100.,125.,150.,175.,200.,225.,250.,275.,300.,325.,350.,375.,400.,425.,450.,475.,500.,525.,550.,575.,600.,625.,650.,675.,700.,725.,750.,775.,800.,825.,850.,875.,900.,925.,950.,975.,1000.,1013.2
/
EOF

${RM} -f fort.*
${RM} -f params_grib2_tbl_new postxconfig-NT.txt eta_micro_lookup.dat
${RM} -f WRF???.GrbF??


  CP_LN="${LN} -sf"
#link/copy parameter files
${CP_LN} ${PARMupp}/params_grib2_tbl_new params_grib2_tbl_new
${CP_LN} ${PARMupp}/postxconfig-NT-3drtma.txt postxconfig-NT.txt
${CP_LN} ${PARMupp}/rap_micro_lookup.dat ./eta_micro_lookup.dat
${CP_LN} ${FIXcrtm}/* .


#=============================================================================#
#
# Run unipost
#
export pgm=${NET}_upp
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin Uni-Post step for 3DRTMA GSI Analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"


#copy executable to running directory
#${CP} ${EXECrtma3d}/${exefile_name_post} ./rtma3d_wrfpost
export APRUN="mpiexec -l -n 128 -ppn 128"
runline="${APRUN} ${EXECrtma3d}/${pgm}"
$runline < itag > ${pgmout} 2>errfile
export err=$? ; err_chk

if [ ${err} -ne 0 ]; then
  ${ECHO} "rtma3d_wrfpost crashed!  Exit status=${err}"
  exit ${err}
fi

# Linking GrbF{HH} to GrbF00 (esp. for firstguess which is from WRF forecast)
GrbFiles=`ls WRF???.GrbF??`
for i in ${GrbFiles}
do
  i_fname=`echo "$i" | cut -d '.' -f 1`
  i_extname=`echo "$i" | cut -d '.' -f 2`
  if [[ "${i_extname}" != "GrbF${FCST_TIME}" ]]
  then
    ln -sf $i    ./${i_fname}.GrbF${FCST_TIME}
  fi
done

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfsubhprs.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfsubhnat.grib2


# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfsubhprs.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhprs.grib2 is missing"
  exit 1
fi
#if [ ! -s "${workdir}/wrfsubhspl.grib2" ]; then
#  ${ECHO} "unipost crashed! wrfsubhspl.grib2 is missing"
#  exit 1
#fi
if [ ! -s "${workdir}/wrfsubhnat.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhnat.grib2 is missing"
  exit 1
fi

# transfer the output grib2 files to $COMOUTpost_rtma3d
# add gust,vis, and howv (wave height) to the prslev and natlev files
# change name from surface gust to 10-m gust

# Note: NET should be RUN - AMG
# Change derived visibility (VIS:surface) to 2m visibility (VIS:2 m above ground)
wgrib2 ${workdir}/wrfsubhprs.grib2 -match "VIS" -if "VIS:surface" -set_lev "2 m above ground" -grib ${workdir}/vis2mprs.grib2 -fi
wgrib2 ${workdir}/wrfsubhnat.grib2 -match "VIS" -if "VIS:surface" -set_lev "2 m above ground" -grib ${workdir}/vis2mnat.grib2 -fi
# Change analyzed gust from GUST:surface to GUST:10 m above ground
${WGRIB2} -V ${COMOUT}/${RUN}.t${cyc}z.anl.gust.grib2 -set_lev "10 m above ground" -grib tmpgust.grib2
# Remove surface visibility from grib2 files
wgrib2 ${workdir}/wrfsubhprs.grib2 -not_if "VIS:surface" -grib ${workdir}/wrfsubhprs.grib2_no_sfcvis 
wgrib2 ${workdir}/wrfsubhnat.grib2 -not_if "VIS:surface" -grib ${workdir}/wrfsubhnat.grib2_no_sfcvis
# Overwrite wrfsubhprs and wrfsubhnat with renamed derived vis, analyzed vis, analyzed gust and analyzed wave height
if [ "${RUN}" == "rtma3d" ]; then
  cat ${workdir}/wrfsubhprs.grib2_no_sfcvis ${workdir}/vis2mprs.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.vis.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.howv.grib2 > ${workdir}/wrfsubhprs.grib2
  cat ${workdir}/wrfsubhnat.grib2_no_sfcvis ${workdir}/vis2mnat.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.vis.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.howv.grib2 > ${workdir}/wrfsubhnat.grib2
else
  cat ${workdir}/wrfsubhprs.grib2_no_sfcvis ${workdir}/vis2mprs.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.vis.grib2 > ${workdir}/wrfsubhprs.grib2
  cat ${workdir}/wrfsubhnat.grib2_no_sfcvis ${workdir}/vis2mprs.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.vis.grib2 > ${workdir}/wrfsubhnat.grib2
fi

${WGRIB2} ${workdir}/wrfsubhprs.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhprs.grib2
${WGRIB2} ${workdir}/wrfsubhnat.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhnat.grib2
${WGRIB2} ${workdir}/wrfsubhprs.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD}.anl_prslev.grib2
${WGRIB2} ${workdir}/wrfsubhnat.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD}.anl_natlev.grib2
# Create index file
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.anl_prslev.grib2 -s > ${RUN}.t${cyc}z.anl_prslev.grib2.idx
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.anl_natlev.grib2 -s > ${RUN}.t${cyc}z.anl_natlev.grib2.idx
cp ${RUN}.t${cyc}z.anl_prslev.grib2.idx $COMOUT/
cp ${RUN}.t${cyc}z.anl_natlev.grib2.idx $COMOUT/



#================================================================================#
  ${RM} -f  ${workdir}/wrfsubh???.grib2
  ${RM} -f  ${workdir}/WRF???.GrbF??

#================================================================================#

${ECHO} "unipost completed at `${DATE}`"

fi



exit 0
