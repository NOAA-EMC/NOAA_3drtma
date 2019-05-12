#!/bin/ksh --login

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile

# Set endian conversion options for use with Intel compilers
# export F_UFMTENDIAN="big;little:10,15,66"
# export GMPIENVVAR=F_UFMTENDIAN
# export MV2_ON_DEMAND_THRESHOLD=256

# Set the path to the gsi executable
NASALARC=${GSI_ROOT}/rap_process_cloud
fixdir=${FIX_ROOT}

# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME}" ]; then
  ${ECHO} "NOTE: DATAHOME directory '${DATAHOME}' does not exist! make one!"
fi

if [ ! "${DATAROOT}" ]; then
  ${ECHO} "ERROR: \$DATAROOT is not defined!"
  exit 1
fi
if [ ! -d "${DATAROOT}" ]; then
  ${ECHO} "ERROR: DATAROOT directory '${DATAROOT}' does not exist!"
  exit 1
fi

if [ ! "${NASALARC_DATA}" ]; then
  ${ECHO} "ERROR: \$NASALARC_DATA is not defined!"
  exit 1
fi
if [ ! -d "${NASALARC_DATA}" ]; then
  ${ECHO} "ERROR: NASALARC_DATA directory '${NASALARC_DATA}' does not exist!"
  exit 1
fi

# Make sure GSI_ROOT is defined and exists
if [ ! "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: \$GSI_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: GSI_ROOT directory '${GSI_ROOT}' does not exist!"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
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
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Make sure the GSI executable exists
if [ ! -x "${NASALARC}" ]; then
  ${ECHO} "ERROR: ${NASALARC} does not exist!"
  exit 1
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

if [ -x "rap_process_cloud" ]; then
  ${RM} rap_process_cloud
fi
if [ -s "NASALaRCCloudInGSI_bufr.bufr" ]; then
  ${RM} NASALaRCCloudInGSI_bufr.bufr
fi

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYJJJ=`${DATE} +"%Y%j" -d "${START_TIME}"`
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
YYYYJJJ12=`${DATE} +"%Y%j" -d "${START_TIME} 1 hours ago "`
HH=`${DATE} +"%H" -d "${START_TIME}"`
HH12=`${DATE} +"%H" -d "${START_TIME} 1 hours ago "`

cp ${fixdir}/prepobs_prep_RAP.bufrtable  ./prepobs_prep.bufrtable

# Save a copy of the GSI executable in the workdir
${CP} ${NASALARC} .

${LN} -s ${STATIC_DIR}/geo_em.d01.nc .

# Link to the NASA LaRC cloud data
if [ "${HH}" = 12 ] || [ "${HH}" = "00" ] ; then
  ${LN} -s ${NASALARC_DATA}/${YYYYJJJHH}00.rap_e.t${HH}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr
else
  ${LN} -s ${NASALARC_DATA}/${YYYYJJJHH}00.rap.t${HH}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr
fi

# echo ${YYYYMMDDHH} > nasaLaRC_cycle_date
# Build the namelist on-the-fly
${CAT} << EOF > namelist_nasalarc
&SETUP
  analysis_time = ${YYYYMMDDHH},
  bufrfile='NASALaRCCloudInGSI.bufr',
  npts_rad=3,
  ioption = 2,
/
EOF


#  Run obs pre-processor
${MPIRUN} ${NASALARC} > stdout_satellite_bufr 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${NASALARC} crashed  Exit status=${error}"
  exit ${error}
fi

exit 0
