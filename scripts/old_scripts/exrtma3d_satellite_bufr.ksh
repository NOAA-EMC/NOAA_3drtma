#!/bin/ksh 

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile

# Set endian conversion options for use with Intel compilers
# export F_UFMTENDIAN="big;little:10,15,66"
# export GMPIENVVAR=F_UFMTENDIAN
# export MV2_ON_DEMAND_THRESHOLD=256

# Set the path to the gsi executable
NASALARC=${GSI_ROOT}/rap_process_cloud
fixdir=${FIX_ROOT}


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

# Link to the prepbufr data
${LN} -s ${NASALARC_DATA}/${YYYYJJJHH}00.rap.t${HH}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr

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
