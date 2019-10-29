#!/bin/ksh --login
set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_cloud} ] ; then
  ${ECHO} "ERROR: NASA cloud obs prcoessing executable '${EXECrtma3d}/${exefile_name_cloud}' does not exist!"
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

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# BUFR Table including the description for HREF
${LN} ${FIXgsi}/prepobs_prep_RAP.bufrtable ./prepobs_prep.bufrtable
if [ ! -s "./prepobs_prep.bufrtable" ]; then
  ${ECHO} "prepobs_prep.bufrtable does not exist or not readable"
  exit 1
fi

# WPS GEO_GRID Data
${LN} -s ${FIXwps}/hrrr_geo_em.d01.nc ./geo_em.d01.nc 
if [ ! -s "./geo_em.d01.nc" ]; then
  ${ECHO} "geo_em.d01.nc does not exist or not readable"
  exit 1 
fi

# print parameters for linking/processing
${ECHO} "START_TIME: "${START_TIME}
${ECHO} "SUBH_TIME: "${SUBH_TIME}
${ECHO} "YYYYMMDDHH: "${YYYYMMDDHH}

# Link to the NASA LaRC cloud data
if [ "${HH}" = 12 ] || [ "${HH}" = "00" ] ; then
  ${LN} -s ${NASALARC_DATA}/${YYYYJJJHH}00.rap_e.t${HH}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr
else
  ${LN} -s ${NASALARC_DATA}/${YYYYJJJHH}00.rap.t${HH}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr
fi
if [ ! -s "NASA_LaRC_cloud.bufr" ]; then
  ${ECHO} "./NASA_LaRC_cloud.bufr does not exist or not readable"
  exit 1
fi

# Build the namelist on-the-fly
${CAT} << EOF > namelist_nasalarc
&SETUP
  analysis_time = ${YYYYMMDDHH},
  bufrfile='NASALaRCCloudInGSI.bufr',
  npts_rad=3,
  ioption = 2,
/
EOF

# Run obs processor
export pgm="rtma3d_process_cloud"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin processing NASA LaRC cloud data"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

${CP} ${EXECrtma3d}/${exefile_name_cloud} ${pgm}
${MPIRUN} ${pgm} > ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

targetfile="NASALaRCCloudInGSI.bufr"
if [ -f ${DATA}/${targetfile} ] ; then
  if [ "${envir}" == "esrl" ]; then
    mv ${DATA}/${targetfile} ${COMINobsproc_rtma3d}/${tz_str}.${targetfile} #to save disk space
    ${LN} -snf ${COMINobsproc_rtma3d}/${tz_str}.${targetfile} ${DATA}/${targetfile}
  else
    cpreq ${DATA}/${targetfile} ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.${targetfile}
  fi
else
  msg="WARNING $pgm terminated normally but ${DATA}/${targetfile} does NOT exist."
  ${ECHO} "$msg"
  postmsg "$jlogfile" "$msg"
  exit 1
fi

exit 0
