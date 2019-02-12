#!/bin/bash

set -x 

#-- Testing the status of some important variables. --#
# Make sure these variables for key directories are defined and exists
if [ ! "${OBS_DIR_CYCLE}" ]; then
  ${ECHO} "ERROR: \${OBS_DIR_CYCLE} is not defined!"
  exit 1
fi
if [ ! -d "${OBS_DIR_CYCLE}" ]; then
  ${ECHO} "ERROR: $OBS_DIR_CYCLE does not exist!"
  exit 1
fi

if [ ! "${COMINobsproc_rtma3d}" ]; then
  ${ECHO} "ERROR: \$COMINobsproc_rtma3d is not defined!"
  exit 1
fi
if [ ! -d "${COMINobsproc_rtma3d}" ]; then
  ${ECHO} "ERROR: $COMINobsproc_rtma3d does not exist!"
  exit 1
fi

if [ ! "${DATA}" ]; then
  ${ECHO} "ERROR: \$DATA is not defined!"
  exit 1
fi
if [ ! -d "${DATA}" ]; then
  ${ECHO} "ERROR: $DATA does not exist!"
  exit 1
fi

if [  "${DATA_OBSPRD}" ]; then
  ${RM} -f  ${DATA_OBSPRD}
  ${LN} -sf ${DATA} ${DATA_OBSPRD}
fi

#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
echo $cyc
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

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
#############################################################################

# Create the working directory and cd into it
workdir=${DATA}
cd ${workdir}

time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin copy bufr format obs data to obsproc.${cycle}/"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# copy/link the prepbufr obs data
# prepbufr obs
cpreq ${OBS_DIR_CYCLE}/newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr ${COMINobsproc_rtma3d}/newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
${LN} -sf ${COMINobsproc_rtma3d}/newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr ${DATA}/newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr

# MRMS MOSAIC RADAR data (pre-processed bufr data/remapped on model grid )
if [ -r "${OBS_DIR_CYCLE}/NSSLRefInGSI.bufr" ]; then
  cpreq ${OBS_DIR_CYCLE}/NSSLRefInGSI.bufr ${COMINobsproc_rtma3d}
  ${LN} -sf ${COMINobsproc_rtma3d}/NSSLRefInGSI.bufr ${DATA}/NSSLRefInGSI.bufr
else
  ${ECHO} "Warning: ${OBS_DIR_CYCLE}/NSSLRefInGSI.bufr dones not exist!"
fi

# lightning obs (pre-processed/re-mapped to model grid)
if [ -r "${OBS_DIR_CYCLE}/LightningInGSI.bufr" ]; then
  cpreq  ${OBS_DIR_CYCLE}/LightningInGSI.bufr ${COMINobsproc_rtma3d}
  ${LN} -sf ${COMINobsproc_rtma3d}/LightningInGSI.bufr ${DATA}/LightningInGSI.bufr
else
  ${ECHO} "Warning: ${OBS_DIR_CYCLE}/LightningInGSI.bufr dones not exist!"
fi

# NASA LaRC Cloud data (pre-processed/re-mapped to model grid)
if [ -r "${OBS_DIR_CYCLE}/NASALaRCCloudInGSI_bufr.bufr" ]; then
  cpreq  ${OBS_DIR_CYCLE}/NASALaRCCloudInGSI_bufr.bufr ${COMINobsproc_rtma3d}
  ${LN} -sf ${COMINobsproc_rtma3d}/NASALaRCCloudInGSI_bufr.bufr ${DATA}/NASALaRCCloudInGSI_bufr.bufr
else
  ${ECHO} "Warning: ${OBS_DIR_CYCLE}/NASALaRCCloudInGSI_bufr.bufr does not exist!"
  ${ECHO} "Warning: try ${OBS_DIR_CYCLE}/NASALaRCCloudInGSI.bufr!"
  if [ -r "${OBS_DIR_CYCLE}/NASALaRCCloudInGSI.bufr" ]; then
    cpreq  ${OBS_DIR_CYCLE}/NASALaRCCloudInGSI.bufr ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/NASALaRCCloudInGSI.bufr ${DATA}/NASALaRCCloudInGSI.bufr
  else
    ${ECHO} "Warning: ${OBS_DIR_CYCLE}/NASALaRCCloudInGSI.bufr does not exist!"
  fi
fi

# statellite radiance data

# the radial velocity data

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle


export err=$? ; err_chk

ls -l ${COMINobsproc_rtma3d} > ${COMINobsproc_rtma3d}/obs_data_${PDY}_t${cycle}_${subcyc}.list

exit 0
