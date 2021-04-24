#!/bin/ksh

set -x 

#-- Testing the status of some important variables. --#
# Make sure these variables for key directories are defined and exists
if [ ! "${COMINrap}" ]; then
  ${ECHO} "ERROR: \${COMINrap} is not defined!"
  exit 1
fi
if [ ! -d "${COMINrap}" ]; then
  ${ECHO} "ERROR: $COMINrap does not exist!"
  exit 1
fi

#if [ ! "${COMINhrrr}" ]; then
#  ${ECHO} "ERROR: \${COMINhrrr} is not defined!"
#  exit 1
#fi
#if [ ! -d "${COMINhrrr}" ]; then
#  ${ECHO} "ERROR: $COMINhrrr does not exist!"
#  exit 1
#fi

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
mm=`${DATE} +"%M" -d "${START_TIME}"`
#############################################################################

# Create the working directory and cd into it
workdir=${DATA}
cd ${workdir}

time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}
pgm=${RUN}_prepobs
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
# msg="  begin copy bufr format obs data to obsproc.${cycle}/"
msg="  begin copy bufr format obs data to ${COMIN}/obsprd.t${HH}z"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# copy/link the prepbufr obs data
# prepbufr obs
if [ -r ${COMINrap}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr ] ; then
  cpreq ${COMINrap}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr                 ${COMINobsproc_rtma3d}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr
  ${LN} -sf ${COMINobsproc_rtma3d}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr  ${DATA}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr
elif [ -f ${COMINrap}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00 ] ; then
  cpreq ${COMINrap}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00                   ${COMINobsproc_rtma3d}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00
  ${LN} -sf ${COMINobsproc_rtma3d}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00    ${DATA}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00
else
  ${ECHO} "Warning: ${COMINrap}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00 does NOT exist! "
fi

# MRMS MOSAIC RADAR data (pre-processed bufr data/remapped on model grid )
if [ ${obsprep_radar} -eq 0 ] ; then
  ${ECHO} " using hrrr.t${HH}z.NSSLRefInGSI.bufr (used in HRRR)"
  if [ -r ${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr ]; then
    cpreq ${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/hrrr.t${HH}z.NSSLRefInGSI.bufr ${DATA}/hrrr.t${HH}${subcyc}z.NSSLRefInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr dones not exist!"
  fi
else
  ${ECHO} "using processed MRMS mosaic data for $RUN"
  if [ -r ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.NSSLRefInGSI.bufr ] ; then
    ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.NSSLRefInGSI.bufr ${DATA}/${RUN}.t${HH}${subcyc}z.NSSLRefInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.t${HH}z.NSSLRefInGSI.bufr dones not exist!"
  fi
fi

# lightning obs (pre-processed/re-mapped to model grid)
if [ $obsprep_lghtn -eq 0 ] ; then
  if [ -r ${COMINrap}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d ]; then
    cpreq  ${COMINrap}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d  ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d ${DATA}/${RUN}.t${HH}${subcyc}z.LightningInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.LightningInGSI.bufr does not exist!"
  fi
else
  if [ -r ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr ]; then
    ${ECHO} "using preocessed RAP BUFR lightning data"
    ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr ${DATA}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr
  else
    ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr  does not exist!"
  fi
fi

# NASA LaRC Cloud data (pre-processed/re-mapped to model grid)
if [ $obsprep_cloud -eq 0 ] ; then
  if [ -r ${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr ]; then
    cpreq  ${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr ${DATA}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr does not exist!"
  fi
else
  if [ -r ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr ]; then
    ${ECHO} "using preocessed satellite cloud data from NASA LaRC NETCDF satellite cloud obs"
    ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr ${DATA}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI(_bufr).bufr  does not exist!"
  fi
fi

# satellite wind data
if [ $obsprep_satwnd -eq 0 ] ; then
  if [ -r ${COMINrap}/rtma_ru.t${HH}z.satwndInGSI.bufr ]; then
    cpreq  ${COMINrap}/rtma_ru.t${HH}z.satwndInGSI.bufr ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/rtma_ru.t${HH}z.satwndInGSI.bufr ${DATA}/rtma_ru.t${HH}z.satwndInGSI.bufr
  else
    ${ECHO} "Warning: ${COMINhrap}/rtma_ru.t${HH}z.satwndInGSI.bufr does not exist!"
  fi
else
  if [ -r ${COMINrap}/rtma_ru.t${HH}${subcyc}z.satwnd.tm00.bufr_d  ]; then
    ${ECHO} "using preocessed satwnd data"
    ${LN} -sf ${COMINrap}/rtma_ru.t${HH}${subcyc}z.satwnd.tm00.bufr_d ${DATA}/rtma_ru.t${HH}${subcyc}z.satwnd.tm00.bufr_d
  else
    ${ECHO} "Warning: ${COMINrap}/rtma_ru.t${HH}${subcyc}z.satwnd.tm00.bufr_d does not exist!"
  fi
fi

# the radial velocity data

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle


export err=$? ; err_chk

