#!/bin/ksh
set -x 

# Make sure PREPBUFR is defined and that the directory exists
if [ ! "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: \$PREPBUFR is not defined"
  exit 1
fi
if [ ! -d "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: directory '${PREPBUFR}' does not exist!"
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
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYJJJHHMM=`${DATE} +"%Y%j%H%M" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
date_str=${YYYYJJJHHMM}

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter workign directory:${DATA}"

### prepare basic observations (prepbufr, satwnd, radiance, ... )
pgm="obsprep_basic"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin processing basic observations (prepbufr, satwnd,...)"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# copy/link the prepbufr obs data
# Jet: Copy the prepbufr to obs directory so don't do I/O to /public directly
if [ "${envir}" != "esrl"  ]; then #For Jet expr runs
  DFILE="prepbufr"
  RUNS[1]="rtma_ru";FILES[1]="${PREPBUFR}/${date_str}.${RUNS[1]}.${tz_str}.${DFILE}.tm00.${YYYYMMDD}"
  RUNS[2]="rap";    FILES[2]="${PREPBUFR}/${date_str}.${RUNS[2]}.${tz_str}.${DFILE}.tm00.${YYYYMMDD}"
  RUNS[3]="rap_e";  FILES[3]="${PREPBUFR}/${date_str}.${RUNS[3]}.${tz_str}.${DFILE}.tm00.${YYYYMMDD}"
  RUNS[4]="rap";    FILES[4]="${PREPBUFR}_test/${date_str}.${RUNS[4]}.${tz_str}.${DFILE}.tm00.${YYYYMMDD}.test"
  RUNS[5]="rap_e";  FILES[5]="${PREPBUFR}_test/${date_str}.${RUNS[5]}.${tz_str}.${DFILE}.tm00.${YYYYMMDD}.test"
  if [ "${subcyc}" == "-1" ]; then #hourly run
    casecade="2 3" #5 4 for prepbufr_test
  else
    casecade="1"
  fi
  for i in $casecade; do
    ${ECHO} "checking ${FILES[$i]}"
    if [ -r ${FILES[$i]} ]; then
      ${CP} ${FILES[$i]}  ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}
      ${LN} -snf ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}  ${DFILE}
      break
    fi
  done
  if [ ! -s ${DFILE} ]; then
    ${ECHO} "ERROR: No ${DFILE} files exist!"
    exit 1
  fi

# # Radial velocity data
# DFILE="nexrad"
# RUNS[2]="rap";    FILES[2]="${RADVELLEV2_DIR}/${YYYYJJJHH00}.${RUNS[2]}.t${HH}z.${DFILE}.tm00.bufr_d"
# RUNS[3]="rap_e";  FILES[3]="${RADVELLEV2_DIR}/${YYYYJJJHH00}.${RUNS[3]}.t${HH}z.${DFILE}.tm00.bufr_d"
# casecade="2 3" #5 4 for prepbufr_test
# for i in $casecade; do
#   ${ECHO} "checking ${FILES[$i]}"
#   if [ -r ${FILES[$i]} ]; then
#     ${CP} ${FILES[$i]}  ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}
#     ${LN} -snf ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}  ${DFILE}
#     break
#   fi
# done
# if [ ! -s ${DFILE} ]; then
#   ${ECHO} "ERROR: No ${DFILE} files exist!"
#   exit 1
# fi

# #  AMV wind
# DFILE="satwnd"
# RUNS[2]="rap";    FILES[2]="${SATWND_DIR}/${YYYYJJJHH00}.${RUNS[2]}.t${HH}z.${DFILE}.tm00.bufr_d"
# RUNS[3]="rap_e";  FILES[3]="${SATWND_DIR}/${YYYYJJJHH00}.${RUNS[3]}.t${HH}z.${DFILE}.tm00.bufr_d"
# casecade="2 3" #5 4 for prepbufr_test
# for i in $casecade; do
#   ${ECHO} "checking ${FILES[$i]}"
#   if [ -r ${FILES[$i]} ]; then
#     ${CP} ${FILES[$i]}  ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}
#     ${LN} -snf ${COMINobsproc_rtma3d}/${YYYYMMDD}.${RUNS[$i]}.${tz_str}.${DFILE}  ${DFILE}
#     break
#   fi
# done
# if [ ! -s ${DFILE} ]; then
#   ${ECHO} "ERROR: No ${DFILE} files exist!"
#   exit 1
# fi

else #--------- on WCOSS ---------------------------------------------------------------------
  if [ -r ${COMINrap}/newgblav.${YYYYMMDD}.rap.${tz_str}.prepbufr ] ; then
    cpreq ${COMINrap}/newgblav.${YYYYMMDD}.rap.${tz_str}.prepbufr ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/newgblav.${YYYYMMDD}.rap.${tz_str}.prepbufr  ${DATA}
  elif [ -f ${COMINrtma_ru}/rtma_ru.${tz_str}.prepbufr.tm00 ] ; then
    cpreq ${COMINrtma_ru}/rtma_ru.${tz_str}z.prepbufr.tm00 ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/rtma_ru.${tz_str}.prepbufr.tm00 ${DATA}
  elif [ -f ${COMINrap}/rap.${tz_str}.prepbufr.tm00 ] ; then
    cpreq ${COMINrap}/rap.${tz_str}z.prepbufr.tm00 ${COMINobsproc_rtma3d}
    ${LN} -sf ${COMINobsproc_rtma3d}/rap.${tz_str}.prepbufr.tm00 ${DATA}
  else
    ${ECHO} "Warning: ${COMINrap}: prepbufr file does NOT exist! "
  fi
  # MRMS MOSAIC RADAR data (pre-processed bufr data/remapped on model grid )
  if [ ${obsprep_radar} -eq 0 ] ; then
    ${ECHO} " using hrrr.t${HH}z.NSSLRefInGSI.bufr (used in HRRR)"
    if [ -r "${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr" ]; then
      cpreq ${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr ${COMINobsproc_rtma3d}
      ${LN} -sf ${COMINobsproc_rtma3d}/hrrr.${tz_str}.NSSLRefInGSI.bufr ${DATA}
    else
      ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.NSSLRefInGSI.bufr dones not exist!"
    fi
  else
    ${ECHO} "using processed MRMS mosaic data for $RUN"
    if [ -r ${COMINobsproc_rtma3d}/${RUN}.t${HH}z.NSSLRefInGSI.bufr ] ; then
      ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NSSLRefInGSI.bufr ${DATA}
    else
      ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.t${HH}z.NSSLRefInGSI.bufr does not exist!"
    fi
  fi

  # lightning obs (pre-processed/re-mapped to model grid)
  if [ $obsprep_lghtn -eq 0 ] ; then
  # if [ -r "${COMINhrrr}/hrrr.t${HH}z.LightningInGSI.bufr" ]; then
  #   cpreq  ${COMINhrrr}/hrrr.t${HH}z.LightningInGSI.bufr  ${COMINobsproc_rtma3d}
  #   ${LN} -sf ${COMINobsproc_rtma3d}/hrrr.t${HH}z.LightningInGSI.bufr ${DATA}/hrrr.t${HH}z.LightningInGSI.bufr
    if [ -r "${COMINrap}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d" ]; then
      cpreq  ${COMINrap}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d  ${COMINobsproc_rtma3d}
      ${LN} -sf ${COMINobsproc_rtma3d}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d ${DATA}/${RUN}.t${HH}${subcyc}z.LightningInGSI.bufr
    else
      ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.LightningInGSI.bufr does not exist!"
    fi
  else
    if [ -r "${COMINobsproc_rtma3d}/${RUN}.${tz_str}.LightningInGSI.bufr" ]; then
      ${ECHO} "using preocessed NLDN lightning data"
      ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.LightningInGSI.bufr ${DATA}
    elif [ -r "${COMINobsproc_rtma3d}/${RUN}.${tz_str}.LightningInGSI_bufr.bufr" ]; then
      ${ECHO} "using preocessed RAP BUFR lightning data"
      ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.LightningInGSI_bufr.bufr ${DATA}
    else
      ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.LightningInGSI(_bufr).bufr  does not exist!"
    fi
  fi

  # NASA LaRC Cloud data (pre-processed/re-mapped to model grid)
  if [ $obsprep_cloud -eq 0 ] ; then
    if [ -r "${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr" ]; then
      cpreq  ${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr ${COMINobsproc_rtma3d}
      ${LN} -sf ${COMINobsproc_rtma3d}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr ${DATA}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr
    else
      ${ECHO} "Warning: ${COMINhrrr}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr does not exist!"
    fi
  else
    if [ -r "${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NASALaRCCloudInGSI.bufr" ]; then
      ${ECHO} "using preocessed satellite cloud data from NASA LaRC NETCDF satellite cloud obs"
      ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NASALaRCCloudInGSI.bufr ${DATA}
    elif [ -r "${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NASALaRCCloudInGSI_bufr.bufr" ]; then
      ${ECHO} "using preocessed satellte cloud data from RAP bufr cloud data"
      ${LN} -sf ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NASALaRCCloudInGSI_bufr.bufr ${DATA}
    else
      ${ECHO} "Warning: ${COMINobsproc_rtma3d}/${RUN}.${tz_str}.NASALaRCCloudInGSI(_bufr).bufr  does not exist!"
    fi
  fi

fi

export err=$? ; err_chk

#{LS} -l ${COMINobsproc_rtma3d} > ${COMINobsproc_rtma3d}/obs_data_${PDY}_${cyc}_${subcyc}.list
msg="obsprep_basic HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
