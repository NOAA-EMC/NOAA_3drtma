#!/bin/ksh --login
set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_lightning} ] ; then
  ${ECHO} "ERROR: lightning obs prcoessing executable '${EXECrtma3d}/${exefile_name_lightning}' does not exist!"
  exit 1
fi

if [ ! "${LIGHTNING_ROOT}" ]; then
  ${ECHO} "ERROR: \$LIGHTNING_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${LIGHTNING_ROOT}" ]; then
  ${ECHO} "ERROR: LIGHTNING_ROOT directory '${LIGHTNING_ROOT}' does not exist!"
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
#for HRRR pre-forecast
#CYCLE_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`
#START_TIME=`${DATE} -d "${CYCLE_TIME} -1 hour"`

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YJH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
NEXTYJH=`${DATE} +"%y%j%H" -d "${START_TIME} 1 hour"`
PREVYJH=`${DATE} +"%y%j%H" -d "${START_TIME} -1 hour"`

if [ ${SUBH_TIME} -eq 0 ]; then
  file1=${PREVYJH}500005r
  file2=${PREVYJH}550005r
  file3=${YJH}000005r
elif [ ${SUBH_TIME} -eq 15 ]; then
  file1=${YJH}050005r
  file2=${YJH}100005r
  file3=${YJH}150005r
elif [ ${SUBH_TIME} -eq 30 ]; then
  file1=${YJH}200005r
  file2=${YJH}250005r
  file3=${YJH}300005r
elif [ ${SUBH_TIME} -eq 45 ]; then
  file1=${YJH}350005r
  file2=${YJH}400005r
  file3=${YJH}450005r
elif [ ${SUBH_TIME} -eq 60 ]; then
  file1=${YJH}500005r
  file2=${YJH}550005r
  file3=${NEXTYJH}000005r
else
  ${ECHO} "ERROR: obsprep_lghtn.ksh not set up for SUBH_TIME = $SUBH_TIME"
  exit 1
fi

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# BUFR Table including the description for HREF
${LN} -sf ${FIXgsi}/prepobs_prep_RAP.bufrtable ./prepobs_prep.bufrtable
if [ ! -s "./prepobs_prep.bufrtable" ]; then
  ${ECHO} "prepobs_prep.bufrtable does not exist or not readable"
  exit 1
fi

# WPS GEO_GRID Data
if [ "${DOMAIN}" == "alaska" ]; then
  ${LN} -sf ${FIXwps}/hrrr_geo_em.d01.nc_AK ./geo_em.d01.nc
else
  ${LN} -sf ${FIXwps}/hrrr_geo_em.d01.nc ./geo_em.d01.nc
fi
if [ ! -s "./geo_em.d01.nc" ]; then
  ${ECHO} "geo_em.d01.nc does not exist or not readable"
  exit 1 
fi

# print parameters for linking/processing
${ECHO} "START_TIME: "${START_TIME}
${ECHO} "SUBH_TIME: "${SUBH_TIME}
${ECHO} "YYYYMMDDHH: "${YYYYMMDDHH}

# Link to the NLDN data
#
filenum=0
LIGHTNING_FILE=${LIGHTNING_ROOT}/vaisala/netcdf/${file1}
if [ -r ${LIGHTNING_FILE} ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE} ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE} does not exist"
fi
LIGHTNING_FILE=${LIGHTNING_ROOT}/vaisala/netcdf/${file2}
if [ -r ${LIGHTNING_FILE} ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE} ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE} does not exist"
fi
LIGHTNING_FILE=${LIGHTNING_ROOT}/vaisala/netcdf/${file3}
if [ -r ${LIGHTNING_FILE} ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE} ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE} does not exist"
fi
${ECHO} "found GLD360 files: ${filenum}"

ifalaska=false
# Build the namelist on-the-fly
${CAT} << EOF > lightning.namelist
 &SETUP
   analysis_time = ${YYYYMMDDHH},
   NLDN_filenum  = ${filenum},
   IfAlaska    = ${ifalaska},
 /
EOF

# Run obs processor
export pgm="rtma3d_process_lightning"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin processing lightning data"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

if [ "${envir}" == "esrl" ]; then #Jet
  CP_LN="${LN} -sf"
else
  CP_LN=${CP}
fi
${CP_LN} ${EXECrtma3d}/${exefile_name_lightning} ${pgm}
${MPIRUN} ${pgm} < lightning.namelist > ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

targetfile="LightningInGSI.bufr"
if [ -f ${DATA}/${targetfile} ] ; then
  if [ "${envir}" == "esrl" ]; then #Jet
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
