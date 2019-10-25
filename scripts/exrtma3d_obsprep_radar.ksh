#!/bin/ksh --login
set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_radar} ] ; then
  ${ECHO} "ERROR: mosaic radar obs prcoessing executable '${EXECrtma3d}/${exefile_name_radar}' does not exist!"
  exit 1
fi

if [ ! "${NSSL}" ]; then
  ${ECHO} "ERROR: \$NSSL is not defined!"
  exit 1
fi
if [ ! -d "${NSSL}" ]; then
  ${ECHO} "ERROR: directory '${NSSL}' does not exist!"
  exit 1
fi

# Make sure minutes are defined
if [ ! "${MM1}" ]; then
  ${ECHO} "ERROR: \$MM1 is not defined!"
  exit 1
fi
if [ ! "${MM2}" ]; then
  ${ECHO} "ERROR: \$MM2 is not defined!"
  exit 1
fi
if [ ! "${MM3}" ]; then
  ${ECHO} "ERROR: \$MM3 is not defined!"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
SUBH_TIME=${subcyc}
if [ "subcyc" == "-1" ]; then #hourly run
  SUBH_TIME='00'
fi
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
  START_TIME=`${DATE} -d "${START_TIME} ${SUBH_TIME} minutes"`
fi

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
if [ ${SUBH_TIME} -eq 60 ]; then
  time_01hago=`${DATE} -d "${START_TIME}  1 hours ago"`
  YYYY=`${DATE} +"%Y" -d "${time_01hago}"`
  MM=`${DATE} +"%m" -d "${time_01hago}"`
  DD=`${DATE} +"%d" -d "${time_01hago}"`
  HH=`${DATE} +"%H" -d "${time_01hago}"`
  YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${time_01hago}"`
fi

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter workign directory:${DATA}"

# BUFR Table including the description for HREF
${CP} ${FIXgsi}/prepobs_prep_RAP.bufrtable   ./prepobs_prep.bufrtable
if [ -s "./prepobs_prep.bufrtable" ]; then
  ${ECHO} "prepobs_prep.bufrtable sucessfully copied"
else
  ${ECHO} "prepobs_prep.bufrtable does not exist or not readable"
  exit 1
fi

# WPS GEO_GRID Data
${LN} -s ${FIXwps}/hrrr_geo_em.d01.nc ./geo_em.d01.nc 
if [ -s "./geo_em.d01.nc" ]; then
  ${ECHO} "geo_em.d01.nc sucessfully linked"
else
  ${ECHO} "geo_em.d01.nc does not exist or not readable"
  exit 1 
fi

# print parameters for linking/processing
${ECHO} "working directory: "${DATA}
${ECHO} "START_TIME: "${START_TIME}
if [ ${SUBH_TIME} -eq 60 ]; then
  ${ECHO} "time_01hago: "${time_01hago}
fi
${ECHO} "SUBH_TIME: "${SUBH_TIME}
${ECHO} "MINUTES: "${MM1}"/"${MM2}"/"${MM3}
${ECHO} "YYYYMMDDHH: "${YYYYMMDDHH}

# create mrms file list
for min in ${MM1} ${MM2} ${MM3}
do
  ${ECHO} "Looking for data valid:"${YYYY}"-"${MM}"-"${DD}" "${HH}":"${min}
  s=0
  while [[ $s -le 59 ]]; do
    if [ $s -lt 10 ]; then
      ss=0${s}
    else
      ss=$s
    fi
    nsslfile=${NSSL}/${YYYY}${MM}${DD}-${HH}${min}${ss}.MRMS_MergedReflectivityQC_00.50_${YYYY}${MM}${DD}-${HH}${min}${ss}.grib2
    if [ -s $nsslfile ]; then
      echo 'Found '${nsslfile}
      numgrib2=`ls ${NSSL}/${YYYY}${MM}${DD}-${HH}${min}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2 | wc -l`
      echo 'Number of GRIB-2 files: '${numgrib2}
      if [ ${numgrib2} -ge 10 ] && [ ! -e filelist_mrms ]; then
        ln -sf ${NSSL}/${YYYY}${MM}${DD}-${HH}${min}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2 . 
        ls ${YYYY}${MM}${DD}-${HH}${min}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2 > filelist_mrms
        echo 'Creating links for SUBH: '${SUBH_TIME}
      fi
    fi
    ((s+=1))
  done 
done

# remove filelist_mrms if zero bytes
if [ ! -s filelist_mrms ]; then
  rm -f filelist_mrms
fi

if [ -s filelist_mrms ]; then
   numgrib2=`more filelist_mrms | wc -l`
   echo "Using radar data from: `head -1 filelist_mrms | cut -c10-15`"
   echo "NSSL grib2 file levels = $numgrib2"
else
   echo "ERROR: Not enough radar reflectivity files available."
   exit 1
fi

cat << EOF > mosaic.namelist
 &setup
  tversion=1,
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
 /

EOF

# Run obs processor
export pgm="rtma3d_process_mosaic"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin pre-processing MRMS MOSAIC RADAR Reflectivity Obs DATA"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

${CP} ${EXECrtma3d}/${exefile_name_radar}   ./rtma3d_process_mosaic
${MPIRUN} ${pgm} > ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

if [ -f ${DATA}/NSSLRefInGSI.bufr ] ; then
  if [ "${envir}" == "expr" ]; then
    ${ECHO} "" #Jet experimental run does not need to copy to COM
  else
    cpreq ${DATA}/NSSLRefInGSI.bufr ${COMOUT}/hrrr.t${cyc}${subcyc}z.NSSLRefInGSI.bufr
    cpreq ${DATA}/NSSLRefInGSI.bufr ${COMINobsproc_rtma3d}/${RUN}.t${cyc}${subcyc}z.NSSLRefInGSI.bufr
  fi
else
  msg="WARNING $pgm terminated normally but ${DATA}/NSSLRefInGSI.bufr does NOT exist."
  ${ECHO} "$msg"
  postmsg "$jlogfile" "$msg"
  exit 1
fi

exit 0
