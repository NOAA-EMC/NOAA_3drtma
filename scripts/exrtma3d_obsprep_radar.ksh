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

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`
#assgin MM1, MM2, MM3
if [ "${SUBH_TIME}" == "00" ]; then
  MM1=0
  MM2=1
  MM2=2
elif [ "${SUBH_TIME}" == "15" ]; then
  MM1=15
  MM2=14
  MM2=16
elif [ "${SUBH_TIME}" == "30" ]; then
  MM1=30
  MM2=29
  MM2=31
elif [ "${SUBH_TIME}" == "45" ]; then
  MM1=45
  MM2=44
  MM2=46
elif [ "${SUBH_TIME}" == "60" ]; then
  MM1=59
  MM2=58
  MM2=57
fi
# Compute date & time components for the analysis time
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
msg="  begin processing MRMS MOSAIC RADAR Reflectivity Obs DATA"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

if [ "${envir}" == "esrl" ]; then
  CP_LN=${LN}
else
  CP_LN=${CP}
fi
${CP_LN} ${EXECrtma3d}/${exefile_name_radar} ${pgm}
${MPIRUN} ${pgm} > ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

targetfile="NSSLRefInGSI.bufr"
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
