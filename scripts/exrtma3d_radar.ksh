#!/bin/ksh --login

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile

# Set endian conversion options for use with Intel compilers
# export F_UFMTENDIAN="big;little:10,15,66"
# export GMPIENVVAR=F_UFMTENDIAN
export MV2_ON_DEMAND_THRESHOLD=256

# Set the path to the gsi executable
MOSAIC=${GSI_ROOT}/process_NSSL_mosaic.exe
fixdir=${FIX_ROOT}

# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME}" ]; then
  ${ECHO} "NOTE: DATAHOME directory '${DATAHOME}' does not exist!"
fi

if [ ! "${DATAROOT}" ]; then
  ${ECHO} "ERROR: \$DATAROOT is not defined!"
  exit 1
fi
if [ ! -d "${DATAROOT}" ]; then
  ${ECHO} "ERROR: DATAROOT directory '${DATAROOT}' does not exist!"
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

# Make sure sub-hourly time is defined and exists
if [ ! "${SUBH_TIME}" ]; then
  ${ECHO} "ERROR: \$SUBH_TIME is not defined!"
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
  START_TIME=`${DATE} -d "${START_TIME} ${SUBH_TIME} minutes"`
fi

if [ ! -d "${NSSL}" ]; then
  ${ECHO} "ERROR: directory '${NSSL}' does not exist!"
  exit 1
fi

# Make sure the GSI executable exists
if [ ! -x "${MOSAIC}" ]; then
  ${ECHO} "ERROR: ${MOSAIC} does not exist!"
  exit 1
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

if [ -x "process_NSSL_mosaic.exe" ]; then
  ${RM} process_NSSL_mosaic.exe
fi
if [ -s "RefInGSI.dat" ]; then
  ${RM} RefInGSI.dat
fi

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

typeset -Z2 mm mm1 mm2 mm3
mm=`${DATE} +"%M" -d "${START_TIME}"`
mm1=$((${mm}+1))
mm2=$((${mm1}+1))
mm3=$((${mm2}+1))

cp ${fixdir}/prepobs_prep_RAP.bufrtable  ./prepobs_prep.bufrtable

# Save a copy of the GSI executable in the workdir
${CP} ${MOSAIC} .

${LN} -s ${STATIC_DIR}/geo_em.d01.nc .

if [ -s filelist_mrms ]; then
   numgrib2=`more filelist_mrms | wc -l`
   echo "Using radar data from: `head -1 filelist_mrms | cut -c10-15`"
   echo "NSSL grib2 file levels = $numgrib2"
else
   echo "ERROR: Not enough radar reflectivity files available."
   exit 1
fi

## echo ${YYYYMMDDHH} > mosaic_cycle_date
cat << EOF > mosaic.namelist
 &setup
  tversion=1,
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
 /

EOF

# Run obs pre-processor
${MPIRUN} ${MOSAIC} > stdout_radar 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${MOSAIC} crashed  Exit status=${error}"
  exit ${error}
fi

rm -f mosaic_*

exit 0
