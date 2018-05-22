#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

# Load modules
module load intel
module load mvapich2
module load netcdf

# Vars used for testing.  Should be commented out for production mode

# Set up paths to unix commands
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
MKDIR=/bin/mkdir
CAT=/bin/cat
ECHO=/bin/echo
CUT=/bin/cut
WC=/usr/bin/wc
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed
MPIRUN=mpiexec

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

if [ ! "${MOSAICTILENUM}" ]; then
  ${ECHO} "ERROR: \$MOSAICTILENUM is not defined!"
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

if [ ! -d "${NSSLMOSAICNC}" ]; then
  ${ECHO} "ERROR: directory '${NSSLMOSAICNC}' does not exist!"
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
if [ -s "mosaic_t1" ]; then
  ${RM} mosaic_t1
fi
if [ -s "mosaic_t2" ]; then
  ${RM} mosaic_t2
fi
if [ -s "mosaic_t3" ]; then
  ${RM} mosaic_t3
fi
if [ -s "mosaic_t4" ]; then
  ${RM} mosaic_t4
fi
if [ -s "mosaic_t5" ]; then
  ${RM} mosaic_t5
fi
if [ -s "mosaic_t6" ]; then
  ${RM} mosaic_t6
fi
if [ -s "mosaic_t7" ]; then
  ${RM} mosaic_t7
fi
if [ -s "mosaic_t8" ]; then
  ${RM} mosaic_t8
fi

numtiles=${MOSAICTILENUM}

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
mm=`${DATE} +"%M" -d "${START_TIME}"`

cp ${fixdir}/prepobs_prep_RAP.bufrtable  ./prepobs_prep.bufrtable

# Save a copy of the GSI executable in the workdir
${CP} ${MOSAIC} .

${LN} -s ${STATIC_DIR}/geo_em.d01.nc .

# Link to the prepbufr data
if [ ${numtiles} -eq 8 ]; then
${CP} ${NSSLMOSAICNC}/tile1/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t1.gz
${CP} ${NSSLMOSAICNC}/tile2/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t2.gz
${CP} ${NSSLMOSAICNC}/tile3/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t3.gz
${CP} ${NSSLMOSAICNC}/tile4/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t4.gz
${CP} ${NSSLMOSAICNC}/tile5/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t5.gz
${CP} ${NSSLMOSAICNC}/tile6/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t6.gz
${CP} ${NSSLMOSAICNC}/tile7/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t7.gz
${CP} ${NSSLMOSAICNC}/tile8/${YYYY}${MM}${DD}-${HH}${mm}00.netcdf.gz ./mosaic_t8.gz
else
${CP} ${NSSLMOSAICNC}/tile1/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t1.gz
${CP} ${NSSLMOSAICNC}/tile2/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t2.gz
${CP} ${NSSLMOSAICNC}/tile3/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t3.gz
${CP} ${NSSLMOSAICNC}/tile4/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t4.gz
fi

gzip -d *.gz

## echo ${YYYYMMDDHH} > mosaic_cycle_date
cat << EOF > mosaic.namelist
 &setup
  tversion=${numtiles},
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
 /

EOF

# Run obs pre-processor
${MPIRUN} -envall -np ${np} ${MOSAIC} > stdout_radar 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${MOSAIC} crashed  Exit status=${error}"
  exit ${error}
fi

rm -f mosaic_*

exit 0
