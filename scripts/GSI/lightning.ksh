#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

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
LN=/bin/ln
MPIRUN=mpiexec

# Set endian conversion options for use with Intel compilers
export F_UFMTENDIAN="big;little:10,15,66"
export GMPIENVVAR=F_UFMTENDIAN
export MV2_ON_DEMAND_THRESHOLD=256

# Set the path to the gsi executable
LIGHTNING=${GSI_ROOT}/process_Lightning.exe

# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME}" ]; then
  ${ECHO} "Warning: DATAHOME directory '${DATAHOME}' does not exist!"
fi

if [ ! "${DATAROOT}" ]; then
  ${ECHO} "ERROR: \$DATAROOT is not defined!"
  exit 1
fi
if [ ! -d "${DATAROOT}" ]; then
  ${ECHO} "ERROR: DATAROOT directory '${DATAROOT}' does not exist!"
  exit 1
fi
if [ ! -d "${LIGHTNING_ROOT}" ]; then
  ${ECHO} "ERROR: LIGHTNING_ROOT directory '${LIGHTNING_ROOT}' does not exist!"
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
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

if [ ! "${PRESTART_TIME}" ]; then
  ${ECHO} "ERROR: \$PRESTART_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${PRESTART_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    PRESTART_TIME=`${ECHO} "${PRESTART_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${PRESTART_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${PRESTART_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  PRESTART_TIME=`${DATE} -d "${PRESTART_TIME}"`
fi

# Make sure the GSI executable exists
if [ ! -x "${LIGHTNING}" ]; then
  ${ECHO} "ERROR: ${LIGHTNING} does not exist!"
  exit 1
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

if [ -x "process_Lightning.exe" ]; then
  ${RM} process_Lightning.exe
fi

# Compute date & time components for the analysis time
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
PREYYJJJHH=`${DATE} +"%y%j%H" -d "${PRESTART_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

# Save a copy of the GSI executable in the workdir
${CP} ${LIGHTNING} .

${LN} -sf ${STATIC_DIR}/geo_em.d01.nc .

#
# Link to the NLDN data
#
filenum=0
## LIGHTNING_FILE=${LIGHTNING_ROOT}/nldn/netcdf
LIGHTNING_FILE=${LIGHTNING_ROOT}/gld360/netcdf
if [ -r "${LIGHTNING_FILE}/${YYJJJHH}050005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${YYJJJHH}050005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${YYJJJHH}050005r does not exist"
fi
if [ -r "${LIGHTNING_FILE}/${YYJJJHH}000005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${YYJJJHH}000005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${YYJJJHH}000005r does not exist"
fi
if [ -r "${LIGHTNING_FILE}/${PREYYJJJHH}550005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${PREYYJJJHH}550005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${PREYYJJJHH}550005r does not exist"
fi
if [ -r "${LIGHTNING_FILE}/${PREYYJJJHH}500005r" ]; then
  ((filenum += 1 ))
  ls ${LIGHTNING_FILE}/${PREYYJJJHH}500005r
  ${LN} -sf ${LIGHTNING_FILE}/${PREYYJJJHH}500005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${PREYYJJJHH}500005r does not exist"
fi
if [ ! 0 ] ; then
if [ -r "${LIGHTNING_FILE}/${PREYYJJJHH}450005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${PREYYJJJHH}450005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${PREYYJJJHH}450005r does not exist"
fi
if [ -r "${LIGHTNING_FILE}/${PREYYJJJHH}400005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${PREYYJJJHH}400005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${PREYYJJJHH}400005r does not exist"
fi
if [ -r "${LIGHTNING_FILE}/${PREYYJJJHH}350005r" ]; then
  ((filenum += 1 ))
  ${LN} -sf ${LIGHTNING_FILE}/${PREYYJJJHH}350005r ./NLDN_lightning_${filenum}
else
   ${ECHO} " ${LIGHTNING_FILE}/${PREYYJJJHH}350005r does not exist"
fi
fi

echo "found GLD360 files: ${filenum}"
#
# Alaska lightning data
#
ifalaska=false
if [ -r "${LIGHTNING_ROOT}/alaska/ascii/${YYYYMMDDHH}0100" ]; then
  ${LN} -sf ${LIGHTNING_ROOT}/alaska/ascii/${YYYYMMDDHH}0100 ./ALSKA_lightning
  ifalaska=true
else
  if  [ -r "${LIGHTNING_ROOT}/alaska/ascii/${YYYYMMDDHH}0101" ]; then
    ${LN} -sf ${LIGHTNING_ROOT}/alaska/ascii/${YYYYMMDDHH}0101 ./ALSKA_lightning
    ifalaska=true
  fi
fi

# Build the namelist on-the-fly
${CAT} << EOF > lightning.namelist
 &SETUP
   analysis_time = ${YYYYMMDDHH},
   NLDN_filenum  = ${filenum},
   IfAlaska    = ${ifalaska},
 /
EOF

cp ${FIX_ROOT}/prepobs_prep_RAP.bufrtable ./prepobs_prep.bufrtable

# Run obs pre-processor
${MPIRUN} -envall -np ${np} ${LIGHTNING} < lightning.namelist > stdout_lighting 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${LIGHTNING} crashed  Exit status=${error}"
  exit ${error}
fi

exit 0
