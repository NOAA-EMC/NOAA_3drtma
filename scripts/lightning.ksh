#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

module load intel
module load mvapich2
module load szip
module load hdf5
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
#export F_UFMTENDIAN="big;little:10,15,66" #not used, G.Ge 2018/9/4
#export GMPIENVVAR=F_UFMTENDIAN  #not used, G.Ge 2018/9/4
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

# Make sure sub-hourly time is defined and exists
if [ ! "${SUBH_TIME}" ]; then
  ${ECHO} "ERROR: \$SUBH_TIME is not defined!"
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
  #START_TIME=`${DATE} -d "${START_TIME} ${SUBH_TIME} minutes"`
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
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME} ${SUBH_TIME} minutes"`
YJH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
NEXTYJH=`${DATE} +"%y%j%H" -d "${START_TIME} 1 hour"`

if [ ${SUBH_TIME} -eq 15 ]; then
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
  ${ECHO} "ERROR: lightning.ksh not set up for SUBH_TIME = $SUBH_TIME"
  exit 1
fi

# Save a copy of the GSI executable in the workdir
${CP} ${LIGHTNING} .

${LN} -sf ${STATIC_DIR}/geo_em.d01.nc .

#
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
echo "found GLD360 files: ${filenum}"

ifalaska=false
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

if [ -s "LightningInGSI.dat" ]; then
  ${ECHO} "Lightning files processed."
else
  ${ECHO} "Lightning files not processed."
  exit 1
fi

exit 0
