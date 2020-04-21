#!/bin/ksh 

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile

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
