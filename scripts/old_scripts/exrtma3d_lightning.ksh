#!/bin/ksh 

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile
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
${MPIRUN} ${LIGHTNING} < lightning.namelist > stdout_lighting 2>&1
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
