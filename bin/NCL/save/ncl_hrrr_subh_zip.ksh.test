#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

DATE=/bin/date
ECHO=/bin/echo

ulimit -s 512000

# DATAROOT=/home/rtrr/hrrr_databasedir/run             # these 3 lines for testing only
# DATAHOME=/home/rtrr/hrrr_databasedir/run/2013070505  #
# START_TIME=2013070505                                #

# Print run parameters
${ECHO}
${ECHO} "ncl_hrrr_zip.ksh started at `${DATE}`"
${ECHO}
${ECHO} "DATAROOT = ${DATAROOT}"
${ECHO} "DATAHOME = ${DATAHOME}"

# Check to make sure that the DATAHOME exists
if [ ! -d ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, '${DATAHOME}', does not exist"
  exit 1
fi

# If START_TIME is not defined, use the current time
if [ ! "${START_TIME}" ]; then
  ${ECHO} "START_TIME not defined - get from date"
  START_TIME=$( date +"%Y%m%d %H" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
else
  ${ECHO} "START_TIME defined and is ${START_TIME}"
  START_TIME=$( date +"%Y%m%d %H" -d "${START_TIME%??} ${START_TIME#????????}" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
fi

# Print out times
${ECHO} "   START_TIME = ${START_TIME}"

set -A domains full t1 t2 t3 t4 t5 t6 t7 t8 z0 z1 z2 z3 z4 z5 z6 z7

zip_error=0

# zip up the files in each domain

i=0
while [ ${i} -lt ${#domains[@]} ]; do
  dir=${DATAHOME}/nclprd/15min/${domains[${i}]}
  cd ${dir}
  zip -g -0 files.zip * -i \*.png

  (( i=i + 1 ))
done

${ECHO} "ncl_hrrr_zip.ksh completed at `${DATE}`"

exit ${zip_error}
