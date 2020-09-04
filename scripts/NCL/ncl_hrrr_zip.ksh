#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

DATE=/bin/date
ECHO=/bin/echo

ulimit -s 512000

# DATAROOT=/lfs3/projects/nrtrr/ncl_graphics/hrrrncepimages   # these 4 lines for testing only
# DATAHOME=${DATAROOT}/2016041905                             #
# START_TIME=2016041905                                       #
# FCST_TIME=01                                                #

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
${ECHO} "    FCST_TIME = ${FCST_TIME}"

set -A domains full a1 a2 a3 

zip_error=0

# zip up the files in each domain

i=0
while [ ${i} -lt ${#domains[@]} ]; do
  dir=${DATAHOME}/nclprd/${domains[${i}]}
  if [ -d ${dir} ]; then
    cd ${dir}
    if (( `ls *.png 2> /dev/null|wc -l` ));then
      zip -g -0 files.zip * -i \*${FCST_TIME}.png
      zip_error=$?
      if [ zip_error -ne 0 ]; then
        ${ECHO} "ERROR - zip failed!"
        ${ECHO} " zip_error = ${zip_error}"
      else
        ${ECHO} "SUCCESS - zip file created"
        ${ECHO} " zip_error = ${zip_error}"
        rm -f *${FCST_TIME}.png
      fi  
    else
      ${ECHO} "no files to zip -- exiting"
    fi  
  else
    ${ECHO} "${dir} does not exist"
  fi  

  (( i=i + 1 ))
done

${ECHO} "ncl_hrrr_zip.ksh completed at `${DATE}`"

exit ${zip_error}
