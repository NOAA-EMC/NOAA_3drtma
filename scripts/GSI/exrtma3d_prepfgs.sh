#!/bin/sh

set -x 

#-- Testing the status of some important variables. --#
# Make sure these variables for key directories are defined and exists
if [ ! "${COMINhrrr}" ]; then
  ${ECHO} "ERROR: \$COMINhrrr is not defined!"
  exit 1
fi
if [ ! -d "${COMINhrrr}" ]; then
  ${ECHO} "ERROR: $COMINhrrr does not exist!"
  exit 1
fi
if [ ! "${COMINhrrr_cycp1}" ]; then
  ${ECHO} "ERROR: \$COMINhrrr_cycp1 is not defined!"
  exit 1
fi
if [ ! -d "${COMINhrrr_cycp1}" ]; then
  ${ECHO} "ERROR: $COMINhrrr_cycp1 does not exist!"
  exit 1
fi

if [ ! "${GESINhrrr_rtma3d}" ]; then
  ${ECHO} "ERROR: \$GESINhrrr_rtma3d is not defined!"
  exit 1
fi
if [ ! -d "${GESINhrrr_rtma3d}" ]; then
  ${ECHO} "ERROR: $GESINhrrr_rtma3d does not exist!"
  exit 1
fi

if [ ! "${DATA}" ]; then
  ${ECHO} "ERROR: \$DATA is not defined!"
  exit 1
fi
if [ ! -d "${DATA}" ]; then
  ${ECHO} "ERROR: $DATA does not exist!"
  exit 1
fi

if [  "${DATA_FGSPRD}" ]; then
  ${RM} -f  ${DATA_FGSPRD}
  ${LN} -sf ${DATA} ${DATA_FGSPRD}
fi

#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
echo $cyc
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

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

HH_cycp1=`echo ${PDYHH_cycp1} | cut -c 9-10`

#############################################################################

# Create the working directory and cd into it
workdir=${DATA}
cd ${workdir}

time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin copy firstguess to fgsprd.${cycle}"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# Look for bqckground from pre-forecast background
FGS_FNAME1="hrrr.t${HH_cycp1}z.wrfguess_rap"
FGS_FNAME2="hrrr.t${HH}z.wrfguess"
if [ -r ${COMINhrrr_cycp1}/${FGS_FNAME1} ] && [ $FGS_OPT -eq "1" ] ; then
  ${ECHO} " Cycled run using ${COMINhrrr_cycp1}/${FGS_FNAME1}"
  # cpfs ${COMINhrrr_cycp1}/${FGS_FNAME1}     ${GESINhrrr_rtma3d}
  ${LN} -sf ${COMINhrrr_cycp1}/${FGS_FNAME1}            ${GESINhrrr_rtma3d}/${FGS_FNAME1}
  ${LN} -sf ${GESINhrrr_rtma3d}/${FGS_FNAME1}     ${DATA}/${FGS_FNAME1}
  ${ECHO} " Cycle ${YYYYMMDDHH}: PREPFGS background=${COMINhrrr_cycp1}/${FGS_FNAME1} "

elif [ -r ${COMINhrrr}/${FGS_FNAME2} ] && [ $FGS_OPT -eq "2"  ] ; then
  ${ECHO} " Cycled run using ${COMINhrrr}/${FGS_FNAME2}"
  # cpfs ${COMINhrrr}/${FGS_FNAME2}     ${GESINhrrr_rtma3d}
  ${LN} -sf ${COMINhrrr}/${FGS_FNAME2}            ${GESINhrrr_rtma3d}/${FGS_FNAME2}
  ${LN} -sf ${GESINhrrr_rtma3d}/${FGS_FNAME2}     ${DATA}/${FGS_FNAME2}
  ${ECHO} " Cycle ${YYYYMMDDHH}: PREPFGS background=${COMINhrrr}/${FGS_FNAME2} "

# No background available so abort
else
  ${ECHO} "${COMINhrrr}/${FGS_FNAME} does not exist!!"
  ${ECHO} "ERROR: No background file for analysis at ${time_run}!!!!"
  ${ECHO} " Cycle ${YYYYMMDDHH}: PREPFGS failed because of no background" >> ${pgmout} 
  exit 1
fi

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle

export err=$? ; err_chk

ls -l ${GESINhrrr_rtma3d} > ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}_${subcyc}.list

exit 0
