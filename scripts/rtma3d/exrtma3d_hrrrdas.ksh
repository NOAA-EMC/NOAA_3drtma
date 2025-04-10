#!/bin/ksh

set -x 

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
HH_cycm1=`echo ${PDYHH_cycm1} | cut -c 9-10`
YYYYMMDDHH_m1hr=`echo ${PDYHH_cycm1} | cut -c 1-10`

CDATEymdh=${YYYYMMDDHH}

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
msg="  begin retrieveing howv/gust and appending them to fgs, then copy firstguess to fgsprd.${cycle}"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
#
#-----------------------------------------------------------------------
#
# Look for background from pre-forecast background
#
#-----------------------------------------------------------------------

filename=${COMOUTgsi_rtma3d}/filelist.hrrrdas

if [[ ! -f $filename ]]; then
echo "$filename does not exist. Exiting."
exit 0
fi

mem_varlist="T,P_TOP,MU,MUB,U,V,QVAPOR,ZNW,Times,TH2,Q2,U10,V10"
c=1
while IFS= read -r line
do
  while [[ $c -le 36 ]]; do
    if [ $c -lt 10 ]; then
       cc="0"$c
    else
       cc=$c
    fi
  ncks -A -v ${mem_varlist} $line $DATA/hrrrdas_small_d02_${PDYHH_cycm1}00f01_mem00${cc}_thinned 
  cp $DATA/hrrrdas_small_d02_${PDYHH_cycm1}00f01_mem00${cc}_thinned ${COMOUThrrrdas_rtma3d}
  c=$(($c + 01 ))
  done
done < "$filename"

exit 0
