#!/bin/ksh
############################################################################

set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_cloud} ] ; then
  ${ECHO} "ERROR: NASA cloud obs prcoessing executable '${EXECrtma3d}/${exefile_name_cloud}' does not exist!"
  exit 1
fi

# working directory
workdir=${DATA}
cd ${workdir}

# export MV2_ON_DEMAND_THRESHOLD=256    # if load module mvapich2 ?

mm=$subcyc
subhtime=$subcyc
${ECHO} $PDY $cyc $mm
# START_TIME="${PDY}${cyc}"      # YYYYMMDDHH
  START_TIME=${START_TIME:-"{PDY} ${cyc}"}      # YYYYMMDD HH
# START_TIME="${PDY} ${cyc} ${subcyc} minutes"  # YYYYMMDD HH MN 

${ECHO} "${START_TIME}"
echo `echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'`
 if [ `echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
   START_TIME=`echo "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
 elif [ ! "`echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
   echo "FATAL ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
   err_exit 1
 fi
 START_TIME=`${DATE} -d "${START_TIME} ${subhtime} minutes"`
echo $START_TIME

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

typeset -Z2 mm mmp1 mmp2 mmp3              # <<-- "-Z2" only work for K-Shell
mm=`${DATE} +"%M" -d "${START_TIME}"`
mmp1=$((${mm}+1))
mmp2=$((${mm}+2))
mmp3=$((${mm}+3))
# mm=`printf "%2.2i\n" $mm`
# mmp1=`printf "%2.2i\n" $mmp1`
# mmp2=`printf "%2.2i\n" $mmp2`
# mmp3=`printf "%2.2i\n" $mmp3`

ymd=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
ymdh=${YYYYMMDDHH}
hh=$HH

# BUFR Table includingthe description for HREF
${CP} -p ${FIXgsi}/prepobs_prep_RAP.bufrtable   ./prepobs_prep.bufrtable
# WPS GEO_GRID Data
${LN} -s ${FIXwps}/hrrr_geo_em.d01.nc           ./geo_em.d01.nc 

#
#--- 
#

# find rap bufr lgycld data file and link to the bufr file
  if [ -s $COMINrap/rap.t${cyc}z.lgycld.tm00.bufr_d ] ; then
    ${CP} -p $COMINrap/rap.t${cyc}z.lgycld.tm00.bufr_d ./rap.t${cyc}z.lgycld.tm00.bufr_d
    ${LN} -sf ./rap.t${cyc}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr
  else
    echo 'No bufr file found for nasa LaRC cloud data processing'
  fi

  echo ${PDY}${cyc} > ./nasaLaRC_cycle_date
# echo ${YYYYMMDDHH} > ./nasaLaRC_cycle_date

# Run process lightning

  if [ -f errfile ] ; then 
    rm -f errfile
  fi

  . prep_step

  startmsg
  msg="***********************************************************"
  postmsg "$jlogfile" "$msg"
  msg="  begin pre-processing RAP bufr lightning data"
  postmsg "$jlogfile" "$msg"
  msg="***********************************************************"
  postmsg "$jlogfile" "$msg"

# Run Processing lightning
# copy the excutable file of processing NASA LaRC Cloud data
  ${CP} ${EXECrtma3d}/${exefile_name_cloud}   ./rtma3d_process_cloud

  runline="${MPIRUN}  -np ${np}  ./rtma3d_process_cloud"
  $runline >> ${pgmout} 2>errfile
  export err=$?; err_chk

  msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
  postmsg "$jlogfile" "$msg"

  if [ -f ${DATA}/NASALaRCCloudInGSI.bufr ] ; then
    cpreq ${DATA}/NASALaRCCloudInGSI.bufr ${COMINobsproc_rtma3d}/${RUN}.t${cyc}z.NASALaRCCloudInGSI.bufr
  else
    msg="WARNING $pgm terminated normally but ${DATA}/NASALaRCCloudInGSI_bufr.bufr does NOT exist."
    ${ECHO} "$msg"
    postmsg "$jlogfile" "$msg"
    exit 1
  fi

exit 0
