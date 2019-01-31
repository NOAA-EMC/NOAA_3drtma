#!/bin/ksh --login
############################################################################

set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_lightning} ] ; then
  ${ECHO} "ERROR: lightning obs prcoessing executable '${EXECrtma3d}/${exefile_name_lightning}' does not exist!"
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

# Julian Day in format YYJJJHH
YYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`

PREVCYC_TIME=${PDYHH_cycm1}
${ECHO} "${PREVCYC_TIME}"
 if [ `echo "${PREVCYC_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
   PREVCYC_TIME=`echo "${PREVCYC_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
 elif [ ! "`echo "${PREVCYC_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
   echo "FATAL ERROR: previous cycle time, '${PREVCYC_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
   err_exit 1
 fi
 PREVCYC_TIME=`${DATE} -d "${PREVCYC_TIME} ${subhtime} minutes"`
echo $PREVCYC_TIME

# Julian Day in format YYJJJHH (two-digits year, day of year, hour.)
YYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
PREYYJJJHH=`${DATE} +"%y%j%H" -d "${PREVCYC_TIME}"`

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
if [ ${obsprep_lghtn} -eq 1 ] ; then

  ${ECHO} " processing NCEP BUFR Lightning Data"

# find lightning bufr file
  if [ -s $COMINrap/rap.t${cyc}z.lghtng.tm00.bufr_d ] ; then
    cp $COMINrap/rap.t${cyc}z.lghtng.tm00.bufr_d ./rap.t${cyc}z.lghtng.tm00.bufr_d
  elif [ -s $COMINrap_e/rap.t${cyc}z.lghtng.tm00.bufr_d ] ; then
    cp $COMINrap_e/rap.t${cyc}z.lghtng.tm00.bufr_d ./rap.t${cyc}z.lghtng.tm00.bufr_d
  else
    echo 'No bufr file found for lightning processing'
  fi

  ln -s rap.t${cyc}z.lghtng.tm00.bufr_d lghtngbufr

  echo ${PDY}${cyc} > ./lightning_cycle_date

  YYYYMMDDHH=${PDY}${cyc}
  minutetime=$subcyc

# Build the namelist on-the-fly
  rm -f ./lightning_bufr.namelist
  cat << EOF > lightning_bufr.namelist
 &SETUP
  analysis_time = ${YYYYMMDDHH},
  minute=${minutetime},
  trange_start=-15.0,
  trange_end=0.0,
 /
EOF

fi

# Run process lightning

if [ -f errfile ] ; then 
  rm -f errfile
fi

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
if [ $obsprep_lghtn -eq 1 ] ; then  
  msg="  begin pre-processing NCEP BUFR lightning data"
fi
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# Run Processing lightning
# copy the excutable file of processing RAP BUFR format lightning data
${CP} ${EXECrtma3d}/${exefile_name_lightning}  ./rtma3d_process_lightning

runline="${MPIRUN}  -np ${np}  ./rtma3d_process_lightning"
if [ ${obsprep_lghtn} -eq 1 ] ; then
  $runline  > ${pgmout} 2>errfile
else
  $runline < lightning.namelist > ${pgmout} 2>errfile
fi
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

if [ $obsprep_lghtn -eq 1 ] ; then
  lghtng_bufr="LightningInGSI_bufr.bufr"
else
  lghtng_bufr="LightningInGSI.bufr"
fi
if [ -f ${DATA}/${lghtng_bufr} ] ; then
  cpreq ${DATA}/${lghtng_bufr} ${COMINobsproc_rtma3d}/${RUN}.t${cyc}z.${lghtng_bufr}
else
  msg="WARNING $pgm terminated normally but ${DATA}/${lghtng_bufr} does NOT exist."
  ${ECHO} "$msg"
  postmsg "$jlogfile" "$msg"
  exit 1
fi

exit 0
