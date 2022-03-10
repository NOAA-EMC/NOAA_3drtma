#!/bin/ksh

set -x 

#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=`${DATE} -d "${PDY} ${cyc} ${subcyc} minutes"`
echo $START_TIME
echo $cyc

# Compute date & time components for the analysis time
YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

#HH_cycp1=`echo ${PDYHH_cycp1} | cut -c 9-10`
#HH_cycm1=`echo ${PDYHH_cycm1} | cut -c 9-10`
#YYYYMMDDHH_m1hr=`echo ${PDYHH_cycm1} | cut -c 1-10`

# Find the directory containing the previous database file
max_cycs=168 # Number of cycles to look back
i=1
export PDYprev=${YYYYMMDDHHMM}
export PDYprev_dir=${COMOUTautoqc_rtma3d}
while [ ${i} -lt ${max_cycs} ]; do
  probe=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.4/exec/ips/ndate -${i} ${YYYYMMDDHH}`
  probe_YYYYMMDD=`echo $probe | cut -c 1-8`
  probe_HH=`echo $probe | cut -c 9-10`
  probe_MM=`echo $YYYYMMDDHHMM | cut -c 11-12`
  probe_dir=${COMOUTautoqc_base}/${RUN}.${probe_YYYYMMDD}/autoqcprd.t${probe_HH}${probe_MM}z
  if [ -s ${probe_dir}/done.${probe_YYYYMMDD}${probe_HH}${probe_MM} ]; then
    echo $probe
    export PDYprev=${probe}${probe_MM}
    export PDYprev_dir=${probe_dir}
    break
  else
    let "i=i+1"
  fi
done

# Find the most recent cycle with computed long-term reject lists
max_cycs=168 # Number of cycles to look back
i=1
export probecyc_long=${YYYYMMDDHHMM}
while [ ${i} -lt ${max_cycs} ]; do
  probe=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.4/exec/ips/ndate -${i} $YYYYMMDDHH`
  probe_YYYYMMDD=`echo $probe | cut -c 1-8`
  probe_HH=`echo $probe | cut -c 9-10`
  probe_MM=`echo $YYYYMMDDHHMM | cut -c 11-12`
  probe_dir=${COMOUTautoqc_base}/${RUN}.${probe_YYYYMMDD}/autoqcprd.t${probe_HH}${probe_MM}z
  if [ $probe_HH -eq "23" ] && [ -s ${probe_dir}/done.${probe_YYYYMMDD}${probe_HH}${probe_MM} ]; then
    export probecyc_long=${probe}${probe_MM}
    break
  else
    let "i=i+1"
  fi
done

# Convert the diagnostic files into a readable format
for ftype in ges anl; do
  if [ ${ftype} == 'ges' ]; then
    cpreq ${COMOUTgsi_rtma3d}/${GESdiagconv_FNAME}.gz .
    export diagfile=${GESdiagconv_FNAME}
  elif [ ${ftype} == 'anl' ]; then
    cpreq ${COMOUTgsi_rtma3d}/${ANLdiagconv_FNAME}.gz .
    export diagfile=${ANLdiagconv_FNAME}
  fi
  gunzip ${diagfile}.gz
  ln -sf ${diagfile} diag_conv.dat
  ${READDIAG} diag_conv.dat
  mv diag_results ${diagfile}
  rm diag_conv.dat

done

#############################################################################

export pgm="rtma3d_autoqc"
# Create the working directory and cd into it
#workdir=${DATA}
#cd ${workdir}
#cd ${DATA}
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
python ${NWROOT}/sorc/rtma_autoqc.fd/gen_database_autoqc.py ${YYYYMMDDHHMM} ${DATA} ${COMOUTautoqc_rtma3d} ${PDYprev_dir} ${PDYprev} ${probecyc_long}

export err=$?; err_chk
if [ err -eq 0 ] ; then
echo "AUTOQC SUCCESS."
elif [ err -gt 0 ] ; then
echo "AUTOQC FAILED."
#mail -s "AUTOQC failed at cycle: $YYYYMMDDHHMM" Edward.Colon@noaa.gov < /dev/null
fi

${CP} -p * ${COMOUTautoqc_rtma3d}

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  finish generatinng automated reject lists"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

#export err=$? ; err_chk

#ls -l ${GESINhrrr_rtma3d} > ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}_${subcyc}.list

exit 0

