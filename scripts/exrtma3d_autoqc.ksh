#!/bin/ksh

set -x 

#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=`${DATE} -d "${PDY} ${cyc} ${subcyc} minutes"`
echo $START_TIME
echo $cyc

# Constants needed for mesonet wind bias correction algorithm
tinf=30. # Constant timescale associated with an observation

# Compute date & time components for the analysis time
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
export PDYprev=${YYYYMMDDHH}
export PDYprev_dir=${COMOUTautoqc_rtma3d}
while [ ${i} -lt ${max_cycs} ]; do
  probe=`${NDATE} -${i} ${YYYYMMDDHH}`
  probe_YYYYMMDD=`echo $probe | cut -c 1-8`
  probe_HH=`echo $probe | cut -c 9-10`
  probe_dir=${COMOUTautoqc_base}/${NET}.${probe_YYYYMMDD}/${dom}/autoqcprd.t${probe_HH}z # MTM - revert NET to RUN
  if [ -s ${probe_dir}/${RUN}.t${probe_HH}z.accept_merged_${probe}.txt ]; then
    echo $probe
    export PDYprev=${probe}
    export PDYprev_dir=${probe_dir}
    break
  else
    let "i=i+1"
  fi
done

# Find the most recent cycle with computed long-term reject lists
max_cycs=168 # Number of cycles to look back
i=1
export probecyc_long=${YYYYMMDDHH}
while [ ${i} -lt ${max_cycs} ]; do
  probe=`${NDATE} -${i} $YYYYMMDDHH`
  probe_YYYYMMDD=`echo $probe | cut -c 1-8`
  probe_HH=`echo $probe | cut -c 9-10`
  probe_dir=${COMOUTautoqc_base}/${NET}.${probe_YYYYMMDD}/${dom}/autoqcprd.t${probe_HH}z # MTM - revert NET to RUN
  if [ $probe_HH -eq "23" ] && [ -s ${probe_dir}/${RUN}.t${probe_HH}z.accept_merged_${probe}.txt ]; then
    export probecyc_long=${probe}
    break
  else
    let "i=i+1"
  fi
done

# Convert the diagnostic files into a readable format
for ftype in ges anl; do
  if [ ${ftype} == 'ges' ]; then
    if [ ${dom} == 'conus' ]; then
      cpreq ${COMOUT}/${RUN}.t${cyc}z.diag_conv_ges.gz ${GESdiagconv_FNAME}.gz
    else
      cpreq ${COMOUT}/${RUN}ak.t${cyc}z.diag_conv_ges.gz ${GESdiagconv_FNAME}.gz
    fi
    export diagfile=${GESdiagconv_FNAME}
  elif [ ${ftype} == 'anl' ]; then
    if [ ${dom} == 'conus' ]; then
      cpreq ${COMOUT}/${RUN}.t${cyc}z.diag_conv_anl.gz ${ANLdiagconv_FNAME}.gz
    else
      cpreq ${COMOUT}/${RUN}ak.t${cyc}z.diag_conv_anl.gz ${ANLdiagconv_FNAME}.gz
    fi
    export diagfile=${ANLdiagconv_FNAME}
  fi
  gunzip ${diagfile}.gz
  ln -sf ${diagfile} diag_conv.dat

  # namelist file for running READDIAG (=> rtma3d_read_diag.exe)
  [[ -f ./namelist.conv ]] && rm -f ./namelist.conv
cat << EOF > ./namelist.conv
&iosetup
    dump_pseudo_obs_too=.true.,
/
EOF

  ${EXECrtma3d}/${NET}_read_diag diag_conv.dat
  mv diag_results ${diagfile}
  rm diag_conv.dat
  [[ -f ./namelist.conv ]] && mv ./namelist.conv ./namelist.conv.readdiag.${ftype}  # saving namelist for check

done

#############################################################################

export pgm="rtma3d_autoqc"
# Create the working directory and cd into it
#workdir=${DATA}
#cd ${workdir}
#cd ${DATA}
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
python ${USHrtma3d}/gen_database_autoqc.py ${RUN} ${YYYYMMDDHH} ${DATA} ${COMOUTautoqc_rtma3d} ${PDYprev_dir} ${PDYprev} ${probecyc_long} ${tinf} ${dom}

export err=$?; err_chk
if [ err -eq 0 ] ; then
echo "AUTOQC SUCCESS."
elif [ err -gt 0 ] ; then
echo "AUTOQC FAILED."
#mail -s "AUTOQC failed at cycle: $YYYYMMDDHH" Edward.Colon@noaa.gov < /dev/null
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

#ls -l ${GESINhrrr_rtma3d} > ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}.list

exit 0

