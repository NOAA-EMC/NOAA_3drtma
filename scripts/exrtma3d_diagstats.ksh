#!/bin/ksh --login
set -x
check_files_exist() { #usage: check_dirs_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -f ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
  done
}

# make sure executable exists
check_files_exist "exef_diagconv" "exef_diagrad" "exef_count"

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`
YYMMDDHH=`${DATE} +"%y%m%d%H" -d "${START_TIME}"`
if [ "${envir}" == "esrl" ]; then #Jet
  CP_LN="${LN} -sf"
else
  CP_LN=${CP}
fi

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# Read conventional observation diag files
${CAT} << EOF > namelist.conv
 &iosetup
  dirname='${DATA}',
  outfilename='./diag_results',
  ndate=${YYMMDDHH},
  nloop=1,0,1,0,0,
 /     
EOF

export pgm="rtma3d_read_diag_conv.exe"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin reading conventional diag files"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
${CP_LN} ${exef_diag_conv} ${pgm} 
${pgm} > stdout_read_diag_conv 2>&1
export err=$?; err_chk

# Read radiance diag file
${CAT} << EOF > namelist.rad
 &iosetup
  dirname='${DATA}',
  outfilename='./diag_results',
  ndate=${YYMMDDHH},
  nloop=1,0,1,0,0,
  instrument='amsub_n16','amsub_n17','hirs3_n17',
 /
EOF

export pgm="rtma3d_read_diag_rad.exe"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin reading radiance diag files"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
${CP_LN} ${exef_diag_rad} ${pgm} 
${pgm} > stdout_read_diag_rad 2>&1
export err=$?; err_chk

#  Data number summary
#
export pgm="rtma3d_count_obs.exe"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin counting observation numbers"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
${CP_LN} ${exef_count} ${pgm} 
${pgm} 176 > stdout_count_obs 2>&1
export err=$?; err_chk

cat obs_num_summary.txt >> ${DATABASE_DIR}/loghistory/HRRR_GSI_dataNumber.log

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
