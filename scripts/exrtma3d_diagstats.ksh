#!/bin/ksh --login
set -x
check_if_defined() { #usage: check_if_defined "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ -z "${path}" ]; then
      ${ECHO} "ERROR: \$${str} is not defined"; exit 1
    fi
  done
}
check_dirs_exist() { #usage: check_dirs_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -d ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
  done
}
check_files_exist() { #usage: check_files_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -f ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
  done
}

# make sure executable exists
check_if_defined "GSIRUN_DIR" "exef_diagconv" "exef_diagrad" "exef_count" "GSIRUN_CORES" 
check_files_exist "exef_diagconv" "exef_diagrad" "exef_count"
check_dirs_exist "GSIRUN_DIR"

#link stdout, fort.*, diag*
${LN} -sf `ls ${GSIRUN_DIR}/output_${PDY}.*gsianl*` stdout.gsianl
${LN} -sf ${GSIRUN_DIR}/fort.* 
${LN} -sf ${GSIRUN_DIR}/diag* .
${LN} -sf ${GSIRUN_DIR}/convinfo .
${SED} -n '/^OBS_PARA/,/m_berror_stats_reg/p' stdout.gsianl > obs_para.txt #for easy view
[[ ! -s "obs_para.txt" ]] && ${ECHO} "OBS_PARA section not found in GSI stdout!" && exit 1

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`
YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
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
  outfilename='./textdiag',
  cdate='${YYYYMMDDHHMM}',
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
${CP_LN} ${exef_diagconv} ${pgm} 
pwd
${DATA}/${pgm} > output_read_diag_conv 2>&1
export err=$?; err_chk

##currently rtma3d does not assimilate radiance
#   # Read radiance diag file
#   ${CAT} << EOF > namelist.rad
#    &iosetup
#     dirname='${DATA}',
#     outfilename='./textdiag',
#     cdate='${YYYYMMDDHHMM}',
#     nloop=1,0,1,0,0,
#     instrument='amsub_n16','amsub_n17','hirs3_n17',
#    /
#   EOF
#
#   export pgm="rtma3d_read_diag_rad.exe"
#   . prep_step
#   startmsg
#   msg="***********************************************************"
#   postmsg "$jlogfile" "$msg"
#   msg="  begin reading radiance diag files"
#   postmsg "$jlogfile" "$msg"
#   msg="***********************************************************"
#   postmsg "$jlogfile" "$msg"
#   ${CP_LN} ${exef_diagrad} ${pgm} 
#   ${DATA}/${pgm} > output_read_diag_rad 2>&1
#   export err=$?; err_chk

#  Data number summary
${CAT} << EOF > namelist.conv
 &iosetup
  infilename='obs_para.txt',
  outfilename='summary_count_obs.txt',
  cdate='${YYYYMMDDHHMM}',
  npe='${GSIRUN_CORES}',
 /     
EOF
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
${DATA}/${pgm} > output_count_obs 2>&1
export err=$?; err_chk

cat "summary_count_obs.txt" >> ${COMROOT}/loghistory/HRRR_GSI_countobs.log

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
