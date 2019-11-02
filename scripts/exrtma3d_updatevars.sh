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

if [ "${envir}" == "esrl" ]; then
  # Set IMPI I/O performance variables
  export I_MPI_EXTRA_FILESYSTEM=on
  export I_MPI_EXTRA_FILESYSTEM_LIST=lustre:panfs
fi

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_wrf} ]; then
  ${ECHO} "ERROR: WRF executable '${EXECrtma3d}/${exefile_name_wrf}' does not exist!"
  exit 1
fi

# Check to make sure required directory defined and existed
check_if_defined "FCST_LENGTH" "GSIRUN_DIR"
check_dirs_exist "GSIRUN_DIR"

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${STATIC_DIR}/run/LANDUSE.TBL          \
                     ${STATIC_DIR}/run/RRTM_DATA            \
                     ${STATIC_DIR}/run/RRTM_DATA_DBL        \
                     ${STATIC_DIR}/run/RRTMG_LW_DATA        \
                     ${STATIC_DIR}/run/RRTMG_LW_DATA_DBL    \
                     ${STATIC_DIR}/run/RRTMG_SW_DATA        \
                     ${STATIC_DIR}/run/RRTMG_SW_DATA_DBL    \
                     ${STATIC_DIR}/run/VEGPARM.TBL          \
                     ${STATIC_DIR}/run/GENPARM.TBL          \
                     ${STATIC_DIR}/run/SOILPARM.TBL         \
                     ${STATIC_DIR}/run/MPTABLE.TBL          \
                     ${STATIC_DIR}/run/URBPARM.TBL          \
                     ${STATIC_DIR}/run/URBPARM_UZE.TBL      \
                     ${STATIC_DIR}/run/ETAMPNEW_DATA        \
                     ${STATIC_DIR}/run/ETAMPNEW_DATA.expanded_rain        \
                     ${STATIC_DIR}/run/ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${STATIC_DIR}/run/ETAMPNEW_DATA_DBL    \
                     ${STATIC_DIR}/run/co2_trans            \
                     ${STATIC_DIR}/run/ozone.formatted      \
                     ${STATIC_DIR}/run/ozone_lat.formatted  \
                     ${STATIC_DIR}/run/ozone_plev.formatted \
                     ${STATIC_DIR}/run/bulkdens.asc_s_0_03_0_9 \
                     ${STATIC_DIR}/run/bulkradii.asc_s_0_03_0_9  \
                     ${STATIC_DIR}/run/capacity.asc         \
                     ${STATIC_DIR}/run/CCN_ACTIVATE.BIN     \
                     ${STATIC_DIR}/run/coeff_p.asc          \
                     ${STATIC_DIR}/run/coeff_q.asc          \
                     ${STATIC_DIR}/run/constants.asc        \
                     ${STATIC_DIR}/run/kernels.asc_s_0_03_0_9  \
                     ${STATIC_DIR}/run/kernels_z.asc           \
                     ${STATIC_DIR}/run/masses.asc              \
                     ${STATIC_DIR}/run/termvels.asc            \
                     ${STATIC_DIR}/run/wind-turbine-1.tbl      \
                     ${STATIC_DIR}/run/tr49t85              \
                     ${STATIC_DIR}/run/tr49t67              \
                     ${STATIC_DIR}/run/tr67t85              \
                     ${STATIC_DIR}/run/grib2map.tbl         \
                     ${STATIC_DIR}/run/gribmap.txt          \
                     ${STATIC_DIR}/run/freezeH2O.dat        \
                     ${STATIC_DIR}/run/qr_acr_qg.dat        \
                     ${STATIC_DIR}/run/qr_acr_qs.dat        \
                     ${STATIC_DIR}/run/eclipse_besselian_elements.dat
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    ${ECHO} "ERROR: ${file} either does not exist or is empty"
    exit 1
  fi
done

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
time_1hour_ago=`${DATE} -d "${START_TIME} 1 hour ago" +%Y%m%d%H`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
time_str2=`${DATE} "+%Y-%m-%d_%H_00_00" -d "${START_TIME}"`
END_TIME=`${DATE} -d "${START_TIME}  ${FCST_LENGTH} seconds"`

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

export WRF_NAMELIST=${DATA}/namelist.input
${CP} ${PARMwrf}/wrf.nl ${WRF_NAMELIST}

# Check to make sure the wrfinput_d01 file exists
if [ -r ${GSIRUN_DIR}/wrf_inout ]; then
  ${ECHO} " Initial condition ${GSIRUN_DIR}/wrf_inout "
  ${LN} -s ${GSIRUN_DIR}/wrf_inout wrfinput_d01
  #${LN} -s ${GSIRUN_DIR}/wrf_inout wrfvar_output
else
  ${ECHO} "ERROR: ${GSIRUN_DIR}/wrf_inout does not exist, or is not readable"
  exit 1
fi

# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  ${LN} -sf ${file} .
done

# Get the start and end time components
start_year=`${DATE} +%Y -d "${START_TIME}"`
start_month=`${DATE} +%m -d "${START_TIME}"`
start_day=`${DATE} +%d -d "${START_TIME}"`
start_hour=`${DATE} +%H -d "${START_TIME}"`
start_minute=`${DATE} +%M -d "${START_TIME}"`
start_second=`${DATE} +%S -d "${START_TIME}"`
end_year=`${DATE} +%Y -d "${END_TIME}"`
end_month=`${DATE} +%m -d "${END_TIME}"`
end_day=`${DATE} +%d -d "${END_TIME}"`
end_hour=`${DATE} +%H -d "${END_TIME}"`
end_minute=`${DATE} +%M -d "${END_TIME}"`
end_second=`${DATE} +%S -d "${END_TIME}"`

# Compute number of days and hours for the run
(( run_days = 0 ))
(( run_hours = 0 ))

# Create patterns for updating the wrf namelist
run=[Rr][Uu][Nn]
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
year=[Yy][Ee][Aa][Rr]
month=[Mm][Oo][Nn][Tt][Hh]
day=[Dd][Aa][Yy]
hour=[Hh][Oo][Uu][Rr]
minute=[Mm][Ii][Nn][Uu][Tt][Ee]
second=[Ss][Ee][Cc][Oo][Nn][Dd]

# Update the run_days in wrf namelist.input
${CAT} ${WRF_NAMELIST} | ${SED} "s/\(${run}_${day}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_days}/" \
   > ${WRF_NAMELIST}.new
${MV} ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the run_hours in wrf namelist
${CAT} ${WRF_NAMELIST} | ${SED} "s/\(${run}_${hour}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_hours}/" \
   > ${WRF_NAMELIST}.new
${MV} ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the start time in wrf namelist
${SED} -i "s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/" \
   | ${SED} "s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/"                   \
   | ${SED} "s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/"                       \
   | ${SED} "s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/"                     \
   | ${SED} "s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/"                 \
   | ${SED} "s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/"                 \
    ${WRF_NAMELIST}
# Update end time in wrf namelist
${SED} -i "s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/" \
   | ${SED} "s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/"                   \
   | ${SED} "s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/"                       \
   | ${SED} "s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/"                     \
   | ${SED} "s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/"                 \
   | ${SED} "s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/"                 \
   ${WRF_NAMELIST}

# Move existing rsl files to a subdir if there are any
${ECHO} "Checking for pre-existing rsl files"
if [ -f "rsl.out.0000" ]; then
  rsldir=rsl.`${LS} -l --time-style=+%Y%m%d%H%M%S rsl.out.0000 | ${CUT} -d" " -f 7`
  ${MKDIR} ${rsldir}
  ${ECHO} "Moving pre-existing rsl files to ${rsldir}"
  ${MV} rsl.out.* ${rsldir}
  ${MV} rsl.error.* ${rsldir}
else
  ${ECHO} "No pre-existing rsl files were found"
fi

# Run WRF
export pgm="rtma3d_wrf"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin updating reflectivity by a one-time_step WRF"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

if [ "${envir}" == "esrl" ]; then #Jet
  CP_LN="${LN} -sf"
else
  CP_LN=${CP}
fi
${CP_LN} ${EXECrtma3d}/${exefile_name_wrf} ${pgm}
now=`${DATE} +%Y%m%d%H%M%S`
${MPIRUN} ${pgm}
export err=$?; err_chk

# Save a copy of the RSL files
rsldir=rsl.wrf.${now}
${MKDIR} ${rsldir}
mv rsl.out.* ${rsldir}
mv rsl.error.* ${rsldir}

# Check to see if the 0h output is there:
if [ ! -e "wrfout_d01_${time_str}" ]; then
  ${ECHO} "${WRF} failed at the first time step!"
  exit 1
fi 

# Output successful so write status to log
#${ECHO} "Assemble  REFL_10CM,U10,V10 back into wrf_inout"
${ECHO} "Assemble Reflectivity fields back into wrf_inout"
#${NCKS} -A -H -v REFL_10CM,COMPOSITE_REFL_10CM,REFL_10CM_1KM,REFL_10CM_4KM,U10,V10 wrfout_d01_${time_str} ${GSIRUN_DIR}/wrf_inout
${NCKS} -A -H -v REFL_10CM,COMPOSITE_REFL_10CM,REFL_10CM_1KM,REFL_10CM_4KM wrfout_d01_${time_str} ${GSIRUN_DIR}/wrf_inout

${ECHO} "update_vars.ksh completed successfully at `${DATE}`"

# Saving some files
${CP} -p namelist.input  ${COMOUTwrf_rtma3d}/namelist.input_${cycle_str}

if [ "${envir}" != "esrl" ]; then #wcoss
  echo "Doing some extra things..."
fi

${RM} -f ${DATA}/sig*
${RM} -f ${DATA}/obs*
${RM} -f ${DATA}/pe*

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
