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
if [ ! -f ${EXECrtma3d}/${exefile_name_updatevars_wrf} ]; then
  ${ECHO} "ERROR: executable '${EXECrtma3d}/${exefile_name_updatevars_wrf}' does not exist!"
  exit 1
fi
if [ ! -f ${EXECrtma3d}/${exefile_name_updatevars_ncfields} ]; then
  ${ECHO} "ERROR: executable '${EXECrtma3d}/${exefile_name_updatevars_ncfields}' does not exist!"
  exit 1
fi
if [ ! -f ${EXECrtma3d}/${exefile_name_updatevars_ndown} ]; then
  ${ECHO} "ERROR: executable '${EXECrtma3d}/${exefile_name_updatevars_ndown}' does not exist!"
  exit 1
fi




# Check to make sure required directory defined and existed
check_if_defined "FCST_LENGTH" "DATA_GSIANL" "FIXwrf" "PDY" "cyc" "subcyc"
check_dirs_exist "DATA_GSIANL" "FIXwrf"

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${FIXwrf}/run/LANDUSE.TBL          \
                     ${FIXwrf}/run/RRTM_DATA            \
                     ${FIXwrf}/run/RRTM_DATA_DBL        \
                     ${FIXwrf}/run/RRTMG_LW_DATA        \
                     ${FIXwrf}/run/RRTMG_LW_DATA_DBL    \
                     ${FIXwrf}/run/RRTMG_SW_DATA        \
                     ${FIXwrf}/run/RRTMG_SW_DATA_DBL    \
                     ${FIXwrf}/run/VEGPARM.TBL          \
                     ${FIXwrf}/run/GENPARM.TBL          \
                     ${FIXwrf}/run/SOILPARM.TBL         \
                     ${FIXwrf}/run/MPTABLE.TBL          \
                     ${FIXwrf}/run/URBPARM.TBL          \
                     ${FIXwrf}/run/URBPARM_UZE.TBL      \
                     ${FIXwrf}/run/ETAMPNEW_DATA        \
                     ${FIXwrf}/run/ETAMPNEW_DATA.expanded_rain        \
                     ${FIXwrf}/run/ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${FIXwrf}/run/ETAMPNEW_DATA_DBL    \
                     ${FIXwrf}/run/co2_trans            \
                     ${FIXwrf}/run/ozone.formatted      \
                     ${FIXwrf}/run/ozone_lat.formatted  \
                     ${FIXwrf}/run/ozone_plev.formatted \
                     ${FIXwrf}/run/bulkdens.asc_s_0_03_0_9 \
                     ${FIXwrf}/run/bulkradii.asc_s_0_03_0_9  \
                     ${FIXwrf}/run/capacity.asc         \
                     ${FIXwrf}/run/CCN_ACTIVATE.BIN     \
                     ${FIXwrf}/run/coeff_p.asc          \
                     ${FIXwrf}/run/coeff_q.asc          \
                     ${FIXwrf}/run/constants.asc        \
                     ${FIXwrf}/run/kernels.asc_s_0_03_0_9  \
                     ${FIXwrf}/run/kernels_z.asc           \
                     ${FIXwrf}/run/masses.asc              \
                     ${FIXwrf}/run/termvels.asc            \
                     ${FIXwrf}/run/wind-turbine-1.tbl      \
                     ${FIXwrf}/run/tr49t85              \
                     ${FIXwrf}/run/tr49t67              \
                     ${FIXwrf}/run/tr67t85              \
                     ${FIXwrf}/run/grib2map.tbl         \
                     ${FIXwrf}/run/gribmap.txt          \
                     ${FIXwrf}/run/freezeH2O.dat        \
                     ${FIXwrf}/run/qr_acr_qg.dat        \
                     ${FIXwrf}/run/qr_acr_qs.dat        \
                     ${FIXwrf}/run/eclipse_besselian_elements.dat
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
cd ${DATAHOME}
${ECHO} "enter working directory:${DATAHOME}"

export WRF_NAMELIST=${DATAHOME}/namelist.input
if [ ${DOMAIN} == "conus" ] ; then
${CP} ${PARMwrf}/wrf.nl ${WRF_NAMELIST} 
elif [ ${DOMAIN} == "alaska" ] ; then 
${CP} ${PARMwrf}/jet.nl ${WRF_NAMELIST}
fi

# Check to make sure the wrfinput_d01 file exists
#if [ -r ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} ]; then
#  ${ECHO} " Initial condition ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} "
#  ${LN} -s ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} wrfinput_d01
#   ${CP} ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} wrfinput_d01
#else
#  ${ECHO} "ERROR: ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} does not exist, or is not readable"
#  exit 1
#fi

if [ -r ${DATAHOME}/wrf_inout ]; then
${ECHO} " Initial condition ${DATAHOME}/wrf_inout "
${LN} -s ${DATAHOME}/wrf_inout wrfinput_d01
#${LN} -s /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/wrf_inout wrfinput_d01
else
  ${ECHO} "ERROR: ${DATAHOME}/wrf_inout does not exist, or is not readable"
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

#start_year=2020
#start_month=01
#start_day=22
#start_hour=16
#start_minute=00
#start_second=00
#end_year=2020
#end_month=01
#end_day=22
#end_hour=16
#end_minute=00
#end_second=20


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

# Update the run_days,run_hours,start_time,end_time in wrf namelist.input
${SED} -i "\
   s/\(${run}_${day}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_days}/;    \
   s/\(${run}_${hour}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_hours}/;  \
\
   s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/;    \
   s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/;  \
   s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/;      \
   s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/;    \
   s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/;\
   s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/;\
\
   s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/;        \
   s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/;      \
   s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/;          \
   s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/;        \
   s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/;    \
   s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/;    \
" ${WRF_NAMELIST}

chmod 755 ${WRF_NAMELIST}

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

# Run WRF to update reflectivity fields
export pgm="rtma3d_updatevars"
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
${CP_LN} ${EXECrtma3d}/${exefile_name_updatevars} ${pgm}
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
  ${ECHO} "WRF failed at the first time step!"
  exit 1
fi 

${LN} -s wrfout_d01_${time_str} wrfout_d01
${CP_LN} ${EXECrtma3d}/${exefile_name_update_ncfields} .

# Output successful so write status to log
${ECHO} "Assemble Reflectivity fields back into wrf_inout"

#${NCKS} -A -H -v REFL_10CM,COMPOSITE_REFL_10CM,REFL_10CM_1KM,REFL_10CM_4KM,U10,V10 wrfout_d01_${time_str} ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} 
#${NCKS} -A -H -v REFL_10CM,COMPOSITE_REFL_10CM,REFL_10CM_1KM,REFL_10CM_4KM wrfout_d01_${time_str} ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} 

${exefile_name_update_ncfields} wrfout_d01 wrf_inout 
export err=$?; err_chk

if [ -f ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} ]; then
  ${ECHO} "Erasing the GSI generated analysis file to be replaced by modified analysis."
  ${RM} ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
fi

${CP_LN} -p wrf_inout ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}

${ECHO} "update_vars.ksh completed successfully at `${DATE}`"

# Saving some files
${CP} -p namelist.input  ${COMOUTwrf_rtma3d}/namelist.input_${cycle_str}


msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
