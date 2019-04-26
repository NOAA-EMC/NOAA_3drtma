#!/bin/ksh --login

# Set IMPI I/O performance variables
export I_MPI_EXTRA_FILESYSTEM=on
export I_MPI_EXTRA_FILESYSTEM_LIST=lustre:panfs

# Set up some constants
export WRF_NAMELIST=${DATAHOME}/namelist.input
export WRF=${WRF_ROOT}/wrf.exe

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

# Check to make sure the wrf executable exists
if [ ! -x ${WRF} ]; then
  ${ECHO} "ERROR: ${WRF} does not exist, or is not executable"
  exit 1
fi

if [ ! -d ${DATAROOT} ]; then
  ${ECHO} "ERROR: ${DATAROOT} does not exist"
  exit 1
fi

# Make sure the forecast length is defined
if [ ! ${FCST_LENGTH} ]; then
  ${ECHO} "ERROR: \$FCST_LENGTH is not defined!"
  exit 1
fi

# Make sure START_TIME is specified
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined"
  exit 1
fi

# Check to make sure WRF DAT files exist
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    ${ECHO} "ERROR: ${file} either does not exist or is empty"
    exit 1
  fi
done

# Convert START_TIME from 'YYYYMMDDHH' format to Unix date format, e.g. "Fri May  6 19:50:23 GMT 2005"
if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
  START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
else
  ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
  exit 1
fi
START_TIME=`${DATE} -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`

# Get the end time string
END_TIME=`${DATE} -d "${START_TIME}  ${FCST_LENGTH} seconds"`

# Print run parameters
${ECHO}
${ECHO} "update_vars.ksh started at `${DATE}`"
${ECHO}
${ECHO} "WRF_ROOT      = ${WRF_ROOT}"
${ECHO} "DATAROOT = ${DATAROOT}"
${ECHO} "DATAHOME = ${DATAHOME}"
${ECHO}
${ECHO} "FCST LENGTH   = ${FCST_LENGTH}"
${ECHO}
${ECHO} "START TIME = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${START_TIME}"`
${ECHO} "  END TIME = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${END_TIME}"`
${ECHO}

# Setup alternative START_TIME options
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}
${MKDIR} -p ${workdir}
if [ "`stat -f -c %T ${workdir}`" == "lustre" ]; then
  lfs setstripe --count 8 ${workdir}
fi
cd ${workdir}

${CP} ${STATIC_DIR}/wrf.nl ${WRF_NAMELIST}

# Check to make sure the wrfinput_d01 file exists
if [ -r ${DATAGSIHOME}/wrf_inout ]; then
  ${ECHO} " Initial condition ${DATAGSIHOME}/wrf_inout "
  ${LN} -s ${DATAGSIHOME}/wrf_inout ${DATAHOME}/wrfinput_d01
  #${LN} -s ${DATAHOME}/wrfinput_d01 ${DATAHOME}/wrfvar_output
else
  ${ECHO} "ERROR: ${DATAGSIHOME}/wrf_inout does not exist, or is not readable"
  exit 1
fi

# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  ${RM} -f `basename ${file}`
  ${LN} -s ${file}
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
${CAT} ${WRF_NAMELIST} | ${SED} "s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/" \
   | ${SED} "s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/"                   \
   | ${SED} "s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/"                       \
   | ${SED} "s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/"                     \
   | ${SED} "s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/"                 \
   | ${SED} "s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/"                 \
   > ${WRF_NAMELIST}.new
${MV} ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update end time in wrf namelist
${CAT} ${WRF_NAMELIST} | ${SED} "s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/" \
   | ${SED} "s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/"                   \
   | ${SED} "s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/"                       \
   | ${SED} "s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/"                     \
   | ${SED} "s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/"                 \
   | ${SED} "s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/"                 \
   > ${WRF_NAMELIST}.new
${MV} ${WRF_NAMELIST}.new ${WRF_NAMELIST}

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

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`

# Run wrf
${MPIRUN} ${WRF}
error=$?

# Save a copy of the RSL files
rsldir=rsl.wrf.${now}
${MKDIR} ${rsldir}
mv rsl.out.* ${rsldir}
mv rsl.error.* ${rsldir}

# Check the exit status of WRF
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${WRF} exited with status: ${error}"
  exit ${error}
else
  # Check to see if the 0h output is there:
  if [ ! -e "wrfout_d01_${time_str}" ]; then
    ${ECHO} "${WRF} failed at the first time step!"
    exit 1
  fi 
  
  # Output successful so write status to log
  #${ECHO} " Cycle ${YYYYMMDDHH}: ARW finished successfully at `${DATE}`" >> ${DATABASE_DIR}/loghistory/update_vars.log
  ${ECHO} "Assemble  REFL_10CM,U10,V10 back into wrf_inout"
  ${NCKS} -A -H -v REFL_10CM,U10,V10 wrfout_d01_${time_str} ${DATAGSIHOME}/wrf_inout

  ${ECHO} "update_vars.ksh completed successfully at `${DATE}`"
fi

exit 0
