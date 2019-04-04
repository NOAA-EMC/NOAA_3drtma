#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

ulimit -s 512000

# Load modules
module load intel
module load mvapich2
#module load szip/2.1
#module load hdf5/1.8.9
#module load netcdf4/4.2.1.1
module load szip
module load hdf5
module load netcdf

# Set paths to unix commands
ECHO=/bin/echo
MKDIR=/bin/mkdir
RM=/bin/rm
LN=/bin/ln
LS=/bin/ls
CAT=/bin/cat
CP=/bin/cp
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed

# Set the path to the ssrc executable
SSRC=${GSI_ROOT}/ssrc.exe

# Set the path to the gsi executable
ENCODE_TAMDAR=${GSI_ROOT}/process_tamdar_netcdf.exe
APPEND_VSESONDE=${GSI_ROOT}/prepbufr_append_vessonndes.exe
APPEND_CLAMPS=${GSI_ROOT}/prepbufr_append_clamps.exe
APPEND_STICKNET=${GSI_ROOT}/prepbufr_append_sticknet.exe

# Make sure DATAHOME is defined
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi

# Make sure GSI_ROOT is defined and exists
if [ ! "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: \$GSI_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: GSI_ROOT directory '${GSI_ROOT}' does not exist!"
  exit 1
fi

if [ ! -d "${TAMDAR_ROOT}" ]; then
  ${ECHO} "ERROR: TAMDAR_ROOT directory '${TAMDAR_ROOT}' does not exist!"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  else
    if [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
      ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
      exit 1
    fi
  fi
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Make sure PREPBUFR is defined and that the directory exists
if [ ! "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: \$PREPBUFR is not defined"
  exit 1
fi
if [ ! -d "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: directory '${PREPBUFR}' does not exist!"
  exit 1
fi
if [ ! "${PREPBUFR_SAT}" ]; then
  ${ECHO} "ERROR: \$PREPBUFR_SAT is not defined"
  exit 1
fi
if [ ! -d "${PREPBUFR_SAT}" ]; then
  ${ECHO} "ERROR: directory '${PREPBUFR_SAT}' does not exist!"
  exit 1
fi
if [ ! "${EARLY}" ]; then
  ${ECHO} "ERROR: \$EARLY is not defined"
  exit 1
fi

# Make sure the ssrc executable exists
if [ ! -x "${SSRC}" ]; then
  ${ECHO} "ERROR: ${SSRC} does not exist!"
  exit 1
fi

# Make sure the GSI executable exists
if [ ! -x "${ENCODE_TAMDAR}" ]; then
  ${ECHO} "ERROR: ${ENCODE_TAMDAR} does not exist!"
  exit 1
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

# Compute date & time components for prepbufr filename
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
PREYYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME} 1 hour ago"`
YYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
PREYYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME} 1 hour ago"`
PREHH=`${DATE} +"%H" -d "${START_TIME} 1 hour ago"`

# Copy the prepbufr to obs directory so we never do I/O to /public directly
if [ ${EARLY} -eq 0 ]; then
  if [ -r "${PREPBUFR}_test/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test" ]; then
    ${CP} ${PREPBUFR}_test/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test .
    ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
  else
    if [ -r "${PREPBUFR}/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}" ]; then
      ${ECHO} "Warning: ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test does not exist!"
      ${CP} ${PREPBUFR}/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} .
      ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
    else
      ${ECHO} "Warning: ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} does not exist!"
      ${ECHO} "ERROR: No prepbufr files exist!"
      exit 1
    fi
  fi
else
  if [ ${EARLY} -eq 1 ]; then
    if [ -r "${PREPBUFR}_test/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test" ]; then
      ${CP} ${PREPBUFR}_test/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test .
      ${LN} -s ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
    else
      if [ -r "${PREPBUFR}/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}" ]; then
        ${CP} ${PREPBUFR}/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} .
        ${LN} -s ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
      else
        ${ECHO} "Warning: ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} does not exist!"
        ${ECHO} "ERROR: No prepbufr files exist!"
        exit 1
      fi
    fi
  else
    ${ECHO} "ERROR: EARLY ${EARLY} is not defined or invalid"
    exit 1
  fi
fi

# Set links to radiance data if available
if [ -r "${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamua.tm00.bufr_d.${YYYYMMDD}" ]; then
  ${CP} ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamua.tm00.bufr_d.${YYYYMMDD} .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.1bamua.tm00.bufr_d.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.1bamua
else
  ${ECHO} "Warning: ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamua.tm00.bufr_d.${YYYYMMDD} does not exist!"
fi

if [ -r "${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamub.tm00.bufr_d.${YYYYMMDD}" ]; then
  ${CP} ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamub.tm00.bufr_d.${YYYYMMDD} .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.1bamub.tm00.bufr_d.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.1bamub
else
  ${ECHO} "Warning: ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bamub.tm00.bufr_d.${YYYYMMDD} does not exist!"
fi

if [ -r "${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs3.tm00.bufr_d.${YYYYMMDD}" ]; then
  ${CP} ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs3.tm00.bufr_d.${YYYYMMDD} .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.1bhrs3.tm00.bufr_d.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs3
else
  ${ECHO} "Warning: ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs3.tm00.bufr_d.${YYYYMMDD} does not exist!"
fi

if [ -r "${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs4.tm00.bufr_d.${YYYYMMDD}" ]; then
  ${CP} ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs4.tm00.bufr_d.${YYYYMMDD} .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.1bhrs4.tm00.bufr_d.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs4
else
  ${ECHO} "Warning: ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bhrs4.tm00.bufr_d.${YYYYMMDD} does not exist!"
fi

if [ -r "${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bmhs.tm00.bufr_d.${YYYYMMDD}" ]; then
  ${CP} ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bmhs.tm00.bufr_d.${YYYYMMDD} .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.1bmhs.tm00.bufr_d.${YYYYMMDD} newgblav.${YYYYMMDD}.rap.t${HH}z.1bmhs
else
  ${ECHO} "Warning: ${PREPBUFR_SAT}/${YYYYJJJHH00}.rap.t${HH}z.1bmhs.tm00.bufr_d.${YYYYMMDD} does not exist!"
fi

# Radial velocity included
if [ -r "${RADVELLEV2_DIR}/${YYYYJJJHH00}.rap.t${HH}z.nexrad.tm00.bufr_d" ]; then
  ${CP} ${RADVELLEV2_DIR}/${YYYYJJJHH00}.rap.t${HH}z.nexrad.tm00.bufr_d .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.nexrad.tm00.bufr_d newgblav.${YYYYMMDD}.rap.t${HH}z.nexrad
elif [ -r "${RADVELLEV2_DIR}/${YYYYJJJHH00}.rap_e.t${HH}z.nexrad.tm00.bufr_d" ]; then
  ${CP} ${RADVELLEV2_DIR}/${YYYYJJJHH00}.rap_e.t${HH}z.nexrad.tm00.bufr_d .
  ${LN} -s ${YYYYJJJHH00}.rap_e.t${HH}z.nexrad.tm00.bufr_d newgblav.${YYYYMMDD}.rap.t${HH}z.nexrad
else
  ${ECHO} "Warning: ${RADVELLEV2_DIR}/${YYYYJJJHH00}.rap.t${HH}z.nexrad.tm00.bufr_d does not exist!"
fi

if [ -r "${RADVELLEV2P5_DIR}/${YYYYJJJHH00}.rap.t${HH}z.radwnd.tm00.bufr_d" ]; then
  ${CP} ${RADVELLEV2P5_DIR}/${YYYYJJJHH00}.rap.t${HH}z.radwnd.tm00.bufr_d .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.radwnd.tm00.bufr_d newgblav.${YYYYMMDD}.rap.t${HH}z.radwnd
else
  ${ECHO} "Warning: ${RADVELLEV2P5_DIR}/${YYYYJJJHH00}.rap.t${HH}z.radwnd.tm00.bufr_d does not exist!"
fi

#  AMV wind
if [ -r "${SATWND_DIR}/${YYYYJJJHH00}.rap.t${HH}z.satwnd.tm00.bufr_d" ]; then
  ${CP} ${SATWND_DIR}/${YYYYJJJHH00}.rap.t${HH}z.satwnd.tm00.bufr_d .
  ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.satwnd.tm00.bufr_d newgblav.${YYYYMMDD}.rap.t${HH}z.satwnd
else
  ${ECHO} "Warning: ${SATWND_DIR}/${YYYYJJJHH00}.rap.t${HH}z.satwnd.tm00.bufr_d does not exist!"
fi

#  Lightning obs 
if [ -r "${BUFRLIGHTNING}/${YYYYMMDDHH}.rap.t${HH}z.lghtng.tm00.bufr_d" ]; then
  ${CP} ${BUFRLIGHTNING}/${YYYYMMDDHH}.rap.t${HH}z.lghtng.tm00.bufr_d .
  ${LN} -s ${YYYYMMDDHH}.rap.t${HH}z.lghtng.tm00.bufr_d newgblav.${YYYYMMDD}.rap.t${HH}z.lghtng
else
  ${ECHO} "Warning: ${BUFRLIGHTNING}/${YYYYMMDDHH}.rap.t${HH}z.lghtng.tm00.bufr_d does not exist!"
fi

# TC vitals
if [ -r "${TCVITALS_DIR}/${YYYYMMDDHH}00.tcvitals" ]; then
  ${CP} ${TCVITALS_DIR}/${YYYYMMDDHH}00.tcvitals .
  ${LN} -s ${YYYYMMDDHH}00.tcvitals newgblav.${YYYYMMDD}.tcvitals.t${HH}z
else
  ${ECHO} "Warning: ${TCVITALS_DIR}/${YYYYMMDDHH}00.tcvitals does not exist!"
fi


# Add TAMDAR data
# Save a copy of the GSI executable in the workdir
${CP} ${ENCODE_TAMDAR} .
#
# Link to the TAMDAR data
#
filenum=0
${LS} ${TAMDAR_ROOT}/${PREYYJJJHH}*5r > tamdar_filelist
${LS} ${TAMDAR_ROOT}/${YYJJJHH}*5r >> tamdar_filelist
filenum=`more tamdar_filelist | wc -l`
filenum=$(( filenum -3 ))

echo "found tamdar files: ${filenum}"

# Build the namelist on-the-fly
${CAT} << EOF > tamdar.namelist
 &SETUP
   analysis_time = ${YYYYMMDDHH},
   time_window = 0.5,
   TAMDAR_filenum  = ${filenum},
 /
EOF

# Run obs pre-processor
${CP} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr prepbufr_tamdar
${ENCODE_TAMDAR} > stdout_append_tamdar 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${ENCODE_TAMDAR} crashed  Exit status=${error}"
  exit ${error}
fi

# Append VSE sondes to prepbufr
VSESONDEPATH=/mnt/lfs3/projects/wrfruc/dturner/vse/sonde_outgoing
${LS} ${VSESONDEPATH}/sonde.*.${YYYYMMDD}${HH}??.txt > filelist_vsesonde
#${LS} ${VSESONDEPATH}/sonde.*.${PREYYYYMMDD}${PREHH}??.txt > filelist_vsesonde
numsonde=`more filelist_vsesonde | wc -l`
numsonde=$((numsonde - 3 ))

cat << EOF > namelist_vsesonde
 &setup
  sondefilelist='filelist_vsesonde'
  numSondes=${numsonde}
  cycledate=${YYYYMMDD}
  cyclehour=${HH}
  /
EOF

if [[ ${numsonde} -gt 00 ]]; then
   ${CP} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr prepbufr_vsesondes
   ${APPEND_VSESONDE} > stdout_append_vsedonde 2>&1
   error=$?
   if [ ${error} -ne 0 ]; then
     ${ECHO} "ERROR: ${APPEND_VSESONDE} crashed  status=${error}"
   #  exit ${error}   # we should continue data process even this step has problem
   else
     ${ECHO} "${numsonde} VSE sondes have been appended to prepbufr"
   fi
else
   echo "cannot find VSE sondes at this time ${YYYYMMDD}${HH}"
fi

# Append VSE CLAMPS data to prepbufr
CLAMPSPATH=/mnt/lfs1/projects/public/data/vortex-se/clamps1
${LS} ${CLAMPSPATH}/clamps1.*.${YYYYMMDD}.${HH}05.txt > filelist_clamps
#${LS} ${CLAMPSPATH}/clamps1.*.${PREYYYYMMDD}.${PREHH}55.txt > filelist_clamps

CLAMPSPATH=/mnt/lfs1/projects/public/data/vortex-se/clamps2
${LS} ${CLAMPSPATH}/clamps2.*.${PREYYYYMMDD}.${PREHH}55.txt >> filelist_clamps

numclamps=`more filelist_clamps | wc -l`
numclamps=$((numclamps - 3 ))

cat << EOF > namelist_clamps
 &setup
  sondefilelist='filelist_clamps'
  numSondes=${numclamps}
  cycledate=${YYYYMMDD}
  cyclehour=${HH}
  /
EOF

if [[ ${numclamps} -gt 00 ]]; then

   if [ -r "prepbufr_vsesondes" ]; then
     ${CP} prepbufr_vsesondes prepbufr_clamps
   else
     ${CP} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr prepbufr_clamps
   fi

   ${APPEND_CLAMPS} > stdout_append_clamps 2>&1
   error=$?
   if [ ${error} -ne 0 ]; then
     ${ECHO} "ERROR: ${APPEND_CLAMPS} crashed  status=${error}"
   #  exit ${error}   # we should continue data process even this step has problem
   else
     ${ECHO} "${numclamps} CLAMPS profiles have been appended to prepbufr"
   fi
else
   echo "cannot find CLAMPS data at this time ${YYYYMMDD}${HH}"
#   echo "cannot find CLAMPS data at this time ${PREYYYYMMDD}${PREHH}"
fi

# Append VSE Stesonet observations to prepbufr
if [ "${STICKNET}" ]; then

  ${ECHO} "StickNet directory = ${STICKNET}"

  if [ -r "${STICKNET}/${YYYYMMDD}_${HH}0000.csv" ]; then

    ${ECHO} "copying StickNet data ${YYYYMMDD}_${HH}0000.csv"
    ${CP} ${STICKNET}/${YYYYMMDD}_${HH}0000.csv .
    ${LN} -s ${YYYYMMDD}_${HH}0000.csv sticknet_obs.input
    ${ECHO} ${YYYYMMDD}${HH} > idate.input

    if [ -r "prepbufr_clamps" ]; then
      ${CP} prepbufr_clamps prepbufr_sticknet
    elif [ -r "prepbufr_vsesondes" ]; then
      ${CP} prepbufr_vsesondes prepbufr_sticknet
    else
      ${CP} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr prepbufr_sticknet
    fi

    ${LN} -s prepbufr_sticknet prepbufr
    if [ ! -x ${APPEND_STICKNET} ]; then
      ${ECHO} "Executable for appending StickNet data not found:  ${APPEND_STICKNET}"
    else
      ${APPEND_STICKNET} > stdout_append_sticknet 2>&1
    fi
    ${RM} -f prepbufr

  else
    ${ECHO} "StickNet data at current time not found:  ${YYYYMMDD}_${HH}0000.csv"
  fi

fi

# Add nacelle, tower and sodar observations if available
${CP} newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr prepbufr_wfip
if [ -r "${NACELLE_RSD}/${YYJJJHH}000010o" ]; then
  ${LN} -s ${NACELLE_RSD}/${YYJJJHH}000010o ./nacelle_restriced.nc
  ${CP} ${GSI_ROOT}/process_nacelledata_rt.exe .
  ./process_nacelledata_rt.exe > stdout_nacelledata
  ${RM} -f nacelle_restriced.nc
else
  ${ECHO} "Warning: ${NACELLE_RSD}/${YYJJJHH}000010o does not exist!"
fi

if [ -r "${TOWER_RSD}/${YYJJJHH}000010o" ]; then
  ${LN} -s ${TOWER_RSD}/${YYJJJHH}000010o ./tower_restricted.nc
  ${LN} -s ${TOWER_RSD}/${YYJJJHH}000010o ./tower_data.nc
  ${CP} ${GSI_ROOT}/process_towerdata_rt.exe .
  ./process_towerdata_rt.exe > stdout_tower_re
  ${RM} -f tower_restricted.nc
  ${RM} -f tower_data.nc
else
  ${ECHO} "Warning: ${TOWER_RSD}/${YYJJJHH}000010o does not exist!"
fi

if [ -r "${TOWER_NRSD}/${YYJJJHH}000100o" ]; then
  ${LN} -s ${TOWER_NRSD}/${YYJJJHH}000100o ./tower_public.nc
  ${LN} -s ${TOWER_NRSD}/${YYJJJHH}000100o ./tower_data.nc
  ${CP} ${GSI_ROOT}/process_towerdata_rt.exe .
  ./process_towerdata_rt.exe > stdout_tower_nr
  ${RM} -f tower_public.nc
  ${RM} -f tower_data.nc
else
  ${ECHO} "Warning: ${TOWER_NRSD}/${YYJJJHH}000100o does not exist!"
fi

if [ -r "${SODAR_NRSD}/${YYJJJHH}000015o" ]; then
  ${LN} -s ${SODAR_NRSD}/${YYJJJHH}000015o ./sodar_data.nc
  ${CP} ${GSI_ROOT}/process_sodardata_rt.exe .
  ./process_sodardata_rt.exe > stdout_sodar_nr
  ${RM} -f sodar_data.nc
else
  ${ECHO} "Warning: ${SODAR_NRSD}/${YYJJJHH}000015o does not exist!"
fi

${ECHO} "End of conventional.ksh"
exit 0
