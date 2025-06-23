#!/bin/ksh 
set -x

START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# BUFR Table including the description for HREF
${LN} -sf ${FIXgsi}/prepobs_prep_RAP.bufrtable ./prepobs_prep.bufrtable
if [ ! -s "./prepobs_prep.bufrtable" ]; then
  ${ECHO} "prepobs_prep.bufrtable does not exist or not readable"
  exit 1
fi

# WPS GEO_GRID Data
  ${LN} -sf ${FIXwps}/hrrr_geo_em.d01.nc ./geo_em.d01.nc
if [ ! -s "./geo_em.d01.nc" ]; then
  ${ECHO} "geo_em.d01.nc does not exist or not readable"
  exit 1 
fi

# print parameters for linking/processing
${ECHO} "START_TIME: "${START_TIME}
${ECHO} "SUBH_TIME: "${SUBH_TIME}
${ECHO} "YYYYMMDDHH: "${YYYYMMDDHH}

# Link to the NASA LaRC cloud data
${LN} -sf ${COMINPREP}/${NET}.${YYYYMMDD}/${NET}.t${HH}z.lgycld.tm00.bufr_d ./${NET}.t${cyc}z.lgycld.tm00.bufr_d
${LN} -sf ./${NET}.t${cyc}z.lgycld.tm00.bufr_d ./NASA_LaRC_cloud.bufr





# Build the namelist on-the-fly
${CAT} << EOF > namelist_nasalarc
&SETUP
analysis_time = ${YYYYMMDDHH},
bufrfile='NASALaRCCloudInGSI.bufr',
npts_rad=3,
ioption = 2,
/
EOF

# Run obs processor
export pgm="${NET}_process_cloud"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin processing NASA LaRC cloud data"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

CP_LN=${CP}
#${MPIRUN} ./${pgm} > ${pgmout} 2>errfile
mpiexec $EXECrtma3d/${pgm} >> ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $NET HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"
cpreq ${DATA}/namelist_nasalarc ${COMINobsproc_rtma3d}
cpreq ${DATA}/${NET}.t${cyc}z.lgycld.tm00.bufr_d ${COMINobsproc_rtma3d} 
targetfile="NASALaRCCloudInGSI.bufr"
if [ -f ${DATA}/${targetfile} ] ; then
  cpreq ${DATA}/${targetfile} ${COMINobsproc_rtma3d}/${RUN}.t${cyc}z.${targetfile}
else
  msg="WARNING $pgm terminated normally but ${DATA}/${targetfile} does NOT exist."
  ${ECHO} "$msg"
  postmsg "$jlogfile" "$msg"
  exit 1
fi

exit 0
