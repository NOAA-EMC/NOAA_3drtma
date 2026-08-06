#!/bin/ksh 

set -x

postmsg "$0 of $job has begun"

cd ${DATA}

# BUFR Table
# Note: the prepbufr table file used here is a special bufr table file, which was created
#       for RAP/HRRR, and must be specifically used in these RAP/HRRR based observation
#       preparation (obsprep) step. Because it incldues the special variable names defined for
#       observations of lightning, satellite observed cloud and MRMS Radar reflectivity, etc.,
#       which are not necessarily available in a general prepbufr table file.
#       Typically, this prepbufr table file is orginally named as "prepobs_prep_RAP.bufrtable",
#       not "prepobs_prep.bufrtable" (which is the general table file).
#       58080 Apr 22  2019 prepobs_prep_RAP.bufrtable
#       86751 Apr 22  2019 prepobs_prep.bufrtable
#       If an inappropriate bufr table file was used in obsprep, the observation data may not 
#       be encoded into a prepbufr file correctly, then later analysis may fail to find the
#       expected observation.  
cpreq -p ${PARMrtma3d}/${RUN}_prepobs_prep.bufrtable prepobs_prep.bufrtable
# WPS GEO_GRID Data
cpreq ${FIXrtma3d}/${RUN}_geo_em.d01.nc geo_em.d01.nc

# Copy the NASA LaRC cloud data
if [ -s "${COMINobsproc}/${RUN}.t${cyc}z.lgycld.tm00.bufr_d" ]; then
  cpreq ${COMINobsproc}/${RUN}.t${cyc}z.lgycld.tm00.bufr_d NASA_LaRC_cloud.bufr
else
  echo "WARNING: ${COMINobsproc}/${RUN}.t${cyc}z.lgycld.tm00.bufr_d is not available ..."
fi

# Build the namelist on-the-fly
cat << EOF > namelist_nasalarc
&SETUP
analysis_time = ${CDATE},
bufrfile='NASALaRCCloudInGSI.bufr',
npts_rad=3,
ioption = 2,
/
EOF

# Run obs processor
export pgm="${NET}_process_cloud"
. prep_step

startmsg

mpiexec $EXECrtma3d/${pgm} >> ${pgmout} 2>errfile
export err=$?; err_chk

cpreq NASA_LaRC_cloud.bufr ${COMOUT}/${RUN}.t${cyc}z.lgycld.tm00.bufr_d

targetfile="NASALaRCCloudInGSI.bufr"
if [ -f ${DATA}/${targetfile} ] ; then
  cpreq ${DATA}/${targetfile} ${COMOUT}/${RUN}.t${cyc}z.NASALaRCCloudInGSI.bufr
else
  msg="WARNING $pgm terminated normally but ${DATA}/${targetfile} does NOT exist."
fi

postmsg "$0 of $job completed normally"
