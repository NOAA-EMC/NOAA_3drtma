#!/bin/ksh 

set -x

postmsg "$0 of $job has begun"

cd ${DATA}

# BUFR Table
cpreq -p ${PARMrtma3d}/${RUN}ak_prepobs_prep.bufrtable prepobs_prep.bufrtable
# WPS GEO_GRID Data
cpreq ${FIXrtma3d}/${RUN}ak_geo_em.d01.nc geo_em.d01.nc

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
  boxlat0=60,61,63,66,68,
  boxhalfy=4, 6, 8, 10, 12,
  boxhalfx=4, 6, 8, 10, 12,
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
  cpreq ${DATA}/${targetfile} ${COMOUT}/${RUN}ak.t${cyc}z.NASALaRCCloudInGSI.bufr
else
  msg="WARNING $pgm terminated normally but ${DATA}/${targetfile} does NOT exist."
fi

postmsg "$0 of $job completed normally"
