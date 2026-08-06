#!/bin/ksh
############################################################################

set -x

postmsg "$0 of $job has begun"

minutetime="00"

cd ${DATA}

# BUFR Table including the description for HREF
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
cpreq -p ${PARMrtma3d}/${RUN}ak_prepobs_prep.bufrtable prepobs_prep.bufrtable
# WPS GEO_GRID Data
cpreq ${FIXrtma3d}/${RUN}ak_geo_em.d01.nc geo_em.d01.nc

echo " processing NCEP BUFR Lightning Data"

# find lightning bufr file

if [ -s "${COMINobsproc}/${RUN}.t${cyc}z.lghtng.tm00.bufr_d" ]; then
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.lghtng.tm00.bufr_d lghtngbufr
else
  echo "WARNING: ${COMINobsproc}/${RUN}.t${cyc}z.lghtng.tm00.bufr_d is not available ..."
fi

# Build the namelist on-the-fly
cat << EOF > lightning_bufr.namelist
&SETUP
analysis_time = ${CDATE},
minute=${minutetime},
trange_start=-15.0,
trange_end=0.0,
/
EOF

# Run process lightning
export pgm="${NET}_process_lightning"
. prep_step

startmsg

# Run Processing lightning
mpiexec $EXECrtma3d/${pgm} >> ${pgmout} 2>errfile
export err=$?; err_chk

cpreq -p lghtngbufr ${COMOUT}/${RUN}.t${cyc}z.lghtng.tm00.bufr_d

chgrp rstprod LightningInGSI.bufr
lghtng_bufr="LightningInGSI.bufr"
if [ -f ${DATA}/${lghtng_bufr} ] ; then
  cpreq -p ${DATA}/${lghtng_bufr} ${COMOUT}/${RUN}ak.t${cyc}z.LightningInGSI_bufr.bufr
else
  msg="WARNING $pgm terminated normally but ${DATA}/${lghtng_bufr} does NOT exist."
fi

postmsg "$0 of $job completed normally"
