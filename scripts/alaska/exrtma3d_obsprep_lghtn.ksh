#!/bin/ksh
############################################################################

set -x

postmsg "$0 of $job has begun"

minutetime="00"

cd ${DATA}

# BUFR Table including the description for HREF
cpreq -p ${PARMrtma3d}/${RUN}ak_prepobs_prep.bufrtable prepobs_prep.bufrtable
# WPS GEO_GRID Data
cpreq ${FIXrtma3d}/${RUN}ak_geo_em.d01.nc geo_em.d01.nc

echo " processing NCEP BUFR Lightning Data"

# find lightning bufr file

if [ -s "${COMINobsproc}/${NET}.t${cyc}z.lghtng.tm00.bufr_d" ]; then
  cpreq -p ${COMINobsproc}/${NET}.t${cyc}z.lghtng.tm00.bufr_d lghtngbufr
else
  echo "WARNING: ${COMINobsproc}/${NET}.t${cyc}z.lghtng.tm00.bufr_d is not available ..."
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

cpreq -p lghtngbufr ${COMOUT}/${RUN}.t${cyc}z.lghtng.tm00.${dom}.bufr_d

chgrp rstprod LightningInGSI.bufr
lghtng_bufr="LightningInGSI.bufr"
if [ -f ${DATA}/${lghtng_bufr} ] ; then
  cpreq -p ${DATA}/${lghtng_bufr} ${COMOUT}/${RUN}.t${cyc}z.LightningInGSI_bufr.${dom}.bufr
else
  msg="WARNING $pgm terminated normally but ${DATA}/${lghtng_bufr} does NOT exist."
fi

postmsg "$0 of $job completed normally"
