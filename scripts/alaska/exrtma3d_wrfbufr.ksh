#!/bin/ksh 
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exrtma3d_wrfbufr.ksh
# Script description:  Run rtma3d wrfbufr jobs
#
# Author:      G Manikin   EMC         Date: 2014-08-01
#
# Abstract: This script runs the rtma3d wrfbufr jobs
#
# Script history log:
# 2014-08-01  G Manikin - new script 
# 2018-01-24  B Blake / G Manikin - HRRRv3
# 2026-05-24  A Gibbs / E Colon - RTMA3Dv1
#

set -xa

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export CORE=RAPR
export OUTTYP=binarympiio

cpreq ${FIXrtma3d}/${RUN}ak_hrrrak_profdat .

OUTTYP=netcdf
model=RAPR
NFILE=1
INCR=01
CDATEm1=`$NDATE -1 $CDATE`
YYYY=`echo $CDATE | cut -c1-4`
MM=`echo $CDATE | cut -c5-6`
DD=`echo $CDATE | cut -c7-8`
fmin=00

oyr=`echo $CDATEm1 | cut -c1-4`
omn=`echo $CDATEm1 | cut -c5-6`
ody=`echo $CDATEm1 | cut -c7-8`
ohr=`echo $CDATEm1 | cut -c9-10`

timeform=${YYYY}"-"${MM}"-"${DD}"_"${cyc}"_00_00"
timeformold=${oyr}"-"${omn}"-"${ody}"_"${ohr}"_00_00"
START_TIME=${YYYY}'-'${MM}'-'${DD}'_'${cyc}':00:00'

cp ${COMIN}/${RUN}ak.t${cyc}z.wrf_inout.nc wrfoutd01_${timeform}
cp ${COMINm1}/${RUN}ak.t${ohr}z.wrf_inout.nc wrfoutd01_${timeformold}

OUTFIL=wrfoutd01_${timeform}
OLDOUTFIL=wrfoutd01_${timeformold}

cat > itag <<EOF
$OUTFIL
$model
$OUTTYP
$START_TIME
$NFILE
$INCR
${fmin}
$OLDOUTFIL
EOF

export pgm="${NET}_wrfbufr_${dom}"
. prep_step
startmsg

ln -sf itag              fort.11
ln -sf ${RUN}ak_hrrrak_profdat  fort.19
ln -sf profilm.c1.tm00 fort.79

runline="mpiexec -n 1 -ppn 1 $EXECrtma3d/${pgm}"
$runline
export err=$?; err_chk

mv profilm.c1.tm00 ${DATA_SHARED}/profilm.c1.f${cyc}

postmsg "$0 of $job completed normally"

date
