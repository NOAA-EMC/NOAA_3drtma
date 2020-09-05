#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hrrr_smartinit.sh
# Script description:  To generate the smartinit products for the Hi-Res Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2014-04-15
#
# Script history log:
# 2014-04-15  G Manikin  -- new script
# 2017-04-11  B Blake    -- grib 2 version of script

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile

#set -x

# Print run parameters
${ECHO}
${ECHO} "smartinit.sh started at `${DATE}`"
${ECHO}
${ECHO} "DATAHOME = ${DATAHOME}"
${ECHO} "     EXE_ROOT = ${EXE_ROOT}"

# Set up some constants
if [ "${MODEL}" == "HRRR" ]; then
  export POST=${EXE_ROOT}/hrrr_smartinit_conus
  export CORE=RAPR
fi

# Check to make sure the EXE_ROOT var was specified
if [ ! -d ${EXE_ROOT} ]; then
   ${ECHO} "ERROR: EXE_ROOT, '${EXE_ROOT}', does not exist"
   exit 1
fi

# Check to make sure the post executable exists
if [ ! -x ${POST} ]; then
  ${ECHO} "ERROR: ${POST} does not exist, or is not executable"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAROOT} ]; then
  ${ECHO} "ERROR: DATAROOT, \$DATAROOT , is not defined"
  exit 1
fi

# If START_TIME is not defined, use the current time
if [ ! "${START_TIME}" ]; then
  START_TIME=`${DATE} +"%Y%m%d %H"`
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}/smartinit_bl
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

cyc=${HH}
fhr=${FCST_TIME}

GRB2INDEX=${EXE_ROOT}/grb2index
#EXEChrrr=${EXE_ROOT}/hrrr_smartinit_conus
EXEChrrr=${EXE_ROOT}/hrrr_smartinit_conus_ge_0c1c0c4
FIX=${STATIC_DIR}
COMOUT=../
WGRIB2=${EXE_ROOT}/wgrib2

typeset -Z2 fhr

export tmmark=tm00

cp ${DATAPOSTHOME}/wrfnat_hrconus_${fhr}.grib2 WRFNAT${fhr}.tm00
${GRB2INDEX} WRFNAT${fhr}.tm00 WRFNAT${fhr}i.tm00

cp ${FIX}/hrrr_terrain_consensus.gb2 TOPONDFDCS
cp ${FIX}/hrrr_smartmask_consensus.gb2 LANDNDFDCS

${GRB2INDEX} TOPONDFDCS TOPONDFDCSI
${GRB2INDEX} LANDNDFDCS LANDNDFDCSI

# strip out records not needed for awips
cp ${FIX}/hrrr_smartnatparams .
${WGRIB2} WRFNAT${fhr}.tm00 | grep -F -f hrrr_smartnatparams | ${WGRIB2} -i -grib hrrr_natgrd.tm00 WRFNAT${fhr}.tm00

${GRB2INDEX} hrrr_natgrd.tm00 grib2file_index 

export wgrib2def="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"
${WGRIB2} hrrr_natgrd.tm00  -set_radius 1:6370000 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} hrrr.NDFDCSf${fhr}.grib2

mv hrrr.NDFDCSf${fhr}.grib2 hrrr.NDFDCSf${fhr}
${GRB2INDEX} hrrr.NDFDCSf${fhr} hrrr.NDFDCSf${fhr}I

cp ${FIX}/DATE_hrrr_smart DATE

export pgm=hrrr_smartinit_conus
#. prep_step

ln -sf hrrr.NDFDCSf${fhr}     fort.11
ln -sf hrrr.NDFDCSf${fhr}I    fort.12
ln -sf TOPONDFDCS             fort.46
ln -sf TOPONDFDCSI            fort.47
ln -sf LANDNDFDCS             fort.48
ln -sf LANDNDFDCSI            fort.49
ln -sf HRRRCS${fhr}.tm00      fort.71

rm -rf smart.ksh
varEOF=EOF
cat > smart.ksh <<EOF
#!/bin/ksh -l
${EXEChrrr} <<EOF >> smartinitcs.out${fhr}
$fhr
$cyc
$varEOF
EOF
chmod 755 smart.ksh
./smart.ksh
export err=$?; err_chk

mv HRRRCS${fhr}.tm00 $COMOUT/hrrr.t${cyc}z.smarthrrrconusf${fhr}_bl.grib2
rm -fr ${workdir}
exit 0
