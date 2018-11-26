#!/bin/sh -l

set -x

export OMP_NUM_THREADS=1

# Make sure we are using GMT time zone for time computations
export TZ="GMT"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
# ANLS_TIME=${PDY}' '${cyc}
ANLS_TIME=${ANLS_TIME:-"${PDY} ${cyc}"}            # YYYYMMDD HH
echo $PDY $cyc $subcyc
# cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"                 # <-- forecast time (hour) to provide fgs for rtma

# For RTMA there is no forecast, so START_TIME is ANLS_TIME
START_TIME=$ANLS_TIME

# Make sure START_TIME is defined and in the correct format (YYYYMMDD HH)
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME} ${subcyc} minutes"`
fi

ANLS_CYC_TIME=`${DATE} --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`
FCST_INI_TIME=`${DATE} --date="${START_TIME} -${FCST_TIME} hour " +"%Y%m%d%H%M"`

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
mm=`${DATE} +"%M" -d "${START_TIME}"`
JJJ=`${DATE} +"%j" -d "${START_TIME}"`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}

HH_fcstinit=`${ECHO} ${FCST_INI_TIME} | cut -c9-10 `
#------------------------------------------------------------------#

CRTMFIX=${CRTMFIX:-"${FIXrtma3d}/CRTM-fix"}
FIXCRTM=$CRTMFIX
CRTM=$CRTMFIX


export pgm=${pgm:-"${EXECrtma3d}/rtma3d_post"}
export POST=${pgm}

export DATAHOME=$DATA
export MODEL="RAP"

export DATAWRFHOME=${COMOUTgsi_rtma3d:-"$COMIN"}
export DATAWRFFILE=${ANLrtma3d_FNAME:-"${RUN}.t${cyc}z.anl.wrf_inout.nc"}

export PARMupp=${UPPPARM:-"${PARMrtma3d}/UPP"}
export PARMwrf=${WRFPARM:-"${PARMrtma3d}/WRF"}

##########################################################################

# Check to make sure the post executable exists
if [ ! -x ${POST} ]; then
  ${ECHO} "ERROR: ${POST} does not exist, or is not executable"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Check to make sure that the DATAWRFHOME is defined
if [ ! ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: DATAWRFHOME, \$DATAWRFHOME, is not defined."
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! -d ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME, does not exist."
  exit 1
fi

if [ ! -f ${DATAWRFHOME}/${DATAWRFFILE} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME/${DATAWRFFILE}, does not exist."
  exit 1
fi



# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

#
# Set up some constants for UPP namlist itag
#
export XLFRTEOPTS="unit_vars=yes"

if [ "${MODEL}" == "RAP" ]; then
  export CORE=RAPR
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  export CORE=NMM
fi

# export tmmark=tm00
# export tmmark=GrbF00
# export tmmark=GrbF${FCST_TIME}
export RSTFNL=${workdir}/

export CORE=RAPR
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2

# export VALIDTIMEUNITS=00

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

${CAT} > itag <<EOF
${DATAWRFHOME}/${DATAWRFFILE}
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

${RM} -f fort.*
${RM} -f post_avblflds.xml params_grib2_tbl_new postcntrl.xml postxconfig-NT.txt eta_micro_lookup.dat

# set up the namelist/control/config input files
case ${UPP_CNTRL} in
  0)
  ln -s ${PARMupp}/hrrr_post_avblflds.xml      post_avblflds.xml
  ln -s ${PARMupp}/hrrr_params_grib2_tbl_new   params_grib2_tbl_new
  ln -s ${PARMupp}/hrrr_postcntrl.xml          postcntrl.xml
  ln -s ${PARMupp}/postxconfig-NT-HRRR.txt     postxconfig-NT.txt
  if [ "${MODEL}" == "RAP" ]; then
    ln -s ${PARMupp}/rap_micro_lookup.dat      eta_micro_lookup.dat
  elif [ "${MODEL}" == "WRF-RR NMM" ]; then
    ln -s ${PARMupp}/nam_micro_lookup.dat      eta_micro_lookup.dat
  fi
  ;;
  1)
  ln -s ${PARMupp}/post_avblflds.xml           post_avblflds.xml
  ln -s ${PARMupp}/params_grib2_tbl_new        params_grib2_tbl_new
  ln -s ${PARMupp}/postcntrl.xml               postcntrl.xml
  ln -s ${PARMupp}/postxconfig-NT.txt          postxconfig-NT.txt
  if [ "${MODEL}" == "RAP" ]; then
    ln -s ${PARMwrf}/ETAMPNEW_DATA             eta_micro_lookup.dat
  elif [ "${MODEL}" == "WRF-RR NMM" ]; then
    ln -s ${PARMwrf}/ETAMPNEW_DATA             eta_micro_lookup.dat
  fi
  ;;
  2)
  ln -s ${PARMupp}/hrrr_post_avblflds.xml      post_avblflds.xml
  ln -s ${PARMupp}/hrrr_params_grib2_tbl_new   params_grib2_tbl_new
  ln -s ${PARMupp}/hrrr_postcntrl.xml          postcntrl.xml
  ln -s ${PARMupp}/hrrr_postxconfig-NT.txt     postxconfig-NT.txt
  if [ "${MODEL}" == "RAP" ]; then
    ln -s ${PARMupp}/rap_micro_lookup.dat      eta_micro_lookup.dat
  elif [ "${MODEL}" == "WRF-RR NMM" ]; then
    ln -s ${PARMupp}/nam_micro_lookup.dat      eta_micro_lookup.dat
  fi
  ;;
  *)
  echo "Unknown dataset option for UPP control/config. Abort!"
  exit 1
  ;;
esac

################################################################################
# ln -s ${CRTM}/EmisCoeff/Big_Endian/Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin
################################################################################

for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" ; do
    ln -s "$FIXCRTM/$what.TauCoeff.bin" .
    ln -s "$FIXCRTM/$what.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "$FIXCRTM/${what}Coeff.bin" .
done

for what in  $FIXCRTM/*Emis* ; do
    ln -s $what .
done

#=============================================================================#
#
# Run unipost
#
if [ -f errfile ] ; then
    rm -f errfile
fi

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin Uni-Post step for 3DRTMA GSI Analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

#runline="${MPIRUN} -np $np ${pgm}< itag"
 runline="${MPIRUN} -np $np ${pgm}"
$runline < itag > ${pgmout} 2>errfile
export err=$? ; err_chk

if [ ${err} -ne 0 ]; then
  ${ECHO} "${pgm} crashed!  Exit status=${err}"
  exit ${err}
fi

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2

${CP}  ${workdir}/WRFTWO.GrbF${FCST_TIME}     ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfprs_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrftwo_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfnat_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi

# transfer the output grib2 files to $COMOUTpost_rtma3d
${CP} ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2
${CP} ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2
${CP} ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2

# softlinks with Julian date
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_${basetime}${FCST_TIME}00
${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_${basetime}${FCST_TIME}00
${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_${basetime}${FCST_TIME}00

${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2
${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2
${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2

#================================================================================#
# The following data transferr is used in GSD old unipost script
#  (Should be removed for NCO usage)
# Move the output files to postprd under $COMOUTpost_rtma3d
${MV} ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2
${MV} ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2
${MV} ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2

# ${RM} -rf ${workdir}

# Create softlinks for transfer
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00
#================================================================================#

${ECHO} "unipost completed at `${DATE}`"

exit 0
