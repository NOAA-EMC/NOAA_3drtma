#!/bin/ksh --login
##########################################################################
#
#Script Name: post.ksh
# 
#     Author: Christopher Harrop
#             Forecast Systems Laboratory
#             325 Broadway R/FST
#             Boulder, CO. 80305
#
#   Released: 10/30/2003
#    Version: 1.0
#    Changes: None
#
# Purpose: This script post processes wrf output.  It is based on scripts
#          whose authors are unknown.
#
#               EXE_ROOT = The full path of the post executables
#          DATAHOME = Top level directory of wrf output and
#                          configuration data.
#             START_TIME = The cycle time to use for the initial time. 
#                          If not set, the system clock is used.
#              FCST_TIME = The two-digit forecast that is to be posted
#              POST_NAME = GRIB-2 file naming convention, e.g., "hrconus"
# 
# A short and simple "control" script could be written to call this script
# or to submit this  script to a batch queueing  system.  Such a "control" 
# script  could  also  be  used to  set the above environment variables as 
# appropriate  for  a  particular experiment.  Batch  queueing options can
# be  specified on the command  line or  as directives at  the top of this
# script.  A set of default batch queueing directives is provided.
#
##########################################################################

np=`cat $PBS_NODEFILE | wc -l`

# Load modules (in jobs/launch.sh)

# Make sure we are using GMT time zone for time computations
export TZ="GMT"

# Set up paths to shell commands (in xml file)

export CRTM="/mnt/lfs1/projects/rtwbl/mhu/code/CRTM_v2.2.6"

# Print run parameters
${ECHO}
${ECHO} "unipost.ksh started at `${DATE}`"
${ECHO}
${ECHO} "DATAHOME = ${DATAHOME}"
${ECHO} "     EXE_ROOT = ${EXE_ROOT}"

# Set up some constants
if [ "${MODEL}" == "RAP" ]; then
  export POST=${EXE_ROOT}/ncep_post.exe
  export CORE=RAPR
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  export POST=${EXE_ROOT}/ncep_post.exe
  export CORE=NMM
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
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Check to make sure that the POST_NAME exists
if [ ! ${POST_NAME} ]; then
  ${ECHO} "ERROR: POST_NAME, \$POST_NAME, is not defined"
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
workdir=${DATAHOME}/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export MP_SHARED_MEMORY=yes
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2


timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

${CAT} > itag <<EOF
${DATAWRFHOME}/wrf_inout
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

${RM} -f fort.*
ln -s ${STATIC_DIR}/post_avblflds.xml post_avblflds.xml
ln -s ${STATIC_DIR}/params_grib2_tbl_new params_grib2_tbl_new
ln -s ${STATIC_DIR}/postcntrl.xml postcntrl.xml
ln -s ${STATIC_DIR}/postxconfig-NT.txt postxconfig-NT.txt
if [ "${MODEL}" == "RAP" ]; then
  ln -s ${STATICWRF_DIR}/run/ETAMPNEW_DATA eta_micro_lookup.dat
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  ln -s ${STATICWRF_DIR}/run/ETAMPNEW_DATA eta_micro_lookup.dat
fi

ln -s ${CRTM}/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
ln -s ${CRTM}/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
ln -s ${CRTM}/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
ln -s ${CRTM}/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
ln -s ${CRTM}/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
ln -s ${CRTM}/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
ln -s ${CRTM}/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
ln -s ${CRTM}/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
ln -s ${CRTM}/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
ln -s ${CRTM}/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
ln -s ${CRTM}/imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin

ln -s ${CRTM}/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
ln -s ${CRTM}/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
ln -s ${CRTM}/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
ln -s ${CRTM}/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
ln -s ${CRTM}/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
ln -s ${CRTM}/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
ln -s ${CRTM}/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
ln -s ${CRTM}/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
ln -s ${CRTM}/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
ln -s ${CRTM}/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
ln -s ${CRTM}/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
ln -s ${CRTM}/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
ln -s ${CRTM}/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
ln -s ${CRTM}/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
ln -s ${CRTM}/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
ln -s ${CRTM}/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
ln -s ${CRTM}/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
ln -s ${CRTM}/v.seviri_m10.TauCoeff.bin v.seviri_m10.TauCoeff.bin
ln -s ${CRTM}/imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin

ln -s ${CRTM}/CloudCoeff.bin CloudCoeff.bin
ln -s ${CRTM}/AerosolCoeff.bin AerosolCoeff.bin
#ln -s ${CRTM}/EmisCoeff.bin EmisCoeff.bin

ln -s ${CRTM}/ssmi_f10.SpcCoeff.bin ssmi_f10.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f10.TauCoeff.bin ssmi_f10.TauCoeff.bin
ln -s ${CRTM}/ssmi_f11.SpcCoeff.bin ssmi_f11.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f11.TauCoeff.bin ssmi_f11.TauCoeff.bin
ln -s ${CRTM}/FASTEM6.MWwater.EmisCoeff.bin FASTEM6.MWwater.EmisCoeff.bin
ln -s ${CRTM}/Nalli.IRwater.EmisCoeff.bin Nalli.IRwater.EmisCoeff.bin
ln -s ${CRTM}/NPOESS.IRice.EmisCoeff.bin NPOESS.IRice.EmisCoeff.bin
ln -s ${CRTM}/NPOESS.IRland.EmisCoeff.bin NPOESS.IRland.EmisCoeff.bin
ln -s ${CRTM}/NPOESS.IRsnow.EmisCoeff.bin NPOESS.IRsnow.EmisCoeff.bin

# Run unipost
# ${MPIRUN} -np $np ${POST}< itag
  ${MPIRUN}         ${POST}< itag
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "${POST} crashed!  Exit status=${error}"
  exit ${error}
fi

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME} ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV} ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
${CAT} WRFNAT.GrbF${FCST_TIME} WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV} WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2

${MV} ${workdir}/WRFTWO.GrbF${FCST_TIME} ${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfprs_${POST_NAME}_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrftwo_${POST_NAME}_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfnat_${POST_NAME}_${FCST_TIME}.grib2 is missing"
  exit 1
fi

# Move the output files to postprd
${MV} ${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}
${MV} ${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}
${MV} ${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}
${RM} -rf ${workdir}

# Create softlinks for transfer
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00

${ECHO} "unipost.ksh completed at `${DATE}`"

exit 0
