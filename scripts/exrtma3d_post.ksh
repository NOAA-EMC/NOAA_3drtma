#!/bin/ksh 
set -x

if [ "${envir}" == "esrl" ] ; then 

check_if_defined() { #usage: check_if_defined "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ -z "${path}" ]; then
      ${ECHO} "ERROR: \$${str} is not defined"; exit 1
    fi
  done
}
# Make sure we are using GMT time zone for time computations
export TZ="GMT"
#currently, UPP needs its own verion of CRTM
  #the post task in the xml file will define $FIXcrtm
export CORE=RAPRRTMA #submodelname='RTMA' to update 10m wind

# Check to make sure the executable exists
if [ ! -s ${EXECrtma3d}/${exefile_name_post} ]; then
  ${ECHO} "ERROR: ${EXECrtma3d}/${exefile_name_post} does not exist"
  exit 1
fi

check_if_defined "POST_NAME" "FCST_TIME" "WRFOUT_DIR" "PDY" "cyc" "subcyc"
# Check to make sure that WRFOUT_DIR exists
if [ ! -s "${WRFOUT_DIR}/wrf_inout" } ]; then
  ${ECHO} "ERROR: $WRFOUT_DIR/wrf_inout, does not exist."
  exit 1
fi

if [[ "${subcyc}" == "-1" ]]; then #it's hourly run
  tz_str=t${cyc}z
  SUBH_TIME=00
else
  tz_str=t${cyc}${subcyc}z
  SUBH_TIME=${subcyc}
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# Set up temporary work directory and cd into it
workdir=${DATA}/${tz_str}
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

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`

${CAT} > itag <<EOF
${WRFOUT_DIR}/wrf_inout
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

  CP_LN="${LN} -sf"
#link/copy parameter files
${CP_LN} ${PARMupp}/post_avblflds.xml post_avblflds.xml
${CP_LN} ${PARMupp}/params_grib2_tbl_new params_grib2_tbl_new
${CP_LN} ${PARMupp}/postcntrl.xml postcntrl.xml
${CP_LN} ${PARMupp}/postxconfig-NT.txt postxconfig-NT.txt  ##postcntrl_subh.xml postxconfig_subh-NT.txt??
${CP_LN} ${PARMupp}/gtg.config.raphrrr gtg.config

${CP_LN} ${FIXupp}/ETAMPNEW_DATA eta_micro_lookup.dat

#link CRTM coefficients
for what in "ahi_himawari8" "abi_gr" "imgr_g11" "imgr_g12" "imgr_g13" "imgr_g15" "imgr_mt1r" "imgr_mt2" \
     "amsre_aqua" "tmi_trmm" "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16"  \
     "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" "seviri_m10" "ssmi_f10"   \
     "v.seviri_m10" "imgr_insat3d" "ssmi_f11"; do
    ln -s "${FIXcrtm}/${what}.TauCoeff.bin" .
    ln -s "${FIXcrtm}/${what}.SpcCoeff.bin" .
done

ln -s ${FIXcrtm}/CloudCoeff.bin .
ln -s ${FIXcrtm}/AerosolCoeff.bin .
for what in  ${FIXcrtm}/*Emis* ; do
    ln -s ${what} .
done

#---- Run unipost
export pgm="rtma3d_post"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin post-processing"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

${CP_LN} ${EXECrtma3d}/${exefile_name_post} ${pgm}
${MPIRUN} ${pgm} <itag > ${pgmout} 2>errfile
export err=$?; err_chk

  wrfprsfile="wrfprs_${POST_NAME}_${FCST_TIME}.grib2"
  wrfnatfile="wrfnat_${POST_NAME}_${FCST_TIME}.grib2"
  wrftwofile="wrftwo_${POST_NAME}_${FCST_TIME}.grib2"
  wrfmslfile="wrfmsl_${POST_NAME}_${FCST_TIME}.grib2"
  wrfprsfile="wrfsubhprs.grib2"
  wrfnatfile="wrfsubhnat.grib2"
  wrftwofile="wrfsubhspl.grib2"
  wrfmslfile="wrfsubhmsl.grib2"
# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}  ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/${wrfprsfile}

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}  ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/${wrfnatfile}

${MV}  ${workdir}/WRFTWO.GrbF${FCST_TIME}     ${workdir}/${wrftwofile}
${MV}  ${workdir}/WRFMSL.GrbF${FCST_TIME}     ${workdir}/${wrfmslfile}

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/${wrfprsfile}" ]; then
  ${ECHO} "unipost crashed! ${wrfprsfile} is missing"
  exit 1
fi
if [ ! -s "${workdir}/${wrftwofile}" ]; then
  ${ECHO} "unipost crashed! ${wrftwofile} is missing"
  exit 1
fi
if [ ! -s "${workdir}/${wrfnatfile}" ]; then
  ${ECHO} "unipost crashed! ${wrfnatfile} is missing"
  exit 1
fi

  # Move the grib2 files one level up to postprd/
  ${MV} ${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrfmsl_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${RM} -rf ${workdir}/WRFPRS.GrbF${FCST_TIME}
  ${RM} -rf ${workdir}/WRFNAT.GrbF${FCST_TIME}

  # Create softlinks for transfer
  basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
  ln -s ${DATA}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfprs_${basetime}${FCST_TIME}00
  ln -s ${DATA}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrftwo_${basetime}${FCST_TIME}00
  ln -s ${DATA}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfnat_${basetime}${FCST_TIME}00
  #ln -s ${DATA}/wrfmsl_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfmsl_${basetime}${FCST_TIME}00


msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

fi

if [ "${envir}" == "lsf" ] ; then

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
export WGRIB2=/gpfs/dell1/nco/ops/nwprod/grib_util.v1.1.0/exec/wgrib2
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

export DATAHOME=$DATA
export CORE="RAPRRTMA"

export DATAWRFHOME=${COMOUTgsi_rtma3d:-"$COMIN"}
export DATAWRFFILE=${ANLrtma3d_FNAME:-"${RUN}.t${cyc}${subcyc}z.anl.wrf_inout.nc"}

##########################################################################

# Check to make sure the post executable exists
if [ ! -x ${EXECrtma3d}/${exefile_name_post} ]; then
  ${ECHO} "ERROR: ${EXECrtma3d}/${exefile_name_post} does not exist, or is not executable"
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
export MP_SHARED_MEMORY=yes
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`

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
${RM} -f WRF???.GrbF??


  CP_LN="${LN} -sf"
#link/copy parameter files
${CP_LN} ${PARMupp}/post_avblflds.xml post_avblflds.xml
${CP_LN} ${PARMupp}/params_grib2_tbl_new params_grib2_tbl_new
${CP_LN} ${PARMupp}/postcntrl.xml postcntrl.xml
${CP_LN} ${PARMupp}/postxconfig-NT.txt postxconfig-NT.txt  ##postcntrl_subh.xml postxconfig_subh-NT.txt??
${CP_LN} ${PARMupp}/gtg.config.raphrrr gtg.config

${CP_LN} ${PARMupp}/ETAMPNEW_DATA eta_micro_lookup.dat

#link CRTM coefficients
for what in "ahi_himawari8" "abi_gr" "imgr_g11" "imgr_g12" "imgr_g13" "imgr_g15" "imgr_mt1r" "imgr_mt2" \
     "amsre_aqua" "tmi_trmm" "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16"  \
     "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" "seviri_m10" "ssmi_f10"   \
     "v.seviri_m10" "imgr_insat3d" "ssmi_f11"; do
    ln -s "${FIXcrtm}/${what}.TauCoeff.bin" .
    ln -s "${FIXcrtm}/${what}.SpcCoeff.bin" .
done

ln -s ${FIXcrtm}/CloudCoeff.bin .
ln -s ${FIXcrtm}/AerosolCoeff.bin .
for what in  ${FIXcrtm}/*Emis* ; do
    ln -s ${what} .
done


#=============================================================================#
#
# Run unipost
#
pgm=${RUN}_post
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin Uni-Post step for 3DRTMA GSI Analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"


#copy executable to running directory
${CP} ${EXECrtma3d}/${exefile_name_post} ./rtma3d_wrfpost

 runline="${MPIRUN}         ./rtma3d_wrfpost"
$runline < itag > ${pgmout} 2>errfile
export err=$? ; err_chk

if [ ${err} -ne 0 ]; then
  ${ECHO} "rtma3d_wrfpost crashed!  Exit status=${err}"
  exit ${err}
fi

# Linking GrbF{HH} to GrbF00 (esp. for firstguess which is from WRF forecast)
GrbFiles=`ls WRF???.GrbF??`
for i in ${GrbFiles}
do
  i_fname=`echo "$i" | cut -d '.' -f 1`
  i_extname=`echo "$i" | cut -d '.' -f 2`
  if [[ "${i_extname}" != "GrbF${FCST_TIME}" ]]
  then
    ln -sf $i    ./${i_fname}.GrbF${FCST_TIME}
  fi
done

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfsubhprs.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfsubhnat.grib2

#${CP}  ${workdir}/WRFTWO.GrbF${FCST_TIME}     ${workdir}/wrfsubhspl.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfsubhprs.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhprs.grib2 is missing"
  exit 1
fi
#if [ ! -s "${workdir}/wrfsubhspl.grib2" ]; then
#  ${ECHO} "unipost crashed! wrfsubhspl.grib2 is missing"
#  exit 1
#fi
if [ ! -s "${workdir}/wrfsubhnat.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhnat.grib2 is missing"
  exit 1
fi

# transfer the output grib2 files to $COMOUTpost_rtma3d

${WGRIB2} ${workdir}/wrfsubhprs.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhprs.grib2
#${WGRIB2} ${workdir}/wrfsubhspl.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhspl.grib2
${WGRIB2} ${workdir}/wrfsubhnat.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhnat.grib2


# softlinks with Julian date
#basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
#${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_subhrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_
#HEAD}.wrfprs_${basetime}${FCST_TIME}00
#${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_subhrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_
#HEAD}.wrftwo_${basetime}${FCST_TIME}00
#${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_subhrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_
#HEAD}.wrfnat_${basetime}${FCST_TIME}00

#================================================================================#
# The following data transferr is used in GSD old unipost script
#  (Should be removed for NCO usage)
# Move the output files to postprd under $COMOUTpost_rtma3d
# ${MV} ${workdir}/wrfprs_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_subhrconus_${FCST_TIME}.grib2
# ${MV} ${workdir}/wrftwo_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_subhrconus_${FCST_TIME}.grib2
# ${MV} ${workdir}/wrfnat_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_subhrconus_${FCST_TIME}.grib2

# ${RM} -rf ${workdir}
  ${RM} -f  ${workdir}/wrfsubh???.grib2
  ${RM} -f  ${workdir}/WRF???.GrbF??

# Create softlinks for transfer
# basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
# ln -s ${DATAHOME}/wrfprs_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
# ln -s ${DATAHOME}/wrftwo_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
# ln -s ${DATAHOME}/wrfnat_subhrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00
#================================================================================#

${ECHO} "unipost completed at `${DATE}`"

fi



exit 0
