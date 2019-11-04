#!/bin/ksh 
set -x
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
export CORE=RAPR #now only "RAPR" works -20191101

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

#link fix files
ln -sf ${PARMupp}/post_avblflds.xml post_avblflds.xml
ln -sf ${PARMupp}/params_grib2_tbl_new params_grib2_tbl_new
ln -sf ${PARMupp}/postcntrl.xml postcntrl.xml
ln -sf ${PARMupp}/postxconfig-NT.txt postxconfig-NT.txt  ##postcntrl_subh.xml postxconfig_subh-NT.txt??
ln -sf ${PARMupp}/gtg.config.raphrrr gtg.config

ln -sf ${FIXupp}/ETAMPNEW_DATA eta_micro_lookup.dat

#link CRTM coefficients
for dsis in "imgr_g11" "imgr_g12" "imgr_g13" "imgr_g15" "imgr_mt1r" "imgr_mt2" \
     "amsre_aqua" "tmi_trmm" "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16"  \
     "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" "seviri_m10" "ssmi_f10"   \
     "v.seviri_m10" "imgr_insat3d" "ssmi_f11"; do
    ln -sf "${FIXcrtm}/${dsis}.SpcCoeff.bin" .
    ln -sf "${FIXcrtm}/${dsis}.TauCoeff.bin" .
done
ln -s ${FIXcrtm}/CloudCoeff.bin .
ln -s ${FIXcrtm}/AerosolCoeff.bin .
ln -s ${FIXcrtm}/FASTEM6.MWwater.EmisCoeff.bin .
ln -s ${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin .
ln -s ${FIXcrtm}/NPOESS.IRice.EmisCoeff.bin .
ln -s ${FIXcrtm}/NPOESS.IRland.EmisCoeff.bin .
ln -s ${FIXcrtm}/NPOESS.IRsnow.EmisCoeff.bin .

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

if [ "${envir}" == "esrl" ]; then #Jet
  CP_LN="${LN} -sf"
else
  CP_LN=${CP}
fi
${CP_LN} ${EXECrtma3d}/${exefile_name_post} ${pgm}
${MPIRUN} ${pgm} <itag > ${pgmout} 2>errfile
export err=$?; err_chk

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}  ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new  ${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}  ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new  ${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2

${MV}  ${workdir}/WRFTWO.GrbF${FCST_TIME}  ${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2
${MV} ${workdir}/WRFMSL.GrbF${FCST_TIME}  ${workdir}/wrfmsl_${POST_NAME}_${FCST_TIME}.grib2

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

if [ "${envir}" == "esrl" ]; then #Jet expr runs
  # Move the grib2 files one level up to postprd/
  ${MV} ${workdir}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${MV} ${workdir}/wrfmsl_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}
  ${RM} -rf ${workdir}

  # Create softlinks for transfer
  basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
  ln -s ${DATA}/wrfprs_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfprs_${basetime}${FCST_TIME}00
  ln -s ${DATA}/wrftwo_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrftwo_${basetime}${FCST_TIME}00
  ln -s ${DATA}/wrfnat_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfnat_${basetime}${FCST_TIME}00
  #ln -s ${DATA}/wrfmsl_${POST_NAME}_${FCST_TIME}.grib2 ${DATA}/wrfmsl_${basetime}${FCST_TIME}00

else #wcoss
  # transfer the output grib2 files to $COMOUTpost_rtma3d
  ${CP} ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2
  ${CP} ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2
  ${CP} ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2 ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2

  ${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrfprs_hrconus_${FCST_TIME}.grib2
  ${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrfnat_hrconus_${FCST_TIME}.grib2
  ${LN} -sf ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2 ${COMIN}/${PROD_HEAD}.wrftwo_hrconus_${FCST_TIME}.grib2
fi

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
