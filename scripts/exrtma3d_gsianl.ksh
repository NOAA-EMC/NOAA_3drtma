#!/bin/ksh --login
set -x
check_if_defined() { #usage: check_if_defined "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ -z "${path}" ]; then
      ${ECHO} "ERROR: \$${str} is not defined"; exit 1
    fi
 done
}
check_dirs_exist() { #usage: check_dirs_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -d ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
 done
}

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_gsi} ]; then
  ${ECHO} "ERROR: GSI Analysis executable '${EXECrtma3d}/${exefile_name_gsi}' does not exist!"
  exit 1
fi

# Check to make sure required directory defined and existed
check_if_defined "ENKF_FCST" "HRRRDAS_DIR" "HRRR_DIR" "OBS_DIR"
check_dirs_exist "ENKF_FCST" "HRRRDAS_DIR" "HRRR_DIR" "OBS_DIR"

if [ "${subcyc}" == "-1" ]; then #hourly run
  SUBH_TIME='00'
  tz_str=t${cyc}z
else
  SUBH_TIME=${subcyc}
  tz_str=t${cyc}${subcyc}z
fi
START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`

# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
time_1hour_ago=`${DATE} -d "${START_TIME} 1 hour ago" +%Y%m%d%H`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
time_str2=`${DATE} "+%Y-%m-%d_%H_00_00" -d "${START_TIME}"`

#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"

# Define the loghistory file depending on if this is the full or partial cycle
ifsoilnudge=.true.
if [ "${envir}" == "esrl" ]; then
  if [ "${FULLCYC}" == "0" ]; then
    loghistoryfile=${COMROOT}/loghistory/HRRR_GSI_HYB_PCYC.log
    ifsoilnudge=.true.
  elif [ "${FULLCYC}" == "2" ]; then
    loghistoryfile=${COMROOT}/loghistory/HRRR_GSI_HYB_early.log
    ifsoilnudge=.true.
  else
    loghistoryfile=${COMROOT}/loghistory/HRRR_GSI_HYB.log
    ifsoilnudge=.true.
  fi
fi

# Bring over background field (it's modified by GSI so we can't link to it)
if [ "${subcyc}" == "-1" ]; then #hourly run
   cycle_str=${YYYYMMDDHH}
else
   cycle_str=${YYYYMMDDHHMM}
fi

# Look for background field for GSI analysis
if [ "${envar}" == "esrl" ]; then
  if [ "${subcyc}" == "45" ]; then #45 subcyc uses 45min fcst from current hrrr cycle
    GSIbackground=${HRRR_DIR}/${YYYYMMDDHH}/wrfprd/wrfout_d01_${time_str}
  else #all other subcyc's use 1h fcst from previous hrrr cycle
    GSIbackground=${HRRR_DIR}/${time_1hour_ago}/wrfprd/wrfout_d01_${time_str}
  fi
else
  GSIbackground=${BKG_DIR}/${FGSrtma3d_FNAME}
fi
if [ -r ${GSIbackground} ]; then
  cpfs ${GSIbackground} ./wrf_inout
  ${ECHO} " Cycle ${cycle_str}: GSI background=${GSIbackground}"
  if [ "${envar}" == "esrl" ]; then
    ${ECHO} " Cycle ${cycle_str}: GSI background=${GSIbackground}" >> ${loghistoryfile}
  fi
else
  # No background available so abort
  ${ECHO} "${GSIbackground} does not exist!!"
  ${ECHO} "FATAL ERROR: No background file for analysis at ${time_str}!!!!"
  if [ "${envar}" == "esrl" ]; then
    ${ECHO} " Cycle ${cycle_str}: GSI failed because of no background" >> ${loghistoryfile}
  fi
  exit 1
fi

# Update SST currently set to run in the 01z cycle
update_SST='00'

# Link to the prepbufr data
if [ -r ${OBS_DIR}/prepbufr ] ; then
  ${LN} -s ${OBS_DIR}/prepbufr ./prepbufr
else
  ${ECHO} "Warning: ${OBS_DIR}/prepbufr does not exist"
fi

if [ -r "${OBS_DIR}/NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/NSSLRefInGSI.bufr ./refInGSI
elif [ -r "${OBS_DIR}/hrrr.${tz_str}.NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/hrrr.${tz_str}.NSSLRefInGSI.bufr ./refInGSI
elif [ -r "${OBS_DIR}/${RUN}.${tz_str}.NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/${RUN}.${tz_str}.NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NSSLRefInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/LightningInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/LightningInGSI.bufr ./lghtInGSI
elif [ -r "${OBS_DIR}/hrrr.{tz_str}.LightningInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/hrrr.{tz_str}.LightningInGSI.bufr ./lghtInGSI
elif [ -r "${OBS_DIR}/${RUN}.{tz_str}.LightningInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/${RUN}.{tz_str}.LightningInGSI.bufr ./lghtInGSI
elif [ -r "${OBS_DIR}/${RUN}.{tz_str}.LightningInGSI_bufr.bufr" ]; then
  ${LN} -s ${OBS_DIR}/${RUN}.{tz_str}.LightningInGSI_bufr.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: LightningInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/NASALaRCCloudInGSI.bufr ./larcInGSI
elif [ -r "${OBS_DIR}/hrrr.{tz_str}.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/hrrr.{tz_str}.NASALaRCCloudInGSI.bufr ./larcInGSI
elif [ -r "${OBS_DIR}/${RUN}.{tz_str}.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${OBS_DIR}/${RUN}.{tz_str}.NASALaRCCloudInGSI.bufr ./larcInGSI
elif [ -r "${OBS_DIR}/rtma_ru.{tz_str}.lgycld.tm00.bufr_d" ]; then
  ${LN} -s ${OBS_DIR}/rtma_ru.{tz_str}.lgycld.tm00.bufr_d ./larcInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NASALaRCCloudInGSI.bufr does not exist!"
fi

## 
## Find closest GFS EnKF forecast to analysis time
# Make a list of the latest GFS EnKF ensemble
stampcycle=`date -d "${START_TIME}" +%s`
minHourDiff=100
loops="009"
for loop in $loops; do
  for timelist in `ls ${ENKF_FCST}/*.gdas.t*z.atmf${loop}s.mem080.nemsio`; do
    availtimeyy=`basename ${timelist} | cut -c 1-2`
    availtimeyyyy=20${availtimeyy}
    availtimejjj=`basename ${timelist} | cut -c 3-5`
    availtimemm=`date -d "${availtimeyyyy}0101 +$(( 10#${availtimejjj} - 1 )) days" +%m`
    availtimedd=`date -d "${availtimeyyyy}0101 +$(( 10#${availtimejjj} - 1 )) days" +%d`
    availtimehh=`basename ${timelist} | cut -c 6-7`
    availtime=${availtimeyyyy}${availtimemm}${availtimedd}${availtimehh}
    AVAIL_TIME=`${ECHO} "${availtime}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
    AVAIL_TIME=`${DATE} -d "${AVAIL_TIME}"`

    stamp_avail=`date -d "${AVAIL_TIME} ${loop} hours" +%s`

    hourDiff=`echo "($stampcycle - $stamp_avail) / (60 * 60 )" | bc`;
    if [[ ${stampcycle} -lt ${stamp_avail} ]]; then
       hourDiff=`echo "($stamp_avail - $stampcycle) / (60 * 60 )" | bc`;
    fi

    if [[ ${hourDiff} -lt ${minHourDiff} ]]; then
       minHourDiff=${hourDiff}
       enkfcstname=${availtimeyy}${availtimejjj}${availtimehh}00.gdas.t${availtimehh}z.atmf${loop}s
    fi
  done
done
EYYYYMMDD=$(echo ${availtime} | cut -c1-8)
EHH=$(echo ${availtime} | cut -c9-10)
${LS} ${ENKF_FCST}/${enkfcstname}.mem???.nemsio > filelist03
#${LS} ${ENKF_FCST}/${enkfcstname}.mem???.nemsio > filelist.tmp
#head -n 36 filelist.tmp > filelist03

## 
## Link to pre-processed GFS EnKF forecast members
##
# for mem in `ls ${DATAROOT}/gfsenkf/enspreproc_arw_mem???`
# do
#   memname=`basename ${mem}`
#   ${LN} -s ${mem} ${memname}
# done
# ${LS} enspreproc_arw_mem??? > filelist

if [ ${HRRRDAS_BEC} -eq 1 ]; then
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will be used if available"
  #----------------------------------------------------
  # generate list of HRRRDAS members for ensemble covariances
  # Use 1-hr forecasts from the HRRRDAS cycling
  if [ ${HRRRDAS_SMALL} -eq 1 ]; then
    {LS} ${HRRRDAS_DIR}/${time_1hour_ago}/wrfprd_mem????/wrfout_small_d02_${time_str2} > filelist.hrrrdas
  else
    {LS} ${HRRRDAS_DIR}/${time_1hour_ago}/wrfprd_mem????/wrfout_d02_${time_str2} > filelist.hrrrdas
  fi
  c=1
  while [[ $c -le 36 ]]; do
   if [ $c -lt 10 ]; then
    cc="0"$c
   else
    cc=$c
   fi
   if [ ${HRRRDAS_SMALL} -eq 1 ]; then
     hrrre_file=${HRRRDAS_DIR}/${time_1hour_ago}/wrfprd_mem00${cc}/wrfout_small_d02_${time_str2}
   else
     hrrre_file=${HRRRDAS_DIR}/${time_1hour_ago}/wrfprd_mem00${cc}/wrfout_d02_${time_str2}
   fi
   {LN} -sf ${hrrre_file} wrf_en0${cc}
   ((c = c + 1))
  done
else
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will NOT be used"
  ${TOUCH} filelist.hrrrdas #so as to avoid "no such file" error message
fi

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
nummem=`more filelist03 | wc -l`
nummem=$((nummem - 3 ))
hrrrmem=`more filelist.hrrrdas | wc -l`
hrrrmem=$((hrrrmem - 3 ))
if [[ ${hrrrmem} -gt 30 ]] && [[ ${HRRRDAS_BEC} -eq 1  ]]; then #if HRRRDAS BEC is available, use it as first choice
  echo "Do hybrid with HRRRDAS BEC"
  nummem=${hrrrmem}
  cp filelist.hrrrdas filelist03

  beta1_inv=0.50 #0.15
  ifhyb=.true.
  regional_ensemble_option=3
  grid_ratio_ens=1
  i_en_perts_io=0
  ens_fast_read=.true. 
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses HRRRDAS BEC with n_ens=${nummem}" >> ${logfile}
elif [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with GDAS directly"
  beta1_inv=0.50 ##0.15
  ifhyb=.true.
  regional_ensemble_option=1
  grid_ratio_ens=12 #ensemble resolution=3 * grid_ratio * grid_ratio_ens
  i_en_perts_io=3
  ens_fast_read=.false. 
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses GDAS directly with n_ens=${nummem}" >> ${logfile}
fi

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=${FIXgsi}/anavinfo_arw_netcdf
BERROR=${FIXgsi}/rap_berror_stats_global_RAP_tune
SATANGL=${FIXgsi}/global_satangbias.txt
SATINFO=${FIXgsi}/global_satinfo.txt
CONVINFO=${FIXgsi}/nam_regional_convinfo_RAP.txt
OZINFO=${FIXgsi}/global_ozinfo.txt    
PCPINFO=${FIXgsi}/global_pcpinfo.txt
OBERROR=${FIXgsi}/nam_errtable.r3dv


# Fixed fields
cp $anavinfo anavinfo
cp $BERROR   berror_stats
cp $SATANGL  satbias_angle
cp $SATINFO  satinfo
cp $CONVINFO convinfo
cp $OZINFO   ozinfo
cp $PCPINFO  pcpinfo
cp $OBERROR  errtable

# CRTM Spectral and Transmittance coefficients
emiscoef_IRwater=${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${FIXcrtm}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${FIXcrtm}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${FIXcrtm}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${FIXcrtm}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${FIXcrtm}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${FIXcrtm}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${FIXcrtm}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${FIXcrtm}/FASTEM6.MWwater.EmisCoeff.bin
aercoef=${FIXcrtm}/AerosolCoeff.bin
cldcoef=${FIXcrtm}/CloudCoeff.bin

ln -s $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
ln -s $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -s $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -s $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -s $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -s $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -s $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -s $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -s $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do 
   ln -s ${FIXcrtm}/${file}.SpcCoeff.bin ./
   ln -s ${FIXcrtm}/${file}.TauCoeff.bin ./
done

# Get aircraft reject list, mesonet_uselist, sfcobs_provider
{CP} ${FIXgsi}/current_bad_aircraft.txt current_bad_aircraft
{CP} ${FIXgsi}/current_mesonet_uselist.txt gsd_sfcobs_uselist.txt
{CP} ${FIXgsi}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

# Only need this file for single obs test
bufrtable=${FIXgsi}/prepobs_prep.bufrtable
{CP} $bufrtable ./prepobs_prep.bufrtable

# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=${JCAP:-62}
export LEVS=${LEVS:-60}
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

# set GSI namelist according to grid resolution of the variational part
# cloud analysis always runs at 3km but GSIANL may run at 12km or 3km
if [ "${GSIANL_RES}" == "12km" ]; then
  grid_ratio=4
  cloudanalysistype=5
  ens_h=40 #110
  ens_v=3
  run_gsi_2times='YES'
else
  run_gsi_2times='NO'
  grid_ratio=1
  cloudanalysistype=1
  ens_h=20 #40 #110
  ens_v=1 #3
fi

# option for hybrid vertical coordinate (HVC) in WRF-ARW
if [ "${envir}" != "esrl" ]; then ## skip the following for Jet experimental runs
  if [ "$NCDUMP" ] ; then
    n_c3f=`$NCDUMP -h ./wrf_inout | grep -i "C3F:" | wc -l`
    n_c4f=`$NCDUMP -h ./wrf_inout | grep -i "C4F:" | wc -l`
    n_c3h=`$NCDUMP -h ./wrf_inout | grep -i "C3H:" | wc -l`
    n_c4h=`$NCDUMP -h ./wrf_inout | grep -i "C4H:" | wc -l`
    if [[ $n_c3f -gt "1"  && $n_c4f -gt "1" && $n_c3h -gt "1" && $n_c4h -gt "1" ]] ; then
      hybridcord=".true."
    else
      hybridcord=".false."
    fi
  else
    if [ ${YYYYMMDDHH} -lt "2018071118" ] ; then
      hybridcord=".false."
    else
      hybridcord=".true."
    fi
  fi
  echo "HVC option is $hybridcord"
fi

# Build the GSI namelist on-the-fly
${CP} ${PARMgsi}/gsiparm.anl.sh ./
source ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

## satellite bias correction
{CP} ${FIXgsi}/rap_satbias_starting_file.txt ./satbias_in
{CP} ${FIXgsi}/rap_satbias_pc_starting_file.txt ./satbias_pc

# Run GSI
export pgm="rtma3d_gsi"
. prep_step
startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
if [ "${run_gsi_2times}" == "NO" ];  then
  msg="  begin gsi analysis (var+cloudanx)"
else
  msg="  begin first gsi analysis - variational analysis"
fi
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

${CP} ${EXECrtma3d}/${exefile_name_gsi} ${pgm}
if [ "${envir}" == "esrl" ];  then ##GSI on Jet needs special treatment
  module load contrib wrap-mpi
  mpirun ${pgm} < gsiparm.anl > ${pgmout} 2>errfile
else
  ${MPIRUN} ${pgm} < gsiparm.anl > ${pgmout} 2>errfile
fi
##save some information for possible debugging before err_chk
${CAT} fort.* >   fits_${cycle_str}.txt
{LS} -l > GSI_workdir_list
${CAT} errfile GSI_workdir_list >> ${pgmout}
${CP} -p ${pgmout} ${COMOUTgsi_rtma3d}
${CP} -p fits_${cycle_str}.txt ${COMOUTgsi_rtma3d}
export err=$?; err_chk

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
#  listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   listall="conv"
   for type in $listall; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         `${CAT} pe*.${type}_${loop}* > diag_${type}_${string}.${cycle_str}`
      fi
   done
done

## link fort files with user-friendly file name
if [ "${envir}" != "esrl" ]; then
  ${LN} fort.201    fit_p1.${cycle_str}
  ${LN} fort.202    fit_w1.${cycle_str}
  ${LN} fort.203    fit_t1.${cycle_str}
  ${LN} fort.204    fit_q1.${cycle_str}
  ${LN} fort.207    fit_rad1.${cycle_str}
  ${LN} fort.220 minimization_fort220.${cycle_str}
fi

###### second GSI run if needed
if [ "${run_gsi_2times}" == "YES" ];  then
  mv gsiparm.anl gsiparm.anl_var
  mv sigf03 sigf03_step1
  mv siganl sigf03
  grid_ratio=1
  cloudanalysistype=6
  ifhyb=.false.
  # Build the GSI namelist on-the-fly
  {CP} ${PARMgsi}/gsiparm.anl.sh ./
  source ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF
  . prep_step
  startmsg
  msg="***********************************************************"
  postmsg "$jlogfile" "$msg"
  msg="  begin second step gsi analysis: cloud analysis"
  postmsg "$jlogfile" "$msg"
  msg="***********************************************************"
  postmsg "$jlogfile" "$msg"
  if [ "${envir}" == "esrl" ];  then ##GSI on Jet needs special treatment
    module load contrib wrap-mpi
    mpirun ${pgm} < gsiparm.anl > ${pgmout} 2>errfile
  else
    ${MPIRUN} ${pgm} < gsiparm.anl > ${pgmout} 2>errfile
  fi
  {LS} -l > GSI_workdir_list
  ${CAT} errfile GSI_workdir_list >> ${pgmout}
  ${CP} -p ${pgmout} ${COMOUTgsi_rtma3d}/${pgmout}.cloudana
  export err=$?; err_chk

fi ###### second GSI run

# Saving ANALYSIS, DIAG, Obs-Fitting files TO COM2 DIRECTORY AS PRODUCT for archive
${CP} -p gsiparm.anl  ${COMOUTgsi_rtma3d}/gsiparm.anl_${cycle_str}
${TAR} -zcvf ${COMOUTgsi_rtma3d}/diag_${cycle_str} diag_*

if [ "${envir}" != "esrl" ]; then
  ${MV} -p ${DATA}/wrf_inout                  ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
  ${CP} -p minimization_fort220.${cycle_str} ${COMOUTgsi_rtma3d}
  ${CP} -p diag_*                             ${COMOUTgsi_rtma3d}
  ${TAR} -zcvf obsfit_fort220.tgz  ./fort.* ./fit_*
  ${CP} -p  obsfit_fort220.tgz                ${COMOUTgsi_rtma3d}
  tar -zcvf misc_info.tgz  ./*info ./errtable ./prepobs_prep.bufrtable  ./*bias*  \
    ./current_bad_aircraft ./gsd_sfcobs_uselist.txt ./gsd_sfcobs_provider.txt ./GSI_workdir_list
  ${CP} -p  misc_info.tgz                      ${COMOUTgsi_rtma3d}
  gzip ${COMOUTgsi_rtma3d}/diag_*

  # extra backup (NOT necessary)
  #${LN} -sf ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} ${COMOUT}/${ANLrtma3d_FNAME}
  #${CP} -p ${pgmout_stdout}        ${COMOUT}/${pgmout_stdout}_gsianl.${cycle_str}
  #${CP} -p fits_${cycle_str}.txt  ${COMOUT}/fits_${cycle_str}.txt
fi

{RM} -f ${DATA}/sig*
{RM} -f ${DATA}/obs*
{RM} -f ${DATA}/pe*

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
