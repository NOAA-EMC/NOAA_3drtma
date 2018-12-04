#!/bin/sh

set -x 

#-- Testing the status of some important variables. --#
# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi

if [ ! "${OBS_DIR}" ]; then
  ${ECHO} "ERROR: \$OBS_DIR is not defined!"
  exit 1
fi
if [ ! -d "${OBS_DIR}" ]; then
  ${ECHO} "ERROR: $OBS_DIR does not exist!"
  exit 1
fi
# rm -rf ${DATAOBSHOME}
# ${LN} -s  ${OBS_DIR}  ${DATAOBSHOME}

if [ ! "${HRRR_DIR}" ]; then
  ${ECHO} "ERROR: \$HRRR_DIR is not defined!"
  exit 1
fi
if [ ! -d "${HRRR_DIR}" ]; then
  ${ECHO} "ERROR: $HRRR_DIR does not exist!"
  exit 1
fi

# test the existence of script genating namelist file for gsi
if [ ! -f ${USHrtma3d}/namelist/gsiparm.anl.sh ] ; then
  ${ECHO} "ERROR: ${USHrtma3d}/namelist/gsiparm.anl.sh does not exist!"
   exit 1
fi

#  PREPBUFR

#  NCEPSNOW

# Make sure GSI_ROOT is defined and exists
if [ ! "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: \$GSI_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: GSI_ROOT directory '${GSI_ROOT}' does not exist!"
  exit 1
fi

# Make sure DATAHOME_BK is defined and exists
if [ ! "${DATAHOME_BK}" ]; then
  ${ECHO} "ERROR: \$DATAHOME_BK is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME_BK}" ]; then
  ${ECHO} "ERROR: DATAHOME_BK directory '${DATAHOME_BK}' does not exist!"
  exit 1
fi

# Check to make sure the number of processors for running GSI was specified
if [ -z "${GSIPROC}" ]; then
  ${ECHO} "ERROR: The variable $GSIPROC must be set to contain the number of processors to run GSI"
  exit 1
fi

# Check to make sure that fix direcory exists
if [ ! -d ${fix_dir} ]; then
  ${ECHO} "ERROR: ${fix_dir} does not exist"
  exit 1
fi

# Check to make sure that ENKF_FCST exists

# Check to make sure that FULLCYC exists
if [ ! "${FULLCYC}" ]; then
  ${ECHO} "ERROR: FULLCYC '${FULLCYC}' does not exist"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format

# START_TIME=${PDY}' '${cyc}
START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
echo $cyc
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
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Make sure the GSI executable exists
if [ ! -x "${GSI}" ]; then
  ${ECHO} "ERROR: ${GSI} does not exist!"
  exit 1
fi

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

# Create the working directory and cd into it
workdir=${DATAHOME}
# ${RM} -rf ${workdir}
# ${MKDIR} -p ${workdir}
# if [ "`stat -f -c %T ${workdir}`" == "lustre" ]; then
#  lfs setstripe --count 8 ${workdir}
# fi
cd ${workdir}

# Define the output log file depending on if this is the full or partial cycle
ifsoilnudge=.true.

# Save a copy of the GSI executable in the workdir
${CP} ${GSI} .

# Bring over background field (it's modified by GSI so we can't link to it)
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}

# Look for bqckground from pre-forecast background
if [ -r ${DATAHOME_BK}/wrfout_d01_${time_str} ]; then
  ${ECHO} " Cycled run using ${DATAHOME_BK}/wrfout_d01_${time_str}"
  cp ${DATAHOME_BK}/wrfout_d01_${time_str} ./wrf_inout
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI background=${DATAHOME_BK}/wrfout_d01_${time_str}"

# No background available so abort
else
  ${ECHO} "${DATAHOME_BK}/wrfout_d01_${time_str} does not exist!!"
  ${ECHO} "ERROR: No background file for analysis at ${time_run}!!!!"
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI failed because of no background" >> ${pgmout} 
  exit 1
fi

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle

# Link to the prepbufr data

${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr ./prepbufr

if [ -r "${DATAOBSHOME}/NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/NSSLRefInGSI.bufr dones not exist!"
fi

if [ -r "${DATAOBSHOME}/LightningInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/LightningInGSI.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/LightningInGSI.bufr dones not exist!"
fi

if [ -r "${DATAOBSHOME}/NASALaRCCloudInGSI_bufr.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NASALaRCCloudInGSI_bufr.bufr ./larcInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/NASALaRCCloudInGSI_bufr.bufr does not exist!"
  ${ECHO} "Warning: try ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr!"
  if [ -r "${DATAOBSHOME}/NASALaRCCloudInGSI.bufr" ]; then
    ${LN} -s ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr ./larcInGSI
  else
    ${ECHO} "Warning: ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr does not exist!"
  fi
fi

# Link statellite radiance data

# Link the radial velocity data

## 
## Find closest GFS EnKF forecast to analysis time
##
## 
## Link to pre-processed GFS EnKF forecast members
##
for mem in `ls ${DATAROOT}/gfsenkf/enspreproc_arw_mem???`
do
  memname=`basename ${mem}`
  ${LN} -s ${mem} ${memname}
done

${LS} enspreproc_arw_mem??? > filelist

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
nummem=`more filelist | wc -l`
nummem=$((nummem - 3 ))
if [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with ${memname}"
  beta1_inv=0.25
  ifhyb=.true.
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses ${memname} with n_ens=${nummem}" >> ${logfile}
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

anavinfo=${fixdir}/anavinfo_arw_netcdf
BERROR=${fixdir}/rap_berror_stats_global_RAP_tune
SATANGL=${fixdir}/global_satangbias.txt
#SATINFO=${fixdir}/global_satinfo.txt
SATINFO=${fixdir2}/rap_global_satinfo.txt
CONVINFO=${fixdir}/nam_regional_convinfo_RAP.txt
OZINFO=${fixdir}/global_ozinfo.txt    
PCPINFO=${fixdir}/global_pcpinfo.txt
OBERROR=${fixdir}/nam_errtable.r3dv


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
CRTMFIX=${FIX_CRTM:-${fixdir}/CRTM_Coefficients}
emiscoef_IRwater=${CRTMFIX}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${CRTMFIX}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${CRTMFIX}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${CRTMFIX}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${CRTMFIX}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${CRTMFIX}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${CRTMFIX}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${CRTMFIX}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${CRTMFIX}/FASTEM5.MWwater.EmisCoeff.bin
aercoef=${CRTMFIX}/AerosolCoeff.bin
cldcoef=${CRTMFIX}/CloudCoeff.bin

ln -s $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
ln -s $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -s $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -s $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -s $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -s $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -s $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -s $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -s $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do 
   ln -s ${CRTMFIX}/${file}.SpcCoeff.bin ./
   ln -s ${CRTMFIX}/${file}.TauCoeff.bin ./
done

# Get aircraft reject list
cp ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft

sfcuselists_path=${SFCOBS_USELIST}
sfcuselists=current_mesonet_uselist.txt
#sfcuselists=${YYYY}-${MM}-${DD}_meso_uselist.txt
cp ${sfcuselists_path}/${sfcuselists} gsd_sfcobs_uselist.txt

cp ${SFCOBS_PROVIDER}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

# Only need this file for single obs test
bufrtable=${fixdir}/prepobs_prep.bufrtable
cp $bufrtable ./prepobs_prep.bufrtable

# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=${JCAP:-62}
export LEVS=${LEVS:-60}
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

ndatrap=62
grid_ratio=${GSI_grid_ratio_in_var:-1}
cloudanalysistype=5

# Build the GSI namelist on-the-fly
cp ${USHrtma3d}/namelist/gsiparm.anl.sh ./
. ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

## satellite bias correction
cp ${fixdir}/rap_satbias_starting_file.txt ./satbias_in
cp ${fixdir}/rap_satbias_pc_starting_file.txt ./satbias_pc

# Run GSI

#export pgm=${RUN}_gsianl
export pgm=${GSI:-"${RUN}_gsianl"}
if [ -f errfile ] ; then
    rm -f errfile
fi

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin gsi analysis for 1st pass: variational analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

#runline="${MPIRUN} -np $np ${GSI} < gsiparm.anl > stdout 2>&1"
#runline="${MPIRUN} -np $np ${pgm} < gsiparm.anl >> ${pgmout} 2>errfile"
 runline="${MPIRUN} -np $np ${pgm}"
$runline < gsiparm.anl >> ${pgmout} 2>errfile
export err=$? ; err_chk

#===========================================================#
# error checking used in GSD script
#===========================================================#
export pgmout_stdout="stdout_var"
cat ${pgmout} > ${pgmout_stdout}
if [ -f errfile ] ; then
    cat errfile >> ${pgmout_stdout}
fi

export error=$err
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${GSI} crashed  Exit status=${error}"
  cp -p ${pgmout_stdout}  ../.
  exit ${error}
fi

ls -l > GSI_workdir_list

# Look for successful completion messages in rsl files
nsuccess=`${TAIL} -200 ${pgmout_stdout} | ${AWK} '/PROGRAM GSI_ANL HAS ENDED/' | ${WC} -l`
ntotal=1 
${ECHO} "Found ${nsuccess} of ${ntotal} completion messages"
if [ ${nsuccess} -ne ${ntotal} ]; then
   ${ECHO} "ERROR: ${GSI} did not complete sucessfully  Exit status=${error}"
   cp -p ${pgmout_stdout}  ../.
   cp GSI_workdir_list ../.
   if [ ${error} -ne 0 ]; then
     exit ${error}
   else
     exit 1
   fi
fi

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
         `cat pe*.${type}_${loop}* > diag_${type}_${string}.${YYYYMMDDHH}`
      fi
   done
done

# save results from 1st run
${CP} fort.201    fit_p1.${YYYYMMDDHH}
${CP} fort.202    fit_w1.${YYYYMMDDHH}
${CP} fort.203    fit_t1.${YYYYMMDDHH}
${CP} fort.204    fit_q1.${YYYYMMDDHH}
${CP} fort.207    fit_rad1.${YYYYMMDDHH}
${CP} ${pgmout_stdout}  ${COMOUT}/${pgmout_stdout}_gsianl.${YYYYMMDDHH}
# cat fort.* > ${DATABASE_DIR}/log/fits_${YYYYMMDDHH}.txt
cat fort.* > ${COMOUT}/fits_${YYYYMMDDHH}.txt

## second GSI run

mv gsiparm.anl gsiparm.anl_var
mv sigf03 sigf03_step1
mv siganl sigf03

ndatrap=67
grid_ratio=${GSI_grid_ratio_in_cldanl:-1}
cloudanalysistype=6
ifhyb=.false.

# Build the GSI namelist on-the-fly
cp ${USHrtma3d}/namelist/gsiparm.anl.sh ./
. ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

if [ -f errfile ] ; then
    rm -f errfile
fi

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin gsi analysis for 2nd pass: cloud analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# Run GSI
#runline="${MPIRUN} -np $np ${GSI} < gsiparm.anl > stdout 2>&1"
#runline="${MPIRUN} -np $np ${pgm} < gsiparm.anl >> ${pgmout} 2>errfile"
 runline="${MPIRUN} -np $np ${pgm}"
$runline < gsiparm.anl >> ${pgmout}  2>errfile
export err=$? ; err_chk

#===========================================================#
# error checking used in GSD script
#===========================================================#
export pgmout_stdout="stdout_cloud"
cat ${pgmout} > ${pgmout_stdout}
if [ -f errfile ] ; then
    cat errfile >> ${pgmout_stdout}
fi

export error=$err
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${GSI} crashed  Exit status=${error}"
  cp ${pgmout_stdout}  ../.
  exit ${error}
fi
ls -l > GSI_workdir_list_cloud

# Look for successful completion messages in rsl files
nsuccess=`${TAIL} -200 ${pgmout_stdout} | ${AWK} '/PROGRAM GSI_ANL HAS ENDED/' | ${WC} -l`
ntotal=1
${ECHO} "Found ${nsuccess} of ${ntotal} completion messages"
if [ ${nsuccess} -ne ${ntotal} ]; then
   ${ECHO} "ERROR: ${GSI} did not complete sucessfully  Exit status=${error}"
   cp ${pgmout_stdout}  ../.
   cp GSI_workdir_list_cloud ../.
   if [ ${error} -ne 0 ]; then
     exit ${error}
   else
     exit 1
   fi
fi
${CP} -p  ${pgmout_stdout}  ${COMOUT}/${pgmout_stdout}_gsianl.${YYYYMMDDHH}

exit 0
