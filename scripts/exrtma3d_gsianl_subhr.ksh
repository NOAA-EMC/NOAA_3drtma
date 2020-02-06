#!/bin/ksh

set -x 

#-- This script is goint to run GSI analysis (3DVar and Cloud analysis) in one-step
#
echo " This script is goint to run GSI analysis (3DVar and Cloud analysis) in one-step"
export HRRRDAS_BEC=0
#-- Testing the status of some important variables. --#
# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME}" ]; then
  ${ECHO} "ERROR: DATAHOME directory '${DATAHOME}' does not exist!"
  exit 1
fi
# Make sure DATAOBSHOME is defined and exists
if [ ! "${DATAOBSHOME}" ]; then
  ${ECHO} "ERROR: \$DATAOBSHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAOBSHOME}" ]; then
  ${ECHO} "ERROR: DATAOBSHOME directory '${DATAOBSHOME}' does not exist!"
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


# test the existence of script genating namelist file for gsi
if [ ! -f ${PARMgsi}/gsiparm.anl.sh ] ; then
  ${ECHO} "ERROR: ${PARMgsi}/gsiparm.anl.sh does not exist!"
   exit 1
fi

if [  "${DATA_GSIANL}" ]; then
  ${RM} -rf ${DATA_GSIANL}
  ${LN} -sf ${DATA} ${DATA_GSIANL}
fi




#  NCEPSNOW

# Make sure the GSI executable exists
if [ ! -f "${EXECrtma3d}/${exefile_name_gsi}" ]; then
  ${ECHO} "ERROR: GSI Analysis executable '${EXECrtma3d}/${exefile_name_gsi}' does not exist!"
  exit 1
fi

# Check to make sure the number of processors for running GSI was specified
if [ -z "${GSIPROC}" ]; then
  ${ECHO} "ERROR: The variable $GSIPROC must be set to contain the number of processors to run GSI"
  exit 1
fi

# Check to make sure that fix direcory exists

# Check to make sure that ENKF_FCST exists

# Check to make sure that FULLCYC exists
if [ ! "${FULLCYC}" ]; then
  ${ECHO} "ERROR: FULLCYC '${FULLCYC}' does not exist"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format

mm=$subcyc
subhtime=$subcyc
${ECHO} $PDY $cyc $mm
  START_TIME=${START_TIME:-"{PDY} ${cyc}"}      # YYYYMMDD HH

echo $START_TIME
echo $subhtime
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
  START_TIME=`${DATE} -d "${START_TIME} ${subhtime} minutes"`
fi
echo $START_TIME

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`


if (((${HH} >= 00) && (${HH} <=  05))) ; then
HHR=00
HHRm1=12
fi
if (((${HH} >= 06) && (${HH} <=  11))) ; then
HHR=06
HHRm1=00
fi
if (((${HH} >= 12) && (${HH} <=  23))) ; then
HHR=12
HHRm1=06
fi

HH_cycp1=`echo ${PDYHH_cycp1} | cut -c 9-10`
# Create the working directory and cd into it
workdir=${DATAHOME}
cd ${workdir}

# Define the output log file depending on if this is the full or partial cycle
ifsoilnudge=.true.

# Bring over background field (it's modified by GSI so we can't link to it)
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}

# Look for bqckground from pre-forecast background
if [ -r ${DATAHOME_BK}/${FGSrtma3d_FNAME} ]; then
# copy the background to running directory (it is going to updatd by analysis)
  cpfs ${DATAHOME_BK}/${FGSrtma3d_FNAME}    ./wrf_inout
  ${ECHO} " Cycle ${YYYYMMDDHHMU}: GSI background=${DATAHOME_BK}/${FGSrtma3d_FNAME}"
else
# No background available so abort
  ${ECHO} "${DATAHOME_BK}/${FGSrtma3d_FNAME} does not exist!!"
  ${ECHO} "ERROR: No background file for analysis at ${time_run}!!!!"
  ${ECHO} " Cycle ${YYYYMMDDHHMMMU}: GSI failed because of no background" >> ${pgmout}
  exit 1
fi

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle

# Link to the prepbufr data

if [ -r ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr ] ; then
  ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr ./prepbufr
elif [ -r ${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00 ] ; then
  ${LN} -s ${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.prepbufr.tm00 ./prepbufr
else
  ${ECHO} "Warning: either ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}${subcyc}z.prepbufr or rap.t${HH}${subcyc}z.prepbufr.tm00 does not exist!"
fi

if [ -r "${DATAOBSHOME}/NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NSSLRefInGSI.bufr ./refInGSI
elif [ -r "${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.NSSLRefInGSI.bufr ./refInGSI
elif [ -r "${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}:NSSLRefInGSI.bufr does not exist!"
fi

if [ -r "${DATAOBSHOME}/LightningInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/LightningInGSI.bufr ./lghtInGSI
elif [ -r "${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.LightningInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.LightningInGSI.bufr ./lghtInGSI
elif [ -r "${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d" ]; then
  ${LN} -s ${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.lghtng.tm00.bufr_d ./lghtInGSI
elif [ -r "${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.LightningInGSI_bufr.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}: LightningInGSI.bufr does not exist!"
fi


if [ -r "${DATAOBSHOME}/NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr ./larcInGSI
elif [ -r "${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/hrrr.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr ./larcInGSI
elif [ -r "${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.lgycld.tm00.bufr_d" ]; then
  ${LN} -s ${DATAOBSHOME}/rtma_ru.t${HH}${subcyc}z.lgycld.tm00.bufr_d ./larcInGSI
elif [ -r "${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/${RUN}.t${HH}${subcyc}z.NASALaRCCloudInGSI.bufr ./larcInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}: NASALaRCCloudInGSI.bufr does not exist!"
fi

# Set runtime and save directories
export endianness=Big_Endian

# Set variables used in script
#   ncp is cp replacement, currently keep as /bin/cp
ncp=/bin/cp

export HYB_ENS=".true."

# Get Fv3GDAS Enkf files
# We expect 80 total files to be present (80 enkf)
export nens=80

# Not using FGAT or 4DEnVar, so hardwire nhr_assimilation to 3
export nhr_assimilation=03
##typeset -Z2 nhr_assimilation


python ${UTILrtma3d_dev}/getbest_EnKF_FV3GDAS.py -v $YYYYMMDDHH --exact=no --minsize=${nens} -d ${COMINGDAS}/enkfgdas -m no -o filelist${nhr_assimilation} --o3fname=gfs_sigf${nhr_assimilation} --gfs_nemsio=yes
 

#Check to see if ensembles were found 
numfiles=`cat filelist03 | wc -l`

if [ $numfiles -ne 80 ]; then
  echo "Ensembles not found - turning off ifhyb!"
  export ifhyb=".false."
else
#   we have 80 files, figure out if they are all the right size
#   if not, set ifhyb=false
    cp ${UTILrtma3d_dev}/convert.sh .
    ${UTILrtma3d_dev}/check_enkf_size.sh
fi

if [ ${HRRRDAS_BEC} -gt 0 ]; then
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will be used if available"
  #----------------------------------------------------
  # generate list of HRRRDAS members for ensemble covariances
  # Use 1-hr forecasts from the HRRRDAS cyclin
  c=1
   ${LS} ${COMINhrrrdas}/hrrr.t${HH}00z.f0100.mem000${c}.netcdf > filelist.hrrrdas

  c=2
  while [[ $c -le 36 ]]; do
   if [ $c -lt 10 ]; then
    cc="0"$c
   else
    cc=$c
   fi
   hrrre_file=${COMINhrrrdas}/hrrr.t${HH}00z.f0100.mem00${cc}.netcdf
   ${LS} ${COMINhrrrdas}/hrrr.t1000z.f0100.mem00${cc}.netcdf >> filelist.hrrrdas
   ${LN} -sf ${hrrre_file} wrf_en0${cc}
   ((c = c + 1))
  done
else
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will NOT be used"
  touch filelist.hrrrdas #so as to avoid "no such file" error message
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
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses HRRRDAS BEC with n_ens=${nummem}" >> ${pgmout}
elif [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with GDAS directly"
  beta1_inv=0.50 ##0.15
  ifhyb=.true.
  regional_ensemble_option=1
  grid_ratio_ens=12 #ensemble resolution=3 * grid_ratio * grid_ratio_ens
  i_en_perts_io=0
  ens_fast_read=.false. 
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses GDAS directly with n_ens=${nummem}" >> ${pgmout}
fi

# Determine if hybrid option is available
#beta1_inv=1.0
#ifhyb=.false.

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
#SATINFO=${FIXgsi}/global_satinfo.txt
SATINFO=${FIXgsi}/rap_global_satinfo.txt
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
emiscoef_MWwater=${FIXcrtm}/FASTEM5.MWwater.EmisCoeff.bin
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
ln -s $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do 
   ln -s ${FIXcrtm}/${file}.SpcCoeff.bin ./
   ln -s ${FIXcrtm}/${file}.TauCoeff.bin ./
done

# Get aircraft reject list
cp ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft

sfcuselists_path=${SFCOBS_USELIST}
sfcuselists=current_mesonet_uselist.txt
#sfcuselists=${YYYY}-${MM}-${DD}_meso_uselist.txt
cp ${sfcuselists_path}/${sfcuselists} gsd_sfcobs_uselist.txt

cp ${SFCOBS_PROVIDER}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

# Only need this file for single obs test
bufrtable=${FIXgsi}/prepobs_prep.bufrtable
cp $bufrtable ./prepobs_prep.bufrtable

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

ndatrap=67  #62 ?

# 3DVar and Cloud analysis in one-step
#grid_ratio=${GSI_grid_ratio_in_var:-1}
#cloudanalysistype=1

# option for hybrid vertical coordinate (HVC) in WRF-ARW

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
  if [ ${YYYYMMDDHHMU} -lt "201807111800" ] ; then
    hybridcord=".false."
  else
    hybridcord=".true."
  fi
fi
echo "HVC option is $hybridcord"

# Build the GSI namelist on-the-fly
cp ${PARMgsi}/gsiparm.anl.sh ./
. ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

## satellite bias correction
cp ${FIXgsi}/rap_satbias_starting_file.txt ./satbias_in
cp ${FIXgsi}/rap_satbias_pc_starting_file.txt ./satbias_pc

# Run GSI
pgm=${RUN}_gsianl
. prep_step

startmsg
msg="***************************************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin gsi analysis  : variational + cloud analysis in one-step"
postmsg "$jlogfile" "$msg"
msg="***************************************************************************"
postmsg "$jlogfile" "$msg"

# Save a copy of the GSI executable in the workdir
${CP} ${EXECrtma3d}/${exefile_name_gsi}   ./rtma3d_gsi

 runline="${MPIRUN}         ./rtma3d_gsi"
$runline < gsiparm.anl >> ${pgmout} 2>errfile
export err=$? ; err_chk

#===========================================================#
# error checking used in GSD script
#===========================================================#
export pgmout_stdout="stdout"
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
         `cat pe*.${type}_${loop}* > diag_${type}_${string}.${YYYYMMDDHHMU}`
      fi
   done
done

# save results from 1st run
${CP} fort.201    fit_p1.${YYYYMMDDHHMU}
${CP} fort.202    fit_w1.${YYYYMMDDHHMU}
${CP} fort.203    fit_t1.${YYYYMMDDHHMU}
${CP} fort.204    fit_q1.${YYYYMMDDHHMU}
${CP} fort.207    fit_rad1.${YYYYMMDDHHMU}
cat   fort.* >    fits_${YYYYMMDDHHMU}.txt
${CP} -p fort.220 minimization_fort220.${YYYYMMDDHHMU}
# cat fort.* > ${COMOUT}/fits_${YYYYMMDDHH}.txt


# Saving ANALYSIS, DIAG, Obs-Fitting files TO COM2 DIRECTORY AS PRODUCT for archive
${CP} -p ${DATA}/wrf_inout                  ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
${CP} -p ${pgmout_stdout}                   ${COMOUTgsi_rtma3d}/${pgmout_stdout}_gsianl.${YYYYMMDDHHMU}
${CP} -p fits_${YYYYMMDDHHMU}.txt             ${COMOUTgsi_rtma3d}/fits_${YYYYMMDDHHMU}.txt
${CP} -p minimization_fort220.${YYYYMMDDHHMU} ${COMOUTgsi_rtma3d}/minimization_fort220.${YYYYMMDDHHMU}
${CP} -p gsiparm.anl                        ${COMOUTgsi_rtma3d}/gsiparm.anl.${YYYYMMDDHHMU}
${CP} -p diag_*                             ${COMOUTgsi_rtma3d}/

tar -zcvf obsfit_fort220.tgz  ./fort.* ./fit_*
${CP} -p  obsfit_fort220.tgz                 ${COMOUTgsi_rtma3d}
tar -zcvf misc_info.tgz       ./*info ./errtable ./prepobs_prep.bufrtable  ./*bias*  ./current_bad_aircraft ./gsd_sfcobs_uselist.txt ./gsd_sfcobs_provider.txt ./GSI_workdir_list
${CP} -p  misc_info.tgz                      ${COMOUTgsi_rtma3d}
gzip ${COMOUTgsi_rtma3d}/diag_*

# extra backup (NOT necessary)
#${LN} -sf ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} ${COMOUT}/${ANLrtma3d_FNAME}
#${CP} -p  ${pgmout_stdout}                       ${COMOUT}/${pgmout_stdout}_gsianl.${YYYYMMDDHHMU}
#${CP} -p  fits_${YYYYMMDDHHMU}.txt                 ${COMOUT}/fits_${YYYYMMDDHHMU}.txt

#/bin/rm -f ${DATA}/wrf_inout
/bin/rm -f ${DATA}/sig*
/bin/rm -f ${DATA}/obs*
/bin/rm -f ${DATA}/pe*

exit 0
