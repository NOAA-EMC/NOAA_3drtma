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
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then
OBS_DIR=${DATAOBSHOME}
BKG_DIR=${DATAHOME_BK}
COMINhrrrdas=${COMINHRRRDAS}
fi
subcyc=${subcyc:-"00"}
START_TIME=`${DATE} -d "${PDY} ${cyc} ${subcyc} minutes"`
# Compute date & time components for the analysis time
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
time_1hour_ago=`${DATE} -d "${START_TIME} 1 hour ago" +%Y%m%d%H`
time_2hour_ago=`${DATE} -d "${START_TIME} 2 hour ago" +%Y%m%d%H`
time_3hour_ago=`${DATE} -d "${START_TIME} 3 hour ago" +%Y%m%d%H`
time_4hour_ago=`${DATE} -d "${START_TIME} 4 hour ago" +%Y%m%d%H`
time_5hour_ago=`${DATE} -d "${START_TIME} 5 hour ago" +%Y%m%d%H`
time_6hour_ago=`${DATE} -d "${START_TIME} 6 hour ago" +%Y%m%d%H`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
time_str2=`${DATE} "+%Y-%m-%d_%H_00_00" -d "${START_TIME}"`
#----- enter working directory -------
cd ${DATA}
${ECHO} "enter working directory:${DATA}"
nc_diag_cat=${EXECrtma3d}/ncdiag_cat_serial.x
# Define the loghistory file depending on if this is the full or partial cycle
#ifsoilnudge=.true.
ifsoilnudge=.true.

cycle_str=${PDY}${cyc}
# Look for background field for GSI analysis
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #wcoss expr runs
  GSIbackground=${BKG_DIR}/${FGSrtma3d_FNAME}
fi

if [ -r ${GSIbackground} ]; then
  cpfs ${GSIbackground} ./wrf_inout
  ${ECHO} " Cycle ${cycle_str}: GSI background=${GSIbackground}"
else
  # No background available so abort
  ${ECHO} "${GSIbackground} does not exist!!"
  ${ECHO} "FATAL ERROR: No background file for analysis at ${time_str}!!!!"
  exit 1
fi

#  Detecting the existence of Ocean Significant Wave Height (HOWV) in firstguess
i_found_howv=0
RUN_HOWV="No"
#if [ "$NCDUMP" ] ; then
# i_found_howv=$($NCDUMP -h ./wrf_inout | grep -i "HOWV" | wc -l)  #-->multiple lines are found
  i_found_howv=$(ncdump  -h ./wrf_inout | grep -i " HOWV(" | wc -l) 
  if [[ "${i_found_howv}" -eq 1 ]] ; then       # found unique variable HOWV
    RUN_HOWV="Yes"
  else
    RUN_HOWV="No"
  fi
#fi
# Note: if RUN_HOWV=TRUE/Yes, then DO analysis of wave height.
#       If no matter HOWV is available in firstguess, user DOES NOT want to analyz wave height,
#       please reset RUN_HOWV="No".
# RUN_HOWV="No"

# Link to the prepbufr data
if [ -r ${OBS_DIR}/${NET}.t${cyc}z.prepbufr.tm00 ]; then
  ${LN} -sf ${OBS_DIR}/${NET}.t${cyc}z.prepbufr.tm00 ./prepbufr
fi

if [ -r "${OBS_DIR}/${NET}.t${cyc}z.NSSLRefInGSI.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/${NET}.t${cyc}z.NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NSSLRefInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/${RUN}.t${cyc}z.LightningInGSI_bufr.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/${RUN}.t${cyc}z.LightningInGSI_bufr.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: LightningInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/${RUN}.t${cyc}z.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/${RUN}.t${cyc}z.NASALaRCCloudInGSI.bufr ./larcInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NASALaRCCloudInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/${NET}.t${cyc}z.satwnd.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/${NET}.t${cyc}z.satwnd.tm00.bufr_d ./satwndbufr
else
  ${ECHO} "Warning: ${OBS_DIR}: satwnd does not exist!"
fi

if [ -r "${OBS_DIR}/${NET}.t${cyc}z.nexrad.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/${NET}.t${cyc}z.nexrad.tm00.bufr_d ./nexradbufr
else
  ${ECHO} "Warning: ${OBS_DIR}: nexrad does not exist!"
fi

if [[ $cyc == $cyc_mitm ]]  ; then
    if [ -s ${DATA_OBSPRDm1}/rtma.${PDY}.mintobs.dat ] #use only if conventional data also available
       then
        cpreq ${DATA_OBSPRDm1}/rtma.${PDY}.mintobs.dat mitmdat
        echo `ls -l mitmdat`
    else
        echo "* WARNING: minT observation file $COM_IN/${NET}.${PDY}/${NET}.${PDY}.mintobs.dat is not available ..."
    fi
fi
if [[ $cyc == $cyc_mxtm ]]  ; then
      if [ -s  ${DATA_OBSPRDm1}/rtma.${PDYm1}.maxtobs.dat ] #use only if conventional data also available
        then
          cpreq  ${DATA_OBSPRDm1}/rtma.${PDYm1}.maxtobs.dat mxtmdat
          echo `ls -l mxtmdat`
       else
         echo "* WARNING: maxT observation file $COM_IN/${NET}.${PDYm1}/${NET}.${PDYm1}.maxtobs.dat is not available ..."
      fi
fi

if [ -r "${OBS_DIR}/${NET}.t${cyc}z.satmar.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/${NET}.t${cyc}z.satmar.tm00.bufr_d ./satmar
else
  ${ECHO} "Warning: ${OBS_DIR}: satmar does not exist!"
fi

# Searching GDAS ensemble forecast for hybrid 3DEnVar analysis
rm -f filelist03
#if [ "${envir}" = "lsf" ] || [ "${envir}" = "pbspro" ] && [ ${HRRRDAS_BEC} -eq 0 ] ; then #WCOSS
  # Set runtime and save directories
  export endianness=Big_Endian

  # Set variables used in script
  #   ncpreq is cpreq replacement, currently keep as /bin/cp
  ncp=/bin/cp

  export HYB_ENS=".true."

  # Get Fv3GDAS Enkf files
  # We expect 80 total files to be present (80 enkf)
  export nens=80

  # Not using FGAT or 4DEnVar, so hardwire nhr_assimilation to 3
  export nhr_assimilation=03
  ##typeset -Z2 nhr_assimilation

  echo "checking if python is available:"
  ls -l /usr/bin/python
  which python

  # /usr/bin/python ${UTILrtma3d_dev}/getbest_EnKF_FV3GDAS.py -v $YYYYMMDDHH --exact=no --minsize=${nens} -d ${COMINGDAS}/enkfgdas -m no -o filelist${nhr_assimilation} --o3fname=gfs_sigf${nhr_assimilation} --gfs_netcdf=yes   
  python ${UTILrtma3d_dev}/getbest_EnKF_FV3GDAS.py -v $YYYYMMDDHH --exact=no --minsize=${nens} -d ${COMINGDAS}/enkfgdas -m no -o filelist${nhr_assimilation} --o3fname=gfs_sigf${nhr_assimilation} --gfs_netcdf=yes   
  #Check to see if ensembles were found 
  numfiles=`cat filelist03 | wc -l`

  if [ $numfiles -ne 80 ]; then
    echo "Ensembles not found - turning off ifhyb!"
    export ifhyb=".false."
  else
  #   we have 80 files, figure out if they are all the right size
  #   if not, set ifhyb=false
      cpreq ${UTILrtma3d_dev}/convert.sh .
  fi
#fi
${TOUCH} filelist03 #so as to avoid "no such file" error message

# Searching HRRRDAS ensemble 
if [ ${HRRRDAS_BEC} -eq 1 ]; then
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will be used if available"
  #----------------------------------------------------
  # generate list of HRRRDAS members for ensemble covariances
  # Use 1-hr forecasts from the HRRRDAS cycling
  rm -f ./filelist.hrrrdas
  c=1
  while [[ $c -le 36 ]]; do
   cc=$(printf "%02d" $c)
   hrrre_file=${COMINhrrrdas}/hrrrdas_small_d02_${time_1hour_ago}00f01_mem00${cc}
#  ${LS} ${COMINhrrrdas}/hrrrdas_small_d02_${time_1hour_ago}00f01_mem00${cc} >> filelist.hrrrdas
   ${LS} ${hrrre_file} >> filelist.hrrrdas
   ${LN} -sf ${hrrre_file} wrf_en0${cc}
   ((c = c + 1))
  done
else
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will NOT be used"
  ${TOUCH} filelist.hrrrdas #so as to avoid "no such file" error message
fi

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
readin_localization=.false.
nummem=`more filelist03 | wc -l`
nummem=$((nummem - 3 ))
hrrrmem=`more filelist.hrrrdas | wc -l`
hrrrmem=$((hrrrmem - 3 ))
if [[ ${hrrrmem} -gt 30 ]] && [[ ${HRRRDAS_BEC} -eq 1  ]]; then #if HRRRDAS BEC is available, use it as first choice
  echo "Do hybrid with HRRRDAS BEC"
  EnsWgt=0.9
  nummem=${hrrrmem}
  cpreq filelist.hrrrdas filelist03
  ${CP} ${PARMgsi}/hybens_info_hrrrdas hybens_info
  beta1_inv=$(( 1 - $EnsWgt  ))
  ifhyb=.true.
  regional_ensemble_option=3
  grid_ratio_ens=1
  i_en_perts_io=0
  ens_fast_read=.true. 
  if [[ "${READIN_LOCALIZATION}" == "TRUE" ]] || [[ "${READIN_LOCALIZATION}" == "true" ]] ; then
     readin_localization=.true.
  fi
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses HRRRDAS BEC with n_ens=${nummem}" >> ${pgmout}
elif [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with GDAS directly"
  EnsWgt=0.5
  ${CP} ${PARMgsi}/hybens_info_hrrrdas hybens_info
  beta1_inv=$(( 1 - $EnsWgt  ))
  ifhyb=.true.
  regional_ensemble_option=1
  grid_ratio_ens=3 #ensemble resolution=3 * grid_ratio * grid_ratio_ens
  i_en_perts_io=0
  ens_fast_read=.false. 
  if [[ "${READIN_LOCALIZATION}" == "TRUE" ]] || [[ "${READIN_LOCALIZATION}" == "true" ]] ; then
     readin_localization=.true.
  fi
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI hybrid uses GDAS directly with n_ens=${nummem}" >> ${pgmout}
else
  beta1_inv=1.0
  ifhyb=.false.
  regional_ensemble_option=1
  grid_ratio_ens=1
  i_en_perts_io=0
  ens_fast_read=.false.
  readin_localization=.false.
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI running pure 3DVar without ensenble covariances." >> ${pgmout}
fi

# copy the read-in localization file for hybrid envar analysis
  HYBENS_INFO="hybens_info"
  if [[ "${readin_localization}" == ".true." ]] ; then
     cp -p ${PARMgsi}/${HYBENS_INFO}  hybens_info
     
     # read in the weight for static background error at the surface level in hybrid envar run
     # the weight would be used to adjust the background error for howv/gust/vis
     hybens_info_file="hybens_info"
     n=0
     set +x
     while read line_str
     do
        echo "Line $n: --> $line_str"
        if [[ $n -eq 1 ]] ; then
           StaticWgt=`echo $line_str | awk -F ' ' '{print $3}' `
           echo "Weight of static background error at level $n  --> $StaticWgt"
        fi
        let "n=n+1"
     done < "$hybens_info_file"
     set -x
  else
     StaticWgt=${beta1_inv}
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

anavinfo=${FIXgsi}/rtma3d_anavinfo_arw_netcdf
BERROR=${FIXgsi}/3drtma_berror_stats_hz01
#BERROR=${FIXgsi}/rap_berror_stats_global_RAP_tune
SATANGL=${FIXgsi}/global_satangbias.txt
SATINFO=${FIXgsi}/global_satinfo.txt
CONVINFO=${FIXgsi}/rtma3d_convinfo_v1.0.txt
#CONVINFO=${FIXgsi}/3drtma_convinfo_v0.6.5_updated
OZINFO=${FIXgsi}/global_ozinfo.txt
PCPINFO=${FIXgsi}/global_pcpinfo.txt
OBERROR=${FIXgsi}/3drtma_errtable_smallSFCerr_ascat
#OBERROR=${FIXgsi}/nam_errtable.r3dv

ANAVINFO_MXTM_FN=rtma3d_anavinfo_arw_netcdf_mxtm
ANAVINFO_MITM_FN=rtma3d_anavinfo_arw_netcdf_mitm
ANAVINFO_HOWV_FN=urma3d_anavinfo_arw_netcdf
ANAVINFO_MXTM_HOWV_FN=urma3d_anavinfo_arw_netcdf_mxtm
ANAVINFO_MITM_HOWV_FN=urma3d_anavinfo_arw_netcdf_mitm
CONVINFO_HOWV_FN=urma3d_convinfo_v1.0.txt

#==========================================================================#
# Note:                                                                    #
#      If running with analysis of mint/maxt, please comment off the       #
#      following if-block, use the next if-block for mint/maxt and howv    #
# If doing the analysis of wave height (HOWV) in 3DRTMA
if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
   anavinfo=${FIXgsi}/${ANAVINFO_HOWV_FN}
   CONVINFO=${FIXgsi}/${CONVINFO_HOWV_FN}
fi
#==========================================================================#

#==========================================================================#
# Note:                                                                    #
#      The code in GSI for direct analysis of mint/maxt is not ready yet,  #
#       so do NOT use the anavinfo file with mint or maxt for now.         #
# if doing the analysis of minT/maxT in 3DRTMA
# if [[ $cyc == $cyc_mitm ]]  ; then
#    anavinfo=${FIXgsi}/${ANAVINFO_MITM_FN}
#    if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
#        anavinfo=${FIXgsi}/${ANAVINFO_MITM_HOWV_FN}
#        CONVINFO=${FIXgsi}/${CONVINFO_HOWV_FN}
#    fi
# elif [[ $cyc == $cyc_mxtm ]]  ; then
#    anavinfo=${FIXgsi}/${ANAVINFO_MXTM_FN}
#    if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
#        anavinfo=${FIXgsi}/${ANAVINFO_MXTM_HOWV_FN}
#        CONVINFO=${FIXgsi}/${CONVINFO_HOWV_FN}
#    fi
# else
#    anavinfo=${FIXgsi}/anavinfo_arw_netcdf_rtma3d
#    CONVINFO=${FIXgsi}/rtma3d_convinfo_v1.0.txt
#    if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
#        anavinfo=${FIXgsi}/${ANAVINFO_HOWV_FN}
#        CONVINFO=${FIXgsi}/${CONVINFO_HOWV_FN}
#    fi
# fi
#==========================================================================#

# Fixed fields
cpreq $anavinfo anavinfo
cpreq $BERROR   berror_stats
cpreq $SATANGL  satbias_angle
cpreq $SATINFO  satinfo
cpreq $CONVINFO convinfo
cpreq $OZINFO   ozinfo
cpreq $PCPINFO  pcpinfo
cpreq $OBERROR  errtable

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

# option to control the usage of gsdsfc_uselist
# 1: using GSD Surface Obs uselist
# 2: using surface obs uselist generated by EMC Automated QC package (<== default)
  i_gsdsfc_uselist=${i_gsdsfc_uselist:-2}

# Get reject/accept lists derived from automated QC package
found_rjlist=False
max_cycs=168 # Number of cycles to look back
i=1
export PDYprev_dir=${COMOUTautoqc_rtma3d}
while [ ${i} -lt ${max_cycs} ]; do
  export probe_cyc=`${NDATE} -${i} ${YYYYMMDDHH}`
  probe_YYYYMMDD=`echo $probe_cyc | cut -c 1-8`
  export probe_HH=`echo $probe_cyc | cut -c 9-10`
  probe_dir=${COMOUTautoqc_base}/${NET}.${probe_YYYYMMDD}/autoqcprd.t${probe_HH}z # MTM - revert NET to RUN
  if [ -s ${probe_dir}/${RUN}.t${probe_HH}z.accept_merged_${probe_cyc}.txt ]; then
    export PDYprev_dir=${probe_dir}
    found_rjlist=True
    break
  else
    let "i=i+1"
  fi
done
echo "PDYprev_dir = " $PDYprev_dir

if [ $found_rjlist == True ]; then
  cpreq ${PDYprev_dir}/${RUN}.t${probe_HH}z.accept_merged_${probe_cyc}.txt sfcobs_uselist.txt
fi

export sfcwndob_biasc=.true.
if [[ "$sfcwndob_biasc" = ".true." ]]; then
  # Search previous cycles for most recent wind bias information
  found_prevcyc=False
  max_cycs=168 # Number of cycles to look back
  i=1
  export PDYprev_dir=${COMOUTautoqc_rtma3d}
  while [ ${i} -lt ${max_cycs} ]; do
    export probe_cyc=`${NDATE} -${i} ${YYYYMMDDHH}`
    probe_YYYYMMDD=`echo $probe_cyc | cut -c 1-8`
    export probe_HH=`echo $probe_cyc | cut -c 9-10`
    probe_dir=${COMOUTautoqc_base}/${NET}.${probe_YYYYMMDD}/autoqcprd.t${probe_HH}z # MTM - revert NET to RUN
    if [ -s ${probe_dir}/${RUN}.t${probe_HH}z.windbias_${probe_cyc}.txt ]; then
      export PDYprev_dir=${probe_dir}
      found_prevcyc=True
      break
    else
      let "i=i+1"
    fi
  done
  echo "PDYprev_dir = " $PDYprev_dir

  if [ $found_prevcyc == True ]; then
    cpreq ${PDYprev_dir}/${RUN}.t${probe_HH}z.windbias_${probe_cyc}.txt stnwindbiascor
  fi
fi

# Get GSD aircraft reject list, mesonet_uselist, sfcobs_provider
#if [ $cyc = "08" ]; then
#${MV} ${AIRCRAFT_REJECT}/current_bad_aircraft.txt  ${AIRCRAFT_REJECT}/${PDYm1}_bad_aircraft.txt
#scpreq Edward.Colon@dtn-jet.boulder.rdhpcs.noaa.gov:/mnt/lfs4/HFIP/hfv3gfs/Edward.Colon/reject_use_lists/current_bad_aircraft.txt ${AIRCRAFT_REJECT}/
#fi
#if [ $cyc = "12" ]; then
#${MV} ${SFCOBS_USELIST}/current_mesonet_uselist.txt ${SFCOBS_USELIST}/${PDYm1}_mesonet_uselist.txt
#scpreq Edward.Colon@dtn-jet.boulder.rdhpcs.noaa.gov:/mnt/lfs4/HFIP/hfv3gfs/Edward.Colon/reject_use_lists/current_mesonet_uselist.txt ${SFCOBS_USELIST}/
#fi
if [ "${i_gsdsfc_uselist}" -eq 1 ] ; then
   ${ECHO} "Using GSD Surface Obs Uselist -- i_gsdsfc_uselist=${i_gsdsfc_uselist}"
   ${CP} ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft
   ${CP} ${SFCOBS_USELIST}/current_mesonet_uselist.txt gsd_sfcobs_uselist.txt
fi
${CP} ${SFCOBS_PROVIDER}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

bufrtable=${FIXgsi}/prepobs_prep.bufrtable
${CP} $bufrtable ./prepobs_prep.bufrtable

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
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #WCOSS
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

# option for netcdf-format obs diag file
  L_NCDIAG=${L_NCDIAG:-".false."}     # false: no output of netcdf obsdiag, and do not combine them

#====  set GSI namelist options for analysis of HOWV/GUST/VIS ====#
#  setup for howv
  corp_howv0=0.42          # static BE of howv (0.42 is tuned for pure 3DVar, needs to be changed in hyrid run)
  hwllp_howv=170000.0      # static BE de-correlation length scale of howv (if<0, using default preset value in GSI code --> hwllp of q at level 1, which is too short)

#  setup for gust
  oerr_gust=1.0            # Obs Err of gust (if<0, use preset value 1.0 defined in read_prepbufr.f90)
  corp_gust0=3.0           # static BE of gust (if<0, use preset 3.0 defined in gsi code)
  hwllp_gust=170000.0      # static BE de-correlation length scale of gust (if <0, using default preset value in GSI)

#  setup for visibility following 2DRTMA
  pvis=0.2                 # power index used in nonlinear transform
  estvisoe=2.61            # Obs Err of visibility (in transofrmed g-space, not in physical space)
  vis_thres=16000.0        # upper-bound set for visibility (16 km, ~10 miles)
  scale_cv=1.0             # scaling factor used in nonlinear transform
  corp_vis0=3.0            # static BE of vis (in transofrmed g-space, not in physical space)
  hwllp_vis=170000.0       # static BE de-correlation length scale of vis (if <0, using default preset value in GSI)
#  changing the static BE and OE for howv and gust in 3DRTMA hybrid EnVar run
   if [[ "${ifhyb}" == ".false." ]] || [[ "${ifhyb}" == ".FALSE." ]] ; then
      export corp_howv=${corp_howv0}
      export corp_gust=${corp_gust0}
      export corp_vis=${corp_vis0}
   else
      echo "The weight of static error at bottom level for howv/gust/vis is ${StaticWgt}"
      tmpvar=$( echo "scale=4; ${corp_howv0} * sqrt(( 1.0 / ${StaticWgt}))" | bc )
      export corp_howv="${tmpvar}"      #changing static BE of howv in hybrid run
      tmpvar=$( echo "scale=4; ${corp_gust0} * sqrt(( 1.0 / ${StaticWgt}))" | bc )
      export corp_gust="${tmpvar}"      #changing static BE of gust in hybrid run
      tmpvar=$( echo "scale=4; ${corp_vis0} * sqrt(( 1.0 / ${StaticWgt}))" | bc )
      export corp_vis="${tmpvar}"      #changing static BE of vis(ibility) in hybrid run
   fi

# if reading surface roughtness in firstguess
# (used in Similarity theory based height adjustment for wind gust)
  i_sfcrough_fgs=${i_sfcrough_fgs:-1}         # 1(default for 3DRTMA) --> read roughness

# if using height adjustment in surface wind and wind gust analysis
#    note: scheme based on Similarity therory for rtma and 3drtma 
  use_similarity_winghgtadj=".true."          # true  (default): using similarity theory
  neutral_stability_winghgtadj=".false."      # false (default): non-neutral stability

# Running GSI with more print-out information for debugging
# (for operational run, set to .false. for less print-out to reduce wall-clock time)
  export VERBOSE_GSI=${VERBOSE_GSI:-".false."}

# Build the GSI namelist on-the-fly
[[ -f ./gsiparm.anl.sh ]] && rm -f ./gsiparm.anl.sh
[[ -f ./gsiparm.anl ]] && rm -f ./gsiparm.anl

# ${CP} ${PARMgsi}/hrrr_gsiparm.anl.sh gsiparm.anl.sh
if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
  ${CP} ${PARMgsi}/urma3d_gsiparm.anl.sh gsiparm.anl.sh      # with setup for howv (wave height)
else
  ${CP} ${PARMgsi}/rtma3d_gsiparm.anl.sh gsiparm.anl.sh
fi

source ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

#==== set addtional GSI namelist file specicifally for features inherited from (2D)RTMA/URMA
# (namelist file: parmcard_input)
# Running GSI with usage of valley-map data
# (default: true --> 0.25 added to obs usage index for sfc T/Q obs)
  export l_valleygcheck=${l_valleygcheck:-".true."}

# Build namelist for usage of valleymap on-the-fly
[[ -f ./parmcard_input ]] && rm -f ./parmcard_input
cat << EOF > parmcard_input
&parmcardreadprepb
    cgrid="hrrr",
    valleygcheck=${l_valleygcheck},
/
EOF

# Copy terrain, slmask and valley_map data files for usage of valley map 
cp -p ${FIXgsi}/rtma3d_conus_terrain.dat          ./rtma_terrain.dat
cp -p ${FIXgsi}/rtma3d_conus_anl_slmask.dat       ./rtma_slmask.dat
# cp -p ${FIXgsi}/valley_map_hrrr_conus_bin.dat   ./valley_map.dat
  cp -p ${FIXgsi}/valley_map_hrrr_conus_ieee.dat  ./valley_map.dat

# Copy MESONET wind observation sensor height list (same as used in 2DRTMA)
  [[ -f ./provider_windheight ]] && rm ./provider_windheight
  cp -p ${FIXgsi}/rtma3d_conus_provider_windheight  ./provider_windheight

## satellite bias correction
${CP} ${FIXgsi}/rap_satbias_starting_file.txt ./satbias_in
${CP} ${FIXgsi}/rap_satbias_pc_starting_file.txt ./satbias_pc

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

CP_LN=${CP}
${CP_LN} ${EXECrtma3d}/${exefile_name_gsi} ${pgm}
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ];  then
#module purge
#module use /lfs/h2/emc/lam/noscrub/Ming.Hu/rrfs/testD/ufs-srweather-app/env
#source /lfs/h2/emc/lam/noscrub/Ming.Hu/rrfs/testD/ufs-srweather-app/env/build_wcoss2_intel.env
#module list
  export FI_OFI_RXM_SAR_LIMIT=3145728
  export OMP_STACKSIZE=${OMP_STACKSIZE:-"512M"}
# export OMP_PLACES=cores
  export OMP_NUM_THREADS=${OMP_NUM_THREADS:-8}
  APRUN="mpiexec -n 450 -ppn 15 --cpu-bind core --depth 8"

  [[ -f ${DATA}/rtma_gsi ]] && rm ${DATA}/rtma_gsi
  cpreq ${EXECrtma3d}/rtma_gsi ${DATA}
  $APRUN ${DATA}/rtma_gsi < ${DATA}/gsiparm.anl > stdout 2>&1
  export err=$?
# err_chk
fi
# Save some information before err_chk
#   (eg, gsiparm.anl, stdout, obs-fitting, etc.) for debugging if GSI crashed.
${CAT} fort.* >   fits_${cycle_str}.txt
#${LS} -l > GSI_workdir_list
${CAT} stdout >> ${pgmout}
#${MV} ${pgmout} ${pgmout}.var
${CP} -p fits_${cycle_str}.txt ${COMOUTgsi_rtma3d}
${CP} -p gsiparm.anl  gsiparm.anl.var_${cycle_str}
${CP} -p gsiparm.anl.var_${cycle_str}       ${COMOUTgsi_rtma3d}
if [ -f parmcard_input ] ; then
   ${CP} -p parmcard_input  parmcard_input.var_${cycle_str}
   ${CP} -p parmcard_input.var_${cycle_str} ${COMOUTgsi_rtma3d}
fi
${CP} -p stdout       stdout.gsi.var_${cycle_str}
${CP} -p stdout.gsi.var_${cycle_str}        ${COMOUTgsi_rtma3d}
err_chk

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
loops="01 02 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
#  listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"


#  binary format obs diag
   listall_cnv_bin="conv"
   for type in $listall_cnv_bin; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         `${CAT} pe*.${type}_${loop}* > diag_${type}_${string}.${cycle_str}`
      fi
   done
#  netcdf format obs diag
   if [[ "${L_NCDIAG}" == ".true." ]] || [[ "${L_NCDIAG}" == ".TRUE." ]]  ; then
      listall_cnv_nc4="uv t q ps gust vis"
      if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
         listall_cnv_nc4="${listall_cnv_nc4} howv"
      fi
      for type in $listall_cnv_nc4; do
        count=`ls pe*.conv_${type}_${loop}.nc4 | wc -l`
        if [[ $count -gt 0 ]]; then
           find ${DATA} -type f -name "pe*.conv_${type}_${loop}.nc4" -size 1k -delete
           $nc_diag_cat -o diag_${type}_${string}.${cycle_str}.HRRR.nc4 pe*.conv_${type}_${loop}.nc4 
        fi
      done
   fi
done

## link fort files with user-friendly file name
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #wcoss
  ${LN} -sf fort.201    fit_p1.${cycle_str}          # <-- psfc (mb)
  ${LN} -sf fort.202    fit_w1.${cycle_str}          # <-- uv-wind (m/s)
  ${LN} -sf fort.203    fit_t1.${cycle_str}          # <-- temperature (K)
  ${LN} -sf fort.204    fit_q1.${cycle_str}          # <-- q (%)
  ${LN} -sf fort.205    fit_pw1.${cycle_str}         # <-- precip. water (mm)
  ${LN} -sf fort.206    fit_oz1.${cycle_str}         # <-- ozone info (not fitting)
  ${LN} -sf fort.207    fit_rad1.${cycle_str}        # <-- radiance (not fitting)
  ${LN} -sf fort.208    fit_pcp.${cycle_str}         # <-- pcp
  ${LN} -sf fort.213    fit_sst.${cycle_str}         # <-- sst (C)
  ${LN} -sf fort.218    fit_gust.${cycle_str}        # <-- surface wind gust (m/s)
  ${LN} -sf fort.219    fit_vis.${cycle_str}         # <-- surface visibility (m)
  if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
    ${LN} -sf fort.228  fit_howv.${cycle_str}        # <-- significant wave height (m)
  fi
  ${LN} -sf fort.220    minimization_fort220.${cycle_str}
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
  ${ECHO} -e "\n\n@@@@@@@@@ second GSI run standard output\n" >> ${pgmout}
  if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ];  then
    ${MPIRUN} ${pgm} < gsiparm.anl >> ${pgmout} 2>errfile
  fi
  export err=$?
# Saving some information (eg, gsiparm.anl) for debugging before err_chk
  #${LS} -l > GSI_workdir_list
  ${CAT} errfile >> ${pgmout}
  ${ECHO} -e "\n\n -- End of second GSI --\n" >> ${pgmout}
  #${CP} -p ${pgmout} ${COMOUTgsi_rtma3d}/${pgmout}.cloudana #this output should be in $LLOG_PGMOUT
  ${CP} -p gsiparm.anl    gsiparm.anl.cloudana_${cycle_str}
  ${CP} -p gsiparm.anl.cloudana_${cycle_str}  ${COMOUTgsi_rtma3d}
  ${CAT} stdout > stdout.gsi.cloudana_${cycle_str}
  ${ECHO} -e "=======================================================" >> stdout.gsi.cloudana_${cycle_str}
  ${ECHO} -e "errfile:" >> stdout.gsi.cloudana_${cycle_str}
  ${CAT} errfile >> stdout.gsi.cloudana_${cycle_str}
  ${CP} -p stdout.gsi.cloudana_${cycle_str}   ${COMOUTgsi_rtma3d}
  err_chk

fi ###### second GSI run

# Saving ANALYSIS, DIAG, Obs-Fitting files TO COM2 DIRECTORY AS PRODUCT for archive
  ${CP} -p gsiparm.anl  ${COMOUTgsi_rtma3d}/gsiparm.anl_${cycle_str}

#  ---- Each obs diag file is saved to COM2 and compressed by gzip individually ---- #
#       these individually gzip-ed diag files will be used in AutoQC step later.
  ${CP} -p diag_*                              ${COMOUTgsi_rtma3d}
  gzip ${COMOUTgsi_rtma3d}/diag_*

#  ---- All obs diag files are archived in one tarball under COM2 ---- #
#       these diag files in tarball will be used in PRDGEN step later.
  tar -cvf  ${COMOUTgsi_rtma3d}/diag_${cycle_str}.tar diag_*  # *.tar is tarball (no compression)
                                                              # (using more space, but saving time)
# tar -czvf ${COMOUTgsi_rtma3d}/diag_${cycle_str}.tgz diag_*  # *.tgz is compressed tarball
                                                              # (saving space, but costing more time)

if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #wcoss
  ${CP} -p ${DATA}/wrf_inout                   ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
  ${CP} -p minimization_fort220.${cycle_str}   ${COMOUTgsi_rtma3d}
  tar -cvf obsfit_fort220_${cycle_str}.tar     ./fort.* ./fit_* ./stdout*
  ${CP} -p  obsfit_fort220_${cycle_str}.tar    ${COMOUTgsi_rtma3d}
  tar -cvf misc_info_${cycle_str}.tar  ./*info ./errtable ./prepobs_prep.bufrtable  ./*bias*  \
    ./current_bad_aircraft ./gsd_sfcobs_uselist.txt ./gsd_sfcobs_provider.txt ./stdout*
  ${CP} -p  misc_info_${cycle_str}.tar         ${COMOUTgsi_rtma3d}
  ${CP} -p filelist.hrrrdas 		       ${COMOUTgsi_rtma3d}
  ${CP} -p filelist03                          ${COMOUTgsi_rtma3d}
  ${CP} -p hybens_info                         ${COMOUTgsi_rtma3d}
  ${CP} -p stdout                              ${COMOUTgsi_rtma3d}
  ${CP} -p OUTPUT*                             ${COMOUTgsi_rtma3d}
  # extra backup (NOT necessary)
  #${LN} -sf ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME} ${COMOUT}/${ANLrtma3d_FNAME}
  #${CP} -p ${pgmout_stdout}        ${COMOUT}/${pgmout_stdout}_gsianl.${cycle_str}
  #${CP} -p fits_${cycle_str}.txt  ${COMOUT}/fits_${cycle_str}.txt
fi  
#${RM} -f ${DATA}/sig*
#${RM} -f ${DATA}/obs*
#${RM} -f ${DATA}/pe*

msg="JOB $job FOR $NET HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
