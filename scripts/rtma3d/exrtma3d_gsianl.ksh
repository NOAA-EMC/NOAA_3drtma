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
#START_TIME=`${DATE} -d "${PDY} ${cyc} ${SUBH_TIME} minutes"`
START_TIME=`${DATE} -d "${PDY} ${cyc} ${subcyc} minutes"`
if [ ${HRRRDAS_BEC} -eq 0 ]; then
EnsWgt=0.5
else
EnsWgt=0.9
fi
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

# options for ocean wave height (howv) and 10-m wind gust (gust)
#   detecting HOWV and GUST in firstguess
i_found_howv=0
RUN_HOWV="FALSE"
#if [ "$NCDUMP" ] ; then
# i_found_howv=$($NCDUMP -h ./wrf_inout | grep -i "HOWV" | wc -l)  #-->multiple lines are found
  i_found_howv=$(ncdump  -h ./wrf_inout | grep -i " HOWV(" | wc -l) 
  if [[ "${i_found_howv}" -eq 1 ]] ; then       # found unique variable HOWV
    RUN_HOWV="TRUE"
  fi
#fi
i_found_gust=0
RUN_GUST="FALSE"
#if [ "$NCDUMP" ] ; then
# i_found_gust=$($NCDUMP -h ./wrf_inout | grep -i "GUST" | wc -l)  #-->multiple lines are found
  i_found_gust=$(ncdump  -h ./wrf_inout | grep -i " GUST(" | wc -l) 
  if [[ "${i_found_gust}" -eq 1 ]] ; then       # found unique variable GUST
    RUN_GUST="TRUE"
  fi
#fi

# Link to the prepbufr data
if [ -r ${OBS_DIR}/rtma.t${cyc}z.prepbufr.tm00 ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.prepbufr.tm00 ./prepbufr
fi

if [ -r "${OBS_DIR}/rtma3d.t${cyc}z.NSSLRefInGSI.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/rtma3d.t${cyc}z.NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NSSLRefInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/rtma.t${cyc}z.LightningInGSI_bufr.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.LightningInGSI_bufr.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: LightningInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/rtma.t${cyc}z.NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.NASALaRCCloudInGSI.bufr ./larcInGSI
else
  ${ECHO} "Warning: ${OBS_DIR}: NASALaRCCloudInGSI.bufr does not exist!"
fi

if [ -r "${OBS_DIR}/rtma.t${cyc}z.satwnd.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.satwnd.tm00.bufr_d ./satwndbufr
else
  ${ECHO} "Warning: ${OBS_DIR}: satwnd does not exist!"
fi

if [ -r "${OBS_DIR}/rtma.t${cyc}z.nexrad.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.nexrad.tm00.bufr_d ./nexradbufr
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

if [ -r "${OBS_DIR}/rtma.t${cyc}z.satmar.tm00.bufr_d" ]; then
  ${LN} -sf ${OBS_DIR}/rtma.t${cyc}z.satmar.tm00.bufr_d ./satmar
else
  ${ECHO} "Warning: ${OBS_DIR}: satmar does not exist!"
fi

if [ "${envir}" = "lsf" ] || [ "${envir}" = "pbspro" ] && [ ${HRRRDAS_BEC} -eq 0 ] ; then #WCOSS
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
fi


if [ ${HRRRDAS_BEC} -eq 1 ]; then
  ${ECHO} "\$HRRRDAS_BEC=${HRRRDAS_BEC}, so HRRRDAS will be used if available"
  #----------------------------------------------------
  # generate list of HRRRDAS members for ensemble covariances
  # Use 1-hr forecasts from the HRRRDAS cycling
  c=1
  while [[ $c -le 36 ]]; do
   if [ $c -lt 10 ]; then
    cc="0"$c
   else
    cc=$c
   fi
   if [ "${envir}" == "lsf" ] || [ "${envir}" = "pbspro" ]; then #WCOSS
     hrrre_file=${COMINhrrrdas}/hrrrdas_small_d02_${time_1hour_ago}00f01_mem00${cc}
     ${LS} ${COMINhrrrdas}/hrrrdas_small_d02_${time_1hour_ago}00f01_mem00${cc} >> filelist.hrrrdas
   elif [ ${HRRRDAS_SMALL} -eq 1 ]; then
     hrrre_file=${COMINhrrrdas}/${time_1hour_ago}/wrfprd_mem00${cc}/wrfout_small_d02_${time_str2}
   else
     hrrre_file=${COMINhrrrdas}/${time_1hour_ago}/wrfprd_mem00${cc}/wrfout_d02_${time_str2}
   fi
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
fi

# copy the read-in localization file for hybrid envar analysis
  HYBENS_INFO="hybens_info"
  if [[ "${readin_localization}" == ".true." ]] ; then
     cp -p ${PARMgsi}/${HYBENS_INFO}  hybens_info
     
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

anavinfo=${FIXgsi}/anavinfo_arw_netcdf
BERROR=${FIXgsi}/3drtma_berror_stats_hz01
#BERROR=${FIXgsi}/rap_berror_stats_global_RAP_tune
SATANGL=${FIXgsi}/global_satangbias.txt
SATINFO=${FIXgsi}/global_satinfo.txt
CONVINFO=${FIXgsi}/3drtma_convinfo_v0.6.5_updated
OZINFO=${FIXgsi}/global_ozinfo.txt
PCPINFO=${FIXgsi}/global_pcpinfo.txt
OBERROR=${FIXgsi}/3drtma_errtable_smallSFCerr_ascat
#OBERROR=${FIXgsi}/nam_errtable.r3dv

# If doing the analysis of wave height (HOWV) and/or wind gust (GUST) in 3DRTMA
ANAVINFO_HOWVGUST_FN=anavinfo_arw_netcdf_howvgust
ANAVINFO_HOWV_FN=anavinfo_arw_netcdf_howv
ANAVINFO_GUST_FN=anavinfo_arw_netcdf_gust
ANAVINFO_HOWVGUST_MXTM_FN=anavinfo_arw_netcdf_howvgust_mxtm
ANAVINFO_HOWVGUST_MITM_FN=anavinfo_arw_netcdf_howvgust_mitm
CONVINFO_HOWVGUST_FN=3drtma_convinfo_v0.6.5_updated_howvgust
CONVINFO_HOWV_FN=3drtma_convinfo_v0.6.5_updated_howvgust
CONVINFO_GUST_FN=3drtma_convinfo_v0.6.5_updated_howvgust
if [[ "${RUN_HOWV}" == "TRUE" ]] && [[ "${RUN_GUST}" == "TRUE" ]]; then
   anavinfo=${FIXgsi}/${ANAVINFO_HOWVGUST_FN}
   CONVINFO=${FIXgsi}/${CONVINFO_HOWVGUST_FN}
elif [[ "${RUN_HOWV}" == "TRUE" ]] && [[ "${RUN_GUST}" == "FALSE" ]]; then
   anavinfo=${FIXgsi}/${ANAVINFO_HOWV_FN}
   CONVINFO=${FIXgsi}/${CONVINFO_HOWV_FN}
elif [[ "${RUN_HOWV}" == "FALSE" ]] && [[ "${RUN_GUST}" == "TRUE" ]]; then
   anavinfo=${FIXgsi}/${ANAVINFO_GUST_FN}
   CONVINFO=${FIXgsi}/${CONVINFO_GUST_FN}
fi

## The code in GSI for direct analysis of mint & maxt is not ready yet, 
##    so do NOT use the anavinfo file with mint or maxt for now.
# Fixed fields
if [[ $cyc == $cyc_mitm ]]  ; then
#  cpreq $FIXgsi/anavinfo_arw_netcdf_mitm anavinfo
   cpreq $anavinfo anavinfo
elif [[ $cyc == $cyc_mxtm ]]  ; then
#  cpreq $FIXgsi/anavinfo_arw_netcdf_mxtm anavinfo
   cpreq $anavinfo anavinfo
else
   cpreq $anavinfo anavinfo
fi

# cpreq $anavinfo anavinfo
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

# Get aircraft reject list, mesonet_uselist, sfcobs_provider
#if [ $cyc = "08" ]; then
#${MV} ${AIRCRAFT_REJECT}/current_bad_aircraft.txt  ${AIRCRAFT_REJECT}/${PDYm1}_bad_aircraft.txt
#scpreq Edward.Colon@dtn-jet.boulder.rdhpcs.noaa.gov:/mnt/lfs4/HFIP/hfv3gfs/Edward.Colon/reject_use_lists/current_bad_aircraft.txt ${AIRCRAFT_REJECT}/
#fi
#if [ $cyc = "12" ]; then
#${MV} ${SFCOBS_USELIST}/current_mesonet_uselist.txt ${SFCOBS_USELIST}/${PDYm1}_mesonet_uselist.txt
#scpreq Edward.Colon@dtn-jet.boulder.rdhpcs.noaa.gov:/mnt/lfs4/HFIP/hfv3gfs/Edward.Colon/reject_use_lists/current_mesonet_uselist.txt ${SFCOBS_USELIST}/
#fi
#${CP} ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft
#${CP} ${SFCOBS_USELIST}/current_mesonet_uselist.txt gsd_sfcobs_uselist.txt
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

#====  set GSI namelist options for analysis of HOWV and GUST  ====#
  oerr_gust="1.0"                #Obs Err of gust (if<0, use preset value 1.0 defined in read_prepbufr.f90)
  corp_howv0="0.42"        #static BE of howv (0.42 is tuned for pure 3DVar, needs to be changed in hyrid run)
  corp_gust0="3.0"         #static BE of gust (if<0, use preset 3.0 defined in gsi code)
  hwllp_howv="170000.0"           #static BE de-correlation length scale of howv (if<0, using default preset value in GSI code --> hwllp of q at level 1, which is too short)
  hwllp_gust="170000.0"           #static BE de-correlation length scale of gust (if <0, using default preset value in GSI)
#  changing the static BE and OE for howv and gust in 3DRTMA hybrid EnVar run
   if [[ "${ifhyb}" == ".false." ]] || [[ "${ifhyb}" == ".FALSE." ]] ; then
      export corp_howv=${corp_howv0}
      export corp_gust=${corp_gust0}
   else
      echo "The weight of static error at bottom level for howv and gust is ${StaticWgt}"
      tmpvar=$( echo "scale=4; ${corp_howv0} * sqrt(( 1.0 / ${StaticWgt}))" | bc )
      export corp_howv="${tmpvar}"      #changing static BE of howv if hybrid run with readin_local=False
      tmpvar=$( echo "scale=4; ${corp_gust0} * sqrt(( 1.0 / ${StaticWgt}))" | bc )
      export corp_gust="${tmpvar}"      #changing static BE of gust if hybrid run with readin_local=False
   fi

# Build the GSI namelist on-the-fly
# ${CP} ${PARMgsi}/hrrr_gsiparm.anl.sh gsiparm.anl.sh
${CP} ${PARMgsi}/rtma3d_gsiparm.anl.sh gsiparm.anl.sh
source ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

export l_valleygcheck=${l_valleygcheck:-".true."}
cat << EOF > parmcard_input
&parmcardreadprepb
    cgrid="hrrr",
    valleygcheck=${l_valleygcheck},
/
EOF

cp -p ${FIXgsi}/rtma3d_conus_terrain.dat          ./rtma_terrain.dat
cp -p ${FIXgsi}/rtma3d_conus_anl_slmask.dat       ./rtma_slmask.dat
cp -p ${FIXgsi}/valley_map_hrrr_conus_ieee.dat  ./valley_map.dat

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
  APRUN="mpiexec -n 360 -ppn 15 --cpu-bind core --depth 8"
  export FI_OFI_RXM_SAR_LIMIT=3145728
  export OMP_STACKSIZE=${OMP_STACKSIZE:-"512M"}
  export OMP_NUM_THREADS=${OMP_NUM_THREADS:-8}
  rm ${DATA}/rtma_gsi
  cpreq ${EXECrtma3d}/rtma_gsi ${DATA}
  $APRUN ${DATA}/rtma_gsi < ${DATA}/gsiparm.anl > stdout 2>&1
  export err=$?
  err_chk
fi
##save some information for possible debugging before err_chk
${CAT} fort.* >   fits_${cycle_str}.txt
#${LS} -l > GSI_workdir_list
${CAT} stdout >> ${pgmout}
#${MV} ${pgmout} ${pgmout}.var
${CP} -p fits_${cycle_str}.txt ${COMOUTgsi_rtma3d}
#err_chk

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


   listall_cnv_bin="conv"
   for type in $listall_cnv_bin; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         `${CAT} pe*.${type}_${loop}* > diag_${type}_${string}.${cycle_str}`
      fi
   done
   listall_cnv_nc4="uv t q ps"
   if [[ "${RUN_HOWV}" == "TRUE" ]] ; then
      listall_cnv_nc4="${listall_cnv_nc4} howv"
   fi
   if [[ "${RUN_GUST}" == "TRUE" ]] ; then
      listall_cnv_nc4="${listall_cnv_nc4} gust"
   fi
   for type in $listall_cnv_nc4; do
     count=`ls pe*.conv_${type}_${loop}.nc4 | wc -l`
     if [[ $count -gt 0 ]]; then
        find ${DATA} -type f -name "pe*.conv_${type}_${loop}.nc4" -size 1k -delete
        $nc_diag_cat -o diag_${type}_${string}.${cycle_str}.HRRR.nc4 pe*.conv_${type}_${loop}.nc4 
     fi
   done
done

## link fort files with user-friendly file name
if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #wcoss
  ${LN} -sf fort.201    fit_p1.${cycle_str}
  ${LN} -sf fort.202    fit_w1.${cycle_str}
  ${LN} -sf fort.203    fit_t1.${cycle_str}
  ${LN} -sf fort.204    fit_q1.${cycle_str}
  ${LN} -sf fort.207    fit_rad1.${cycle_str}
  ${LN} -sf fort.208    fit_pcp.${cycle_str}
  ${LN} -sf fort.213    fit_sst.${cycle_str}
  if [[ "${RUN_GUST}" == "TRUE" ]] ; then
    ${LN} -sf fort.218    fit_gust.${cycle_str}
  fi
  
  if [[ "${RUN_HOWV}" == "TRUE" ]] ; then
    ${LN} -sf fort.228    fit_howv.${cycle_str}
  fi
  ${LN} -sf fort.220 minimization_fort220.${cycle_str}
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
  #${LS} -l > GSI_workdir_list
  ${CAT} errfile >> ${pgmout}
  ${ECHO} -e "\n\n -- End of second GSI --\n" >> ${pgmout}
  #${CP} -p ${pgmout} ${COMOUTgsi_rtma3d}/${pgmout}.cloudana #this output should be in $LLOG_PGMOUT
  err_chk

fi ###### second GSI run

# Saving ANALYSIS, DIAG, Obs-Fitting files TO COM2 DIRECTORY AS PRODUCT for archive
${CP} -p gsiparm.anl  ${COMOUTgsi_rtma3d}/gsiparm.anl_${cycle_str}
tar -cvf ${COMOUTgsi_rtma3d}/diag_${cycle_str}.tgz diag_*

if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then #wcoss
  ${CP} -p ${DATA}/wrf_inout                  ${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
  ${CP} -p minimization_fort220.${cycle_str} ${COMOUTgsi_rtma3d}
  ${CP} -p diag_*                             ${COMOUTgsi_rtma3d}
  tar -cvf obsfit_fort220.tgz  ./fort.* ./fit_* ./stdout*
  ${CP} -p  obsfit_fort220.tgz                ${COMOUTgsi_rtma3d}
  tar -cvf misc_info.tgz  ./*info ./errtable ./prepobs_prep.bufrtable  ./*bias*  \
    ./current_bad_aircraft ./gsd_sfcobs_uselist.txt ./gsd_sfcobs_provider.txt ./stdout*
  ${CP} -p  misc_info.tgz                      ${COMOUTgsi_rtma3d}
  gzip ${COMOUTgsi_rtma3d}/diag_*
  ${CP} -p filelist.hrrrdas 		       ${COMOUTgsi_rtma3d}
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
