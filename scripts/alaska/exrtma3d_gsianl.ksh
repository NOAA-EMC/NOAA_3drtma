#!/bin/ksh 
set -x

YYYYMMDDHH=${CDATE}
time_1hour_ago=`$NDATE -1 $YYYYMMDDHH`
ifsoilnudge=.true.

if [ -r ${COMIN}/${RUN}ak.t${cyc}z.firstguess.nc ]; then
  cpfs ${COMIN}/${RUN}ak.t${cyc}z.firstguess.nc ./wrf_inout
  echo " Cycle ${CDATE}: GSI background=${COMIN}/${RUN}ak.t${cyc}z.firstguess.nc"
else
  # No background available so abort
  err_exit "FATAL ERROR: No background file for analysis at ${CDATE}!!!!"
fi

#  Detecting the existence of Ocean Significant Wave Height (HOWV) in firstguess
i_found_howv=0
RUN_HOWV="No"
i_found_howv=$(ncdump  -h ./wrf_inout | grep -i " HOWV(" | wc -l) 
if [[ "${i_found_howv}" -eq 1 ]] ; then       # found unique variable HOWV
  RUN_HOWV="Yes"
else
  RUN_HOWV="No"
fi

if [ -s ${COMINobsproc}/${RUN}.t${cyc}z.prepbufr.tm00 ]; then
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.prepbufr.tm00 ./prepbufr
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.prepbufr.tm00 ${COMOUT}/${RUN}.t${cyc}z.prepbufr.tm00
else
  err_exit "prepbufr file $COMINobsproc/${NET}.t${cyc}z.prepbufr.tm00 not found"
fi

if [ -s ${COMIN}/${RUN}ak.t${cyc}z.NSSLRefInGSI.bufr ]; then
  cpreq -p ${COMIN}/${RUN}ak.t${cyc}z.NSSLRefInGSI.bufr ./refInGSI
else
  echo "WARNING: ${RUN}ak.t${cyc}z.NSSLRefInGSI.bufr is not available ..."
fi

if [ -s ${COMIN}/${RUN}ak.t${cyc}z.LightningInGSI_bufr.bufr ]; then
  cpreq -p ${COMIN}/${RUN}ak.t${cyc}z.LightningInGSI_bufr.bufr ./lghtInGSI
else
  echo "WARNING: ${RUN}ak.t${cyc}z.LightningInGSI_bufr.bufr is not available ..."
fi

if [ -s ${COMIN}/${RUN}ak.t${cyc}z.NASALaRCCloudInGSI.bufr ]; then
  cpreq -p ${COMIN}/${RUN}ak.t${cyc}z.NASALaRCCloudInGSI.bufr ./larcInGSI
else
  echo "WARNING: ${RUN}ak.t${cyc}z.NASALaRCCloudInGSI.bufr is not available ..."
fi

if [ -s ${COMINobsproc}/${RUN}.t${cyc}z.satwnd.tm00.bufr_d ]; then
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.satwnd.tm00.bufr_d ./satwndbufr
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.satwnd.tm00.bufr_d ${COMOUT}
else
  echo "WARNING: ${RUN}.t${cyc}z.satwnd.tm00.bufr_d is not available ..."
fi

if [ -s ${COMINobsproc}/${RUN}.t${cyc}z.nexrad.tm00.bufr_d ]; then
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.nexrad.tm00.bufr_d ./l2rwbufr
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.nexrad.tm00.bufr_d ${COMOUT}
else
  echo "WARNING: ${RUN}.t${cyc}z.nexrad.tm00.bufr_d is not available ..."
fi

if [ -s ${COMINobsproc}/${RUN}.t${cyc}z.satmar.tm00.bufr_d ]; then
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.satmar.tm00.bufr_d ./satmar
  cpreq -p ${COMINobsproc}/${RUN}.t${cyc}z.satmar.tm00.bufr_d ${COMOUT}
else
  echo "WARNING: ${RUN}.t${cyc}z.satmar.tm00.bufr_d is not available ..."
fi

## 
## Find closest GFS EnKF forecast to analysis time
##

found_ens=no
found_backup=no
ic=0

while [ $ic -le 24 ]; do
  ensFHH=$ic
  ensFHH=`printf %03d $ensFHH`
  ensFHH_backup="009"
  ensCYCLE=`$NDATE -$ensFHH $CDATE`
  ensPDY=`echo $ensCYCLE |cut -c1-8`
  ensCC=`echo $ensCYCLE |cut -c9-10`

  set -A probe_ens_nc "${COMINgfs}/enkfgdas.${ensPDY}/${ensCC}/atmos/mem080/gdas.t${ensCC}z.atmf${ensFHH}.nc"
  set -A probe_ens_nc_backup "${COMINgfs}/enkfgdas.${ensPDY}/${ensCC}/atmos/mem080/gdas.t${ensCC}z.atmf${ensFHH_backup}.nc"

  if [ -s "${probe_ens_nc}" ]; then
    ls ${COMINgfs}/enkfgdas.${ensPDY}/${ensCC}/atmos/mem???/gdas.t${ensCC}z.atmf${ensFHH}.nc > filelist03
    found_ens=yes
    break
  elif [ -s "${probe_ens_nc_backup}" -a $found_backup == "no" ]; then
    ls ${COMINgfs}/enkfgdas.${ensPDY}/${ensCC}/atmos/mem???/gdas.t${ensCC}z.atmf${ensFHH_backup}.nc > filelist03_backup
    found_backup=yes
    break
  fi

  let "ic=ic+1"

done
echo "ic="$ic, "found_ens="$found_ens, "found_backup="$found_backup

if [ -e filelist03_backup ]; then
  cp filelist03_backup filelist03
fi

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
readin_localization=.true.
nummem=`more filelist03 | wc -l`
nummem=$((nummem - 3 ))
if [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with GDAS directly"
  EnsWgt=0.5
  beta1_inv=$(( 1 - $EnsWgt  ))
  ifhyb=.true.
  regional_ensemble_option=1
  grid_ratio_ens=3 #ensemble resolution=3 * grid_ratio * grid_ratio_ens
  i_en_perts_io=0
  ens_fast_read=.false. 
  grid_ratio=1
  cloudanalysistype=1
  ens_h=20 #40 #110
  ens_v=1 #3
  echo " Cycle ${YYYYMMDDHH}: GSI hybrid uses GDAS directly with n_ens=${nummem}"
else
  beta1_inv=1.0
  ifhyb=.false.
  regional_ensemble_option=1
  grid_ratio_ens=1
  i_en_perts_io=0
  ens_fast_read=.false.
  readin_localization=.false.
  echo " Cycle ${YYYYMMDDHH}: GSI running pure 3DVar without ensenble covariances."
fi

# copy the read-in localization file for hybrid envar analysis
if [[ "${readin_localization}" == ".true." ]] ; then
   cpreq ${PARMrtma3d}/${RUN}ak_hybens_info hybens_info     
   # read in the weight for static background error at the surface level in hybrid envar run
   # the weight would be used to adjust the background error for howv/gust/vis
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
   done < ./hybens_info
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

anavinfo=${FIXrtma3d}/${RUN}ak_anavinfo_arw_netcdf

# Setting if (as default) using the berror file in which the De-correlation Length Scales (DLS) 
#     had been tuned and hard-wired to 1/8 of original values from the original berror of HRRRDAS 
BERROR=${FIXrtma3d}/${RUN}ak_berror_stats_hz01_DLS_TUNED_to_8th

SATANGL=${FIXrtma3d}/${RUN}ak_global_satangbias.txt
SATINFO=${FIXrtma3d}/${RUN}ak_global_satinfo.txt
CONVINFO=${FIXrtma3d}/${RUN}ak_convinfo_v1.0.txt
OZINFO=${FIXrtma3d}/${RUN}ak_global_ozinfo.txt
PCPINFO=${FIXrtma3d}/${RUN}ak_global_pcpinfo.txt
OBERROR=${FIXrtma3d}/${RUN}ak_errtable_smallSFCerr_ascat

#==========================================================================#
# If doing the analysis of wave height (HOWV) in 3DRTMA
if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
   anavinfo=${FIXrtma3d}/${RUN}ak_anavinfo_arw_howv_netcdf
   CONVINFO=${FIXrtma3d}/${RUN}ak_convinfo_v1.0_howv.txt
fi
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

# Matt please revise with your backup accept list changes
# COMOUTautoqc_rtma3d needs to be changed to COMIN
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
  probe_dir=${COMOUTautoqc_base}/${RUN}.${probe_YYYYMMDD}/${dom}/autoqcprd.t${probe_HH}z
  if [ -s ${probe_dir}/${RUN}ak.t${probe_HH}z.accept_merged.txt ]; then
    export PDYprev_dir=${probe_dir}
    found_rjlist=True
    break
  else
    let "i=i+1"
  fi
done
echo "PDYprev_dir = " $PDYprev_dir

if [ $found_rjlist == True ]; then
  cpreq ${PDYprev_dir}/${RUN}ak.t${probe_HH}z.accept_merged.txt sfcobs_uselist.txt
fi

# Get aircraft reject list derived from automated QC package
found_rjlist=False
max_cycs=168 # Number of cycles to look back
i=1
export PDYprev_dir=${COMOUTautoqc_rtma3d}
while [ ${i} -lt ${max_cycs} ]; do
  export probe_cyc=`${NDATE} -${i} ${YYYYMMDDHH}`
  probe_YYYYMMDD=`echo $probe_cyc | cut -c 1-8`
  export probe_HH=`echo $probe_cyc | cut -c 9-10`
  probe_dir=${COMOUTautoqc_base}/${RUN}.${probe_YYYYMMDD}/${dom}/autoqcprd.t${probe_HH}z
  if [ -s ${probe_dir}/${RUN}ak.t${probe_HH}z.aircraft_rjs_merged.txt ]; then
    export PDYprev_dir=${probe_dir}
    found_rjlist=True
    break
  else
    let "i=i+1"
  fi
done
echo "PDYprev_dir = " $PDYprev_dir

if [ $found_rjlist == True ]; then
  cpreq ${PDYprev_dir}/${RUN}ak.t${probe_HH}z.aircraft_rjs_merged.txt current_bad_aircraft
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
    probe_dir=${COMOUTautoqc_base}/${RUN}.${probe_YYYYMMDD}/${dom}/autoqcprd.t${probe_HH}z
    if [ -s ${probe_dir}/${RUN}ak.t${probe_HH}z.windbias.txt ]; then
      export PDYprev_dir=${probe_dir}
      found_prevcyc=True
      break
    else
      let "i=i+1"
    fi
  done
  echo "PDYprev_dir = " $PDYprev_dir

  if [ $found_prevcyc == True ]; then
    cpreq ${PDYprev_dir}/${RUN}ak.t${probe_HH}z.windbias.txt stnwindbiascor
  fi
fi

# Matt please revise with your backup accept list changes.
if [ "${i_gsdsfc_uselist}" -eq 1 ] ; then
   echo "Using GSD Surface Obs Uselist -- i_gsdsfc_uselist=${i_gsdsfc_uselist}"
   cp ${FIXrtma3d}/${RUN}ak_aircraft_reject_list.txt current_bad_aircraft
   cp ${FIXrtma3d}/${RUN}ak_mesonet_uselist.txt gsd_sfcobs_uselist.txt
fi
cp ${FIXrtma3d}/${RUN}ak_gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

# Only need this file for single obs test
bufrtable=${PARMrtma3d}/${RUN}ak_prepobs_prep.bufrtable
cp $bufrtable ./prepobs_prep.bufrtable

# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=${JCAP:-62}
export LEVS=${LEVS:-60}
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

# option for netcdf-format obs diag file
RUN_NCDIAG=${RUN_NCDIAG:-"Yes"} # netcdf format obs-diag file (default: Yes)
if [[ ${RUN_NCDIAG} =~ [TtYy] ]] ; then
   L_NCDIAG=".true."            # gsi dumps out netcdf obsdiag, run ncdiag to combine them
else
   L_NCDIAG=".false."
fi

#====  set GSI namelist options for analysis of HOWV/GUST/VIS ====#
#  setup for howv
corp_howv0=0.42          # static BE of howv (0.42 is tuned for pure 3DVar, needs to be changed in hyrid run)
hwllp_howv=100000.0      # static BE de-correlation length scale of howv (if<0, using default preset value

#  setup for gust
oerr_gust=1.0            # Obs Err of gust (if<0, use preset value 1.0 defined in read_prepbufr.f90)
corp_gust0=3.0           # static BE of gust (if<0, use preset 3.0 defined in gsi code)
hwllp_gust=100000.0      # static BE de-correlation length scale of gust (if <0, using default preset value in GSI)

#  setup for visibility following 2DRTMA
pvis=0.2                 # power index used in nonlinear transform
estvisoe=2.61            # Obs Err of visibility (in transofrmed g-space, not in physical space)
vis_thres=16000.0        # upper-bound set for visibility (16 km, ~10 miles)
scale_cv=1.0             # scaling factor used in nonlinear transform
corp_vis0=3.0            # static BE of vis (in transofrmed g-space, not in physical space)
hwllp_vis=100000.0       # static BE de-correlation length scale of vis (if <0, using default preset value in GSI)

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

# Setting for the factors applied to horizontal and vertical de-correlation length scales (DLS) in berror
# if using the berror file in which the De-correlation Length Scales (DLS) had been tuned
#    to 1/8 of the values in original berror file of RAP/HRRR, then using the 
#    same values of hzscl & vs as used in RAP/HRRR
vs=1.0                      # used in RAP/HRRR
hzscl1=0.373                # used in RAP/HRRR
hzscl2=0.746                # used in RAP/HRRR
hzscl3=1.500                # used in RAP/HRRR

if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
  ${CP} ${PARMrtma3d}/${RUN}ak_gsiparm.anl_howv.sh gsiparm.anl.sh      # with setup for howv (wave height)
else
  ${CP} ${PARMrtma3d}/${RUN}ak_gsiparm.anl.sh gsiparm.anl.sh
fi

source ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

# Run GSI with the usage of valley-map data
# Build namelist for usage of valleymap on-the-fly

cat << EOF > parmcard_input
&parmcardreadprepb
    cgrid="akhrrr",
    valleygcheck=.true.,
/
EOF

# Copy terrain, slmask and valley_map data files for usage of valley map 
cpreq -p ${FIXrtma3d}/${RUN}ak_alaska_terrain.dat        ./rtma_terrain.dat
cpreq -p ${FIXrtma3d}/${RUN}ak_alaska_anl_slmask.dat     ./rtma_slmask.dat
cpreq -p ${FIXrtma3d}/${RUN}ak_valley_map_akhrrr_ieee.dat     ./valley_map.dat

# Copy MESONET wind observation sensor height list (same as used in 2DRTMA)
cpreq -p ${FIXrtma3d}/${RUN}ak_alaska_provider_windheight  ./provider_windheight

## satellite bias correction
cpreq ${FIXrtma3d}/${RUN}ak_rap_satbias_starting_file.txt ./satbias_in
cpreq ${FIXrtma3d}/${RUN}ak_rap_satbias_pc_starting_file.txt ./satbias_pc

# Run GSI
export pgm="${NET}_gsi"
. prep_step
startmsg

export FI_OFI_RXM_SAR_LIMIT=3145728
export OMP_STACKSIZE=${OMP_STACKSIZE:-"512M"}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-4}
APRUN="mpiexec -n $ntasks -ppn $ppn --cpu-bind core --depth $threads"
 
$APRUN ${EXECrtma3d}/${pgm} < ${DATA}/gsiparm.anl >>$pgmout 2>errfile
export err=$?; err_chk

loops="01 02 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
#  binary format obs diag
   listall_cnv_bin="conv"
   for type in $listall_cnv_bin; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat pe*.${type}_${loop}* > diag_${type}_${string}
         gzip diag_${type}_${string}
         cpreq diag_${type}_${string}.gz $COMOUT/${RUN}ak.t${cyc}z.diag_${type}_${string}.gz
         chgrp rstprod $COMOUT/${RUN}ak.t${cyc}z.diag_${type}_${string}.gz
      fi
   done
done

mv fort.201    fit_p1         # <-- psfc (mb)
mv fort.202    fit_w1         # <-- uv-wind (m/s)
mv fort.203    fit_t1         # <-- temperature (K)
mv fort.204    fit_q1         # <-- q (%)
mv fort.205    fit_pw1        # <-- precip. water (mm)
mv fort.206    fit_oz1         # <-- ozone info (not fitting)
mv fort.207    fit_rad1        # <-- radiance (not fitting)
mv fort.208    fit_pcp         # <-- pcp
mv fort.209    fit_rw1         # <-- rw (radar radial wind)
mv fort.213    fit_sst         # <-- sst (C)
mv fort.218    fit_gust        # <-- surface wind gust (m/s)
mv fort.219    fit_vis         # <-- surface visibility (m)

if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
  mv fort.228  fit_howv        # <-- significant wave height (m)
  cat fit_p1 fit_w1 fit_t1 fit_q1 fit_pw1 fit_rad1 fit_rw1 fit_sst fit_gust fit_vis fit_howv > ${COMOUT}/${RUN}ak.t${cyc}z.fits
else
  cat fit_p1 fit_w1 fit_t1 fit_q1 fit_pw1 fit_rad1 fit_rw1 fit_sst fit_gust fit_vis > ${COMOUT}/${RUN}ak.t${cyc}z.fits
fi

# Saving ANALYSIS, DIAG, Obs-Fitting files TO COM DIRECTORY AS PRODUCT for archive
cpreq -p gsiparm.anl ${COMOUT}/${RUN}ak.t${cyc}z.gsiparm.anl

mv ${DATA}/wrf_inout                   ${DATA_SHARED}/wrf_inout # copying big file takes time

tar -cvf misc_info_${CDATE}.tar  ./*info ./errtable ./*bias*  \
    ./current_bad_aircraft ./*sfcobs_uselist* ./gsd_sfcobs_provider.txt ./filelist* \
    ./OUTPUT*
cpreq -p misc_info_${CDATE}.tar          ${COMOUT}/${RUN}ak.t${cyc}z.misc_info.tar

cpreq -p fort.220   ${COMOUT}/${RUN}ak.t${cyc}z.minimization_info
cpreq -p filelist03                          ${COMOUT}/${RUN}ak.t${cyc}z.filelist03

#   Copy the split obs-diag files (nc4) to shared directory (for the follow-up ncdiag step)
mv pe*.nc4 ${DATA_SHARED}

postmsg "$0 of $job completed normally"

