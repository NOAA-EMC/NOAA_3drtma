#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

source /home/rtrr/PARM_EXEC/modulefiles/modulefile.jet.GSI_UPP_WRF

# Set up paths to unix commands
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
MKDIR=/bin/mkdir
CAT=/bin/cat
ECHO=/bin/echo
LS=/bin/ls
CUT=/bin/cut
WC=/usr/bin/wc
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed
TAIL=/usr/bin/tail
CNVGRIB=/apps/cnvgrib/1.2.3/bin/cnvgrib
MPIRUN=mpirun

# Set endian conversion options for use with Intel compilers
## export F_UFMTENDIAN="big;little:10,15,66"
## export F_UFMTENDIAN="big;little:10,13,15,66"
## export GMPIENVVAR=F_UFMTENDIAN
## export MV2_ON_DEMAND_THRESHOLD=256

# Set the path to the gsi executable
GSI=${GSI_ROOT}/HRRR_gsi_hyb

# Set the path to the GSI static files
fixdir=${FIX_ROOT}

# Make sure DATAHOME is defined and exists
if [ ! "${SYSTEM_ID}" ]; then
  ${ECHO} "ERROR: $SYSTEM_ID is not defined!"
  SYSTEM_ID="RAP"
fi

if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi

#  PREPBUFR
if [ ! "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: \$PREPBUFR is not defined!"
  exit 1
fi
if [ ! -d "${PREPBUFR}" ]; then
  ${ECHO} "ERROR: directory '${PREPBUFR}' does not exist!"
  exit 1
fi

#  NCEPSNOW
if [ ! "${NCEPSNOW}" ]; then  ${ECHO} "ERROR: \$NCEPSNOW is not defined!"
  exit 1
fi
if [ ! -d "${NCEPSNOW}" ]; then
  ${ECHO} "ERROR: directory '${NCEPSNOW}' does not exist!"
  exit 1
fi

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

# Check to make sure that STATIC_PATH exists
if [ ! -d ${STATIC_DIR} ]; then
  ${ECHO} "ERROR: ${STATIC_DIR} does not exist"
  exit 1
fi

# Check to make sure that ENKF_FCST exists
if [ ! -d ${ENKF_FCST} ]; then
  ${ECHO} "ERROR: ${ENKF_FCST} does not exist"
  exit 1
fi

# Check to make sure that FULLCYC exists
if [ ! "${FULLCYC}" ]; then
  ${ECHO} "ERROR: FULLCYC '${FULLCYC}' does not exist"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
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

echo "Running system: ${SYSTEM_ID}"
# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

# Create the working directory and cd into it
workdir=${DATAHOME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
# if [ "`stat -f -c %T ${workdir}`" == "lustre" ]; then
#  lfs setstripe --count 8 ${workdir}
# fi
cd ${workdir}

# Define the output log file depending on if this is the full or partial cycle
ifsoilnudge=.true.
if [ ${FULLCYC} -eq 0 ]; then
  logfile=${DATABASE_DIR}/loghistory/HRRR_GSI_HYB_PCYC.log
  ifsoilnudge=.true.
elif [ ${FULLCYC} -eq 1 ]; then
  logfile=${DATABASE_DIR}/loghistory/HRRR_GSI_HYB.log
  ifsoilnudge=.true.
elif [ ${FULLCYC} -eq 2 ]; then
  logfile=${DATABASE_DIR}/loghistory/HRRR_GSI_HYB_early.log
  ifsoilnudge=.true.
else  
  echo "ERROR: Unknown CYCLE ${FULLCYC} definition!"
  exit 1
fi

# Save a copy of the GSI executable in the workdir
${CP} ${GSI} .

# Bring over background field (it's modified by GSI so we can't link to it)
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"

# Look for bqckground from pre-forecast background
if [ -r ${DATAHOME_BK}/wrfout_d01_${time_str} ]; then
  ${ECHO} " Cycled run using ${DATAHOME_BK}/wrfout_d01_${time_str}"
  cp ${DATAHOME_BK}/wrfout_d01_${time_str} ./wrf_inout
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI background=${DATAHOME_BK}/wrfout_d01_${time_str}" >> ${logfile}

# No background available so abort
else
  ${ECHO} "${DATAHOME_BK}/wrfout_d01_${time_str} does not exist!!"
  ${ECHO} "ERROR: No background file for analysis at ${time_run}!!!!"
  ${ECHO} " Cycle ${YYYYMMDDHH}: GSI failed because of no background" >> ${logfile} 
  exit 1
fi

# Update SST currently set to run in the 01z cycle
update_SST='00'

# Compute date & time components for the SST analysis time relative to current analysis time
YYJJJ00000000=`${DATE} +"%y%j00000000" -d "${START_TIME} 1 day ago"`
YYJJJ1200=`${DATE} +"%y%j1200" -d "${START_TIME} 1 day ago"`

if [ ${HH} -eq ${update_SST} ]; then
  echo "Update SST"
  if [ -r "${SST_ROOT}/latest.SST" ]; then
    cp ${SST_ROOT}/latest.SST .
  elif [ -r "${SST_ROOT}/${YYJJJ00000000}" ]; then
    cp ${SST_ROOT}/${YYJJJ00000000} latest.SST
  else
    ${ECHO} "${SST_ROOT} data does not exist!!"
    ${ECHO} "ERROR: No SST update at ${time_str}!!!!"
  fi  
  if [ -r "latest.SST" ]; then
    ${CP} ${STATIC_DIR}/UPP/RTG_SST_landmask.dat ./RTG_SST_landmask.dat
    ${CP} ${STATIC_DIR}/WPS/geo_em.d01.nc ./geo_em.d01.nc
    ${MPIRUN} -np $np ${GSI_ROOT}/process_SST.exe > stdout_sstupdate 2>&1
  else
    ${ECHO} "ERROR: No latest SST file for update at ${time_str}!!!!"
  fi  
else
  ${ECHO} "NOTE: No update for SST at ${time_str}!"
fi

# Link to the prepbufr data in obsproc directory if available
# turn off the link to prepbufr_tamdar because NCEP feed works. Aug 4th, 2017
#if [ -r ${DATAOBSHOME}/prepbufr_tamdar ]; then
#  ${LN} -s ${DATAOBSHOME}/prepbufr_tamdar ./prepbufr
# If obsproc has not executed then look for prepbufr file directly on /public
#else
  # Copy the prepbufr to obs directory so we never do I/O to /public directly
  if [[ ${HH} -ne 00 && ${HH} -ne 12 ]]; then
    if [ -r "${PREPBUFR}_test/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test" ]; then
      ${CP} ${PREPBUFR}_test/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test .
      ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test ./prepbufr
    else
      if [ -r "${PREPBUFR}/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}" ]; then
        ${ECHO} "Warning: ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test does not exist!"
        ${CP} ${PREPBUFR}/${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} .
        ${LN} -s ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} ./prepbufr
      else
        ${ECHO} "Warning: ${YYYYJJJHH00}.rap.t${HH}z.prepbufr.tm00.${YYYYMMDD} does not exist!"
      fi
    fi
  else
    if [[ ${HH} -eq 00 || ${HH} -eq 12 ]]; then
      if [ -r "${PREPBUFR}_test/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test" ]; then
        ${CP} ${PREPBUFR}_test/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test .
        ${LN} -s ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}.test ./prepbufr
      else
        if [ -r "${PREPBUFR}/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD}" ]; then
          ${CP} ${PREPBUFR}/${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} .
          ${LN} -s ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} ./prepbufr
        else
          ${ECHO} "Warning: ${YYYYJJJHH00}.rap_e.t${HH}z.prepbufr.tm00.${YYYYMMDD} does not exist!"
        fi
      fi
    else
      ${ECHO} "ERROR: EARLY ${EARLY} is not defined or invalid"
    fi
  fi
#fi

if [ -r "${DATAOBSHOME}/prepbufr_vsesondes" ]; then
   ${CP} ${DATAOBSHOME}/prepbufr_vsesondes .
   ${RM} ./prepbufr
   ${LN} -s prepbufr_vsesondes ./prepbufr
else
   ${ECHO} "Warning: ${DATAOBSHOME}/prepbufr_vsesondes does not exist!"
fi
if [ -r "${DATAOBSHOME}/prepbufr_clamps" ]; then
   ${CP} ${DATAOBSHOME}/prepbufr_clamps .
   ${RM} ./prepbufr
   ${LN} -s prepbufr_clamps ./prepbufr
else
   ${ECHO} "Warning: ${DATAOBSHOME}/prepbufr_clamps does not exist!"
fi
if [ -r "${DATAOBSHOME}/prepbufr_sticknet" ]; then
   ${CP} ${DATAOBSHOME}/prepbufr_sticknet .
   ${RM} ./prepbufr
   ${LN} -s prepbufr_sticknet ./prepbufr
else
   ${ECHO} "Warning: ${DATAOBSHOME}/prepbufr_sticknet does not exist!"
fi

if [ -r "${DATAOBSHOME}/NSSLRefInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NSSLRefInGSI.bufr ./refInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/NSSLRefInGSI.bufr does not exist!"
fi

if [ -r "${DATAOBSHOME}/LightningInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/60/LightningInGSI.bufr ./lghtInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/LightningInGSI.bufr does not exist!"
fi

if [ -r "${DATAOBSHOME}/NASALaRCCloudInGSI.bufr" ]; then
  ${LN} -s ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr ./larcInGSI
else
  ${ECHO} "Warning: ${DATAOBSHOME}/NASALaRCCloudInGSI.bufr does not exist!"
fi

# Link statellite radiance data
# if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamua" ]; then
#  ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamua ./amsuabufr
# else
#   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamua does not exist!"
# fi
# if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamub" ]; then
#   ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamub ./amsubbufr
# else
#   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bamub does not exist!"
# fi
# if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs3" ]; then
#   ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs3 ./hirs3bufr
# else
#   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs3 does not exist!"
# fi
# if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs4" ]; then
#   ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs4 ./hirs4bufr
# else
#   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bhrs4 does not exist!"
# fi
# if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bmhs" ]; then
#   ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bmhs ./mhsbufr
# else
#   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.1bmhs does not exist!"
# fi

# Link the radial velocity data

if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.radwnd" ]; then
  ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.radwnd ./radarbufr
else
  ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.radwnd does not exist!"
fi
if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.nexrad" ]; then
  ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.nexrad ./l2rwbufr
else
  ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.nexrad does not exist!"
fi

# Link the AMV data
if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.satwnd" ]; then
  ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.satwnd ./satwndbufr
else
  ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.rap.t${HH}z.satwnd does not exist!"
fi

# Link the TC vital data
if [ -r "${DATAOBSHOME}/newgblav.${YYYYMMDD}.tcvitals.t${HH}z" ]; then
   ${LN} -s ${DATAOBSHOME}/newgblav.${YYYYMMDD}.tcvitals.t${HH}z ./tcvitl
else
   ${ECHO} "Warning: ${DATAOBSHOME}/newgblav.${YYYYMMDD}.tcvitals.t${HH}z does not exist!"
fi

## 
## Find closest GFS EnKF forecast to analysis time
##
# Make a list of the latest GFS EnKF ensemble
## 
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

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
nummem=`more filelist03 | wc -l`
nummem=$((nummem - 3 ))
if [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with ${memname}"
  beta1_inv=0.15
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
SATINFO=${fixdir}/global_satinfo.txt
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
CRTMFIX=${fixdir}/CRTM_Coefficients
emiscoef_IRwater=${CRTMFIX}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${CRTMFIX}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${CRTMFIX}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${CRTMFIX}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${CRTMFIX}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${CRTMFIX}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${CRTMFIX}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${CRTMFIX}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${CRTMFIX}/FASTEM6.MWwater.EmisCoeff.bin
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
ln -s $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do 
   ln -s ${CRTMFIX}/${file}.SpcCoeff.bin ./
   ln -s ${CRTMFIX}/${file}.TauCoeff.bin ./
done

# Get aircraft reject list
cp ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft

sfcuselists=current_mesonet_uselist.txt
#sfcuselists=${YYYY}-${MM}-${DD}_meso_uselist.txt
sfcuselists_path=${SFCOBS_USELIST}
cp ${sfcuselists_path}/${sfcuselists} gsd_sfcobs_uselist.txt

cp ${fixdir}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

# Only need this file for single obs test
bufrtable=${fixdir}/prepobs_prep.bufrtable
cp $bufrtable ./prepobs_prep.bufrtable

# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=62
export LEVS=60
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}
ndatrap=62
grid_ratio=1 #4
grid_ratio_ens=12 #ensemble resolution=3 * grid_ratio * grid_ratio_ens
cloudanalysistype=1 #5
ens_h=40 #110
ens_v=3 #3

# Build the GSI namelist on-the-fly
. ${fixdir}/gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

## satellite bias correction
cp ${fixdir}/rap_satbias_starting_file.txt ./satbias_in
cp ${fixdir}/rap_satbias_pc_starting_file.txt ./satbias_pc

# Run GSI
${MPIRUN} -np $np ${GSI} < gsiparm.anl > stdout 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${GSI} crashed  Exit status=${error}"
  cp stdout ../.
  exit ${error}
fi

ls -l > GSI_workdir_list

# Look for successful completion messages in rsl files
nsuccess=`${TAIL} -20 stdout | ${AWK} '/PROGRAM GSI_ANL HAS ENDED/' | ${WC} -l`
ntotal=1 
${ECHO} "Found ${nsuccess} of ${ntotal} completion messages"
if [ ${nsuccess} -ne ${ntotal} ]; then
   ${ECHO} "ERROR: ${GSI} did not complete sucessfully  Exit status=${error}"
   cp stdout ../.
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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         `cat pe*.${type}_${loop}* > diag_${type}_${string}.${YYYYMMDDHH}`
      fi
   done
done

cat fort.* > ${DATABASE_DIR}/log/fits_${YYYYMMDDHH}.txt

exit 0
