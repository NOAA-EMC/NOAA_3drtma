#!/bin/sh -l
##================================================================#
##PBS -l procs=192
#PBS -l nodes=8:ppn=12
#PBS -l walltime=0:30:00
#PBS -A <your_cpu_account>            # <---- define the cpu account
#PBS -N rtma3d_gsi_test               # <---- define the job name
#PBS -q batch                         # <---- define the queue
#PBS -j oe                            # joined stderr and stdout
#PBS -o <your_log_dir>/${PBS_JOBNAME}_${PBS_JOBID}.oe
#PBS -m a
##================================================================#

# change directory to the working directory of the job
# Use the if clause so that this script stays portable
#
set -x

if [ x$PBS_O_WORKDIR != x ]; then
   cd $PBS_O_WORKDIR
else
   cd .
fi

np=`cat $PBS_NODEFILE | wc -l`
# np=$PBS_NP
GSIPROC=${np}
echo "number of total processes is ${np} $PBS_NP "
export OMP_NUM_THREADS=1

# module load newdefaults
# module list
module load intel
module load impi
module load netcdf
module list

# Set up paths to unix commands
DATE=/bin/date
ECHO=/bin/echo
AWK="/bin/awk --posix"
SED=/bin/sed
WC=/usr/bin/wc
CUT=/bin/cut
MKDIR=/bin/mkdir
CAT=/bin/cat
LS=/bin/ls
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
RM=/bin/rm
TAIL=/usr/bin/tail

CNVGRIB=/apps/cnvgrib/1.2.3/bin/cnvgrib
MPIRUN=mpirun
NCDUMP=ncdump

##########################################################################
#                                                                        # 
#  User Defined variables                                                #
#                                                                        #
##########################################################################
sysname="rtma3d"
SYSNAME=`echo ${sysname} | tr '[:lower:]' '[:upper:]'`
expname="test"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
ANLS_TIME=2018050118         # <-- define analysis date and time (YYYYMMDDHH)
subcyc=00                    # <-- define sub cycle time (minute)
cyc_intvl="60 minutes"       # <-- cycle interval (minute)

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
PREV_CYC_TIME=`${DATE} --date="${START_TIME} -${cyc_intvl} " +"%Y%m%d%H%M"`
NEXT_CYC_TIME=`${DATE} --date="${START_TIME} +${cyc_intvl} " +"%Y%m%d%H%M"`

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

HH_cycm1=`${DATE} +"%H" -d "${PREV_CYC_TIME}"`
HH_cycp1=`${DATE} +"%H" -d "${NEXT_CYC_TIME}"`
#------------------------------------------------------------------#

# Where the data is stored
DATAHOME_OBS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/obs
DATAHOME_FGS=/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/Cases_for_RTMA3D/fgs
OBSDIR=${DATAHOME_OBS}/${YYYYMMDD}
FGSDIR=${DATAHOME_FGS}/${YYYYMMDD}

# Conventional Obs Prepbufr
OBSDIR_prepbufr=${OBSDIR}
# fname_prepbufr=newgblav.${YYYYMMDD}.rap.t${HH}z.prepbufr
fname_prepbufr=rap.t${HH}z.prepbufr.tm00

# Radar obs
OBSDIR_radar=${OBSDIR}
fname_radar=hrrr.t${HH}z.NSSLRefInGSI.bufr

# Lightning obs
OBSDIR_lghtn=${OBSDIR}
fname_lghtn=rtma3d.t${HH}z.LightningInGSI.bufr

# NASA cloud
OBSDIR_cloud=${OBSDIR}
fname_cloud=hrrr.t${HH}z.NASALaRCCloudInGSI.bufr

# Firstguess
FGSDIR_rtma3d=${FGSDIR}
fname_fgs="hrrr.t${HH}z.wrfguess"                # hrrr 1 hour pre-forcast
#fname_fgs="hrrr.t${HH_cycp1}z.wrfguess_rap"     # hrrr restart file based on rap

# where the test is going to run
PDATABASE=/scratch3/NCEPDEV/stmp2/${USER}
TESTROOT=${PDATABASE}/wrkdir_${sysname}/${ANLS_CYC_TIME}
RUNDIR=${TESTROOT}/gsiprd_${expname}

# where the system (exe, static, script, etc.) is placed
SYSROOT=/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev             # <-- modify
RTMA3D_DIR=${SYSROOT}/rtma3d_workflow                              # <-- modify

FIX_DIR=${RTMA3D_DIR}/fix
FIX_GSI=${FIX_DIR}/GSI-fix
FIX_CRTM=${FIX_DIR}/CRTM-fix
OBS_USELIST=${FIX_DIR}/ObsUseList
SFCOBS_USELIST="${OBS_USELIST}/mesonet_uselists"
AIRCRAFT_REJECT="${OBS_USELIST}/amdar_reject_lists"
SFCOBS_PROVIDER="${FIX_GSI}"

fixdir=${FIX_GSI}      # used in GSD old script
CRTMFIX=${FIX_CRTM}    # used in GSD old script

USH_DIR=${RTMA3D_DIR}/ush
PARM_DIR=${RTMA3D_DIR}/parm
SORC_DIR=${RTMA3D_DIR}/sorc
GSI_SORC=${SORC_DIR}/rtma_gsi.fd

BUILD_GSI=${SORC_DIR}/build_gsi
BIN_GSI=${BUILD_GSI}/bin
EXEC_DIR=${RTMA3D_DIR}/exec                    # <-- modify it if need
GSIEXE="gsi.x"                                 # <-- modify it if need 
pgm=${GSIEXE}

#-- Testing the status of some important variables. --#
# Make sure RUNDIR is defined 
if [ ! "${RUNDIR}" ]; then
  ${ECHO} "ERROR: \$RUNDIR is not defined!"
  exit 1
fi
# Check to make sure that running/working directory exists or to make it
# Create the working directory and cd into it
workdir=${RUNDIR}
if [ ! -d ${workdir} ] ; then
  ${RM} -rf ${workdir}
  ${MKDIR} -p ${workdir}
fi
cd ${workdir}

# Make sure FGSDIR_rtma3d is defined and exists
if [ ! "${FGSDIR_rtma3d}" ]; then
  ${ECHO} "ERROR: \$FGSDIR_rtma3d is not defined!"
  exit 1
fi
if [ ! -d "${FGSDIR_rtma3d}" ]; then
  ${ECHO} "ERROR: Firstguess data directory '${FGSDIR_rtma3d}' does not exist!"
  exit 1
fi
# Make sure OBSDIR_prepbufr is defined and exists
if [ ! "${OBSDIR_prepbufr}" ]; then
  ${ECHO} "ERROR: \$OBSDIR_prepbufr is not defined!"
  exit 1
fi
if [ ! -d "${OBSDIR_prepbufr}" ]; then
  ${ECHO} "ERROR: conventional prepbufr data directory '${OBSDIR_prepbufr}' does not exist!"
  exit 1
fi

#  NCEPSNOW

# Check to make sure that fix direcory exists
if [ ! -d ${FIX_GSI} ]; then
  ${ECHO} "ERROR: ${FIX_GSI} does not exist"
  exit 1
fi
if [ ! -d ${FIX_CRTM} ]; then
  ${ECHO} "ERROR: ${FIX_CRTM} does not exist"
  exit 1
fi

cd ${workdir}

# Save a copy of the GSI executable in the workdir
${CP} ${EXEC_DIR}/${GSIEXE}  ./

######################################################
# Bring over background field                        #
# (it's modified by GSI so we can't link to it)      #
######################################################
#   FGSDIR_rtma3d
#   fname_fgs
if [ -r ${FGSDIR_rtma3d}/${fname_fgs} ]; then
  ${ECHO} " Cycled run using ${FGSDIR_rtma3d}/${fname_fgs}"
  ${CP} -p ${FGSDIR_rtma3d}/${fname_fgs}            ./wrf_inout
  ${ECHO} " Cycle ${ANLS_CYC_TIME}: GSI background=${FGSDIR_rtma3d}/${fname_fgs}"

# No background available so abort
else
  ${ECHO} "firstguess ${FGSDIR_rtma3d}/${fname_fgs} does not exist!!"
  ${ECHO} "ERROR: No background file for analysis at ${time_run}!!!!"
  ${ECHO} " Cycle ${ANLS_CYC_TIME}: GSI failed because of no background"
  exit 1
fi

# Snow cover building and trimming currently set to run in the 00z cycle

# Update SST currently set to run in the 01z cycle

######################################################
# Bring over Obs Data                                #
######################################################
# Link to the prepbufr data
#   OBSDIR_prepbufr
#   fname_prepbufr
if [ -r ${OBSDIR_prepbufr}/${fname_prepbufr} ] ; then
  ${LN} -s ${OBSDIR_prepbufr}/${fname_prepbufr}      ./prepbufr
else
  ${ECHO} "Warning: prepbufr file:${OBSDIR_prepbufr}/${fname_prepbufr} does not exist!"
fi

# Link to MRMS MOSAIC Radar Reflecitivity
#   OBSDIR_radar=${OBSDIR}
#   fname_radar=hrrr.t${HH}z.NSSLRefInGSI.bufr
if [ -r ${OBSDIR_radar}/${fname_radar} ] ; then
  ${LN} -s ${OBSDIR_radar}/${fname_radar}            ./refInGSI
else
  ${ECHO} "Warning: radar refInGSI file:${OBSDIR_radar}/${fname_radar} does not exist!"
fi

# Link to Lightning Data
#   OBSDIR_lghtn
#   fname_lghtn
if [ -r ${OBSDIR_lghtn}/${fname_lghtn} ] ; then
  ${LN} -s ${OBSDIR_lghtn}/${fname_lghtn}            ./lghtInGSI
else
  ${ECHO} "Warning: Lightning lghtInGSI file:${OBSDIR_lghtn}/${fname_lghtn} does not exist!"
fi

# Link to NASA LaRC Cloud Data
#   OBSDIR_cloud
#   fname_cloud
if [ -r ${OBSDIR_cloud}/${fname_cloud} ] ; then
  ${LN} -s ${OBSDIR_cloud}/${fname_cloud}            ./lghtInGSI
else
  ${ECHO} "Warning: NASA LaRC Cloud larcInGSI file:${OBSDIR_cloud}/${fname_cloud} does not exist!"
fi

# Link statellite radiance data

# Link the radial velocity data

# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.

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

anavinfo=${FIX_GSI}/anavinfo_arw_netcdf
BERROR=${FIX_GSI}/rap_berror_stats_global_RAP_tune
SATANGL=${FIX_GSI}/global_satangbias.txt
#SATINFO=${FIX_GSI}/global_satinfo.txt
SATINFO=${FIX_GSI}/rap_global_satinfo.txt
CONVINFO=${FIX_GSI}/nam_regional_convinfo_RAP.txt
OZINFO=${FIX_GSI}/global_ozinfo.txt    
PCPINFO=${FIX_GSI}/global_pcpinfo.txt
OBERROR=${FIX_GSI}/nam_errtable.r3dv

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
emiscoef_IRwater=${FIX_CRTM}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${FIX_CRTM}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${FIX_CRTM}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${FIX_CRTM}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${FIX_CRTM}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${FIX_CRTM}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${FIX_CRTM}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${FIX_CRTM}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${FIX_CRTM}/FASTEM5.MWwater.EmisCoeff.bin
aercoef=${FIX_CRTM}/AerosolCoeff.bin
cldcoef=${FIX_CRTM}/CloudCoeff.bin

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
   ln -s ${FIX_CRTM}/${file}.SpcCoeff.bin ./
   ln -s ${FIX_CRTM}/${file}.TauCoeff.bin ./
done

## satellite bias correction
cp ${FIX_GSI}/rap_satbias_starting_file.txt ./satbias_in
cp ${FIX_GSI}/rap_satbias_pc_starting_file.txt ./satbias_pc

#
###### Observation Reject Lists
# Get aircraft reject list
cp ${AIRCRAFT_REJECT}/current_bad_aircraft.txt current_bad_aircraft

# surface observation rejection list
sfcuselists_path=${SFCOBS_USELIST}
sfcuselists=current_mesonet_uselist.txt
#sfcuselists=${YYYY}-${MM}-${DD}_meso_uselist.txt
cp ${sfcuselists_path}/${sfcuselists} gsd_sfcobs_uselist.txt

cp ${SFCOBS_PROVIDER}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt

## Only need this file for single obs test
bufrtable=${FIX_GSI}/prepobs_prep.bufrtable
cp $bufrtable ./prepobs_prep.bufrtable

#
# Set some parameters for use by the GSI executable and to build the namelist
#
export JCAP=${JCAP:-62}
export LEVS=${LEVS:-60}
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

# option for hybrid vertical coordinate (HVC) in WRF-ARW
#    detecting whether the bachground file is from WRF-ARW run with HVC on
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

#
# First Pass of GSI Run:  Variatioanl analysis
#
ndatrap=62
grid_ratio=${GSI_grid_ratio_in_var:-1}
cloudanalysistype=5

# Build the GSI namelist on-the-fly
cp ${USH_DIR}/namelist/gsiparm.anl.sh ./
. ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

# Run GSI
echo "***********************************************************"
echo "  begin gsi analysis for 1st pass: variational analysis"
echo "***********************************************************"

export pgmout_var="./stdout_var"
runline="${MPIRUN} -np $np ${GSIEXE}"
$runline < gsiparm.anl > ${pgmout_var} 2>&1

export error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

ls -l > GSI_workdir_list

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
${CP} fort.201    fit_p1.${YYYYMMDDHH}${mm}
${CP} fort.202    fit_w1.${YYYYMMDDHH}${mm}
${CP} fort.203    fit_t1.${YYYYMMDDHH}${mm}
${CP} fort.204    fit_q1.${YYYYMMDDHH}${mm}
${CP} fort.207    fit_rad1.${YYYYMMDDHH}${mm}
${CP} stdout_var  stdout_var_gsianl.${YYYYMMDDHH}${mm}

#
# Second Pass of GSI run: Cloud Analysis
#

mv gsiparm.anl gsiparm.anl_var
mv sigf03 sigf03_step1
mv siganl sigf03

ndatrap=67
grid_ratio=${GSI_grid_ratio_in_cldanl:-1}
cloudanalysistype=6
ifhyb=.false.

# Build the GSI namelist on-the-fly
cp ${USH_DIR}/namelist/gsiparm.anl.sh ./
. ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

# Run GSI
echo "***********************************************************"
echo "  begin gsi analysis for 2nd pass: cloud analysis"
echo "***********************************************************"

export pgmout_cloud="./stdout_cloud"
runline="${MPIRUN} -np $np ${GSIEXE}"
$runline < gsiparm.anl > ${pgmout_cloud}  2>&1
export error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${GSIEXE} crashed at cloud analysis step.  Exit status=${error}"
  exit ${error}
fi

ls -l > GSI_workdir_list_finalcloud

# COPY ANALYSIS TO COM2 DIRECTORY AS PRODUCT
# ${CP}    ${RUNDIR}/wrf_inout             ${COMIN}/gsianl_wrf_inout_d01_${time_str}

exit 0
