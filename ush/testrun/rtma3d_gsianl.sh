#!/bin/sh -l
##================================================================#
#PBS -l nodes=8:ppn=12
#PBS -l walltime=0:30:00
#PBS -A da-cpu                        # <---- define the cpu account
#PBS -N rtma3d_gsi_test               # <---- define the job name
#PBS -q debug                         # <---- define the queue
#PBS -j oe                            # joined stderr and stdout
###### user must specify the following line to define the path for log file
#PBS -o /scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/rtma3d_workflow/dev/log/${PBS_JOBNAME}_${PBS_JOBID}.oe
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
export nummem=0                   # ensemble size

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
# cp ${USH_DIR}/namelist/gsiparm.anl.sh ./
# . ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   qoption=2,
   gencode=78,factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.flase.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.true.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,emiss_bc=.true.,
   diag_precon=.true.,step_start=1.e-3,
   l4densvar=.false.,nhr_obsbin=3,
   verbose=.true.,
 /     
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
   grid_ratio_wrfmass=${grid_ratio},
   wrf_mass_hybridcord=${hybridcord},
 /
 &BKGERR
   vs=1.0,
   hzscl=0.373,0.746,1.5,
   bw=0.,fstat=.true.,
/
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=1.5,time_window_rad=1.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null      ps                   1.0     0     0
   prepbufr       t           null      t                    1.0     0     0
   prepbufr       q           null      q                    1.0     0     0
   prepbufr       pw          null      pw                   1.0     0     0
   satwndbufr     uv          null      uv                   1.0     0     0
   prepbufr       uv          null      uv                   1.0     0     0
   prepbufr       spd         null      spd                  1.0     0     0
   prepbufr       dw          null      dw                   1.0     0     0
   radarbufr      rw          null      rw                   1.0     0     0
   prepbufr       sst         null      sst                  1.0     0     0
   gpsrobufr      gps_ref     null      gps                  1.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             1.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              1.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            1.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            1.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            1.0     0     0
   hirs3bufr      hirs3       n16       hirs3_n16            0.0     1     0
   hirs3bufr      hirs3       n17       hirs3_n17            6.0     1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        6.0     2     0
   hirs4bufr      hirs4       n18       hirs4_n18            0.0     1     0
   hirs4bufr      hirs4       n19       hirs4_n19            1.0     2     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        1.0     1     0
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  20.0     2     0
   amsuabufr      amsua       n15       amsua_n15           10.0     2     0
   amsuabufr      amsua       n18       amsua_n18           10.0     2     0
   amsuabufr      amsua       n19       amsua_n19           10.0     2     0
   amsuabufr      amsua       metop-a   amsua_metop-a       10.0     2     0
   amsuabufr      amsua       metop-b   amsua_metop-b       10.0     2     0
   airsbufr       amsua       aqua      amsua_aqua           5.0     2     0
   amsubbufr      amsub       n17       amsub_n17            1.0     1     0
   mhsbufr        mhs         n18       mhs_n18              3.0     2     0
   mhsbufr        mhs         n19       mhs_n19              3.0     2     0
   mhsbufr        mhs         metop-a   mhs_metop-a          3.0     2     0
   mhsbufr        mhs         metop-b   mhs_metop-b          3.0     2     0
   ssmitbufr      ssmi        f13       ssmi_f13             0.0     2     0
   ssmitbufr      ssmi        f14       ssmi_f14             0.0     2     0
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     2     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     2     0
   ssmisbufr      ssmis_las   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_img   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_env   f16       ssmis_f16            0.0     2     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           1.5     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           1.5     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           1.5     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           1.5     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           1.5     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           1.5     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           1.5     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           1.5     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           1.5     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           1.5     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           1.5     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           1.5     1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15           1.5     2     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15           1.5     2     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15           1.5     2     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15           1.5     2     0
   iasibufr       iasi        metop-a   iasi616_metop-a     20.0     1     0
   gomebufr       gome        metop-a   gome_metop-a         1.0     2     0
   omibufr        omi         aura      omi_aura             1.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            1.0     0     0
   tcvitl         tcp         null      tcp                  1.0     0     0
   seviribufr     seviri      m08       seviri_m08           1.0     1     0
   seviribufr     seviri      m09       seviri_m09           1.0     1     0
   seviribufr     seviri      m10       seviri_m10           1.0     1     0
   iasibufr       iasi        metop-b   iasi616_metop-b      0.0     1     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     1     0
   crisbufr       cris        npp       cris_npp             0.0     1     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
   prepbufr       mta_cld     null      mta_cld              1.0     0     0
   prepbufr       gos_ctp     null      gos_ctp              1.0     0     0
   refInGSI       rad_ref     null      rad_ref              1.0     0     0
   lghtInGSI      lghtn       null      lghtn                1.0     0     0
   larcInGSI      larccld     null      larccld              1.0     0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000., l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${ifhyb},
   uv_hyb_ens=.true.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${nummem},
   beta_s0=${beta1_inv},s_ens_h=110,s_ens_v=3,
   regional_ensemble_option=1,
   pseudo_hybens = .false.,
   grid_ratio_ens = 3,
   l_ens_in_diff_time=.true.,
   ensemble_path='',
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=10.0,
   metar_impact_radius=20.0,
   metar_impact_radius_lowCloud=8.0,
   l_gsd_terrain_match_surfTobs=.true.,
   l_sfcobserror_ramp_t=.true.,
   l_sfcobserror_ramp_q=.true.,
   l_PBL_pseudo_SurfobsT=.true.,
   l_PBL_pseudo_SurfobsQ=.true.,
   l_PBL_pseudo_SurfobsUV=.false.,
   pblH_ration=0.75,
   pps_press_incr=20.0,
   l_gsd_limit_ocean_q=.true.,
   l_pw_hgt_adjust=.true.,
   l_limit_pw_innov=.true.,
   max_innov_pct=0.1,
   l_cleanSnow_WarmTs=.true.,
   r_cleanSnow_WarmTs_threshold=5.0,
   l_conserve_thetaV=.true.,
   i_conserve_thetaV_iternum=3,
   l_gsd_soilTQ_nudge=${ifsoilnudge},
   l_cld_bld=.true.,
   cld_bld_hgt=30000.0,
   l_numconc=.true.,
   l_closeobs=.true.,
   build_cloud_frac_p=0.50,
   clear_cloud_frac_p=0.10,
   iclean_hydro_withRef_allcol=1,
   i_use_2mQ4B=2,
   i_use_2mT4B=1,
   i_gsdcldanal_type=${cloudanalysistype},
   i_gsdsfc_uselist=1,
   i_lightpcp=1,
   i_sfct_gross=1,
   i_coastline=3,
   i_gsdqc=2,
   l_use_hydroretrieval_all=.true.,
   ioption=1, 
 /
 &CHEM
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${YYYYMMDDHH},
   obhourset=0.,
 /
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
# cp ${USH_DIR}/namelist/gsiparm.anl.sh ./
# . ./gsiparm.anl.sh
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   qoption=2,
   gencode=78,factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.flase.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.true.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,emiss_bc=.true.,
   diag_precon=.true.,step_start=1.e-3,
   l4densvar=.false.,nhr_obsbin=3,
   verbose=.true.,
 /     
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
   grid_ratio_wrfmass=${grid_ratio},
   wrf_mass_hybridcord=${hybridcord},
 /
 &BKGERR
   vs=1.0,
   hzscl=0.373,0.746,1.5,
   bw=0.,fstat=.true.,
/
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=1.5,time_window_rad=1.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null      ps                   1.0     0     0
   prepbufr       t           null      t                    1.0     0     0
   prepbufr       q           null      q                    1.0     0     0
   prepbufr       pw          null      pw                   1.0     0     0
   satwndbufr     uv          null      uv                   1.0     0     0
   prepbufr       uv          null      uv                   1.0     0     0
   prepbufr       spd         null      spd                  1.0     0     0
   prepbufr       dw          null      dw                   1.0     0     0
   radarbufr      rw          null      rw                   1.0     0     0
   prepbufr       sst         null      sst                  1.0     0     0
   gpsrobufr      gps_ref     null      gps                  1.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             1.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              1.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            1.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            1.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            1.0     0     0
   hirs3bufr      hirs3       n16       hirs3_n16            0.0     1     0
   hirs3bufr      hirs3       n17       hirs3_n17            6.0     1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        6.0     2     0
   hirs4bufr      hirs4       n18       hirs4_n18            0.0     1     0
   hirs4bufr      hirs4       n19       hirs4_n19            1.0     2     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        1.0     1     0
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  20.0     2     0
   amsuabufr      amsua       n15       amsua_n15           10.0     2     0
   amsuabufr      amsua       n18       amsua_n18           10.0     2     0
   amsuabufr      amsua       n19       amsua_n19           10.0     2     0
   amsuabufr      amsua       metop-a   amsua_metop-a       10.0     2     0
   amsuabufr      amsua       metop-b   amsua_metop-b       10.0     2     0
   airsbufr       amsua       aqua      amsua_aqua           5.0     2     0
   amsubbufr      amsub       n17       amsub_n17            1.0     1     0
   mhsbufr        mhs         n18       mhs_n18              3.0     2     0
   mhsbufr        mhs         n19       mhs_n19              3.0     2     0
   mhsbufr        mhs         metop-a   mhs_metop-a          3.0     2     0
   mhsbufr        mhs         metop-b   mhs_metop-b          3.0     2     0
   ssmitbufr      ssmi        f13       ssmi_f13             0.0     2     0
   ssmitbufr      ssmi        f14       ssmi_f14             0.0     2     0
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     2     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     2     0
   ssmisbufr      ssmis_las   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_img   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_env   f16       ssmis_f16            0.0     2     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           1.5     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           1.5     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           1.5     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           1.5     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           1.5     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           1.5     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           1.5     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           1.5     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           1.5     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           1.5     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           1.5     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           1.5     1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15           1.5     2     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15           1.5     2     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15           1.5     2     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15           1.5     2     0
   iasibufr       iasi        metop-a   iasi616_metop-a     20.0     1     0
   gomebufr       gome        metop-a   gome_metop-a         1.0     2     0
   omibufr        omi         aura      omi_aura             1.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            1.0     0     0
   tcvitl         tcp         null      tcp                  1.0     0     0
   seviribufr     seviri      m08       seviri_m08           1.0     1     0
   seviribufr     seviri      m09       seviri_m09           1.0     1     0
   seviribufr     seviri      m10       seviri_m10           1.0     1     0
   iasibufr       iasi        metop-b   iasi616_metop-b      0.0     1     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     1     0
   crisbufr       cris        npp       cris_npp             0.0     1     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
   prepbufr       mta_cld     null      mta_cld              1.0     0     0
   prepbufr       gos_ctp     null      gos_ctp              1.0     0     0
   refInGSI       rad_ref     null      rad_ref              1.0     0     0
   lghtInGSI      lghtn       null      lghtn                1.0     0     0
   larcInGSI      larccld     null      larccld              1.0     0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000., l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${ifhyb},
   uv_hyb_ens=.true.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${nummem},
   beta_s0=${beta1_inv},s_ens_h=110,s_ens_v=3,
   regional_ensemble_option=1,
   pseudo_hybens = .false.,
   grid_ratio_ens = 3,
   l_ens_in_diff_time=.true.,
   ensemble_path='',
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=10.0,
   metar_impact_radius=20.0,
   metar_impact_radius_lowCloud=8.0,
   l_gsd_terrain_match_surfTobs=.true.,
   l_sfcobserror_ramp_t=.true.,
   l_sfcobserror_ramp_q=.true.,
   l_PBL_pseudo_SurfobsT=.true.,
   l_PBL_pseudo_SurfobsQ=.true.,
   l_PBL_pseudo_SurfobsUV=.false.,
   pblH_ration=0.75,
   pps_press_incr=20.0,
   l_gsd_limit_ocean_q=.true.,
   l_pw_hgt_adjust=.true.,
   l_limit_pw_innov=.true.,
   max_innov_pct=0.1,
   l_cleanSnow_WarmTs=.true.,
   r_cleanSnow_WarmTs_threshold=5.0,
   l_conserve_thetaV=.true.,
   i_conserve_thetaV_iternum=3,
   l_gsd_soilTQ_nudge=${ifsoilnudge},
   l_cld_bld=.true.,
   cld_bld_hgt=30000.0,
   l_numconc=.true.,
   l_closeobs=.true.,
   build_cloud_frac_p=0.50,
   clear_cloud_frac_p=0.10,
   iclean_hydro_withRef_allcol=1,
   i_use_2mQ4B=2,
   i_use_2mT4B=1,
   i_gsdcldanal_type=${cloudanalysistype},
   i_gsdsfc_uselist=1,
   i_lightpcp=1,
   i_sfct_gross=1,
   i_coastline=3,
   i_gsdqc=2,
   l_use_hydroretrieval_all=.true.,
   ioption=1, 
 /
 &CHEM
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${YYYYMMDDHH},
   obhourset=0.,
 /
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
