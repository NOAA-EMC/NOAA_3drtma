#!/bin/sh -l
##================================================================#
#PBS -l nodes=8:ppn=12
#PBS -l walltime=0:30:00
#PBS -A fv3-cpu                       # <---- define the cpu account: fv3-cpu; da-cpu;
#PBS -N rtma3d_unipost                # <---- define the job name
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
POST_PROC=${np}
echo "number of total processes is ${np} $PBS_NP ${POST_PROC}"
export OMP_NUM_THREADS=1

# module load newdefaults
# module list
module load intel
module load impi
module load netcdf
module load cnvgrib
module list

# Make sure we are using GMT time zone for time computations
export TZ="GMT"

# Set up paths to unix commands
DATE=/bin/date
ECHO=/bin/echo
AWK="/bin/gawk --posix"
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
BC=/usr/bin/bc

CNVGRIB=/apps/cnvgrib/1.4.0/bin/cnvgrib
MPIRUN=mpirun
NCDUMP=ncdump
# WGRIB2=${EXE_ROOT}/wgrib2_new

##########################################################################
#                                                                        # 
#  User Defined variables                                                #
#                                                                        #
##########################################################################
sysname="rtma3d"
SYSNAME=`echo ${sysname} | tr '[:lower:]' '[:upper:]'`
expname="test_MstrMrgInRap_anl"
# expname="test_MstrMrgInRap_fgs"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
ANLS_TIME=2018050118         # <-- define analysis date and time (YYYYMMDDHH)
subcyc=00                    # <-- define sub cycle time (minute)
cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"

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

# where the test is going to run
PDATABASE=/scratch3/NCEPDEV/stmp2/${USER}
TESTROOT=${PDATABASE}/wrkdir_${sysname}/${ANLS_CYC_TIME}
RUNDIR=${TESTROOT}/postprd_${expname}


# where the system (exe, static, script, etc.) is placed
SYSROOT=/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev             # <-- modify
RTMA3D_DIR=${SYSROOT}/rtma3d_workflow                              # <-- modify

# export CRTM="/lfs3/projects/nrtrr/alexander/code/contrib/nceplibs/nwprod/lib/sorc/crtm_v2.0.7/fix"
export CRTM="/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.3/fix_update"
FIXCRTM=$CRTM


UPP_DIR="/scratch4/NCEPDEV/meso/save/Gang.Zhao/EMC_post_dev/EMC_post"
# UPP_DIR="/scratch4/NCEPDEV/fv3-cam/save/Edward.Colon/EMC_post"
# UPP_SORC=${UPP_DIR}/sorc/ncep_post.fd
# BUILD_UPP=${SORC_DIR}/ncep_post.fd
# BIN_UPP=${BUILD_UPP}
EXEC_DIR=${UPP_DIR}/exec                    # <-- modify it if need
UPPEXE="ncep_post"                                 # <-- modify it if need 
pgm=${UPPEXE}

export EXE_ROOT=${EXEC_DIR}
export DATAHOME=$RUNDIR
export MODEL="RAP"

export DATAWRFHOME="/scratch3/NCEPDEV/stmp1/Gang.Zhao/wrkdir_rtma3d/data/retro6_111/rtma3d.201805011800/gsiprd.35095643"
# export DATAWRFHOME="/scratch3/NCEPDEV/stmp1/Gang.Zhao/wrkdir_rtma3d/com2/rtma3d/retro6_111/rtma3d.20180501/fgsprd.t18z"
# ${LN} -sf ${DATAWRFHOME}/hrrr.t19z.wrfguess_rap   ${DATAWRFHOME}/wrf_inout

export STATIC_DIR="/scratch4/NCEPDEV/da/save/Gang.Zhao/rtma3d_dev/testrun/static/UPP"
export STATICWRF_DIR="/scratch4/NCEPDEV/meso/save/Gang.Zhao/GSD_GSI_dev/rapid-refresh/WRFV3.7.1"

##########################################################################

# Print run parameters
${ECHO}
${ECHO} "unipost.ksh started at `${DATE}`"
${ECHO}
${ECHO} "DATAHOME = ${DATAHOME}"
${ECHO} "     EXE_ROOT = ${EXE_ROOT}"

# Set up some constants
if [ "${MODEL}" == "RAP" ]; then
  export POST=${EXE_ROOT}/${UPPEXE}
  export CORE=RAPR
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  export POST=${EXE_ROOT}/${UPPEXE}
  export CORE=NMM
fi

# Check to make sure the EXE_ROOT var was specified
if [ ! -d ${EXE_ROOT} ]; then
  ${ECHO} "ERROR: EXE_ROOT, '${EXE_ROOT}', does not exist"
  exit 1
fi

# Check to make sure the post executable exists
if [ ! -x ${POST} ]; then
  ${ECHO} "ERROR: ${POST} does not exist, or is not executable"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}/${FCST_TIME}
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


timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${FCST_TIME} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${FCST_TIME} hours"`

${CAT} > itag <<EOF
${DATAWRFHOME}/wrf_inout
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

${RM} -f fort.*
ln -s ${STATIC_DIR}/post_avblflds.xml post_avblflds.xml
ln -s ${STATIC_DIR}/params_grib2_tbl_new params_grib2_tbl_new
ln -s ${STATIC_DIR}/postcntrl.xml postcntrl.xml
ln -s ${STATIC_DIR}/postxconfig-NT.txt postxconfig-NT.txt
if [ "${MODEL}" == "RAP" ]; then
  ln -s ${STATICWRF_DIR}/run/ETAMPNEW_DATA eta_micro_lookup.dat
elif [ "${MODEL}" == "WRF-RR NMM" ]; then
  ln -s ${STATICWRF_DIR}/run/ETAMPNEW_DATA eta_micro_lookup.dat
fi

# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g15.SpcCoeff.bin imgr_g11.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g13.SpcCoeff.bin imgr_g12.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/imgr_g15.TauCoeff.bin imgr_g11.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/imgr_g13.TauCoeff.bin imgr_g12.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODPS/Big_Endian/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
# ln -s ${CRTM}/CloudCoeff/Big_Endian/CloudCoeff.bin CloudCoeff.bin
# ln -s ${CRTM}/AerosolCoeff/Big_Endian/AerosolCoeff.bin AerosolCoeff.bin
# ln -s ${CRTM}/EmisCoeff/Big_Endian/Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin

#=============================================================================#
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
# ln -s ${CRTM}/SpcCoeff/Big_Endian/imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin

# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/ODAS/Big_Endian/v.seviri_m10.TauCoeff.bin v.seviri_m10.TauCoeff.bin
# ln -s ${CRTM}/TauCoeff/Big_Endian/imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin

# ln -s ${CRTM}/CloudCoeff/Big_Endian/CloudCoeff.bin CloudCoeff.bin
# ln -s ${CRTM}/AerosolCoeff/Big_Endian/AerosolCoeff.bin AerosolCoeff.bin
# ln -s ${CRTM}/EmisCoeff/Big_Endian/EmisCoeff.bin EmisCoeff.bin
#=============================================================================#
for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" ; do
    ln -s "$FIXCRTM/$what.TauCoeff.bin" .
    ln -s "$FIXCRTM/$what.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "$FIXCRTM/${what}Coeff.bin" .
done

for what in  $FIXCRTM/*Emis* ; do
   ln -s $what .
done

#=============================================================================#

# Run unipost
${MPIRUN} -np $np ${POST}< itag
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "${POST} crashed!  Exit status=${error}"
  exit ${error}
fi

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME} ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV} ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2

# Append entire wrftwo to wrfnat
${CAT} WRFNAT.GrbF${FCST_TIME} WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV} WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2

${MV} ${workdir}/WRFTWO.GrbF${FCST_TIME} ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfprs_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrftwo_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi
if [ ! -s "${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2" ]; then
  ${ECHO} "unipost crashed! wrfnat_hrconus_${FCST_TIME}.grib2 is missing"
  exit 1
fi

# Move the output files to postprd
${MV} ${workdir}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2
${MV} ${workdir}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2
${MV} ${workdir}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2
# ${RM} -rf ${workdir}

# Create softlinks for transfer
basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
ln -s ${DATAHOME}/wrfprs_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfprs_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrftwo_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrftwo_${basetime}${FCST_TIME}00
ln -s ${DATAHOME}/wrfnat_hrconus_${FCST_TIME}.grib2 ${DATAHOME}/wrfnat_${basetime}${FCST_TIME}00

${ECHO} "unipost.ksh completed at `${DATE}`"

exit 0
