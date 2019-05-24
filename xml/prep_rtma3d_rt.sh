#!/bin/bash

#This script preps directories for ROCOTO-controlled RTMA3D real time runs (on Jet for now).

#

########################################################################

#

#   Functions:

#     1. generate the rtma3d_rt_[expname].xml for ROCOTO to control the workflow

#     2. generate script run_rtma3d_rt_[expname].sh to run the workflow

#     3. generate script chk_rtma3d_rt_[expname].sh to check the status of workflow

#

########################################################################

#

###########################################################

#--- Detect the machine platform                          #

###########################################################

#

if [[ -d /dcom && -d /hwrf ]] ; then

    . /usrx/local/Modules/3.2.10/init/sh

#   MODULESHOME="/usrx/local/Modules/3.2.10"

#   . $MODULESHOME/init/sh

    MACHINE=wcoss 

    echo "Machine not configured for 3d rtma."

elif [[ -d /cm ]] ; then

#   MODULESHOME="/usrx/local/Modules/3.2.10"

#   . $MODULESHOME/init/sh

    conf_target=nco

    MACHINE=cray

    echo "Machine not configured for 3d rtma."

elif [[ -d /ioddev_dell ]]; then

#   MODULESHOME="/usrx/local/Modules/3.2.10"

#   . $MODULESHOME/init/sh

    conf_target=nco

    MACHINE=dell

    echo "Machine not yet configured for real time 3d rtma but is being developed."

    exit 1

elif [[ -d /scratch3 ]] ; then

    . /etc/profile

    . /etc/profile.d/modules.sh >/dev/null # Module Support

    MACHINE=theia

    echo "Machine not configured for real time 3d rtma."

    exit 1

elif [[ -d /mnt/lfs3/projects ]] ; then

    . /etc/profile

    . /etc/profile.d/modules.sh >/dev/null # Module Support

    MACHINE=jet

    nwprod_path="/mnt/lfs3/projects/hfv3gfs/nwprod/lib/modulefiles"

    produtil_path="/mnt/lfs3/projects/hfv3gfs/emc.nemspara/soft/NCEPLIBS-prod_util"

else

    MACHINE="unknown"

    echo 'Running on $MACHINE '

    echo ' ---------> Warning Warning Warning Warning <--------- '

    echo '     Machine $machine is NOT ready for running $NET.'

    exit 1

fi


echo "Running on Machine: $MACHINE "

#

###########################################################

#--- detect path & name of RTMA3D home directory          #

###########################################################

#

i_max=4; i=0;

while [ "$i" -lt "$i_max" ]

do

  let "i=$i+1"

  if [ -d ./scripts ]

  then

    cd ./scripts

    TOP_RTMA=`dirname $(readlink -f .)`

    TOP_0000=`dirname ${TOP_RTMA}`

    TOP_BASE=`basename ${TOP_RTMA}`

    echo " found the rtma3d system home directory is $TOP_RTMA"

    break

  else

    cd ..

  fi

done

if [ "$i" -ge "$i_max" ]

then

  echo ' RTMA3D root directory could not be found. Abort the task of compilation.'

  exit 1

fi

cd ${TOP_RTMA}/xml

# set up the tmp working directory

TMP_WRKDIR=${TOP_RTMA}/xml/tmp

mkdir ${TMP_WRKDIR}

cd ${TMP_WRKDIR}

#

###########################################################

#--- User defined variables                               #

###########################################################

#

set -x

export startCDATE=201902131200              #yyyymmddhhmm - Starting day of RTMA3D run (mainly used for retrospective run)

export endCDATE=201905310000                #yyyymmddhhmm - Ending day of RTMA3D run (mainly used for retrospective run) 

export ExpDateWindows="08 05 2019 *"        # dd mm yyyy weekday (crontab-like date format, mainly used for real-time run)

export NET=rtma3d                           #selection of rtma3d (or rtma,urma)

export RUN=rtma3d_rt                        #selection of rtma3d (or rtma,urma)

export envir=""                             #environment (test, prod, dev, etc.)

export run_envir=""                         #

export expname="lsf"                      # experiment name

export NWROOT=${TOP_RTMA}                   #root directory for RTMA/URMA j-job scripts, scripts, parm files, etc. 

export SCHEDULER="lsf"                    # SLURM (after 05/01/2019)

                                            # PBS (not available after May 2019)

if [ ${SCHEDULER} = "PBS" ] || [ ${SCHEDULER} = "MOAB" ]; then

  SCHD_ATTRB="moabtorque"

elif [ ${SCHEDULER} = "SLURM" ] || [ ${SCHEDULER} = "slurm" ]; then

  SCHD_ATTRB="slurm"

elif [ ${SCHEDULER} = "LSF" ] || [ ${SCHEDULER} = "lsf" ]; then

  SCHD_ATTRB="lsf"

else

  echo "user specified an Unknown Scheduler: ${SCHEDULER}. Please re-set : either PBS or SLURM "

  exit 1

fi

#

# Note: Definition for the following variables 

#       depends on the machine platform, 

#       and different user and/or experiment.

#

if [ ${MACHINE} = "jet" ] ; then

# path to system home directory

  HOMEBASE_DIR=${NWROOT}               # path to system home directory

# Path to top running directory

# DATABASE_DIR="/mnt/lfs3/projects/hfv3gfs/${USER}/gsd_dev1_jjob_databasedir"

  DATABASE_DIR="/mnt/lfs3/projects/hfv3gfs/${USER}/rtma_3d_databasedir"

# Computational resources

  ACCOUNT="hfv3gfs"                    # account for CPU resources

  QUEUE="batch"                        #

  QUEUE_DBG="debug"                    #

  QUEUE_SVC="service"                  # queue/partition for transferring data

  if [ ${SCHEDULER} = "SLURM" ] || [ ${SCHEDULER} = "slurm" ]; then

    PARTITION="kjet"

    PARTITION_DA="kjet"

    RESERVATION="<native>--export=ALL,SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account><partition>&PARTITION;</partition>"

#   RESERVATION_DA="<native>--partition=&PARTITION_DA; --reservation=&RES_DA; --export=SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT_DA;</account>"

    RESERVATION_DA="<native>--export=ALL,SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT_DA;</account><partition>&PARTITION;</partition>"

    RESERVATION_SMARTINIT="<native>--export=ALL,SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

#   RESERVATION_POST="<native>--partition=&PARTITION; --reservation=&RES_POST; --export=SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

    RESERVATION_POST="<native>--export=ALL,SLURM_UMAK=022 --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account><partition>&PARTITION;</partition>"

    RESERVATION_SERIAL="<native>--ntasks=1 --export=ALL,SLURM_UMAK=02  --mail-type=NONE</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account><partition>&PARTITION;</partition>"

  elif [ ${SCHEDULER} = "PBS" ] || [ ${SCHEDULER} = "MOAB" ]; then

    PARTITION="sjet:tjet"

    PARTITION_DA="sjet:tjet"

    RESERVATION="<native>-l partition=&PARTITION; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

#   RESERVATION_DA="<native>-l partition=&PARTITION_DA;,flags=ADVRES:&RES_DA; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT_DA;</account>"

    RESERVATION_DA="<native>-l partition=&PARTITION_DA; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT_DA;</account>"

    RESERVATION_SMARTINIT="<native>-l partition=&PARTITION; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

#   RESERVATION_POST="<native>-l partition=&PARTITION;,flags=ADVRES:&RES_POST; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

    RESERVATION_POST="<native>-l partition=&PARTITION; -W umask=022 -m n</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

  fi

elif [ ${MACHINE} = "dell" ] ; then

# path to system home directory

  HOMEBASE_DIR=${NWROOT}               # path to system home directory

# Path to top running directory

  DATABASE_DIR="/gpfs/dell2/stmp/${USER}/${NET}_wrkdir_databasedir"

# Computational resources

  ACCOUNT="RTMA-T2O"                    # account for CPU resources

  QUEUE="dev_shared"                        #

  QUEUE_DBG="debug"                    #

  QUEUE_SVC="dev_transfer"                  # queue/partition for transferring data

  RESERVATION="<native>-R rusage[mem=2000] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

  RESERVATION_DA="<native>-R rusage[mem=1900] -R span[ptile=14] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT_DA;</account>"

  RESERVATION_SMARTINIT="<native>-R rusage[mem=2000] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

  RESERVATION_POST="<native></native>-R rusage[mem=3300] -R span[ptile=8] -R affinity[core]<queue>&QUEUE;</queue><account>&ACCOUNT;</account>"

  RESERVATION_SERIAL="<native>-R rusage[mem=2000] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"
fi

#

#--- ptmp_base: top running and archive directory (variable equivalent to DATABASE_DIR)

#

  export ptmp_base=${DATABASE_DIR}

  export CAP_NET=`echo ${NET} | tr '[:lower:]' '[:upper:]'`

  export CAP_RUN=`echo ${RUN} | tr '[:lower:]' '[:upper:]'`

  export CAP_ENVIR=`echo ${envir} | tr '[:lower:]' '[:upper:]'`

  export CAP_RUN_ENVIR=`echo ${run_envir} | tr '[:lower:]' '[:upper:]'`

#

# Specified Directory Names (to some special or other user's or external directory)

#

if [ ${MACHINE} = 'jet' ] ; then

  rtrr_hrrr='/home/rtrr/HRRR'

  rtrr_rap='/home/rtrr/RAP'

  EXECrtrr_hrrr=${rtrr_hrrr}/exec

  EXECrtrr_rap=${rtrr_rap}/exec

  RTMA3D_GSD='/home/${USER}/rtma3d_repo/GSD'

  RTMA3D_GSD_dev1='/home/${USER}/rtma3d_repo/GSD_dev1'

elif [ ${MACHINE} = 'dell' ] ; then

  rtrr_hrrr='/home/rtrr/HRRR'

  rtrr_rap='/home/rtrr/RAP'

  EXECrtrr_hrrr=${rtrr_hrrr}/exec

  EXECrtrr_rap=${rtrr_rap}/exec

  RTMA3D_GSD='/home/${USER}/rtma3d_repo/GSD'

  RTMA3D_GSD_dev1='/home/${USER}/rtma3d_repo/GSD_dev1'
  


fi



#

###########################################################

#

# User defines executable file name for each task

#      then links them to their real executables.

#

###########################################################

#

EXEC_DIR=${NWROOT}/exec

if [[ ! -d ${EXEC_DIR} ]] ; then

  mkdir -p ${EXEC_DIR}

fi

EXEC_mine=/home/${USER}/rtma3d_repo/rt_nco_jet_GSD_dev1/exec

# GSI

export exefile_name_gsi='rtma3d_gsi_hyb'

ln -sf ${EXECrtrr_hrrr}/GSI/HRRR_gsi_hyb ${EXEC_DIR}/${exefile_name_gsi}

export exefile_name_radar='rtma3d_process_NSSL_mosaic'

ln -sf ${EXECrtrr_hrrr}/GSI/process_NSSL_mosaic.exe ${EXEC_DIR}/${exefile_name_radar}

export exefile_name_lightning='rtma3d_process_lightning'

ln -sf ${EXECrtrr_hrrr}/GSI/process_Lightning.exe ${EXEC_DIR}/${exefile_name_lightning}

export exefile_name_lightning_bufr="rtma3d_process_lightning_bufr"

ln -sf ${EXECrtrr_hrrr}/GSI/process_Lightning_bufr.exe ${EXEC_DIR}/${exefile_name_lightning_bufr}

export exefile_name_cloud="rtma3d_process_NASALaRC_cloud"

ln -sf ${EXECrtrr_hrrr}/GSI/process_NASALaRC_cloud.exe ${EXEC_DIR}/${exefile_name_cloud}

export exefile_name_diagconv="rtma3d_read_diag_conv"

ln -sf ${EXECrtrr_hrrr}/GSI/read_diag_conv.exe ${EXEC_DIR}/${exefile_name_diagconv}

export exefile_name_diagrad="rtma3d_read_diag_rad"

ln -sf ${EXECrtrr_hrrr}/GSI/read_diag_rad.exe ${EXEC_DIR}/${exefile_name_diagrad}

export exefile_name_countobs="rtma3d_count_obs"

ln -sf ${EXECrtrr_hrrr}/GSI/count_obs.exe ${EXEC_DIR}/${exefile_name_countobs}

# UPP

export exefile_name_post="rtma3d_ncep_post"

ln -sf ${EXECrtrr_hrrr}/UPP/ncep_post.exe ${EXEC_DIR}/${exefile_name_post}

# SMARTINIT

export exefile_name_smartinit="rtma3d_smartinit_conus"

ln -sf ${RTMA3D_GSD_dev1}/exec/smartinit/hrrr_smartinit_conus ${EXEC_DIR}/${exefile_name_smartinit}

# MET

export exefile_name_verif=""    # executable of verification (MET) is defined by loading module met

echo 

echo " check up the symbol link name for executables under ${EXEC_DIR}"

ls -l ${EXEC_DIR}

echo 

#

###########################################################

#

#--- define the path to the static data

#

###########################################################

#

#    fix/

#      gsi/: fixed data, e.g., statistical file of B-Matrix)

#      crtm/: (CRTM coefficients)

#      wrf/

#      wps/: e.g., geo_em.d01.nc

#      obsuselist/

#		amdar_reject_lists/

#		mesonet_uselists/

#		sfcobs_provider/

#

#    parm/

#      gsi/: namelist file, e.g., gsiparm.anl.sh)

#      upp/: configuration fiile for upp, like postcntrl-NT.txt)

#      verif/

#      wrf/

#

#    Note:

#       User can specify the path to use user's static data.

#       The variable name with "_udef" means: user may define

#       the path to their specific static data, 

#

#       then link these paths to the symbol links

#          under fix/ and parm/.

#

###########################################################

# User also may use the static data under this 3DRTMA system home static/ (GSI/, UPP/, etc.)

# then use $STATIC_mine to define the follwing FIXgsi_udef, Fixcrtm_udef, etc.

  STATIC_mine=${NWROOT}/static

  if [ $MACHINE = jet ] ; then

    export FIXgsi_udef=${RTMA3D_GSD_dev1}/static/GSI

    export FIXcrtm_udef=${RTMA3D_GSD_dev1}/static/GSI/CRTM_Coefficients

    export FIXwps_udef=${RTMA3D_GSD_dev1}/static/WPS

#   export OBS_USELIST_udef="/mnt/lfs3/projects/hfv3gfs/${USER}/FixData/ObsUseList_rtma3d"

#   export SFCOBS_USELIST_udef="/mnt/lfs3/projects/hfv3gfs/${USER}/FixData/ObsUseList_rtma3d/gsd/mesonet_uselists"

#   export AIRCRAFT_REJECT_udef="/mnt/lfs3/projects/hfv3gfs/${USER}/FixData/ObsUseList_rtma3d/gsd/amdar_reject_lists"

#   export SFCOBS_PROVIDER_udef="/mnt/lfs3/projects/hfv3gfs/${USER}/FixData/GSI-fix_rtma3d_emc_test"

    export PARMupp_udef=${RTMA3D_GSD_dev1}/static/UPP

  fi

#       define the variable names for symbol links under fix/ and parm/

  export FIXrtma3d="${NWROOT}/fix"

  export FIXgsi="${FIXrtma3d}/gsi"

  export FIXcrtm="${FIXrtma3d}/crtm"

  export FIXwps="${FIXrtma3d}/wps"

# export OBS_USELIST="${FIXrtma3d}/obsuselist"

# export SFCOBS_USELIST="${OBS_USELIST}/mesonet_uselists"

# export AIRCRAFT_REJECT="${OBS_USELIST}/amdar_reject_lists"

# export SFCOBS_PROVIDER="${OBS_USELIST}/sfcobs_provider"

  export PARMrtma3d="${NWROOT}/parm"

  export PARMupp="${PARMrtma3d}/upp"

  export PARMgsi="${PARMrtma3d}/gsi"

#

#        link to the symbol links

#

  if [ ! -d ${FIXrtma3d}   ] ; then mkdir -p ${FIXrtma3d}   ; fi

  if [ ! -d ${PARMrtma3d}  ] ; then mkdir -p ${PARMrtma3d}  ; fi

# if [ ! -d ${OBS_USELIST} ] ; then mkdir -p ${OBS_USELIST} ; fi

  if [ ${MACHINE} = 'jet' ] ; then

    cd ${FIXrtma3d}

    echo " linking fixed data on ${MACHINE} for GSI analysis"

    rm -rf $FIXgsi

    ln -sf ${FIXgsi_udef}        ${FIXgsi}

    rm -rf $FIXcrtm

    ln -sf ${FIXcrtm_udef}       ${FIXcrtm}

    rm -rf $FIXwps

    ln -sf ${FIXwps_udef}        ${FIXwps}

#   cd ${OBS_USELIST}

#   rm -rf $SFCOBS_USELIST

#   echo " ln -sf ${SFCOBS_USELIST_udef}        ${SFCOBS_USELIST}"

#   ln -sf ${SFCOBS_USELIST_udef}        ${SFCOBS_USELIST}

#   rm -rf $AIRCRAFT_REJECT

#   echo " ln -sf ${AIRCRAFT_REJECT_udef}       ${AIRCRAFT_REJECT}"

#   ln -sf ${AIRCRAFT_REJECT_udef}       ${AIRCRAFT_REJECT}

#   rm -rf $SFCOBS_PROVIDER

#   echo " ln -sf ${SFCOBS_PROVIDER_udef}       ${SFCOBS_PROVIDER}"

#   ln -sf ${SFCOBS_PROVIDER_udef}       ${SFCOBS_PROVIDER}

    cd ${PARMrtma3d}

#   if [ ! -d $PARMgsi ] && [ ! -f ${PARMgsi}/gsiparm.anl.sh ]  

#   then

#     echo " WARNING ---- ${PARMgsi} does NOT exist. Check and Abort this task! ---- WARNING ! "

#     exit 1

#   fi

    rm -rf $PARMupp

    ln -sf ${PARMupp_udef}               ${PARMupp}

  else

    echo " Warning Warning Warning"

    echo " Failed to set up static data directories. Abort task."

    echo 

    exit 1

  fi

  echo; ls -ltr $FIXrtma3d ; echo

# echo; ls -ltr $OBS_USELIST; echo

  echo; ls -ltr $PARMrtma3d; echo

#

#--- user-defined variables for running gsi analysis (in namelist file: gsiparm.anl.sh)

#

  export gsi_grid_ratio_in_var=1     # can be 4 if running hybrid to save time

  export gsi_grid_rario_in_cldanl=1

#

#--- Definition for common Linux commands and tools

#

# linux_cmd_list="which grep rm cp mv ln mkdir cat echo ls cut date wc bc sed awk tail cnvgrib mpirun cpfs unzip "

# LINUX_CMD_LIST=`echo ${linux_cmd_list} | tr '[:lower:]' '[:upper:]'`

#

#--- Computational Resources

#

###########################################################

#

#             User definition section ends here.

#             User definition section ends here.

#             User definition section ends here.

#

###########################################################

#

###########################################################

#                                                         #

#        generate XML workflow file for ROCOTO            #

#        generate XML workflow file for ROCOTO            #

#        generate XML workflow file for ROCOTO            #

#                                                         #

###########################################################

#

WRKFLOW_DIR=${NWROOT}/xml

# workflow control XML file -->  rtma3d_rt.xml

XML_FNAME="${RUN}_${expname}.xml"

# workflow control database file -->  rtma3d_rt.db

DB_FNAME="${RUN}_${expname}.db"

# run_rtma3d.sh (that can be used in crontab)

run_scriptname="run_${RUN}_${expname}.sh"

# chk_rtma3d.sh to check the status of workflow 

chk_scriptname="chk_${RUN}_${expname}.sh"

rm -f ${NWROOT}/xml/${XML_FNAME}

#

###########################################################

#                                                         #

#--- ENTITY Definition Block                              #

#                                                         #

###########################################################

#

cat > ${NWROOT}/xml/${XML_FNAME} <<EOF 
<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE workflow [

<!-- Block of NWP System Definition -->

<!ENTITY NET "${NET}">

<!ENTITY RUN "${RUN}">

<!-- EOB -->

<!ENTITY ACCOUNT "${ACCOUNT}">

<!ENTITY ACCOUNT_DA "${ACCOUNT}">

<!ENTITY SCHEDULER "${SCHEDULER}">

<!ENTITY PARTITION "${PARTITION}">

<!ENTITY PARTITION_DA "${PARTITION_DA}">

<!ENTITY QUEUE "${QUEUE}">

<!ENTITY RES_DA "rtma-kjet">

<!ENTITY RES_POST "rtma-kjet">

<!ENTITY SYSTEM_ID "RTMA_3D"> 

<!ENTITY HOMEBASE_DIR "${HOMEBASE_DIR}">

<!ENTITY DATABASE_DIR "${DATABASE_DIR}">

<!ENTITY OBS_DIR "/public/data">

<!ENTITY NCL_VER "6.5.0">

<!ENTITY SCRIPTS "&HOMEBASE_DIR;/scripts">

<!ENTITY STATIC_DIR "&HOMEBASE_DIR;/fix">

<!ENTITY WPS_ROOT "&HOMEBASE_DIR;/exec/WPS">

<!ENTITY WRF_ROOT "&HOMEBASE_DIR;/exec/WRF">

<!ENTITY UNIPOST_EXEC "&HOMEBASE_DIR;/exec/UPP">

<!ENTITY SMARTINIT_EXEC "&HOMEBASE_DIR;/exec/smartinit"> 

<!ENTITY WGRIB1 "&HOMEBASE_DIR;/exec/UPP/wgrib">

<!ENTITY WGRIB "&HOMEBASE_DIR;/exec/UPP/wgrib2">

<!ENTITY GSI_ROOT "&HOMEBASE_DIR;/exec/GSI">

<!ENTITY FIX_ROOT "&STATIC_DIR;/GSI">

<!ENTITY HOMErtma3d "&HOMEBASE_DIR;">

<!ENTITY SCRIPT_DIR "&HOMEBASE_DIR;/scripts">

<!ENTITY JJOB_DIR   "&HOMEBASE_DIR;/jobs">

<!ENTITY WRKFLW_DIR "&HOMEBASE_DIR;/xml">

<!ENTITY MACHINE    "${MACHINE}">

<!-- Definition Block of Datasets for Real-Time RTMA3D on Jet -->

<!--     (Better DO NOT TOUCH this Block)                     -->

<!ENTITY HRRR_DIR "/home/rtrr/hrrr">

<!ENTITY SST_DIR "&OBS_DIR;/grids/ncep/sst/0p083deg/grib2">

<!ENTITY GFS_DIR "&OBS_DIR;/grids/gfs/0p5deg/grib2">

<!ENTITY ENKFFCST_DIR "&OBS_DIR;/grids/enkf/atm">

<!ENTITY AIRCRAFT_REJECT "/home/rtruc/amdar_reject_lists">

<!ENTITY SFCOBS_USELIST "/lfs3/projects/amb-verif/mesonet_uselists">

<!ENTITY PREPBUFR_DIR "&OBS_DIR;/grids/rap/prepbufr">

<!ENTITY PREPBUFR_EARLY_DIR "&OBS_DIR;/grids/rap/prepbufr_test">

<!ENTITY PREPBUFR_SAT_DIR "&OBS_DIR;/grids/rap/radiance">

<!ENTITY LIGHTNING_DIR "&OBS_DIR;/lightning">

<!ENTITY BUFRLIGHTNING_DIR "/mnt/lfs1/projects/rtwbl/mhu/rapobs/radiance">

<!ENTITY RADAR_DIR "&OBS_DIR;/radar/mrms">

<!ENTITY SATELLITE_DIR "&OBS_DIR;/sat/nasa">

<!ENTITY LANGLEY_BUFR_DIR "&OBS_DIR;/grids/rap/langley">

<!ENTITY RADVELLEV2_DIR "&OBS_DIR;/grids/rap/nexrad">

<!ENTITY RADVELLEV2P5_DIR "&OBS_DIR;/grids/rap/radwnd">

<!ENTITY SATWND_DIR "&OBS_DIR;/grids/rap/satwnd">

<!ENTITY NACELLE_RSD "&OBS_DIR;/tower/restricted/nacelle/netcdf">

<!ENTITY TOWER_RSD "&OBS_DIR;/tower/restricted/met/netcdf">

<!ENTITY TOWER_NRSD "&OBS_DIR;/tower/public/netcdf">

<!ENTITY SODAR_NRSD "&OBS_DIR;/profiler/wind/external/netcdf">

<!ENTITY TAMDAR_DIR "&OBS_DIR;/acars/raw/netcdf">

<!ENTITY NCEPSNOW_DIR "&OBS_DIR;/grids/ncep/snow/ims96/grib2">

<!ENTITY HIGHRES_SST_DIR "&OBS_DIR;/grids/ncep/sst/0p083deg/grib2">

<!ENTITY HIGHRES_SST14KM_DIR "&OBS_DIR;/grids/ncep/sst/grib">

<!ENTITY TCVITALS_DIR "&OBS_DIR;/nhc/tcvitals">

<!ENTITY STICKNET_DIR "&OBS_DIR;/vortex-se/stesonet">

<!-- END OF BLOCK -->

<!-- Definition Block of Top Directories for running Real-Time RTMA3D on Jet -->

<!ENTITY LOG_DIR "&DATABASE_DIR;/log">

<!ENTITY DATAROOT_ENS "&DATABASE_DIR;/gfsenkf">

<!ENTITY DATAROOT "&DATABASE_DIR;/data">

<!ENTITY DATAROOT_BC "&DATABASE_DIR;/data">

<!ENTITY DATAROOT_PCYC "&DATABASE_DIR;/data">

<!-- EOB -->

<!-- Block of Job Definition -->

<!ENTITY JOBNAME_PRE "&RUN;">

<!ENTITY POST_NAME "hrconus">

<!-- EOB -->

<!-- Definition Block of RESOURCES used to run Real-Time RTMA3D on Jet -->

<!ENTITY SMARTINIT_PROC "1">

<!ENTITY CONVENTIONAL_PROC "1">

<!ENTITY LIGHTNING_PROC "1">

<!ENTITY RADAR_PROC "33">

<!ENTITY SATELLITE_PROC "1">

<!ENTITY GSI_HYB_PROC "280">

<!ENTITY GSI_DIAG_PROC "1">

<!ENTITY RADARLINKS_PROC "1">

<!ENTITY POST_PROC "32">

<!ENTITY NCL_PROC "1">

<!ENTITY NCL_MAIN_PROC "8">

<!ENTITY CLEAN_PROC "1">

<!ENTITY SMARTINIT_RESOURCES "<walltime>00:10:00</walltime><memory>10G</memory>">

<!ENTITY CLEAN_RESOURCES "<walltime>00:45:00</walltime><memory>500M</memory>">

<!ENTITY LIGHTNING_RESOURCES "<walltime>00:05:00</walltime><memory>4G</memory>">

<!ENTITY CONVENTIONAL_RESOURCES "<walltime>00:05:00</walltime><memory>2G</memory>">

<!ENTITY RADAR_RESOURCES "<walltime>00:05:00</walltime>">

<!ENTITY SATELLITE_RESOURCES "<walltime>00:05:00</walltime><memory>10G</memory>">

<!ENTITY GSI_HYB_RESOURCES "<walltime>00:40:00</walltime>">

<!ENTITY GSI_DIAG_RESOURCES "<walltime>00:05:00</walltime><memory>600M</memory>">

<!ENTITY POST_RESOURCES "<walltime>00:45:00</walltime>">

<!ENTITY NCL_RESOURCES "<walltime>00:55:00</walltime>">

<!ENTITY NCL_SKEWT_RESOURCES "<walltime>00:50:00</walltime><memory>9G</memory>">

<!ENTITY NCL_HTXS_RESOURCES "<walltime>00:25:00</walltime><memory>9G</memory>">

<!-- End gsi_hyb by 3 hr -->

<!-- End all post-processing by 6 hrs -->

<!ENTITY START_TIME_RADARLINKS "00:10:00">

<!ENTITY START_TIME_RADAR "00:20:00">

<!ENTITY START_TIME_CONVENTIONAL "00:37:00">

<!ENTITY START_TIME_GSI "01:40:00">

<!ENTITY DEADLINE_DA "03:00:00">

<!ENTITY DEADLINE_PP "06:00:00">

<!ENTITY WALL_LIMIT_DA '<deadline><cyclestr offset="&DEADLINE_DA;">@Y@m@d@H@M</cyclestr></deadline>'>

<!ENTITY WALL_LIMIT_PP '<deadline><cyclestr offset="&DEADLINE_PP;">@Y@m@d@H@M</cyclestr></deadline>'>

<!ENTITY RESERVATION '${RESERVATION}'>

<!ENTITY RESERVATION_DA '${RESERVATION_DA}'>

<!ENTITY RESERVATION_SMARTINIT '${RESERVATION_SMARTINIT}'>

<!ENTITY RESERVATION_POST '${RESERVATION_POST}'>

<!-- END OF BLOCK -->

<!-- Definition Block of COMMON Variables used in all tasks -->

<!ENTITY ENVARS

   '<envar>

      <name>HOMErtma3d</name>

      <value>&HOMErtma3d;</value>

    </envar>

    <envar>

      <name>SCHEDULER</name>

      <value>&SCHEDULER;</value>

    </envar>

    <envar>

      <name>MACHINE</name>

      <value>&MACHINE;</value>

    </envar>

    <envar>

      <name>LOG_DIR</name>

      <value>&LOG_DIR;</value>

    </envar>

    <envar>

      <name>jlogfile</name>

      <value><cyclestr>&LOG_DIR;/jlogfile_@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>PDY</name>

      <value><cyclestr>@Y@m@d</cyclestr></value>

    </envar>

    <envar>

      <name>cyc</name>

      <value><cyclestr>@H</cyclestr></value>

    </envar>

    <envar>

      <name>subcyc</name>

      <value><cyclestr>@M</cyclestr></value>

    </envar>

   '>

EOF

#

#--- Definition for common Linux commands and tools

#

#

#--- adding the Ending Mark to end of whole ENTITY Definition Block

#

cat >> ${NWROOT}/xml/${RUN}_${expname}.xml <<EOF 

]>

EOF

#

###########################################################

#                                                         #

#--- Workflow Task Definition Block                       #

#                                                         #

###########################################################

#

cat >> ${NWROOT}/xml/${XML_FNAME} <<EOF 

<workflow realtime="T" scheduler="${SCHD_ATTRB}" cyclethrottle="30" cyclelifespan="01:00:00:00">

  <log>

    <cyclestr>&LOG_DIR;/workflow_@Y@m@d@H.log</cyclestr>

  </log>

  <cycledef group="00hr">00 00,12 ${ExpDateWindows}</cycledef>

  <cycledef group="01hr">00 01,13 ${ExpDateWindows}</cycledef>

  <cycledef group="02-11hr">00 02-11,14-23 ${ExpDateWindows}</cycledef>

  <metatask>

<!-- only need 60 for hourly running

    <var name="min">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

    <var name="min">60</var>

    <var name="off">00:00:00</var>

  <task name="lightning_gsi_#min#" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &LIGHTNING_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_LIGHTNING</command>

    <cores>&LIGHTNING_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_lightning_@H_#min#</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/lightning_gsi_@Y@m@d@H00_#min#.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr offset="-1:00:00">@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>SUBH_TIME</name>

      <value>#min#</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/#min#</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>FIX_ROOT</name>

      <value>&FIX_ROOT;</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/WPS</value>

    </envar>

    <envar>

      <name>LIGHTNING_ROOT</name>

      <value>&LIGHTNING_DIR;</value>

    </envar>

      <dependency>

        <or>

          <datadep><cyclestr offset="-#off#">&LIGHTNING_DIR;/vaisala/netcdf/@y@j@H@M0005r</cyclestr></datadep>

          <timedep><cyclestr offset="&START_TIME_RADAR;">@Y@m@d@H@M00</cyclestr></timedep>

        </or>

      </dependency>

  </task>

  </metatask>

  <metatask>

  <var name="min">15 30 45 60 00 </var>

  <var name="mm1">13 29 44 57 00 </var>

  <var name="mm2">14 30 45 58 01 </var>

  <var name="mm3">15 31 46 59 02 </var>

  <var name="offset">-1:00:00 -1:00:00 -1:00:00 -1:00:00 00:00:00</var>

  <task name="radar_links_#min#" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &RADAR_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_RADAR_LINKS</command>

    <cores>&RADARLINKS_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_radarlinks_#min#_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/radar_links_#min#_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr offset="-01:00:00">@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>NSSL</name>

      <value>&RADAR_DIR;</value>

    </envar>

    <envar>

      <name>SUBH_TIME</name>

      <value>#min#</value>

    </envar>

    <dependency>

      <or>
      <!-- for testing correctness of cyclestr in the ruby snippet

      <sh> ls <cyclestr offset="-#offset#">&RADAR_DIR;/@Y@m@d-@H#mm1#*.MRMS_MergedReflectivityQC_00.50_@Y@m@d-@H*.grib2</cyclestr> </sh>

      -->

      <rb><cyclestr offset="#offset#">!Dir.glob('&RADAR_DIR;/@Y@m@d-@H#mm1#*.MRMS_MergedReflectivityQC_00.50_@Y@m@d-@H*.grib2').empty?</cyclestr></rb>

      <rb><cyclestr offset="#offset#">!Dir.glob('&RADAR_DIR;/@Y@m@d-@H#mm2#*.MRMS_MergedReflectivityQC_00.50_@Y@m@d-@H*.grib2').empty?</cyclestr></rb>

      <rb><cyclestr offset="#offset#">!Dir.glob('&RADAR_DIR;/@Y@m@d-@H#mm3#*.MRMS_MergedReflectivityQC_00.50_@Y@m@d-@H*.grib2').empty?</cyclestr></rb>

    </or>

    </dependency>

  </task>
  </metatask>

<!-- running radar_gsi for "00" step -->

    <task name="radar_gsi" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &RADAR_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_RADAR</command>

    <cores>&RADAR_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_radar_@H_00</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/radar_gsi_@Y@m@d@H00_00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>SUBH_TIME</name>

      <value>00</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>FIX_ROOT</name>

      <value>&FIX_ROOT;</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/WPS</value>

    </envar>

    <envar>

      <name>NSSL</name>

      <value>&RADAR_DIR;</value>

    </envar>

    <dependency>

      <or>

        <datadep><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/filelist_mrms</cyclestr></datadep>

        <timedep><cyclestr offset="&START_TIME_RADAR;">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

  </task>


<!-- still running radar_gsi for "60" step as a backup if "00" step fails -->

  <metatask>

<!-- <var name="subh">15 30 45 60</var> -->

    <var name="subh">60</var>

    <task name="radar_gsi_#subh#" cycledefs="02-11hr,00hr,01hr" maxtries="3">

      &RADAR_RESOURCES;

      &WALL_LIMIT_DA;

      &RESERVATION;

      &ENVARS;

      <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_RADAR</command>

      <cores>&RADAR_PROC;</cores>

      <jobname><cyclestr>&JOBNAME_PRE;_radar_@H_#subh#</cyclestr></jobname>

      <join><cyclestr>&LOG_DIR;/radar_gsi_@Y@m@d@H00_#subh#.log</cyclestr></join>

      <envar>

        <name>START_TIME</name>

        <value><cyclestr offset="-01:00:00">@Y@m@d@H</cyclestr></value>

      </envar>

      <envar>

        <name>SUBH_TIME</name>

        <value>#subh#</value>

      </envar>

      <envar>

        <name>DATAROOT</name>

        <value>&DATAROOT;</value>

      </envar>

      <envar>

        <name>DATAHOME</name>

        <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/#subh#</cyclestr></value>

      </envar>

      <envar>

        <name>GSI_ROOT</name>

        <value>&GSI_ROOT;</value>

      </envar>

      <envar>

        <name>FIX_ROOT</name>

        <value>&FIX_ROOT;</value>

      </envar>

      <envar>

        <name>STATIC_DIR</name>

        <value>&STATIC_DIR;/WPS</value>

      </envar>

      <envar>

        <name>NSSL</name>

        <value>&RADAR_DIR;</value>

      </envar>

      <dependency>

        <or>

          <datadep><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/#subh#/filelist_mrms</cyclestr></datadep>

          <timedep><cyclestr offset="&START_TIME_RADAR;">@Y@m@d@H@M00</cyclestr></timedep>

        </or>

      </dependency>

    </task>

  </metatask>

  <task name="satellite_gsi_bufr" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &SATELLITE_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_SATELLITE_BUFR</command>

    <cores>&SATELLITE_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_satellite_bufr_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/satellite_gsi_bufr_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>FIX_ROOT</name>

      <value>&FIX_ROOT;</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/WPS</value>

    </envar>

    <envar>

      <name>NASALARC_DATA</name>

      <value>&LANGLEY_BUFR_DIR;</value>

    </envar>

    <dependency>

      <or>

        <datadep><cyclestr>&LANGLEY_BUFR_DIR;/@Y@j@H00.rap.t@Hz.lgycld.tm00.bufr_d</cyclestr></datadep>

        <timedep><cyclestr offset="&START_TIME_CONVENTIONAL;">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

  </task>

  <task name="conventional_gsi" cycledefs="02-11hr,01hr" maxtries="3">

    &CONVENTIONAL_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_CONVENTIONAL</command>

    <cores>&CONVENTIONAL_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_conventional_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/conventional_gsi_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>EARLY</name>

      <value>0</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>PREPBUFR</name>

      <value>&PREPBUFR_DIR;</value>

    </envar>

    <envar>

      <name>PREPBUFR_SAT</name>

      <value>&PREPBUFR_SAT_DIR;</value>

    </envar>

    <envar>

      <name>BUFRLIGHTNING</name>

      <value>&BUFRLIGHTNING_DIR;</value>

    </envar>

    <envar>

      <name>RADVELLEV2_DIR</name>

      <value>&RADVELLEV2_DIR;</value>

    </envar>

    <envar>

      <name>RADVELLEV2P5_DIR</name>

      <value>&RADVELLEV2P5_DIR;</value>

    </envar>

    <envar>

      <name>SATWND_DIR</name>

      <value>&SATWND_DIR;</value>

    </envar>

    <envar>

      <name>TAMDAR_ROOT</name>

      <value>&TAMDAR_DIR;</value>

    </envar>

    <envar>

      <name>NACELLE_RSD</name>

      <value>&NACELLE_RSD;</value>

    </envar>

    <envar>

      <name>TOWER_RSD</name>

      <value>&TOWER_RSD;</value>

    </envar>

    <envar>

      <name>TOWER_NRSD</name>

      <value>&TOWER_NRSD;</value>

    </envar>

    <envar>

      <name>SODAR_NRSD</name>

      <value>&SODAR_NRSD;</value>

    </envar>

    <envar>

      <name>TCVITALS_DIR</name>

      <value>&TCVITALS_DIR;</value>

    </envar>

    <envar>

      <name>STICKNET</name>

      <value>&STICKNET_DIR;</value>

    </envar>

    <dependency>

      <or>

        <and>

          <datadep><cyclestr>&PREPBUFR_DIR;/@Y@j@H00.rap.t@Hz.prepbufr.tm00.@Y@m@d</cyclestr></datadep>

          <datadep><cyclestr>&SODAR_NRSD;/@y@j@H000015o</cyclestr></datadep>

        </and>

        <timedep><cyclestr offset="&START_TIME_CONVENTIONAL;">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

  </task>

  <task name="conventional_gsi_early" cycledefs="00hr" maxtries="3">

    &CONVENTIONAL_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_CONVENTIONAL</command>

    <cores>&CONVENTIONAL_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_conventional_early_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/conventional_gsi_early_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>EARLY</name>

      <value>1</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>PREPBUFR</name>

      <value>&PREPBUFR_DIR;</value>

    </envar>

    <envar>

      <name>PREPBUFR_SAT</name>

      <value>&PREPBUFR_SAT_DIR;</value>

    </envar>

    <envar>

      <name>BUFRLIGHTNING</name>

      <value>&BUFRLIGHTNING_DIR;</value>

    </envar>

    <envar>

      <name>RADVELLEV2_DIR</name>

      <value>&RADVELLEV2_DIR;</value>

    </envar>

    <envar>

      <name>RADVELLEV2P5_DIR</name>

      <value>&RADVELLEV2P5_DIR;</value>

    </envar>

    <envar>

      <name>SATWND_DIR</name>

      <value>&SATWND_DIR;</value>

    </envar>

    <envar>

      <name>TAMDAR_ROOT</name>

      <value>&TAMDAR_DIR;</value>

    </envar>

    <envar>

      <name>NACELLE_RSD</name>

      <value>&NACELLE_RSD;</value>

    </envar>

    <envar>

      <name>TOWER_RSD</name>

      <value>&TOWER_RSD;</value>

    </envar>

    <envar>

      <name>TOWER_NRSD</name>

      <value>&TOWER_NRSD;</value>

    </envar>

    <envar>

      <name>SODAR_NRSD</name>

      <value>&SODAR_NRSD;</value>

    </envar>

    <envar>

      <name>TCVITALS_DIR</name>

      <value>&TCVITALS_DIR;</value>

    </envar>

    <envar>

      <name>STICKNET</name>

      <value>&STICKNET_DIR;</value>

    </envar>

    <dependency>

      <or>

        <and>

          <datadep><cyclestr>&PREPBUFR_DIR;/@Y@j@H00.rap_e.t@Hz.prepbufr.tm00.@Y@m@d</cyclestr></datadep>

          <datadep><cyclestr>&SODAR_NRSD;/@y@j@H000015o</cyclestr></datadep>

        </and>

        <timedep><cyclestr offset="&START_TIME_CONVENTIONAL;">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

  </task>

  <task name="gsi_hyb" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &GSI_HYB_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION_DA;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_GSI_HYB</command>

    <cores>&GSI_HYB_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_gsi_hyb_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/gsi_hyb_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>SYSTEM_ID</name>

      <value>&SYSTEM_ID;</value>

    </envar>

    <envar>

      <name>GSIPROC</name>

      <value>&GSI_HYB_PROC;</value>

    </envar>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>FULLCYC</name>

      <value>1</value>

    </envar>

    <envar>

      <name>DATABASE_DIR</name>

      <value>&DATABASE_DIR;</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME_BK</name>

      <value><cyclestr offset="-1:00:00">&HRRR_DIR;/@Y@m@d@H/wrfprd</cyclestr></value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/gsiprd</cyclestr></value>

    </envar>

    <envar>

      <name>DATAOBSHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <envar>

      <name>FIX_ROOT</name>

      <value>&FIX_ROOT;</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;</value>

    </envar>

    <envar>

      <name>AIRCRAFT_REJECT</name>

      <value>&AIRCRAFT_REJECT;</value>

    </envar>

    <envar>

      <name>SFCOBS_USELIST</name>

      <value>&SFCOBS_USELIST;</value>

    </envar>

    <envar>

      <name>PREPBUFR</name>

      <value>&PREPBUFR_DIR;</value>

    </envar>

    <envar>

      <name>NCEPSNOW</name>

      <value>&NCEPSNOW_DIR;</value>

    </envar>

    <envar>

      <name>SST_ROOT</name>

      <value>&HIGHRES_SST_DIR;</value>

    </envar>

    <envar>

      <name>SST_ROOT14km</name>

      <value>&HIGHRES_SST14KM_DIR;</value>

    </envar>

    <envar>

      <name>ENKF_FCST</name>

      <value>&ENKFFCST_DIR;</value>

    </envar>

    <dependency>

      <and>

          <datadep>&HRRR_DIR;/<cyclestr offset="-1:00:00">@Y@m@d@H</cyclestr>/wrfprd/<cyclestr>wrfout_d01_@Y-@m-@d_@H_00_00</cyclestr></datadep>

        <or> 

          <and>

            <or>

              <datadep age="00:02:00"><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/NSSLRefInGSI.bufr</cyclestr></datadep>

              <datadep age="00:02:00"><cyclestr>&DATAROOT;/@Y@m@d@H/obsprd/60/NSSLRefInGSI.bufr</cyclestr></datadep>

            </or>

            <taskdep task="lightning_gsi_60"/>

            <taskdep task="satellite_gsi_bufr"/>

            <or>

              <taskdep task="conventional_gsi"/>

              <taskdep task="conventional_gsi_early"/>

            </or>

          </and>

          <timedep><cyclestr offset="&START_TIME_GSI;">@Y@m@d@H@M00</cyclestr></timedep>

        </or>

      </and>

    </dependency>

  </task>

  <task name="gsi_diag" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &GSI_DIAG_RESOURCES;

    &WALL_LIMIT_DA;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_GSI_DIAG</command>

    <cores>&GSI_DIAG_PROC;</cores>

    <jobname><cyclestr>gsi_diag_@H</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/gsi_diag_@Y@m@d@H00.log</cyclestr></join>

    <envar>

      <name>GSIPROC</name>

      <value>&GSI_DIAG_PROC;</value>

    </envar>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATABASE_DIR</name>

      <value>&DATABASE_DIR;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>GSI_ROOT</name>

      <value>&GSI_ROOT;</value>

    </envar>

    <dependency>

      <taskdep task="gsi_hyb"/>

    </dependency>

  </task>

  <task name="post_00" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &POST_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_UNIPOST</command>

    <cores>&POST_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_post_@H_00</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/post_@Y@m@d@H00_00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>FCST_TIME</name>

      <value>00</value>

    </envar>

    <envar>

      <name>STATICWRF_DIR</name>

      <value>&STATIC_DIR;/WRF</value>

    </envar>

    <envar>

      <name>WRF_ROOT</name>

      <value>&WRF_ROOT;</value>

    </envar>

    <envar>

      <name>EXE_ROOT</name>

      <value>&UNIPOST_EXEC;</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/postprd</cyclestr></value>

    </envar>

    <envar>

      <name>DATAWRFHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/gsiprd</cyclestr></value>

    </envar>

    <envar>

      <name>MODEL</name>

      <value>RAP</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/UPP</value>

    </envar>

    <envar>

      <name>POST_NAME</name>

      <value>&POST_NAME;</value>

    </envar>

    <dependency>

      <taskdep task="gsi_hyb"/>

    </dependency>

  </task>

  <task name="smartinit_bl" cycledefs="02-11hr,02hr,03hr,04hr,05hr,06hr,07hr,08hr,09hr,10hr,11hr,00hr" maxtries="3">

    &SMARTINIT_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION_SMARTINIT;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_SMARTINIT_BL</command>

    <cores>&SMARTINIT_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_smartinit_@H_bl</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/smartinit_@Y@m@d@H00_bl.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>FCST_TIME</name>

      <value>00</value>

    </envar>

    <envar>

      <name>EXE_ROOT</name>

      <value>&SMARTINIT_EXEC;</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/smtiprd</cyclestr></value>

    </envar>

    <envar>

      <name>DATAPOSTHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/postprd</cyclestr></value>

    </envar>

    <envar>

      <name>MODEL</name>

      <value>HRRR</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/smartinit</value>

    </envar>

    <dependency>

      <datadep age="00:02:00">&DATAROOT;/<cyclestr>@Y@m@d@H</cyclestr>/postprd<cyclestr>/wrfnat_hrconus_00.grib2</cyclestr></datadep>

    </dependency>

  </task>

  <task name="smartinit_nb" cycledefs="02-11hr,02hr,03hr,04hr,05hr,06hr,07hr,08hr,09hr,10hr,11hr,00hr" maxtries="3">

    &SMARTINIT_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION_SMARTINIT;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_SMARTINIT_NB</command>

    <cores>&SMARTINIT_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_smartinit_@H_nb</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/smartinit_@Y@m@d@H00_nb.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>FCST_TIME</name>

      <value>00</value>

    </envar>

    <envar>

      <name>EXE_ROOT</name>

      <value>&SMARTINIT_EXEC;</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/smtiprd</cyclestr></value>

    </envar>

    <envar>

      <name>DATAPOSTHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H/postprd</cyclestr></value>

    </envar>

    <envar>

      <name>MODEL</name>

      <value>HRRR</value>

    </envar>

    <envar>

      <name>STATIC_DIR</name>

      <value>&STATIC_DIR;/smartinit</value>

    </envar>

    <dependency>

      <datadep age="00:02:00">&DATAROOT;/<cyclestr>@Y@m@d@H</cyclestr>/postprd<cyclestr>/wrfnat_hrconus_00.grib2</cyclestr></datadep>

    </dependency>

  </task>

  <task name="ncl_00" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &NCL_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION;

    &ENVARS;

    <command>&JJOB_DIR;/launch.ksh &JJOB_DIR;/JRTMA3D_NCL_HRRR</command>

    <cores>&NCL_MAIN_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_00</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_00.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>FCST_TIME</name>

      <value>00</value>

    </envar>

    <envar>

      <name>NCL_VER</name>

      <value>&NCL_VER;</value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>MODEL</name>

      <value>RTMA_3D</value>

    </envar>

    <dependency>

      <taskdep task="post_00"/>

    </dependency>

  </task>

  <metatask>

    <var name="skewt">skewt1 skewt2 skewt3 skewt4 skewt5</var>

    <task name="ncl_00_#skewt#" cycledefs="02-11hr,00hr,01hr" maxtries="3">

      &NCL_SKEWT_RESOURCES;

      &WALL_LIMIT_PP;

      &RESERVATION;

      <command>&SCRIPTS;/NCL/ncl_hrrr_#skewt#.ksh</command>

      <cores>&NCL_PROC;</cores>

      <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_00_#skewt#</cyclestr></jobname>

      <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_00_#skewt#.log</cyclestr></join>

      <envar>

        <name>START_TIME</name>

        <value><cyclestr>@Y@m@d@H</cyclestr></value>

      </envar>

      <envar>

        <name>FCST_TIME</name>

        <value>00</value>

      </envar>

      <envar>

        <name>NCL_VER</name>

        <value>&NCL_VER;</value>

      </envar>

      <envar>

        <name>DATAROOT</name>

        <value>&DATAROOT;</value>

      </envar>

      <envar>

        <name>DATAHOME</name>

        <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

      </envar>

      <envar>

        <name>MODEL</name>

        <value>RTMA_3D</value>

      </envar>

      <dependency>

        <taskdep task="post_00"/>

      </dependency>

    </task>

  </metatask>

  <metatask>

    <var name="htxs">htxs1</var>

    <task name="ncl_00_#htxs#" cycledefs="02-11hr,00hr,01hr" maxtries="3">

      &NCL_HTXS_RESOURCES;

      &WALL_LIMIT_PP;

      &RESERVATION;

      <command>&SCRIPTS;/NCL/ncl_hrrr_#htxs#.ksh</command>

      <cores>&NCL_PROC;</cores>

      <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_00_#htxs#</cyclestr></jobname>

      <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_00_#htxs#.log</cyclestr></join>

      <envar>

        <name>START_TIME</name>

        <value><cyclestr>@Y@m@d@H</cyclestr></value>

      </envar>

      <envar>

        <name>FCST_TIME</name>

        <value>00</value>

      </envar>

      <envar>

        <name>NCL_VER</name>

        <value>&NCL_VER;</value>

      </envar>

      <envar>

        <name>DATAROOT</name>

        <value>&DATAROOT;</value>

      </envar>

      <envar>

        <name>DATAHOME</name>

        <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

      </envar>

      <envar>

        <name>MODEL</name>

        <value>RTMA_3D</value>

      </envar>

      <dependency>

        <taskdep task="post_00"/>

      </dependency>

    </task>

  </metatask>

  <task name="ncl_zip" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &NCL_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION;

    <command>&SCRIPTS;/NCL/ncl_hrrr_zip.ksh</command>

    <cores>&NCL_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_zip</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_zip.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

    </envar>

    <dependency>

      <or>

        <taskdep task="ncl_00"/>

        <timedep><cyclestr offset="03:00:00">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

    

  </task>

  <task name="ncl_skewt_zip" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &NCL_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION;

    <command>&SCRIPTS;/NCL/ncl_hrrr_skewt_zip.ksh</command>

    <cores>&NCL_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_skewt_zip</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_skewt_zip.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

    </envar>

    <dependency>

      <or>

        <and>

          <taskdep task="ncl_00_skewt1"/>

          <taskdep task="ncl_00_skewt2"/>

          <taskdep task="ncl_00_skewt3"/>

          <taskdep task="ncl_00_skewt4"/>

          <taskdep task="ncl_00_skewt5"/>

        </and>

        <timedep><cyclestr offset="03:00:00">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

    

  </task>

  <task name="ncl_htxs_zip" cycledefs="02-11hr,00hr,01hr" maxtries="3">

    &NCL_RESOURCES;

    &WALL_LIMIT_PP;

    &RESERVATION;

    <command>&SCRIPTS;/NCL/ncl_hrrr_htxs_zip.ksh</command>

    <cores>&NCL_PROC;</cores>

    <jobname><cyclestr>&JOBNAME_PRE;_ncl_@H_htxs_zip</cyclestr></jobname>

    <join><cyclestr>&LOG_DIR;/ncl_@Y@m@d@H00_htxs_zip.log</cyclestr></join>

    <envar>

      <name>START_TIME</name>

      <value><cyclestr>@Y@m@d@H</cyclestr></value>

    </envar>

    <envar>

      <name>DATAROOT</name>

      <value>&DATAROOT;</value>

    </envar>

    <envar>

      <name>DATAHOME</name>

      <value><cyclestr>&DATAROOT;/@Y@m@d@H</cyclestr></value>

    </envar>

    <dependency>

      <or>

        <and>

          <taskdep task="ncl_00_htxs1"/>

        </and>

        <timedep><cyclestr offset="02:55:00">@Y@m@d@H@M00</cyclestr></timedep>

      </or>

    </dependency>

    

  </task>

EOF

#

# adding Ending Mark to the end of WORKFLOW TASK Definition Block

#

cat >> ${NWROOT}/xml/${XML_FNAME} <<EOF 

</workflow>

EOF

########################################################################################

#   Done building xml file 

########################################################################################

########################################################################################

#   Set up and create the directories for running and log files

########################################################################################

if [ ! -d $ptmp_base ]; then

  mkdir -p $ptmp_base

fi

export DATAROOT=$ptmp_base/data

if [ ! -d $DATAROOT ]; then

  mkdir -p $DATAROOT

fi

export COMROOT=$ptmp_base/com2

if [ ! -d $COMROOT ]; then

  mkdir -p $COMROOT

fi

if [ ! -d  $ptmp_base/run ] ; then

    mkdir $ptmp_base/run

fi

if [ ! -d  $ptmp_base/log ] ; then

    mkdir $ptmp_base/log

fi

if [ ! -d  $ptmp_base/loghistory ] ; then

    mkdir $ptmp_base/loghistory

fi

export LOGDIR=$NWROOT/xml/logs

if [ ! -d $LOGDIR ] ; then

  mkdir -p $LOGDIR

fi

export JLOGFILES=$LOGDIR/jlogfiles

if [ ! -d $JLOGFILES ] ; then

  mkdir -p $JLOGFILES

fi

export PGMOUT=$LOGDIR/pgmout

if [ ! -d $PGMOUT ] ; then

  mkdir -p $PGMOUT

fi

#

#   Creating script to submit the workflow of 3DRTMA 

#

if [ ${MACHINE} = 'theia' ] || [ ${MACHINE} = 'jet' ]; then

  cat > ${NWROOT}/xml/${run_scriptname} <<EOF 

#!/bin/bash

. /etc/profile

. /etc/profile.d/modules.sh >/dev/null # Module Support

module purge

module load intel

module load rocoto/1.3.0-RC5

EOF

  if [ ${SCHEDULER} = "SLURM" ] || [ ${SCHEDULER} = "slurm" ]; then

    cat >> ${NWROOT}/xml/${run_scriptname} <<EOF 

module load slurm/18.08.7p1

EOF

  fi

  cat >> ${NWROOT}/xml/${run_scriptname} <<EOF 

rocotorun -v 10 -w ${NWROOT}/xml/${XML_FNAME} -d ${NWROOT}/xml/${DB_FNAME}

EOF

  cat > ${NWROOT}/xml/${chk_scriptname} <<EOF 

#!/bin/bash

. /etc/profile

. /etc/profile.d/modules.sh >/dev/null # Module Support

module purge

module load intel

module load rocoto/1.3.0-RC5

EOF

  if [ ${SCHEDULER} = "SLURM" ] || [ ${SCHEDULER} = "slurm" ]; then

    cat >> ${NWROOT}/xml/${chk_scriptname} <<EOF 

module load slurm/18.08.7p1

EOF

  fi

  cat >> ${NWROOT}/xml/${chk_scriptname} <<EOF 

subhr="00"

timewindow=\$1

timewindow=\${timewindow:-"6"}

date1=\`date +%Y%m%d%H -d "now"\`

date1="\${date1}\${subhr}"

date0=\`date +%Y%m%d%H -d "\${timewindow} hour ago"\`

date0="\${date0}\${subhr}"

rocotostat -v 10 -w ${NWROOT}/xml/${RUN}_${expname}.xml -d ${NWROOT}/xml/${DB_FNAME} -c \${date0}:\${date1}

EOF

fi

chmod 744 ${NWROOT}/xml/${run_scriptname}

chmod 744 ${NWROOT}/xml/${chk_scriptname}

echo "RTMA3D is ready to go! Make sure your xml file has consistent directory settings!"

echo "Using ${run_scriptname} to run workflow "

echo "Using ${chk_scriptname} to check status of workflow "

set +x

# clean up the tmp working directory

cd ${TMP_WRKDIR}

cd ../

rm -rf ${TMP_WRKDIR}

exit 
