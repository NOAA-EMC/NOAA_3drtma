#!/bin/bash

#This script preps directories for ROCOTO-controlled RTMA/URMA real time and retro runs.
#
####################################################################################################
#
#                                     Detect the Machine/Platform
#   Note: either by testing the unique filesystem
#                ( if -d /scratch, /mnt/lfs, etc.)
#             or by testing the unique cpu info in /proc/cpuinfo (Haswell chip E5-2690 v3) 
#                ( if `grep -c 'E5-2690 v3' /proc/cpuinfo` -gt 0 )
#
####################################################################################################
    if [[ -d /ioddev_dell ]]; then
       conf_target=nco
       MACHINE=dell
       export SCHEDULER="LSF"
    else
       echo 'Script only runs on the dell machines. Exiting....'
       exit 0
    fi
echo 'Running on $MACHINE '

#
#--- detect existence of directory scripts/
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
    echo " found the rtma3d root directory is $TOP_RTMA"
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

#####################################################
#--- User defined variables                         #
#####################################################
set -x

export ExpDateWindows="24 06 2019 *"        # dd mm yyyy weekday (crontab-like date format, mainly used for real-time run)
export startCDATE=201904271200              #yyyymmddhhmm - Starting day of retro run 
export endCDATE=201904271400                #yyyymmddhhmm - Ending day of RTMA3D run (needed for both RETRO and REAL TIME). 
export NET=rtma3d                           #selection of rtma3d (or rtma,urma)
export RUN=rtma3d                           #selection of rtma3d (or rtma,urma)
export run_envir="dev_shared"                      #
export NWROOT=${TOP_RTMA}                   #root directory for RTMA/URMA j-job scripts, scripts, parm files, etc. 
export SCHD_ATTRB="lsf"

export envir="${SCHD_ATTRB}"                      #environment (test, prod, dev, etc.)
export expname="${envir}"                   # experiment name
export realtime="T"
#====================================================================#
# Note: Definition for the following variables 
#       depends on the machine platform, 
#       and different user and/or experiment.
#====================================================================#


  QUEUE=dev_shared                        #user-specified processing queue
  QUEUE_DBG="debug"                    #user-specified processing queue -- debug
  QUEUE_SVC="dev_transfer"                  #user-specified transfer queue

# Path to top running and archiving directory
  ptmp_base="/gpfs/dell2/stmp/${USER}/${NET}_wrkdir_retro"

  DATABASE_DIR=${ptmp_base}            # (equivalent to ptmp_base)
  HOMEBASE_DIR=${NWROOT}               # path to system home directory
  COMINRAP="/gpfs/hps/nco/ops/com/rap/prod"
  COMINRAP_E="/gpfs/hps/nco/ops/com/rap/prod"
  COMINHRRR="/gpfs/hps/nco/ops/com/hrrr/prod"
  GESINHRRR="/gpfs/hps/nco/ops/nwges/prod"
# Computational resources
  ACCOUNT="RTMA-T2O"                    #account for CPU resources

  RESERVATION="<native>-R rusage[mem=2000] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"
  RESERVATION_GSI="<native>-R rusage[mem=1900] -R span[ptile=14] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"
  RESERVATION_UPP="<native>-R rusage[mem=3300] -R span[ptile=8] -R affinity[core]</native><queue>&QUEUE;</queue><account>&ACCOUNT;</account>"
  RESERVATION_SVC="<native>-R rusage[mem=1000] -R affinity[core]</native><queue>&QUEUE_SVC;</queue><account>&ACCOUNT;</account>"

# General definition of computation resources for each task
  OBSPREP_RADAR_PROC="1"
  OBSPREP_RADAR_RESOURCES="<cores>&OBSPREP_RADAR_PROC;</cores><walltime>00:30:00</walltime>"
  OBSPREP_RADAR_RESERVATION=${RESERVATION}

  OBSPREP_LGHTN_PROC="1"
  OBSPREP_LGHTN_RESOURCES="<cores>&OBSPREP_LGHTN_PROC;</cores><walltime>00:30:00</walltime>"
  OBSPREP_LGHTN_RESERVATION=${RESERVATION}

  OBSPREP_CLOUD_PROC="1"
  OBSPREP_CLOUD_RESOURCES="<cores>&OBSPREP_CLOUD_PROC;</cores><walltime>00:30:00</walltime>"
  OBSPREP_CLOUD_RESERVATION=${RESERVATION}

  PREPOBS_PROC="1"
  PREPOBS_RESOURCES="<cores>&PREPOBS_PROC;</cores><walltime>00:30:00</walltime>"
  PREPOBS_RESERVATION=${RESERVATION}

  PREPFGS_PROC="1"
  PREPFGS_RESOURCES="<cores>&PREPFGS_PROC;</cores><walltime>00:30:00</walltime>"
  PREPFGS_RESERVATION=${RESERVATION}

  GSI_PROC="192"
  GSI_THREADS=1
  GSI_OMP_STACKSIZE="512M"
  GSI_RESOURCES="<cores>&GSI_PROC;</cores><walltime>00:30:00</walltime>"
  GSI_RESERVATION=${RESERVATION_GSI}

  POST_PROC="96"
  POST_THREADS=1
  POST_OMP_STACKSIZE="512MB"
  POST_RESOURCES="<cores>&POST_PROC;</cores><walltime>00:30:00</walltime>"
  POST_RESERVATION=${RESERVATION_UPP}

  PLOT_PROC="1"
  PLOT_RESOURCES="<cores>&PLOT_PROC;</cores><walltime>00:30:00</walltime><memory>3G</memory>"
  PLOT_RESERVATION=${RESERVATION}

  VERIF_PROC="1"
  VERIF_RESOURCES="<cores>&VERIF_PROC;</cores><walltime>00:30:00</walltime><memory>3G</memory>"
  VERIF_RESERVATION=${RESERVATION}

# if [[ ! -d ${ptmp_base} ]] ; then
#     echo " ${ptmp_base} does NOT exist !"
#     echo " Please define the variable and create this directory."
#     echo " Abort! "
#     exit 1
# fi

export CAP_NET=`echo ${NET} | tr '[:lower:]' '[:upper:]'`
export CAP_RUN=`echo ${RUN} | tr '[:lower:]' '[:upper:]'`
export CAP_ENVIR=`echo ${envir} | tr '[:lower:]' '[:upper:]'`
export CAP_RUN_ENVIR=`echo ${run_envir} | tr '[:lower:]' '[:upper:]'`

#########################################################
#
# User defined executable file name for each task
#
#########################################################
export exefile_name_gsi="rtma3d_gsi"
export exefile_name_post="rtma3d_wrfpost"
export exefile_name_radar="rtma3d_process_mosaic"
export exefile_name_lightning="rtma3d_process_lightning"
export exefile_name_cloud="rtma3d_process_cloud"
export exefile_name_verif=""    # executable of verification (MET) is defined by loading module met
#########################################################
#--- define the path to the static data
#    fix/
#      gsi/: fixed data, e.g., statistical file of B-Matrix)
#      crtm/: (CRTM coefficients)
#      wrf/
#      wps/: e.g., geo_em.d01.nc used in obs pre-processing to model grid.
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
#########################################################
# Note: the following absolute path for static data are only valid for Theia.
#       User can specify the path to use user's static data.
#       The variable name with "_udef" means: user may define the path to their
#       specific static data, 
#       then link these paths to the symbol links under fix/ and parm/.
#

   export FIXgsi_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/GSI-fix"
   export FIXcrtm_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/CRTM-fix"
   export FIXwps_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/wps"

   export OBS_USELIST_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/obsuselist"
   export SFCOBS_USELIST_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/obsuselist/mesonet_uselists"
   export AIRCRAFT_REJECT_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/obsuselist/amdar_reject_lists"
   export SFCOBS_PROVIDER_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/obsuselist/sfcobs_provider"
   
   export PARMgsi_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/parm/gsi"
   export PARMupp_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/parm/upp"
   export PARMwrf_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/parm/wrf"
   export PARMverf_udef="/gpfs/dell2/emc/modeling/noscrub/Edward.Colon/FixData/parm/verif" 


#       define the variable names for symbol links under fix/ and parm/

  export FIXrtma3d="${NWROOT}/fix"
  export FIXgsi="${FIXrtma3d}/gsi"
  export FIXcrtm="${FIXrtma3d}/crtm"
  export FIXwps="${FIXrtma3d}/wps"

  export OBS_USELIST="${FIXrtma3d}/obsuselist"
  export SFCOBS_USELIST="${OBS_USELIST}/mesonet_uselists"
  export AIRCRAFT_REJECT="${OBS_USELIST}/amdar_reject_lists"
  export SFCOBS_PROVIDER="${OBS_USELIST}/sfcobs_provider"

  export PARMrtma3d="${NWROOT}/parm"
  export PARMgsi="${PARMrtma3d}/gsi"
  export PARMupp="${PARMrtma3d}/upp"
  export PARMwrf="${PARMrtma3d}/wrf"
  export PARMverf="${PARMrtma3d}/verif"




#
#        link to the symbol links
#

  if [ ! -d ${FIXrtma3d}   ] ; then mkdir -p ${FIXrtma3d}   ; fi
  if [ ! -d ${PARMrtma3d}  ] ; then mkdir -p ${PARMrtma3d}  ; fi
  if [ ! -d ${OBS_USELIST} ] ; then mkdir -p ${OBS_USELIST} ; fi
  if [ ${MACHINE} = 'theia' ] || [ ${MACHINE} = 'jet' ] || [ ${MACHINE} = 'dell' ] ; then
    cd ${FIXrtma3d}
    echo " linking fixed data on ${MACHINE} for GSI analysis"
    rm -rf $FIXgsi
    echo " ln -sf ${FIXgsi_udef}        ${FIXgsi}"
    ln -sf ${FIXgsi_udef}        ${FIXgsi}
    rm -rf $FIXcrtm
    echo " ln -sf ${FIXcrtm_udef}       ${FIXcrtm}"
    ln -sf ${FIXcrtm_udef}       ${FIXcrtm}
    rm -rf $FIXwps
    echo " ln -sf ${FIXwps_udef}        ${FIXwps}"
    ln -sf ${FIXwps_udef}        ${FIXwps}

    cd ${OBS_USELIST}
    rm -rf $SFCOBS_USELIST
    echo " ln -sf ${SFCOBS_USELIST_udef}        ${SFCOBS_USELIST}"
    ln -sf ${SFCOBS_USELIST_udef}        ${SFCOBS_USELIST}
    rm -rf $AIRCRAFT_REJECT
    echo " ln -sf ${AIRCRAFT_REJECT_udef}       ${AIRCRAFT_REJECT}"
    ln -sf ${AIRCRAFT_REJECT_udef}       ${AIRCRAFT_REJECT}
    rm -rf $SFCOBS_PROVIDER
    echo " ln -sf ${SFCOBS_PROVIDER_udef}       ${SFCOBS_PROVIDER}"
    ln -sf ${SFCOBS_PROVIDER_udef}       ${SFCOBS_PROVIDER}

    cd ${PARMrtma3d}
    rm -rf $PARMgsi
    echo " ln -sf ${PARMgsi_udef}        ${PARMgsi}"
    ln -sf ${PARMgsi_udef}               ${PARMgsi}
    rm -rf $PARMupp
    echo " ln -sf ${PARMupp_udef}        ${PARMupp}"
    ln -sf ${PARMupp_udef}               ${PARMupp}
    rm -rf $PARMverf
    echo " ln -sf ${PARMverf_udef}       ${PARMverf}"
    ln -sf ${PARMverf_udef}              ${PARMverf}
    rm -rf $PARMwrf
    echo " ln -sf ${PARMwrf_udef}        ${PARMwrf}"
    ln -sf ${PARMwrf_udef}               ${PARMwrf}

  else
    echo " the fixed data directories have not set up yet for machine ${MACHINE}."
    echo " Abort linking task."
    exit 9
  fi

  echo
  ls -ltr $FIXrtma3d
  echo
  echo
  ls -ltr $OBS_USELIST
  echo
  echo
  ls -ltr $PARMrtma3d
  echo
  echo

#
#--- option control for obs pre-processing (esp. for obs used in cloud analysis)
#
################################################################################
#THESE SETTINGS ARE SPECIFIC TO DELL REAL-TIME 3D RTMA. DO NOT CHANGE.
################################################################################



  export obsprep_radar=0  # 0: No (using archived hrrr.t{HH}z.NSSLRefInGSI.bufr processed in operational hrrr run)
                          # 1: pre-processing MRMS grib2 radar reflectivity obs

  export obsprep_lghtn=1  # 0: No pre-processing lightning obs data
                          # 1: processing archived bufr data (rap.t{HH}z.lghtng.tm00.bufr_d) from operation RAP run to HRRR grid
                          # 2: processing  NLDN lightning data (if retrospective run, also retrieving  NLDN data from HPSS)
                          # 3: processing ENTLN lightning data (if retrospective run, also retrieving ENTLN data from HPSS)

  export obsprep_cloud=0  # 0: No (using archived hrrr.t{HH}z.NASALaRCCloudInGSI.bufr processed in operational hrrr)
                          # 1: processing bufr data from rap run

#
#--- option to plot the firstguess/analysis/increment
  export run_plt=0        # default is 1 to plot with GrADS
                          # >0: plot (and post-process of firstguess fields)
                          # =1: plot with GrADS 
                          # =2: plot with NCL (not available yet)
                          # =3: plot with Python (not available yet)
                          #<=0: no plot (and no post-process of firstguess fields)

# control option for using hrrr forecast as firstguess for rtma3d
  export fgs_opt=1        # 1: hrrr.t{HH}z.wrfguess   (1 hr forecast to analysis time from wrfguess_rap)
                          # 1: recommended and default
                          # 2: wrfguess_rap (directly downscaled from RAP to HRRR grid  at 1 hr before analysis time)
                          # 2: Not recommended (missing some hydrometer information and leading to failure of UPP on CEIL)

#
#--- option for two-step gsi analysis (var + cloud analysis in two steps)
#
  export gsi_2steps=0     # default is single step (var + cloud anl in one step)
                          # 1: two-step analysis

  export gsi2=""
  export gsi_grid_ratio_in_var=1
  export gsi_grid_rario_in_cldanl=1
  if [ $gsi_2steps -eq 1 ]
  then
    export gsi2="2"
    export gsi_grid_ratio_in_var=1   # can be 4 if running hybrid to save time
    export gsi_grid_rario_in_cldanl=1
  fi 

#
#--- Computational Resources
#
########################################################################################
#
#             User definition section ends here.
#             User definition section ends here.
#             User definition section ends here.
#
# Workflow is specified using user-derived settings in xml format    
########################################################################################

rm -f ${NWROOT}/xml/${RUN}_${expname}_rt.xml
cat > ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 
<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE workflow [

<!-- Key variables -->
<!-- date/time of cases or cycles -->
<!ENTITY startCDATE     "${startCDATE}">
<!ENTITY endCDATE       "${endCDATE}">
<!ENTITY realtime       "${realtime}">
<!ENTITY NET		"${NET}">
<!ENTITY RUN		"${RUN}">
<!ENTITY envir		"${envir}">
<!ENTITY RUN_ENVIR	"${run_envir}">
<!ENTITY CAP_NET	"${CAP_NET}">
<!ENTITY CAP_RUN	"${CAP_RUN}">
<!ENTITY CAP_ENVIR	"${CAP_ENVIR}">
<!ENTITY CAP_RUN_ENVIR	"${CAP_RUN_ENVIR}">
<!ENTITY model		"&RUN;">

<!ENTITY MACHINE	"${MACHINE}">
<!ENTITY machine	"&MACHINE;">

<!ENTITY SCHEDULER      "${SCHEDULER}">

<!-- Variables Defined by absolute paths -->

<!ENTITY ptmp_base	"${ptmp_base}">
<!ENTITY COMINRAP       "${COMINRAP}">
<!ENTITY COMINRAP_E     "${COMINRAP_E}">
<!ENTITY COMINHRRR      "${COMINHRRR}">
<!ENTITY GESINHRRR      "${GESINHRRR}">
<!ENTITY NWROOT		"${NWROOT}">

<!ENTITY OBS_DIR	"/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/Data/GSD_GSI_Case/obs">
<!ENTITY HRRR_DIR	"/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/Data/GSD_GSI_Case/fgs">

<!--  -->

<!ENTITY DATAROOT	"&ptmp_base;/data">
<!ENTITY COMROOT	"&ptmp_base;/com2">
<!ENTITY DCOMROOT	"&ptmp_base;/dcom">
<!ENTITY GESROOT	"&ptmp_base;/nwges2/&NET;">

<!ENTITY HOMErtma3d	"&NWROOT;">
<!ENTITY LOG_DIR	"&HOMErtma3d;/xml/logs">
<!ENTITY JJOB_DIR	"&HOMErtma3d;/jobs">
<!ENTITY SCRIPT_DIR	"&HOMErtma3d;/scripts/">
<!ENTITY USHrtma3d	"&HOMErtma3d;/ush">
<!ENTITY UTILrtma3d	"&HOMErtma3d;/util">
<!ENTITY UTILrtma3d_dev	"&HOMErtma3d;/util_dev">
<!ENTITY MODULEFILES	"&HOMErtma3d;/modulefiles">
<!ENTITY EXECrtma3d	"&HOMErtma3d;/exec">
<!ENTITY PARMrtma3d	"${PARMrtma3d}">
<!ENTITY FIXrtma3d	"${FIXrtma3d}">

<!-- Specific Definition for static data -->
<!ENTITY FIXcrtm        "${FIXcrtm}">
<!ENTITY FIXgsi         "${FIXgsi}">
<!ENTITY FIXwps         "${FIXwps}">
<!ENTITY OBS_USELIST    "${OBS_USELIST}">
<!ENTITY AIRCRAFT_REJECT        "${AIRCRAFT_REJECT}">
<!ENTITY SFCOBS_USELIST         "${SFCOBS_USELIST}">
<!ENTITY SFCOBS_PROVIDER        "${SFCOBS_PROVIDER}">
<!ENTITY PARMwrf        "${PARMwrf}">
<!ENTITY PARMupp        "${PARMupp}">
<!ENTITY PARMgsi        "${PARMgsi}">
<!ENTITY PARMverf       "${PARMverf}">

<!ENTITY LOG_WRKFLW	"&LOG_DIR;">
<!ENTITY LOG_JJOB	"&LOG_DIR;/jlogfiles">
<!ENTITY LOG_SCHDLR	"&LOG_DIR;">
<!ENTITY LOG_PGMOUT     "&LOG_DIR;/pgmout">
<!ENTITY jlogfile       "&LOG_JJOB;/jlogfile_${expname}_rt.@Y@m@d@H">

<!-- definition of name of the top running directory for all tasks -->
<!ENTITY DATA_RUNDIR    "&DATAROOT;/&envir;/&RUN;.@Y@m@d@H@M">
<!--               names of the running directory for each task -->
<!-- Note: -->
<!--      these directories are just links pointing to real running directories -->
<!ENTITY DATA_GSIANL    "&DATA_RUNDIR;/gsiprd">
<!ENTITY DATA_OBSPRD    "&DATA_RUNDIR;/obsprd">
<!ENTITY DATA_FGSPRD    "&DATA_RUNDIR;/fgsprd">
<!ENTITY DATA_POST      "&DATA_RUNDIR;/postprd">
<!ENTITY DATA_POST4FGS  "&DATA_RUNDIR;/postprd4fgs">
<!ENTITY DATA_PLOTGRADS "&DATA_RUNDIR;/plotgrads">
<!ENTITY DATA_VERIF     "&DATA_RUNDIR;/verifprd">
<!ENTITY DATA_OBSPREP_LGHTN    "&DATA_RUNDIR;/obsprep_lghtn">
<!ENTITY DATA_OBSPREP_RADAR    "&DATA_RUNDIR;/obsprep_radar">
<!ENTITY DATA_OBSPREP_CLOUD    "&DATA_RUNDIR;/obsprep_cloud">

<!ENTITY hpsspath1      "/NCEPPROD/hpssprod/runhistory">
<!ENTITY hpsspath1_1yr  "/NCEPPROD/1year/hpssprod/runhistory">
<!ENTITY hpsspath1_gsd  "/BMC/fdr/Permanent">
<!ENTITY hpsspath1_AGibbs  "/NCEPDEV/emc-meso/1year/Annette.Gibbs">

<!ENTITY FGS_OPT        "${fgs_opt}">

<!-- for obs pre-processing -->
<!ENTITY obsprep_radar  "${obsprep_radar}">
<!ENTITY obsprep_lghtn  "${obsprep_lghtn}">
<!ENTITY obsprep_cloud  "${obsprep_cloud}">

<!-- Variables used in GSD scripts -->
<!ENTITY HOMEBASE_DIR	"&NWROOT;">
<!ENTITY DATABASE_DIR	"&ptmp_base;">


<!-- for workflow -->

<!ENTITY maxtries	"10">
<!ENTITY KEEPDATA	"YES">
<!ENTITY SENDCOM	"YES">

<!-- for various observations used in RTMA3D -->

<!-- ex-shell and J-job script name -->
<!ENTITY JJOB_OBSPREP_RADAR    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_RADAR">
EOF
if [ $realtime = 'T' ]; then 
cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF
<!ENTITY exSCR_OBSPREP_RADAR   "&SCRIPT_DIR;/ex&RUN;_obsprep_radar_rt.ksh">
EOF
else
cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF
<!ENTITY exSCR_OBSPREP_RADAR   "&SCRIPT_DIR;/ex&RUN;_obsprep_radar.ksh">
EOF
fi
cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF
<!ENTITY exefile_name_mosaic   "${exefile_name_mosaic}">
<!ENTITY JJOB_OBSPREP_LGHTN    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_LGHTN">
<!ENTITY exSCR_OBSPREP_LGHTN   "&SCRIPT_DIR;/ex&RUN;_obsprep_lghtn.ksh">
<!ENTITY exefile_name_lightning "${exefile_name_lightning}">
<!ENTITY JJOB_OBSPREP_CLOUD    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_CLOUD">
<!ENTITY exSCR_OBSPREP_CLOUD   "&SCRIPT_DIR;/ex&RUN;_obsprep_cloud.ksh">
<!ENTITY exefile_name_cloud    "${exefile_name_cloud}">
<!ENTITY JJOB_PREPOBS    "&JJOB_DIR;/J&CAP_RUN;_PREPOBS">
<!ENTITY exSCR_PREPOBS   "&SCRIPT_DIR;/ex&RUN;_prepobs.ksh">
<!ENTITY JJOB_PREPFGS    "&JJOB_DIR;/J&CAP_RUN;_PREPFGS">
<!ENTITY exSCR_PREPFGS   "&SCRIPT_DIR;/ex&RUN;_prepfgs.ksh">
<!ENTITY JJOB_GSIANL	 "&JJOB_DIR;/J&CAP_RUN;_GSIANL${gsi2}">
<!ENTITY exSCR_GSIANL	 "&SCRIPT_DIR;/ex&RUN;_gsianl${gsi2}.ksh">
<!ENTITY exefile_name_gsi      "${exefile_name_gsi}">
<!ENTITY JJOB_POST  	 "&JJOB_DIR;/J&CAP_RUN;_POST">
<!ENTITY exSCR_POST      "&SCRIPT_DIR;/ex&RUN;_post.ksh">
<!ENTITY exefile_name_post     "${exefile_name_post}">
<!ENTITY JJOB_POST4FGS   "&JJOB_DIR;/J&CAP_RUN;_POST4FGS">
<!ENTITY exSCR_POST4FGS  "&SCRIPT_DIR;/ex&RUN;_post4fgs.ksh">
<!ENTITY JJOB_PLOTGRADS  "&JJOB_DIR;/J&CAP_RUN;_PLOTGRADS">
<!ENTITY exSCR_PLOTGRADS "&SCRIPT_DIR;/ex&RUN;_plotgrads.ksh">
<!ENTITY JJOB_VERIF     "&JJOB_DIR;/J&CAP_RUN;_VERIF">
<!ENTITY exSCR_VERIF    "&SCRIPT_DIR;/ex&RUN;_verif.ksh">
<!ENTITY exefile_name_verif    "${exefile_name_verif}">

<!-- Resources -->

<!ENTITY ACCOUNT         "${ACCOUNT}">
<!ENTITY QUEUE           "${QUEUE}">
<!ENTITY QUEUE_DBG       "${QUEUE_DBG}">
<!ENTITY QUEUE_SVC       "${QUEUE_SVC}">

<!ENTITY PARTITION       "${PARTITION}">
<!ENTITY PARTITION_DA    "${PARTITION_DA}">
<!ENTITY PARTITION_SVC   "${PARTITION_SVC}">

<!ENTITY time_sig  "@Y@m@d@H">
<!ENTITY time_int  "1hr">
<!ENTITY time_int_ex "01:00:00">

<!ENTITY OBSPREP_RADAR_PROC "${OBSPREP_RADAR_PROC}">
<!ENTITY OBSPREP_RADAR_RESOURCES '${OBSPREP_RADAR_RESOURCES}'>
<!ENTITY OBSPREP_RADAR_RESERVATION '${OBSPREP_RADAR_RESERVATION}'>

<!ENTITY OBSPREP_LGHTN_PROC "${OBSPREP_LGHTN_PROC}">
<!ENTITY OBSPREP_LGHTN_RESOURCES '${OBSPREP_LGHTN_RESOURCES}'>
<!ENTITY OBSPREP_LGHTN_RESERVATION '${OBSPREP_LGHTN_RESERVATION}'>

<!ENTITY OBSPREP_CLOUD_PROC "${OBSPREP_CLOUD_PROC}">
<!ENTITY OBSPREP_CLOUD_RESOURCES '${OBSPREP_CLOUD_RESOURCES}'>
<!ENTITY OBSPREP_CLOUD_RESERVATION '${OBSPREP_CLOUD_RESERVATION}'>

<!ENTITY PREPOBS_PROC "${PREPOBS_PROC}">
<!ENTITY PREPOBS_RESOURCES '${PREPOBS_RESOURCES}'>
<!ENTITY PREPOBS_RESERVATION '${PREPOBS_RESERVATION}'>

<!ENTITY PREPFGS_PROC "${PREPFGS_PROC}">
<!ENTITY PREPFGS_RESOURCES '${PREPFGS_RESOURCES}'>
<!ENTITY PREPFGS_RESERVATION '${PREPFGS_RESERVATION}'>

<!ENTITY GSI_PROC "${GSI_PROC}">
<!ENTITY GSI_THREADS "${GSI_THREADS}">
<!ENTITY GSI_OMP_STACKSIZE "${GSI_OMP_STACKSIZE}">
<!ENTITY GSI_RESOURCES '${GSI_RESOURCES}'> 
<!ENTITY GSI_RESERVATION '${GSI_RESERVATION}'>

<!ENTITY GSI_START_TIME "00:40:00">
<!ENTITY GSI_DEADLINE   "01:30:00">
<!ENTITY GSI_WALL_LIMIT 
   '<deadline><cyclestr offset="&GSI_DEADLINE;">@Y@m@d@H@M</cyclestr></deadline>'>

<!ENTITY POST_PROC "${POST_PROC}">
<!ENTITY POST_THREADS "${POST_THREADS}">
<!ENTITY POST_OMP_STACKSIZE "${POST_OMP_STACKSIZE}">
<!ENTITY POST_RESOURCES '${POST_RESOURCES}'> 
<!ENTITY POST_RESERVATION '${POST_RESERVATION}'>

<!ENTITY PLOT_PROC "${PLOT_PROC}">
<!ENTITY PLOT_RESOURCES '${PLOT_RESOURCES}'>
<!ENTITY PLOT_RESERVATION '${PLOT_RESERVATION}'>

<!ENTITY VERIF_PROC "${VERIF_PROC}">
<!ENTITY VERIF_RESOURCES '${VERIF_RESOURCES}'>
<!ENTITY VERIF_RESERVATION '${VERIF_RESERVATION}'>

<!-- Variables in Namelist -->
<!ENTITY GSI_grid_ratio_in_var       "${gsi_grid_ratio_in_var}">
<!ENTITY GSI_grid_ratio_in_cldanl    "${gsi_grid_rario_in_cldanl}">


<!-- Block of Variables passed to workflow and tasks -->

<!ENTITY ENVARS
   '<envar>
        <name>envir</name>
        <value>&envir;</value>
   </envar>
   <envar>
        <name>realtime</name>
        <value>&realtime;</value>
   </envar>
   <envar>
        <name>RUN_ENVIR</name>
        <value>&RUN_ENVIR;</value>
   </envar>
   <envar>
        <name>machine</name>
        <value>&machine;</value>
   </envar>
   <envar>
        <name>MACHINE</name>
        <value>&MACHINE;</value>
   </envar>
   <envar>
        <name>SCHEDULER</name>
        <value>&SCHEDULER;</value>
   </envar>
   <envar>
        <name>NET</name>
        <value>&NET;</value>
   </envar>
   <envar>
        <name>RUN</name>
        <value>&RUN;</value>
   </envar>
   <envar>
        <name>model</name>
        <value>&model;</value>
   </envar>
   <envar>
        <name>OBS_DIR</name>
        <value>&OBS_DIR;</value>
   </envar>
   <envar>
        <name>HRRR_DIR</name>
        <value>&HRRR_DIR;</value>
   </envar>
   <envar>
        <name>ptmp_base</name>
        <value>&ptmp_base;</value>
   </envar>
   <envar>
        <name>COMINRAP</name>
        <value>&COMINRAP;</value>
   </envar>
   <envar>
        <name>COMINRAP_E</name>
        <value>&COMINRAP_E;</value>
   </envar>
   <envar>
        <name>COMINHRRR</name>
        <value>&COMINHRRR;</value>
   </envar>
   <envar>
        <name>GESINHRRR</name>
        <value>&GESINHRRR;</value>
   </envar>
   <envar>
        <name>NWROOT</name>
        <value>&NWROOT;</value>
   </envar>
   <envar>
        <name>DATAROOT</name>
        <value>&DATAROOT;</value>
   </envar>
   <envar>
        <name>COMROOT</name>
        <value>&COMROOT;</value>
   </envar>
   <envar>
        <name>DCOMROOT</name>
        <value>&DCOMROOT;</value>
   </envar>
   <envar>
        <name>HOMErtma3d</name>
        <value>&HOMErtma3d;</value>
   </envar>
   <envar>
        <name>EXECrtma3d</name>
        <value>&EXECrtma3d;</value>
   </envar>
   <envar>
        <name>FIXrtma3d</name>
        <value>&FIXrtma3d;</value>
   </envar>
   <envar>
        <name>USHrtma3d</name>
        <value>&USHrtma3d;</value>
   </envar>
   <envar>
        <name>UTILrtma3d</name>
        <value>&UTILrtma3d;</value>
   </envar>
   <envar>
        <name>UTILrtma3d_dev</name>
        <value>&UTILrtma3d_dev;</value>
   </envar>
   <envar>
        <name>PARMrtma3d</name>
        <value>&PARMrtma3d;</value>
   </envar>
   <envar>
        <name>FIXcrtm</name>
        <value>&FIXcrtm;</value>
   </envar>
   <envar>
        <name>FIXgsi</name>
        <value>&FIXgsi;</value>
   </envar>
   <envar>
        <name>FIXwps</name>
        <value>&FIXwps;</value>
   </envar>
   <envar>
        <name>PARMwrf</name>
        <value>&PARMwrf;</value>
   </envar>
   <envar>
        <name>PARMupp</name>
        <value>&PARMupp;</value>
   </envar>
   <envar>
        <name>PARMgsi</name>
        <value>&PARMgsi;</value>
   </envar>
   <envar>
        <name>PARMverf</name>
        <value>&PARMverf;</value>
   </envar>
   <envar>
        <name>OBS_USELIST</name>
        <value>&OBS_USELIST;</value>
   </envar>
   <envar>
        <name>LOG_DIR</name>
        <value>&LOG_DIR;</value>
   </envar>
   <envar>
        <name>JJOB_DIR</name>
        <value>&JJOB_DIR;</value>
   </envar>
   <envar>
        <name>jlogfile</name>
        <value><cyclestr>&jlogfile;</cyclestr></value>
   </envar>
   <envar>
        <name>SCRIPT_DIR</name>
        <value>&SCRIPT_DIR;</value>
   </envar>
   <envar>
        <name>LOG_WRKFLW</name>
        <value>&LOG_WRKFLW;</value>
   </envar>
   <envar>
        <name>LOG_DIR</name>
        <value>&LOG_DIR;</value>
   </envar>
   <envar>
        <name>LOG_JJOB</name>
        <value>&LOG_JJOB;</value>
   </envar>
   <envar>
        <name>LOG_PGMOUT</name>
        <value>&LOG_PGMOUT;</value>
   </envar>
   <envar>
        <name>CAP_NET</name>
        <value>&CAP_NET;</value>
   </envar>
   <envar>
        <name>CAP_RUN</name>
        <value>&CAP_RUN;</value>
   </envar>
   <envar>
        <name>CAP_ENVIR</name>
        <value>&CAP_ENVIR;</value>
   </envar>
   <envar>
        <name>FGS_OPT</name>
        <value>&FGS_OPT;</value>
   </envar>
   <envar>
        <name>obsprep_radar</name>
        <value>&obsprep_radar;</value>
   </envar>
   <envar>
        <name>obsprep_lghtn</name>
        <value>&obsprep_lghtn;</value>
   </envar>
   <envar>
        <name>obsprep_cloud</name>
        <value>&obsprep_cloud;</value>
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
   <envar>
        <name>PROD_HEAD</name>
        <value>&RUN;.t<cyclestr>@H</cyclestr>z</value>
   </envar>
   <envar>
        <name>GESROOT</name>
        <value>&GESROOT;</value>
   </envar>
   <envar>
        <name>MODULEFILES</name>
        <value>&MODULEFILES;</value>
   </envar>
   <envar>
      <name>DATA_RUNDIR</name>
      <value><cyclestr>&DATA_RUNDIR;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_GSIANL</name>
      <value><cyclestr>&DATA_GSIANL;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_POST</name>
      <value><cyclestr>&DATA_POST;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_POST4FGS</name>
      <value><cyclestr>&DATA_POST4FGS;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_VERIF</name>
      <value><cyclestr>&DATA_VERIF;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_PLOTGRADS</name>
      <value><cyclestr>&DATA_PLOTGRADS;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_OBSPRD</name>
      <value><cyclestr>&DATA_OBSPRD;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_OBSPREP_LGHTN</name>
      <value><cyclestr>&DATA_OBSPREP_LGHTN;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_OBSPREP_RADAR</name>
      <value><cyclestr>&DATA_OBSPREP_RADAR;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_OBSPREP_CLOUD</name>
      <value><cyclestr>&DATA_OBSPREP_CLOUD;</cyclestr></value>
   </envar>
   <envar>
      <name>DATA_FGSPRD</name>
      <value><cyclestr>&DATA_FGSPRD;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_gsi</name>
      <value><cyclestr>&exefile_name_gsi;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_post</name>
      <value><cyclestr>&exefile_name_post;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_mosaic</name>
      <value><cyclestr>&exefile_name_mosaic;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_lightning</name>
      <value><cyclestr>&exefile_name_lightning;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_cloud</name>
      <value><cyclestr>&exefile_name_cloud;</cyclestr></value>
   </envar>
   <envar>
      <name>exefile_name_verif</name>
      <value><cyclestr>&exefile_name_verif;</cyclestr></value>
   </envar>
   <envar>
        <name>SENDCOM</name>
        <value>&SENDCOM;</value>
   </envar>
   <envar>
        <name>KEEPDATA</name>
        <value>&KEEPDATA;</value>
   </envar>
   <envar>
     <name>outid</name>
     <value><cyclestr>&RUN;_@Y@m@d@H@M</cyclestr></value>
   </envar>
   <envar>
        <name>remove_wrkdirs</name>
        <value>no</value>
   </envar>'>

<!ENTITY ENVARS_GSI
    '<envar>
      <name>GSIPROC</name>
      <value>&GSI_PROC;</value>
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
      <name>DATA_GES</name>
      <value>&HRRR_DIR;/<cyclestr offset="-1:00:00">@Y@m@d@H</cyclestr>/wrfprd</value>
    </envar>
    <envar>
      <name>GSI_grid_ratio_in_var</name>
      <value>&GSI_grid_ratio_in_var;</value>
    </envar>
    <envar>
      <name>GSI_grid_ratio_in_cldanl</name>
      <value>&GSI_grid_ratio_in_cldanl;</value>
    </envar>
    <envar>
      <name>exSCR_GSIANL</name>
      <value>&exSCR_GSIANL;</value>
    </envar>
    <envar>
      <name>JJOB_GSIANL</name>
      <value>&JJOB_GSIANL;</value>
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
      <name>SFCOBS_PROVIDER</name>
      <value>&SFCOBS_PROVIDER;</value>
    </envar>'>

<!-- BLOCKS for OPTIMIZATIONS FOR GSIANL DA step-->
<!-- Including Set endian conversion options for use with Intel compilers -->

<!ENTITY OPTIMIZATIONS_GSI
   '<envar>
        <name>LANG</name>
        <value>en_US</value>
   </envar>
   <envar>
        <name>OMP_NUM_THREADS</name>
        <value>&GSI_THREADS;</value>
   </envar>
   <envar>
        <name>OMP_STACKSIZE</name>
        <value>&GSI_OMP_STACKSIZE;</value>
   </envar>'>

<!ENTITY ENVARS_PREPJOB
    '<envar>
      <name>START_TIME</name>
      <value><cyclestr>@Y@m@d@H</cyclestr></value>
    </envar>
    <envar>
      <name>DATABASE_DIR</name>
      <value>&DATABASE_DIR;</value>
    </envar>
    <envar>
      <name>exSCR_OBSPREP_RADAR</name>
      <value>&exSCR_OBSPREP_RADAR;</value>
    </envar>
    <envar>
      <name>JJOB_OBSPREP_RADAR</name>
      <value>&JJOB_OBSPREP_RADAR;</value>
    </envar>
    <envar>
      <name>exSCR_OBSPREP_LGHTN</name>
      <value>&exSCR_OBSPREP_LGHTN;</value>
    </envar>
    <envar>
      <name>JJOB_OBSPREP_LGHTN</name>
      <value>&JJOB_OBSPREP_LGHTN;</value>
    </envar>
    <envar>
      <name>exSCR_OBSPREP_CLOUD</name>
      <value>&exSCR_OBSPREP_CLOUD;</value>
    </envar>
    <envar>
      <name>JJOB_OBSPREP_CLOUD</name>
      <value>&JJOB_OBSPREP_CLOUD;</value>
    </envar>
    <envar>
      <name>exSCR_PREPOBS</name>
      <value>&exSCR_PREPOBS;</value>
    </envar>
    <envar>
      <name>JJOB_PREPOBS</name>
      <value>&JJOB_PREPOBS;</value>
    </envar>
    <envar>
      <name>exSCR_PREPFGS</name>
      <value>&exSCR_PREPFGS;</value>
    </envar>
    <envar>
      <name>JJOB_PREPFGS</name>
      <value>&JJOB_PREPFGS;</value>
    </envar>'>

<!ENTITY ENVARS_POST
    '<envar>
      <name>START_TIME</name>
      <value><cyclestr>@Y@m@d@H</cyclestr></value>
    </envar>
    <envar>
      <name>DATABASE_DIR</name>
      <value>&DATABASE_DIR;</value>
    </envar>
    <envar>
      <name>exSCR_POST4FGS</name>
      <value>&exSCR_POST4FGS;</value>
    </envar>
    <envar>
      <name>JJOB_POST4FGS</name>
      <value>&JJOB_POST4FGS;</value>
    </envar>
    <envar>
      <name>exSCR_POST</name>
      <value>&exSCR_POST;</value>
    </envar>
    <envar>
      <name>JJOB_POST</name>
      <value>&JJOB_POST;</value>
    </envar>'>

<!ENTITY ENVARS_VERIF
    '<envar>
      <name>START_TIME</name>
      <value><cyclestr>@Y@m@d@H</cyclestr></value>
    </envar>
    <envar>
      <name>exSCR_VERIF</name>
      <value>&exSCR_VERIF;</value>
    </envar>
    <envar>
      <name>JJOB_VERIF</name>
      <value>&JJOB_VERIF;</value>
    </envar>'>


<!ENTITY ENVARS_PLOT
    '<envar>
      <name>START_TIME</name>
      <value><cyclestr>@Y@m@d@H</cyclestr></value>
    </envar>
    <envar>
      <name>DATABASE_DIR</name>
      <value>&DATABASE_DIR;</value>
    </envar>
    <envar>
      <name>exSCR_PLOTGRADS</name>
      <value>&exSCR_PLOTGRADS;</value>
    </envar>
    <envar>
      <name>JJOB_PLOTGRADS</name>
      <value>&JJOB_PLOTGRADS;</value>
    </envar>'>
EOF

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

]>
EOF

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

<workflow realtime="$realtime" scheduler="${SCHD_ATTRB}" cyclethrottle="1" taskthrottle="350" cyclelifespan="15:00:00:00">

  <log>
    <cyclestr>&LOG_DIR;/&NET;_workflow_&envir;_@Y@m@d@H.log</cyclestr>
  </log>


  <cycledef group="00hr">00 00,12 ${ExpDateWindows}</cycledef>

  <cycledef group="01hr">00 01,13 ${ExpDateWindows}</cycledef>

  <cycledef group="02-11hr">00 02-11,14-23 ${ExpDateWindows}</cycledef>

EOF

if [ ${obsprep_lghtn} -eq 1 ] ; then

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

     <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

    <var name="subh">60</var>

  <task name="&NET;_obsprep_lghtn_#subh#" cycledefs="02-11hr,00hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPREP_LGHTN;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_OBSPREP_LGHTN;</command>
    <jobname><cyclestr>&NET;_obsprep_lghtn_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_obsprep_lghtn_@Y@m@d@H@M.log</cyclestr></join>

    &OBSPREP_LGHTN_RESOURCES;
    &OBSPREP_LGHTN_RESERVATION;

    &ENVARS_PREPJOB;

    <dependency>
       <datadep><cyclestr>&COMINRAP;/rap.@Y@m@d/rap.t@Hz.lghtng.tm00.bufr_d</cyclestr></datadep>
   </dependency>

  </task>
  </metatask>

EOF

fi


if [ ${obsprep_cloud} -eq 1 ] ; then

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF    

    <metatask>

<!-- <var name="subh">15 30 45 60</var> -->

    <var name="subh">60</var>


cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

  <task name="&NET;_obsprep_cloud_#subh#" cycledefs="02-11hr,00hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPREP_CLOUD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_OBSPREP_CLOUD;</command>
    <jobname><cyclestr>&NET;_obsprep_cloud_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_obsprep_cloud_@Y@m@d@H@M.log</cyclestr></join>

    &OBSPREP_CLOUD_RESOURCES;
    &OBSPREP_CLOUD_RESERVATION;

    &ENVARS_PREPJOB; 

    <dependency>
       <datadep><cyclestr>&COMINRAP;/rap.@Y@m@d/rap.t@Hz.lgycld.tm00.bufr_d</cyclestr></datadep>
    </dependency>

  </task>
  </metatask>

EOF

fi

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

    <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

    <var name="subh">60</var>


  <task name="&NET;_prepobs_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPRD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_PREPOBS;</command>
    <jobname><cyclestr>&NET;_prepobs_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_prepobs_@Y@m@d@H@M.log</cyclestr></join>

    &PREPOBS_RESOURCES;
    &PREPOBS_RESERVATION;

    &ENVARS_PREPJOB;
    <dependency>
       <and>
       <taskdep task="&NET;_obsprep_lghtn_#subh#"/>
       <datadep><cyclestr>&COMINRAP;/rap.@Y@m@d/rap.t@Hz.prepbufr.tm00</cyclestr></datadep>       
       <datadep><cyclestr>&COMINHRRR;/hrrr.@Y@m@d/conus/hrrr.t@Hz.NSSLRefInGSI.bufr</cyclestr></datadep>
       <datadep><cyclestr>&COMINHRRR;/hrrr.@Y@m@d/conus/hrrr.t@Hz.NASALaRCCloudInGSI.bufr</cyclestr></datadep>
       </and>   
   </dependency>
  </task>
  </metatask>

  <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

    <var name="subh">60</var>


  <task name="&NET;_prepfgs_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_FGSPRD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_PREPFGS;</command>
    <jobname><cyclestr>&NET;_prepfgs_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_prepfgs_@Y@m@d@H@M.log</cyclestr></join>

    &PREPFGS_RESOURCES;
    &PREPFGS_RESERVATION;

    &ENVARS_PREPJOB;

    <dependency>
       <datadep><cyclestr>&GESINHRRR;/hrrr/hrrrges_sfc/conus/hrrr_@Y@m@d@Hf001</cyclestr></datadep>
    </dependency>

  </task>
  </metatask>

  <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

    <var name="subh">60</var>


  <task name="&NET;_gsianl_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    &GSI_RESOURCES;
    &GSI_RESERVATION;
    &OPTIMIZATIONS_GSI;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_GSIANL;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_GSIANL;</command>
    <jobname><cyclestr>&NET;_gsianl_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_gsianl_@Y@m@d@H@M.log</cyclestr></join>

    &ENVARS_GSI;

    <dependency>
      <and>
          <taskdep task="&NET;_prepobs_#subh#"/>
          <taskdep task="&NET;_prepfgs_#subh#"/>
      </and>
    </dependency>

  </task>
  </metatask>

  <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

  <var name="subh">60</var>


  <task name="&NET;_post_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    &POST_RESOURCES;
    &POST_RESERVATION;

    <envar>
        <name>OMP_NUM_THREADS</name>
        <value>&POST_THREADS;</value>
    </envar>
    <envar>
        <name>OMP_STACKSIZE</name>
        <value>&POST_OMP_STACKSIZE;</value>
    </envar>
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_POST;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_POST;</command>
    <jobname><cyclestr>&NET;_post_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_post_@Y@m@d@H@M.log</cyclestr></join>

    &ENVARS_POST;

    <dependency>
          <taskdep task="&NET;_gsianl_#subh#"/>
    </dependency>

  </task>
  </metatask>

  <metatask>

<!-- only need 60 for hourly running

    <var name="subh">15 30 45 60</var>

    <var name="off">00:45:00 00:30:00 00:15:00 00:00:00</var>

-->

  <var name="subh">60</var>



  <task name="&NET;_verif_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">
    &ENVARS;
    &VERIF_RESOURCES;
    &VERIF_RESERVATION;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_VERIF;</cyclestr></value>
    </envar>
    <command>&JJOB_DIR;/launch.ksh &JJOB_VERIF;</command>
    <jobname><cyclestr>&NET;_verif_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_verif_@Y@m@d@H@M.log</cyclestr></join>
    &ENVARS_VERIF;
    <dependency>
       <and>
          <taskdep task="&NET;_post_#subh#"/>
       </and>
    </dependency>

  </task>
  </metatask>


EOF

# if running the step to plot (with GrADS)
if [ ${run_plt} -gt 0 ] ; then
cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 
  <metatask>

  <!-- <var name="subh">15 30 45 60</var> -->

  <var name="subh">60</var>
 
  <task name="&NET;_post4fgs_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    &POST_RESOURCES;
    &POST_RESERVATION;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_POST4FGS;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_POST4FGS;</command>
    <jobname><cyclestr>&NET;_post4fgs_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_post4fgs_@Y@m@d@H@M.log</cyclestr></join>

    &ENVARS_POST;

    <dependency>
          <taskdep task="&NET;_gsianl_#subh#"/>
    </dependency>

  </task>
  </metatask>

  <metatask>

  <!-- <var name="subh">15 30 45 60</var> -->

  <var name="subh">60</var>


  <task name="&NET;_plotgrads_#subh#" cycledefs="02-11hr,01hr" maxtries="&maxtries;">

    &ENVARS;
    &PLOT_RESOURCES;
    &PLOT_RESERVATION;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_PLOTGRADS;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.ksh &JJOB_PLOTGRADS;</command>
    <jobname><cyclestr>&NET;_plotgrads_@H_#subh#</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_plotgrads_@Y@m@d@H@M.log</cyclestr></join>

    &ENVARS_PLOT;

    <dependency>
      <and>
          <taskdep task="&NET;_post_#subh#"/>
          <taskdep task="&NET;_post4fgs_#subh#"/>
      </and>
    </dependency>

  </task>
  </metatask>

EOF
fi

cat >> ${NWROOT}/xml/${RUN}_${expname}_rt.xml <<EOF 

</workflow>

EOF

########################################################################################
#   Done building xml file 
########################################################################################

########################################################################################
#   Setting up and creating RTMA3D directories 
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
#--- set up the log directory for rocoto xml running job
#
# WORKFLOW_DIR=${TOP_RTMA}/xml
# mkdir -p ${WORKFLOW_DIR}/logs
# mkdir -p ${WORKFLOW_DIR}/logs/jlogfiles
# mkdir -p ${WORKFLOW_DIR}/logs/pgmout

######################################################
#   Creating script to submit the workflow of 3DRTMA #
######################################################
# Now make the run_rtma3d.sh script that can be invoked from a crontab

  cat > ${NWROOT}/xml/run_${RUN}_${expname}_rt.sh <<EOF 
#!/bin/bash

. /etc/profile.d/lmod.sh >/dev/null # Module Support

module purge
module load lsf/10.1
module load ruby/2.5.1
module load rocoto/complete

rocotorun -v 10 -w ${NWROOT}/xml/${RUN}_${expname}_rt.xml -d ${NWROOT}/xml/${RUN}_${expname}_rt.db 
EOF


chmod 744 ${NWROOT}/xml/run_${RUN}_${expname}_rt.sh
echo "RTMA3D is ready to go! Run using run_${RUN}_${expname}_rt.sh.  Make sure your xml file has consistent directory settings!"

#####################################################
# script to check the status of workflow            #
#####################################################
  cat > ${NWROOT}/xml/chk_${RUN}_${expname}_rt.sh <<EOF 
#!/bin/bash

. /etc/profile
. /etc/profile.d/lmod.sh >/dev/null # Module Support
module purge
module load lsf/10.1
module load ruby/2.5.1
module load rocoto/complete

rocotostat -v 10 -w ${NWROOT}/xml/${RUN}_${expname}_rt.xml -d ${NWROOT}/xml/${RUN}_${expname}_rt.db 

EOF

chmod 744 ${NWROOT}/xml/chk_${RUN}_${expname}_rt.sh

exit 
