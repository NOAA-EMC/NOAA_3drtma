#!/bin/sh

#This script preps directories for ROCOTO-controlled RTMA/URMA real time and retro runs.

#
#--- detect existence of directory sorc/
#
i_max=4; i=0;
while [ "$i" -lt "$i_max" ]
do
  let "i=$i+1"
  if [ -d ./sorc ]
  then
    cd ./sorc
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
export startCDATE=201810230000              #yyyymmddhhmm - Starting day of retro run 
export endCDATE=201810252300                #yyyymmddhhmm - Ending day of RTMA3D run (needed for both RETRO and REAL TIME). 
export NET=rtma3d                           #selection of rtma3d (or rtma,urma)
export RUN=rtma3d                           #selection of rtma3d (or rtma,urma)
=======
  export envir="retro"                      #environment (test, prod, dev, etc.)
# export envir="retro10"                    #environment (test, prod, dev, etc.)
export run_envir="dev"                      #
export expname="${envir}"
export ptmp_base="/scratch3/NCEPDEV/stmp1/${USER}/wrkdir_${NET}"  #base subdirectory for all subsequent working and storage directories
export NWROOT=${TOP_RTMA}                   #root directory for RTMA/URMA j-job scripts, scripts, parm files, etc. 
export QUEUE="batch"                        #user-specified processing queue
export QUEUE_DBG="debug"                    #user-specified processing queue -- debug
export QUEUE_SVC="service"                  #user-specified transfer queue
export ACCOUNT="nems"                     #Theia account for CPU resources

# detect the machine/platform
if [ `grep -c 'E5-2690 v3' /proc/cpuinfo` -gt 0 ]; then
  # Look for the Haswell chip
  echo 'Running on Theia'
  MACHINE=theia
else
  echo 'Running on wcoss'
  MACHINE=wcoss
  echo ' $machine is ready for running $NET.'
  exit 1
fi

export CAP_NET=`echo ${NET} | tr '[:lower:]' '[:upper:]'`
export CAP_RUN=`echo ${RUN} | tr '[:lower:]' '[:upper:]'`
export CAP_ENVIR=`echo ${envir} | tr '[:lower:]' '[:upper:]'`
export CAP_RUN_ENVIR=`echo ${run_envir} | tr '[:lower:]' '[:upper:]'`

#
#--- define the path to the static data
#
# Note: the following absolute path for static data are only valid for Theia.
#       User can specify the path to use user's static data.
# export Fixrtma3d="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData"
# export FIX_GSI="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/GSI-fix_rtma3d_emc_test"
# export FIX_CRTM="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/CRTM-fix_rtma3d"
# export OBS_USELIST="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/ObsUseList_rtma3d"
# export SFCOBS_USELIST="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/ObsUseList_rtma3d/gsd/mesonet_uselists"
# export AIRCRAFT_REJECT="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/ObsUseList_rtma3d/gsd/amdar_reject_lists"
# export SFCOBS_PROVIDER="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/GSI-fix_rtma3d_emc_test"
# export PARMwps="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/WPS"
# export PARMupp="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/static_gsd_rtma3d_gge/UPP"
# export PARMwrf="/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/FixData/static_gsd_rtma3d_gge/WRF"

  export FIXrtma3d="${NWROOT}/fix"
  export FIX_GSI="${FIXrtma3d}/GSI-fix"
  export FIX_CRTM="${FIXrtma3d}/CRTM-fix"
  export OBS_USELIST="${FIXrtma3d}/ObsUseList"
  export SFCOBS_USELIST="${OBS_USELIST}/mesonet_uselists"
  export AIRCRAFT_REJECT="${OBS_USELIST}/amdar_reject_lists"
  export SFCOBS_PROVIDER="${FIX_GSI}"
  export PARMwps="${FIXrtma3d}/WPS"

  export PARMrtma3d="${NWROOT}/parm"
  export PARMupp="${PARMrtma3d}/UPP"
  export PARMwrf="${PARMrtma3d}/WRF"
  export PARMverf="${PARMrtma3d}/VERIF"

#
#--- option control for obs pre-processing (esp. for obs used in cloud analysis)
#
  export obsprep_radar=0  # 0: No (using processed ReflInGSI.bufr_d of hrrr)
                          # 1: pre-processing MRMS grib2 radar reflectivity obs
  export obsprep_lghtn=1  # 0: No pre-processing lightning obs data
                          # 1: processing bufr data from rap run
                          # 2: processing entln data
                          # 3: processing Vaisala data
  export obsprep_cloud=0  # 0: No (using processed CloudInGSI.bufr_d of hrrr)
                          # 1: processing bufr data from rap run

#
#--- option control for UPP control parameter data set 
#
  export upp_cntrl=1      # 0: UPP default set for hrrr 
                          # 1: GSD
                          # 2: operational hrrr

#
  option_plt=$1
  export run_plt=${option_plt:-0}  # 1:    plot (and    post-process of firstguess fields)
                    # any other number: no plot (and no post-process of firstguess fields)
# control option for using hrrr wrfguess(_rap) as firstguess for rtma3d
  export fgshrrr_opt=1    # 1: wrfguess_rap (init made from RAP at 1 hr before analysis time)
                          # 2: wrfguess     (1 hr forecast to analysis time with wrfguess_rap)

########################################################################################
# Workflow is specified using user-derived settings in xml format    
########################################################################################

rm -f ${NWROOT}/workflow/${RUN}_${expname}.xml
cat > ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 
<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE workflow [

<!-- Key variables -->
<!-- date/time of cases or cycles -->
<!ENTITY startCDATE     "${startCDATE}">
<!ENTITY endCDATE       "${endCDATE}">

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

<!-- Variables Defined by absolute paths -->

<!ENTITY ptmp_base	"${ptmp_base}">
<!ENTITY NWROOT		"${NWROOT}">

<!ENTITY OBS_DIR	"/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/Data/GSD_GSI_Case/obs">
<!ENTITY HRRR_DIR	"/scratch4/NCEPDEV/fv3-cam/save/Gang.Zhao/Data/GSD_GSI_Case/fgs">

<!--  -->

<!ENTITY DATAROOT	"&ptmp_base;/data">
<!ENTITY COMROOT	"&ptmp_base;/com2">
<!ENTITY DCOMROOT	"&ptmp_base;/dcom">
<!ENTITY GESROOT	"&ptmp_base;/nwges2/&NET;">

<!ENTITY HOMErtma3d	"&NWROOT;">
<!ENTITY LOG_DIR	"&HOMErtma3d;/workflow/logs">
<!ENTITY JJOB_DIR	"&HOMErtma3d;/jobs">
<!ENTITY SCRIPT_DIR	"&HOMErtma3d;/scripts/">
<!ENTITY USHrtma3d	"&HOMErtma3d;/ush">
<!ENTITY MODULEFILES	"&HOMErtma3d;/modulefiles">
<!ENTITY EXECrtma3d	"&HOMErtma3d;/exec">
<!ENTITY PARMrtma3d	"&HOMErtma3d;/parm">

<!-- Specific Definition for static data -->
<!ENTITY FIXrtma3d	"${FIXrtma3d}">
<!ENTITY FIX_CRTM       "${FIX_CRTM}">
<!ENTITY FIX_GSI        "${FIX_GSI}">
<!ENTITY OBS_USELIST    "${OBS_USELIST}">
<!ENTITY AIRCRAFT_REJECT        "${AIRCRAFT_REJECT}">
<!ENTITY SFCOBS_USELIST         "${SFCOBS_USELIST}">
<!ENTITY SFCOBS_PROVIDER        "${SFCOBS_PROVIDER}">
<!ENTITY PARMwps        "${PARMwps}">
<!ENTITY PARMwrf        "${PARMwrf}">
<!ENTITY PARMupp        "${PARMupp}">
<!ENTITY PARMverf        "${PARMverf}">

<!ENTITY LOG_WRKFLW	"&LOG_DIR;">
<!ENTITY LOG_JJOB	"&LOG_DIR;/jlogfiles">
<!ENTITY LOG_SCHDLR	"&LOG_DIR;">
<!ENTITY LOG_PGMOUT     "&LOG_DIR;/pgmout">

<!ENTITY DATA_RUNDIR    "&DATAROOT;/&envir;/&RUN;.@Y@m@d@H@M">
<!ENTITY DATA_GSIANL    "&DATA_RUNDIR;/gsiprd">
<!ENTITY DATA_OBSPRD    "&DATA_RUNDIR;/obsprd">
<!ENTITY DATA_FGSPRD    "&DATA_RUNDIR;/fgsprd">
<!ENTITY DATA_POST      "&DATA_RUNDIR;/postprd">
<!ENTITY DATA_POST4FGS  "&DATA_RUNDIR;/postprd4fgs">
<!ENTITY DATA_PLOTGRADS "&DATA_RUNDIR;/plotgrads">
<!ENTITY DATA_FETCHHPSS "&DATA_RUNDIR;/fetchhpss">
<!ENTITY DATA_VERIF     "&DATA_RUNDIR;/verif">
<!ENTITY DATA_OBSPREP_LGHTN    "&DATA_RUNDIR;/obsprep_lghtn">
<!ENTITY DATA_OBSPREP_RADAR    "&DATA_RUNDIR;/obsprep_radar">
<!ENTITY DATA_OBSPREP_CLOUD    "&DATA_RUNDIR;/obsprep_cloud">

<!ENTITY hpsspath1      "/NCEPPROD/hpssprod/runhistory">
<!ENTITY hpsspath1_1yr  "/NCEPPROD/1year/hpssprod/runhistory">
<!ENTITY hpsspath1_gsd  "/BMC/fdr/Permanent">

<!ENTITY FGS_OPT        "${fgshrrr_opt}">

<!-- for obs pre-processing -->
<!ENTITY obsprep_radar  "${obsprep_radar}">
<!ENTITY obsprep_lghtn  "${obsprep_lghtn}">
<!ENTITY obsprep_cloud  "${obsprep_cloud}">

<!-- for upp control/config parameter dataset -->
<!ENTITY UPP_CNTRL      "${upp_cntrl}">


<!-- for obs pre-processing -->
<!ENTITY obsprep_radar  "${obsprep_radar}">
<!ENTITY obsprep_lghtn  "${obsprep_lghtn}">
<!ENTITY obsprep_cloud  "${obsprep_cloud}">

<!-- Variables used in GSD scripts -->
<!ENTITY HOMEBASE_DIR	"&NWROOT;">
<!ENTITY DATABASE_DIR	"&ptmp_base;">


<!-- for workflow -->

<!ENTITY maxtries	"5">
<!ENTITY KEEPDATA	"YES">
<!ENTITY SENDCOM	"YES">

<!-- for various observations used in RTMA3D -->

<!-- ex-shell and J-job script name -->
<!ENTITY JJOB_FETCHHPSS  "&JJOB_DIR;/J&CAP_RUN;_FETCHHPSS">
<!ENTITY exSCR_FETCHHPSS "&SCRIPT_DIR;/ex&RUN;_fetchhpss.sh">
<!ENTITY JJOB_OBSPREP_RADAR    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_RADAR">
<!ENTITY exSCR_OBSPREP_RADAR   "&SCRIPT_DIR;/ex&RUN;_obsprep_radar.sh">
<!ENTITY JJOB_OBSPREP_LGHTN    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_LGHTN">
<!ENTITY exSCR_OBSPREP_LGHTN   "&SCRIPT_DIR;/ex&RUN;_obsprep_lghtn.sh">
<!ENTITY JJOB_OBSPREP_CLOUD    "&JJOB_DIR;/J&CAP_RUN;_OBSPREP_CLOUD">
<!ENTITY exSCR_OBSPREP_CLOUD   "&SCRIPT_DIR;/ex&RUN;_obsprep_cloud.sh">
<!ENTITY JJOB_PREPOBS    "&JJOB_DIR;/J&CAP_RUN;_PREPOBS">
<!ENTITY exSCR_PREPOBS   "&SCRIPT_DIR;/ex&RUN;_prepobs.sh">
<!ENTITY JJOB_PREPFGS    "&JJOB_DIR;/J&CAP_RUN;_PREPFGS">
<!ENTITY exSCR_PREPFGS   "&SCRIPT_DIR;/ex&RUN;_prepfgs.sh">
<!ENTITY JJOB_GSIANL	 "&JJOB_DIR;/J&CAP_RUN;_GSIANL">
<!ENTITY exSCR_GSIANL	 "&SCRIPT_DIR;/ex&RUN;_gsianl.sh">
<!ENTITY JJOB_POST  	 "&JJOB_DIR;/J&CAP_RUN;_POST">
<!ENTITY exSCR_POST      "&SCRIPT_DIR;/ex&RUN;_post.sh">
<!ENTITY JJOB_POST4FGS   "&JJOB_DIR;/J&CAP_RUN;_POST4FGS">
<!ENTITY exSCR_POST4FGS  "&SCRIPT_DIR;/ex&RUN;_post4fgs.sh">
<!ENTITY JJOB_PLOTGRADS  "&JJOB_DIR;/J&CAP_RUN;_PLOTGRADS">
<!ENTITY exSCR_PLOTGRADS "&SCRIPT_DIR;/ex&RUN;_plotgrads.sh">
<!ENTITY JJOB_VERIF     "&JJOB_DIR;/J&CAP_RUN;_VERIF">
<!ENTITY exSCR_VERIF    "&SCRIPT_DIR;/ex&RUN;_verif.sh">

<!-- Resources -->

<!ENTITY ACCOUNT         "${ACCOUNT}">
<!ENTITY QUEUE           "${QUEUE}">
<!ENTITY QUEUE_DBG       "${QUEUE_DBG}">
<!ENTITY QUEUE_SVC       "${QUEUE_SVC}">

<!ENTITY time_sig  "@Y@m@d@H">
<!ENTITY time_int  "1hr">
<!ENTITY time_int_ex "01:00:00">

<!ENTITY FETCHHPSS_PROC "1">
<!ENTITY FETCHHPSS_RESOURCES
   '<cores>&FETCHHPSS_PROC;</cores>
    <walltime>01:30:00</walltime>
    <queue>&QUEUE_SVC;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY OBSPREP_RADAR_PROC "36">
<!ENTITY OBSPREP_RADAR_RESOURCES
   '<cores>&OBSPREP_RADAR_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY OBSPREP_LGHTN_PROC "1">
<!ENTITY OBSPREP_LGHTN_RESOURCES
   '<cores>&OBSPREP_LGHTN_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY OBSPREP_CLOUD_PROC "4">
<!ENTITY OBSPREP_CLOUD_RESOURCES
   '<cores>&OBSPREP_CLOUD_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY PREPOBS_PROC "1">
<!ENTITY PREPOBS_RESOURCES
   '<cores>&PREPOBS_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>
<!ENTITY PREPFGS_PROC "1">
<!ENTITY PREPFGS_RESOURCES
   '<cores>&PREPFGS_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY GSI_CORES "96">
<!ENTITY GSI_THREADS "4">
<!ENTITY GSI_RESOURCES  
   "<nodes>8:ppn=12</nodes>
    <walltime>00:30:00</walltime>">
<!ENTITY GSI_OMP_STACKSIZE "512M">

<!ENTITY GSI_START_TIME "00:40:00">
<!ENTITY GSI_DEADLINE   "01:30:00">
<!ENTITY GSI_WALL_LIMIT 
   '<deadline><cyclestr offset="&GSI_DEADLINE;">@Y@m@d@H@M</cyclestr></deadline>'>
<!ENTITY GSI_RESERVATION 
   '<native>-m n</native>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY POST_CORES "96">
<!ENTITY POST_OMP_STACKSIZE "512M">
<!ENTITY POST_THREADS "1">
<!ENTITY POST_RESOURCES
   '<nodes>8:ppn=12</nodes>
    <walltime>00:30:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>

<!ENTITY PLOT_PROC "1">
<!ENTITY PLOT_RESOURCES
   '<cores>&PLOT_PROC;</cores>
    <walltime>00:30:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <memory>3G</memory>
    <account>&ACCOUNT;</account>'>

<!ENTITY VERIF_PROC "1">
<!ENTITY VERIF_RESOURCES
   '<cores>&VERIF_PROC;</cores>
    <walltime>00:15:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>


<!-- Variables in Namelist -->
<!ENTITY GSI_grid_ratio_in_var       "1">
<!ENTITY GSI_grid_ratio_in_cldanl    "1">


<!-- Block of Variables passed to workflow and tasks -->

<!ENTITY ENVARS
   '<envar>
        <name>envir</name>
        <value>&envir;</value>
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
        <name>PARMrtma3d</name>
        <value>&PARMrtma3d;</value>
   </envar>
   <envar>
        <name>FIX_CRTM</name>
        <value>&FIX_CRTM;</value>
   </envar>
   <envar>
        <name>FIX_GSI</name>
        <value>&FIX_GSI;</value>
   </envar>
   <envar>
        <name>PARMwps</name>
        <value>&PARMwps;</value>
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
      <name>DATA_FETCHHPSS</name>
      <value><cyclestr>&DATA_FETCHHPSS;</cyclestr></value>
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
      <value>&GSI_CORES;</value>
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
      <name>RUNDIR_GSD</name>
      <value><cyclestr>&DATA_RUNDIR;</cyclestr></value>
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

<!ENTITY ENVARS_FETCHHPSS
    '<envar>
      <name>START_TIME</name>
      <value><cyclestr>@Y@m@d@H</cyclestr></value>
    </envar>
    <envar>
      <name>hpsspath1</name>
      <value>&hpsspath1;</value>
    </envar>
    <envar>
      <name>hpsspath1_1yr</name>
      <value>&hpsspath1_1yr;</value>
    </envar>
    <envar>
      <name>hpsspath1_gsd</name>
      <value>&hpsspath1_gsd;</value>
    </envar>
    <envar>
      <name>exSCR_FETCHHPSS</name>
      <value>&exSCR_FETCHHPSS;</value>
    </envar>
    <envar>
      <name>JJOB_FETCHHPSS</name>
      <value>&JJOB_FETCHHPSS;</value>
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
      <name>RUNDIR_GSD</name>
      <value><cyclestr>&DATAROOT;/&RUN;_@Y@m@d@H@M</cyclestr></value>
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
      <name>UPP_CNTRL</name>
      <value>&UPP_CNTRL;</value>
    </envar>
    <envar>
      <name>DATABASE_DIR</name>
      <value>&DATABASE_DIR;</value>
    </envar>
    <envar>
      <name>RUNDIR_GSD</name>
      <value><cyclestr>&DATAROOT;/&RUN;_@Y@m@d@H@M</cyclestr></value>
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
      <name>RUNDIR_GSD</name>
      <value><cyclestr>&DATAROOT;/&RUN;_@Y@m@d@H@M</cyclestr></value>
    </envar>
    <envar>
      <name>exSCR_PLOTGRADS</name>
      <value>&exSCR_PLOTGRADS;</value>
    </envar>
    <envar>
      <name>JJOB_PLOTGRADS</name>
      <value>&JJOB_PLOTGRADS;</value>
    </envar>'>

<!-- Set of system(LINUX, MPI, etc.) commands -->
<!ENTITY SYS_COMMANDS 
   '<envar>
        <name>RM</name>
        <value>/bin/rm</value>
   </envar>
   <envar>
        <name>CP</name>
        <value>/bin/cp</value>
   </envar>
   <envar>
        <name>MV</name>
        <value>/bin/mv</value>
   </envar>
   <envar>
        <name>LN</name>
        <value>/bin/ln</value>
   </envar>
   <envar>
        <name>MKDIR</name>
        <value>/bin/mkdir</value>
   </envar>
   <envar>
        <name>CAT</name>
        <value>/bin/cat</value>
   </envar>
   <envar>
        <name>ECHO</name>
        <value>/bin/echo</value>
   </envar>
   <envar>
        <name>LS</name>
        <value>/bin/ls</value>
   </envar>
   <envar>
        <name>CUT</name>
        <value>/bin/cut</value>
   </envar>
   <envar>
        <name>DATE</name>
        <value>/bin/date</value>
   </envar>
   <envar>
        <name>WC</name>
        <value>/bin/wc</value>
   </envar>
   <envar>
        <name>SED</name>
        <value>/bin/sed</value>
   </envar>
   <envar>
        <name>AWK</name>
        <value>/bin/awk --posix</value>
   </envar>
   <envar>
        <name>TAIL</name>
        <value>/bin/tail</value>
   </envar>
   <envar>
        <name>CNVGRIB</name>
        <value>cnvgrib</value>
   </envar>
   <envar>
        <name>MPIRUN</name>
        <value>mpirun</value>
   </envar>'>
]>

<workflow realtime="F" scheduler="moabtorque" cyclethrottle="1" taskthrottle="350" cyclelifespan="03:00:00:00">

  <log>
    <cyclestr>&LOG_WRKFLW;/&NET;_workflow_&envir;_@Y@m@d@H.log</cyclestr>
  </log>

  <cycledef group="&time_int;">&startCDATE; &endCDATE; &time_int_ex;</cycledef>

  <task name="&NET;_fetchhpss" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_FETCHHPSS;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_FETCHHPSS</command>
    <jobname><cyclestr>&NET;_fetchhpss_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_fetchhpss_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &FETCHHPSS_RESOURCES;
    &ENVARS_FETCHHPSS;

  </task>
EOF

if [ ${obsprep_radar} -eq 1 ] ; then

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

  <task name="&NET;_obsprep_radar" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPREP_RADAR;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_OBSPREP_RADAR</command>
    <jobname><cyclestr>&NET;_obsprep_radar_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_obsprep_radar_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &OBSPREP_RADAR_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <taskdep task="&NET;_fetchhpss"/>
    </dependency>

  </task>

EOF

fi

if [ ${obsprep_lghtn} -eq 1 ] || [ ${obsprep_lghtn} -eq 2 ] || [ ${obsprep_lghtn} -eq 3 ] ; then

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

  <task name="&NET;_obsprep_lghtn" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPREP_LGHTN;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_OBSPREP_LGHTN</command>
    <jobname><cyclestr>&NET;_obsprep_lghtn_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_obsprep_lghtn_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &OBSPREP_LGHTN_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <taskdep task="&NET;_fetchhpss"/>
    </dependency>

  </task>

EOF

fi

if [ ${obsprep_cloud} -eq 1 ] ; then

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

  <task name="&NET;_obsprep_cloud" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPREP_CLOUD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_OBSPREP_CLOUD</command>
    <jobname><cyclestr>&NET;_obsprep_cloud_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_obsprep_cloud_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &OBSPREP_CLOUD_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <taskdep task="&NET;_fetchhpss"/>
    </dependency>

  </task>

EOF

fi

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

  <task name="&NET;_prepobs" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPRD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_PREPOBS</command>
    <jobname><cyclestr>&NET;_prepobs_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_prepobs_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &PREPOBS_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <and>
        <taskdep task="&NET;_fetchhpss"/>
EOF

if [ $obsprep_radar -eq 1 ] ; then
cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 
        <taskdep task="&NET;_obsprep_radar"/>
EOF
fi

if [ $obsprep_lghtn -eq 1 ] || [ $obsprep_lghtn -eq 2 ] || [ $obsprep_lghtn -eq 3 ] ; then
cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 
        <taskdep task="&NET;_obsprep_lghtn"/>
EOF
fi

if [ $obsprep_cloud -eq 1 ] ; then
cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 
        <taskdep task="&NET;_obsprep_cloud"/>
EOF
fi

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 
      </and>
    </dependency>

  </task>

  <task name="&NET;_prepfgs" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_FGSPRD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_PREPFGS</command>
    <jobname><cyclestr>&NET;_prepfgs_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_prepfgs_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &PREPFGS_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <taskdep task="&NET;_fetchhpss"/>
    </dependency>

  </task>

  <task name="&NET;_gsianl" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &GSI_RESOURCES;
    &GSI_RESERVATION;
    &SYS_COMMANDS;
    &OPTIMIZATIONS_GSI;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_GSIANL;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_GSIANL</command>
    <jobname><cyclestr>&NET;_gsianl_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_gsianl_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &ENVARS_GSI;

    <dependency>
      <and>
          <taskdep task="&NET;_prepobs"/>
          <taskdep task="&NET;_prepfgs"/>
      </and>
    </dependency>

  </task>

  <task name="&NET;_post" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &POST_RESOURCES;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_POST;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_POST</command>
    <jobname><cyclestr>&NET;_post_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_post_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &ENVARS_POST;

    <dependency>
      <and>
          <taskdep task="&NET;_gsianl"/>
      </and>
    </dependency>

  </task>

  <task name="&NET;_verif" cycledefs="&time_int;" maxtries="&maxtries;">
    &ENVARS;
    &VERIF_RESOURCES;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_VERIF;</cyclestr></value>
    </envar>
    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_VERIF</command>
    <jobname><cyclestr>&NET;_verif_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_submit_verif_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>
    &ENVARS_VERIF;
    <dependency>
          <taskdep task="&NET;_post"/>
    </dependency>

  </task>


EOF

# if running the step to plot (with GrADS)
if [ ${run_plt} -eq 100 ] ; then
cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

  <task name="&NET;_post4fgs" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &POST_RESOURCES;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_POST4FGS;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_POST4FGS</command>
    <jobname><cyclestr>&NET;_post4fgs_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_post4fgs_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &ENVARS_POST;

    <dependency>
      <and>
          <taskdep task="&NET;_gsianl"/>
      </and>
    </dependency>

  </task>

  <task name="&NET;_plotgrads" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &PLOT_RESOURCES;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_PLOTGRADS;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_PLOTGRADS</command>
    <jobname><cyclestr>&NET;_plotgrads_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_&envir;_plotgrads_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &ENVARS_PLOT;

    <dependency>
      <and>
          <taskdep task="&NET;_post"/>
          <taskdep task="&NET;_post4fgs"/>
      </and>
    </dependency>

  </task>

EOF
fi

cat >> ${NWROOT}/workflow/${RUN}_${expname}.xml <<EOF 

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

export LOGDIR=$NWROOT/workflow/logs
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

########################################################################################
#   Creating RTMA3D run script
########################################################################################


# Now make the run_NAMRR.sh script that can be invoked from a crontab

if [ ${MACHINE} = 'theia' ]; then
cat > ${NWROOT}/workflow/run_${RUN}_${expname}.sh <<EOF 
#!/bin/sh -l

# . /etc/profile
# . /apps/lmod/lmod/init/ksh >/dev/null # Module Support

module load intel
module load rocoto
EOF
fi

cat >> ${NWROOT}/workflow/run_${RUN}_${expname}.sh <<EOF
rocotorun -v 10 -w ${NWROOT}/workflow/${RUN}_${expname}.xml -d ${NWROOT}/workflow/${RUN}_${expname}.db 
EOF

chmod 744 ${NWROOT}/workflow/run_${RUN}_${expname}.sh
echo "RTMA3D is ready to go! Run using run_${RUN}_${expname}.sh.  Make sure your xml file has consistent directory settings!"

# script to check the workflow running status
if [ ${MACHINE} = 'theia' ]; then
cat > ${NWROOT}/workflow/chk_${RUN}_${expname}.sh <<EOF 
#!/bin/sh -l

# . /etc/profile
# . /apps/lmod/lmod/init/ksh >/dev/null # Module Support

module load intel
module load rocoto
EOF
fi

cat >> ${NWROOT}/workflow/chk_${RUN}_${expname}.sh <<EOF
rocotostat -v 10 -w ${NWROOT}/workflow/${RUN}_${expname}.xml -d ${NWROOT}/workflow/${RUN}_${expname}.db 
EOF

chmod 744 ${NWROOT}/workflow/chk_${RUN}_${expname}.sh

exit 
