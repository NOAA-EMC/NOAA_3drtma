#!/bin/sh

#This script preps directories for ROCOTO-controlled RTMA/URMA real time and retro runs.

# detect existence of directory sorc/
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

set -x
export startCDATE=201807110000              #yyyymmddhhmm - Starting day of retro run 
export endCDATE=201807120000                #yyyymmddhhmm - Ending day of RTMA3D run (needed for both RETRO and REAL TIME). 
export NET=rtma3d                          #selection of rtma or urma 
export RUN=rtma3d                          #selection of rtma or urma 
export envir="retro2"                        #environment (test, prod, dev, etc.)
export run_envir="dev"                      #
export expname="retro"
export ptmp_base="/scratch3/NCEPDEV/stmp1/${USER}/wrkdir_${NET}"  #base subdirectory for all subsequent working and storage directories
export NWROOT=${TOP_RTMA}                   #root directory for RTMA/URMA j-job scripts, scripts, parm files, etc. 
export QUEUE="batch"                        #user-specified processing queue
export QUEUE_DBG="debug"                    #user-specified processing queue -- debug
export QUEUE_SVC="service"                  #user-specified transfer queue
export ACCOUNT="fv3-cpu"                    #Theia account
export ACCOUNT_DA="da-cpu"                  #Theia account

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

########################################################################################
# Workflow is specified using user-derived settings in xml format    
########################################################################################

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

<!ENTITY OBS_DIR	"/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/GSD_GSI_Case/obs">
<!ENTITY HRRR_DIR	"/scratch4/NCEPDEV/meso/save/Gang.Zhao/Data/GSD_GSI_Case/fgs">

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
<!ENTITY FIXrtma3d	"&HOMErtma3d;/fix">

<!ENTITY FIX_CRTM       "&FIXrtma3d;/CRTM-fix">
<!ENTITY FIX_GSI        "&FIXrtma3d;/GSI-fix">
<!ENTITY OBS_USELIST    "&FIXrtma3d;/ObsUseList/gsd">
<!ENTITY LOG_WRKFLW	"&LOG_DIR;">
<!ENTITY LOG_JJOB	"&LOG_DIR;/jlogfiles">
<!ENTITY LOG_SCHDLR	"&LOG_DIR;">
<!ENTITY LOG_PGMOUT     "&LOG_DIR;/pgmout">

<!ENTITY DATA_RUNDIR    "&DATAROOT;/&envir;/&RUN;.@Y@m@d@H@M">
<!ENTITY DATA_GSIANL    "&DATA_RUNDIR;/gsiprd">
<!ENTITY DATA_OBSPRD    "&DATA_RUNDIR;/obsprd">
<!ENTITY DATA_FGSPRD    "&DATA_RUNDIR;/fgsprd">
<!ENTITY DATA_FETCHHPSS "&DATA_RUNDIR;/fetchhpss">

<!ENTITY hpsspath1      "/NCEPPROD/hpssprod/runhistory">
<!ENTITY hpsspath1_1yr  "/NCEPPROD/1year/hpssprod/runhistory">

<!ENTITY FGS_OPT        "1">

<!-- Variables used in GSD scripts -->
<!ENTITY HOMEBASE_DIR	"&NWROOT;">
<!ENTITY DATABASE_DIR	"&ptmp_base;">

<!-- Specific Definition -->
<!ENTITY AIRCRAFT_REJECT        "&OBS_USELIST;/amdar_reject_lists">
<!ENTITY SFCOBS_USELIST         "&OBS_USELIST;/mesonet_uselists">
<!ENTITY SFCOBS_PROVIDER        "&FIX_GSI;">

<!-- for workflow -->

<!ENTITY maxtries	"5">
<!ENTITY KEEPDATA	"YES">
<!ENTITY SENDCOM	"YES">

<!-- for various observations used in RTMA3D -->

<!-- ex-shell and J-job script name -->
<!ENTITY JJOB_FETCHHPSS  "&JJOB_DIR;/J&CAP_RUN;_FETCHHPSS">
<!ENTITY exSCR_FETCHHPSS "&SCRIPT_DIR;/GSI/ex&RUN;_fetchhpss.sh">
<!ENTITY JJOB_PREPOBS    "&JJOB_DIR;/J&CAP_RUN;_PREPOBS">
<!ENTITY exSCR_PREPOBS "&SCRIPT_DIR;/GSI/ex&RUN;_prepobs.sh">
<!ENTITY JJOB_PREPFGS    "&JJOB_DIR;/J&CAP_RUN;_PREPFGS">
<!ENTITY exSCR_PREPFGS "&SCRIPT_DIR;/GSI/ex&RUN;_prepfgs.sh">
<!ENTITY JJOB_GSIANL	 "&JJOB_DIR;/J&CAP_RUN;_GSIANL">
<!ENTITY exSCR_GSIANL	 "&SCRIPT_DIR;/GSI/ex&RUN;_gsianl.sh">

<!-- Resources -->

<!ENTITY ACCOUNT         "${ACCOUNT}">
<!ENTITY ACCOUNT_DA      "${ACCOUNT_DA}">
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
    <account>&ACCOUNT_DA;</account>'>

<!ENTITY PREPOBS_PROC "1">
<!ENTITY PREPOBS_RESOURCES
   '<cores>&PREPOBS_PROC;</cores>
    <walltime>00:05:00</walltime>
    <queue>&QUEUE_DBG;</queue>
    <account>&ACCOUNT;</account>'>
<!ENTITY PREPFGS_PROC "1">
<!ENTITY PREPFGS_RESOURCES
   '<cores>&PREPFGS_PROC;</cores>
    <walltime>00:05:00</walltime>
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
      <name>DATA_OBSPRD</name>
      <value><cyclestr>&DATA_OBSPRD;</cyclestr></value>
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
        <value>/bin/awk</value>
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

<workflow realtime="F" scheduler="moabtorque" cyclethrottle="1" taskthrottle="350" cyclelifespan="01:00:00:00">

  <log>
    <cyclestr>&LOG_WRKFLW;/&NET;_workflow_@Y@m@d@H.log</cyclestr>
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
    <join><cyclestr>&LOG_SCHDLR;/&NET;_submit_fetchhpss_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &FETCHHPSS_RESOURCES;
    &ENVARS_FETCHHPSS;

  </task>

  <task name="&NET;_prepobs" cycledefs="&time_int;" maxtries="&maxtries;">

    &ENVARS;
    &SYS_COMMANDS;
    <envar>
       <name>rundir_task</name>
       <value><cyclestr>&DATA_OBSPRD;</cyclestr></value>
    </envar>

    <command>&JJOB_DIR;/launch.sh &JJOB_DIR;/J&CAP_NET;_PREPOBS</command>
    <jobname><cyclestr>&NET;_prepobs_@H</cyclestr></jobname>
    <join><cyclestr>&LOG_SCHDLR;/&NET;_submit_prepobs_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &PREPOBS_RESOURCES;
    &ENVARS_PREPJOB;

    <dependency>
      <taskdep task="&NET;_fetchhpss"/>
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
    <join><cyclestr>&LOG_SCHDLR;/&NET;_submit_prepfgs_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

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
    <join><cyclestr>&LOG_SCHDLR;/&NET;_submit_gsianl_@Y@m@d@H@M.log\${PBS_JOBID}</cyclestr></join>

    &ENVARS_GSI;

    <dependency>
      <and>
          <taskdep task="&NET;_prepobs"/>
          <taskdep task="&NET;_prepfgs"/>
      </and>
    </dependency>

  </task>

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

exit 
