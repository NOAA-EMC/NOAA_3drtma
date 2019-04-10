#!/bin/ksh -l

# --- for debug --- #
date
export PS4=' $SECONDS + ' 
set -x

COMMAND=$1

#############################################################
# load modulefile and set up the environment for job runnning
#############################################################
  MODULEFILES=${MODULEFILES:-${HOMErtma3d}/modulefiles}

if [ "${MACHINE}" = "jet" ] ; then

# loading modules in general module file
  . ${MODULEFILES}/${MACHINE}/run/modulefile.rtma3d_rt.run.${MACHINE}

# Specific modules and configurations used in individual task 
  case "$COMMAND" in
    *LIGHTNING*|*SATELLITE*|*GSI_DIAG*)
      export TAIL=/usr/bin/tail
      export MPIRUN=mpiexec
      ;;
    *GSI_HYB*)
      export TAIL=/usr/bin/tail
      module load cnvgrib
#     export CNVGRIB=/apps/cnvgrib/1.2.3/bin/cnvgrib     # not exist
#     export CNVGRIB=/apps/cnvgrib/1.4.0/bin/cnvgrib
      export CNVGRIB=${CNVGRIB:-"cnvgrib"}
      export MPIRUN=mpirun
      ;;
    *POST*)
#     module load newdefaults                            # not exist
      module load nco
      module load cnvgrib
      module load wgrib
      module load wgrib2/2.0.8
      module unload pnetcdf/1.6.1
      export BC=/usr/bin/bc
      export CNVGRIB=${CNVGRIB:-"cnvgrib"}
      export MPIRUN=mpirun
#     export WGRIB2=${EXE_ROOT}/wgrib2_new               # Not exist
#     export WGRIB2="/home/rtrr/HRRR/exec/UPP/wgrib2"    # 2.0.7 (used in GSD rap/hrrr)
      export WGRIB2=${WGRIB2:-"wgrib"}
      ;;
    *)
      export MPIRUN=${MPIRUN:-"mpirun"}
      ;;
  esac
  module list
else
  echo "modulefile has not set up for this unknow machine -->${MACHINE}. Job abort!"
  exit 1
fi


############################################################
#                                                          #
#        obtain unique process id (pid)                    #
#                                                          #
############################################################
if [ "${MACHINE}" = "theia" ] || [ "${MACHINE}" = "jet" ] ; then    ### PBS job Scheduler
  case ${SCHEDULER} in
    PBS|pbs|moab*)                                    # PBS maob/torque
      export job=${job:-"${PBS_JOBNAME}"}
      export jid=`echo ${PBS_JOBID} | cut -f1 -d.`
#     export jid=`echo ${PBS_JOBID} | awk -F'.' '{print $1}'`
      export jobid=${jobid:-"${job}.${jid}"}
      export np=`cat $PBS_NODEFILE | wc -l`
      export NCDUMP="ncdump"
      export MPIRUN=${MPIRUN:-"mpirun -np $np"}
      echo " number of cores : $np for job $job with id as $jobid "
      ;;
    SLURM|slum)                                       # SLURM
      export NCDUMP="ncdump"
      export MPIRUN=${MPIRUN:-"srun"}
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
elif [ "${MACHINE}" = "wcoss" ] ; then  ### LSB scheduler
  export job=${job:-"${LSB_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${LSB_JOBID} `
  export jobid=${jobid:-"${job}.${jid}"}
  echo " job $job with id as $jobid "
  export MPIRUN=${MPIRUN:-"aprun"}
else
  export job=${job:-"${outid}.o$$"}
  export jobid=${jobid:-"${outid}.o$$"}
  export jid=$$
  export MPIRUN=${MPIRUN:-"mpirun"}
fi

############################################################
#                                                          #
#    define the name of running directory with job name.   #
#        (NCO: only data.${jobid})                         #
#                                                          #
############################################################
if [ -n ${rundir_task} ] ; then
  export DATA=${rundir_task}.${jid}
fi

$COMMAND
