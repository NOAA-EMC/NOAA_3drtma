#!/bin/ksh -l

# --- for debug --- #
date
export PS4=' $SECONDS + ' 
#set -x ##too much outputs with this

COMMAND=$1

#############################################################
# load modulefile and set up the environment for job runnning
#############################################################

MODULEFILES=${MODULEFILES:-${HOMErtma3d}/modulefiles}

if [ "${MACHINE}" = "jet" ] ; then

# loading modules in the general module file
#  (also including path definition of some common UNIX commands)

  . ${MODULEFILES}/modulefile.rtma3d.${MACHINE}

# loading  Specific modules and configurations used in individual task 
#   and path to some specific command/tool used

  case "$COMMAND" in
    *LIGHTNING*|*SATELLITE*|*GSI_DIAG*)
      ;;
    *GSI_HYB*)
      ;;
    *POST*)
      ;;
    *SMARTINIT*)
      ;;
    *)
      ;;
  esac
elif  [ "${machine}" = "theia" ] ; then
  module purge
# load modules used when compiling the code
  modulefile=${modulefile:-"${MODULEFILES}/modulefile.${target}.${machine}"}
  source $modulefile
# load extra modules to run the job
  modulefile_run=${modulefile_run:-"${MODULEFILES}/modulefile.RUN.${machine}"}
  source $modulefile_run
  module list
else
  echo "launch.ksh: modulefile is not set up yet for this machine-->${MACHINE}."
  echo "Job abort!"
  exit 1
fi

# print out loaded modules
module list

############################################################
#                                                          #
#        different actions based on $SCHEDULER             #
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
      export MPIRUN="mpiexec -np $np"
      ;;
    SLURM|slurm)                                       # SLURM
      module load slurm
      export job=${job:-"${SLURM_JOB_NAME}"}
      export jid=${SLURM_JOBID}
      export jobid=${jobid:-"${job}.${jid}"}
      export np=${SLURM_NNODES}
      export MPIRUN="srun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
fi
echo "MPIRUN command is: $MPIRUN"
echo " number of cores : $np for job $job with id as $jobid "

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
