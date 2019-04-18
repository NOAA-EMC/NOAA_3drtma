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

# loading modules in the general module file
#  (also including path definition of some common UNIX commands)

  . ${MODULEFILES}/${MACHINE}/run/modulefile.rtma3d_rt.run.${MACHINE}

# loading  Specific modules and configurations used in individual task 
#   and path to some specific command/tool used
  export AWK="/bin/awk --posix"

  case "$COMMAND" in
    *LIGHTNING*|*SATELLITE*|*GSI_DIAG*)
      export TAIL=/usr/bin/tail
      export MPIRUN=mpiexec
      ;;
    *GSI_HYB*)
      export TAIL=/usr/bin/tail
      export MPIRUN=mpirun
      ;;
    *POST*)
      module unload pnetcdf/1.6.1
      module load wgrib
      module load wgrib2/2.0.8
#     export WGRIB2="/home/rtrr/HRRR/exec/UPP/wgrib2"    # 2.0.7 (used in GSD rap/hrrr)
      export WGRIB2=${WGRIB2:-"wgrib"}
      export BC=/usr/bin/bc
      export MPIRUN=mpirun
      ;;
    *SMARTINIT*)
      export AWK="/bin/gawk --posix"
      export BC=/usr/bin/bc
      export GREP=/bin/grep
      ;;
    *)
      export MPIRUN=${MPIRUN:-"mpirun"}
      ;;
  esac
  module list
else
  echo "launch.ksh: modulefile is not set up yet for this machine-->${MACHINE}."
  echo "Job abort!"
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
      export MPIRUN=${MPIRUN:-"mpirun -np $np"}
      echo " number of cores : $np for job $job with id as $jobid "
      ;;
    SLURM|slum)                                       # SLURM
#     Not working for this version
#     (need to remove "-envall -np $np" in the mpirun command line in low-level ex-shell scripts)
      module load rocoto/1.3.0-RC3
      module load slurm/18.08.6-2p1
      export MPIRUN="srun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
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
