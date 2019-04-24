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
    SLURM|slurm)                                      # SLURM
      module load slurm
      export job=${job:-"${SLURM_JOB_NAME}"}
      export jid=${SLURM_JOBID}
      export jobid=${jobid:-"${job}.${jid}"}

      case "$COMMAND" in
        *NCL_HRRR)
          cd ${SLURM_SUBMIT_DIR}
          if [ ! -d ${LOG_DIR}/nodefiles ] ; then
            mkdir -p ${LOG_DIR}/nodefiles
          fi
          export PBS_NODEFILE=${LOG_DIR}/nodefiles/pbs_nodefile.${SLURM_JOB_NAME}.${SLURM_JOB_ID}
          rm -f ${PBS_NODEFILE}
          i=0
          imax=${SLURM_NTASKS}
          while [[ $i -lt ${SLURM_NTASKS} ]]
          do
            cat >> ${PBS_NODEFILE} << EOF
jet$i
EOF
            (( i += 1 ))
          done
          np0=`cat $PBS_NODEFILE | wc -l`
          export np=${SLURM_NTASKS}
          if [[ $np != $np0 ]] ; then
            echo "WARNING: launch.sh: wrong with setting up PBS_NODEFILE."
          fi 
          ;;
        *)
          export np=${SLURM_NTASKS}
          ;;
      esac
#     export MPIRUN="srun -n ${np}"
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
