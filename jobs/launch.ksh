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

if [ "${machine}" = "theia" ] ; then
  . /etc/profile
  . /etc/profile.d/modules.sh >/dev/null # Module Support
# . /apps/lmod/lmod/init/bash >/dev/null # Module Support
  module purge
# loading modules used when building the code
  case "$COMMAND" in
    *POST*)
      modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.post.${machine}"}
      moduledir=`dirname $(readlink -f ${modulefile_build})`
      module use ${moduledir}
      module load modulefile.build.post.${machine}
      ;;
    *)
      modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.gsi.${machine}"}
      source $modulefile_build
      ;;
  esac
# loading modules for running
  modulefile_run=${modulefile_run:-"${MODULEFILES}/${machine}/run/modulefile.run.${machine}"}
  source ${modulefile_run}
# loading modules for specific task
  case "$COMMAND" in
    *VERIF*)
      module load met/7.0
      ;;
    *)
      ;;
  esac
  module list
elif [ "${machine}" = "jet" ] ; then
  . /etc/profile
  . /etc/profile.d/modules.sh >/dev/null # Module Support
# . /apps/lmod/lmod/init/bash >/dev/null # Module Support
  module purge
# loading modules used when building the code
  case "$COMMAND" in
    *POST*)
      modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.post.${machine}"}
      moduledir=`dirname $(readlink -f ${modulefile_build})`
      module use ${moduledir}
      module load modulefile.build.post.${machine}
      ;;
    *GSIANL*)
      modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.gsi.${machine}"}
      source $modulefile_build
      ;;
    *)
#     modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.gsi.PreInstalledLibs.${machine}"}
      modulefile_build=${modulefile_build:-"${MODULEFILES}/${machine}/build/modulefile.build.gsi.${machine}"}
      source $modulefile_build
      ;;
  esac
# loading modules for running
  modulefile_run=${modulefile_run:-"${MODULEFILES}/${machine}/run/modulefile.run.${machine}"}
  source ${modulefile_run}
# loading modules for specific task
  case "$COMMAND" in
    *VERIF*)
      module load met/8.1_beta2
      ;;
    *)
      ;;
  esac
  module list
else
  echo "modulefile has not set up for this unknow machine. Job abort!"
  exit 1
fi


###########################################################################
# obtain unique process id (pid) and define the name of  temp directories
###########################################################################
if [ "${machine}" = "theia" ] ; then    ### PBS job Scheduler

  case ${SCHEDULER} in
    PBS|pbs|MOAB*|moab*)                                    # PBS maob/torque
#     module load rocoto
      export np=`cat $PBS_NODEFILE | wc -l`
      export MPIRUN="mpirun -np $np"
      ;;
    SLURM|slum)                                       # SLURM
#     module load rocoto/1.3.0-RC5                    # not necessady for running j-job and ex-shell scripts
#     module load slurm/18.08                         # slurm is loaded as system default

      export PBS_JOBID=${SLURM_JOB_ID}
      export PBS_JOBNAME=${SLURM_JOB_NAME}
      export PBS_NP=${SLURM_NTASKS}
      export PBS_O_DIR=${SLURM_SUBMIT_DIR}

      cd ${SLURM_SUBMIT_DIR}
      if [ ! -d ${LOG_DIR}/nodefiles ] ; then
        mkdir -p ${LOG_DIR}/nodefiles
      fi
      slurm_hfile=${LOG_DIR}/nodefiles/hostfile.${SLURM_JOB_NAME}.${SLURM_JOB_ID}
      scontrol show hostname $SLURM_NODELSIT > ${slurm_hfile}
      export PBS_NODEFILE=${LOG_DIR}/nodefiles/pbs_nodefile.${SLURM_JOB_NAME}.${SLURM_JOB_ID}
      if [ -f ${PBS_NODEFILE} ] ; then
        rm -f ${PBS_NODEFILE}
      fi
      i=0
      imax=${SLURM_NTASKS}
      while [[ $i -lt ${SLURM_NTASKS} ]]
      do
        cat >> ${PBS_NODEFILE} << EOF
node$i
EOF
        (( i += 1 ))
      done
      np=`cat $PBS_NODEFILE | wc -l`
      echo "Launch.sh: ${SLURM_JOB_NAME} np=$np (in $PBS_NODEFILE)"
#     export MPIRUN="srun -n ${np}"
      export MPIRUN="srun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
  export job=${job:-"${PBS_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${PBS_JOBID} | cut -f1 -d.`  # removal of tailing sub-server string
# export jid=`echo ${PBS_JOBID} | awk -F'.' '{print $1}'`
  export jobid=${jobid:-"${job}.${jid}"}
  echo " number of cores : $np for job $job with id as $jobid "

elif [ "${machine}" = "jet" ] ;  then    ### PBS job Scheduler

  case ${SCHEDULER} in
    PBS|pbs|MOAB*|moab*)                                    # PBS maob/torque
#     module load rocoto
      export np=`cat $PBS_NODEFILE | wc -l`
#     export MPIRUN="mpiexec -np $np"
      export MPIRUN="mpirun -np $np"
      ;;
    SLURM|slum)                                             # SLURM
#     module load rocoto/1.3.0-RC5                          # not necessary for running j-job and ex-shell scripts
#     module load slurm/18.08.7p1                           # slurm is loaded as system default

      export PBS_JOBID=${SLURM_JOB_ID}
      export PBS_JOBNAME=${SLURM_JOB_NAME}
      export PBS_NP=${SLURM_NTASKS}
      export PBS_O_DIR=${SLURM_SUBMIT_DIR}

      cd ${SLURM_SUBMIT_DIR}
      if [ ! -d ${LOG_DIR}/nodefiles ] ; then
        mkdir -p ${LOG_DIR}/nodefiles
      fi
      slurm_hfile=${LOG_DIR}/nodefiles/hostfile.${SLURM_JOB_NAME}.${SLURM_JOB_ID}
      scontrol show hostname $SLURM_NODELSIT > ${slurm_hfile}
      export PBS_NODEFILE=${LOG_DIR}/nodefiles/pbs_nodefile.${SLURM_JOB_NAME}.${SLURM_JOB_ID}
      if [ -f ${PBS_NODEFILE} ] ; then
        rm -f ${PBS_NODEFILE}
      fi
      i=0
      imax=${SLURM_NTASKS}
      while [[ $i -lt ${SLURM_NTASKS} ]]
      do
        cat >> ${PBS_NODEFILE} << EOF
node$i
EOF
        (( i += 1 ))
      done
      np=`cat $PBS_NODEFILE | wc -l`
      echo "Launch.sh: ${SLURM_JOB_NAME} np=$np (in $PBS_NODEFILE)"
#     export MPIRUN="srun -n ${np}"
      export MPIRUN="srun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
  export job=${job:-"${PBS_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${PBS_JOBID} | cut -f1 -d.`  # removal of tailing sub-server string
# export jid=`echo ${PBS_JOBID} | awk -F'.' '{print $1}'`
  export jobid=${jobid:-"${job}.${jid}"}
  echo " number of cores : $np for job $job with id as $jobid "

elif [ "${machine}" = "wcoss" ] ; then  ### LSB scheduler

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
# Notice: the following line is to                         #
#            name the running directory with job name.     #
#                              (not used for NCO.)         #
############################################################
if [ "${rundir_task}" ]; then
  export DATA=${rundir_task}.${jid}
fi

$COMMAND
