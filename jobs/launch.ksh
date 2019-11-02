#!/bin/ksh 

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
      module load anaconda/anaconda2-4.4.0
      module use contrib/modulefiles
      module load met/8.0
      ;;
    *)
      ;;
  esac
  module list
elif [ "${machine}" = "jet" ] ; then
  set +x
  . /etc/profile
  . /etc/profile.d/modules.sh >/dev/null # Module Support
  source ${modulefile_jet}
  module list
  set -x

elif [ "${machine}" = "dell" ] ; then
  . /etc/profile
  . /usrx/local/prod/lmod/lmod/init/sh >/dev/null # Module Support 
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
      module use /gpfs/dell2/emc/verification/noscrub/Julie.Prestopnik/modulefiles/
      module load met/8.0
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
    SLURM|slurm)                                       # SLURM
      module load rocoto/1.3.0-RC5
      module load slurm/18.08

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

#----------- for Jet realtime run ------------------------------------------------------------------
elif [ "${machine}" = "jet" ] ;  then
  case ${SCHEDULER} in
    SLURM|slurm)
      set +x
      module load rocoto
      set -x
      np=${SLURM_NTASKS}
      export MPIRUN="srun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
  export job=${job:-"${SLURM_JOB_NAME}"}    # job is defined as job name
  export jid=`echo ${SLURM_JOB_ID} | cut -f1 -d.`  # removal of tailing sub-server string
  export jobid=${jobid:-"${job}.${jid}"}
  echo " number of cores : $np for job $job with id as $jobid "
  __ms_shell=ksh

  if [ -z "${rundir_task}" ]; then
    ${ECHO} "Fatal error: \$rundir_task is not defined"
    exit 1
  fi
  export DATA=${rundir_task} #Jet experimental run will not append ${jid} to rundir
  if [[ "${envir}"=="esrl"  ]]; then # Jet experimental runs
    export EXECrtma3d=${EXECrtma3d:-$HOMErtma3d/exec}
    export FIXrtma3d=${FIXrtma3d:-$HOMErtma3d/fix}
    export PARMrtma3d=${PARMrtma3d:-$HOMErtma3d/parm}
    export USHrtma3d=${USHrtma3d:-$HOMErtma3d/ush}
    export UTILrtma3d=${UTILrtma3d:-$HOMErtma3d/util}
    export LOG_PGMOUT="${COMROOT}/stdout/${PDY}"
    export pgmout="output_${PDY}.${job}"
    export LOG_JJOB="${COMROOT}/log/${PDY}"
    [ ! -d ${COMROOT}/loghistory ] && ${MKDIR} -p ${COMROOT}/loghistory
    export COMINobsproc_rtma3d="${COMROOT}/ptmp/obs/${PDY}"
    export COMOUTgsi_rtma3d="${COMROOT}/ptmp/gsi/${PDY}"
    export COMOUTwrf_rtma3d="${COMROOT}/ptmp/wrf/${PDY}"

    # the following is just to pass dir check for Jet esrl runs
    export GESINhrrr_rtma3d="/tmp" 
    export COMIN="/tmp"
    export COMOUT="/tmp"
    export COMINrap="/tmp"
    export COMINhrrr="/tmp"
  fi

#----------- for Dell ------------------------------------------------------------------------
elif [ "${machine}" = "dell" ] ; then  ### LSB scheduler

  case ${SCHEDULER} in
    LSF|lfs)
      module load lsf/10.1
      module load ruby/2.5.1
      module load rocoto/complete
      export np=`cat $PBS_NODEFILE | wc -l`
#      export MPIRUN="mpirun -np $np"
      export MPIRUN="mpirun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
  export job=${job:-"${LSB_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${LSB_JOBID} | cut -f1 -d.`  # removal of tailing sub-server string
  export jobid=${jobid:-"${job}.${jid}"}
  echo " number of cores : $np for job $job with id as $jobid "
  ############################################################
  # Notice: the following line is to                         #
  #            name the running directory with job name.     #
  #                              (not used for NCO.)         #
  ############################################################
  if [ "${rundir_task}" ]; then
      export DATA=${rundir_task}.${jid}
  fi

 
else
  
  export job=${job:-"${outid}.o$$"}
  export jobid=${jobid:-"${outid}.o$$"}
  export jid=$$
  export MPIRUN=${MPIRUN:-"mpirun"}

fi


$COMMAND
