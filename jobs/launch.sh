#!/bin/sh -l

# --- for debug --- #
date
export PS4=' $SECONDS + ' 
set -x

COMMAND=$1

#############################################################
# load modulefile and set up the environment for job runnning
#############################################################
MODULEFILES=${MODULEFILES:-${HOMErtma3d}/modulefiles}
target=${ENV_MODULES:-"ProdGSI"}

if [ "${machine}" = "theia" ] ; then

  # . /etc/profile
  # . /apps/lmod/lmod/init/sh >/dev/null # Module Support
  module purge

# load modules used when compiling the code
  modulefile=${modulefile:-"${MODULEFILES}/modulefile.${target}.${machine}"}
  source $modulefile
# load extra modules to run the job
  modulefile_run=${modulefile_run:-"${MODULEFILES}/modulefile.RUN.${machine}"}
  source $modulefile_run
  module list
else
  echo "modulefile has not set up for this unknow machine. Job abort!"
  exit 1
fi

##########################################################
# obtain unique process id (pid) and make temp directories
##########################################################
if [ "${machine}" = "theia" ] ; then    ### PBS job Scheduler
  export job=${job:-"${PBS_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${PBS_JOBID} | cut -f1 -d.`  # removal of tailing sub-server string
# export jid=`echo ${PBS_JOBID} | awk -F'.' '{print $1}'`
  export jobid=${jobid:-"${job}.${jid}"}
  export np=`cat $PBS_NODEFILE | wc -l`
  export NCDUMP="ncdump"
  export MPIRUN=${MPIRUN:-"mpirun -np $np"}
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
# Notice: this is not used for NCO                         #
#         pre-defined running directory                    #
############################################################
if [ "${rundir_task}" ]; then
  export DATA=${rundir_task}.${jid}
fi

# if [ $RUN_ENVIR == 'dev' ]; then
#   if [ $MACHINE = 'theia' ]; then
#     . /etc/profile
#     . /apps/lmod/lmod/init/ksh >/dev/null # Module Support
#   else
#     . /usrx/local/Modules/3.2.9/init/ksh
#   fi
#   module use -a ${MODULEFILES}
#   module load run/v2.7.0
# fi

$COMMAND
