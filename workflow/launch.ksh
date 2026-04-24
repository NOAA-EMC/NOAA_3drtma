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
versionfile=${versionfile:-${HOMErtma3d}/versions/run.ver}
MODULEFILES_GSI=${MODULEFILES_GSI:-${HOMErtma3d}/sorc/rtma3d_gsi.fd/modulefiles}
MODULEFILES_UPP=${MODULEFILES_UPP:-"${HOMErtma3d}/sorc/rtma3d_post.fd/modulefiles"}

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
      module load met/8.0
      ;;
    *)
      ;;
  esac
  module list

elif [ "${machine}" = "dell" ] ; then
  . /etc/profile
  . /usrx/local/prod/lmod/lmod/init/sh >/dev/null # Module Support 
#  module purge
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

elif [ "${machine}" = "cray" ] ; then
  . /etc/profile
  . /usr/share/lmod/lmod/init/sh >/dev/null # Module Support 
#  module purge
# loading modules used when building the code
# loading modules for running
# modulefile_run=${modulefile_run:-"${MODULEFILES}/${machine}/run/modulefile.run.${machine}"}
# source ${modulefile_run}
# loading modules for specific task
# case "$COMMAND" in
#   *VERIF*)
#     module use /gpfs/dell2/emc/verification/noscrub/Julie.Prestopnik/modulefiles/
#     module load met/8.0
#     ;;
#   *)
#     ;;
# esac

case "$COMMAND" in
  # loading modules for GSI
  *GSIANL*)
    echo "loading modules for running new GSI ..."
    module reset
    module use ${MODULEFILES_GSI}
    module load envvar/1.0
    module load gsi_wcoss2.intel
    module load cray-pals/1.2.2
    ;;
  # loading modules for UPP
  *POST*)
    echo "loading modules for running new UPP ..."
    module reset
    module use ${MODULEFILES_UPP}
    module load envvar/1.0
    module load wcoss2_intel
    module load cray-pals/1.0.12
    module load libjpeg/9c
    module load prod_util/2.0.14
    module load wgrib2/2.0.8
    module load cfp/2.0.4
    ;;
  # loading modules for other tasks
  *)
. ${versionfile} 

#module reset
module purge

module load envvar/${envvar_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
module load cray-mpich/${cray_mpich_ver}
module load ip/${ip_ver}
module load bufr/${bufr_ver}
module load sfcio/${sfcio_ver}
module load sigio/${sigio_ver}
module load gfsio/${gfsio_ver}
module load sp/${sp_ver}
module load bacio/${bacio_ver}
module load jasper/${jasper_ver}
module load libpng/${libpng_ver}
module load zlib/${zlib_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf_ver}
module load cray-pals/${cray_pals_ver}
module load g2/${g2_ver}
module load g2tmpl/${g2tmpl_ver}
module load udunits/${udunits_ver}
module load gsl/${gsl_ver}
module load nco/${nco_ver}
module load crtm/${crtm_ver}
module load libxmlparse/${libxmlparse_ver}
module load libjpeg/${libjpeg_ver}
module load wgrib2/${wgrib2_ver}
module load grib_util/${grib_util_ver}
module load cfp/${cfp_ver}
module load python/${python_ver}
module load python-modules/${python_ver}
#module load prod_envir/${prod_envir_ver}
module load prod_util/${prod_util_ver}
#module load w3nco/2.4.1
module load ncdiag/${ncdiag_ver}
  ;;
esac

module list

export RM=/bin/rm
export CP=/bin/cp
export MV=/bin/mv
export LN=/bin/ln
export MKDIR=/bin/mkdir
export CAT=/bin/cat
export ECHO=/bin/echo
export LS=/bin/ls
export CUT=/bin/cut
export WC=/usr/bin/wc
export DATE=/bin/date
export AWK=/bin/awk
export SED=/bin/sed
export TAIL=/usr/bin/tail
export BC=/usr/bin/bc
export WHICH=/usr/bin/which
export GREP=/usr/bin/grep
export UNZIP=/bin/unzip
export TOUCH=/usr/bin/touch

# for mail
  export MAILX=/usr/bin/mailx       # env variable MAIL is used by linux system
  export TO_RECIPIENTS="${USER}@noaa.gov"
  export CC_RECIPIENTS="annette.gibbs@noaa.gov,matthew.t.morris@noaa.gov,manuel.pondeca@noaa.gov,gang.zhao@noaa.gov"
# export BCC_RECIPIENTS=""

else
  echo "modulefile has not set up for this unknow machine. Job abort!"
  exit 1
fi


###########################################################################
# obtain unique process id (pid) and define the name of  temp directories
###########################################################################
if [ "${machine}" = "theia" ] ; then    ### PBS job Scheduler

  case ${SCHEDULER} in
    SLURM|slum)                                       # SLURM
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

elif [ "${machine}" = "jet" ] ;  then    ### PBS job Scheduler

  case ${SCHEDULER} in
    SLURM|slum)                                       # SLURM
      module load rocoto/1.3.0-RC5
      module load slurm/18.08.7p1

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

elif [ "${machine}" = "cray" ] ; then  ### LSB scheduler

  case ${SCHEDULER} in
    PBSPRO|pbspro)
      module use /apps/ops/test/nco/modulefiles/core
      module load rocoto
      export MPIRUN="mpirun"
      ;;
    *)
      echo "unknown scheduler: ${SCHEDULER}. $0 abort! "
      exit 1
      ;;
  esac
  export job=${job:-"${PBS_JOBNAME}"}    # job is defined as job name
  export jid=`echo ${PBS_JOBID} | cut -f1 -d.`  # removal of tailing sub-server string
  export jobid=${jobid:-"${job}.${jid}"}
  echo " number of cores : $np for job $job with id as $jobid "

 
else
  
  export job=${job:-"${outid}.o$$"}
  export jobid=${jobid:-"${outid}.o$$"}
  export jid=$$
#  export MPIRUN=${MPIRUN:-"mpirun"}

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
