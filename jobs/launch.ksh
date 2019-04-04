#!/bin/ksh -l

# --- for debug --- #
date
export PS4=' $SECONDS + ' 
set -x

COMMAND=$1

#############################################################
# load modulefile and set up the environment for job runnning
#############################################################
# MODULEFILES=${MODULEFILES:-${HOMErtma3d}/modulefiles}

if [ "${machine}" = "jet" ] ; then
  . /etc/profile
  . /etc/profile.d/modules.sh >/dev/null # Module Support
  module purge
  module load intel/18.0.5.274
  module load impi/2018.4.274
  module load szip
  module load hdf5/1.8.9
  module load netcdf/4.2.1.1
  module load pnetcdf/1.6.1
  export MPIRUN=mpirun
# loading modules used when building the code
  case "$COMMAND" in
    *LIGHTNING*|*SATELLITE*|*GSI_DIAG*)
      export TAIL=/usr/bin/tail
      export MPIRUN=mpiexec
      ;;
    *GSI_HYB*)
      export TAIL=/usr/bin/tail
      export CNVGRIB=/apps/cnvgrib/1.2.3/bin/cnvgrib
      export MPIRUN=mpirun
      ;;
    *POST*)
#     module load newdefaults
      module load nco
      module load cnvgrib
      module load wgrib
      module load wgrib2
      module unload pnetcdf/1.6.1
      export BC=/usr/bin/bc
      export MPIRUN=mpirun
      export WGRIB2=${EXE_ROOT}/wgrib2_new
      ;;
    *)
      export MPIRUN=mpirun
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

############################################################
# Notice: the following line is to                         #
#            name the running directory with job name.     #
#                              (not used for NCO.)         #
############################################################
if [ "${rundir_task}" ]; then
  export DATA=${rundir_task}.${jid}
fi

$COMMAND
