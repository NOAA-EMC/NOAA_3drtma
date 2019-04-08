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

if [ "${machine}" = "jet" ] ; then

# loading modules in general module file
  . ${MODULEFILES}/${machine}/run/modulefile.rtma3d_rt.run.${machine}

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
      export WGRIB2="/home/rtrr/HRRR/exec/UPP/wgrib2"    # 2.0.7 (used in GSD rap/hrrr)
      export WGRIB2=${WGRIB2:-"wgrib"}
      ;;
    *)
      export MPIRUN=${MPIRUN:-"mpirun"}
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
