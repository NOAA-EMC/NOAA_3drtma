set -x

##############################

export BASE=`pwd`
cd $BASE

module reset

# Load modules
export COMP=ftn
export COMP_MP=ftn
export COMP_MPI=ftn

export C_COMP=cc
export C_COMP_MP=cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

#module use $BASE/../modulefiles
#source $BASE/../modulefiles/HRRR/v4.0.0

module list

cd ${BASE}
if [[ ! -d ./rtma3d_gsi.fd ]] ; then
    echo " ****** WARNING WARNING WARNING ****** "
    echo " ****** WARNING WARNING WARNING ****** "
    echo " ****** WARNING WARNING WARNING ****** "
    echo " No GSI source code package under sorc/, Abort the building process. Please check your 3DRTMA package "
    exit 1
fi

# get the gsi source code package from repository on github
  cd ${BASE}/rtma3d_gsi.fd

  BUILD_GSI_SCRIPT="build_gsi4rtma3d.sh"
# BUILD_GSI_SCRIPT="build_gsi4rtma3d_bufr.sh" # for old Matthew Morris' GSI

  [[ -d ./build ]] && rm -fr build
  mkdir build

  cd ush
#  use the gsi building script
  if [[ -f ./${BUILD_GSI_SCRIPT} ]] ; then
      ./${BUILD_GSI_SCRIPT}
  elif [[ -f ${BASE}/../util_dev/${BUILD_GSI_SCRIPT} ]] ; then
      cp -p ${BASE}/../util_dev/${BUILD_GSI_SCRIPT}  ./
      ./${BUILD_GSI_SCRIPT}
  else
      echo " ****** WARNING WARNING WARNING ****** "
      echo " ****** WARNING WARNING WARNING ****** "
      echo " ****** WARNING WARNING WARNING ****** "
      echo "cannot find the GSI building script build_gsi4rtma3d.sh, abort building GSI ..."
      exit 2
  fi

#cp -p ${BASE}/rtma3d_gsi.fd/build/src/gsi/gsi.x        ${BASE}/../exec/rtma3d_gsi
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/gsi.x        ${BASE}/../exec/rtma3d_gsi
#cp -p ${BASE}/rtma3d_gsi.fd/build/src/enkf/enkf.x      ${BASE}/../exec/rtma3d_enkf
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/enkf.x       ${BASE}/../exec/rtma3d_enkf
#cp bin/enspreproc.x ${BASE}/../exec/rtma3d_process_enkf
#cp bin/initialens.x ${BASE}/../exec/rtma3d_initialens

##############################
