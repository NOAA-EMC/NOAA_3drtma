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
[[ -d ./rtma_gsi.fd ]] && rm -rf ./rtma_gsi.fd

# get the gsi source code package from repository on github
  echo "git clone https://github.com/NOAA-EMC/GSI.git  ./rtma_gsi.fd"
  git clone https://github.com/NOAA-EMC/GSI.git  ./rtma_gsi.fd
  cd ${BASE}/rtma_gsi.fd
  echo "git checkout develop"
  git checkout develop       # <--- checking out the latest commit of develop branch
# git checkout 0ef8d87       # <--- specifying the commit with updates for analysis fof howv and gust

  [[ -d ./build ]] && rm -fr build
  mkdir build

  cd ush
#  use the gsi building script
  if [[ -f ${BASE}/../util_dev/build_gsi4rtma3d.sh ]] ; then
     cp -p ${BASE}/../util_dev/build_gsi4rtma3d.sh  ./
    ./build_gsi4rtma3d.sh
  else
     echo "cannot find the GSI building script build_gsi4rtma3d.sh, abort build GSI ..."
  fi

cp -p ${BASE}/rtma_gsi.fd/build/src/gsi/gsi.x        ${BASE}/../exec/rtma_gsi
# cp -p ${BASE}/rtma_gsi.fd/install/bin/gsi.x        ${BASE}/../exec/rtma_gsi
cp -p ${BASE}/rtma_gsi.fd/build/src/enkf/enkf.x      ${BASE}/../exec/rtma_enkf
# cp -p ${BASE}/rtma_gsi.fd/install/bin/enkf.x       ${BASE}/../exec/rtma_enkf
#cpreq bin/enspreproc.x ${BASE}/../exec/rtma_process_enkf
#cpreq bin/initialens.x ${BASE}/../exec/rtma_initialens

##############################
