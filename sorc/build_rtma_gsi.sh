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

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0

module list

cd ${BASE}/rtma_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/rtma_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make -j1

cpreq bin/gsi.x        ${BASE}/../exec/rtma_gsi
cpreq bin/enkf_wrf.x   ${BASE}/../exec/rtma_enkf
cpreq bin/enspreproc.x ${BASE}/../exec/rtma_process_enkf
cpreq bin/initialens.x ${BASE}/../exec/rtma_initialens

##############################
