set -x

##############################

export BASE=`pwd`

module reset

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/rtma_process_cloud.fd
make clean
make

##############################
