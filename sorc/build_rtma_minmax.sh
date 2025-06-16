set -x

##############################

export BASE=`pwd`

module reset
module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/rtma_maxtbg.fd
make clean
make

cd ${BASE}/rtma_mintbg.fd
make clean
make

cd ${BASE}/rtma_maxrh.fd
module load w3emc
make clean
make

cd ${BASE}/rtma_minrh.fd
module load w3emc
make clean
make



##############################
