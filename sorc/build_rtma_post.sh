set -x

##############################

export BASE=`pwd`

module reset
#module use $BASE/../modulefiles
#source $BASE/../modulefiles/HRRR/v4.0.0
#module list

cd ${BASE}/rtma_post.fd/tests
./compile_upp.sh

cp  ${BASE}/rtma_post.fd/exec/upp.x  ${BASE}/../exec

##############################
