set -x

##############################

export BASE=`pwd`

module reset

cd ${BASE}/rtma3d_post.fd/tests
./compile_upp.sh

#cp -p ${BASE}/rtma3d_post.fd/exec/upp.x  ${BASE}/../exec/rtma3d_upp

##############################
