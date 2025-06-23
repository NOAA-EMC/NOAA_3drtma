set -x

##############################

export BASE=`pwd`

module reset
moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}
source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua
module list

cd ${BASE}/rtma3d_maxtbg.fd
make clean
make

cd ${BASE}/rtma3d_mintbg.fd
make clean
make

cd ${BASE}/rtma3d_maxrh.fd
make clean
make

cd ${BASE}/rtma3d_minrh.fd
make clean
make



##############################
