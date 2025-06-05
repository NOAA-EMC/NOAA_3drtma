set -x

##############################

export BASE=`pwd`

module reset

moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}
source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua
module list

cd ${BASE}/rtma3d_process_cloud.fd
make clean
make

##############################
