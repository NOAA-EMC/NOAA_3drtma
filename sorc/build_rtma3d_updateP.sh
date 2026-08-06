set -x

##############################

export BASE=`pwd`

set +x

module reset
moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}

source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua
module list

set -x

cd ${BASE}/rtma3d_updateP.fd
make -f makefile_wcoss2 clean
make -f makefile_wcoss2

[[ -x rtma3d_updateP ]] || echo "Warning --> rtma3d_updateP is not generated successfully. Check!"

##############################
