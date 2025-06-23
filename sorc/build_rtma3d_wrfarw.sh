set -x

##############################

export BASE=`pwd`
cd $BASE

module reset
moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}
source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua

#module load craype-hugepages256M

echo "modules for rtma wrf"
module list

sleep 1

cd ${BASE}/rtma3d_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp -p configure.wrf.useme configure.wrf

export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_DFI_RADAR=1
export WRF_SMOKE=1

./compile -j 1 em_real

#cp -p main/real.exe ${BASE}/../exec/rtma3d_wrfarw_real
#cp -p main/wrf.exe ${BASE}/../exec/rtma3d_wrfarw_fcst

##############################
