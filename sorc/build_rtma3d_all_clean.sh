set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_rtma3d_wrfpost=yes
export BUILD_rtma3d_wrfarw=yes
export BUILD_rtma3d_gsi=yes
export BUILD_rtma3d_process_cloud=yes
export BUILD_rtma3d_process_mosaic=yes
export BUILD_rtma3d_process_lightning=yes
export BUILD_rtma3d_sndp=yes
export BUILD_rtma3d_wrfbufr_conus=yes
export BUILD_rtma3d_wrfbufr_alaska=yes
export BUILD_rtma3d_stnmlist=yes
export BUILD_rtma3d_smartinit=yes
export BUILD_rtma3d_obslist=yes
export BUILD_rtma3d_read_diag=yes
export BUILD_rtma3d_minmax=yes

module reset
moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}
source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua
module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

if [ $BUILD_rtma3d_wrfpost = yes ] ; then

cd ${BASE}/rtma3d_wrfpost.fd
make clean

fi


if [ $BUILD_rtma3d_wrfarw = yes ] ; then

cd ${BASE}/rtma3d_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean

fi

if [ $BUILD_rtma3d_gsi = yes ] ; then

cd ${BASE}/rtma3d_gsi.fd
make clean

fi

if [ $BUILD_rtma3d_sndp = yes ] ; then

cd ${BASE}/rtma3d_sndp.fd
make clean

fi


if [ $BUILD_rtma3d_process_mosaic = yes ] ; then

cd ${BASE}/rtma3d_process_mosaic.fd
make clean

fi

if [ $BUILD_rtma3d_process_lightning = yes ] ; then

cd ${BASE}/rtma3d_process_lightning.fd
make clean

fi

if [ $BUILD_rtma3d_wrfbufr_conus = yes ] ; then

cd ${BASE}/rtma3d_wrfbufr_conus.fd
make clean

fi

if [ $BUILD_rtma3d_wrfbufr_alaska = yes ] ; then

cd ${BASE}/rtma3d_wrfbufr_alaska.fd
make clean

fi

if [ $BUILD_rtma3d_stnmlist = yes ] ; then

cd ${BASE}/rtma3d_stnmlist.fd

make clean

fi

if [ $BUILD_rtma3d_smartinit = yes ] ; then

cd ${BASE}/rtma3d_smartinit.fd
make clean

fi

if [ $BUILD_rtma3d_obslist = yes ] ; then

cd ${BASE}/rtma3d_obslist.fd
make clean

fi

if [ $BUILD_rtma3d_read_diag = yes ] ; then

cd ${BASE}/rtma3d_read_diag.fd
make clean

fi

if [ $BUILD_rtma3d_process_cloud = yes ] ; then

cd ${BASE}/rtma3d_process_cloud.fd
make clean

fi


if [ $BUILD_rtma3d_minmax = yes ] ; then

cd ${BASE}/rtma3d_maxtbg.fd
make clean 
cd ..
cd ${BASE}/rtma3d_mintbg.fd
make clean
cd ..
cd ${BASE}/rtma3d_maxrh.fd
make clean
cd ..
cd ${BASE}/rtma3d_minrh.fd
make clean
cd ..

fi

if [ $BUILD_rtma3d_process_cloud = yes ] ; then

cd ${BASE}/rtma3d_process_cloud.fd
make clean

fi

rm -f ${BASE}/../exec/*
