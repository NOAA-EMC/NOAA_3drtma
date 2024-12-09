set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_rtma_wrfpost=yes
export BUILD_rtma_wrfarw=yes
export BUILD_rtma_gsi=yes
export BUILD_rtma_process_cloud=yes
export BUILD_rtma_process_mosaic=yes
export BUILD_rtma_process_lightning=yes
export BUILD_rtma_sndp=yes
export BUILD_rtma_wrfbufr_conus=yes
export BUILD_rtma_wrfbufr_alaska=yes
export BUILD_rtma_stnmlist=yes
export BUILD_rtma_smartinit=yes
export BUILD_rtma_minmax=yes

module reset

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0

module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

if [ $BUILD_rtma_wrfpost = yes ] ; then

cd ${BASE}/rtma_wrfpost.fd
make clean

fi


if [ $BUILD_rtma_wrfarw = yes ] ; then

cd ${BASE}/rtma_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean

fi

if [ $BUILD_rtma_gsi = yes ] ; then

cd ${BASE}/rtma_gsi.fd
make clean

fi

if [ $BUILD_rtma_sndp = yes ] ; then

cd ${BASE}/rtma_sndp.fd
make clean

fi


if [ $BUILD_rtma_process_mosaic = yes ] ; then

cd ${BASE}/rtma_process_mosaic.fd
make clean

fi

if [ $BUILD_rtma_process_lightning = yes ] ; then

cd ${BASE}/rtma_process_lightning.fd
make clean

fi

if [ $BUILD_rtma_wrfbufr_conus = yes ] ; then

cd ${BASE}/rtma_wrfbufr_conus.fd
make clean

fi

if [ $BUILD_rtma_wrfbufr_alaska = yes ] ; then

cd ${BASE}/rtma_wrfbufr_alaska.fd
make clean

fi

if [ $BUILD_rtma_stnmlist = yes ] ; then

cd ${BASE}/rtma_stnmlist.fd

make clean

fi

if [ $BUILD_rtma_smartinit = yes ] ; then

cd ${BASE}/rtma_smartinit.fd
make clean

fi

if [ $BUILD_rtma_process_cloud = yes ] ; then

cd ${BASE}/rtma_process_cloud.fd
make clean

fi


if [ $BUILD_rtma_minmax = yes ] ; then

cd ${BASE}/rtma_maxtbg.fd
make clean 
cd ..
cd ${BASE}/rtma_mintbg.fd
make clean
cd ..
cd ${BASE}/rtma_maxrh.fd
make clean
cd ..
cd ${BASE}/rtma_minrh.fd
make clean
cd ..

fi

if [ $BUILD_rtma_process_cloud = yes ] ; then

cd ${BASE}/rtma_process_cloud.fd
make clean

fi

rm -f ${BASE}/../exec/*
