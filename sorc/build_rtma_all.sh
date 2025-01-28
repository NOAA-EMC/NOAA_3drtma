set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_rtma_links=yes
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

if [ ! -d $BASE/logs ]; then
mkdir $BASE/logs
fi

export logs_dir=$BASE/logs
sleep 1

if [ ! -d $BASE/../exec ]; then 
mkdir $BASE/../exec 
fi

if [ $BUILD_rtma_links = yes ] ; then

mkdir $BASE/../parm
cd $BASE/../parm
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/wrf wrf
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/upp_new upp
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/gsi gsi

mkdir $BASE/../fix
cd $BASE/../fix
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/obsuselist obsuselist
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/UPP-fix upp
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/CRTM-fix crtm
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/GSI-fix gsi
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/minmaxtrh minmax
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/wps wps
cpreq -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/wrf wrf
fi

cd $BASE

##############################

if [ $BUILD_rtma_wrfpost = yes ] ; then

echo " .... Building rtma_post .... "
$BASE/build_rtma_post.sh > $logs_dir/build_rtma_post.log 2>&1

fi


if [ $BUILD_rtma_gsi = yes ] ; then

echo " .... Building rtma_gsi .... "
$BASE/build_rtma_gsi.sh > $logs_dir/build_rtma_gsi.log 2>&1

fi


module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0


##############################

if [ $BUILD_rtma_wrfarw = yes ] ; then

echo " .... Building rtma_wrfarw .... "
$BASE/build_rtma_wrfarw.sh > $logs_dir/build_rtma_wrfarw.log 2>&1

fi

##############################

if [ $BUILD_rtma_process_cloud = yes ] ; then

echo " .... Building rtma_process_cloud .... "
$BASE/build_rtma_process_cloud.sh > $logs_dir/build_process_cloud.log 2>&1

fi

##############################

if [ $BUILD_rtma_process_mosaic = yes ] ; then

echo " .... Building rtma_process_mosaic .... "
$BASE/build_rtma_process_mosaic.sh > $logs_dir/build_process_mosaic.log 2>&1

fi


##############################

if [ $BUILD_rtma_process_lightning = yes ] ; then

echo " .... Building rtma_process_lightning .... "
$BASE/build_rtma_process_lightning.sh > $logs_dir/build_process_lightning.log 2>&1

fi

##############################

if [ $BUILD_rtma_sndp = yes ] ; then

echo " .... Building rtma_sndp .... "
$BASE/build_rtma_sndp.sh > $logs_dir/build_sndp.log 2>&1

fi

##############################

if [ $BUILD_rtma_wrfbufr_conus = yes ] ; then

echo " .... Building rtma_wrfbufr_conus .... "
$BASE/build_rtma_wrfbufr_conus.sh > $logs_dir/build_wrfbufr_conus.log 2>&1

fi

##############################

if [ $BUILD_rtma_wrfbufr_alaska = yes ] ; then

echo " .... Building rtma_wrfbufr_alaska .... "
$BASE/build_rtma_wrfbufr_alaska.sh > $logs_dir/build_wrfbufr_alaska.log 2>&1

fi

##############################

if [ $BUILD_rtma_stnmlist = yes ] ; then

echo " .... Building rtma_stnmlist .... "
$BASE/build_rtma_stnmlist.sh > $logs_dir/build_stnmlist.log 2>&1

fi

##############################

if [ $BUILD_rtma_smartinit = yes ] ; then

echo " .... Building rtma_smartinit .... "
$BASE/build_rtma_smartinit.sh > $logs_dir/build_smartinit.log 2>&1

fi

##############################

if [ $BUILD_rtma_minmax = yes ] ; then

echo " .... Building rtma_minmax .... "
$BASE/build_rtma_minmax.sh > $logs_dir/build_minmax.log 2>&1

fi


##############################

cd $BASE
