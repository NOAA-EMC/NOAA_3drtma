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
export BUILD_rtma_obslist=yes
export BUILD_rtma3d_read_diag=yes
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

[[ -d $BASE/../parm ]] || mkdir $BASE/../parm
cd $BASE/../parm
rm -rf ./wrf ./upp ./gsi ./rtma3d ./akrtma3d
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/wrf wrf
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/upp_new upp
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/rtma3d rtma3d
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/akrtma3d akrtma3d
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/parm/gsi gsi

[[ -d $BASE/../fix ]] || mkdir $BASE/../fix
cd $BASE/../fix
rm -rf ./obsuselist ./upp ./crtm ./gsi ./minmax ./wps ./wrf ./wrfbufr ./rtma3d ./akrtma3d
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/obsuselist obsuselist
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/UPP-fix upp
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/CRTM-fix crtm
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/GSI-fix gsi
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/minmaxtrh minmax
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/wps wps
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/wrf wrf
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/wrfbufr wrfbufr
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/rtma3d rtma3d
cp -r /lfs/h2/emc/da/noscrub/edward.colon/FixData/akrtma3d akrtma3d
fi

cd $BASE

##############################

if [ $BUILD_rtma_wrfpost = yes ] ; then

echo " .... Building rtma_post .... "
$BASE/build_rtma_post.sh > $logs_dir/build_rtma_post.log 2>&1

fi

##############################

if [ $BUILD_rtma_gsi = yes ] ; then

   unset GSI_SOURCE
   export GSI_SOURCE="emcgsi"       # emc-gsi/emc_gsi/emcgsi: using official EMC GSI
                                    # auto-qc/auto_qc/autoqc: using Matthew Morris's fork of GSI
                                    # if not defined, using official GSI (as default for now)
   echo " .... Building rtma_gsi with GSI_SOURCE=${GSI_SOURCE} .... "
   $BASE/build_rtma_gsi.sh > $logs_dir/build_rtma_gsi.log 2>&1
   cp  /lfs/h2/emc/da/noscrub/edward.colon/NOAA_3drtma_new/exec/ncdiag_cat_serial.x ../exec
   unset GSI_SOURCE

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

if [ $BUILD_rtma_obslist = yes ] ; then

echo " .... Building rtma_obslist .... "
$BASE/build_rtma_obslist.sh > $logs_dir/build_obslist.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_read_diag = yes ] ; then

echo " .... Building rtma3d_read_diag .... "
$BASE/build_rtma3d_read_diag.sh > $logs_dir/build_rtma3d_read_diag.log 2>&1
cp $BASE/rtma3d_read_diag.fd/rtma3d_read_diag.exe ../exec

fi

##############################

if [ $BUILD_rtma_minmax = yes ] ; then

echo " .... Building rtma_minmax .... "
$BASE/build_rtma_minmax.sh > $logs_dir/build_minmax.log 2>&1

fi

##############################

cd $BASE
