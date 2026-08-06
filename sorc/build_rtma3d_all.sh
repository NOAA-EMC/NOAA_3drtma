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

if [ ! -d $BASE/logs ]; then
mkdir $BASE/logs
fi

export logs_dir=$BASE/logs
sleep 1

if [ ! -d $BASE/../exec ]; then 
mkdir $BASE/../exec 
fi

cd $BASE

##############################

if [ $BUILD_rtma3d_wrfpost = yes ] ; then

   unset  UPP_SOURCE
   export UPP_SOURCE="local"        # local : using local copy of UPP in 3DRTMA package (default)
                                    #         (sorc/rtma3d_sorc_info.fd/README_sorc_info.md for version detail)
                                    # emcupp: using official EMC UPP
                                    # any other string: using local copy of UPP in 3DRTMA package
   echo " .... Building rtma3d_post with UPP_SOURCE=${UPP_SOURCE} .... "
   $BASE/build_rtma3d_post.sh > $logs_dir/build_rtma3d_post.log 2>&1
   unset  UPP_SOURCE

fi

##############################

if [ $BUILD_rtma3d_gsi = yes ] ; then

   unset  GSI_SOURCE
   export GSI_SOURCE="local"        # local : using local copy of GSI in 3DRTMA package (default)
                                    #         (sorc/rtma3d_sorc_info.fd/README_sorc_info.md for version detail)
                                    # emcgsi: using official EMC GSI
                                    # autoqc: using Matthew Morris's fork of GSI (might be outdated)
                                    # any other string: using local copy of GSI in 3DRTMA package
   echo " .... Building rtma3d_gsi with GSI_SOURCE=${GSI_SOURCE} .... "
   $BASE/build_rtma3d_gsi.sh > $logs_dir/build_rtma3d_gsi.log 2>&1
   unset  GSI_SOURCE

fi

moduledir=`dirname $(readlink -f ../modulefiles/RTMA3D)`
module use ${moduledir}
source ../versions/build.ver
module load RTMA3D/${rtma3d_ver}.lua
module list

##############################

if [ $BUILD_rtma3d_wrfarw = yes ] ; then

echo " .... Building rtma3d_wrfarw .... "
$BASE/build_rtma3d_wrfarw.sh > $logs_dir/build_rtma3d_wrfarw.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_process_cloud = yes ] ; then

echo " .... Building rtma3d_process_cloud .... "
$BASE/build_rtma3d_process_cloud.sh > $logs_dir/build_process_cloud.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_process_mosaic = yes ] ; then

echo " .... Building rtma3d_process_mosaic .... "
$BASE/build_rtma3d_process_mosaic.sh > $logs_dir/build_process_mosaic.log 2>&1

fi


##############################

if [ $BUILD_rtma3d_process_lightning = yes ] ; then

echo " .... Building rtma3d_process_lightning .... "
$BASE/build_rtma3d_process_lightning.sh > $logs_dir/build_process_lightning.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_sndp = yes ] ; then

echo " .... Building rtma3d_sndp .... "
$BASE/build_rtma3d_sndp.sh > $logs_dir/build_sndp.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_wrfbufr_conus = yes ] ; then

echo " .... Building rtma3d_wrfbufr_conus .... "
$BASE/build_rtma3d_wrfbufr_conus.sh > $logs_dir/build_wrfbufr_conus.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_wrfbufr_alaska = yes ] ; then

echo " .... Building rtma3d_wrfbufr_alaska .... "
$BASE/build_rtma3d_wrfbufr_alaska.sh > $logs_dir/build_wrfbufr_alaska.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_stnmlist = yes ] ; then

echo " .... Building rtma3d_stnmlist .... "
$BASE/build_rtma3d_stnmlist.sh > $logs_dir/build_stnmlist.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_smartinit = yes ] ; then

echo " .... Building rtma3d_smartinit .... "
$BASE/build_rtma3d_smartinit.sh > $logs_dir/build_smartinit.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_obslist = yes ] ; then

echo " .... Building rtma3d_obslist .... "
$BASE/build_rtma3d_obslist.sh > $logs_dir/build_obslist.log 2>&1

fi

##############################

if [ $BUILD_rtma3d_read_diag = yes ] ; then

echo " .... Building rtma3d_read_diag .... "
$BASE/build_rtma3d_read_diag.sh > $logs_dir/build_rtma3d_read_diag.log 2>&1
#cp $BASE/rtma3d_read_diag.fd/rtma3d_read_diag.exe ../exec

fi

##############################

if [ $BUILD_rtma3d_minmax = yes ] ; then

echo " .... Building rtma3d_minmax .... "
$BASE/build_rtma3d_minmax.sh > $logs_dir/build_minmax.log 2>&1

fi


##############################

cd $BASE
