set -x

export BASE=`pwd`
cd $BASE

#build switches

export INSTALL_rtma3d_post=yes
export INSTALL_rtma3d_wrfarw=yes
export INSTALL_rtma3d_gsi=yes
export INSTALL_rtma3d_process_cloud=yes
export INSTALL_rtma3d_process_mosaic=yes
export INSTALL_rtma3d_process_lightning=yes
export INSTALL_rtma3d_sndp=yes
export INSTALL_rtma3d_wrfbufr_conus=yes
export INSTALL_rtma3d_wrfbufr_alaska=yes
export INSTALL_rtma3d_stnmlist=yes
export INSTALL_rtma3d_smartinit=yes
export INSTALL_rtma3d_obslist=yes
export INSTALL_rtma3d_read_diag=yes
export INSTALL_rtma3d_minmax=yes

if [ $INSTALL_rtma3d_post = yes ] ; then

cp -p ${BASE}/rtma3d_post.fd/exec/upp.x ../exec/rtma3d_upp

fi

if [ $INSTALL_rtma3d_wrfarw = yes ] ; then

# cp -p ${BASE}/rtma3d_wrfarw.fd/WRFV3.9/main/wrf.exe ../exec/rtma3d_wrfarw_fcst
cp -p ${BASE}/rtma3d_wrfarw.fd/WRFV3.9/main/wrf_nofcst.exe ../exec/rtma3d_wrfarw_fcst_nofcst
cp -p ${BASE}/rtma3d_wrfarw.fd/WRFV3.9/main/wrf_orig.exe   ../exec/rtma3d_wrfarw_fcst_orig

fi

if [ $INSTALL_rtma3d_gsi = yes ] ; then

cp -p ${BASE}/rtma3d_gsi.fd/build/src/gsi/gsi.x ../exec/rtma3d_gsi

fi

if [ $INSTALL_rtma3d_process_cloud = yes ] ; then

cp -p ${BASE}/rtma3d_process_cloud.fd/rtma3d_process_cloud ../exec/rtma3d_process_cloud

fi

if [ $INSTALL_rtma3d_process_mosaic = yes ] ; then

cp -p ${BASE}/rtma3d_process_mosaic.fd/rtma3d_process_mosaic ../exec/rtma3d_process_mosaic

fi

if [ $INSTALL_rtma3d_process_lightning = yes ] ; then

cp -p ${BASE}/rtma3d_process_lightning.fd/rtma3d_process_lightning ../exec/rtma3d_process_lightning

fi

if [ $INSTALL_rtma3d_sndp = yes ] ; then

cp -p ${BASE}/rtma3d_sndp.fd/rtma3d_sndp ../exec/rtma3d_sndp

fi

if [ $INSTALL_rtma3d_wrfbufr_conus = yes ] ; then

cp -p ${BASE}/rtma3d_wrfbufr_conus.fd/rtma3d_wrfbufr_conus ../exec/rtma3d_wrfbufr_conus

fi

if [ $INSTALL_rtma3d_wrfbufr_alaska = yes ] ; then

cp -p ${BASE}/rtma3d_wrfbufr_alaska.fd/rtma3d_wrfbufr_alaska ../exec/rtma3d_wrfbufr_alaska

fi

if [ $INSTALL_rtma3d_stnmlist = yes ] ; then

cp -p ${BASE}/rtma3d_stnmlist.fd/rtma3d_stnmlist ../exec/rtma3d_stnmlist

fi

if [ $INSTALL_rtma3d_smartinit = yes ] ; then

cp -p ${BASE}/rtma3d_smartinit.fd/rtma3d_smartinit ../exec/rtma3d_smartinit

fi

if [ $INSTALL_rtma3d_obslist = yes ] ; then

cp -p ${BASE}/rtma3d_obslist.fd/rtma3d_obslist ../exec/rtma3d_obslist

fi

if [ $INSTALL_rtma3d_read_diag = yes ] ; then

cp -p ${BASE}/rtma3d_read_diag.fd/rtma3d_read_diag.exe ../exec/rtma3d_read_diag

fi

if [ $INSTALL_rtma3d_minmax = yes ] ; then

cp -p ${BASE}/rtma3d_maxtbg.fd/rtma3d_maxtbg ../exec/rtma3d_maxtbg

cp -p ${BASE}/rtma3d_mintbg.fd/rtma3d_mintbg ../exec/rtma3d_mintbg

cp -p ${BASE}/rtma3d_maxrh.fd/rtma3d_maxrh ../exec/rtma3d_maxrh

cp -p ${BASE}/rtma3d_minrh.fd/rtma3d_minrh ../exec/rtma3d_minrh

fi
