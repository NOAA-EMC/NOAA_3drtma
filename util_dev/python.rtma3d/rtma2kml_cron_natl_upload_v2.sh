#!/bin/ksh

#job card goes here
#BSUB -o /gpfs/dell2/stmp/Matthew.T.Morris/kml_logfiles/urmakml_upload.%J
#BSUB -e /gpfs/dell2/stmp/Matthew.T.Morris/kml_logfiles/urmakml_upload.%J
#BSUB -W 0:20
#BSUB -w "ended(rtmakmlprod)"
#BSUB -J urmakmlftp
#BSUB -P RTMA-T2O
#BSUB -n 1
#BSUB -q "dev_transfer"
#BSUB -R "rusage[mem=500]"
#BSUB -R "affinity[core]"
#BSUB -cwd /gpfs/dell2/ptmp/Matthew.T.Morris

#abstract: Upload previously made KML non-restricted files to FTP server
#upload URMA files only

START=`date -u +%Y%m%d%H`
CYCLE=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate -2 $START`
UCYCLE=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate -8 $START`
DCYCLE=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate -168 $UCYCLE` #cycle to delete folder - 7 days old

set -x

COM_IN=/gpfs/dell2/ptmp/Matthew.T.Morris/rtmaplot_kml

YYYY=`echo $UCYCLE | cut -c 1-4`
YYYYMM=`echo $UCYCLE | cut -c 1-6`
YYYYMMDD=`echo $UCYCLE | cut -c 1-8`
HH=`echo $UCYCLE | cut -c 9-10`
ddate=`echo $DCYCLE | cut -c 1-8`
runs="urma2p5 akurma prurma hiurma"

COMIN=${COM_IN}/${YYYYMMDD}
cd ${COMIN}
export ftppath=/home/ftp/emc/mmb/rtma2/for_eval/ob_files

sftp mmorris@rzdm.ncep.noaa.gov <<EOF
cd ${ftppath}
mkdir ${YYYYMMDD}
cd ${YYYYMMDD}
put akurmaob_t${HH}z_temp_nr.kml
put akurmaob_t${HH}z_moist_nr.kml
put akurmaob_t${HH}z_pres_nr.kml
put akurmaob_t${HH}z_vis_nr.kml
put akurmaob_t${HH}z_gust_nr.kml
put akurmaob_t${HH}z_wspd10m_nr.kml
put akurmaob_t${HH}z_dwpt_nr.kml
put akurmaob_t${HH}z_cldch_nr.kml
put hiurmaob_t${HH}z_temp_nr.kml
put hiurmaob_t${HH}z_moist_nr.kml
put hiurmaob_t${HH}z_pres_nr.kml
put hiurmaob_t${HH}z_vis_nr.kml
put hiurmaob_t${HH}z_gust_nr.kml
put hiurmaob_t${HH}z_dwpt_nr.kml
put hiurmaob_t${HH}z_cldch_nr.kml
put prurmaob_t${HH}z_temp_nr.kml
put prurmaob_t${HH}z_moist_nr.kml
put prurmaob_t${HH}z_pres_nr.kml
put prurmaob_t${HH}z_vis_nr.kml
put prurmaob_t${HH}z_gust_nr.kml
put prurmaob_t${HH}z_dwpt_nr.kml
put prurmaob_t${HH}z_cldch_nr.kml
put urma2p5ob_t${HH}z_temp_nr.kml
put urma2p5ob_t${HH}z_moist_nr.kml
put urma2p5ob_t${HH}z_pres_nr.kml
put urma2p5ob_t${HH}z_vis_nr.kml
put urma2p5ob_t${HH}z_gust_nr.kml
put urma2p5ob_t${HH}z_wspd10m_nr.kml
put urma2p5ob_t${HH}z_dwpt_nr.kml
put urma2p5ob_t${HH}z_cldch_nr.kml
bye

EOF

if [ ${HH} -eq "08" ]; then
    sftp mmorris@rzdm.ncep.noaa.gov <<EOF
cd ${ftppath}/${YYYYMMDD}
put akurmaob_t${HH}z_mxtm_nr.kml
put hiurmaob_t${HH}z_mxtm_nr.kml
put prurmaob_t${HH}z_mxtm_nr.kml
put urma2p5ob_t${HH}z_mxtm_nr.kml
bye
EOF
fi

if [ ${HH} -eq "20" ]; then
    sftp mmorris@rzdm.ncep.noaa.gov <<EOF
cd ${ftppath}/${YYYYMMDD}
put akurmaob_t${HH}z_mitm_nr.kml
put hiurmaob_t${HH}z_mitm_nr.kml
put prurmaob_t${HH}z_mitm_nr.kml
put urma2p5ob_t${HH}z_mitm_nr.kml
bye
EOF
fi

if [ ${HH} -eq "00" ] ; then
    sftp mmorris@rzdm.ncep.noaa.gov <<EOF
cd ${ftppath}/${ddate}
rm *kml
cd ${ftppath}
rmdir ${ddate}
bye
EOF
fi

exit
