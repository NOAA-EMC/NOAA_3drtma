#!/bin/ksh --login

set -x 

PROD_HEAD=rtma3d
DATA_RHIST=${OUTDIR}/grib2_wrkdir_${YMD}${cyc}${subcyc}
CHECK_HPSS_IDX=YES
tarfile_1=com2_rtma3d
YYYY=`echo ${YMD} | cut -c 1-4`
YYYYMM=`echo ${YMD} | cut -c 1-6`
NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate
YMDm1=`$NDATE -24 ${YMD}${cyc} | cut -c 1-8`
export DATAIN=${COMRTMA3D}/rtma3d.${YMD}/postprd.t${cyc}${subcyc}z
export HPSSOUT=/NCEPDEV/emc-meso/5year/Edward.Colon/${PROD_HEAD}

hpssdir0=${HPSSOUT}/rh${YYYY}/${YYYYMM}/${YMD}

hpssdir=$hpssdir0
tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
tarfile=${tarfile}.${YMD}${cyc}${subcyc}.tar

if [ ! -d ${DATAIN} ] ; then
	mkdir -p ${DATAIN}
fi

cd  ${DATAIN}


#    /bin/rm -rf select_list_1.txt_tmp
#    /bin/rm -rf select_list_1.txt


    htar -vtf ${hpssdir}/${tarfile}  > list_all_1.txt

    cat list_all_1.txt | grep ${PROD_HEAD}.t${cyc}${subcyc}z | grep wrfsubhprs >> select_list_1.txt_tmp
    cat list_all_1.txt | grep ${PROD_HEAD}.t${cyc}${subcyc}z | grep wrfsubhnat >> select_list_1.txt_tmp

    nlines=`wc -l select_list_1.txt_tmp`
    nlines=${nlines% select*}
    echo "nlines ="$nlines

    it=1
     while [ $it -le $nlines ] ; do
       var="`cat select_list_1.txt_tmp | head -n $it | tail  -1`"
       echo "./"${var#* ./} >> select_list_1.txt
       let "it=it+1"
    done
    htar -xvf ${hpssdir}/${tarfile} -L select_list_1.txt


exit
