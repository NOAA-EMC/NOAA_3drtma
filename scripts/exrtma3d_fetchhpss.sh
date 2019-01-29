#!/bin/ksh
##########################################################################
####  UNIX Script Documentation Block                                    #
#                                                                        #
# Script name:  ext${NET}_hpssfetch.sh                                     #
# Script description: - convert output to grads format.                  #
#                                                                        #
# Author:      Manuel Pondeca        Org: NP22         Date: 2005-10-30  #
#                                                                        #
# Script history log:                                                    #
##########################################################################

set -x 

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"


#========================= begin changes =======================================================
envir=${envir}
CYCLE=${PDY}${cyc}
CYCLE_LAST=${CYCLE}
CYCLE_p1=${CYCLE}
mynoscrub=${COMIN}
mynoscrubm1=${COMINm1}
CYCLE_3=`$NDATE -0 $CYCLE`
CYCLE_LAST=`$NDATE +1 $CYCLE`
YYYYMMDDm1=`$NDATE -24 ${CYCLE} | cut -c 1-8`
#=========================  end changes  =======================================================

i=0
while [ $CYCLE_3 -le $CYCLE_LAST ] ; do

    let "i=i+1"

    YYYY=`echo $CYCLE_3 | cut -c 1-4`
    YYYYMM=`echo $CYCLE_3 | cut -c 1-6`
    YYYYMMDD=`echo $CYCLE_3 | cut -c 1-8`
    YYYYMMDDHH=`echo $CYCLE_3 | cut -c 1-10`
    MM=`echo $CYCLE_3 | cut -c 5-6`
    DD=`echo $CYCLE_3 | cut -c 7-8`
    HH=`echo $CYCLE_3 | cut -c 9-10`
    MN="00"
    MNp1="01"
    MNp2="02"

    YY2=`echo $CYCLE_3 | cut -c 3-4`
    # JJJ=`/bin/date --date="${MM}/${DD}/${YYYY}" +"%j" `
    JJJ=`/bin/date --date="${YYYYMMDD}" +"%j" `

    CYCLE_3_m1hr=`$NDATE -1 $CYCLE_3`
    YYYY_m1hr=`echo $CYCLE_3_m1hr | cut -c 1-4`
    YYYYMM_m1hr=`echo $CYCLE_3_m1hr | cut -c 1-6`
    YYYYMMDD_m1hr=`echo $CYCLE_3_m1hr | cut -c 1-8`
    YYYYMMDDHH_m1hr=`echo $CYCLE_3_m1hr | cut -c 1-10`
    MM_m1hr=`echo $CYCLE_3_m1hr | cut -c 5-6`
    DD_m1hr=`echo $CYCLE_3_m1hr | cut -c 7-8`
    HH_m1hr=`echo $CYCLE_3_m1hr | cut -c 9-10`
    YY2_m1hr=`echo $CYCLE_3_m1hr | cut -c 3-4`
    # JJJ_m1hr=`/bin/date --date="${MM_m1hr}/${DD_m1hr}/${YYYY_m1hr}" +"%j" `
    JJJ_m1hr=`/bin/date --date="${YYYYMMDD_m1hr}" +"%j" `

    hpsspath=$hpsspath1/rh${YYYY}/${YYYYMM}/${YYYYMMDD}
    hpsspath_1yr=$hpsspath1_1yr/rh${YYYY}/${YYYYMM}/${YYYYMMDD}

    hpsspath_gsd=$hpsspath1_gsd/${YYYY}/${MM}/${DD}
    radar_gsd_dir=$hpsspath_gsd/data/radar
    mrmsradar_dir=$radar_gsd_dir/mrms
    lghtn_gsd_dir=$hpsspath_gsd/data/lightning
    entln_dir=$lghtn_gsd_dir/entln/netcdf
    vaisala_dir=$lghtn_gsd_dir/vaisala/netcdf

    if [ $YYYYMMDD -le "20160105" ] ; then
       prefix="com"
       domain_hrrr=""
       hpsspath_hrrr=$hpsspath
     else
       if [ $YYYYMMDDHH -lt "2018071118" ] ; then
	 prefix="com2"
         domain_hrrr=""
	 hpsspath_hrrr=$hpsspath
       else
         prefix="gpfs_hps_nco_ops_com"
         domain_hrrr="_conus"
	 hpsspath_hrrr=$hpsspath_1yr
       fi
    fi


    if (((${HH} >= 00) && (${HH} <=  05))) ; then
#      tarfile_rap=${prefix}_${NET}_${envir}_${RUN}.${YYYYMMDD}00-05.tar
       tarfile_rap_bufr=${prefix}_rap_prod_rap.${YYYYMMDD}00-05.bufr.tar
       tarfile_hrrr_init=${prefix}_hrrr_prod_hrrr.${YYYYMMDD}${domain_hrrr}00-05.init.tar
    fi

    if (((${HH} >= 06) && (${HH} <=  11))) ; then
       tarfile_rap_bufr=${prefix}_rap_prod_rap.${YYYYMMDD}06-11.bufr.tar
       tarfile_hrrr_init=${prefix}_hrrr_prod_hrrr.${YYYYMMDD}${domain_hrrr}06-11.init.tar
    fi

    if (((${HH} >= 12) && (${HH} <=  17))) ; then
       tarfile_rap_bufr=${prefix}_rap_prod_rap.${YYYYMMDD}12-17.bufr.tar
       tarfile_hrrr_init=${prefix}_hrrr_prod_hrrr.${YYYYMMDD}${domain_hrrr}12-17.init.tar
    fi

    if (((${HH} >= 18) && (${HH} <=  23))) ; then
       tarfile_rap_bufr=${prefix}_rap_prod_rap.${YYYYMMDD}18-23.bufr.tar
       tarfile_hrrr_init=${prefix}_hrrr_prod_hrrr.${YYYYMMDD}${domain_hrrr}18-23.init.tar
    fi

#--- mrms radar data file naming rule 
    if (((${HH} >= 00) && (${HH} <=  02))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}0000.zip

    elif (((${HH} >= 03) && (${HH} <=  05))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}0300.zip

    elif (((${HH} >= 06) && (${HH} <=  08))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}0600.zip

    elif (((${HH} >= 09) && (${HH} <=  11))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}0900.zip

    elif (((${HH} >= 12) && (${HH} <=  14))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}1200.zip

    elif (((${HH} >= 15) && (${HH} <=  17))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}1500.zip

    elif (((${HH} >= 18) && (${HH} <=  20))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}1800.zip

    elif (((${HH} >= 21) && (${HH} <=  23))) ; then
       zipfile_mrms_3hr=${YYYYMMDD}2100.zip
    fi
    zipfile_mrms_1mi=${YYYYMMDD}${HH}${MN}.zip
    zipfile_mrms_1mip1=${YYYYMMDD}${HH}${MNp1}.zip
    zipfile_mrms_1mip2=${YYYYMMDD}${HH}${MNp2}.zip
    mrmsradar_grib2_dir=mrms.t${HH}z/conus

#--- lightning data naming rule
    zipfile_lghtn_1dd=${YYYYMMDD}0000.zip
    fnmhead_lghtn=${YY2}${JJJ}
    fnmhead_lghtn_m1hr=${YY2_m1hr}${JJJ_m1hr}
    lghtn_entln_dir=entln.t${HH}z
    lghtn_vaisala_dir=vaisala.t${HH}z

    wrkdir_hrrr=${mynoscrub}/hrrr.${YYYYMMDD}/conus     
    if [ ! -d $wrkdir_hrrr ]; then
     mkdir -p  $wrkdir_hrrr
    fi
#   wrkdir_nam=${mynoscrub}/nam.${YYYYMMDD}
#   if [ ! -d $wrkdir_nam ]; then
#    mkdir -p  $wrkdir_nam
#   fi
    if [ "$i" -eq 1 ] ; then 
      wrkdir_rap=${mynoscrub}/rap.${YYYYMMDD}     
      if [ ! -d  $wrkdir_rap ]; then
       mkdir -p  $wrkdir_rap
      fi
    fi
    if [ "$i" -eq 1 ] ; then 
      wrkdir_radar=${mynoscrub}/radar.${YYYYMMDD}     
      if [ ! -d  $wrkdir_radar ]; then
       mkdir -p  $wrkdir_radar
      fi
    fi
    if [ "$i" -eq 1 ] ; then 
      wrkdir_lightning=${mynoscrub}/lightning.${YYYYMMDD}     
      if [ ! -d  $wrkdir_lightning ]; then
       mkdir -p  $wrkdir_lightning
      fi
    fi
#   wrkdir_rtma=${mynoscrub}/${NET}.${YYYYMMDD}   
#   if [ ! -d  $wrkdir_rtma ]; then
#    mkdir -p  $wrkdir_rtma
#   fi
#   wrkdir_hrrr_m1=${mynoscrub}/hrrr.${YYYYMMDDm1}/conus 
#   if [ ! -d  $wrkdir_hrrr_m1 ]; then
#    mkdir -p  $wrkdir_hrrr_m1
#   fi
#   wrkdir_nam_m1=${mynoscrub}/nam.${YYYYMMDDm1}                     
#   if [ ! -d  $wrkdir_nam_m1 ]; then
#    mkdir -p  $wrkdir_nam_m1
#   fi
#   wrkdir_rap_m1=${mynoscrub}/rap.${YYYYMMDDm1}    
#   if [ ! -d  $wrkdir_rap_m1 ]; then
#    mkdir -p  $wrkdir_rap_m1
#   fi
#   tmp_1=${mynoscrub}/tmp_1
#   cd ${mynoscrub}
#   if [[ -d tmp_1 ]] ; then 
#       /bin/rm -rf tmp_1 
#   fi
#   if [[ -e tmp_1 ]] ; then 
#      /bin/rm -rf tmp_1 
#   fi
#   mkdir -p $tmp_1
#==================================================================================================

#==================================================================================================
    if [ "$i" -eq 1 ] ; then
      cd $wrkdir_rap

      /bin/rm -rf select_list_1.txt_tmp
      /bin/rm -rf select_list_1.txt

      htar -tvf ${hpsspath}/${tarfile_rap_bufr}  > list_all_1.txt

      cat list_all_1.txt | grep rap.t${HH}z.prepbufr.tm00 >> select_list_1.txt_tmp
      cat list_all_1.txt | grep rap.t${HH}z.lgycld.tm00.bufr_d >> select_list_1.txt_tmp
      cat list_all_1.txt | grep rap.t${HH}z.lghtng.tm00.bufr_d >> select_list_1.txt_tmp

      nlines=`wc -l select_list_1.txt_tmp`
      nlines=${nlines% select*}
      echo "nlines ="$nlines

      it=1
      while [ $it -le $nlines ] ; do
        var="`cat select_list_1.txt_tmp | head -n $it | tail  -1`" 
        echo "./"${var#* ./} >> select_list_1.txt
        let "it=it+1"
      done
      htar -xvf ${hpsspath}/${tarfile_rap_bufr} -L select_list_1.txt
    fi
        
#==================================================================================================
    cd $wrkdir_hrrr

    /bin/rm -rf select_list_1.txt_tmp
    /bin/rm -rf select_list_1.txt

    htar -tvf ${hpsspath_hrrr}/${tarfile_hrrr_init}  > list_all_1.txt

    if [ "$i" -eq 1 ] ; then
      cat list_all_1.txt | grep hrrr.t${HH}z.NSSLRefInGSI.bufr >> select_list_1.txt_tmp
      cat list_all_1.txt | grep hrrr.t${HH}z.NASALaRCCloudInGSI.bufr >> select_list_1.txt_tmp
#     cat list_all_1.txt | grep "\bhrrr.t${HH}z.wrfguess\b"  >> select_list_1.txt_tmp
#     cat list_all_1.txt | grep "\<hrrr.t${HH}z.wrfguess\>"  >> select_list_1.txt_tmp
      cat list_all_1.txt | grep -w hrrr.t${HH}z.wrfguess  >> select_list_1.txt_tmp
    else
      cat list_all_1.txt | grep -w hrrr.t${HH}z.wrfguess_rap >> select_list_1.txt_tmp
    fi

    nlines=`wc -l select_list_1.txt_tmp`
    nlines=${nlines% select*}
    echo "nlines ="$nlines

    it=1
    while [ $it -le $nlines ] ; do
      var="`cat select_list_1.txt_tmp | head -n $it | tail  -1`" 
      echo "./"${var#* ./} >> select_list_1.txt
      let "it=it+1"
    done
    htar -xvf ${hpsspath_hrrr}/${tarfile_hrrr_init} -L select_list_1.txt
        
#==================================================================================================
    if [ "$i" -eq 1 ] ; then
      cd $wrkdir_radar
      rm -rf $mrmsradar_grib2_dir
      if [ ! -d $mrmsradar_grib2_dir ] ; then
        mkdir -p $mrmsradar_grib2_dir
      fi

      hsi get "${mrmsradar_dir}/${zipfile_mrms_3hr}"

      /bin/rm -rf select_list_1.txt_tmp
      /bin/rm -rf select_list_1.txt
      /bin/rm -rf list_all_1.txt
      /bin/rm -rf select_list_1.txt_total

      /bin/unzip -v ${zipfile_mrms_3hr} > list_all_1.txt

      zfiles="${zipfile_mrms_1mi} ${zipfile_mrms_1mip1} ${zipfile_mrms_1mip2}"
      for zf in $zfiles
      do
        nlines=`cat list_all_1.txt | awk {'print $8'} | grep "${zf}" | wc -l`
        if [ $nlines -eq 1 ]
        then
          /bin/unzip ${zipfile_mrms_3hr} "${zf}" -d ${mrmsradar_grib2_dir}

          /bin/rm -rf select_list_1.txt_tmp
          /bin/rm -rf select_list_1.txt
          /bin/unzip -v ${mrmsradar_grib2_dir}/${zf} > select_list_1.txt_tmp
          cat select_list_1.txt_tmp | grep "MRMS_MergedReflectivityQC_" | awk {'if($8 !~ /conusPlus/) print $8'} > select_list_1.txt
          nl=`cat select_list_1.txt | wc -l `
          if [ $nl -ge 1 ]
          then
            cat select_list_1.txt | xargs -d '\n' unzip ${mrmsradar_grib2_dir}/${zf} -d ${mrmsradar_grib2_dir}
          fi
          /bin/rm -f ${mrmsradar_grib2_dir}/${zf}
          cat select_list_1.txt >> select_list_1.txt_total
          echo "=================================="  >> select_list_1.txt_total
        fi
      done

      /bin/rm -f ${zipfile_mrms_3hr}
      
    fi
        
#==================================================================================================

    if [ "$i" -eq 1 ] && [ 1 -eq 2 ] ; then
      echo "======================================================================================="
      echo "======== retrieving raw lightning data from GSD retrieval dataset on HPSS ============="
      echo "======================================================================================="
      cd $wrkdir_lightning

      for lghtn_dir in $lghtn_entln_dir $lghtn_vaisala_dir
      do
        rm -rf $lghtn_dir
        if [ ! -d $lghtn_dir ] ; then
          mkdir -p $lghtn_dir
        fi

        if [ $lghtn_dir = $lghtn_entln_dir ] ; then
          hsi get "${entln_dir}/${zipfile_lghtn_1dd}"
        elif [ $lghtn_dir = $lghtn_vaisala_dir ] ; then
          hsi get "${vaisala_dir}/${zipfile_lghtn_1dd}"
        fi

        /bin/rm -rf select_list_1.txt_tmp
        /bin/rm -rf select_list_1.txt
        /bin/rm -rf list_all_1.txt
        /bin/rm -rf select_list_1.txt_total

        /bin/unzip -v ${zipfile_lghtn_1dd} > list_all_1.txt

        if [ "${fnmhead_lghtn}" = "${fnmhead_lghtn_m1hr}" ] ; then
          fheads="${fnmhead_lghtn}"
        else
          fheads="${fnmhead_lghtn} ${fnmhead_lghtn_m1hr} "
        fi
        for fh in $fheads
        do
          cat list_all_1.txt | grep "${fh}"  > select_list_1.txt_tmp
          cat select_list_1.txt_tmp | awk '{print $8}' > select_list_1.txt
          nlines=`cat select_list_1.txt | wc -l `
          if [ $nlines -ge 1 ]
          then
            cat select_list_1.txt | xargs -d '\n' unzip ${zipfile_lghtn_1dd} -d ${lghtn_dir}
          fi
          cat select_list_1.txt >> select_list_1.txt_total
          echo "=================================="  >> select_list_1.txt_total
        done

        /bin/rm -f ${zipfile_lghtn_1dd}

      done
      
    fi
        
#==================================================================================================

#   cd $tmp_1
#==================================================================================================


#==================================================================================================
#==> rm tmp_1 directory + advance CYCLE
#==================================================================================================
    cd ${mynoscrub}

    CYCLE_3=`$NDATE +1 $CYCLE_3`
done


################################################################################################
postmsg $jlogfile "$0 of $job completed normally"
################################### END OF SCRIPT ###############################################


exit
