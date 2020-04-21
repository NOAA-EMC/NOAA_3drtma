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

# set -x 

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"


#========================= begin changes =======================================================
envir=${envir}
CYCLE=${PDY}${cyc}
CYCLE_p1=${CYCLE}
mynoscrub=${COMIN}
mynoscrubm1=${COMINm1}
CYCLE_3=`$NDATE -0 $CYCLE`
YYYYMMDDm1=`$NDATE -24 ${CYCLE} | cut -c 1-8`
#=========================  end changes  =======================================================

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
    JJJ_m1hr=`/bin/date --date="${YYYYMMDD_m1hr}" +"%j" `

    CYCLE_3_p1hr=`$NDATE +1 $CYCLE_3`
    YYYY_p1hr=`echo $CYCLE_3_p1hr | cut -c 1-4`
    YYYYMM_p1hr=`echo $CYCLE_3_p1hr | cut -c 1-6`
    YYYYMMDD_p1hr=`echo $CYCLE_3_p1hr | cut -c 1-8`
    YYYYMMDDHH_p1hr=`echo $CYCLE_3_p1hr | cut -c 1-10`
    MM_p1hr=`echo $CYCLE_3_p1hr | cut -c 5-6`
    DD_p1hr=`echo $CYCLE_3_p1hr | cut -c 7-8`
    HH_p1hr=`echo $CYCLE_3_p1hr | cut -c 9-10`
    YY2_p1hr=`echo $CYCLE_3_p1hr | cut -c 3-4`
    JJJ_p1hr=`/bin/date --date="${YYYYMMDD_p1hr}" +"%j" `

    hpsspath=$hpsspath1/rh${YYYY}/${YYYYMM}/${YYYYMMDD}
    hpsspath_1yr=$hpsspath1_1yr/rh${YYYY}/${YYYYMM}/${YYYYMMDD}

    hpsspath_1yr_p1hr=$hpsspath1_1yr/rh${YYYY_p1hr}/${YYYYMM_p1hr}/${YYYYMMDD_p1hr}
    hpsspath_AGibbs=$hpsspath1_AGibbs/rh${YYYY_m1hr}/${YYYYMM_m1hr}/${YYYYMMDD_m1hr}

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

#   hpsspath_hrrr_p1hr=$hpsspath_1yr_p1hr

    prefix_hrrr_AGibbs="gpfs_hps_nco_ops_nwges_prod_hrrr_hrrrges_sfc_conus"
    hpsspath_hrrr_AGibbs=$hpsspath_AGibbs
    tarfile_hrrr_AGibbs=${prefix_hrrr_AGibbs}.netcdf.${YYYYMMDD_m1hr}.tar

#   if [ ${HH} -ge 00 ] && [ ${HH} -le 05 ] ; then   # Good with Bash and Ksh
#   if (((10#${HH} >= 00) && (10#${HH} <=  05))) ; then # Good with Bash and Ksh
    if (((${HH} >= 00) && (${HH} <=  05))) ; then    # NOT recommended for BASH, but work well with ksh.
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
    wrkdir_rap=${mynoscrub}/rap.${YYYYMMDD}     
    if [ ! -d  $wrkdir_rap ]; then
     mkdir -p  $wrkdir_rap
    fi
    wrkdir_radar=${mynoscrub}/radar.${YYYYMMDD}     
    if [ ! -d  $wrkdir_radar ]; then
     mkdir -p  $wrkdir_radar
    fi
    wrkdir_lightning=${mynoscrub}/lightning.${YYYYMMDD}     
    if [ ! -d  $wrkdir_lightning ]; then
     mkdir -p  $wrkdir_lightning
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
      echo " ---- >   Retrieving Obs (prepbufr/nexrad/lgycld/lghtng) from archive of operational RAP"
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
        
#==================================================================================================
    cd $wrkdir_hrrr
    echo " ---- >   Retrieving Obs (mosaic_radar/nasa_satcloud) from archived dataset of operational HRRR"
    echo " ---- >   Retrieving fgs (wrfguess if exists) from archived dataset of operational HRRR"

    /bin/rm -rf select_list_1.txt_tmp
    /bin/rm -rf select_list_1.txt
    /bin/rm -rf list_all_1.txt

    htar -tvf ${hpsspath_hrrr}/${tarfile_hrrr_init}  > list_all_1.txt
    cat list_all_1.txt | grep hrrr.t${HH}z.NSSLRefInGSI.bufr >> select_list_1.txt_tmp
    cat list_all_1.txt | grep hrrr.t${HH}z.NASALaRCCloudInGSI.bufr >> select_list_1.txt_tmp
    cat list_all_1.txt | grep -w hrrr.t${HH}z.wrfguess  >> select_list_1.txt_tmp

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


    if [ $YYYYMMDDHH -ge "2018071118"  ] && [ $YYYYMMDDHH -le "2018102000" ] ; then
      echo " ---- >   Retrieving fgs (wrfguess_rap 20180711_18Z ~ 20181020_00z) from archive of operational HRRR"
      /bin/rm -rf select_list_1.txt_tmp
      /bin/rm -rf select_list_1.txt
      /bin/rm -rf list_all_1.txt

      hpsspath_hrrr_p1hr=$hpsspath_1yr_p1hr
      prefix1="gpfs_hps_nco_ops_com"
      domain_hrrr1="_conus"
      if (((${HH_p1hr} >= 00) && (${HH_p1hr} <=  05))) ; then    # NOT recommended for BASH, but work well with ksh.
        tarfile_hrrr_init_p1hr=${prefix1}_hrrr_prod_hrrr.${YYYYMMDD_p1hr}${domain_hrrr1}00-05.init.tar
      fi

      if (((${HH_p1hr} >= 06) && (${HH_p1hr} <=  11))) ; then
        tarfile_hrrr_init_p1hr=${prefix1}_hrrr_prod_hrrr.${YYYYMMDD_p1hr}${domain_hrrr1}06-11.init.tar
      fi

      if (((${HH_p1hr} >= 12) && (${HH_p1hr} <=  17))) ; then
        tarfile_hrrr_init_p1hr=${prefix1}_hrrr_prod_hrrr.${YYYYMMDD_p1hr}${domain_hrrr1}12-17.init.tar
      fi

      if (((${HH_p1hr} >= 18) && (${HH_p1hr} <=  23))) ; then
        tarfile_hrrr_init_p1hr=${prefix1}_hrrr_prod_hrrr.${YYYYMMDD_p1hr}${domain_hrrr1}18-23.init.tar
      fi

      htar -tvf ${hpsspath_hrrr_p1hr}/${tarfile_hrrr_init_p1hr}  > list_all_1.txt

      cat list_all_1.txt | grep -w hrrr.t${HH_p1hr}z.wrfguess_rap > select_list_1.txt_tmp
      nlines=`wc -l select_list_1.txt_tmp`
      nlines=${nlines% select*}
      echo "nlines ="$nlines

      it=1
      while [ $it -le $nlines ] ; do
        var="`cat select_list_1.txt_tmp | head -n $it | tail  -1`" 
        echo "./"${var#* ./} >> select_list_1.txt
        let "it=it+1"
      done
      htar -xvf ${hpsspath_hrrr_p1hr}/${tarfile_hrrr_init_p1hr} -L select_list_1.txt

    elif [ $YYYYMMDDHH -ge "2018102001" ] ; then

      echo " ---- >   Retrieving fgs (hrrr_f001 1-hr forecast) from operational HRRR (archived by Annett Gibbs after 20181020_01Z)"
      htar -xvf ${hpsspath_hrrr_AGibbs}/${tarfile_hrrr_AGibbs} hrrr_${YYYYMMDDHH_m1hr}f001  ./

    fi
        
#==================================================================================================
    if [ ${obsprep_radar} -eq 1 ] ; then
      set -x
      cd ${wrkdir_radar}
      echo "=== Retrieving MRMS Radar mosaic Reflectivity from GSD Archived Dataset on HPSS"
      rm -rf $mrmsradar_grib2_dir
      mkdir -p $mrmsradar_grib2_dir
      rm -f ${wrkdir_radar}/${zipfile_mrms_3hr}

#     hsi get "${mrmsradar_dir}/${zipfile_mrms_3hr}"
      hsi get ${wrkdir_radar}/${zipfile_mrms_3hr} : ${mrmsradar_dir}/${zipfile_mrms_3hr}

      /bin/rm -f select_list_1.txt_tmp
      /bin/rm -f select_list_1.txt
      /bin/rm -f list_all_1.txt
      /bin/rm -f select_list_1.txt_total

#     ${UNZIP} -v ${zipfile_mrms_3hr} >list_all_1.txt
      ${UNZIP} -Z1 ${zipfile_mrms_3hr} >list_all_1.txt

      zfiles="${zipfile_mrms_1mi} ${zipfile_mrms_1mip1} ${zipfile_mrms_1mip2}"
      for zf in $zfiles
      do
#       nlines=`cat list_all_1.txt | awk {'print $8'} | grep "${zf}" | wc -l`
        nlines=`cat list_all_1.txt | awk {'print $1'} | grep "${zf}" | wc -l`
        if [ $nlines -eq 1 ]
        then
          ${UNZIP} ${zipfile_mrms_3hr} "${zf}" -d ${mrmsradar_grib2_dir}

          /bin/rm -rf select_list_1.txt_tmp
          /bin/rm -rf select_list_1.txt
          ${UNZIP} -v ${mrmsradar_grib2_dir}/${zf} > select_list_1.txt_tmp
          cat select_list_1.txt_tmp | grep "MRMS_MergedReflectivityQC_" | awk {'if($8 !~ /conusPlus/) print $8'} > select_list_1.txt
          nl=`cat select_list_1.txt | wc -l `
          if [ $nl -ge 1 ]
          then
            cat select_list_1.txt | xargs -d '\n' ${UNZIP} ${mrmsradar_grib2_dir}/${zf} -d ${mrmsradar_grib2_dir}
          fi
          /bin/rm -f ${mrmsradar_grib2_dir}/${zf}
          cat select_list_1.txt >> select_list_1.txt_total
          echo "=================================="  >> select_list_1.txt_total
        fi
      done

#     /bin/rm -f ${zipfile_mrms_3hr}
      set +x
      
    fi
        
#==================================================================================================

    if [ ${obsprep_lghtn} -gt 1 ] ; then

      cd $wrkdir_lightning

      case ${obsprep_lghtn} in
        2)
          lghtn_dir=$lghtn_vaisala_dir
          lghtn_resource="NDLN_vaisala"
          lghtn_sourcedir=${vaisala_dir}
          ;;
        3)
          lghtn_dir=$lghtn_entln_dir
          lghtn_resource="entln"
          lghtn_sourcedir=${entln_dir}
          ;;
        *)
          echo "NOt defined value for obsprep_lghtn -- > ${obsprep_lghtn}"
          echo "Not retrieving lighting data for thsi cycle."
          ;;
      esac

      echo
      echo "=== retrieving ${lghtn_resource} lightning data from GSD Archived dataset on HPSS ==="
      echo

        rm -rf $lghtn_dir
        mkdir -p $lghtn_dir

        hsi get "${lghtn_sourcedir}/${zipfile_lghtn_1dd}"

        /bin/rm -rf select_list_1.txt_tmp
        /bin/rm -rf select_list_1.txt
        /bin/rm -rf list_all_1.txt
        /bin/rm -rf select_list_1.txt_total

        ${UNZIP} -v ${zipfile_lghtn_1dd} > list_all_1.txt

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
            cat select_list_1.txt | xargs -d '\n' ${UNZIP} ${zipfile_lghtn_1dd} -d ${lghtn_dir}
          fi
          cat select_list_1.txt >> select_list_1.txt_total
          echo "=================================="  >> select_list_1.txt_total
        done

        /bin/rm -f ${zipfile_lghtn_1dd}

      
    fi
        
#==================================================================================================

#   cd $tmp_1
#==================================================================================================


#==================================================================================================
#==> rm tmp_1 directory + advance CYCLE
#==================================================================================================
    cd ${mynoscrub}


################################################################################################
postmsg $jlogfile "$0 of $job completed normally"
################################### END OF SCRIPT ###############################################


exit
