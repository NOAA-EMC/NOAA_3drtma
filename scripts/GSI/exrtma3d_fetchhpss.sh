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
    DD=`echo $CYCLE_3 | cut -c 7-8`
    HH=`echo $CYCLE_3 | cut -c 9-10`

    hpsspath=$hpsspath1/rh${YYYY}/${YYYYMM}/${YYYYMMDD}
    hpsspath_1yr=$hpsspath1_1yr/rh${YYYY}/${YYYYMM}/${YYYYMMDD}

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
