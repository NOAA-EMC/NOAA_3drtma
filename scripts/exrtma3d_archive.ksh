#!/bin/ksh

set -x


. prep_step
startmsg


#############################################################################
# Make sure START_TIME is defined and in the correct format
#START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
FCST_TIME="00"                 # <-- forecast time (hour) to provide fgs for rtma
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
mm=`${DATE} +"%M" -d "${START_TIME}"`

export PDY=${PDY:-$YYYYMMDD}
export cyc=${cyc:-$HH}
export cycle=${cycle:-t${cyc}z} 
export tarfile_1=${tarfile_1:-com2_$NET_$envir_$RUN.$PDY}
export tarfile_2=${tarfile_2:-com2_$NET_$envir_$NET.$PDY}
export PROD_HEAD=${PROD_HEAD:-"${RUN}.t${cyc}z"}
export DATA_RHIST=${DATA_RHIST:-$DATAROOT/wrkdir_rhist.$PDY$cyc}

##########################################
#Sanity checks
##########################################
CHECK_HPSS_IDX=NO
DRY_RUN_ONLY=NO

################################################################################################
#==> check if system ran to completion
################################################################################################
iflg=0

suffixlist="wrfprs_hrconus wrftwo_hrconus wrfnat_hrconus"
    
for suffix in $suffixlist ; do
      if [ -s ${DATAHOME}/${suffix}_${FCST_TIME}.grib2 ] ; then
          let "iflg=iflg+0"
      else
          let "iflg=iflg-1"
      fi
done

if (( $iflg < 0 )) ; then
   echo "missing output GRIB files. System did not run to completion. exit now!"
   exit -111
fi
################################################################################################
#==> write output files to ftp and www servers
################################################################################################

if [ ${write_to_rzdm} = yes ] ; then

set +x
. /u/${USER}/.Utils
   export w1=mpondeca
   export w2=$rzdm

#   ftppath0=/home/people/emc/ftp/mmb/rtma/v${vernum}/${NET}/para
   ftppath0=/home/people/emc/ftp/mmb/rtma/${NET}/para_TEST
   ftppath=${ftppath0}/${RUN}.${PDY}
   ssh ${w1}@emcrzdm "mkdir -p ${ftppath}"

#   wwwpath0=/home/people/emc/www/htdocs/mmb/aor/rtma/v${vernum}/${NET}/para
   wwwpath0=/home/people/emc/www/htdocs/mmb/aor/${NET}/para_TEST
   wwwpath=${wwwpath0}/${RUN}.${PDY}/${cyc}z
   ssh ${w1}@emcrzdm "mkdir -p ${wwwpath}"

   ftp -n -v -i << EOF > $LLOGS1/ftp_to_server_${RUN}_${CDATE}.log
   open emcrzdm.ncep.noaa.gov
   user $w1 $w2
   binary
   cd ${ftppath}
   lcd $COMOUT
   put ${RUN}.t${cyc}z.2dvaranl_ndfd.grb2
   put ${RUN}.t${cyc}z.2dvarges_ndfd.grb2
   put ${RUN}.t${cyc}z.2dvarerr_ndfd.grb2
   put ${RUN}.t${cyc}z.2dvaranl_ndfd.grb2_wexp
   put ${RUN}.t${cyc}z.2dvarges_ndfd.grb2_wexp
   put ${RUN}.t${cyc}z.2dvarerr_ndfd.grb2_wexp
   put ${RUN}.t${cyc}z.2dvaranl_nwrfc.grb2
   put ${RUN}.t${cyc}z.2dvarges_nwrfc.grb2
   put ${RUN}.t${cyc}z.2dvarerr_nwrfc.grb2
   put ${RUN}.t${cyc}z.2dvaranl_ndfd_3p0.grb2
   put ${RUN}.t${cyc}z.2dvarges_ndfd_3p0.grb2
   put ${RUN}.t${cyc}z.2dvarerr_ndfd_3p0.grb2
   cd ${wwwpath}
   mput ${RUN}.t${cyc}z.*_obs.listing_iter_*
   bye
EOF

   list="${PDYm3} ${PDYm4}"
   for item in $list ; do
      ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${RUN}.${item}"
      err1=$?
      if (( $err1 == 0 )) ; then
         ssh ${w1}@emcrzdm "rm -rf ${ftppath0}/${RUN}.${item}"
      fi

      ssh ${w1}@emcrzdm "ls -d ${wwwpath0}/${RUN}.${item}"
      err2=$?
      if (( $err2 == 0 )) ; then
         ssh ${w1}@emcrzdm "rm -rf ${wwwpath0}/${RUN}.${item}"
      fi
   done
fi

set -x
################################################################################################
#==> write output files to hpss
################################################################################################

if [ ${hpss_save} = yes ] ; then

   #Because in development runs the various observations files can come from different places,
   #create an equivalent to NCO's $COMINobsproc_rtma first, and dump the observations files there
   #before evoking the rhist script

   mkdir -p $DATA_RHIST
   cd $DATA_RHIST

cp $DATAOBS/60/LightningInGSI.bufr LightningInGSI.t${HH}z.bufr
cp $DATAOBS/NASALaRCCloudInGSI.bufr NASALaRCCloudInGSI.t${HH}z.bufr
cp $DATAOBS/60/NSSLRefInGSI.bufr NSSLRefInGSI.t${HH}z.bufr
cp $DATAOBS/60/NLDN_lightning_1 NLDN_lightning_1.t${HH}z
cp $DATAOBS/60/NLDN_lightning_2 NLDN_lightning_2.t${HH}z
cp $DATAOBS/60/NLDN_lightning_3 NLDN_lightning_3.t${HH}z
cp $DATAOBS/*.rap.t${HH}z.nexrad.tm00.bufr_d .
cp $DATAOBS/*.rap.t${HH}z.satwnd.tm00.bufr_d .
cp $DATAOBS/NASA_LaRC_cloud.bufr NASA_LaRC_cloud.t${HH}z.bufr
cp $DATAOBS/stdout_append_clamps stdout_append_clamps.t${HH}z
cp $DATAOBS/stdout_append_sticknet stdout_append_sticknet.t${HH}z
cp $DATAOBS/stdout_append_tamdar stdout_append_tamdar.t${HH}z
cp $DATAOBS/stdout_satellite_bufr stdout_satellite_bufr.t${HH}z
cp $DATAOBS/stdout_sodar_nr stdout_sodar_nr.t${HH}z
cp $DATAOBS/*.grib2 .
cp $DATAOBS/*prepbufr* .
cp $DATAWRFHOME/stdout stdout.t${HH}z
cp $DATAWRFHOME/stdout_count_obs stdout_count_obs.t${HH}z
cp $DATAWRFHOME/stdout_read_diag_conv stdout_read_diag_conv.t${HH}z
cp $DATAWRFHOME/stdout_read_diag_rad stdout_read_diag_rad.t${HH}z
cp $DATAWRFHOME/diag_conv* .
cp $DATAWRFHOME/wrf_inout wrf_inout.t${HH}z
cp $DATAWRFHOME/siganl siganl.t${HH}z
cp $DATAWRFHOME/sigf03 sigf03.t${HH}z
cp $DATAWRFHOME/fits_* .
cp $DATAHOME/wrfnat_hrconus_00.grib2 wrfnat_hrconus_00.t${HH}z.grib2
cp $DATAHOME/wrfprs_hrconus_00.grib2 wrfprs_hrconus_00.t${HH}z.grib2
cp $DATAHOME/wrftwo_hrconus_00.grib2 wrftwo_hrconus_00.t${HH}z.grib2




   export HPSSOUT=$hpsspath0
   hpssdir2=${HPSSOUT}/2year/rh${YYYY}/${YYYY}${MM}/${YYYYMMDD}
   hpssdir0=${HPSSOUT}/rh${YYYY}/${YYYY}${MM}/${YYYYMMDD}
   

cyclist=t25z
if [ $cyc = "00" ]
then
   cyclist="t00z|t01z|t02z|t03z|t04z|t05z"
   endcyc=05
elif [ $cyc = "06" ]
then
   cyclist="t06z|t07z|t08z|t09z|t10z|t11z"
   endcyc=11
elif [ $cyc = "12" ]
then
   cyclist="t12z|t13z|t14z|t15z|t16z|t17z"
   endcyc=17
elif [ $cyc = "18" ]
then
   cyclist="t18z|t19z|t20z|t21|t22z|t23z"
   endcyc=23:1
fi

   #Redefine cyclist to save single analysis files MPondeca/30Jul2017
   cyclist="${cyc}"                              #MPondeca /30Jul2017

ls -1 $DATA_RHIST | grep -E "${cyclist}" | grep -v bufr_d.listing | awk '
            /prepbufr/ { print "./"$0 > "perm" ; next }
            /NLDN/ { print "./"$0 > "perm" ; next }
            /NASALaRCCloudInGSI/ { print "./"$0 > "perm" ; next }
            /NSSLRefInGSI/ { print "./"$0 > "perm" ; next }
            /LightningInGSI.bufr/ { print "./"$0 > "perm" ; next }
            /wrf_inout/ { print "./"$0 > "perm" ; next }
            /siganl/ { print "./"$0 > "perm" ; next }
            /sigf03/ { print "./"$0 > "perm" ; next }
            /wrfnat_hrconus_${cyc}/ { print "./"$0 > "perm" ; next }
            /wrfprs_hrconus_${cyc}/ { print "./"$0 > "perm" ; next }
            /wrftwo_hrconus_${cyc}/ { print "./"$0 > "perm" ; next } '

ls -1 $DATA_RHIST | grep -E "${cyclist}" | awk ' 
        /satbias_out/ { print "./"$0 > "2yr" ; next }
        /diag_conv/ { print "./"$0 > "2yr" ; next }
        /diag_results/  { print "./"$0 > "2yr" ; next }
        /namelist/ { print "./"$0 > "2yr" ; next }
        /fits/ { print "./"$0 > "2yr" ; next }
        /stdout/ { print "./"$0 > "2yr" ; next }
        /filelist/ { print "./"$0 > "2yr" ; next } '
export sync_list="perm 2yr"

for file in ${sync_list}
do

   #
   #   Pick 2year or permanent archive.
   #
   case $file in
      perm)   hpssdir=$hpssdir0
              rhistdir=$rhistdir0;;
      2yr)    hpssdir=$hpssdir2
              rhistdir=$rhistdir2;;
      *) hpssdir=$hpssdir2
         rhistdir=$rhistdir2;;
   esac
   tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
   tarfile=${tarfile}${cyc}.tar       

   if [[ $CHECK_HPSS_IDX == "YES" ]] ; then
           hsi "ls -l ${hpssdir}/${tarfile}.idx"
           tar_file_exists=$?
           if [ $tar_file_exists -eq 0 ]
           then
               echo "File $tarfile already saved."
               continue
           fi
    fi 

     if [[ $DRY_RUN_ONLY == "YES" ]] ; then
         echo "DRY RUN, list of files that would be archived:"
         cat ${DATA_RHIST}/$file | sort
         continue
     else
         htar -P -cvf ${hpssdir}/$tarfile -L ${DATA_RHIST}/$file
         err=$?
     fi
     if [ $err -ne 0 ]
     then
       echo "exrtma_archive.sh:  File $tarfile was not successfully created."
       exit 3
     fi
     date

   #
   #   Read the tarfile and save a list of files that are in the tar file.
   #

     htar -tvf $hpssdir/$tarfile
     err=$?
     if [ $err -ne 0 ]
     then
       echo "exrtma_archive.sh:  Tar file $tarfile was not successfully read to"
       echo "             generate a list of the files."
       exit 4
     fi

   #
   #


done


if [ ${remove_archdir} = yes ] ; then
     cd $DATA_RHIST/..
     /bin/rm -rf $DATA_RHIST
fi

fi

exit

