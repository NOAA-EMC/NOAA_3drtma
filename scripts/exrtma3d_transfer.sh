#!/bin/ksh

set -x


. prep_step
startmsg


#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
echo $cyc
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

################################################################################################
#==> check if system ran to completion
################################################################################################
iflg=0

suffixlist="wrfprs_hrconus wrftwo_hrconus wrfnat_hrconus"
    
for suffix in $suffixlist ; do
      if [ -s ${COMOUTpost_rtma3d}/${PROD_HEAD}.${suffix}_${FCST_TIME}.grib2 ] ; then
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

   MY_COMINobsproc_rtma=$COMROOT/${NET}/${envir}/${NET}.${PDY}
   mkdir -p $MY_COMINobsproc_rtma
   cd $MY_COMINobsproc_rtma

   cp -p ${COMINobsproc_rtma3d}/rap.t${HH}z.prepbufr.tm00 .
   cp -p ${COMINobsproc_rtma3d}/${RUN}.t${HH}z.LightningInGSI_bufr.bufr .
   cp -p ${COMINobsproc_rtma3d}/hrrr.t${HH}z.NASALaRCCloudInGSI.bufr .
   cp -p ${COMINobsproc_rtma3d}/hrrr.t00z.NSSLRefInGSI.bufr . 

   mkdir -p $DATA_RHIST
   cd $DATA_RHIST

   export HPSSOUT=$hpsspath0
   export TSM_FLAG=NO

   RUN[1]=$RUN
   RUN[2]=$NET
   dir[1]=$COMROOT/${NET}/${envir}/${RUN}.${PDY}
   dir[2]=$MY_COMINobsproc_rtma    
   tarfile[1]=$tarfile_1
   tarfile[2]=$tarfile_2

   ntotal=2

   it=1
   while [ $it -le $ntotal ] ; do
      export DATA=$DATA_RHIST/${RUN}
      mkdir -p $DATA
      $HOMErtma3d/scripts/rhist_savertma3d_emc.sh ${dir[$it]} ${PDY}${cyc} ${tarfile[$it]}
      let "it=it+1"
   done

   if [ ${remove_wrkdirs} = yes ] ; then
      cd $DATA_RHIST/..
      /bin/rm -rf $DATA_RHIST
   fi

fi


exit
------------------------------------------------------------------------
