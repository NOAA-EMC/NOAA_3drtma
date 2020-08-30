#!/bin/ksh

set -x

pgm=${RUN}_arch
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

export cycle=${cycle:-t${cyc}${subcyc}z} 

export tarfile_1=${tarfile_1:-com2_$NET_$envir_$RUN.$PDY}

export tarfile_2=${tarfile_2:-com2_$NET_$envir_$NET.$PDY}

export PROD_HEAD=${PROD_HEAD:-"${RUN}.${cycle}"}

basetime=`${DATE} +%y%j%H%M -d "${START_TIME}"`
##########################################

#Sanity checks

##########################################

CHECK_HPSS_IDX=NO

DRY_RUN_ONLY=NO

################################################################################################

#==> check if system ran to completion

################################################################################################

iflg=0

suffixlist="wrfsubhprs wrfsubhspl wrfsubhnat"

    

for suffix in $suffixlist ; do

      if [ -s ${COMOUTpost_rtma3d}/${PROD_HEAD}.${suffix}.grib2 ] ; then

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

#. /u/${USER}/.Utils

   export w1=ecolon

#   export w2=$rzdm

#   ftppath0=/home/people/emc/ftp/mmb/rtma/v${vernum}/${NET}/para

   ftppath0=/home/people/emc/www/htdocs/mmb/rtma/3drtma

   ftppath=${ftppath0}/3drtma.${PDY}/t${cyc}${subcyc}z

   ssh ${w1}@emcrzdm "mkdir -p ${ftppath}"

#   wwwpath0=/home/people/emc/www/htdocs/mmb/aor/rtma/v${vernum}/${NET}/para

   wwwpath0=//www.emc.ncep.noaa.gov/mmb/rtma/3drtma

   wwwpath=${wwwpath0}/3drtma.${PDY}/${cyc}${subcyc}z

   ssh ${w1}@emcrzdm "mkdir -p ${wwwpath}"

   ftp -n -v -i << EOF > ${LOG_DIR}/ftp_to_server_${RUN}_${PDY}${cyc}${subcyc}.log

   open emcrzdm.ncep.noaa.gov

#   user $w1 $w2
   user $w1

   binary

   cd ${ftppath}

   lcd $COMOUTpost_rtma3d

   put ${PROD_HEAD}.wrfsubhprs_fgs.grib2
   
   put ${PROD_HEAD}.wrfsubhspl_fgs.grib2

   put ${PROD_HEAD}.wrfsubhnat_fgs.grib2

   put ${PROD_HEAD}.wrfsubhprs.grib2
   
   put ${PROD_HEAD}.wrfsubhspl.grib2

   put ${PROD_HEAD}.wrfsubhnat.grib2

   cd ${wwwpath}

#   mput ${RUN}.${cycle}.*_obs.listing_iter_*

   bye

EOF

#    ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.fgs.wrfprs_subhrconus_${FCST_TIME}.grib2
#    err1=$?
#    if (( $err1 == 0 )) ; then
#           ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.fgs.wrftwo_subhrconus_${FCST_TIME}.grib2
#           err2=$?
#    if (( $err2 == 0 )) ; then
#           ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.fgs.wrfnat_subhrconus_${FCST_TIME}.grib2
#           err3=$?
#    if (( $err3 == 0 )) ; then 
#           ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.wrfprs_subhrconus_${FCST_TIME}.grib2
#           err4=$?
#    if (( $err4 == 0 )) ; then 
#           ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.wrftwo_subhrconus_${FCST_TIME}.grib2
#           err5=$?
#    if (( $err5 == 0 )) ; then
#           ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${PROD_HEAD}.wrftwo_subhrconus_${FCST_TIME}.grib2
#           err6=$?
#    if (( $err6 == 0 )) ; then 
#           echo "All of the UPP output files have not been tranferred to RZDM."
#    fi
          
        
#   list="${PDYm3} ${PDYm4}"

#   for item in $list ; do

#      ssh ${w1}@emcrzdm "ls -d ${ftppath0}/${RUN}.${item}"

#      err1=$?

#      if (( $err1 == 0 )) ; then

#         ssh ${w1}@emcrzdm "rm -rf ${ftppath0}/${RUN}.${item}"

#      fi

#      ssh ${w1}@emcrzdm "ls -d ${wwwpath0}/${RUN}.${item}"

#      err2=$?

#      if (( $err2 == 0 )) ; then

#         ssh ${w1}@emcrzdm "rm -rf ${wwwpath0}/${RUN}.${item}"

#      fi

#   done

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

cp $COMINobsproc_rtma3d/${PROD_HEAD}.LightningInGSI_bufr.bufr .

cp $COMINobsproc_rtma3d/${PROD_HEAD}.NASALaRCCloudInGSI.bufr .

cp $COMINobsproc_rtma3d/${PROD_HEAD}.NSSLRefInGSI.bufr .

cp $COMINobsproc_rtma3d/rtma_ru.${cycle}.prepbufr.tm00 .

cp $COMOUTgsi_rtma3d/${PROD_HEAD}.wrf_inout.nc .

cp $COMOUTgsi_rtma3d/fits_*.txt .

cp $COMOUTgsi_rtma3d/minimization_fort220.* .

cp $COMOUTgsi_rtma3d/gsiparm.anl* .

cp $COMOUTgsi_rtma3d/diag_* .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhprs_fgs.grib2 .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhspl_fgs.grib2 .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhnat_fgs.grib2 .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhprs.grib2 .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhspl.grib2 .

cp $COMOUTpost_rtma3d/${PROD_HEAD}.wrfsubhnat.grib2 .


   export HPSSOUT=$hpsspath0

   hpssdir2=${HPSSOUT}/2year/rh${YYYY}/${YYYY}${MM}/${YYYYMMDD}

   hpssdir0=${HPSSOUT}/rh${YYYY}/${YYYY}${MM}/${YYYYMMDD}

   

cyclist=t2560z

if [ $cyc$subcyc = "0000" ]

then

   cyclist="t0000z|t0100z|t0200z|t0300z|t0400z|t0500z"

   endcyc=0500

elif [ $cyc$subcyc = "0015" ] ; then

   cyclist="t0015z|t0115z|t0215z|t0315z|t0415z|t0515z"

   endcyc=0515

elif [ $cyc$subcyc = "0030" ] ; then

   cyclist="t0030z|t0130z|t0230z|t0330z|t0430z|t0530z"

   endcyc=0530

elif [ $cyc$subcyc = "0045" ] ; then

   cyclist="t0045z|t0145z|t0245z|t0345z|t0445z|t0545z"

   endcyc=0545

elif [ $cyc$subcyc = "0600" ] ; then

   cyclist="t0600z|t0700z|t0800z|t0900z|t1000z|t1100z"

   endcyc=1100

elif [ $cyc$subcyc = "0615" ] ; then

   cyclist="t0615z|t0715z|t0815z|t0915z|t1015z|t1115z"

   endcyc=1115

elif [ $cyc$subcyc = "0630" ] ; then

   cyclist="t0630z|t0730z|t0830z|t0930z|t1030z|t1130z"

   endcyc=1130

elif [ $cyc$subcyc = "0645" ] ; then 

   cyclist="t0645z|t0745z|t0845z|t0945z|t1045z|t1145z"

   endcyc=1145

elif [ $cyc$subcyc = "1200" ] ; then

   cyclist="t1200z|t1300z|t1400z|t1500z|t1600z|t1700z"

   endcyc=1700

elif [ $cyc$subcyc = "1215" ] ; then

   cyclist="t1215z|t1315z|t1415z|t1515z|t1615z|t1715z"

   endcyc=1715

elif [ $cyc$subcyc = "1230" ] ; then

   cyclist="t1230z|t1330z|t1430z|t1530z|t1630z|t1730z"

   endcyc=1730

elif [ $cyc$subcyc = "1245" ] ; then

   cyclist="t1245z|t1345z|t1445z|t1545z|t1645z|t1745z"

   endcyc=1745


elif [ $cyc$subcyc = "1800" ] ; then

   cyclist="t1800z|t1900z|t2000z|t2100z|t2200z|t2300z"

   endcyc=2300

elif [ $cyc$subcyc = "1815" ] ; then

   cyclist="t1815z|t1915z|t2015z|t2115z|t2215z|t2315z"

   endcyc=2315

elif [ $cyc$subcyc = "1830" ] ; then

   cyclist="t1830z|t1930z|t2030z|t2130z|t2230z|t2330z"

   endcyc=2330

elif [ $cyc$subcyc = "1845" ] ; then

   cyclist="t1845z|t1945z|t2045z|t2145z|t2245z|t2345z"

   endcyc=2345:1

fi




   #Redefine cyclist to save single analysis files MPondeca/30Jul2017

   cyclist="${cyc}${subcyc}"                              #MPondeca /30Jul2017

ls -1 $DATA_RHIST | grep -E "${cyclist}" | grep -v bufr_d.listing | awk '

            /prepbufr/ { print "./"$0 > "perm" ; next }

            /NASALaRCCloudInGSI/ { print "./"$0 > "perm" ; next }

            /NSSLRefInGSI/ { print "./"$0 > "perm" ; next }

            /LightningInGSI.bufr/ { print "./"$0 > "perm" ; next }

            /wrf_inout/ { print "./"$0 > "perm" ; next } 

            /subhprs/ { print "./"$0 > "perm" ; next } 

            /subhnat/ { print "./"$0 > "perm" ; next }'

ls -1 $DATA_RHIST | grep -E "${cyclist}" | awk ' 

        /minimization/ { print "./"$0 > "2yr" ; next }

        /gsiparm.anl/ { print "./"$0 > "2yr" ; next }

 	/diag/  { print "./"$0 > "2yr" ; next }'

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

   tarfile=${tarfile}${cyc}${subcyc}.tar       

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


