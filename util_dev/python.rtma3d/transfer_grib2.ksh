#!/bin/ksh --login

set -x 

PROD_HEAD=rtma3d
DATA_RHIST=/gpfs/dell1/ptmp/Edward.Colon/rtma3dda/grib2_wrkdir_${YMD}${cyc}${subcyc}
CHECK_HPSS_IDX=YES
tarfile_1=com2_rtma3d
YYYY=`echo ${YMD} | cut -c 1-4`
YYYYMM=`echo ${YMD} | cut -c 1-6`
NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate
YMDm1=`$NDATE -24 ${YMD}${cyc} | cut -c 1-8`


if [ -d ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z ] ; then

cd ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z

rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs.grib2 ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/analyses/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs.grib2
rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat.grib2 ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/analyses/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat.grib2

cd ${COMFV3}/${PROD_HEAD}.${YMD}/snds.t${cyc}${subcyc}z
rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.snds.grib2 ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/spc/${PROD_HEAD}.t${cyc}${subcyc}z.snds.grib2
rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.snds.gem ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/spc/${PROD_HEAD}.t${cyc}${subcyc}z.snds.gem

rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.grib2 ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/spc/${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.grib2
rsync -t  ${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.gem ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/spc/${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.gem


cd ${COMFV3}/${PROD_HEAD}.${YMD}/verifprd.t${cyc}${subcyc}z

rsync -t * ecolon@emcrzdm.ncep.noaa.gov:/home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/stats


ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs.grib2"
ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat.grib2"

ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs.grib2"
ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat.grib2"

ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.snds.grib2"
ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.snds.gem"

ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.grib2"
ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm1}/${PROD_HEAD}.t${cyc}${subcyc}z.snds_fgs.gem"


fi




   #Because in development runs the various observations files can come from different places,

   #create an equivalent to NCO's $COMINobsproc_rtma first, and dump the observations files there

   #before evoking the rhist scripta

export GESINHRRR=/gpfs/dell1/ptmp/Annette.Gibbs/com/hrrr/prod
export PDYHH_cycm1=$($NDATE -01  "${YMD}${cyc}")
HH_cycm1=`echo ${PDYHH_cycm1} | cut -c 9-10` 
YMDm1=`echo ${PDYHH_cycm1} | cut -c 1-8`
export GESINhrrr=${GESINHRRR}/hrrr.${YMD}/conus
export GESINhrrrm1=${GESINHRRR}/hrrr.${YMDm1}/conus
export FGShrrr_FNAME2="hrrr.t${HH_cycm1}00z.f01${subcyc}.netcdf"


   mkdir -p $DATA_RHIST

   cd $DATA_RHIST

cp /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD}/*_t${cyc}${subcyc}z.* .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/snds.t${cyc}${subcyc}z/*.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/snds.t${cyc}${subcyc}z/*.gem .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/verif.t${cyc}${subcyc}z/* .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/obsprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.LightningInGSI_bufr.bufr .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/obsprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.NASALaRCCloudInGSI.bufr .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/obsprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.NSSLRefInGSI.bufr .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/obsprd.t${cyc}${subcyc}z/rtma_ru.t${cyc}${subcyc}z.prepbufr.tm00 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/gsiprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrf_inout.nc .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/gsiprd.t${cyc}${subcyc}z/fits_*.txt .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/gsiprd.t${cyc}${subcyc}z/minimization_fort220.* .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/gsiprd.t${cyc}${subcyc}z/gsiparm.anl* .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/gsiprd.t${cyc}${subcyc}z/diag_* .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs_fgs.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhspl_fgs.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat_fgs.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhprs.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhspl.grib2 .

cp ${COMFV3}/${PROD_HEAD}.${YMD}/postprd.t${cyc}${subcyc}z/${PROD_HEAD}.t${cyc}${subcyc}z.wrfsubhnat.grib2 .


if  [ ${HH_cycm1} -eq 23 ]; then

	if [ -r ${GESINhrrrm1}/${FGShrrr_FNAME2} ] ; then
		cp ${GESINhrrrm1}/${FGShrrr_FNAME2} .
	else
	  	${ECHO} "ERROR: No background file for analysis."
	fi
else
        if [ -r ${GESINhrrr}/${FGShrrr_FNAME2} ] ; then
		cp ${GESINhrrr}/${FGShrrr_FNAME2} .
        else
                ${ECHO} "ERROR: No background file for analysis."
	fi
fi



   export HPSSOUT=/NCEPDEV/emc-meso/5year/Edward.Colon/${PROD_HEAD}

   hpssdir2=${HPSSOUT}/2year/rh${YYYY}/${YYYYMM}/${YMD}

   hpssdir0=${HPSSOUT}/rh${YYYY}/${YYYYMM}/${YMD}

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

 	/diag/  { print "./"$0 > "2yr" ; next }

        /snds.grib2/  { print "./"$0 > "2yr" ; next }

        /snds.gem/  { print "./"$0 > "2yr" ; next } 

     	/.stat/ { print "./"$0 > "2yr" ; next } 

	/.txt/ { print "./"$0 > "2yr" ; next } 

       /.png/ { print "./"$0 > "2yr" ; next }'


ls -1 $DATA_RHIST | grep -E "${HH_cycm1}00z.f01${subcyc}" | awk ' 

       /netcdf/ { print "./"$0 > "perm_hrrrfgs" ; next }'



export sync_list="perm perm_hrrrfgs 2yr"

for file in ${sync_list}

do

   #

   #   Pick 2year or permanent archive.

   #

   case $file in
	perm)   hpssdir=$hpssdir0
                tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
                tarfile=${tarfile}.${YMD}${cyc}${subcyc}.tar
	;;
        perm_hrrrfgs) hpssdir=$hpssdir0
                tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
                tarfile=${tarfile}_fgs.${YMD}${cyc}${subcyc}.tar

	;;
	2yr)   hpssdir=$hpssdir2
               tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
               tarfile=${tarfile}.${YMD}${cyc}${subcyc}.tar
        ;;
        *)      hpssdir=$hpssdir2
               tarfile=$tarfile_1                                                #MPondeca , 30Jul2017
               tarfile=${tarfile}.${YMD}${cyc}${subcyc}.tar
	;;
   esac
   if [[ $CHECK_HPSS_IDX == "YES" ]] ; then

           hsi "ls -l ${hpssdir}/${tarfile}.idx"

           tar_file_exists=$?

           if [ $tar_file_exists -eq 0 ]

           then

               echo "File $tarfile already saved."

               continue

           fi

    fi 


         htar -P -cvf ${hpssdir}/$tarfile -L ${DATA_RHIST}/$file

         err=$?


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


     cd $DATA_RHIST/..

     /bin/rm -rf $DATA_RHIST



exit
