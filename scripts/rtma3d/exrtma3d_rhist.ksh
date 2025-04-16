#!/bin/ksh
################################################################3
#
#  This script will tar up all the data for a given forecast cycle for
#  the directory specified by the first
#  argument ($1) and place the tar file on the HPSS server
#  under ${HPSSOUT}.  The tar file is put in the directory
#  appropriate for data valid for the day specified as the second 
#  command line argument ($2).
#
#  This script breaks up the rtma data directory and saves selected
#  files into four different tar files, where each tarfile contains
#  data for 6 forecast cycles.  For example, data files from the 00Z through 
#  05Z runs are saved together in a tar file labelled as *YYYYMMDD00-05.tar.  
#  For the rtma, all files are save in the 2 year archive.
#
#  Usage: rhist_savertma.sh Directory Date(YYYYMMDDHH format)
#
#  Where: Directory  = Directory to be tarred.
#         Date(YYYYMMDDHH format) = Day that the tar file should be saved under.
#
################################################################3
set -x

#if [ $# -ne 2 ]
#then
#  echo "Usage: rhist_savertma.sh Directory Date(YYYYMMDDHH format) "
#  exit 1
#fi 

#if [ $# -ne 3 ]
#then
#  echo "Usage: rhist_savertma.sh Directory Date(YYYYMMDDHH format) tarfile #"
#  exit 1
#fi

#${USHrhist}/rhist_check.sh $1 $2
#if [ $? -eq 0 ] ; then
#    echo "Log entry found in $LOGrhist, skipped processing for: $0 $1 $2"
#    exit 0
#fi

#
#   Get directory to be tarred from the first command line argument,
#   and check to make sure that the directory exists.
#

dir=$DATA
if [ ! -d $dir ]
then
  echo "rhist_savertma.sh:  Directory $dir does not exist."
  exit 2
fi 

export CHECK_HPSS_IDX="YES"
#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS.
#

region=$RUN
year=`echo $PDY$cyc | cut -c 1-4`
yearmo=`echo $PDY$cyc | cut -c 1-6`
yrmoday=`echo $PDY$cyc | cut -c 1-8`
rhcyc=`echo $PDY$cyc | cut -c 9-10`
rhcycle=t${rhcyc}z

hpssdir2=${HPSSOUT}/2year/rh${year}/${yearmo}/$yrmoday
hpssdir0=${HPSSOUT}/rh${year}/${yearmo}/$yrmoday

#
#   Get a listing of all files in the directory to be tarred
#   and break the file list up into groups of files.
#   Each list of files names the contents of its associated tar file.
#   Then cd to the directory to be tarred.
# 
#cyclist=t25z
#if [ $rhcyc = "00" ]
#then
#   cyclist="t00z|t01z|t02z|t03z|t04z|t05z"
#   endcyc=05
#elif [ $rhcyc = "06" ]
#then
#   cyclist="t06z|t07z|t08z|t09z|t10z|t11z"
#   endcyc=11
#elif [ $rhcyc = "12" ]
#then
#   cyclist="t12z|t13z|t14z|t15z|t16z|t17z"
#   endcyc=17
#elif [ $rhcyc = "18" ]
##then
 #  cyclist="t18z|t19z|t20z|t21z|t22z|t23z"
 #  endcyc=23
#fi

   #Redefine cyclist to save single analysis files MPondeca/30Jul2017
   cyclist="${rhcyc}"                            #MPondeca/30Jul2017

cd $DATA

cp $COMINm1/rtma3d.${cyclem1}.* $DATA/$PDYm1
cp $COMINm2/rtma3d.${cyclem2}.* $DATA/$PDYm2
cp $COMINm3/rtma3d.${cyclem3}.* $DATA/$PDYm3

ln -sf  $COMINobsproc_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMINobsproc_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMINobsproc_rtma3dm3 $DATA/$PDYm3

ln -sf  $GESINhrrr_rtma3dm1 $DATA/$PDYm1
ln -sf  $GESINhrrr_rtma3dm2 $DATA/$PDYm2
ln -sf  $GESINhrrr_rtma3dm3 $DATA/$PDYm3

ln -sf  $COMOUTgsi_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMOUTgsi_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMOUTgsi_rtma3dm3 $DATA/$PDYm3

ln -sf  $COMOUTpost_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMOUTpost_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMOUTpost_rtma3dm3 $DATA/$PDYm3

ln -sf  $COMOUTbufrsnd_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMOUTbufrsnd_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMOUTbufrsnd_rtma3dm3 $DATA/$PDYm3

ln -sf  $COMOUTautoqc_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMOUTautoqc_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMOUTautoqc_rtma3dm3 $DATA/$PDYm3

ln -sf  $COMOUThrrrdas_rtma3dm1 $DATA/$PDYm1
ln -sf  $COMOUThrrrdas_rtma3dm2 $DATA/$PDYm2
ln -sf  $COMOUThrrrdas_rtma3dm3 $DATA/$PDYm3


ls -1r */*/* | awk '

            /prepbufr/ { print "./"$0 > "perm" ; next }

	    /bufr/ { print "./"$0 > "perm" ; next }

            /grib2/ { print "./"$0 > "perm" ; next }

	    /grb2/ { print "./"$0 > "perm" ; next }

            /rejectlist/ { print "./"$0 > "perm" ; next }

	    /NASALaRCCloudInGSI/ { print "./"$0 > "perm" ; next }

            /NSSLRefInGSI/ { print "./"$0 > "perm" ; next }

            /LightningInGSI.bufr/ { print "./"$0 > "perm" ; next }

            /satwnd/ { print "./"$0 > "perm" ; next }

	    /nexrad/ { print "./"$0 > "perm" ; next }

	    /firstguess.nc/ { print "./"$0 > "perm" ; next }
           
            /subhprs/ { print "./"$0 > "perm" ; next } 

            /subhnat/ { print "./"$0 > "perm" ; next }
           
            /smarthrrrconus/ { print "./"$0 > "perm" ; next }

	    /smarthrrrak/ { print "./"$0 > "perm" ; next }

            /smartak/ { print "./"$0 > "perm" ; next }

            /wavebg/ { print "./"$0 > "perm" ; next }

	    /multi_1/ { print "./"$0 > "perm" ; next }
       
       	    /ww3/ { print "./"$0 > "perm" ; next }

	    /minimization/ { print "./"$0 > "perm" ; next }

            /gsiparm.anl/ { print "./"$0 > "perm" ; next }

            /diag/  { print "./"$0 > "perm" ; next }

           /.stat/ { print "./"$0 > "perm" ; next } 

           /.csv/ { print "./"$0 > "perm" ; next }

           /.txt/ { print "./"$0 > "perm" ; next } 

           /envir/ { print "./"$0 > "perm" ; next } 

           /.db/ { print "./"$0 > "perm" ; next }

	   /obs.listing/ { print "./"$0 > "perm" ; next }

           /stn_analysis/ { print "./"$0 > "perm" ; next }

           /envir.sh/ { print "./"$0 > "perm" ; next }
           
           /firstguess.nc/ { print "./"$0 > "perm" ; next }'


ls -1r */*/* | awk '

            /fits/ { print "./"$0 > "2yr" ; next }

            /stdout/ { print "./"$0 > "2yr" ; next }

	    /OUTPUT/ { print "./"$0 > "2yr" ; next }'

ls -1r */*/* | awk '

            /hrrrdas_small/ { print "./"$0 > "perm_hrrrdas" ; next }'


export sync_list="perm 2yr perm_hrrrdas"

for file in ${sync_list}
do

   case $file in
      perm)   hpssdir=$hpssdir0
	      tarfile=com_rtma3d_${PDY}_${cycm3}z-${cycm1}z.tar;;

      2yr)    hpssdir=$hpssdir2
	      tarfile=com_rtma3d_${PDY}_${cycm3}z-${cycm1}z.tar;;

      perm_hrrrdas) hpssdir=$hpssdir0
              tarfile=com_rtma3d_hrrrdas_${PDY}_${cycm3}z-${cycm1}z.tar;;
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

     htar -P -cvf ${hpssdir}/$tarfile -L ${DATA}/$file 
     err=$?
     if [ $err -ne 0 ]
     then
       echo "exrtma3d_rhist.sh:  Tar file $tarfile was not successfully read to"
       echo "             generate a list of the files."
       exit 4
     fi
 
   #
   #  Restrict tar file, if it contains restricted data.
   #
       ${UTILrtma3d_dev}/rhist_restrict.sh ${hpssdir}/$tarfile htar
 

#rm  ${DATA}/$file                     #MPondeca 30Jul2017
   
done

#[[ $DRY_RUN_ONLY != "YES" ]] && ${USHrhist}/rhist_log.sh $1 $2   #MPondeca 30Jul2017
#exit 0                                #MPondeca 30Jul2017

