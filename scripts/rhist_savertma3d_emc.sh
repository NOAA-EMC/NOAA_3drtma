#!/bin/sh
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
if [ $# -ne 3 ]
then
  echo "Usage: rhist_savertma.sh Directory Date(YYYYMMDDHH format) tarfile "
  exit 1
fi

#${USHrhist}/rhist_check.sh $1 $2
#if [ $? -eq 0 ] ; then
#    echo "Log entry found in $LOGrhist, skipped processing for: $0 $1 $2"
#    exit 0
#fi

#
#   Get directory to be tarred from the first command line argument,
#   and check to make sure that the directory exists.
#

dir=$1
if [ ! -d $dir ]
then
  echo "rhist_savertma.sh:  Directory $dir does not exist."
  exit 2
fi 
export TSM_FLAG=NO
#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS.
#

region=`echo $1 | cut -c 6-7`
year=`echo $2 | cut -c 1-4`
yearmo=`echo $2 | cut -c 1-6`
yrmoday=`echo $2 | cut -c 1-8`
rhcyc=`echo $2 | cut -c 9-10`
rhcycle=t${rhcyc}z

if [ $TSM_FLAG = 'NO' ]
then
 hpssdir2=${HPSSOUT}/2year/rh${year}/${yearmo}/$yrmoday
 hpssdir0=${HPSSOUT}/rh${year}/${yearmo}/$yrmoday
  
elif [ $TSM_FLAG = 'YES' ]
then
  rhistdir2=${TSMOUT}/2year/rh${year}/${yearmo}/$yrmoday
  rhistdir0=${TSMOUT}/rh${year}/${yearmo}/$yrmoday
  
  ssh ibmtsm1.ncep.noaa.gov "mkdir -p -m 755 $rhistdir2" 
fi

#
#   Get a listing of all files in the directory to be tarred
#   and break the file list up into groups of files.
#   Each list of files names the contents of its associated tar file.
#   Then cd to the directory to be tarred.
# 
cyclist=t25z
if [ $rhcyc = "00" ]
then
   cyclist="t00z|t01z|t02z|t03z|t04z|t05z"
   endcyc=05
elif [ $rhcyc = "06" ]
then
   cyclist="t06z|t07z|t08z|t09z|t10z|t11z"
   endcyc=11
elif [ $rhcyc = "12" ]
then
   cyclist="t12z|t13z|t14z|t15z|t16z|t17z"
   endcyc=17
elif [ $rhcyc = "18" ]
then
   cyclist="t18z|t19z|t20z|t21z|t22z|t23z"
   endcyc=23:1
fi

   #Redefine cyclist to save single analysis files MPondeca/30Jul2017
   cyclist="${rhcyc}"                              #MPondeca /30Jul2017

mkdir -p $dir/tmp

cd $dir/lightning.${PDY}/vaisala.${rhcycle}
for f in *5r; do mv $f `basename $f 5r`5r_vaisala; done;
cp * $dir/tmp
cd $dir/lightning.${PDY}/entln.${rhcycle}
for f in *5r; do mv $f `basename $f 5r`5r_entln; done;
cp * $dir/tmp
cd $dir/lightning.${PDY}
cp select_list_1.txt_total ../tmp/select_list_1.txt_total_lght
cp list_all_1.txt ../tmp/list_all_1.txt_lght



cd $dir/tmp

cp ../postprd.${rhcycle}/* .
cp ../fgsprd.${rhcycle}/*list .
cp ../radar.${PDY}/mrms.${rhcycle}/conus/* .
cp ../radar.${PDY}/select_list_1.txt_total select_list_1.txt_total_radar
cp ../radar.${PDY}/select_list_1.txt select_list_1.txt_radar
cp ../radar.${PDY}/list_all_1.txt list_all_1.txt_radar
cp ../rap.${PDY}/rap* .
cp ../rap.${PDY}/select_list_1.txt_total select_list_1.txt_total_rap
cp ../rap.${PDY}/list_all_1.txt list_all_1.txt_rap
cp ../gsiprd.${rhcycle}/* .
cp ../obsprd.${rhcycle}/* .
cp ../verif.${rhcycle}/* .
cp ../hrrr.${PDY}/conus/select_list_1.txt select_list_1.txt_hrrr
cp ../hrrr.${PDY}/conus/list_all_1.txt list_all_1.txt_hrrr
cp ../hrrr.${PDY}/conus/hrrr* .
cp ../fits_${PDY}${rhcyc}.txt .
cp ../stdout_var_gsianl.${PDY}${rhcyc} .
cp ../fgs_data_${PDY}_*.list .
cp ../stdout_cloud_gsianl.${PDY}${rhcyc} .



ls -1 $dir/tmp | grep -E "${cyclist}" | grep -v bufr_d.listing | awk '
            /wfrguess/ { print "./"$0 > "perm" ; next }
            /lgycld/ { print "./"$0 > "perm" ; next }
            /lghtng/ { print "./"$0 > "perm" ; next }
            /prepbufr/ { print "./"$0 > "perm" ; next }
            /grib2/ { print "./"$0 > "perm" ; next }
            /5r/ { print "./"$0 > "perm" ; next }
            /firstguess/ { print "./"$0 > "perm" ; next } 
            /NASALaRCCloudInGSI/ { print "./"$0 > "perm" ; next }
            /NSSLRefInGSI/ { print "./"$0 > "perm" ; next }
	    /LightningInGSI_bufr/ { print "./"$0 > "perm" ; next }
	    /wrf_inout/ { print "./"$0 > "perm" ; next }
            /wrfnat_hrconus_${rhcyc}/ { print "./"$0 > "perm" ; next }
            /wrfprs_hrconus_${rhcyc}/ { print "./"$0 > "perm" ; next }
            /wrftwo_hrconus_${rhcyc}/ { print "./"$0 > "perm" ; next } '

ls -1 $dir/tmp | grep -E "${cyclist}" | awk ' 
        /point_stat/ { print "./"$0 > "2yr" ; next }
        /select_list_1/ { print "./"$0 > "2yr" ; next }
        /list_all_1/  { print "./"$0 > "2yr" ; next }
        /fgs_data/ { print "./"$0 > "2yr" ; next }
        /fits/ { print "./"$0 > "2yr" ; next }
        /stdout_var_gsianl/ { print "./"$0 > "2yr" ; next }
        /stdout_cloud_gsianl/ { print "./"$0 > "2yr" ; next } '
export sync_list="perm 2yr"



#  Now create a tar file for each group of files

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

   #
   #   Generate the name of the tarfile, which should be the same
   #   as the absolute path name of the directory being
   #   tarred, except that "/" are replaced with "_".
   #

#  tarfile=`echo $PWD | cut -c 2- | tr "/" "_"`
#  #tarfile=${tarfile}${rhcyc}.${file}.tar
#  res=`echo $tarfile | grep -c rtma`
#  if [ $res -eq 0 ]
#  then
#     tarfile=${tarfile}${rhcyc}.tar
#  else
#     tarfile=${tarfile}${rhcyc}-${endcyc}.tar
#  fi
   tarfile=$3                                                #MPondeca , 30Jul2017
   tarfile=${tarfile}${rhcyc}.tar                            #MPondeca , 30Jul2017

   #
   #   Check if the tarfile index exists.  If it does, assume that
   #   the data for the corresponding directory has already been
   #   tarred and saved.
   #
 
   if [ $TSM_FLAG = 'NO' ]
   then
       if [[ $CHECK_HPSS_IDX == "YES" ]] ; then
	   hsi "ls -l ${hpssdir}/${tarfile}.idx"
	   tar_file_exists=$?
	   if [ $tar_file_exists -eq 0 ]
	   then
	       echo "File $tarfile already saved."
	       continue
	   fi
       fi
   elif [ $TSM_FLAG = 'YES' ]
   then
     size=`ssh ibmtsm1.ncep.noaa.gov ls -l ${rhistdir}/${tarfile} | awk '{ print \$5}'`
     if [  -n "$size" ]
     then
       if [ $size -gt 0 ]
       then
          echo "Directory $dir already saved."
          continue
       fi
     fi
   fi

   #   If on Stratus:
   #   htar is used to create the archive, -P creates
   #   the directory path if it does not already exist,
   #   and an index file is also made.
   #

   if [ $TSM_FLAG = 'NO' ]
   then
     date
     if [[ $DRY_RUN_ONLY == "YES" ]] ; then
	 echo "DRY RUN, list of files that would be archived:"
	 cat ${dir}/tmp/$file | sort
	 continue
     else
#         hsi mkdir -p ${hpssdir}
	 htar -P -cvf ${hpssdir}/$tarfile -L ${dir}/tmp/$file
	 err=$?
     fi
     if [ $err -ne 0 ]
     then
       echo "rhist_savertma.sh:  File $tarfile was not successfully created."
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
       echo "rhist_savertma.sh:  Tar file $tarfile was not successfully read to"
       echo "             generate a list of the files."
       exit 4
     fi
 
   #
   #  Restrict tar file, if it contains restricted data.
   #
       $HOMErtma3d/scripts/rhist_restrict.sh ${hpssdir}/$tarfile
 
   #
   #  send to HSM
   #
   elif [ $TSM_FLAG = 'YES' ]
   then

   #
   #   Tar up the directory and put the tarred file in the 
   #   appropriate directory in ${HPSSOUT}.
   #  
   
     date
     gtar -cvf ${dir}/tmp/$tarfile -T ${dir}/tmp/$file
     err=$?
     if [ $err -ne 0 ]
     then
       echo "rhist_savertma.sh:  File $tarfile was not successfully created."
       exit 3
     fi 

     $SCP $SCP_CONFIG ${dir}/tmp/${tarfile} ibmtsm1.ncep.noaa.gov:${rhistdir}/${tarfile}
     date
   fi

#rm  ${dir}/tmp/$file                     #MPondeca 30Jul2017
   
done

rm -r $dir/tmp

#[[ $DRY_RUN_ONLY != "YES" ]] && ${USHrhist}/rhist_log.sh $1 $2   #MPondeca 30Jul2017
#exit 0                                #MPondeca 30Jul2017

