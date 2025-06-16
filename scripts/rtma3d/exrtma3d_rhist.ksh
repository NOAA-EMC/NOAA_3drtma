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


export CHECK_HPSS_IDX="YES"
#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS.
#

region=$RUN

year=`echo $PDYn1$cyc | cut -c 1-4`
yearmo=`echo $PDYn1$cyc | cut -c 1-6`
yrmoday=`echo $PDYn1$cyc | cut -c 1-8`

rhcyc=`echo $PDY$cyc | cut -c 9-10`
rhcycle=t${rhcyc}z

hpssdir2=${HPSSOUT_2yr}/rh${year}/${yearmo}/$yrmoday
hpssdir0=${HPSSOUT_5yr}/rh${year}/${yearmo}/$yrmoday


   #Redefine cyclist to save single analysis files MPondeca/30Jul2017
   cyclist="${rhcyc}"                            #MPondeca/30Jul2017

if [ $rhcyc -eq 0 ] ; then
   tarcyc=$PDYn1
else
   tarcyc=$PDY
fi

cd $DATA

cp $COMINm1/rtma3d.${cyclem1}.* $DATA/$PDYm1/prdgen.${cyclem1}
cp $COMINm2/rtma3d.${cyclem2}.* $DATA/$PDYm2/prdgen.${cyclem2}
cp $COMINm3/rtma3d.${cyclem3}.* $DATA/$PDYm3/prdgen.${cyclem3}

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

            /diag/  { print "./"$0 > "perm" ; next }

           /gsiparm.anl/ { print "./"$0 > "perm" ; next }

            /filelist/ { print "./"$0 > "perm" ; next }

            /wrf_inout/ { print "./"$0 > "perm" ; next }

           /stn_analysis/ { print "./"$0 > "perm" ; next }

           /firstguess.nc/ { print "./"$0 > "perm" ; next }  
 
           /.stat/ { print "./"$0 > "perm" ; next }

           /.csv/ { print "./"$0 > "perm" ; next }

           /.txt/ { print "./"$0 > "perm" ; next }

           /envir/ { print "./"$0 > "perm" ; next }

           /.db/ { print "./"$0 > "perm" ; next }'  

ls -1r */*/* | awk '

            /stdout/ { print "./"$0 > "2yr" ; next }
	    
            /obs.listing/ { print "./"$0 > "2yr" ; next }
	    
            /fits/ { print "./"$0 > "2yr" ; next }
	    
            /minimization/ { print "./"$0 > "2yr" ; next }

	    /OUTPUT/ { print "./"$0 > "2yr" ; next }'

ls -1r */*/* | awk '

            /hrrrdas_small/ { print "./"$0 > "perm_hrrrdas" ; next }'


export sync_list="perm 2yr perm_hrrrdas"

for file in ${sync_list}
do

   case $file in
      perm)   hpssdir=$hpssdir0
              tarfile=com_rtma3d_${tarcyc}_${cycm3}z-${cycm1}z.tar;;
      2yr)    hpssdir=$hpssdir2
	            tarfile=com_rtma3d_${tarcyc}_${cycm3}z-${cycm1}z.tar;;
      perm_hrrrdas) hpssdir=$hpssdir0
              tarfile=com_rtma3d_hrrrdas_${tarcyc}_${cycm3}z-${cycm1}z.tar;;
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

