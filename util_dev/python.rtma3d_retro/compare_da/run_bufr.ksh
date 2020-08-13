#!/bin/ksh

#-----------------------------------------------------------------------
#-- This script does the folowing functions:
#   1) Produces profdat file as output
#   2) Runs BUFR using the profdat file as input to produce the intermed.bufr file
#   3) Runs sounding post using intermed.bufr file to produce class1.bufr file
#   4) Runs GEMPAK using class1.bufr file to produce sounding (exper.snd) and
#      surface (exper.sfc*) GEMPAK files
#-----------------------------------------------------------------------

set -x

starttime=`date`

#-----------------------------------------------------------------------
#-- Input: 
DATA_DIR=$1       #-- Launcher's output working directory
LENGTH=$2         #-- Forecast length (h)
HISTH=$3          #-- Frequency of output history files in *hours*
#-----------------------------------------------------------------------

let nhrs=LENGTH/HISTH+1

#-- Go to working directory

wkdir=${DATA_DIR}/snds
mkdir -p $wkdir
cd $wkdir
if [ -a tmp ]; then
   rm -rf tmp
fi
rm -rf *.?? intermed.bufr*
mkdir tmp

FILES="bufr_hr sndp.parm nam_bufr.tbl modtop.parm sneta.prm sfeta.prm"
for file in $FILES ; do
   if [ ! -a $file ]; then
      echo Cannot find $file ... exit 1
      exit 1
   fi
done

if [ ! -x staids_nems.x ]; then
   echo Cannot find staids_nems.x or it is not an executable ... exit 2
   exit 2
fi

if [ ! -x wrfbufr_allinone.x ]; then
   if [ ! -x ${SRC_DIR}/exe/wrfbufr_allinone.x ]; then
      echo Cannot find wrfbufr_allinone.x or it is not an executable ... exit 2
      exit 2
   fi
   ln -s ${SRC_DIR}/exe/wrfbufr_allinone.x . 
fi

if [ -a abort ]; then
   rm abort     #-- Remove possible "abort" file from a previous attempt
fi

#-- Set up GEMPAK variables; temporarily turn off command echoing
set +x
echo Setting up GEMPAK variables without command echoing
. /gpfs/gp1/nco/ops/nwprod/gempak/.gempak > /dev/null 2>&1
set -x

#export MP_I_BINDPROC=NO
#export MP_BINDPROC=NO

errcode=0

#***********************************************************************
#-----  START of $grid loop through various domains  -----
#********************************************************

typeset -Z2 fhr grid

#-- Because the grid IDs start at 0 in the ksh scripts, MAXDOM=0 for single 
#   domain runs, =1 for a parent and a nest, =2 for a parent and 2 nests, etc.
#-- grid=01 for the parent, grid=02 etc for the nests
#-- Let ngrids be the whole number of grids, parent + nests, to process

let ngrids=1
let grid=1

while [ $grid -le $ngrids ]; do

#==================================================================================
#-- 1) Generate "profdat_$grid" file used in step 2

   profdat=profdat_$grid
   if [ -a $profdat ]; then
      rm $profdat
   fi

    model_input=/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_realtime/com2/rtma3d/lsf/rtma3d.${YMD}/gsiprd.t${cyc}${subcyc}z/rtma3d.t${cyc}${subcyc}z.wrf_inout.nc

   if [ ! -e $model_input ]; then
      echo Cannot find $model_input ... please check output to make sure everything is OK
   fi

   cp $model_input nemsiowrt_f000
   ln -sf nam_staids.parm   fort.15
   ln -sf $profdat          fort.63
   
   ./staids_nems.x > staids_${grid}.log
   let errcode=errcode+$?

   if [ $errcode -ne 0 ]; then
      echo Error $errcode trying to produce $prodat ... please check output to make sure everything is OK
   fi

#==================================================================================
#-- 2) Submit parallel LSF job to produce intermed.bufr_${grid}.$fhr files 
#      (grid=00,01,etc) for each forecast hour ($fhr).  Once they are all finished, 
#      combine them into a single intermed.bufr_$grid file.

   cat bufr_hr | sed s:HHH:$grid: > bufr_$grid
   let fhr=0
   while [ $fhr -le $LENGTH ]; do
      cat bufr_$grid | sed s:GGG:$fhr: > bufr_${grid}_$fhr
      bsub < bufr_${grid}_$fhr
      let fhr=fhr+HISTH
   done

#-- Wait for all intermed.bufr.$fhr or intermed.bufr_d0X.$fhr files to be produced

   bufrfile=intermed.bufr_$grid
   let nbufr=0
   let elapse=0
   while [ $nbufr -lt $nhrs ]; do
      nbufr=`ls -1 ${bufrfile}.?? | wc -l`
      sleep 10
      let elapse=elapse+10
      echo Waiting $elapse s - $nbufr out of $nhrs files have finished
   done

   if [ -a abort ]; then
      echo Please look at the file abort, which means that one of the 
      echo bufr jobs finished with a nonzero error code
   fi
   
#-- Merge/concatenate individual forecast times (HH:MM) to a combined file

   let fhr=0
   while [ $fhr -le $LENGTH ]; do
      cat ${bufrfile}.$fhr >> $bufrfile
      let fhr=fhr+HISTH
   done   #- fhr

#-- Remove working directories and individual forecast hour files
#   rm -rf *.[0-9][0-9] ${bufrfile}.*

#==================================================================================
#-- 3) Run sounding post to produce output class1_${grid}.bufr file using the
#      intermed.bufr_$grid file (output from previous step)

   class1=class1_${grid}.bufr
   rm fort.* $class1

   ln -sf sndp.parm        fort.11        # Input
   ln -sf nam_bufr.tbl     fort.32        # Input
   ln -sf $bufrfile        fort.66        # Input (output from previous WHILE loop)
   ln -sf $class1          fort.78        # Output
   
   ./sndp.exe < modtop.parm > sndp_${grid}.out
   let errcode=errcode+$?

   if [ $errcode -ne 0 ]; then
      echo Error $errcode trying to produce $class1 ... exit 2
      exit 2
   fi

   /gpfs/gp1/nco/ops/nwprod/ush/cwordsh unblk class1_${grid}.bufr class1_${grid}.bufr_unblk
   /gpfs/gp1/nco/ops/nwprod/ush/cwordsh block class1_${grid}.bufr_unblk class1_${grid}.bufr_blk
   
   rm $class1
   
   class1=class1_${grid}.bufr_blk

#==================================================================================
#-- 4) Run GEMPAK to using input class_${grid}.bufr file (output from previous step)
#      to produce output GEMPAK surface (*.sfc) and sounding (*.snd) files.

   gemfile=exper_$grid
   sndfile=${gemfile}.snd
   sfcfile=${gemfile}.sfc
   rm fort.* ${gemfile}*

   namsnd << EOF
 SNBUFR   = $class1
 SNOUTF   = $sndfile
 SFOUTF   = ${sfcfile}+
 SNPRMF   = sneta.prm
 SFPRMF   = sfeta.prm
 TIMSTN   = 100/2000
r

ex

EOF

#-- Finished processing this grid, producing an exper_${grid}.snd sounding file,
#   an exper_${grid}.sfc and exper_${grid}.sfc_aux surface files

   let errcode=errcode+$?

   if [ $errcode -ne 0 ]; then
      echo Error $errcode trying to produce $sndfile and $sfcfile ... exit 3
      exit 3
   fi

   echo Start time was $starttime and end time was `date` for grid $grid

   let grid=grid+1

done       #-- while [ $grid -le $MAXDOM ]; do

#***********************************************************************
#-----  END of $grid loop through various domains  -----
#********************************************************

#--- Make copies to use legacy codes for plotting soundings & vertical profiles

cp exper_01.snd exper.snd
cp exper_01.sfc exper.sfc
cp exper_01.sfc_aux exper.sfc_aux

if [ $errcode -eq 0 ]; then
   echo No errors, so move temporary files into the /tmp subdirectory
   mkdir ../snds_tmp
   mv *.snd *.sfc* staids*.log *.nts sites ../snds_tmp
   mv tmp ../tmp.$$
   mv * ../tmp.$$
   mv ../tmp.$$ tmp
   mv ../snds_tmp/* .
   rmdir ../snds_tmp
fi

cat > README << EOF
Key files:
=========
exper_XX.snd - GEMPAK sounding file(s) for domain XX, where XX=01 (parent), 02 etc (nests)
exper.snd - Copy of exper_01.snd for legacy codes
exper_XX.sfc - GEMPAK surface file(s) for domain XX=01 (parent), 02 etc (nests)
exper_XX.sfc_aux - auxiliary GEMPAK surface files for additional stations
staids_XX.log - station ID output used to produce profdat_XX files for domain XX=01, 02, etc

All other working files and directories are moved into the "work" subdirectory for debugging.
EOF

#--- Initiate the GEMPAK plotting script (run_pltsnd) if script is present.

cd $cwd
if [ -a run_pltsnd ]; then
   bsub < run_pltsnd
else
   echo "run_pltsnd missing."
fi

echo Start time was $starttime and end time was `date` for BUFR processing of all grids

exit
