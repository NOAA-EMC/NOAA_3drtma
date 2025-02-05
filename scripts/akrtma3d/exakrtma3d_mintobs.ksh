#!/bin/ksh

#   DOCBLOCK
#
# Script Name: urma_maxtobs.sh
# Author: Steven Levine
# Abstract: Read in previous 25 URMA prepbufr files, use those files to compute
# maxT obs.  Output text files of obs is used in 08Z URMA, which includes MaxT.
# History Log:
#   9/2015: Initial write for WCOSS
#
# Usage:
#  Parameters: None
#  Input files:
#    urma.tHHz.prepbufr, for previous 25 URMA cycles
#  Output files:
#    urma.YYYYMMDD.maxtobs.dat
#
# Condition codes:
#     99  - Missing input file
#
# User controllable options: None
#
# DOCBLOCK
#-----------------------------------------------------------------------
##
## Source the variable definitions file and the bash utility functions.
##
##-----------------------------------------------------------------------
##
## Save current shell options (in a global array).  Then set new options
## for this script/function.
##
##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------
##

cd ${DATA}

if [ -z "$WGRIB2" ]; then
    err_exit "\$WGRIB2 must be set to the location of the wgrib2 executable!"
fi

#input list is of domains
#default to cohreswexp akhres prico hawaii

if [[ -z "$1" ]] ; then
    grids="hrrr"
else
    grids=$1
fi

gridsarray=($grids)
num=${#gridsarray[@]}

if [[ $num -gt 10 ]] ; then
    err_exit "Only ten grid names may be specified.  User specified ${num}."
fi


ulimit -s unlimited
ulimit -a

CYCLE="${PDY}18"
CYCLE_STOP="${PDYm1}18"

OBS_DIR=${DATAOBSHOME}
obsfileprefix=akrtma3d

while [[ $CYCLE -ge $CYCLE_STOP ]] ; do

   YYYYMMDD=`echo $CYCLE | cut -c1-8`
   HH=`echo $CYCLE | cut -c 9-10`

   #copy URMA prepbufr files into workind directory
   #account for 25 hours by using _prevday for same hour, prev day


   if [[ $CYCLE -eq $CYCLE_STOP ]] ; then
      cpfs $OBS_DIR/${obsfileprefix}.t${HH}z.prepbufr.tm00 ${obsfileprefix}.t${HH}z.prepbufr.tm00_prevday
   else
      cpfs $OBS_DIR/${obsfileprefix}.t${HH}z.prepbufr.tm00 ${obsfileprefix}.t${HH}z.prepbufr.tm00
   fi
   CYCLE=`$NDATE -01 $CYCLE | cut -c 1-10`
done
#. prep_step
#Assign fortran unit variables
#INPUT
ln -sf $FIXminmax/metar.dat fort.11
ln -sf $FIXminmax/metarak.dat fort.12
ln -sf $FIXminmax/mesoa.dat fort.13
ln -sf $FIXminmax/mesob.dat fort.14
ln -sf $FIXminmax/mesoc.dat fort.15
ln -sf $FIXminmax/mesod.dat fort.16
ln -sf $FIXminmax/mesoe.dat fort.17
ln -sf $FIXminmax/mesof.dat fort.18
ln -sf $FIXminmax/mesoak.dat fort.19
ln -sf $FIXminmax/mesopr.dat fort.20
ln -sf $FIXminmax/mesohi.dat fort.21
ln -sf $FIXminmax/mesogu.dat fort.22
ln -sf $FIXminmax/ship.dat fort.23
ln -sf $FIXminmax/shipak.dat fort.24
export FORT25=${obsfileprefix}.t06z.prepbufr.tm00_prevday
export FORT26=${obsfileprefix}.t07z.prepbufr.tm00
export FORT27=${obsfileprefix}.t08z.prepbufr.tm00
export FORT28=${obsfileprefix}.t09z.prepbufr.tm00
export FORT29=${obsfileprefix}.t10z.prepbufr.tm00
export FORT30=${obsfileprefix}.t11z.prepbufr.tm00
export FORT31=${obsfileprefix}.t12z.prepbufr.tm00
export FORT32=${obsfileprefix}.t13z.prepbufr.tm00
export FORT33=${obsfileprefix}.t14z.prepbufr.tm00
export FORT34=${obsfileprefix}.t15z.prepbufr.tm00
export FORT35=${obsfileprefix}.t16z.prepbufr.tm00
export FORT36=${obsfileprefix}.t17z.prepbufr.tm00
export FORT37=${obsfileprefix}.t18z.prepbufr.tm00
export FORT38=${obsfileprefix}.t19z.prepbufr.tm00
export FORT39=${obsfileprefix}.t20z.prepbufr.tm00
export FORT40=${obsfileprefix}.t21z.prepbufr.tm00
export FORT41=${obsfileprefix}.t22z.prepbufr.tm00
export FORT42=${obsfileprefix}.t23z.prepbufr.tm00
export FORT43=${obsfileprefix}.t00z.prepbufr.tm00
export FORT44=${obsfileprefix}.t01z.prepbufr.tm00
export FORT45=${obsfileprefix}.t02z.prepbufr.tm00
export FORT46=${obsfileprefix}.t03z.prepbufr.tm00
export FORT47=${obsfileprefix}.t04z.prepbufr.tm00
export FORT48=${obsfileprefix}.t05z.prepbufr.tm00
export FORT49=${obsfileprefix}.t06z.prepbufr.tm00

#OUTPUT
export FORT61=rtma.${PDYm1}.maxtobs.dat
export FORT71=adpsfc_max_diag.dat
export FORT72=adpsfcak_max_diag.dat
export FORT73=ships_max_diag.dat
export FORT74=shipsak_max_diag.dat
export FORT75=mesomaxa_diag.dat
export FORT76=mesomaxb_diag.dat
export FORT77=mesomaxc_diag.dat
export FORT78=mesomaxd_diag.dat
export FORT79=mesomaxe_diag.dat
export FORT80=mesomaxf_diag.dat
export FORT81=mesopr_max_diag.dat
export FORT82=mesohi_max_diag.dat
export FORT83=mesogu_max_diag.dat
export FORT84=mesoak_max_diag.dat

export pgm=rtma_mintobs.exe
startmsg
$EXECrtma3d/$pgm  > $pgmout 2>errfile
export err=$?; err_chk
cat $pgmout

if [ -s rtma.${PDYm1}.maxtobs.dat ] ; then
    cp rtma.${PDYm1}.maxtobs.dat  $COMOUTpost_rtma3d/
else
    echo "WARNING: RTMA maxT ob file was not generated properly!"
    echo "URMA maxT will have no obs!"
fi

