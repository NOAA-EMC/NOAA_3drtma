#!/bin/ksh

#   DOCBLOCK
#
# Script Name: urma_maxrh.sh
# Author: Steven Levine
# Abstract: Read in previous 25 URMA background/analysis files, use those
# to compute maxRH for 20Z URMA
# History Log:
#    04/2017: Initial write to WCOSS - originated with exurma_getmaxt.sh
#    08/2017: Bug fix to make sure dates in maxRH file are correct - use dummy file to start
##
# Usage:
#  Parameters: None
#  Input files:
#    Analysis (2dvaranl) and guess (2dvarges) files from each of the last 25 URMA
#    cycles.  Exact file names depend on the grids specified by the user.  They are
#    specified in variables opsfile and gesfile
#    Also dummy grib2 files for each grid name used.  Dummy files are in ${HOMEurma}/fix/maxmint
#    and are called ${gridname}.temprh.grb2_0.  Grid names are specified in sorc file domain_dims.f.
#
#
#  Output files:
#    urma2p5.${PDY}.maxRH.bin, urma2p5.${PDY}.minRH.grb2
#    akurma.${PDY}.maxRH.bin, akurma.${PDY}.minRH.grb2
#    hiurma.${PDY}.maxRH.bin, hiurma.${PDY}.minRH.grb2
#    prurma.${PDY}.maxRH.bin, prurma.${PDY}.minRH.grb2
#
#  User controllable options:
#     $1 is an array of grid names.  Grid names are specified in sorc file domain_dims.f
#
#     In file blend_input (generated in script), bnum can be 1 (use guess files only), 2 (use analysis files only), or
#     3 (use larger value in analysis vs guess file at each grid point).  Any other value will
#     cause executable to fail.
#
#    NOTE 1: Not all domain names included in sorc file domain_dims.f are listed in this script because
#           they are not used in operations and their use is not conceived of any time soon.
#           If user tries to include grid name in domain_dims.f but not specified in beginning of first
#           while loop, the script will abort.
#           The missing grid names are: juneau and hrrr
#           If new domain names are ever added, this script will have to be updated to accomodate.  Also, new dummy/fixed
#           grib2 files will be needed.
#    NOTE 2: A maximum of one grid for each domain (CONUS/HI/PR/AK) may be specified in $1.  Examples of grid combinations
#           not allowed include: conus & cohreswexp, alaska & akhres.  This is to prevent file name conflicts.
#    NOTE 3: that if no grid names are provided in $1, script will default to cohreswexp, akhres, prico, and hawaii
#           which are the current grids in operation.
#
#  DOCBLOCK


set -x 

cd ${DATA}


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
    err_exit "FATAL ERROR: Only ten grid naems may be specified.  User specified ${num}."
fi

#generate namelist input file, make sure that no grid in same domain is repeated twice
#also copy into workspace template files
Tur=no; Tak=no; Tpr=no; Thi=no; Tgu=no
echo "&gridsinfo" > gridsinfo_input
echo "     grids=${num}" >> gridsinfo_input
nn=0
while [[ $nn -lt $num ]] ; do
    let nnp1="nn+1"
    dname=${gridsarray[$nn]}
    echo "     gridnames($nnp1)=${dname}," >> gridsinfo_input
    Tur=yes
    run=urma2p5
#    cpfs $FIXminmax/urma2p5.temprh.grb2_0 tempgrib_urma2p5.grb2
	Tur=yes
#	cpfs $FIXminmax/urma2p5.temprh.grb2_0 tempgrib_urma2p5.grb2

	CYCLE=${PDY}1800
        CYCLE_STOP=${PDY}0600

    hr=0 #hour number to go in output (from wgrib2) file name
    
    while [[ $CYCLE -ge $CYCLE_STOP ]] ; do
	
	let hr="$hr+1"
	if [[ $hr -lt 10 ]] ; then
	    hr="0${hr}"
	fi

	YYYY=`echo $CYCLE | cut -c 1-4`
	YYYYMM=`echo $CYCLE | cut -c 1-6`
	YYYYMMDD=`echo $CYCLE | cut -c 1-8`
	HH=`echo $CYCLE | cut -c 9-10`
	
	#find proper name of ges and analysis files to run wgrib2 on based on run and domain name
       #find proper name of ges and analysis files to run wgrib2 on based on run and domain name
        if [[ $run = "urma2p5" ]] ; then
            if [[ $dname == "hrrr" ]] ; then
                opsfile=${COM_IN}/${RUN}.${YYYYMMDD}/postprd.t${HH}00z/${RUN}.t${HH}00z.wrfsubhnat.grib2
                gesfile=${COM_IN}/${RUN}.${YYYYMMDD}/postprd.t${HH}00z/${RUN}.t${HH}00z.wrfsubhnat_fgs.grib2
            fi
        fi
	#now use wgrib2 to pull ges/analysis from the file
	#use run in binary filename, regardless of grid
	if [[ -s $gesfile ]] ; then
	    tmpops=${run}_temp_anl_hour_${hr}.bin
	    tmpges=${run}_temp_ges_hour_${hr}.bin
	    dptops=${run}_dwpt_anl_hour_${hr}.bin
	    dptges=${run}_dwpt_ges_hour_${hr}.bin
	    $WGRIB2 $opsfile -match ":TMP:2 m above ground:" -ieee $tmpops
	    $WGRIB2 $gesfile -match ":TMP:2 m above ground:" -ieee $tmpges
	    $WGRIB2 $opsfile -match ":DPT:2 m above ground:" -ieee $dptops
	    $WGRIB2 $gesfile -match ":DPT:2 m above ground:" -ieee $dptges
	else
	    echo "WARNING: ${run} file unavailable for ${CYCLE}!";
	fi	
	CYCLE=`$MDATE -60 $CYCLE`

        if [ ${HH} -eq 06 ] ; then
            if [[ $run == "urma2p5" ]] ; then
                cpfs $gesfile gesfileus.grb2
            fi
        fi


done #$CYCLE -le $CYCLE_STOP (number of cycles to run)
    
    let nn="$nn+1"
done #$nn -lt $num (number of grids)


echo "/" >> gridsinfo_input

#make blend?  1=bgs only 2=anls only, 3=both
export bnum=2
cat <<EOF > blend_input
&blendinput
bnum=${bnum}
/
EOF

. prep_step

ln -sf $run.${PDYm1}.maxrh_anl.dat fort.61
ln -sf $run.${PDYm1}.maxrh_bg.dat fort.62

export pgm=urma_maxrh
startmsg
$EXECrtma3d/rtma3d_maxrh >> $pgmout 2> errfile
export err=$?; err_chk
cat $pgmout

#wgrib2 options: -set_byte 4 48 1 ensures we are dealing with succession of analyses
#-set_byte 4 47 3 ensures we are dealing with maximum value
#-set_date ${PDY}18 - must be start time, not end time

if [[ $Tur = yes ]] ; then
    if [[ $bnum -eq "2" || $bnum -eq "3" ]] ; then
	if [ -s $DATA/$run.${PDYm1}.maxrh_anl.dat ] ; then
            $WGRIB2 $DATA/gesfileus.grb2 -match ":RH:" -grib_out $DATA/tempgribus.grb2
            $WGRIB2 $DATA/tempgribus.grb2 -import_ieee urma2p5.${PDYm1}.maxrh_anl.dat -set_date ${PDYm1}18 -set_var MAXRH -set_ftime '12 hour fcst' -undefine_val 0 -grib_out $DATA/$run.${PDYm1}.maxRH.grb2 
            cpfs $DATA/$run.${PDYm1}.maxrh_anl.dat $DATA/$run.${PDYm1}.maxrh_anl.dat
            cpfs $DATA/$run.${PDYm1}.maxRH.grb2 $COMOUTpost_rtma3d/rtma3d.maxRH.grib2
	else
	    echo "WARNING: CONUS Min RH analysis not available from main program!"
	fi
    fi
    if [[ $bnum -eq "1" || $bnum -eq "3" ]] ; then
	if [ -s $DATA/$run..${PDYm1}.maxrh_bg.dat ] ; then 
            $WGRIB2 $DATA/gesfileus.grb2 -match ":RH:" -grib_out $DATA/tempgribus.grb2
            $WGRIB2 $DATA/tempgribus.grb2 -import_ieee urma2p5.${PDYm1}.maxrh_bg.dat -set_date ${PDYm1}18 -set_var MAXRH -set_ftime '12 hour fcst' -undefine_val 0 -grib_out $DATA/$run.${PDYm1}.maxRH.grb2
            cpfs $DATA/$run.${PDYm1}.maxrh_bg.dat $DATA/$run.${PDYm1}.maxrh_bg.dat
	    cpfs $DATA/$run.${PDYm1}.maxRH.grb2 $COMOUTpost_rtma3d/rtma3d.maxRH.grib2
	else
	    echo "WARNING: CONUS Min RH background no available from main program!"
	fi
    fi
fi
