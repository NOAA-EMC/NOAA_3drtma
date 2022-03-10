#!/bin/ksh

#   DOCBLOCK
#
# Script Name: urma_mintbg.sh
# Author: Steven Levine
# Abstract: Read in previous 25 URMA background/analysis files, use those
# to compute minT background for 08Z URMA
# History Log:
#    9/2015: Initial write for WCOSS
#    11/2016: Generalized to some extent to account for westward expanded
#             CONUS domain.  Grids used are now user specified,
#             but there is a default of w-extended CONUS, AK (hres), HI and PR grids
#    02/2017: Fixed typo when grabbing CONUS files
##
# Usage:
#  Parameters: None
#  Input files:
#    Analysis (2dvaranl) and guess (2dvarges) files from each of the last 25 URMA 
#    cycles.  Exact file names depend on the grids specified by the user.  They are 
#    specified in variables opsfile and gesfile
#
#  Output files:
#    urma2p5.${PDY}.minT.bin, urma2p5.${PDY}.minT.grb2
#    akurma.${PDY}.minT.bin, akurma.${PDY}.minT.grb2
#    hiurma.${PDY}.minT.bin, hiurma.${PDY}.minT.grb2
#    prurma.${PDY}.minT.bin, prurma.${PDY}.minT.grb2
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
#    NOTE 2: A minimum of one grid for each domain (CONUS/HI/PR/AK) may be specified in $1.  Examples of grid combinations 
#           not allowed include: conus & cohreswexp, alaska & akhres.  This is to prevent file name conflicts.
#    NOTE 3: that if no grid names are provided in $1, script will default to cohreswexp, ahres, prico, and hawaii
#           which are the current grids in operation.
#
#  DOCBLOCK

set -x

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

#generate namelist input file, make sure that no grid in same domain is repeated twice
Tur=no; Tak=no; Tpr=no; Thi=no; Tgu=no
echo "&gridsinfo" > gridsinfo_input
echo "     grids=${num}" >> gridsinfo_input
nn=0
while [[ $nn -lt $num ]] ; do
    let nnp1="nn+1"
    dname=${gridsarray[$nn]}
    echo "     gridnames($nnp1)=${dname}," >> gridsinfo_input
    if [[ $dname = "cohreswexp" || $dname = "cohresext" || $dname = "cohres" || $dname = "urma2p5" || $dname = "hrrr" ]] ; then
	if [[ $Tur = yes ]] ; then
	    err_exit echo "MULTIPLE GRIDS FROM URMA2P5 DOMAIN!"
	else
	    run="urma2p5"
	    Tur=yes
	fi
    else
	err_exit "Domain name number ${nn} is invalid: $dname."
    fi

    #now find and run wgrib2 on all the relevant files for that grid
    CYCLE="${PDY}1800"
    CYCLE_STOP="${PDYm1}1800"

    while [[ $CYCLE -ge $CYCLE_STOP ]]; do

	YYYYMMDD=`echo $CYCLE | cut -c 1-8`
	HH=`echo $CYCLE | cut -c 9-10`

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
	    opnew=${run}_anl_valid${HH}.bin
	    gesnew=${run}_ges_valid${HH}.bin
	    if [[ -s $gesnew ]] ; then
		opnew=${run}_anl_valid${HH}_prevday.bin
		gesnew=${run}_ges_valid${HH}_prevday.bin
	    fi
            $WGRIB2 $gesfile -match ":TMP:2 m above ground:" -ieee $gesnew
            $WGRIB2 $opsfile -match ":TMP:2 m above ground:" -ieee $opnew
	else
	    echo "WARNING: ${RUN} file unavailable for ${CYCLE}!"
	fi

	#get template grid for 20Z

	if [ ${HH} -eq 20 ] ; then
	    if [[ $run == "urma2p5" ]] ; then
		cpfs $gesfile gesfileus.grb2
	    fi
	fi

	CYCLE=`$MDATE -60 $CYCLE`
    done
    let nn="$nn+1"
done
echo "/" >> gridsinfo_input

#in blend_input: 1=use bgs only, 2=use anls only, 3=use both
cat <<EOF > blend_input
&blendinput
bnum=3
/
EOF

. prep_step

export FORT71=urma2p5.${PDYm1}.mint_diag_bg.dat
export FORT72=urma2p5.${PDYm1}.mint_diag_anl.dat

cpfs $FIXminmax/aktz.bin .
cpfs $FIXminmax/conus2p5exttz.bin .
cpfs $FIXminmax/conus2p5tz.bin .
cpfs $FIXminmax/conus2p5tz_ndfdonly.bin .

export pgm=rtma3d_mintgb
startmsg
$EXECrtma3d/rtma3d_mintbg >> $pgmout 2> errfile
export err=$?; err_chk
cat $pgmout

if [[ $Tur = yes ]]; then
if [ -s $DATA/mint_urma2p5_bg.bin ] ; then
    $WGRIB2 $DATA/gesfileus.grb2 -match ":TMP:" -grib_out $DATA/tempgribus.grb2
    $WGRIB2 $DATA/tempgribus.grb2 -import_ieee $DATA/mint_urma2p5_bg.bin -set_date "${PDY}20" -set_var TMIN -set_ftime "12 hour fcst" -undefine_val 0 -grib_out $DATA/urma2p5.${PDYm1}.minT.grb2
    cpfs $DATA/mint_urma2p5_bg.bin $DATA/urma2p5.${PDYm1}.minT.bin
    cpfs $DATA/urma2p5.${PDYm1}.minT.grb2 $COMOUTpost_rtma3d/rtma3d.minT.grib2
else
    err_exit "URMA2P5 background was not generated or copied properly!"
fi
fi

