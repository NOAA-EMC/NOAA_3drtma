#!/bin/ksh

date

# set -x
ulimit -S -s unlimited
#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
# ANLS_TIME=${PDY}' '${cyc}
ANLS_TIME=${ANLS_TIME:-"${PDY} ${cyc}"}            # YYYYMMDD HH
echo $PDY $cyc $subcyc
# cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"                 # <-- forecast time (hour) to provide fgs for rtma

# For RTMA there is no forecast, so START_TIME is ANLS_TIME
START_TIME=$ANLS_TIME

# Make sure START_TIME is defined and in the correct format (YYYYMMDD HH)
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 111
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 111
  fi
  START_TIME=`${DATE} -d "${START_TIME} ${subcyc} minutes"`
fi

ANLS_CYC_TIME=`${DATE} --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`
FCST_INI_TIME=`${DATE} --date="${START_TIME} -${FCST_TIME} hour " +"%Y%m%d%H%M"`

# Compute date & time components for the analysis time
YYYYMMDDHHMU=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`
mm=`${DATE} +"%M" -d "${START_TIME}"`
JJJ=`${DATE} +"%j" -d "${START_TIME}"`
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}
#
#------------------------------------------------------------------#

export PLTDIR=$DATA
# export myg2tool="/home/Gang.Zhao/local/grib/g2ctl"
export myg2tool="${UTILrtma3d_dev}/plot/grads/g2ctl"

if [ ! -d ${PLTDIR} ] ; then
  echo " running/plotting directory is not found. Abort!"
  exit 111
fi
cd $PLTDIR

#
# grib2 to Grads
#
#   linking grib2 data file for UPP post-processed firstguess
if [ ! "${PROD_HEAD}" ] ; then
  FGS_NAT_FNAME="${RUN}.t{cyc}z.fgs.wrfnat_hrconus_00.grib2"
  FGS_PRS_FNAME="${RUN}.t{cyc}z.fgs.wrfprs_hrconus_00.grib2"
else
  FGS_NAT_FNAME="${PROD_HEAD}.fgs.wrfnat_hrconus_00.grib2"
  FGS_PRS_FNAME="${PROD_HEAD}.fgs.wrfprs_hrconus_00.grib2"
fi
rm -f ./fgs_nat.grib2 ./fgs_prs.grib2
ln -sf ${COMOUTpost_rtma3d}/${FGS_NAT_FNAME}  ./fgs_nat.grib2
ln -sf ${COMOUTpost_rtma3d}/${FGS_PRS_FNAME}  ./fgs_prs.grib2

#   linking grib2 data file for UPP post-processed analysis
if [ ! "${PROD_HEAD}" ] ; then
  ANL_NAT_FNAME="${RUN}.t{cyc}z.wrfnat_hrconus_00.grib2"
  ANL_PRS_FNAME="${RUN}.t{cyc}z.wrfprs_hrconus_00.grib2"
else
  ANL_NAT_FNAME="${PROD_HEAD}.wrfnat_hrconus_00.grib2"
  ANL_PRS_FNAME="${PROD_HEAD}.wrfprs_hrconus_00.grib2"
fi
rm -f ./anl_nat.grib2 ./anl_prs.grib2
ln -sf ${COMOUTpost_rtma3d}/${ANL_NAT_FNAME}  ./anl_nat.grib2
ln -sf ${COMOUTpost_rtma3d}/${ANL_PRS_FNAME}  ./anl_prs.grib2

#
# g2ctl
#
fnames="fgs_nat fgs_prs anl_nat anl_prs"
for fn in $fnames
do
  g2file="${fn}.grib2"
  ctlfile="${fn}.ctl"
  idxfile="${g2file}.idx"
  echo "g2ctl for $g2file"
  rm -f $ctlfile $idxfile
  $myg2tool/g2ctl.0.1.4 -0 $g2file > $ctlfile
#  $myg2tool/g2ctl -0 $g2file > $ctlfile
  gribmap -0 -i $ctlfile
  if [ ! -f $ctlfile ] || [ ! -f $idxfile ] ; then
    echo "g2ctl step failed. Abort!"
    exit 111
  fi
done

#
# plot with GrADS
#
set -x
export gmf_fhead="Fgs_Anl_Inc_${time_str}"
export adate="${YYYY}${MM}${DD}${HH}${mm}"

ulimit -S -s unlimited

rm -f ./plt_fai.gs      ./variables_list_for_plot.txt      ./panels.gsf     ./cbarn.gs
cp -p ${UTILrtma3d_dev}/plot/grads/script/plt_fai.tmplt.gs			./plt_fai.gs
cp -p ${UTILrtma3d_dev}/plot/grads/script/variables_list_for_plot.txt	        ./variables_list_for_plot.txt
cp -p ${UTILrtma3d_dev}/plot/grads/script/panels.gsf                            ./panels.gsf
cp -p ${UTILrtma3d_dev}/plot/grads/script/cbarn.gs                              ./cbarn.gs
sed -i 's/GMFNAME/'${gmf_fhead}'/g' ./plt_fai.gs
sed -i 's/Y4M2D2H2M2/'${adate}'/g'  ./plt_fai.gs

rm -f ./${gmf_fhead}_*.gmf ./${gmf_fhead}_*.ps  ./${gmf_fhead}_*.png ./${gmf_fhead}_*.pdf

#
. prep_step
startmsg

grads -lbcx ./plt_fai.gs

gmf_fname=`ls ${gmf_fhead}_*.gmf`
nv=0
for i in ${gmf_fname}
do
  let nv=nv+1 
  echo "====> converting format for ${i}. ( # ${nv} )"
  i_fn=`basename ${i} .gmf`
  gxps -c -i ./${i_fn}.gmf -o ./${i_fn}.ps
# gxeps   -i ./${i_fn}.gmf -o ./${i_fn}.eps
  ps2pdf ./${i_fn}.ps
  convert -density 256 -rotate 90 -background "#FFFFFF" -flatten ./${i_fn}.ps ./${i_fn}.png
  if [ -f ./${i_fn}.png ] && [ -f ./${i_fn}.pdf ] ; then
    rm -f ./${i_fn}.ps ./${i_fn}.gmf
    cp -p ./${i_fn}.pdf  ${COMOUTplot_rtma3d}/${PROD_HEAD}.${i_fn}.pdf
    cp -p ./${i_fn}.png  ${COMOUTplot_rtma3d}/${PROD_HEAD}.${i_fn}.png
  else
    echo "picture format conversion crashed. No png or pdf file generated for gmf file ${i}."
    exit 111
  fi
  
done

set +x
date

exit
