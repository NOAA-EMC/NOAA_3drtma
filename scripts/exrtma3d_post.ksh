#!/bin/ksh 
set -x

#if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then

export OMP_NUM_THREADS=1

# Make sure we are using GMT time zone for time computations
export TZ="GMT"

#------------------------------------------------------------------#
#
# set up of Analysis Time 
#
# ANLS_TIME=${PDY}' '${cyc}
ANLS_TIME=${ANLS_TIME:-"${PDY} ${cyc}"}            # YYYYMMDD HH
echo $PDY $cyc 
# cyc_intvl="60 minutes"       # <-- cycle interval (minute)
FCST_TIME="00"                 # <-- forecast time (hour) to provide fgs for rtma

# For RTMA there is no forecast, so START_TIME is ANLS_TIME
START_TIME=$ANLS_TIME

# Make sure START_TIME is defined and in the correct format (YYYYMMDD HH)
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
  START_TIME=`${DATE} -d "${START_TIME} ${FCST_TIME} minutes"`
#fi

ANLS_CYC_TIME=`${DATE} --date="${START_TIME}  0 hour " +"%Y%m%d%H%M"`
FCST_INI_TIME=`${DATE} --date="${START_TIME} -${FCST_TIME} hour " +"%Y%m%d%H%M"`
export WGRIB2=/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.8_wmo/bin/wgrib2
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

HH_fcstinit=`${ECHO} ${FCST_INI_TIME} | cut -c9-10 `
#------------------------------------------------------------------#

export DATAHOME=$DATA
export CORE="RAPRRTMA"
export MODELNAME="RAPR"
export SUBMODELNAME="RTMA"
export fileNameFlux="DUMMY"
export fileNameFlat="DUMMY"
export DATAWRFHOME=${COMOUT:-"$COMIN"}
if [ $dom == "conus" ]; then
DATAWRFFILE="${RUN}.t${cyc}z.wrf_inout.nc"
else
DATAWRFFILE="${RUN}ak.t${cyc}z.wrf_inout.nc"
fi

##########################################################################

# Check to make sure the post executable exists
#if [ ! -x ${EXECrtma3d}/${exefile_name_post} ]; then
#  ${ECHO} "ERROR: ${EXECrtma3d}/${exefile_name_post} does not exist, or is not executable"
#  exit 1
#fi

# Check to make sure that the DATAHOME exists
if [ ! ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, \$DATAHOME, is not defined"
  exit 1
fi

# Check to make sure that the DATAWRFHOME is defined
if [ ! ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: DATAWRFHOME, \$DATAWRFHOME, is not defined."
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! -d ${DATAWRFHOME} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME, does not exist."
  exit 1
fi

if [ ! -f ${DATAWRFHOME}/${DATAWRFFILE} ]; then
  ${ECHO} "ERROR: $DATAWRFHOME/${DATAWRFFILE}, does not exist."
  exit 1
fi



# Print out times
${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`
${ECHO} "    FCST_TIME = ${FCST_TIME}"

export STARTTIME_STR=`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`

# Set up the work directory and cd into it
workdir=${DATAHOME}/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

#
# Set up some constants for UPP namlist itag
#

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`

#
# Set namelist options
#
i_cch260_rtma=0            # 0: using cloud base calcualted with origianl Ferrier's algorithm (see CLDRAD.f in UPP)
                           # 1: using cloud base calcualted with GSL legacy cloud ceiling algorithm (see CLDRAD.f in UPP)
                           #    choose 1 if user prefers CCH 260 to be same as calcualted with 
                           #    original UPP code used in 3DRTMAv1.
                           #    Important Note: CCH 260 is changed to height ASL (as same as CCH 408), not AGL.
                           #                    in original UPP of 3DRTMAv1, CCH 260 is height AGL.

cat > itag <<EOF
&model_inputs
fileName='${DATAWRFHOME}/${DATAWRFFILE}'
IOFORM='netcdf'
grib='grib2'
DateStr='${timestr2}'
MODELNAME='RAPR'
SUBMODELNAME='RTMA'
/
&NAMPGB
KPO=47,PO=2.,5.,7.,10.,20.,30.,50.,70.,75.,100.,125.,150.,175.,200.,225.,250.,275.,300.,325.,350.,375.,400.,425.,450.,475.,500.,525.,550.,575.,600.,625.,650.,675.,700.,725.,750.,775.,800.,825.,850.,875.,900.,925.,950.,975.,1000.,1013.2,
synthetic_cfr=.true., capecin_2m=.true., i_cch260_rtma=${i_cch260_rtma},
/
EOF

${RM} -f fort.*
${RM} -f params_grib2_tbl_new postxconfig-NT.txt eta_micro_lookup.dat
${RM} -f WRF???.GrbF??


  CP_LN="${LN} -sf"
#link/copy parameter files
${CP_LN} ${PARMupp}/params_grib2_tbl_new params_grib2_tbl_new
${CP_LN} ${PARMupp}/postxconfig-NT-3drtma.txt postxconfig-NT.txt
${CP_LN} ${PARMupp}/rap_micro_lookup.dat ./eta_micro_lookup.dat
#${CP_LN} ${FIXupp}/rap_micro_lookup.dat ./eta_micro_lookup.dat
#${CP_LN} ${FIXcrtm}/* .
manual_link="false"
if [ $manual_link == "true" ]; then 
ln -sf ${FIXcrtm}/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
ln -sf ${FIXcrtm}/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
ln -sf ${FIXcrtm}/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
ln -sf ${FIXcrtm}/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
#ln -sf ${FIXhrrr}/hrrr_imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin

ln -sf ${FIXcrtm}/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
ln -sf ${FIXcrtm}/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
ln -sf ${FIXcrtm}/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.TauCoeff.bin v.seviri_m10.TauCoeff.bin
#ln -sf ${FIXhrrr}/hrrr_imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin

ln -sf ${FIXcrtm}/NPOESS.IRice.EmisCoeff.bin NPOESS.IRice.EmisCoeff.bin
ln -sf ${FIXcrtm}/NPOESS.IRland.EmisCoeff.bin NPOESS.IRland.EmisCoeff.bin
ln -sf ${FIXcrtm}/NPOESS.IRsnow.EmisCoeff.bin NPOESS.IRsnow.EmisCoeff.bin
ln -sf ${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin Nalli.IRwater.EmisCoeff.bin

ln -sf ${FIXcrtm}/FASTEM6.MWwater.EmisCoeff.bin FASTEM6.MWwater.EmisCoeff.bin

ln -sf ${FIXcrtm}/CloudCoeff.bin CloudCoeff.bin
ln -sf ${FIXcrtm}/AerosolCoeff.bin AerosolCoeff.bin
#ln -sf ${FIXcrtm}/Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin
ln -sf ${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin EmisCoeff.bin
ln -sf ${FIXcrtm}/abi_gr.SpcCoeff.bin abi_gr.SpcCoeff.bin
ln -sf ${FIXcrtm}/abi_g16.SpcCoeff.bin abi_g16.SpcCoeff.bin
ln -sf ${FIXcrtm}/abi_g17.SpcCoeff.bin abi_g16.SpcCoeff.bin
ln -sf ${FIXcrtm}/ahi_himawari8.SpcCoeff.bin ahi_himawari8.SpcCoeff.bin
ln -sf ${FIXcrtm}/abi_gr.TauCoeff.bin abi_gr.TauCoeff.bin
ln -sf ${FIXcrtm}/abi_g16.TauCoeff.bin abi_g16.TauCoeff.bin
ln -sf ${FIXcrtm}/abi_g17.TauCoeff.bin abi_g17.TauCoeff.bin
ln -sf ${FIXcrtm}/zssmis_f16.TauCoeff.bin zssmis_f16.TauCoeff.bin
ln -sf ${FIXcrtm}/zssmis_f17.TauCoeff.bin zssmis_f17.TauCoeff.bin
ln -sf ${FIXcrtm}/zssmis_f18.TauCoeff.bin zssmis_f18.TauCoeff.bin
ln -sf ${FIXcrtm}/zssmis_f19.TauCoeff.bin zssmis_f19.TauCoeff.bin
ln -sf ${FIXcrtm}/zssmis_f20.TauCoeff.bin zssmis_f20.TauCoeff.bin
fi

# get crtm fix files
for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" \
    "ahi_himawari8" ; do
    ln -s "${FIXcrtm}/${what}.TauCoeff.bin" .
    ln -s "${FIXcrtm}/${what}.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "${FIXcrtm}/${what}Coeff.bin" .
done

for what in  ${FIXcrtm}/*Emis* ; do
   ln -s $what .
done

#=============================================================================#
#
# Run unipost
#
export pgm=${NET}_upp
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin Uni-Post step for 3DRTMA GSI Analysis"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"


#copy executable to running directory
#${CP} ${EXECrtma3d}/${exefile_name_post} ./rtma3d_wrfpost
export APRUN="mpiexec -l -n 128 -ppn 128"
runline="${APRUN} ${EXECrtma3d}/${pgm}"
$runline < itag > ${pgmout} 2>errfile
export err=$? ; err_chk

if [ ${err} -ne 0 ]; then
  ${ECHO} "rtma3d_wrfpost crashed!  Exit status=${err}"
  exit ${err}
fi

# Linking GrbF{HH} to GrbF00 (esp. for firstguess which is from WRF forecast)
GrbFiles=`ls WRF???.GrbF??`
for i in ${GrbFiles}
do
  i_fname=`echo "$i" | cut -d '.' -f 1`
  i_extname=`echo "$i" | cut -d '.' -f 2`
  if [[ "${i_extname}" != "GrbF${FCST_TIME}" ]]
  then
    ln -sf $i    ./${i_fname}.GrbF${FCST_TIME}
  fi
done

# Append entire wrftwo to wrfprs
${CAT} ${workdir}/WRFPRS.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFPRS.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfsubhprs.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfsubhnat.grib2


# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfsubhprs.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhprs.grib2 is missing"
  exit 1
fi
#if [ ! -s "${workdir}/wrfsubhspl.grib2" ]; then
#  ${ECHO} "unipost crashed! wrfsubhspl.grib2 is missing"
#  exit 1
#fi
if [ ! -s "${workdir}/wrfsubhnat.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhnat.grib2 is missing"
  exit 1
fi

# transfer the output grib2 files to $COMOUTpost_rtma3d
# add gust,vis, and howv (wave height) to the prslev and natlev files
# change name from surface gust to 10-m gust

# Note: NET should be RUN - AMG
# Change analyzed gust from GUST:surface to GUST:10 m above ground
# Change analyzed visibility from VIS:surface to VIS:2 m above ground
# Annette - need to copy, can't read file from COMOUT
if [ $dom == "conus" ]; then
  ${WGRIB2} -V ${COMOUT}/${RUN}.t${cyc}z.anl.gust.grib2 -set_lev "10 m above ground" -grib tmpgust.grib2
  ${WGRIB2} -V ${COMOUT}/${RUN}.t${cyc}z.anl.vis.grib2 -set_lev "2 m above ground" -grib tmpvis.grib2
else
  ${WGRIB2} -V ${COMOUT}/${RUN}ak.t${cyc}z.anl.gust.grib2 -set_lev "10 m above ground" -grib tmpgust.grib2
  ${WGRIB2} -V ${COMOUT}/${RUN}ak.t${cyc}z.anl.vis.grib2 -set_lev "2 m above ground" -grib tmpvis.grib2
fi
# Add analyzed visibility, gust, and wave height to wrfsubhprs and wrfsubhnat grib2 files
# No analyzed wave height is added to the alaska grib2 files
if [ "${dom}" == "conus" ]; then
  cat tmpvis.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.howv.grib2 >> ${workdir}/wrfsubhprs.grib2
  cat tmpvis.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl.howv.grib2 >> ${workdir}/wrfsubhnat.grib2
else
  cat tmpvis.grib2 tmpgust.grib2 >> ${workdir}/wrfsubhprs.grib2
  cat tmpvis.grib2 tmpgust.grib2 >> ${workdir}/wrfsubhnat.grib2
fi

${WGRIB2} ${workdir}/wrfsubhprs.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhprs.grib2
${WGRIB2} ${workdir}/wrfsubhnat.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD}.wrfsubhnat.grib2
${WGRIB2} ${workdir}/wrfsubhprs.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD}.anl_prslev.grib2
${WGRIB2} ${workdir}/wrfsubhnat.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD}.anl_natlev.grib2
# Create index file
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.anl_prslev.grib2 -s > ${RUN}.t${cyc}z.anl_prslev.grib2.idx
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.anl_natlev.grib2 -s > ${RUN}.t${cyc}z.anl_natlev.grib2.idx
cp ${RUN}.t${cyc}z.anl_prslev.grib2.idx $COMOUT/
cp ${RUN}.t${cyc}z.anl_natlev.grib2.idx $COMOUT/



#================================================================================#
  ${RM} -f  ${workdir}/wrfsubh???.grib2
  ${RM} -f  ${workdir}/WRF???.GrbF??

#================================================================================#

${ECHO} "unipost completed at `${DATE}`"

fi



exit 0
