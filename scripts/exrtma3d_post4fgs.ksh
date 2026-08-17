#!/bin/ksh 

set -x
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
fi

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

#export DATAWRFHOME=${GESINhrrr_rtma3d:-"$COMIN"}
export DATAWRFHOME=${COMOUT:-"$COMIN"}
if [ $dom == "conus" ]; then
  DATAWRFFILE="${RUN}.t${cyc}z.firstguess.nc"
else
  DATAWRFFILE="${RUN}ak.t${cyc}z.firstguess.nc"
fi
export PROD_HEAD2="${PROD_HEAD}"

##########################################################################


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

export XLFRTEOPTS="unit_vars=yes"
export MP_SHARED_MEMORY=yes
export SPLNUM=46
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`

#${CAT} > itag <<EOF
#${DATAWRFHOME}/${DATAWRFFILE}
#netcdf
#grib2
#${timestr2}
#${CORE}
#${SPLNUM}
#${SPL}

#EOF

#
# Set namelist options
#
# "i_cch260_rtma" can be pre-set in xml file under workflow directory
i_cch260_rtma=${i_cch260_rtma:-0} # 0: using cloud base calcualted with origianl Ferrier's algorithm (see CLDRAD.f in UPP)
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
msg="  begin Uni-Post step for 3DRTMA First Guess"
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

mv WRFPRS.GrbF01.?? WRFPRS.GrbF00
mv WRFTWO.GrbF01.?? WRFTWO.GrbF00
mv WRFNAT.GrbF01.?? WRFNAT.GrbF00

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
${MV}  ${workdir}/WRFPRS.GrbF${FCST_TIME}.new ${workdir}/wrfsubhprs_fgs.grib2

# Append entire wrftwo to wrfnat
${CAT} ${workdir}/WRFNAT.GrbF${FCST_TIME}     ${workdir}/WRFTWO.GrbF${FCST_TIME} > ${workdir}/WRFNAT.GrbF${FCST_TIME}.new
${MV}  ${workdir}/WRFNAT.GrbF${FCST_TIME}.new ${workdir}/wrfsubhnat_fgs.grib2

#${CP}  ${workdir}/WRFTWO.GrbF${FCST_TIME}     ${workdir}/wrfsubhspl_fgs.grib2

# Check to make sure all Post  output files were produced
if [ ! -s "${workdir}/wrfsubhprs_fgs.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhprs.grib2 is missing"
  exit 1
fi
#if [ ! -s "${workdir}/wrfsubhspl_fgs.grib2" ]; then
#  ${ECHO} "unipost crashed! wrfsubhspl.grib2 s missing"
#  exit 1
#fi
if [ ! -s "${workdir}/wrfsubhnat_fgs.grib2" ]; then
  ${ECHO} "unipost crashed! wrfsubhnat.grib2 is missing"
  exit 1
fi

# transfer the output grib2 files to $COMOUTpost_rtma3d
# add gust, visibility, and howv (wave height) to the prslev and natlev files
# change name from surface gust to 10-m gust

# Note: NET should be RUN - AMG
# Change first guess gust from GUST:surface to GUST:10 m above ground
# Change first guess visibility from VIS:surface to VIS:2 m above ground
# Annette - need to copy, can't read file from COMOUT
if [ $dom == "conus" ]; then
  ${WGRIB2} -V ${COMOUT}/${RUN}.t${cyc}z.fgs.gust.grib2 -set_lev "10 m above ground" -grib tmpgust.grib2
  ${WGRIB2} -V ${COMOUT}/${RUN}.t${cyc}z.fgs.vis.grib2 -set_lev "2 m above ground" -grib tmpvis.grib2
else
  ${WGRIB2} -V ${COMOUT}/${RUN}ak.t${cyc}z.fgs.gust.grib2 -set_lev "10 m above ground" -grib tmpgust.grib2
  ${WGRIB2} -V ${COMOUT}/${RUN}ak.t${cyc}z.fgs.vis.grib2 -set_lev "2 m above ground" -grib tmpvis.grib2
fi
# Add first guess visibility, gust, and wave height to wrfsubhprs and wrfsubhnat grib2 files
# No first guess wave height is added to the alaska grib2 files
if [ "${dom}" == "conus" ]; then
  cat tmpvis.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.fgs.howv.grib2 >> ${workdir}/wrfsubhprs_fgs.grib2
  cat tmpvis.grib2 tmpgust.grib2 ${COMOUT}/${RUN}.t${cyc}z.fgs.howv.grib2 >> ${workdir}/wrfsubhnat_fgs.grib2
else
  cat tmpvis.grib2 tmpgust.grib2 >> ${workdir}/wrfsubhprs_fgs.grib2
  cat tmpvis.grib2 tmpgust.grib2 >> ${workdir}/wrfsubhnat_fgs.grib2
fi

${WGRIB2} ${workdir}/wrfsubhprs_fgs.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD2}.wrfsubhprs_fgs.grib2
${WGRIB2} ${workdir}/wrfsubhnat_fgs.grib2 -set center 7 -grib ${COMOUTpost_rtma3d}/${PROD_HEAD2}.wrfsubhnat_fgs.grib2
${WGRIB2} ${workdir}/wrfsubhprs_fgs.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD2}.fgs_prslev.grib2
${WGRIB2} ${workdir}/wrfsubhnat_fgs.grib2 -set center 7 -grib ${COMOUT}/${PROD_HEAD2}.fgs_natlev.grib2
# Create index file
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.fgs_prslev.grib2 -s > ${RUN}.t${cyc}z.fgs_prslev.grib2.idx
wgrib2 ${COMOUT}/${RUN}.t${cyc}z.fgs_natlev.grib2 -s > ${RUN}.t${cyc}z.fgs_natlev.grib2.idx
cp ${RUN}.t${cyc}z.fgs_prslev.grib2.idx $COMOUT/
cp ${RUN}.t${cyc}z.fgs_natlev.grib2.idx $COMOUT/


# parallel processing

  wgrib2 ${COMIN}/${RUN}.t${cyc}z.fgs_prslev.grib2 -not_if ":GUST:surface:|:VIS:2 m above ground" -grib ${RUN}.t${cyc}z.fgs_prslev.grib2_no_sfcgustvis
# Note: NET should be RUN - AMG 
  if [ "${dom}" == "conus" ]; then
    cat ${COMOUT}/${RUN}.t${cyc}z.fgs.howv.grib2 >> ${RUN}.t${cyc}z.fgs_prslev.grib2_no_sfcgustvis 
  fi
  infile_prslev=${workdir}/${RUN}.t${cyc}z.fgs_prslev.grib2_no_sfcgustvis
# infile_prslev=${DATA}/${RUN}.t${cyc}z.fgs_prslev.grib2
##infile_prslev=${COMIN}/${RUN}.t${cyc}z.fgs_prslev.grib2
  wgrib2 ${infile_prslev} > prslev.txt

  if [ "${dom}" == "conus" ]; then
    domain="conus"
  else
    domain="alaska"
  fi

# Create parm files for subsetting on the fly 
# 48 subpieces for CONUS or Alaska prslev and natlev files

  sed -n -e '1,15p' prslev.txt > ${domain}_prslev_1.txt
  sed -n -e '16,31p' prslev.txt > ${domain}_prslev_2.txt
  sed -n -e '32,48p' prslev.txt > ${domain}_prslev_3.txt
  sed -n -e '49,64p' prslev.txt > ${domain}_prslev_4.txt
  sed -n -e '65,80p' prslev.txt > ${domain}_prslev_5.txt
  sed -n -e '81,96p' prslev.txt > ${domain}_prslev_6.txt
  sed -n -e '97,112p' prslev.txt > ${domain}_prslev_7.txt
  sed -n -e '113,128p' prslev.txt > ${domain}_prslev_8.txt
  sed -n -e '129,144p' prslev.txt > ${domain}_prslev_9.txt
  sed -n -e '145,160p' prslev.txt > ${domain}_prslev_10.txt
  sed -n -e '161,176p' prslev.txt > ${domain}_prslev_11.txt
  sed -n -e '177,192p' prslev.txt > ${domain}_prslev_12.txt
  sed -n -e '193,208p' prslev.txt > ${domain}_prslev_13.txt
  sed -n -e '209,224p' prslev.txt > ${domain}_prslev_14.txt
  sed -n -e '225,240p' prslev.txt > ${domain}_prslev_15.txt
  sed -n -e '241,256p' prslev.txt > ${domain}_prslev_16.txt
  sed -n -e '257,272p' prslev.txt > ${domain}_prslev_17.txt
  sed -n -e '273,288p' prslev.txt > ${domain}_prslev_18.txt
  sed -n -e '289,304p' prslev.txt > ${domain}_prslev_19.txt
  sed -n -e '305,320p' prslev.txt > ${domain}_prslev_20.txt
  sed -n -e '321,336p' prslev.txt > ${domain}_prslev_21.txt
  sed -n -e '337,352p' prslev.txt > ${domain}_prslev_22.txt
  sed -n -e '353,368p' prslev.txt > ${domain}_prslev_23.txt
  sed -n -e '369,384p' prslev.txt > ${domain}_prslev_24.txt
  sed -n -e '385,400p' prslev.txt > ${domain}_prslev_25.txt
  sed -n -e '401,416p' prslev.txt > ${domain}_prslev_26.txt
  sed -n -e '417,432p' prslev.txt > ${domain}_prslev_27.txt
  sed -n -e '433,448p' prslev.txt > ${domain}_prslev_28.txt
  sed -n -e '449,463p' prslev.txt > ${domain}_prslev_29.txt
  sed -n -e '464,479p' prslev.txt > ${domain}_prslev_30.txt
  sed -n -e '480,495p' prslev.txt > ${domain}_prslev_31.txt
  sed -n -e '496,512p' prslev.txt > ${domain}_prslev_32.txt
  sed -n -e '513,528p' prslev.txt > ${domain}_prslev_33.txt
  sed -n -e '529,544p' prslev.txt > ${domain}_prslev_34.txt
  sed -n -e '545,560p' prslev.txt > ${domain}_prslev_35.txt
  sed -n -e '561,576p' prslev.txt > ${domain}_prslev_36.txt
  sed -n -e '577,593p' prslev.txt > ${domain}_prslev_37.txt
  sed -n -e '594,609p' prslev.txt > ${domain}_prslev_38.txt
  sed -n -e '610,625p' prslev.txt > ${domain}_prslev_39.txt
  sed -n -e '626,641p' prslev.txt > ${domain}_prslev_40.txt
  sed -n -e '642,657p' prslev.txt > ${domain}_prslev_41.txt
  sed -n -e '658,673p' prslev.txt > ${domain}_prslev_42.txt
  sed -n -e '674,689p' prslev.txt > ${domain}_prslev_43.txt
  sed -n -e '690,705p' prslev.txt > ${domain}_prslev_44.txt
  sed -n -e '706,721p' prslev.txt > ${domain}_prslev_45.txt
  sed -n -e '722,737p' prslev.txt > ${domain}_prslev_46.txt
  sed -n -e '738,753p' prslev.txt > ${domain}_prslev_47.txt
  sed -n -e '754,$p' prslev.txt > ${domain}_prslev_48.txt

  tasks=(48)
# domain=conus
  count=0
# for leveltype in prslev
  for leveltype in prslev
  do
    for task in $(seq ${tasks[count]})
    do
      if [ "${leveltype}" = "prslev" ]; then
        infile=${infile_prslev}
      else
        infile=${infile_natlev}
      fi
      mkdir -p ${workdir}/prdgen_${domain}_${leveltype}_${task}
      echo "$USHrtma3d//${RUN}_prdgen_subpiece.sh $cyc $task $domain ${infile} ${workdir} ${COMOUT} ${leveltype} " >> ${workdir}/poescript
    done
    count=$count+1
  done

  chmod 775 ${workdir}/poescript

# Execute the script
  export CMDFILE=${workdir}/poescript
  mpiexec -np 48 --cpu-bind core cfp $CMDFILE >>$pgmout 2>errfile
  export err=$?; err_chk

# reassemble the output grids
  tasks=(48)
# domain=conus
  count=0
# for leveltype in prslev
  for leveltype in prslev
  do
    for task in $(seq ${tasks[count]})
    do
      cat ${workdir}/prdgen_${domain}_${leveltype}_${task}/${domain}_${leveltype}_${task}.grib2 >> ${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2
    done
    count=$count+1

    wgrib2 ${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2 -s > ${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2.idx
    cpreq ${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2
    cpreq ${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.fgs_${leveltype}_ndfd.grib2.idx
  done

#================================================================================#
  ${RM} -f  ${workdir}/wrfsubh???_fgs.grib2
  ${RM} -f  ${workdir}/WRF???.GrbF??

#================================================================================#

${ECHO} "unipost completed at `${DATE}`"

exit 0
