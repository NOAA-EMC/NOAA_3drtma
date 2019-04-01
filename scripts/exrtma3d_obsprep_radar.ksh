#!/bin/ksh
############################################################################

set -x

# make sure executable exists
if [ ! -f ${EXECrtma3d}/${exefile_name_radar} ] ; then
  ${ECHO} "ERROR: mosaic radar obs prcoessing executable '${EXECrtma3d}/${exefile_name_radar}' does not exist!"
  exit 1
fi

# working directory
workdir=${DATA}
cd ${workdir}

# export MV2_ON_DEMAND_THRESHOLD=256    # if load module mvapich2 ?

export MOSAICTILENUM=${MOSAICTILENUM:-"1"}  # Tile numbers of mosaic data
                                            # 1: 1 single tile for each level covering whole conus (grib2)
                                            # 4/8/81: more sub-tiles for each level coving whole domain (bin or netcdf) 

numtiles=${MOSAICTILENUM}

mm=$subcyc
subhtime=$subcyc
${ECHO} $PDY $cyc $mm
# START_TIME="${PDY}${cyc}"      # YYYYMMDDHH
  START_TIME=${START_TIME:-"{PDY} ${cyc}"}      # YYYYMMDD HH
# START_TIME="${PDY} ${cyc} ${subcyc} minutes"  # YYYYMMDD HH MN 

${ECHO} "${START_TIME}"
echo `echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'`
 if [ `echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
   START_TIME=`echo "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
 elif [ ! "`echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
   echo "FATAL ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
   err_exit 1
 fi
 START_TIME=`${DATE} -d "${START_TIME} ${subhtime} minutes"`
echo $START_TIME

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

typeset -Z2 mm mmp1 mmp2 mmp3              # <<-- "-Z2" only work for K-Shell
mm=`${DATE} +"%M" -d "${START_TIME}"`
mmp1=$((${mm}+1))
mmp2=$((${mm}+2))
mmp3=$((${mm}+3))
# mm=`printf "%2.2i\n" $mm`                    # 0-padding with right-justification
# mmp1=`printf "%2.2i\n" $mmp1`
# mmp2=`printf "%2.2i\n" $mmp2`
# mmp3=`printf "%2.2i\n" $mmp3`

ymd=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
ymdh=${YYYYMMDDHH}
hh=$HH

# BUFR Table includingthe description for HREF
${CP} -p ${FIXgsi}/prepobs_prep_RAP.bufrtable   ./prepobs_prep.bufrtable
# WPS GEO_GRID Data
${LN} -s ${FIXwps}/hrrr_geo_em.d01.nc           ./geo_em.d01.nc 

# find NSSL grib2 mosaic files
COM_MOSAIC_GRIB2=${COMINmosaic}/mrms.t${HH}z/conus

# gb2_time="${YYYY}${MM}${DD}-${HH}"
gb2_time="${PDY}-${cyc}"
obsname="MRMS_MergedReflectivityQC"

# test if grib2.gz file, then "gzip -d" the gz file first
data_timewindow="${mm} ${mmp1} ${mmp2}"
for m2 in ${data_timewindow}
do
  gb2gz_fname="${gb2_time}${m2}??.${obsname}_*_${gb2_time}${m2}??.grib2.gz"
  numgz=`ls ${COM_MOSAIC_GRIB2}/${gb2gz_fname} | wc -l`
  if [ $numgz -ge 1 ] ; then
    ${ECHO} " gunzip the compressed the MRMS grib2 data for ${HH}z ${mm} minute"
    gzip -d ${COM_MOSAIC_GRIB2}/${gb2gz_fname}
  fi
done

numgrib2_00=`ls ${COM_MOSAIC_GRIB2}/${gb2_time}${mm}??.${obsname}_*_${gb2_time}${mm}??.grib2 | wc -l`
numgrib2_01=`ls ${COM_MOSAIC_GRIB2}/${gb2_time}${mmp1}??.${obsname}_*_${gb2_time}${mmp1}??.grib2 | wc -l`
numgrib2_02=`ls ${COM_MOSAIC_GRIB2}/${gb2_time}${mmp2}??.${obsname}_*_${gb2_time}${mmp2}??.grib2 | wc -l`
if [ ${numgrib2_00} -eq 33 ]; then
   gb2_fname="${gb2_time}${mm}??.${obsname}_*_${gb2_time}${mm}??.grib2"
   ${CP} ${COM_MOSAIC_GRIB2}/${gb2_fname}           .
#  ${LS} ${gb2_time}????.${obsname}_*_${gb2time}????.grib2 > filelist_mrms
   ${LS} ${gb2_fname}                                      > filelist_mrms
else
   if [ ${numgrib2_01} -eq 33 ]; then
      gb2_fname="${gb2_time}${mmp1}??.${obsname}_*_${gb2_time}${mmp1}??.grib2"
      ${CP} ${COM_MOSAIC_GRIB2}/${gb2_fname}            .
#     ${LS} ${gb2_time}????.${obsname}_*_${gb2time}????.grib2 > filelist_mrms
      ${LS} ${gb2_fname}                                      > filelist_mrms
   else
      if [ ${numgrib2_02} -eq 33 ]; then
         gb2_fname="${gb2_time}${mmp2}??.${obsname}_*_${gb2_time}${mmp2}??.grib2"
         ${CP} ${COM_MOSAIC_GRIB2}/${gb2_fname}         .
#        ${LS} ${gb2_time}????.${obsname}_*_${gb2time}????.grib2 > filelist_mrms
         ${LS} ${gb2_fname}                                      > filelist_mrms
      else
         ${ECHO} " No NSSL gribs data available, use NCEP 8 tiles binary"
         if [ -s filelist_mrms ]; then
            ${RM} -f filelist_mrms
         fi
      fi
   fi
fi


if [ -s filelist_mrms ]; then
   cp -p filelist_mrms filelist_mrms_tmp
   numgrib2=`more filelist_mrms | wc -l`
   echo "NSSL grib2 file level number = $numgrib2"
else
   numgrib2=0
fi

# Link to the radar data
if [ $numgrib2 -eq 36 ]; then
#  gzip -d *.gz
   numtiles=1
   rm -f filelist_mrms
#  gb2_fname="${gb2_time}${mm}??.${obsname}_*_${gb2_time}${mm}??.grib2"
   gb2_fname="${gb2_time}????.${obsname}_*_${gb2_time}????.grib2"
#  ls MergedReflectivityQC_*_${PDY}-${cyc}????.grib2 > filelist_mrms
   ls ${gb2_fname} > filelist_mrms
else
   if [ -s ${COMINmosaic}/tile1/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ] && \
      [ -s ${COMINmosaic}/tile2/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ] && \
      [ -s ${COMINmosaic}/tile3/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ] && \
      [ -s ${COMINmosaic}/tile4/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ]; then
      numtiles=4
      cp ${COMINmosaic}/tile1/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ./mosaic_t1.gz
      cp ${COMINmosaic}/tile2/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ./mosaic_t2.gz
      cp ${COMINmosaic}/tile3/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ./mosaic_t3.gz
      cp ${COMINmosaic}/tile4/mrefl/MREF3D33L.${PDY}.${cyc}${mm}00.gz ./mosaic_t4.gz
      gzip -d *.gz
   else
      numtiles=81
      export MOSAICdir=$COMINradar/radar.${PDY}${cyc}
      if [ $subhtime -eq 16 -o $subhtime -eq 46 ]; then
        ln -s ${MOSAICdir}/tile1/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t1
        ln -s ${MOSAICdir}/tile2/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t2
        ln -s ${MOSAICdir}/tile3/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t3
        ln -s ${MOSAICdir}/tile4/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t4
        ln -s ${MOSAICdir}/tile5/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t5
        ln -s ${MOSAICdir}/tile6/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t6
        ln -s ${MOSAICdir}/tile7/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t7
        ln -s ${MOSAICdir}/tile8/${PDY}_${cyc}${mmm1}.mosaic ./mosaic_t8
      else
        ln -s ${MOSAICdir}/tile1/${PDY}_${cyc}${mm}.mosaic ./mosaic_t1
        ln -s ${MOSAICdir}/tile2/${PDY}_${cyc}${mm}.mosaic ./mosaic_t2
        ln -s ${MOSAICdir}/tile3/${PDY}_${cyc}${mm}.mosaic ./mosaic_t3
        ln -s ${MOSAICdir}/tile4/${PDY}_${cyc}${mm}.mosaic ./mosaic_t4
        ln -s ${MOSAICdir}/tile5/${PDY}_${cyc}${mm}.mosaic ./mosaic_t5
        ln -s ${MOSAICdir}/tile6/${PDY}_${cyc}${mm}.mosaic ./mosaic_t6
        ln -s ${MOSAICdir}/tile7/${PDY}_${cyc}${mm}.mosaic ./mosaic_t7
        ln -s ${MOSAICdir}/tile8/${PDY}_${cyc}${mm}.mosaic ./mosaic_t8
      fi
   fi
fi

echo ${YYYYMMDDHH} > ./mosaic_cycle_date

cat << EOF > mosaic.namelist
 &setup
  tversion=${numtiles},
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
 /

EOF

if [ -f errfile ] ; then 
  rm -f errfile
fi

. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin pre-processing MRMS Grib2 MOSAIC RADAR Reflectivity Obs DATA"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"

# Run Process_mosaic
# copy the excutable file of processing mosaic data
${CP} ${EXECrtma3d}/${exefile_name_radar}   ./rtma3d_process_mosaic

runline="${MPIRUN}  -np ${np}  ./rtma3d_process_mosaic"
$runline > ${pgmout} 2>errfile
export err=$?; err_chk

msg="JOB $job FOR $RUN HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

if [ -f ${DATA}/NSSLRefInGSI.bufr ] ; then
# cpreq ${DATA}/NSSLRefInGSI.bufr ${COMOUT}/hrrr.t${cyc}z.NSSLRefInGSI.bufr
  cpreq ${DATA}/NSSLRefInGSI.bufr ${COMINobsproc_rtma3d}/${RUN}.t${cyc}z.NSSLRefInGSI.bufr
else
  msg="WARNING $pgm terminated normally but ${DATA}/NSSLRefInGSI.bufr does NOT exist."
  ${ECHO} "$msg"
  postmsg "$jlogfile" "$msg"
  exit 1
fi
  
exit 0
