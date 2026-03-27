#!/bin/ksh 

set -x

postmsg "$0 of $job has begun"

#assgin MM1, MM2, MM3
MM1=00
MM2=01
MM3=02

# Compute date & time components for the analysis time
# Extract from CDATE the starting year, month, day, and hour

YYYY=${CDATE:0:4}
MM=${CDATE:4:2}
DD=${CDATE:6:2}
HH=${CDATE:8:2}
YYYYMMDD=${CDATE:0:8}

#----- enter working directory -------
cd ${DATA}
echo "enter working directory:${DATA}"

# BUFR Table including the description for HREF
cpreq ${PARMrtma3d}/${RUN}_prepobs_prep.bufrtable prepobs_prep.bufrtable

# WPS GEO_GRID Data
cpreq ${FIXrtma3d}/${RUN}_geo_em.d01.nc geo_em.d01.nc

# print parameters for linking/processing
echo "CDATE: "${CDATE}
echo "MINUTES: "${MM1}"/"${MM2}"/"${MM3}

# create mrms file list
obsname="MergedReflectivityQC"

for min in ${MM1} ${MM2} ${MM3}
do
  echo "Looking for data valid:"${YYYY}"-"${MM}"-"${DD}" "${HH}":"${min}
  s=0
  while [[ $s -le 59 ]]; do
    ss=$(printf %2.2i ${s})
    radarfilez=${COMINradar}/${dom}/${obsname}/${obsname}_00.50_${YYYY}${MM}${DD}-${HH}${min}${ss}.grib2.gz
    if [ -s $radarfilez ]; then
      echo 'Found '${radarfilez}
      numgrib2=`ls ${COMINradar}/${dom}/${obsname}/${obsname}_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2.gz | wc -l`
      echo 'Number of GRIB-2 files: '${numgrib2}
      if [ ${numgrib2} -ge 1 ] && [ ! -e filelist_mrms ]; then
        cpreq ${COMINradar}/${dom}/${obsname}/${obsname}_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2.gz .
        gzip -d ${obsname}_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2.gz
        ls ${obsname}_*_${YYYY}${MM}${DD}-${HH}${min}*.grib2 > filelist_mrms
# store information needed for retrospective runs in rerun_info.txt
        echo "export mrms_time=${YYYY}${MM}${DD}-${HH}${min}${ss}" >> $COMOUT/${RUN}.t${cyc}z.rerun_info.txt
        echo 'Creating links for ' ${YYYYMMDDHH}
      fi
    fi
    ((s+=1))
  done 
done


# remove filelist_mrms if zero bytes
if [ ! -s filelist_mrms ]; then
  rm -f filelist_mrms
fi

if [ -s filelist_mrms ]; then
  mv filelist_mrms filelist_mrms_org
  ls MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
  numgrib2=`more filelist_mrms | wc -l`
  echo "Using radar data from: `head -1 filelist_mrms | cut -c10-15`"
  echo "NSSL grib2 file levels = $numgrib2"
else
  echo "WARNING: Not enough radar reflectivity files available."
fi

cat << EOF > mosaic.namelist
 &setup
  tversion=1,
  analysis_time = ${CDATE},
  dataPath = './',
 /

EOF

# Run obs processor
export pgm="${NET}_process_mosaic"
. prep_step

startmsg

APRUN="mpiexec -n $ntasks -ppn $ppn --cpu-bind core --depth 1"
${APRUN} ${EXECrtma3d}/${pgm} >> ${pgmout} 2>errfile
export err=$?; err_chk

targetfile="NSSLRefInGSI.bufr"
if [ -f ${DATA}/${targetfile} ] ; then
  cpreq ${DATA}/${targetfile} ${COMOUT}/${RUN}.t${cyc}z.NSSLRefInGSI.bufr
else
  postmsg "WARNING $pgm terminated normally but ${DATA}/${targetfile} does NOT exist."
fi

postmsg "$0 of $job completed normally"
