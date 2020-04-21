#!/bin/ksh 

# loading modules and set common unix commands from outside
#   in jobs/launch.sh and/or modulefile


#for subh in 00 15 30 45 60
#do
  if [ $subh -ne 00 ]; then
    YYYY=$YYYY1
    MM=$MM1
    DD=$DD1
    HH=$HH1
    WORKDIR=${DATAHOME}/${subh}
  else
    YYYY=$YYYY2
    MM=$MM2
    DD=$DD2
    HH=$HH2
    WORKDIR=${DATAHOME}
  fi
  ${MKDIR} -p ${WORKDIR}
  cd ${WORKDIR}
  if [ $subh -eq 60 ]; then
    mm1=59
    mm2=58
    mm3=57
  elif [ $subh -eq 00 ]; then
    mm1=00
    mm2=01
    mm3=02
  else
    mm1=$((${subh}-1))
    mm2=$subh
    mm3=$((${subh}+1))
  fi
  for mm in $mm1 $mm2 $mm3
  do
    s=0
    while [[ $s -le 59 ]]; do
      if [ $s -lt 10 ]; then
        ss=0${s}
      else
        ss=$s
      fi
      nsslfile=${NSSL}/${YYYY}${MM}${DD}-${HH}${mm}${ss}.MRMS_MergedReflectivityQC_00.50_${YYYY}${MM}${DD}-${HH}${mm}${ss}.grib2
      if [ -s $nsslfile ]; then
        echo 'Found '${nsslfile}
        numgrib2=`ls ${NSSL}/${YYYY}${MM}${DD}-${HH}${mm}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${mm}*.grib2 | wc -l`
        if [ ${numgrib2} -ge 10 ]; then
          ${RM} -f ${YYYY}${MM}${DD}-${HH}*.MRMS_MergedReflectivityQC*.grib2
          ln -sf ${NSSL}/${YYYY}${MM}${DD}-${HH}${mm}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${mm}*.grib2 . 
          ls ${YYYY}${MM}${DD}-${HH}${mm}*.MRMS_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${mm}*.grib2 > filelist_mrms
          echo 'Creating links for SUBH: '${subh}
          break 10
        fi
      fi
      ((s+=1))
    done 
  done
  if [ ! -s filelist_mrms ]; then
    rm -f filelist_mrms
  fi
#done

exit 0
