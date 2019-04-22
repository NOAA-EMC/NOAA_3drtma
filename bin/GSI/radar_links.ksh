#!/bin/ksh --login

source /home/rtrr/PARM_EXEC/modulefiles/modulefile.jet.GSI_UPP_WRF

# Vars used for testing.  Should be commented out for production mode

# Set up paths to unix commands
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
MKDIR=/bin/mkdir
CAT=/bin/cat
ECHO=/bin/echo
CUT=/bin/cut
WC=/usr/bin/wc
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed

# for testing purposes
#DATAHOME="/home/rtrr/hrrr/2019032720/obsprd"
#START_TIME=2019032719
#NSSL="/public/data/radar/mrms"

# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
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
fi

if [ ! -d "${NSSL}" ]; then
  ${ECHO} "ERROR: directory '${NSSL}' does not exist!"
  exit 1
fi

if [ ! "${SUBH_TIME}" ]; then
  ${ECHO} "ERROR: \$SUBH_TIME is not defined!"
  exit 1
else
  subh=${SUBH_TIME}
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

# Compute date & time components for the analysis time
YYYY1=`${DATE} +"%Y" -d "${START_TIME}"`
MM1=`${DATE} +"%m" -d "${START_TIME}"`
DD1=`${DATE} +"%d" -d "${START_TIME}"`
HH1=`${DATE} +"%H" -d "${START_TIME}"`
NEXT_HOUR=`${DATE} +"%Y%m%d%H" -d "${START_TIME} + 1 hour"`
NEXT_HOUR=`${ECHO} "${NEXT_HOUR}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
YYYY2=`${DATE} +"%Y" -d "${NEXT_HOUR}"`
MM2=`${DATE} +"%m" -d "${NEXT_HOUR}"`
DD2=`${DATE} +"%d" -d "${NEXT_HOUR}"`
HH2=`${DATE} +"%H" -d "${NEXT_HOUR}"`

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
