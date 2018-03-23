#!/bin/ksh --login

ulimit -s 512000

np=$PBS_NP

# Load modules
module load intel
module load mpt
module load netcdf

# Make sure WORK_ROOT is defined and exists
if [ ! "${WORK_ROOT}" ]; then
  ${ECHO} "ERROR: WORK_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${WORK_ROOT}" ]; then
  ${ECHO} "ERROR: WORK_ROOT directory '${WORK_ROOT}' does not exist!"
  exit 1
fi
if [ ! "${CYCLE_HOUR}" ]; then
  ${ECHO} "ERROR: CYCLE_HOUR is not defined!"
  exit 1
fi

currentime=`date`
cyclehour=${CYCLE_HOUR}
savelogtime=`date +%Y%m%d%H -d "${currentime}"`
mainroot=${WORK_ROOT}

# Delete run directories
deletetime=`date +%Y%m%d%H -d "${currentime}  1 day ago"`
set -A workdir "${mainroot}/run"
echo "Delete directory before ${deletetime}"
for currentdir in ${workdir[*]}; do
  cd ${currentdir}
  echo "Working on directory ${currentdir}"
  set -A XX `ls -d 20* | sort -r`
  maxnum=${#XX[*]}
  for onetime in ${XX[*]};do
    if [[ ${onetime} -le ${deletetime} ]]; then
      echo "Delete data in ${onetime}"
      rm -rf ${onetime}
    fi
  done
done

# Save recent logs into directory
echo "Cycle hour = ${cyclehour}"
saveloghour='03'
if [ ${cyclehour} -eq ${saveloghour} ]; then
  echo "Save log file"
  cd ${mainroot}/log
  mkdir ${savelogtime}
  mv *.log ./${savelogtime}
  cd ${mainroot}/LOGS
  mkdir ${savelogtime}
  mv * ./${savelogtime}
fi

# Delete log file directories
deletetime=`date +%Y%m%d%H -d "${currentime}  720 hours ago"`
set -A workdir "${mainroot}/log" \
               "${mainroot}/LOGS"
for currentdir in ${workdir[*]}; do
  cd ${workdir}
  set -A XX `ls -d 20*`
  for dir in ${XX[*]}; do
    if [[ ${dir} -le ${deletetime} ]]; then
      echo "Delete log directory : ${dir}"
      rm -rf ${dir}
    fi
  done
done

exit 0
