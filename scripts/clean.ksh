#!/bin/ksh 
set -x
check_if_defined() { #usage: check_if_defined "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ -z "${path}" ]; then
      ${ECHO} "ERROR: \$${str} is not defined"; exit 1
    fi
  done
}
check_dirs_exist() { #usage: check_dirs_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -d ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
  done
}

# make sure variables/dirs exist
check_if_defined "WORK_ROOT" "CYCLE_HOUR" "subcyc"
check_dirs_exist "WORK_ROOT"

currentime=`date`
cyclehour=${CYCLE_HOUR}
mainroot=${WORK_ROOT}

# Delete run directories
if [ "${subcyc}" == "-1" ]; then #hourly run
  deletetime=`date +%Y%m%d%H -d "${currentime}  50 hours ago"`
else #subhourly run
  deletetime=`date +%Y%m%d%H%M -d "${currentime}  24 hours ago"`
fi
set -A workdir "${mainroot}/run"
echo "Delete run directory before ${deletetime}"
for currentdir in ${workdir[*]}; do
  cd ${currentdir}
  echo "Working on directory ${currentdir}"
  set -A XX `ls -d 20??????* | sort -r`
  maxnum=${#XX[*]}
  for onetime in ${XX[*]};do
    if [[ ${onetime} -le ${deletetime} ]]; then
      echo "Delete data in ${onetime}"
      rm -rf ${onetime}
    fi
  done
done

# Delete wrfout files
if [ "${subcyc}" == "-1" ]; then #hourly run
  deletetime=`date +%Y%m%d%H -d "${currentime}  12 hours ago"`  # keeping latest 12 cycles is enough
else
  deletetime=`date +%Y%m%d%H%M -d "${currentime}  3 hours ago"`  #4 cycles per hour, so keeping latest 12 cycles is enough
fi
set -A workdir "${mainroot}/run"
echo "Delete wrfouts/wrf_inout before ${deletetime}"
cd ${workdir}
echo "Working on directory ${workdir}"
set -A XX `ls -d 20??????* | sort -r`
maxnum=${#XX[*]}
for onetime in ${XX[*]};do
  if [[ ${onetime} -le ${deletetime} ]]; then
    echo "${onetime}: Delete wrfout_d01* in wrfprd/ and wrf_inout in gsiprd/"
    rm -f ${onetime}/wrfprd/wrfout_d01_*
    rm -f ${onetime}/gsiprd/wrf_inout
  fi
done

#=====================the following is to delete YYYYMMDD subdirectories under ptmp,stdout,log
# Delete ptmp directories
deletetime=`date +%Y%m%d -d "${currentime}  10 days ago"`
set -A workdir "${mainroot}/ptmp"
echo "Delete ptmp directory before ${deletetime}"
for currentdir in ${workdir[*]}; do
  cd ${currentdir}
  echo "Working on directory ${currentdir}"
  set -A XX `ls -d 20??????* | sort -r`
  maxnum=${#XX[*]}
  for onetime in ${XX[*]};do
    if [[ ${onetime} -le ${deletetime} ]]; then
      echo "Delete data in ${onetime}"
      rm -rf ${onetime}
    fi
  done
done

# Delete stdout directories
deletetime=`date +%Y%m%d -d "${currentime}  300 days ago"`
set -A workdir "${mainroot}/stdout"
echo "Delete stdout directory before ${deletetime}"
for currentdir in ${workdir[*]}; do
  cd ${currentdir}
  echo "Working on directory ${currentdir}"
  set -A XX `ls -d 20??????* | sort -r`
  maxnum=${#XX[*]}
  for onetime in ${XX[*]};do
    if [[ ${onetime} -le ${deletetime} ]]; then
      echo "Delete data in ${onetime}"
      rm -rf ${onetime}
    fi
  done
done

# Delete log directories
deletetime=`date +%Y%m%d -d "${currentime}  300 days ago"`
set -A workdir "${mainroot}/log"
echo "Delete log directory before ${deletetime}"
for currentdir in ${workdir[*]}; do
  cd ${currentdir}
  echo "Working on directory ${currentdir}"
  set -A XX `ls -d 20??????* | sort -r`
  maxnum=${#XX[*]}
  for onetime in ${XX[*]};do
    if [[ ${onetime} -le ${deletetime} ]]; then
      echo "Delete data in ${onetime}"
      rm -rf ${onetime}
    fi
  done
done

exit 0
