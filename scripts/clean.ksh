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
check_if_defined "WORK_ROOT" "CYCLE_HOUR"
check_dirs_exist "WORK_ROOT"

currentime=`date`
cyclehour=${CYCLE_HOUR}
savelogtime=`date +%Y%m%d -d "${currentime}  2 days ago"`
mainroot=${WORK_ROOT}

# Delete run directories
deletetime=`date +%Y%m%d%H -d "${currentime}  42 hours ago"`
set -A workdir "${mainroot}/run"
echo "Delete directory before ${deletetime}"
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
deletetime=`date +%Y%m%d%H -d "${currentime}  12 hours ago"`
set -A workdir "${mainroot}/run"
echo "Delete wrfouts before ${deletetime}"
cd ${workdir}
echo "Working on directory ${workdir}"
set -A XX `ls -d 20??????* | sort -r`
maxnum=${#XX[*]}
for onetime in ${XX[*]};do
  if [[ ${onetime} -le ${deletetime} ]]; then
    echo "Delete wrfout files in ${onetime}/wrfprd"
    rm -f ${onetime}/wrfprd/wrfout_d01_*
  fi
done

exit 0
