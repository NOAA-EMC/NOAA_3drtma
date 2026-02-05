#!/bin/ksh

#-----------------------------------------------------------------------
#
set -x

currentime=$(echo "${PDYHH}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/')

#-----------------------------------------------------------------------
# Delete ptmp directories
#-----------------------------------------------------------------------
deletetime=$(date +%Y%m%d -d "${currentime} ${CLEAN_OLDCOM_HRS} hours ago")
echo "Deleting ptmp directories before ${deletetime}..."
#cd ${COMROOT}/${NET}/pbspro
cd ${COMROOT}/${NET}/${envir}
set -A XX $(ls -d ${NET}.20* | sort -r)
for dir in ${XX[*]};do
  onetime=$(echo $dir | cut -d'.' -f2)
  if [[ ${onetime} =~ ^[0-9]+$ ]] && [[ ${onetime} -le ${deletetime} ]]; then
    if [ -d ${COMROOT}/${NET}/${envir}/${NET}.${onetime}/${dom} ]; then
      rm -rf ${COMROOT}/${NET}/${envir}/${NET}.${onetime}/${dom}/*t${cyc}z*
      echo "Deleted t${cyc}z files in ${COMROOT}/${NET}/${envir}/${NET}.${onetime}/${dom}"
      if [ ${cyc} == 23 ] ; then
              rm -rf ${COMROOT}/${NET}/${envir}/${NET}.${onetime}/${dom}
              echo "Directory ${COMROOT}/${NET}/${envir}/${NET}.${onetime}/${dom} deleted."
      fi
    else
      echo "Nothing to delete in ${COMROOT}/${NET}/${envir}/${NET}.${onetime} for cycle t${cyc}z."
      echo "Files already scrubbed or parallel has not run long enough."
    fi
  fi
done

#-----------------------------------------------------------------------
# Delete data directories
#-----------------------------------------------------------------------
deletetime=$(date +%Y%m%d%H -d "${currentime} ${CLEAN_OLDRUN_HRS} hours ago")
echo "Deleting data directories before ${deletetime}..."
cd ${DATAROOT}/${envir}
set -A XX $(ls -d ${NET}.20* | sort -r)
for dir in ${XX[*]};do
  onetime=$(echo $dir | cut -d'.' -f2)
  if [[ ${onetime} =~ ^[0-9]+$ ]] && [[ ${onetime} -le ${deletetime} ]]; then
    if [ -d ${DATAROOT}/${envir}/${NET}.${onetime} ]; then
      rm -rf ${DATAROOT}/${envir}/${NET}.${onetime}
      echo "Deleted ${DATAROOT}/${envir}/${NET}.${onetime} for cycle t${cyc}z."
    fi
  else
    echo "Nothing to delete in ${DATAROOT}/${envir}/${NET}.${onetime} for cycle t${cyc}z."
    echo "Files already scrubbed or parallel has not run long enough."
  fi
done

#-----------------------------------------------------------------------
exit 0
