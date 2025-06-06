#!/bin/ksh

#-----------------------------------------------------------------------
#
currentime=$(echo "${PDYHH}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/')

#-----------------------------------------------------------------------
# Delete ptmp directories
#-----------------------------------------------------------------------
deletetime=$(date +%Y%m%d -d "${currentime} ${CLEAN_OLDCOM_HRS} hours ago")
echo "Deleting ptmp directories before ${deletetime}..."
cd ${COMROOT}/${NET}/pbspro
set -A XX $(ls -d ${NET}.20* | sort -r)
for dir in ${XX[*]};do
  onetime=$(echo $dir | cut -d'.' -f2)
  if [[ ${onetime} =~ ^[0-9]+$ ]] && [[ ${onetime} -le ${deletetime} ]]; then
    if [ -d ${COMROOT}/${NET}/pbspro/${NET}.${onetime} ]; then
      rm -rf ${COMROOT}/${NET}/pbspro/${NET}.${onetime}/*t${cyc}*
      echo "Deleted t${cyc}z files in ${COMROOT}/${NET}/pbspro/${NET}.${onetime}"
      if [ ${cyc} == 23 ]; then
            rm -rf ${COMROOT}/${NET}/pbspro/${NET}.${onetime}
            echo "Deleted ${COMROOT}/${NET}/pbspro/${NET}.${onetime}"
      fi
    else
      echo "Nothing to delete."
    fi
done

#-----------------------------------------------------------------------
# Delete stmp directories
#-----------------------------------------------------------------------
deletetime=$(date +%Y%m%d%H%M -d "${currentime} ${CLEAN_OLDRUN_HRS} hours ago")
echo "Deleting stmp directories before ${deletetime}..."
cd ${DATAROOT}/pbspro
set -A XX $(ls -d ${NET}.20* | sort -r)
for dir in ${XX[*]};do
  onetime=$(echo $dir | cut -d'.' -f2)
  if [[ ${onetime} =~ ^[0-9]+$ ]] && [[ ${onetime} -le ${deletetime} ]]; then
    if [ -d ${DATAROOT}/pbspro/${NET}.${onetime} ]; then
      rm -rf ${DATAROOT}/pbspro/${NET}.${onetime}
      echo "Deleted ${DATAROOT}/pbspro/${NET}.${onetime}"
    else
      echo "Nothing to delete."
    fi
  fi
done

#-----------------------------------------------------------------------
exit 0
