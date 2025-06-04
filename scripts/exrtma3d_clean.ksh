#!/bin/ksh

#
#-----------------------------------------------------------------------
# Source the variable definitions file.
#-----------------------------------------------------------------------
#
#
#-----------------------------------------------------------------------
# Save current shell options (in a global array).  Then set new options
# for this script/function.
#-----------------------------------------------------------------------
#
#
#-----------------------------------------------------------------------
# set up currentime from CDATE 
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
    rm -rf ${COMROOT}/${NET}/pbspro/${NET}.${onetime}
    echo "Deleted ${COMROOT}/${NET}/pbspro/${NET}.${onetime}"
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
    rm -rf ${DATAROOT}/pbspro/${NET}.${onetime}
    echo "Deleted ${DATAROOT}/pbspro/${NET}.${onetime}"
  fi
done

#-----------------------------------------------------------------------
exit 0
