#!/bin/sh

#  This script checks a tar file to see if it contains any restricted files
#  belonging to the "rstprod" group.  If it does, the file is put in the rstprod
#  group and the access permissions are set to 640.
#
#  Usage:  rhist_restrict.sh HPSStarfile method
#   HPSStarfile is the full path to the tar file we want to restrict,
#   method is 'htar' or 'tarrsync'
#
# If rsync support is added to runhistory, either this file will
# need to be updated, or rsyncserver will have to be exported in an
# upstream script.

set -x

if [ $# -ne 2 ]
then
  echo "Usage: rhist_restrict.sh HPSStarfile method"
  exit 1
fi 

file=$1
method=$2

case "$method" in
htar)
 num=$(htar -tvf $hpssdir $file | awk ' { print $3 } ' | grep -c rstprod)
 if [ $num -ne 0 ]
 then
  echo "$file CONTAINS RESTRICTED DATA."
  hsi "chgrp rstprod $file"
  hsi "chmod 640 $file"
 else
  echo "$file does not contain restricted data."
 fi
 ;;
tarrsync)
 num=$(ssh $rsyncserver "tar tvf $file | awk '{print \$2}' | grep -c /rstprod")
 if [ $num -ne 0 ]
 then
  echo "$file CONTAINS RESTRICTED DATA."
  ssh $rsyncserver "chgrp rstprod $file"
  ssh $rsyncserver "chmod 640 $file"
 else
  echo "$file does not contain restricted data."
 fi
 ;;
*)
 echo "rhist_restrict.sh ERROR: method '$method' not valid (should be 'htar' or 'tarrsync')" ; exit 0
;;
esac

exit
