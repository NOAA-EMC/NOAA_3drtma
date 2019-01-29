#!/bin/sh

# set -x

echo "*==================================================================*"
echo " this script is going to link the fixed data to directory fix/ "
echo "*==================================================================*"

#===================================================================#
#
#--- detect the machine/platform
#
if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=cray
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=dell
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."

BASE=`pwd`;
echo " current directory is $BASE "

#
#--- detect existence of directory sorc/
#
i_max=5; i=0;
while [ "$i" -lt "$i_max" ]
do
  let "i=$i+1"
  if [ -d ./sorc ]
  then
    cd ./sorc
    TOP_SORC=`pwd`
    TOP_RTMA=`dirname $(readlink -f .)`
    echo " found sorc/ is under $TOP_RTMA"
    break
  else
    cd ..
  fi
done
if [ "$i" -ge "$i_max" ]
then
  echo ' directory sorc/ could not be found. Abort the task of compilation.'
  exit 1
fi

USH_DIR=${TOP_RTMA}/ush

echo
echo

#
#--- copy/link executable files (GSI utility files, etc.)
#
GSI_BIN=${TOP_SORC}/build_gsi/bin
EXEC=${TOP_RTMA}/exec

echo " linking GSI executable files ... "
cd ${GSI_BIN}
exe_fnames=`ls *`
cd $EXEC
for fn in $exe_fnames
do
  echo " cp -p  $GSI_BIN/$fn  ./$fn "
# ln -sf $GSI_BIN/$fn  ./$fn
  cp -p  $GSI_BIN/$fn  ./$fn
done

POST_BIN=${TOP_SORC}/rtma_post.fd/exec
EXEC=${TOP_RTMA}/exec

echo " linking UNI-POST executable files ... "
cd ${POST_BIN}
exe_fnames=`ls *`
cd $EXEC
for fn in $exe_fnames
do
  echo " cp -p  $POST_BIN/$fn  ./$fn "
# ln -sf $POST_BIN/$fn  ./$fn
  cp -p  $POST_BIN/$fn  ./$fn
done

echo 
echo " ====> list out te executable files in exec/"
ls -ltr $EXEC

#set +x

date

exit 0
