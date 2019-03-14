#!/bin/sh

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_post: POST master branch in repository EMC_post
branch_post_gsd="master"

# branch_post_source: source branch  # the user-specified branch to check out.
                                     # if not specified by user, 
                                     # it is branch_master by default.

# branch_post_source="<specify_your_source_branch_name_here>"    # e.g.: RAPHRRR_201706_R94737
branch_post_source=${branch_post_source:-"$branch_post_gsd"}

#=========================================================================#

echo "*==================================================================*"
echo " this script is going to clone a local repository of post"
echo " to sub-directory of sorc/rtma_post.fd for RTMA3D system and "
echo " check out the source branch  "
echo 
echo "     ----> ${branch_post_source}"
echo
echo " please look at the branch name and make sure it is the branch you want to check out "
echo " if it is not, abort and change the definition of branch_post_source in this script ($0)  "
read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
echo
echo "*==================================================================*"

#
# --- Finding the RTMA ROOT DIRECTORY --- #
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
elif [[ -d /mnt/lfs3/projects ]] ; then
    . /apps/lmod/lmod/init/sh
    target=jet
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."

BASE=`pwd`;
echo " current directory is $BASE "

# detect existence of directory sorc/
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
DIRNAME_POST="rtma_post.fd"
SORCDIR_POST=${TOP_SORC}/${DIRNAME_POST}

#
#--- check out ProdGSi for RTMA3D and the specified branch
#
cd ${TOP_SORC}

echo " make a local clone of the EMC_post repository under ${TOP_SORC}/${DIRNAME_POST} ... "
echo " ====> git clone gerrit:EMC_post  ./${DIRNAME_POST} "
git clone gerrit:EMC_post  ./${DIRNAME_POST}
cd ./${DIRNAME_POST}
scp -p gerrit:hooks/commit-msg  .git/hooks

echo " check out the source branch --> ${branch_post_source}"
if [ ${branch_post_source} = "master" ] ; then
  echo " ====> master branch is checked out already as default. No need to git checkout again."
else
  echo " ====> git checkout ${branch_post_source} "
  git checkout ${branch_post_source}
fi

echo " ====> check up with the git status "
git status

if [ $? -ne 0 ] ; then
  echo " WARNING: checking branch ${branch_post_source} failed!"
  echo " WARNING: user-specified branch ${branch_post_source} does NOT exist!"
  echo " please double check with the list of branches (git branch -a)"
  exit 1
fi


#
#--- link modulefiles used in POST
#
 MODULEFILES=${TOP_RTMA}/modulefiles
 SORCDIR_POST=${TOP_SORC}/rtma_post.fd
 echo " --> copying POST modulefile and build script to appropriate UPP directories "

cp ${MODULEFILES}/jet_files/v8.0.0-$target ${SORCDIR_POST}/modulefiles/post
cp ${MODULEFILES}/jet_files/build_ncep_post.sh ${SORCDIR_POST}/sorc


date

exit 0
