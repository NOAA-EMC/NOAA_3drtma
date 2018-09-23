#!/bin/sh

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_post: POST master branch in repository EMC_post
branch_post_gsd="master"

# branch_post_source: source branch  # the user-specified branch to check out.
                                    # if not specified by user, 
                                    #   it is branch_master by default.

# branch_post_source="<specify_your_source_branch_name_here>"
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
echo " ====> git checkout ${branch_post_source} "
git checkout ${branch_post_source}

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
echo " --> linking POST modulefiles to RTMA3D modulefiles (used for compilation of POST)  "
cd ${MODULEFILES}
mfiles="modulefile.POST.wcoss modulefile.POST.theia modulefile.global_post.theia"
for modfile in $mfiles
do
  echo " ----> ln -sf ${MODULEFILES}/$modfile ${SORCDIR_POST}/modulefiles/$modfile "
  ln -sf ${SORCDIR_POST}/modulefiles/$modfile ${MODULEFILES}/$modfile 
done

# set +x

date

exit 0
