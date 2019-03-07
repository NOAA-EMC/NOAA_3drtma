#!/bin/sh

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_post: POST master branch in repository EMC_post
# branch_post_gsd="master"
branch_post_gsd="ncep_post_raphrrr.v5.0"

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
#--- detect the machine/platform
#
if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    target=wcoss
elif [[ -d /cm ]] ; then
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    conf_target=nco
    target=cray
elif [[ -d /ioddev_dell ]]; then
#   MODULESHOME="/usrx/local/Modules/3.2.10"
#   . $MODULESHOME/init/sh
    conf_target=nco
    target=dell
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
elif [[ -d /jetmon ]] ; then
    . /apps/lmod/lmod/init/sh
    target=jet
else
    echo "unknown target = $target"
    exit 9
fi
echo " This machine is $target ."
#===================================================================#

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
#--- check out EMC_post (UPP) for RTMA3D and the specified branch
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
#--- First trying to use the modulefile used in EMC_post repo for this machine,
#--- If no modulefile for this machine in EMC_post, 
#--- then using the pre-defined modulefile in 3DRTMA package for this machine to build unipost
#
MODULEFILES=${TOP_RTMA}/modulefiles
SORCDIR_POST=${TOP_SORC}/rtma_post.fd
modules_fname=modulefile.build.post.${target}
modules_fname2=modulefile.run.post.${target}

if [ ! -f ${MODULEFILES}/${target}/build/modulefile.build.post.${target} ] ; then

echo " --> Using UPP modulefile to building post-processing code of 3DRTMA  "
  mfiles="v8.0.0-${target}"
  for modfile in $mfiles
  do
    if [ -f ${SORCDIR_POST}/modulefiles/post/${modfile} ] ; then
      if [ -f ${MODULEFILES}/${target}/build/${module_fname} ] ; then
        /bin/diff ${SORCDIR_POST}/modulefiles/post/${modfile} ${MODULEFILES}/${target}/build/${module_fname} > ${MODULEFILES}/${target}/build/diff_tmp_post.txt
        diff_word=` /bin/wc -w ${MODULEFILES}/${target}/build/diff_tmp_post.txt | /bin/awk '{print $1}' `
        if [ "${diff_word}" != "0" ] ; then
          echo "       ----> UPP package has ${modfile} different to 3DRTMA ${module_fname} <----  "
          mv    ${MODULEFILES}/${target}/build/${module_fname} ${MODULEFILES}/${target}/build/${module_fname}.orig
          cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}    ${MODULEFILES}/${target}/build/${module_fname}
          mv    ${MODULEFILES}/${target}/run/${module_fname}   ${MODULEFILES}/${target}/run/${module_fname2}.orig
          cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}    ${MODULEFILES}/${target}/run/${module_fname2}
        else
          rm -f ${MODULEFILES}/${target}/build/diff_tmp_post.txt
        fi
      else
        cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}   ${MODULEFILES}/${target}/build/${module_fname}
        cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}   ${MODULEFILES}/${target}/run/${module_fname2}
      fi
    else
      if [ -f ${MODULEFILES}/${target}/build/${module_fname} ] ; then
        echo "         ----> using pre-defined 3DRTMA modulefile for this machine: ${target}"
        cp -p ${MODULEFILES}/${target}/build/${module_fname}  ${SORCDIR_POST}/modulefiles/post/${modfile} 
        echo "         ----> adding lines in ${SORCDIR_POST}/sorc/build_ncep_post.sh for machine ${target}"
        cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh   ${MODULEFILES}/${target}/build/build_ncep_post.sh
        cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh   ${MODULEFILES}/${target}/build/build_ncep_post.sh.orig
#       /bin/sed -i '/elif.*WCOSS/i\elif [ $mac2 = fe  ] ; then                      # For Jet\n . /etc/profile\n . /etc/profile.d/modules.sh\n export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/nwprod/lib\n export machine=jet' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        /bin/sed -i '/elif.*WCOSS/i\elif [ $mac2 = fe  ] ; then                      # For Jet\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        /bin/sed -i '/elif.*WCOSS/i\ . /etc/profile\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        /bin/sed -i '/elif.*WCOSS/i\ . /etc/profile.d/modules.sh\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        /bin/sed -i '/elif.*WCOSS/i\ export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/nwprod/lib\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        /bin/sed -i '/elif.*WCOSS/i\ export machine=jet\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        mv    ${SORCDIR_POST}/sorc/build_ncep_post.sh            ${SORCDIR_POST}/sorc/build_ncep_post.sh.orig
        cp -p ${MODULEFILES}/${target}/build/build_ncep_post.sh  ${SORCDIR_POST}/sorc/build_ncep_post.sh
      else
        echo " ----> Neither UPP modulefile nor 3DRTMA modulefile found for this ${target} to build unipost code. "
        echo " ---->  Abort! Abort! Abort! Abort! Abort! Abort! "
        exit 1
      fi
    fi
  done
fi

# set +x

date

exit 0
