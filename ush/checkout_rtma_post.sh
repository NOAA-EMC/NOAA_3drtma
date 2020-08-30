#!/bin/bash

date
# set -x

#=========================================================================#
# User define the following variables:

# branch_post: POST master branch in repository EMC_post
# branch_post_gsd="master"
  branch_post_gsd="develop"

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
# read -p " Press [Enter] key to continue (or Press Ctrl-C to abort) "
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
    . /etc/profile
    . /etc/profile.d/modules.sh >/dev/null # Module Support
    target=theia
    nwprod_path="/scratch3/NCEPDEV/nwprod/lib/modulefiles"
elif [[ -d /jetmon ]] ; then
    . /etc/profile
    . /etc/profile.d/modules.sh >/dev/null # Module Support
    target=jet
    nwprod_path="/lfs3/projects/hfv3gfs/nwprod/lib/modulefiles"
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

if [ -d ${DIRNAME_POST} ] ; then
    rm -rf ${DIRNAME_POST}
fi

echo " ====> git clone gerrit:EMC_post  ./${DIRNAME_POST} "
git clone https://github.com/NOAA-EMC/EMC_post  ./${DIRNAME_POST}
cd ./${DIRNAME_POST}
scp -p .git/hooks/commit-msg  .git/hooks

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

  echo " --> Using UPP modulefile to building post-processing code of 3DRTMA  "
  mfiles="v8.0.0-wcoss_${target}_p3"
  for modfile in $mfiles
  do
    if [ -f ${SORCDIR_POST}/modulefiles/post/${modfile} ] ; then

#     # check whether the module file sets up the module path to nwprod libs (missed for theia module file.)
      if [ ${target} = "theia" ] ; then
        line_nwprod=`sed -n '/module.*use.*nwprod/p' ${SORCDIR_POST}/modulefiles/post/${modfile} | wc -l`
        if [ ${line_nwprod} -eq 0 ] ; then
          echo "EMC post module file does NOT have have the line to use nwprod modules"
          echo " need to add the following line"
          echo " ---> module use -a ???/nwprod/lib/modulefiles <--- "
          cp -p ${SORCDIR_POST}/modulefiles/post/${modfile} ${SORCDIR_POST}/modulefiles/post/${modfile}.orig
          sed -i -e "/set .*ver /a module use -a ${nwprod_path}\n" ${SORCDIR_POST}/modulefiles/post/${modfile}
          cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh            ${MODULEFILES}/${target}/build/build_ncep_post.sh
          sed -i '/elif.*Dell/i\ module purge\'           ${MODULEFILES}/${target}/build/build_ncep_post.sh
          mv    ${SORCDIR_POST}/sorc/build_ncep_post.sh            ${SORCDIR_POST}/sorc/build_ncep_post.sh.orig
          cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh.orig       ${MODULEFILES}/${target}/build/build_ncep_post.sh.orig
          cp -p ${MODULEFILES}/${target}/build/build_ncep_post.sh  ${SORCDIR_POST}/sorc/build_ncep_post.sh
        fi
      fi

#     # comparing the modulefile of EMC post with modulefile of 3DRTMA for post (if not same, use the one for EMC post)
      if [ -f ${MODULEFILES}/${target}/build/${modules_fname} ] ; then
        /bin/diff ${SORCDIR_POST}/modulefiles/post/${modfile} ${MODULEFILES}/${target}/build/${modules_fname} > ${MODULEFILES}/${target}/build/log.diff_modulefile_post
        diff_word=` /bin/wc -w ${MODULEFILES}/${target}/build/log.diff_modulefile_post | /bin/awk '{print $1}' `
        if [ "${diff_word}" != "0" ] ; then
          echo "       ----> UPP package has ${modfile} different to 3DRTMA ${modules_fname} <----  "
          mv    ${MODULEFILES}/${target}/build/${modules_fname} ${MODULEFILES}/${target}/build/${modules_fname}.orig
          cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}    ${MODULEFILES}/${target}/build/${modules_fname}
#         mv    ${MODULEFILES}/${target}/run/${modules_fname}   ${MODULEFILES}/${target}/run/${modules_fname2}.orig
#         cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}    ${MODULEFILES}/${target}/run/${modules_fname2}
        else
          echo "       ----> UPP package has ${modfile} SAME as 3DRTMA ${modules_fname} <----  "
        fi
      else
        cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}   ${MODULEFILES}/${target}/build/${modules_fname}
#       cp -p ${SORCDIR_POST}/modulefiles/post/${modfile}   ${MODULEFILES}/${target}/run/${modules_fname2}
      fi

    else

      if [ -f ${MODULEFILES}/${target}/build/${modules_fname} ] ; then
        echo "         ----> using pre-defined 3DRTMA modulefile for this machine: ${target}"
        cp -p ${MODULEFILES}/${target}/build/${modules_fname}  ${SORCDIR_POST}/modulefiles/post/${modfile} 
        echo "         ----> adding lines in ${SORCDIR_POST}/sorc/build_ncep_post.sh for machine ${target}"
        cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh   ${MODULEFILES}/${target}/build/build_ncep_post.sh
#       sed -i '/elif.*WCOSS/i\elif [ $mac2 = fe  ] ; then                      # For Jet\n . /etc/profile\n . /etc/profile.d/modules.sh\n export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/nwprod/lib\n export machine=jet' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\elif [ $mac2 = fe  ] ; then                      # For Jet\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\ . /etc/profile\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\ . /etc/profile.d/modules.sh\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\ module purge\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\ export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/nwprod/lib\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/elif.*WCOSS/i\ export machine=jet\' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        sed -i '/07\/18 Wen Meng:/a\# 03/19 Edward Colon:   update to use modules on ${target}' ${MODULEFILES}/${target}/build/build_ncep_post.sh
        mv    ${SORCDIR_POST}/sorc/build_ncep_post.sh            ${SORCDIR_POST}/sorc/build_ncep_post.sh.orig
        cp -p ${SORCDIR_POST}/sorc/build_ncep_post.sh.orig       ${MODULEFILES}/${target}/build/build_ncep_post.sh.orig
        cp -p ${MODULEFILES}/${target}/build/build_ncep_post.sh  ${SORCDIR_POST}/sorc/build_ncep_post.sh
      else
        echo " ----> Neither UPP modulefile nor 3DRTMA modulefile found for this ${target} to build unipost code. "
        echo " ---->  Abort! Abort! Abort! Abort! Abort! Abort! "
        exit 1
      fi

    fi
  done

# set +x

date

exit 0
