set -x

##############################

export BASE=`pwd`
cd $BASE

module reset

# Load modules
export COMP=ftn
export COMP_MP=ftn
export COMP_MPI=ftn

export C_COMP=cc
export C_COMP_MP=cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

#module use $BASE/../modulefiles
#source $BASE/../modulefiles/HRRR/v4.0.0

module list

cd ${BASE}

# get the gsi source code package from local copy or from repository on github
GSI_SOURCE=${GSI_SOURCE:-"local"}  # "local" as default if no pre-defined GSI_SOURCE
                                   # to use the local copy of GSI in this 3DRTMA pacakge.
                                   # (sorc/rtma3d_sorc_info.fd/README_sorc_info.md for version detail)
shopt -s extglob
case "${GSI_SOURCE}" in
    emc?([-_])gsi|EMC?([-_])GSI )  # get a copy of GSI fro EMC GSI repo on github
        echo "git clone https://github.com/NOAA-EMC/GSI.git  ./rtma3d_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        cd ${BASE}
        [[ -d ./rtma3d_gsi.fd ]] && rm -rf ./rtma3d_gsi.fd
        git clone https://github.com/NOAA-EMC/GSI.git  ./rtma3d_gsi.fd
        cd ${BASE}/rtma3d_gsi.fd
        if [[ "${GSI_SOURCE}" == "EMCGSI" ]] ; then
            echo "git checkout latest version of main branch ==> develop"
            git checkout develop   # <--- checking out the latest commit of develop branch
        else
            echo "git checkout develop branch with commit ==> #ec8215d"
#           git checkout 964bcc3   # commit 964bcc3 is the old commit which still works with netcdf 4.7.r42
                                   # and does not need to load hdf5 lib when building and running GSI.
                                   # After this commit (from commit 2ddc1ac), GSI, by default,
                                   # uses bufr v12, netcdf 4.9.2 (require loading hdf5 lib), IP lib 5.x.
                                   # So need to update the module files when building and running 3DRTMA package.
                                   # After further testing with new GSI, the new modules would be updated
                                   # in 3DRTMA pacakge.
            git checkout ec8215d   # commit ec8215d (commited in the mid July 2026)
                                   # including the use of GSD terrain match to 2-m sfcobs of T (kx=188/195, 192/193)
        fi
        BUILD_GSI_SCRIPT="build_gsi4rtma3d.sh"
        ;;
    auto?([-_])qc|Auto?([-_])QC )  # get a copy of Matthew Morris's fork of GSI with his work on AutoQC
        echo "git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        cd ${BASE}
        [[ -d ./rtma3d_gsi.fd ]] && rm -rf ./rtma3d_gsi.fd
        git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd
        cd ${BASE}/rtma3d_gsi.fd
        echo "git checkout rtma3d_autoqc"
        git checkout rtma3d_autoqc
        BUILD_GSI_SCRIPT="build_gsi4rtma3d_bufr.sh"  # for Matthew Morris' old GSI
        ;;
    *)                             # no GSI_SOURCE is specified, use the local copy of GSI in this 3DRTMA pacakge
        echo "Using the local copy of GSI ==> sorc/rtma3d_gsi.fd  # <== Default GSI SOURCE"
        echo "   check sorc/rtma3d_sorc_info.fd/README_sorc_info.md for version detail."
        cd ${BASE}
        if [[ ! -d ${BASE}/rtma3d_gsi.fd ]] ; then
            echo " ****** WARNING WARNING WARNING ****** "
            echo " ****** WARNING WARNING WARNING ****** "
            echo " ****** WARNING WARNING WARNING ****** "
            echo " No GSI source code package under sorc/rtma3d_gsi.fd, Abort the building process. Please check your 3DRTMA package "
            exit 1
        fi
        cd ${BASE}/rtma3d_gsi.fd
        BUILD_GSI_SCRIPT="build_gsi4rtma3d.sh"
        ;;
esac
shopt -u extglob

  cd ${BASE}/rtma3d_gsi.fd
  [[ -d ./build ]] && rm -fr build
  mkdir build

  cd ush
#  use the gsi building script modified for HRRR-based 3DRTMA with GSD Cloud Analysis
  if [[ -f ./${BUILD_GSI_SCRIPT} ]] ; then 
      ./${BUILD_GSI_SCRIPT}
  elif [[ -f ${BASE}/../util_dev/${BUILD_GSI_SCRIPT} ]] ; then
      cp -p ${BASE}/../util_dev/${BUILD_GSI_SCRIPT}  ./
      ./${BUILD_GSI_SCRIPT}
  else
      echo " ****** WARNING WARNING WARNING ****** "
      echo " ****** WARNING WARNING WARNING ****** "
      echo " ****** WARNING WARNING WARNING ****** "
      echo "cannot find the GSI building script ${BUILD_GSI_SCRIPT}, abort building GSI ..."
  fi

#cp -p ${BASE}/rtma3d_gsi.fd/build/src/gsi/gsi.x        ${BASE}/../exec/rtma3d_gsi
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/gsi.x        ${BASE}/../exec/rtma3d_gsi
#cp -p ${BASE}/rtma3d_gsi.fd/build/src/enkf/enkf.x      ${BASE}/../exec/rtma3d_enkf
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/enkf.x       ${BASE}/../exec/rtma3d_enkf
#cp bin/enspreproc.x ${BASE}/../exec/rtma3d_process_enkf
#cp bin/initialens.x ${BASE}/../exec/rtma3d_initialens

##############################
