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
[[ -d ./rtma3d_gsi.fd ]] && rm -rf ./rtma3d_gsi.fd

# get the gsi source code package from repository on github
GSI_SOURCE=${GSI_SOURCE:-"autoqc"}       # autoqc as default if no pre-defined GSI_SOURCE
shopt -s extglob
case "${GSI_SOURCE}" in
    emc?([-_])gsi|EMC?([-_])GSI )
        echo "git clone https://github.com/NOAA-EMC/GSI.git  ./rtma3d_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        git clone https://github.com/NOAA-EMC/GSI.git  ./rtma3d_gsi.fd
        cd ${BASE}/rtma3d_gsi.fd
        echo "git checkout develop"
#       git checkout develop       # <--- checking out the latest commit of develop branch
#       git checkout 964bcc3       # commit 964bcc3 is the old commit which still works with netcdf 4.7.r42
                                   # and does not need to load hdf5 lib when building and running GSI.
                                   # After this commit (from commit 2ddc1ac), GSI, by default,
                                   # uses bufr v12, netcdf 4.9.2 (require loading hdf5 lib), IP lib 5.x.
                                   # So need to update the module files when building and running 3DRTMA package.
                                   # After further testing with new GSI, the new modules would be updated
                                   # in 3DRTMA pacakge.
        git checkout 005343a       # commit 005343a requires pnetcdf (v1.12.*), netcdf (v4.9.*), bufr (v12.*)
                                   # and support the use of gsd_terrain_match to mesonet obs of t (kx=188).
        BUILD_GSI_SCRIPT="build_gsi4rtma3d.sh"
        ;;
    auto?([-_])qc|Auto?([-_])QC )
        echo "git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd
        cd ${BASE}/rtma3d_gsi.fd
        echo "git checkout rtma3d_autoqc"
        git checkout rtma3d_autoqc
        BUILD_GSI_SCRIPT="build_gsi4rtma3d_bufr.sh"
        ;;
    *)
#       If no GSI_SOURCE is specified, check out Matthew Morris's fork of GSI as default
        echo "git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd  # <== Default GSI SOURCE"
        git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma3d_gsi.fd
        cd ${BASE}/rtma3d_gsi.fd
        echo "git checkout rtma3d_autoqc"
        git checkout rtma3d_autoqc
        BUILD_GSI_SCRIPT="build_gsi4rtma3d_bufr.sh"
        ;;
esac
shopt -u extglob

  [[ -d ./build ]] && rm -fr build
  mkdir build

  cd ush
#  use the gsi building script
  if [[ -f ${BASE}/../util_dev/${BUILD_GSI_SCRIPT} ]] ; then
      cp -p ${BASE}/../util_dev/${BUILD_GSI_SCRIPT}  ./
      ./${BUILD_GSI_SCRIPT}
  else
      echo "cannot find the GSI building script build_gsi4rtma3d.sh, abort building GSI ..."
  fi

#cp -p ${BASE}/rtma3d_gsi.fd/build/src/gsi/gsi.x        ${BASE}/../exec/rtma3d_gsi
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/gsi.x        ${BASE}/../exec/rtma3d_gsi
#cp -p ${BASE}/rtma3d_gsi.fd/build/src/enkf/enkf.x      ${BASE}/../exec/rtma3d_enkf
# cp -p ${BASE}/rtma3d_gsi.fd/install/bin/enkf.x       ${BASE}/../exec/rtma3d_enkf
#cp bin/enspreproc.x ${BASE}/../exec/rtma3d_process_enkf
#cp bin/initialens.x ${BASE}/../exec/rtma3d_initialens

##############################
