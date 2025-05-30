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
[[ -d ./rtma_gsi.fd ]] && rm -rf ./rtma_gsi.fd

# get the gsi source code package from repository on github
GSI_SOURCE=${GSI_SOURCE:-"autoqc"}       # autoqc as default if no pre-defined GSI_SOURCE
shopt -s extglob
case "${GSI_SOURCE}" in
    emc?([-_])gsi|EMC?([-_])GSI )
        echo "git clone https://github.com/NOAA-EMC/GSI.git  ./rtma_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        git clone https://github.com/NOAA-EMC/GSI.git  ./rtma_gsi.fd
        cd ${BASE}/rtma_gsi.fd
        echo "git checkout develop"
        git checkout develop       # <--- checking out the latest commit of develop branch
#       git checkout 0ef8d87       # <--- specifying the commit
        ;;
    auto?([-_])qc|Auto?([-_])QC )
        echo "git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma_gsi.fd  # GSI_SOURCE=${GSI_SOURCE}"
        git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma_gsi.fd
        cd ${BASE}/rtma_gsi.fd
        echo "git checkout rtma3d_autoqc"
        git checkout rtma3d_autoqc
        ;;
    *)
#       If no GSI_SOURCE is specified, check out Matthew Morris's fork of GSI as default
        echo "git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma_gsi.fd  # <== Default GSI SOURCE"
        git clone https://github.com/MatthewMorris-NOAA/GSI.git ./rtma_gsi.fd
        cd ${BASE}/rtma_gsi.fd
        echo "git checkout rtma3d_autoqc"
        git checkout rtma3d_autoqc
        ;;
esac
shopt -u extglob

  [[ -d ./build ]] && rm -fr build
  mkdir build

  cd ush
#  use the gsi building script
  if [[ -f ${BASE}/../util_dev/build_gsi4rtma3d.sh ]] ; then
     cp -p ${BASE}/../util_dev/build_gsi4rtma3d.sh  ./
    ./build_gsi4rtma3d.sh
  else
     echo "cannot find the GSI building script build_gsi4rtma3d.sh, abort building GSI ..."
  fi

cp -p ${BASE}/rtma_gsi.fd/build/src/gsi/gsi.x        ${BASE}/../exec/rtma_gsi
# cp -p ${BASE}/rtma_gsi.fd/install/bin/gsi.x        ${BASE}/../exec/rtma_gsi
cp -p ${BASE}/rtma_gsi.fd/build/src/enkf/enkf.x      ${BASE}/../exec/rtma_enkf
# cp -p ${BASE}/rtma_gsi.fd/install/bin/enkf.x       ${BASE}/../exec/rtma_enkf
#cp bin/enspreproc.x ${BASE}/../exec/rtma_process_enkf
#cp bin/initialens.x ${BASE}/../exec/rtma_initialens

##############################
