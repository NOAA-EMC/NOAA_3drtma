set -x

##############################

export BASE=`pwd`

module reset
module list

cd ${BASE}

# get the upp source code package from local copy or from repository on github
UPP_SOURCE=${UPP_SOURCE:-"local"}  # "local" as default if no pre-defined UPP_SOURCE
                                   # to use the local copy of UPP in this 3DRTMA pacakge
                                   # see sorc/Readme_subpackages.md for version detail.
shopt -s extglob
case "${UPP_SOURCE}" in
    emc?([-_])upp|EMC?([-_])UPP )  # get a copy of UPP fro EMC UPP repo on github
        echo "git clone https://github.com/NOAA-EMC/UPP.git  ./rtma3d_post.fd # UPP_SOURCE=${UPP_SOURCE}"
        cd ${BASE}
        [[ -d ./rtma3d_post.fd ]] && rm -rf ./rtma3d_post.fd
        git clone https://github.com/NOAA-EMC/UPP.git  ./rtma3d_post.fd
        cd ${BASE}/rtma3d_post.fd
        if [[ "${GSI_SOURCE}" == "EMCUPP" ]] ; then
            echo "git checkout latest version of 3DRTMAv1 release branch ==> release/3drtma_v1"
            git checkout release/3drtma_v1  # <--- checking out the latest commit of branch release/3drtma_v1
        else
            echo "git checkout 3DRTMAv1 release branch with commit ==> #ff3a3ab"
            git checkout ff3a3ab   # commit ff3a3ab (commited on 6th of May 2026)
                                   # updated for HWT SFE 2026
        fi
        ;;
    *)                             # no UPP_SOURCE is specified, use the local copy of UPP in this 3DRTMA pacakge
        echo "Using the local copy of UPP ==> sorc/rtma3d_post.fd  # <== Default UPP SOURCE"
        echo "   check sorc/rtma3d_sorc_info.fd/README_sorc_info.md for version detail."
        cd ${BASE}
        if [[ ! -d ${BASE}/rtma3d_post.fd ]] ; then
            echo " ****** WARNING WARNING WARNING ****** "
            echo " ****** WARNING WARNING WARNING ****** "
            echo " ****** WARNING WARNING WARNING ****** "
            echo " No UPP source code package under sorc/rtma3d_post.fd, Abort the building process. Please check your 3DRTMA package "
            exit 1
        fi
        cd ${BASE}/rtma3d_post.fd
        ;;
esac
shopt -u extglob


cd ${BASE}/rtma3d_post.fd/tests
./compile_upp.sh

#cp -p ${BASE}/rtma3d_post.fd/exec/upp.x  ${BASE}/../exec/rtma3d_upp

##############################
