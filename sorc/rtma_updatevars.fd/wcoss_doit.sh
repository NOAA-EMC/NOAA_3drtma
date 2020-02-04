#! /bin/sh

# Most of this is stolen from the HWRF v12.0.2

module purge

# These get unset by the module purge:
module use /opt/cray/ari/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/modulefiles/

# System and compiler prereqs:
module load modules/3.2.6.7
module load switch/1.0-1.0502.57058.1.58.ari
module load craype-network-aries
module load ncep/1.0
module load xt-lsfhpc/9.1.3
module load craype/2.3.0
module load PrgEnv-intel/5.2.56
module load craype-sandybridge
module switch intel intel/15.0.3.187
module load hpss/4.1.0.3
module load dvs
module load eswrap

# Load iobuf module to add buffering to unbuffered I/O in
# applications:
module load iobuf/2.0.6

#PNetCDF:
module load PNetCDF-intel-sandybridge/1.5.0

#NetCDF:
module load HDF5-serial-intel-sandybridge/1.8.9
module load NetCDF-intel-sandybridge/4.2

export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_DFI_RADAR=1
export WRF_SMOKE=1

export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf -lz ${HDF5_LDFLAGS}";

if [[ "$1" == configure ]] ; then
    ./clean -aa
    ./clean -a
    ./clean
    find . -name '*.o' -o -name '*.a' -o -name '*.mod' | xargs rm -f
    ( echo 51 ; echo 1 ) | ./configure -hyb
    cat configure.wrf.useme > configure.wrf
    test -s configure.wrf
elif [[ "$1" == compile ]] ; then
    ./compile em_real
else
    echo Do you want to configure or compile \?
fi

