#! /bin/sh

#BSUB -J hrrr-test
#BSUB -o hrrr-test-%J.out
#BSUB -e hrrr-test-%J.err
#BSUB -q dev
#BSUB -P HRRR-T2O
#BSUB -cwd /gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_wrfarw.fd/WRFV3.9_sam_pnetcdf_ok
#BSUB -M 2000
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 03:59
#BSUB -L /bin/sh

export NODES=1

cd /gpfs/hps3/emc/meso/save/Ming.Hu/nwprod/rap.v5.0.0/sorc/rap_wrfarw.fd/WRFV3.9_sam_pnetcdf_ok
aprun -n 1 -d 24 -N 1 -j 1 ./wcoss_doit.sh compile
