#!/bin/bash -l
set +x
. /usrx/local/prod/lmod/lmod/init/sh
set -x

module load impi/18.0.1
module load lsf/10.1
module load HPSS/5.0.2.5

module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles/
module load ruby/2.5.1 rocoto/1.2.4


module use -a /u/Benjamin.Blake/modulefiles
module load anaconda2/latest
module load met/8.1
export GRIB_DEFINITION_PATH=/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/EXT/grib_api.1.14.4/share/grib_api/definitions
export PROJ_DIR=/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/EXT/anaconda2/share/proj


#module use -a /u/Benjamin.Blake/modulefiles
#module load anaconda2/latest

#export GRIB_DEFINITION_PATH=/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/EXT/grib_api.1.14.4/share/grib_api/definitions
#export PYTHONPATH=${PYTHONPATH}:/gpfs/dell2/emc/modeling/noscrub/Jacob.Carley/python/lib


#rocotocheck -v 10 -w /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_snds.xml -d /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_snds.db -c 202003091445 -t makeplots_snds

#rocotostat -v 10 -w /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_snds.xml -d /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_snds.db
rocotoboot -v 10 -w /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_verif.xml -d /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_verif.db -c 202004200015 -t verif
#rocotorun -v 10 -w /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_verif.xml -d /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/drive_plots_verif.db
