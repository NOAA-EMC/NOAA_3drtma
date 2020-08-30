#!/bin/ksh 

set -x

cd /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD}

NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate

time_check=`$NDATE`

time_comp=`echo $time_check | cut -c 1-8`

if [ ${YMD} -lt ${time_comp} ] ; then

rsync -t *${cyc}${subcyc}*.png ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm1/images

else

rsync -t *${cyc}${subcyc}*.png ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/cyc/images

fi


exit
