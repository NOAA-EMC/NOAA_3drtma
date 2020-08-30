#!/bin/ksh 

test=`$NDATE`

YMD=2020042623

echo $test

echo $YMD

if [ $YMD -lt $test ]; then
echo "correct"
else
echo "wrong"
fi


#cd /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD}

#rsync -t *${cyc}${subcyc}*.png ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/cyc/images

exit
