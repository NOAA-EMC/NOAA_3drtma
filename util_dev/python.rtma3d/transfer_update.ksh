#!/bin/ksh --login


NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate
YMDm1=`$NDATE -24 ${YMD}${cyc} | cut -c 1-8`
YMDm2=`$NDATE -48 ${YMD}${cyc} | cut -c 1-8`

PROD_HEAD=rtma3d

if [ ! -d /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD} ] ; then
mkdir -p /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD}
fi

cd /gpfs/dell1/ptmp/${USER}/rtma3dda/${YMD}

# Retrieve main.php to update cycle dates

scp ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/main.php .

DATE=$(sed -n "340p" main.php | cut -c 15-22)
DATEm1=$(sed -n "340p" main.php | cut -c 26-33)
DATEm2=$(sed -n "340p" main.php | cut -c 37-44)
DATEm3=$(sed -n "340p" main.php | cut -c 48-55)
DATEm4=$(sed -n "340p" main.php | cut -c 59-66)
DATEm5=$(sed -n "340p" main.php | cut -c 70-77)


echo $YMD
echo $DATE
echo $DATEm1
echo $DATEm2
echo $DATEm3
echo $DATEm4
echo $DATEm5


sed '340s/var cyclist=\["'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'","'${DATEm5}'"\]/var cyclist=\["'${YMD}'","'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'"\]/' main.php > tmpfile ; mv tmpfile main.php

scp main.php ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d

ssh ecolon@emcrzdm.ncep.noaa.gov "/home/people/emc/www/htdocs/mmb/rtma/rtma3d/update.sh"

ssh ecolon@emcrzdm.ncep.noaa.gov "mkdir -p /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/analyses"

ssh ecolon@emcrzdm.ncep.noaa.gov "mkdir -p /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/spc"

ssh ecolon@emcrzdm.ncep.noaa.gov "mkdir -p /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMD}/stats"

ssh ecolon@emcrzdm.ncep.noaa.gov "rm -f -r /home/ftp/emc/mmb/rtma/${PROD_HEAD}/${YMDm2}"

exit
