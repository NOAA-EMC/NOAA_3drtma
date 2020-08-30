#!/bin/ksh --login

#USER=Benjamin.Blake
#CDATE=2019051700
#cyc=00

cd /gpfs/dell2/stmp/${USER}/fv3nam/${cyc}

# Retrieve main.php to update cycle dates
scp bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/main.php .

DATE=$(sed -n "330p" main.php | cut -c 15-24)
DATEm1=$(sed -n "330p" main.php | cut -c 28-37)
DATEm2=$(sed -n "330p" main.php | cut -c 41-50)
DATEm3=$(sed -n "330p" main.php | cut -c 54-63)
DATEm4=$(sed -n "330p" main.php | cut -c 67-76)
DATEm5=$(sed -n "330p" main.php | cut -c 80-89)
echo $DATE
echo $DATEm1
echo $DATEm2
echo $DATEm3
echo $DATEm4
echo $DATEm5

sed '330s/var cyclist=\["'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'","'${DATEm5}'"\]/var cyclist=\["'${CDATE}'","'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'"\]/' main.php > tmpfile ; mv tmpfile main.php

scp main.php bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3

# Move images into correct directories on emcrzdm
# remove images from cycm5 directory
ssh bblake@emcrzdm.ncep.noaa.gov "rm /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/*.png"

# move cycm4 images to cycm5 directory
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm5/nam/images/"

# move cycm3 images to cycm4 directory
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm4/nam/images/"

# move cycm2 images to cycm3 directory
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm3/nam/images/"

# move cycm1 images to cycm2 directory
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm2/nam/images/"

# move cyc images to cycm1 directory
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/cycm1/nam/images/"


# Copy images from WCOSS to emcrzdm
rsync -t *conus*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *BN*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *CE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *CO*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *LA*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *MA*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *NC*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *NE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *NW*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *OV*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *SC*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *SE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *SF*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *SP*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *SW*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/
rsync -t *UM*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/cyc/nam/images/


exit
