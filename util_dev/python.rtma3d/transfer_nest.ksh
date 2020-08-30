#!/bin/ksh --login

#USER=Benjamin.Blake
#CDATE=2019051700
#cyc=00

cd /gpfs/dell2/stmp/${USER}/fv3nest/${cyc}

# Retrieve main.php to update cycle dates
scp bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/main2.php .

DATEm1=$(sed -n "333p" main2.php | cut -c 15-24)
DATEm2=$(sed -n "333p" main2.php | cut -c 28-37)
DATEm3=$(sed -n "333p" main2.php | cut -c 41-50)
DATEm4=$(sed -n "333p" main2.php | cut -c 54-63)
echo $DATEm1
echo $DATEm2
echo $DATEm3
echo $DATEm4

sed '333s/var cyclist=\["'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'"\]/var cyclist=\["'${CDATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'"\]/' main2.php > tmpfile ; mv tmpfile main2.php

scp main2.php bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3

# Move images into correct directories on emcrzdm
# remove images from daym3 directories
ssh bblake@emcrzdm.ncep.noaa.gov "rm /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/*.png"

# move daym2 images to daym3 directories
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym3/${cyc}z/nest/images/"

# move daym1 images to daym2 directories
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym2/${cyc}z/nest/images/"

# move previous day's images to daym1 directories
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*f0*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*f1*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*f2*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*f3*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*f4*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"
ssh bblake@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/*.png /home/people/emc/www/htdocs/mmb/bblake/fv3/daym1/${cyc}z/nest/images/"


# Copy images from WCOSS to emcrzdm
rsync -t *conus*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *BN*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *CE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *CO*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *LA*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *MA*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *NC*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *NE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *NW*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *OV*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *SC*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *SE*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *SF*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *SP*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *SW*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/
rsync -t *UM*.png bblake@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/bblake/fv3/${cyc}z/nest/images/


exit
