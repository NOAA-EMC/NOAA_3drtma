#!/bin/ksh --login


set -x
#cd /gpfs/dell2/stmp/${USER}/rtma3dda/${YMD}

# Retrieve main.php to update cycle dates
#scp ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/main.php .

DATE=$(sed -n "340p" main.php | cut -c 15-22)
DATEm1=$(sed -n "340p" main.php | cut -c 26-33)
DATEm2=$(sed -n "340p" main.php | cut -c 37-44)
DATEm3=$(sed -n "340p" main.php | cut -c 48-55)
DATEm4=$(sed -n "340p" main.php | cut -c 59-66)
DATEm5=$(sed -n "340p" main.php | cut -c 70-77)


YMD=20191008
echo $YMD
echo $DATE
echo $DATEm1
echo $DATEm2
echo $DATEm3
echo $DATEm4
echo $DATEm5

sed '340s/var cyclist=\["'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'","'${DATEm5}'"\]/var cyclist=\["'${YMD}'","'${DATE}'","'${DATEm1}'","'${DATEm2}'","'${DATEm3}'","'${DATEm4}'"\]/' main.php > tmpfile 

#ssh ecolon@emcrzdm.ncep.noaa.gov "rm /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm5/compare/images/*.png"

# move cycm4 images to cycm5 directory

#ssh ecolon@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm4/compare/images/*.png /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm5/compare/images"

# move cycm3 images to cycm4 directory

#ssh ecolon@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm3/compare/images/*.png /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm4/compare/images"

# move cycm2 images to cycm3 directory

#ssh ecolon@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm2/compare/images/*.png /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm3/compare/images"

# move cycm1 images to cycm2 directory

#ssh ecolon@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm1/compare/images/*.png /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm2/compare/images"

# move cyc images to cycm1 directory

#ssh ecolon@emcrzdm.ncep.noaa.gov "mv /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cyc/compare/images/*.png /home/people/emc/www/htdocs/mmb/rtma/rtma3d/cycm1/compare/images"

# Copy images from WCOSS to emcrzdm
#rsync -t *.png ecolon@emcrzdm.ncep.noaa.gov:/home/people/emc/www/htdocs/mmb/rtma/rtma3d/cyc/compare/images

exit
