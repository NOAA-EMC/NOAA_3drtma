#!/bin/ksh

set -x

# Copy filelist03 from COM direcory to determine which hrrrdas files were used
cpreq ${COMOUT}/${RUN}.t${cyc}z.filelist03 filelist03

mem_varlist="T,P_TOP,MU,MUB,U,V,QVAPOR,ZNW,Times,TH2,Q2,U10,V10"
while IFS= read -r line
do
  name=$(basename "$line")
  echo $line
  echo $name
  cpreq -p $line .
  fname_thinned=${RUN}.t${cyc}z.${name}_thinned
  ncks    -v ${mem_varlist} $name ${fname_thinned}
  cpreq -p ${fname_thinned} $COMOUT
done < "filelist03"

postmsg "$0 of $job completed normally"
