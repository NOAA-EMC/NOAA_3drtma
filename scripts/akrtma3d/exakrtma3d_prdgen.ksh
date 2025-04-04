#!/bin/ksh 

##########################################################################
####  UNIX Script Documentation Block                                    #
#                                                                        #
# Script name:         exakrtma3d_prdgen.ksh                             #
# Script description:  Generate the NDFD 3 km grids, run Smartinit,      #
#                      and run the RTMA "obslist/FAA" product generation #
#                      code.                                             #
#                                                                        #
# Author:      Annette Gibbs         Org: NOAA/EMC     Date: 2024-12-20  #
#                                                                        #
# Script history log:                                                    #
# 2024-12-20  Annette Gibbs                                              #
#                                                                        #
##########################################################################

set -x

ulimit -s unlimited
ulimit -a
  
export OMP_NUM_THREADS=1
 
cd $DATA

# Define the Alaska NDFD 3 km grid
grid_specs="nps:210:60 181.429:1649:2976.563 40.530101:1105:2976.563"

# Set to true if doing parallel interpolation
do_parallel_prdgen="true"

if [ "${do_parallel_prdgen}" = "true" ]; then
# parallel processing
  
  wgrib2 ${COMIN}/${RUN}.t${cyc}z.anl_prslev.grib2 -not_if ":HTSGW:surface:" -grib ${RUN}.t${cyc}z.anl_prslev.grib2_no_howv
  infile_prslev=${DATA}/${RUN}.t${cyc}z.anl_prslev.grib2_no_howv
  wgrib2 ${infile_prslev} > prslev.txt

# Create parm files for subsetting on the fly 
# 48 subpieces for CONUS prslev and natlev files

  sed -n -e '1,15p' prslev.txt > alaska_prslev_1.txt
  sed -n -e '16,31p' prslev.txt > alaska_prslev_2.txt
  sed -n -e '32,48p' prslev.txt > alaska_prslev_3.txt
  sed -n -e '49,64p' prslev.txt > alaska_prslev_4.txt
  sed -n -e '65,80p' prslev.txt > alaska_prslev_5.txt
  sed -n -e '81,96p' prslev.txt > alaska_prslev_6.txt
  sed -n -e '97,112p' prslev.txt > alaska_prslev_7.txt
  sed -n -e '113,128p' prslev.txt > alaska_prslev_8.txt
  sed -n -e '129,144p' prslev.txt > alaska_prslev_9.txt
  sed -n -e '145,160p' prslev.txt > alaska_prslev_10.txt
  sed -n -e '161,176p' prslev.txt > alaska_prslev_11.txt
  sed -n -e '177,192p' prslev.txt > alaska_prslev_12.txt
  sed -n -e '193,208p' prslev.txt > alaska_prslev_13.txt
  sed -n -e '209,224p' prslev.txt > alaska_prslev_14.txt
  sed -n -e '225,240p' prslev.txt > alaska_prslev_15.txt
  sed -n -e '241,256p' prslev.txt > alaska_prslev_16.txt
  sed -n -e '257,272p' prslev.txt > alaska_prslev_17.txt
  sed -n -e '273,288p' prslev.txt > alaska_prslev_18.txt
  sed -n -e '289,304p' prslev.txt > alaska_prslev_19.txt
  sed -n -e '305,320p' prslev.txt > alaska_prslev_20.txt
  sed -n -e '321,336p' prslev.txt > alaska_prslev_21.txt
  sed -n -e '337,352p' prslev.txt > alaska_prslev_22.txt
  sed -n -e '353,368p' prslev.txt > alaska_prslev_23.txt
  sed -n -e '369,384p' prslev.txt > alaska_prslev_24.txt
  sed -n -e '385,400p' prslev.txt > alaska_prslev_25.txt
  sed -n -e '401,416p' prslev.txt > alaska_prslev_26.txt
  sed -n -e '417,432p' prslev.txt > alaska_prslev_27.txt
  sed -n -e '433,448p' prslev.txt > alaska_prslev_28.txt
  sed -n -e '449,463p' prslev.txt > alaska_prslev_29.txt
  sed -n -e '464,479p' prslev.txt > alaska_prslev_30.txt
  sed -n -e '480,495p' prslev.txt > alaska_prslev_31.txt
  sed -n -e '496,512p' prslev.txt > alaska_prslev_32.txt
  sed -n -e '513,528p' prslev.txt > alaska_prslev_33.txt
  sed -n -e '529,544p' prslev.txt > alaska_prslev_34.txt
  sed -n -e '545,560p' prslev.txt > alaska_prslev_35.txt
  sed -n -e '561,576p' prslev.txt > alaska_prslev_36.txt
  sed -n -e '577,593p' prslev.txt > alaska_prslev_37.txt
  sed -n -e '594,609p' prslev.txt > alaska_prslev_38.txt
  sed -n -e '610,625p' prslev.txt > alaska_prslev_39.txt
  sed -n -e '626,641p' prslev.txt > alaska_prslev_40.txt
  sed -n -e '642,657p' prslev.txt > alaska_prslev_41.txt
  sed -n -e '658,673p' prslev.txt > alaska_prslev_42.txt
  sed -n -e '674,689p' prslev.txt > alaska_prslev_43.txt
  sed -n -e '690,705p' prslev.txt > alaska_prslev_44.txt
  sed -n -e '706,721p' prslev.txt > alaska_prslev_45.txt
  sed -n -e '722,737p' prslev.txt > alaska_prslev_46.txt
  sed -n -e '738,753p' prslev.txt > alaska_prslev_47.txt
  sed -n -e '754,$p' prslev.txt > alaska_prslev_48.txt

  tasks=(48)
  domain=alaska
  count=0
# for leveltype in prslev
  for leveltype in prslev
  do
    for task in $(seq ${tasks[count]})
    do
      if [ "${leveltype}" = "prslev" ]; then
        infile=${infile_prslev}
      else
        infile=${infile_natlev}
      fi
      mkdir -p $DATA/prdgen_${domain}_${leveltype}_${task}
      echo "$USHrtma3d/${RUN}/${RUN}_prdgen_subpiece.sh $cyc $task $domain ${infile} ${DATA} ${COMOUT} ${leveltype} " >> $DATA/poescript
    done 
    count=$count+1
  done

  chmod 775 ${DATA}/poescript

# Execute the script
  export CMDFILE=${DATA}/poescript
  mpiexec -np 48 --cpu-bind core cfp $CMDFILE >>$pgmout 2>errfile
  export err=$?; err_chk

# reassemble the output grids
  tasks=(48)
  domain=alaska
  count=0
  for leveltype in prslev
  do
    for task in $(seq ${tasks[count]})
    do
      cat $DATA/prdgen_${domain}_${leveltype}_${task}/${domain}_${leveltype}_${task}.grib2 >> ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
    done
    count=$count+1

#   cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
#   cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx
  done

else

# serial processing

#for leveltype in prslev natlev  # urma3d
  for leveltype in prslev # rtma3d
  do
    infile=${COMIN}/${RUN}.t${cyc}z.anl_${leveltype}.grib2 
    cp ${infile} .
# keep -set_radius? AMG
# Vector or scalar wind interpolation?  -set_radius 1:6370000 AMG
    wgrib2 ${infile} -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv inputs.${domain}${leveltype}.grib2.uv
    wgrib2 inputs.${domain}${leveltype}.grib2.uv -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid \
     -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
     -new_grid_interpolation bilinear \
     -if ":(WEASD|APCP|NCPCP|ACPCP|SNOD):" -new_grid_interpolation neighbor -fi \
     -if ":(NCONCD|NCCICE|SPNCR|CLWMR|CICE|RWMR|SNMR|GRLE|PMTF|PMTC|REFC|CSNOW|CICEP|CFRZR|CRAIN|LAND|ICEC|TMP:surface|VEG|CCOND|SFEXC|MSLMA|PRES:tropopause|LAI|HPBL|HGT:planetary boundary layer):|ICPRB|SIPD|ICESEV|CEIL|VIS" -new_grid_interpolation neighbor -fi \
     -new_grid ${grid_specs} ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2

# Create index file
    wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -s > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

done
fi

# interpolate the "howv" field using the mask field for each NDFD grid

# interpolate the howv field to the 3 km NDFD grid

wgrib2 ${COMIN}/${NET}.t${cyc}z.anl.howv.grib2 -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid \
 -new_grid_interpolation bilinear \
 -new_grid ${grid_specs} ${RUN}.t${cyc}z.anl.howv_ndfd.grib2

field=':HTSGW:surface:'
mask=':LAND:surface:anl:'
leveltype=prslev

cp ${FIXrtma3d}/${RUN}/${RUN}_slmask_nolakes.grb2 tmpmask.grib2tmp
cat ${RUN}.t${cyc}z.anl.howv_ndfd.grib2 >> tmpmask.grib2tmp
wgrib2 tmpmask.grib2tmp \
  -if '^1:' \
     -rpn '0:==:sto_1' \
  -fi \
  -if $field \
     -rpn 'rcl_1:mask' \
     -set_bitmap 0 -set_grib_type c3 \
     -grib_out tmpout.grib2tmp
cat tmpout.grib2tmp >> ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
#wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -s > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

#cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
#cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

date
echo "run the smartinit code"
DATAsmartinit=${DATA}/prdgen_smartinit
mkdir -p $DATAsmartinit
export fhr=00
${USHrtma3d}/${RUN}/${RUN}_prdgen_smartinit.sh $cyc $PDY $DATAsmartinit ${COMOUT} ${COMIN} ${USHrtma3d} $RUN $EXECrtma3d $FIXrtma3d $PARMrtma3d >> stdout 2>&1
date

smart_fields=':(TMP|DPT|SPFH):2 m above ground:anl:|:(UGRD|VGRD|WIND|WDIR|GUST):10 m above ground:anl:|:(GUST|PRES|HGT|VIS|HTSGW):surface:|:TCDC:entire atmosphere|HGT:cloud ceiling:'
wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -not_if "$smart_fields" -grib ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2_no_smart
cat ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2_no_smart $COMIN/${RUN}.t${cyc}z.smart.alaska.grib2 > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2

wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -s > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

echo "run the obslist code"
DATAobslist=${DATA}/prdgen_obslist
mkdir -p $DATAobslist
${USHrtma3d}/${RUN}/${RUN}_prdgen_obslist.sh $cyc $PDY $DATAobslist ${COMOUT} ${COMIN} ${USHrtma3d} $RUN $EXECrtma3d $FIXrtma3d $PARMrtma3d >> stdout 2>&1
date

exit

