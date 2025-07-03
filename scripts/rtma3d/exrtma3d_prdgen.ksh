#!/bin/ksh 

##########################################################################
####  UNIX Script Documentation Block                                    #
#                                                                        #
# Script name:         exrtma3d_prdgen.ksh                               #
# Script description:  Generate the NDFD 2.5 km grids, run Smartinit,    #
#                      and run the RTMA "obslist/FAA" product generation #
#                      code.                                             r#
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

# Define the CONUS NDFD 2.5 km grid
grid_specs="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"

# Set to true if doing parallel interpolation
do_parallel_prdgen="true"

if [ "${do_parallel_prdgen}" = "true" ]; then
# parallel processing
  
  wgrib2 ${COMIN}/${RUN}.t${cyc}z.anl_prslev.grib2 -not_if ":HTSGW:surface:" -grib ${RUN}.t${cyc}z.anl_prslev.grib2_no_howv
  infile_prslev=${DATA}/${RUN}.t${cyc}z.anl_prslev.grib2_no_howv 
  wgrib2 ${infile_prslev} > prslev.txt

# Create parm files for subsetting on the fly 
# 48 subpieces for CONUS prslev and natlev files

  sed -n -e '1,15p' prslev.txt > conus_prslev_1.txt
  sed -n -e '16,31p' prslev.txt > conus_prslev_2.txt
  sed -n -e '32,48p' prslev.txt > conus_prslev_3.txt
  sed -n -e '49,64p' prslev.txt > conus_prslev_4.txt
  sed -n -e '65,80p' prslev.txt > conus_prslev_5.txt
  sed -n -e '81,96p' prslev.txt > conus_prslev_6.txt
  sed -n -e '97,112p' prslev.txt > conus_prslev_7.txt
  sed -n -e '113,128p' prslev.txt > conus_prslev_8.txt
  sed -n -e '129,144p' prslev.txt > conus_prslev_9.txt
  sed -n -e '145,160p' prslev.txt > conus_prslev_10.txt
  sed -n -e '161,176p' prslev.txt > conus_prslev_11.txt
  sed -n -e '177,192p' prslev.txt > conus_prslev_12.txt
  sed -n -e '193,208p' prslev.txt > conus_prslev_13.txt
  sed -n -e '209,224p' prslev.txt > conus_prslev_14.txt
  sed -n -e '225,240p' prslev.txt > conus_prslev_15.txt
  sed -n -e '241,256p' prslev.txt > conus_prslev_16.txt
  sed -n -e '257,272p' prslev.txt > conus_prslev_17.txt
  sed -n -e '273,288p' prslev.txt > conus_prslev_18.txt
  sed -n -e '289,304p' prslev.txt > conus_prslev_19.txt
  sed -n -e '305,320p' prslev.txt > conus_prslev_20.txt
  sed -n -e '321,336p' prslev.txt > conus_prslev_21.txt
  sed -n -e '337,352p' prslev.txt > conus_prslev_22.txt
  sed -n -e '353,368p' prslev.txt > conus_prslev_23.txt
  sed -n -e '369,384p' prslev.txt > conus_prslev_24.txt
  sed -n -e '385,400p' prslev.txt > conus_prslev_25.txt
  sed -n -e '401,416p' prslev.txt > conus_prslev_26.txt
  sed -n -e '417,432p' prslev.txt > conus_prslev_27.txt
  sed -n -e '433,448p' prslev.txt > conus_prslev_28.txt
  sed -n -e '449,463p' prslev.txt > conus_prslev_29.txt
  sed -n -e '464,479p' prslev.txt > conus_prslev_30.txt
  sed -n -e '480,495p' prslev.txt > conus_prslev_31.txt
  sed -n -e '496,512p' prslev.txt > conus_prslev_32.txt
  sed -n -e '513,528p' prslev.txt > conus_prslev_33.txt
  sed -n -e '529,544p' prslev.txt > conus_prslev_34.txt
  sed -n -e '545,560p' prslev.txt > conus_prslev_35.txt
  sed -n -e '561,576p' prslev.txt > conus_prslev_36.txt
  sed -n -e '577,593p' prslev.txt > conus_prslev_37.txt
  sed -n -e '594,609p' prslev.txt > conus_prslev_38.txt
  sed -n -e '610,625p' prslev.txt > conus_prslev_39.txt
  sed -n -e '626,641p' prslev.txt > conus_prslev_40.txt
  sed -n -e '642,657p' prslev.txt > conus_prslev_41.txt
  sed -n -e '658,673p' prslev.txt > conus_prslev_42.txt
  sed -n -e '674,689p' prslev.txt > conus_prslev_43.txt
  sed -n -e '690,705p' prslev.txt > conus_prslev_44.txt
  sed -n -e '706,721p' prslev.txt > conus_prslev_45.txt
  sed -n -e '722,737p' prslev.txt > conus_prslev_46.txt
  sed -n -e '738,753p' prslev.txt > conus_prslev_47.txt
  sed -n -e '754,$p' prslev.txt > conus_prslev_48.txt

  tasks=(48)
  domain=conus
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
  domain=conus
  count=0
# for leveltype in prslev
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

# for leveltype in prslev natlev  # for urma3d
  for leveltype in prslev          # for rtma3d
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

# Save to com directory 
    cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
    cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx
done
fi

# interpolate the "howv" field using the mask field for each NDFD grid

# interpolate the howv field to the 2.5 km NDFD grid

wgrib2 ${COMIN}/${RUN}.t${cyc}z.anl.howv.grib2 -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid \
   -new_grid_interpolation bilinear \
   -new_grid ${grid_specs} ${RUN}.t${cyc}z.anl.howv_ndfd.grib2

cp ${FIXrtma3d}/${RUN}/${RUN}_slmask_howv_HR.grb2 slmask.grb2

CDATE=$PDY$cyc
echo $CDATE

# Ice Analysis
found_seaice=no
ic=0  
while [ $ic -le 120 ] ; do 
   sice_FHH=`printf %03d $ic`
   siceCYCLE=`$NDATE -$sice_FHH $CDATE`
   sicePDY=`echo $siceCYCLE |cut -c1-8`
               
   probe_sice_grb2=$COMINsice/seaice_analysis.${sicePDY}/seaice.t00z.5min.grb.grib2
#
   if [ -s ${probe_sice_grb2} ]; then
      cpreq $probe_sice_grb2 seaice.grb2
         
# 1. Ice Interpolation
      wgrib2 seaice.grb2 -match "ICEC:mean sea level" \
             -new_grid_winds earth -new_grid ${grid_specs} ice_int.grb2
         
# 2. Create the ice mask#
      wgrib2 ice_int.grb2 -set_grib_type c3 \
      -if "ICEC:mean sea" -rpn "0.0:>" -fi -grib_out ice_mask.grb2

# 3. Ice + SLmask
      cat slmask.grb2 ice_mask.grb2 > ISL_mask.grb2

# 4. Create the new SLMask
      wgrib2 ISL_mask.grb2 \
         -if ":LAND:" -rpn "sto_1" -fi \
         -if ":ICEC:" -rpn "sto_2" -fi \
         -if_reg 1:2 \
            -rpn "rcl_1:rcl_2:max:clr_1" \
            -set_var LAND \
            -grib_out howv_mask.grb2

# 6. Delete temporary files
         #rm -rf ice_int.grb2 ice_mask.grb2 ISL_mask.grb2

      found_seaice=yes
      break
   else
      let "ic=ic+24"
   fi
done
if [[ ${found_seaice} = no ]] ; then
    err_exit "No sice available. The missing files in the above while-do loop are of the from $COMINsice/seaice_analysis.${sicePDY}/seaice.t00z.grb.grib2. 
              The script must be able to find at least one file out of the 5 files that it queries"
fi

field=':HTSGW:surface:'
mask=':LAND:surface:anl:'
leveltype=prslev

cp howv_mask.grb2 tmpmask.grib2tmp
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
USHrrfs=$USHdir/prdgen
export fhr=00
${USHrtma3d}/${RUN}/${RUN}_prdgen_smartinit.sh $cyc $PDY $DATAsmartinit ${COMOUT} ${COMIN} ${USHrtma3d} $RUN $EXECrtma3d $FIXrtma3d $PARMrtma3d ${NET} >> stdout 2>&1
date

smart_fields=':(TMP|DPT|SPFH):2 m above ground:anl:|:(UGRD|VGRD|WIND|WDIR|GUST):10 m above ground:anl:|:(GUST|PRES|HGT|HTSGW):surface:|:TCDC:entire atmosphere|HGT:cloud ceiling:|VIS'
wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -not_if "$smart_fields" -grib ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2_no_smart
cat ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2_no_smart $COMIN/${RUN}.t${cyc}z.smart.conus.grib2 > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
  
wgrib2 ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -s > ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2
cpreq ${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx ${COMOUT}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2.idx

echo "run the obslist code"
DATAobslist=${DATA}/prdgen_obslist
mkdir -p $DATA/prdgen_obslist
USHrrfs=$USHdir/prdgen
${USHrtma3d}/${RUN}/${RUN}_prdgen_obslist.sh $cyc $PDY $DATAobslist ${COMOUT} ${COMIN} ${USHrtma3d} $RUN $EXECrtma3d $FIXrtma3d $PARMrtma3d ${NET} >> stdout 2>&1
export err=$?; err_chk                       #GZ: ==> exit abnormally if obslisting crashed
date

exit

