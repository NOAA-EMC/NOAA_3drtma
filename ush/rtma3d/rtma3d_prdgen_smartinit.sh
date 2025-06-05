#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rtma_prdgen_smartinit.sh
# Script description:  Run RTMA "smartinit" product generation job
#
# Author: Annette Gibbs  Org: NOAA/EMC         Date: 2024-10-18
#
# Abstract: This script runs the RTMA PRDGEN jobs
#
# Script history log:
# 2024-10-18  Annette Gibbs
#

set -x

cyc=$1
CDATE=$2
DATAsmartinit=$3
COMIN=$4
COMOUT=$5
USHrtma=$6
RUN=$7      #rtma or urma
EXECdir=$8
fixdir=$9
parmdir=${10}
NET=${11}
fhr=00

cd $DATAsmartinit

# Run Smartinit
date
# extract the output fields for Smartinit
smartinit_fields_fn=${RUN}_natlev_smartinit.params
$WGRIB2 ${COMIN}/postprd.t${cyc}z/${RUN}.t${cyc}z.wrfsubhnat.grib2 | grep -F -f ${parmdir}/${RUN}/${smartinit_fields_fn} | $WGRIB2 -i -grib ${RUN}_natgrd.tm00 ${COMIN}/postprd.t${cyc}z/${RUN}.t${cyc}z.wrfsubhnat.grib2

# Define the CONUS 2.5 km NDFD grid
export wgrib2def_ndfd="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"
export wgrib2def_nwrfc="lambert:265:25:25 234.042704:709:2539.703 37.979684:795:2539.703"

ndfdstrings=(CS)

for ndfdstring in ${ndfdstrings[@]}
  do
    date
    case $ndfdstring in
      CS) domain=ndfd
          cp ${fixdir}/${RUN}/${RUN}_terrain_consensus.gb2 TOPONDFDCS
          cp ${fixdir}/${RUN}/${RUN}_smartmask_consensus.gb2 LANDNDFDCS
          grb2index TOPONDFDCS TOPONDFDCSI
          grb2index LANDNDFDCS LANDNDFDCSI
          export wgrib2def=${wgrib2def_ndfd} ;;
    esac

# Do we set radius like we do for HRRR? AMG
#grib2 hrrr_natgrd.tm00 -set_radius 1:6370000 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid \
do_parallel_smart="true"

if [ "${do_parallel_smart}" = "true" ]; then

  cp ${parmdir}/${RUN}/${smartinit_fields_fn} natlev.txt

  sed -n -e '1,18p' natlev.txt > conus_natlev_1.txt
  sed -n -e '19,36p' natlev.txt > conus_natlev_2.txt
  sed -n -e '37,54p' natlev.txt > conus_natlev_3.txt
  sed -n -e '55,72p' natlev.txt > conus_natlev_4.txt
  sed -n -e '73,90p' natlev.txt > conus_natlev_5.txt
  sed -n -e '91,108p' natlev.txt > conus_natlev_6.txt
  sed -n -e '109,126p' natlev.txt > conus_natlev_7.txt
  sed -n -e '127,$p' natlev.txt > conus_natlev_8.txt

  tasks=(8)
  leveltype=natlev
  domain=conus
  infile=${DATAsmartinit}/${RUN}_natgrd.tm00

  for task in $(seq ${tasks[count]})
  do
    mkdir -p $DATAsmartinit/prdgen_${domain}_${leveltype}_${task}
    echo "$USHrtma3d/${RUN}/${RUN}_prdgen_subpiece.sh $cyc $task $domain ${infile} ${DATAsmartinit} ${COMOUT} ${leveltype} " >> $DATAsmartinit/poescript
  done

  chmod 755 ${DATAsmartinit}/poescript

# Execute the script
  export CMDFILE=${DATAsmartinit}/poescript
  mpiexec -np 8 --cpu-bind core cfp $CMDFILE >>stdout 2>errfile
  export err=$?; err_chk

# reassemble the output

  tasks=(8)
  domain=conus
  count=0
  for task in $(seq ${tasks[count]})
  do
    cat $DATAsmartinit/prdgen_${domain}_${leveltype}_${task}/${domain}_${leveltype}_${task}.grib2 >> ${RUN}.NDFD${ndfdstring}.grib2
  done

else

date
    $WGRIB2 ${RUN}_natgrd.tm00 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid \
           -new_grid_interpolation bilinear \
           -if ":(SFCR|LAND|VGTYP|CEIL|VIS):" -new_grid_interpolation neighbor -fi \
           -new_grid ${wgrib2def} ${RUN}.NDFD${ndfdstring}.grib2
fi
date

    mv ${RUN}.NDFD${ndfdstring}.grib2 ${RUN}.NDFD${ndfdstring}
    grb2index ${RUN}.NDFD${ndfdstring} ${RUN}.NDFD${ndfdstring}I

    echo "DATE  "${CDATE}"00WASHINGTON" >DATE

    export pgm=${NET}_smartinit
    . prep_step

    ln -sf ${RUN}.NDFD${ndfdstring}     fort.11
    ln -sf ${RUN}.NDFD${ndfdstring}I    fort.12
    ln -sf TOPONDFD${ndfdstring}             fort.46
    ln -sf TOPONDFD${ndfdstring}I            fort.47
    ln -sf LANDNDFD${ndfdstring}             fort.48
    ln -sf LANDNDFD${ndfdstring}I            fort.49
    ln -sf ${RUN}${ndfdstring}.tm00      fort.71

    rm smart.nml
    cat > smart.nml <<EOF
3DRTMA
${ndfdstring}
GRIB2
$fhr
$cyc
EOF

date
    mpiexec -n 1 -ppn 1 $EXECdir/${pgm} < smart.nml >>$pgmout 2>errfile
    export err=$?; err_chk
date

    cat ../tmpout.grib2tmp >> ${RUN}${ndfdstring}.tm00
    cp ${RUN}${ndfdstring}.tm00 ${COMOUT}/${RUN}.t${cyc}z.smart.${domain}.grib2
    $WGRIB2 ${COMOUT}/${RUN}.t${cyc}z.smart.${domain}.grib2 -s > ${COMOUT}/${RUN}.t${cyc}z.smart.${domain}.grib2.idx

    date
  done

date

exit

