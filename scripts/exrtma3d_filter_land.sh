#!/bin/sh
###################################################################################################
#### UNIX Script Documentation Block
#
# Script name           :  filter_land.sh
# Script description    :  Filters grib2 outputs according to a binary mask
# 
# Instructions          :  filter_land.sh "input_file_with_data" "file_with_mask" "Output_file"
#                          e.g. > ./filter_land.sh anl.grib2_wexp urma2p5_slmask.grib2_nolakes output.grib2
#                          If "Output_file" is not given the script overwrites the "input_file_with_data"
#
#                          In the file the variables "field" and "mask" can be modified according 
#                          to the user's needs
#
# Author                :  Stelios Flampouris
#
# Script History Log    :  
# 2017-03-23            :  v1.0
#
####+++++++++--- To be modified by the user ---+++++++++####
field=':HTSGW:surface:'
mask=':LAND:surface:anl:' 
####+++++++++--- To be modified by the user ---+++++++++####
#
###################################################################################################
set -x
echo
if [ -z "$1" ]; then
   echo *** No input data file! Exiting...
   exit
else
   filedata=$1
   echo 1. Input data file : $filedata
fi
if [ -z "$2" ]; then
   echo *** No mask file! Exiting...
   exit
else
   filemask=$2
   echo 2. Mask file       : $filemask
fi
fileout=$3
if [ -z "$fileout" ]; then
   fileout=$filedata
   echo "*** Caution the Output will overwrite the Input File!"
else 
   fileout=$3
fi
echo 3. Output data file : $fileout
echo

if [ ! -f $filedata ] || [ ! -f $filemask ]; then
   echo "Check input files! Exiting..."
   exit 
fi

$WGRIB2 $filedata \
   -not_if $field \
   -grib tmpout_no_waves.grib2tmp

#$WGRIB2 $filedata \
#   -not_if $field \
#   -set_grib_type same \
#   -set_scaling same same \
#   -grib tmpout_no_waves.grib2tmp

$WGRIB2 $filemask -match $mask -grib tmpmask.grib2tmp
$WGRIB2 $filedata -match $field -grib tmpdata.grib2tmp

cat tmpdata.grib2tmp >> tmpmask.grib2tmp

$WGRIB2 tmpmask.grib2tmp \
   -if '^1:' \
      -rpn '0:==:sto_1' \
   -fi \
   -if $field \
      -rpn 'rcl_1:mask' \
      -grib_out tmpout.grib2tmp

#$WGRIB2 tmpmask.grib2tmp \
#   -if '^1:' \
#      -rpn '0:==:sto_1' \
#   -fi \
#   -if $field \
#      -rpn 'rcl_1:mask' \
#      -set_grib_type same \
#      -set_scaling same same \
#      -grib_out tmpout.grib2tmp

cat tmpout_no_waves.grib2tmp tmpout.grib2tmp > $fileout

rm *.grib2tmp 

####### EoF filter_land.sh #######
