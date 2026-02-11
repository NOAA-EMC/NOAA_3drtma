#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rtma3d_prdgen_namerica.sh
# Script description:  Run RTMA3D product generation job for NA grid
#
# Author:        Annette Gibbs   Org: NOAA/EMC         Date: 2025-03-31
#
# Abstract: This script runs the RTMA3D PRDGEN jobs
#
# Script history log:
# 2025-03-31  Annette Gibbs
#

set -x

cyc=$1
subpiece=$2
domain=$3
infile=$4
DATA=$5
comout=$6
leveltype=$7
export compress_type=c3

cd $DATA/prdgen_${domain}_${leveltype}_${subpiece}

# 3-km NPS NDFD Alaska domain
gridspecs="nps:210:60 181.429:1649:2976.563 40.530101:1105:2976.563"
parmfile=${DATA}/alaska_${leveltype}_${subpiece}.txt

# Use different parm file for each subpiece
# Follow the RRFS_NA_13km option in the prdgen script and add Ceiling and Visibility to nearest neighbor (Annette/Manuel)
# Use -set_radius 1:6370000 AMG
wgrib2 ${infile} | grep -F -f ${parmfile} | wgrib2 -i -grib inputs.grib${domain} ${infile}
wgrib2 inputs.grib${domain} -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv inputs.grib${domain}.uv
wgrib2 inputs.grib${domain}.uv -set_bitmap 1 -set_grib_type ${compress_type} \
  -new_grid_winds grid -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
  -new_grid_interpolation bilinear \
  -if ":(WEASD|APCP|NCPCP|ACPCP|SNOD):" -new_grid_interpolation neighbor -fi \
  -if ":(NCONCD|NCCICE|SPNCR|CLWMR|CICE|RWMR|SNMR|GRLE|PMTF|PMTC|REFC|CSNOW|CICEP|CFRZR|CRAIN|LAND|ICEC|TMP:surface|VEG|CCOND|SFEXC|MSLMA|PRES:tropopause|LAI|HPBL|HGT:planetary boundary layer):|ICPRB|SIPD|ICESEV|CEIL|VIS" -new_grid_interpolation neighbor -fi \
  -new_grid ${gridspecs} ${domain}_${leveltype}_${subpiece}.grib2

# Send data to COMOUT in the ex-script after the grid is re-assembled

exit
