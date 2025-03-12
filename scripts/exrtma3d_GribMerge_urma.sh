#/bin/bash 
#
# * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * #
# *  UNIX Script Documentation Block                                            #
#                                                                               #
# Script name        : GribMerge.sh                                             #
# Script description : Read in the 2d fields from grib2 files and               #
#                      interpolate to the new grid.                             #
# Script how to use  : GribMerge.sh -i [input_grid_1] -i [input_grid_2] \       #
#                      -i [input_grid_N] -v [Variable to interpolate]   \       #
#                      -g ["Parameters of the new grid"] -o [outpur_grid]       #
#                                                                               #
# Details            :  -i To import a grid, e.g. multi_1.ak_4m.t12z.f000.grib  #
#                       1. Use an "-i" for each grid that you want to           #
#                       import.                                                 #
#                       2. The input_grid_1 is the grid with the highest        #
#                       resolution and input_grid_N is the grid with the        #
#                       coarsest resolution. The order of input grids           #
#                       has to be from the one of the highest resolution to     #
#                       the one with lowest.                                    #
#                                                                               #
#                       -v The name of the variable to be read and interpolated,#
#                       e.g. HTGSW                                              #
#                                                                               #
#                       -g Declare the grid options as required by wgrib2, e.g. #
#                       "nps:210.0:60.0 181.429:1649:2976.0 40.530:1105:2976.0" #
#                       The quotation marks ("") have to be used.               #
#                                                                               #
#                       -m Name of the mask in grib2 format                     #
#                                                                               #
#                       -o Name of output grid.                                 #
#                                                                               #
# Author:      Stelios Flampouris          Date: 2017-09-30                     #
#                                                                               #
# Script history log:                                                           #
# 2017-09-30  Stelios   - v.1.0 Original                                        #
# 2018-10-23  Stelios   - v.1.1 Minor Bug                                       #
#                                                                               #
# * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * | * #
#
# set -x
TEMPGRID='tmp_merge_grid_'
#
while getopts "i:v:g:m:o:" opt; do
   case $opt in
      i) GridIn+=("$OPTARG");;
      v) VarMet=(${OPTARG});;
      g) GridOpt=${OPTARG};;
      m) Mask=${OPTARG};;
      o) GridOut=${OPTARG};;
   esac
done
shift $((OPTIND -1))

count=0
for val in "${GridIn[@]}"; do
	$WGRIB2 ${val} -match ${VarMet} -new_grid_winds earth -new_grid ${GridOpt} ${TEMPGRID}${count}.grib2
   	if [ -f "${Mask}" ]; then 
   		${HOMEscript}/exurma_filter_land.sh ${TEMPGRID}${count}.grib2 ${Mask} ${TEMPGRID}${count}.grib2
	else
	  	echo "Warning: The sea land ${Mask} does not exist. The fields will be merged by using \
		 the sea gridpoints of the first guess. This can create some erroneous values close to \
		 the shoreline."
  	fi
   ((count++))
done
#echo "The number of input grids is: " ${count}
s="$WGRIB2 ${TEMPGRID}0.grib2 -rpn sto_1"

mincount=$((${count}-2))
if (( ${mincount} > 0 )); then 
	for ((i=1;i <= ${mincount}; i++)); do 
		s+=" -import_grib ${TEMPGRID}${i}.grib2 -rpn rcl_1:merge:sto_1 "
	done
fi
s+=" -import_grib ${TEMPGRID}$((${count}-1)).grib2 -rpn rcl_1:merge -grib_out ${GridOut}"
eval $s
rm -rf ${TEMPGRID}?.grib2 
