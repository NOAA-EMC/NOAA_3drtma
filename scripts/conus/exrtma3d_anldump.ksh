#!/bin/ksh 
set -x
check_if_defined() { #usage: check_if_defined "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ -z "${path}" ]; then
      ${ECHO} "ERROR: \$${str} is not defined"; exit 1
    fi
  done
}
check_dirs_exist() { #usage: check_dirs_exist "var1_name" "var2_name" ...
  for str in "$@"; do
    eval "path=\${$str}"
    if [ ! -d ${path} ]; then
      ${ECHO} "ERROR: ${path}/ does not exist"; exit 1
    fi
  done
}
# checking if the required directories and files are defined and do exist
#  checking working directory
  check_if_defined "DATA" 
  check_dirs_exist "DATA" 
#  checking the directory where analysis file is saved, and the definition of analysis file name
# check_if_defined "COMOUTgsi_rtma3d"
# check_dirs_exist "COMOUTgsi_rtma3d"
  check_if_defined "ANLrtma3d_FNAME"
  check_if_defined "DATA_SHARED"
  check_dirs_exist "DATA_SHARED"
#  checking the directory where firstguess file (grib2) of howv/gust/vis is saved
  check_if_defined "COMOUT"
  check_dirs_exist "COMOUT"

# ncf_anl=${COMOUTgsi_rtma3d}/${ANLrtma3d_FNAME}
  ncf_anl=${DATA_SHARED}/wrf_inout

#############################################################################
# Make sure START_TIME is defined and in the correct format
START_TIME=${START_TIME:-"{PDY} ${cyc}"}
echo $START_TIME
echo $cyc
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME}"`
fi

# Compute date & time components for the analysis time
YYYYJJJHH00=`${DATE} +"%Y%j%H00" -d "${START_TIME}"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

HH_cycp1=`echo ${PDYHH_cycp1} | cut -c 9-10`
HH_cycm1=`echo ${PDYHH_cycm1} | cut -c 9-10`
YYYYMMDDHH_m1hr=`echo ${PDYHH_cycm1} | cut -c 1-10`

# DATE & TIME used in processing
ADATEymdh="${YYYYMMDDHH}"

#############################################################################

# Create the working directory and cd into it
workdir=${DATA}
cd ${workdir}
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}
export pgm=${NET}_anldump
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  retrieveing the analysis fields of howv/gust/vis from analysis file (netcdf), and dumped out to grib2 file and save."
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
 
############################################################################
  if [[ ! -f ${ncf_anl} ]] ; then
     echo "ANLDUMP@ Cycle ${START_TIME}: Could NOT find the analysis file (netcdf format) -->  \
                ${ncf_anl}.  exit with error.  "
     exit 1
  fi
    
############################################################################
# grid_specs_hrrr: for exp hrrr-based 3D RTMA on CONUS domain
  grid_specs_hrrr="lambert:-97.5:38.5:38.5 -122.719528:1799:3000.0 21.138123:1059:3000.0"
  grid_specs=$grid_specs_hrrr

#   checking if howv/gust/vis exists in the analysis file (netcdf)
  i_found_howv=0
  RUN_HOWV="FALSE"
# i_found_howv=$($NCDUMP -h ${ncf_anl} | grep -i "HOWV" | wc -l)  #-->multiple lines are found
  i_found_howv=$(ncdump  -h ${ncf_anl} | grep -i " HOWV(" | wc -l) 
  if [[ "${i_found_howv}" -eq 1 ]] ; then       # found unique variable HOWV
    RUN_HOWV="TRUE"
  fi
  i_found_gust=0
  RUN_GUST="FALSE"
# i_found_gust=$($NCDUMP -h ${ncf_anl} | grep -i "GUST" | wc -l)  #-->multiple lines are found
  i_found_gust=$(ncdump  -h ${ncf_anl} | grep -i " GUST(" | wc -l) 
  if [[ "${i_found_gust}" -eq 1 ]] ; then       # found unique variable GUST
    RUN_GUST="TRUE"
  fi
  i_found_vis=0
  RUN_VIS="FALSE"
# i_found_vis=$($NCDUMP -h ${ncf_anl} | grep -i "vis" | wc -l)  #-->multiple lines are found
  i_found_vis=$(ncdump  -h ${ncf_anl} | grep -i " VIS(" | wc -l) 
  if [[ "${i_found_vis}" -eq 1 ]] ; then       # found unique variable VIS
    RUN_VIS="TRUE"
  fi

# rm -f ${COMOUT}/${NET}.t${HH}z.anl.DirectAnl2Ds.grib2

#   Wave Height (howv)
  if [[ "${RUN_HOWV}" == "TRUE" ]] ; then
     varname="howv"
     varname_ncf="HOWV"
     varname_grb="HTSGW"
     varname_long="wave height"
     undefval="-0.01"       # -9999. ; -0.01
     level_info="surface"
     grib_type="c3"
     scaling_set=" -set_scaling 0 -4"

     grib2_tmplt_path=${COMOUT}
     grib2_tmplt_file=${grib2_tmplt_path}/${NET}.t${HH}z.fgs.${varname}.grib2

     grib2_fname="anl_${varname}.grib2"

     echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

     cd ${workdir}

     if [[ -f ${grib2_tmplt_file} ]] ; then
        echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
        echo "              ncks==> netcdf to binary"
        rm -f ./grb2_tmplate_${varname}.grib2
        ln -sf ${grib2_tmplt_file}      ./grb2_tmplate_${varname}.grib2
        rm -f ./analysis_wrf_inout_${varname}.nc
        ln -sf ${ncf_anl}               ./analysis_wrf_inout_${varname}.nc
        # 1. netcdf --> binary (ncks)
        rm -f ./anl_${varname}_bin.dat ./tmp_${varname}.nc
        ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat -p ./ ./analysis_wrf_inout_${varname}.nc ./tmp_${varname}.nc
        export err=$? ; err_chk

        # convert real8 to real4 in binary file (if the binary write-out of ncks is in real-8, but wgrib2 only handles real-4)

        # 2. binary --> grib2 (wgrib2)
        echo "              wgrib2 ==> binary to grib2"
        rm -f ./${grib2_fname}
        # wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -undefine_val ${undefval}  -set_lev "${level_info}" -set_grib_type $grib_type ${scaling_set} -grib_out ./${grib2_fname}
        wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
        export err=$? ; err_chk

#       if [[ -f $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2 ]] ; then
#         echo "Sea-Land no-lakes mask file --> $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2"
#         cp -p $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2    ./slmask.grib2
        if [[ -f $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2 ]] ; then
          echo "Sea-Land no-lakes mask file --> $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2"
          cp -p $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2    ./slmask.grib2
        else
          echo "No Sea-Land no-lakes mask file is used for Wave Height analysis"
        fi

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
              cat slmask.grib2 ice_mask.grb2 > ISL_mask.grb2

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
#     wgrib2 ${COMIN}/${RUN}.t${cyc}z.anl_${leveltype}_ndfd.grib2 -not_if $field -grib tmpout_no_waves.grib2tmp
#     wgrib2 ${FIXrtma3d}/${RUN}/${RUN}_slmask_nolakes.grb2 -match ${mask} -grib tmpmask.grib2tmp
#     wgrib2 ${COMIN}/${RUN}.t${cyc}z.prslev.f${fhr}.${domain}.grib2 -match ${field} -grib tmpdata.grib2tmp

      cp howv_mask.grb2 tmpmask.grib2tmp
      cat ${grib2_fname} >> tmpmask.grib2tmp
#     cat ${RUN}.t${cyc}z.anl.howv_ndfd.grib2 >> tmpmask.grib2tmp
#     cp tmpdata.grib2tmp tmpdata.grib2tmp.${domain}
#     cat tmpdata.grib2tmp >> tmpmask.grib2tmp
      wgrib2 tmpmask.grib2tmp \
        -if '^1:' \
           -rpn '0:==:sto_1' \
        -fi \
        -if $field \
           -rpn 'rcl_1:mask' \
           -set_bitmap 0 -set_grib_type c3 \
           -grib_out tmpout.grib2tmp

        export err=$?
        if [ $err -eq 0 ] ; then
           echo "           Successfully convert netcdf file to grib2 file for ${varname}."
           # save the analysis file (grib2) to $COMOUT
#          cp -p ./${grib2_fname}     ${COMOUT}/${RUN}.t${HH}z.anl.${varname}.grib2     
           cp -p tmpout.grib2tmp     ${COMOUT}/${RUN}.t${HH}z.anl.${varname}.grib2
           # appending to a single grib2 file
#          wgrib2 ${grib2_fname}      -append -grib ${COMOUT}/${RUN}.t${HH}z.anl.DirectAnl2Ds.grib2
#          wgrib2 tmpout.grib2tmp      -append -grib ${COMOUT}/${RUN}.t${HH}z.anl.DirectAnl2Ds.grib2
        else
           echo "conversion of ${varname} in analysis from netcdf to grib2 failed."
        fi
     else
        echo "missing grib2-template ${grib2_tmplt_file}, cannot convert ${varname} in analysis to grib2 file"
     fi
  fi

#   10-m Wind Gust (gust)
  if [[ "${RUN_GUST}" == "TRUE" ]] ; then
     varname="gust"
     varname_ncf="GUST"
     varname_grb="GUST"
     varname_long="10-m wind gust"
     undefval="-9999."       # -9999. ; -0.01
     level_info="surface"
     grib_type="c3"
     scaling_set=" -set_scaling 0 -4"

     grib2_tmplt_path=${COMOUT}
     grib2_tmplt_file=${grib2_tmplt_path}/${NET}.t${HH}z.fgs.${varname}.grib2

     grib2_fname="anl_${varname}.grib2"

     echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

     cd ${workdir}

     if [[ -f ${grib2_tmplt_file} ]] ; then
        echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
        echo "              ncks==> netcdf to binary"
        rm -f ./grb2_tmplate_${varname}.grib2
        ln -sf ${grib2_tmplt_file}      ./grb2_tmplate_${varname}.grib2
        rm -f ./analysis_wrf_inout_${varname}.nc
        ln -sf ${ncf_anl}               ./analysis_wrf_inout_${varname}.nc
        # 1. netcdf --> binary (ncks)
        rm -f ./anl_${varname}_bin.dat ./tmp_${varname}.nc
        ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat -p ./ ./analysis_wrf_inout_${varname}.nc ./tmp_${varname}.nc
        export err=$? ; err_chk

        # convert real8 to real4 in binary file (if the binary write-out of ncks is in real-8, but wgrib2 only handles real-4)

        # 2. binary --> grib2 (wgrib2)
        echo "              wgrib2 ==> binary to grib2"
        rm -f ./${grib2_fname}
        # wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -undefine_val ${undefval}  -set_lev "${level_info}" -set_grib_type $grib_type ${scaling_set} -grib_out ./${grib2_fname}
        wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
        export err=$?
        if [ $err -eq 0 ] ; then
           echo "           Successfully convert netcdf file to grib2 file for ${varname}."
           # save the analysis file (grib2) to $COMOUT
           cp -p ./${grib2_fname}     ${COMOUT}/${RUN}.t${HH}z.anl.${varname}.grib2     
           # appending to a single grib2 file
#          wgrib2 ${grib2_fname}      -append -grib ${COMOUT}/${RUN}.t${HH}z.anl.DirectAnl2Ds.grib2
        else
           echo "conversion of ${varname} in analysis from netcdf to grib2 failed."
        fi
     else
        echo "missing grib2-template ${grib2_tmplt_file}, cannot convert ${varname} in analysis to grib2 file"
     fi
  fi

#   Surface Visibility (vis)
  if [[ "${RUN_VIS}" == "TRUE" ]] ; then
     varname="vis"
     varname_ncf="VIS"
     varname_grb="VIS"
     varname_long="surface visibility"
     undefval="90000.0"       # 90000.0
     level_info="surface"
     grib_type="c3"
     scaling_set=" -set_scaling 0 -4"

     grib2_tmplt_path=${COMOUT}
     grib2_tmplt_file=${grib2_tmplt_path}/${NET}.t${HH}z.fgs.${varname}.grib2

     grib2_fname="anl_${varname}.grib2"

     echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

     cd ${workdir}

     if [[ -f ${grib2_tmplt_file} ]] ; then
        echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
        echo "              ncks==> netcdf to binary"
        rm -f ./grb2_tmplate_${varname}.grib2
        ln -sf ${grib2_tmplt_file}      ./grb2_tmplate_${varname}.grib2
        rm -f ./analysis_wrf_inout_${varname}.nc
        ln -sf ${ncf_anl}               ./analysis_wrf_inout_${varname}.nc
        # 1. netcdf --> binary (ncks)
        rm -f ./anl_${varname}_bin.dat ./tmp_${varname}.nc
        ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat -p ./ ./analysis_wrf_inout_${varname}.nc ./tmp_${varname}.nc
        export err=$? ; err_chk

        # convert real8 to real4 in binary file (if the binary write-out of ncks is in real-8, but wgrib2 only handles real-4)

        # 2. binary --> grib2 (wgrib2)
        echo "              wgrib2 ==> binary to grib2"
        rm -f ./${grib2_fname}
        # wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -undefine_val ${undefval}  -set_lev "${level_info}" -set_grib_type $grib_type ${scaling_set} -grib_out ./${grib2_fname}
        wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
        export err=$?
        if [ $err -eq 0 ] ; then
           echo "           Successfully convert netcdf file to grib2 file for ${varname}."
           # save the analysis file (grib2) to $COMOUT
           cp -p ./${grib2_fname}     ${COMOUT}/${RUN}.t${HH}z.anl.${varname}.grib2     
           # appending to a single grib2 file
#          wgrib2 ${grib2_fname}      -append -grib ${COMOUT}/${RUN}.t${HH}z.anl.DirectAnl2Ds.grib2
        else
           echo "conversion of ${varname} in analysis from netcdf to grib2 failed."
        fi
     else
        echo "missing grib2-template ${grib2_tmplt_file}, cannot convert ${varname} in analysis to grib2 file"
     fi
  fi

  set +x

  exit 0
