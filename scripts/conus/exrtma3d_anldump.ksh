#!/bin/ksh 
set -x

ncf_anl=${DATA_SHARED}/wrf_inout

ADATEymdh="${CDATE}"

#############################################################################

echo " Retrieving the analysis fields of howv/gust/vis from analysis file (netcdf), and dumped out to grib2 file and save."
 
############################################################################
if [[ ! -f ${ncf_anl} ]] ; then
  echo "FATAL ERROR: ${ncf_anl} does not exist"
  err_exit
fi
    
############################################################################
# grid_specs: for exp hrrr-based 3D RTMA on CONUS domain
grid_specs="lambert:-97.5:38.5:38.5 -122.719528:1799:3000.0 21.138123:1059:3000.0"

#   checking if howv/gust/vis exists in the analysis file (netcdf)
i_found_howv=0
RUN_HOWV="FALSE"
i_found_howv=$(ncdump  -h ${ncf_anl} | grep -i " HOWV(" | wc -l) 
if [[ "${i_found_howv}" -eq 1 ]] ; then       # found unique variable HOWV
  RUN_HOWV="TRUE"
fi
i_found_gust=0
RUN_GUST="FALSE"
i_found_gust=$(ncdump  -h ${ncf_anl} | grep -i " GUST(" | wc -l) 
if [[ "${i_found_gust}" -eq 1 ]] ; then       # found unique variable GUST
  RUN_GUST="TRUE"
fi
i_found_vis=0
RUN_VIS="FALSE"
i_found_vis=$(ncdump  -h ${ncf_anl} | grep -i " VIS(" | wc -l) 
if [[ "${i_found_vis}" -eq 1 ]] ; then       # found unique variable VIS
  RUN_VIS="TRUE"
fi

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

   cpreq ${COMIN}/${RUN}.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

   grib2_fname="anl_${varname}.grib2"

   echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

   if [ -f "grb2_tmplate_${varname}.grib2" ] ; then
      echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
      echo "              ncks==> netcdf to binary"
      # 1. netcdf --> binary (ncks)
      ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat ${ncf_anl} ./tmp_${varname}.nc
      export err=$? ; err_chk

      # 2. binary --> grib2 (wgrib2)
      echo "              wgrib2 ==> binary to grib2"
      wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
      export err=$? ; err_chk

      if [[ -f $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2 ]] ; then
        echo "Sea-Land no-lakes mask file --> $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2"
        cp -p $FIXrtma3d/${RUN}_hrrr_conus_3km_slmask_nolakes.grib2    ./slmask.grib2
      else
        echo "No Sea-Land no-lakes mask file is used for Wave Height analysis"
      fi

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

 cp howv_mask.grb2 tmpmask.grib2tmp
 cat ${grib2_fname} >> tmpmask.grib2tmp
   wgrib2 tmpmask.grib2tmp \
     -if '^1:' \
        -rpn '0:==:sto_1' \
     -fi \
     -if $field \
        -rpn 'rcl_1:mask' \
        -set_bitmap 0 -set_grib_type c3 \
        -grib_out tmpout.grib2tmp

   export err=$?; err_chk
   echo "           Successfully convert netcdf file to grib2 file for ${varname}."
        # save the analysis file (grib2) to $COMOUT
   cpreq -p tmpout.grib2tmp     ${COMOUT}/${RUN}.t${cyc}z.anl.${varname}.grib2
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

   cpreq ${COMIN}/${RUN}.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

   grib2_fname="anl_${varname}.grib2"

   echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

   if [ -f "grb2_tmplate_${varname}.grib2" ] ; then
      echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
      echo "              ncks==> netcdf to binary"
      # 1. netcdf --> binary (ncks)
      ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat ${ncf_anl} ./tmp_${varname}.nc
      export err=$? ; err_chk

      # 2. binary --> grib2 (wgrib2)
      echo "              wgrib2 ==> binary to grib2"
      wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
      export err=$?; err_chk
      echo "           Successfully convert netcdf file to grib2 file for ${varname}."
      # save the analysis file (grib2) to $COMOUT
      cpreq -p ./${grib2_fname}     ${COMOUT}/${RUN}.t${cyc}z.anl.${varname}.grib2     
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

   cpreq ${COMIN}/${RUN}.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

   grib2_fname="anl_${varname}.grib2"

   echo "    --> dump out ${varname_ncf} from ${ncf_anl} and then write to ${grib2_fname}"

   if [ -f "grb2_tmplate_${varname}.grib2" ] ; then
      echo "         found ${varname_long} firstguess grib2 file (as grib2 template) "
      echo "              ncks==> netcdf to binary"
      # 1. netcdf --> binary (ncks)
      ncks -C -O -v ${varname_ncf} -b ./anl_${varname}_bin.dat ${ncf_anl} ./tmp_${varname}.nc
      export err=$? ; err_chk

      # 2. binary --> grib2 (wgrib2)
      echo "              wgrib2 ==> binary to grib2"
      wgrib2 ./grb2_tmplate_${varname}.grib2 -import_bin ./anl_${varname}_bin.dat -no_header -set_var ${varname_grb} -set_ftime "anl" -set_date ${ADATEymdh} -set_lev "${level_info}" -grib_out ./${grib2_fname}
      export err=$? ; err_chk
      echo "           Successfully convert netcdf file to grib2 file for ${varname}."
      # save the analysis file (grib2) to $COMOUT
      cpreq -p ./${grib2_fname}     ${COMOUT}/${RUN}.t${cyc}z.anl.${varname}.grib2     
   else
     echo "missing grib2-template ${grib2_tmplt_file}, cannot convert ${varname} in analysis to grib2 file"
   fi
fi

if [ $SENDCOM = YES ]
then
   cp seaice.grb2               $COMOUT/${RUN}.t${cyc}z.seaice.grb2
   cp seaice.grb2               $COMOUT/${RUN}.t${cyc}z.sice.${sicePDY}_seaice.t00z.5min.grb.grib2
fi

postmsg "$0 of $job completed normally"
