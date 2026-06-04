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

   cpreq ${COMIN}/${RUN}ak.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

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

# save the analysis file (grib2) to $COMOUT
      cpreq -p ./${grib2_fname}     ${COMOUT}/${RUN}ak.t${cyc}z.anl.${varname}.grib2     
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

   cpreq ${COMIN}/${RUN}ak.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

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
      cpreq -p ./${grib2_fname}     ${COMOUT}/${RUN}ak.t${cyc}z.anl.${varname}.grib2     
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

   cpreq ${COMIN}/${RUN}ak.t${cyc}z.fgs.${varname}.grib2 ./grb2_tmplate_${varname}.grib2

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
      cpreq -p ./${grib2_fname}     ${COMOUT}/${RUN}ak.t${cyc}z.anl.${varname}.grib2     
   else
     echo "missing grib2-template ${grib2_tmplt_file}, cannot convert ${varname} in analysis to grib2 file"
   fi
fi

postmsg "$0 of $job completed normally"
