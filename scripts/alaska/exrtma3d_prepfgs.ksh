#!/bin/ksh

set -x 

postmsg "$0 of $job has begun"
    
cd ${DATA}

echo "***********************************************************"
echo "  begin retrieving howv/gust/vis and appending them to fgs, then copy firstguess to COMOUT"
echo "***********************************************************"

###################################################################################
# Look for the HRRR-first guess
###################################################################################

   found_hrrrges=no
   ic=1
   ic_max=9                           # max hours to search back for hrrr forecast file
   targetsize_hrrr=9092419248         # HRRRv4 on Alaska on WCOSS2 (forecast/restart history file)
   ics_max=15                         # max times to check the filesize of hrrr forecast
   sleep_time=60
#  loop of searching for firstguess in HRRR forecast file
   while [ $ic -le ${ic_max} ] ; do
     hrrrFHH=$ic
     hrrrFHH=`printf %02d $hrrrFHH`
     hrrrCYCLE=`$NDATE -$hrrrFHH $CDATE`
     hrrrPDY=`echo $hrrrCYCLE |cut -c1-8`
     hrrrCC=`echo $hrrrCYCLE |cut -c9-10`

     if [[ $hrrrCC == "00" || $hrrrCC == "03" || $hrrrCC == "06" \
        || $hrrrCC == "09" || $hrrrCC == "12" || $hrrrCC == "15" || $hrrrCC == "18" || $hrrrCC == "21" ]] ; then
        probe_hrrr_guess_nc=hrrrak_${hrrrCYCLE}f0${hrrrFHH}
        size_match=no
        found_hrrrges=no
        if [ -s $GESINhrrr/$probe_hrrr_guess_nc ]; then
            found_hrrrges=yes

            ics=1
#           loop of checking the filesize of hrrr forecast
            while [ $ics -le ${ics_max} ] ; do
                filesize=$(stat -c %s $GESINhrrr/$probe_hrrr_guess_nc)
                if [[ ${filesize} -eq ${targetsize_hrrr} ]] ; then
                    size_match="yes"
                    break  # breaking out the loop of checking file size
                else
                    size_match="no"
                    echo "${probe_hrrr_guess_nc} filesize (${filesize}) does not match the standard size (${targetsize_hrrr}). Sleep for 60 seconds and check again ..."
                    sleep ${sleep_time}
                fi
                let "ics=ics+1"
            done

            if [[ ${size_match} =~ [yYtT] ]] ; then
                cpreq $GESINhrrr/$probe_hrrr_guess_nc ${RUN}ak.t${cyc}z.firstguess.nc
                ind=$ic
                PDYHH_AK=$hrrrCYCLE
                echo "HRRR-AK ${ic}-hour forecast file ${probe_hrrr_guess_nc} is used as the firstguess for analysis cycle at ${PDY} ${cyc}Z"
                echo "export hrrrCYCLE=$hrrrCYCLE" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
                echo "export hrrrFHH=$hrrrFHH" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
                break      # breaking out the loop of searching for firstguess in HRRR forecast
            else
# Saving this problematic file for investigation later 
             cpreq -p $GESINhrrr/${probe_hrrr_guess_nc} ${COMOUT}/${RUN}ak.t${cyc}z.${probe_hrrr_guess_nc}
# Store information needed for retrospective runs in rerun_info.txt
             echo "Error for $GESINhrrr/${probe_hrrr_guess_nc} : Filesize (${filesize}) significantly smaller than standard size (${targetsize_hrrr})." >> rerun_info.txt
             echo "WARNING: HRRR-AK ${ic}-hour forecast file ${probe_hrrr_guess_nc} exists for analysis cycle at ${PDY} ${cyc}Z, but its filesize (${filesize}) does not match the standard size (${targetsize_hrrr}) even after waiting for ${ics_max} minutes. Try to search in the earlier HRRR-AK forecast files ..."
            fi
        else
            echo "HRRR-AK ${ic}-hour forecat file ${probe_hrrr_guess_nc} is not available for analysis cycle at ${PDY} ${cyc}Z. Try to search in the earlier HRRR foreast files ... "
        fi
     fi

     let "ic=ic+1"

   done
   echo "found_hrrrges: "$found_hrrrges  "  size_match: ${size_match}"

   if [[ ${found_hrrrges} = no ]] ; then
       err_exit "No HRRR-AK guess available. The missing files are $GESINhrrr/${dom}/hrrrak_${hrrrCYCLE}f0${hrrrFHH}. The script must be able to find at least one file in the above querying do-while loop"
   fi
 
#-----------------------------------------------------------------------
#
# Appending Firstguess of Ocean Significant Wave Height (HOWV) to Firstguess file
#
#-----------------------------------------------------------------------
RUN_HOWV=${RUN_HOWV:-"Yes"}
if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
#
# 1.1 define the Grid Specification for domain of 3DRTMA (used by wgrib2)

# grid_specs: for hrrr-based 3D RTMA on Alaska domain
  grid_specs="nps:225:60.0 185.117126:1299:3000.0 41.612949:919:3000.0"

# 1.2  fix dir (for slmask.grib2 file)
#  Sea-Land Mask for the correct interpolation of the howv Background.
  echo "No Sea-Land no-lakes mask file for Alaska 3-km grid domain yet."
#
# 2.1 Ocean Waves Background
   echo "COMINww3 is $COMINww3 (Wave background from WW3 Ocean Wave model)"
   found_ww3ges=no
   ic=0
   while [ $ic -le 24 ] ; do
      ww3FHH=$ic
      ww3FHH=`printf %03d $ww3FHH`
      ww3CYCLE=`$NDATE -$ww3FHH $CDATE`
      ww3PDY=`echo $ww3CYCLE |cut -c1-8`
      ww3CC=`echo $ww3CYCLE |cut -c9-10`

      set -A  probe_ww3_guess_grb2  "$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2" \
                                    "$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2"

      if [ -s "${probe_ww3_guess_grb2[0]}" ] && \
         [ -s "${probe_ww3_guess_grb2[1]}" ]    ; then

         echo "found wave background for Arctic: ${probe_ww3_guess_grb2[0]}"
         echo "found wave background for Global: ${probe_ww3_guess_grb2[1]}"

         cpreq ${probe_ww3_guess_grb2[0]} ww3.guess0.grib2
         cpreq ${probe_ww3_guess_grb2[1]} ww3.guess1.grib2

         ${HOMErtma3d}/scripts/ex${RUN}_GribMerge.sh -i ww3.guess0.grib2 -i ww3.guess1.grib2 \
                        -v HTSGW -g "${grid_specs}" \
                        -m slmask.grib2 \
                        -o ww3.guess.grib2
 
         echo "export ww3CYCLE=$ww3CYCLE" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         echo "export ww3FHH=$ww3FHH" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         found_ww3ges=yes
         break
      else
         let "ic=ic+1"
      fi
   done
   if [[ ${found_ww3ges} = no ]] ; then
       err_exit "No ocean WW3 guess available. Check availability of  \
gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2, \
gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2 \
queried in the above while-do-loop."
   fi

# 3. Appending Wave height (2-D) field to 3D-RTMA firstguess file (netcdf format)

    keyword_data="howv"
    varname_grb2ncf="HTSGW_surface"
    varname_ncf="HOWV"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height;
    elif [[ "${keyword_data}" == "vis" ]] ; then
      FillValue=90000.00                  # 90000 for visibility;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

    data_grb2="ww3.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

    FGS_FILE="${RUN}ak.t${cyc}z.firstguess.nc"
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    wgrib2 ./${data_grb2} -netcdf ./${data_ncf}

    if [ ! -f ./${data_ncf} ] ; then
      err_exit " 'wgrib2' failed to convert  ./${data_grb2} to ./${data_ncf}"
    fi

# step b. pre-processing the ww3-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., HTSGW_surface --> HOWV) and some attributes
    cp -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
    ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v ${varname_grb2ncf},${varname_ncf}  ./${data_ncf_new}

#   Do NOT set the -0.01 as undefined value for HOWV (following Manuel's suggestion to use the filled value in grib2 file)
#   MUST set the undefined value to be a meaningful value (-0.01 here for land area, otherwise it is very huge number)
    ncatted  -h -O -a _FillValue,${varname_ncf},o,f,${FillValue} ./${data_ncf_new}

    ncatted  -h -O -a coordinates,${varname_ncf},o,c,"XLONG XLAT XTIME"   ./${data_ncf_new}
    ncatted  -h -O -a stagger,${varname_ncf},c,c,""   ./${data_ncf_new}
    ncatted  -h -O -a units,${varname_ncf},o,c,"M"   ./${data_ncf_new}
    ncatted  -h -O -a MemoryOrder,${varname_ncf},c,c,"XY "   ./${data_ncf_new}
    ncatted  -h -O -a FieldType,${varname_ncf},c,l,"104"   ./${data_ncf_new}
    ncatted  -h -O -a description,${varname_ncf},c,c,"Significant Height of Combined Wind Waves and Swell"   ./${data_ncf_new}
  
#   ii) fetching out only the requested HOWV data and writing to a new netcdf file
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}
  
# step c. appending netcdf data into the firstguess data file (in netcdf format)
    ls -l ./${FGS_FILE_new}

    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}

    if [ $? -ne 0 ] ; then
      err_exit "Failed to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}. Exit abnormally   "
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new} "
      ls -l ./${FGS_FILE_new}
    fi

fi     # RUN_HOWV=True/true/Yes/yes, then retrieving fgs of howv
 
#-----------------------------------------------------------------------
#
# Appending Firstguess of 10-meter Wind Gust (GUST) to Firstguess File
#
#-----------------------------------------------------------------------
 
# 1. Retrieving Wind Gust from HRRR forecast (grib2 file)
#    and dumping out to grib2 file
   found_gustges=no
   PRE_YYYYMMDD=$(echo ${PDYHH_AK} | cut -c1-8)
   PRE_HH=$(echo ${PDYHH_AK} | cut -c9-10)
   hrrr_guess_grb2=${COMINhrrr}/hrrr.${PRE_YYYYMMDD}/${dom}/hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2

      if [[ -f ${hrrr_guess_grb2} ]] ; then 
         echo "found HRRR-AK ${ind} hour forecast grib2 file ${hrrr_guess_grb2} and retrieve 10-m Wind Gust from it: "
         cpreq ${hrrr_guess_grb2}   ./hrrr_guess.grib2
         if [ $ind == 0 ]; then
            FHH_string=":GUST:surface:anl:"
         else
            FHH_string=":GUST:surface:$ind hour fcst:"
         fi
         wgrib2 ./hrrr_guess.grib2 -match "${FHH_string}" -grib ./gust.guess.grib2
         export err=$?; err_chk

         found_gustges=yes
         echo "export gusthrrrCYCLE=${PDYHH_AK}" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         echo "export gusthrrrFHH=$ind" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         echo "export gusthrrrfgs=hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2" >> rerun_info.txt

      else
         found_gustges=no
      fi

   if [[ "${found_gustges}" == "no" ]] ; then
      err_exit "Could NOT find any HRRR 0~3 hours forecast grib2 file to \
                provide firstguess for 10-m wind gust. "
   fi
#
# 2. Appending wind gust to firstguess (netcdf format)
    keyword_data="gust"
    varname_grb2ncf="GUST_surface"
    varname_ncf="GUST"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height;
    elif [[ "${keyword_data}" == "vis" ]] ; then
      FillValue=90000.00                  # 90000 for visibility;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

    data_grb2="gust.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

    FGS_FILE="${RUN}ak.t${cyc}z.firstguess.nc"
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    wgrib2 ./${data_grb2} -netcdf ./${data_ncf}

    if [ ! -f ./${data_ncf} ] ; then
      err_exit " 'wgrib2' failed to convert  ./${data_grb2} to ./${data_ncf}"
    fi

# step b. pre-processing the GUST-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., GUST_surface --> GUST) and some attributes
    cpreq -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
    ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v ${varname_grb2ncf},${varname_ncf}  ./${data_ncf_new}

#   set the undefined value = -9999.0
    ncatted  -h -O -a _FillValue,${varname_ncf},o,f,${FillValue} ./${data_ncf_new}

    ncatted  -h -O -a coordinates,${varname_ncf},o,c,"XLONG XLAT XTIME"         ./${data_ncf_new}
    ncatted  -h -O -a stagger,${varname_ncf},c,c,""                             ./${data_ncf_new}
    ncatted  -h -O -a units,${varname_ncf},o,c,"M/S"                            ./${data_ncf_new}
    ncatted  -h -O -a MemoryOrder,${varname_ncf},c,c,"XY "   ./${data_ncf_new}
    ncatted  -h -O -a FieldType,${varname_ncf},c,l,"104"   ./${data_ncf_new}
    ncatted  -h -O -a description,${varname_ncf},c,c,"GUST Wind Speed (Gust)"   ./${data_ncf_new}

#   ii) fetching out only the required GUST data and writing to a new netcdf file
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}

# step c. appending netcdf data into the firstguess data file (in netcdf format)
    ls -l ./${FGS_FILE_new}

    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}

    if [ $? -ne 0 ] ; then
      err_exit "Failed to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}."
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new}."
      ls -l ./${FGS_FILE_new}
    fi
 
#-----------------------------------------------------------------------
#
# Appending Firstguess of Surface Visibility (VIS) to Firstguess File
#
#-----------------------------------------------------------------------
 
# 1. Retrieving Surface Visibility from HRRR forecast (grib2 file)
#    and dumping out to grib2 file
   found_visges=no
      PRE_YYYYMMDD=$(echo ${PDYHH_AK} | cut -c1-8)
      PRE_HH=$(echo ${PDYHH_AK} | cut -c9-10)
      hrrr_guess_grb2=${COMINhrrr}/hrrr.${PRE_YYYYMMDD}/${dom}/hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2

      if [[ -f ${hrrr_guess_grb2} ]] ; then 
         echo "found HRRR-AK ${ind} hour forecast grib2 file ${hrrr_guess_grb2} and retrieve Surface Visibility from it: "
         cpreq ${hrrr_guess_grb2}   ./hrrr_guess.grib2
         if [ $ind == 0 ]; then
            FHH_string=":VIS:surface:anl:"
         else
            FHH_string=":VIS:surface:$ind hour fcst:"
         fi
# Change max from 90000 m to 16000 m
         wgrib2 ./hrrr_guess.grib2 -match "${FHH_string}" -rpn "16000:min" -grib_out ./vis.guess.grib2
         export err=$?; err_chk

         found_visges=yes
         echo "export vishrrrCYCLE=$PDYHH_AK" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         echo "export vishrrrFHH=$ind" >> $COMOUT/${RUN}ak.t${cyc}z.envir.sh
         echo "export vishrrrfgs=hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2" >> rerun_info.txt

      else
         found_visges=no
      fi

   if [[ "${found_visges}" == "no" ]] ; then
      err_exit "Could NOT find any HRRR 0~3 hours forecast grib2 file to \
                provide firstguess for surface visibility.  exit with error.  "
   fi
#
# 2. Appending surface visibility to firstguess (netcdf format)
    keyword_data="vis"
    varname_grb2ncf="VIS_surface"
    varname_ncf="VIS"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height;
    elif [[ "${keyword_data}" == "vis" ]] ; then
      FillValue=90000.00                  # 90000 for visibility;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

    data_grb2="vis.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

    FGS_FILE="${RUN}ak.t${cyc}z.firstguess.nc"
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    wgrib2 ./${data_grb2} -netcdf ./${data_ncf}

    if [ ! -f ./${data_ncf} ] ; then
      err_exit " 'wgrib2' failed to convert  ./${data_grb2} to ./${data_ncf}"
    fi

# step b. pre-processing the VIS-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., VIS_surface --> VIS) and some attributes
    cp -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
    ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v ${varname_grb2ncf},${varname_ncf}  ./${data_ncf_new}

    ncatted  -h -O -a _FillValue,${varname_ncf},o,f,${FillValue}                ./${data_ncf_new}

    ncatted  -h -O -a coordinates,${varname_ncf},o,c,"XLONG XLAT XTIME"         ./${data_ncf_new}
    ncatted  -h -O -a stagger,${varname_ncf},c,c,""                             ./${data_ncf_new}
    ncatted  -h -O -a units,${varname_ncf},o,c,"M"                              ./${data_ncf_new}
    ncatted  -h -O -a MemoryOrder,${varname_ncf},c,c,"XY "                      ./${data_ncf_new}
    ncatted  -h -O -a FieldType,${varname_ncf},c,l,"104"                        ./${data_ncf_new}
    ncatted  -h -O -a description,${varname_ncf},c,c,"Surface Visibility (Vis)" ./${data_ncf_new}

#   ii) fetching out only the required VIS data and writing to a new netcdf file
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}

# step c. appending netcdf data into the firstguess data file (in netcdf format)
    ls -l ./${FGS_FILE_new}

    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}

    if [ $? -ne 0 ] ; then
      err_exit "Failed to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}."
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new} "
      ls -l ./${FGS_FILE_new}
    fi
#
#-----------------------------------------------------------------------
#
#  Copy files to COM directory
#
#-----------------------------------------------------------------------
if [ $SENDCOM = YES ]; then
  echo "PREPFGS: Saving the Firstguess of Cycle ${CDATE} --> $COMOUT/${RUN}.t${cyc}z.firstguess.nc"
  cat $COMOUT/${RUN}ak.t${cyc}z.envir.sh rerun_info.txt >> $COMOUT/${RUN}ak.t${cyc}z.rerun_info.txt
  cpreq ${RUN}ak.t${cyc}z.firstguess.nc    $COMOUT
  cpreq $GESINhrrr/${probe_hrrr_guess_nc} $COMOUT/${RUN}ak.t${cyc}z.hrrrak_${hrrrCYCLE}f0${hrrrFHH}
  wgrib2 $COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2 \
         -match "HTSGW" -grib $COMOUT/${RUN}ak.t${cyc}z.gfs.${ww3PDY}_gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2_htsgw
  wgrib2 $COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2 \
         -match "HTSGW" -grib $COMOUT/${RUN}ak.t${cyc}z.gfs.${ww3PDY}_gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2_htsgw
  cpreq ww3.guess.grib2 $COMOUT/${RUN}ak.t${cyc}z.fgs.howv.grib2
  cpreq gust.guess.grib2 $COMOUT/${RUN}ak.t${cyc}z.fgs.gust.grib2
  cpreq vis.guess.grib2 $COMOUT/${RUN}ak.t${cyc}z.fgs.vis.grib2
fi

########################################################
postmsg "$0 of $job completed normally"
################## END OF SCRIPT #######################
