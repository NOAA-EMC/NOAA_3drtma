#!/bin/ksh

set -x 

if [ ! "${GESINhrrr_rtma3d}" ]; then
  ${ECHO} "ERROR: \$GESINhrrr_rtma3d is not defined!"
  exit 1
fi
if [ ! -d "${GESINhrrr_rtma3d}" ]; then
  ${ECHO} "ERROR: $GESINhrrr_rtma3d does not exist!"
  exit 1
fi

if [ ! "${DATA}" ]; then
  ${ECHO} "ERROR: \$DATA is not defined!"
  exit 1
fi
if [ ! -d "${DATA}" ]; then
  ${ECHO} "ERROR: $DATA does not exist!"
  exit 1
fi

if [  "${DATA_FGSPRD}" ]; then
  ${RM} -f  ${DATA_FGSPRD}
  ${LN} -sf ${DATA} ${DATA_FGSPRD}
fi

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

# No "hrrr.tHHz.wrfguess" archived from operational hrrr after 18Z of 07/11/2018.
# if [ $YYYYMMDDHH -ge "2018071118" ] && [ $FGS_OPT -eq "1"  ] ; then
#   export FGS_OPT=2
# fi

#############################################################################

# Create the working directory and cd into it
workdir=${DATA}
cd ${workdir}
time_str=`${DATE} "+%Y-%m-%d_%H_%M_%S" -d "${START_TIME}"`
${ECHO} " time_str = ${time_str}"
time_run=${time_str}
pgm=${NET}_prepfgs
. prep_step

startmsg
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
msg="  begin retrieveing howv/gust and appending them to fgs, then copy firstguess to fgsprd.${cycle}"
postmsg "$jlogfile" "$msg"
msg="***********************************************************"
postmsg "$jlogfile" "$msg"
#
CDATE=$PDY$cyc

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
#       probe_hrrr_guess_nc=$GESINhrrr/alaska/hrrrak_${hrrrCYCLE}f0${hrrrFHH}
#       probe_hrrr_guess_nc=$GESINhrrr/hrrrak_${hrrrCYCLE}f0${hrrrFHH}
        export probe_hrrr_guess_nc=hrrrak_${hrrrCYCLE}f0${hrrrFHH}
        size_match=no
        found_hrrrges=no
        if [ -s $GESINhrrr/$probe_hrrr_guess_nc ]; then
            found_hrrrges=yes

            ics=1
#           loop of checking the filesize of hrrr forecast
            while [ $ics -le ${ics_max} ] ; do
                if [ -L "$GESINhrrr/$probe_hrrr_guess_nc" ] ; then
                    realfgsfile=$(readlink -f $GESINhrrr/$probe_hrrr_guess_nc)
                else
                    realfgsfile="$GESINhrrr/$probe_hrrr_guess_nc"
                fi
                filesize=$(stat -c %s ${realfgsfile})
                if [[ ${filesize} -eq ${targetsize_hrrr} ]] ; then
                    size_match="yes"
#                   cpreq $probe_hrrr_guess_nc $COMOUT/${RUN}.t${cyc}z.hrrrak_${hrrrCYCLE}f0${hrrrFHH}
#                   cpreq $GESINhrrr/$probe_hrrr_guess_nc ${DATA}/
                    break  # breaking out the loop of checking file size
                else
                    size_match="no"
                    msg="${probe_hrrr_guess_nc} filesize (${filesize}) does not match the standard size (${targetsize_hrrr}). Sleep for 60 seconds and check again ..."
                    ${ECHO} "${msg}"
                    sleep ${sleep_time}
                fi
                let "ics=ics+1"
            done

            if [[ ${size_match} =~ [yYtT] ]] ; then
                cpreq $GESINhrrr/$probe_hrrr_guess_nc ${DATA}/${FGSrtma3d_FNAME}
                ind=$ic
                PDYHH_AK=$hrrrCYCLE
                msg="HRRR-AK ${ic}-hour forecast file ${probe_hrrr_guess_nc} is used as the firstguess for analysis cycle at ${PDY} ${cyc}Z"
                ${ECHO} "${msg}"
                postmsg "$jlogfile" "$msg"
                break      # breaking out the loop of searching for firstguess in HRRR forecast
            else
#               cpreq $GESINhrrr/$probe_hrrr_guess_nc ${DATA}/${probe_hrrr_guess_nc}.wrongfsize  # saving this problematic file for investigation later
                cpreq $GESINhrrr/$probe_hrrr_guess_nc ${GESINhrrr_rtma3d}/${probe_hrrr_guess_nc}.wrongfsize  # saving this problematic file for investigation later
                msg="HRRR-AK ${ic}-hour forecast file ${probe_hrrr_guess_nc} exists for analysis cycle at ${PDY} ${cyc}Z, but its filesize (${filesize}) does not match the standard size (${targetsize_hrrr}) even after waiting for ${ics_max} minutes. Try to search in the earlier HRRR-AK forecast files ..."
                ${ECHO} "${msg}"
                postmsg "$jlogfile" "$msg"
                SUBJECT=" ${NET} ${RUN} Warning Email: File Size of Firstguess Does Not Match for analysis cycle at ${PDY} ${cyc}Z"
                MESSAGE="WARNING: ${msg}"
                ${ECHO} "${MESSAGE}" | ${MAILX} -s "$SUBJECT" -c "${CC_RECIPIENTS}"  "${TO_RECIPIENTS}"
#               ${ECHO} "${MESSAGE}" | mail.py   #<-- using this line only when system is delivered to NCO
            fi
        else
            msg="HRRR-AK ${ic}-hour forecat file ${probe_hrrr_guess_nc} is not available for analysis cycle at ${PDY} ${cyc}Z. Try to search in the earlier HRRR-AK foreast files ... "
            ${ECHO} "${msg}"
            postmsg "$jlogfile" "$msg"
            SUBJECT=" ${NET} ${RUN} Warning Email: Missing Firstguess File for analysis cycle at ${PDY} ${cyc}Z"
            MESSAGE="WARNING: ${msg}"
            ${ECHO} "${MESSAGE}" | ${MAILX} -s "$SUBJECT" -c "${CC_RECIPIENTS}"  "${TO_RECIPIENTS}"
#           ${ECHO} "${MESSAGE}" | mail.py   #<-- using this line only when system is delivered to NCO
        fi
     fi

     let "ic=ic+1"

   done
   echo "found_hrrrges: "$found_hrrrges  "  size_match: ${size_match}"

   if [[ ${found_hrrrges} = no ]] ; then
       err_exit "No HRRR guess available. The missing files are GESINhrrr/alaska/hrrrak__${hrrrCYCLE}f0${hrrrFHH}. The script must be able to find at least one file in the above querying do-while loop"
   fi

# rm -f $COMOUT/${NET}.t${HH}z.fgs.DirectAnl2Ds.grib2  # single grib2 file with howv/gust/vis in it
#
#-----------------------------------------------------------------------
#
# Appending Firstguess of Ocean Significant Wave Height (HOWV) to Firstguess file
#
#-----------------------------------------------------------------------
RUN_HOWV=${RUN_HOWV:-"No"}
if [[ ${RUN_HOWV} =~ [TtYy] ]] ; then
#
# 1.1 define the Grid Specification for domain of 3DRTMA (used by wgrib2)
#
# grid_specs_WG2wexp: for operational RTMA/URMA
  grid_specs_WG2wexp="lambert:265.0:25.0:25.0 233.723448:2345:2539.703 19.228976:1597:2539.703"

# grid_specs_hrrr: for exp hrrr-based 3D RTMA on CONUS domain
  grid_specs_hrrr="lambert:-97.5:38.5:38.5 -122.719528:1799:3000.0 21.138123:1059:3000.0"

# grid_specs_hrrrak: for hrrr-based 3D RTMA on Alaska domain
  grid_specs_hrrrak="nps:225:60.0 185.117126:1299:3000.0 41.612949:919:3000.0"

# grid_specs_rrfsnarll: for exp RRFS-based 3D RTMA on North America domain on Rotated Latlon grid
  grid_specs_rrfsnarll="rot-ll:247.0:-35.0:0.0 299.0:4881:0.025 -37.0:2961:0.025"

# grid_specs_rrfsnapol: for exp RRFS-based 3D RTMA on North America domain on Polar Stereographic grid
  grid_specs_rrfsnapol="nps:245.0:60.0 206.5:5200:3170.0 -4.0:3268:3170.0"

  grid_specs=${grid_specs_hrrrak}
#
# 1.2  fix dir (for slmask.grib2 file)
# print_info_msg "$VERBOSE" "FIXgsi is $FIXgsi"   (print_info_msg is only available in RRFS worklfow)
  info_msg="FIXgsi is $FIXgsi"
  echo "${info_msg}"
#
#  Sea-Land Mask for the correct interpolation of the howv Background.
  rm -f ./slmask.grib2
  echo "No Sea-Land no-lakes mask file for Alaska 3-km grid domain yet."
# if [[ -f $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2 ]] ; then
#     echo "Sea-Land no-lakes mask file --> $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2"
#     cp -p $FIXgsi/hrrr_conus_3km_slmask_nolakes.grib2    ./slmask.grib2
# else
#     echo "No Sea-Land no-lakes mask file is used for Wave Height firtguess"
# fi
#
# 2. Retrieving significant wave height from WW3 forecast, re-mapping to HRRR-conus model grid,
#    then dumping out to grib2 file
# 2.1 Wave Background at Great Lakes
#
  info_msg="COMINww3GL is $COMINww3GL (Wave background from Great Lakes model)"
  echo "${info_msg}"

   found_ww3gesGL=no
   ic=0
   while [ $ic -le 23 ] ; do
      ww3FHH_GL=$ic
      ww3FHH_GL=`printf %03d $ww3FHH_GL`
      ww3CYCLE_GL=`$NDATE -$ww3FHH_GL $CDATEymdh`
      ww3PDY_GL=`echo $ww3CYCLE_GL |cut -c1-8`
      ww3CC_GL=`echo $ww3CYCLE_GL |cut -c9-10`
#
      probe_ww3_GL_guess_grb2=$COMINww3GL/glwu.${ww3PDY_GL}/glwu.grlr_500m.t${ww3CC_GL}z.grib2
#
      if [ -s $probe_ww3_GL_guess_grb2 ]; then

         info_msg="found wave background for Great Lakes: $probe_ww3_GL_guess_grb2"
         echo "${info_msg}"
         cpreq $probe_ww3_GL_guess_grb2 ww3.guess5.grib2
#        cp -p $probe_ww3_GL_guess_grb2 ww3.guess5.grib2
         cp -p $probe_ww3_GL_guess_grb2 $COMOUT/glwu.grlr_500m.t${ww3CC_GL}z.grib2     # save for retro run
         if [ $ic == 0 ]; then
            FHH_st="(HTSGW:surface:anl)"
         else
            FHH_st="(HTSGW:surface:$ic hour fcst)"
         fi
         $WGRIB2 ww3.guess5.grib2 -match "${FHH_st}" -grib ww3GL.guess.grib2

         GL_InputGribmerge=' -i ww3GL.guess.grib2 '

         echo "export ww3CYCLE_GL=$ww3CYCLE_GL" >> $COMOUT/${RUN}.t${cyc}z.envir.sh
         echo "export ww3FHH_GL=$ww3FHH_GL" >> $COMOUT/${RUN}.t${cyc}z.envir.sh

         found_ww3gesGL=yes

         break
      else
         let "ic=ic+1"
      fi
   done
   if [[ ${found_ww3gesGL} = no ]] ; then
       err_exit "No WW3 guess for Great Lakes available. The missing files in the above while-do loop are of the from $COMINww3GL/glwu.${ww3PDY}/glwu.grlr_500m.t${ww3CC}z.grib2. The script must be able to find at least one file out of the 24 files that it queries"
   fi
#
# 2.2 Ocean Waves Background
   info_msg="COMINww3 is $COMINww3 (Wave background from WW3 Ocean Wave model)"
   echo "${info_msg}"
   found_ww3ges=no
   ic=0
   while [ $ic -le 24 ] ; do
      ww3FHH=$ic
      ww3FHH=`printf %03d $ww3FHH`
      ww3CYCLE=`$NDATE -$ww3FHH $CDATEymdh`
      ww3PDY=`echo $ww3CYCLE |cut -c1-8`
      ww3CC=`echo $ww3CYCLE |cut -c9-10`

#     "set -A" only works for K-Shell 
      set -A  probe_ww3_guess_grb2  "$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2" \
                                    "$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2"
#     In Bash, to create an array:
#     declare -a  probe_ww3_guess_grb2=( \
#         [0]="$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2"  \
#         [1]="$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2" \
#     )
#     or the following way to create 
#       probe_ww3_guess_grb2=("$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2"   \
#                             "$COMINww3/gfs.${ww3PDY}/${ww3CC}/wave/gridded/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2"   )

      if [ -s "${probe_ww3_guess_grb2[0]}" ] && \
         [ -s "${probe_ww3_guess_grb2[1]}" ]    ; then

         info_msg="found wave background for Arctic: ${probe_ww3_guess_grb2[0]}"
         echo "${info_msg}"
         info_msg="found wave background for Global: ${probe_ww3_guess_grb2[1]}"
         echo "${info_msg}"
         cpreq ${probe_ww3_guess_grb2[0]} ww3.guess0.grib2
#        cp -p ${probe_ww3_guess_grb2[0]} ww3.guess0.grib2
         cpreq ${probe_ww3_guess_grb2[1]} ww3.guess1.grib2
#        cp -p ${probe_ww3_guess_grb2[1]} ww3.guess1.grib2

         # save gfswave forecast files to $COMOUT
         cp -p ${probe_ww3_guess_grb2[0]} $COMOUT/gfswave.t${ww3CC}z.arctic.9km.f${ww3FHH}.grib2     # save for retro run
         cp -p ${probe_ww3_guess_grb2[1]} $COMOUT/gfswave.t${ww3CC}z.global.0p16.f${ww3FHH}.grib2     # save for retro run

#        Alaska domain does NOT need Great Lakes data (if use it, code crashes.)
#        ${HOMEscript}/exrtma3d_GribMerge_urma.sh ${GL_InputGribmerge} -i ww3.guess0.grib2 -i ww3.guess1.grib2 \
         ${HOMEscript}/exrtma3d_GribMerge_urma.sh                      -i ww3.guess0.grib2 -i ww3.guess1.grib2 \
                        -v HTSGW -g "${grid_specs}" \
                        -m slmask.grib2 \
                        -o ww3.guess.grib2
 
         echo "export ww3CYCLE=$ww3CYCLE" >> $COMOUT/${RUN}.t${cyc}z.envir.sh
         echo "export ww3FHH=$ww3FHH" >> $COMOUT/${RUN}.t${cyc}z.envir.sh
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

   # save the firstguess grib2 file to $COMOUT
#  cp -p ww3.guess.grib2 $COMOUT/${RUN}.t${ww3CC}z.fgs.howv.f${ww3FHH}.grib2       # forecast time saved in name of firstguess file
   cp -p ww3.guess.grib2 $COMOUT/${RUN}.t${HH}z.fgs.howv.grib2                     # analysis time saved in name of firstguess file
#  wgrib2 ww3.guess.grib2 -append -grib $COMOUT/${RUN}.t${HH}z.fgs.DirectAnl2Ds.grib2  # single grib2 file

# 3. Appending Wave height (2-D) field to 3D-RTMA firstguess file (netcdf format)

    keyword_data="howv"
    keyword_howv="howv"
    varname_grb2ncf="HTSGW_surface"
    varname_ncf="HOWV"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height; -9999.0 for other variables;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

    data_grb2="ww3.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

#   DATDIR_FGS="${GESINhrrr_rtma3d}"
    DATDIR_FGS="./"
#   FGS_FILE="hrrr.t${F_HOUR}00z.f0100.netcdf"          # old naming rule for 3drtma fgs from hrrr forecast
    FGS_FILE="${FGSrtma3d_FNAME}"                       #<-- ${NET}.${cycle}.firstguess.nc
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
#   FGS_FILE_new="${FGS_FILE_basename}.${keyword_data}.netcdf"
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    DATDIR_HOWV="./"
    if [ ! -f ${DATDIR_HOWV}/${data_grb2} ] ; then
      echo " Cannot find grib2 file for ${keyword_data}:  ${DATDIR_HOWV}/${data_grb2}, job aborted ..."
      exit 1
    fi
    rm -f ./${data_ncf}
    $WGRIB2 ./${data_grb2} -netcdf ./${data_ncf}
    if [ ! -f ./${data_ncf} ] ; then
      echo " '$WGRIB2' failed to convert  ./${data_grb2} to ./${data_ncf}"
      exit 2
    fi

# step b. pre-processing the ww3-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., HTSGW_surface --> HOWV) and some attributes
    rm -f ./${data_ncf_new}
    cp -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
#   ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v HTSGW_surface,HOWV  ./${data_ncf_new}
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
    rm -f ./${data_ONLY_ncf}
#    ncks -h -C -3/4/5/6/7/? -v HOWV           ./${data_ncf_new} -o ./${data_ONLY_ncf}
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}
  
# step c. appending netcdf data into the firstguess data file (in netcdf format)
    if [ ! -f ${DATDIR_FGS}/${FGS_FILE} ] ; then
      echo "Cannot find hrrr firstguess file: ${DATDIR_FGS}/${FGS_FILE}, job aborted ..."
      exit 3
    fi
    ls -l ./${FGS_FILE_new}

#   set -x
#   ncks -A -v HOWV           ./${data_ONLY_ncf} ./${FGS_FILE_new}
    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}
#   set +x

    if [ $? -ne 0 ] ; then
      echo "Failled to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}. Exit abnormally   "
      exit 4
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new} and updated to the fgs file under directory ${DATDIR_FGS}, and Check the file size: ? "
      ls -l ./${FGS_FILE_new}   ${DATDIR_FGS}/${FGS_FILE}
    fi

fi     # RUN_HOWV=True/true/Yes/yes, then retrieving fgs of howv
#
#-----------------------------------------------------------------------
#
# Appending Firstguess of 10-meter Wind Gust (GUST) to Firstguess File
#
#-----------------------------------------------------------------------
#
# 1. Retrieving Wind Gust from HRRR forecast (grib2 file)
#    and dumping out to grib2 file
   found_gustges=no
#  ic=0
#  while [ $ic -le 3 ] ; do
#     PRE_YYYYMMDDHH=$(date +"%Y%m%d%H" -d "${START_TIME} ${ic} hour ago")
#     PRE_YYYYMMDD=$(echo ${PRE_YYYYMMDDHH} | cut -c1-8)
#     PRE_HH=$(echo ${PRE_YYYYMMDDHH} | cut -c9-10)
#     ic3=$(printf %03d ${ic})
#     ic2=$(printf %02d ${ic})

#     hrrr_guess_grb2=${COMINHRRR}/hrrr.${PRE_YYYYMMDD}/alaska/hrrr.t${PRE_HH}z.wrfprsf${ic2}.ak.grib2    # wrfprs; wrfnat; wrfsfc;
      PRE_YYYYMMDD=$(echo ${PDYHH_AK} | cut -c1-8)
      PRE_HH=$(echo ${PDYHH_AK} | cut -c9-10)
      hrrr_guess_grb2=${COMINHRRR}/hrrr.${PRE_YYYYMMDD}/alaska/hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2    # wrfprs; wrfnat; wrfsfc;

      if [[ -f ${hrrr_guess_grb2} ]] ; then 
         info_msg="found HRRR-AK ${ind} hour forecast (from ${PRE_YYYYMMDD}_${PRE_HH}Z) grib2 file ${hrrr_guess_grb2} and retrieve 10-m Wind Gust from it: "
         echo "${info_msg}"
         rm -f ./hrrr_guess.grib2
         ln -sf ${hrrr_guess_grb2}   ./hrrr_guess.grib2
         if [ $ind == 0 ]; then
            FHH_string=":GUST:surface:anl:"
         else
            FHH_string=":GUST:surface:$ind hour fcst:"
         fi
         # wgrib2 ./hrrr_guess.grib2 | grep "GUST" | wgrib2 -i ./hrrr_guess.grib2 -grib ./gust.guess.grib2
         # wgrib2 ./hrrr_guess.grib2 -match ":GUST:surface" -grib ./gust.guess.grib2
         wgrib2 ./hrrr_guess.grib2 -match "${FHH_string}" -grib ./gust.guess.grib2
         export err=$?; err_chk
         # save the firstguess grib2 file to $COMOUT
#        cp -p ./gust.guess.grib2 $COMOUT/${RUN}.t${PRE_HH}z.fgs.gust.f0${ind}.grib2       # forecast time saved in name of firstguess file
         cp -p ./gust.guess.grib2 $COMOUT/${RUN}.t${HH}z.fgs.gust.grib2                   # analysis time saved in name of firstguess file
#        wgrib2 gust.guess.grib2 -append -grib $COMOUT/${RUN}.t${HH}z.fgs.DirectAnl2Ds.grib2  # single grib2 file

         found_gustges=yes

#        break
      else
#        let "ic=ic+1"
         found_gustges=no
      fi
#  done
   if [[ "${found_gustges}" == "no" ]] ; then
      err_exit "Could NOT find any HRRR 0~3 hours forecast grib2 file to \
                provide firstguess for 10-m wind gust.  exit with error.  "
   fi
#
# 2. Appending wind gust to firstguess (netcdf format)
    keyword_data="gust"
    keyword_gust="gust"
    varname_grb2ncf="GUST_surface"
#   varname_ncf=$(echo ${keyword_data} | tr '[:lower:]' '[:upper:]')       # standard POSIX way with tr
#   varname_ncf=$(echo ${keyword_data} | awk '{print toupper($0)}')        # standard POSIX way with awk
    varname_ncf="GUST"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height; -9999.0 for other variables;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

#   data_grb2="hrrr.t23z.gust.surf.f001.grib2"
    data_grb2="gust.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
#   data_ONLY_ncf="${keyword_data}.guess.GUST.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

#   DATDIR_FGS="${GESINhrrr_rtma3d}"
    DATDIR_FGS="./"
    FGS_FILE="${FGSrtma3d_FNAME}"                       #<-- ${NET}.${cycle}.firstguess.nc
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    DATDIR_GUST="./"
    if [ ! -f ${DATDIR_GUST}/${data_grb2} ] ; then
      echo " Cannot find grib2 file for ${keyword_data} :  ${DATDIR_GUST}/${data_grb2}, job aborted ..."
      exit 1
    fi
    rm -f ./${data_ncf}
    $WGRIB2 ./${data_grb2} -netcdf ./${data_ncf}
    if [ ! -f ./${data_ncf} ] ; then
      echo " '$WGRIB2' failed to convert  ./${data_grb2} to ./${data_ncf}"
      exit 2
    fi

# step b. pre-processing the GUST-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., GUST_surface --> GUST) and some attributes
    rm -f ./${data_ncf_new}
    cp -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
#   ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v GUST_surface,GUST  ./${data_ncf_new}
    ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v ${varname_grb2ncf},${varname_ncf}  ./${data_ncf_new}

#   Do not set the filled value (undefined value)
#   set the undefined value = -9999.0
    ncatted  -h -O -a _FillValue,${varname_ncf},o,f,${FillValue} ./${data_ncf_new}

    ncatted  -h -O -a coordinates,${varname_ncf},o,c,"XLONG XLAT XTIME"         ./${data_ncf_new}
    ncatted  -h -O -a stagger,${varname_ncf},c,c,""                             ./${data_ncf_new}
    ncatted  -h -O -a units,${varname_ncf},o,c,"M/S"                            ./${data_ncf_new}
    ncatted  -h -O -a MemoryOrder,${varname_ncf},c,c,"XY "   ./${data_ncf_new}
    ncatted  -h -O -a FieldType,${varname_ncf},c,l,"104"   ./${data_ncf_new}
    ncatted  -h -O -a description,${varname_ncf},c,c,"GUST Wind Speed (Gust)"   ./${data_ncf_new}

#   ii) fetching out only the required GUST data and writing to a new netcdf file
    rm -f ./${data_ONLY_ncf}
#    ncks -h -C -3/4/5/6/7/?  -v GUST ./${data_ncf_new} -o ./${data_ONLY_ncf}
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}

# step c. appending netcdf data into the firstguess data file (in netcdf format)
    if [ ! -f ${DATDIR_FGS}/${FGS_FILE} ] ; then
      echo "Cannot find hrrr firstguess file: ${DATDIR_FGS}/${FGS_FILE}, job aborted ..."
      exit 3
    fi
    ls -l ./${FGS_FILE_new}

#   set -x
#   ncks -A -v GUST ./${data_ONLY_ncf} ./${FGS_FILE_new}
    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}
#   set +x

    if [ $? -ne 0 ] ; then
      echo "Failled to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}. Exit abnormally   "
      exit 4
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new} and updated to the fgs file under directory ${DATDIR_FGS}, and Check the file size: ? "
      ls -l ./${FGS_FILE_new}   ${DATDIR_FGS}/${FGS_FILE}
    fi
#
#-----------------------------------------------------------------------
#
# Appending Firstguess of Surface Visibility (VIS) to Firstguess File
#
#-----------------------------------------------------------------------
#
# 1. Retrieving Surface Visibility from HRRR forecast (grib2 file)
#    and dumping out to grib2 file
   found_visges=no
#  ic=0
#  while [ $ic -le 3 ] ; do
#     PRE_YYYYMMDDHH=$(date +"%Y%m%d%H" -d "${START_TIME} ${ic} hour ago")
#     PRE_YYYYMMDD=$(echo ${PRE_YYYYMMDDHH} | cut -c1-8)
#     PRE_HH=$(echo ${PRE_YYYYMMDDHH} | cut -c9-10)
#     ic3=$(printf %03d ${ic})
#     ic2=$(printf %02d ${ic})

#     hrrr_guess_grb2=${COMINHRRR}/hrrr.${PRE_YYYYMMDD}/alaska/hrrr.t${PRE_HH}z.wrfprsf${ic2}.ak.grib2    # wrfprs; wrfnat; wrfsfc;
      PRE_YYYYMMDD=$(echo ${PDYHH_AK} | cut -c1-8)
      PRE_HH=$(echo ${PDYHH_AK} | cut -c9-10)
      hrrr_guess_grb2=${COMINHRRR}/hrrr.${PRE_YYYYMMDD}/alaska/hrrr.t${PRE_HH}z.wrfprsf0${ind}.ak.grib2    # wrfprs; wrfnat; wrfsfc;

      if [[ -f ${hrrr_guess_grb2} ]] ; then 
         info_msg="found HRRR-AK ${ind} hour forecast (from ${PRE_YYYYMMDD}_${PRE_HH}Z) grib2 file ${hrrr_guess_grb2} and retrieve Surface Visibility from it: "
         echo "${info_msg}"
         rm -f ./hrrr_guess.grib2
         ln -sf ${hrrr_guess_grb2}   ./hrrr_guess.grib2
         if [ $ind == 0 ]; then
            FHH_string=":VIS:surface:anl:"
         else
            FHH_string=":VIS:surface:$ind hour fcst:"
         fi
         # wgrib2 ./hrrr_guess.grib2 | grep "VIS" | wgrib2 -i ./hrrr_guess.grib2 -grib ./vis.guess.grib2
         # wgrib2 ./hrrr_guess.grib2 -match ":VIS:surface" -grib ./vis.guess.grib2
         # wgrib2 ./hrrr_guess.grib2 -match "${FHH_string}" -grib ./vis.guess.grib2
         wgrib2 ./hrrr_guess.grib2 -match "${FHH_string}" -rpn "16000:min" -grib_out ./vis.guess.grib2
         export err=$?; err_chk
         # save the firstguess grib2 file to $COMOUT
#        cp -p ./vis.guess.grib2 $COMOUT/${RUN}.t${PRE_HH}z.fgs.vis.f0${ind}.grib2       # forecast time saved in name of firstguess file
         cp -p ./vis.guess.grib2 $COMOUT/${RUN}.t${HH}z.fgs.vis.grib2                   # analysis time saved in name of firstguess file
#        wgrib2  vis.guess.grib2 -append -grib $COMOUT/${RUN}.t${HH}z.fgs.DirectAnl2Ds.grib2  # single grib2 file

         found_visges=yes

#        break
      else
#        let "ic=ic+1"
         found_visges=no
      fi
#  done
   if [[ "${found_visges}" == "no" ]] ; then
      err_exit "Could NOT find any HRRR 0~3 hours forecast grib2 file to \
                provide firstguess for surface visibility.  exit with error.  "
   fi
#
# 2. Appending surface visibility to firstguess (netcdf format)
    keyword_data="vis"
    keyword_vis="vis"
    varname_grb2ncf="VIS_surface"
#   varname_ncf=$(echo ${keyword_data} | tr '[:lower:]' '[:upper:]')       # standard POSIX way with tr
#   varname_ncf=$(echo ${keyword_data} | awk '{print toupper($0)}')        # standard POSIX way with awk
    varname_ncf="VIS"

    if [[ "${keyword_data}" == "howv" ]] ; then
      FillValue=-0.01                     # -0.01 for wave height;
    elif [[ "${keyword_data}" == "vis" ]] ; then
      FillValue=90000.00                  # 90000 for visibility;
    else
      FillValue=-9999.00                  # -0.01 for wave height; -9999.0 for other variables;
    fi

#   data_grb2="hrrr.t23z.vis.surf.f001.grib2"
    data_grb2="vis.guess.grib2"
    data_ncf="${keyword_data}.guess.nc"
    data_ncf_new="${keyword_data}.guess.new.nc"
#   data_ONLY_ncf="${keyword_data}.guess.VIS.nc"
    data_ONLY_ncf="${keyword_data}.guess.${keyword_data}.nc"

#   DATDIR_FGS="${GESINhrrr_rtma3d}"
    DATDIR_FGS="./"
    FGS_FILE="${FGSrtma3d_FNAME}"                       #<-- ${NET}.${cycle}.firstguess.nc
    FGS_FILE_basename=$(basename ${FGS_FILE} ".nc")
    FGS_FILE_new="${FGS_FILE}"

    echo " retrieving ${keyword_data} from grib2 (${data_grb2}) and appending it to firstguess (${FGS_FILE}) "

# step a. convert grib2 data to netcdf data
    DATDIR_VIS="./"
    if [ ! -f ${DATDIR_VIS}/${data_grb2} ] ; then
      echo " Cannot find grib2 file for ${keyword_data} :  ${DATDIR_VIS}/${data_grb2}, job aborted ..."
      exit 1
    fi
    rm -f ./${data_ncf}
    $WGRIB2 ./${data_grb2} -netcdf ./${data_ncf}
    if [ ! -f ./${data_ncf} ] ; then
      echo " '$WGRIB2' failed to convert  ./${data_grb2} to ./${data_ncf}"
      exit 2
    fi

# step b. pre-processing the VIS-guess netdf data before appending to fgs netcdf file
#   i) renaming some variables (e.g., VIS_surface --> VIS) and some attributes
    rm -f ./${data_ncf_new}
    cp -p ./${data_ncf} ./${data_ncf_new}
    ncrename -h -d y,south_north -d x,west_east -d time,Time               ./${data_ncf_new}
#   ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v VIS_surface,VIS  ./${data_ncf_new}
    ncrename -h -v latitude,XLAT -v longitude,XLONG -v time,XTIME -v ${varname_grb2ncf},${varname_ncf}  ./${data_ncf_new}

#   Do not set the filled value (undefined value)
#   set the undefined value = -9999.0
    ncatted  -h -O -a _FillValue,${varname_ncf},o,f,${FillValue}                ./${data_ncf_new}

    ncatted  -h -O -a coordinates,${varname_ncf},o,c,"XLONG XLAT XTIME"         ./${data_ncf_new}
    ncatted  -h -O -a stagger,${varname_ncf},c,c,""                             ./${data_ncf_new}
    ncatted  -h -O -a units,${varname_ncf},o,c,"M"                              ./${data_ncf_new}
    ncatted  -h -O -a MemoryOrder,${varname_ncf},c,c,"XY "                      ./${data_ncf_new}
    ncatted  -h -O -a FieldType,${varname_ncf},c,l,"104"                        ./${data_ncf_new}
    ncatted  -h -O -a description,${varname_ncf},c,c,"Surface Visibility (Vis)" ./${data_ncf_new}

#   ii) fetching out only the required VIS data and writing to a new netcdf file
    rm -f ./${data_ONLY_ncf}
#   ncks -h -C -3/4/5/6/7/?  -v VIS ./${data_ncf_new} -o ./${data_ONLY_ncf}
    ncks -h -C               -v ${varname_ncf} ./${data_ncf_new} -o ./${data_ONLY_ncf}

# step c. appending netcdf data into the firstguess data file (in netcdf format)
    if [ ! -f ${DATDIR_FGS}/${FGS_FILE} ] ; then
      echo "Cannot find hrrr firstguess file: ${DATDIR_FGS}/${FGS_FILE}, job aborted ..."
      exit 3
    fi
    ls -l ./${FGS_FILE_new}

#   set -x
#   ncks -A -v VIS ./${data_ONLY_ncf} ./${FGS_FILE_new}
    ncks -A -v ${varname_ncf} ./${data_ONLY_ncf} ./${FGS_FILE_new}
#   set +x

    if [ $? -ne 0 ] ; then
      echo "Failled to append ${keyword_data} in ${data_ONLY_ncf} to ${FGS_FILE_new}. Exit abnormally   "
      exit 4
    else
      echo "${keyword_data} data is appended to ${FGS_FILE_new} and updated to the fgs file under directory ${DATDIR_FGS}, and Check the file size: ? "
      ls -l ./${FGS_FILE_new}   ${DATDIR_FGS}/${FGS_FILE}
    fi
#
#-----------------------------------------------------------------------
#
#  Copy/Link the background file to cycle running directory
#
#-----------------------------------------------------------------------
    if [ -r ${DATA}/${FGSrtma3d_FNAME} ] ; then
#      ${LN} -sf ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME}     ${DATA}/${FGSrtma3d_FNAME}
       ${ECHO} "PREPFGS: Saving the Firstguess of Cycle ${YYYYMMDDHH} --> ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME} "
#      cp -p ${DATA}/${probe_hrrr_guess_nc} ${GESINhrrr_rtma3d}/     # ${DATA}/${probe_hrrr_guess_nc} does not exist
       cp -p ${DATA}/${FGSrtma3d_FNAME}     ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME}    

#      to save the disck space, removing the firstguess file under working directry (fgsprd), 
#        and making a link for the fgs file to the saved fgs file ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME}
       rm -f ${DATA}/${FGSrtma3d_FNAME}
       ${LN} -sf ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME}     ${DATA}/${FGSrtma3d_FNAME}
       ls -l ${DATA}/${FGSrtma3d_FNAME} ${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME}
    else
       ${ECHO} "ERROR: No background file under working directory for analysis at ${time_run}!!!!"
       ${ECHO} " Cycle ${YYYYMMDDHH}: PREPFGS failed because of no background" >> ${pgmout}
       exit 1
    fi

export err=$? ; err_chk

ls -l ${GESINhrrr_rtma3d} > ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}.list
${ECHO} "===========================" >> ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}.list
${ECHO} "${GESINhrrr_rtma3d}/${FGSrtma3d_FNAME} comes originally from ${GESINhrrr}/${probe_hrrr_guess_nc}" >> ${GESINhrrr_rtma3d}/fgs_data_${PDY}_${cyc}.list

exit 0
