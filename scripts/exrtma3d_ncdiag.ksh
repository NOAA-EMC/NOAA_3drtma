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

# Set the name to the executable of ncdiag (default: using serial code)
  export exefile_name_ncdiag=${exefile_name_ncdiag:-"ncdiag_cat_serial.x"}

if [ "${envir}" == "lsf" ] || [ "${envir}" == "pbspro" ]; then

# make sure executable exists
  if [ -n ${ncdiag_VERSION} ] && [ -d ${ncdiag_ROOT} ] ; then  # module ncdiag was loaded successfully on wcoss2
    PATH_to_NCDIAG="${ncdiag_ROOT}/bin"                        # path to ncdiag exe already added into $PATH
    nc_diag_cat=${PATH_to_NCDIAG}/${exefile_name_ncdiag}
    echo "using ${nc_diag_cat} to concatenate nc4 obs-diag files ..."
  elif [ -f ${EXECrtma3d}/ncdiag_cat_serial.x ] ; then
    nc_diag_cat=${EXECrtma3d}/ncdiag_cat_serial.x
    echo "using ${nc_diag_cat} to concatenate nc4 obs-diag files ..."
  else
    ${ECHO} "ERROR: executable to concatenate netcdf obs-diag file '${exefile_name_gsi}' could not be found! ncdiag job abort ..."
    err_exit
  fi

fi

# Directory where the original not-comnbined nc4 obs-diag files are saved
#           (default ==> GSI RUNING/WORKING directory)
  DATAGSI=${DATAGSIHOME:-"../gsiprd"}

# Compute date & time components for the analysis time
  subcyc=${subcyc:-"00"}
  START_TIME=`${DATE} -d "${PDY} ${cyc} ${subcyc} minutes"`
  YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
  YYYYMMDDHHMM=`${DATE} +"%Y%m%d%H%M" -d "${START_TIME}"`
  cycle_str=${PDY}${cyc}

#----- enter working directory -------
  cd ${DATA}
  ${ECHO} "enter working directory:${DATA}"
  rm -rf ${DATA}/*

# option for netcdf-format obs diag file
  RUN_NCDIAG=${RUN_NCDIAG:-"Yes"} # netcdf format obs-diag file (default: Yes)
  if [[ "${RUN_NCDIAG}" =~ [YfTt] ]]  ; then

#========================================================================================#
#    searching for the max outer-loop
     maxlimit=51
     imax=1
     found_loopend=no
     loopend="01"
     loops=""
     while [[ $imax -ge 1 ]] && [[ $found_loopend =~ [NnFf] ]] && [[ $imax -le $maxlimit ]]
     do
        ima2=`printf %02d $imax`
        n_found=0
#       n_found=`find ${DATAGSI}/ -type f -name "pe*.conv_t_${ima2}.nc4" | wc -l`
        n_found=`ls ${DATAGSI}/pe*.conv_t_${ima2}.nc4 | wc -l`
        if [[ "${n_found}" -gt 0 ]] ; then
           loops="$loops $ima2"
           found_loopend="no"      # keep searching
           let "imax=imax+1"
        else
           if [[ $imax -eq 1 ]] ; then
              echo "WARNING:==> Even no diag file was found for outer-loop ${imax}. Abort this ncdiag job ..."
              found_loopend="no"
              err_exit
           else
              found_loopend="yes"
              break                # jump out the search.
           fi
        fi
     done
     if [[ ${found_loopend} =~ [NnFf] ]] ; then
        echo "WARNING:==> There is diag file for outer-loop larger than $maxlimit. This is ABNORMAL. Please double check your GSI running configuration."
        err_exit
     else
        let "imax=imax-1"
        loopend=`printf %02d $imax`
        echo "Max outer-loop index = ${loopend}"
     fi
  
#========================================================================================#
#    searching for avaialble obs variables
     listall_conv="uv t q ps gust vis howv"
     listall_conv_nc4=""
     nvar=0
     for ivar in ${listall_conv}
     do
#       n_ivar=`find ${DATAGSI}/ -type f -name "pe*.conv_${ivar}_01.nc4" | wc -l`
        n_ivar=`ls ${DATAGSI}/pe*.conv_${ivar}_01.nc4 | wc -l`
        if [[ ${n_ivar} -gt 0 ]] ; then
           listall_conv_nc4="${listall_conv_nc4} ${ivar}"
#          let "nvar=nvar+1"
           nvar=$((nvar + 1 ))
        fi
     done
     echo "nc4 obs-diag files are avaible for $nvar obs ==> ${listall_conv_nc4}"

#========================================================================================#
#  creating command file for CFP on wcoss2

     rm -f ${DATA}/ncdiag_cmdfile
     icount=0

# The following lines are copied from original gsianl.ksh
#
# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#
#    loops="01 02 03"    # loops and loopend are found in the do-while block above.
     echo "outer-loops is $loops"
     for loop in $loops; do

        case $loop in
           01) string=ges;;
           $loopend) string=anl;;
           *) string=$loop;;
        esac

#  Collect diagnostic files for obs types (groups) below
#       listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"


#  binary format obs diag
#  netcdf format obs diag
        for type in $listall_conv_nc4; do
           count=`ls ${DATAGSI}/pe*.conv_${type}_${loop}.nc4 | wc -l`
           if [[ $count -gt 0 ]]; then
#              take out the small size files (<1k, no data inside) and remove them
#               Then no warning message when running ncdiag_cat. 
#               But even running with the small files, the results are same.
              find ${DATAGSI}/ -type f -name "pe*.conv_${type}_${loop}.nc4" -size 1k -delete
              echo "$nc_diag_cat -o ${DATA}/diag_${type}_${string}.${cycle_str}.${RUN}.nc4 ${DATAGSI}/pe*.conv_${type}_${loop}.nc4" >> ${DATA}/ncdiag_cmdfile
#             let "icount=icount+1"
              icount=$((icount + 1))
           fi
        done
     done

#    Execute the command file with CFP
     export exeName=cfp
     export CMDFILE=${DATA}/ncdiag_cmdfile
#    command="mpiexec -np 6 --cpu-bind verbose,core ${exeName} $CMDFILE >>$pgmout 2>errfile"
     command="mpiexec -np ${nvar} --cpu-bind verbose,core ${exeName} $CMDFILE >>$pgmout 2>errfile"
     echo $command
     $command
     export err=$?; err_chk
     echo "using $exeName to run ncdiag COMPLETED"
  
#    Saving combined nc4 obs-daig files to COM2 and compressing thme with gzip to save space
     for type in $listall_conv_nc4; do
        ${CP} -p ${DATA}/diag_${type}_*.${cycle_str}.${RUN}.nc4      ${COMOUTgsi_rtma3d}
        rm -f ${COMOUTgsi_rtma3d}/diag_${type}_*.${cycle_str}.${RUN}.nc4.gz
        gzip  ${COMOUTgsi_rtma3d}/diag_${type}_*.${cycle_str}.${RUN}.nc4
     done

  else

     echo "RUN_NCDIAG is set to be $RUN_NCDIAG, so no running of concatenation of nc4 obs-diag files for this cycle ${cycle_str}"

  fi

  exit
