#!/bin/ksh 
set -x

# option for netcdf-format obs diag file
RUN_NCDIAG=${RUN_NCDIAG:-"Yes"} # netcdf format obs-diag file (default: Yes)
if [[ "${RUN_NCDIAG}" =~ [YyTt] ]]  ; then

#========================================================================================#
#  creating command file for CFP on wcoss2

  listall_conv_nc4="uv t q ps gust vis howv"

  icount=0

  loops="01 02 03"
  pgm="ncdiag_cat_serial.x"
  for loop in $loops; do

    case $loop in
       01) string=ges;;
       03) string=anl;;
       *) string=$loop;;
    esac

    for type in $listall_conv_nc4; do
      count=`ls ${DATA_SHARED}/pe*.conv_${type}_${loop}.nc4 | wc -l`
      if [[ $count -gt 0 ]]; then
#        take out the small size files (<1k, no data inside) and remove them
#        Then no warning message when running ncdiag_cat. 
#        But even running with the small files, the results are same.
        find ${DATA_SHARED}/ -type f -name "pe*.conv_${type}_${loop}.nc4" -size 1k -delete
        echo "$pgm -o ${DATA}/${RUN}.t${cyc}z.diag_${type}_${string}.nc4 ${DATA_SHARED}/pe*.conv_${type}_${loop}.nc4" >> ${DATA}/ncdiag_cmdfile
        icount=$((icount + 1))
      fi
    done
  done

#    Execute the command file with CFP
  export CMDFILE=${DATA}/ncdiag_cmdfile
  command="mpiexec -np 21 --cpu-bind verbose,core cfp $CMDFILE >>$pgmout 2>errfile"
  $command
  export err=$?; err_chk
  echo "using cfp to run ncdiag COMPLETED"
  
#    Saving combined nc4 obs-diag files to COM and compressing them with gzip to save space
     chgrp rstprod *diag*nc4
     tar -czvf ${COMOUT}/${RUN}.t${cyc}z.diag.nc4.tgz *diag*nc4  # compressed tarball
     chgrp rstprod ${COMOUT}/${RUN}.t${cyc}z.diag.nc4.tgz 
else

  echo "RUN_NCDIAG is set to be $RUN_NCDIAG, so no running of concatenation of nc4 obs-diag files for this cycle ${CDATE}"
fi

postmsg "$0 of $job completed normally"
