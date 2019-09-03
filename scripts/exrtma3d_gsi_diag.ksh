#!/bin/ksh 

# Read conventional observation diag files

${CAT} << EOF > namelist.conv
 &iosetup
  dirname='${workdir}',
  outfilename='./diag_results',
  ndate=${YYMMDDHH},
  nloop=1,0,1,0,0,
  $iosetup
 /     
EOF

cp ${GSI_ROOT}/read_diag_conv.exe .
./read_diag_conv.exe > stdout_read_diag_conv 2>&1

# Read radiance diag file
${CAT} << EOF > namelist.rad
 &iosetup
  dirname='${workdir}',
  outfilename='./diag_results',
  ndate=${YYMMDDHH},
  nloop=1,0,1,0,0,
  instrument='amsub_n16','amsub_n17','hirs3_n17',
  $iosetup
 /
EOF

cp ${GSI_ROOT}/read_diag_rad.exe .
./read_diag_rad.exe > stdout_read_diag_rad 2>&1

#
#  Data number summary
#
cp ${GSI_ROOT}/count_obs.exe . 
./count_obs.exe 176 > stdout_count_obs 2>&1
cat obs_num_summary.txt >> ${DATABASE_DIR}/loghistory/HRRR_GSI_dataNumber.log

exit 0
