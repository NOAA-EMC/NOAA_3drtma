#!/bin/ksh --login

# Make sure we are using GMT time zone for time computations
# export NCL_VER=6.1.2  # for testing
# export DATAROOT="/home/rtrr/hrrr"  # for testing
# export FCST_TIME=3  # for testing
# export START_TIME=2014111719  # for testing
export TZ="GMT"
export NCARG_ROOT="/apps/ncl/${NCL_VER}"
export NCARG_LIB="/apps/ncl/${NCL_VER}/lib"
export NCL_HOME="/whome/Brian.D.Jamison/fim/svncode/ncl/fimall"
export UDUNITS2_XML_PATH=$NCARG_ROOT/lib/ncarg/udunits/udunits2.xml

# Set up paths to shell commands
LS=/bin/ls
LN=/bin/ln
RM=/bin/rm
MKDIR=/bin/mkdir
CP=/bin/cp
MV=/bin/mv
ECHO=/bin/echo
CAT=/bin/cat
GREP=/bin/grep
CUT=/bin/cut
AWK="/bin/gawk --posix"
SED=/bin/sed
DATE=/bin/date
BC=/usr/bin/bc
XARGS=${XARGS:-/usr/bin/xargs}
BASH=${BASH:-/bin/bash}
NCL=${NCARG_ROOT}/bin/ncl
CTRANS=${NCARG_ROOT}/bin/ctrans
PS2PDF=/usr/bin/ps2pdf
CONVERT=`which convert`
MONTAGE=`which montage`
PATH=${NCARG_ROOT}/bin:${PATH}

typeset -RZ2 FCST_TIME
typeset -RZ2 FCST_TIME_AHEAD1
typeset -RZ2 FCST_TIME_AHEAD2
typeset -RZ2 FCST_TIME_BACK1

ulimit -s 512000

set -A ncgms  sfc_temp   \
              2m_temp    \
              2m_ptemp   \
              2m_dewp    \
              2m_rh      \
              2ds_temp   \
              10m_wind   \
              80m_wind   \
              80m_wchg   \
              850_wind   \
              250_wind   \
              ua_vort    \
              sfc_pwtr   \
              sfc_totp   \
              sfc_cref   \
              sfc_ptyp   \
              sfc_cape   \
              sfc_cin    \
              sfc_acp    \
              sfc_weasd  \
              sfc_1hsnw  \
              sfc_acsnw  \
              sfc_acpcp  \
              sfc_sfcp   \
              sfc_hpbl   \
              ua_rh      \
              ua_rh8     \
              sfc_rhpw   \
              ua_vvel    \
              sfc_vis    \
              ua_ceil    \
              ua_ctop    \
              max_wind   \
              10m_gust   \
              mdn_wind   \
              mup_wind   \
              esbl_hlcy  \
              esbl_cref  \
              esbl_wchg  \
              esbl_acp   \
              esblmn_acp \
              esbl_hvyacp

set -A monpngs montage.png

set -A webpfx temp temp ptemp dewp rh temp wind wind wchg wind wind vort pwtr totp cref \
              ptyp cape cin 3hap weasd 1hsnw acsnw acpcp sfcp hpbl rh rh rhpw vvel vis ceil ctop \
              wind gust wind wind hlcy cref wchg acp acp hvyacp

set -A fhr 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15

set -A websfx sfc 2m 2m 2m 2m 2ds 10m 80m 80m 850 250 500 sfc sfc sfc sfc sfc sfc sfc sfc \
              sfc sfc sfc sfc sfc 500 850 sfc 700 sfc ua ua max 10m mdn mup esbl esbl esbl esbl \
              esblmn esbl

set -A tiles dum t1 t2 t3 t4 t5 t6 t7 t8 z0 z1 z2 z3 z4

set -A webmon montage

i=0
p=0
while [ ${i} -lt ${#ncgms[@]} ]; do
  j=0 
  numtiles=${#tiles[@]}
  (( numtiles=numtiles - 1 ))
  while [ ${j} -le ${numtiles} ]; do
    pngs[${p}]=${ncgms[${i}]}-${j}.png
    # echo ${pngs[${p}]}
    if [ ${j} -eq 0 ]; then
      if [ "${websfx[${i}]}" = "ua" ]; then
        webnames[${p}]=${webpfx[${i}]}
      else
        webnames[${p}]=${webpfx[${i}]}_${websfx[${i}]}
      fi  
    else
      if [ "${websfx[${i}]}" = "ua" ]; then
        webnames[${p}]=${webpfx[${i}]}_${tiles[${j}]}
      else
        webnames[${p}]=${webpfx[${i}]}_${tiles[${j}]}${websfx[${i}]}
      fi  
    fi  
    # echo ${webnames[${p}]}
    (( j=j + 1 ))
# p is total number of images (image index)
    (( p=p + 1 ))
  done
  (( i=i + 1 ))
done

ncl_error=0

DATAHOME=/home/rtrr/hrrr/2015060312
FCST_TIME=00

cd ${DATAHOME}/nclprd/00part1
ls *.png

i=0
# Convert the .ras files into .png files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting convert for ${plot}.ras at `${DATE}`"

  if [ -s ${plot}.ras ]; then 
# normal image
    ${CONVERT} -colors 128 -trim -border 25x25 -bordercolor black ${plot}.ras ${plot}.png
    error=$?
    if [ ${error} -ne 0 ]; then
      ${ECHO} "ERROR: convert ${plot}.ras crashed!  Exit status=${error}"
      ncl_error=${error}
    fi
  else 
    ${ECHO} "No file to convert, exit gracefully"
    ncl_error=0
  fi  
  ${ECHO} "Finished convert for ${plot}.ras at `${DATE}`"

  (( i=i + 1 ))
  
done

# Copy png files to their proper names
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
  ${ECHO} ${i}
  pngfile=${pngs[${i}]}
  fulldir=${DATAHOME}/nclprd/full
  ${MKDIR} -p ${fulldir}
  webfile=${fulldir}/${webnames[${i}]}_f${FCST_TIME}.png
#  webfile=${webnames[${i}]}_f${FCST_TIME}.png    # for testing
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t1dir=${DATAHOME}/nclprd/t1
  ${MKDIR} -p ${t1dir}
  webfile=${t1dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t2dir=${DATAHOME}/nclprd/t2
  ${MKDIR} -p ${t2dir}
  webfile=${t2dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t3dir=${DATAHOME}/nclprd/t3
  ${MKDIR} -p ${t3dir}
  webfile=${t3dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t4dir=${DATAHOME}/nclprd/t4
  ${MKDIR} -p ${t4dir}
  webfile=${t4dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t5dir=${DATAHOME}/nclprd/t5
  ${MKDIR} -p ${t5dir}
  webfile=${t5dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t6dir=${DATAHOME}/nclprd/t6
  ${MKDIR} -p ${t6dir}
  webfile=${t6dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t7dir=${DATAHOME}/nclprd/t7
  ${MKDIR} -p ${t7dir}
  webfile=${t7dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  t8dir=${DATAHOME}/nclprd/t8
  ${MKDIR} -p ${t8dir}
  webfile=${t8dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  z0dir=${DATAHOME}/nclprd/z0
  ${MKDIR} -p ${z0dir}
  webfile=${z0dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  z1dir=${DATAHOME}/nclprd/z1
  ${MKDIR} -p ${z1dir}
  webfile=${z1dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  z2dir=${DATAHOME}/nclprd/z2
  ${MKDIR} -p ${z2dir}
  webfile=${z2dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  z3dir=${DATAHOME}/nclprd/z3
  ${MKDIR} -p ${z3dir}
  webfile=${z3dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  z4dir=${DATAHOME}/nclprd/z4
  ${MKDIR} -p ${z4dir}
  webfile=${z4dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
done

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
