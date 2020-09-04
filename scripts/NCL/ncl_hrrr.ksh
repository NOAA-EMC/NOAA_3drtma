#!/bin/ksh --login
##########################################################################
#
#Script Name: ncl.ksh
# 
#     Author: Christopher Harrop
#             Forecast Systems Laboratory
#             325 Broadway R/FST
#             Boulder, CO. 80305
#
#   Released: 10/30/2003
#    Version: 1.0
#    Changes: None
#
# Purpose: This script generates NCL graphics from wrf output.  
#
#               EXE_ROOT = The full path of the ncl executables
#          DATAHOME = Top level directory of wrf output and
#                          configuration data.
#             START_TIME = The cycle time to use for the initial time. 
#                          If not set, the system clock is used.
#              FCST_TIME = The two-digit forecast that is to be ncled
# 
# A short and simple "control" script could be written to call this script
# or to submit this  script to a batch queueing  system.  Such a "control" 
# script  could  also  be  used to  set the above environment variables as 
# appropriate  for  a  particular experiment.  Batch  queueing options can
# be  specified on the command  line or  as directives at  the top of this
# script.  A set of default batch queueing directives is provided.
#
##########################################################################

#DATAROOT="/mnt/lfs3/projects/nrtrr/HRRR_AK/run"
#START_TIME=2016032112
#FCST_TIME=3
#DATAHOME=${DATAROOT}/${START_TIME}  # for testing
#export NCL_VER=6.3.0  # for testing
EXE_ROOT=/misc/whome/wrfruc/bin/ncl/nclhrrrak
#export MODEL="HRRR-AK"

echo ${DATAHOME}

THREADS=16
echo "Using $THREADS thread(s) for procesing."

# Load modules
source ${MODULE_FILE}

# Make sure we are using GMT time zone for time computations
# export DATAROOT="/home/rtrr/hrrr"  # for testing
# export FCST_TIME=3  # for testing
# export START_TIME=2014111719  # for testing
export TZ="GMT"
export NCARG_ROOT="/apps/ncl/6.5.0"
export NCARG_LIB="/apps/ncl/6.5.0/lib"
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
PATH=${NCARG_ROOT}/bin:${PATH}

#typeset -RZ2 FCST_TIME
typeset -RZ2 FCST_TIME_AHEAD1
typeset -RZ2 FCST_TIME_AHEAD2
typeset -Z6 j
typeset -Z6 k

ulimit -s 512000

# Print run parameters
${ECHO}
${ECHO} "ncl.ksh started at `${DATE}`"
${ECHO}
${ECHO} "DATAROOT = ${DATAROOT}"
${ECHO} "DATAHOME = ${DATAHOME}"
${ECHO} "     EXE_ROOT = ${EXE_ROOT}"

# Check to make sure the EXE_ROOT var was specified
if [ ! -d ${EXE_ROOT} ]; then
  ${ECHO} "ERROR: EXE_ROOT, '${EXE_ROOT}', does not exist"
  exit 1
fi

# Check to make sure that the DATAHOME exists
if [ ! -d ${DATAHOME} ]; then
  ${ECHO} "ERROR: DATAHOME, '${DATAHOME}', does not exist"
  exit 1
fi
# If START_TIME is not defined, use the current time
if [ ! "${START_TIME}" ]; then
  ${ECHO} "START_TIME not defined - get from date"
  START_TIME=$( date +"%Y%m%d %H" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
else
  ${ECHO} "START_TIME defined and is ${START_TIME}"
  START_TIME=$( date +"%Y%m%d %H" -d "${START_TIME%??} ${START_TIME#????????}" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
fi

# Print out times
${ECHO} "   START_TIME = ${START_TIME}"
${ECHO} "   FCST_TIME = ${FCST_TIME}"

# Set up the work directory and cd into it
workdir=${DATAHOME}/nclprd/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Link to input file
${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME}.grib2 hrrrfile.grb
${ECHO} "hrrrfile.grb" > arw_file.txt

ls -al hrrrfile.grb

set -A ncgms  sfc_temp   \
              sfc_cape   \
              sfc_cin   \
              sfc_mucp   \
              sfc_bli   \
              sfc_lcl   \
              sfc_6kshr   \
              sfc_hlcy   \
              sfc_vig   \
              sfc_mnvv   \
              mup_wind   \
              mdn_wind   \
              sfc_ltg3   \
              2m_temp    \
              2m_ptemp    \
              2ds_temp    \
              2m_dewp    \
              2m_rh      \
              10m_wind   \
              max_wind   \
              10m_gust   \
              80m_wind   \
              80m_topowind   \
              sfc_acp   \
              sfc_totp   \
              sfc_ptyp   \
              sfc_acsnw  \
              sfc_acsnod  \
              sfc_1hsnw  \
              sfc_snod   \
              sfc_weasd  \
              sfc_cref   \
              sfc_1ref   \
              sfc_mref   \
              nta_ulwrf  \
              nta_uswrf  \
              sfc_ulwrf  \
              sfc_uswrf  \
              sfc_vbdsf  \
              sfc_vddsf  \
              sfc_shtfl  \
              sfc_lhtfl  \
              sfc_hpbl   \
              sfc_vis    \
              sfc_flru   \
              sfc_ectp   \
              ua_ceil    \
              ua_ctop    \
              sfc_tcc   \
              sfc_lcc   \
              sfc_mcc   \
              sfc_hcc   \
              sat_G113bt \
              sat_G114bt \
              sfc_sfcp   \
              sfc_pwtr   \
              sfc_rhpw   \
              925_temp   \
              925_wind   \
              925_rh     \
              850_temp   \
              850_wind   \
              850_rh     \
              700_temp   \
              700_wind   \
              700_rh     \
              700_vvel     \
              500_vort   \
              500_temp   \
              500_wind   \
              500_rh     \
              250_wind   \
              sfc_mfrp   \
              sfc_trc1   \
              int_trc1

set -A webpfx temp cape cin mucp bli lcl 6kshr hlcy vig mnvv wind wind ltg3 temp ptemp temp dewp rh wind wind gust wind topowind acp totp ptyp acsnw acsnod 1hsnw snod weasd cref 1ref mref ulwrf uswrf ulwrf uswrf vbdsf vddsf shtfl lhtfl hpbl vis flru ectp ceil ctop tcc lcc mcc hcc G113bt G114bt sfcp pwtr rhpw temp wind rh temp wind rh temp wind rh vvel vort temp wind rh wind mfrp trc1 trc1
set -A websfx sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc mup mdn sfc 2m 2m 2ds 2m 2m 10m max 10m 80m 80m sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc nta nta sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc ua ua sfc sfc sfc sfc sat sat sfc sfc sfc 925 925 925 850 850 850 700 700 700 700 500 500 500 500 250 sfc sfc int
set -A fhr 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
set -A tiles full a1 a2 a3

i=0
p=0
while [ ${i} -lt ${#ncgms[@]} ]; do
  j=000000
  k=000000
  numtiles=${#tiles[@]}
  (( numtiles=numtiles - 1 )) 
  while [ ${j} -le ${numtiles} ]; do
    (( k=j + 1 )) 
    pngs[${p}]=${ncgms[${i}]}.${k}.png
#    echo ${pngs[${p}]}
    if [ ${j} -eq 000000 ]; then 
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
#    echo ${webnames[${p}]}
    (( j=j + 1 )) 
# p is total number of images (image index)
    (( p=p + 1 )) 
  done 
  (( i=i + 1 )) 
done

ncl_error=0

# Run the NCL scripts for each plot
cp /whome/wrfruc/bin/ncl/Airpor* .
cp ${EXE_ROOT}/names_grib2.txt .
cp ${EXE_ROOT}/hrrrak_tiles_loop.ncl .
i=0
echo "FIRST While, ${#ncgms[@]} items"
CMDFN=/tmp/cmd.hrrrak.$$
${RM} -f $CMDFN

while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}

  echo ${NCL} ${EXE_ROOT}/rr_${plot}.ncl >> $CMDFN

  (( i=i + 1 ))

done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}" 
ncl_error=$?
${RM} -f $CMDFN

# Copy png files to their proper names
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
  pngfile=${pngs[${i}]}
  ${CONVERT} -colors 255 -trim ${pngfile} ${pngfile}
  fulldir=${DATAHOME}/nclprd/full
  ${MKDIR} -p ${fulldir}
  webfile=${fulldir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  ${CONVERT} -colors 255 -trim ${pngfile} ${pngfile}
  a1dir=${DATAHOME}/nclprd/a1
  ${MKDIR} -p ${a1dir}
  webfile=${a1dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  ${CONVERT} -colors 255 -trim ${pngfile} ${pngfile}
  a2dir=${DATAHOME}/nclprd/a2
  ${MKDIR} -p ${a2dir}
  webfile=${a2dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  ${CONVERT} -colors 255 -trim ${pngfile} ${pngfile}
  a3dir=${DATAHOME}/nclprd/a3
  ${MKDIR} -p ${a3dir}
  webfile=${a3dir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
done

# Remove the workdir
${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
