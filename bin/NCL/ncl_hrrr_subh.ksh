#!/bin/ksh --login
#
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

if [ "${PBS_NODEFILE:-unset}" != "unset" ]; then
        THREADS=$(cat $PBS_NODEFILE | wc -l)
else
        THREADS=1
fi
echo "Using $THREADS thread(s) for procesing."

# Load modules
module load intel
module load mvapich2
module load netcdf
module load ncl/${NCL_VER}
module load imagemagick/6.2.8

# Make sure we are using GMT time zone for time computations
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
NCL=`which ncl`
CTRANS=${NCARG_ROOT}/bin/ctrans
PS2PDF=/usr/bin/ps2pdf
CONVERT=`which convert`
MONTAGE=`which montage`
PATH=${NCARG_ROOT}/bin:${PATH}

ulimit -s 512000

typeset -RZ2 FCST_TIME FCST_TIME_AHEAD1 FCST_TIME_AHEAD2 hour

# Settings for testing
EXE_ROOT=/misc/whome/wrfruc/bin/ncl/nclhrrr
# START_TIME=2011072815
# DATAROOT=/home/rtrr/hrrr
# DATAHOME=${DATAROOT}/${START_TIME}
# START_TIME_BACK1=2011050313
# START_TIME_BACK2=2011050312
# FCST_TIME=15

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
  START_TIME_BACK1=$( date +"%Y%m%d %H" -d "1 hour ago" )
  START_TIME_BACK2=$( date +"%Y%m%d %H" -d "2 hours ago" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
  START_TIME_BACK1=$( date +"%Y%m%d%H" -d "${START_TIME_BACK1}" )
  START_TIME_BACK2=$( date +"%Y%m%d%H" -d "${START_TIME_BACK2}" )
else
  ${ECHO} "START_TIME defined and is ${START_TIME}"
  START_TIME=$( date +"%Y%m%d %H" -d "${START_TIME%??} ${START_TIME#????????}" )
  START_TIME_BACK1=$( date +"%Y%m%d %H" -d "${START_TIME} 1 hour ago" )
  START_TIME_BACK2=$( date +"%Y%m%d %H" -d "${START_TIME} 2 hours ago" )
  START_TIME=$( date +"%Y%m%d%H" -d "${START_TIME}" )
  START_TIME_BACK1=$( date +"%Y%m%d%H" -d "${START_TIME_BACK1}" )
  START_TIME_BACK2=$( date +"%Y%m%d%H" -d "${START_TIME_BACK2}" )
fi

# DATAHOME=${DATAROOT}/${START_TIME}  # for testing

# To be valid at the same time, FCST_TIME_AHEAD1 matches with START_TIME_BACK1,
# and FCST_TIME_AHEAD2 matches with START_TIME_BACK2

FCST_TIME_AHEAD1=99
FCST_TIME_AHEAD2=99
if (( ${FCST_TIME} <= 13 )); then
  FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  FCST_TIME_AHEAD2=$(($FCST_TIME + 2))
else
  if (( ${FCST_TIME} == 14 )); then
    FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  fi
fi

# Print out times
# ${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "   START_TIME = ${START_TIME}"
${ECHO} "   START_TIME_BACK1 = ${START_TIME_BACK1}"
${ECHO} "   START_TIME_BACK2 = ${START_TIME_BACK2}"
${ECHO} "   FCST_TIME = ${FCST_TIME}"
${ECHO} "   FCST_TIME_AHEAD1 = ${FCST_TIME_AHEAD1}"
${ECHO} "   FCST_TIME_AHEAD2 = ${FCST_TIME_AHEAD2}"

# Set up the work directory and cd into it
# workdir=nclprd/15min/${FCST_TIME}   # for testing
workdir=${DATAHOME}/nclprd/15min/${FCST_TIME}
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Link to input file
BACK1_DATAROOT=${DATAROOT}/${START_TIME_BACK1}
BACK2_DATAROOT=${DATAROOT}/${START_TIME_BACK2}
${LN} -s ${DATAHOME}/postprd/wrftwo_subh_hrconus_${FCST_TIME}.grib2 subhdatafile.grb
${ECHO} "subhdatafile.grb" > newvil.txt
if (( ${FCST_TIME_AHEAD1} != 99 )); then
  ${LN} -s ${BACK1_DATAROOT}/postprd/wrftwo_subh_hrconus_${FCST_TIME_AHEAD1}.grib2 subhback1file.grb
  ${ECHO} "subhback1file.grb" > subh_back1_file.txt
fi
if (( ${FCST_TIME_AHEAD2} != 99 )); then
  ${LN} -s ${BACK2_DATAROOT}/postprd/wrftwo_subh_hrconus_${FCST_TIME_AHEAD2}.grib2 subhback2file.grb
  ${ECHO} "subhback2file.grb" > subh_back2_file.txt
fi
ls -al ${DATAHOME}/postprd/wrftwo_subh_hrconus_${FCST_TIME}.grib2
ls -al subhdatafile.grb
ls -al subhback1file.grb
ls -al subhback2file.grb

# Link to static file
${LN} -s /whome/rtrr/static_hrrr_conus/geo_em.d01.nc geo_em.d01.nc

set -A ncgms  2m_temp15min    \
              2m_dewp15min    \
              10m_wind15min   \
              10m_wind5minavg15min \
              80m_wind15min   \
              sfc_solar15min  \
              sfc_dswrfavg15min \
              sfc_vil15min    \
              sfc_rvil15min   \
              sfc_ectp15min   \
              sfc_sfcp15min   \
              sfc_cref15min   \
              sfc_1ref15min   \
              esbl_cref15min  \
              in25_hlcy15min  \
              in16_hlcy15min  \
              sfc_ca115min    \
              sfc_ca215min    \
              sfc_ca315min    \
              sfc_ci115min    \
              sfc_ci215min    \
              sfc_ci315min    \
              nta_ulwrf15min  \
              sat_G113bt15min \
              sat_G114bt15min \
              sat_G123bt15min \
              sat_G124bt15min


set -A webpfx temp15min   \
              dewp15min   \
              wind15min   \
              wind5minavg15min \
              wind15min   \
              solar15min  \
              dswrfavg15min \
              vil15min    \
              rvil15min   \
              ectp15min   \
              sfcp15min   \
              cref15min   \
              1ref15min   \
              cref15min   \
              hlcy15min   \
              hlcy15min   \
              ca115min    \
              ca215min    \
              ca315min    \
              ci115min    \
              ci215min    \
              ci315min    \
              ulwrf15min  \
              G113bt15min \
              G114bt15min \
              G123bt15min \
              G124bt15min

set -A websfx 2m 2m 10m 10m 80m sfc sfc sfc sfc sfc sfc sfc sfc esbl in25 in16 sfc sfc sfc sfc sfc sfc nta sat sat sat sat

if [ ${FCST_TIME} -eq 00 ]; then
  set -A mins 00
else
  set -A mins 15 30 45 00
fi
set -A tiles t0 t1 t2 t3 t4 t5 t6 t7 t8 z0 z1 z2 z3 z4 z5 z6 z7

i=0
p=0
while [ ${i} -lt ${#ncgms[@]} ]; do
  j=0
  nummins=${#mins[@]}
  numtiles=${#tiles[@]}
# maxj is number of images per product (times * tiles) - 1
  ((maxj=nummins*numtiles-1))
  while [ ${j} -le ${maxj} ]; do
    pngs[${p}]=${ncgms[${i}]}-${j}.png
#    echo ${pngs[${p}]}
    (( j=j + 1 ))
# p is total number of images (image index)
    (( p=p + 1 ))
  done
  (( i=i + 1 ))
done

k=0
n=0

while [ ${k} -lt ${#webpfx[@]} ]; do
  m=0
  while [ ${m} -lt ${#mins[@]} ]; do
    t=0
    nummins=${#mins[@]}
    lastm=$(( ${#mins[@]} - 1 ))
    if [ ${m} -eq ${lastm} ]; then
      hour=${FCST_TIME}
    else
      hour=$(( FCST_TIME - 1 ))
    fi
    while [ ${t} -lt ${#tiles[@]} ]; do
      if [ ${t} -eq 0 ]; then
        webnames[${n}]=${webpfx[${k}]}_${websfx[${k}]}_f${hour}${mins[${m}]}
#        echo ${webnames[${n}]}
      else
        webnames[${n}]=${webpfx[${k}]}_${tiles[${t}]}${websfx[${k}]}_f${hour}${mins[${m}]}
#        echo ${webnames[${n}]}
      fi
      (( n=n + 1 ))
      (( t=t + 1 ))
    done
    (( m=m + 1 ))
  done
  (( k=k + 1 ))
done

ncl_error=0

# Run the NCL scripts for each plot
cp /whome/wrfruc/bin/ncl/Airpor* .
cp /whome/wrfruc/bin/ncl/nclhrrr/names_grib2.txt .
i=0
echo "FIRST While, ${#ncgms[@]} items"
CMDFN=/tmp/cmd.hrrr_part1.$$
${RM} -f $CMDFN

while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting rr_${plot}.ncl at `${DATE}`"
#  ${NCL} < ${EXE_ROOT}/rr_${plot}.ncl
#  error=$?
#  if [ ${error} -ne 0 ]; then
#    ${ECHO} "ERROR: rr_${plot} crashed!  Exit status=${error}"
#    ncl_error=${error}
#  fi
#  ${ECHO} "Finished rr_${plot}.ncl at `${DATE}`"

  echo ${NCL} ${EXE_ROOT}/rr_${plot}.ncl >> $CMDFN

  (( i=i + 1 ))

done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}"
ncl_error=$?
${RM} -f $CMDFN

# Run ctrans on all the .ncgm files to translate them into Sun Raster files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
#  ${ECHO} "Starting ctrans for ${plot}.ncgm at `${DATE}`"
## normal image
#  ${CTRANS} -d sun ${plot}.ncgm -resolution 1132x906 > ${plot}.ras
#
#  error=$?
#  if [ ${error} -ne 0 ]; then
#    ${ECHO} "ERROR: ctrans ${plot}.ncgm crashed!  Exit status=${error}"
#    ncl_error=${error}
#  fi
#  ${ECHO} "Finished ctrans for ${plot}.ncgm at `${DATE}`"

  echo "${CTRANS} -d sun ${plot}.ncgm -resolution 1132x906 > ${plot}.ras" >> $CMDFN

  (( i=i + 1 ))
 
done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}"
ncl_error=$?
${RM} -f $CMDFN

# Convert the .ras files into .png files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting convert for ${plot}.ras at `${DATE}`"

  if [ -s ${plot}.ras ]; then
# normal image
#    ${CONVERT} -colors 128 -trim -border 25x25 -bordercolor black ${plot}.ras ${plot}.png
#    error=$?
#    if [ ${error} -ne 0 ]; then
#      ${ECHO} "ERROR: convert ${plot}.ras crashed!  Exit status=${error}"
#      ncl_error=${error}
#    fi
   echo ${CONVERT} -colors 128 -trim -border 25x25 -bordercolor black ${plot}.ras ${plot}.png >> $CMDFN

  else
    ${ECHO} "No file to convert, exit gracefully"
    ncl_error=0
  fi
  ${ECHO} "Finished convert for ${plot}.ras at `${DATE}`"

  (( i=i + 1 ))
  
done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}"
ncl_error=$?
${RM} -f $CMDFN

# Copy png files to their proper names
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  fulldir=${DATAHOME}/nclprd/15min/full
  ${MKDIR} -p ${fulldir}
  webfile=${fulldir}/${webnames[${i}]}.png
#  webfile=${webnames[${i}]}_f${FCST_TIME}.png    # for testing
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t1dir=${DATAHOME}/nclprd/15min/t1
  ${MKDIR} -p ${t1dir}
  webfile=${t1dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t2dir=${DATAHOME}/nclprd/15min/t2
  ${MKDIR} -p ${t2dir}
  webfile=${t2dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t3dir=${DATAHOME}/nclprd/15min/t3
  ${MKDIR} -p ${t3dir}
  webfile=${t3dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t4dir=${DATAHOME}/nclprd/15min/t4
  ${MKDIR} -p ${t4dir}
  webfile=${t4dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t5dir=${DATAHOME}/nclprd/15min/t5
  ${MKDIR} -p ${t5dir}
  webfile=${t5dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t6dir=${DATAHOME}/nclprd/15min/t6
  ${MKDIR} -p ${t6dir}
  webfile=${t6dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t7dir=${DATAHOME}/nclprd/15min/t7
  ${MKDIR} -p ${t7dir}
  webfile=${t7dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  t8dir=${DATAHOME}/nclprd/15min/t8
  ${MKDIR} -p ${t8dir}
  webfile=${t8dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z0dir=${DATAHOME}/nclprd/15min/z0
  ${MKDIR} -p ${z0dir}
  webfile=${z0dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z1dir=${DATAHOME}/nclprd/15min/z1
  ${MKDIR} -p ${z1dir}
  webfile=${z1dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z2dir=${DATAHOME}/nclprd/15min/z2
  ${MKDIR} -p ${z2dir}
  webfile=${z2dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z3dir=${DATAHOME}/nclprd/15min/z3
  ${MKDIR} -p ${z3dir}
  webfile=${z3dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z4dir=${DATAHOME}/nclprd/15min/z4
  ${MKDIR} -p ${z4dir}
  webfile=${z4dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z5dir=${DATAHOME}/nclprd/15min/z5
  ${MKDIR} -p ${z5dir}
  webfile=${z5dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z6dir=${DATAHOME}/nclprd/15min/z6
  ${MKDIR} -p ${z6dir}
  webfile=${z6dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
  pngfile=${pngs[${i}]}
  webname=${webnames[${i}]}
  webnamelengthfull=${#webnames[${i}]}
  ((webnamelength = ${webnamelengthfull} - 2))
  webnamebegin=${webnames[${i}]:0:4}
  webnameend=${webnames[${i}]:webnamelength:webnamelengthfull}
  z7dir=${DATAHOME}/nclprd/15min/z7
  ${MKDIR} -p ${z7dir}
  webfile=${z7dir}/${webnames[${i}]}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
done

# Remove the workdir
${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
