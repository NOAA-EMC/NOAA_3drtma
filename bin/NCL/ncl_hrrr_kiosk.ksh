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
# module load ncl/6.3.0  # for testing
module load imagemagick/6.2.8

# Make sure we are using GMT time zone for time computations
# export NCL_VER=6.3.0  # for testing
# export DATAHOME="/home/rtrr/hrrr"  # for testing
# export DATAROOT="/home/rtrr/hrrr"  # for testing
# export FCST_TIME=3  # for testing
# export START_TIME=2015120912  # for testing
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
CTRANS=`which ctrans`
PS2PDF=/usr/bin/ps2pdf
CONVERT=`which convert`
MONTAGE=`which montage`
PATH=${NCARG_ROOT}/bin:${PATH}

typeset -RZ2 FCST_TIME
typeset -RZ2 FCST_TIME_AHEAD1
typeset -RZ2 FCST_TIME_AHEAD2
typeset -RZ2 FCST_TIME_BACK1
typeset -RZ2 FCST_TIME_BACK3

ulimit -s 512000

EXE_ROOT=/misc/whome/wrfruc/bin/ncl/nclhrrr

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

# To be valid at the same time, FCST_TIME_AHEAD1 matches with START_TIME_BACK1,
# and FCST_TIME_AHEAD2 matches with START_TIME_BACK2

FCST_TIME_AHEAD1=99
FCST_TIME_AHEAD2=99
if (( ${FCST_TIME} <= 22 )); then
  FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  FCST_TIME_AHEAD2=$(($FCST_TIME + 2))
else
  if (( ${FCST_TIME} == 23 )); then
    FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  fi
fi

# These used for 1hr 80m wind speed change, and esbl 1h 80m change
FCST_TIME_BACK1=-9
if (( ${FCST_TIME} >= 1 )); then
  FCST_TIME_BACK1=$(($FCST_TIME - 1))
fi

# Used for 3h pressure change
FCST_TIME_BACK3=-9
if (( ${FCST_TIME} >= 3 )); then
  FCST_TIME_BACK3=$(($FCST_TIME - 3))
fi

# Print out times
# ${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "   START_TIME = ${START_TIME}"
${ECHO} "   START_TIME_BACK1 = ${START_TIME_BACK1}"
${ECHO} "   START_TIME_BACK2 = ${START_TIME_BACK2}"
${ECHO} "   FCST_TIME = ${FCST_TIME}"
${ECHO} "   FCST_TIME_AHEAD1 = ${FCST_TIME_AHEAD1}"
${ECHO} "   FCST_TIME_AHEAD2 = ${FCST_TIME_AHEAD2}"
${ECHO} "   FCST_TIME_BACK1 = ${FCST_TIME_BACK1}"
if (( ${FCST_TIME} <= 3 )); then
  ${ECHO} "   FCST_TIME_BACK3 = ${FCST_TIME_BACK3}"
fi

# Set up the work directory and cd into it
# workdir=nclprd/${FCST_TIME}kiosk   # for testing
workdir=${DATAHOME}/nclprd/${FCST_TIME}kiosk
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Link to input file
BACK1_DATAROOT=${DATAROOT}/${START_TIME_BACK1}
BACK2_DATAROOT=${DATAROOT}/${START_TIME_BACK2}
# DATAHOME=${DATAROOT}/${START_TIME}  # for testing
${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME}.grib2 hrrrfile.grb
${ECHO} "hrrrfile.grb" > arw_file.txt
if (( ${FCST_TIME_AHEAD1} != 99 )); then
  ${LN} -s ${BACK1_DATAROOT}/postprd/wrfprs_hrconus_${FCST_TIME_AHEAD1}.grib2 back1file.grb
  ${ECHO} "back1file.grb" > back1_file.txt
  ${LN} -s ${BACK1_DATAROOT}/postprd/wrfprs_hrconus_${FCST_TIME}.grib2 back1fileback1hour.grb
  ${ECHO} "back1fileback1hour.grb" > back1_file_back1_hour.txt
fi
if (( ${FCST_TIME_AHEAD2} != 99 )); then
  ${LN} -s ${BACK2_DATAROOT}/postprd/wrfprs_hrconus_${FCST_TIME_AHEAD2}.grib2 back2file.grb
  ${ECHO} "back2file.grb" > back2_file.txt
  ${LN} -s ${BACK2_DATAROOT}/postprd/wrfprs_hrconus_${FCST_TIME_AHEAD1}.grib2 back2fileback1hour.grb
  ${ECHO} "back2fileback1hour.grb" > back2_file_back1_hour.txt
fi
if (( ${FCST_TIME_BACK1} != -9 )); then
  ${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME_BACK1}.grib2 back1hour.grb
  ${ECHO} "back1hour.grb" > back1_hour.txt
fi
if (( ${FCST_TIME_BACK3} != -9 )); then
  ${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME_BACK3}.grib2 back3file.grb
  ${ECHO} "back3file.grb" > back3_file.txt
fi

ls -al hrrrfile.grb
ls -al back1file.grb
ls -al back1fileback1hour.grb
ls -al back2file.grb
ls -al back2fileback1hour.grb
ls -al back1hour.grb
ls -al back3file.grb

set -A ncgms  sfc_cref_kiosk  \
              sfc_ptyp_kiosk

set -A webpfx cref ptyp

set -A fhr 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15

set -A websfx sfc_kiosk sfc_kiosk

set -A tiles dum t1 t2 t3 t4 t5 t6 t7 t8 z0 z1 z2 z3 z4 z5 z6

set -A webmon montage

i=0
while [ ${i} -lt ${#ncgms[@]} ]; do
  j=0 
    pngs[${i}]=${ncgms[${i}]}.png
    # echo ${pngs[${i}]}
    webnames[${i}]=${webpfx[${i}]}_${websfx[${i}]}
    # echo ${webnames[${p}]}
  (( i=i + 1 ))
done

ncl_error=0

# Run the NCL scripts for each plot
cp /whome/wrfruc/bin/ncl/Airpor* .
cp ${EXE_ROOT}/names_grib2.txt .
i=0
echo "FIRST While, ${#ncgms[@]} items"
CMDFN=/tmp/cmd.hrrr_kiosk.$$
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

# Convert the .png files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting convert for ${plot}.png at `${DATE}`"

  if [ -s ${plot}.png ]; then 
# kiosk image
    echo ${CONVERT} -trim -border 25x25 -bordercolor black ${plot}.png ${plot}.png >> $CMDFN
  else 
    ${ECHO} "No file to convert, exit gracefully"
    ncl_error=0
  fi
  ${ECHO} "Finished convert for ${plot}.png at `${DATE}`"

  (( i=i + 1 )) 
  
done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}" 
ncl_error=$?
${RM} -f $CMDFN

# Copy png files to their proper names
kioskdir=${DATAHOME}/nclprd/kiosk
${MKDIR} -p ${kioskdir}
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
  pngfile=${pngs[${i}]}
  webfile=${kioskdir}/${webnames[${i}]}_f${FCST_TIME}.png
  ${MV} ${pngfile} ${webfile}
  (( i=i + 1 ))
done

# Remove the workdir
${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
