#!/bin/ksh --login

np=`cat $PBS_NODEFILE | wc -l`

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
NCL=${NCARG_ROOT}/bin/ncl
CTRANS=${NCARG_ROOT}/bin/ctrans
PS2PDF=/usr/bin/ps2pdf
CONVERT=`which convert`
MONTAGE=`which montage`
PATH=${NCARG_ROOT}/bin:${PATH}

typeset -RZ2 FCST_TIME

# ulimit -s 512000
ulimit -s 1024000

# Settings for testing
EXE_ROOT=/misc/whome/wrfruc/bin/ncl/nclhrrr
# START_TIME=2014032811
# FCST_TIME=12
# DATAROOT=/home/rtrr/hrrr
# DATAHOME=${DATAROOT}/${START_TIME}

# Print run parameters
${ECHO}
${ECHO} "ncl.ksh started at `${DATE}`"
${ECHO}
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
# ${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "   START_TIME = ${START_TIME}"
${ECHO} "   FCST_TIME = ${FCST_TIME}"

# Set up the work directory and cd into it
# workdir=nclprd/${FCST_TIME}htxs   # for testing
workdir=${DATAHOME}/nclprd/${FCST_TIME}htxs
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
# htxsdir=nclprd/htxs   # for testing
htxsdir=${DATAHOME}/nclprd/htxs
${MKDIR} -p ${htxsdir}
cd ${workdir}

# Link to input file
${LN} -s ${DATAHOME}/postprd/wrfnat_hrconus_${FCST_TIME}.grib2 nat_file.grb
${ECHO} "nat_file.grb" > nat_file.txt
ls -al nat_file.grb
${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME}.grib2 prs_file.grb
${ECHO} "prs_file.grb" > prs_file.txt
ls -al prs_file.grb
${LN} -s ${EXE_ROOT}/WRFUserARW.ncl WRFUserARW.ncl
${LN} -s ${EXE_ROOT}/stdatm.txt stdatm.txt

set -A ncgms sitesA_htxswind  \
             sitesA_htxscond

set -A pngs sitesA_htxswind-0.png \
            sitesA_htxswind-1.png \
            sitesA_htxswind-2.png \
            sitesA_htxswind-3.png \
            sitesA_htxswind-4.png \
            sitesA_htxswind-5.png \
            sitesA_htxscond-0.png \
            sitesA_htxscond-1.png \
            sitesA_htxscond-2.png \
            sitesA_htxscond-3.png \
            sitesA_htxscond-4.png \
            sitesA_htxscond-5.png

set -A webnames htxswind_bouA  \
                htxswind_lwxA  \
                htxswind_mkxA  \
                htxswind_seaA  \
                htxswind_miaA  \
                htxswind_atlA  \
                htxscond_bouA  \
                htxscond_lwxA  \
                htxscond_mkxA  \
                htxscond_seaA  \
                htxscond_miaA  \
                htxscond_atlA

ncl_error=0

# Run the NCL scripts for each plot
cp ${EXE_ROOT}/names_grib2.txt .
cp ${EXE_ROOT}/parms*.txt .

i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting rr_${plot}.ncl at `${DATE}`"
  ${NCL} < ${EXE_ROOT}/rr_${plot}.ncl
  error=$?
  if [ ${error} -ne 0 ]; then
    ${ECHO} "ERROR: rr_${plot} crashed!  Exit status=${error}"
    ncl_error=${error}
  fi
  ${ECHO} "Finished rr_${plot}.ncl at `${DATE}`"

  (( i=i + 1 ))

done

# Run ctrans on all the .ncgm files to translate them into Sun Raster files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting ctrans for ${plot}.ncgm at `${DATE}`"
# normal image
  ${CTRANS} -d sun ${plot}.ncgm -resolution 1132x906 > ${plot}.ras

  error=$?
  if [ ${error} -ne 0 ]; then
    ${ECHO} "ERROR: ctrans ${plot}.ncgm crashed!  Exit status=${error}"
    ncl_error=${error}
  fi
  ${ECHO} "Finished ctrans for ${plot}.ncgm at `${DATE}`"

  (( i=i + 1 ))
 
done

# Convert the .ras files into .png files
i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
  ${ECHO} "Starting convert for ${plot} at `${DATE}`"

  if [ -s ${plot}.ras ]; then
# htxs image
    ${CONVERT} -colors 128 -trim -density 300 -geometry 700x700 -border 25x25 -bordercolor black ${plot}.ras ${plot}.png
    error=$?
    if [ ${error} -ne 0 ]; then
      ${ECHO} "ERROR: convert ${plot}.ps crashed!  Exit status=${error}"
      ncl_error=${error}
    fi
    ${ECHO} "Finished convert for ${plot}.ras at `${DATE}`"
  else
    ${ECHO} "No file to convert, exit gracefully"
    ncl_error=0
  fi

  (( i=i + 1 ))
  
done

# Copy png files to their proper names
${MKDIR} -p ${DATAHOME}/nclprd/htxs
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
  pngfile=${pngs[${i}]}
  webfile=${DATAHOME}/nclprd/htxs/${webnames[${i}]}_f${FCST_TIME}.png
#  webfile=${webnames[${i}]}_f${FCST_TIME}.png    # for testing
  ${MV} ${pngfile} ${webfile}

  (( i=i + 1 ))
done

# Remove the workdir
${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
