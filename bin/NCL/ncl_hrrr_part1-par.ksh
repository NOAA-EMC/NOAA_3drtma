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
if (( ${FCST_TIME} <= 13 )); then
  FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  FCST_TIME_AHEAD2=$(($FCST_TIME + 2))
else
  if (( ${FCST_TIME} == 14 )); then
    FCST_TIME_AHEAD1=$(($FCST_TIME + 1))
  fi
fi

# These used for 1hr 80m wind speed change, and esbl 1h 80m change
FCST_TIME_BACK1=-9
if (( ${FCST_TIME} >= 1 )); then
  FCST_TIME_BACK1=$(($FCST_TIME - 1))
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

# Set up the work directory and cd into it
# workdir=nclprd/${FCST_TIME}part1   # for testing
workdir=${DATAHOME}/nclprd/${FCST_TIME}part1
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

ls -al hrrrfile.grb
ls -al back1file.grb
ls -al back1fileback1hour.grb
ls -al back2file.grb
ls -al back2fileback1hour.grb
ls -al back1hour.grb

set -A ncgms  sfc_temp  \
              2m_temp   \
              2m_ptemp  \
              2m_dewp   \
              2m_rh     \
              2ds_temp  \
              10m_wind  \
              80m_wind  \
              80m_wchg  \
              850_wind  \
              250_wind  \
              ua_vort   \
              sfc_pwtr  \
              sfc_totp  \
              sfc_cref  \
              sfc_ptyp  \
              sfc_cape  \
              sfc_cin   \
              sfc_acp   \
              sfc_weasd \
              sfc_1hsnw \
              sfc_acsnw \
              sfc_acpcp \
              sfc_sfcp  \
              sfc_hpbl  \
              ua_rh     \
              ua_rh8    \
              sfc_rhpw  \
              ua_vvel   \
              sfc_vis   \
              ua_ceil   \
              ua_ctop   \
              max_wind  \
              10m_gust  \
              mdn_wind  \
              mup_wind  \
              esbl_hlcy \
              esbl_cref \
              esbl_wchg

set -A pngs sfc_temp-0.png  \
            sfc_temp-1.png  \
            sfc_temp-2.png  \
            sfc_temp-3.png  \
            sfc_temp-4.png  \
            sfc_temp-5.png  \
            sfc_temp-6.png  \
            sfc_temp-7.png  \
            sfc_temp-8.png  \
            2m_temp-0.png   \
            2m_temp-1.png   \
            2m_temp-2.png   \
            2m_temp-3.png   \
            2m_temp-4.png   \
            2m_temp-5.png   \
            2m_temp-6.png   \
            2m_temp-7.png   \
            2m_temp-8.png   \
            2m_ptemp-0.png  \
            2m_ptemp-1.png  \
            2m_ptemp-2.png  \
            2m_ptemp-3.png  \
            2m_ptemp-4.png  \
            2m_ptemp-5.png  \
            2m_ptemp-6.png  \
            2m_ptemp-7.png  \
            2m_ptemp-8.png  \
            2m_dewp-0.png   \
            2m_dewp-1.png   \
            2m_dewp-2.png   \
            2m_dewp-3.png   \
            2m_dewp-4.png   \
            2m_dewp-5.png   \
            2m_dewp-6.png   \
            2m_dewp-7.png   \
            2m_dewp-8.png   \
            2m_rh-0.png     \
            2m_rh-1.png     \
            2m_rh-2.png     \
            2m_rh-3.png     \
            2m_rh-4.png     \
            2m_rh-5.png     \
            2m_rh-6.png     \
            2m_rh-7.png     \
            2m_rh-8.png     \
            2ds_temp-0.png  \
            2ds_temp-1.png  \
            2ds_temp-2.png  \
            2ds_temp-3.png  \
            2ds_temp-4.png  \
            2ds_temp-5.png  \
            2ds_temp-6.png  \
            2ds_temp-7.png  \
            2ds_temp-8.png  \
            10m_wind-0.png  \
            10m_wind-1.png  \
            10m_wind-2.png  \
            10m_wind-3.png  \
            10m_wind-4.png  \
            10m_wind-5.png  \
            10m_wind-6.png  \
            10m_wind-7.png  \
            10m_wind-8.png  \
            80m_wind-0.png  \
            80m_wind-1.png  \
            80m_wind-2.png  \
            80m_wind-3.png  \
            80m_wind-4.png  \
            80m_wind-5.png  \
            80m_wind-6.png  \
            80m_wind-7.png  \
            80m_wind-8.png  \
            80m_wchg-0.png  \
            80m_wchg-1.png  \
            80m_wchg-2.png  \
            80m_wchg-3.png  \
            80m_wchg-4.png  \
            80m_wchg-5.png  \
            80m_wchg-6.png  \
            80m_wchg-7.png  \
            80m_wchg-8.png  \
            850_wind-0.png  \
            850_wind-1.png  \
            850_wind-2.png  \
            850_wind-3.png  \
            850_wind-4.png  \
            850_wind-5.png  \
            850_wind-6.png  \
            850_wind-7.png  \
            850_wind-8.png  \
            250_wind-0.png  \
            250_wind-1.png  \
            250_wind-2.png  \
            250_wind-3.png  \
            250_wind-4.png  \
            250_wind-5.png  \
            250_wind-6.png  \
            250_wind-7.png  \
            250_wind-8.png  \
            ua_vort-0.png   \
            ua_vort-1.png   \
            ua_vort-2.png   \
            ua_vort-3.png   \
            ua_vort-4.png   \
            ua_vort-5.png   \
            ua_vort-6.png   \
            ua_vort-7.png   \
            ua_vort-8.png   \
            sfc_pwtr-0.png  \
            sfc_pwtr-1.png  \
            sfc_pwtr-2.png  \
            sfc_pwtr-3.png  \
            sfc_pwtr-4.png  \
            sfc_pwtr-5.png  \
            sfc_pwtr-6.png  \
            sfc_pwtr-7.png  \
            sfc_pwtr-8.png  \
            sfc_totp-0.png  \
            sfc_totp-1.png  \
            sfc_totp-2.png  \
            sfc_totp-3.png  \
            sfc_totp-4.png  \
            sfc_totp-5.png  \
            sfc_totp-6.png  \
            sfc_totp-7.png  \
            sfc_totp-8.png  \
            sfc_cref-0.png  \
            sfc_cref-1.png  \
            sfc_cref-2.png  \
            sfc_cref-3.png  \
            sfc_cref-4.png  \
            sfc_cref-5.png  \
            sfc_cref-6.png  \
            sfc_cref-7.png  \
            sfc_cref-8.png  \
            sfc_ptyp-0.png  \
            sfc_ptyp-1.png  \
            sfc_ptyp-2.png  \
            sfc_ptyp-3.png  \
            sfc_ptyp-4.png  \
            sfc_ptyp-5.png  \
            sfc_ptyp-6.png  \
            sfc_ptyp-7.png  \
            sfc_ptyp-8.png  \
            sfc_cape-0.png  \
            sfc_cape-1.png  \
            sfc_cape-2.png  \
            sfc_cape-3.png  \
            sfc_cape-4.png  \
            sfc_cape-5.png  \
            sfc_cape-6.png  \
            sfc_cape-7.png  \
            sfc_cape-8.png  \
            sfc_cin-0.png   \
            sfc_cin-1.png   \
            sfc_cin-2.png   \
            sfc_cin-3.png   \
            sfc_cin-4.png   \
            sfc_cin-5.png   \
            sfc_cin-6.png   \
            sfc_cin-7.png   \
            sfc_cin-8.png   \
            sfc_acp-0.png   \
            sfc_acp-1.png   \
            sfc_acp-2.png   \
            sfc_acp-3.png   \
            sfc_acp-4.png   \
            sfc_acp-5.png   \
            sfc_acp-6.png   \
            sfc_acp-7.png   \
            sfc_acp-8.png   \
            sfc_weasd-0.png \
            sfc_weasd-1.png \
            sfc_weasd-2.png \
            sfc_weasd-3.png \
            sfc_weasd-4.png \
            sfc_weasd-5.png \
            sfc_weasd-6.png \
            sfc_weasd-7.png \
            sfc_weasd-8.png \
            sfc_1hsnw-0.png \
            sfc_1hsnw-1.png \
            sfc_1hsnw-2.png \
            sfc_1hsnw-3.png \
            sfc_1hsnw-4.png \
            sfc_1hsnw-5.png \
            sfc_1hsnw-6.png \
            sfc_1hsnw-7.png \
            sfc_1hsnw-8.png \
            sfc_acsnw-0.png \
            sfc_acsnw-1.png \
            sfc_acsnw-2.png \
            sfc_acsnw-3.png \
            sfc_acsnw-4.png \
            sfc_acsnw-5.png \
            sfc_acsnw-6.png \
            sfc_acsnw-7.png \
            sfc_acsnw-8.png \
            sfc_acpcp-0.png \
            sfc_acpcp-1.png \
            sfc_acpcp-2.png \
            sfc_acpcp-3.png \
            sfc_acpcp-4.png \
            sfc_acpcp-5.png \
            sfc_acpcp-6.png \
            sfc_acpcp-7.png \
            sfc_acpcp-8.png \
            sfc_sfcp-0.png  \
            sfc_sfcp-1.png  \
            sfc_sfcp-2.png  \
            sfc_sfcp-3.png  \
            sfc_sfcp-4.png  \
            sfc_sfcp-5.png  \
            sfc_sfcp-6.png  \
            sfc_sfcp-7.png  \
            sfc_sfcp-8.png  \
            sfc_hpbl-0.png  \
            sfc_hpbl-1.png  \
            sfc_hpbl-2.png  \
            sfc_hpbl-3.png  \
            sfc_hpbl-4.png  \
            sfc_hpbl-5.png  \
            sfc_hpbl-6.png  \
            sfc_hpbl-7.png  \
            sfc_hpbl-8.png  \
            ua_rh-0.png     \
            ua_rh-1.png     \
            ua_rh-2.png     \
            ua_rh-3.png     \
            ua_rh-4.png     \
            ua_rh-5.png     \
            ua_rh-6.png     \
            ua_rh-7.png     \
            ua_rh-8.png     \
            ua_rh8-0.png    \
            ua_rh8-1.png    \
            ua_rh8-2.png    \
            ua_rh8-3.png    \
            ua_rh8-4.png    \
            ua_rh8-5.png    \
            ua_rh8-6.png    \
            ua_rh8-7.png    \
            ua_rh8-8.png    \
            sfc_rhpw-0.png  \
            sfc_rhpw-1.png  \
            sfc_rhpw-2.png  \
            sfc_rhpw-3.png  \
            sfc_rhpw-4.png  \
            sfc_rhpw-5.png  \
            sfc_rhpw-6.png  \
            sfc_rhpw-7.png  \
            sfc_rhpw-8.png  \
            ua_vvel-0.png   \
            ua_vvel-1.png   \
            ua_vvel-2.png   \
            ua_vvel-3.png   \
            ua_vvel-4.png   \
            ua_vvel-5.png   \
            ua_vvel-6.png   \
            ua_vvel-7.png   \
            ua_vvel-8.png   \
            sfc_vis-0.png   \
            sfc_vis-1.png   \
            sfc_vis-2.png   \
            sfc_vis-3.png   \
            sfc_vis-4.png   \
            sfc_vis-5.png   \
            sfc_vis-6.png   \
            sfc_vis-7.png   \
            sfc_vis-8.png   \
            ua_ceil-0.png   \
            ua_ceil-1.png   \
            ua_ceil-2.png   \
            ua_ceil-3.png   \
            ua_ceil-4.png   \
            ua_ceil-5.png   \
            ua_ceil-6.png   \
            ua_ceil-7.png   \
            ua_ceil-8.png   \
            ua_ctop-0.png   \
            ua_ctop-1.png   \
            ua_ctop-2.png   \
            ua_ctop-3.png   \
            ua_ctop-4.png   \
            ua_ctop-5.png   \
            ua_ctop-6.png   \
            ua_ctop-7.png   \
            ua_ctop-8.png   \
            max_wind-0.png  \
            max_wind-1.png  \
            max_wind-2.png  \
            max_wind-3.png  \
            max_wind-4.png  \
            max_wind-5.png  \
            max_wind-6.png  \
            max_wind-7.png  \
            max_wind-8.png  \
            10m_gust-0.png  \
            10m_gust-1.png  \
            10m_gust-2.png  \
            10m_gust-3.png  \
            10m_gust-4.png  \
            10m_gust-5.png  \
            10m_gust-6.png  \
            10m_gust-7.png  \
            10m_gust-8.png  \
            mdn_wind-0.png  \
            mdn_wind-1.png  \
            mdn_wind-2.png  \
            mdn_wind-3.png  \
            mdn_wind-4.png  \
            mdn_wind-5.png  \
            mdn_wind-6.png  \
            mdn_wind-7.png  \
            mdn_wind-8.png  \
            mup_wind-0.png  \
            mup_wind-1.png  \
            mup_wind-2.png  \
            mup_wind-3.png  \
            mup_wind-4.png  \
            mup_wind-5.png  \
            mup_wind-6.png  \
            mup_wind-7.png  \
            mup_wind-8.png  \
            esbl_hlcy-0.png \
            esbl_hlcy-1.png \
            esbl_hlcy-2.png \
            esbl_hlcy-3.png \
            esbl_hlcy-4.png \
            esbl_hlcy-5.png \
            esbl_hlcy-6.png \
            esbl_hlcy-7.png \
            esbl_hlcy-8.png \
            esbl_cref-0.png \
            esbl_cref-1.png \
            esbl_cref-2.png \
            esbl_cref-3.png \
            esbl_cref-4.png \
            esbl_cref-5.png \
            esbl_cref-6.png \
            esbl_cref-7.png \
            esbl_cref-8.png \
            esbl_wchg-0.png \
            esbl_wchg-1.png \
            esbl_wchg-2.png \
            esbl_wchg-3.png \
            esbl_wchg-4.png \
            esbl_wchg-5.png \
            esbl_wchg-6.png \
            esbl_wchg-7.png \
            esbl_wchg-8.png

set -A monpngs montage.png

set -A webnames temp_sfc    \
                temp_t1sfc  \
                temp_t2sfc  \
                temp_t3sfc  \
                temp_t4sfc  \
                temp_t5sfc  \
                temp_t6sfc  \
                temp_t7sfc  \
                temp_t8sfc  \
                temp_2m     \
                temp_t12m   \
                temp_t22m   \
                temp_t32m   \
                temp_t42m   \
                temp_t52m   \
                temp_t62m   \
                temp_t72m   \
                temp_t82m   \
                ptemp_2m    \
                ptemp_t12m  \
                ptemp_t22m  \
                ptemp_t32m  \
                ptemp_t42m  \
                ptemp_t52m  \
                ptemp_t62m  \
                ptemp_t72m  \
                ptemp_t82m  \
                dewp_2m     \
                dewp_t12m   \
                dewp_t22m   \
                dewp_t32m   \
                dewp_t42m   \
                dewp_t52m   \
                dewp_t62m   \
                dewp_t72m   \
                dewp_t82m   \
                rh_2m       \
                rh_t12m     \
                rh_t22m     \
                rh_t32m     \
                rh_t42m     \
                rh_t52m     \
                rh_t62m     \
                rh_t72m     \
                rh_t82m     \
                temp_2ds    \
                temp_t12ds  \
                temp_t22ds  \
                temp_t32ds  \
                temp_t42ds  \
                temp_t52ds  \
                temp_t62ds  \
                temp_t72ds  \
                temp_t82ds  \
                wind_10m    \
                wind_t110m  \
                wind_t210m  \
                wind_t310m  \
                wind_t410m  \
                wind_t510m  \
                wind_t610m  \
                wind_t710m  \
                wind_t810m  \
                wind_80m    \
                wind_t180m  \
                wind_t280m  \
                wind_t380m  \
                wind_t480m  \
                wind_t580m  \
                wind_t680m  \
                wind_t780m  \
                wind_t880m  \
                wchg_80m    \
                wchg_t180m  \
                wchg_t280m  \
                wchg_t380m  \
                wchg_t480m  \
                wchg_t580m  \
                wchg_t680m  \
                wchg_t780m  \
                wchg_t880m  \
                wind_850    \
                wind_t1850  \
                wind_t2850  \
                wind_t3850  \
                wind_t4850  \
                wind_t5850  \
                wind_t6850  \
                wind_t7850  \
                wind_t8850  \
                wind_250    \
                wind_t1250  \
                wind_t2250  \
                wind_t3250  \
                wind_t4250  \
                wind_t5250  \
                wind_t6250  \
                wind_t7250  \
                wind_t8250  \
                vort_500    \
                vort_t1500  \
                vort_t2500  \
                vort_t3500  \
                vort_t4500  \
                vort_t5500  \
                vort_t6500  \
                vort_t7500  \
                vort_t8500  \
                pwtr_sfc    \
                pwtr_t1sfc  \
                pwtr_t2sfc  \
                pwtr_t3sfc  \
                pwtr_t4sfc  \
                pwtr_t5sfc  \
                pwtr_t6sfc  \
                pwtr_t7sfc  \
                pwtr_t8sfc  \
                totp_sfc    \
                totp_t1sfc  \
                totp_t2sfc  \
                totp_t3sfc  \
                totp_t4sfc  \
                totp_t5sfc  \
                totp_t6sfc  \
                totp_t7sfc  \
                totp_t8sfc  \
                cref_sfc    \
                cref_t1sfc  \
                cref_t2sfc  \
                cref_t3sfc  \
                cref_t4sfc  \
                cref_t5sfc  \
                cref_t6sfc  \
                cref_t7sfc  \
                cref_t8sfc  \
                ptyp_sfc    \
                ptyp_t1sfc  \
                ptyp_t2sfc  \
                ptyp_t3sfc  \
                ptyp_t4sfc  \
                ptyp_t5sfc  \
                ptyp_t6sfc  \
                ptyp_t7sfc  \
                ptyp_t8sfc  \
                cape_sfc    \
                cape_t1sfc  \
                cape_t2sfc  \
                cape_t3sfc  \
                cape_t4sfc  \
                cape_t5sfc  \
                cape_t6sfc  \
                cape_t7sfc  \
                cape_t8sfc  \
                cin_sfc     \
                cin_t1sfc   \
                cin_t2sfc   \
                cin_t3sfc   \
                cin_t4sfc   \
                cin_t5sfc   \
                cin_t6sfc   \
                cin_t7sfc   \
                cin_t8sfc   \
                3hap_sfc    \
                3hap_t1sfc  \
                3hap_t2sfc  \
                3hap_t3sfc  \
                3hap_t4sfc  \
                3hap_t5sfc  \
                3hap_t6sfc  \
                3hap_t7sfc  \
                3hap_t8sfc  \
                weasd_sfc   \
                weasd_t1sfc \
                weasd_t2sfc \
                weasd_t3sfc \
                weasd_t4sfc \
                weasd_t5sfc \
                weasd_t6sfc \
                weasd_t7sfc \
                weasd_t8sfc \
                1hsnw_sfc   \
                1hsnw_t1sfc \
                1hsnw_t2sfc \
                1hsnw_t3sfc \
                1hsnw_t4sfc \
                1hsnw_t5sfc \
                1hsnw_t6sfc \
                1hsnw_t7sfc \
                1hsnw_t8sfc \
                acsnw_sfc   \
                acsnw_t1sfc \
                acsnw_t2sfc \
                acsnw_t3sfc \
                acsnw_t4sfc \
                acsnw_t5sfc \
                acsnw_t6sfc \
                acsnw_t7sfc \
                acsnw_t8sfc \
                acpcp_sfc   \
                acpcp_t1sfc \
                acpcp_t2sfc \
                acpcp_t3sfc \
                acpcp_t4sfc \
                acpcp_t5sfc \
                acpcp_t6sfc \
                acpcp_t7sfc \
                acpcp_t8sfc \
                sfcp_sfc    \
                sfcp_t1sfc  \
                sfcp_t2sfc  \
                sfcp_t3sfc  \
                sfcp_t4sfc  \
                sfcp_t5sfc  \
                sfcp_t6sfc  \
                sfcp_t7sfc  \
                sfcp_t8sfc  \
                hpbl_sfc    \
                hpbl_t1sfc  \
                hpbl_t2sfc  \
                hpbl_t3sfc  \
                hpbl_t4sfc  \
                hpbl_t5sfc  \
                hpbl_t6sfc  \
                hpbl_t7sfc  \
                hpbl_t8sfc  \
                rh_500      \
                rh_t1500    \
                rh_t2500    \
                rh_t3500    \
                rh_t4500    \
                rh_t5500    \
                rh_t6500    \
                rh_t7500    \
                rh_t8500    \
                rh_850      \
                rh_t1850    \
                rh_t2850    \
                rh_t3850    \
                rh_t4850    \
                rh_t5850    \
                rh_t6850    \
                rh_t7850    \
                rh_t8850    \
                rhpw_sfc    \
                rhpw_t1sfc  \
                rhpw_t2sfc  \
                rhpw_t3sfc  \
                rhpw_t4sfc  \
                rhpw_t5sfc  \
                rhpw_t6sfc  \
                rhpw_t7sfc  \
                rhpw_t8sfc  \
                vvel_700    \
                vvel_t1700  \
                vvel_t2700  \
                vvel_t3700  \
                vvel_t4700  \
                vvel_t5700  \
                vvel_t6700  \
                vvel_t7700  \
                vvel_t8700  \
                vis_sfc     \
                vis_t1sfc   \
                vis_t2sfc   \
                vis_t3sfc   \
                vis_t4sfc   \
                vis_t5sfc   \
                vis_t6sfc   \
                vis_t7sfc   \
                vis_t8sfc   \
                ceil        \
                ceil_t1     \
                ceil_t2     \
                ceil_t3     \
                ceil_t4     \
                ceil_t5     \
                ceil_t6     \
                ceil_t7     \
                ceil_t8     \
                ctop        \
                ctop_t1     \
                ctop_t2     \
                ctop_t3     \
                ctop_t4     \
                ctop_t5     \
                ctop_t6     \
                ctop_t7     \
                ctop_t8     \
                wind_max    \
                wind_t1max  \
                wind_t2max  \
                wind_t3max  \
                wind_t4max  \
                wind_t5max  \
                wind_t6max  \
                wind_t7max  \
                wind_t8max  \
                gust_10m    \
                gust_t110m  \
                gust_t210m  \
                gust_t310m  \
                gust_t410m  \
                gust_t510m  \
                gust_t610m  \
                gust_t710m  \
                gust_t810m  \
                wind_mdn    \
                wind_t1mdn  \
                wind_t2mdn  \
                wind_t3mdn  \
                wind_t4mdn  \
                wind_t5mdn  \
                wind_t6mdn  \
                wind_t7mdn  \
                wind_t8mdn  \
                wind_mup    \
                wind_t1mup  \
                wind_t2mup  \
                wind_t3mup  \
                wind_t4mup  \
                wind_t5mup  \
                wind_t6mup  \
                wind_t7mup  \
                wind_t8mup  \
                hlcy_esbl   \
                hlcy_t1esbl \
                hlcy_t2esbl \
                hlcy_t3esbl \
                hlcy_t4esbl \
                hlcy_t5esbl \
                hlcy_t6esbl \
                hlcy_t7esbl \
                hlcy_t8esbl \
                cref_esbl   \
                cref_t1esbl \
                cref_t2esbl \
                cref_t3esbl \
                cref_t4esbl \
                cref_t5esbl \
                cref_t6esbl \
                cref_t7esbl \
                cref_t8esbl \
                wchg_esbl   \
                wchg_t1esbl \
                wchg_t2esbl \
                wchg_t3esbl \
                wchg_t4esbl \
                wchg_t5esbl \
                wchg_t6esbl \
                wchg_t7esbl \
                wchg_t8esbl

set -A webmon montage

ncl_error=0

# Run the NCL scripts for each plot
cp /whome/wrfruc/bin/ncl/Airpor* .
cp ${EXE_ROOT}/names_grib2.txt .
i=0
echo "FIRST While, ${#ncgms[@]} items"
CMDFN=/tmp/cmd.hrrr_part1.$$
${RM} -f $CMDFN

while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}
#  ${ECHO} "Starting rr_${plot}.ncl at `${DATE}`"
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
## montage image
#  ${CTRANS} -d sun ${plot}.ncgm -resolution 2678x1673 > ${plot}_mon.ras
#
#  error=$?
#  if [ ${error} -ne 0 ]; then
#    ${ECHO} "ERROR: ctrans ${plot}.ncgm crashed!  Exit status=${error}"
#    ncl_error=${error}
#  fi
#  ${ECHO} "Finished ctrans for ${plot}.ncgm at `${DATE}`"

echo "${CTRANS} -d sun ${plot}.ncgm -resolution 1132x906 > ${plot}.ras" >> $CMDFN
echo "${CTRANS} -d sun ${plot}.ncgm -resolution 2678x1673 > ${plot}_mon.ras" >> $CMDFN

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

  if [ -s ${plot}_mon.ras ]; then
# montage image
#    ${CONVERT} -colors 128 -trim -border 190x12 -bordercolor black ${plot}_mon.ras ${plot}_mon.png
#    error=$?
#    if [ ${error} -ne 0 ]; then
#      ${ECHO} "ERROR: convert ${plot}_mon.ras crashed!  Exit status=${error}"
#      ncl_error=${error}
#    fi
    echo ${CONVERT} -colors 128 -trim -border 190x12 -bordercolor black ${plot}_mon.ras ${plot}_mon.png >> $CMDFN
  else
    ${ECHO} "No file to convert, exit gracefully"
    ncl_error=0
  fi
  ${ECHO} "Finished convert for ${plot}_mon.ras at `${DATE}`"

  (( i=i + 1 ))
  
done

${CAT} $CMDFN | ${XARGS} -P $THREADS -I {} ${BASH} -c "{}"
ncl_error=$?
${RM} -f $CMDFN

# put together the montage images
${MONTAGE} sfc_cref_mon-0.png 2m_temp_mon-0.png 10m_wind_mon-0.png ua_ceil_mon-0.png -tile 2x2 -geometry 1877x1048+21+4 -background black montage.png

# Copy png files to their proper names
i=0
while [ ${i} -lt ${#pngs[@]} ]; do
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
done

# Copy montage files to their proper names
i=0
while [ ${i} -lt ${#monpngs[@]} ]; do
  pngfile=${monpngs[${i}]}
  fulldir=${DATAHOME}/nclprd/full
  ${MKDIR} -p ${fulldir}
  webfile=${fulldir}/${webmon[${i}]}_f${FCST_TIME}.png
#  webfile=${webmon[${i}]}_f${FCST_TIME}.png    # for testing
  ${MV} ${pngfile} ${webfile}

  (( i=i + 1 ))
done

# Remove the workdir
  ${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
