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

ulimit -s 512000

typeset -RZ2 FCST_TIME
typeset -RZ2 FCST_TIME_BACK3

# Settings for testing
#DATAHOME=/tg0/projects/wrfruc/wrf2.2/DOMAINS/hrrr3/2008021215
EXE_ROOT=/misc/whome/wrfruc/bin/ncl/nclhrrr
#START_TIME=2008021215
#FCST_TIME=12

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
  DATESTAMP=$( expr $( date +"%s"))
  START_TIME=$( date -d@${DATESTAMP} +"%Y%m%d%H" )
fi

FCST_TIME_BACK3=-9
if (( ${FCST_TIME} >= 3 )); then
  FCST_TIME_BACK3=$(($FCST_TIME - 3))
fi

# Print out times
# ${ECHO} "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
${ECHO} "   START_TIME = ${START_TIME}"
${ECHO} "    FCST_TIME = ${FCST_TIME}"
if (( ${FCST_TIME} <= 3 )); then
  ${ECHO} "   FCST_TIME_BACK3 = ${FCST_TIME_BACK3}"
fi


# Set up the work directory and cd into it
#workdir=nclprd/${FCST_TIME}   # for testing
workdir=${DATAHOME}/nclprd/${FCST_TIME}part2
${RM} -rf ${workdir}
${MKDIR} -p ${workdir}
cd ${workdir}

# Link to input file
${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME}.grib2 hrrrfile.grb
${ECHO} "hrrrfile.grb" > arw_file.txt
if (( ${FCST_TIME_BACK3} != -9 )); then
  ${LN} -s ${DATAHOME}/postprd/wrfprs_hrconus_${FCST_TIME_BACK3}.grib2 back3file.grb
  ${ECHO} "back3file.grb" > back3_file.txt
  ls -al back3file.grb
fi

set -A ncgms  nta_ulwrf \
		sfc_hlcy  \
              mx16_hlcy \
              in25_hlcy \
              in16_hlcy \
              sfc_ca1   \
              sfc_ca2   \
              sfc_ca3   \
              sfc_ci1   \
              sfc_ci2   \
              sfc_ci3   \
              sfc_ltg1  \
              sfc_ltg2  \
              sfc_ltg3  \
              sfc_pchg  \
              sfc_lcl   \
              sfc_tcc   \
              sfc_lcc   \
              sfc_mcc   \
              sfc_hcc   \
              sfc_mnvv  \
              sfc_mref  \
              sfc_mucp  \
              sfc_mulcp \
              sfc_mxcp  \
              sfc_1hsm  \
              sfc_3hsm  \
              sfc_vig   \
              sfc_s1shr \
              sfc_6kshr \
              500_temp  \
              700_temp  \
              850_temp  \
              925_temp  \
              sfc_1ref  \
              sfc_bli   \
              sfc_lhtfl \
              sfc_shtfl \
              sfc_flru  \
              80m_wpwr  \
              sfc_solar \
              sfc_ectp  \
              sfc_vil   \
              sfc_rvil

set -A pngs sfc_hlcy-0.png  \
            sfc_hlcy-1.png  \
            sfc_hlcy-2.png  \
            sfc_hlcy-3.png  \
            sfc_hlcy-4.png  \
            sfc_hlcy-5.png  \
            sfc_hlcy-6.png  \
            sfc_hlcy-7.png  \
            sfc_hlcy-8.png  \
            mx16_hlcy-0.png \
            mx16_hlcy-1.png \
            mx16_hlcy-2.png \
            mx16_hlcy-3.png \
            mx16_hlcy-4.png \
            mx16_hlcy-5.png \
            mx16_hlcy-6.png \
            mx16_hlcy-7.png \
            mx16_hlcy-8.png \
            in25_hlcy-0.png \
            in25_hlcy-1.png \
            in25_hlcy-2.png \
            in25_hlcy-3.png \
            in25_hlcy-4.png \
            in25_hlcy-5.png \
            in25_hlcy-6.png \
            in25_hlcy-7.png \
            in25_hlcy-8.png \
            in16_hlcy-0.png \
            in16_hlcy-1.png \
            in16_hlcy-2.png \
            in16_hlcy-3.png \
            in16_hlcy-4.png \
            in16_hlcy-5.png \
            in16_hlcy-6.png \
            in16_hlcy-7.png \
            in16_hlcy-8.png \
            sfc_ca1-0.png   \
            sfc_ca1-1.png   \
            sfc_ca1-2.png   \
            sfc_ca1-3.png   \
            sfc_ca1-4.png   \
            sfc_ca1-5.png   \
            sfc_ca1-6.png   \
            sfc_ca1-7.png   \
            sfc_ca1-8.png   \
            sfc_ca2-0.png   \
            sfc_ca2-1.png   \
            sfc_ca2-2.png   \
            sfc_ca2-3.png   \
            sfc_ca2-4.png   \
            sfc_ca2-5.png   \
            sfc_ca2-6.png   \
            sfc_ca2-7.png   \
            sfc_ca2-8.png   \
            sfc_ca3-0.png   \
            sfc_ca3-1.png   \
            sfc_ca3-2.png   \
            sfc_ca3-3.png   \
            sfc_ca3-4.png   \
            sfc_ca3-5.png   \
            sfc_ca3-6.png   \
            sfc_ca3-7.png   \
            sfc_ca3-8.png   \
            sfc_ci1-0.png   \
            sfc_ci1-1.png   \
            sfc_ci1-2.png   \
            sfc_ci1-3.png   \
            sfc_ci1-4.png   \
            sfc_ci1-5.png   \
            sfc_ci1-6.png   \
            sfc_ci1-7.png   \
            sfc_ci1-8.png   \
            sfc_ci2-0.png   \
            sfc_ci2-1.png   \
            sfc_ci2-2.png   \
            sfc_ci2-3.png   \
            sfc_ci2-4.png   \
            sfc_ci2-5.png   \
            sfc_ci2-6.png   \
            sfc_ci2-7.png   \
            sfc_ci2-8.png   \
            sfc_ci3-0.png   \
            sfc_ci3-1.png   \
            sfc_ci3-2.png   \
            sfc_ci3-3.png   \
            sfc_ci3-4.png   \
            sfc_ci3-5.png   \
            sfc_ci3-6.png   \
            sfc_ci3-7.png   \
            sfc_ci3-8.png   \
            sfc_ltg1-0.png  \
            sfc_ltg1-1.png  \
            sfc_ltg1-2.png  \
            sfc_ltg1-3.png  \
            sfc_ltg1-4.png  \
            sfc_ltg1-5.png  \
            sfc_ltg1-6.png  \
            sfc_ltg1-7.png  \
            sfc_ltg1-8.png  \
            sfc_ltg2-0.png  \
            sfc_ltg2-1.png  \
            sfc_ltg2-2.png  \
            sfc_ltg2-3.png  \
            sfc_ltg2-4.png  \
            sfc_ltg2-5.png  \
            sfc_ltg2-6.png  \
            sfc_ltg2-7.png  \
            sfc_ltg2-8.png  \
            sfc_ltg3-0.png  \
            sfc_ltg3-1.png  \
            sfc_ltg3-2.png  \
            sfc_ltg3-3.png  \
            sfc_ltg3-4.png  \
            sfc_ltg3-5.png  \
            sfc_ltg3-6.png  \
            sfc_ltg3-7.png  \
            sfc_ltg3-8.png  \
            sfc_pchg-0.png  \
            sfc_pchg-1.png  \
            sfc_pchg-2.png  \
            sfc_pchg-3.png  \
            sfc_pchg-4.png  \
            sfc_pchg-5.png  \
            sfc_pchg-6.png  \
            sfc_pchg-7.png  \
            sfc_pchg-8.png  \
            sfc_lcl-0.png   \
            sfc_lcl-1.png   \
            sfc_lcl-2.png   \
            sfc_lcl-3.png   \
            sfc_lcl-4.png   \
            sfc_lcl-5.png   \
            sfc_lcl-6.png   \
            sfc_lcl-7.png   \
            sfc_lcl-8.png   \
            sfc_tcc-0.png   \
            sfc_tcc-1.png   \
            sfc_tcc-2.png   \
            sfc_tcc-3.png   \
            sfc_tcc-4.png   \
            sfc_tcc-5.png   \
            sfc_tcc-6.png   \
            sfc_tcc-7.png   \
            sfc_tcc-8.png   \
            sfc_lcc-0.png   \
            sfc_lcc-1.png   \
            sfc_lcc-2.png   \
            sfc_lcc-3.png   \
            sfc_lcc-4.png   \
            sfc_lcc-5.png   \
            sfc_lcc-6.png   \
            sfc_lcc-7.png   \
            sfc_lcc-8.png   \
            sfc_mcc-0.png   \
            sfc_mcc-1.png   \
            sfc_mcc-2.png   \
            sfc_mcc-3.png   \
            sfc_mcc-4.png   \
            sfc_mcc-5.png   \
            sfc_mcc-6.png   \
            sfc_mcc-7.png   \
            sfc_mcc-8.png   \
            sfc_hcc-0.png   \
            sfc_hcc-1.png   \
            sfc_hcc-2.png   \
            sfc_hcc-3.png   \
            sfc_hcc-4.png   \
            sfc_hcc-5.png   \
            sfc_hcc-6.png   \
            sfc_hcc-7.png   \
            sfc_hcc-8.png   \
            sfc_mnvv-0.png  \
            sfc_mnvv-1.png  \
            sfc_mnvv-2.png  \
            sfc_mnvv-3.png  \
            sfc_mnvv-4.png  \
            sfc_mnvv-5.png  \
            sfc_mnvv-6.png  \
            sfc_mnvv-7.png  \
            sfc_mnvv-8.png  \
            sfc_mref-0.png  \
            sfc_mref-1.png  \
            sfc_mref-2.png  \
            sfc_mref-3.png  \
            sfc_mref-4.png  \
            sfc_mref-5.png  \
            sfc_mref-6.png  \
            sfc_mref-7.png  \
            sfc_mref-8.png  \
            sfc_mucp-0.png  \
            sfc_mucp-1.png  \
            sfc_mucp-2.png  \
            sfc_mucp-3.png  \
            sfc_mucp-4.png  \
            sfc_mucp-5.png  \
            sfc_mucp-6.png  \
            sfc_mucp-7.png  \
            sfc_mucp-8.png  \
            sfc_mulcp-0.png \
            sfc_mulcp-1.png \
            sfc_mulcp-2.png \
            sfc_mulcp-3.png \
            sfc_mulcp-4.png \
            sfc_mulcp-5.png \
            sfc_mulcp-6.png \
            sfc_mulcp-7.png \
            sfc_mulcp-8.png \
            sfc_mxcp-0.png  \
            sfc_mxcp-1.png  \
            sfc_mxcp-2.png  \
            sfc_mxcp-3.png  \
            sfc_mxcp-4.png  \
            sfc_mxcp-5.png  \
            sfc_mxcp-6.png  \
            sfc_mxcp-7.png  \
            sfc_mxcp-8.png  \
            sfc_1hsm-0.png  \
            sfc_1hsm-1.png  \
            sfc_1hsm-2.png  \
            sfc_1hsm-3.png  \
            sfc_1hsm-4.png  \
            sfc_1hsm-5.png  \
            sfc_1hsm-6.png  \
            sfc_1hsm-7.png  \
            sfc_1hsm-8.png  \
            sfc_3hsm-0.png  \
            sfc_3hsm-1.png  \
            sfc_3hsm-2.png  \
            sfc_3hsm-3.png  \
            sfc_3hsm-4.png  \
            sfc_3hsm-5.png  \
            sfc_3hsm-6.png  \
            sfc_3hsm-7.png  \
            sfc_3hsm-8.png  \
            sfc_vig-0.png   \
            sfc_vig-1.png   \
            sfc_vig-2.png   \
            sfc_vig-3.png   \
            sfc_vig-4.png   \
            sfc_vig-5.png   \
            sfc_vig-6.png   \
            sfc_vig-7.png   \
            sfc_vig-8.png   \
            sfc_s1shr-0.png \
            sfc_s1shr-1.png \
            sfc_s1shr-2.png \
            sfc_s1shr-3.png \
            sfc_s1shr-4.png \
            sfc_s1shr-5.png \
            sfc_s1shr-6.png \
            sfc_s1shr-7.png \
            sfc_s1shr-8.png \
            sfc_6kshr-0.png \
            sfc_6kshr-1.png \
            sfc_6kshr-2.png \
            sfc_6kshr-3.png \
            sfc_6kshr-4.png \
            sfc_6kshr-5.png \
            sfc_6kshr-6.png \
            sfc_6kshr-7.png \
            sfc_6kshr-8.png \
            500_temp-0.png  \
            500_temp-1.png  \
            500_temp-2.png  \
            500_temp-3.png  \
            500_temp-4.png  \
            500_temp-5.png  \
            500_temp-6.png  \
            500_temp-7.png  \
            500_temp-8.png  \
            700_temp-0.png  \
            700_temp-1.png  \
            700_temp-2.png  \
            700_temp-3.png  \
            700_temp-4.png  \
            700_temp-5.png  \
            700_temp-6.png  \
            700_temp-7.png  \
            700_temp-8.png  \
            850_temp-0.png  \
            850_temp-1.png  \
            850_temp-2.png  \
            850_temp-3.png  \
            850_temp-4.png  \
            850_temp-5.png  \
            850_temp-6.png  \
            850_temp-7.png  \
            850_temp-8.png  \
            925_temp-0.png  \
            925_temp-1.png  \
            925_temp-2.png  \
            925_temp-3.png  \
            925_temp-4.png  \
            925_temp-5.png  \
            925_temp-6.png  \
            925_temp-7.png  \
            925_temp-8.png  \
            sfc_1ref-0.png  \
            sfc_1ref-1.png  \
            sfc_1ref-2.png  \
            sfc_1ref-3.png  \
            sfc_1ref-4.png  \
            sfc_1ref-5.png  \
            sfc_1ref-6.png  \
            sfc_1ref-7.png  \
            sfc_1ref-8.png  \
            sfc_bli-0.png   \
            sfc_bli-1.png   \
            sfc_bli-2.png   \
            sfc_bli-3.png   \
            sfc_bli-4.png   \
            sfc_bli-5.png   \
            sfc_bli-6.png   \
            sfc_bli-7.png   \
            sfc_bli-8.png   \
            nta_ulwrf-0.png \
            nta_ulwrf-1.png \
            nta_ulwrf-2.png \
            nta_ulwrf-3.png \
            nta_ulwrf-4.png \
            nta_ulwrf-5.png \
            nta_ulwrf-6.png \
            nta_ulwrf-7.png \
            nta_ulwrf-8.png \
            sfc_lhtfl-0.png \
            sfc_lhtfl-1.png \
            sfc_lhtfl-2.png \
            sfc_lhtfl-3.png \
            sfc_lhtfl-4.png \
            sfc_lhtfl-5.png \
            sfc_lhtfl-6.png \
            sfc_lhtfl-7.png \
            sfc_lhtfl-8.png \
            sfc_shtfl-0.png \
            sfc_shtfl-1.png \
            sfc_shtfl-2.png \
            sfc_shtfl-3.png \
            sfc_shtfl-4.png \
            sfc_shtfl-5.png \
            sfc_shtfl-6.png \
            sfc_shtfl-7.png \
            sfc_shtfl-8.png \
            sfc_flru-0.png  \
            sfc_flru-1.png  \
            sfc_flru-2.png  \
            sfc_flru-3.png  \
            sfc_flru-4.png  \
            sfc_flru-5.png  \
            sfc_flru-6.png  \
            sfc_flru-7.png  \
            sfc_flru-8.png  \
            80m_wpwr-0.png  \
            80m_wpwr-1.png  \
            80m_wpwr-2.png  \
            80m_wpwr-3.png  \
            80m_wpwr-4.png  \
            80m_wpwr-5.png  \
            80m_wpwr-6.png  \
            80m_wpwr-7.png  \
            80m_wpwr-8.png  \
            sfc_solar-0.png \
            sfc_solar-1.png \
            sfc_solar-2.png \
            sfc_solar-3.png \
            sfc_solar-4.png \
            sfc_solar-5.png \
            sfc_solar-6.png \
            sfc_solar-7.png \
            sfc_solar-8.png \
            sfc_ectp-0.png  \
            sfc_ectp-1.png  \
            sfc_ectp-2.png  \
            sfc_ectp-3.png  \
            sfc_ectp-4.png  \
            sfc_ectp-5.png  \
            sfc_ectp-6.png  \
            sfc_ectp-7.png  \
            sfc_ectp-8.png  \
            sfc_vil-0.png   \
            sfc_vil-1.png   \
            sfc_vil-2.png   \
            sfc_vil-3.png   \
            sfc_vil-4.png   \
            sfc_vil-5.png   \
            sfc_vil-6.png   \
            sfc_vil-7.png   \
            sfc_vil-8.png   \
            sfc_rvil-0.png  \
            sfc_rvil-1.png  \
            sfc_rvil-2.png  \
            sfc_rvil-3.png  \
            sfc_rvil-4.png  \
            sfc_rvil-5.png  \
            sfc_rvil-6.png  \
            sfc_rvil-7.png  \
            sfc_rvil-8.png

set -A webnames hlcy_sfc    \
                hlcy_t1sfc  \
                hlcy_t2sfc  \
                hlcy_t3sfc  \
                hlcy_t4sfc  \
                hlcy_t5sfc  \
                hlcy_t6sfc  \
                hlcy_t7sfc  \
                hlcy_t8sfc  \
                hlcy_mx16   \
                hlcy_t1mx16 \
                hlcy_t2mx16 \
                hlcy_t3mx16 \
                hlcy_t4mx16 \
                hlcy_t5mx16 \
                hlcy_t6mx16 \
                hlcy_t7mx16 \
                hlcy_t8mx16 \
                hlcy_in25   \
                hlcy_t1in25 \
                hlcy_t2in25 \
                hlcy_t3in25 \
                hlcy_t4in25 \
                hlcy_t5in25 \
                hlcy_t6in25 \
                hlcy_t7in25 \
                hlcy_t8in25 \
                hlcy_in16   \
                hlcy_t1in16 \
                hlcy_t2in16 \
                hlcy_t3in16 \
                hlcy_t4in16 \
                hlcy_t5in16 \
                hlcy_t6in16 \
                hlcy_t7in16 \
                hlcy_t8in16 \
                ca1_sfc     \
                ca1_t1sfc   \
                ca1_t2sfc   \
                ca1_t3sfc   \
                ca1_t4sfc   \
                ca1_t5sfc   \
                ca1_t6sfc   \
                ca1_t7sfc   \
                ca1_t8sfc   \
                ca2_sfc     \
                ca2_t1sfc   \
                ca2_t2sfc   \
                ca2_t3sfc   \
                ca2_t4sfc   \
                ca2_t5sfc   \
                ca2_t6sfc   \
                ca2_t7sfc   \
                ca2_t8sfc   \
                ca3_sfc     \
                ca3_t1sfc   \
                ca3_t2sfc   \
                ca3_t3sfc   \
                ca3_t4sfc   \
                ca3_t5sfc   \
                ca3_t6sfc   \
                ca3_t7sfc   \
                ca3_t8sfc   \
                ci1_sfc     \
                ci1_t1sfc   \
                ci1_t2sfc   \
                ci1_t3sfc   \
                ci1_t4sfc   \
                ci1_t5sfc   \
                ci1_t6sfc   \
                ci1_t7sfc   \
                ci1_t8sfc   \
                ci2_sfc     \
                ci2_t1sfc   \
                ci2_t2sfc   \
                ci2_t3sfc   \
                ci2_t4sfc   \
                ci2_t5sfc   \
                ci2_t6sfc   \
                ci2_t7sfc   \
                ci2_t8sfc   \
                ci3_sfc     \
                ci3_t1sfc   \
                ci3_t2sfc   \
                ci3_t3sfc   \
                ci3_t4sfc   \
                ci3_t5sfc   \
                ci3_t6sfc   \
                ci3_t7sfc   \
                ci3_t8sfc   \
                ltg1_sfc    \
                ltg1_t1sfc  \
                ltg1_t2sfc  \
                ltg1_t3sfc  \
                ltg1_t4sfc  \
                ltg1_t5sfc  \
                ltg1_t6sfc  \
                ltg1_t7sfc  \
                ltg1_t8sfc  \
                ltg2_sfc    \
                ltg2_t1sfc  \
                ltg2_t2sfc  \
                ltg2_t3sfc  \
                ltg2_t4sfc  \
                ltg2_t5sfc  \
                ltg2_t6sfc  \
                ltg2_t7sfc  \
                ltg2_t8sfc  \
                ltg3_sfc    \
                ltg3_t1sfc  \
                ltg3_t2sfc  \
                ltg3_t3sfc  \
                ltg3_t4sfc  \
                ltg3_t5sfc  \
                ltg3_t6sfc  \
                ltg3_t7sfc  \
                ltg3_t8sfc  \
                pchg_sfc    \
                pchg_t1sfc  \
                pchg_t2sfc  \
                pchg_t3sfc  \
                pchg_t4sfc  \
                pchg_t5sfc  \
                pchg_t6sfc  \
                pchg_t7sfc  \
                pchg_t8sfc  \
                lcl_sfc     \
                lcl_t1sfc   \
                lcl_t2sfc   \
                lcl_t3sfc   \
                lcl_t4sfc   \
                lcl_t5sfc   \
                lcl_t6sfc   \
                lcl_t7sfc   \
                lcl_t8sfc   \
                tcc_sfc     \
                tcc_t1sfc   \
                tcc_t2sfc   \
                tcc_t3sfc   \
                tcc_t4sfc   \
                tcc_t5sfc   \
                tcc_t6sfc   \
                tcc_t7sfc   \
                tcc_t8sfc   \
                lcc_sfc     \
                lcc_t1sfc   \
                lcc_t2sfc   \
                lcc_t3sfc   \
                lcc_t4sfc   \
                lcc_t5sfc   \
                lcc_t6sfc   \
                lcc_t7sfc   \
                lcc_t8sfc   \
                mcc_sfc     \
                mcc_t1sfc   \
                mcc_t2sfc   \
                mcc_t3sfc   \
                mcc_t4sfc   \
                mcc_t5sfc   \
                mcc_t6sfc   \
                mcc_t7sfc   \
                mcc_t8sfc   \
                hcc_sfc     \
                hcc_t1sfc   \
                hcc_t2sfc   \
                hcc_t3sfc   \
                hcc_t4sfc   \
                hcc_t5sfc   \
                hcc_t6sfc   \
                hcc_t7sfc   \
                hcc_t8sfc   \
                mnvv_sfc    \
                mnvv_t1sfc  \
                mnvv_t2sfc  \
                mnvv_t3sfc  \
                mnvv_t4sfc  \
                mnvv_t5sfc  \
                mnvv_t6sfc  \
                mnvv_t7sfc  \
                mnvv_t8sfc  \
                mref_sfc    \
                mref_t1sfc  \
                mref_t2sfc  \
                mref_t3sfc  \
                mref_t4sfc  \
                mref_t5sfc  \
                mref_t6sfc  \
                mref_t7sfc  \
                mref_t8sfc  \
                mucp_sfc    \
                mucp_t1sfc  \
                mucp_t2sfc  \
                mucp_t3sfc  \
                mucp_t4sfc  \
                mucp_t5sfc  \
                mucp_t6sfc  \
                mucp_t7sfc  \
                mucp_t8sfc  \
                mulcp_sfc   \
                mulcp_t1sfc \
                mulcp_t2sfc \
                mulcp_t3sfc \
                mulcp_t4sfc \
                mulcp_t5sfc \
                mulcp_t6sfc \
                mulcp_t7sfc \
                mulcp_t8sfc \
                mxcp_sfc    \
                mxcp_t1sfc  \
                mxcp_t2sfc  \
                mxcp_t3sfc  \
                mxcp_t4sfc  \
                mxcp_t5sfc  \
                mxcp_t6sfc  \
                mxcp_t7sfc  \
                mxcp_t8sfc  \
                1hsm_sfc    \
                1hsm_t1sfc  \
                1hsm_t2sfc  \
                1hsm_t3sfc  \
                1hsm_t4sfc  \
                1hsm_t5sfc  \
                1hsm_t6sfc  \
                1hsm_t7sfc  \
                1hsm_t8sfc  \
                3hsm_sfc    \
                3hsm_t1sfc  \
                3hsm_t2sfc  \
                3hsm_t3sfc  \
                3hsm_t4sfc  \
                3hsm_t5sfc  \
                3hsm_t6sfc  \
                3hsm_t7sfc  \
                3hsm_t8sfc  \
                vig_sfc     \
                vig_t1sfc   \
                vig_t2sfc   \
                vig_t3sfc   \
                vig_t4sfc   \
                vig_t5sfc   \
                vig_t6sfc   \
                vig_t7sfc   \
                vig_t8sfc   \
                s1shr_sfc   \
                s1shr_t1sfc \
                s1shr_t2sfc \
                s1shr_t3sfc \
                s1shr_t4sfc \
                s1shr_t5sfc \
                s1shr_t6sfc \
                s1shr_t7sfc \
                s1shr_t8sfc \
                6kshr_sfc   \
                6kshr_t1sfc \
                6kshr_t2sfc \
                6kshr_t3sfc \
                6kshr_t4sfc \
                6kshr_t5sfc \
                6kshr_t6sfc \
                6kshr_t7sfc \
                6kshr_t8sfc \
                temp_500    \
                temp_t1500  \
                temp_t2500  \
                temp_t3500  \
                temp_t4500  \
                temp_t5500  \
                temp_t6500  \
                temp_t7500  \
                temp_t8500  \
                temp_700    \
                temp_t1700  \
                temp_t2700  \
                temp_t3700  \
                temp_t4700  \
                temp_t5700  \
                temp_t6700  \
                temp_t7700  \
                temp_t8700  \
                temp_850    \
                temp_t1850  \
                temp_t2850  \
                temp_t3850  \
                temp_t4850  \
                temp_t5850  \
                temp_t6850  \
                temp_t7850  \
                temp_t8850  \
                temp_925    \
                temp_t1925  \
                temp_t2925  \
                temp_t3925  \
                temp_t4925  \
                temp_t5925  \
                temp_t6925  \
                temp_t7925  \
                temp_t8925  \
                1ref_sfc    \
                1ref_t1sfc  \
                1ref_t2sfc  \
                1ref_t3sfc  \
                1ref_t4sfc  \
                1ref_t5sfc  \
                1ref_t6sfc  \
                1ref_t7sfc  \
                1ref_t8sfc  \
                bli_sfc     \
                bli_t1sfc   \
                bli_t2sfc   \
                bli_t3sfc   \
                bli_t4sfc   \
                bli_t5sfc   \
                bli_t6sfc   \
                bli_t7sfc   \
                bli_t8sfc   \
                ulwrf_nta   \
                ulwrf_t1nta \
                ulwrf_t2nta \
                ulwrf_t3nta \
                ulwrf_t4nta \
                ulwrf_t5nta \
                ulwrf_t6nta \
                ulwrf_t7nta \
                ulwrf_t8nta \
                lhtfl_sfc   \
                lhtfl_t1sfc \
                lhtfl_t2sfc \
                lhtfl_t3sfc \
                lhtfl_t4sfc \
                lhtfl_t5sfc \
                lhtfl_t6sfc \
                lhtfl_t7sfc \
                lhtfl_t8sfc \
                shtfl_sfc   \
                shtfl_t1sfc \
                shtfl_t2sfc \
                shtfl_t3sfc \
                shtfl_t4sfc \
                shtfl_t5sfc \
                shtfl_t6sfc \
                shtfl_t7sfc \
                shtfl_t8sfc \
                flru_sfc    \
                flru_t1sfc  \
                flru_t2sfc  \
                flru_t3sfc  \
                flru_t4sfc  \
                flru_t5sfc  \
                flru_t6sfc  \
                flru_t7sfc  \
                flru_t8sfc  \
                wpwr_80m    \
                wpwr_t180m  \
                wpwr_t280m  \
                wpwr_t380m  \
                wpwr_t480m  \
                wpwr_t580m  \
                wpwr_t680m  \
                wpwr_t780m  \
                wpwr_t880m  \
                solar_sfc   \
                solar_t1sfc \
                solar_t2sfc \
                solar_t3sfc \
                solar_t4sfc \
                solar_t5sfc \
                solar_t6sfc \
                solar_t7sfc \
                solar_t8sfc \
                ectp_sfc    \
                ectp_t1sfc  \
                ectp_t2sfc  \
                ectp_t3sfc  \
                ectp_t4sfc  \
                ectp_t5sfc  \
                ectp_t6sfc  \
                ectp_t7sfc  \
                ectp_t8sfc  \
                vil_sfc     \
                vil_t1sfc   \
                vil_t2sfc   \
                vil_t3sfc   \
                vil_t4sfc   \
                vil_t5sfc   \
                vil_t6sfc   \
                vil_t7sfc   \
                vil_t8sfc   \
                rvil_sfc    \
                rvil_t1sfc  \
                rvil_t2sfc  \
                rvil_t3sfc  \
                rvil_t4sfc  \
                rvil_t5sfc  \
                rvil_t6sfc  \
                rvil_t7sfc  \
                rvil_t8sfc

ncl_error=0

CMDFN=/tmp/cmdfn.hrrr_part2.$$
${RM} -f $CMDFN

# Run the NCL scripts for each plot
cp /whome/wrfruc/bin/ncl/Airpor* .
cp ${EXE_ROOT}/names_grib2.txt .
i=0
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
# NOTE: ctrans ONLY works for 32-bit versions of NCL

i=0
while [ ${i} -lt ${#ncgms[@]} ]; do

  plot=${ncgms[${i}]}

#  ${ECHO} "Starting ctrans for ${plot}.ncgm at `${DATE}`"
## normal image
##  ${CTRANS} -d sun ${plot}.ncgm -resolution 1510x1208 > ${plot}.ras
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
#    ${CONVERT} -colors 128 -trim -border 25x25 -bordercolor black ${plot}.ras ${plot}.png
#    error=$?
#    if [ ${error} -ne 0 ]; then
#      ${ECHO} "ERROR: convert ${plot}.ras crashed!  Exit status=${error}"
#      ncl_error=${error}
#    fi
    echo "${CONVERT} -colors 128 -trim -border 25x25 -bordercolor black ${plot}.ras ${plot}.png" >> $CMDFN
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

# Remove the workdir
  ${RM} -rf ${workdir}

${ECHO} "ncl.ksh completed at `${DATE}`"

exit ${ncl_error}
