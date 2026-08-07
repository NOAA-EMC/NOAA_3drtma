#!/bin/ksh 
set -x

export OMP_NUM_THREADS=1

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${PARMrtma3d}/${RUN}_run_LANDUSE.TBL          \
                     ${PARMrtma3d}/${RUN}_run_RRTM_DATA            \
                     ${PARMrtma3d}/${RUN}_run_RRTM_DATA_DBL        \
                     ${PARMrtma3d}/${RUN}_run_RRTMG_LW_DATA        \
                     ${PARMrtma3d}/${RUN}_run_RRTMG_LW_DATA_DBL    \
                     ${PARMrtma3d}/${RUN}_run_RRTMG_SW_DATA        \
                     ${PARMrtma3d}/${RUN}_run_RRTMG_SW_DATA_DBL    \
                     ${PARMrtma3d}/${RUN}_run_VEGPARM.TBL          \
                     ${PARMrtma3d}/${RUN}_run_GENPARM.TBL          \
                     ${PARMrtma3d}/${RUN}_run_SOILPARM.TBL         \
                     ${PARMrtma3d}/${RUN}_run_MPTABLE.TBL          \
                     ${PARMrtma3d}/${RUN}_run_URBPARM.TBL          \
                     ${PARMrtma3d}/${RUN}_run_URBPARM_UZE.TBL      \
                     ${PARMrtma3d}/${RUN}_run_ETAMPNEW_DATA        \
                     ${PARMrtma3d}/${RUN}_run_ETAMPNEW_DATA.expanded_rain        \
                     ${PARMrtma3d}/${RUN}_run_ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${PARMrtma3d}/${RUN}_run_ETAMPNEW_DATA_DBL    \
                     ${PARMrtma3d}/${RUN}_run_co2_trans            \
                     ${PARMrtma3d}/${RUN}_run_ozone.formatted      \
                     ${PARMrtma3d}/${RUN}_run_ozone_lat.formatted  \
                     ${PARMrtma3d}/${RUN}_run_ozone_plev.formatted \
                     ${PARMrtma3d}/${RUN}_run_bulkdens.asc_s_0_03_0_9 \
                     ${PARMrtma3d}/${RUN}_run_bulkradii.asc_s_0_03_0_9  \
                     ${PARMrtma3d}/${RUN}_run_capacity.asc         \
                     ${PARMrtma3d}/${RUN}_run_CCN_ACTIVATE.BIN     \
                     ${PARMrtma3d}/${RUN}_run_coeff_p.asc          \
                     ${PARMrtma3d}/${RUN}_run_coeff_q.asc          \
                     ${PARMrtma3d}/${RUN}_run_constants.asc        \
                     ${PARMrtma3d}/${RUN}_run_kernels.asc_s_0_03_0_9  \
                     ${PARMrtma3d}/${RUN}_run_kernels_z.asc           \
                     ${PARMrtma3d}/${RUN}_run_masses.asc              \
                     ${PARMrtma3d}/${RUN}_run_termvels.asc            \
                     ${PARMrtma3d}/${RUN}_run_wind-turbine-1.tbl      \
                     ${PARMrtma3d}/${RUN}_run_tr49t85              \
                     ${PARMrtma3d}/${RUN}_run_tr49t67              \
                     ${PARMrtma3d}/${RUN}_run_tr67t85              \
                     ${PARMrtma3d}/${RUN}_run_grib2map.tbl         \
                     ${PARMrtma3d}/${RUN}_run_gribmap.txt          \
                     ${PARMrtma3d}/${RUN}_run_freezeH2O.dat        \
                     ${PARMrtma3d}/${RUN}_run_qr_acr_qg.dat        \
                     ${PARMrtma3d}/${RUN}_run_qr_acr_qs.dat        \
                     ${PARMrtma3d}/${RUN}_run_eclipse_besselian_elements.dat

for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    echo "FATAL ERROR: ${file} either does not exist or is empty"
    err_exit
  fi
done

export WRF_NAMELIST=namelist.input
cpreq ${PARMrtma3d}/${RUN}_wrf.nl ${WRF_NAMELIST}      # No IO-Quilting in wrf namelist as default

if [ -r ${DATA_SHARED}/wrf_inout ]; then
  echo " Initial condition ==> ${DATA_SHARED}/wrf_inout "
  cpreq ${DATA_SHARED}/wrf_inout wrf_inout
  ln -s wrf_inout wrfinput_d01
else
  err_exit "FATAL ERROR: ${DATA_SHARED}/wrf_inout does not exist, or is not readable"
fi


# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  tempfile=`basename ${file}`
  tempname=`echo ${tempfile} | sed s/${RUN}_run_//`
  rm -f ${tempname}
  ln -sf ${file} ${tempname}
done

start_year=${CDATE:0:4}
start_month=${CDATE:4:2}
start_day=${CDATE:6:2}
start_hour=${CDATE:8:2}
start_minute=00
start_second=00
end_year=${CDATE:0:4}
end_month=${CDATE:4:2}
end_day=${CDATE:6:2}
end_hour=${CDATE:8:2}
end_minute=00
end_second=20

time_str="${start_year}-${start_month}-${start_day}_${start_hour}_${start_minute}_${start_second}"

# Compute number of days and hours for the run
(( run_days = 0 ))
(( run_hours = 0 ))

# Create patterns for updating the wrf namelist
run=[Rr][Uu][Nn]
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
year=[Yy][Ee][Aa][Rr]
month=[Mm][Oo][Nn][Tt][Hh]
day=[Dd][Aa][Yy]
hour=[Hh][Oo][Uu][Rr]
minute=[Mm][Ii][Nn][Uu][Tt][Ee]
second=[Ss][Ee][Cc][Oo][Nn][Dd]

# Update the run_days,run_hours,start_time,end_time in wrf namelist.input
sed -i "\
   s/\(${run}_${day}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_days}/;    \
   s/\(${run}_${hour}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_hours}/;  \
\
   s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/;    \
   s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/;  \
   s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/;      \
   s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/;    \
   s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/;\
   s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/;\
\
   s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/;        \
   s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/;      \
   s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/;          \
   s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/;        \
   s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/;    \
   s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/;    \
" ${WRF_NAMELIST}

# Run WRF to update reflectivity fields
echo "run with modified WRF model that does not actually foreast"
export pgm="${NET}_wrfarw_nofcst"              # using the modified WRF which does not integrate
. prep_step
startmsg

echo "  begin updating reflectivity by a one-time_step WRF"

APRUN="mpiexec -n $ntasks -ppn $ppn --cpu-bind core "
$APRUN ${EXECrtma3d}/${pgm} >>$pgmout 2>errfile
export err=$?; err_chk

# Check to see if the 0h output is there:
if [ -e "wrfout_d01_${time_str}" ]; then
  ln -s wrfout_d01_${time_str} wrfout_d01
else
  err_exit "FATAL ERROR: WRF failed at the first time step!"
fi 

# Output successful so write status to log
echo "Assemble Reflectivity fields back into wrf_inout"

ncks -A -v REFL_10CM,COMPOSITE_REFL_10CM,REFL_10CM_1KM,REFL_10CM_4KM wrfout_d01 wrf_inout

# Updating the pressure fields (including surface pressure) in the analysis file (wrf_inout)
if [[ ${RUN_UPDATEP:-"No"} =~ [yYtT] ]] ; then
    echo "Pressure fields in wrf_inout will be updated ... "
    if [[ -f wrf_inout ]] ; then
        export OMP_NUM_THREADS=1
        echo "*****************************************************"
        echo "******* Updating Pressure Fields in wrf_inout *******"
#       ${EXECrtma3d}/rtma3d_updateP >>$pgmout 2> errfile
        ${EXECrtma3d}/rtma3d_updateP
        echo "******* End of Updating Pressure Fields       *******"
        export err=$?; err_chk
        echo "Pressure fields (including Ps) are updated in analysis file (wrf_inout)"
    else
        echo "WARNING: Analysis file (wrf_inout) is missing, no rtma3d_updateP will run ..."
    fi
else
    echo "Pressure fields in wrf_inout are NOT be updated by code rtma3d_updateP ... "
fi

cpreq -p wrf_inout ${COMOUT}/${RUN}.t${cyc}z.wrf_inout.nc
cpreq -p rsl.out.0000 ${COMOUT}/${RUN}.t${cyc}z.rslout
cat rsl.out.0000

postmsg "$0 of $job completed normally"

