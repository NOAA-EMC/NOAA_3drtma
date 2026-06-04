#! /bin/ksh 
###################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_sndpost.sh.ecf
# Script description:  Create HRRR bufr sounding files
#
# Author:        Geoff Manikin      NCEP/EMC 
# 2014-08-01  G Manikin - new script 
# 2016-02-05  G Manikin - HRRRv2 (extension to f18)
# 2018-01-24  B Blake / G Manikin - HRRRv3
# 2026-05-24  A Gibbs / E Colon - RTMA3Dv1
####################################################
set -x

export PS4='SNDP $SECONDS + '

cpreq ${PARMrtma3d}/${RUN}ak_bufr.tbl .
cpreq ${PARMrtma3d}/${RUN}ak_modtop.parm .
cpreq ${PARMrtma3d}/${RUN}ak_sndp.parm.mono sndp.parm
cpreq ${PARMrtma3d}/${RUN}_sndp_input sndp_input

fhr=00
tmmark=tm00

cpreq ${DATA_SHARED}/profilm.c1.f${cyc} profilm.c1.${tmmark}

ln -sf sndp.parm    fort.11
ln -sf ${RUN}ak_bufr.tbl fort.32
ln -sf profilm.c1.${tmmark} fort.66
ln -sf class1.bufr fort.78

export pgm="${NET}_sndp"
. prep_step
startmsg

${EXECrtma3d}/${pgm} < ${RUN}ak_modtop.parm  > sndp.out
export err=$?; err_chk

if [ $SENDCOM == "YES" ]; then
  cpreq class1.bufr ${COMOUT}/${RUN}ak.t${cyc}z.class1.bufr
  cpreq profilm.c1.${tmmark} ${COMOUT}/${RUN}ak.t${cyc}z.profilm.c1
fi

### break out bufr file into individual station files
cat <<EOF > stnmlist_input
1
class1.bufr
bufr.${cyc}/bufr
EOF

mkdir -p bufr.${cyc}

ln -sf class1.bufr fort.20
export DIRD=bufr.${cyc}/bufr

export pgm=${NET}_stnmlist
. prep_step
startmsg
${EXECrtma3d}/${pgm} < stnmlist_input >> $pgmout 2> errfile
export err;err_chk

# Tar and gzip the individual bufr files and send them to COM
mkdir -p ${COMOUT}/bufrsnd.t${cyc}z
cd bufr.${cyc}
tar -cf - . | /usr/bin/gzip > ../hrrr.${cyc}.bufrsnd.tar.gz
cpreq ../hrrr.${cyc}.bufrsnd.tar.gz $COMOUT/${RUN}ak.t${cyc}z.bufrsnd.tar.gz
cpreq * ${COMOUT}/bufrsnd.t${cyc}z

postmsg "$0 of $job completed normally"

date
