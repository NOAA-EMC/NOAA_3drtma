#!/bin/bash

  set -eux

  export BASE=`pwd`
  cd $BASE

  PARMDIR_SOURCE='/lfs/h2/emc/da/noscrub/matthew.t.morris/packages/rtma3d.v1.0.0/parm'
  FIXDIR_SOURCE='/lfs/h2/emc/da/noscrub/matthew.t.morris/packages/rtma3d.v1.0.0/fix'
#  PARMDIR_SOURCE='/lfs/h2/emc/da/noscrub/annette.gibbs/packages/rtma3d.v1.0.0_nco2/parm'
#  FIXDIR_SOURCE='/lfs/h2/emc/da/noscrub/annette.gibbs/packages/rtma3d.v1.0.0_nco2/fix'

  GSIPARMDIR_CONUS='/lfs/h2/emc/da/noscrub/gang.zhao/FixData/GSI_rtma3d_mine/parm/conus_new'
  GSIPARMDIR_ALASKA='/lfs/h2/emc/da/noscrub/gang.zhao/FixData/GSI_rtma3d_mine/parm/alaska_new'

# copy or link parm data
  echo "copying or linking parm data from ${PARMDIR_SOURCE}"
  [[ -d $BASE/../parm ]] || mkdir $BASE/../parm
  cd $BASE/../parm
  parmdirs_list='alaska conus upp wrf'
  for pdir in ${parmdirs_list}
  do
      echo "copying or linking parm data for ${pdir}"
      rm -rf ./{pdir}
      case "${pdir}" in
          "gsi")
              cp -pr ${PARMDIR_SOURCE}/${pdir}   ./${pdir}
#             ln -sf ${PARMDIR_SOURCE}/${pdir}   ./${pdir}
              ;;
              *)
              cp -pr ${PARMDIR_SOURCE}/${pdir}   ./${pdir}
#             ln -sf ${PARMDIR_SOURCE}/${pdir}   ./${pdir}
              ;;
      esac
  done
  ls -l

#====> copying mchanged parm files from different sources
  cd $BASE/../parm
# copying parm files for new UPP (from upp package)
  mv upp upp_old
  mkdir upp
  cp -p $BASE/../sorc/rtma3d_post.fd/parm/params_grib2_tbl*                   ./upp/
  cp -p $BASE/../sorc/rtma3d_post.fd/parm/3drtma/3drtma_postcntrl.xml         ./upp/
  cp -p $BASE/../sorc/rtma3d_post.fd/parm/3drtma/postxconfig-NT-3drtma.txt    ./upp/
  cp -p $BASE/../sorc/rtma3d_post.fd/fix/*micro_lookup.dat                    ./upp/
  ls -l ./upp

# copying the modified gsiparm.anl.sh files for new GSI (from Gang's directory)
  cd $BASE/../parm/conus
  mkdir gsi_old
  mv rtma3d_gsiparm.anl*.sh   ./gsi_old/
  cp -p ${GSIPARMDIR_CONUS}/rtma3d_gsiparm.anl_howv.sh     ./
  cp -p ${GSIPARMDIR_CONUS}/rtma3d_gsiparm.anl.sh          ./
  ls -l
  cd $BASE
  
  cd $BASE/../parm/alaska
  mkdir gsi_old
  mv rtma3dak_gsiparm.anl*.sh ./gsi_old/
  cp -p ${GSIPARMDIR_ALASKA}/rtma3dak_gsiparm.anl_howv.sh   ./
  cp -p ${GSIPARMDIR_ALASKA}/rtma3dak_gsiparm.anl.sh        ./
  ls -l
  cd $BASE

# copy or link fix data
  echo "copying or linking fix data from ${PARMDIR_SOURCE}"
  [[ -d $BASE/../fix ]] || mkdir $BASE/../fix
  cd $BASE/../fix
  fixdirs_list='alaska  conus  minmax  obsuselist  upp  wrf  wrfbufr'
  for fdir in ${fixdirs_list}
  do
      echo "copying or linking fix data for ${fdir}"
      rm -rf ./{fdir}
      case "${fdir}" in
          "obsuselist")
#             cp -pr ${FIXDIR_SOURCE}/${fdir}   ./${fdir}
              ln -s  ${FIXDIR_SOURCE}/${fdir}   ./${fdir}
              ;;
              *)
              cp -pr ${FIXDIR_SOURCE}/${fdir}   ./${fdir}
#             ln -s  ${FIXDIR_SOURCE}/${fdir}   ./${fdir}
              ;;
      esac
  done
  ls -l

# copying fix files for new UPP (from upp package)
  cp -p $BASE/../sorc/rtma3d_post.fd/fix/*micro_lookup.dat                    ./upp/

  cd $BASE
   
# [[ -d ${BASE}/../workflow/logs ]] || mkdir ${BASE}/../workflow/logs

#----
# cd ${BASE}/../workflow
# ln -sf /u/gang.zhao/my_util/mystat_rocoto.sh       ./myStat
# ln -sf /u/gang.zhao/my_util/mycheck_rocoto.sh      ./myChk
# ln -sf /u/gang.zhao/my_util/myrun_rocoto.sh        ./myRun
# ln -sf /u/gang.zhao/my_util/myboot_rocoto.sh       ./myBoot

  exit
