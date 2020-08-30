#!/bin/sh
export USER='Edward.Colon'
export CDATE='202006271230'
export YMD='20200627'
export cyc='12'
export subcyc='30'
export ROCOTO_TASK_GEO='{(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)}'
bsub -q dev -P RTMA-T2O -W 0:45 -R span[ptile=16] -n 16 -J makeplots_compareDA -o /gpfs/dell2/stmp/Edward.Colon/transfer_logs/log.makeplots.compareDA.202006271230.out /gpfs/dell3/usrx/local/dev/emc_rocoto/rocoto-1.2.4/sbin/lsfwrapper.sh /gpfs/dell2/emc/modeling/noscrub/Edward.Colon/python.rtma3d/compare_da/launchALL
