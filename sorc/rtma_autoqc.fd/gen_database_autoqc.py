#!/usr/bin/env python

from datetime import datetime,timedelta
import numpy as np
import pandas as pd
import os,sys,math
import sqlite3
from functools import reduce
from collections import OrderedDict

def gc_dist(lat1,lon1,lat2,lon2):
# Return the great circle distance (km) between two pairs of lat/lon points
  EARTH_RADIUS = 6378137 # earth radius in meters
  dLat = np.radians(lat2 - lat1)
  dLon = np.radians(lon2 - lon1)
  a = (np.sin(dLat / 2) * np.sin(dLat / 2) +
  np.cos(np.radians(lat1)) * np.cos(np.radians(lat2)) *
  np.sin(dLon / 2) * np.sin(dLon / 2))
  c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))
  d = EARTH_RADIUS * c
  return d/1000.0

def calc_sza(dat_var,dateobj):

  cyc_YYYY=np.int(dateobj[0:4])
  cyc_MM=np.int(dateobj[4:6])
  cyc_DD=np.int(dateobj[6:8])
  cyc_HH=np.int(dateobj[8:10])

  leap_day = 0
  if ( np.float(cyc_YYYY)%4==0 ):
    if ( np.float(cyc_YYYY)%100!=0 or np.float(cyc_YYYY)%400==0 ): leap_day = 1
  mday = [0,31,59,90,120,151,181,212,243,273,304,334]
  day_of_year = mday[cyc_MM-1] + cyc_DD
  if cyc_MM > 2: day_of_year += leap_day

  # Calculation solar declination
  declin=math.radians( 23.45*math.sin(2.0*math.pi*(284.+day_of_year)/365.) )

  # csza = fraction of solar constant (cos of zenith angle)
  dat_var['RLAT']=dat_var['LAT']*(math.pi/180.)
  dat_var['hrang']=(15.*cyc_HH + dat_var['LON']-180.)*(math.pi/180.) # cyc_HH in UTC
  dat_var['csza']=np.sin(dat_var['RLAT'])*np.sin(declin)+np.cos(dat_var['RLAT'])*np.cos(declin)*np.cos(dat_var['hrang'])
  dat_var['csza']=np.maximum(-1.,np.minimum(dat_var['csza'],1.))
  dat_var['sza']=np.arccos(dat_var['csza'])*(180./math.pi)

  return dat_var

def gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse):

  # If file exists, merge new dat_var array with data from existing database
  if os.path.exists(COMm1+'/'+exp+'_'+thisRUN+'_'+vars[var]+'.db'):
    # Open the connection to the SQLite database (will create one if it doesn't exist)
    cnx = sqlite3.connect(COMm1+'/'+exp+'_'+thisRUN+'_'+vars[var]+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    columns=list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_purge),columns[len(keep_cols):]))]
    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    cnx.close()

  # Open the connection to the SQLite database (will create one if it doesn't exist)
  cnx = sqlite3.connect(exp+'_'+thisRUN+'_'+vars[var]+'.db')

  dat_var = duplicates(dat_var)

  dat_var = gen_rjlist(dat_var,eps,geps,rjrmse)

  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('OB_','INC-GES_','GES_'))]

  dat_var_save = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  # Convert to SQL database and close connection
  dat_var_save['COUNT']=dat_var_save.groupby('SAID')['SAID'].transform('count')
  dat_var_save=dat_var_save[dat_var_save['COUNT']<max_dups]
  dat_var_save.drop(['COUNT'],axis=1,inplace=True)
  dat_var_save.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
  dat_var_save.to_csv(exp+'_'+thisRUN+'_'+vars[var]+'.csv', index=False)
  cnx.close()

  # Generate SQL database with statistics output

  # If file exists, merge new dat_var array with data from existing database
  if os.path.exists(COMm1+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'.db'):
    # Open the connection to the SQLite database (will create one if it doesn't exist)
    cnx = sqlite3.connect(COMm1+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    stat_cols =  list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_purge_stats),stat_cols[len(keep_cols):]))]

    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    cnx.close()

  # Open the connection to the SQLite database (will create one if it doesn't exist)
  cnx = sqlite3.connect(COM+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'.db')

  stats_cols=['counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_']
  dat_var_stats = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+stats_cols))]

  # Convert to SQL database and close connection
  dat_var_stats['COUNT']=dat_var_stats.groupby('SAID')['SAID'].transform('count')
  dat_var_stats=dat_var_stats[dat_var_stats['COUNT']<max_dups]
  dat_var_stats.drop(['COUNT'],axis=1,inplace=True)
  dat_var_stats.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
  dat_var_stats.to_csv(COM+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'.csv', index=False)
  cnx.close()

  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  return(dat_var)

def gen_rjlist(dat_var,eps,geps,rjrmse):

  ij = 0
  cyc_unpack = []
  rmse_flag=False
  itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H%M')
  while ij < max(num_cycs,num_stuck):
    cyc_unpack.append(itercyc.strftime('%Y%m%d%H%M'))
    itercyc = itercyc - delta
    ij+=1
  if len(cyc_unpack)>=num_cycs: rmse_flag=True

  dat_var_subset = dat_var.loc[:, [x for x in dat_var.columns if x.startswith('DAT_')]]
  dat_var_subset.replace(np.nan,'NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)

  for cyc in cyc_unpack:
    print('UNPACK = ',cyc_unpack)

    if 'DAT_'+cyc in dat_var.columns:
      dat_var['OB_'+cyc]=[np.float64(str(x).split('|')[0]) for x in dat_var_subset['DAT_'+cyc]]
      dat_var['INC-GES_'+cyc]=[np.float64(str(x).split('|')[1]) for x in dat_var_subset['DAT_'+cyc]]
      dat_var['GES_'+cyc]=[np.float64(str(x).split('|')[2]) for x in dat_var_subset['DAT_'+cyc]]

  ij = 0
  stuck_flag=False
  cyc_nstuck = []
  itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H%M')
  while ij < num_stuck:
    cyc_nstuck.append(itercyc.strftime('%Y%m%d%H%M'))
    itercyc = itercyc - delta
    ij+=1

  dat_var_stuck = dat_var.loc[:, dat_var.columns.str.endswith(tuple(keep_cols+cyc_nstuck))].copy()
  dat_var_stuck.dropna(inplace=True)

  if vars[var] in ['t','ps','q','wst']:

    dat_var_obs = dat_var_stuck.loc[:, [x for x in dat_var_stuck.columns if x.startswith('OB_')]]
    if dat_var_obs.shape[1]>=num_stuck:

      # Sanity check for Step #1 (Before)
      dat_var.to_csv('dat_var_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

      if dat_var_obs.shape[0]>0:
        #print('Performing the stuck instrument check for cycle',cyclestr)
        #print('First: Observations must vary less than "eps" over specified # of hours')
        dat_var_obs_subset=dat_var_obs
        suspect_obs = dat_var_stuck[dat_var_obs_subset.apply(lambda x: max(np.float64(x))-min(np.float64(x))<eps,axis=1)]
        if suspect_obs.shape[0]>0:
          # Sanity check for Step #1 (After)
          suspect_obs.to_csv('suspect_obs_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
          #print('Second: Background must vary more than "geps" over specified # of hours')
          dat_var_obs_subset=suspect_obs.loc[:, [y for y in suspect_obs.columns if y.startswith('GES_')]]
          stuck_inst = suspect_obs[dat_var_obs_subset.apply(lambda y: max(np.float64(y))-min(np.float64(y))>geps,axis=1)]
          # Finalize and write to CSV file
          if stuck_inst.shape[0]>0:
            stuck_flag=True
            stuck_inst[keep_cols].to_csv('stuck_inst_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

  # Run a check for flatlining temperature reports and merge results with the stuck instrument check to be included in
  # the generation of automated reject lists.
  if vars[var] in ['t']:
    dat_var_flat = dat_var_stuck[dat_var_stuck['PROVIDER']=='APRSWXNE'].copy()
    flat_inst = dat_var_flat[dat_var_flat['OB_'+cyclestr]==233.15]
    if flat_inst.shape[0]>0: flat_inst[keep_cols].to_csv('flat_inst_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
    if stuck_flag==True:
      stuck_inst=pd.concat([stuck_inst,flat_inst],sort=True)
    else:
      stuck_inst=flat_inst
      stuck_flag=True

  if np.float(cycle_HH)%num_cycs==num_cycs-1 and dat_var.shape[0]>0 and rmse_flag==True:
    dat_var_sum = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]
    columns=list(dat_var_sum.columns)
    columns=columns[len(keep_cols):]
    cyc_delim=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(hours=-num_cycs)
    dat_var_sum=dat_var_sum[list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_delim),columns))]
    dat_var['counts_'+cyclestr]=dat_var_sum.loc[:, [x for x in dat_var_sum.columns if x.startswith('INC-GES_')]].count(axis=1)
    dat_var['SUM_OmFs_'+cyclestr]=dat_var_sum.loc[:, [x for x in dat_var_sum.columns if x.startswith('INC-GES_')]].sum(axis=1)
    dat_var['SUM_OmFs2_'+cyclestr]=dat_var_sum.loc[:, [x for x in dat_var_sum.columns if x.startswith('INC-GES_')]].pow(2).sum(axis=1)
    dat_var['RMSE_'+cyclestr]=np.sqrt(dat_var['SUM_OmFs2_'+cyclestr]/dat_var['counts_'+cyclestr])
    dat_var['Bias_'+cyclestr]=dat_var['SUM_OmFs_'+cyclestr]/dat_var['counts_'+cyclestr]

    dat_var['SUM_OmFs_'+cyclestr]=dat_var['SUM_OmFs_'+cyclestr].round(3)
    dat_var['SUM_OmFs2_'+cyclestr]=dat_var['SUM_OmFs2_'+cyclestr].round(3)
    dat_var['RMSE_'+cyclestr]=dat_var['RMSE_'+cyclestr].round(3)
    dat_var['Bias_'+cyclestr]=dat_var['Bias_'+cyclestr].round(3)

    if vars[var] in ['t','ps','q']:
      # VMAP currently defined as 1 everywhere
      dat_var_reject=dat_var[dat_var['RMSE_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var['VMAP']))]
      dat_var_reject[keep_cols].to_csv('reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      if stuck_flag==True: dat_var_reject=pd.concat([stuck_inst,dat_var_reject],sort=True)
      dat_var_reject.drop_duplicates(subset=keep_cols,inplace=True)
      dat_var_reject[keep_cols].to_csv('reject_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject[keep_cols],fname)
    elif vars[var] in ['wst']:
      # VMAP currently defined as 1 everywhere
      dat_var_meso=dat_var[(dat_var['PBUFTYP']==188) | (dat_var['PBUFTYP']==288)]
      dat_var_accept=dat_var_meso[dat_var_meso['RMSE_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_meso['VMAP']))]
      dat_var_accept[keep_cols].to_csv('accept_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      fname = 'accept_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_accept[keep_cols],fname)

      dat_var_metar=dat_var[(dat_var['PBUFTYP']!=188) & (dat_var['PBUFTYP']!=288)]
      dat_var_reject=dat_var_metar[dat_var_metar['RMSE_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var_metar['VMAP']))]
      dat_var_reject[keep_cols].to_csv('reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

      if stuck_flag==True: dat_var_reject=pd.concat([stuck_inst,dat_var_reject],sort=True)
      dat_var_reject.drop_duplicates(subset=keep_cols,inplace=True)

      dat_var_reject[keep_cols].to_csv('reject_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject[keep_cols],fname)

  else:

    ij = 0
    itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H%M')
    while ij < num_cycs:
      iter_HH = itercyc.strftime('%H')
      if (num_cycs - np.float64(iter_HH))%num_cycs==1:
        probecyc = itercyc.strftime('%Y%m%d%H%M')
        probeday = itercyc.strftime('%Y%m%d')
        probeHHMM = itercyc.strftime('%H%M')
        COMprior = os.path.abspath(os.path.join(os.path.dirname(COM), '../'+'/'+exp+'.'+probeday+'/autoqcprd.t'+probeHHMM+'z'))
        print('COMPRIOR = ',COMprior)
        break
      itercyc = itercyc - delta
      ij+=1

    if vars[var] in ['t','ps','q','wst']:
      # Probe for previous partial reject list generated with RMSE stats and merge with stuck instrument check if file is found

      if os.path.exists(COMprior+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv'):
        prior_rjlist = pd.read_csv(COMprior+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv')

        if stuck_flag==True:
          dat_var_reject=pd.concat([stuck_inst,prior_rjlist],sort=True)
          dat_var_reject.drop_duplicates(subset=keep_cols,inplace=True)
        else:
          dat_var_reject=prior_rjlist

      elif not os.path.exists(COMprior+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv') and stuck_flag==True:
        dat_var_reject=stuck_inst

      else:
        dat_var_reject=pd.DataFrame(columns=keep_cols)

      dat_var_reject[keep_cols].to_csv('reject_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject[keep_cols],fname)

    if vars[var] in ['wst']:
      # Probe for previously-generated accept list for wind - this file should be identical to that one

      if os.path.exists(COMprior+'/accept_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv'):
        dat_var_accept = pd.read_csv(COMprior+'/accept_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv')
      else:
        dat_var_accept = pd.DataFrame(columns=keep_cols)

      dat_var_accept[keep_cols].to_csv('accept_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      fname = 'accept_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_accept[keep_cols],fname)

  return(dat_var)

def write_lists(input_list,fname):
  with open(fname,'w') as out_file:
    ltyp=fname.split('_')[0]
    if ltyp!='accept': out_file.write('********************************************************************************\n')
    if vars[var]=='t': out_file.write(fname.split('_')[0]+' list for temperature obs\n')
    elif vars[var]=='ps': out_file.write(fname.split('_')[0]+' list for surface pressure obs\n')
    elif vars[var]=='q': out_file.write(fname.split('_')[0]+' list for specific humidity obs\n')
    elif vars[var]=='wst' and ltyp!='accept': out_file.write(fname.split('_')[0]+' list for wind obs\n')
    if ltyp!='accept': out_file.write('********************************************************************************\n')
    for index, row in input_list.iterrows():
      if not(vars[var]=='wst' and ltyp=='accept'): line="'"+str(row[0]).ljust(8)+"| itype="+str(row[3])+"  lat="+str("%.4f" %row[4]).rjust(10)+"  lon="+\
        str("%.4f" %(row[5]-360)).rjust(10)+"  loc=US  origin: ---------------'"+'\n'
      else: line=str(row[0]).ljust(8)+"| itype="+str(row[3])+"  lat="+str("%.4f" %row[4]).rjust(10)+"  lon="+\
        str("%.4f" %(row[5]-360)).rjust(10)+"  loc=US  origin: ---------------"+'\n'
      out_file.write(line)

def duplicates(dat_var):

  dat_var_id_duplicates = dat_var[keep_cols+['DAT_'+cyclestr]].copy()
  dat_var_id_duplicates.dropna(subset=['DAT_'+cyclestr],inplace=True)
  # Find any duplicates in the newly read-in diagnostic file
  dat_var_new_duplicates = dat_var_id_duplicates[dat_var_id_duplicates.duplicated(['SAID'],keep=False)]
  dat_var_new_duplicates['LAST_CYC']=np.int(cyclestr)
  dat_var_new_duplicates = dat_var_new_duplicates[keep_cols+['LAST_CYC']]
  print('NEW DUPLICATES CURRENT CYCLE = ',dat_var_new_duplicates.shape[0])

  if os.path.exists(COMm1+'/'+'duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'.csv'):
    # Duplicates found in previous diagnostic files
    orig_duplicates = pd.read_csv(COMm1+'/'+'duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'.csv')
    print('ORIGINAL DUPLICATES = ',orig_duplicates.shape[0])
    stations = orig_duplicates['SAID'].str.strip()
    # Make sure none of the original duplicates show up using a different configuration
    new_duplicates = dat_var_id_duplicates.loc[dat_var_id_duplicates['SAID'].isin(stations)]
    new_duplicates = new_duplicates[keep_cols]
    new_duplicates['LAST_CYC'] = np.int(cyclestr)
    print('NEW DUPLICATES UNDER OLD IDS = ',new_duplicates.shape[0])
    # Merge together each list of duplicates
    dfs=[orig_duplicates,new_duplicates,dat_var_new_duplicates]
    dat_var_duplicates = pd.concat(dfs,sort=True)
  else:
    dat_var_duplicates = dat_var_new_duplicates

  print('BEFORE = ',dat_var_duplicates.shape[0])
  unique_stns = []
  for stn in dat_var_duplicates['SAID'].unique():
    dat_var_stn = dat_var_duplicates[dat_var_duplicates['SAID']==stn].sort_values(by=['LAT'])
    dat_var_stn[['LAT','LON']]=dat_var_stn[['LAT','LON']].astype(float)
    dat_var_stn['DISTANCE'] = gc_dist(dat_var_stn['LAT'], dat_var_stn['LON'],dat_var_stn['LAT'].shift(1), dat_var_stn['LON'].shift(1))
    dat_var_stn.dropna(subset=['DISTANCE'],inplace=True)
    if min(dat_var_stn['DISTANCE'].values)>10.0: unique_stns.append(stn)

  dat_var_duplicates = dat_var_duplicates.loc[~dat_var_duplicates['SAID'].isin(unique_stns)]
  print('AFTER = ',dat_var_duplicates.shape[0])

  dat_var_duplicates = dat_var_duplicates.sort_values('LAST_CYC')
  dat_var_duplicates.drop_duplicates(subset=keep_cols,keep='last',inplace=True)

  print('AFTER #2 = ',dat_var_duplicates.shape[0])

  # Write duplicates to a spreadsheet for reference
  dat_var_duplicates = dat_var_duplicates[keep_cols+['LAST_CYC']]
  dat_var_duplicates = dat_var_duplicates[dat_var_duplicates['LAST_CYC']>np.int(cyc_purge_dups.strftime('%Y%m%d%H'))]
  dat_var_duplicates = dat_var_duplicates[dat_var_duplicates.duplicated(['SAID'],keep=False)]
  if not dat_var_duplicates.empty: dat_var_duplicates.to_csv('duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'.csv', index=False)

  # Update: Allow the duplicate stations to populate the SQL database
  #dat_var = dat_var.loc[~dat_var['SAID'].isin(dat_var_duplicates['SAID'].values)]

  return(dat_var)

if __name__ == "__main__":

  dateobj=sys.argv[1]
  DATA=sys.argv[2]
  COM=sys.argv[3]
  COMm1=sys.argv[4]

  thisRUN='conus'
  exp='rtma3d'

  cyclestr=dateobj
  print('cyclestr = ',cyclestr)
  datestr=dateobj[0:8]
  cycle_HH=dateobj[8:10]
  cyc_purge=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-40)

  #--------------------------------------------------------------------#
  # Parameters for reading the diagnostic files later on.
  #--------------------------------------------------------------------#
  
  bmiss=0.10000E+10
  my_cols = ["OBTYPE","SAID","PROVIDER","SUBPROVIDER","PBUFTYP","DHR","LAT","LON",\
             "PRES","HGHT","IUSE","OB","INC","VOB","VINC","OBERR","RUSAGE"]
  usecols=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
  dtypedict={"OBTYPE":str,"SAID":str,"PROVIDER":str,"SUBPROVIDER":str,"PBUFTYP":np.int64,\
          "DHR":np.float64,"LAT":np.float64,"LON":np.float64,"PRES":np.float64,\
          "HGHT":np.float64,"IUSE":np.int64,"OB":np.float64,"INC":np.float64,\
          "VOB":np.float64,"VINC":np.float64,"OBERR":np.float64,"RUSAGE":np.float64}

  delta = timedelta(minutes=60)

  num_cycs=6 # Number of cycles to compute stats (RMSE, bias) over.
  num_stuck=6 # Number of cycles for the "stuck" instrument check.
  max_dups=5 # Maximum number of entries allowed per unique station ID

  epsw=0.001
  epst=0.001
  epsp=0.001
  epsq=0.0001

  gepsw=0.5
  gepst=0.5
  gepsp=0.5
  gepsq=0.05

  rjrmsew=5.0
  rjrmset=5.0
  rjrmsep=3.25
  rjrmseq=1.5

  keep_cols=['SAID','PROVIDER','SUBPROVIDER','PBUFTYP','LAT','LON','HGHT','VMAP']
  columns=keep_cols.copy()

  ndays_purge = math.ceil( (999-len(keep_cols))/((24./num_cycs)*5.) )
  print('TESTING PURGE = ',ndays_purge)
  cyc_purge_stats=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-ndays_purge)
  cyc_purge_dups=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-7)
  print('TESTING PURGE DUPS = ',cyc_purge_dups)

  diagdir=DATA

  #--------------------------------------------------------------------#
  # Read in data from the analysis (anl) diagnostic file.
  #--------------------------------------------------------------------#

  diagfile_anl=diagdir+'/diag_conv_anl.'+cyclestr
  dat_anl=pd.read_table(diagfile_anl,names=my_cols,dtype=dtypedict,encoding='latin1',
               delim_whitespace=True,usecols=usecols,header=None,na_values=['Infinity',bmiss])
  dat_anl.dropna(subset=['OB','INC'])

  #--------------------------------------------------------------------#
  # Read in data from the background (ges) diagnostic file.
  #--------------------------------------------------------------------#

  diagfile_ges=diagdir+'/diag_conv_ges.'+cyclestr
  dat_ges=pd.read_table(diagfile_ges,names=my_cols,dtype=dtypedict,encoding='latin1',
               delim_whitespace=True,usecols=usecols,header=None,na_values=['Infinity',bmiss])
  dat_ges.dropna(subset=['OB','INC'])

  #--------------------------------------------------------------------#
  # Address instances of duplicates observations.
  #--------------------------------------------------------------------#

  # Filter out observations from report types 196-199 (GLERL)
  dat_anl = dat_anl[((dat_anl['PBUFTYP']>180) & (dat_anl['PBUFTYP']<=195)) | ((dat_anl['PBUFTYP']>280) & (dat_anl['PBUFTYP']<=295))]
  dat_ges = dat_ges[((dat_ges['PBUFTYP']>180) & (dat_ges['PBUFTYP']<=195)) | ((dat_ges['PBUFTYP']>280) & (dat_ges['PBUFTYP']<=295))]

  # Filter out observations from station IDs that may be used for multiple platforms
  dat_anl = dat_anl[(dat_anl['SAID']!='/////') & (dat_anl['SAID']!='SHIP')]
  dat_ges = dat_ges[(dat_ges['SAID']!='/////') & (dat_ges['SAID']!='SHIP')]
  # Filter out observations from stations ending in "__a"
  #dat_anl = dat_anl[~dat_anl['SAID'].str.endswith('__a')]
  #dat_ges = dat_ges[~dat_ges['SAID'].str.endswith('__a')]
  # Remove '__a' from end of station IDs to allow match with other reports
  dat_anl['SAID'] = dat_anl['SAID'].str.replace('____a','')
  dat_ges['SAID'] = dat_ges['SAID'].str.replace('____a','')
  dat_anl['SAID'] = dat_anl['SAID'].str.replace('___a','')
  dat_ges['SAID'] = dat_ges['SAID'].str.replace('___a','')
  dat_anl['SAID'] = dat_anl['SAID'].str.replace('__a','')
  dat_ges['SAID'] = dat_ges['SAID'].str.replace('__a','')

  #--------------------------------------------------------------------#
  # Reverse the time series ordering of diagnostic files and merge
  # together the anl and ges files.
  #--------------------------------------------------------------------#

  dat_anl['VMAP']=1.0 # MTM
  dat_ges['VMAP']=1.0 # MTM

  # Added [::-1] to reverse time series, such that positive DHRs will be encountered before negative DHRs, thus giving those
  # preference in the event that two DHRs with same absolute value, but of opposite sign, are encoutnered.  This is how these
  # observations are selected in the GSI ob selection algorithm to only use the observation valid closest to the analysis time.
  dat_anl=dat_anl[::-1]
  dat_ges=dat_ges[::-1]

  dfs = [dat_anl,dat_ges]

  for j, df in enumerate(dfs, start=0):
    if j==0: df.rename(columns={col:'{}-ANL'.format(col, j) for col in ('IUSE','INC','VINC','RUSAGE')}, inplace=True)
    if j==1: df.rename(columns={col:'{}-GES'.format(col, j) for col in ('IUSE','INC','VINC','RUSAGE')}, inplace=True)

  merge_cols=keep_cols+['OBTYPE','DHR','OB','VOB']
  dat_merged = reduce(lambda left,right: pd.merge(left,right,on=merge_cols), dfs)

  dat_merged.replace({'PBUFTYP': {192:181, 193:187, 194:183, 195:188, 292:281, 293:287, 294:284, 295:288}},inplace=True)

  dat_merged = dat_merged[(dat_merged['PBUFTYP']!=290) & (dat_merged['PROVIDER']!='GST-MoPE')]

  dat_merged = dat_merged[((dat_merged['PBUFTYP']==187) | (dat_merged['PBUFTYP']==188)) | ((dat_merged['PBUFTYP']==287) | (dat_merged['PBUFTYP']==288))]

  dat_merged['GES']=dat_merged['OB']-dat_merged['INC-GES']

  # Loop through each variable
  vars=['t','ps','q','wst']
  for var in range(len(vars)):

    if vars[var]=='t': var_str='temperature'; eps=epst; geps=gepst; rjrmse=rjrmset
    elif vars[var]=='ps': var_str='pressure'; eps=epsp; geps=gepsp; rjrmse=rjrmsep
    elif vars[var]=='q': var_str='moisture'; eps=epsq; geps=gepsq; rjrmse=rjrmseq
    elif vars[var]=='wst': var_str='wind_speed'; eps=epsw; geps=gepsw; rjrmse=rjrmsew

    #dat_var=dat_merged[dat_merged['OBTYPE']==vars[var]].copy()

    if vars[var]=='wst':

      dat_var=dat_merged[dat_merged['OBTYPE']=='uv'].copy()

      dat_var['WSPD_OB'] = np.sqrt( dat_var['OB']**2 + dat_var['VOB']**2 )
      dat_var['WSPD_GES'] = np.sqrt( (dat_var['OB'] - dat_var['INC-GES'])**2 + (dat_var['VOB'] - dat_var['VINC-GES'])**2 )
      dat_var['WSPD_INC'] = dat_var['WSPD_OB'] - dat_var['WSPD_GES']

      dat_var.drop(['OB','GES','INC-GES','VOB','VINC-GES'],axis=1,inplace=True)
      dat_var.rename(columns={"WSPD_OB": "OB", "WSPD_GES": "GES", "WSPD_INC": "INC-GES"},inplace=True)

    else:

      dat_var=dat_merged[dat_merged['OBTYPE']==vars[var]].copy()

    # Find the observation selected for assimilation (i.e., nearest analysis time)
    dat_var['ABSDHR']=dat_var['DHR'].abs()
    dat_var = dat_var.loc[dat_var.groupby(keep_cols)['ABSDHR'].idxmin()]

    # Retain the following columns
    dat_var=dat_var[keep_cols+['OB','INC-GES','GES','IUSE-GES','RUSAGE-GES']]

    dat_var['OB']=dat_var['OB'].round(3); dat_var['INC-GES']=dat_var['INC-GES'].round(3); dat_var['GES']=dat_var['GES'].round(3)

    dat_var['DAT_'+cyclestr] = dat_var['OB'].map(str) + '|' + dat_var['INC-GES'].map(str) + '|' + dat_var['GES'].map(str) + '|' + dat_var['IUSE-GES'].map(str) + '|' + dat_var['RUSAGE-GES'].map(str)

    dat_var.rename(columns={col:('{}_'+cyclestr).format(col,dat_var) for col in ('OB','INC-GES','GES','IUSE-GES','RUSAGE-GES')}, inplace=True)

    #dat_var=calc_sza(dat_var,dateobj)

    dat_var=dat_var[keep_cols+['DAT_'+cyclestr]]

    gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse)

  f_out='done.'+cyclestr
  with open(f_out,'w') as out_file:
    out_file.write('AUTOQC step has completed successfully for '+cyclestr)

