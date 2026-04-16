#!/usr/bin/env python

from datetime import datetime,timedelta
import numpy as np
import pandas as pd
import os,sys,math,re
import sqlite3
from functools import reduce

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
  dat_var['SZA']=np.arccos(dat_var['csza'])*(180./math.pi)

  return dat_var

def windbias(dat_var):
  # Begin by making sure the prior precision weight w_hat, and associated
  # log-bias, alpha, are updated to the present time, and use this
  # up-to-date alpha, and its implied bias correction factor, a,
  # to bias-correct the observed wind u and v velocity components:

  # If the background and raw-observation speeds exceed a low threshold,
  # use the logarithm of the ratio of background speed to observed speed
  # as an "observation" (a very noisy one) of the negative of the
  # logarithmic bias of the raw observation, and combine this "observation"
  # with the current best estimate (alpha) of it to produce an updated
  # value (also called alpha), and correspondingly update the reliability
  # weight, or precision weight, w.

  # Current time in days since epoch
  currcyc = datetime.strptime(str(cyclestr),'%Y%m%d%H')
  t_now = (currcyc - epochcyc).total_seconds()/timedelta(days=1).total_seconds()
  dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 't_now_'+cyclestr] = t_now

  # Set t_bar and alpha_bar values to those from preceding cycle, if available; otherwise, use default values.
  try:
    dat_var['t_bar_'+cyclestr]=dat_var['t_bar_'+cyclestr_m1]
    dat_var['alpha_bar_'+cyclestr]=dat_var['alpha_bar_'+cyclestr_m1]
    dat_var[['t_bar_'+cyclestr,'t_now_'+cyclestr,'alpha_bar_'+cyclestr]]=dat_var[['t_bar_'+cyclestr,'t_now_'+cyclestr,'alpha_bar_'+cyclestr]].astype(np.float64)
    dat_var.loc[(dat_var['PBUFTYP'].isin(mnet_bctypes)) & (dat_var['t_bar_'+cyclestr].isna()),'t_bar_'+cyclestr]=t_now-tinf
    dat_var.loc[(dat_var['PBUFTYP'].isin(mnet_bctypes)) & (dat_var['alpha_bar_'+cyclestr].isna()),'alpha_bar_'+cyclestr]=alpha_0
  except:
    dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 't_bar_'+cyclestr]=t_now-tinf
    dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 'alpha_bar_'+cyclestr]=alpha_0

  # The previous cycle's t_bar should always be less than t_now. Exit if this condition is not met.
  if np.where(dat_var['t_bar_'+cyclestr]>=dat_var['t_now_'+cyclestr],True,False).any():
    print('WARNING: Exiting mesonet wind bias correction scheme.'); exit()

  # Compute precision weight of log-bias estimate
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)),'w_'+cyclestr] = \
          winf/np.tanh((t_now-dat_var['t_bar_'+cyclestr])/tinf)+1.

  # Compute logarithmic correction factor for present bias
  dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 'alpha_'+cyclestr] = dat_var['alpha_bar_'+cyclestr]/np.cosh((t_now-dat_var['t_bar_'+cyclestr])/tinf)
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)),'alpha_'+cyclestr] = \
          dat_var['alpha_'+cyclestr]+(np.log(dat_var['GES_'+cyclestr]/dat_var['OB_'+cyclestr])-dat_var['alpha_'+cyclestr])/dat_var['w_'+cyclestr]
  dat_var['a_'+cyclestr] = np.exp(dat_var['alpha_'+cyclestr])
  # Restrict adjustment factor values to range [0.75,1.5]
  dat_var['a_'+cyclestr] = dat_var['a_'+cyclestr].clip(lower=0.75,upper=1.5)

  # Update t_bar, i.e., effective time-of-origin of present bias estimate, from new w,
  # and update alpha_bar from new alpha:
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)),'t_bar_'+cyclestr] = \
          t_now-tinf*np.arctanh(winf/dat_var['w_'+cyclestr])
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)), \
          'alpha_bar_'+cyclestr] = dat_var['alpha_'+cyclestr]*np.cosh((t_now-dat_var['t_bar_'+cyclestr])/tinf) # Logarithmic bias correction estimate

  # Testing ONLY: Compute effective (bias-corrected) observation's wind speed
  dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 'EFFECTIVE_OB_'+cyclestr] = np.exp(dat_var['alpha_'+cyclestr])*dat_var['OB_'+cyclestr]

  # Generate spreadsheet with the wind bias correction output (current and most recent cycle ONLY)
  PDYm1=datetime.strptime(str(cyclestr_m1),'%Y%m%d%H')
  cols_wbias=['t_now_','t_bar_','alpha_','a_','alpha_bar_','w_','EFFECTIVE_OB_']
  dat_wbias = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+cols_wbias))].copy()
  dat_wbias=dat_wbias[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') >= PDYm1),dat_wbias.columns[len(keep_cols):]))]
  dat_wbias.to_csv(thisRUN+'.t'+cycle_HH+'z.windbias.csv', index=False)

  # Generate text files with the wind bias correction output (current cycle ONLY)
  fname = thisRUN+'.t'+cycle_HH+'z.windbias.txt'
  dat_wbias=dat_wbias.loc[(dat_var['PBUFTYP'].isin(mnet_bctypes))]
  write_wbias(dat_wbias[keep_cols+['a_'+cyclestr]],fname)

  # Drop the previous cycle's wind bias information from the SQL database
  columns_wbias=[x + cyclestr_m1 for x in cols_wbias]
  try: dat_var.drop(columns_wbias,axis=1,inplace=True)
  except: pass

  return dat_var

def gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse):

  # If file exists, merge new dat_var array with data from existing database
  if os.path.exists(COMm1+'/'+thisRUN+'.t'+HHm1+'z.database_'+vars[var]+'.db'):
    # Open the connection to the SQLite database
    cnx = sqlite3.connect(COMm1+'/'+thisRUN+'.t'+HHm1+'z.database_'+vars[var]+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    columns=list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') > cyc_purge),columns[len(keep_cols):]))]
    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    dat_var[['PBUFTYP']]=dat_var[['PBUFTYP']].astype(np.int)
    cnx.close()

  # Open the connection to the SQLite database (will create one if it doesn't exist)
  cnx = sqlite3.connect(thisRUN+'.t'+cycle_HH+'z.database_'+vars[var]+'.db')

  dat_var = gen_accept_lists(dat_var,eps,geps,rjrmse)

  # Execute mesonet wind bias correction algorithm
  if vars[var]=='wst' and dat_var.shape[0]>0:
    # Replace NaN values with np.nan to ensure proper dtypes in windbias routine
    dat_var.replace('NaN|NaN|NaN|NaN|NaN|NaN|NaN|NaN',np.nan,regex=True,inplace=True)
    dat_var = windbias(dat_var)

  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('PRES-','OB_','INC-GES_','GES_','WDIR_'))]

  dat_var_save = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','mean_OmFs_','sum_devs_squared','stddev_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  # Convert to SQL database and close connection
  dat_var_save['COUNT']=dat_var_save.groupby('SAID')['SAID'].transform('count')
  dat_var_save=dat_var_save[dat_var_save['COUNT']<max_dups]
  dat_var_save.drop(['COUNT'],axis=1,inplace=True)
  # Drop stations from the database that haven't reported recently (i.e., all values are missing)
  if vars[var]=='wst': dat_var_save.replace(np.nan,'NaN|NaN|NaN|NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)
  else: dat_var_save.replace(np.nan,'NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)
  dat_var_save.dropna(subset=[column for column in dat_var_save.columns if column.startswith('DAT_')],how='all',inplace=True)
  dat_var_save.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
  dat_var_save.to_csv(thisRUN+'.t'+cycle_HH+'z.database_'+vars[var]+'.csv', index=False)
  cnx.close()

  # Generate SQL database with statistics output - Only computed daily at 23Z

  if cycle_HH=='23':

    itercyc=datetime.strptime(probecyc_daily,'%Y%m%d%H')
    probeday_stats = itercyc.strftime('%Y%m%d')
    probeHH_stats = itercyc.strftime('%H')
    COMprev_stats = os.path.abspath(os.path.join(os.path.dirname(COM), '../../'+'/'+NET+'.'+probeday_stats+'/'+dom+'/autoqcprd.t'+probeHH_stats+'z'))

    # If file exists, merge new dat_var array with data from existing database
    if os.path.exists(COMprev_stats+'/'+thisRUN+'.t'+cycle_HH+'z.stats_database_'+vars[var]+'.db'):
      # Open the connection to the SQLite database
      cnx = sqlite3.connect(COMprev_stats+'/'+thisRUN+'.t'+cycle_HH+'z.stats_database_'+vars[var]+'.db')
      data = pd.read_sql("SELECT * FROM "+var_str,cnx)
      stat_cols =  list(data.columns)
      data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') > cyc_purge_stats),stat_cols[len(keep_cols):]))]
  
      dfs = [data,dat_var]
      dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
      dat_var[['PBUFTYP']]=dat_var[['PBUFTYP']].astype(np.int)
      cnx.close()
  
    # Open the connection to the SQLite database (will create one if it doesn't exist)
    cnx = sqlite3.connect(thisRUN+'.t'+cycle_HH+'z.stats_database_'+vars[var]+'.db')
  
    stats_cols=['counts_','mean_OmFs_','sum_devs_squared','stddev_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_']
    dat_var_stats = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+stats_cols))]
  
    # Convert to SQL database and close connection
    dat_var_stats['COUNT']=dat_var_stats.groupby('SAID')['SAID'].transform('count')
    dat_var_stats=dat_var_stats[dat_var_stats['COUNT']<max_dups]
    dat_var_stats.drop(['COUNT'],axis=1,inplace=True)
    # Drop stations from the database that haven't reported recently (i.e., all values are missing)
    dat_var_stats.dropna(subset=[column for column in dat_var_stats.columns if column.startswith(tuple(stats_cols[1:]))],how='all',inplace=True)
    dat_var_stats.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
    dat_var_stats.to_csv(thisRUN+'.t'+cycle_HH+'z.stats_database_'+vars[var]+'.csv', index=False)
    cnx.close()
  
  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  return(dat_var)

def gen_accept_lists(dat_var,eps,geps,rjrmse):

  ij = 0
  cyc_unpack = []
  itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H')
  while ij < max(num_stuck,num_cycs_daily):
    cyc_unpack.append(itercyc.strftime('%Y%m%d%H'))
    itercyc = itercyc - delta
    ij+=1

  dat_var_subset = dat_var.loc[:, [x for x in dat_var.columns if x.startswith('DAT_')]]
  if vars[var]=='wst': dat_var_subset.replace(np.nan,'NaN|NaN|NaN|NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)
  else: dat_var_subset.replace(np.nan,'NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)

  print('UNPACK = ',cyc_unpack)
  for cyc in cyc_unpack:

    if 'DAT_'+cyc in dat_var.columns:
      dat_var['OB_'+cyc]=[np.float64(str(x).split('|')[0]) for x in dat_var_subset['DAT_'+cyc]]
      dat_var['INC-GES_'+cyc]=[np.float64(str(x).split('|')[1]) for x in dat_var_subset['DAT_'+cyc]]
      dat_var['GES_'+cyc]=[np.float64(str(x).split('|')[2]) for x in dat_var_subset['DAT_'+cyc]]
      if vars[var]=='wst':
        dat_var['WDIR_OB_'+cyc]=[float(str(x).split('|')[3]) for x in dat_var_subset['DAT_'+cyc]]
        dat_var['WDIR_INC_'+cyc]=[float(str(x).split('|')[4]) for x in dat_var_subset['DAT_'+cyc]]
        dat_var['WDIR_GES_'+cyc]=[float(str(x).split('|')[5]) for x in dat_var_subset['DAT_'+cyc]]

  ij = 0
  stuck_flag=False
  cyc_nstuck = []
  itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H')
  while ij < num_stuck:
    cyc_nstuck.append(itercyc.strftime('%Y%m%d%H'))
    itercyc = itercyc - delta
    ij+=1

  dat_var_stuck = dat_var.loc[:, dat_var.columns.str.endswith(tuple(keep_cols+cyc_nstuck))].copy()
  # Drop rows with NaN values to ensure we have the correct # of observations
  dat_var_stuck.dropna(inplace=True)

  # Do not run stuck instrument check for wind speed observations
  if vars[var] in ['t','ps','q']:

    dat_var_obs = dat_var_stuck.loc[:, [x for x in dat_var_stuck.columns if x.startswith('OB_')]]
    if dat_var_obs.shape[1]>=num_stuck-num_relax:

      if dat_var_obs.shape[0]>0:
        # Performing the stuck instrument check
        # First: Observations must vary less than "eps" over specified # of hours
        dat_var_obs_subset=dat_var_obs
        suspect_obs = dat_var_stuck[dat_var_obs_subset.apply(lambda x: max(np.float64(x))-min(np.float64(x))<eps,axis=1)]
        if suspect_obs.shape[0]>0:
          # Second: Background must vary more than "geps" over specified # of hours
          dat_var_obs_subset=suspect_obs.loc[:, [y for y in suspect_obs.columns if y.startswith('GES_')]]
          stuck_inst = suspect_obs[dat_var_obs_subset.apply(lambda y: max(np.float64(y))-min(np.float64(y))>geps,axis=1)]
          stuck_inst = stuck_inst[keep_cols]
          stuck_inst['STUCK']=1.
          # Finalize and write to CSV file
          if stuck_inst.shape[0]>0:
            stuck_flag=True
            stuck_inst[keep_cols+['STUCK']].to_csv(thisRUN+'.t'+cycle_HH+'z.stuck_inst_'+vars[var]+'.csv', index=False)

  # Run a check for flatlining temperature reports and merge results with the stuck instrument check to be EXCLUDED in
  # the generation of automated accept lists.
  if vars[var] in ['t']:
    dat_var_flat = dat_var_stuck[dat_var_stuck['PROVIDER']=='APRSWXNE'].copy()
    flat_inst = dat_var_flat[dat_var_flat['OB_'+cyclestr]==233.15]
    flat_inst = flat_inst[keep_cols]
    flat_inst['FLAT']=1.
    if flat_inst.shape[0]>0:
      flat_inst[keep_cols+['FLAT']].to_csv(thisRUN+'.t'+cycle_HH+'z.flat_inst_'+vars[var]+'.csv', index=False)
      if stuck_flag==True:
        stuck_inst=pd.concat([stuck_inst,flat_inst],sort=False)
      else:
        stuck_inst=flat_inst
        stuck_flag=True

  dat_var_sum = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]
  columns=list(dat_var_sum.columns)
  columns=columns[len(keep_cols):]
  columns_obs=[x for x in columns if x.startswith('OB_')]

  itercyc=datetime.strptime(probecyc_daily,'%Y%m%d%H')
  probeday_daily = itercyc.strftime('%Y%m%d')
  probeHH_daily = itercyc.strftime('%H')
  COMprev_daily = os.path.abspath(os.path.join(os.path.dirname(COM), '../../'+'/'+NET+'.'+probeday_daily+'/'+dom+'/autoqcprd.t'+probeHH_daily+'z'))

  if cycle_HH=='23':
    cyc_delim_daily=datetime.strptime(cyclestr,'%Y%m%d%H')+timedelta(hours=-num_cycs_daily)
    dat_var_daily=dat_var_sum[list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') > cyc_delim_daily),columns))]
    dat_var['counts_daily_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('INC-GES_')]].count(axis=1)

    # Calculate long-term standard deviation (stddev)
    dat_var['mean_OmFs_daily_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('INC-GES_')]].abs().sum(axis=1).div(dat_var['counts_daily_'+cyclestr],axis=0)
    dat_var['sum_devs_squared_daily_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('INC-GES_')]].abs().sub(dat_var['mean_OmFs_daily_'+cyclestr],axis=0).pow(2).sum(axis=1)
    dat_var['stddev_daily_'+cyclestr]=np.sqrt(dat_var['sum_devs_squared_daily_'+cyclestr]/dat_var['counts_daily_'+cyclestr])
    # Wind direction
    if vars[var]=='wst':
      dat_var['mean_OmFs_daily_wdir_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('WDIR_INC_')]].abs().sum(axis=1).div(dat_var['counts_daily_'+cyclestr],axis=0)
      dat_var['sum_devs_squared_daily_wdir_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('WDIR_INC_')]].abs().sub(dat_var['mean_OmFs_daily_wdir_'+cyclestr],axis=0).pow(2).sum(axis=1)
      dat_var['stddev_daily_wdir_'+cyclestr]=np.sqrt(dat_var['sum_devs_squared_daily_wdir_'+cyclestr]/dat_var['counts_daily_'+cyclestr])

    # Calculate long-term RMSE and Bias
    dat_var['SUM_OmFs_daily_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('INC-GES_')]].sum(axis=1)
    dat_var['SUM_OmFs2_daily_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('INC-GES_')]].pow(2).sum(axis=1)
    dat_var['RMSE_daily_'+cyclestr]=np.sqrt(dat_var['SUM_OmFs2_daily_'+cyclestr]/dat_var['counts_daily_'+cyclestr])
    dat_var['Bias_daily_'+cyclestr]=dat_var['SUM_OmFs_daily_'+cyclestr]/dat_var['counts_daily_'+cyclestr]
    # Wind direction
    if vars[var]=='wst':
      dat_var['SUM_OmFs_daily_wdir_'+cyclestr]=dat_var_daily.loc[:, [x for x in dat_var_daily.columns if x.startswith('WDIR_INC_')]].sum(axis=1)
      dat_var['Bias_daily_wdir_'+cyclestr]=dat_var['SUM_OmFs_daily_wdir_'+cyclestr]/dat_var['counts_daily_'+cyclestr]

    # Replace computed values with NaN in cases where the station reports fewer than the required # of observations
    dat_var.loc[dat_var['counts_daily_'+cyclestr]<min_cycs_daily,('stddev_daily_'+cyclestr,'SUM_OmFs_daily_'+cyclestr,'SUM_OmFs2_daily_'+cyclestr,'RMSE_daily_'+cyclestr,'Bias_daily_'+cyclestr)]=np.nan

    if vars[var]=='wst':
      dat_var.loc[dat_var['counts_daily_'+cyclestr]<min_cycs_daily,('stddev_daily_wdir_'+cyclestr,'SUM_OmFs_daily_wdir_'+cyclestr,'SUM_OmFs2_daily_wdir_'+cyclestr,'RMSE_daily_wdir_'+cyclestr,'Bias_daily_wdir_'+cyclestr)]=np.nan

    dat_var['stddev_daily_'+cyclestr]=dat_var['stddev_daily_'+cyclestr].round(3)
    dat_var['SUM_OmFs_daily_'+cyclestr]=dat_var['SUM_OmFs_daily_'+cyclestr].round(3)
    dat_var['SUM_OmFs2_daily_'+cyclestr]=dat_var['SUM_OmFs2_daily_'+cyclestr].round(3)
    dat_var['RMSE_daily_'+cyclestr]=dat_var['RMSE_daily_'+cyclestr].round(3)
    dat_var['Bias_daily_'+cyclestr]=dat_var['Bias_daily_'+cyclestr].round(3)
    # Wind direction
    if vars[var]=='wst':
      dat_var['stddev_daily_wdir_'+cyclestr]=dat_var['stddev_daily_wdir_'+cyclestr].round(3)
      dat_var['SUM_OmFs_daily_wdir_'+cyclestr]=dat_var['SUM_OmFs_daily_wdir_'+cyclestr].round(3)
      dat_var['Bias_daily_wdir_'+cyclestr]=dat_var['Bias_daily_wdir_'+cyclestr].round(3)

  if vars[var] in ['t','ps','q','wst']:
    dat_var_accept=dat_var.copy()
    if cycle_HH=='23':
      if aclist_type=='std_bias':
        if vars[var]=='wst':
          dat_var_accept['AC_DAILY']=np.where((dat_var_accept['stddev_daily_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))) & \
                                             (dat_var_accept['Bias_daily_'+cyclestr]>(-1.)*rjbias*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))) & \
                                             (dat_var_accept['Bias_daily_'+cyclestr]<rjbias*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))) & \
                                             (dat_var_accept['stddev_daily_wdir_'+cyclestr]<rjstd_wdir) & (dat_var_accept['Bias_daily_wdir_'+cyclestr]<rjbias_wdir),1,0)
        else:
          dat_var_accept['AC_DAILY']=np.where((dat_var_accept['stddev_daily_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))) & \
                                             (dat_var_accept['Bias_daily_'+cyclestr]>(-1.)*rjbias*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))) & \
                                             (dat_var_accept['Bias_daily_'+cyclestr]<rjbias*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP']))),1,0)
      elif aclist_type=='rmse':
        dat_var_accept['AC_DAILY']=np.where(dat_var_accept['RMSE_daily_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_accept['VMAP'])),1,0)
      else:
        print('Invalid choice of aclist_type. Exiting...'); exit()
      dat_var_accept = dat_var_accept.loc[:, dat_var_accept.columns.str.startswith(tuple(keep_cols+['counts_','stddev_','Bias_','AC_DAILY']))].copy()
      dat_var_accept.to_csv(thisRUN+'.t'+cycle_HH+'z.accept_daily_'+vars[var]+'.csv', index=False)
    elif cycle_HH!='23' and os.path.exists(COMprev_daily+'/'+thisRUN+'.t'+probeHH_daily+'z.accept_daily_'+vars[var]+'.csv'):
      print('FOUND PRIOR DAILY ACCEPT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
      prior_daily_aclist = pd.read_csv(COMprev_daily+'/'+thisRUN+'.t'+probeHH_daily+'z.accept_daily_'+vars[var]+'.csv')
      dat_var_accept = pd.concat([prior_daily_aclist,dat_var_accept],sort=False)

    if stuck_flag==True and dat_var_accept.shape[0]>0:
      dat_var_accept.loc[(dat_var_accept['SAID'].isin(stuck_inst['SAID']) & dat_var_accept['PROVIDER'].isin(stuck_inst['PROVIDER'])),'AC_DAILY']=0.

    fname = thisRUN+'.t'+cycle_HH+'z.accept_'+vars[var]+'.txt'
    write_lists(dat_var_accept,fname)

  return(dat_var)

def write_lists(input_list,fname):
  ltyp=re.split(r"[._]",fname)[2]
  if ltyp!='accept': out_file=open(fname,'w')
  if ltyp!='accept':
    out_file.write('********************************************************************************\n')
    if vars[var]=='t': out_file.write(fname.split('_')[0]+' list for temperature obs\n')
    elif vars[var]=='ps': out_file.write(fname.split('_')[0]+' list for surface pressure obs\n')
    elif vars[var]=='q': out_file.write(fname.split('_')[0]+' list for specific humidity obs\n')
    elif vars[var]=='wst': out_file.write(fname.split('_')[0]+' list for wind obs\n')

  if ltyp=='reject':
    if input_list.shape[0]>0:
      if 'RJ_DAILY' not in input_list: input_list['RJ_DAILY']=0.
      if 'STUCK' not in input_list: input_list['STUCK']=0.
      if 'FLAT' not in input_list: input_list['FLAT']=0.
      input_list[['RJ_DAILY','STUCK','FLAT']] = input_list[['RJ_DAILY','STUCK','FLAT']].fillna(value=0.)
      input_list[['RJ_DAILY','STUCK','FLAT']] = input_list.groupby(keep_cols)[['RJ_DAILY','STUCK','FLAT']].transform('sum')
      input_list['RJ_DAILY'].replace({0.: "---", 1.: "RMS"}, inplace=True)
      input_list['STUCK'].replace({0.: "---", 1.: "stk"}, inplace=True)
      input_list['FLAT'].replace({0.: "---", 1.: "flt"}, inplace=True)
      input_list['rjl_str']=input_list['RJ_DAILY']+input_list['STUCK']+input_list['FLAT']+'---'
  elif ltyp=='accept':
    if input_list.shape[0]>0:
      if 'AC_DAILY' not in input_list: input_list['AC_DAILY']=0.
      input_list[['AC_DAILY']] = input_list[['AC_DAILY']].fillna(value=0.)
      input_list[['AC_DAILY']] = input_list.groupby(keep_cols)[['AC_DAILY']].transform('sum')
  input_list.drop_duplicates(subset=keep_cols,inplace=True)
  input_list.to_csv(thisRUN+'.t'+cycle_HH+'z.'+ltyp+'_'+vars[var]+'.csv', index=False)

def write_wbias(input_file,fname):
  with open(fname,'w') as out_file:
    for prov in input_file['PROVIDER'].unique():
      out_file.write('PROVIDER: '+prov+'\n')
      input_file_prov=input_file[input_file['PROVIDER']==prov]
      for subprov in input_file_prov['SUBPROVIDER'].unique():
        input_file_subprov=input_file_prov[input_file_prov['SUBPROVIDER']==subprov]
        out_file.write('SUBPROVIDER: '+subprov+'\n')
        for index,row in input_file_subprov.iterrows():
          line=str(row[0]).ljust(8)+"| itype="+str(row[3])+"  lat="+str("%.4f" %row[4]).rjust(10)+"  lon="+\
               str("%.4f" %(row[5]-360)).rjust(10)+"  loc=US  a="+str("%.4f" %row[7])+'\n'
          out_file.write(line)
        out_file.write('End of subprovider list\n')
      out_file.write('End of provider list\n')

def combine_accept_lists(dat_var):
  # Read in individual accept lists
  temp_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.accept_t.csv')
  wind_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.accept_wst.csv')
  dwpt_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.accept_q.csv')
  pres_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.accept_ps.csv')

  temp_list.replace({'PBUFTYP': {187:"SFC", 188:"MSO", 287:"SFC", 288:"MSO"}},inplace=True)
  wind_list.replace({'PBUFTYP': {187:"SFC", 188:"MSO", 287:"SFC", 288:"MSO"}},inplace=True)
  dwpt_list.replace({'PBUFTYP': {187:"SFC", 188:"MSO", 287:"SFC", 288:"MSO"}},inplace=True)
  pres_list.replace({'PBUFTYP': {187:"SFC", 188:"MSO", 287:"SFC", 288:"MSO"}},inplace=True)

  # Rename columns
  temp_list.rename(columns={col:'{}-T'.format(col, j) for col in [col for col in temp_list if col.startswith(tuple(['counts_daily','stddev','Bias','AC_DAILY']))]}, inplace=True)
  wind_list.rename(columns={col:'{}-W'.format(col, j) for col in [col for col in wind_list if col.startswith(tuple(['counts_daily','stddev','Bias','AC_DAILY']))]}, inplace=True)
  dwpt_list.rename(columns={col:'{}-Td'.format(col, j) for col in [col for col in dwpt_list if col.startswith(tuple(['counts_daily','stddev','Bias','AC_DAILY']))]}, inplace=True)
  pres_list.rename(columns={col:'{}-P'.format(col, j) for col in [col for col in pres_list if col.startswith(tuple(['counts_daily','stddev','Bias','AC_DAILY']))]}, inplace=True)

  if 'AC_DAILY-T' not in temp_list: temp_list['AC_DAILY-T']=0.
  if 'AC_DAILY-W' not in wind_list: wind_list['AC_DAILY-W']=0.
  if 'AC_DAILY-Td' not in dwpt_list: dwpt_list['AC_DAILY-Td']=0.
  if 'AC_DAILY-P' not in pres_list: pres_list['AC_DAILY-P']=0.

  # Combine lists
  dfs = [temp_list,wind_list,dwpt_list,pres_list]
  combine_cols=['SAID','PROVIDER','SUBPROVIDER','PBUFTYP','LAT','LON']
  merged_list = reduce(lambda left,right: pd.merge(left,right,on=combine_cols,how='outer'), dfs)

  # Assign flag to reject observations if no data were available
  merged_list[['AC_DAILY-T','AC_DAILY-W','AC_DAILY-Td','AC_DAILY-P']] = merged_list[['AC_DAILY-T','AC_DAILY-W','AC_DAILY-Td','AC_DAILY-P']].fillna(value=0.)

  # Create final usage flag
  merged_list[['AC_DAILY-T','AC_DAILY-W','AC_DAILY-Td','AC_DAILY-P']]=merged_list[['AC_DAILY-T','AC_DAILY-W','AC_DAILY-Td','AC_DAILY-P']].astype(np.int64)
  merged_list['W-T-Td-G-P'] = merged_list['AC_DAILY-W'].astype(str) + '-' + merged_list['AC_DAILY-T'].astype(str) + '-' + merged_list['AC_DAILY-Td'].astype(str) + '-' + merged_list['AC_DAILY-W'].astype(str) +'-' + merged_list['AC_DAILY-P'].astype(str)

  cols_accept=['counts_daily','stddev_daily','Bias_daily','W-T-Td-G-P']
  merged_list=merged_list.loc[:, merged_list.columns.str.startswith(tuple(keep_cols+cols_accept))].copy()

  if cycle_HH=='23': stats_cycle=cyclestr
  else: stats_cycle=probecyc_daily

  merged_list.rename(columns={"stddev_daily_"+stats_cycle+"-T": "std-T", "Bias_daily_"+stats_cycle+"-T": "Bias-T"},inplace=True)
  merged_list.rename(columns={"stddev_daily_"+stats_cycle+"-W": "std-W", "Bias_daily_"+stats_cycle+"-W": "Bias-W"},inplace=True)
  merged_list.rename(columns={"stddev_daily_wdir_"+stats_cycle+"-W": "std-Wdir", "Bias_daily_wdir_"+stats_cycle+"-W": "Bias-Wdir"},inplace=True)
  merged_list.rename(columns={"stddev_daily_"+stats_cycle+"-Td": "std-Td", "Bias_daily_"+stats_cycle+"-Td": "Bias-Td"},inplace=True)
  merged_list.rename(columns={"stddev_daily_"+stats_cycle+"-P": "std-P", "Bias_daily_"+stats_cycle+"-P": "Bias-P"},inplace=True)

  # Record the # of observations for each variable; set this to 0 if a station doesn't report a variable
  merged_list.rename(columns={"counts_daily_"+stats_cycle+"-T": "N-T","counts_daily_"+stats_cycle+"-Td": "N-Td","counts_daily_"+stats_cycle+"-W": "N-W","counts_daily_"+stats_cycle+"-P": "N-P"},inplace=True)
  try: merged_list[['N-T','N-Td','N-W','N-P']] = merged_list[['N-T','N-Td','N-W','N-P']].fillna(value=0).astype(np.int64)
  except: pass

  fname_merged=thisRUN+'.t'+cycle_HH+'z.accept_merged.txt'
  with open(fname_merged,'w') as out_file:
    header=';Station Provider Subprov Type Lat   Lon    W-T-Td-G-P N-W  Std-W    Bias-W   Std-DIR  Bias-DIR N-T  Std-T    Bias-T   N-Td Std-Td   Bias-Td N-P  Std-P    Bias-P'+'\n'
    out_file.write(header)
    for index, row in merged_list.iterrows():
      try:
        line=str(row['SAID']).ljust(8)+" "+str(row['PROVIDER']).ljust(8)+" "+str(row['SUBPROVIDER']).ljust(8)+" "+\
          str(row['PBUFTYP'])+" "+"{:.2f}".format(row['LAT'])+" "+"{:.2f}".format(row['LON'])+" "+str(row['W-T-Td-G-P'])+"  "+\
          str(row['N-W']).ljust(4)+" "+"{:.3f}".format(row['std-W']).ljust(8)+" "+"{:.3f}".format(row['Bias-W']).ljust(8)+" "+\
          "{:.3f}".format(row['std-Wdir']).ljust(8)+" "+"{:.3f}".format(row['Bias-Wdir']).ljust(8)+" "+\
          str(row['N-T']).ljust(4)+" "+"{:.3f}".format(row['std-T']).ljust(8)+" "+"{:.3f}".format(row['Bias-T']).ljust(8)+" "+\
          str(row['N-Td']).ljust(4)+" "+"{:.3f}".format(row['std-Td']).ljust(8)+" "+"{:.3f}".format(row['Bias-Td']).ljust(8)+\
          str(row['N-P']).ljust(4)+" "+"{:.3f}".format(row['std-P']).ljust(8)+" "+"{:.3f}".format(row['Bias-P']).ljust(8)+'\n'
        out_file.write(line)
      except: pass

if __name__ == "__main__":

  print('Starting Python program.')

  NET=sys.argv[1]
  thisRUN=sys.argv[2]
  dateobj=sys.argv[3]
  DATA=sys.argv[4]
  COM=sys.argv[5]
  COMm1=sys.argv[6]
  cyclestr_m1=sys.argv[7]
  probecyc_daily=sys.argv[8]
  tinf=np.float64(sys.argv[9]) # Constant timescale associated with an observation
  dom=sys.argv[10]

  cyclestr=dateobj
  datestr=dateobj[0:8]
  cycle_HH=dateobj[8:10]
  HHm1=cyclestr_m1[8:10]
  cyc_purge=datetime.strptime(cyclestr,'%Y%m%d%H')+timedelta(days=-7)

  epochcyc=datetime.strptime(str(197001010000),'%Y%m%d%H%M')

  #--------------------------------------------------------------------#
  # Parameters for reading the diagnostic files later on.
  #--------------------------------------------------------------------#
  
  bmiss=0.10000E+10
  my_cols = ["OBTYPE","SAID","PROVIDER","SUBPROVIDER","PBUFTYP","DHR","LAT","LON",\
             "PRES","HGHT","IUSE","OB","INC","VOB","VINC","OBERR","RUSAGE","TDRY"]
  usecols=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
  dtypedict={"OBTYPE":str,"SAID":str,"PROVIDER":str,"SUBPROVIDER":str,"PBUFTYP":np.int64,\
          "DHR":np.float64,"LAT":np.float64,"LON":np.float64,"PRES":np.float64,"HGHT":np.float64,\
          "IUSE":np.int64,"OB":np.float64,"INC":np.float64,"VOB":np.float64,"VINC":np.float64,\
          "OBERR":np.float64,"RUSAGE":np.float64,"TDRY":np.float64}

  # Define constants
  mnet_bctypes=[288,295] # PREPBUFR report types for mesonet wind bias correction
  winf=2. # Constant asymptotic weight associated with an observation
  alpha_0=0. # Effective logarithmic bias correction for an observation without any history
  speed_min=2. # Low cut-off for acceptable windspeeds (GES & ANL)

  delta = timedelta(minutes=60)
  aclist_type='std_bias' # Select from std_bias or rmse
  num_stuck=8 # Number of cycles for the "stuck" instrument check
  num_cycs_daily=24*7 # Number of hours to compute long-term RMSE stats
  min_cycs_daily=20 # Minimum # of hours required to compute long-term stats
  num_relax=2 # Number of cycles allowed to be "missing" (stuck instrument and RMSE tests)
  max_dups=5 # Maximum number of entries allowed per unique station ID

  eps_w=0.01
  eps_t=0.01
  eps_p=0.01
  eps_td=0.01
    
  geps_w=1.0
  geps_t=1.0
  geps_p=1.0
  geps_td=1.0

  rjrmse_w=1.5 #5.0 # m/s
  rjrmse_t=5.0 #1000.0 #9.0 #6.0 #5.0 #4.5 #4.0 #3.5 #3.0 #2.5 # K
  rjrmse_p=2.0 #2.5 #3.0 #1000.0 #6.5 #3.25
  rjrmse_td=5.0 ##9.0 #6.0 #5.0 #4.5 #4.0 #3.5 #3.0 #2.5 # K

  rjbias_w=0.5 #5.0 # m/s
  rjbias_t=4.0 #1000.0 #8.5 #5.5 #4.5 #4.0 #3.5 #3.0 #2.5 #2.0 # K
  rjbias_p=2.0 #2.5 #3.0 #1000.0 #6.5 #3.25
  rjbias_td=4.0 #2.4 #1000.0 #7.5 #4.5 #3.5 #3.0 #2.5 #2.0 #1.5 #1.2 # K

  rjstd_wdir=90. # Degrees
  rjbias_wdir=40. # Degrees

  keep_cols=['SAID','PROVIDER','SUBPROVIDER','PBUFTYP','LAT','LON','VMAP']
  columns=keep_cols.copy()

  cyc_purge_stats=datetime.strptime(cyclestr,'%Y%m%d%H')+timedelta(days=-7)

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

  # Added [::-1] to reverse time series, such that positive DHRs will be encountered before negative DHRs, thus giving those
  # preference in the event that two DHRs with same absolute value, but of opposite sign, are encoutnered.  This is how these
  # observations are selected in the GSI ob selection algorithm to only use the observation valid closest to the analysis time.
  dat_anl=dat_anl[::-1]
  dat_ges=dat_ges[::-1]

  dfs = [dat_anl,dat_ges]

  for j, df in enumerate(dfs, start=0):
    if j==0: df.rename(columns={col:'{}-ANL'.format(col, j) for col in ('PRES','IUSE','INC','VINC','RUSAGE')}, inplace=True)
    if j==1: df.rename(columns={col:'{}-GES'.format(col, j) for col in ('PRES','IUSE','INC','VINC','RUSAGE')}, inplace=True)

  merge_cols=['SAID','PROVIDER','SUBPROVIDER','PBUFTYP','LAT','LON','HGHT','OBTYPE','DHR','OB','VOB','TDRY']
  dat_merged = reduce(lambda left,right: pd.merge(left,right,on=merge_cols), dfs)

  # Determine VMAP information. Set to 0 for valleys (RUSAGE ends in 0.25 or 0.75).
  dat_merged['RUSAGE_remainder']=np.modf(dat_merged['RUSAGE-ANL'])[0]
  dat_merged['VMAP']=np.where((dat_merged['RUSAGE_remainder']==0.25) | (dat_merged['RUSAGE_remainder']==0.75),0,1)

  dat_merged.replace({'PBUFTYP': {192:181, 193:187, 194:183, 195:188, 292:281, 293:287, 294:284, 295:288}},inplace=True)

  # Remove underscores from provider and subprovider to match what is listed in the prepbufr file
  dat_merged['PROVIDER']    = dat_merged['PROVIDER'].str.replace('_+',' ')
  dat_merged['SUBPROVIDER'] = dat_merged['SUBPROVIDER'].str.replace('_+',' ')

  dat_merged = dat_merged[(dat_merged['PBUFTYP']!=290) & (dat_merged['PROVIDER']!='GST-MoPE')]

  dat_merged = dat_merged[((dat_merged['PBUFTYP']==187) | (dat_merged['PBUFTYP']==188)) | ((dat_merged['PBUFTYP']==287) | (dat_merged['PBUFTYP']==288))]

  dat_merged['GES']=dat_merged['OB']-dat_merged['INC-GES']

  # Loop through each variable
  vars=['t','ps','q','wst']
  for var in range(len(vars)):

    if vars[var]=='t': var_str='temperature'; eps=eps_t; geps=geps_t; rjrmse=rjrmse_t; rjbias=rjbias_t
    elif vars[var]=='ps': var_str='pressure'; eps=eps_p; geps=geps_p; rjrmse=rjrmse_p; rjbias=rjbias_p
    elif vars[var]=='q': var_str='moisture'; eps=eps_td; geps=geps_td; rjrmse=rjrmse_td; rjbias=rjbias_td
    elif vars[var]=='wst': var_str='wind_speed'; eps=eps_w; geps=geps_w; rjrmse=rjrmse_w; rjbias=rjbias_w

    if vars[var]=='wst':

      dat_var=dat_merged[dat_merged['OBTYPE']=='uv'].copy()

      dat_var['WSPD_OB'] = np.sqrt( dat_var['OB']**2 + dat_var['VOB']**2 )
      dat_var['U_GES'] = dat_var['OB'] - dat_var['INC-GES']
      dat_var['V_GES'] = dat_var['VOB'] - dat_var['VINC-GES']
      dat_var['WSPD_GES'] = np.sqrt( (dat_var['U_GES'])**2 + (dat_var['V_GES'])**2 )
      dat_var['WSPD_INC'] = dat_var['WSPD_OB'] - dat_var['WSPD_GES']

      rad2deg = 180.0/np.pi

      # Calculate observed wind direction:
      dat_var['WDIR_OB']=90.0-(rad2deg*np.arctan2(-1.0*dat_var['VOB'],-1.0*dat_var['OB']))
      dat_var.loc[dat_var['WDIR_OB']<0,'WDIR_OB']=dat_var['WDIR_OB']+360. # Correct negative wind directions

      # Calculate background/GES wind direction:
      dat_var['WDIR_GES']=90.0-(rad2deg*np.arctan2(-1.0*dat_var['V_GES'],-1.0*dat_var['U_GES']))
      dat_var.loc[dat_var['WDIR_GES']<0,'WDIR_GES']=dat_var['WDIR_GES']+360. # Correct negative wind directions

      # Calculate the difference between the above two wind direction
      dat_var['WDIR_DIFF1']=(dat_var['WDIR_OB']-dat_var['WDIR_GES'])%360
      dat_var['WDIR_DIFF2']=(dat_var['WDIR_GES']-dat_var['WDIR_OB'])%360
      dat_var['WDIR_INC']=dat_var[['WDIR_DIFF1','WDIR_DIFF2']].min(axis=1)

      dat_var.drop(['OB','GES','INC-GES','VOB','VINC-GES','WDIR_DIFF1','WDIR_DIFF2'],axis=1,inplace=True)
      dat_var.rename(columns={"WSPD_OB": "OB", "WSPD_GES": "GES", "WSPD_INC": "INC-GES"},inplace=True)

    elif vars[var]=='q':

      dat_var=dat_merged[dat_merged['OBTYPE']=='q'].copy()

      # Note: The computed dewpoint temperature cannot be higher than the observed Tdry
      dat_var['TDRY'] = dat_var['TDRY']-273.15                                 # Convert from K to C

      # Compute observed dewpoint temperature
      q_obs  = dat_var[dat_var['OBTYPE']=='q']['OB'].values/1000               # Convert from g/kg to kg/kg
      ps_obs = dat_var[dat_var['OBTYPE']=='q']['PRES-ANL'].values              # Units: hPa
      w_obs  = q_obs/(1-q_obs)                                                 # Units: kg/kg
      e_obs  = (ps_obs * w_obs)/(0.62197+w_obs)                                # Units: hPa
      dat_var['DEWPT_OB'] = (243.5*np.log(e_obs)-440.8)/(19.48-np.log(e_obs))  # Units: C
      dat_var['DEWPT_OB'] = dat_var[["DEWPT_OB","TDRY"]].min(axis=1)           # Units: C

      # Compute background dewpoint temperature
      q_inc  = dat_var[dat_var['OBTYPE']=='q']['INC-GES'].values/1000          # Convert from g/kg to kg/kg
      q_ges  = q_obs - q_inc                                                   # Units: kg/kg
      ps_ges = dat_var[dat_var['OBTYPE']=='q']['PRES-GES'].values              # Units: hPa
      w_ges  = q_ges/(1-q_ges)                                                 # Units: kg/kg
      e_ges  = (ps_ges * w_ges)/(0.62197+w_ges)                                # Units: hPa
      dat_var['DEWPT_GES'] = (243.5*np.log(e_ges)-440.8)/(19.48-np.log(e_ges)) # Units: C
      dat_var['DEWPT_GES'] = dat_var[["DEWPT_GES","TDRY"]].min(axis=1)         # Units: C

      # Compute the dewpoint innovation
      dat_var['DEWPT_INC'] = dat_var['DEWPT_OB'] - dat_var['DEWPT_GES']

      dat_var.drop(['OB','GES','INC-GES','TDRY'],axis=1,inplace=True)
      dat_var.rename(columns={"DEWPT_OB": "OB", "DEWPT_GES": "GES", "DEWPT_INC": "INC-GES"},inplace=True)

    else:

      dat_var=dat_merged[dat_merged['OBTYPE']==vars[var]].copy()

    # Find the observation selected for assimilation (i.e., nearest analysis time)
    dat_var['ABSDHR']=dat_var['DHR'].abs()
    dat_var = dat_var.loc[dat_var.groupby(keep_cols)['ABSDHR'].idxmin()]

    # Retain only the necessary columns, then format data for SQL databases
    dat_var['OB']=dat_var['OB'].round(3); dat_var['INC-GES']=dat_var['INC-GES'].round(3); dat_var['GES']=dat_var['GES'].round(3)
    if vars[var]=='wst':
      dat_var['WDIR_OB']=dat_var['WDIR_OB'].round(3); dat_var['WDIR_INC']=dat_var['WDIR_INC'].round(3); dat_var['WDIR_GES']=dat_var['WDIR_GES'].round(3)
      dat_var=dat_var[keep_cols+['OB','INC-GES','GES','WDIR_OB','WDIR_INC','WDIR_GES','IUSE-GES','RUSAGE-GES']]
      dat_var['DAT_'+cyclestr] = dat_var['OB'].map(str) + '|' + dat_var['INC-GES'].map(str) + '|' + dat_var['GES'].map(str) + '|' + \
                                 dat_var['WDIR_OB'].map(str) + '|' + dat_var['WDIR_INC'].map(str) + '|' + dat_var['WDIR_GES'].map(str) + '|' + \
                                 dat_var['IUSE-GES'].map(str) + '|' + dat_var['RUSAGE-GES'].map(str)
      dat_var.rename(columns={col:('{}_'+cyclestr).format(col,dat_var) for col in ('OB','INC-GES','GES','WDIR_OB','WDIR_INC','WDIR_GES','IUSE-GES','RUSAGE-GES')},inplace=True)
    else:
      dat_var=dat_var[keep_cols+['OB','INC-GES','GES','IUSE-GES','RUSAGE-GES']]
      dat_var['DAT_'+cyclestr] = dat_var['OB'].map(str) + '|' + dat_var['INC-GES'].map(str) + '|' + dat_var['GES'].map(str) + '|' + \
                                 dat_var['IUSE-GES'].map(str) + '|' + dat_var['RUSAGE-GES'].map(str)
      dat_var.rename(columns={col:('{}_'+cyclestr).format(col,dat_var) for col in ('OB','INC-GES','GES','IUSE-GES','RUSAGE-GES')},inplace=True)

    dat_var=dat_var[keep_cols+['DAT_'+cyclestr]]

    gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse)

  # Once the individual lists are created, merge together to form the final accept list
  combine_accept_lists(dat_var)

