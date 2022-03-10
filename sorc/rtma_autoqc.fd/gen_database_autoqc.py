#!/usr/bin/env python

from datetime import datetime,timedelta
import numpy as np
import pandas as pd
import os,sys,math
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
  dat_var['sza']=np.arccos(dat_var['csza'])*(180./math.pi)

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
  currcyc = datetime.strptime(str(cyclestr),'%Y%m%d%H%M')
  t_now = (currcyc - epochcyc).total_seconds()/timedelta(days=1).total_seconds()
  dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 't_now_'+cyclestr] = t_now

  # Set t_bar and alpha_bar values to those from preceding cycle, if available; otherwise, use default values.
  try:
    dat_var['t_bar_'+cyclestr]=dat_var['t_bar_'+cyclestr_m1]
    dat_var['alpha_bar_'+cyclestr]=dat_var['alpha_bar_'+cyclestr_m1]
    dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes),'t_bar_'+cyclestr].fillna(t_now-tinf,inplace=True)
    dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes),'alpha_bar_'+cyclestr].fillna(alpha_0,inplace=True)
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

  # Update t_bar, i.e., effective time-of-origin of present bias estimate, from new w,
  # and update alpha_bar from new alpha:
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)),'t_bar_'+cyclestr] = \
          t_now-tinf*np.arctanh(winf/dat_var['w_'+cyclestr])
  dat_var.loc[(dat_var['OB_'+cyclestr]>speed_min) & (dat_var['GES_'+cyclestr]>speed_min) & (dat_var['PBUFTYP'].isin(mnet_bctypes)), \
          'alpha_bar_'+cyclestr] = dat_var['alpha_'+cyclestr]*np.cosh((t_now-dat_var['t_bar_'+cyclestr])/tinf) # Logarithmic bias correction estimate

  # Testing ONLY: Compute effective (bias-corrected) observation's wind speed
  dat_var.loc[dat_var['PBUFTYP'].isin(mnet_bctypes), 'EFFECTIVE_OB_'+cyclestr] = np.exp(dat_var['alpha_'+cyclestr])*dat_var['OB_'+cyclestr]

  # Generate spreadsheet with the wind bias correction output (current and most recent cycle ONLY)
  PDYm1=datetime.strptime(str(cyclestr_m1),'%Y%m%d%H%M')
  cols_wbias=['t_now_','t_bar_','alpha_','a_','alpha_bar_','w_','EFFECTIVE_OB_']
  dat_wbias = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+cols_wbias))].copy()
  dat_wbias=dat_wbias[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') >= PDYm1),dat_wbias.columns[len(keep_cols):]))]
  dat_wbias.to_csv('windbias_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

  # Drop the previous cycle's wind bias information from the SQL database
  columns_wbias=[x + cyclestr_m1 for x in cols_wbias]
  try: dat_var.drop(columns_wbias,axis=1,inplace=True)
  except: pass

  return dat_var

def gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse):

  # If file exists, merge new dat_var array with data from existing database
  if os.path.exists(COMm1+'/'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.db'):
    # Open the connection to the SQLite database
    cnx = sqlite3.connect(COMm1+'/'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    columns=list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_purge),columns[len(keep_cols):]))]
    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    cnx.close()

  # Open the connection to the SQLite database (will create one if it doesn't exist)
  cnx = sqlite3.connect(COM+'/'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.db')

  dat_var = duplicates(dat_var)

  dat_var = gen_rjlist(dat_var,eps,geps,rjrmse)

  if vars[var]=='wst' and dat_var.shape[0]>0: dat_var = windbias(dat_var)

  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('OB_','INC-GES_','GES_'))]

  dat_var_save = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  # Convert to SQL database and close connection
  dat_var_save['COUNT']=dat_var_save.groupby('SAID')['SAID'].transform('count')
  dat_var_save=dat_var_save[dat_var_save['COUNT']<max_dups]
  dat_var_save.drop(['COUNT'],axis=1,inplace=True)
  # Drop stations from the database that haven't reported recently (i.e., all values are missing)
  dat_var_save.replace('NaN|NaN|NaN|NaN|NaN',np.nan,regex=True,inplace=True)
  dat_var_save.dropna(subset=[column for column in dat_var_save.columns if column.startswith('DAT_')],how='all',inplace=True)
  dat_var_save.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
  dat_var_save.to_csv(COM+'/'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
  cnx.close()

  # Generate SQL database with statistics output

  # If file exists, merge new dat_var array with data from existing database
  if os.path.exists(COMm1+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.db'):
    # Open the connection to the SQLite database
    cnx = sqlite3.connect(COMm1+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    stat_cols =  list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_purge_stats),stat_cols[len(keep_cols):]))]

    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    cnx.close()

  # Open the connection to the SQLite database (will create one if it doesn't exist)
  cnx = sqlite3.connect(COM+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.db')

  stats_cols=['counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_']
  dat_var_stats = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+stats_cols))]

  # Convert to SQL database and close connection
  dat_var_stats['COUNT']=dat_var_stats.groupby('SAID')['SAID'].transform('count')
  dat_var_stats=dat_var_stats[dat_var_stats['COUNT']<max_dups]
  dat_var_stats.drop(['COUNT'],axis=1,inplace=True)
  # Drop stations from the database that haven't reported recently (i.e., all values are missing)
  dat_var_stats.dropna(subset=[column for column in dat_var_stats.columns if column.startswith(tuple(stats_cols[1:]))],how='all',inplace=True)
  dat_var_stats.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
  dat_var_stats.to_csv(COM+'/stats_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
  cnx.close()

  dat_var = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]

  return(dat_var)

def gen_rjlist(dat_var,eps,geps,rjrmse):

  ij = 0
  cyc_unpack = []
  itercyc=datetime.strptime(str(cyclestr),'%Y%m%d%H%M')
  while ij < max(num_cycs,num_stuck,num_cycs_long):
    cyc_unpack.append(itercyc.strftime('%Y%m%d%H%M'))
    itercyc = itercyc - delta
    ij+=1

  dat_var_subset = dat_var.loc[:, [x for x in dat_var.columns if x.startswith('DAT_')]]
  dat_var_subset.replace(np.nan,'NaN|NaN|NaN|NaN|NaN',regex=True,inplace=True)

  print('UNPACK = ',cyc_unpack)
  for cyc in cyc_unpack:

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
          # Sanity check for Step #1 (After)
          suspect_obs.to_csv(COM+'/suspect_obs_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
          # Second: Background must vary more than "geps" over specified # of hours
          dat_var_obs_subset=suspect_obs.loc[:, [y for y in suspect_obs.columns if y.startswith('GES_')]]
          stuck_inst = suspect_obs[dat_var_obs_subset.apply(lambda y: max(np.float64(y))-min(np.float64(y))>geps,axis=1)]
          stuck_inst = stuck_inst[keep_cols]
          stuck_inst['STUCK']=1.
          # Finalize and write to CSV file
          if stuck_inst.shape[0]>0:
            stuck_flag=True
            stuck_inst[keep_cols+['STUCK']].to_csv(COM+'/stuck_inst_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

  # Run a check for flatlining temperature reports and merge results with the stuck instrument check to be included in
  # the generation of automated reject lists.
  if vars[var] in ['t']:
    dat_var_flat = dat_var_stuck[dat_var_stuck['PROVIDER']=='APRSWXNE'].copy()
    flat_inst = dat_var_flat[dat_var_flat['OB_'+cyclestr]==233.15]
    flat_inst = flat_inst[keep_cols]
    flat_inst['FLAT']=1.
    if flat_inst.shape[0]>0: flat_inst[keep_cols+['FLAT']].to_csv(COM+'/flat_inst_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
    if stuck_flag==True:
      stuck_inst=pd.concat([stuck_inst,flat_inst],sort=False)
    else:
      stuck_inst=flat_inst
      stuck_flag=True

  dat_var_sum = dat_var.loc[:, ~dat_var.columns.str.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_','RMSE_','Bias_'))]
  columns=list(dat_var_sum.columns)
  columns=columns[len(keep_cols):]
  columns_obs=[x for x in columns if x.startswith('OB_')]

  itercyc=datetime.strptime(probecyc_long,'%Y%m%d%H%M')
  probeday_long = itercyc.strftime('%Y%m%d')
  probeHHMM_long = itercyc.strftime('%H%M')
  COMprev_long = os.path.abspath(os.path.join(os.path.dirname(COM), '../'+'/'+exp+'.'+probeday_long+'/autoqcprd.t'+probeHHMM_long+'z'))

  if np.float(cycle_HH)%num_cycs==num_cycs-1 and dat_var.shape[0]>0 and len(columns_obs)>=num_cycs-num_relax:
    cyc_delim=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(hours=-num_cycs)
    dat_var_short=dat_var_sum[list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_delim),columns))]
    dat_var['counts_'+cyclestr]=dat_var_short.loc[:, [x for x in dat_var_short.columns if x.startswith('INC-GES_')]].count(axis=1)
    dat_var['SUM_OmFs_'+cyclestr]=dat_var_short.loc[:, [x for x in dat_var_short.columns if x.startswith('INC-GES_')]].sum(axis=1)
    dat_var['SUM_OmFs2_'+cyclestr]=dat_var_short.loc[:, [x for x in dat_var_short.columns if x.startswith('INC-GES_')]].pow(2).sum(axis=1)
    dat_var['RMSE_'+cyclestr]=np.sqrt(dat_var['SUM_OmFs2_'+cyclestr]/dat_var['counts_'+cyclestr])
    dat_var['Bias_'+cyclestr]=dat_var['SUM_OmFs_'+cyclestr]/dat_var['counts_'+cyclestr]
    # Replace computed values with NaN in cases where the station reports fewer than the required # of observations
    dat_var.loc[dat_var['counts_'+cyclestr]<num_cycs-num_relax,('SUM_OmFs_'+cyclestr,'SUM_OmFs2_'+cyclestr,'RMSE_'+cyclestr,'Bias_'+cyclestr)]=np.nan

    dat_var['SUM_OmFs_'+cyclestr]=dat_var['SUM_OmFs_'+cyclestr].round(3)
    dat_var['SUM_OmFs2_'+cyclestr]=dat_var['SUM_OmFs2_'+cyclestr].round(3)
    dat_var['RMSE_'+cyclestr]=dat_var['RMSE_'+cyclestr].round(3)
    dat_var['Bias_'+cyclestr]=dat_var['Bias_'+cyclestr].round(3)

    cyc_delim_long=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(hours=-num_cycs_long)
    dat_var_long=dat_var_sum[list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H%M') > cyc_delim_long),columns))]
    if cycle_HH=='23':
      dat_var['counts_long_'+cyclestr]=dat_var_long.loc[:, [x for x in dat_var_long.columns if x.startswith('INC-GES_')]].count(axis=1)
      dat_var['SUM_OmFs_long_'+cyclestr]=dat_var_long.loc[:, [x for x in dat_var_long.columns if x.startswith('INC-GES_')]].sum(axis=1)
      dat_var['SUM_OmFs2_long_'+cyclestr]=dat_var_long.loc[:, [x for x in dat_var_long.columns if x.startswith('INC-GES_')]].pow(2).sum(axis=1)
      dat_var['RMSE_long_'+cyclestr]=np.sqrt(dat_var['SUM_OmFs2_long_'+cyclestr]/dat_var['counts_long_'+cyclestr])
      dat_var['Bias_long_'+cyclestr]=dat_var['SUM_OmFs_long_'+cyclestr]/dat_var['counts_long_'+cyclestr]

      dat_var['SUM_OmFs_long_'+cyclestr]=dat_var['SUM_OmFs_long_'+cyclestr].round(3)
      dat_var['SUM_OmFs2_long_'+cyclestr]=dat_var['SUM_OmFs2_long_'+cyclestr].round(3)
      dat_var['RMSE_long_'+cyclestr]=dat_var['RMSE_long_'+cyclestr].round(3)
      dat_var['Bias_long_'+cyclestr]=dat_var['Bias_long_'+cyclestr].round(3)

    if vars[var] in ['t','ps','q']:
      # VMAP currently defined as 1 everywhere
      if comp_partial_flag==True:
        dat_var_reject=dat_var[dat_var['RMSE_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var['VMAP']))]
        dat_var_reject=dat_var_reject[keep_cols]
        dat_var_reject['RJ_SHORT']=1.
        dat_var_reject[keep_cols+['RJ_SHORT']].to_csv(COM+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      else: dat_var_reject = pd.DataFrame(columns=keep_cols)
      if cycle_HH=='23':
        dat_var_reject_long=dat_var[dat_var['RMSE_long_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var['VMAP']))]
        dat_var_reject_long=dat_var_reject_long[keep_cols]
        dat_var_reject_long['RJ_LONG']=1.
        dat_var_reject_long.to_csv(COM+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
        dat_var_reject=pd.concat([dat_var_reject,dat_var_reject_long],sort=False)
      elif cycle_HH!='23' and os.path.exists(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv'):
        print('FOUND PRIOR LONG REJECT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
        prior_long_rjlist = pd.read_csv(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv')
        dat_var_reject=pd.concat([dat_var_reject,prior_long_rjlist],sort=False)
      if stuck_flag==True: dat_var_reject=pd.concat([stuck_inst,dat_var_reject],sort=False)
      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject,fname)
    elif vars[var] in ['wst']:
      # VMAP currently defined as 1 everywhere
      dat_var_meso=dat_var[(dat_var['PBUFTYP']==188) | (dat_var['PBUFTYP']==288)]
      if comp_partial_flag==True:
        dat_var_accept=dat_var_meso[dat_var_meso['RMSE_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_meso['VMAP']))]
        dat_var_accept=dat_var_accept[keep_cols]
        dat_var_accept['AC_SHORT']=1.
        dat_var_accept[keep_cols+['AC_SHORT']].to_csv(COM+'/accept_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      else: dat_var_accept = pd.DataFrame(columns=keep_cols)
      if cycle_HH=='23':
        dat_var_accept_long=dat_var_meso[dat_var_meso['RMSE_long_'+cyclestr]<rjrmse*(1.0+(3.0-1.0)*(1-dat_var_meso['VMAP']))]
        dat_var_accept_long=dat_var_accept_long[keep_cols]
        dat_var_accept_long['AC_LONG']=1.
        dat_var_accept_long[keep_cols+['AC_LONG']].to_csv(COM+'/accept_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
        dat_var_accept=pd.concat([dat_var_accept,dat_var_accept_long],sort=False)
      elif cycle_HH!='23' and os.path.exists(COMprev_long+'/accept_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv'):
        print('FOUND PRIOR LONG ACCEPT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
        prior_long_aclist = pd.read_csv(COMprev_long+'/accept_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv')
        dat_var_accept=pd.concat([dat_var_accept,prior_long_aclist],sort=False)
      fname = 'accept_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_accept,fname)

      dat_var_metar=dat_var[(dat_var['PBUFTYP']!=188) & (dat_var['PBUFTYP']!=288)]
      if comp_partial_flag==True:
        dat_var_reject=dat_var_metar[dat_var_metar['RMSE_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var_metar['VMAP']))]
        dat_var_reject=dat_var_reject[keep_cols]
        dat_var_reject['RJ_SHORT']=1.
        dat_var_reject[keep_cols+['RJ_SHORT']].to_csv(COM+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
      else: dat_var_reject = pd.DataFrame(columns=keep_cols)
      if cycle_HH=='23':
        dat_var_reject_long=dat_var_metar[dat_var_metar['RMSE_long_'+cyclestr]>=rjrmse*(1.0+(3.0-1.0)*(1-dat_var_metar['VMAP']))]
        dat_var_reject_long=dat_var_reject_long[keep_cols]
        dat_var_reject_long['RJ_LONG']=1.
        dat_var_reject_long.to_csv(COM+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)
        dat_var_reject=pd.concat([dat_var_reject,dat_var_reject_long],sort=False)
      elif cycle_HH!='23' and os.path.exists(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv'):
        print('FOUND PRIOR LONG REJECT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
        prior_long_rjlist = pd.read_csv(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv')
        dat_var_reject=pd.concat([dat_var_reject,prior_long_rjlist],sort=False)
      if stuck_flag==True: dat_var_reject=pd.concat([stuck_inst,dat_var_reject],sort=False)
      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject,fname)

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
        probecyc_m1 = datetime.strftime(datetime.strptime(probecyc,'%Y%m%d%H%M')-timedelta(hours=num_cycs),'%Y%m%d%H%M')
        COMprior_m1 = os.path.abspath(os.path.join(os.path.dirname(COM), '../'+'/'+exp+'.'+probecyc_m1[0:8]+'/autoqcprd.t'+probecyc_m1[8:12]+'z'))
        print('COMprior_m1 =',COMprior_m1)
        break
      itercyc = itercyc - delta
      ij+=1

    if vars[var] in ['t','ps','q','wst']:
      # Probe for previous partial reject list generated with RMSE stats and merge with stuck instrument check if file is found
      if os.path.exists(COMprior+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv'):
        prior_rjlist = pd.read_csv(COMprior+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv')
      elif os.path.exists(COMprior_m1+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_m1+'.csv'):
        prior_rjlist = pd.read_csv(COMprior_m1+'/reject_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_m1+'.csv')
      else: prior_rjlist=pd.DataFrame(columns=keep_cols)

      if os.path.exists(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv'):
        print('FOUND PRIOR LONG REJECT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
        prior_long_rjlist = pd.read_csv(COMprev_long+'/reject_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv')
        prior_rjlist = pd.concat([prior_rjlist,prior_long_rjlist],sort=False)

      if stuck_flag==True: dat_var_reject=pd.concat([stuck_inst,prior_rjlist],sort=False)
      else: dat_var_reject=prior_rjlist

      fname = 'reject_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_reject,fname)

    if vars[var] in ['wst']:
      # Probe for previous partial accept list generated with RMSE stats
      if os.path.exists(COMprior+'/accept_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv'):
        dat_var_accept = pd.read_csv(COMprior+'/accept_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc+'.csv')
      elif os.path.exists(COMprior_m1+'/accept_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_m1+'.csv'):
        dat_var_accept = pd.read_csv(COMprior_m1+'/accept_partial_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_m1+'.csv')
      else: dat_var_accept=pd.DataFrame(columns=keep_cols)

      if os.path.exists(COMprev_long+'/accept_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv'):
        print('FOUND PRIOR LONG ACCEPT LIST FOR DOMAIN =',thisRUN,'CYCLESTR =',cyclestr,'and VAR =',vars[var])
        prior_long_aclist = pd.read_csv(COMprev_long+'/accept_long_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+probecyc_long+'.csv')
        dat_var_accept = pd.concat([dat_var_accept,prior_long_aclist],sort=False)

      fname = 'accept_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.txt'
      write_lists(dat_var_accept,fname)

  return(dat_var)

def write_lists(input_list,fname):
  with open(fname,'w') as out_file:
    ltyp=fname.split('_')[0]
    if ltyp!='accept': out_file.write('********************************************************************************\n')
    if vars[var]=='t': out_file.write(fname.split('_')[0]+' list for temperature obs\n')
    elif vars[var]=='ps': out_file.write(fname.split('_')[0]+' list for surface pressure obs\n')
    elif vars[var]=='q': out_file.write(fname.split('_')[0]+' list for specific humidity obs\n')
    elif vars[var]=='wst' and ltyp!='accept': out_file.write(fname.split('_')[0]+' list for wind obs\n')

    if ltyp=='reject':
      if input_list.shape[0]>0:
        if 'RJ_SHORT' not in input_list: input_list['RJ_SHORT']=0.
        if 'RJ_LONG' not in input_list: input_list['RJ_LONG']=0.
        if 'STUCK' not in input_list: input_list['STUCK']=0.
        if 'FLAT' not in input_list: input_list['FLAT']=0.
        input_list[['RJ_SHORT','RJ_LONG','STUCK','FLAT']] = input_list[['RJ_SHORT','RJ_LONG','STUCK','FLAT']].fillna(value=0.)
        input_list[['RJ_SHORT','RJ_LONG','STUCK','FLAT']] = input_list.groupby(keep_cols)[['RJ_SHORT','RJ_LONG','STUCK','FLAT']].transform('sum')
        input_list['RJ_SHORT'].replace({0.: "---", 1.: "rms"}, inplace=True)
        input_list['RJ_LONG'].replace({0.: "---", 1.: "RMS"}, inplace=True)
        input_list['STUCK'].replace({0.: "---", 1.: "stk"}, inplace=True)
        input_list['FLAT'].replace({0.: "---", 1.: "flt"}, inplace=True)
        input_list['rjl_str']=input_list['RJ_SHORT']+input_list['RJ_LONG']+input_list['STUCK']+input_list['FLAT']+'---'
    elif ltyp=='accept':
      if input_list.shape[0]>0:
        if 'AC_SHORT' not in input_list: input_list['AC_SHORT']=0.
        if 'AC_LONG' not in input_list: input_list['AC_LONG']=0.
        input_list[['AC_SHORT','AC_LONG']] = input_list[['AC_SHORT','AC_LONG']].fillna(value=0.)
        input_list[['AC_SHORT','AC_LONG']] = input_list.groupby(keep_cols)[['AC_SHORT','AC_LONG']].transform('sum')
        input_list['AC_SHORT'].replace({0.: "---", 1.: "rms"}, inplace=True)
        input_list['AC_LONG'].replace({0.: "---", 1.: "RMS"}, inplace=True)
        input_list['rjl_str']=input_list['AC_SHORT']+input_list['AC_LONG']+'---------'
    input_list.drop_duplicates(subset=keep_cols,inplace=True)
    input_list.to_csv(ltyp+'_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

    if ltyp!='accept': out_file.write('********************************************************************************\n')
    for index, row in input_list.iterrows():
      if (vars[var]=='wst' and ltyp=='accept'): line=str(row[0]).ljust(8)+"| itype="+str(row[3])+"  lat="+str("%.4f" %row[4]).rjust(10)+"  lon="+\
        str("%.4f" %(row[5]-360)).rjust(10)+"  loc=US  origin: "+row[10]+'\n'
      else: line="'"+str(row[0]).ljust(8)+"| itype="+str(row[3])+"  lat="+str("%.4f" %row[4]).rjust(10)+"  lon="+\
        str("%.4f" %(row[5]-360)).rjust(10)+"  loc=US  origin: "+row[12]+"'"+'\n'
      out_file.write(line)

def duplicates(dat_var):

  dat_var_id_duplicates = dat_var[keep_cols+['DAT_'+cyclestr]].copy()
  dat_var_id_duplicates.dropna(subset=['DAT_'+cyclestr],inplace=True)
  # Find any duplicates in the newly read-in diagnostic file
  dat_var_new_duplicates = dat_var_id_duplicates[dat_var_id_duplicates.duplicated(['SAID'],keep=False)]
  dat_var_new_duplicates['LAST_CYC']=np.int(cyclestr)
  dat_var_new_duplicates = dat_var_new_duplicates[keep_cols+['LAST_CYC']]

  if os.path.exists(COMm1+'/'+'duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.csv'):
    # Duplicates found in previous diagnostic files
    orig_duplicates = pd.read_csv(COMm1+'/'+'duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr_m1+'.csv')
    stations = orig_duplicates['SAID'].str.strip()
    # Make sure none of the original duplicates show up using a different configuration
    new_duplicates = dat_var_new_duplicates.loc[dat_var_new_duplicates['SAID'].isin(stations)]
    new_duplicates = new_duplicates[keep_cols]
    new_duplicates['LAST_CYC'] = np.int(cyclestr)
    # Merge together each list of duplicates
    dfs=[orig_duplicates,new_duplicates,dat_var_new_duplicates]
    dat_var_duplicates = pd.concat(dfs,sort=True)
  else:
    dat_var_duplicates = dat_var_new_duplicates

  unique_stns = []
  for stn in dat_var_duplicates['SAID'].unique():
    dat_var_stn = dat_var_duplicates[dat_var_duplicates['SAID']==stn].sort_values(by=['LAT'])
    dat_var_stn[['LAT','LON']]=dat_var_stn[['LAT','LON']].astype(float)
    dat_var_stn['DISTANCE'] = gc_dist(dat_var_stn['LAT'], dat_var_stn['LON'],dat_var_stn['LAT'].shift(1), dat_var_stn['LON'].shift(1))
    dat_var_stn.dropna(subset=['DISTANCE'],inplace=True)
    if min(dat_var_stn['DISTANCE'].values)>10.0: unique_stns.append(stn)
  dat_var_duplicates = dat_var_duplicates.loc[~dat_var_duplicates['SAID'].isin(unique_stns)]

  dat_var_duplicates = dat_var_duplicates.sort_values('LAST_CYC')
  dat_var_duplicates.drop_duplicates(subset=keep_cols,keep='last',inplace=True)

  # Write duplicates to a spreadsheet for reference
  dat_var_duplicates = dat_var_duplicates[keep_cols+['LAST_CYC']]
  dat_var_duplicates = dat_var_duplicates[dat_var_duplicates['LAST_CYC']>np.int(cyc_purge_dups.strftime('%Y%m%d%H%M'))]
  dat_var_duplicates = dat_var_duplicates[dat_var_duplicates.duplicated(['SAID'],keep=False)]
  if not dat_var_duplicates.empty: dat_var_duplicates.to_csv(COM+'/duplicates_'+exp+'_'+thisRUN+'_'+vars[var]+'_'+cyclestr+'.csv', index=False)

  # Update: Allow the duplicate stations to populate the SQL database
  #dat_var = dat_var.loc[~dat_var['SAID'].isin(dat_var_duplicates['SAID'].values)]

  return(dat_var)

if __name__ == "__main__":

  print('Starting Python program.')

  dateobj=sys.argv[1]
  DATA=sys.argv[2]
  COM=sys.argv[3]
  COMm1=sys.argv[4]
  cyclestr_m1=sys.argv[5]
  probecyc_long=sys.argv[6]

  thisRUN='conus'
  exp='rtma3d'

  cyclestr=dateobj
  datestr=dateobj[0:8]
  cycle_HH=dateobj[8:10]
  cyc_purge=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-40)

  epochcyc=datetime.strptime(str(197001010000),'%Y%m%d%H%M')

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

  # Define constants
  mnet_bctypes=[288,295] # PREPBUFR report types for mesonet wind bias correction
  winf=2. # Constant asymptotic weight associated with an observation
  tinf=365.24 # Constant timescale associated with an observation
  alpha_0=0. # Effective logarithmic bias correction for an observation without any history
  speed_min=2. # Low cut-off for acceptable windspeeds (GES & ANL)

  delta = timedelta(minutes=60)
  comp_partial_flag=False # Controls whether the short-term stats & reject lists are computed
  num_cycs=6 # Number of cycles to compute stats (RMSE, bias) over
  num_stuck=6 # Number of cycles for the "stuck" instrument check
  num_cycs_long=48 # Number of hours to compute long-term RMSE stats
  num_relax=2 # Number of cycles allowed to be "missing" (stuck instrument and RMSE tests)
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

  ndays_purge = math.ceil((999-len(keep_cols))/((1.+24./num_cycs)*5.))-1. # Account for current day by subtracting 1
  cyc_purge_stats=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-ndays_purge)
  cyc_purge_dups=datetime.strptime(cyclestr,'%Y%m%d%H%M')+timedelta(days=-7)

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

  dat_anl['VMAP']=1.0
  dat_ges['VMAP']=1.0

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

    dat_var=calc_sza(dat_var,dateobj)

    dat_var=dat_var[keep_cols+['DAT_'+cyclestr]]

    gen_database(dat_var,columns,cyc_purge,eps,geps,rjrmse)

  f_out='done.'+cyclestr
  with open(f_out,'w') as out_file:
    out_file.write('AUTOQC step has completed successfully for '+cyclestr)

