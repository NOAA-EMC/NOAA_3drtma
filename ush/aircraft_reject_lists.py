#!/usr/bin/env python

from datetime import datetime,timedelta
import numpy as np
import pandas as pd
import os,sys,math,re
import gzip,shutil
import sqlite3
from functools import reduce

def read_diagconv(diagfile):
  #unzip diag file
  print('diagfile = ',diagfile)
  diagout=os.path.basename(os.path.splitext(diagfile)[0])+'_'+cyclestr
  with gzip.open(diagfile, 'rb') as f_in, open(diagout, 'wb') as f_out:
    shutil.copyfileobj(f_in, f_out)

  shutil.copyfile(diagout,'diag_conv.dat')

  # Read diag file and put in ascii format
  READDIAG='/lfs/h2/emc/da/noscrub/matthew.t.morris/packages/rtma3d.v1.0.0/exec/rtma3d_read_diag'
  os.system(READDIAG+' '+diagout)
  shutil.copyfile('diag_results', diagout)

def gen_aircraft_reject_lists(dat_var,cyc_purge,rjrmse):

  dat_var['counts_'+cyclestr]=dat_var.groupby(['SAID','PBUFTYP'])['SAID'].transform('count')
  dat_var['SUM_OmFs_'+cyclestr]=dat_var.groupby(['SAID','PBUFTYP'])['INC-GES'].transform('sum')
  dat_var['SUM_OmFs2_'+cyclestr]=dat_var.groupby(['SAID','PBUFTYP'])['INC-GES'].transform(lambda x: (x**2).sum())
  if vars[var]=='wst':
    dat_var['SUM_OmFs_wdir_'+cyclestr]=dat_var.groupby(['SAID','PBUFTYP'])['WDIR_INC'].transform('sum')
    dat_var['SUM_OmFs2_wdir_'+cyclestr]=dat_var.groupby(['SAID','PBUFTYP'])['WDIR_INC'].transform(lambda x: (x**2).sum())

  # Remove duplicates to maintain one entry per unique aircraft ID
  dat_var.drop_duplicates(subset=keep_cols,inplace=True)

  # If file exists, merge new dat_var array with data from existing database

  if cyclestr==startdate and os.path.exists(COMprevday+'/'+thisRUN+'.t'+probeHH_aircraft+'z.aircraft_database_'+vars[var]+'.db'):
    # Open the connection to the SQLite database
    print("Opening file: ",COMprevday+'/'+thisRUN+'.t'+probeHH_aircraft+'z.aircraft_database_'+vars[var]+'.db')
    cnx = sqlite3.connect(COMprevday+'/'+thisRUN+'.t'+probeHH_aircraft+'z.aircraft_database_'+vars[var]+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    columns=list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') > cyc_purge),columns[len(keep_cols):]))]
    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    dat_var=dat_var[keep_cols+(dat_var.columns.drop(keep_cols).tolist())] # Reorder so keep_cols are at beginning of dat_var
    cnx.close()

  if os.path.exists(thisRUN+'.t'+stopHH_aircraft+'z.aircraft_database_'+vars[var]+'.db'):
    # Open the connection to the SQLite database
    cnx = sqlite3.connect(thisRUN+'.t'+stopHH_aircraft+'z.aircraft_database_'+vars[var]+'.db')
    data = pd.read_sql("SELECT * FROM "+var_str,cnx)
    columns=list(data.columns)
    data=data[keep_cols+list(filter(lambda x: (datetime.strptime(x.split('_')[-1],'%Y%m%d%H') > cyc_purge),columns[len(keep_cols):]))]
    dfs = [data,dat_var]
    dat_var = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)
    dat_var=dat_var[keep_cols+(dat_var.columns.drop(keep_cols).tolist())] # Reorder so keep_cols are at beginning of dat_var

  else:
    # Open the connection to the SQLite database (will create one if it doesn't exist)
    cnx = sqlite3.connect(thisRUN+'.t'+stopHH_aircraft+'z.aircraft_database_'+vars[var]+'.db')

  dat_var[['PBUFTYP']]=dat_var[['PBUFTYP']].astype(np.int)

  if cycle_HH=='23':
    # Aggregate the stats for the current day
    dat_var['daily_counts_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('counts_'+datestr)]].sum(axis=1)
    dat_var['daily_SUM_OmFs_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('SUM_OmFs_'+datestr)]].sum(axis=1)
    dat_var['daily_SUM_OmFs2_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('SUM_OmFs2_'+datestr)]].sum(axis=1)
    if vars[var]=='wst':
      dat_var['daily_SUM_OmFs_wdir_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('SUM_OmFs_wdir_'+datestr)]].sum(axis=1)
      dat_var['daily_SUM_OmFs2_wdir_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('SUM_OmFs2_wdir_'+datestr)]].sum(axis=1)

    # Aggregate the stats for the entire period of interest
    dat_var['total_counts_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('daily_counts_')]].sum(axis=1)
    dat_var['total_SUM_OmFs_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('daily_SUM_OmFs_')]].sum(axis=1)
    dat_var['total_SUM_OmFs2_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('daily_SUM_OmFs2_')]].sum(axis=1)
    if vars[var]=='wst':
      dat_var['total_SUM_OmFs_wdir_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('daily_SUM_OmFs_wdir_')]].sum(axis=1)
      dat_var['total_SUM_OmFs2_wdir_'+cyclestr]=dat_var.loc[:, [x for x in dat_var.columns if x.startswith('daily_SUM_OmFs2_wdir_')]].sum(axis=1)

    # Calculate Standard Deviation (stddev)
    dat_var['mean_OmFs_'+cyclestr]=dat_var['total_SUM_OmFs_'+cyclestr]/dat_var['total_counts_'+cyclestr]
    dat_var['stddev_'+cyclestr]=np.sqrt((dat_var['total_SUM_OmFs2_'+cyclestr]/dat_var['total_counts_'+cyclestr])-dat_var['mean_OmFs_'+cyclestr].pow(2))
    if vars[var]=='wst': 
      dat_var['mean_OmFs_wdir_'+cyclestr]=dat_var['total_SUM_OmFs_wdir_'+cyclestr]/dat_var['total_counts_'+cyclestr]
      dat_var['stddev_wdir_'+cyclestr]=np.sqrt((dat_var['total_SUM_OmFs2_wdir_'+cyclestr]/dat_var['total_counts_'+cyclestr])-dat_var['mean_OmFs_wdir_'+cyclestr].pow(2))

    # Calculate Bias
    dat_var['Bias_'+cyclestr]=dat_var['total_SUM_OmFs_'+cyclestr]/dat_var['total_counts_'+cyclestr]
    # Wind direction
    if vars[var]=='wst':
      dat_var['Bias_wdir_'+cyclestr]=dat_var['total_SUM_OmFs_wdir_'+cyclestr]/dat_var['total_counts_'+cyclestr]

    dat_var['stddev_'+cyclestr]=dat_var['stddev_'+cyclestr].round(3)
    dat_var['SUM_OmFs_'+cyclestr]=dat_var['SUM_OmFs_'+cyclestr].round(3)
    dat_var['SUM_OmFs2_'+cyclestr]=dat_var['SUM_OmFs2_'+cyclestr].round(3)
    dat_var['Bias_'+cyclestr]=dat_var['Bias_'+cyclestr].round(3)
    # Wind direction
    if vars[var]=='wst':
      dat_var['stddev_wdir_'+cyclestr]=dat_var['stddev_wdir_'+cyclestr].round(3)
      dat_var['SUM_OmFs_wdir_'+cyclestr]=dat_var['SUM_OmFs_wdir_'+cyclestr].round(3)
      dat_var['Bias_wdir_'+cyclestr]=dat_var['Bias_wdir_'+cyclestr].round(3)

    if vars[var] in ['t','q','wst']:
      dat_var_reject=dat_var.copy()
      if vars[var]=='wst':
        dat_var_reject['RJ_FLAG']=np.where((dat_var_reject['stddev_'+cyclestr]>=rjrmse) | \
                                           (dat_var_reject['Bias_'+cyclestr]<=(-1.)*rjbias) | \
                                           (dat_var_reject['Bias_'+cyclestr]>=rjbias) | \
                                           (dat_var_reject['stddev_wdir_'+cyclestr]>=rjstd_wdir) | (dat_var_reject['Bias_wdir_'+cyclestr]>=rjbias_wdir),1,0)
        dat_var_reject['STD_FLAG']=np.where((dat_var_reject['stddev_'+cyclestr]>=rjrmse),1,0)
        dat_var_reject['BIAS_FLAG']=np.where((dat_var_reject['Bias_'+cyclestr]<=(-1.)*rjbias) | \
                                             (dat_var_reject['Bias_'+cyclestr]>=rjbias),1,0)
        dat_var_reject['STD_FLAG_DIR']=np.where((dat_var_reject['stddev_wdir_'+cyclestr]>=rjstd_wdir),1,0)
        dat_var_reject['BIAS_FLAG_DIR']=np.where((dat_var_reject['Bias_wdir_'+cyclestr]>=rjbias_wdir),1,0)
      else:
        dat_var_reject['RJ_FLAG']=np.where((dat_var_reject['stddev_'+cyclestr]>=rjrmse) | \
                                           (dat_var_reject['Bias_'+cyclestr]<=(-1.)*rjbias) | \
                                           (dat_var_reject['Bias_'+cyclestr]>=rjbias),1,0)
        dat_var_reject['STD_FLAG']=np.where((dat_var_reject['stddev_'+cyclestr]>=rjrmse),1,0)
        dat_var_reject['BIAS_FLAG']=np.where((dat_var_reject['Bias_'+cyclestr]<=(-1.)*rjbias) | \
                                             (dat_var_reject['Bias_'+cyclestr]>=rjbias),1,0)
      dat_var_reject = dat_var_reject.loc[:, dat_var_reject.columns.str.startswith(tuple(keep_cols+['daily_','total_','counts_','stddev_','Bias_','RJ_FLAG','STD_FLAG','BIAS_FLAG']))].copy()
      dat_var_reject.to_csv(thisRUN+'.t'+cycle_HH+'z.aircraft_reject_'+vars[var]+'.csv', index=False)

  # Convert to SQL database and close connection
  dat_var_save = dat_var.loc[:, dat_var.columns.str.startswith(tuple(keep_cols+['daily_','total_','counts_','SUM_OmFs_','SUM_OmFs2_','mean_','stddev_','Bias_']))].copy()
  # Drop stations from the database that haven't reported recently (i.e., all values are missing)
  dat_var_save.dropna(subset=[column for column in dat_var_save.columns if column.startswith(('counts_','SUM_OmFs_','SUM_OmFs2_'))],how='all',inplace=True)
  dat_var_save.to_sql(name=var_str,con=cnx,index=False,if_exists ='replace')
#  dat_var_save.to_csv(thisRUN+'.t'+stopHH_aircraft+'z.aircraft_database_'+vars[var]+'.csv', index=False)
  cnx.close()

  return(dat_var)

def combine_reject_lists(dat_var):
  # Read in individual reject lists
  temp_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.aircraft_reject_t.csv')
  wind_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.aircraft_reject_wst.csv')
  rhel_list = pd.read_csv(thisRUN+'.t'+cycle_HH+'z.aircraft_reject_q.csv')

  temp_list.replace({'PBUFTYP': {130:"UPA", 131:"UPA", 133:"UPA", 134:"UPA", 135:"UPA"}},inplace=True)
  wind_list.replace({'PBUFTYP': {230:"UPA", 231:"UPA", 233:"UPA", 234:"UPA", 235:"UPA"}},inplace=True)
  rhel_list.replace({'PBUFTYP': {130:"UPA", 131:"UPA", 133:"UPA", 134:"UPA", 135:"UPA"}},inplace=True)

  # Rename columns
  rename_cols=['daily','total','counts','stddev','Bias','RJ_FLAG','STD_FLAG','BIAS_FLAG']
  temp_list.rename(columns={col:'{}-T'.format(col, j) for col in [col for col in temp_list if col.startswith(tuple(rename_cols))]}, inplace=True)
  wind_list.rename(columns={col:'{}-W'.format(col, j) for col in [col for col in wind_list if col.startswith(tuple(rename_cols))]}, inplace=True)
  rhel_list.rename(columns={col:'{}-RH'.format(col, j) for col in [col for col in rhel_list if col.startswith(tuple(rename_cols))]}, inplace=True)

  if 'RJ_FLAG-T' not in temp_list: temp_list['RJ_FLAG-T']=0.
  if 'RJ_FLAG-W' not in wind_list: wind_list['RJ_FLAG-W']=0.
  if 'RJ_FLAG-RH' not in rhel_list: rhel_list['RJ_FLAG-RH']=0.

  # Combine lists
  dfs = [temp_list,wind_list,rhel_list]
  merged_list = reduce(lambda left,right: pd.merge(left,right,on=keep_cols,how='outer'), dfs)

  # Assign flag to accept observations if no data were available
  merged_list[['RJ_FLAG-T','RJ_FLAG-W','RJ_FLAG-RH']] = merged_list[['RJ_FLAG-T','RJ_FLAG-W','RJ_FLAG-RH']].fillna(value=0.)

  # Create final usage flag; drop aircraft with no reject flags
  merged_list.replace({'RJ_FLAG-T': {0:"-",1:"T"},'RJ_FLAG-W': {0:"-",1:"W"},'RJ_FLAG-RH': {0:"-",1:"R"}},inplace=True)
  merged_list['T-W-RH'] = merged_list['RJ_FLAG-T'].astype(str) + ' ' + merged_list['RJ_FLAG-W'].astype(str) + ' ' + merged_list['RJ_FLAG-RH'].astype(str)
  merged_list=merged_list[merged_list['T-W-RH'] != '- - -']

  # Create string that indicates why aircraft obs are to be rejected
  merged_list[['STD_FLAG-T','STD_FLAG-W','STD_FLAG_DIR-W','STD_FLAG-RH']] = merged_list[['STD_FLAG-T','STD_FLAG-W','STD_FLAG_DIR-W','STD_FLAG-RH']].fillna(value=0.)
  merged_list[['BIAS_FLAG-T','BIAS_FLAG-W','BIAS_FLAG_DIR-W','BIAS_FLAG-RH']] = merged_list[['BIAS_FLAG-T','BIAS_FLAG-W','BIAS_FLAG_DIR-W','BIAS_FLAG-RH']].fillna(value=0.)

  merged_list.replace({'STD_FLAG-T': {0:"",1:"std_T "},'BIAS_FLAG-T': {0:"",1:"bias_T "}},inplace=True)
  merged_list.replace({'STD_FLAG-W': {0:"",1:"std_W "},'BIAS_FLAG-W': {0:"",1:"bias_W "}},inplace=True)
  merged_list.replace({'STD_FLAG_DIR-W': {0:"",1:"std_DIR "},'BIAS_FLAG_DIR-W': {0:"",1:"bias_DIR "}},inplace=True)
  merged_list.replace({'STD_FLAG-RH': {0:"",1:"std_RH "},'BIAS_FLAG-RH': {0:"",1:"bias_RH "}},inplace=True)

  merged_list['Reason'] = merged_list['BIAS_FLAG-T'].astype(str) + merged_list['STD_FLAG-T'].astype(str) +\
                          merged_list['BIAS_FLAG-W'].astype(str) + merged_list['STD_FLAG-W'].astype(str) +\
                          merged_list['BIAS_FLAG_DIR-W'].astype(str) + merged_list['STD_FLAG_DIR-W'].astype(str) +\
                          merged_list['BIAS_FLAG-RH'].astype(str) + merged_list['STD_FLAG-RH'].astype(str)

  cols_reject=['daily','total','counts','stddev','Bias','T-W-RH','Reason']
  merged_list=merged_list.loc[:, merged_list.columns.str.startswith(tuple(keep_cols+cols_reject))].copy()

  merged_list.rename(columns={"stddev_"+cyclestr+"-T": "std-T", "Bias_"+cyclestr+"-T": "Bias-T"},inplace=True)
  merged_list.rename(columns={"stddev_"+cyclestr+"-W": "std-W", "Bias_"+cyclestr+"-W": "Bias-W"},inplace=True)
  merged_list.rename(columns={"stddev_wdir_"+cyclestr+"-W": "std-Wdir", "Bias_wdir_"+cyclestr+"-W": "Bias-Wdir"},inplace=True)
  merged_list.rename(columns={"stddev_"+cyclestr+"-RH": "std-RH", "Bias_"+cyclestr+"-RH": "Bias-RH"},inplace=True)

  # Record the # of observations for each variable; set this to 0 if a station doesn't report a variable
  merged_list.rename(columns={"total_counts_"+cyclestr+"-T": "N-T","total_counts_"+cyclestr+"-RH": "N-RH","total_counts_"+cyclestr+"-W": "N-W"},inplace=True)
  try: merged_list[['N-T','N-RH','N-W']] = merged_list[['N-T','N-RH','N-W']].fillna(value=0).astype(np.int64)
  except: pass

  merged_list.to_csv(thisRUN+'.t'+stopHH_aircraft+'z.merged_list_aircraft.csv',index=False)

  fname_merged=thisRUN+'.t'+cycle_HH+'z.aircraft_rjs_merged.txt'
  with open(fname_merged,'w') as out_file:
    #header=';Tail     Errors N-T    Bias-T   Std-T    N-W    Bias-W   Std-W    Bias-DIR Std-DIR  N-RH   Bias-RH  Std-RH'+'\n'
    header=[
      ";Using the AutoQC databases containing the previous 7 days of observational data,\n",
      ";aircraft observations are rejected if one or more of the following conditions is met\n",
      ";for a given field (temperature, wind speed & direction, relative humidity):\n",
      ";  bias_T > 2,\n",
      ";  std_T > 2,\n",
      ";  bias_W > 2,\n",
      ";  std_W > 5,\n",
      ";  bias_DIR > 7,\n",
      ";  std_DIR > 30,\n",
      ";  bias_RH > 10,\n",
      ";  std_RH > 20,\n",
      ";\n",";\n",";\n",
      ';Tail      Errors N-T    Bias-T   Std-T    N-W    Bias-W   Std-W    Bias-DIR Std-DIR  N-RH   Bias-RH  Std-RH'+'\n',
      "\n"
      ]

    out_file.writelines(header)
    for index, row in merged_list.iterrows():
      try:
        line=str(row['SAID']).ljust(8)+" "*2+str(row['T-W-RH'])+"  "+\
          str(row['N-T']).ljust(6)+" "+"{:.3f}".format(row['Bias-T']).ljust(8)+" "+"{:.3f}".format(row['std-T']).ljust(8)+" "+\
          str(row['N-W']).ljust(6)+" "+"{:.3f}".format(row['Bias-W']).ljust(8)+" "+"{:.3f}".format(row['std-W']).ljust(8)+" "+\
          "{:.3f}".format(row['Bias-Wdir']).ljust(8)+" "+"{:.3f}".format(row['std-Wdir']).ljust(8)+" "+\
          str(row['N-RH']).ljust(6)+" "+"{:.3f}".format(row['Bias-RH']).ljust(8)+" "+"{:.3f}".format(row['std-RH']).ljust(8)+\
          '( '+str(row['Reason'])+')'+'\n'
        out_file.write(line)
      except: pass

if __name__ == "__main__":

  print('Starting Python program.')

  thisRUN=sys.argv[1]
  startdate=sys.argv[2]
  stopdate=sys.argv[3]
  DATA=sys.argv[4]
  COM=sys.argv[5]
  COMprevday=sys.argv[6]
  probecyc_aircraft=sys.argv[7]
  dom=sys.argv[8]

  stopHH_aircraft=stopdate[8:10]
  probeHH_aircraft=probecyc_aircraft[8:10]

  NET='rtma3d' # MTM - remove after RUN is defined correctly

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
  aircraft_pbuftypes=[130,131,133,134,135,230,231,233,234,235] # PREPBUFR report types for aircraft observations

  num_days_stats=7 # Number of days to use in computing aircraft reject lists

  rjrmse_w=5. # m/s
  rjrmse_t=2. # K
  rjrmse_rh=20. # %

  rjbias_w=2. # m/s
  rjbias_t=2. # K
  rjbias_rh=10. # %

  rjstd_wdir=30. # Degrees
  rjbias_wdir=7. # Degrees

  #--------------------------------------------------------------------#
  # Loop through each forecast hour.
  #--------------------------------------------------------------------#
  
  startcyc=datetime.strptime(str(startdate),'%Y%m%d%H')
  stopcyc=datetime.strptime(str(stopdate),'%Y%m%d%H')
  
  i=0
  dateobj=startcyc
  delta = timedelta(minutes=60)
  keep_cols=['SAID','PBUFTYP']
  columns=keep_cols.copy()
  
  while dateobj <= stopcyc:
    cyclestr=dateobj.strftime("%Y%m%d%H")
    datestr=dateobj.strftime("%Y%m%d")
    cycle_HH=dateobj.strftime("%H")
    cyc_purge=datetime.strptime(cyclestr,'%Y%m%d%H')+timedelta(days=-num_days_stats)

    diagdir=os.path.abspath(os.path.join(os.path.dirname(COM), '../../'+'/'+thisRUN+'.'+datestr+'/'+dom))

    #--------------------------------------------------------------------#
    # Read in data from the analysis (anl) diagnostic file.
    #--------------------------------------------------------------------#

    diagfile_anl=diagdir+'/rtma3d.t'+cycle_HH+'z.diag_conv_anl.gz'  
    read_diagconv(diagfile_anl)
    try: dat_anl=pd.read_table('rtma3d.t'+cycle_HH+'z.diag_conv_anl_'+cyclestr,names=my_cols,dtype=dtypedict,encoding='latin1',
                 delim_whitespace=True,usecols=usecols,header=None,na_values=['Infinity',bmiss])
    except: dat_anl = pd.DataFrame(columns=my_cols)
    dat_anl.dropna(subset=['OB','INC'])
    # Select the aircraft observations only
    dat_anl_filtered=dat_anl[dat_anl['PBUFTYP'].isin(aircraft_pbuftypes)]
    print('DEBUG: cycle = ',cyclestr,'dat_anl.shape = , dat_anl_filtered.shape = ',dat_anl.shape,dat_anl_filtered.shape)

    #--------------------------------------------------------------------#
    # Read in data from the background (ges) diagnostic file.
    #--------------------------------------------------------------------#

    diagfile_ges=diagdir+'/rtma3d.t'+cycle_HH+'z.diag_conv_ges.gz'
    read_diagconv(diagfile_ges)
    try: dat_ges=pd.read_table('rtma3d.t'+cycle_HH+'z.diag_conv_ges_'+cyclestr,names=my_cols,dtype=dtypedict,encoding='latin1',
                 delim_whitespace=True,usecols=usecols,header=None,na_values=['Infinity',bmiss])
    except: dat_ges = pd.DataFrame(columns=my_cols)
    dat_ges.dropna(subset=['OB','INC'])
    # Select the aircraft observations only
    dat_ges_filtered=dat_ges[dat_ges['PBUFTYP'].isin(aircraft_pbuftypes)]

    #--------------------------------------------------------------------#
    # Reverse the time series ordering of diagnostic files and merge
    # together the anl and ges files.
    #--------------------------------------------------------------------#
  
    # Added [::-1] to reverse time series, such that positive DHRs will be encountered before negative DHRs, thus giving those
    # preference in the event that two DHRs with same absolute value, but of opposite sign, are encoutnered.  This is how these
    # observations are selected in the GSI ob selection algorithm to only use the observation valid closest to the analysis time.
    dat_anl=dat_anl[::-1]
    dat_ges=dat_ges[::-1]

    dfs = [dat_anl_filtered,dat_ges_filtered]

    for j, df in enumerate(dfs, start=0):
      if j==0: df.rename(columns={col:'{}-ANL'.format(col, j) for col in ('IUSE','INC','VINC','RUSAGE')}, inplace=True)
      if j==1: df.rename(columns={col:'{}-GES'.format(col, j) for col in ('IUSE','INC','VINC','RUSAGE')}, inplace=True)

    merge_cols=['SAID','PROVIDER','SUBPROVIDER','PBUFTYP','LAT','LON','HGHT','PRES','OBTYPE','DHR','OB','VOB','TDRY']
    dat_merged = reduce(lambda left,right: pd.merge(left,right,on=merge_cols), dfs)

    dat_merged['GES']=dat_merged['OB']-dat_merged['INC-GES']

    # Loop through each variable
    vars=['t','q','wst']
    for var in range(len(vars)):
  
      if vars[var]=='t': var_str='temperature'; rjrmse=rjrmse_t; rjbias=rjbias_t
      elif vars[var]=='q': var_str='moisture'; rjrmse=rjrmse_rh; rjbias=rjbias_rh
      elif vars[var]=='wst': var_str='wind_speed'; rjrmse=rjrmse_w; rjbias=rjbias_w

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
        #print('Observation: Min Wind Direction = ',dat_var['WDIR_OB'].min(),'Max Wind Direction =',dat_var['WDIR_OB'].max())

        # Calculate background/GES wind direction:
        dat_var['WDIR_GES']=90.0-(rad2deg*np.arctan2(-1.0*dat_var['V_GES'],-1.0*dat_var['U_GES']))
        dat_var.loc[dat_var['WDIR_GES']<0,'WDIR_GES']=dat_var['WDIR_GES']+360. # Correct negative wind directions
        #print('Background: Min Wind Direction = ',dat_var['WDIR_GES'].min(),'Max Wind Direction =',dat_var['WDIR_GES'].max())

        # Calculate the difference between the above two wind direction
        dat_var['WDIR_DIFF1']=(dat_var['WDIR_OB']-dat_var['WDIR_GES'])%360
        dat_var['WDIR_DIFF2']=(dat_var['WDIR_GES']-dat_var['WDIR_OB'])%360
        dat_var['WDIR_INC']=dat_var[['WDIR_DIFF1','WDIR_DIFF2']].min(axis=1)

        dat_var.drop(['PRES','OB','GES','INC-GES','VOB','VINC-GES','WDIR_DIFF1','WDIR_DIFF2'],axis=1,inplace=True)
        dat_var.rename(columns={"WSPD_OB": "OB", "WSPD_GES": "GES", "WSPD_INC": "INC-GES"},inplace=True)

      elif vars[var]=='q':

        dat_var=dat_merged[dat_merged['OBTYPE']=='q'].copy()

        #--------------------------------------------------------------------#
        # Compute the observed relative humidity (RH):
        #--------------------------------------------------------------------#

        # Constants:
        es_w_0=611.2 # saturation vapor pressure of water at 0degC (Pa)
        L_c=2.5e+06 # latent heat of condensation at 273.15K (J kg^-1)
        rv=4.6150e2 # specific gas constant for water vapor (J K^-1 kg^-1)
        t0c=273.15 # temperature at zero celsius (K)
        epsilon=0.62198 # ratio of molecular weight of water and dry air

        # First, compute the saturation mixing ratio (same for ANL/GES)
        tdry_obs = dat_var[dat_var['OBTYPE']=='q']['TDRY'].values                # Units: K
        ps_obs = dat_var[dat_var['OBTYPE']=='q']['PRES'].values * 100.           # Units: Pa
        es_obs = es_w_0 * np.exp((L_c / rv) * ((1.0 / t0c) - (1.0 / tdry_obs)))  # Saturation vapor pressure (Pa)
        w_sat = (epsilon * es_obs) / (ps_obs - es_obs)                           # Saturation mixing ratio (kg/kg)

        # Compute the observed mixing ratio
        q_obs  = dat_var[dat_var['OBTYPE']=='q']['OB'].values/1000               # Convert from g/kg to kg/kg
        w_obs  = q_obs/(1-q_obs)                                                 # Units: kg/kg

        # Compute observed relative humidity (RH):
        dat_var['RH_OB'] = w_obs/w_sat*100.
        dat_var['RH_OB'] = dat_var['RH_OB'].round(2)

        #--------------------------------------------------------------------#
        # Compute the background relative humidity (RH):
        #--------------------------------------------------------------------#

        # Compute the background mixing ratio
        q_ges  = dat_var[dat_var['OBTYPE']=='q']['GES'].values/1000              # Convert from g/kg to kg/kg
        w_ges  = q_ges/(1-q_ges)                                                 # Units: kg/kg

        # Compute background relative humidity (RH):
        dat_var['RH_GES'] = w_ges/w_sat*100.
        dat_var['RH_GES'] = dat_var['RH_GES'].round(2)

        #--------------------------------------------------------------------#
        # Compute the relative humidity (RH) innovation:
        #--------------------------------------------------------------------#

        dat_var['RH_INC'] = dat_var['RH_OB'] - dat_var['RH_GES']

        dat_var.drop(['PRES','OB','GES','INC-GES'],axis=1,inplace=True)
        dat_var.rename(columns={"RH_OB": "OB", "RH_GES": "GES", "RH_INC": "INC-GES"},inplace=True)

      else:

        dat_var=dat_merged[dat_merged['OBTYPE']==vars[var]].copy()
        dat_var.drop(['PRES'],axis=1,inplace=True)
  
      dat_var = gen_aircraft_reject_lists(dat_var,cyc_purge,rjrmse)

    # Once the individual lists are created, merge together to form the final reject list
    if cycle_HH=='23': combine_reject_lists(dat_var)

    dateobj += delta
    i += 1

