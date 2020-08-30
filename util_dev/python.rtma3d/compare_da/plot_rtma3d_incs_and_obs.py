import pygrib
import matplotlib
matplotlib.use('Agg')
import io
import matplotlib.pyplot as plt
#from PIL import Image
import matplotlib.image as image
from matplotlib.gridspec import GridSpec
import mpl_toolkits
mpl_toolkits.__path__.append('/gpfs/dell2/emc/modeling/noscrub/gwv/py/lib/python/basemap-1.2.1-py3.6-linux-x86_64.egg/mpl_toolkits/')
from mpl_toolkits.basemap import Basemap, maskoceans
import numpy as np
import pandas as pd
import time,os,sys,multiprocessing
import ncepy, gzip, shutil
from scipy import ndimage
from netCDF4 import Dataset
from datetime import datetime,timedelta

def main():

  # Number of processes must coincide with the number of domains to plot
  pool = multiprocessing.Pool(len(domains))
#  print domains
  pool.map(plot_all,domains)


def read_diagconv(diagfile):
  #unzip diag file
  diagout=os.path.basename(os.path.splitext(diagfile)[0])
  with gzip.open(diagfile, 'rb') as f_in, open(diagout, 'wb') as f_out:
    shutil.copyfileobj(f_in, f_out)
  # Read diag file and put in ascii format - may want to send the location of this exec
  #  in a more general way.  Prefer not use env variables put maybe best choice for later.
  
  READDIAG=os.getenv('READDIAG')
  if READDIAG==None:
    sys.exit('READDIAG environment variable not defined.  Needs to be specified to read diag files. Exit.')
  os.system(READDIAG+' '+diagout)
  # read obs from diag file

  my_cols = ["OBTYPE","SAID","PROVIDER","SUBPROVIDER","PBUFTYP",\
             "DHR","LAT","LON","PRES","HGHT","IUSE","UOB","UINC","VOB","VINC","OBERR","RUSAGE"]
  usecols=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
  dtypedict={"OBTYPE":str,"SAID":str,"PROVIDER":str,"SUBPROVIDER":str,"PBUFTYP":np.int64,\
          "DHR":np.float64,"LAT":np.float64,"LON":np.float64,"PRES":np.float64,\
          "HGHT":np.float64,"IUSE":np.int64,"UOB":np.float64,"UINC":np.float64,\
          "VOB":np.float64,"VINC":np.float64,"OBERR":np.float64,"RUSAGE":np.float64}
  
  bmiss=0.10000E+10
  dat=pd.read_table('./diag_results',names=my_cols,encoding='latin1',na_values=['Infinity',bmiss],
                   delim_whitespace=True,dtype=dtypedict,usecols=usecols,header=None)

  #  Remove all data from restricted providers
  rst_providers_arr = get_restricted_providers()
  dat=dat[~dat['PROVIDER'].isin(rst_providers_arr)]

  # Toss erroneous data - i.e. obs which have NaNs.
  dat=dat.drop(dat[dat['UOB'].isnull()].index)

  return dat

def writefig(fname):
  plt.savefig(fname+'.png',bbox_inches='tight')
  os.system('convert '+fname+'.png '+fname+'.gif')
  os.remove(fname+'.png')

def get_restricted_providers():
  restricted_providers=['AFA',
                        'AWS',
                        'AWX',
                        'CAIC',
                        'CO_E-470',
                        'GADOT',
                        'GLDNWS',
                        'IEM',
                        'KSDOT',
                        'LSU-JSU',
                        'NC-ECONet',
                        'OK-Meso',
                        'WxFlow',
                        'WXforYou',
                        'ABASINA',
                        'ASPENSKICO',
                        'KCCI-TV',
                        'KELO-TV',
                        'KIMT-TV',
                        'NPPHA',
                        'LAIS',]
  return restricted_providers

def get_diag_otype_fname_var(var):
  if var=="tcamt":
    diag_o="tca"
    fname_v=var
  elif var=="t":
    diag_o=var
    fname_v="t2m"
  elif var=="gust":
    diag_o="gst"
    fname_v="gust10m"
  elif var=="spd":
    diag_o="uv"
    fname_v="spd10m"
  elif var=="u":
    diag_o="uv"
    fname_v="u10m"    
  elif var=="v":
    diag_o="uv"
    fname_v="v10m"    
  elif var=="mxtm":
    diag_o="mxt"
    fname_v=var
  elif var=="mitm":
    diag_o="mit"
    fname_v="mitm"
  elif var=="ps":
    diag_o=var
    fname_v='sfcp'
  elif var=="howv":
    diag_o="hwv"
    fname_v="howv"
  elif var=="q":
    diag_o=var
    fname_v="spfh"
  else:
    diag_o=var
    fname_v=var

  return diag_o, fname_v

def do_obs(diagdat,var,domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m):

   # Get the variable name used in the 1) diag file and 2) the output file names

   diag_otype,fname_var=get_diag_otype_fname_var(var)

   if diag_otype=='uv' and fname_var=="spd10m":
     dat=diagdat[diagdat['OBTYPE']==diag_otype]
     u=dat['UOB'].values
     v=dat['VOB'].values
     ob=np.sqrt(u*u+v*v)
   elif diag_otype=='uv' and fname_var=="u10m":
     dat=diagdat[diagdat['OBTYPE']==diag_otype]
     ob=dat['UOB'].values
   elif diag_otype=='uv' and fname_var=="v10m":
     dat=diagdat[diagdat['OBTYPE']==diag_otype]
     ob=dat['VOB'].values
   else:
     dat=diagdat[diagdat['OBTYPE']==diag_otype].dropna(axis=1,how='all') 
     ob=dat['UOB'].values

   # Get array of use/toss info
   iuse=dat['IUSE'].values
   assim_ind,=np.where(iuse==1)
   rejected_ind,=np.where(iuse==-1)
   lats=dat['LAT'].values
   lons=dat['LON'].values
   lw=None
   myformat=None #default text formatting for colorbar labels
   extend='both'
   if var == "vis":
     my_usedobs_title=domid+' Assim/Rejected Visibility Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Visibility Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob*0.000621371
     vmin=0.5
     vmax=5.
     clevs = [0.5,1.,3.,5.]
     colors = ['#a569bd','#148f77','#5dade2','#7f8c8d']
     cm=matplotlib.colors.ListedColormap(colors)
     cm.set_under('#f1948a')
     cm.set_over('white')
     units='mi'
     extend='both'
     lw=0.25
   elif var=="q":
     my_usedobs_title=domid+' Assim/Rejected Specific Humidity Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Specific Humidity Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob # already in g/kg from the diag file.  no need to convert here 
     clevs = np.arange(1.,18.5,0.5)
     cm=ncepy.cmap_q2m()
     vmin=clevs[0]
     vmax=clevs[-1]
     units='g/kg'
     extend='both'
     lw=0.25
   elif var == "gust":
     my_usedobs_title=domid+' Assim/Rejected 10 m Wind Gust Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' 10 m Wind Gust Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(2.,52.,2.)
     ob=ncepy.ms2kts(ob)
     vmin=2
     vmax=50
     cm = plt.get_cmap(name='Paired')
     units='kts'
     lw=.15
   elif var == "spd":
     my_usedobs_title=domid+' Assim/Rejected 10 m Wind Speed Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' 10 m Wind Speed Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(2.,52.,2.)
     ob=ncepy.ms2kts(ob)
     vmin=2.
     vmax=50.
     cm = plt.get_cmap(name='Paired')
     units='kts'
     lw=.15
   elif var == "u":
     my_usedobs_title=domid+' Assim/Rejected 10 m U Wind Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' 10 m U Wind Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(-20.,22.,2.)
     ob=ncepy.ms2kts(ob)
     vmin=-20.
     vmax=20.
     cm = plt.get_cmap(name='RdBu')
     units='kts'
     lw=.15
   elif var == "v":
     my_usedobs_title=domid+' Assim/Rejected 10 m V Wind Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' 10 m V Wind Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(-20.,22.,2.)
     ob=ncepy.ms2kts(ob)
     vmin=-20.
     vmax=20.
     cm = plt.get_cmap(name='RdBu')
     units='kts'
     lw=.15
   elif var == "t":
     my_usedobs_title=domid+' Assim/Rejected 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(-36.,104.,4)
     ob=ncepy.Kelvin2F(ob)
     vmin=-36.
     vmax=100.
     cm = ncepy.ncl_t2m()
     units='F'
     myformat='%.0f'
   elif var == "mxtm":
     my_usedobs_title=domid+' Assim/Rejected Max 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Max 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(-36.,104.,4)
     ob=ncepy.Kelvin2F(ob)
     vmin=-36.
     vmax=100.
     cm = ncepy.ncl_t2m()
     units='F'
     myformat='%.0f'
   elif var == "mitm":
     my_usedobs_title=domid+' Assim/Rejected Min 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Min 2 m Temperature Observations\n Valid '+valpdy+' '+valcyc+'Z'
     clevs = np.arange(-36.,104.,4)
     ob=ncepy.Kelvin2F(ob)
     vmin=-36.
     vmax=100.
     cm = ncepy.ncl_t2m()
     units='F'
     myformat='%.0f'
   elif var == "ps":
     my_usedobs_title=domid+' Assim/Rejected Surface Pressure Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Surface Pressure Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob   # Is in hPa already
     vmin=700.
     vmax=1090.
     clevs = np.arange(vmin,vmax+10.,10.) #np.arange(700,1100.,10.)  Set to none here to not overwhelm the tickmark routine
     cm=plt.get_cmap(name='RdBu')
     units='hPa'
   elif var == "tcamt":
     my_usedobs_title=domid+' Assim/Rejected Total Cloud Amount Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Total Cloud Amount Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob
     units='%'
     vmin=0.
     vmax=99.
     clevs = [0.,25.,50.,75.,99.]
     cm=matplotlib.colors.ListedColormap(['white','darkred','gold','darkgreen'])
     cm.set_over('darkblue')
     fmt='%.1f'
     extend='max'
     lw=0.15
   elif var == "cei":
     my_usedobs_title=domid+' Assim/Rejected Ceiling Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Ceiling Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob*0.0328084 # convert to hundreds of ft
     units='Hundreds of feet (AGL)'
     vmin=4.
     vmax=30.
     clevs = [4.,8.,10.,15.,30.]
     colors = ['#a569bd','#148f77','#5dade2','#7f8c8d']
     cm=matplotlib.colors.ListedColormap(colors)
     cm.set_under('#f1948a')
     cm.set_over('white')
     fmt='%.0f'
     extend='both'
     lw=.25
   elif var=='howv':
     my_usedobs_title=domid+' Assim/Rejected Hs Observations\n Valid '+valpdy+' '+valcyc+'Z'
     myobs_title=domid+' Hs Observations\n Valid '+valpdy+' '+valcyc+'Z'
     ob=ob*3.28084 #convert to feet
     vmin=0.
     vmax=86.
     units='ft'
     clevs=np.arange(vmin,vmax+3.,3.)
     fmt='%.0f'
     cm = ncepy.howv_cmap()
     extend='max'
     lw=.15
   else:
     sys.exit("Error!  type: "+var+" is not supported!  Exit!")

   # Setup file names 
   my_usedobs_fig='./usedobs_'+fname_var+'_'+dom+'_'+domid
   myobs_fig='./obs_'+fname_var+'_'+dom+'_'+domid


   #Plot the points via the scatter plot function
   circ_size=15
   if domid == 'CONUS' or domid=='AK': circ_size=0.75
   if domid=='SAK' or domid=='NAK' or domid=='SWAK' or domid=='SEAK': circ_size=25
   # enforce a discrete color bar by normalizing to clevs
   # define the bins and normalize
   if lw is not None:
     thislw=lw
   else:
     thislw=0.
   # ----- Plot all the obs (rejected and assimilated)
   norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)
   sc=m.scatter(lons,lats,circ_size,c=ob,cmap=cm,norm=norm,latlon=True,vmin=vmin,vmax=vmax,lw=thislw)
   sc.set_clim(vmin,vmax)
   # Do not plot individual clevs for ps, there are too many
   if var == "ps": clevs=None

   # plot the color bar
   cbar = m.colorbar(sc,location='bottom',pad="5%",format=myformat,extend=extend)
   cbar.ax.tick_params(labelsize=8.5)
   #plot the units label for the color bar
   cbar.set_label(units)
   plt.title(myobs_title)
   writefig(myobs_fig)

   #Clear off the plottables!
   ncepy.clear_plotables(ax,keep_ax_lst,fig)

   # ----- Plot locations of rejected and assimilated obs as red and blue circles (respectively)
   # - Rejected
   try:
     m.scatter(lons[rejected_ind],lats[rejected_ind],circ_size,c='r',latlon=True,lw=0,alpha=0.5,label='Rejected')
   except:
#     print 'Unable to plot rejected obs for ',domid,' probably because there are not enough obs and basemap is trying to run shiftdata'
#     print 'Number of rejected obs: ',np.size(lons[rejected_ind])
#     print lons[rejected_ind],lats[rejected_ind]
#     print 'Padding with an erroneous point at 0W and 90S'
     rejlons=lons[rejected_ind]
     rejlats=lats[rejected_ind]
     rejlons=np.append(rejlons,[0.0,0.0,0.0])
     rejlats=np.append(rejlats,[-90.0,-90.0,-90.0])
#     print rejlons,rejlats
     m.scatter(rejlons,rejlats,circ_size,c='r',latlon=True,lw=0,alpha=0.5,label='Rejected')
   # - Assimilated
   try:
     m.scatter(lons[assim_ind],lats[assim_ind],circ_size,c='b',latlon=True,lw=0,alpha=0.5,label='Assimilated')
   except:
#     print 'Unable to plot assimilated obs for ',domid,' probably because there are not enough obs and basemap is trying to run shiftdata'
#     print 'Number of assimilated obs: ',np.size(lons[assim_ind])
#     print 'Padding with an erroneous point at 0W and 90S'
     assimlons=lons[assim_ind]
     assimlats=lats[assim_ind]
     assimlons=np.append(assimlons,[0.0,0.0,0.0])
     assimlats=np.append(assimlats,[-90.0,-90.0,-90.0])


   ax.annotate('Assimilated',xy=(0,-0.05),xycoords='axes fraction',color='b', fontsize=12,weight='bold',
               ha='left', va='center')
   ax.annotate('Rejected',xy=(1,-0.05),xycoords='axes fraction', color='r', fontsize=12,weight='bold',
               ha='right', va='center')
   plt.title(my_usedobs_title)
   writefig(my_usedobs_fig)



def shade_incs(fld,type,domid,lats,lons,dom,m):
  if type=='U' or type=='V' or type == 'SPD' or type == 'GUST':
    units='kts'
    vmin=-10.0
    vmax=10.0
    clevs=np.arange(vmin,vmax+0.5,0.5)
    fmt='%.1f'
  elif type=='DIR':
    units='deg'
    vmin=-180.0
    vmax=180.0
    clevs=np.arange(vmin,vmax+5,5.0)
    fmt='%.0f'
  elif type=='SFCP':
    units='hPa'
    vmin=-4.0
    vmax=4.0
    clevs=np.arange(vmin,vmax+0.20,0.20)
    fmt='%.1f'
  elif type=='T' or type=='MXTM' or type=='MITM':
    units='F'
    vmin=-8.0
    vmax=8.0
    clevs=np.arange(vmin,vmax+0.25,0.25)
    fmt='%.1f'
  elif type =='DPT':
    units='F'
    vmin=-8.0
    vmax=8.0
    clevs=np.arange(vmin,vmax+0.25,0.25)
    fmt='%.2f'
  elif type=='Q':
    units='g/kg'
    vmin=-2.0
    vmax=2.0
    clevs=np.arange(vmin,vmax+0.1,0.1)
    fmt='%.1f'
  elif type=='VIS':
    units='mi'
    vmin=-8.0
    vmax=8
    clevs=np.arange(vmin,vmax+0.25,0.25)
    fmt='%.1f'
  elif type=='TCAMT':
    units='%'
    vmin=-100.0
    vmax=100
    clevs=np.arange(vmin,vmax+10.,10.)
    fmt='%.1f'
  elif  type=='CEILING':
    units='Hundreds of feet (AGL)'
    vmin=-100.0
    vmax=100.0
    clevs=np.arange(vmin,vmax+5.,5.)
    fmt='%.0f'
  elif type=='HOWV':
    units='ft'
    vmin=-1.5
    vmax=1.5
    clevs=np.arange(vmin,vmax+.25,.25)
    fmt='%.2f'
  else:
    sys.exit("Error!  type: "+type+" is not supported!  Exit!")

  cm = plt.get_cmap('RdBu_r')
  cs = m.contourf(lons,lats,fld,clevs,cmap=cm,latlon=True,extend='both')
  cbar = m.colorbar(cs,location='bottom',pad="5%")
  cbar.set_label(units)
  if 1 == 0 : #Skip plotting the contours.  Gets too cluttered too easily
#  if dom != 'CONUS':
    matplotlib.rcParams['contour.negative_linestyle'] = 'solid'
    cs1 = m.contour(lons,lats,fld,clevs,colors='k',linewidths=1.0,latlon=True)
    clab = plt.clabel(cs1,fontsize=10,fmt=fmt)

def plot_all(dom):
  ###################################################
  #       START PLOTTING FOR EACH DOMAIN            #
  ###################################################
    t1dom=time.clock()
    print('Working on '+dom)

    # create figure and axes instances
    fig = plt.figure(figsize=(8,10))
    ax = fig.add_axes([0.1,0.1,0.8,0.8])

    # create basemap instance and set the dimensions
    llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom)

    # Reset map resolution here to something higher res (unless it is conus or is already hi-res)
    if dom != 'CONUS' and res != 'h' and res != 'f' : res='i'

    if gribproj.lower()=='lambert':
      llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj='lcc')
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='lcc',\
                lat_1=Lat1,lat_2=Lat2,lat_0=Lat0,lon_0=Lon0,ax=ax)
    elif gribproj.lower()=='polar_stereographic':
      # create basemap instance and set the dimensions
      llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj='stere')
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='stere',\
                lat_ts=Lat_ts,lat_0=Lat0,lon_0=Lon0,ax=ax)
    elif gribproj.lower()=='mercator':
      # Grib grid projection info
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='merc',\
                lat_ts=Lat_ts,lon_0=Lon0,ax=ax)
    else:
      sys.exit('BAD PROJECTION SELECTION! EXIT!')


    m.drawcoastlines(linewidth=1.25)
    m.drawstates(linewidth=1.25)
    m.drawcountries(linewidth=1.25)

    if dom != 'CONUS' and dom != 'AK' :
      # Draw the the counties if not CONUS
      # Note that drawing the counties can slow things down!
      m.drawcounties(linewidth=0.2, color='k')
      skip=22
      veclength=5
      if dom=='SAK' or dom=='NAK':
        skip=15
        veclength=4.5
      if dom=='SWAK' or dom=='SEAK':
        skip=10
        veclength=4.5
      if dom=='HI':
        skip=10
        veclength=4.5
      if dom=='PR':
        skip=5
        veclength=4.5
      if dom=='GUAM':
        skip=6
        veclength=4.5
    else:
      skip=43
      veclength=5


    #  Map/figure has been set up here (bulk of the work), save axes instances for
    #     use again later
    keep_ax_lst = ax.get_children()[:]

    t1=time.clock()
    # u 10m incs
    shade_incs(inc_u10,'U',domid,lats,lons,dom,m)
    plt.title(domid+' 10 m U Wind Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_u10m_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs u10m for: "+dom) % t3
    #Doing the obs now!
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    do_obs(diagdat,'u',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    # v 10m incs
    t1=time.clock()
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    shade_incs(inc_v10,'V',domid,lats,lons,dom,m)
    plt.title(domid+' 10 m V Wind Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_v10m_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs v10m for: "+dom) % t3
    #Doing the obs now!
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    do_obs(diagdat,'v',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)


    if tcamt is not None:
      # tcamt incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_tcamt,'TCAMT',domid,lats,lons,dom,m)
      plt.title(domid+' Total Cloud Amount Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_tcamt_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs tcamt for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'tcamt',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    if ceiling is not None:
      # ceiling incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_ceiling,'CEILING',domid,lats,lons,dom,m)
      plt.title(domid+' Ceiling Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_ceiling_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs ceiling for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'cei',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    if howv is not None:
      # howv incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_howv,'HOWV',domid,lats,lons,dom,m)
      plt.title(domid+' Hs Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_howv_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs howv for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'howv',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)


    # spd 10m incs
    t1=time.clock()
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    shade_incs(inc_spd10m,'SPD',domid,lats,lons,dom,m)
    plt.title(domid+' 10 m Wind Speed Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_spd10m_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs spd10m for: "+dom) % t3
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    do_obs(diagdat,'spd',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    if gust is not None:
      # gust incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_gust,'GUST',domid,lats,lons,dom,m)
      plt.title(domid+' 10 m Wind Gust Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_gust10m_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs gust for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'gust',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)



    #Skip wind direction for now - difficult to come up with meaningful
    # halfway-decent looking increment plots in complexe terrain
    if 1 == 0 :
      # wdir10m incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_wdir10m,'DIR',domid,lats,lons,dom,m)
      plt.title(domid+' 10 m Wind Direction Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_wdir10m_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs wdir10m for: "+dom) % t3

    # sfcp incs
    t1=time.clock()
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    shade_incs(inc_sfcp,'SFCP',domid,lats,lons,dom,m)
    plt.title(domid+' Surface Pressure Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_sfcp_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs sfcp for: "+dom) % t3
    #Doing the obs now!
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    do_obs(diagdat,'ps',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    if vis is not None:
      # vis incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_vis,'VIS',domid,lats,lons,dom,m)
      plt.title(domid+' Visibiliy Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_vis_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs vis for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'vis',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)


    # t2m incs
    t1=time.clock()
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    shade_incs(inc_t2m,'T',domid,lats,lons,dom,m)
    plt.title(domid+' 2 m Temperature Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_t2m_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs t2m for: "+dom) % t3
    #Doing the obs now!
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    do_obs(diagdat,'t',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)


    if mxtm is not None:
      # mxtm incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_mxtm,'MXTM',domid,lats,lons,dom,m)
      plt.title(domid+' Max 2 m Temperature Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_mxtm_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot mxtm incs for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'mxtm',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)

    if mitm is not None:
      # mitm incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_mitm,'MITM',domid,lats,lons,dom,m)
      plt.title(domid+' Min 2 m Temperature Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_mitm_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot mitm incs for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'mitm',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)


    # td2m incs
    t1=time.clock()
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    shade_incs(inc_td2m,'DPT',domid,lats,lons,dom,m)
    plt.title(domid+' 2 m Dewpoint Temperature Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
    writefig('./inc_td2m_'+dom+'_'+domid)
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot incs td2m for: "+dom) % t3

    if spfh is not None:
      # spfh incs
      t1=time.clock()
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      shade_incs(inc_spfh,'Q',domid,lats,lons,dom,m)
      plt.title(domid+' Specific Humidity Analysis Increments\n Valid '+valpdy+' '+valcyc+'Z')
      writefig('./inc_spfh_'+dom+'_'+domid)
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot incs spfh for: "+dom) % t3
      #Doing the obs now!
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      do_obs(diagdat,'q',domid,dom,valpdy,valcyc,ax,keep_ax_lst,fig,m)



    t3dom=round(t2-t1dom, 3)
    print("%.3f seconds to plot ALL for: "+dom) % t3dom
    plt.clf()


if __name__ == '__main__':

  #Necessary to generate figs when not running an Xserver (e.g. via PBS)
  plt.switch_backend('agg')
  t1a = time.clock()
  print("Starting...")

  if len(sys.argv) < 5:
    sys.exit('Usage:python %s diagfile anl_grib ges_grib Name' % sys.argv[0])


  # Read input dir location, grib file, and domain id (e.g. exp name) from command line
  diagfile=sys.argv[1]
  anl_grib=sys.argv[2]
  ges_grib=sys.argv[3]
  domid=sys.argv[4]
  READDIAG=sys.argv[5]


  diagdat=read_diagconv(diagfile)


  anl= pygrib.open(anl_grib)

  #####################################################################
  #                 READ IN COMMON FIELDS                             #
  #####################################################################

  # Get the lats and lons
  lats, lons = anl[1].latlons()

  gribproj=anl[1]['gridType']
  if gribproj.lower()=='lambert':
    # Everything below was lifted directly from pygrib.pyx
    Lon0=anl[1]['LoVInDegrees']
    Lat0=anl[1]['LaDInDegrees']
    Lat1=anl[1]['Latin1InDegrees']
    Lat2=anl[1]['Latin2InDegrees']
  elif gribproj.lower()=='polar_stereographic':
    # Everything below was lifted directly from pygrib.pyx
    Lon0=anl[1]['orientationOfTheGridInDegrees']
    Lat_ts=anl[1]['latitudeWhereDxAndDyAreSpecifiedInDegrees']
    if anl[1].has_key('projectionCentreFlag'):
      projcenterflag = anl[1]['projectionCentreFlag']
    elif anl[1].has_key('projectionCenterFlag'):
      projcenterflag = anl[1]['projectionCenterFlag']
    if projcenterflag == 0:
      Lat0=90.
    else:
      Lat0=-90.
  elif gribproj.lower()=='mercator':
    scale = float(anl[1]['grib2divider'])
    lon1 = anl[1]['longitudeOfFirstGridPoint']/scale
    if anl[1]['truncateDegrees']:
      lon1 = int(lon1)
    lon2 = anl[1]['longitudeOfLastGridPoint']/scale
    if anl[1]['truncateDegrees']:
      lon2 = int(lon2)
    Lat_ts=anl[1]['LaD']/scale
    Lon0=0.5*(lon1+lon2)


#  print 'MY PROJECTION IS: ',gribproj

  rearth=anl[1]['radius']

  try:
    dx=anl[1]['DxInMetres']/1000.
  except:
    dx=anl[1]['DiInMetres']/1000.

  try:
    nx=anl[1]['Nx']
  except:
    nx=anl[1]['Ni']

  try:
    ny=anl[1]['Ny']
  except:
    ny=anl[1]['Nj']

  #Make sure lons are west oriented
  if Lon0 > 0:
    Lon0=Lon0-360
  lons=np.where(lons>0.,lons-360.,lons)
  
  domid=domid.upper()
  # Specify some plotting domains which have the regions pre-set in ncepy
  if (domid.strip() == 'RTMA' or domid.strip() == 'RTMAP' or domid.strip() == 'URMA' or
     domid.strip() == 'URMAP' or domid.strip() == 'RTMA2P5_RU' or domid.strip() == 'RTMA2P5_RUP'):
    domains=['CONUS','NW','NWRFC','NC','NE','SW','SC','SE','Great_Lakes','MIDATL']
  if domid.strip() == 'AKRTMA' or domid.strip() == 'AKRTMAP' or domid.strip() == 'AKURMA' or domid.strip() == 'AKURMAP':
    domains=['AK','NAK','SAK','SWAK','SEAK']
  if domid.strip() == 'PRRTMA' or domid.strip() == 'PRRTMAP' or domid.strip() == 'PRURMA' or domid.strip() == 'PRURMAP':
    domains=['PR']
  if domid.strip() == 'GURTMA' or domid.strip() == 'GURTMAP' or domid.strip() == 'GUURMA' or domid.strip() == 'GUURMAP':
    domains=['GUAM']
  if domid.strip() == 'HIRTMA' or domid.strip() == 'HIRTMAP' or domid.strip() == 'HIURMA' or domid.strip() == 'HIURMAP':
    domains=['HI']

  #Orography (meters)  # Hopefully this does not change during the analysis!
  #  orog = anl.select(name='Orography',typeOfLevel='surface')[0].values

  #####################################################################
  #                 READ IN ANL FIELDS                                #
  #####################################################################

  # read 10m agl winds (ms), THESE ARE ASSUMED TO BE EARTH RELATIVE BASED ON PRE-PROCESSING STEP WITH WGRIB2
  u10e  = anl.select(name='10 metre U wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  v10e  = anl.select(name='10 metre V wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  u10 = ncepy.ms2kts(u10e)
  v10 = ncepy.ms2kts(v10e)

  # 2mT
  t2mK = anl.select(name='2 metre temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  t2m=ncepy.Kelvin2F(t2mK) #convert to F

  #mxtm
  try:
    mxtmK=anl.select(name='Maximum temperature',typeOfLevel='heightAboveGround',level=2)[0].values
    mxtm=ncepy.Kelvin2F(mxtmK) #convert to F
  except:
    mxtm=None

  #mitm
  try:
    mitmK=anl.select(name='Minimum temperature',typeOfLevel='heightAboveGround',level=2)[0].values
    mitm=ncepy.Kelvin2F(mitmK) #convert to F
  except:
    mitm=None

  #2mTd
  td2mK=anl.select(name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  td2m=ncepy.Kelvin2F(td2mK) #convert to F

  # 10m agl wind speed
  spd10mms  = anl.select(name='10 metre wind speed',typeOfLevel='heightAboveGround',level=10)[0].values
  spd10m = ncepy.ms2kts(spd10mms)
  wdir10m = anl.select(parameterName='Wind direction [from which blowing]',typeOfLevel='heightAboveGround',level=10)[0].values #deg

  #wind gust
  try:
    gustms = anl.select(name='Wind speed (gust)',typeOfLevel='heightAboveGround',level=10)[0].values
    gust = ncepy.ms2kts(gustms)
  except:
    gust=None

  #visibility (meters)
  try:
    vis = anl.select(name='Visibility')[0].values
  except:
    vis=None

  #tcamt
  try:
    tcamt=anl.select(name='Total Cloud Cover')[0].values
  except:
    tcamt=None

  #ceiling
  try:
    ceiling=anl.select(name='Ceiling')[0].values*0.0328084 # convert to hundreds of ft (AGL)
  except:
    ceiling=None

  #howv
  try:
    howv=anl.select(name='Significant height of combined wind waves and swell')[0].values*3.28084 # feet
    # mask out the land points by inverting the ocean mask
    #howv=maskoceans(lons, lats, howv, inlands=False, resolution='h')
    #howv.mask=np.logical_not(howv.mask)
  except:
    howv=None



  #sfcp
  sfcp = (anl.select(name='Surface pressure',typeOfLevel='surface')[0].values)/100.0 # convert to hPa

  #spfh  -Skip this for now
#  spfh=None
  try:
    spfh = (anl.select(name='Specific humidity',typeOfLevel='heightAboveGround',level=2)[0].values)*1000.0 # Convert to g/kg
  except:
    spfh=None

  #####################################################################
  #                 READ IN GES FIELDS                                #
  #####################################################################

  # Open input grib file but make sure it is readable and it exists - otherwise wait 10s and try again
  # do this a maximum of 30 times.

  ges = pygrib.open(ges_grib)

  # read 10m agl winds (ms), THESE ARE ASSUMED TO BE EARTH RELATIVE BASED ON PRE-PROCESSING STEP WITH WGRIB2
  ges_u10e  = ges.select(name='10 metre U wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  ges_v10e  = ges.select(name='10 metre V wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  ges_u10 = ncepy.ms2kts(ges_u10e)
  ges_v10 = ncepy.ms2kts(ges_v10e)

  # 2mT
  ges_t2mK = ges.select(name='2 metre temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  ges_t2m=ncepy.Kelvin2F(ges_t2mK) #convert to F

  #mxtm
  if mxtm is not None:
    ges_mxtmK=ges.select(name='Maximum temperature',typeOfLevel='heightAboveGround',level=2)[0].values
    ges_mxtm=ncepy.Kelvin2F(ges_mxtmK) #convert to F

  #mitm
  if mitm is not None:
    ges_mitmK=ges.select(name='Minimum temperature',typeOfLevel='heightAboveGround',level=2)[0].values
    ges_mitm=ncepy.Kelvin2F(ges_mitmK) #convert to F

  # 2mTd
  ges_td2mK=ges.select(name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  ges_td2m=ncepy.Kelvin2F(ges_td2mK) #convert to F
  # 10m agl wind speed
  ges_spd10mms  = ges.select(name='10 metre wind speed',typeOfLevel='heightAboveGround',level=10)[0].values
  ges_spd10m = ncepy.ms2kts(ges_spd10mms)
  ges_wdir10m = ges.select(parameterName='Wind direction [from which blowing]',typeOfLevel='heightAboveGround',level=10)[0].values #deg

  #wind gust
  if gust is not None:
    ges_gustms = ges.select(name='Wind speed (gust)',typeOfLevel='heightAboveGround',level=10)[0].values
    ges_gust = ncepy.ms2kts(ges_gustms)

  #visibility (meters)
  if vis is not None: ges_vis = ges.select(name='Visibility')[0].values

  #sfcp
  ges_sfcp = (ges.select(name='Surface pressure',typeOfLevel='surface')[0].values)/100.0 # convert to hPa

  #spfh
  if spfh is not None: ges_spfh = (ges.select(name='Specific humidity',typeOfLevel='heightAboveGround',level=2)[0].values)*1000.0 # Convert to g/kg

  #tcamt
  if tcamt is not None: ges_tcamt = ges.select(name='Total Cloud Cover')[0].values

  if ceiling is not None: ges_ceiling = ges.select(name='Ceiling')[0].values*0.0328084 # convert to hundreds of ft (AGL)

  #howv
  if howv is not None:
    ges_howv = ges.select(name='Significant height of combined wind waves and swell')[0].values*3.28084 # feet
    # mask out the land points by inverting the ocean mask
    #ges_howv=maskoceans(lons, lats, ges_howv, inlands=False, resolution='h')
    #ges_howv.mask=np.logical_not(ges_howv.mask)

  # Calculate the increments

  inc_u10=u10-ges_u10 #kts
  inc_v10=v10-ges_v10 #kts
  inc_spd10m=spd10m-ges_spd10m  #kts
  inc_wdir10m=wdir10m-ges_wdir10m  #deg
  if gust is not None: inc_gust=gust-ges_gust   #kts
  if vis is not None:  inc_vis=(vis-ges_vis)*0.000621371      # (mi)
  inc_t2m=t2m-ges_t2m      # F
  if mxtm is not None: inc_mxtm=mxtm-ges_mxtm  #F
  if mitm is not None: inc_mitm=mitm-ges_mitm  #F
  inc_td2m=td2m-ges_td2m   # F
  inc_sfcp=sfcp-ges_sfcp   #hPa
  if spfh is not None: inc_spfh=spfh-ges_spfh   #g/kg
  if tcamt is not None: inc_tcamt=tcamt-ges_tcamt #%
  if ceiling is not None: inc_ceiling=ceiling-ges_ceiling # ft
  if howv is not None: inc_howv=howv-ges_howv # feet

  t2a=time.clock()
  t3a=round(t2a-t1a, 3)
  print("%.3f seconds to read all gribs msgs and calc incs!")  % t3a


  # Set up date and time stuff

  #Are we in minutes or hours - get the info from the ges grib file
  minsorhours=ges[1]['stepUnits']  #1 in hrs, 0 then minutes
  ftime=str(ges[1]['stepRange']) # Forecast hour

  #Get the date/time and forecast hour
  if minsorhours==1 and int(ftime) < 10:
    ftime=str(ftime).zfill(2)

  #Get cycle time and pad with a zero and convert to a string
  cyctime=anl[1].dataTime #Cycle (e.g. 1200 UTC)
  grbtime=str(cyctime).zfill(4)

  #Get the PDY from the anl
  anldate=str(anl[1].dataDate)+grbtime

  # format valid date time and find the forecast init time as well
  #
  vdateobj=datetime.strptime(anldate,'%Y%m%d%H%M')
  if minsorhours==1:
    initdateobj=vdateobj+timedelta(hours=-int(ftime))
    valcyc=vdateobj.strftime('%H')
  else:
    initdateobj=vdateobj+timedelta(minutes=-int(ftime))
    valcyc=vdateobj.strftime('%H%M')
  valpdy=vdateobj.strftime('%Y%m%d')
  initdate=initdateobj.strftime('%Y%m%d%H%M')

  ###################################################
  #       START PLOTTING FOR EACH DOMAIN            #
  ###################################################
  main()
  t3all=round(time.clock()-t1a,3)
  print("%.3f seconds to run %s") % (t3all, sys.argv[0])

