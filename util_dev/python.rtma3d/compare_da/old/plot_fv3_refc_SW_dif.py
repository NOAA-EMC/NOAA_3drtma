import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
from mpl_toolkits.basemap import Basemap
import numpy as np
import pygrib, datetime, os, sys, subprocess
from netCDF4 import Dataset

ymdh = str(sys.argv[1])

ymd = ymdh[0:8]
year = int(ymdh[0:4])
month = int(ymdh[4:6])
day = int(ymdh[6:8])
hour = int(ymdh[8:10])
cyc = str(hour).zfill(2)
print year, month, day, hour

outpath='/gpfs/dell2/stmp/Benjamin.Blake/fv3da/00/'
os.system("cd " + outpath)

fhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]
dtime = datetime.datetime(year,month,day,hour,0)
date_list = [dtime + datetime.timedelta(hours=x) for x in fhours]
print date_list
for j in range(len(date_list)):

  fhour = str(fhours[j]).zfill(2)
  fhr = int(fhour)
  print 'fhour '+fhour

  data1 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')
  refc1 = data1.select(name='Maximum/Composite radar reflectivity')[0].values
  data2 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')
  refc2 = data2.select(name='Maximum/Composite radar reflectivity')[0].values
  units = 'dBz'

  lat1,lon1 = data1.select(name='Maximum/Composite radar reflectivity')[0].latlons()
  lat2,lon2 = data2.select(name='Maximum/Composite radar reflectivity')[0].latlons()
  

# Create the figure
  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]
  par = 1

# Setup map corners for plotting.
  llcrnrlon = -125.0
  llcrnrlat = 30.0
  urcrnrlon = -107.0
  urcrnrlat = 44.0

  for ax in axes:
    print par
    m = Basemap(ax=ax,projection='gnom',lat_0=37.0,lon_0=-113.0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=1.0)
    m.drawstates(linewidth=0.75)
    m.drawcountries(linewidth=0.75)
#    parallels = np.arange(0.,90.,10.)
#    m.drawparallels(parallels,labels=[1,0,0,0],fontsize=6)
#    meridians = np.arange(180.,360.,10.)
#    m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=6)
    x1,y1 = m(lon1,lat1)
    x2,y2 = m(lon2,lat2)

    clevs = np.linspace(5,70,14)
    clevsdif = [20,1000]
    colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']
    itime = dtime.strftime("%m/%d/%Y %Hz")
    vtime = date_list[j].strftime("%m/%d/%Y %Hz") 

# Plot SAR w/out DA
    if par == 1:  
      cs_sar1 = m.contourf(x1,y1,refc1,clevs,colors=colorlist,extend='max')
      cs_sar1.cmap.set_over('darkmagenta')
      cbar = m.colorbar(cs_sar1,ax=ax,location='bottom',pad=0.05,ticks=[5,15,25,35,45,55,65])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Composite Reflectivity ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

# Plot SAR w/ DA
    elif par == 2:
      cs_sar2 = m.contourf(x2,y2,refc2,clevs,colors=colorlist,extend='max')
      cs_sar2.cmap.set_over('darkmagenta')
      cbar = m.colorbar(cs_sar2,ax=ax,location='bottom',pad=0.05,ticks=[5,15,25,35,45,55,65])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Composite Reflectivity ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      csdif = m.contourf(x1,y1,refc1,clevsdif,colors='red')
      csdif2 = m.contourf(x2,y2,refc2,clevsdif,colors='dodgerblue')
      ax.text(.5,1.03,'FV3 (red) and FV3-DA (blue) Composite Reflectivity > 20 ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1

#  plt.tight_layout() 
  plt.savefig(outpath+'comparerefc_SW_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()
