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
import time,os,sys,multiprocessing
import ncepy
from scipy import ndimage
from netCDF4 import Dataset

def clear_plotables(ax,keep_ax_lst,fig):
  #### - step to clear off old plotables but leave the map info - ####
  if len(keep_ax_lst) == 0:
    print("clear_plotables WARNING keep_ax_lst has length 0. Clearing ALL plottables including map info!")
  cur_ax_children = ax.get_children()[:]
  if len(cur_ax_children) > 0:
    for a in cur_ax_children:
      if a not in keep_ax_lst:
       # if thr artist isn't part of the initial set up, remove it
        a.remove()

def compress_and_save(filename):
  #### - compress and save the image - ####
#  ram = io.StringIO()
#  ram = io.BytesIO()
#  plt.savefig(ram, format='png', bbox_inches='tight', dpi=150)
  plt.savefig(filename, format='png', bbox_inches='tight', dpi=300)
#  ram.seek(0)
#  im = Image.open(ram)
#  im2 = im.convert('RGB').convert('P', palette=Image.ADAPTIVE)
#  im2.save(filename, format='PNG')


#lcdchex=["#E65956", "#D93B3A", "#D22C2C", "#CC1E1E", "darkred"]
lcdchex=["#E65956", "#D93B3A", "#D22C2C", "firebrick", "darkred"]
mcdchex=["#5EE240", "#4DC534", "#3DA828", "#2D8B1C", "#1D6F11"]
hcdchex=["#64B3E8", "#5197D7", "#3E7CC6", "#2B60B5", "#1945A4"]

ymdhm = str(sys.argv[1])

ymd=ymdhm[0:8]
year=int(ymdhm[0:4])
month=int(ymdhm[4:6])
day=int(ymdhm[6:8])
hour=int(ymdhm[8:10])
minute=int(ymdhm[10:12])
cyc = str(hour).zfill(2)
subcyc = str(minute).zfill(2)
vtime = ymdhm
print(year, month , day, hour, minute)

im = image.imread('/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/python.fv3/noaa.png')
#dom="conus"
dom="custom"
# Map corners for each domain
if dom == 'conus':
  llcrnrlon = -120.5
  llcrnrlat = 21.0
  urcrnrlon = -64.5
  urcrnrlat = 49.0
  lat_0 = 35.4
  lon_0 = -97.6
  xscale=0.15
  yscale=0.2
elif dom == 'BN':
  llcrnrlon = -75.75
  llcrnrlat = 40.0
  urcrnrlon = -69.5
  urcrnrlat = 43.0
  lat_0 = 41.0
  lon_0 = -74.6
  xscale=0.14
  yscale=0.19
elif dom == 'CE':
  llcrnrlon = -103.0
  llcrnrlat = 32.5
  urcrnrlon = -88.5
  urcrnrlat = 41.5
  lat_0 = 35.0
  lon_0 = -97.0
  xscale=0.15
  yscale=0.18
elif dom == 'CO':
  llcrnrlon = -110.5
  llcrnrlat = 35.0
  urcrnrlon = -100.5
  urcrnrlat = 42.0
  lat_0 = 38.0
  lon_0 = -105.0
  xscale=0.17
  yscale=0.18
elif dom == 'LA':
  llcrnrlon = -121.0
  llcrnrlat = 32.0
  urcrnrlon = -114.0
  urcrnrlat = 37.0
  lat_0 = 34.0
  lon_0 = -114.0
  xscale=0.16
  yscale=0.18
elif dom == 'MA':
  llcrnrlon = -82.0
  llcrnrlat = 36.5
  urcrnrlon = -73.5
  urcrnrlat = 42.0
  lat_0 = 37.5
  lon_0 = -80.0
  xscale = 0.18
  yscale = 0.18
elif dom == 'NC':
  llcrnrlon = -111.0
  llcrnrlat = 39.0
  urcrnrlon = -93.5
  urcrnrlat = 49.0
  lat_0 = 44.5
  lon_0 = -102.0
  xscale=0.16
  yscale=0.18
elif dom == 'NE':
  llcrnrlon = -80.0  
  llcrnrlat = 40.5
  urcrnrlon = -66.0
  urcrnrlat = 47.5
  lat_0 = 42.0
  lon_0 = -80.0
  xscale=0.16
  yscale=0.18
elif dom == 'NW':
  llcrnrlon = -125.5 
  llcrnrlat = 40.5
  urcrnrlon = -109.0
  urcrnrlat = 49.5
  lat_0 = 44.0
  lon_0 = -116.0
  xscale=0.15
  yscale=0.18
elif dom == 'OV':
  llcrnrlon = -91.5 
  llcrnrlat = 34.75
  urcrnrlon = -80.0
  urcrnrlat = 43.0
  lat_0 = 38.0
  lon_0 = -87.0      
  xscale=0.18
  yscale=0.17
elif dom == 'SC':
  llcrnrlon = -108.0 
  llcrnrlat = 25.0
  urcrnrlon = -88.0
  urcrnrlat = 37.0
  lat_0 = 32.0
  lon_0 = -98.0      
  xscale=0.14
  yscale=0.18
elif dom == 'SE':
  llcrnrlon = -91.5 
  llcrnrlat = 24.0
  urcrnrlon = -74.0
  urcrnrlat = 36.5
  lat_0 = 34.0
  lon_0 = -85.0
  xscale = 0.14
  yscale = 0.18
elif dom == 'SF':
  llcrnrlon = -123.25 
  llcrnrlat = 37.25
  urcrnrlon = -121.25
  urcrnrlat = 38.5
  lat_0 = 37.5
  lon_0 = -121.0
  xscale=0.16
  yscale=0.19
elif dom == 'SP':
  llcrnrlon = -125.0
  llcrnrlat = 45.0
  urcrnrlon = -119.5
  urcrnrlat = 49.2
  lat_0 = 46.0
  lon_0 = -120.0
  xscale=0.19
  yscale=0.18
elif dom == 'SW':
  llcrnrlon = -125.0 
  llcrnrlat = 30.0
  urcrnrlon = -108.0
  urcrnrlat = 42.5
  lat_0 = 37.0
  lon_0 = -113.0
  xscale=0.17
  yscale=0.18
elif dom == 'UM':
  llcrnrlon = -96.75 
  llcrnrlat = 39.75
  urcrnrlon = -81.0
  urcrnrlat = 49.0
  lat_0 = 44.0
  lon_0 = -91.5
  xscale=0.18
  yscale=0.18
elif dom == 'custom':
  llcrnrlon=-113.0
  llcrnrlat=39.0
  urcrnrlon=-110.0
  urcrnrlat=42.0
  lat_0 = 39.3
  lon_0 = -111.1
  xscale=0.17
  yscale=0.18


#fhours = [0]
#dtime=datetime.datetime(year,month,day,hour,0)
#date_list = [dtime + datetime.timedelta(hours=x) for x in fhours]
#print date_list

# Create the figure
fig = plt.figure()
gs = GridSpec(4,12,wspace=0.0,hspace=0.0)
ax1 = plt.subplot(gs[0:4,0:6])
ax2 = plt.subplot(gs[0:4,6:12])
axes = [ax1, ax2]
par = 1

# Setup map corners for plotting.

for ax in axes:
  if dom == 'BN' or dom == 'LA' or dom == 'SF' or dom == 'SP':
    m = Basemap(ax=ax,projection='stere',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat,urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon,urcrnrlon=urcrnrlon,\
                resolution='h')
  else:
    m = Basemap(ax=ax,projection='stere',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat,urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon,urcrnrlon=urcrnrlon,\
                resolution='l')
  m.fillcontinents(color='LightGrey',zorder=0)
  m.drawstates(linewidth=.5,color='k')
  m.drawcoastlines(linewidth=.75, color='k')
  m.drawcountries(linewidth=.5, color='k')

  # Map/figure has been set up here, save axes instances for use again later
  if par == 1:
    keep_ax_lst_1 = ax.get_children()[:]
  elif par == 2:
    keep_ax_lst_2 = ax.get_children()[:]

  par += 1
par = 1

#for j in range(len(date_list)):
#  ymdhm=dtime.strftime("%Y%m%d%H%M")
#  fhour=date_list[j].strftime("%H")
#  fminute=date_list[j].strftime("%M")

paraind = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_realtime/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs.grib2')

prodind = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_realtime/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs_fgs.grib2')

lcdcprod=prodind.select(parameterName='Low cloud cover')[0].values
mcdcprod=prodind.select(parameterName='Medium cloud cover')[0].values
hcdcprod=prodind.select(parameterName='High cloud cover')[0].values
lcdcpara=paraind.select(parameterName='Low cloud cover')[0].values
mcdcpara=paraind.select(parameterName='Medium cloud cover')[0].values
hcdcpara=paraind.select(parameterName='High cloud cover')[0].values

lats,lons=prodind.select(name='Categorical snow',level=0)[0].latlons()
#  if (int(fhr) == 0):
x,y = m(lons,lats)

clevs=[50,60,70,80,90,100]

  # Clear off old plotables but keep all the map info
#  if (int(fhr) > 0):
clear_plotables(ax1,keep_ax_lst_1,fig)
clear_plotables(ax2,keep_ax_lst_2,fig)

for ax in axes:
  xmin, xmax = ax.get_xlim()
  ymin, ymax = ax.get_ylim()
  xmax = int(round(xmax))
  ymax = int(round(ymax))

if par == 1:
#      intime=dtime.strftime("%m/%d/%Y %H%MZ")
#      vtime=date_list[j].strftime("%m/%d/%Y %H%MZ")
    cshcdc=m.contourf(x,y,hcdcprod,clevs,colors=hcdchex,ax=ax)
    csmcdc=m.contourf(x,y,mcdcprod,clevs,colors=mcdchex,alpha=0.9,ax=ax)
    cslcdc=m.contourf(x,y,lcdcprod,clevs,colors=lcdchex,alpha=0.8,ax=ax)
    ax.text(.5,1.03,'3D-RTMA FGS Cloud Cover (low-red, mid-green, high-blue) \n valid: '+ vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
    ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
elif par == 2:
    cshcdc=m.contourf(x,y,hcdcpara,clevs,colors=hcdchex,ax=ax)
    csmcdc=m.contourf(x,y,mcdcpara,clevs,colors=mcdchex,alpha=0.9,ax=ax)
    cslcdc=m.contourf(x,y,lcdcpara,clevs,colors=lcdchex,alpha=0.8,ax=ax)
    ax.text(.5,1.03,'3D-RTMA ANL Cloud Cover (low-red, mid-green, high-blue) valid: '+ vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
    ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      if (int(fhr) == 0):
    caxlcdc=fig.add_axes([.27,.25,.1,.03])
    cblcdc=fig.colorbar(cslcdc,cax=caxlcdc,ticks=clevs,orientation='horizontal')
    cblcdc.ax.tick_params(labelsize=5)
    cblcdc.ax.set_xticklabels(['50','60','70','80','90','100'])

    caxmcdc=fig.add_axes([.45,.25,.1,.03])
    cbmcdc=fig.colorbar(csmcdc,cax=caxmcdc,ticks=clevs,orientation='horizontal')
    cbmcdc.ax.tick_params(labelsize=5)
    cbmcdc.ax.set_xticklabels(['50','60','70','80','90','100'])

    caxhcdc=fig.add_axes([.63,.25,.1,.03])
    cbhcdc=fig.colorbar(cshcdc,cax=caxhcdc,ticks=clevs,orientation='horizontal')
    cbhcdc.ax.tick_params(labelsize=5)
    cbhcdc.ax.set_xticklabels(['50','60','70','80','90','100'])

#        caxfrzra=fig.add_axes([.63,.2,.1,.03])
#        cbfrzra=fig.colorbar(csfrzra,cax=caxfrzra,ticks=clevs,orientation='horizontal')
#        cbfrzra.ax.tick_params(labelsize=5)
#        cbfrzra.ax.set_xticklabels(['light freezing rain','','','','heavy freezing rain'])

#        caxmix=fig.add_axes([.81,.2,.1,.03])
#        cbmix=fig.colorbar(csmix,cax=caxmix,ticks=clevs,orientation='horizontal')
#        cbmix.ax.tick_params(labelsize=5)
#        cbmix.ax.set_xticklabels(['light mix','','','','heavy mix'])
par += 1
par = 1

compress_and_save('comparecloud_'+dom+'_t'+cyc+subcyc+'z.png')

plt.close()
