import pygrib
import datetime
import numpy as np
import matplotlib
matplotlib.use('Agg')
import cStringIO
import matplotlib.pyplot as plt
from PIL import Image
import matplotlib.image as image
from mpl_toolkits.basemap import Basemap
import subprocess
from matplotlib.gridspec import GridSpec
import sys, os


def clear_plotables(ax,keep_ax_lst,fig):
  #### - step to clear off old plotables but leave the map info - ####
  if len(keep_ax_lst) == 0:
    print "clear_plotables WARNING keep_ax_lst has length 0. Clearing ALL plottables including map info!"
  cur_ax_children = ax.get_children()[:]
  if len(cur_ax_children) > 0:
    for a in cur_ax_children:
      if a not in keep_ax_lst:
       # if thr artist isn't part of the initial set up, remove it
        a.remove()

def compress_and_save(filename):
  #### - compress and save the image - ####
  ram = cStringIO.StringIO()
  plt.savefig(ram, format='png', bbox_inches='tight', dpi=150)
  ram.seek(0)
  im = Image.open(ram)
  im2 = im.convert('RGB').convert('P', palette=Image.ADAPTIVE)
  im2.save(filename, format='PNG')


prodhex=['#F00000','#32B2BD','#32B2BD']
parahex=['#F00000','#C15033','#C15033']
snowhex=["#64B3E8", "#5197D7", "#3E7CC6", "#2B60B5", "#1945A4"]
rainhex=["#5EE240", "#4DC534", "#3DA828", "#2D8B1C", "#1D6F11"]
sleethex=["#947EEC", "#7F62CB", "#6B47AB", "#562B8A", "#42106A"]
freezehex=["#E65956", "#DF4A48", "#D93B3A", "#D22C2C", "#CC1E1E"]
mixhex=["#E75FD5", "#D54DBB", "#C33BA2", "#B12989", "#A01870"]

ymdhm = str(sys.argv[1])
dom = 'conus'
ymd=ymdhm[0:8]
year=int(ymdhm[0:4])
month=int(ymdhm[4:6])
day=int(ymdhm[6:8])
hour=int(ymdhm[8:10])
minute=int(ymdhm[10:12])
cyc = str(hour).zfill(2)
subcyc = str(minute).zfill(2)
vtime = ymdhm
print year, month , day, hour, minute

im = image.imread('/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/python.fv3/noaa.png')

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

fhours = [0]
dtime=datetime.datetime(year,month,day,hour,0)
date_list = [dtime + datetime.timedelta(hours=x) for x in fhours]
print date_list

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

for j in range(len(date_list)):
  ymd=dtime.strftime("%Y%m%d")
  fhour=date_list[j].strftime("%H")
  fhr = str(fhours[j]).zfill(2)
  prodind = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_retro/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfprs_hrconus_00.grib2')
  paraind = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_retro/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc++subcyc+'z.fgs.wrfprs_hrconus_00.grib2')

  refprod=np.asarray(prodind.select(name='Maximum/Composite radar reflectivity',level=0)[0].values)
  rainprod=np.asarray(prodind.select(name='Categorical rain',level=0)[0].values)
  frprod=np.asarray(prodind.select(name='Categorical freezing rain',level=0)[0].values)
  plprod=np.asarray(prodind.select(name='Categorical ice pellets',level=0)[0].values)
  snprod=np.asarray(prodind.select(name='Categorical snow',level=0)[0].values)
  refpara=np.asarray(paraind.select(name='Maximum/Composite radar reflectivity',level=0)[0].values)
  rainpara=np.asarray(paraind.select(name='Categorical rain',level=0)[0].values)
  frpara=np.asarray(paraind.select(name='Categorical freezing rain',level=0)[0].values)
  plpara=np.asarray(paraind.select(name='Categorical ice pellets',level=0)[0].values)
  snpara=np.asarray(paraind.select(name='Categorical snow',level=0)[0].values)

  typespara=np.zeros(frpara.shape)
  typespara[rainpara==1]=typespara[rainpara==1]+1
  typespara[frpara==1]=typespara[frpara==1]+3
  typespara[plpara==1]=typespara[plpara==1]+5
  typespara[snpara==1]=typespara[snpara==1]+7
  rain1para=np.copy(refpara)
  fr1para=np.copy(refpara)
  pl1para=np.copy(refpara)
  sn1para=np.copy(refpara)
  mix1para=np.copy(refpara)
  rain1para[typespara!=1]=-1
  fr1para[typespara!=3]=-1
  pl1para[typespara!=5]=-1
  sn1para[typespara!=7]=-1
  mix1para[typespara==0]=-1
  mix1para[typespara==1]=-1
  mix1para[typespara==3]=-1
  mix1para[typespara==5]=-1
  mix1para[typespara==7]=-1

  typesprod=np.zeros(frprod.shape)
  typesprod[rainprod==1]=typesprod[rainprod==1]+1
  typesprod[frprod==1]=typesprod[frprod==1]+3
  typesprod[plprod==1]=typesprod[plprod==1]+5
  typesprod[snprod==1]=typesprod[snprod==1]+7
  rain1prod=np.copy(refprod)
  fr1prod=np.copy(refprod)
  pl1prod=np.copy(refprod)
  sn1prod=np.copy(refprod)
  mix1prod=np.copy(refprod)
  rain1prod[typesprod!=1]=-1
  fr1prod[typesprod!=3]=-1
  pl1prod[typesprod!=5]=-1
  sn1prod[typesprod!=7]=-1
  mix1prod[typesprod==0]=-1
  mix1prod[typesprod==1]=-1
  mix1prod[typesprod==3]=-1
  mix1prod[typesprod==5]=-1
  mix1prod[typesprod==7]=-1


  lats,lons=prodind.select(name='Categorical snow',level=0)[0].latlons()
  if (int(fhr) == 0):
    x,y=m(lons,lats)

  clevs=[0,10,20,30,40]
  clevs1=[20,1000]

  # Clear off old plotables but keep all the map info
  if (int(fhr) > 0):
    clear_plotables(ax1,keep_ax_lst_1,fig)
    clear_plotables(ax2,keep_ax_lst_2,fig)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
#      intime=dtime.strftime("%m/%d/%Y %HZ")
#      vtime=date_list[j].strftime("%m/%d/%Y %HZ")
      csrain=m.contourf(x,y,rain1prod,clevs,colors=rainhex,extend='max',ax=ax)
      csmix=m.contourf(x,y,mix1prod,clevs,colors=mixhex,extend='max',ax=ax)
      cssnow=m.contourf(x,y,sn1prod,clevs,colors=snowhex,extend='max',ax=ax)
      cssleet=m.contourf(x,y,pl1prod,clevs,colors=sleethex,extend='max',ax=ax)
      csfrzra=m.contourf(x,y,fr1prod,clevs,colors=freezehex,extend='max',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS composite reflectivity by ptype \n vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    elif par == 2:
      csrain=m.contourf(x,y,rain1para,clevs,colors=rainhex,extend='max',ax=ax)
      csmix=m.contourf(x,y,mix1para,clevs,colors=mixhex,extend='max',ax=ax)
      cssnow=m.contourf(x,y,sn1para,clevs,colors=snowhex,extend='max',ax=ax)
      cssleet=m.contourf(x,y,pl1para,clevs,colors=sleethex,extend='max',ax=ax)
      csfrzra=m.contourf(x,y,fr1para,clevs,colors=freezehex,extend='max',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL composite reflectivity by ptype \n valid: '+ vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

      if (int(fhr) == 0):
        caxrain=fig.add_axes([.09,.2,.1,.03])
        cbrain=fig.colorbar(csrain,cax=caxrain,ticks=clevs,orientation='horizontal')
        cbrain.ax.tick_params(labelsize=5)
        cbrain.ax.set_xticklabels(['light rain','','','','heavy rain'])

        caxsnow=fig.add_axes([.27,.2,.1,.03])
        cbsnow=fig.colorbar(cssnow,cax=caxsnow,ticks=clevs,orientation='horizontal')
        cbsnow.ax.tick_params(labelsize=5)
        cbsnow.ax.set_xticklabels(['light snow','','','','heavy snow'])

        caxsleet=fig.add_axes([.45,.2,.1,.03])
        cbsleet=fig.colorbar(cssleet,cax=caxsleet,ticks=clevs,orientation='horizontal')
        cbsleet.ax.tick_params(labelsize=5)
        cbsleet.ax.set_xticklabels(['light sleet','','','','heavy sleet'])

        caxfrzra=fig.add_axes([.63,.2,.1,.03])
        cbfrzra=fig.colorbar(csfrzra,cax=caxfrzra,ticks=clevs,orientation='horizontal')
        cbfrzra.ax.tick_params(labelsize=5)
        cbfrzra.ax.set_xticklabels(['light freezing rain','','','','heavy freezing rain'])

        caxmix=fig.add_axes([.81,.2,.1,.03])
        cbmix=fig.colorbar(csmix,cax=caxmix,ticks=clevs,orientation='horizontal')
        cbmix.ax.tick_params(labelsize=5)
        cbmix.ax.set_xticklabels(['light mix','','','','heavy mix'])
    par += 1
  par = 1

  compress_and_save('comparetype_'+dom+'t'+cyc+subcyc+'z.png')

plt.close()
