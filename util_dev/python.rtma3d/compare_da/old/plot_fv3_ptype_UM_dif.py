import pygrib
import datetime
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.image as image
from mpl_toolkits.basemap import Basemap
import subprocess
from matplotlib.gridspec import GridSpec
import sys, os

prodhex=['#F00000','#32B2BD','#32B2BD']
parahex=['#F00000','#C15033','#C15033']
snowhex=["#64B3E8", "#5197D7", "#3E7CC6", "#2B60B5", "#1945A4"]
rainhex=["#5EE240", "#4DC534", "#3DA828", "#2D8B1C", "#1D6F11"]
sleethex=["#947EEC", "#7F62CB", "#6B47AB", "#562B8A", "#42106A"]
freezehex=["#E65956", "#DF4A48", "#D93B3A", "#D22C2C", "#CC1E1E"]
mixhex=["#E75FD5", "#D54DBB", "#C33BA2", "#B12989", "#A01870"]

ymdh = str(sys.argv[1])
ymd=ymdh[0:8]
year=int(ymdh[0:4])
month=int(ymdh[4:6])
day=int(ymdh[6:8])
hour=int(ymdh[8:10])
cyc = str(hour).zfill(2)
print year, month , day, hour

im = image.imread('/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/python.fv3/noaa.png')
xscale = 0.18
yscale = 0.18

fhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]
#fhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60]
dtime=datetime.datetime(year,month,day,hour,0)
date_list = [dtime + datetime.timedelta(hours=x) for x in fhours]
print date_list

for j in range(len(date_list)):
  ymd=dtime.strftime("%Y%m%d")
  fhour=date_list[j].strftime("%H")
  fhr = str(fhours[j]).zfill(2)
  prodind=pygrib.index('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhr+'.grib2','name','level')
  paraind=pygrib.index('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3nest/para/fv3nest.'+str(ymd)+'/'+cyc+'/fv3nest.t'+cyc+'z.conus.f'+fhr+'.grib2','name','level')

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

  fig = plt.figure()
  gs = GridSpec(4,12,wspace=0.0,hspace=0.0)
#  ax = plt.subplot(gs[0:4,6:12])
#  gs = GridSpec(4,8,wspace=0.0,hspace=0.0)
#  ax = plt.subplot(gs[0:4,1:7])

#  if (fhours[j] > 18):
#    ax1 = plt.subplot(gs[0:4,6:12])
#    axes = [ax1]
#    ll = 1
#  else:
  ax1 = plt.subplot(gs[0:4,0:6])
  ax2 = plt.subplot(gs[0:4,6:12])
  axes = [ax1, ax2]
  ll = 0
  clevs=[0,10,20,30,40]
  clevs1=[20,1000]
  for ax in axes:
    print ll
    map = Basemap(ax=ax,projection='stere',lat_0=44.0,lon_0=-91.5,\
                  llcrnrlat=39.75,urcrnrlat=49.0,llcrnrlon=-96.75,urcrnrlon=-81.0,\
                  resolution='l')
    x,y=map(lons,lats)
    map.fillcontinents(color='LightGrey',zorder=0)
    #map.drawmeridians(np.arange(0,360,10))
    #map.drawparallels(np.arange(-90,90,5))
    #map.drawcounties(linewidth=.25,color='DimGray')
    map.drawstates(linewidth=.5,color='k')
    map.drawcoastlines(linewidth=.75, color='k')
    map.drawcountries(linewidth=.5, color='k')

    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if ll==0:
      intime=dtime.strftime("%m/%d/%Y %HZ")
      vtime=date_list[j].strftime("%m/%d/%Y %HZ")
      csrain=map.contourf(x,y,rain1prod,clevs,colors=rainhex,extend='max')
      csmix=map.contourf(x,y,mix1prod,clevs,colors=mixhex,extend='max')
      cssnow=map.contourf(x,y,sn1prod,clevs,colors=snowhex,extend='max')
      cssleet=map.contourf(x,y,pl1prod,clevs,colors=sleethex,extend='max')
      csfrzra=map.contourf(x,y,fr1prod,clevs,colors=freezehex,extend='max')
      ax.text(.5,1.03,'FV3SAR composite reflectivity by ptype \n initialized: '+intime +' valid: '+ vtime + ' (F'+fhr+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    elif ll==1:
      csrain=map.contourf(x,y,rain1para,clevs,colors=rainhex,extend='max')
      csmix=map.contourf(x,y,mix1para,clevs,colors=mixhex,extend='max')
      cssnow=map.contourf(x,y,sn1para,clevs,colors=snowhex,extend='max')
      cssleet=map.contourf(x,y,pl1para,clevs,colors=sleethex,extend='max')
      csfrzra=map.contourf(x,y,fr1para,clevs,colors=freezehex,extend='max')
      ax.text(.5,1.03,'FV3SAR-DA composite reflectivity by ptype \n initialized: '+intime +' valid: '+ vtime + ' (F'+fhr+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

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
    ll+=1

#  plt.tight_layout()
  plt.savefig('comparetype_UM_f'+fhr+'.png', bbox_inches='tight',dpi=150)
  plt.close()
