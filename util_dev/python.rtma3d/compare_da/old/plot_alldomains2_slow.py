import pygrib
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
from mpl_toolkits.basemap import Basemap
import numpy as np
import time,os,sys,multiprocessing
import ncepy
from scipy import ndimage
from netCDF4 import Dataset

#--------------Define some functions ------------------#

def cmap_t2m():
 # Create colormap for 2-m temperature
 # Modified version of the ncl_t2m colormap from Jacob's ncepy code
    r=np.array([255,128,0,  70, 51, 0,  255,0, 0,  51, 255,255,255,255,255,171,128,128,36,162,255])
    g=np.array([0,  0,  0,  70, 102,162,255,92,128,185,255,214,153,102,0,  0,  0,  68, 36,162,255])
    b=np.array([255,128,128,255,255,255,255,0, 0,  102,0,  112,0,  0,  0,  56, 0,  68, 36,162,255])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    green = []
    blue = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    cmap_t2m_coltbl = matplotlib.colors.LinearSegmentedColormap('CMAP_T2M_COLTBL',colorDict)
    return cmap_t2m_coltbl


def cmap_t850():
 # Create colormap for 850-mb equivalent potential temperature
    r=np.array([255,128,0,  70, 51, 0,  0,  0, 51, 255,255,255,255,255,171,128,128,96,201])
    g=np.array([0,  0,  0,  70, 102,162,225,92,153,255,214,153,102,0,  0,  0,  68, 96,201])
    b=np.array([255,128,128,255,255,255,162,0, 102,0,  112,0,  0,  0,  56, 0,  68, 96,201])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    green = []
    blue = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    cmap_t850_coltbl = matplotlib.colors.LinearSegmentedColormap('CMAP_T850_COLTBL',colorDict)
    return cmap_t850_coltbl


def cmap_terra():
 # Create colormap for terrain height
 # Emerald green to light green to tan to gold to dark red to brown to light brown to white
    r=np.array([0,  152,212,188,127,119,186])
    g=np.array([128,201,208,148,34, 83, 186])
    b=np.array([64, 152,140,0,  34, 64, 186])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    green = []
    blue = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    cmap_terra_coltbl = matplotlib.colors.LinearSegmentedColormap('CMAP_TERRA_COLTBL',colorDict)
    cmap_terra_coltbl.set_over(color='#E0EEE0')
    return cmap_terra_coltbl


def extrema(mat,mode='wrap',window=100):
    # find the indices of local extrema (max only) in the input array.
    mx = ndimage.filters.maximum_filter(mat,size=window,mode=mode)
    # (mat == mx) true if pixel is equal to the local max
    return np.nonzero(mat == mx)

#-------------------------------------------------------#

# Necessary to generate figs when not running an Xserver (e.g. via PBS)
# plt.switch_backend('agg')

# Read date/time and forecast hour from command line
ymdh = str(sys.argv[1])
ymd = ymdh[0:8]
year = int(ymdh[0:4])
month = int(ymdh[4:6])
day = int(ymdh[6:8])
hour = int(ymdh[8:10])
cyc = str(hour).zfill(2)
print year, month, day, hour

fhr = int(sys.argv[2])
# For all files except the 2D fv3 history file, the files start at forecast hour 1 (so fhr = 0 even though we are plotting the first forecast hour)
fhrm1 = fhr - 1
fhrm2 = fhr - 2
fhrm6 = fhr - 6
fhrm24 = fhr - 24
fhour = str(fhr).zfill(2)
fhour1 = str(fhrm1).zfill(2)
fhour2 = str(fhrm2).zfill(2)
fhour6 = str(fhrm6).zfill(2)
fhour24 = str(fhrm24).zfill(2)
print 'fhour '+fhour

# Define the output files
data1 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')
data2 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')


# Get the lats and lons
lat,lon = data1.select(name='2 metre temperature')[0].latlons()
lat2,lon2 = data2.select(name='2 metre temperature')[0].latlons()

# Forecast valid date/time
itime = ymdh
vtime = ncepy.ndate(itime,int(fhr))

# Specify plotting domains
domains = ['conus','MA','NC','NE','NW','OV','SC','SE','SW','UM']

###################################################
# Read in all variables and calculate differences #
###################################################
t1a = time.clock()

# Terrain height with 10-m winds
uwind_1 = data1.select(name='10 metre U wind component')[0].values * 1.94384
uwind_2 = data2.select(name='10 metre U wind component')[0].values * 1.94384
vwind_1 = data1.select(name='10 metre V wind component')[0].values * 1.94384
vwind_2 = data2.select(name='10 metre V wind component')[0].values * 1.94384
wspd10m_1 = np.sqrt(uwind_1**2 + vwind_1**2)
wspd10m_2 = np.sqrt(uwind_2**2 + vwind_2**2)
terra_1 = data1.select(name='Orography')[0].values * 3.28084
terra_2 = data2.select(name='Orography')[0].values * 3.28084
terra_dif = terra_2 - terra_1

# Hybrid level 1 fields
clwmr_1 = data1.select(name='Cloud mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
clwmr_2 = data2.select(name='Cloud mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
clwmr_dif = clwmr_2 - clwmr_1

icmr_1 = data1.select(name='Ice water mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
icmr_2 = data2.select(name='Ice water mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
icmr_dif = icmr_2 - icmr_1

rwmr_1 = data1.select(name='Rain mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
rwmr_2 = data2.select(name='Rain mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
rwmr_dif = rwmr_2 - rwmr_1

snmr_1 = data1.select(name='Snow mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
snmr_2 = data2.select(name='Snow mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
snmr_dif = snmr_2 - snmr_1

grle_1 = data1.select(name='Graupel (snow pellets)',typeOfLevel='hybrid',level=1)[0].values * 1000
grle_2 = data2.select(name='Graupel (snow pellets)',typeOfLevel='hybrid',level=1)[0].values * 1000
grle_dif = grle_2 - grle_1

refd_1 = data1.select(name='Derived radar reflectivity',typeOfLevel='hybrid',level=1)[0].values
refd_2 = data2.select(name='Derived radar reflectivity',typeOfLevel='hybrid',level=1)[0].values

tmphyb_1 = data1.select(name='Temperature',typeOfLevel='hybrid',level=1)[0].values - 273.15
tmphyb_2 = data2.select(name='Temperature',typeOfLevel='hybrid',level=1)[0].values - 273.15

# Soil type - Integer (0-16) - only plot for f00
sotyp_1 = data1.select(name='Soil type')[0].values
sotyp_2 = data2.select(name='Soil type')[0].values
sotyp_dif = sotyp_2 - sotyp_1

# Vegetation Type - Integer (0-19) - only plot for f00
vgtyp_1 = data1.select(name='Vegetation Type')[0].values
vgtyp_2 = data2.select(name='Vegetation Type')[0].values
vgtyp_dif = vgtyp_2 - vgtyp_1

# Vegetation Fraction
veg_1 = data1.select(name='Vegetation')[0].values
veg_2 = data2.select(name='Vegetation')[0].values
veg_dif = veg_2 - veg_1

# Soil Temperature
tsoil_1 = data1.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=0)[0].values
tsoil_1 = (tsoil_1 - 273.15)*1.8 + 32.0
tsoil_2 = data2.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=0)[0].values
tsoil_2 = (tsoil_2 - 273.15)*1.8 + 32.0
tsoil_dif = tsoil_2 - tsoil_1

# Soil Moisture
soilw_1 = data1.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=0)[0].values * 100
soilw_2 = data2.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=0)[0].values * 100
soilw_dif = soilw_2 - soilw_1


t2a = time.clock()
t3a = round(t2a-t1a, 3)
print("%.3f seconds to read all messages") % t3a

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
difcolors2 = ['white']
#difcolors = ['red','orangered','darkorange','#EEC900','#EEEE00','white','white','turquoise','deepskyblue','dodgerblue','#1874CD','blue']

########################################
#    START PLOTTING FOR EACH DOMAIN    #
########################################

def main():

  # Number of processes must coincide with the number of domains to plot
  pool = multiprocessing.Pool(len(domains))
  pool.map(plot_all,domains)

def plot_all(dom):

  t1dom = time.clock()
  print('Working on '+dom)

  # create figure and axes instances
#  fig = plt.figure()
#  plt.figure()
#  gs = GridSpec(9,8,wspace=0.0,hspace=0.0)
#  ax1 = fig.add_subplot(gs[0:4,0:4])
#  ax2 = fig.add_subplot(gs[5,0:4])
#  ax3 = fig.add_subplot(gs[1:8,4:])
#  ax1 = plt.subplot(gs[0:4,0:4])
#  ax2 = plt.subplot(gs[5:,0:4])
#  ax3 = plt.subplot(gs[1:8,4:])
#  axes = [ax1, ax2, ax3]
  par = 1

  # Map corners for each domain
  if dom == 'conus':
    llcrnrlon = -120.5
    llcrnrlat = 21.0 
    urcrnrlon = -64.5
    urcrnrlat = 49.0
    lat_0 = 35.4
    lon_0 = -97.6
  elif dom == 'MA':
    llcrnrlon = -83.0
    llcrnrlat = 35.5
    urcrnrlon = -73.0
    urcrnrlat = 41.5
    lat_0 = 36.5
    lon_0 = -81.0
  elif dom == 'NC':
    llcrnrlon = -109.0
    llcrnrlat = 35.5
    urcrnrlon = -87.0
    urcrnrlat = 48.5
    lat_0 = 42.5
    lon_0 = -99.0
  elif dom == 'NE':
    llcrnrlon = -81.0     
    llcrnrlat = 39.0
    urcrnrlon = -65.0
    urcrnrlat = 47.5
    lat_0 = 40.0
    lon_0 = -80.0
  elif dom == 'NW':
    llcrnrlon = -126.0     
    llcrnrlat = 38.0
    urcrnrlon = -107.0
    urcrnrlat = 50.0
    lat_0 = 44.0
    lon_0 = -114.0
  elif dom == 'OV':
    llcrnrlon = -93.0 
    llcrnrlat = 32.0
    urcrnrlon = -78.0
    urcrnrlat = 43.0
    lat_0 = 38.0
    lon_0 = -87.0            
  elif dom == 'SC':
    llcrnrlon = -109.0 
    llcrnrlat = 26.0
    urcrnrlon = -87.0
    urcrnrlat = 40.0
    lat_0 = 35.0
    lon_0 = -98.0      
  elif dom == 'SE':
    llcrnrlon = -92.0 
    llcrnrlat = 23.5
    urcrnrlon = -73.0
    urcrnrlat = 38.5
    lat_0 = 35.0
    lon_0 = -85.0
  elif dom == 'SW':
    llcrnrlon = -125.0 
    llcrnrlat = 30.0
    urcrnrlon = -107.0
    urcrnrlat = 44.0
    lat_0 = 37.0
    lon_0 = -113.0
  elif dom == 'UM':
    llcrnrlon = -97.0 
    llcrnrlat = 38.5
    urcrnrlon = -81.0
    urcrnrlat = 48.5
    lat_0 = 43.0
    lon_0 = -90.0

  # Create basemap instance and set the dimensions
#  for ax in axes:
#    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
#                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
#                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
#                resolution='l')
#    m.fillcontinents(color='LightGrey',zorder=0)
#    m.drawcoastlines(linewidth=1.0)
#    m.drawstates(linewidth=0.75)
#    m.drawcountries(linewidth=0.75)
##  parallels = np.arange(0.,90.,10.)
##  map.drawparallels(parallels,labels=[1,0,0,0],fontsize=6)
##  meridians = np.arange(180.,360.,10.)
##  map.drawmeridians(meridians,labels=[0,0,0,1],fontsize=6)
#    x,y = m(lon,lat)

  # Map/figure has been set up here, save axes instances for use again later
#    if par == 1:
#      keep_ax_lst_1 = ax.get_children()[:]
#    elif par == 2:
#      keep_ax_lst_2 = ax.get_children()[:]
#    elif par == 3:
#      keep_ax_lst_3 = ax.get_children()[:]
#    par += 1
#  par = 1


#################################
  # Plot Terrain with 10-m WSPD
#################################
  t1 = time.clock()
  print('Working on Terrain for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'ft'
  if dom == 'conus':
    skip = 80
    barblength = 4.
  else:
    skip = 40
    barblength = 4.5
  clevs = [1,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,7250,7500,7750,8000,8250,8500,8750,9000,9250,9500,9750,10000]
  clevsdif = [-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300]
  cm = cmap_terra()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,terra_1,clevs,cmap=cm,extend='max')
      cs_1.cmap.set_over('ghostwhite')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],uwind_1[::skip,::skip],vwind_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3 Terrain Height ('+units+') and 10-m Winds (kts) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,terra_2,clevs,cmap=cm,extend='max')
      cs_2.cmap.set_over('ghostwhite')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],uwind_2[::skip,::skip],vwind_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3-DA Terrain Height ('+units+') and 10-m Winds (kts) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,terra_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Terrain Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compareterra_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Terrain for: '+dom) % t3


#################################
  # Plot soil type
#################################
#  if (fhr == 0):
  t1 = time.clock()
  print('Working on soil type for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'Integer(0-16)'
  clevs = [-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5]
  clevsdif = [-0.1,0.1]
  colorlist = ['#00CDCD','saddlebrown','khaki','gray','#3D9140','palegreen','firebrick','lightcoral','darkorchid','plum','blue','lightskyblue','#CDAD00','yellow','#FF4500','lightsalmon','#CD1076']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,sotyp_1,clevs,colors=colorlist)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Soil Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x,y,sotyp_2,clevs,colors=colorlist)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Soil Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,sotyp_dif,clevsdif,colors=difcolors2,extend='both')
      cs.cmap.set_under('darkred')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=[0])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Soil Type ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

  plt.savefig('./comparesotyp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot soil type for: '+dom) % t3


#################################
  # Plot vegetation type
#################################
#  if (fhr == 0):
  t1 = time.clock()
  print('Working on vegetation type for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'Integer(0-19)'
  clevs = [-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5]
  clevsdif = [-0.1,0.1]
  colorlist = ['#00CDCD','saddlebrown','khaki','gray','#3D9140','palegreen','firebrick','lightcoral','darkorchid','plum','blue','lightskyblue','#CDAD00','yellow','#FF4500','lightsalmon','#CD1076']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,vgtyp_1,clevs,colors=colorlist)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Vegetation Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x,y,vgtyp_2,clevs,colors=colorlist)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Vegetation Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,vgtyp_dif,clevsdif,colors=difcolors2,extend='both')
      cs.cmap.set_under('darkred')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=[0])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Vegetation Type ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

  plt.savefig('./comparevgtyp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot vegetation type for: '+dom) % t3


#################################
  # Plot vegetation fraction
#################################
  t1 = time.clock()
  print('Working on vegetation fraction for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [10,20,30,40,50,60,70,80,90,100]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  cm = ncepy.cmap_q2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='White',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,veg_1,clevs,cmap=cm)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x,y,veg_2,clevs,cmap=cm)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,veg_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

  plt.savefig('./compareveg_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot vegetation fraction for: '+dom) % t3


#################################
  # Plot 0-10cm soil temperature
#################################
  t1 = time.clock()
  print('Working on 0-10cm soil temperature for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-36,104,36)
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  cm = cmap_t2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='White',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,tsoil_1,clevs,cmap=cm,extend='both')
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 0-10cm Soil Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x,y,tsoil_2,clevs,cmap=cm,extend='both')
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA 0-10cm Soil Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,tsoil_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 0-10cm Soil Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

  plt.savefig('./comparetsoil_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 0-10cm soil temperature for: '+dom) % t3


#################################
  # Plot 0-10cm Soil Moisture Content
#################################
  t1 = time.clock()
  print('Working on 0-10cm soil moisture for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [5,10,15,20,25,30,35,40,45,50]
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  colorlist = ['crimson','darkorange','darkgoldenrod','#EEC900','chartreuse','limegreen','green','#1C86EE','deepskyblue']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='White',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,soilw_1,clevs,colors=colorlist,extend='both')
      cs_1.cmap.set_under('darkred')
      cs_1.cmap.set_over('white')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 0-10cm Soil Moisture Content ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x,y,soilw_2,clevs,colors=colorlist,extend='both')
      cs_2.cmap.set_under('darkred')
      cs_2.cmap.set_over('white')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA 0-10cm Soil Moisture Content ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,soilw_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 0-10cm Soil Moisture Content ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

  plt.savefig('./comparesoilw_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 0-10cm soil moisture content for: '+dom) % t3


#################################
  # Plot lowest model level cloud water
#################################
  t1 = time.clock()
  print('Working on lowest model level cloud water for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']
  
  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey')
      cs_1 = m.contourf(x,y,clwmr_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Lowest Mdl Lvl Cld Water ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey')
      cs_2 = m.contourf(x2,y2,clwmr_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Lowest Mdl Lvl Cld Water ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,clwmr_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA - FV3 Lowest Mdl Lvl Cld Water ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1

  plt.savefig('./compareclwmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot lowest model level cloud water for: '+dom) % t3

#################################
  # Plot lowest model level cloud ice
#################################
  t1 = time.clock()
  print('Working on lowest model level cloud ice for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey')
      cs_1 = m.contourf(x,y,icmr_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Lowest Mdl Lvl Cld Ice ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey')
      cs_2 = m.contourf(x2,y2,icmr_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Lowest Mdl Lvl Cld Ice ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,icmr_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Lowest Mdl Lvl Cld Ice ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1
  
  plt.savefig('./compareicmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot lowest model level cloud ice for: '+dom) % t3

#################################
  # Plot lowest model level rain
#################################
  t1 = time.clock()
  print('Working on lowest model level rain for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey')
      cs_1 = m.contourf(x,y,rwmr_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Lowest Mdl Lvl Rain ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey')
      cs_2 = m.contourf(x2,y2,rwmr_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Lowest Mdl Lvl Rain ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,rwmr_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Lowest Mdl Lvl Rain ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1

  plt.savefig('./comparerwmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot lowest model level rain for: '+dom) % t3

#################################
  # Plot lowest model level snow
#################################
  t1 = time.clock()
  print('Working on lowest model level snow for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey')
      cs_1 = m.contourf(x,y,snmr_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Lowest Mdl Lvl Snow ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 

    elif par == 2:
      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey')
      cs_2 = m.contourf(x2,y2,snmr_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Lowest Mdl Lvl Snow ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 

    elif par == 3:
      cs = m.contourf(x2,y2,snmr_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Lowest Mdl Lvl Snow ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1

  plt.savefig('./comparesnmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot lowest model level snow for: '+dom) % t3

#################################
  # Plot lowest model level graupel
#################################
  t1 = time.clock()
  print('Working on lowest model level graupel for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:4])
  ax2 = plt.subplot(gs[0:4,5:])
  ax3 = plt.subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]

  for ax in axes:
    m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                resolution='l')
    m.fillcontinents(color='white',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey')
      cs_1 = m.contourf(x,y,grle_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3 Lowest Mdl Lvl Graupel ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 

    elif par == 2:
      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey')
      cs_2 = m.contourf(x2,y2,grle_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.set_xticklabels(clevs)
      cbar.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'FV3-DA Lowest Mdl Lvl Graupel ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 

    elif par == 3:
      cs = m.contourf(x2,y2,grle_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Lowest Mdl Lvl Graupel ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1

  plt.savefig('./comparegrle_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot lowest model level graupel for: '+dom) % t3


######################################################

  t3dom = round(t2-t1dom, 3)
  print("%.3f seconds to plot all variables for: "+dom) % t3dom
  plt.clf()

######################################################

main()
