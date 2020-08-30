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

#--------------Define some functions ------------------#

def clear_plotables(ax,keep_ax_lst,fig):
  #### - step to clear off old plottables but leave the map info - ####
  if len(keep_ax_lst) == 0 :
    print("clear_plotables WARNING keep_ax_lst has length 0. Clearing ALL plottables including map info!")
  cur_ax_children = ax.get_children()[:]
  if len(cur_ax_children) > 0:
    for a in cur_ax_children:
      if a not in keep_ax_lst:
       # if the artist isn't part of the initial set up, remove it
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
ymdhm = str(sys.argv[1])
ymd = ymdhm[0:8]
year = int(ymdhm[0:4])
month = int(ymdhm[4:6])
day = int(ymdhm[6:8])
hour = int(ymdhm[8:10])
minute = int(ymdhm[10:12])
cyc = str(hour).zfill(2)
subcyc = str(minute).zfill(2)
print(year, month, day, hour, minute)

#fhr = int(sys.argv[2])
#fhour = str(fhr).zfill(2)
#print 'fhour '+fhour

# Define the output files
data2 = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_realtime/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhnat.grib2')
data1 = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_realtime/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhnat_fgs.grib2')

# Get the lats and lons
lat,lon = data1.select(name='2 metre temperature')[0].latlons()
lat2,lon2 = data2.select(name='2 metre temperature')[0].latlons()
Lon0 = data1[1]['LoVInDegrees']
Lat0 = data1[1]['LaDInDegrees']

# Forecast valid date/time
#itime = ymdh
#vtime = ncepy.ndate(itime,int(fhr))
vtime = ymdhm
# Specify plotting domains
domains = ['conus','BN','CE','CO','LA','MA','NC','NE','NW','OV','SC','SE','SF','SP','SW','UM']

###################################################
# Read in all variables and calculate differences #
###################################################
t1a = time.clock()

# Terrain height with 10-m winds
uwind_1 = data1.select(name='10 metre U wind component')[0].values * 1.94384
uwind_2 = data2.select(name='10 metre U wind component')[0].values * 1.94384
vwind_1 = data1.select(name='10 metre V wind component')[0].values * 1.94384
vwind_2 = data2.select(name='10 metre V wind component')[0].values * 1.94384
# Rotate winds from grid relative to Earth relative
uwind_1, vwind_1 = ncepy.rotate_wind(Lat0,Lon0,lon,uwind_1,vwind_1,'lcc',inverse=False)
uwind_2, vwind_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,uwind_2,vwind_2,'lcc',inverse=False)
wspd10m_1 = np.sqrt(uwind_1**2 + vwind_1**2)
wspd10m_2 = np.sqrt(uwind_2**2 + vwind_2**2)
terra_1 = data1.select(name='Orography')[0].values * 3.28084
terra_2 = data2.select(name='Orography')[0].values * 3.28084
terra_dif = terra_2 - terra_1

# Hybrid level 1 fields
clwmr_1 = data1.select(name='Cloud mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
clwmr_2 = data2.select(name='Cloud mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
clwmr_dif = clwmr_2 - clwmr_1

#icmr_1 = data1.select(name='Ice water mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
#icmr_2 = data2.select(name='Ice water mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
#icmr_dif = icmr_2 - icmr_1

rwmr_1 = data1.select(name='Rain mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
rwmr_2 = data2.select(name='Rain mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
rwmr_dif = rwmr_2 - rwmr_1

snmr_1 = data1.select(name='Snow mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
snmr_2 = data2.select(name='Snow mixing ratio',typeOfLevel='hybrid',level=1)[0].values * 1000
snmr_dif = snmr_2 - snmr_1

grle_1 = data1.select(name='Graupel (snow pellets)',typeOfLevel='hybrid',level=1)[0].values * 1000
grle_2 = data2.select(name='Graupel (snow pellets)',typeOfLevel='hybrid',level=1)[0].values * 1000
grle_dif = grle_2 - grle_1

#refd_1 = data1.select(name='Derived radar reflectivity',typeOfLevel='hybrid',level=1)[0].values
#refd_2 = data2.select(name='Derived radar reflectivity',typeOfLevel='hybrid',level=1)[0].values

tmphyb_1 = data1.select(name='Temperature',typeOfLevel='hybrid',level=1)[0].values - 273.15
tmphyb_2 = data2.select(name='Temperature',typeOfLevel='hybrid',level=1)[0].values - 273.15

# Soil type - Integer (0-16) - only plot for f00
#sotyp_1 = data1.select(name='Soil type')[0].values
#sotyp_2 = data2.select(name='Soil type')[0].values
#sotyp_dif = sotyp_2 - sotyp_1

# Vegetation Type - Integer (0-19) - only plot for f00
#vgtyp_1 = data1.select(name='Vegetation Type')[0].values
#vgtyp_2 = data2.select(name='Vegetation Type')[0].values
#vgtyp_dif = vgtyp_2 - vgtyp_1

# Vegetation Fraction
#veg_1 = data1.select(name='Vegetation')[0].values
#veg_2 = data2.select(name='Vegetation')[0].values
#veg_dif = veg_2 - veg_1

# Soil Temperature
tsoil_0_10_1 = (data1.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=0)[0].values - 273.15)*1.8 + 32.0
tsoil_0_10_2 = (data2.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=0)[0].values - 273.15)*1.8 + 32.0
tsoil_0_10_dif = tsoil_0_10_2 - tsoil_0_10_1

#tsoil_10_40_1 = (data1.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=10)[0].values - 273.15)*1.8 + 32.0
#tsoil_10_40_2 = (data2.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=10)[0].values - 273.15)*1.8 + 32.0
#tsoil_10_40_dif = tsoil_10_40_2 - tsoil_10_40_1

#tsoil_40_100_1 = (data1.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=40)[0].values - 273.15)*1.8 + 32.0
#tsoil_40_100_2 = (data2.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=40)[0].values - 273.15)*1.8 + 32.0
#tsoil_40_100_dif = tsoil_40_100_2 - tsoil_40_100_1

#tsoil_100_200_1 = (data1.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=100)[0].values - 273.15)*1.8 + 32.0
#tsoil_100_200_2 = (data2.select(name='Soil Temperature',scaledValueOfFirstFixedSurface=100)[0].values - 273.15)*1.8 + 32.0
#tsoil_100_200_dif = tsoil_100_200_2 - tsoil_100_200_1

# Soil Moisture
#soilw_0_10_1 = data1.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=0)[0].values
#soilw_0_10_2 = data2.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=0)[0].values
#soilw_0_10_dif = soilw_0_10_2 - soilw_0_10_1

#soilw_10_40_1 = data1.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=10)[0].values
#soilw_10_40_2 = data2.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=10)[0].values
#soilw_10_40_dif = soilw_10_40_2 - soilw_10_40_1

#soilw_40_100_1 = data1.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=40)[0].values
#soilw_40_100_2 = data2.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=40)[0].values
#soilw_40_100_dif = soilw_40_100_2 - soilw_40_100_1

#soilw_100_200_1 = data1.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=100)[0].values
#soilw_100_200_2 = data2.select(name='Volumetric soil moisture content',scaledValueOfFirstFixedSurface=100)[0].values
#soilw_100_200_dif = soilw_100_200_2 - soilw_100_200_1

# Downward shortwave radiation
swdown_1 = data1.select(name='Downward short-wave radiation flux',stepType='instant')[0].values
swdown_2 = data2.select(name='Downward short-wave radiation flux',stepType='instant')[0].values
swdown_dif = swdown_2 - swdown_1

# Upward shortwave radiation
swup_1 = data1.select(name='Upward short-wave radiation flux',stepType='instant')[0].values
swup_2 = data2.select(name='Upward short-wave radiation flux',stepType='instant')[0].values
swup_dif = swup_2 - swup_1

# Downward longwave radiation
lwdown_1 = data1.select(name='Downward long-wave radiation flux',stepType='instant')[0].values
lwdown_2 = data2.select(name='Downward long-wave radiation flux',stepType='instant')[0].values
lwdown_dif = lwdown_2 - lwdown_1

# Upward longwave radiation
lwup_1 = data1.select(name='Upward short-wave radiation flux',stepType='instant')[0].values
lwup_2 = data2.select(name='Upward short-wave radiation flux',stepType='instant')[0].values
lwup_dif = lwup_2 - lwup_1

# Ground heat flux
gdhfx_1 = data1.select(name='Ground heat flux',stepType='instant',typeOfLevel='surface')[0].values
gdhfx_2 = data2.select(name='Ground heat flux',stepType='instant',typeOfLevel='surface')[0].values
gdhfx_dif = gdhfx_2 - gdhfx_1

# Latent heat flux
lhfx_1 = data1.select(name='Latent heat net flux',stepType='instant',typeOfLevel='surface')[0].values
lhfx_2 = data2.select(name='Latent heat net flux',stepType='instant',typeOfLevel='surface')[0].values
lhfx_dif = lhfx_2 - lhfx_1

# Sensible heat flux
snhfx_1 = data1.select(name='Sensible heat net flux',stepType='instant',typeOfLevel='surface')[0].values
snhfx_2 = data2.select(name='Sensible heat net flux',stepType='instant',typeOfLevel='surface')[0].values
snhfx_dif = snhfx_2 - snhfx_1

# PBL height
#hpbl_1 = data1.select(name='Planetary boundary layer height')[0].values
#hpbl_2 = data2.select(name='Planetary boundary layer height')[0].values
#hpbl_dif = hpbl_2 - hpbl_1

# Total column condensate
#cond_1 = data1.select(name='Total Column Integrate Graupel',stepType='instant')[0].values
#cond_2 = data2.select(name='Total Column Integrate Graupel',stepType='instant')[0].values
#cond_dif = cond_2 - cond_1

# Total column integrated in)
#tqw_1 = data1.select(name='Total column-integrated cloud water',stepType='instant')[0].values
#tqw_2 = data2.select(name='Total column-integrated cloud water',stepType='instant')[0].values
#tqr_1 = data1.select(name='Total column integrated rain',stepType='instant')[0].values
#tqr_2 = data2.select(name='Total column integrated rain',stepType='instant')[0].values
#tcolw_1 = tqw_1 + tqr_1
#tcolw_2 = tqw_2 + tqr_2
#tcolw_dif = tcolw_2 - tcolw_1

# Total column integrated ice (cloud ice + snow)
#tqi_1 = data1.select(name='Total column-integrated cloud ice',stepType='instant')[0].values
#tqi_2 = data2.select(name='Total column-integrated cloud ice',stepType='instant')[0].values
#tqs_1 = data1.select(name='Total column integrated snow',stepType='instant')[0].values
#tqs_2 = data2.select(name='Total column integrated snow',stepType='instant')[0].values
#tcoli_1 = tqi_1 + tqs_1
#tcoli_2 = tqi_2 + tqs_2
#tcoli_dif = tcoli_2 - tcoli_1

# 0-3 km Storm Relative Helicity
hel3km_1 = data1.select(name='Storm relative helicity',topLevel=3000,bottomLevel=0)[0].values
hel3km_2 = data2.select(name='Storm relative helicity',topLevel=3000,bottomLevel=0)[0].values
hel3km_dif = hel3km_2 - hel3km_1

# 0-1 km Storm Relative Helicity
hel1km_1 = data1.select(name='Storm relative helicity',topLevel=1000,bottomLevel=0)[0].values
hel1km_2 = data2.select(name='Storm relative helicity',topLevel=1000,bottomLevel=0)[0].values
hel1km_dif = hel1km_2 - hel1km_1



t2a = time.clock()
t3a = round(t2a-t1a, 3)
print(("%.3f seconds to read all messages") % t3a)

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
difcolors2 = ['white']

########################################
#    START PLOTTING FOR EACH DOMAIN    #
########################################

def main():

  # Number of processes must coincide with the number of domains to plot
  pool = multiprocessing.Pool(len(domains))
  pool.map(plot_all,domains)

def plot_all(dom):

  t1dom = time.clock()
  print(('Working on '+dom))

  # create figure and axes instances
  fig = plt.figure()
  gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
  ax1 = fig.add_subplot(gs[0:4,0:4])
  ax2 = fig.add_subplot(gs[0:4,5:])
  ax3 = fig.add_subplot(gs[5:,1:8])
  axes = [ax1, ax2, ax3]
  im = image.imread('/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/python.fv3/noaa.png')
  par = 1

  # Map corners for each domain
  if dom == 'conus':
    llcrnrlon = -120.5
    llcrnrlat = 21.0 
    urcrnrlon = -64.5
    urcrnrlat = 49.0
    lat_0 = 35.4
    lon_0 = -97.6
    xscale = 0.15
    yscale = 0.20
  elif dom == 'BN':
    llcrnrlon = -75.75
    llcrnrlat = 40.0
    urcrnrlon = -69.5
    urcrnrlat = 43.0
    lat_0 = 41.0
    lon_0 = -74.6
    xscale = 0.14
    yscale = 0.19
  elif dom == 'CE':
    llcrnrlon = -103.0
    llcrnrlat = 32.5
    urcrnrlon = -88.5
    urcrnrlat = 41.5
    lat_0 = 35.0
    lon_0 = -97.0
    xscale = 0.15
    yscale = 0.18
  elif dom == 'CO':
    llcrnrlon = -110.5
    llcrnrlat = 35.0
    urcrnrlon = -100.5
    urcrnrlat = 42.0
    lat_0 = 38.0
    lon_0 = -105.0
    xscale = 0.17
    yscale = 0.18
  elif dom == 'LA':
    llcrnrlon = -121.0
    llcrnrlat = 32.0
    urcrnrlon = -114.0
    urcrnrlat = 37.0
    lat_0 = 34.0
    lon_0 = -114.0
    xscale = 0.16
    yscale = 0.18
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
    xscale = 0.16
    yscale = 0.18
  elif dom == 'NE':
    llcrnrlon = -80.0     
    llcrnrlat = 40.5
    urcrnrlon = -66.0
    urcrnrlat = 47.5
    lat_0 = 42.0
    lon_0 = -80.0
    xscale = 0.16
    yscale = 0.18
  elif dom == 'NW':
    llcrnrlon = -125.5     
    llcrnrlat = 40.5
    urcrnrlon = -109.0
    urcrnrlat = 49.5
    lat_0 = 44.0
    lon_0 = -116.0
    xscale = 0.15
    yscale = 0.18
  elif dom == 'OV':
    llcrnrlon = -91.5 
    llcrnrlat = 34.75
    urcrnrlon = -80.0
    urcrnrlat = 43.0
    lat_0 = 38.0
    lon_0 = -87.0
    xscale = 0.18
    yscale = 0.17            
  elif dom == 'SC':
    llcrnrlon = -108.0 
    llcrnrlat = 25.0
    urcrnrlon = -88.0
    urcrnrlat = 37.0
    lat_0 = 32.0
    lon_0 = -98.0
    xscale = 0.14
    yscale = 0.18      
  elif dom == 'SE':
    llcrnrlon = -91.5 
    llcrnrlat = 24.0
    urcrnrlon = -74.0
    urcrnrlat = 36.5
    lat_0 = 34.0
    lon_0 = -85.0
    xscale = 0.17
    yscale = 0.18
  elif dom == 'SF':
    llcrnrlon = -123.25
    llcrnrlat = 37.25
    urcrnrlon = -121.25
    urcrnrlat = 38.5
    lat_0 = 37.5
    lon_0 = -121.0
    xscale = 0.16
    yscale = 0.19
  elif dom == 'SP':
    llcrnrlon = -125.0
    llcrnrlat = 45.0
    urcrnrlon = -119.5
    urcrnrlat = 49.2
    lat_0 = 46.0
    lon_0 = -120.0
    xscale = 0.19
    yscale = 0.18
  elif dom == 'SW':
    llcrnrlon = -125.0 
    llcrnrlat = 30.0
    urcrnrlon = -108.0
    urcrnrlat = 42.5
    lat_0 = 37.0
    lon_0 = -113.0
    xscale = 0.17
    yscale = 0.18
  elif dom == 'UM':
    llcrnrlon = -96.75 
    llcrnrlat = 39.75
    urcrnrlon = -81.0
    urcrnrlat = 49.0
    lat_0 = 44.0
    lon_0 = -91.5
    xscale = 0.18
    yscale = 0.18

  # Create basemap instance and set the dimensions
  for ax in axes:
    if dom == 'BN' or dom == 'LA' or dom == 'SF' or dom == 'SP':
      m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='h')
    else:
      m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
##  parallels = np.arange(0.,90.,10.)
##  map.drawparallels(parallels,labels=[1,0,0,0],fontsize=6)
##  meridians = np.arange(180.,360.,10.)
##  map.drawmeridians(meridians,labels=[0,0,0,1],fontsize=6)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

  # Map/figure has been set up here, save axes instances for use again later
    if par == 1:
      keep_ax_lst_1 = ax.get_children()[:]
    elif par == 2:
      keep_ax_lst_2 = ax.get_children()[:]
    elif par == 3:
      keep_ax_lst_3 = ax.get_children()[:]

    par += 1
  par = 1


#################################
  # Plot Terrain with 10-m WSPD
#################################
  t1 = time.clock()
  print(('Working on Terrain for '+dom))

  units = 'ft'
  if dom == 'conus':
    skip = 80
    barblength = 4.
  elif dom == 'CO' or dom == 'LA':
    skip = 12
    barblength = 4.5
  elif dom == 'BN':
    skip = 10
    barblength = 4.5
  elif dom == 'SP':
    skip = 9
    barblength = 4.5
  elif dom == 'SF':
    skip = 3
    barblength = 4.5
  else:
    skip = 40
    barblength = 4.5
  clevs = [1,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,7250,7500,7750,8000,8250,8500,8750,9000,9250,9500,9750,10000]
  clevsdif = [-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300]
  cm = cmap_terra()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  # Rotate winds to gnomonic projection
  urot_1, vrot_1 = m.rotate_vector(uwind_1,vwind_1,lon,lat)
  urot_2, vrot_2 = m.rotate_vector(uwind_2,vwind_2,lon2,lat2)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,terra_1,clevs,cmap=cm,norm=norm,extend='max',ax=ax)
      cs_1.cmap.set_over('ghostwhite')
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS Terrain Height ('+units+') and 10-m Winds (kts) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,terra_2,clevs,cmap=cm,norm=norm,extend='max',ax=ax)
      cs_2.cmap.set_over('ghostwhite')
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL Terrain Height ('+units+') and 10-m Winds (kts) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,terra_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Terrain Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareterra_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareterra_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot Terrain for: '+dom) % t3)


#################################
  # Plot soil type
#################################
#  if (fhr == 0):
#  t1 = time.clock()
#  print('Working on soil type for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'Integer(0-16)'
#  clevs = [-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5]
#  clevsdif = [-0.1,0.1]
#  colorlist = ['#00CDCD','saddlebrown','khaki','gray','#3D9140','palegreen','firebrick','lightcoral','darkorchid','plum','blue','lightskyblue','#CDAD00','yellow','#FF4500','lightsalmon','#CD1076']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,sotyp_1,clevs,colors=colorlist,ax=ax)
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA ANL Soil Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,sotyp_2,clevs,colors=colorlist,ax=ax)
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA FGS Soil Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,sotyp_dif,clevsdif,colors=difcolors2,extend='both',ax=ax)
#      cs.cmap.set_under('darkred')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=[0])
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Soil Type ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparesotyp_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparesotyp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot soil type for: '+dom) % t3


#################################
  # Plot vegetation type
#################################
#  if (fhr == 0):
#  t1 = time.clock()
#  print('Working on vegetation type for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'Integer(0-19)'
#  clevs = [-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5]
#  clevsdif = [-0.1,0.1]
#  colorlist = ['#00CDCD','saddlebrown','khaki','gray','#3D9140','palegreen','firebrick','lightcoral','darkorchid','plum','blue','lightskyblue','#CDAD00','yellow','#FF4500','lightsalmon','#CD1076']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,vgtyp_1,clevs,colors=colorlist,ax=ax)
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA ANL Vegetation Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,vgtyp_2,clevs,colors=colorlist,ax=ax)
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA FGS Vegetation Type \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,vgtyp_dif,clevsdif,colors=difcolors2,extend='both',ax=ax)
#      cs.cmap.set_under('darkred')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=[0])
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
      #ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Vegetation Type ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparevgtyp_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparevgtyp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot vegetation type for: '+dom) % t3


#################################
  # Plot vegetation fraction
#################################
#  t1 = time.clock()
#  print('Working on vegetation fraction for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = '%'
#  clevs = [10,20,30,40,50,60,70,80,90,100]
#  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
#  cm = ncepy.cmap_q2m()
#  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,veg_1,clevs,cmap=cm,extend='min',ax=ax)
#     cs_1.cmap.set_under('white')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x,y,veg_2,clevs,cmap=cm,extend='min',ax=ax)
#      cs_2.cmap.set_under('white')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,veg_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Vegetation Fraction ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('compareveg_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./compareveg_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot vegetation fraction for: '+dom) % t3


#################################
  # Plot 0-10cm soil temperature
#################################
  t1 = time.clock()
  print(('Working on 0-10cm soil temperature for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '\xb0''F'
  clevs = np.linspace(-36,104,36)
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  cm = cmap_t2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      mysoilt = maskoceans(lon,lat,tsoil_0_10_1,inlands=True,resolution='l')
      cs_1 = m.contourf(lon,lat,mysoilt,clevs,cmap=cm,latlon=True,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 0-10 cm Soil Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      mysoilt = maskoceans(lon2,lat2,tsoil_0_10_2,inlands=True,resolution='l')
      cs_2 = m.contourf(lon2,lat2,mysoilt,clevs,cmap=cm,latlon=True,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 0-10 cm Soil Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      mysoilt = maskoceans(lon2,lat2,tsoil_0_10_dif,inlands=True,resolution='l')
      cs = m.contourf(lon2,lat2,mysoilt,clevsdif,colors=difcolors,latlon=True,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 0-10 cm Soil Temperature ('+units+') \n valid: '+vtime ,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparetsoil_0_10_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparetsoil_0_10_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot 0-10 cm soil temperature for: '+dom) % t3)

#################################
  # Plot lowest model level cloud water
#################################
  t1 = time.clock()
  print(('Working on lowest model level cloud water for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'g/kg'
  clevs = [0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2]
  clevsref = [20,1000]
  clevsw = [-1000,1]
  clevsdif = [-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      csw_1 = m.contourf(x,y,clwmr_1,clevsw,colors='white',ax=ax)
#      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey',ax=ax)
      cs_1 = m.contourf(x,y,clwmr_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.set_xticklabels(clevs)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Lowest Mdl Lvl Cld Water ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      csw_2 = m.contourf(x,y,clwmr_2,clevsw,colors='white',ax=ax)
#      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey',ax=ax)
      cs_2 = m.contourf(x2,y2,clwmr_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.set_xticklabels(clevs)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Lowest Mdl Lvl Cld Water ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,clwmr_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Lowest Mdl Lvl Cld Water ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareclwmr_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareclwmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot lowest model level cloud water for: '+dom) % t3)

#################################
  # Plot lowest model level cloud ice
#################################
#  t1 = time.clock()
#  print('Working on lowest model level cloud ice for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      csw_1 = m.contourf(x,y,icmr_1,clevsw,colors='white',ax=ax)
#      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey',ax=ax)
#      cs_1 = m.contourf(x,y,icmr_1,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_1.cmap.set_over('hotpink')
#      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5,ax=ax)
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.set_xticklabels(clevs)
#      cbar1.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA ANL Lowest Mdl Lvl Cld Ice ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      csw_2 = m.contourf(x2,y2,icmr_2,clevsw,colors='white',ax=ax)
#      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey',ax=ax)
#      cs_2 = m.contourf(x2,y2,icmr_2,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_2.cmap.set_over('hotpink')
#      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5,ax=ax)
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.set_xticklabels(clevs)
#      cbar2.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA FGS Lowest Mdl Lvl Cld Ice ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,icmr_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Lowest Mdl Lvl Cld Ice ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1
  
#  compress_and_save('compareicmr_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./compareicmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot lowest model level cloud ice for: '+dom) % t3

#################################
  # Plot lowest model level rain
#################################
  t1 = time.clock()
  print(('Working on lowest model level rain for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      csw_1 = m.contourf(x,y,rwmr_1,clevsw,colors='white',ax=ax)
#      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey',ax=ax)
      cs_1 = m.contourf(x,y,rwmr_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.set_xticklabels(clevs)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Lowest Mdl Lvl Rain ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      csw_2 = m.contourf(x2,y2,rwmr_2,clevsw,colors='white',ax=ax)
#      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey',ax=ax)
      cs_2 = m.contourf(x2,y2,rwmr_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.set_xticklabels(clevs)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Lowest Mdl Lvl Rain ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,rwmr_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Lowest Mdl Lvl Rain ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparerwmr_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparerwmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot lowest model level rain for: '+dom) % t3)

#################################
  # Plot lowest model level snow
#################################
  t1 = time.clock()
  print(('Working on lowest model level snow for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      csw_1 = m.contourf(x,y,snmr_1,clevsw,colors='white',ax=ax)
#      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey',ax=ax)
      cs_1 = m.contourf(x,y,snmr_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('hotpink')
      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.set_xticklabels(clevs)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Lowest Mdl Lvl Snow ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      csw_2 = m.contourf(x2,y2,snmr_2,clevsw,colors='white',ax=ax)
#      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey',ax=ax)
      cs_2 = m.contourf(x2,y2,snmr_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('hotpink')
      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.set_xticklabels(clevs)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Lowest Mdl Lvl Snow ('+units+'), Reflectivity (gray), 0''\xb0''C line (red) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,snmr_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Lowest Mdl Lvl Snow ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparesnmr_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparesnmr_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot lowest model level snow for: '+dom) % t3)

#################################
  # Plot lowest model level graupel
#################################
#  t1 = time.clock()
#  print('Working on lowest model level graupel for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      csw_1 = m.contourf(x,y,grle_1,clevsw,colors='white',ax=ax)
#      csref_1 = m.contourf(x,y,refd_1,clevsref,colors='Grey',ax=ax)
#      cs_1 = m.contourf(x,y,grle_1,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_1.cmap.set_over('hotpink')
#      cstmp_1 = m.contour(x,y,tmphyb_1,[0],colors='red',linewidths=0.5,ax=ax)
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.set_xticklabels(clevs)
#      cbar1.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA ANL Lowest Mdl Lvl Graupel ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      csw_2 = m.contourf(x2,y2,grle_2,clevsw,colors='white',ax=ax)
#      csref_2 = m.contourf(x2,y2,refd_2,clevsref,colors='Grey',ax=ax)
#      cs_2 = m.contourf(x2,y2,grle_2,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_2.cmap.set_over('hotpink')
#      cstmp_2 = m.contour(x2,y2,tmphyb_2,[0],colors='red',linewidths=0.5,ax=ax)
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.set_xticklabels(clevs)
#      cbar2.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA FGS Lowest Mdl Lvl Graupel ('+units+'), Reflectivity (gray), 0'u'\xb0''C line (red) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,grle_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Lowest Mdl Lvl Graupel ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparegrle_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparegrle_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot lowest model level graupel for: '+dom) % t3


#################################
  # Plot downward shortwave
#################################
  t1 = time.clock()
  print(('Working on downward shortwave for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = np.arange(0,1025,25)
  clevsdif = [-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,swdown_1,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Downward Shortwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,swdown_2,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Downward Shortwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,swdown_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Downward Shortwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareswdown_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareswdown_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot downward shortwave for: '+dom) % t3)

#################################
  # Plot upward shortwave
#################################
  t1 = time.clock()
  print(('Working on upward shortwave for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = np.arange(0,525,25)
  clevsdif = [-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,swup_1,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Upward Shortwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,swup_2,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Upward Shortwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,swup_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Upward Shortwave Flux ('+units+') valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareswup_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareswup_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot upward shortwave for: '+dom) % t3)

#################################
  # Plot downward longwave
#################################
  t1 = time.clock()
  print(('Working on downward longwave for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = np.arange(0,525,25)
  clevsdif = [-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,lwdown_1,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Downward Longwave Flux ('+units+') \n  valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,lwdown_2,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Downward Longwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,lwdown_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Downward Longwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparelwdown_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparelwdown_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot downward longwave for: '+dom) % t3)

#################################
  # Plot upward longwave
#################################
  t1 = time.clock()
  print(('Working on upward longwave for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = np.arange(0,525,25)
  clevsdif = [-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,lwup_1,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Upward Longwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,lwup_2,clevs,cmap=plt.get_cmap(name='Spectral_r'),extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Upward Longwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,lwup_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Upward Longwave Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparelwup_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparelwup_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot upward longwave for: '+dom) % t3)

#################################
  # Plot ground heat flux
#################################
  t1 = time.clock()
  print(('Working on ground heat flux for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = [-300,-200,-100,-75,-50,-25,-10,0,10,25,50,75,100,200,300]
  clevsdif = [-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60]
  cm = ncepy.ncl_grnd_hflux()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,gdhfx_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Ground Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,gdhfx_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,ticks=clevs,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Ground Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,gdhfx_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Ground Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparegdhfx_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparegdhfx_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot ground heat flux for: '+dom) % t3)

#################################
  # Plot latent heat flux
#################################
  t1 = time.clock()
  print(('Working on latent heat flux for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = [-2000,-1500,-1000,-750,-500,-300,-200,-100,-75,-50,-25,0,25,50,75,100,200,300,500,750,1000,1500,2000]
  clevsdif = [-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150]
  cm = ncepy.ncl_grnd_hflux()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,lhfx_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,ticks=[-2000,-500,-100,-50,0,50,100,500,1000,2000],location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Latent Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,lhfx_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,ticks=[-2000,-500,-100,-50,0,50,100,500,1000,2000],location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Latent Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,lhfx_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Latent Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparelhfx_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparelhfx_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot latent heat flux for: '+dom) % t3)

#################################
  # Plot sensible heat flux
#################################
  t1 = time.clock()
  print(('Working on sensible heat flux for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'W m${^{-2}}$'
  clevs = [-2000,-1500,-1000,-750,-500,-300,-200,-100,-75,-50,-25,0,25,50,75,100,200,300,500,750,1000,1500,2000]
  clevsdif = [-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150]
  cm = ncepy.ncl_grnd_hflux()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,snhfx_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,ticks=[-2000,-500,-100,-50,0,50,100,500,1000,2000],location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA FGS Sensible Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,snhfx_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,ticks=[-2000,-500,-100,-50,0,50,100,500,1000,2000],location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=5)
      ax.text(.5,1.03,'3D-RTMA ANL Sensible Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,snhfx_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Sensible Heat Flux ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparesnhfx_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparesnhfx_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot sensible heat flux for: '+dom) % t3)

#################################
  # Plot PBL height
#################################
#  t1 = time.clock()
#  print('Working on PBL height for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'm'
#  clevs = [50,100,250,500,1000,1500,2000,2500,3000,3500,4000,4500,5000]
#  clevsdif = [-1800,-1500,-1200,-900,-600,-300,0,300,600,900,1200,1500,1800]
#  colorlist= ['gray','blue','dodgerblue','cyan','mediumspringgreen','#FAFAD2','#EEEE00','#EEC900','darkorange','crimson','darkred','darkviolet']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,hpbl_1,clevs,colors=colorlist,extend='both',ax=ax)
#      cs_1.cmap.set_under('white')
#      cs_1.cmap.set_over('black')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA ANL PBL Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,hpbl_2,clevs,colors=colorlist,extend='both',ax=ax)
#      cs_2.cmap.set_under('white')
#      cs_2.cmap.set_over('black')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=5)
#      ax.text(.5,1.03,'3D-RTMA FGS PBL Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,hpbl_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL PBL Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparehpbl_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparehpbl_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot PBL height for: '+dom) % t3

#################################
  # Plot total column condensate
#################################
#  t1 = time.clock()
#  print('Working on Total condensate for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'kg m${^{-2}}$'
#  clevs = [0.001,0.005,0.01,0.05,0.1,0.25,0.5,1,2,4,6,10,15,20,25]
#  clevsdif = [-6,-4,-2,-1,-0.5,-0.25,0,0.25,0.5,1,2,4,6]
#  q_color_list = plt.cm.gist_stern_r(np.linspace(0, 1, len(clevs)+1))
#  cm = matplotlib.colors.ListedColormap(q_color_list)
#  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,cond_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_1.cmap.set_under('white')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Total Column Condensate ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,cond_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_2.cmap.set_under('white')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Total Column Condensate ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,cond_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Total Column Condensate ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparecond_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparecond_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Total condensate for: '+dom) % t3

#################################
  # Plot total column liquid
#################################
#  t1 = time.clock()
#  print('Working on Total column liquid for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,tcolw_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_1.cmap.set_under('white')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Total Column Cloud Water + Rain ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,tcolw_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_2.cmap.set_under('white')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Total Column Cloud Water + Rain ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,cond_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Total Column Cloud Water + Rain ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparetcolw_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparetcolw_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Total column liquid for: '+dom) % t3

#################################
  # Plot total column ice
#################################
#  t1 = time.clock()
#  print('Working on Tcoli for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,tcoli_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_1.cmap.set_under('white')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Total Column Cloud Ice + Snow ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,tcoli_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
#      cs_2.cmap.set_under('white')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.set_xticklabels([0.001,0.01,0.1,0.5,2,6,15,25])
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Total Column Cloud Ice + Snow ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,cond_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Total Column Cloud Ice + Snow ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparetcoli_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparetcoli_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Tcoli for: '+dom) % t3

#################################
  # Plot 0-3 km Storm Relative Helicity
#################################
  t1 = time.clock()
  print(('Working on 0-3 km SRH for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'm${^2}$ s$^{-2}$'
  clevs = [50,100,150,200,250,300,400,500,600,700,800]
  clevsdif = [-120,-100,-80,-60,-40,-20,0,20,40,60,80,100,120]
  colorlist = ['mediumblue','dodgerblue','chartreuse','limegreen','darkgreen','#EEEE00','orange','orangered','firebrick','darkmagenta']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,hel3km_1,clevs,colors=colorlist,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 0-3 km Storm Relative Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,hel3km_2,clevs,colors=colorlist,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 0-3 km Storm Relative Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,hel3km_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 0-3 km Storm Relative Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparehel3km_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparehel3km_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot 0-3 km SRH for: '+dom) % t3)

#################################
  # Plot 0-1 km Storm Relative Helicity
#################################
  t1 = time.clock()
  print(('Working on 0-1 km SRH for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,hel1km_1,clevs,colors=colorlist,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 0-1 km Storm Relative Helicity ('+units+') \n  valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,hel1km_2,clevs,colors=colorlist,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 0-1 km Storm Relative Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,hel1km_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 0-1 km Storm Relative Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparehel1km_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot 0-1 km SRH for: '+dom) % t3)

######################################################

  t3dom = round(t2-t1dom, 3)
  print(("%.3f seconds to plot all variables for: "+dom) % t3dom)
  plt.clf()

######################################################

main()
