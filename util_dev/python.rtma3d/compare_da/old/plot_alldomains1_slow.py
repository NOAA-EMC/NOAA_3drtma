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

if (fhr > 2):
  data1_m1 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour1+'.grib2')
  data2_m1 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour1+'.grib2')
  data1_m2 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour2+'.grib2')
  data2_m2 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour2+'.grib2')
if (fhr >= 6):
  data1_m6 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour6+'.grib2')
  data2_m6 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour6+'.grib2')
if (fhr >= 24):
  data1_m24 = pygrib.open('/gpfs/dell1/ptmp/Benjamin.Blake/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour24+'.grib2')
  data2_m24 = pygrib.open('/gpfs/dell2/ptmp/Eric.Rogers/com/fv3sar/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour24+'.grib2')

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


# Sea level pressure
slp_1 = data1.select(name='Pressure reduced to MSL')[0].values * 0.01
slpsmooth1 = ndimage.filters.gaussian_filter(slp_1, 13.78)
slp_2 = data2.select(name='Pressure reduced to MSL')[0].values * 0.01
slpsmooth2 = ndimage.filters.gaussian_filter(slp_2, 13.78)
slp_dif = slp_2 - slp_1

# 2-m temperature
tmp2m_1 = data1.select(name='2 metre temperature')[0].values
tmp2m_1 = (tmp2m_1 - 273.15)*1.8 + 32.0
tmp2m_2 = data2.select(name='2 metre temperature')[0].values
tmp2m_2 = (tmp2m_2 - 273.15)*1.8 + 32.0
tmp2m_dif = tmp2m_2 - tmp2m_1

# Surface temperature
tmpsfc_1 = data1.select(name='Temperature',typeOfLevel='surface')[0].values
tmpsfc_1 = (tmpsfc_1 - 273.15)*1.8 + 32.0
tmpsfc_2 = data2.select(name='Temperature',typeOfLevel='surface')[0].values
tmpsfc_2 = (tmpsfc_2 - 273.15)*1.8 + 32.0
tmpsfc_dif = tmpsfc_2 - tmpsfc_1

# 2-m specific humidity
#spfh2m_1 = data1.select(name='Specific humidity',level=2)[0].values * 1000
#spfh2m_2 = data2.select(name='Specific humidity',level=2)[0].values * 1000
#spfh2m_dif = spfh2m_2 - spfh2m_1
# 2-m dew point temperature
dew2m_1 = data1.select(name='2 metre dewpoint temperature')[0].values
dew2m_1 = (dew2m_1 - 273.15)*1.8 + 32.0
dew2m_2 = data2.select(name='2 metre dewpoint temperature')[0].values
dew2m_2 = (dew2m_2 - 273.15)*1.8 + 32.0
dew2m_dif = dew2m_2 - dew2m_1

# 10-m winds and terrain height
uwind_1 = data1.select(name='10 metre U wind component')[0].values * 1.94384
uwind_2 = data2.select(name='10 metre U wind component')[0].values * 1.94384
vwind_1 = data1.select(name='10 metre V wind component')[0].values * 1.94384
vwind_2 = data2.select(name='10 metre V wind component')[0].values * 1.94384
wspd10m_1 = np.sqrt(uwind_1**2 + vwind_1**2)
wspd10m_2 = np.sqrt(uwind_2**2 + vwind_2**2)
wspd10m_dif = wspd10m_2 - wspd10m_1
#terra = regdata.select(name='Orography')[0].values * 3.28084

# Most unstable CAPE
mucape_1 = data1.select(name='Convective available potential energy',topLevel=18000)[0].values
mucape_2 = data2.select(name='Convective available potential energy',topLevel=18000)[0].values
mucape_dif = mucape_2 - mucape_1

# Surface-based CAPE
cape_1 = data1.select(name='Convective available potential energy',typeOfLevel='surface')[0].values
cape_2 = data2.select(name='Convective available potential energy',typeOfLevel='surface')[0].values
cape_dif = cape_2 - cape_1

# Surface-based CIN
cin_1 = data1.select(name='Convective inhibition',typeOfLevel='surface')[0].values
cin_2 = data2.select(name='Convective inhibition',typeOfLevel='surface')[0].values
cin_dif = cin_2 - cin_1

# 850-mb equivalent potential temperature
t850_1 = data1.select(name='Temperature',level=850)[0].values
rh850_1 = data1.select(name='Relative humidity',level=850)[0].values
es_1 = 6.11*np.exp((53.49*np.ones(t850_1.shape))-((6808.*np.ones(t850_1.shape))/t850_1)-5.09*np.log(t850_1))
ws_1 = 0.622*(es_1/850.)
thetae_1 = (t850_1+((2450000/1005.7)*((rh850_1/100)*ws_1))) * (1000/850)**(0.2854)
t850_2 = data2.select(name='Temperature',level=850)[0].values
rh850_2 = data2.select(name='Relative humidity',level=850)[0].values
es_2 = 6.11*np.exp((53.49*np.ones(t850_2.shape))-((6808.*np.ones(t850_2.shape))/t850_2)-5.09*np.log(t850_2))
ws_2 = 0.622*(es_2/850.)
thetae_2 = (t850_2+((2450000/1005.7)*((rh850_2/100)*ws_2))) * (1000/850)**(0.2854)
thetae_dif = thetae_2 - thetae_1

# 700-mb omega and relative humidity
omg700_1 = data1.select(name='Vertical velocity',level=700)[0].values
omg700_2 = data2.select(name='Vertical velocity',level=700)[0].values
rh700_1 = data1.select(name='Relative humidity',level=700)[0].values
rh700_2 = data2.select(name='Relative humidity',level=700)[0].values
rh700_dif = rh700_2 - rh700_1

# 500 mb height, wind, vorticity
z500_1 = data1.select(name='Geopotential Height',level=500)[0].values * 0.1
z500_1 = ndimage.filters.gaussian_filter(z500_1, 6.89)
z500_2 = data2.select(name='Geopotential Height',level=500)[0].values * 0.1
z500_2 = ndimage.filters.gaussian_filter(z500_2, 6.89)
z500_dif = z500_2 - z500_1
vort500_1 = data1.select(name='Absolute vorticity',level=500)[0].values * 100000
vort500_1 = ndimage.filters.gaussian_filter(vort500_1,1.7225)
vort500_2 = data2.select(name='Absolute vorticity',level=500)[0].values * 100000
vort500_2 = ndimage.filters.gaussian_filter(vort500_2,1.7225)
u500_1 = data1.select(name='U component of wind',level=500)[0].values * 1.94384
u500_2 = data2.select(name='U component of wind',level=500)[0].values * 1.94384
v500_1 = data1.select(name='V component of wind',level=500)[0].values * 1.94384
v500_2 = data2.select(name='V component of wind',level=500)[0].values * 1.94384

# 250 mb winds
u250_1 = data1.select(name='U component of wind',level=250)[0].values * 1.94384
u250_2 = data2.select(name='U component of wind',level=250)[0].values * 1.94384
v250_1 = data1.select(name='V component of wind',level=250)[0].values * 1.94384
v250_2 = data2.select(name='V component of wind',level=250)[0].values * 1.94384
wspd250_1 = np.sqrt(u250_1**2 + v250_1**2)
wspd250_2 = np.sqrt(u250_2**2 + v250_2**2)
wspd250_dif = wspd250_2 - wspd250_1

# Visibility
visemc_1 = data1.select(name='Visibility',typeOfLevel='surface')[0].values * 0.000621371
visemc_2 = data2.select(name='Visibility',typeOfLevel='surface')[0].values * 0.000621371
visemc_dif = visemc_2 - visemc_1

visgsd_1 = data1.select(name='Visibility',typeOfLevel='cloudTop')[0].values * 0.000621371
visgsd_2 = data2.select(name='Visibility',typeOfLevel='cloudTop')[0].values * 0.000621371
visgsd_dif = visgsd_2 - visgsd_1

# Cloud Base Height
zbase_1 = data1.select(name='Geopotential Height',typeOfLevel='cloudBase')[0].values * (3.28084/1000)
zbase_2 = data2.select(name='Geopotential Height',typeOfLevel='cloudBase')[0].values * (3.28084/1000)
zbase_dif = zbase_2 - zbase_1

# Cloud Top Height
ztop_1 = data1.select(name='Geopotential Height',typeOfLevel='cloudTop')[0].values * (3.28084/1000)
ztop_2 = data2.select(name='Geopotential Height',typeOfLevel='cloudTop')[0].values * (3.28084/1000)
ztop_dif = ztop_2 - ztop_1

# Precipitable water
pw_1 = data1.select(name='Precipitable water',level=0)[0].values * 0.0393701
pw_2 = data2.select(name='Precipitable water',level=0)[0].values * 0.0393701
pw_dif = pw_2 - pw_1

# Percent of frozen precipitation
pofp_1 = data1.select(name='Percent frozen precipitation')[0].values
pofp_2 = data2.select(name='Percent frozen precipitation')[0].values
pofp_dif = pofp_2 - pofp_1

# Total precipitation
qpf_1 = data1.select(name='Total Precipitation',lengthOfTimeRange=fhr)[0].values * 0.0393701
qpf_2 = data2.select(name='Total Precipitation',lengthOfTimeRange=fhr)[0].values * 0.0393701
qpf_dif = qpf_2 - qpf_1

# 3-hr precipitation
if (fhr > 2):  # Do not make 3-hr plots for forecast hours 1 and 2
  qpfm2_1 = data1_m2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpfm1_1 = data1_m1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpfm0_1 = data1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpf3_1 = qpfm2_1 + qpfm1_1 + qpfm0_1
  qpfm2_2 = data2_m2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpfm1_2 = data2_m1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpfm0_2 = data2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
  qpf3_2 = qpfm2_2 + qpfm1_2 + qpfm0_2
  qpf3_dif = qpf3_2 - qpf3_1

# Snow depth
snow_1 = data1.select(name='Snow depth')[0].values * 39.3701
snow_2 = data2.select(name='Snow depth')[0].values * 39.3701
snow_dif = snow_2 - snow_1
if (fhr >=6):	# Do not make 6-hr plots for forecast hours less than 6
  snowm6_1 = data1_m6.select(name='Snow depth')[0].values * 39.3701
  snow6_1 = snow_1 - snowm6_1 
  snowm6_2 = data2_m6.select(name='Snow depth')[0].values * 39.3701
  snow6_2 = snow_2 - snowm6_2
  snow6_dif = snow6_2 - snow6_1



t2a = time.clock()
t3a = round(t2a-t1a, 3)
print("%.3f seconds to read all messages") % t3a

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
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

################################
  # Plot SLP
################################
  t1 = time.clock()
  print('Working on slp for '+dom)

  units = 'mb'
  clevs = [976,980,984,988,992,996,1000,1004,1008,1012,1016,1020,1024,1028,1032,1036,1040,1044,1048,1052]
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]

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
      cs1_a = m.contourf(x,y,slpsmooth1,clevs,cmap=plt.cm.Spectral_r,extend='both')  
      cbar = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      cs1_b = m.contour(x,y,slpsmooth1,np.arange(940,1060,4),colors='black',linewidths=1.25)
      plt.clabel(cs1_b,np.arange(940,1060,4),inline=1,fmt='%d',fontsize=6,zorder=12)
      ax.text(.5,1.03,'FV3 SLP ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
  # plot highs and lows - window parameter controls the number of highs and lows detected
      ncepy.plt_highs_and_lows(m,slp_1,lon,lat,mode='reflect',window='500')

    elif par == 2:
      cs2_a = m.contourf(x2,y2,slpsmooth2,clevs,cmap=plt.cm.Spectral_r,extend='both')  
      cbar = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      cs2_b = m.contour(x2,y2,slpsmooth2,np.arange(940,1060,4),colors='black',linewidths=1.25)
      plt.clabel(cs2_b,np.arange(940,1060,4),inline=1,fmt='%d',fontsize=6,zorder=12)
      ax.text(.5,1.03,'FV3-DA SLP ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
  # plot highs and lows - window parameter controls the number of highs and lows detected
      ncepy.plt_highs_and_lows(m,slp_2,lon,lat,mode='reflect',window='500')

    elif par == 3:
      cs = m.contourf(x2,y2,slp_dif,clevsdif,colors=difcolors,extend='both')  
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 SLP ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compareslp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot slp for: '+dom) % t3


#################################
  # Plot 2-m T
#################################
  t1 = time.clock()
  print('Working on t2m for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-16,134,51)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
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
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,tmp2m_1,clevs,cmap=cm,extend='both')
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 2-m Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
 
    elif par == 2:
      cs_2 = m.contourf(x2,y2,tmp2m_2,clevs,cmap=cm,extend='both')
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA 2-m Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    elif par == 3:
      cs = m.contourf(x2,y2,tmp2m_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 2-m Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare2mt_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 2mt for: '+dom) % t3


#################################
# Plot SFCT
#################################
  t1 = time.clock()
  print('Working on tsfc for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-16,134,51)
  clevsdif = [-18,-15,-12,-9,-6,-3,0,3,6,9,12,15,18]
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
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,tmpsfc_1,clevs,cmap=cm,extend='both')
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Surface Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,tmpsfc_2,clevs,cmap=cm,extend='both')
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Surface Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,tmpsfc_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Surface Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparetsfc_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()
  
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot tsfc for: '+dom) % t3

#################################
  # Plot 2-m Dew Point
#################################
  t1 = time.clock()
  print('Working on 2mdew for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-5,80,35)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
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
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,dew2m_1,clevs,cmap=cm,extend='both')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 2-m Dew Point Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,dew2m_2,clevs,cmap=cm,extend='both')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA 2-m Dew Point Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,dew2m_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 2-m Dew Point Temperature ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare2mdew_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 2mdew for: '+dom) % t3

#################################
  # Plot 10-m WSPD with Terrain
#################################
  t1 = time.clock()
  print('Working on 10mwspd for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kts'
  if dom == 'conus':
    skip = 80
    barblength = 4.
  else:
    skip = 40
    barblength = 4.5
#  clevs = [1,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,7250,7500,7750,8000,8250,8500,8750,9000,9250,9500,9750,10000]
  clevs = [5,10,15,20,25,30,35,40,45,50]
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  colorlist = ['turquoise','dodgerblue','blue','#FFF68F','#E3CF57','peru','brown','crimson','red']
#  cm = cmap_terra()
#  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

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
      cs_1 = m.contourf(x,y,wspd10m_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('fuchsia')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],uwind_1[::skip,::skip],vwind_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3 10-m Winds ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,wspd10m_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('fuchsia')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],uwind_2[::skip,::skip],vwind_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3-DA 10-m Winds ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,wspd10m_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label('kts',fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 10-m Wind Speed (kts) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare10mwind_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 10mwspd for: '+dom) % t3

#################################
  # Plot Most Unstable CAPE
#################################
  t1 = time.clock()
  print('Working on sfcape for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'J/kg'
  clevs = [100,250,500,1000,1500,2000,2500,3000,3500,4000,4500,5000]
  clevsdif = [-2000,-1500,-1000,-500,-250,-100,0,100,250,500,1000,1500,2000]
  colorlist = ['dodgerblue','darkturquoise','cyan','mediumspringgreen','#FFF68F','#EEEE00','#EEC900','darkorange','red','crimson','darkred']

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
      cs_1 = m.contourf(x,y,mucape_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('darkviolet')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3 Most Unstable CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,mucape_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('darkviolet')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3-DA Most Unstable CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,mucape_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3-DA - FV3 Most Unstable CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparemucape_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot mucape for: '+dom) % t3

#################################
  # Plot Surface-Based CAPE
#################################
  t1 = time.clock()
  print('Working on sfcape for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'J/kg'
  clevs = [100,250,500,1000,1500,2000,2500,3000,3500,4000,4500,5000]
  clevsdif = [-2000,-1500,-1000,-500,-250,-100,0,100,250,500,1000,1500,2000]
  colorlist = ['dodgerblue','darkturquoise','cyan','mediumspringgreen','#FFF68F','#EEEE00','#EEC900','darkorange','red','crimson','darkred']

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
      cs_1 = m.contourf(x,y,cape_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('darkviolet')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3 Surface-Based CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,cape_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('darkviolet')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3-DA Surface-Based CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,cape_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'FV3-DA - FV3 Surface-Based CAPE ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparesfcape_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot sfcape for: '+dom) % t3

#################################
  # Plot Surface-Based CIN
#################################
  t1 = time.clock()
  print('Working on sfcin for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units='J/kg'
  clevs = [-1000,-750,-500,-250,-100,-50,-25,-1]
  clevsdif = [-1000,-500,-250,-100,-50,-25,0,25,50,100,250,500,1000]
  colorlist = ['darkviolet','indigo','darkred','red','darkorange','gold','#FFF68F']

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
      cs_1 = m.contourf(x,y,cin_1,clevs,colors=colorlist,extend='min')
      cs_1.cmap.set_under('violet')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Surface-Based CIN ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,cin_2,clevs,colors=colorlist,extend='min')
      cs_2.cmap.set_under('violet')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Surface-Based CIN ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,cin_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Surface-Based CIN ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par =1

#  plt.tight_layout()
  plt.savefig('./comparecin_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot sfcin for: '+dom) % t3

#################################
  # Plot 850-mb THETAE
#################################
  t1 = time.clock()
  print('Working on 850 mb Theta-e for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'K'
  clevs = np.linspace(250,340,31)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  cm = cmap_t850()
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
      cs_1 = m.contourf(x,y,thetae_1,clevs,cmap=cm,extend='both')
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[250,256,262,268,274,280,286,292,298,304,310,316,322,328,334,340])
      cbar.set_label(units,fontsize=6)   
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 850 mb $\Theta$e ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,thetae_2,clevs,cmap=cm,extend='both')
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[250,256,262,268,274,280,286,292,298,304,310,316,322,328,334,340])
      cbar.set_label(units,fontsize=6)   
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA 850 mb $\Theta$e ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
    
    elif par == 3:
      cs = m.contourf(x2,y2,thetae_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)   
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 850 mb $\Theta$e ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare850t_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 850 mb Theta-e for: '+dom) % t3

#################################
  # Plot 700-mb OMEGA and RH
#################################
  t1 = time.clock()
  print('Working on 700 mb omega and RH for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [50,60,70,80,90,100]
  clevsw = [-100,-5]
  clevsdif = [-30,-25,-20,-15,-10,-5,-0,5,10,15,20,25,30]

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
      cs1_a = m.contourf(x,y,rh700_1,clevs,cmap=plt.cm.BuGn)
      cbar = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      cs1_b = m.contourf(x,y,omg700_1,clevsw,colors='blue')
      ax.text(.5,1.03,'FV3 700 mb $\omega$ (rising motion in blue) and RH ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs2_a = m.contourf(x2,y2,rh700_2,clevs,cmap=plt.cm.BuGn)
      cbar = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      cs2_b = m.contourf(x2,y2,omg700_2,clevsw,colors='blue')
      ax.text(.5,1.03,'FV3-DA 700 mb $\omega$ (rising motion in blue) and RH ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,rh700_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 700 mb RH ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare700_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 700 mb $\omega$ and RH for: '+dom) % t3

#################################
  # Plot 500 mb HGT/WIND/VORT
#################################
  t1 = time.clock()
  print('Working on 500 mb Hgt/Wind/Vort for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'x10${^5}$ s${^{-1}}$'
  if dom == 'conus':
    skip = 120
    barblength = 4.
  else:
    skip = 60
    barblength = 4.5
  vortlevs = [16,20,24,28,32,36,40]
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  colorlist = ['yellow','gold','goldenrod','orange','orangered','red']

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
    m.drawcoastlines(linewidth=0.75,color='saddlebrown')
    m.drawstates(linewidth=0.5,color='saddlebrown')
    m.drawcountries(linewidth=0.5,color='saddlebrown')
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs1_a = m.contourf(x,y,vort500_1,vortlevs,colors=colorlist,extend='both')
      cs1_a.cmap.set_under('white')
      cs1_a.cmap.set_over('darkred')
      cbar = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05,ticks=vortlevs)
      cbar.set_label(units,fontsize=6)   
      cbar.ax.tick_params(labelsize=6)

      # plot vorticity maxima as black X's
      local_max = extrema(vort500_1,mode='wrap',window=100)
      xhighs = lon[local_max]
      yhighs = lat[local_max]
      highvals = vort500_1[local_max]
      xyplotted = []
      # don't plot if there is already a X within dmin meters
      yoffset = 0.022*(m.ymax - m.ymin)
      dmin = yoffset
      for x,y,p in zip(xhighs, yhighs, highvals):
        if x < m.xmax and x > m.xmin and y < m.ymax and y > m.ymin and p > 35:
          dist = [np.sqrt((x-x0)**2+(y-y0)**2) for x0,y0 in xyplotted]
          if not dist or min(dist) > dmin:
            ax.text(x,y,'x',fontsize=6,fontweight='bold',\
                    ha='center',va='center',color='black',zorder=10)
            xyplotted.append((x,y))

      m.barbs(lon[::skip,::skip],lat[::skip,::skip],u500_1[::skip,::skip],v500_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='steelblue')
      x,y = m(lon,lat)	# need to redefine to avoid index error
      cs1_b = m.contour(x,y,z500_1,np.arange(486,600,6),colors='black',linewidths=1)
      plt.clabel(cs1_b,np.arange(486,600,6),inline_spacing=1,fmt='%d',fontsize=6,dorder=12)
      ax.text(.5,1.03,'FV3 500 mb Heights (dam), Winds (kts), and $\zeta$ ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs2_a = m.contourf(x2,y2,vort500_2,vortlevs,colors=colorlist,extend='both')
      cs2_a.cmap.set_under('white')
      cs2_a.cmap.set_over('darkred')
      cbar = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05,ticks=vortlevs)
      cbar.set_label(units,fontsize=6)   
      cbar.ax.tick_params(labelsize=6)

      # plot vorticity maxima as black X's
      local_max = extrema(vort500_2,mode='wrap',window=100)
      xhighs = lon[local_max]
      yhighs = lat[local_max]
      highvals = vort500_2[local_max]
      xyplotted = []
      # don't plot if there is already a X within dmin meters
      yoffset = 0.022*(m.ymax - m.ymin)
      dmin = yoffset
      for x,y,p in zip(xhighs, yhighs, highvals):
        if x < m.xmax and x > m.xmin and y < m.ymax and y > m.ymin and p > 35:
          dist = [np.sqrt((x-x0)**2+(y-y0)**2) for x0,y0 in xyplotted]
          if not dist or min(dist) > dmin:
            ax.text(x,y,'x',fontsize=6,fontweight='bold',\
                    ha='center',va='center',color='black',zorder=10)
            xyplotted.append((x,y))

      m.barbs(lon[::skip,::skip],lat[::skip,::skip],u500_2[::skip,::skip],v500_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='steelblue')
#      x,y = m(lon,lat)	# need to redefine to avoid index error
      cs2_b = m.contour(x2,y2,z500_2,np.arange(486,600,6),colors='black',linewidths=1)
      plt.clabel(cs2_b,np.arange(486,600,6),inline_spacing=1,fmt='%d',fontsize=6,dorder=12)
      ax.text(.5,1.03,'FV3-DA 500 mb Heights (dam), Winds (kts), and $\zeta$ ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,z500_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 500 mb Heights (dam) \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./compare500_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 500 mb Hgt/Wind/Vort for: '+dom) % t3

#################################
  # Plot 250 mb WIND
#################################
  t1 = time.clock()
  print('Working on 250 mb WIND for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kts'
  if dom == 'conus':  
    skip = 120
    barblength = 4.
  else:
    skip = 60
    barblength = 4.5
  clevs = [50,60,70,80,90,100,110,120,130,140,150]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  colorlist = ['turquoise','deepskyblue','dodgerblue','#1874CD','blue','beige','khaki','peru','brown','crimson']

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
      cs_1 = m.contourf(x,y,wspd250_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('red')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],u250_1[::skip,::skip],v250_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3 250 mb Winds ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,wspd250_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('red')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],u250_2[::skip,::skip],v250_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black')
      ax.text(.5,1.03,'FV3-DA 250 mb Winds ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,wspd250_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 250 mb Winds ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1
   
#  plt.tight_layout()
  plt.savefig('./compare250wind_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 250 mb WIND for: '+dom) % t3

#################################
  # Plot EMC Visibility
#################################
  t1 = time.clock()
  print('Working on EMC Surface Visibility for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'miles'
  clevs = [0.25,0.5,1,2,3,4,5,10]
  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15]
  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']

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
      cs_1 = m.contourf(x,y,visemc_1,clevs,colors=colorlist,extend='min')
      cs_1.cmap.set_under('firebrick')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 EMC Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,visemc_2,clevs,colors=colorlist,extend='min')
      cs_2.cmap.set_under('firebrick')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA EMC Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,visemc_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 EMC Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparevis_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot EMC Surface Visibility for: '+dom) % t3


#################################
  # Plot GSD Surface Visibility
#################################
  t1 = time.clock()
  print('Working on GSD Surface Visibility for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'miles'
  clevs = [0.25,0.5,1,2,3,4,5,10]
  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0.,2.5,5,7.5,10,12.5,15]
  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']

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
      cs_1 = m.contourf(x,y,visgsd_1,clevs,colors=colorlist,extend='min')
      cs_1.cmap.set_under('firebrick')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 GSD Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,visgsd_2,clevs,colors=colorlist,extend='min')
      cs_2.cmap.set_under('firebrick')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA GSD Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,visgsd_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 GSD Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparevis2_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot GSD Surface Visibility for: '+dom) % t3

#################################
  # Plot Cloud Base Height
#################################
  t1 = time.clock()
  print('Working on Cloud Base Height for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kft'
  clevs = [0,0.1,0.3,0.5,1,5,10,15,20,25,30,35,40]
  clevsdif = [-12,-10,-8,-6,-4,-2,0.,2,4,6,8,10,12]
  colorlist = ['firebrick','tomato','salmon','lightsalmon','goldenrod','khaki','gold','yellow','palegreen','mediumspringgreen','lime','limegreen']

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
      cs_1 = m.contourf(x,y,zbase_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('darkgreen')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Cloud Base Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,zbase_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('darkgreen')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Cloud Base Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,zbase_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Cloud Base Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

  plt.savefig('./comparezbase_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Cloud Base Height for: '+dom) % t3

#################################
  # Plot Cloud Top Height
#################################
  t1 = time.clock()
  print('Working on Cloud Top Height for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kft'
  clevs = [1,5,10,15,20,25,30,35,40,45,50]
  clevsdif = [-12,-10,-8,-6,-4,-2,0.,2,4,6,8,10,12]
  colorlist = ['firebrick','tomato','salmon','lightsalmon','goldenrod','yellow','palegreen','mediumspringgreen','lime','limegreen']

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
      cs_1 = m.contourf(x,y,ztop_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('darkgreen')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Cloud Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ztop_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('darkgreen')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Cloud Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,ztop_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Cloud Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

  plt.savefig('./compareztop_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Cloud Top Height for: '+dom) % t3

#################################
  # Plot PW
#################################
  t1 = time.clock()
  print('Working on PW for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'in'
  clevs = [0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25]
  clevsdif = [-1.25,-1,-.75,-.5,-.25,-.1,0.,.1,.25,.50,.75,1,1.25]
  colorlist = ['lightsalmon','khaki','palegreen','cyan','turquoise','cornflowerblue','mediumslateblue','darkorchid','deeppink']

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
      cs_1 = m.contourf(x,y,pw_1,clevs,colors=colorlist,extend='max')
      cs_1.cmap.set_over('hotpink')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Precipitable Water ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,pw_2,clevs,colors=colorlist,extend='max')
      cs_2.cmap.set_over('hotpink')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Precipitable Water ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,pw_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Precipitable Water ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

#  plt.tight_layout()
  plt.savefig('./comparepw_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot PW for: '+dom) % t3

#################################
  # Plot % FROZEN PRECIP
#################################
  t1 = time.clock()
  print('Working on PERCENT FROZEN PRECIP for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [10,20,30,40,50,60,70,80,90,100]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
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
    m.fillcontinents(color='LightGrey',zorder=0)
    m.drawcoastlines(linewidth=0.75)
    m.drawstates(linewidth=0.5)
    m.drawcountries(linewidth=0.5)
    x,y = m(lon,lat)
    x2,y2 = m(lon2,lat2)

    if par == 1:
      cs_1 = m.contourf(x,y,pofp_1,clevs,colors=colorlist)
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Percent of Frozen Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,pofp_2,clevs,colors=colorlist)
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Percent of Frozen Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,pofp_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar.set_label(units,fontsize=6) 
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Percent of Frozen Precipitaion ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    par += 1
  par = 1

  plt.savefig('./comparepofp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot PERCENT FROZEN PRECIP for: '+dom) % t3


#################################
  # Plot Total QPF
#################################
  if (fhr > 0):
    t1 = time.clock()
    print('Working on total qpf for '+dom)

    # Clear off old plottables but keep all the map info
#    ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#    ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#    ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

    units = 'in'
    clevs = [0.01,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]
    clevsdif = [-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5]
    colorlist = ['chartreuse','limegreen','green','blue','dodgerblue','deepskyblue','cyan','mediumpurple','mediumorchid','darkmagenta','darkred','crimson','orangered','darkorange','goldenrod','gold','yellow']  

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
        cs_1 = m.contourf(x,y,qpf_1,clevs,colors=colorlist,extend='max')
        cs_1.cmap.set_over('pink')
        cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3 '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 2:
        cs_2 = m.contourf(x2,y2,qpf_2,clevs,colors=colorlist,extend='max')
        cs_2.cmap.set_over('pink')
        cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 3:
        cs = m.contourf(x2,y2,qpf_dif,clevsdif,colors=difcolors,extend='both')
        cs.cmap.set_under('darkblue')
        cs.cmap.set_over('darkred')
        cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA - FV3 '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

      par += 1
    par = 1

    plt.savefig('./compareqpf_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
    plt.close()

    t2 = time.clock()
    t3 = round(t2-t1, 3)
    print('%.3f seconds to plot total qpf for: '+dom) % t3

#################################
  # Plot QPF3
#################################
  if (fhr % 3 == 0) and (fhr > 0):
    t1 = time.clock()
    print('Working on qpf3 for '+dom)

    # Clear off old plottables but keep all the map info
#    ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#    ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#    ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

    units = 'in'
    clevs = [0.01,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]
    clevsdif = [-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5]
    colorlist = ['chartreuse','limegreen','green','blue','dodgerblue','deepskyblue','cyan','mediumpurple','mediumorchid','darkmagenta','darkred','crimson','orangered','darkorange','goldenrod','gold','yellow']  
   
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
        cs_1 = m.contourf(x,y,qpf3_1,clevs,colors=colorlist,extend='max')
        cs_1.cmap.set_over('pink')
        cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 2:
        cs_2 = m.contourf(x2,y2,qpf3_2,clevs,colors=colorlist,extend='max')
        cs_2.cmap.set_over('pink')
        cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 3:
        cs = m.contourf(x2,y2,qpf3_dif,clevsdif,colors=difcolors,extend='both')
        cs.cmap.set_under('darkblue')
        cs.cmap.set_over('darkred')
        cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA - FV3 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

      par += 1
    par = 1

    plt.savefig('./compareqpf3_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
    plt.close()

    t2 = time.clock()
    t3 = round(t2-t1, 3)
    print('%.3f seconds to plot qpf3 for: '+dom) % t3

#################################
  # Plot snow depth
#################################
  t1 = time.clock()
  print('Working on snow depth for '+dom)

  # Clear off old plottables but keep all the map info
#  ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#  ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#  ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'in'
  clevs = [0.1,1,2,3,6,9,12,18,24,36,48]
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  cm = ncepy.ncl_perc_11Lev()
  
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
      cs_1 = m.contourf(x,y,snow_1,clevs,cmap=cm,extend='max')
      cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3 Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 2:
      cs_2 = m.contourf(x2,y2,snow_2,clevs,cmap=cm,extend='max')
      cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

    elif par == 3:
      cs = m.contourf(x2,y2,snow_dif,clevsdif,colors=difcolors,extend='both')
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar.set_label(units,fontsize=6)
      cbar.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'FV3-DA - FV3 Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

    par += 1
  par = 1

  plt.savefig('./comparesnow_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  plt.close()

  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot snow depth for: '+dom) % t3

#################################
  # Plot 6-hr change in snow depth
#################################
  if (fhr % 3 == 0) and (fhr >= 6):
    t1 = time.clock()
    print('Working on 6-hr change in snow depth for '+dom)

    # Clear off old plottables but keep all the map info
#    ncepy.clear_plotables(ax1,keep_ax_lst_1,fig)
#    ncepy.clear_plotables(ax2,keep_ax_lst_2,fig)
#    ncepy.clear_plotables(ax3,keep_ax_lst_3,fig)

    units = 'in'
    clevs = [-6,-4,-3,-2,-1,-0.5,0,0.5,1,2,3,4,6]
    clevsdif = [-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3]
    colorlist = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']

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
        cs_1 = m.contourf(x,y,snow6_1,clevs,colors=colorlist,extend='both')
        cs_1.cmap.set_under('darkblue')
        cs_1.cmap.set_over('darkred')
        cbar = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3 6-hr Change in Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 2:
        cs_2 = m.contourf(x2,y2,snow6_2,clevs,colors=colorlist,extend='both')
        cs_2.cmap.set_under('darkblue')
        cs_2.cmap.set_over('darkred')
        cbar = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA 6-hr Change in Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))

      elif par == 3:
        cs = m.contourf(x2,y2,snow6_dif,clevsdif,colors=colorlist,extend='both')
        cs.cmap.set_under('darkblue')
        cs.cmap.set_over('darkred')
        cbar = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
        cbar.set_label(units,fontsize=6)
        cbar.ax.tick_params(labelsize=6)
        ax.text(.5,1.03,'FV3-DA - FV3 6-hr Change in Snow Depth ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         

      par += 1
    par = 1

    plt.savefig('./comparesnow6_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
    plt.close()

    t2 = time.clock()
    t3 = round(t2-t1, 3)
    print('%.3f seconds to plot snow depth for: '+dom) % t3



######################################################

  t3dom = round(t2-t1dom, 3)
  print("%.3f seconds to plot all variables for: "+dom) % t3dom
  plt.clf()

######################################################

main()
