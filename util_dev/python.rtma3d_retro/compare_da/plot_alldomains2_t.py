import pygrib
import matplotlib
matplotlib.use('Agg')
import cStringIO
import matplotlib.pyplot as plt
from PIL import Image
import matplotlib.image as image
from matplotlib.gridspec import GridSpec
from mpl_toolkits.basemap import Basemap
import numpy as np
import time,os,sys,multiprocessing
import ncepy
from scipy import ndimage
from netCDF4 import Dataset

#--------------Define some functions ------------------#

def clear_plotables(ax,keep_ax_lst,fig):
  #### - step to clear off old plottables but leave the map info - ####
  if len(keep_ax_lst) == 0 :
    print "clear_plotables WARNING keep_ax_lst has length 0. Clearing ALL plottables including map info!"
  cur_ax_children = ax.get_children()[:]
  if len(cur_ax_children) > 0:
    for a in cur_ax_children:
      if a not in keep_ax_lst:
       # if the artist isn't part of the initial set up, remove it
        a.remove()

def compress_and_save(filename):
  #### - compress and save the image - ####
  ram = cStringIO.StringIO()
  plt.savefig(ram, format='png', bbox_inches='tight', dpi=150)
  ram.seek(0)
  im = Image.open(ram)
  im2 = im.convert('RGB').convert('P', palette=Image.ADAPTIVE)
  im2.save(filename, format='PNG')

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
subcyc=str(minute).zfill(2)
print year, month, day, hour, minute

# Define the output files
data2 = pygrib.open('/gpfs/dell1/ptmp/Edward.Colon/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs.grib2')
data1 = pygrib.open('/gpfs/dell1/ptmp/Edward.Colon/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs_fgs.grib2')

print('/gpfs/dell1/ptmp/Edward.Colon/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'00z/rtma3d.t'+cyc+subcyc+'00z.wrfsubhprs.grib2')
print('/gpfs/dell1/ptmp/Edward.Colon/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'00z/rtma3d.t'+cyc+subcyc+'00z.wrfsubhprs_fgs.grib2')

#data1 = pygrib.open('/gpfs/gp2/ptmp/Benjamin.Blake/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')
#data2 = pygrib.open('/gpfs/gp2/ptmp/Eric.Rogers/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour+'.grib2')

#if (fhr > 2):
#  data1_m1 = pygrib.open('/gpfs/gp2/ptmp/Benjamin.Blake/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour1+'.grib2')
#  data2_m1 = pygrib.open('/gpfs/gp2/ptmp/Eric.Rogers/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour1+'.grib2')
#  data1_m2 = pygrib.open('/gpfs/gp2/ptmp/Benjamin.Blake/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour2+'.grib2')
#  data2_m2 = pygrib.open('/gpfs/gp2/ptmp/Eric.Rogers/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour2+'.grib2')
#if (fhr >= 6):
#  data1_m6 = pygrib.open('/gpfs/gp2/ptmp/Benjamin.Blake/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour6+'.grib2')
#  data2_m6 = pygrib.open('/gpfs/gp2/ptmp/Eric.Rogers/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour6+'.grib2')
#if (fhr >= 24):
#  data1_m24 = pygrib.open('/gpfs/gp2/ptmp/Benjamin.Blake/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour24+'.grib2')
#  data2_m24 = pygrib.open('/gpfs/gp2/ptmp/Eric.Rogers/com/fv3cam/para/fv3sar.'+str(ymd)+'/'+cyc+'/fv3sar.t'+cyc+'z.conus.f'+fhour24+'.grib2')

# Get the lats and lons
lat,lon = data1.select(name='2 metre temperature')[0].latlons()
lat2,lon2 = data2.select(name='2 metre temperature')[0].latlons()
Lon0 = data1[1]['LoVInDegrees']
Lat0 = data1[1]['LaDInDegrees']

# Forecast valid date/time
vtime = ymdhm
#vtime = ncepy.ndate(itime,int(fhr))

# Specify plotting domains
domains = ['conus','BN','CE','CO','LA','MA','NC','NE','NW','OV','SC','SE','SF','SP','SW','UM']

###################################################
# Read in all variables and calculate differences #
###################################################
t1a = time.clock()


# Sea level pressure
sfp_1 = (data1.select(name='Surface pressure',typeOfLevel='surface')[0].values)/100.0
sfpsmooth1 = ndimage.filters.gaussian_filter(sfp_1, 13.78)
sfp_2 = (data2.select(name='Surface pressure',typeOfLevel='surface')[0].values)/100.0
sfpsmooth2 = ndimage.filters.gaussian_filter(sfp_2, 13.78)
sfp_dif = sfp_2 - sfp_1

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

# 2-m dew point temperature
dew2m_1 = data1.select(name='2 metre dewpoint temperature')[0].values
dew2m_1 = (dew2m_1 - 273.15)*1.8 + 32.0
dew2m_2 = data2.select(name='2 metre dewpoint temperature')[0].values
dew2m_2 = (dew2m_2 - 273.15)*1.8 + 32.0
dew2m_dif = dew2m_2 - dew2m_1

# 2-m relative humidity
rh2m_1 = data1.select(name='Relative humidity',level=2)[0].values
rh2m_2 = data2.select(name='Relative humidity',level=2)[0].values
rh2m_dif = rh2m_2 - rh2m_1

# surface wind gusts

swg_1 = data1.select(name='Wind speed (gust)',typeOfLevel='surface')[0].values * 1.94384
swg_2 = data2.select(name='Wind speed (gust)',typeOfLevel='surface')[0].values * 1.94384
swg_dif = swg_2 - swg_1

# 10-m wind speed
uwind_1 = data1.select(name='10 metre U wind component')[0].values * 1.94384
uwind_2 = data2.select(name='10 metre U wind component')[0].values * 1.94384
vwind_1 = data1.select(name='10 metre V wind component')[0].values * 1.94384
vwind_2 = data2.select(name='10 metre V wind component')[0].values * 1.94384
# Rotate winds from grid relative to Earth relative
uwind_1, vwind_1 = ncepy.rotate_wind(Lat0,Lon0,lon,uwind_1,vwind_1,'lcc',inverse=False)
uwind_2, vwind_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,uwind_2,vwind_2,'lcc',inverse=False)
wspd10m_1 = np.sqrt(uwind_1**2 + vwind_1**2)
wspd10m_2 = np.sqrt(uwind_2**2 + vwind_2**2)
wspd10m_dif = wspd10m_2 - wspd10m_1

# Fosberg Index
emc_1 = np.zeros_like(tmp2m_1)
emc_1 = np.where(rh2m_1.any < 10, 0.03229 + (0.281073*rh2m_1) - (0.000578*rh2m_1*tmp2m_1), emc_1)
emc_1 = np.where(10 <= rh2m_1.any <= 50, 2.22749 + (0.160107*rh2m_1) - (0.01478*tmp2m_1), emc_1)
emc_1 = np.where(rh2m_1.any > 50, 21.0606 + (0.005565*(rh2m_1**2)) - (0.00035*rh2m_1*tmp2m_1) - (0.483199*rh2m_1), emc_1)
mdc_1 = 1 - 2*(emc_1/30) + 1.5*((emc_1/30)**2) - 0.5*((emc_1/30)**3)
ffwi_1 = (mdc_1 * np.sqrt(1 + wspd10m_1**2)) / 0.3002

emc_2 = np.zeros_like(tmp2m_2)
emc_2 = np.where(rh2m_2.any < 10, 0.03229 + (0.281073*rh2m_2) - (0.000578*rh2m_2*tmp2m_2), emc_2)
emc_2 = np.where(10 <= rh2m_2.any <= 50, 2.22749 + (0.160107*rh2m_2) - (0.01478*tmp2m_2), emc_2)
emc_2 = np.where(rh2m_2.any > 50, 21.0606 + (0.005565*(rh2m_2**2)) - (0.00035*rh2m_2*tmp2m_2) - (0.483199*rh2m_2), emc_2)
mdc_2 = 1 - 2*(emc_2/30) + 1.5*((emc_2/30)**2) - 0.5*((emc_2/30)**3)
ffwi_2 = (mdc_2 * np.sqrt(1 + wspd10m_2**2)) / 0.3002

ffwi_dif = ffwi_2 - ffwi_1

# Most unstable CAPE
mucape_1 = data1.select(name='Convective available potential energy',topLevel=18000)[0].values
mucape_2 = data2.select(name='Convective available potential energy',topLevel=18000)[0].values
mucape_dif = mucape_2 - mucape_1

# Most Unstable CIN
#mucin_1 = data1.select(name='Convective inhibition',topLevel=18000)[0].values
#mucin_2 = data2.select(name='Convective inhibition',topLevel=18000)[0].values
#mucin_dif = mucin_2 - mucin_1

# Surface-based CAPE
cape_1 = data1.select(name='Convective available potential energy',typeOfLevel='surface')[0].values
cape_2 = data2.select(name='Convective available potential energy',typeOfLevel='surface')[0].values
cape_dif = cape_2 - cape_1

# Surface-based CIN
#sfcin_1 = data1.select(name='Convective inhibition',typeOfLevel='surface')[0].values
#sfcin_2 = data2.select(name='Convective inhibition',typeOfLevel='surface')[0].values
#sfcin_dif = sfcin_2 - sfcin_1

# Mixed Layer CAPE
mlcape_1 = data1.select(name='Convective available potential energy',topLevel=9000)[0].values
mlcape_2 = data2.select(name='Convective available potential energy',topLevel=9000)[0].values
mlcape_dif = mlcape_2 - mlcape_1

# Mixed Layer CIN
#mlcin_1 = data1.select(name='Convective inhibition',topLevel=9000)[0].values
#mlcin_2 = data2.select(name='Convective inhibition',topLevel=9000)[0].values
#mlcin_dif = mlcin_2 - mlcin_1

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

# 850-mb winds
u850_1 = data1.select(name='U component of wind',level=850)[0].values * 1.94384
u850_2 = data2.select(name='U component of wind',level=850)[0].values * 1.94384
v850_1 = data1.select(name='V component of wind',level=850)[0].values * 1.94384
v850_2 = data2.select(name='V component of wind',level=850)[0].values * 1.94384
# Rotate winds from grid relative to Earth relative
u850_1, v850_1 = ncepy.rotate_wind(Lat0,Lon0,lon,u850_1,v850_1,'lcc',inverse=False)
u850_2, v850_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,u850_2,v850_2,'lcc',inverse=False)

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
# Rotate winds from grid relative to Earth relative
u500_1, v500_1 = ncepy.rotate_wind(Lat0,Lon0,lon,u500_1,v500_1,'lcc',inverse=False)
u500_2, v500_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,u500_2,v500_2,'lcc',inverse=False)

# 250 mb winds
u250_1 = data1.select(name='U component of wind',level=250)[0].values * 1.94384
u250_2 = data2.select(name='U component of wind',level=250)[0].values * 1.94384
v250_1 = data1.select(name='V component of wind',level=250)[0].values * 1.94384
v250_2 = data2.select(name='V component of wind',level=250)[0].values * 1.94384
# Rotate winds from grid relative to Earth relative
u250_1, v250_1 = ncepy.rotate_wind(Lat0,Lon0,lon,u250_1,v250_1,'lcc',inverse=False)
u250_2, v250_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,u250_2,v250_2,'lcc',inverse=False)
wspd250_1 = np.sqrt(u250_1**2 + v250_1**2)
wspd250_2 = np.sqrt(u250_2**2 + v250_2**2)
wspd250_dif = wspd250_2 - wspd250_1

# Visibility
visemc_1 = data1.select(name='Visibility',typeOfLevel='surface')[0].values * 0.000621371
visemc_2 = data2.select(name='Visibility',typeOfLevel='surface')[0].values * 0.000621371
visemc_dif = visemc_2 - visemc_1

#visgsd_1 = data1.select(name='Visibility',typeOfLevel='cloudTop')[0].values * 0.000621371
#visgsd_2 = data2.select(name='Visibility',typeOfLevel='cloudTop')[0].values * 0.000621371
#visgsd_dif = visgsd_2 - visgsd_1

# Cloud Base Height
zbase_1 = data1.select(name='Geopotential Height',typeOfLevel='cloudBase')[0].values * (3.28084/1000)
zbase_2 = data2.select(name='Geopotential Height',typeOfLevel='cloudBase')[0].values * (3.28084/1000)
zbase_dif = zbase_2 - zbase_1

# Cloud Ceiling Height
zceil_1 = data1.select(name='Geopotential Height',nameOfFirstFixedSurface='215')[0].values * (3.28084/1000)
zceil_2 = data2.select(name='Geopotential Height',nameOfFirstFixedSurface='215')[0].values * (3.28084/1000)
zceil_dif = zceil_2 - zceil_1

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
#qpf_1 = data1.select(name='Total precipitation',typeOfLevel='surface')[0].values * 0.0393701
#qpf_2 = data2.select(name='Total precipitation',typeOfLevel='surface')[0].values * 0.0393701
#qpf_dif = qpf_2 - qpf_1

# 3-hr precipitation
#if (fhr > 2):  # Do not make 3-hr plots for forecast hours 1 and 2
#  qpfm2_1 = data1_m2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpfm1_1 = data1_m1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpfm0_1 = data1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpf3_1 = qpfm2_1 + qpfm1_1 + qpfm0_1
#  qpfm2_2 = data2_m2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpfm1_2 = data2_m1.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpfm0_2 = data2.select(name='Total Precipitation',lengthOfTimeRange=1)[0].values * 0.0393701
#  qpf3_2 = qpfm2_2 + qpfm1_2 + qpfm0_2
#  qpf3_dif = qpf3_2 - qpf3_1

# Snow depth
snow_1 = data1.select(name='Snow depth')[0].values * 39.3701
snow_2 = data2.select(name='Snow depth')[0].values * 39.3701
snow_dif = snow_2 - snow_1
#if (fhr >=6):	# Do not make 6-hr plots for forecast hours less than 6
#  snowm6_1 = data1_m6.select(name='Snow depth')[0].values * 39.3701
#  snow6_1 = snow_1 - snowm6_1 
#  snowm6_2 = data2_m6.select(name='Snow depth')[0].values * 39.3701
#  snow6_2 = snow_2 - snowm6_2
#  snow6_dif = snow6_2 - snow6_1

# 1-km reflectivity
ref1km_1 = data1.select(name='Derived radar reflectivity',typeOfLevel="heightAboveGround",stepType='instant',level=1000)[0].values
ref1km_2 = data2.select(name='Derived radar reflectivity',typeOfLevel="heightAboveGround",stepType='instant',level=1000)[0].values
ref1km_dif = ref1km_2 - ref1km_1
# 4-km reflectivity
ref4km_1 = data1.select(name='Derived radar reflectivity',typeOfLevel="heightAboveGround",stepType='instant',level=4000)[0].values
ref4km_2 = data2.select(name='Derived radar reflectivity',typeOfLevel="heightAboveGround",stepType='instant',level=4000)[0].values
ref4km_dif = ref4km_2 - ref4km_1 
# -10C isothermal reflectivity
ref10C_1 = data1.select(name='Derived radar reflectivity',typeOfLevel="isothermal",stepType='instant',level=263)[0].values
ref10C_2 = data2.select(name='Derived radar reflectivity',typeOfLevel="isothermal",stepType='instant',level=263)[0].values
ref10C_dif = ref10C_2 - ref10C_1
# Composite reflectivity
refc_1 = data1.select(name='Maximum/Composite radar reflectivity',stepType='instant',level=0)[0].values 
refc_2 = data2.select(name='Maximum/Composite radar reflectivity',stepType='instant',level=0)[0].values  
refc_dif = refc_2 - refc_1


t2a = time.clock()
t3a = round(t2a-t1a, 3)
print("%.3f seconds to read all messages") % t3a

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']

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
    xscale=0.18
    yscale=0.18
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
    xscale=0.17
    yscale=0.18
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

################################
  # Plot SLP
################################
#  t1 = time.clock()
#  print('Working on sfp for '+dom)

#  units = 'mb'
#  clevs = [976,980,984,988,992,996,1000,1004,1008,1012,1016,1020,1024,1028,1032,1036,1040,1044,1048,1052]
#  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs1_a = m.contourf(x,y,sfpsmooth1,clevs,cmap=plt.cm.Spectral_r,extend='both',ax=ax)  
#      cbar1 = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      cs1_b = m.contour(x,y,sfpsmooth1,np.arange(940,1060,4),colors='black',linewidths=1.25,ax=ax)
#      plt.clabel(cs1_b,np.arange(940,1060,4),inline=1,fmt='%d',fontsize=6,zorder=12,ax=ax)
#      ax.text(.5,1.03,'3D-RTMA FGS Surface Pressure ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
 # plot highs and lows - window parameter controls the number of highs and lows detected
#      ncepy.plt_highs_and_lows(m,sfp_1,lon,lat,mode='reflect',window='500')

#    elif par == 2:
#      cs2_a = m.contourf(x2,y2,sfpsmooth2,clevs,cmap=plt.cm.Spectral_r,extend='both',ax=ax)
#      cbar2 = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      cs2_b = m.contour(x2,y2,sfpsmooth2,np.arange(940,1060,4),colors='black',linewidths=1.25,ax=ax)
#      plt.clabel(cs2_b,np.arange(940,1060,4),inline=1,fmt='%d',fontsize=6,zorder=12,ax=ax)
#      ax.text(.5,1.03,'3D-RTMA ANL Surface Pressure ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
  # plot highs and lows - window parameter controls the number of highs and lows detected
#      ncepy.plt_highs_and_lows(m,sfp_2,lon,lat,mode='reflect',window='500')

#    elif par == 3:
#      cs = m.contourf(x2,y2,sfp_dif,clevsdif,colors=difcolors,extend='both',ax=ax)  
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Pressure ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1
#  compress_and_save('comparesfp_'+dom+'_t'+cyc+subcyc+'z.png')
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot sfp for: '+dom) % t3


#################################
  # Plot 2-m T
#################################
  t1 = time.clock()
  print('Working on t2m for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-16,134,51)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  cm = cmap_t2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,tmp2m_1,clevs,cmap=cm,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
#      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 2-m Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
 
    elif par == 2:
      cs_2 = m.contourf(x2,y2,tmp2m_2,clevs,cmap=cm,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
#      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 2-m Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,tmp2m_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
#      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 2-m Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2')) 
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare2mt_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare2mt_'+dom+'t'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 2mt for: '+dom) % t3


#################################
# Plot SFCT
#################################
  t1 = time.clock()
  print('Working on tsfc for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-16,134,51)
  clevsdif = [-18,-15,-12,-9,-6,-3,0,3,6,9,12,15,18]
  cm = cmap_t2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,tmpsfc_1,clevs,cmap=cm,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,tmpsfc_2,clevs,cmap=cm,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[-16,-4,8,20,32,44,56,68,80,92,104,116,128])
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,tmpsfc_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparetsfc_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparetsfc_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot tsfc for: '+dom) % t3

#################################
  # Plot 2-m Dew Point
#################################
  t1 = time.clock()
  print('Working on 2mdew for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = u'\xb0''F'
  clevs = np.linspace(-5,80,35)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  cm = ncepy.cmap_q2m()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,dew2m_1,clevs,cmap=cm,extend='both',ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 2-m Dew Point Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,dew2m_2,clevs,cmap=cm,extend='both',ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 2-m Dew Point Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,dew2m_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 2-m Dew Point Temperature ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare2mdew_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 2mdew for: '+dom) % t3

#################################
  # Plot 2-m Relative Humidity
#################################
  t1 = time.clock()
  print('Working on 2-m Relative Humidity for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [0,10,20,30,40,50,60,70,80,90,100]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,rh2m_1,clevs,cmap=cm,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 2-m Relative Humidity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,rh2m_2,clevs,cmap=cm,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 2-m Relative Humidity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,rh2m_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 2-m Relative Humidity ('+units+') valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare2mrh_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 2-m Relative Humidity for: '+dom) % t3

#################################
  # Plot Fosberg Index
#################################
  t1 = time.clock()
  print('Working on Filip Forsberg Index for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = ''
  clevs = [20,25,30,35,40,45,50,55,60,65,70]
  clevsdif = [-18,-15,-12,-9,-6,-3,0,3,6,9,12,15,18]
  colorlist = ['dodgerblue','darkturquoise','cyan','mediumspringgreen','#FFF68F','#EEEE00','#EEC900','darkorange','crimson','darkred']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,ffwi_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('darkviolet')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Fosberg Index \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ffwi_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('darkviolet')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Fosberg Index \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,ffwi_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Fosberg Index \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareffwi_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Filip Forsberg Index for: '+dom) % t3

#################################
  # Plot Surface Wind Gusts
#################################
  t1 = time.clock()
  print('Working on Surface wind gusts for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kts'
  clevs = [5,10,15,20,25,30,35,40,45,50,55,60]
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  colorlist = ['turquoise','dodgerblue','blue','#FFF68F','#E3CF57','peru','brown','crimson','red','fuchsia','DarkViolet']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,swg_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Surface Wind Gusts ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85
,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,swg_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Surface Wind Gusts ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85
,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,swg_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label('kts',fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface Wind Gusts (kts) valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',
alpha=0.85,boxstyle='square,pad=0.2'))       
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareswg_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot swg for: '+dom) % t3

#################################
  # Plot 10-m WSPD
#################################
  t1 = time.clock()
  print('Working on 10mwspd for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kts'
  if dom == 'conus':
    skip = 80
  elif dom == 'SE':
    skip = 35
  elif dom == 'CO' or dom == 'LA' or dom == 'MA':
    skip = 12
  elif dom == 'BN':
    skip = 10
  elif dom == 'SP':
    skip = 9
  elif dom == 'SF':
    skip = 3
  else:
    skip = 20
  barblength = 4
  clevs = [5,10,15,20,25,30,35,40,45,50,55,60]
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  colorlist = ['turquoise','dodgerblue','blue','#FFF68F','#E3CF57','peru','brown','crimson','red','fuchsia','DarkViolet']

  # Rotate winds to gnomonic projection
  urot_1, vrot_1 = m.rotate_vector(uwind_1,vwind_1,lon,lat)
  urot_2, vrot_2 = m.rotate_vector(uwind_2,vwind_2,lon2,lat2)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,wspd10m_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS 10-m Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 2:
      cs_2 = m.contourf(x2,y2,wspd10m_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL 10-m Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,wspd10m_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label('kts',fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 10-m Wind Speed (kts) valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare10mwind_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare10mwind_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 10mwspd for: '+dom) % t3

#################################
  # Plot Most Unstable CAPE/CIN
#################################
  t1 = time.clock()
  print('Working on mucapecin for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'J/kg'
  clevs = [100,250,500,1000,1500,2000,2500,3000,3500,4000,4500,5000]
  clevsdif = [-2000,-1500,-1000,-500,-250,-100,0,100,250,500,1000,1500,2000]
  colorlist = ['blue','dodgerblue','cyan','mediumspringgreen','#FAFAD2','#EEEE00','#EEC900','darkorange','crimson','darkred','darkviolet']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,mucape_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA FGS MUCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,mucape_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA ANL MUCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,mucape_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Most Unstable CAPE ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparemucape_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot mucapecin for: '+dom) % t3

#################################
  # Plot Surface-Based CAPE/CIN
#################################
  t1 = time.clock()
  print('Working on sfcapecin for '+dom)

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
      cs_1 = m.contourf(x,y,cape_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA FGS SFCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,cape_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA ANL SFCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,cape_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Surface-Based CAPE ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparesfcape_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot sfcapecin for: '+dom) % t3

#################################
  # Plot Mixed Layer CAPE/CIN
#################################
  t1 = time.clock()
  print('Working on mlcapecin for '+dom)

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
      cs_1 = m.contourf(x,y,mlcape_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('black')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA FGS MLCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,mlcape_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('black')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=4)
      ax.text(.5,1.05,'3D-RTMA ANL MLCAPE ('+units+') \n <-500 (*), -500<-250 (+), -250<-100 (/), -100<-25 (.) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,mlcape_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=4)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Mixed Layer CAPE ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparemlcape_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot mlcapecin for: '+dom) % t3

#################################
  # Plot 850-mb THETAE
#################################
  t1 = time.clock()
  print('Working on 850 mb Theta-e for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'K'
# Wind barb density settings for 850, 500, and 250 mb plots
  if dom == 'conus':
    skip = 100
  elif dom == 'SE':
    skip = 40
  elif dom == 'CO' or dom == 'LA' or dom =='MA':
    skip = 18
  elif dom == 'BN':
    skip = 15
  elif dom == 'SP':
    skip = 13
  elif dom == 'SF':
    skip = 4
  else:
    skip = 30
  barblength = 4

  clevs = np.linspace(250,340,31)
  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
  cm = cmap_t850()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N)

  # Rotate winds to gnomonic projection
  urot_1, vrot_1 = m.rotate_vector(u850_1,v850_1,lon,lat)
  urot_2, vrot_2 = m.rotate_vector(u850_2,v850_2,lon2,lat2)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,thetae_1,clevs,cmap=cm,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=[250,256,262,268,274,280,286,292,298,304,310,316,322,328,334,340])
      cbar1.set_label(units,fontsize=6)   
      cbar1.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS 850 mb $\Theta$e ('+units+') and Winds (kts) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,thetae_2,clevs,cmap=cm,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=[250,256,262,268,274,280,286,292,298,304,310,316,322,328,334,340])
      cbar2.set_label(units,fontsize=6)   
      cbar2.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL 850 mb $\Theta$e ('+units+') and Winds (kts) \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
    elif par == 3:
      cs = m.contourf(x2,y2,thetae_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)   
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 850 mb $\Theta$e ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare850t_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare850t_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 850 mb Theta-e for: '+dom) % t3

#################################
  # Plot 700-mb OMEGA and RH
#################################
  t1 = time.clock()
  print('Working on 700 mb omega and RH for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [50,60,70,80,90,100]
  clevsw = [-100,-5]
  clevsdif = [-30,-25,-20,-15,-10,-5,-0,5,10,15,20,25,30]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs1_a = m.contourf(x,y,rh700_1,clevs,cmap=plt.cm.BuGn,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6) 
      cbar1.ax.tick_params(labelsize=6)
      cs1_b = m.contourf(x,y,omg700_1,clevsw,colors='blue',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS 700 mb $\omega$ (rising motion in blue) and RH ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs2_a = m.contourf(x2,y2,rh700_2,clevs,cmap=plt.cm.BuGn,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6) 
      cbar2.ax.tick_params(labelsize=6)
      cs2_b = m.contourf(x2,y2,omg700_2,clevsw,colors='blue',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL 700 mb $\omega$ (rising motion in blue) and RH ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,rh700_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 700 mb RH ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare700_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare700_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 700 mb $\omega$ and RH for: '+dom) % t3

#################################
  # Plot 500 mb HGT/WIND/VORT
#################################
  t1 = time.clock()
  print('Working on 500 mb Hgt/Wind/Vort for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'x10${^5}$ s${^{-1}}$'
  vortlevs = [16,20,24,28,32,36,40]
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  colorlist = ['yellow','gold','goldenrod','orange','orangered','red']

  # Rotate winds to gnomonic projection
  urot_1, vrot_1 = m.rotate_vector(u500_1,v500_1,lon,lat)
  urot_2, vrot_2 = m.rotate_vector(u500_2,v500_2,lon2,lat2)

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs1_a = m.contourf(x,y,vort500_1,vortlevs,colors=colorlist,extend='both',ax=ax)
      cs1_a.cmap.set_under('white')
      cs1_a.cmap.set_over('darkred')
      cbar1.remove()
      cbar1 = m.colorbar(cs1_a,ax=ax,location='bottom',pad=0.05,ticks=vortlevs)
      cbar1.set_label(units,fontsize=6)   
      cbar1.ax.tick_params(labelsize=6)

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

      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='steelblue',ax=ax)
      x,y = m(lon,lat)	# need to redefine to avoid index error
      cs1_b = m.contour(x,y,z500_1,np.arange(486,600,6),colors='black',linewidths=1,ax=ax)
      plt.clabel(cs1_b,np.arange(486,600,6),inline_spacing=1,fmt='%d',fontsize=6,dorder=12,ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS 500 mb Heights (dam), Winds (kts), and $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs2_a = m.contourf(x2,y2,vort500_2,vortlevs,colors=colorlist,extend='both',ax=ax)
      cs2_a.cmap.set_under('white')
      cs2_a.cmap.set_over('darkred')
      cbar2.remove()
      cbar2 = m.colorbar(cs2_a,ax=ax,location='bottom',pad=0.05,ticks=vortlevs)
      cbar2.set_label(units,fontsize=6)   
      cbar2.ax.tick_params(labelsize=6)

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

      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='steelblue',ax=ax)
#      x,y = m(lon,lat)	# need to redefine to avoid index error
      cs2_b = m.contour(x2,y2,z500_2,np.arange(486,600,6),colors='black',linewidths=1,ax=ax)
      plt.clabel(cs2_b,np.arange(486,600,6),inline_spacing=1,fmt='%d',fontsize=6,dorder=12,ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL 500 mb Heights (dam), Winds (kts), and $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,z500_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 500 mb Heights (dam) \n valid: '+vtime, horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compare500_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare500_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 500 mb Hgt/Wind/Vort for: '+dom) % t3

#################################
  # Plot 250 mb WIND
#################################
  t1 = time.clock()
  print('Working on 250 mb WIND for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kts'
  clevs = [50,60,70,80,90,100,110,120,130,140,150]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  colorlist = ['turquoise','deepskyblue','dodgerblue','#1874CD','blue','beige','khaki','peru','brown','crimson']

  # Rotate winds to gnomonic projection
  urot_1, vrot_1 = m.rotate_vector(u250_1,v250_1,lon,lat)
  urot_2, vrot_2 = m.rotate_vector(u250_2,v250_2,lon2,lat2)

  x,y = m(lon,lat)	# need to redefine to avoid index error

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,wspd250_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('red')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA FGS 250 mb Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,wspd250_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('red')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
      ax.text(.5,1.03,'3D-RTMA ANL 250 mb Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,wspd250_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 250 mb Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1
   
  compress_and_save('compare250wind_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare250wind_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 250 mb WIND for: '+dom) % t3

#################################
  # Plot EMC Visibility
#################################
  t1 = time.clock()
  print('Working on EMC Surface Visibility for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'miles'
  clevs = [0.25,0.5,1,2,3,4,5,10]
  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15]
  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,visemc_1,clevs,colors=colorlist,extend='min',ax=ax)
      cs_1.cmap.set_under('firebrick')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS EMC Surface Visibility ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,visemc_2,clevs,colors=colorlist,extend='min',ax=ax)
      cs_2.cmap.set_under('firebrick')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL EMC Surface Visibility ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,visemc_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS EMC Surface Visibility ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparevis_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparevis_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot EMC Surface Visibility for: '+dom) % t3


#################################
  # Plot GSD Surface Visibility
#################################
#  t1 = time.clock()
#  print('Working on GSD Surface Visibility for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'miles'
#  clevs = [0.25,0.5,1,2,3,4,5,10]
#  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0.,2.5,5,7.5,10,12.5,15]
#  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,visgsd_1,clevs,colors=colorlist,extend='min',ax=ax)
#      cs_1.cmap.set_under('firebrick')
#      cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS GSD Surface Visibility ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,visgsd_2,clevs,colors=colorlist,extend='min',ax=ax)
#      cs_2.cmap.set_under('firebrick')
#      cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL GSD Surface Visibility ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,visgsd_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6) 
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS GSD Surface Visibility ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparevis2_'+dom+'_f'+fhour+'.png')
#  plt.savefig('./comparevis2_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot GSD Surface Visibility for: '+dom) % t3

#################################
  # Plot Cloud Base Height
#################################
  t1 = time.clock()
  print('Working on Cloud Base Height for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kft'
  clevs = [0,0.1,0.3,0.5,1,5,10,15,20,25,30,35,40]
  clevsdif = [-12,-10,-8,-6,-4,-2,0.,2,4,6,8,10,12]
  colorlist = ['firebrick','tomato','salmon','lightsalmon','goldenrod','khaki','gold','yellow','palegreen','mediumspringgreen','lime','limegreen']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,zbase_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('darkgreen')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.set_xticklabels(clevs)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Cloud Base Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,zbase_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('darkgreen')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.set_xticklabels(clevs)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Cloud Base Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,zbase_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Cloud Base Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparezbase_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparezbase_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Cloud Base Height for: '+dom) % t3

#################################
  # Plot Cloud Ceiling Height
#################################
  t1 = time.clock()
  print('Working on Cloud Ceiling Height for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kft'
  clevs = [0,0.1,0.3,0.5,1,5,10,15,20,25,30,35,40]
  clevsdif = [-12,-10,-8,-6,-4,-2,0.,2,4,6,8,10,12]
  colorlist = ['firebrick','tomato','salmon','lightsalmon','goldenrod','khaki','gold','yellow','palegreen','mediumspringgreen','lime','limegreen']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,zceil_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.set_xticklabels(clevs)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Cloud Ceiling Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,zceil_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.set_xticklabels(clevs)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Cloud Ceiling Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,zceil_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Cloud Ceiling Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparezceil_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparezceil_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Cloud Ceiling Height for: '+dom) % t3

#################################
  # Plot Cloud Top Height
#################################
  t1 = time.clock()
  print('Working on Cloud Top Height for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'kft'
  clevs = [1,5,10,15,20,25,30,35,40,45,50]
  clevsdif = [-12,-10,-8,-6,-4,-2,0.,2,4,6,8,10,12]
  colorlist = ['firebrick','tomato','salmon','lightsalmon','goldenrod','yellow','palegreen','mediumspringgreen','lime','limegreen']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,ztop_1,clevs,colors=colorlist,extend='max',ax=ax)
      cs_1.cmap.set_over('darkgreen')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Cloud Top Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ztop_2,clevs,colors=colorlist,extend='max',ax=ax)
      cs_2.cmap.set_over('darkgreen')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Cloud Top Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,ztop_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Cloud Top Height ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareztop_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareztop_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot Cloud Top Height for: '+dom) % t3

#################################
  # Plot PW
#################################
  t1 = time.clock()
  print('Working on PW for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'in'
  clevs = [0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25]
  clevsdif = [-1.25,-1,-.75,-.5,-.25,-.1,0.,.1,.25,.50,.75,1,1.25]
  colorlist = ['lightsalmon','khaki','palegreen','cyan','turquoise','cornflowerblue','mediumslateblue','darkorchid','deeppink']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,pw_1,clevs,colors=colorlist,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cs_1.cmap.set_over('hotpink')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Precipitable Water ('+units+') \n valid: '+vtime ,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,pw_2,clevs,colors=colorlist,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cs_2.cmap.set_over('hotpink')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Precipitable Water ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,pw_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Precipitable Water ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparepw_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparepw_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot PW for: '+dom) % t3

#################################
  # Plot % FROZEN PRECIP
#################################
  t1 = time.clock()
  print('Working on PERCENT FROZEN PRECIP for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = '%'
  clevs = [10,20,30,40,50,60,70,80,90,100]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  colorlist = ['blue','dodgerblue','deepskyblue','mediumspringgreen','khaki','sandybrown','salmon','crimson','maroon']

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,pofp_1,clevs,colors=colorlist,ax=ax)
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Percent of Frozen Precipitation ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,pofp_2,clevs,colors=colorlist,ax=ax)
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Percent of Frozen Precipitation ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,pofp_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05,ticks=clevsdif)
      cbar3.set_label(units,fontsize=6) 
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Percent of Frozen Precipitaion ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparepofp_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparepofp_'+dom+'_f'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot PERCENT FROZEN PRECIP for: '+dom) % t3


#################################
  # Plot Total QPF
#################################
#  if (fhr > 0):
#    t1 = time.clock()
#    print('Working on total qpf for '+dom)

    # Clear off old plottables but keep all the map info
#    clear_plotables(ax1,keep_ax_lst_1,fig)
#    clear_plotables(ax2,keep_ax_lst_2,fig)
#    clear_plotables(ax3,keep_ax_lst_3,fig)

#    units = 'in'
#    clevs = [0.01,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]
#    clevsdif = [-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3]
#    colorlist = ['chartreuse','limegreen','green','blue','dodgerblue','deepskyblue','cyan','mediumpurple','mediumorchid','darkmagenta','darkred','crimson','orangered','darkorange','goldenrod','gold','yellow']  

#    for ax in axes:
#      xmin, xmax = ax.get_xlim()
#      ymin, ymax = ax.get_ylim()
#      xmax = int(round(xmax))
#      ymax = int(round(ymax))

#      if par == 1:
#        cs_1 = m.contourf(x,y,qpf_1,clevs,colors=colorlist,extend='max',ax=ax)
#        cs_1.cmap.set_over('pink')
#        cbar1.remove()
#        cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#        cbar1.set_label(units,fontsize=6)
#        cbar1.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 2:
#        cs_2 = m.contourf(x2,y2,qpf_2,clevs,colors=colorlist,extend='max',ax=ax)
#        cs_2.cmap.set_over('pink')
#        cbar2.remove()
#        cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#        cbar2.set_label(units,fontsize=6)
#        cbar2.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 3:
#        cs = m.contourf(x2,y2,qpf_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#        cs.cmap.set_under('darkblue')
#        cs.cmap.set_over('darkred')
#        cbar3.remove()
#        cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#        cbar3.set_label(units,fontsize=6)
#        cbar3.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS '+fhour+'-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      par += 1
#    par = 1

#    compress_and_save('compareqpf_'+dom+'_f'+fhour+'z.png')
#    plt.savefig('./compareqpf_'+dom+'_f'+fhour+'z.png', bbox_inches='tight',dpi=150)
#    t2 = time.clock()
#    t3 = round(t2-t1, 3)
#    print('%.3f seconds to plot total qpf for: '+dom) % t3

#################################
  # Plot QPF3
#################################
#  if (fhr % 3 == 0) and (fhr > 0):
#    t1 = time.clock()
#    print('Working on qpf3 for '+dom)

    # Clear off old plottables but keep all the map info
#    clear_plotables(ax1,keep_ax_lst_1,fig)
#    clear_plotables(ax2,keep_ax_lst_2,fig)
#    clear_plotables(ax3,keep_ax_lst_3,fig)

#    units = 'in'
#    clevs = [0.01,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]
#    clevsdif = [-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5]
#    colorlist = ['chartreuse','limegreen','green','blue','dodgerblue','deepskyblue','cyan','mediumpurple','mediumorchid','darkmagenta','darkred','crimson','orangered','darkorange','goldenrod','gold','yellow']  
   
#    for ax in axes:
#      xmin, xmax = ax.get_xlim()
#      ymin, ymax = ax.get_ylim()
#      xmax = int(round(xmax))
#      ymax = int(round(ymax))

#      if par == 1:
#        cs_1 = m.contourf(x,y,qpf3_1,clevs,colors=colorlist,extend='max',ax=ax)
#        cs_1.cmap.set_over('pink')
#        cbar1.remove()
#        cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#        cbar1.set_label(units,fontsize=6)
#        cbar1.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 2:
#        cs_2 = m.contourf(x2,y2,qpf3_2,clevs,colors=colorlist,extend='max',ax=ax)
#        cs_2.cmap.set_over('pink')
#        cbar2.remove()
#        cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#        cbar2.set_label(units,fontsize=6)
#        cbar2.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 3:
#        cs = m.contourf(x2,y2,qpf3_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#        cs.cmap.set_under('darkblue')
#        cs.cmap.set_over('darkred')
#        cbar3.remove()
#        cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#        cbar3.set_label(units,fontsize=6)
#        cbar3.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 3-hr Accumulated Precipitation ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      par += 1
#    par = 1

#    compress_and_save('compareqpf3_'+dom+'_f'+fhour+'z.png')
#    plt.savefig('./compareqpf3_'+dom+'_f'+fhour+'z.png', bbox_inches='tight',dpi=150)
#    t2 = time.clock()
#    t3 = round(t2-t1, 3)
#    print('%.3f seconds to plot qpf3 for: '+dom) % t3

#################################
  # Plot snow depth
#################################
  t1 = time.clock()
  print('Working on snow depth for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'in'
  clevs = [0.1,1,2,3,6,9,12,18,24,36,48]
  clevsdif = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]
  cm = ncepy.ncl_perc_11Lev()
  norm = matplotlib.colors.BoundaryNorm(clevs, cm.N) 
 
  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,snow_1,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,snow_2,clevs,cmap=cm,norm=norm,extend='both',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,snow_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))         
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('comparesnow_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparesnow_'+dom+'_f'+fhour+'z.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot snow depth for: '+dom) % t3

#################################
  # Plot 6-hr change in snow depth
#################################
#  if (fhr % 3 == 0) and (fhr >= 6):
#    t1 = time.clock()
#    print('Working on 6-hr change in snow depth for '+dom)

    # Clear off old plottables but keep all the map info
#    clear_plotables(ax1,keep_ax_lst_1,fig)
#    clear_plotables(ax2,keep_ax_lst_2,fig)
#    clear_plotables(ax3,keep_ax_lst_3,fig)

#    units = 'in'
#    clevs = [-6,-4,-3,-2,-1,-0.5,0,0.5,1,2,3,4,6]
#    clevsdif = [-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3]
#    colorlist = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']

#    for ax in axes:
#      xmin, xmax = ax.get_xlim()
#      ymin, ymax = ax.get_ylim()
#      xmax = int(round(xmax))
#      ymax = int(round(ymax))

#      if par == 1:
#        cs_1 = m.contourf(x,y,snow6_1,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_1.cmap.set_under('darkblue')
#        cs_1.cmap.set_over('darkred')
#        cbar1.remove()
#        cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#        cbar1.set_label(units,fontsize=6)
#        cbar1.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS 6-hr Change in Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 2:
#        cs_2 = m.contourf(x2,y2,snow6_2,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_2.cmap.set_under('darkblue')
#        cs_2.cmap.set_over('darkred')
#        cbar2.remove()
#        cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
#        cbar2.set_label(units,fontsize=6)
#        cbar2.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL 6-hr Change in Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 3:
#        cs = m.contourf(x2,y2,snow6_dif,clevsdif,colors=colorlist,extend='both',ax=ax)
#        cs.cmap.set_under('darkblue')
#        cs.cmap.set_over('darkred')
#        cbar3.remove()
#        cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#        cbar3.set_label(units,fontsize=6)
#        cbar3.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 6-hr Change in Snow Depth ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      par += 1
#    par = 1

#    compress_and_save('comparesnow6_'+dom+'t'+cyc+subcyc+'z.png')
#    plt.savefig('./comparesnow6_'+dom+'_f'+fhour+'z.png', bbox_inches='tight',dpi=150)
#    t2 = time.clock()
#    t3 = round(t2-t1, 3)
#    print('%.3f seconds to plot snow depth for: '+dom) % t3

#################################
  # Plot 1-km reflectivity
#################################
  t1 = time.clock()
  print('Working on 1-km reflectivity for '+dom)


  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'dBz'
  clevs = np.linspace(5,70,14)
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']
  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,ref1km_1,clevs,colors=colorlist,extend='min',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 1-km Reflectivity  ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ref1km_2,clevs,colors=colorlist,extend='min',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 1-km Reflectivity  ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,ref1km_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 1-km Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareref1km_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareref1km_'+dom+'_t'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 1-km reflectivity for: '+dom) % t3

#################################
  # Plot 4-km reflectivity
#################################
  t1 = time.clock()
  print('Working on 4-km reflectivity for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)
  units = 'dBz'
#  clevs = [0.25,0.5,1,2,3,4,5,10]
#  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15]
#  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']
  clevs = np.linspace(5,70,14)
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
#  clevsdif = [20,1000]
  colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']
  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,ref4km_1,clevs,colors=colorlist,extend='min',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS 1-km Reflectivity  ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ref4km_2,clevs,colors=colorlist,extend='min',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL 1-km Reflectivity  ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,ref4km_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS 1-km Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1



  compress_and_save('compareref4km_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compareref4km_'+dom+'_t'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 4-km reflectivity for: '+dom) % t3

#################################


#################################
  # Plot composite reflectivity
#################################
  t1 = time.clock()
  print('Working on composite reflectivity for '+dom)

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'dBz'
#  clevs = [0.25,0.5,1,2,3,4,5,10]
#  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15]
#  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']
  clevs = np.linspace(5,70,14)
#  clevsdif = [20,1000]
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
  colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']
  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,refc_1,clevs,colors=colorlist,extend='min',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS Composite Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,refc_2,clevs,colors=colorlist,extend='min',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL Composite Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,refc_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS Composite Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1


  compress_and_save('comparerefc_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./comparerefc_'+dom+'_t'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot composite reflectivity for: '+dom) % t3

#################################
  # Plot -10C Reflectivity
#################################
  t1 = time.clock()
 # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)
  clear_plotables(ax3,keep_ax_lst_3,fig)

  units = 'dBz'
#  clevs = [0.25,0.5,1,2,3,4,5,10]
#  clevsdif = [-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15]
#  colorlist = ['salmon','goldenrod','#EEEE00','palegreen','darkturquoise','blue','mediumpurple']
  clevs = np.linspace(5,70,14)
  clevsdif = [-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30]
#  clevsdif = [20,1000]
  colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']
  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
      cs_1 = m.contourf(x,y,ref10C_1,clevs,colors=colorlist,extend='min',ax=ax)
      cs_1.cmap.set_under('white')
      cbar1.remove()
      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar1.set_label(units,fontsize=6)
      cbar1.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA FGS -10'u'\xb0''C Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 2:
      cs_2 = m.contourf(x2,y2,ref10C_2,clevs,colors=colorlist,extend='min',ax=ax)
      cs_2.cmap.set_under('white')
      cbar2.remove()
      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05,ticks=clevs)
      cbar2.set_label(units,fontsize=6)
      cbar2.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL -10'u'\xb0''C Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    elif par == 3:
      cs = m.contourf(x2,y2,ref10C_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
      cs.cmap.set_under('darkblue')
      cs.cmap.set_over('darkred')
      cbar3.remove()
      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
      cbar3.set_label(units,fontsize=6)
      cbar3.ax.tick_params(labelsize=6)
      ax.text(.5,1.03,'3D-RTMA ANL - 3D-RTMA FGS -10'u'\xb0''C Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

    par += 1
  par = 1

  compress_and_save('compareref10C_'+dom+'_t'+cyc+subcyc+'z.png')
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print('%.3f seconds to plot 263K Reflectivity for: '+dom) % t3



######################################################

  t3dom = round(t2-t1dom, 3)
  print("%.3f seconds to plot all variables for: "+dom) % t3dom
  plt.clf()

######################################################

main()
