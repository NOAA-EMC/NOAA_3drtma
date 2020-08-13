import pygrib
import matplotlib
matplotlib.use('Agg')
import cStringIO
import matplotlib.pyplot as plt
from PIL import Image
import matplotlib.image as image
from matplotlib.gridspec import GridSpec
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
subcyc = str(minute).zfill(2)
print year, month, day, hour, minute

#fhr = int(sys.argv[2])
#fhour = str(fhr).zfill(2)
#print 'fhour '+fhour

# Define the output files
data1 = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_retro/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfprs_hrconus_00.grib2')
data2 = pygrib.open('/gpfs/dell2/stmp/Edward.Colon/rtma3d_wrkdir_retro/com2/rtma3d/lsf/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.fgs.wrfprs_hrconus_00.grib2')

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
domains = ['conus']

###################################################
# Read in all variables and calculate differences #
###################################################
t1a = time.clock()

#if (fhr > 0):
# Max/Min Hourly 2-5 km Updraft Helicity
maxuh25_1 = data1.select(stepType='max',parameterName="199",topLevel=5000,bottomLevel=2000)[0].values
maxuh25_2 = data2.select(stepType='max',parameterName="199",topLevel=5000,bottomLevel=2000)[0].values
minuh25_1 = data1.select(stepType='min',parameterName="200",topLevel=5000,bottomLevel=2000)[0].values
minuh25_2 = data2.select(stepType='min',parameterName="200",topLevel=5000,bottomLevel=2000)[0].values
maxuh25_1[maxuh25_1 < 10] = 0
maxuh25_2[maxuh25_2 < 10] = 0
minuh25_1[minuh25_1 > -10] = 0
minuh25_1[minuh25_2 > -10] = 0
uh25_1 = maxuh25_1 + minuh25_1
uh25_2 = maxuh25_2 + minuh25_2
uh25_dif = uh25_2 - uh25_1

# Max/Min Hourly 0-3 km Updraft Helicity
maxuh03_1 = data1.select(stepType='max',parameterName="199",topLevel=3000,bottomLevel=0)[0].values
maxuh03_2 = data2.select(stepType='max',parameterName="199",topLevel=3000,bottomLevel=0)[0].values
minuh03_1 = data1.select(stepType='min',parameterName="200",topLevel=3000,bottomLevel=0)[0].values
minuh03_2 = data2.select(stepType='min',parameterName="200",topLevel=3000,bottomLevel=0)[0].values
maxuh03_1[maxuh03_1 < 10] = 0
maxuh03_2[maxuh03_2 < 10] = 0
minuh03_1[minuh03_1 > -10] = 0
minuh03_1[minuh03_2 > -10] = 0
uh03_1 = maxuh03_1 + minuh03_1
uh03_2 = maxuh03_2 + minuh03_2
uh03_dif = uh03_2 - uh03_1

# Max Hourly Updraft Speed
#  maxuvv_1 = data1.select(stepType='max',parameterName="220",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values
#  maxuvv_2 = data2.select(stepType='max',parameterName="220",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values
#  maxuvv_dif = maxuvv_2 - maxuvv_1

# Max Hourly Downdraft Speed
#  maxdvv_1 = data1.select(stepType='max',parameterName="221",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values * -1
#  maxdvv_2 = data2.select(stepType='max',parameterName="221",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values * -1
#  maxdvv_dif = maxdvv_2 - maxdvv_1

# Max Hourly 1-km AGL reflectivity
maxref1km_1 = data1.select(parameterName="198",typeOfLevel="heightAboveGround",level=1000)[0].values
maxref1km_2 = data2.select(parameterName="198",typeOfLevel="heightAboveGround",level=1000)[0].values

# Max Hourly -10C reflectivity
#  maxref10C_1 = data1.select(parameterName="198",typeOfLevel="isothermal",level=263)[0].values
#  maxref10C_2 = data2.select(parameterName="198",typeOfLevel="isothermal",level=263)[0].values

# Max Hourly Wind
maxwind_1 = data1.select(name='10 metre wind speed',stepType='max')[0].values * 1.94384
maxwind_2 = data2.select(name='10 metre wind speed',stepType='max')[0].values * 1.94384
maxwind_dif = maxwind_2 - maxwind_1

# Max 0-1 km Vertical Vorticity
relv01_1 = data1.select(name='Vorticity (relative)',stepType='max',typeOfLevel='heightAboveGroundLayer',topLevel=1000,bottomLevel=0)[0].values
relv01_2 = data2.select(name='Vorticity (relative)',stepType='max',typeOfLevel='heightAboveGroundLayer',topLevel=1000,bottomLevel=0)[0].values
relv01_dif = relv01_2 - relv01_1

# Max 0-2 km Vertical Vorticity
relv02_1 = data1.select(name='Vorticity (relative)',stepType='max',typeOfLevel='heightAboveGroundLayer',topLevel=2000,bottomLevel=0)[0].values
relv02_2 = data2.select(name='Vorticity (relative)',stepType='max',typeOfLevel='heightAboveGroundLayer',topLevel=2000,bottomLevel=0)[0].values
relv02_dif = relv02_2 - relv02_1

# Max Hybrid Level 1 Vertical Vorticity
#  relvhyb_1 = data1.select(name='Vorticity (relative)',stepType='max',typeOfLevel='hybrid',level=1)[0].values
#relvhyb_2 = data2.select(name='Vorticity (relative)',stepType='max',typeOfLevel='heightAboveGroundLayer',topLevel=2000,bottomLevel=0)[0].values
#relvhyb_dif = relvhyb_2 - relvhyb_1

# Haines index
#hindex_1 = data1.select(parameterName="2",typeOfLevel='surface')[0].values
#hindex_2 = data2.select(parameterName="2",typeOfLevel='surface')[0].values
#hindex_dif = hindex_2 - hindex_1

# Transport wind
#utrans_1 = data1.select(name='U component of wind',nameOfFirstFixedSurface="220")[0].values * 1.94384
#vtrans_1 = data1.select(name='V component of wind',nameOfFirstFixedSurface="220")[0].values * 1.94384
#utrans_2 = data2.select(name='U component of wind',nameOfFirstFixedSurface="220")[0].values * 1.94384
#vtrans_2 = data2.select(name='V component of wind',nameOfFirstFixedSurface="220")[0].values * 1.94384
# Rotate winds from grid relative to Earth relative
#utrans_1, vtrans_1 = ncepy.rotate_wind(Lat0,Lon0,lon,utrans_1,vtrans_1,'lcc',inverse=False)
#utrans_2, vtrans_2 = ncepy.rotate_wind(Lat0,Lon0,lon2,utrans_2,vtrans_2,'lcc',inverse=False)
#trans_1 = np.sqrt(utrans_1**2 + vtrans_1**2)
#trans_2 = np.sqrt(utrans_2**2 + vtrans_2**2)
#trans_dif = trans_2 - trans_1

# Total cloud cover
#tcdc_1 = data1.select(name='Total Cloud Cover')[0].values
#tcdc_2 = data2.select(name='Total Cloud Cover')[0].values
#tcdc_dif = tcdc_2 - tcdc_1

# Echo top height
#retop_1 = data1.select(parameterName="197",stepType='instant',nameOfFirstFixedSurface='200')[0].values * (3.28084/1000)
#retop_2 = data2.select(parameterName="197",stepType='instant',nameOfFirstFixedSurface='200')[0].values * (3.28084/1000)
#retop_dif = retop_2 - retop_1

# Cloud base pressure
pbase_1 = data1.select(name='Pressure',typeOfLevel='cloudBase')[0].values * 0.01
pbase_2 = data2.select(name='Pressure',typeOfLevel='cloudBase')[0].values * 0.01
pbase_dif = pbase_2 - pbase_1

# Cloud top pressure
ptop_1 = data1.select(name='Pressure',typeOfLevel='cloudTop')[0].values * 0.01
ptop_2 = data2.select(name='Pressure',typeOfLevel='cloudTop')[0].values * 0.01
ptop_dif = ptop_2 - ptop_1


t2a = time.clock()
t3a = round(t2a-t1a, 3)
print("%.3f seconds to read all messages") % t3a

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
difcolors2 = ['blue','dodgerblue','turquoise','white','white','#EEEE00','darkorange','red']

########################################
#    START PLOTTING FOR EACH DOMAIN    #
########################################

def main():

  dom='conus'
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
  # Plot Max/Min Hourly 2-5 km UH
#################################
#  if (fhr > 0):
t1 = time.clock()
dom='conus'
print('Working on Max/Min Hourly 2-5 km UH for '+dom)
  # create figure and axes instances
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
im = image.imread('/gpfs/dell2/emc/modeling/noscrub/Benjamin.Blake/python.fv3/noaa.png')
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

units = 'm${^2}$ s$^{-2}$'
clevs = [-150,-100,-75,-50,-25,-10,0,10,25,50,75,100,150,200,250,300]
clevsdif = [-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60]
#    colorlist = ['white','skyblue','mediumblue','green','orchid','firebrick','#EEC900','DarkViolet']
colorlist = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','#E5E5E5','#E5E5E5','#EEEE00','#EEC900','darkorange','orangered','red','firebrick','mediumvioletred','darkviolet']

for ax in axes:
  xmin, xmax = ax.get_xlim()
  ymin, ymax = ax.get_ylim()
  xmax = int(round(xmax))
  ymax = int(round(ymax))

  if par == 1:
    cs_1 = m.contourf(x,y,uh25_1,clevs,colors=colorlist,extend='both',ax=ax)
    cs_1.cmap.set_under('darkblue')
    cs_1.cmap.set_over('black')
    cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
    cbar1.set_label(units,fontsize=6)
    cbar1.ax.tick_params(labelsize=6)
    ax.text(.5,1.03,'3D-RTMA ANL 1-h Max/Min 2-5 km Updraft Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
    ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
  elif par == 2:
    cs_2 = m.contourf(x2,y2,uh25_2,clevs,colors=colorlist,extend='both',ax=ax)
    cs_2.cmap.set_under('darkblue')
    cs_2.cmap.set_over('black')
    cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
    cbar2.set_label(units,fontsize=6)
    cbar2.ax.tick_params(labelsize=6)
    ax.text(.5,1.03,'3D-RTMA FGS 1-h Max/Min 2-5 km Updraft Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
    ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

  elif par == 3:
    cs = m.contourf(x2,y2,uh25_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
    cs.cmap.set_under('darkblue')
    cs.cmap.set_over('darkred')
    cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
    cbar3.set_label(units,fontsize=6)
    cbar3.ax.tick_params(labelsize=6)
    ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max/Min 2-5 km Updraft Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
    ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

  par += 1
par = 1

compress_and_save('compareuh25_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max/Min Hourly 2-5 km UH for: '+dom) % t3

#################################
  # Plot Max Hourly 0-3 km UH
#################################
t1 = time.clock()
dom='conus'
print('Working on Max Hourly 0-3 km UH for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1

m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

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
     cs_1 = m.contourf(x,y,uh03_1,clevs,colors=colorlist,extend='both',ax=ax)
     cs_1.cmap.set_under('darkblue')
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max/Min 0-3 km Updraft Helicity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
   elif par == 2:
     cs_2 = m.contourf(x2,y2,uh03_2,clevs,colors=colorlist,extend='both',ax=ax)
     cs_2.cmap.set_under('darkblue')
     cs_2.cmap.set_over('black')
     # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max/Min 0-3 km Updraft Helicity ('+units+') \n  valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     cs = m.contourf(x2,y2,uh03_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
     cs.cmap.set_under('darkblue')
     cs.cmap.set_over('darkred')
     # cbar3.remove()
     cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
     cbar3.set_label(units,fontsize=6)
     cbar3.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max/Min 0-3 km Updraft Helicity ('+units+') \n  valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('compareuh03_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max/Min Hourly 0-3 km UH for: '+dom) % t3

#################################
  # Plot Max Hourly Updraft Speed
#################################
#    t1 = time.clock()
#    print('Working on Max Hourly Updraft Speed for '+dom)

  # Clear off old plottables but keep all the map info
#    clear_plotables(ax1,keep_ax_lst_1,fig)
#    clear_plotables(ax2,keep_ax_lst_2,fig)
#    clear_plotables(ax3,keep_ax_lst_3,fig)

#    units = 'm s$^{-1}$'
#    clevs = [0.5,1,2.5,5,7.5,10,12.5,15,20,25,30,35,40,50,75]
#    clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
#    colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia','mediumpurple']

#    for ax in axes:
#      xmin, xmax = ax.get_xlim()
#      ymin, ymax = ax.get_ylim()
#      xmax = int(round(xmax))
#      ymax = int(round(ymax))

#      if par == 1:
#        cs_1 = m.contourf(x,y,maxuvv_1,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_1.cmap.set_under('white')
#        cs_1.cmap.set_over('black')
#        # cbar1.remove()
#        cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#        cbar1.set_label(units,fontsize=6)
#        cbar1.ax.set_xticklabels(clevs)
#        cbar1.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 100-1000 mb Updraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#      elif par == 2:
#        cs_2 = m.contourf(x2,y2,maxuvv_2,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_2.cmap.set_under('white')
#        cs_2.cmap.set_over('black')
#        # cbar2.remove()
#        cbar2 = m.colorbar(cs_2,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#        cbar2.set_label(units,fontsize=6)
#        cbar2.ax.set_xticklabels(clevs)
#        cbar2.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 100-1000 mb Updraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 3:
#        cs = m.contourf(x2,y2,maxuvv_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#        cs.cmap.set_under('darkblue')
#        cs.cmap.set_over('darkred')
#        # cbar3.remove()
#        cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#        cbar3.set_label(units,fontsize=6)
#        cbar3.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max 100-1000 mb Updraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      par += 1
#    par = 1

#    compress_and_save('comparemaxuvv_'+dom+'_f'+fhour+'.png')
#    t2 = time.clock()
#    t3 = round(t2-t1, 3)
#    print('%.3f seconds to plot Max Hourly Updraft Speed for: '+dom) % t3

#################################
  # Plot Max Hourly Downdraft Speed
#################################
#    t1 = time.clock()
#    print('Working on Max Hourly Downdraft Speed for '+dom)

  # Clear off old plottables but keep all the map info
#    clear_plotables(ax1,keep_ax_lst_1,fig)
#    clear_plotables(ax2,keep_ax_lst_2,fig)
#    clear_plotables(ax3,keep_ax_lst_3,fig)

#    for ax in axes:
#      xmin, xmax = ax.get_xlim()
#      ymin, ymax = ax.get_ylim()
#      xmax = int(round(xmax))
#      ymax = int(round(ymax))

#      if par == 1:
#        cs_1 = m.contourf(x,y,maxdvv_1,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_1.cmap.set_under('white')
#        cs_1.cmap.set_over('black')
#        # cbar1.remove()
#        cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#        cbar1.set_label(units,fontsize=6)
#        cbar1.ax.set_xticklabels(clevs)
#        cbar1.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 100-1000 mb Downdraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#      elif par == 2:
#        cs_2 = m.contourf(x2,y2,maxdvv_2,clevs,colors=colorlist,extend='both',ax=ax)
#        cs_2.cmap.set_under('white')
#        cs_2.cmap.set_over('black')
#        # cbar2.remove()
#        cbar2 = m.colorbar(cs_2,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#        cbar2.set_label(units,fontsize=6)
#        cbar2.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 100-1000 mb Downdraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      elif par == 3:
#        cs = m.contourf(x2,y2,maxdvv_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#        cs.cmap.set_under('darkblue')
#        cs.cmap.set_over('darkred')
#        # cbar3.remove()
#        cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#
#        cbar3.set_label(units,fontsize=6)
#        cbar3.ax.tick_params(labelsize=6)
#        ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max 100-1000 mb Downdraft Speed ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
#        ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#      par += 1
#    par = 1

#    compress_and_save('comparemaxdvv_'+dom+'_f'+fhour+'.png')
#    t2 = time.clock()
#    t3 = round(t2-t1, 3)
#    print('%.3f seconds to plot Max Hourly Downdraft Speed for: '+dom) % t3

#################################
  # Plot Max Hourly 1-km Reflectivity
#################################
t1 = time.clock()
dom='conus'
print('Working on Max Hourly 1-km Reflectivity for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

  # Clear off old plottables but keep all the map info
clear_plotables(ax1,keep_ax_lst_1,fig)
clear_plotables(ax2,keep_ax_lst_2,fig)
clear_plotables(ax3,keep_ax_lst_3,fig)

units='dBz'
clevs = np.linspace(5,70,14)
clevsdif = [20,1000]
colorlist = ['turquoise','dodgerblue','mediumblue','lime','limegreen','green','#EEEE00','#EEC900','darkorange','red','firebrick','darkred','fuchsia']

for ax in axes:
   xmin, xmax = ax.get_xlim()
   ymin, ymax = ax.get_ylim()
   xmax = int(round(xmax))
   ymax = int(round(ymax))

   if par == 1:
     cs_1 = m.contourf(x,y,maxref1km_1,clevs,colors=colorlist,extend='max',ax=ax)
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 1-km Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
   elif par == 2:
     cs_2 = m.contourf(x2,y2,maxref1km_2,clevs,colors=colorlist,extend='max',ax=ax)
     cs_2.cmap.set_over('black')
     # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 1-km Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     csdif = m.contourf(x,y,maxref1km_1,clevsdif,colors='red')
     csdif2 = m.contourf(x,y,maxref1km_2,clevsdif,colors='dodgerblue')
     # cbar3.remove()
     ax.text(.5,1.03,'3D-RTMA ANL (red) and 3D-RTMA FGS (blue) 1-h Max 1-km Reflectivity > 20 ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('comparemaxref1km_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max Hourly 1-km Reflectivity for: '+dom) % t3

#################################
  # Plot Max Hourly -10C Reflectivity
#################################
t1 = time.clock()
dom='conus'
print('Working on Max Hourly 263K Reflectivity for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

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
     cs_1 = m.contourf(x,y,maxref10C_1,clevs,colors=colorlist,extend='max',ax=ax)
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max -10'u'\xb0''C Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
   elif par == 2:
     cs_2 = m.contourf(x2,y2,maxref10C_2,clevs,colors=colorlist,extend='max',ax=ax)
     cs_2.cmap.set_over('black')
     # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max -10'u'\xb0''C Reflectivity ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     csdif = m.contourf(x,y,maxref10C_1,clevsdif,colors='red')
     csdif2 = m.contourf(x,y,maxref10C_2,clevsdif,colors='dodgerblue')
     ax.text(.5,1.03,'3D-RTMA ANL (red) and 3D-RTMA FGS (blue) 1-h Max -10'u'\xb0''C Reflectivity > 20 ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))       
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('comparemaxref10C_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max Hourly 263K Reflectivity for: '+dom) % t3

#################################
  # Plot Max Hourly 10-m Winds
#################################
t1 = time.clock()
dom='conus'
print('Working on Max Hourly 10-m Wind Speed for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

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
     cs_1 = m.contourf(x,y,maxwind_1,clevs,colors=colorlist,extend='max',ax=ax)
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 10-m Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
  
   elif par == 2:
     cs_2 = m.contourf(x2,y2,maxwind_2,clevs,colors=colorlist,extend='max',ax=ax)
     cs_2.cmap.set_over('black')
      # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 10-m Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     cs = m.contourf(x2,y2,maxwind_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
     cs.cmap.set_under('darkblue')
     cs.cmap.set_over('darkred')
     cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
     cbar3.set_label(units,fontsize=6)
     cbar3.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max 10-m Winds ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('comparemaxwind_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max Hourly 10-m Wind Speed for: '+dom) % t3

#################################
  # Plot Max 0-1 km Vertical Vorticity
#################################
t1 = time.clock()
dom='conus'
print('Working on Max 0-1 km Vorticity for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')


  # Clear off old plottables but keep all the map info
clear_plotables(ax1,keep_ax_lst_1,fig)
clear_plotables(ax2,keep_ax_lst_2,fig)
clear_plotables(ax3,keep_ax_lst_3,fig)

units = 's$^{-1}$'
clevs = [0.001,0.0025,0.005,0.0075,0.01,0.0125,0.015]
clevsdif = [-0.006,-0.005,-0.004,-0.003,-0.002,-0.001,0,0.001,0.002,0.003,0.004,0.005,0.006]
colorlist = ['#EEEE00','#EEC900','darkorange','red','firebrick','DarkViolet']

for ax in axes:
   xmin, xmax = ax.get_xlim()
   ymin, ymax = ax.get_ylim()
   xmax = int(round(xmax))
   ymax = int(round(ymax))

   if par == 1:
     cs_1 = m.contourf(x,y,relv01_1,clevs,colors=colorlist,extend='both',ax=ax)
     cs_1.cmap.set_under('white')
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.set_xticklabels(clevs)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 0-1 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
   elif par == 2:
     cs_2 = m.contourf(x2,y2,relv01_2,clevs,colors=colorlist,extend='both',ax=ax)
     cs_2.cmap.set_under('white')
     cs_2.cmap.set_over('black')
     # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.set_xticklabels(clevs)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 0-1 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     cs = m.contourf(x2,y2,relv01_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
     cs.cmap.set_under('darkblue')
     cs.cmap.set_over('darkred')
     # cbar3.remove()
     cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
     cbar3.set_label(units,fontsize=6)
     cbar3.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max 0-1 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('comparerelv01_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max 0-1 km Vorticity for: '+dom) % t3

#################################
  # Plot Max 0-2 km Vertical Vorticity
#################################
t1 = time.clock()
dom='conus'
print('Working on Max 0-2 km Vorticity for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

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
     cs_1 = m.contourf(x,y,relv02_1,clevs,colors=colorlist,extend='both',ax=ax)
     cs_1.cmap.set_under('white')
     cs_1.cmap.set_over('black')
     # cbar1.remove()
     cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
     cbar1.set_label(units,fontsize=6)
     cbar1.ax.set_xticklabels(clevs)
     cbar1.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max 0-2 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
   elif par == 2:
     cs_2 = m.contourf(x2,y2,relv02_2,clevs,colors=colorlist,extend='both',ax=ax)
     cs_2.cmap.set_under('white')
     cs_2.cmap.set_over('black')
     # cbar2.remove()
     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
     cbar2.set_label(units,fontsize=6)
     cbar2.ax.set_xticklabels(clevs)
     cbar2.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max 0-2 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   elif par == 3:
     cs = m.contourf(x2,y2,relv02_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
     cs.cmap.set_under('darkblue')
     cs.cmap.set_over('darkred')
     # cbar3.remove()
     cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
     cbar3.set_label(units,fontsize=6)
     cbar3.ax.tick_params(labelsize=6)
     ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max 0-2 km Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

     par += 1
   par = 1

compress_and_save('comparerelv02_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Max 0-2 km Vorticity for: '+dom) % t3

#################################
  # Plot Max Hybrid Level 1 Vertical Vorticity
#################################
#t1 = time.clock()
#print('Working on Max Hybrid Level 1 Vorticity for '+dom)

  # Clear off old plottables but keep all the map info
#clear_plotables(ax1,keep_ax_lst_1,fig)
#clear_plotables(ax2,keep_ax_lst_2,fig)
#clear_plotables(ax3,keep_ax_lst_3,fig)

#for ax in axes:
#   xmin, xmax = ax.get_xlim()
#   ymin, ymax = ax.get_ylim()
#   xmax = int(round(xmax))
#   ymax = int(round(ymax))

#   if par == 1:
#     cs_1 = m.contourf(x,y,relvhyb_1,clevs,colors=colorlist,extend='both',ax=ax)
#     cs_1.cmap.set_under('white')
#     cs_1.cmap.set_over('black')
     # cbar1.remove()
#     cbar1 = m.colorbar(cs_1,ax=ax,ticks=clevs,location='bottom',pad=0.05)
#     cbar1.set_label(units,fontsize=6)
#     cbar1.ax.set_xticklabels(clevs)
#     cbar1.ax.tick_params(labelsize=6)
#     ax.text(.5,1.03,'3D-RTMA ANL 1-h Max Lowest Mdl Lvl Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#   elif par == 2:
#     cs_2 = m.contourf(x2,y2,relvhyb_2,clevs,colors=colorlist,extend='both',ax=ax)
#     cs_2.cmap.set_under('white')
#     cs_2.cmap.set_over('black')
     # cbar2.remove()
#     cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#     cbar2.set_label(units,fontsize=6)
#     cbar2.ax.set_xticklabels(clevs)
#     cbar2.ax.tick_params(labelsize=6)
#     ax.text(.5,1.03,'3D-RTMA FGS 1-h Max Lowest Mdl Lvl Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#   elif par == 3:
#     cs = m.contourf(x2,y2,relvhyb_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#     cs.cmap.set_under('darkblue')
#     cs.cmap.set_over('darkred')
     # cbar3.remove()
#     cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#     cbar3.set_label(units,fontsize=6)
#     cbar3.ax.tick_params(labelsize=6)
#     ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL 1-h Max Lowest Mdl Lvl Vertical $\zeta$ ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#     ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#     par += 1
#   par = 1

#compress_and_save('comparerelvhyb_'+dom+'t'+cyc+subcyc+'z.png')
#t2 = time.clock()
#t3 = round(t2-t1, 3)
#print('%.3f seconds to plot Max Hybrid Level 1 Vorticity for: '+dom) % t3

#################################
  # Plot Haines Index
#################################
#  t1 = time.clock()
#  print('Working on Haines Index for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = ''
#  clevs = [1.5,2.5,3.5,4.5,5.5,6.5]
#  clevsdif = [-4,-3,-2,-1,0,1,2,3,4]
#  colorlist = ['dodgerblue','limegreen','#EEEE00','darkorange','crimson']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,hindex_1,clevs,colors=colorlist,ax=ax)
#      if (fhr > 0):
#        # cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,ticks=[2,3,4,5,6],location='bottom',pad=0.05)
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Haines Index \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,hindex_2,clevs,colors=colorlist,ax=ax)
#      if (fhr > 0):
#        # cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,ticks=[2,3,4,5,6],location='bottom',pad=0.05)
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Haines Index \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,hindex_dif,clevsdif,colors=difcolors2,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      if (fhr > 0):
#        # cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Haines Index \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparehindex_'+dom+'_f'+fhour+'.png')
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Haines Index for: '+dom) % t3

#################################
  # Plot transport wind
#################################
#  t1 = time.clock()
#  print('Working on transport wind for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'kts'
#  if dom == 'conus':
#    skip = 80
#  elif dom == 'SE':
#    skip = 35
#  elif dom == 'CO' or dom == 'LA' or dom == 'MA':
#    skip = 12
#  elif dom == 'BN':
#    skip = 10
#  elif dom == 'SP':
#    skip = 9
#  elif dom == 'SF':
#    skip = 3
#  else:
#    skip = 20
#  barblength = 4
#  clevs = [5,10,15,20,25,30,35,40,45,50,55,60]
#  clevsdif = [-18,-15,-12,-9,-6,-3,0,3,6,9,12,15,18]
#  colorlist = ['turquoise','dodgerblue','blue','#FFF68F','#E3CF57','peru','brown','crimson','red','fuchsia','DarkViolet']

  # Rotate winds to gnomonic projection
#  urot_1, vrot_1 = m.rotate_vector(utrans_1,vtrans_1,lon,lat)
#  urot_2, vrot_2 = m.rotate_vector(utrans_2,vtrans_2,lon2,lat2)

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,trans_1,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_1.cmap.set_over('black')
#      # cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_1[::skip,::skip],vrot_1[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
#      ax.text(.5,1.03,'3D-RTMA ANL Transport Wind ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,trans_2,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_2.cmap.set_over('black')
#      # cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      m.barbs(lon[::skip,::skip],lat[::skip,::skip],urot_2[::skip,::skip],vrot_2[::skip,::skip],latlon=True,length=barblength,linewidth=0.5,color='black',ax=ax)
#      ax.text(.5,1.03,'3D-RTMA FGS Transport Wind ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,trans_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      # cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Transport Wind ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparetrans_'+dom+'_f'+fhour+'.png')
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot transport wind for: '+dom) % t3

#################################
  # Plot Total Cloud Cover
#################################
#  t1 = time.clock()
#  print('Working on Total Cloud Cover for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = '%'
#  clevs = [0,10,20,30,40,50,60,70,80,90,100]
#  clevsdif = [-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60]

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,tcdc_1,clevs,cmap=plt.cm.BuGn,ax=ax)
#      # cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Total Cloud Cover ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,tcdc_2,clevs,cmap=plt.cm.BuGn,ax=ax)
#      # cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Total Cloud Cover ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,tcdc_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      # cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Total Cloud Cover ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('comparetcdc_'+dom+'_f'+fhour+'.png')
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Total Cloud Cover for: '+dom) % t3

#################################
  # Plot Echo Top Height
#################################
#  t1 = time.clock()
#  print('Working on Echo Top Height for '+dom)

  # Clear off old plottables but keep all the map info
#  clear_plotables(ax1,keep_ax_lst_1,fig)
#  clear_plotables(ax2,keep_ax_lst_2,fig)
#  clear_plotables(ax3,keep_ax_lst_3,fig)

#  units = 'kft'
#  clevs = [1,5,10,15,20,25,30,35,40]
#  clevsdif = [-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12]
#  colorlist = ['firebrick','tomato','lightsalmon','goldenrod','#EEEE00','palegreen','mediumspringgreen','limegreen']

#  for ax in axes:
#    xmin, xmax = ax.get_xlim()
#    ymin, ymax = ax.get_ylim()
#    xmax = int(round(xmax))
#    ymax = int(round(ymax))

#    if par == 1:
#      cs_1 = m.contourf(x,y,retop_1,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_1.cmap.set_over('darkgreen')
#      # cbar1.remove()
#      cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
#      cbar1.set_label(units,fontsize=6)
#      cbar1.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA ANL Echo Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
#    elif par == 2:
#      cs_2 = m.contourf(x2,y2,retop_2,clevs,colors=colorlist,extend='max',ax=ax)
#      cs_2.cmap.set_over('darkgreen')
#      # cbar2.remove()
#      cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
#      cbar2.set_label(units,fontsize=6)
#      cbar2.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS Echo Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    elif par == 3:
#      cs = m.contourf(x2,y2,retop_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
#      cs.cmap.set_under('darkblue')
#      cs.cmap.set_over('darkred')
#      # cbar3.remove()
#      cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
#      cbar3.set_label(units,fontsize=6)
#      cbar3.ax.tick_params(labelsize=6)
#      ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Echo Top Height ('+units+') \n initialized: '+itime+' valid: '+vtime + ' (f'+fhour+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
#      ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

#    par += 1
#  par = 1

#  compress_and_save('compareretop_'+dom+'_f'+fhour+'.png')
#  t2 = time.clock()
#  t3 = round(t2-t1, 3)
#  print('%.3f seconds to plot Echo Top Height for: '+dom) % t3

#################################
  # Plot Cloud Base Pressure
#################################
t1 = time.clock()
dom='conus'
print('Working on Cloud Base Pressure for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1

m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

  # Clear off old plottables but keep all the map info
clear_plotables(ax1,keep_ax_lst_1,fig)
clear_plotables(ax2,keep_ax_lst_2,fig)
clear_plotables(ax3,keep_ax_lst_3,fig)

units = 'mb'
clevs = [50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000]
clevsdif = [-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300]
hex=['#F00000','#F03800','#F55200','#F57200','#FA8900','#FFA200','#FFC800','#FFEE00','#BFFF00','#8CFF00','#11FF00','#05FF7E','#05F7FF','#05B8FF','#0088FF','#0055FF','#002BFF','#3700FF','#6E00FF','#A600FF','#E400F5']
hex=hex[::-1]

for ax in axes:
 xmin, xmax = ax.get_xlim()
 ymin, ymax = ax.get_ylim()
 xmax = int(round(xmax))
 ymax = int(round(ymax))

 if par == 1:
   cs_1 = m.contourf(x,y,pbase_1,clevs,colors=hex,extend='max',ax=ax)
   cs_1.cmap.set_over('red')
   # cbar1.remove()
   cbar1 = m.colorbar(cs_1,ax=ax,location='bottom',pad=0.05)
   cbar1.set_label(units,fontsize=6)
   cbar1.ax.tick_params(labelsize=6)
   ax.text(.5,1.03,'3D-RTMA ANL Pressure at Cloud Base ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
    
 elif par == 2:
   cs_2 = m.contourf(x2,y2,pbase_2,clevs,colors=hex,extend='max',ax=ax)
   cs_2.cmap.set_over('red')
   # cbar2.remove()
   cbar2 = m.colorbar(cs_2,ax=ax,location='bottom',pad=0.05)
   cbar2.set_label(units,fontsize=6)
   cbar2.ax.tick_params(labelsize=6)
   ax.text(.5,1.03,'3D-RTMA FGS Pressure at Cloud Base ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

 elif par == 3:
   cs = m.contourf(x2,y2,pbase_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
   cs.cmap.set_under('darkblue')
   cs.cmap.set_over('darkred')
   # cbar3.remove()
   cbar3 = m.colorbar(cs,ax=ax,location='bottom',pad=0.05)
   cbar3.set_label(units,fontsize=6)
   cbar3.ax.tick_params(labelsize=6)
   ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Pressure at Cloud Base ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

   par += 1
 par = 1

compress_and_save('comparepbase_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Cloud Base Pressure for: '+dom) % t3

#################################
  # Plot Cloud Top Pressure
#################################
t1 = time.clock()
dom='conus'
print('Working on Cloud Top Pressure for '+dom)
fig = plt.figure()
gs = GridSpec(9,9,wspace=0.0,hspace=0.0)
ax1 = fig.add_subplot(gs[0:4,0:4])
ax2 = fig.add_subplot(gs[0:4,5:])
ax3 = fig.add_subplot(gs[5:,1:8])
axes = [ax1, ax2, ax3]
par=1
m = Basemap(ax=ax,projection='gnom',lat_0=lat_0,lon_0=lon_0,\
                  llcrnrlat=llcrnrlat, urcrnrlat=urcrnrlat,\
                  llcrnrlon=llcrnrlon, urcrnrlon=urcrnrlon,\
                  resolution='l')

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
   cs_1 = m.contourf(x,y,ptop_1,clevs,colors=hex,extend='max',ax=ax)
   cs_1.cmap.set_over('red')
   ax.text(.5,1.03,'3D-RTMA ANL Pressure at Cloud Top ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)
  
 elif par == 2:
   cs_2 = m.contourf(x2,y2,ptop_2,clevs,colors=hex,extend='max',ax=ax)
   cs_2.cmap.set_over('red')
   ax.text(.5,1.03,'3D-RTMA FGS Pressure at Cloud Top ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

 elif par == 3:
   cs = m.contourf(x2,y2,ptop_dif,clevsdif,colors=difcolors,extend='both',ax=ax)
   cs.cmap.set_under('darkblue')
   cs.cmap.set_over('darkred')
   ax.text(.5,1.03,'3D-RTMA FGS - 3D-RTMA ANL Pressure at Cloud Top ('+units+') \n valid: '+vtime,horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=0.85,boxstyle='square,pad=0.2'))
   ax.imshow(im,aspect='equal',alpha=0.5,origin='upper',extent=(0,int(round(xmax*xscale)),0,int(round(ymax*yscale))),zorder=4)

 par += 1
 par = 1

compress_and_save('compareptop_'+dom+'t'+cyc+subcyc+'z.png')
t2 = time.clock()
t3 = round(t2-t1, 3)
print('%.3f seconds to plot Cloud Top Pressure for: '+dom) % t3


######################################################

t3dom = round(t2-t1dom, 3)
print("%.3f seconds to plot all variables for: "+dom) % t3dom
plt.clf()

######################################################

main()
