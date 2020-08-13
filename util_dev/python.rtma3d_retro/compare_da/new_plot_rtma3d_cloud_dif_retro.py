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
subcyc=str(minute).zfill(2)
print(year, month, day, hour, minute)

graph_option = np.int(str(sys.argv[2]))
llcrnrlon1 = np.float(str(sys.argv[3]))
llcrnrlat1 = np.float(str(sys.argv[4]))
urcrnrlon1 = np.float(str(sys.argv[5]))
urcrnrlat1 = np.float(str(sys.argv[6]))
lat_01 = np.float(str(sys.argv[7]))
lon_01 = np.float(str(sys.argv[8]))
xscale1 = np.float(str(sys.argv[9]))
yscale1 = np.float(str(sys.argv[10]))
comrtma3d = str(sys.argv[11])

graph_a = np.int(0)
graph_b = np.int(1)



# Define the output files
paraind = pygrib.open(comrtma3d+'/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs.grib2')
prodind = pygrib.open(comrtma3d+'/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'z/rtma3d.t'+cyc+subcyc+'z.wrfsubhprs_fgs.grib2')

print((comrtma3d+'/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'00z/rtma3d.t'+cyc+subcyc+'00z.wrfsubhprs.grib2'))
print((comrtma3d+'/rtma3d.'+str(ymd)+'/postprd.t'+cyc+subcyc+'00z/rtma3d.t'+cyc+subcyc+'00z.wrfsubhprs_fgs.grib2'))


# Get the lats and lons
lat,lon = prodind.select(name='2 metre temperature')[0].latlons()
lat2,lon2 = paraind.select(name='2 metre temperature')[0].latlons()
Lon0 = prodind[1]['LoVInDegrees']
Lat0 = prodind[1]['LaDInDegrees']

# Forecast valid date/time
vtime = ymdhm
#vtime = ncepy.ndate(itime,int(fhr))


if graph_option == graph_a:
	domains = ['conus','custom']
elif graph_option == graph_b:
	domains = ['conus']
else:
	domains = ['custom']


###################################################
# Read in all variables and calculate differences #
###################################################
t1a = time.clock()

#lcdchex=["#E65956", "#D93B3A", "#D22C2C", "#CC1E1E", "darkred"]
lcdchex=["#E65956", "#D93B3A", "#D22C2C", "firebrick", "darkred",'blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
mcdchex=["#5EE240", "#4DC534", "#3DA828", "#2D8B1C", "#1D6F11",'blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
hcdchex=["#64B3E8", "#5197D7", "#3E7CC6", "#2B60B5", "#1945A4",'blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']



lcdcprod=prodind.select(parameterName='Low cloud cover')[0].values
mcdcprod=prodind.select(parameterName='Medium cloud cover')[0].values
hcdcprod=prodind.select(parameterName='High cloud cover')[0].values
lcdcpara=paraind.select(parameterName='Low cloud cover')[0].values
mcdcpara=paraind.select(parameterName='Medium cloud cover')[0].values
hcdcpara=paraind.select(parameterName='High cloud cover')[0].values

lats,lons=prodind.select(name='Categorical snow',level=0)[0].latlons()


t2a = time.clock()
t3a = round(t2a-t1a, 3)
print(("%.3f seconds to read all messages") % t3a)

# colors for difference plots, only need to define once
difcolors = ['blue','#1874CD','dodgerblue','deepskyblue','turquoise','white','white','#EEEE00','#EEC900','darkorange','orangered','red']
#print('Length of', domains, 'is', len(domains))
########################################
#    START PLOTTING FOR EACH DOMAIN    #
########################################

def main():

  # Number of processes must coincide with the number of domains to plot
  pool = multiprocessing.Pool(6)
  pool.map(plot_all,domains)

def plot_all(dom):

  t1dom = time.clock()
  print(('Working on '+dom))

  # create figure and axes instances
  fig = plt.figure()
  gs = GridSpec(4,12,wspace=0.0,hspace=0.0)
  ax1 = plt.subplot(gs[0:4,0:6])
  ax2 = plt.subplot(gs[0:4,6:12])
  axes = [ax1, ax2]
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
  elif dom == 'custom':
    llcrnrlon=llcrnrlon1
    llcrnrlat=llcrnrlat1
    urcrnrlon=urcrnrlon1
    urcrnrlat=urcrnrlat1
    lat_0 = lat_01
    lon_0 = lon_01
    xscale=xscale1
    yscale=yscale1


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
  print(('Working on cloud for '+dom))

  # Clear off old plottables but keep all the map info
  clear_plotables(ax1,keep_ax_lst_1,fig)
  clear_plotables(ax2,keep_ax_lst_2,fig)

  clevs=[50,60,70,80,90,100]

  for ax in axes:
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    xmax = int(round(xmax))
    ymax = int(round(ymax))

    if par == 1:
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

    par += 1
  par = 1

  compress_and_save('comparecloud_'+dom+'_t'+cyc+subcyc+'z.png')
#  plt.savefig('./compare2mt_'+dom+'t'+fhour+'.png', bbox_inches='tight',dpi=150)
  t2 = time.clock()
  t3 = round(t2-t1, 3)
  print(('%.3f seconds to plot cloud for: '+dom) % t3)


main()
