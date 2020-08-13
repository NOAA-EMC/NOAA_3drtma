import numpy as np
import sys
from scipy.ndimage.filters import minimum_filter, maximum_filter
import matplotlib.pyplot as plt
import dateutil.relativedelta, dateutil.parser
from subprocess import call
from matplotlib import colors

'''

Time and date

'''

def ndate(cdate,hours):
   if not isinstance(cdate, str):
     if isinstance(cdate, int):
       cdate=str(cdate)
     else:
       sys.exit('NDATE: Error - input cdate must be string or integer.  Exit!')
   if not isinstance(hours, int):
     if isinstance(hours, str):
       hours=int(hours)
     else:
       sys.exit('NDATE: Error - input delta hour must be a string or integer.  Exit!')

   indate=cdate.strip()
   hh=indate[8:10]
   yyyy=indate[0:4]
   mm=indate[4:6]
   dd=indate[6:8]
   #set date/time field
   parseme=(yyyy+' '+mm+' '+dd+' '+hh)
   datetime_cdate=dateutil.parser.parse(parseme)
   valid=datetime_cdate+dateutil.relativedelta.relativedelta(hours=+hours)
   vyyyy=str(valid.year)
   vm=str(valid.month).zfill(2)
   vd=str(valid.day).zfill(2)
   vh=str(valid.hour).zfill(2)
   return vyyyy+vm+vd+vh

'''

WINDS

'''

def get_rotll_rotation_angles(instnlat,instnlon,TLM0D,TPH0D):
  #
  # Returns the cosine and sine of the rotation anlge(s) needed to
  # transform rotated lat lon grid-relative winds to
  # earth-relative and vice-versa.
  #
  # INPUT Args:
  #
  # TLM0D - the angle of rotation of the transformed lat-lon
  #         system in the longitudinal direction, degs
  # TPH0D - the angle of rotation of the transformed lat-lon
  #         system in the latitudinal direction, degs
  # instnlat - latitude in earth coords (degs)
  # instnlon - longitude in earth coords (degs) ---> Input is assumed to be
  #            negative west and we convert in this routine to get the right
  #            rotation angles
  # u_grid - u component grid relative winds from rotated lat-lon grid
  # v_grid - v component grid relative winds from rotated lat-lon grid
  #
  # Returns CROT,SROT
  ###################################################################

  #########################################
  DTR=np.pi/180.
  stnlat=instnlat*DTR
  stnlon=instnlon*DTR
  TPH0=TPH0D*DTR
  TLM0=TLM0D*DTR
  CTPH0=np.cos(TPH0)
  STPH0=np.sin(TPH0)
  RELM=stnlon-TLM0
  SRLM=np.sin(RELM)
  CRLM=np.cos(RELM)
  SPH=np.sin(stnlat)
  CPH=np.cos(stnlat)
  TPH=np.arcsin(CTPH0*SPH-STPH0*CPH*CRLM)
  RCTPH=1.0/np.cos(TPH)
  SROT=STPH0*SRLM*RCTPH
  CROT=(CTPH0*CPH+STPH0*SPH*CRLM)*RCTPH
  return CROT,SROT

def rotll2earth_winds(u,v,earthlat,earthlon,TLM0D,TPH0D):
  #
  # Rotate winds from rotated lat lon grid to earth relative
  #
  # INPUT Args:
  #   u, v -  Grid relative u and v winds
  #   TLM0D - the angle of rotation of the transformed lat-lon
  #         system in the longitudinal direction, degs
  #   TPH0D - the angle of rotation of the transformed lat-lon
  #         system in the latitudinal direction, degs
  #   earthlat - latitude in earth coords (degs)
  #   earthlon - longitude in earth coords (degs)
  #
  #
  # Returns u_earth,v_earth
  ###################################################################
  COSALP,SINALP=get_rotll_rotation_angles(earthlat,earthlon,TLM0D,TPH0D)
  u_earth = u*COSALP+v*SINALP #This is an elementwise product: NOT a matrix multiply
  v_earth = v*COSALP-u*SINALP #This is an elementwise product: NOT a matrix multiply
  return u_earth,v_earth

def earth2rotll_winds(u,v,earthlat,earthlon,TLM0D,TPH0D):
  #
  # Rotate winds from earth relative to rotated lat-lon grid
  #
  # INPUT Args:
  #   u, v -  Earth relative u and v winds
  #   TLM0D - the angle of rotation of the transformed lat-lon
  #         system in the longitudinal direction, degs
  #   TPH0D - the angle of rotation of the transformed lat-lon
  #         system in the latitudinal direction, degs
  #   earthlat - latitude in earth coords (degs)
  #   earthlon - longitude in earth coords (degs)
  #
  # Returns u_grid,v_grid
  ###################################################################
  COSALP,SINALP=get_rotll_rotation_angles(earthlat,earthlon,TLM0D,TPH0D)
  u_grid = u*COSALP-v*SINALP #This is an elementwise product: NOT a matrix multiply
  v_grid = u*SINALP+v*COSALP #This is an elementwise product: NOT a matrix multiply
  return u_grid,v_grid

def lcc_2_earth_winds(true_lat,lov_lon,earth_lons,ug,vg):
  #  Rotate winds from LCC relative to earth relative.
  proj='lcc'
  ue,ve=rotate_wind(true_lat,lov_lon,earth_lons,ug,vg,proj,inverse=False)
  return ue,ve

def rotate_wind(true_lat,lov_lon,earth_lons,uin,vin,proj,inverse=False):
  #  Rotate winds from LCC relative to earth relative (or vice-versa if inverse==true)
  #   This routine is vectorized and *should* work on any size 2D vg and ug arrays.
  #   Program will quit if dimensions are too large.
  #
  # Input args:
  #  true_lat = True latitidue for LCC projection (single value in degrees)
  #  lov_lon  = The LOV value from grib (e.g. - -95.0) (single value in degrees)
  #              Grib doc says: "Lov = orientation of the grid; i.e. the east longitude value of
  #                              the meridian which is parallel to the Y-axis (or columns of the grid)
  #                              along which latitude increases as the Y-coordinate increases (the
  #                              orientation longitude may or may not appear on a particular grid).
  #
  #  earth_lons = Earth relative longitudes (can be an array, in degrees)
  #  uin, vin     = Input winds to rotate
  #
  # Returns:
  #  uout, vout = Output, rotated winds
  #-----------------------------------------------------------------------------------------------------

  # Get size and length of input u winds, if not 2d, raise an error
  q=np.shape(uin)
  ndims=len(q)
  if ndims > 2:
    # Raise error and quit!
    raise SystemExit("Input winds for rotation have greater than 2 dimensions!")
  if lov_lon > 0.: lov_lon=lov_lon-360.
  dtr=np.pi/180.0             # Degrees to radians

  if not isinstance(inverse, bool):
    raise TypeError("**kwarg inverse must be of type bool.")

  # Compute rotation constant which is also
  # known as the Lambert cone constant.  In the case
  # of a polar stereographic projection, this is one.
  # See the following pdf for excellent documentation
  # http://www.dtcenter.org/met/users/docs/write_ups/velocity.pdf
  if proj.lower()=='lcc':
    rotcon_p=np.sin(true_lat*dtr)
  elif proj.lower() in ['stere','spstere', 'npstere']:
    rotcon_p=1.0
  else:
    raise SystemExit("Unsupported map projection: "+proj.lower()+" for wind rotation.")

  angles = rotcon_p*(earth_lons-lov_lon)*dtr
  sinx2 = np.sin(angles)
  cosx2 = np.cos(angles)

  # Steps below are elementwise products, not matrix mutliplies
  if inverse==False:
    # Return the earth relative winds
    uout = cosx2*uin+sinx2*vin
    vout =-sinx2*uin+cosx2*vin
  elif inverse==True:
    # Return the grid relative winds
    uout = cosx2*uin-sinx2*vin
    vout = sinx2*uin+cosx2*vin

  return uout,vout


def sd2uv(spd,dir):
    # - Converts speed and direction to u and v
    rad = np.pi/180.0
    u = -spd*np.sin(rad*dir)
    v = -spd*np.cos(rad*dir)
    return u,v

def uv2sd(u,v):
    # - Converts u and v to speed and direction
    rad2deg = 180.0/np.pi
    spd=np.sqrt(u*u+v*v)
    dir=90.0-(rad2deg*np.arctan2(-1.0*v,-1.0*u))

    # Make fixing of negative directions np array compatible
    a=np.where(dir<0.0)
    dir[a]=dir[a]+360.0

    return spd,dir


def kts2ms(spd):
    #Convert wind in knots to meters/second
    return spd*0.514444

def ms2kts(spd):
    #Convert wind in meters/second to knots
    return spd*1.94384449


'''

AVIATION

'''
def get_artcc_name(city):
# Return Air Route Traffic Control Center (ARTCC) abbreviations for input city
  if city.upper()=='SEATTLE':
    name='ZSE'
  elif city.upper()=='OAKLAND':
    name='ZOA'
  elif city.upper()=='SALT_LAKE_CITY':
    name='ZSC'
  elif city.upper()=='LOS_ANGELES':
    name='ZLA'
  elif city.upper()=='DENVER':
    name='ZDV'
  elif city.upper()=='ALBUQUERQUE':
    name='ZAB'
  elif city.upper()=='HOUSTON':
    name='ZHU'
  elif city.upper()=='FORT_WORTH':
    name='ZFW'
  elif city.upper()=='KANSAS_CITY':
    name='ZKC'
  elif city.upper()=='MINNEAPOLIS':
    name='ZMP'
  elif city.upper()=='CHICAGO':
    name='ZAU'
  elif city.upper()=='INDIANAPOLIS':
    name='ZID'
  elif city.upper()=='MEMPHIS':
    name='ZME'
  elif city.upper()=='ATLANTA':
    name='ZTL'
  elif city.upper()=='JACKSONVILLE':
    name='ZJX'
  elif city.upper()=='MIAMI':
    name='ZMA'
  elif city.upper()=='WASHINGTON':
    name='ZDC'
  elif city.upper()=='CLEVELAND':
    name='ZOB'
  elif city.upper()=='NEW_YORK':
    name='ZNY'
  elif city.upper()=='BOSTON':
    name='ZBW'
  elif city.upper()=='ANCHORAGE':
    name='ZAN'
  else:
    print 'Unable to locate ARTCC abbrev. name for ',city,' - using input value'
    name=city.upper()

  return name


def top25airways():
# Returns a list of the top25 aviation routes over the CONUS provided by AWC.
#  Generally used in conjunction with an airways shapefile.
  return  ['J36','J95','J64','J60','J80','J6','J48','J75','J121',
           'J174','J79','J53','J109','J211','J518','J220','J134',
            'J149','J89','J99','J85','J78','J180','J87','J74','J166']

'''
COLOR SHADING / COLOR BARS

'''

def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
# Grabbed from stackoverflow post:
#   http://stackoverflow.com/questions/18926031/how-to-extract-a-subset-of-a-colormap-as-a-new-colormap-in-matplotlib
    new_cmap = colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap


def howv_cmap():
    r=[0.000000,0.568627,0.627451,0.682353,0.741177,0.800000,0.858824,0.913725,1.000000,0.427451,0.396078,
       0.419608,0.439216,0.462745,0.482353,0.505882,0.529412,0.560784,0.376471,0.392157,0.407843,0.423529,
       0.443137,0.458824,0.474510,0.490196,0.509804,0.082353,0.086275,0.094118,0.098039,0.105882,0.109804,
       0.117647,0.121569,0.129412,0.329412,0.352941,0.376471,0.396078,0.419608,0.443137,0.466667,0.490196,
       0.517647,0.349020,0.360784,0.368627,0.380392,0.388235,0.400000,0.407843,0.419608,0.431373,0.125490,
       0.141176,0.152941,0.168627,0.184314,0.196078,0.211765,0.227451,0.243137,0.717647,0.745098,0.772549,
       0.800000,0.827451,0.854902,0.882353,0.909804,0.945098,0.850980,0.870588,0.886274,0.905882,0.925490,
       0.941176,0.960784,0.976471,1.000000,0.878431,0.894118,0.909804,0.921569,0.937255,0.952941,0.968627,
       0.980392,1.000000,0.850980,0.870588,0.886274,0.905882,0.925490,0.941176,0.960784,0.976471,1.000000,
       0.749020,0.780392,0.811765,0.839216,0.870588,0.901961,0.933333,0.964706,1.000000,0.458824,0.494118,
       0.529412,0.560784,0.596078,0.631373,0.666667,0.698039,0.741177,0.450980,0.486274,0.521569,0.556863,
       0.592157,0.627451,0.662745,0.698039,0.741177,0.352941,0.372549,0.388235,0.407843,0.427451,0.447059,
       0.462745,0.482353,0.505882,0.701961,0.705882,0.713726,0.717647,0.725490,0.729412,0.737255,0.737255,
       0.741177,0.741177,0.745098,0.745098,0.749020,0.752941,0.752941,0.756863,0.756863,0.760784,0.764706,
       0.764706,0.768627,0.768627,0.772549,0.776471,0.776471,0.780392,0.780392,0.784314,0.784314,0.788235,
       0.792157,0.792157,0.796078,0.796078,0.800000,0.803922,0.803922,0.807843,0.807843,0.811765,0.811765,
       0.815686,0.819608,0.819608,0.823529,0.823529,0.827451,0.831373,0.831373,0.835294,0.835294,0.839216,
       0.843137,0.843137,0.847059,0.847059,0.850980,0.850980,0.854902,0.858824,0.858824,0.862745,0.862745,
       0.866667,0.870588,0.870588,0.874510,0.874510,0.878431,0.882353,0.882353,0.886274,0.886274,0.890196,
       0.890196,0.894118,0.898039,0.898039,0.901961,0.901961,0.905882,0.909804,0.909804,0.913725,0.913725,
       0.917647,0.921569,0.921569,0.925490,0.925490,0.929412,0.929412,0.933333,0.937255,0.937255,0.941176,
       0.941176,0.945098,0.949020,0.949020,0.952941,0.952941,0.956863,0.956863,0.960784,0.964706,0.964706,
       0.968627,0.968627,0.972549,0.976471,0.976471,0.980392,0.980392,0.984314,0.988235,0.988235,0.992157,
       0.992157,0.996078,1.000000]
    g=[0.000000,0.568627,0.627451,0.682353,0.741177,0.800000,0.858824,0.913725,1.000000,0.749020,0.698039,
       0.737255,0.776471,0.811765,0.850980,0.890196,0.929412,0.984314,0.509804,0.533333,0.552941,0.576471,
       0.596078,0.619608,0.643137,0.662745,0.690196,0.250980,0.270588,0.290196,0.305882,0.325490,0.345098,
       0.364706,0.380392,0.403922,0.639216,0.682353,0.725490,0.772549,0.815686,0.858824,0.901961,0.945098,
       1.000000,0.678431,0.698039,0.717647,0.737255,0.756863,0.776471,0.796078,0.815686,0.839216,0.411765,
       0.458824,0.501961,0.549020,0.596078,0.639216,0.686275,0.733333,0.788235,0.760784,0.788235,0.819608,
       0.847059,0.878431,0.905882,0.937255,0.964706,1.000000,0.729412,0.745098,0.760784,0.776471,0.796078,
       0.811765,0.827451,0.843137,0.862745,0.552941,0.560784,0.572549,0.580392,0.588235,0.600000,0.607843,
       0.615686,0.627451,0.121569,0.125490,0.129412,0.133333,0.137255,0.141176,0.145098,0.152941,0.156863,
       0.227451,0.235294,0.247059,0.254902,0.262745,0.274510,0.282353,0.290196,0.301961,0.121569,0.129412,
       0.141176,0.149020,0.156863,0.168627,0.176471,0.184314,0.196078,0.133333,0.145098,0.152941,0.164706,
       0.172549,0.184314,0.192157,0.203922,0.215686,0.329412,0.345098,0.364706,0.380392,0.400000,0.415686,
       0.431373,0.450980,0.470588,0.329412,0.333333,0.337255,0.341176,0.345098,0.352941,0.356863,0.360784,
       0.364706,0.368627,0.372549,0.376471,0.384314,0.388235,0.392157,0.396078,0.400000,0.403922,0.411765,
       0.415686,0.419608,0.423529,0.427451,0.435294,0.439216,0.443137,0.447059,0.450980,0.458824,0.462745,
       0.466667,0.470588,0.478431,0.482353,0.486274,0.494118,0.498039,0.501961,0.505882,0.513725,0.517647,
       0.521569,0.529412,0.533333,0.537255,0.545098,0.549020,0.552941,0.560784,0.564706,0.568627,0.576471,
       0.580392,0.584314,0.592157,0.596078,0.603922,0.607843,0.611765,0.619608,0.623529,0.631373,0.635294,
       0.639216,0.647059,0.650980,0.658824,0.662745,0.670588,0.674510,0.682353,0.686275,0.694118,0.698039,
       0.705882,0.709804,0.713726,0.721569,0.725490,0.733333,0.741177,0.745098,0.752941,0.756863,0.764706,
       0.768627,0.776471,0.780392,0.788235,0.792157,0.800000,0.807843,0.811765,0.819608,0.823529,0.831373,
       0.835294,0.843137,0.850980,0.854902,0.862745,0.870588,0.874510,0.882353,0.886274,0.894118,0.901961,
       0.905882,0.913725,0.921569,0.925490,0.933333,0.941176,0.945098,0.952941,0.960784,0.968627,0.972549,
       0.980392,0.988235,1.000000]
    b=[0.000000,0.568627,0.627451,0.682353,0.741177,0.800000,0.858824,0.913725,1.000000,0.760784,0.709804,
       0.749020,0.788235,0.827451,0.866667,0.905882,0.941176,1.000000,0.741177,0.772549,0.803922,0.835294,
       0.866667,0.898039,0.929412,0.960784,1.000000,0.619608,0.666667,0.713726,0.756863,0.803922,0.850980,
       0.898039,0.945098,1.000000,0.200000,0.211765,0.227451,0.239216,0.254902,0.266667,0.282353,0.294118,
       0.309804,0.184314,0.188235,0.196078,0.200000,0.203922,0.211765,0.215686,0.219608,0.227451,0.082353,
       0.090196,0.101961,0.109804,0.117647,0.129412,0.137255,0.145098,0.156863,0.137255,0.141176,0.149020,
       0.152941,0.156863,0.164706,0.168627,0.172549,0.180392,0.137255,0.141176,0.145098,0.149020,0.149020,
       0.152941,0.156863,0.160784,0.164706,0.062745,0.062745,0.062745,0.066667,0.066667,0.066667,0.066667,
       0.070588,0.070588,0.043137,0.043137,0.047059,0.047059,0.047059,0.050980,0.050980,0.054902,0.054902,
       0.082353,0.086275,0.090196,0.094118,0.094118,0.098039,0.101961,0.105882,0.109804,0.000000,0.000000,
       0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.458824,0.494118,0.529412,0.564706,
       0.600000,0.635294,0.670588,0.705882,0.749020,0.701961,0.737255,0.776471,0.811765,0.847059,0.882353,
       0.921569,0.956863,1.000000,0.721569,0.725490,0.725490,0.729412,0.729412,0.733333,0.733333,0.733333,
       0.733333,0.733333,0.729412,0.729412,0.729412,0.729412,0.729412,0.725490,0.725490,0.725490,0.725490,
       0.725490,0.725490,0.725490,0.725490,0.721569,0.721569,0.721569,0.721569,0.721569,0.721569,0.721569,
       0.721569,0.721569,0.721569,0.721569,0.721569,0.721569,0.725490,0.725490,0.725490,0.725490,0.725490,
       0.725490,0.725490,0.729412,0.729412,0.729412,0.729412,0.729412,0.733333,0.733333,0.733333,0.737255,
       0.737255,0.737255,0.741177,0.741177,0.741177,0.745098,0.745098,0.749020,0.749020,0.749020,0.752941,
       0.752941,0.756863,0.756863,0.760784,0.764706,0.764706,0.768627,0.768627,0.772549,0.776471,0.776471,
       0.780392,0.784314,0.784314,0.788235,0.792157,0.796078,0.800000,0.800000,0.803922,0.807843,0.811765,
       0.815686,0.819608,0.823529,0.827451,0.831373,0.835294,0.839216,0.843137,0.847059,0.850980,0.854902,
       0.858824,0.862745,0.870588,0.874510,0.878431,0.882353,0.890196,0.894118,0.898039,0.905882,0.909804,
       0.913725,0.921569,0.925490,0.933333,0.937255,0.945098,0.949020,0.956863,0.960784,0.968627,0.972549,
       0.980392,0.988235,1.000000]
    rgb=zip(r,g,b)
    cmap=colors.ListedColormap(rgb,len(r))
    cmap.set_over(color='white')
    cmap.set_under(color='white')
    return cmap



def mrms_radarmap(cint=None):
  if cint != None:  #Map colors to provided contour intervals (cint)
    r=[1.00,0.00,0.00,0.00,0.00,0.00,0.00,1.00,0.91,1.00,1.00,0.80,0.60,1.00,0.60]
    g=[1.00,0.93,0.63,0.00,1.00,0.78,0.56,1.00,0.75,0.56,0.00,0.20,0.00,0.00,0.20]
    b=[1.00,0.93,0.96,0.96,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,1.00,0.80]
    rgb=zip(r,g,b)
    cmap=colors.ListedColormap(rgb,len(r))
    cmap.set_over(color='white')
    cmap.set_under(color='white')
    norm = colors.BoundaryNorm(cint, cmap.N)
    return cmap,norm
  else:
    r=[0.00,0.00,0.00,0.00,0.00,0.00,1.00,0.91,1.00,1.00,0.80,0.60,1.00,0.60]
    g=[0.93,0.63,0.00,1.00,0.78,0.56,1.00,0.75,0.56,0.00,0.20,0.00,0.00,0.20]
    b=[0.93,0.96,0.96,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,1.00,0.80]
    rgb=zip(r,g,b)
    cmap=colors.ListedColormap(rgb,len(r))
    cmap.set_over(color='white')
    cmap.set_under(color='white')
    return cmap

def truncated_ncl_radarmap():
      # Radar color map from NCL
      r = np.array([255,255,8,255,255,255,221,188,121,121,295])
      g = np.array([255,255,175,214,152,0,0,0,0,51,163])
      b = np.array([255,255,20,0,0,0,27,54,109,160,212])
      xsize=np.arange(np.size(r))
      r = r/255.
      g = g/255.
      b = b/255.
      red = []
      blue = []
      green = []
      for i in range(len(xsize)):
          xNorm=np.float(i)/(np.float(np.size(r))-1.0)
          red.append([xNorm,r[i],r[i]])
          green.append([xNorm,g[i],g[i]])
          blue.append([xNorm,b[i],b[i]])
      colorDict = {"red":red, "green":green, "blue":blue}
      ncl_reflect_coltbl = colors.LinearSegmentedColormap('NCL_REFLECT_COLTBL',colorDict)
      ncl_reflect_coltbl.set_over(color='white')
      return ncl_reflect_coltbl

def ncl_radarmap():
      # Radar color map from NCL
      r = np.array([255,255,0,0,0,9,0,8,255,255,255,221,188,121,121,295])
      g = np.array([255,255,255,157,0,130,255,175,214,152,0,0,0,0,51,163])
      b = np.array([255,255,255,255,255,175,0,20,0,0,0,27,54,109,160,212])
      xsize=np.arange(np.size(r))
      r = r/255.
      g = g/255.
      b = b/255.
      red = []
      blue = []
      green = []
      for i in range(len(xsize)):
          xNorm=np.float(i)/(np.float(np.size(r))-1.0)
	  red.append([xNorm,r[i],r[i]])
          green.append([xNorm,g[i],g[i]])
          blue.append([xNorm,b[i],b[i]])
      colorDict = {"red":red, "green":green, "blue":blue}
      ncl_reflect_coltbl = colors.LinearSegmentedColormap('NCL_REFLECT_COLTBL',colorDict)
      ncl_reflect_coltbl.set_over(color='white')
      return ncl_reflect_coltbl

def tcamt():
      r = np.array([255,230,200,180,150,120,80,55,30,15,225,180,150,120,80,60,40,30,20,220,192,160,128,112,72,60,45,40,250,240,225,200,180,160,140,120,100])
      g = np.array([255,255,255,250,245,245,240,210,180,160,255,240,210,185,165,150,130,110,100,220,180,140,112,96,60,40,30,0,240,220,190,160,140,120,100,80,60])
      b = np.array([255,225,190,170,140,115,80,60,30,15,255,250,250,250,245,245,240,235,210,255,255,255,235,220,200,180,165,160,230,210,180,150,130,110,90,70,50])
      xsize=np.arange(np.size(r))
      r = r/255.
      g = g/255.
      b = b/255.
      red = []
      blue = []
      green = []
      for i in range(len(xsize)):
          xNorm=np.float(i)/(np.float(np.size(r))-1.0)
          red.append([xNorm,r[i],r[i]])
          green.append([xNorm,g[i],g[i]])
          blue.append([xNorm,b[i],b[i]])
      colorDict = {"red":red, "green":green, "blue":blue}
      tcamt = colors.LinearSegmentedColormap('TCAMT',colorDict)
      return tcamt

def cmap_q2m():
    r=np.array([255,179,96,128,0, 0,  51, 0,  0,  0,  133,51, 70, 0,  128,128,180])
    g=np.array([255,179,96,128,92,128,153,155,155,255,162,102,70, 0,  0,  0,  0])
    b=np.array([255,179,96,0,  0, 0,  102,155,255,255,255,255,255,128,255,128,128])
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
    cmap_q2m_coltbl = colors.LinearSegmentedColormap('CMAP_Q2M_COLTBL',colorDict)
    cmap_q2m_coltbl.set_over(color='deeppink')
    return cmap_q2m_coltbl



def ncl_t2m():
    # Grabbed this colormap from NCL
    # Converted from MeteoSwiss NCL library

    r=np.array([109,175,255,255,255,128,0,70,51,133,255,204,179,153,96,128,0,0,51,157,212,255,255,255,255,255,188,171,128,163])
    g=np.array([227,240,196,153,0,0,0,70,102,162,255,204,179,153,96,128,92,128,153,213,255,255,184,153,102,0,75,0,0,112])
    b=np.array([255,255,226,204,255,128,128,255,255,255,255,204,179,153,96,0,0,0,102,0,91,0,112,0,0,0,0,56,0,255])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    blue = []
    green = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    ncl_t2m_coltbl = colors.LinearSegmentedColormap('NCL_T2M_COLTBL',colorDict)
    ncl_t2m_coltbl.set_over(color='white')
    return ncl_t2m_coltbl

def ncl_snow():
    r=np.array([255,237,205,153,83,50,50,5,5,10,44,106])
    g=np.array([255,250,255,240,189,166,150,112,80,31,2,44])
    b=np.array([255,194,205,178,159,150,180,176,140,150,70,90])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    blue = []
    green = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    ncl_snow_coltbl = colors.LinearSegmentedColormap('NCL_SNOW_COLTBL',colorDict)
    return ncl_snow_coltbl

def ncl_perc_11Lev():
    r=np.array([202,89,139,96,26,145,217,254,252,215,150])
    g=np.array([202,141,239,207,152,207,239,224,141,48,0])
    b=np.array([200,252,217,145,80,96,139,139,89,39,100])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    blue = []
    green = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    my_coltbl = colors.LinearSegmentedColormap('NCL_PERC_11LEV_COLTBL',colorDict)
    return my_coltbl

def ncl_grnd_hflux():
    r=np.array([0,8,16,24,32,40,48,85,133,181,230,253,253,253,253,253,253,253,253,253,253,253])
    g=np.array([253,222,189,157,125,93,60,85,133,181,230,230,181,133,85,60,93,125,157,189,224,253])
    b=np.array([253,253,253,253,253,253,253,253,253,253,253,230,181,133,85,48,40,32,24,16,8,0])
    xsize=np.arange(np.size(r))
    r = r/255.
    g = g/255.
    b = b/255.
    red = []
    blue = []
    green = []
    for i in range(len(xsize)):
        xNorm=np.float(i)/(np.float(np.size(r))-1.0)
        red.append([xNorm,r[i],r[i]])
        green.append([xNorm,g[i],g[i]])
        blue.append([xNorm,b[i],b[i]])
    colorDict = {"red":red, "green":green, "blue":blue}
    my_coltbl = colors.LinearSegmentedColormap('NCL_GRND_HFLUX_COLTBL',colorDict)
    return my_coltbl


def gempak_colortbl():

   '''
   Color table from gempak

   WHITE	   WHI    255	 255	255  white
   BLACK	   BLA      0	   0	  0  black
   RED  	   RED    255	   0	  0  red
   GREEN	   GRE      0	 255	  0  green
   BLUE 	   BLU      0	   0	255  blue
   YELLOW	   YEL    255	 255	  0  yellow
   CYAN 	   CYA      0	 255	255  cyan
   MAGENTA	   MAG    255	   0	255  magenta
   BROWN	   BRO    139	  71	 38  sienna3
   CORAL	   COR    255	 130	 71  sienna1
   APRICOT	   APR    255	 165	 79  tan1
   PINK 	   PIN    255	 174	185  LightPink1
   DKPINK	   DKP    255	 106	106  IndianRed1
   MDVIOLET	   MDV    238	  44	 44  firebrick2
   MAROON	   MAR    139	   0	  0  red4
   FIREBRIC	   FIR    205	   0	  0  red3
   ORRED	   ORR    238	  64	  0  OrangeRed2
   ORANGE	   ORA    255	 127	  0  DarkOrange1
   DKORANGE	   DKO    205	 133	  0  orange3
   GOLD 	   GOL    255	 215	  0  gold1
   DKYELLOW	   DKY    238	 238	  0  yellow2
   LWNGREEN	   LWN    127	 255	  0  chartreuse1
   MDGREEN	   MDG      0	 205	  0  green3
   DKGREEN	   DKG      0	 139	  0  green4
   GRPBLUE	   GRP     16	  78	139  DodgerBlue4
   LTBLUE	   LTB     30	 144	255  DodgerBlue1
   SKY  	   SKY      0	 178	238  DeepSkyBlue2
   MDCYAN	   MDC      0	 238	238  cyan2
   VIOLET	   VIO    137	 104	205  MediumPurple3
   PURPLE	   PUR    145	  44	238  purple2
   PLUM 	   PLU    139	   0	139  magenta4
   VANILLA	   VAN    255	 228	220  bisque1
   WHITE	   WHI    255	 255	255  white
   '''

   r=np.array([255,0,255,0,0,255,0,255,139,255,255,255,255,238,139,205,238,255,205,255,238,127,0,0,16,30,0,0,137,145,139,255,255])
   g=np.array([255,0,0,255,0,255,255,0,71,130,165,174,106,44,0,0,64,127,133,215,238,255,205,139,78,144,178,238,104,44,0,228,255])
   b=np.array([255,0,0,0,255,0,255,255,38,71,79,185,106,44,0,0,0,0,0,0,0,0,0,0,139,255,238,238,205,238,139,220,255])
   xsize=np.arange(np.size(r))
   r = r/255.
   g = g/255.
   b = b/255.
   red = []
   blue = []
   green = []
   for i in range(len(xsize)):
       xNorm=np.float(i)/(np.float(np.size(r))-1.0)
       red.append([xNorm,r[i],r[i]])
       green.append([xNorm,g[i],g[i]])
       blue.append([xNorm,b[i],b[i]])
   colorDict = {"red":red, "green":green, "blue":blue}
   gempak_coltbl = colors.LinearSegmentedColormap('GEMPAK_COLTBL',colorDict)
   return gempak_coltbl

def gem_color_list():

   '''
   Color table from gempak

   WHITE	   WHI    255	 255	255  white
   BLACK	   BLA      0	   0	  0  black
   RED  	   RED    255	   0	  0  red
   GREEN	   GRE      0	 255	  0  green
   BLUE 	   BLU      0	   0	255  blue
   YELLOW	   YEL    255	 255	  0  yellow
   CYAN 	   CYA      0	 255	255  cyan
   MAGENTA	   MAG    255	   0	255  magenta
   BROWN	   BRO    139	  71	 38  sienna3
   CORAL	   COR    255	 130	 71  sienna1
   APRICOT	   APR    255	 165	 79  tan1
   PINK 	   PIN    255	 174	185  LightPink1
   DKPINK	   DKP    255	 106	106  IndianRed1
   MDVIOLET	   MDV    238	  44	 44  firebrick2
   MAROON	   MAR    139	   0	  0  red4
   FIREBRIC	   FIR    205	   0	  0  red3
   ORRED	   ORR    238	  64	  0  OrangeRed2
   ORANGE	   ORA    255	 127	  0  DarkOrange1
   DKORANGE	   DKO    205	 133	  0  orange3
   GOLD 	   GOL    255	 215	  0  gold1
   DKYELLOW	   DKY    238	 238	  0  yellow2
   LWNGREEN	   LWN    127	 255	  0  chartreuse1
   MDGREEN	   MDG      0	 205	  0  green3
   DKGREEN	   DKG      0	 139	  0  green4
   GRPBLUE	   GRP     16	  78	139  DodgerBlue4
   LTBLUE	   LTB     30	 144	255  DodgerBlue1
   SKY  	   SKY      0	 178	238  DeepSkyBlue2
   MDCYAN	   MDC      0	 238	238  cyan2
   VIOLET	   VIO    137	 104	205  MediumPurple3
   PURPLE	   PUR    145	  44	238  purple2
   PLUM 	   PLU    139	   0	139  magenta4
   VANILLA	   VAN    255	 228	220  bisque1
   WHITE	   WHI    255	 255	255  white
   '''

   r=np.array([255,0,255,0,0,255,0,255,139,255,255,255,255,238,139,205,238,255,205,255,238,127,0,0,16,30,0,0,137,145,139,255,255])
   g=np.array([255,0,0,255,0,255,255,0,71,130,165,174,106,44,0,0,64,127,133,215,238,255,205,139,78,144,178,238,104,44,0,228,255])
   b=np.array([255,0,0,0,255,0,255,255,38,71,79,185,106,44,0,0,0,0,0,0,0,0,0,0,139,255,238,238,205,238,139,220,255])


   xsize=np.arange(np.size(r))
   r = r/255.
   g = g/255.
   b = b/255.
   rgb = []
   for i in range(len(xsize)):
       rgb.append([r[i],g[i],b[i]])
   return rgb


def reflect():
      # Another Radar colormap which was found online
         # source: http://www.atmos.washington.edu/~lmadaus/pyscript/coltbls.txt
	reflect_cdict ={'red':	((0.000, 0.40, 0.40),
				(0.067, 0.20, 0.20),
				(0.133, 0.00, 0.00),
				(0.200, 0.00, 0.00),
				(0.267, 0.00, 0.00),
				(0.333, 0.00, 0.00),
				(0.400, 1.00, 1.00),
				(0.467, 1.00, 1.00),
				(0.533, 1.00, 1.00),
				(0.600, 1.00, 1.00),
				(0.667, 0.80, 0.80),
				(0.733, 0.60, 0.60),
				(0.800, 1.00, 1.00),
				(0.867, 0.60, 0.60),
				(0.933, 1.00, 1.00),
				(1.000, 0.00, 0.00)),
		'green':	((0.000, 1.00, 1.00),
				(0.067, 0.60, 0.60),
				(0.133, 0.00, 0.00),
				(0.200, 1.00, 1.00),
				(0.267, 0.80, 0.80),
				(0.333, 0.60, 0.60),
				(0.400, 1.00, 1.00),
				(0.467, 0.80, 0.80),
				(0.533, 0.40, 0.40),
				(0.600, 0.00, 0.00),
				(0.667, 0.20, 0.20),
				(0.733, 0.00, 0.00),
				(0.800, 0.00, 0.00),
				(0.867, 0.20, 0.20),
				(0.933, 1.00, 1.00),
				(1.000, 1.00, 1.00)),
		'blue':		((0.000, 1.00, 1.00),
				(0.067, 1.00, 1.00),
				(0.133, 1.00, 1.00),
				(0.200, 0.00, 0.00),
				(0.267, 0.00, 0.00),
				(0.333, 0.00, 0.00),
				(0.400, 0.00, 0.00),
				(0.467, 0.00, 0.00),
				(0.533, 0.00, 0.00),
				(0.600, 0.00, 0.00),
				(0.667, 0.00, 0.00),
				(0.733, 0.00, 0.00),
				(0.800, 1.00, 1.00),
				(0.867, 0.80, 0.80),
				(0.933, 1.00, 1.00),
				(1.000, 1.00, 1.00))}
	reflect_coltbl = colors.LinearSegmentedColormap('REFLECT_COLTBL',reflect_cdict)
        return reflect_coltbl

def create_ncep_radar_ref_color_table():

#set radar reflectivity color table to match that used at ncep operation
   cmap = colors.ListedColormap(['lightgrey','skyblue','dodgerblue','mediumblue',\
               'lime','limegreen','green','yellow','gold','darkorange','red','firebrick',\
               'darkred','fuchsia','darkorchid','black'])
   bounds=[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80]
   norm = colors.BoundaryNorm(bounds, cmap.N)
   return cmap,bounds,norm


'''

MISC.

'''

def tagrstprod(fname):
  ##### Make fname file restricted #####
  try:
    call(["chmod","640",fname])
    call(["chgrp","rstprod",fname])
  except:
    sys.exit("ERROR from ncepy.tagrstprod - unable to change permissons/group access for for: "+fname)

def clear_plotables(ax,keep_ax_lst,fig):
  ######### - step to clear off old plottables but leave the map info - #####
  if len(keep_ax_lst) == 0 :
    print "ncepy.clear_plotables WARNING keep_ax_lst has length 0. Clearing ALL plottables including map info!"
  cur_ax_children = ax.get_children()[:]
  if len(cur_ax_children) > 0:
    for a in cur_ax_children:
      if a not in keep_ax_lst:
        # if the artist isn't part of the initial set up, remove it
        a.remove()
  # check for presence of old colorbar(s) and delete them. They are stored as a separate axis
  x=0
  for a in fig.axes:
    x=x+1
    if x > 1:
      try:
        fig.delaxes(a)
      except Exception,e:
        print "--- ncepy.clear_plotables: Exception: %s ---- Unable to delete figure axis at %d with axis size of %d " % (e,x,len(fig.axes))

  ############################################################################


def gc_dist(lat1,lon1,lat2,lon2):
# Return the great circle distance (m) between a two pairs of lat/lon points
  EARTH_CIRCUMFERENCE = 6378137 # earth circumference in meters
  dLat = np.radians(lat2 - lat1)
  dLon = np.radians(lon2 - lon1)
  a = (np.sin(dLat / 2) * np.sin(dLat / 2) +
  np.cos(np.radians(lat1)) * np.cos(np.radians(lat2)) *
  np.sin(dLon / 2) * np.sin(dLon / 2))
  c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))
  d = EARTH_CIRCUMFERENCE * c
  return d


def find_nearest_ij(lats,lat0,lons,lon0):
# Returns the nearest gridpoint based upon an input/specified lat/lon point (lat0 and lon0)
  gc=gc_dist(lats,lons,lat0,lon0)
  idx_j,idx_i=np.unravel_index(gc.argmin(), gc.shape)
  return idx_j,idx_i

def cal_td_from_ptRH(p,t,rh):
    # Find Td in Kelvin using Bolton's approx.
    #
    # Bolton, David, 1980: The Computation of
    #   Equivalent Potential Temperature.
    #   Mon. Wea. Rev., 108, 1046-1053.

    # q ----> specific humiditity in kg/kg
    # p ----> pressure in Pa
    # dew --> returns dew, the dewpoint temperature

    # --- convert specific humidity to mixing ratio

    T_zero = 273.15

    Rd = 287.04  # gas constant dry air
    Rv = 461.51  # gas constant vapor
    eps = Rd/Rv

    emin=np.zeros(rh.shape)
    emin=0.001                 # e---> vapor pressure in hPa

    # ---- calc. the vapor pressure ----> needs to be in hPa
    e = ((es(t)*rh)/100.)/100.

    # ---- make sure the vapor pressure values aren't too small
    e = np.maximum(e, emin)

    # get the dewpoint in Kelvin ----- np.log is the ln
    dew = 273.15 + (243.5 / ((17.67 / np.log(e/6.112)) - 1.0) )

    return dew




def cal_td(p, q):
    # Find Td in Kelvin using Bolton's approx.
    #
    # Bolton, David, 1980: The Computation of
    #   Equivalent Potential Temperature.
    #   Mon. Wea. Rev., 108, 1046-1053.

    # q ----> specific humiditity in kg/kg
    # p ----> pressure in Pa
    # dew --> returns dew, the dewpoint temperature

    # --- convert specific humidity to mixing ratio

    T_zero = 273.15

    Rd = 287.04  # gas constant dry air
    Rv = 461.51  # gas constant vapor
    eps = Rd/Rv

    w=q/(1.0-q)

    emin=np.zeros(q.shape)
    emin=0.001                 # e---> vapor pressure in hPa

    # ---- calc. the vapor pressure ----> must convert pressure to hPa in here
    e = w * (p/100.) / (eps + w)

    # ---- make sure the vapor pressure values aren't too small
    e = np.maximum(e, emin)

    # get the dewpoint in Kelvin ----- np.log is the ln
    dew = 273.15 + (243.5 / ((17.67 / np.log(e/6.112)) - 1.0) )

    return dew

def Kelvin2F(T):
    """Returns Temperature in degrees Fahrenheit given initial temperature in Kelvin."""
    return (T-273.15)*(9.0/5.0)+32.0

def C2F(T):
    """Returns Temperature in degrees Fahrenheit given initial temperature in Celsius."""
    return (T)*(9.0/5.0)+32.0

def es(T):
    """Returns saturation vapor pressure (Pascal) at temperature T (Celsius)
    Formula 2.17 in Rogers&Yau"""
    return 611.2*np.exp(17.67*T/(T+243.5))



def SMOOTH(FIELD,SMTH=0.5):

  '''
   Smoothing Function pulled from NCEP UPP code SMOOTH.f

   SUBPROGRAM:    SMOOTH      SMOOTH A METEOROLOGICAL FIELD
      PRGMMR: STAN BENJAMIN    ORG: FSL/PROFS  DATE: 90-06-15

    ABSTRACT: SHAPIRO SMOOTHER.

    PROGRAM HISTORY LOG:
      85-12-09  S. BENJAMIN   ORIGINAL VERSION
    2013        J. CARLEY     Python port

    USAGE:    CALL SMOOTH (FIELD,HOLD,IX,IY,SMTH)
      INPUT ARGUMENT LIST:
        FIELD    - REAL ARRAY  FIELD(IX,IY)
                               METEOROLOGICAL FIELD

      OPTIONAL INPUT:
        SMTH     - REAL (defaults to 0.5)

     OUTPUT ARGUMENT LIST:
       FIELD    - REAL ARRAY  FIELD(IX,IY)
                              SMOOTHED METEOROLOGICAL FIELD

   REMARKS: REFERENCE: SHAPIRO, 1970: "SMOOTHING, FILTERING, AND
     BOUNDARY EFFECTS", REV. GEOPHYS. SP. PHYS., 359-387.
     THIS FILTER IS OF THE TYPE
           Z(I) = (1-S)Z(I) + S(Z(I+1)+Z(I-1))/2
     FOR A FILTER WHICH IS SUPPOSED TO DAMP 2DX WAVES COMPLETELY
     BUT LEAVE 4DX AND LONGER WITH LITTLE DAMPING,
     IT SHOULD BE RUN WITH 2 PASSES USING SMTH (OR S) OF 0.5
     AND -0.5.

   Directions on calling the routine.  Call from inside a loop where the
     iterator is the number of smoothing passes.  Smoothing passes can
     set as a function of grid-spacing (how it's handled for the RAP/HRRR)
       Example:

      Number of smoothing passes for each field
      SMOOTH=int(5.*(13500./dxm)) # for u, v, and heights
      for N in np.arange(NSMOOTH):
        SMOOTH(FIELD,SMTH)

  '''

  IX,IY=np.shape(FIELD)

  HOLD=np.zeros([IX,2])
  SMTH1 = 0.25 * SMTH * SMTH
  SMTH2 = 0.5  * SMTH * (1.-SMTH)
  SMTH3 = (1.-SMTH) * (1.-SMTH)
  SMTH4 = (1.-SMTH)
  SMTH5 = 0.5 * SMTH
  I1 = 1
  I2 = 0

  for J in np.arange(1,IY-1):
    IT = I1
    I1 = I2
    I2 = IT
    SUM1 = FIELD[0:IX-2,J+1] + FIELD[0:IX-2,J-1] + FIELD[2:IX,J+1] + FIELD[2:IX,J-1]
    SUM2 = FIELD[1:IX-1,J+1] + FIELD[2:IX,J  ] + FIELD[1:IX-1,J-1] + FIELD[0:IX-2,J  ]
    HOLD[1:IX-1,I1] = SMTH1*SUM1 + SMTH2*SUM2 + SMTH3*FIELD[1:IX-1,J]
    if J!=1: FIELD[1:IX-1,J-1] = HOLD[1:IX-1,I2]
    # END OF J LOOP

  FIELD[1:IX-1,IY-2] = HOLD[1:IX-1,I1]

  for I in np.arange(1,IX-1):
    FIELD[I     ,   0] = SMTH4* FIELD[I     ,   0] + SMTH5 * (FIELD[I-1   ,   0] + FIELD[I+1, 0])
    FIELD[I     ,IY-1] = SMTH4* FIELD[I     ,IY-1] + SMTH5 * (FIELD[I-1   ,IY-1] + FIELD[I+1,IY-1])

  for J in np.arange(1,IY-1):
    FIELD[0,J] = SMTH4* FIELD[0,J] + SMTH5 * (FIELD[0,J-1] + FIELD[0,J+1])


def pint2pmid_3d(pint):
 #convert pressure at interface layers to mid-layers
 #
 # routine assumes input is a 3d array

 IM,JM,LP1=np.shape(pint)
 pmid=np.zeros((IM,JM,LP1-1))

 for l in np.arange(1,LP1): #Start at 1 so l-1 is not < 0
   pmid[:,:,l-1]=(pint[:,:,l-1]+pint[:,:,l])*0.5 # representative of what model does
 return pmid

def cal_zmid(fis,tmpk,spfh,pint):
  """Calculate height given T, Q, PINT, and terrain height.
     Return height at midlayers."""
  rd=287.04  # gas constant dry air
  g=9.81
  d608=0.608
  IM,JM,LP1=np.shape(pint)
  zint=np.zeros((IM,JM,LP1))
  zmid=np.zeros((IM,JM,LP1-1))
  LM=LP1-1
  fi=fis
  zint[:,:,0]=fis/g
  pmid=pint2pmid_3d(pint)
  lpint=np.log(pint)
  #start from the surface and work upward
  # we assume the surface is at level 0
  for L in np.arange(1,LM+1):
      zint[:,:,L]=(tmpk[:,:,L-1]*(spfh[:,:,L-1]*d608+1.0)*rd*(lpint[:,:,L-1]-lpint[:,:,L])+fi)/g
      fi=zint[:,:,L]*g
  #Now start at the top and work downward
  for L in np.arange(LP1-1,0,-1):
      fact=(np.log(pmid[:,:,L-1])-lpint[:,:,L-1])/(lpint[:,:,L]-lpint[:,:,L-1])
      zmid[:,:,L-1]=zint[:,:,L-1]+(zint[:,:,L]-zint[:,:,L-1])*fact
  return zmid


def extrema(mat,mode='wrap',window=10):
  # From: http://matplotlib.org/basemap/users/examples.html

  """find the indices of local extrema (min and max)
  in the input array."""
  mn = minimum_filter(mat, size=window, mode=mode)
  mx = maximum_filter(mat, size=window, mode=mode)
  # (mat == mx) true if pixel is equal to the local max
  # (mat == mn) true if pixel is equal to the local in
  # Return the indices of the maxima, minima
  return np.nonzero(mat == mn), np.nonzero(mat == mx)


def plt_highs_and_lows(m,mat,lons,lats,mode='wrap',window='10'):
  # From: http://matplotlib.org/basemap/users/examples.html
  # m is the map handle
  x, y = m(lons, lats)
  local_min, local_max = extrema(mat,mode,window)
  xlows = x[local_min]; xhighs = x[local_max]
  ylows = y[local_min]; yhighs = y[local_max]
  lowvals = mat[local_min]; highvals = mat[local_max]
  # plot lows as red L's, with min pressure value underneath.
  xyplotted = []
  # don't plot if there is already a L or H within dmin meters.
  yoffset = 0.022*(m.ymax-m.ymin)
  dmin = yoffset
  for x,y,p in zip(xlows, ylows, lowvals):
    if x < m.xmax and x > m.xmin and y < m.ymax and y > m.ymin:
        dist = [np.sqrt((x-x0)**2+(y-y0)**2) for x0,y0 in xyplotted]
        if not dist or min(dist) > dmin:
            plt.text(x,y,'L',fontsize=14,fontweight='bold',
                    ha='center',va='center',color='r',zorder=10,clip_on=True)
            plt.text(x,y-yoffset,repr(int(p)),fontsize=9,zorder=10,
                    ha='center',va='top',color='r',
                    bbox = dict(boxstyle="square",ec='None',fc=(1,1,1,0.5)),clip_on=True)
            xyplotted.append((x,y))
  # plot highs as blue H's, with max pressure value underneath.
  xyplotted = []
  for x,y,p in zip(xhighs, yhighs, highvals):
    if x < m.xmax and x > m.xmin and y < m.ymax and y > m.ymin:
        dist = [np.sqrt((x-x0)**2+(y-y0)**2) for x0,y0 in xyplotted]
        if not dist or min(dist) > dmin:
            plt.text(x,y,'H',fontsize=14,fontweight='bold',
                    ha='center',va='center',color='b',zorder=10,clip_on=True)
            plt.text(x,y-yoffset,repr(int(p)),fontsize=9,
                    ha='center',va='top',color='b',zorder=10,
                    bbox = dict(boxstyle="square",ec='None',fc=(1,1,1,0.5)),clip_on=True)
            xyplotted.append((x,y))

def corners_res(dom,proj='lcc'):
#Sets domain corners and
# plotting resolutions
# for commonly used domains

# resolution       resolution of boundary database to use. Can be ``c``
#                  (crude), ``l`` (low), ``i`` (intermediate), ``h``
#                  (high), ``f`` (full) or None.
#                  If None, no boundary data will be read in (and
#                  class methods such as drawcoastlines will raise an
#                  if invoked).
#                  Resolution drops off by roughly 80% between datasets.
#                  Higher res datasets are much slower to draw.
#                  Default ``c``.


  if proj.lower() in ['lcc','laea','merc']:
    llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=_default_corners_res(dom)
  elif proj.lower() in ['stere','spstere', 'npstere']:
    llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=_stere_corners_res(dom)
  else:
    llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=_default_corners_res(dom)

  return llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res

def _stere_corners_res(dom):
    if dom=='AK':
      llcrnrlon=-165.0
      llcrnrlat=50.0
      urcrnrlon=-121.0
      urcrnrlat=70.0
      res='l'
    elif dom=='NAK':
      llcrnrlon=-168.75
      llcrnrlat=62.5
      urcrnrlon=-137.0
      urcrnrlat=75.0
      res='i'
    elif dom=='SAK':
      llcrnrlon=-163.0
      llcrnrlat=53.5
      urcrnrlon=-137.0
      urcrnrlat=66.5
      res='i'
    elif dom=='SWAK':
      llcrnrlon=-172.5
      llcrnrlat=45.0
      urcrnrlon=-147.5
      urcrnrlat=62.5
      res='i'
    elif dom=='SEAK':
      llcrnrlon=-146.0
      llcrnrlat=52.0
      urcrnrlon=-125.5
      urcrnrlat=59.5
      res='i'
    else:
      #Default to Alaska if we cannot find a match
      llcrnrlon=-175.0
      llcrnrlat=42.5
      urcrnrlon=-115.0
      urcrnrlat=75.0
      res='l'
    return llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res

def _default_corners_res(dom):
    if dom=='CONUS':
      llcrnrlon=-121.5
      llcrnrlat=22.0
      urcrnrlon=-64.5
      urcrnrlat=50.0
      res='l'
    elif dom=='NW':
      llcrnrlon=-125.0
      llcrnrlat=35.0
      urcrnrlon=-105.0
      urcrnrlat=52.0
      res='l'
    elif dom=='NWRFC':
      llcrnrlon=-125.927124
      llcrnrlat=38.0
      urcrnrlon=-108.5
      urcrnrlat=54.756331
      res='i'
    elif dom=='NC':
      llcrnrlon=-110.0
      llcrnrlat=36.0
      urcrnrlon=-83.0
      urcrnrlat=50.0
      res='l'
    elif dom=='NE':
      llcrnrlon=-84.0
      llcrnrlat=41.0
      urcrnrlon=-64.0
      urcrnrlat=47.5
      res='i'
    elif dom=='SC':
      llcrnrlon=-109.0
      llcrnrlat=25.0
      urcrnrlon=-83.0
      urcrnrlat=38.0
      res='l'
    elif dom=='SE':
      llcrnrlon=-93.0
      llcrnrlat=24.5
      urcrnrlon=-70.0
      urcrnrlat=35.0
      res='l'
    elif dom=='SW':
      llcrnrlon=-123.0
      llcrnrlat=25.0
      urcrnrlon=-105.0
      urcrnrlat=40.0
      res='l'
    elif dom=='MIDATL':
      llcrnrlon=-85.5
      llcrnrlat=34.5
      urcrnrlon=-71.5
      urcrnrlat=40.25
      res='i'
    elif dom=='Great_Lakes':
      llcrnrlon=-96.0
      llcrnrlat=37.0
      urcrnrlon=-71.0
      urcrnrlat=47.5
      res='l'
    elif dom=='AK':
      llcrnrlon=-180.0
      llcrnrlat=45.0
      urcrnrlon=-115.0
      urcrnrlat=70.0
      res='l'
    elif dom=='NAK':
      llcrnrlon=-168.25
      llcrnrlat=64.0
      urcrnrlon=-137.0
      urcrnrlat=71.25
      res='i'
    elif dom=='SAK':
      llcrnrlon=-163.0
      llcrnrlat=53.5
      urcrnrlon=-137.0
      urcrnrlat=65.5
      res='i'
    elif dom=='SWAK':
      llcrnrlon=-165.0
      llcrnrlat=52.0
      urcrnrlon=-145.0
      urcrnrlat=63.0
      res='i'
    elif dom=='SEAK':
      llcrnrlon=-146.0
      llcrnrlat=52.0
      urcrnrlon=-125.5
      urcrnrlat=63.0
      res='i'
    elif dom=='PR':
      llcrnrlon=-68.1953123
      llcrnrlat=17.5
      urcrnrlon=-63.9724
      urcrnrlat=19.
      res='h'
    elif dom=='GUAM':
      llcrnrlon=-216.25
      llcrnrlat=12.80
      urcrnrlon=-211.72
      urcrnrlat=16.5
      res='h'
    elif dom=='HI':
      llcrnrlon=-160.525001
      llcrnrlat=18.5
      urcrnrlon=-153.869001
      urcrnrlat=22.5
      res='h'
    elif dom=='POWER':
      llcrnrlon=-75.0
      llcrnrlat=40.0
      urcrnrlon=-65.0
      urcrnrlat=45.0
      res='i'
    elif dom=='OK':
      llcrnrlon=-104.0
      llcrnrlat=32.0
      urcrnrlon=-92.0
      urcrnrlat=38.5
      res='h'
    elif dom=='MIDATL':
      llcrnrlon=-82.37732
      llcrnrlat=34.85889
      urcrnrlon=-72.676392
      urcrnrlat=40.405131
      res='h'
    elif dom=='LAKE_VICTORIA':
      llcrnrlon=30.0
      llcrnrlat=-3.0
      urcrnrlon=35.0
      urcrnrlat=1.0
      res='h'
    elif dom=='AFRICA':
      llcrnrlon=-25.0
      llcrnrlat=-25.0
      urcrnrlon=60.0
      urcrnrlat=30.0
      res='l'
    elif dom=='MEDFORD':
      llcrnrlon=-124.0
      llcrnrlat=41.5
      urcrnrlon=-122.0
      urcrnrlat=43.2
      res='h'
    else:
      #default to CONUS
      llcrnrlon=-121.5
      llcrnrlat=22.0
      urcrnrlon=-64.5
      urcrnrlat=50.0
      res='l'
    return llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res

