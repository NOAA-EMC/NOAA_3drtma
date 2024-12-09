local envvar_ver = os.getenv("envvar_ver")
local PrgEnv_intel_ver = os.getenv("PrgEnv_intel_ver")
local intel_ver = os.getenv("intel_ver")
local libjpeg_ver = os.getenv("libjpeg_ver")
local craype_ver = os.getenv("craype_ver")
local prod_util_ver = os.getenv("prod_util_ver") 
local grib_util_ver = os.getenv("grib_util_ver")
local gempak_ver = os.getenv("gempak_ver")


load("envvar/"..envvar_ver)
load("PrgEnv-intel/"..PrgEnv_intel_ver)
load("intel/"..intel_ver)
load("libjpeg/"..libjpeg_ver)
load("craype/"..craype_ver)
load("prod_util/"..prod_util_ver) 
load("grib_util/"..grib_util_ver)
load("gempak/"..gempak_ver)

