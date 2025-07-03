
Build instructions

1. Clone the `3DRTMAv1` branch of the NOAA_3drtma repository into a packages subdirectory:
```
mkdir packages
cd packages
git clone -b 3DRTMAv1 https://github.com/NOAA-EMC/NOAA_3drtma rtma3d.v1.0.0
```

2. Copy the fix and parm files:
```
cd rtma3d.v1.0.0
cp -r /lfs/h2/emc/da/noscrub/matthew.t.morris/packages/rtma3d.v1.0.0/fix/* fix/
cp -r /lfs/h2/emc/da/noscrub/matthew.t.morris/packages/rtma3d.v1.0.0/parm/* parm/
```

3. Move to the sorc directory:
```
cd sorc
```

4. Build the RTMA3D workflow:
```
./build_rtma3d_all.sh
```

5. Install the executables
```
./install_rtma3d_all.sh
```

5. Change user.name in the workflow/*xml files
