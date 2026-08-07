# Information of Sub-packages of HRRR-3DRTMA Package

## rtma3d_gsi.fd/:
```
                github: https://github.com/NOAA-EMC/GSI.git
                history:
			202607  	branch: develop			commit: #ec8215d
                notes:
                        .gitignore (modified to track log*.f90)
```

## rtma3d_post.fd/:
```
                github: https://github.com/NOAA-EMC/UPP.git
                history:
			202607  	branch: release/3drtma_v1	commit: #ff3a3ab
			20260807	branch: release/3drtma_v1	commit: #a303f7b (see UPP PR #1579 for details)
					Note: cloud ceiling height 260 is changed to height ASL as same as 408, not AGL
```

## other subpackages:
```
                including WRF-ARW model, obs-preprocessing code, etc.
                originally from EMC HRRR systems
                github:https://github.com/NOAA-EMC/HRRR.git
```
## Notes:
```
		.git, .github are removed from sub-packages and moved here as record, tar-ed into multiple volumes (bytes=45M) of tar ball.
```
