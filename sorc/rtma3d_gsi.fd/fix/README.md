# GSI-fix
ASCII files that are used in the [GSI](https://github.com/NOAA-EMC/GSI) by various applications to describe the characteristics of the system.

These include (but are not limited to):
- observation error specifications for conventional and satellite-based measurements
- observation rejection and acceptance lists
- background error covariance specifications
- hybrid covariance weights
- control and state vector descriptions

Binary files used by the [GSI](https://github.com/NOAA-EMC/GSI) are **NOT** in [GSI-fix](https://github.com/NOAA-EMC/GSI-fix).
Instead, GSI binary fix files are available from two sources:
- staged in directory `GSI_BINARY_SOURCE_DIR` on WCOSS2 and select NOAA RHDPCS machines.
  Variable `GSI_BINARY_SOURCE_DIR` is defined in [GSI](https://github.com/NOAA-EMC/GSI) `modulefiles/gsi_$machine.lua`
  for some, but not all, machines.
- downloadable as a tarball from https://ftp.emc.ncep.noaa.gov/static_files/public/gsi.


## What files are what
The top level directory structure groups the files as follow:

| File/directory            | Purpose |
| --------------            | ------- |
| `README.md`           | This file with basic pointers to more information. |
| `LICENSE.md`          | A copy of the GNU Lesser General Public License, Version 3. |
| `CMakeLists.txt`      | `CMakeLists` file for use with GSI. |
| `gsi_binary_files.cmake` | List of all binary files used in the GSI. |

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.

