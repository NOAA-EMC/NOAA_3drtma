# This file is configured by CMake automatically as DartConfiguration.tcl
# If you choose not to use CMake, this file may be hand configured, by
# filling in the required variables.


# Configuration directories and files
SourceDirectory: /home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd
BuildDirectory: /home/Edward.Colon/EMC_noaa-3drtma/sorc/build_gsi

# Where to place the cost data store
CostDataFile: 

# Site is something like machine.domain, i.e. pragmatic.crd
Site: fe5

# Build name is osname-revision-compiler, i.e. Linux-2.4.2-2smp-c++
BuildName: Linux-icpc

# Submission information
IsCDash: 
CDashVersion: 
QueryCDashVersion: 
DropSite: 
DropLocation: 
DropSiteUser: 
DropSitePassword: 
DropSiteMode: 
DropMethod: http
TriggerSite: 
ScpCommand: /bin/scp

# Dashboard start time
NightlyStartTime: 00:00:00 EDT

# Commands for the build/test/submit cycle
ConfigureCommand: "/usr/bin/cmake" "/home/Edward.Colon/EMC_noaa-3drtma/sorc/rtma_gsi.fd"
MakeCommand: /bin/gmake -i
DefaultCTestConfigurationType: Release

# CVS options
# Default is "-d -P -A"
CVSCommand: /bin/cvs
CVSUpdateOptions: -d -A -P

# Subversion options
SVNCommand: /bin/svn
SVNOptions: 
SVNUpdateOptions: 

# Git options
GITCommand: /bin/git
GITUpdateOptions: 
GITUpdateCustom: 

# Generic update command
UpdateCommand: /bin/git
UpdateOptions: 
UpdateType: git

# Compiler info
Compiler: /apps/intel/composer_xe_2015.3.187/bin/intel64/icpc

# Dynamic analysis (MemCheck)
PurifyCommand: 
ValgrindCommand: 
ValgrindCommandOptions: 
MemoryCheckCommand: MEMORYCHECK_COMMAND-NOTFOUND
MemoryCheckCommandOptions: 
MemoryCheckSuppressionFile: 

# Coverage
CoverageCommand: /bin/gcov
CoverageExtraFlags: -l

# Cluster commands
SlurmBatchCommand: /apps/slurm/default/bin/sbatch
SlurmRunCommand: /apps/slurm/default/bin/srun

# Testing options
# TimeOut is the amount of time in seconds to wait for processes
# to complete during testing.  After TimeOut seconds, the
# process will be summarily terminated.
# Currently set to 25 minutes
TimeOut: 1500

UseLaunchers: 
CurlOptions: 
# warning, if you add new options here that have to do with submit,
# you have to update cmCTestSubmitCommand.cxx

# For CTest submissions that timeout, these options
# specify behavior for retrying the submission
CTestSubmitRetryDelay: 5
CTestSubmitRetryCount: 3
