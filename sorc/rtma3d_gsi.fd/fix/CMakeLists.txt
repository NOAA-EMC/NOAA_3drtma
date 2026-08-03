# Include the list of binary files in the GSI
include(gsi_binary_files.cmake)

# Decide if GSI Binary files need to be downloaded or simply copied.
if(DEFINED ENV{GSI_BINARY_SOURCE_DIR})
  set(DOWNLOAD_TARBALL OFF)
  set(GSI_BIN_FIX_DIR $ENV{GSI_BINARY_SOURCE_DIR})
else()
  set(DOWNLOAD_TARBALL ON)
  set(GSI_BIN_FIX_DIR ${CMAKE_CURRENT_BINARY_DIR})
endif()

# Set URL, sha256sum and tar name of the file to download.
set(URL "https://ftp.emc.ncep.noaa.gov/static_files/public/gsi")
set(SHA "b90f7b939482f1f0dd2700e2b108554bf527fe39a6f44d275545b1bf67fef09d")
string(SUBSTRING ${SHA} 0 6 SHORTSHA)
set(TAR "gsi-fix-${SHORTSHA}.tgz") # poor-man's version control

# Check if the tarball is already downloaded.
# This is possible if one runs cmake in the same build directory
# In this case, we don't want to keep downloading over and over again
# Check the hash of the tarball to ensure integrity.
if(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${TAR})
  file(SHA256 ${CMAKE_CURRENT_BINARY_DIR}/${TAR} SHA256SUM)
  if("${SHA256SUM}" STREQUAL "${SHA}")
    message(STATUS "${TAR} found, skipping ...")
    set(DOWNLOAD_TARBALL OFF)
  else()
    message(STATUS "Incomplete ${TAR}, downloading ...")
    set(DOWNLOAD_TARBALL ON)
  endif()
endif()

if(DOWNLOAD_TARBALL)
  message(STATUS "Downloading ${TAR} from ${URL}")

  # Download binary fix files and check SHA256
  file(DOWNLOAD
    ${URL}/${TAR}
    ${CMAKE_CURRENT_BINARY_DIR}/${TAR}
    INACTIVITY_TIMEOUT 30
    TIMEOUT 600
    SHOW_PROGRESS
    STATUS _DOWNLOAD_STATUS
    EXPECTED_HASH SHA256=${SHA}
    )

  list(GET _DOWNLOAD_STATUS 0 _STATUS)
  list(GET _DOWNLOAD_STATUS 1 _MESSAGE)
  if(_STATUS)
    message(FATAL_ERROR "Error downloading ${TAR}: ${_MESSAGE}")
  endif()

  # Extract downloaded tarball.
  file(ARCHIVE_EXTRACT INPUT ${TAR})
  message(DEBUG "Download ${TAR} succeeded")
endif()

# Copy from the downloaded or central storage location
message(STATUS "Copying GSI binary fix files from ${GSI_BIN_FIX_DIR}")
file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/Big_Endian)
foreach(file IN LISTS gsi_binaries)
  message(DEBUG "copying ... ${file}")
  configure_file(${GSI_BIN_FIX_DIR}/${file} ${CMAKE_CURRENT_SOURCE_DIR}/${file} COPYONLY)
  # TODO: When "who-shall-not-be-named" platforms update their CMake to v3.21.
  # replace configure_file above with the file(COPY_FILE) below.
  #file(COPY_FILE ${GSI_BIN_FIX_DIR}/${file} ${CMAKE_CURRENT_SOURCE_DIR}/${file} ONLY_IF_DIFFERENT)
endforeach()

