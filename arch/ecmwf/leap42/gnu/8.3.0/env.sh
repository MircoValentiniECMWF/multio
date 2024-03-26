# Source me to get the correct configure/build/run environment

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  echo "+ module load $1"
  module load $1
}
module_unload() {
  echo "+ module unload $1"
  module unload $1
}

# Unload to be certain
module_unload aec
module_unload odb
module_unload odc
module_unload ecbuild
module_unload metview
module_unload emos
module_unload eccodes
module_unload fftw
module_unload openmpi
module_unload boost
module_unload fcm
module_unload netcdf
module_unload netcdf4
module_unload hdf5
module_unload python3
module_unload nag
module_unload proj4
module_unload gnu
module_unload clang
module_unload intel
module_unload cmake

# Load modules
module_load gnu/8.3.0
module_load fftw/3.3.4
module_load boost/1.61.0
module_load openmpi/2.1.3
module_load eigen/3.2.0
module_load cmake/3.15.0
module_load ninja/1.7.2
module_load netcdf4/4.4.1
module_load hdf5/1.8.17
module_load aec/1.0.4
module_load fcm/2015.03.0
module_load python3/3.6.8-01
module_load proj4/4.8.0

module list 2>&1

# Increase stack size to maximum
ulimit -S -s unlimited

# This is used to download binary test data
export http_proxy="http://slb-proxy-web.ecmwf.int:3333/"

# Restore tracing to stored setting
if [[ -n "$tracing_" ]]; then set -x; else set +x; fi

