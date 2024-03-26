# Source me to get the correct configure/build/run environment

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  if [ "${2:-""}" == "ECBUNDLE_CONFIGURE_ONLY" ]; then
    if [ -n "${ECBUNDLE_CONFIGURE:-""}" ]; then
      echo "+ module load $1"
      module load $1
    else
      echo " WARNING: Module $1 not loaded (only during configuration)"
    fi
  else
    echo "+ module load $1"
    module load $1
  fi
}
module_unload() {
  echo "+ module unload $1"
  module unload $1
}

# Unload to be certain
module_purge() {
  echo "+ module purge"
  module --force purge
}

# Unload all modules to be certain
[[ ${IFS_RUNTIME_ENV:-unset} == "unset" ]] && module_purge


# Load modules
module_load LUMI/23.03
module_load partition/C
module_load cpeCray/23.03
module_load cray-mpich/8.1.25
module_load libaec/1.0.6-cpeCray-23.03

if [[ ${IFS_RUNTIME_ENV:-unset} == "unset" ]]; then
  module_load Eigen/3.4
  module_load Boost/1.81.0-cpeCray-23.03
  module_load ncurses/6.4-cpeCray-23.03
  module_load buildtools/23.03
  module_load cray-python/3.9.13.1 ECBUNDLE_CONFIGURE_ONLY
fi

### Handling of "magic" cray modules
# 1) Load the cray modules
module_load cray-libsci/23.02.1.1
module_load cray-fftw/3.3.10.3
module_load cray-hdf5/1.12.2.3
module_load cray-netcdf/4.9.0.3
# 2) Store variables to locate the packages
export CRAY_LIBSCI=${CRAY_LIBSCI_PREFIX_DIR}/lib/libsci_cray.so
_FFTW_ROOT=${FFTW_ROOT}
_HDF5_ROOT=${CRAY_HDF5_PREFIX}
_NETCDF_ROOT=${CRAY_NETCDF_PREFIX}
# 3) Unload the cray modules in reverse order, removing all the magic
module unload cray-netcdf
module unload cray-hdf5
module unload cray-fftw
module unload cray-libsci
# 4) Define variables that CMake introspects
export FFTW_ROOT=${_FFTW_ROOT}
export HDF5_ROOT=${_HDF5_ROOT}
export NETCDF_ROOT=${_NETCDF_ROOT}

module list 2>&1
set -x

export CRAY_ADD_RPATH=yes

# This is required to work around SIGSEGV in ectrans' SGEMM calls, which
# occur when "rome" or "milan" are used (backtrace points to openblas_sgemm__naples)
export LIBSCI_ARCH_OVERRIDE=broadwell

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null
