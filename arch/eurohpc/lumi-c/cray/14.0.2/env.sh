# Source me to get the correct configure/build/run environment

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  if [ "${2:-""}" == "ECBUILD_CONFIGURE_ONLY" ]; then
    if [ -n "${ECBUILD_CONFIGURE}" ]; then
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
module_load LUMI/22.08
module_load partition/C
module_load cpeCray/22.08
module_load cray-mpich/8.1.23
module_load cray-fftw/3.3.10.1
module_load cray-hdf5/1.12.1.5
module_load cray-netcdf/4.8.1.5
module_load libaec/1.0.6-cpeCray-22.08

if [[ ${IFS_RUNTIME_ENV:-unset} == "unset" ]]; then
  module_load cray-python/3.9.12.1
  module_load Eigen/3.4
  module_load Boost/1.79.0-cpeCray-22.08
  module_load ncurses/6.2-cpeCray-22.08
  module_load buildtools/22.08-minimal
fi

module list 2>&1
set -x

export ECBUILD_TOOLCHAIN=./toolchain.cmake

export CRAY_ADD_RPATH=yes


# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null
