# Override ecBuild's default flags containing -march=native.
# On lxc cluster, some of the nodes (e.g. lxc19) are based on a more modern
# Haswell architecture (with richer instruction set), while others are using
# older Ivybridge processors. We need to use the lowest common denominator to
# make the binaries portable across all the nodes.

set(ECBUILD_C_FLAGS_BIT       "-g -O2 -m64 -march=ivybridge -DNDEBUG")
set(ECBUILD_CXX_FLAGS_BIT     "-g -O2 -m64 -march=ivybridge -DNDEBUG")
set(ECBUILD_Fortran_FLAGS_BIT "-g -O2 -m64 -march=ivybridge -DNDEBUG -fno-range-check -fconvert=big-endian")

set(IFS_C_FLAGS       "-march=ivybridge")
set(IFS_CXX_FLAGS     "-march=ivybridge")
set(IFS_Fortran_FLAGS "-march=ivybridge")
