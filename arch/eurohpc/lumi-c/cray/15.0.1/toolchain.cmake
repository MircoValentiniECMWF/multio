# Fix failure to discover OpenMP_Fortran_LIB_NAMES
set( OpenACC_Fortran_FLAGS "-hacc" )
set( OpenMP_Fortran_FLAGS "-homp" )
set( OpenMP_Fortran_LIB_NAMES "craymp" )

# Enforce linking to serial libsci
set( BLAS_LIBRARIES   "$ENV{CRAY_LIBSCI}" CACHE PATH "BLAS_LIBRARIES" FORCE )
set( LAPACK_LIBRARIES "$ENV{CRAY_LIBSCI}" CACHE PATH "LAPACK_LIBRARIES" FORCE )
