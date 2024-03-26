if( DEFINED ecbuild_VERSION AND NOT ${ecbuild_VERSION} VERSION_LESS 3.0 )
    if( DEFINED ENABLE_NEMO AND NOT ENABLE_NEMO )
         ecbuild_warn( "Current environment in combination with ENABLE_NEMO=OFF seems to "
                       "have problems with MPI_INIT_THREAD(), causing SEGFAULTS. "
                       "It seems to work with ENABLE_MGRIDS=OFF for obscure reasons." )
         set( ENABLE_MGRIDS OFF CACHE STRING "Disabled MGRIDS in IFS when ENABLE_NEMO=OFF" )
         set( ECBUILD_CXX_FLAGS_BIT "-O2 -hfp1 -G2 -hflex_mp=conservative -DNDEBUG" )
    endif()
    if( DEFINED ENABLE_SINGLE_PRECISION AND NOT ENABLE_SINGLE_PRECISION )
         ecbuild_warn( "Current environment in combination with ENABLE_SINGLE_PRECISION=OFF seems to "
                       "have problems with MPI_INIT_THREAD(), causing SEGFAULTS. "
                       "It seems to work with ENABLE_MGRIDS=OFF for obscure reasons." )
         set( ENABLE_MGRIDS OFF CACHE STRING "Disabled MGRIDS in IFS when ENABLE_SINGLE_PRECISION=ON" )
    endif()
endif()

ecbuild_warn( "FDB_REMOTE does not compile for Cray 8.5.8. Forcefully disabling" )
set( ENABLE_FDB_REMOTE OFF CACHE BOOL "Disable FDB_REMOTE for Cray 8.5.8" FORCE )
