
# DataSink plugins

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_sink_plugins
        -Wl,--no-as-needed
    )
endif()

if( HAVE_FDB5 )
    list( APPEND multio_sink_plugins
        multio-fdb5
    )
endif()

if( HAVE_MAESTRO )
    list( APPEND multio_sink_plugins
        multio-maestro
    )
endif()

if (DEBUG_FAPI)
target_compile_definitions(multio-fapi DEBUG_FAPI=1)
endif()

if (DUMMY_FAPI)
target_compile_definitions(multio-fapi DUMMY_FAPI=1)
endif()

# Action plugins

list( APPEND multio_action_plugins
    multio-action-aggregate
    multio-action-encode
    multio-action-mask
    multio-action-metadata-mapping
    multio-action-null
    multio-action-print
    multio-action-select
    multio-action-single-field-sink
    multio-action-sink
    multio-action-statistics
    multio-action-transport
)

if( HAVE_MIR )
    list( APPEND multio_action_plugins
        multio-action-interpolate
    )
endif()

add_compile_definitions(DEBUG_FAPI)
add_compile_options(-ffree-line-length-none)
ecbuild_add_library(

    TARGET multio-api

    SOURCES
        multio_c.cc
        multio_c.h

    PUBLIC_LIBS
        ${multio_sink_plugins}
        ${multio_action_plugins}
        multio
)

# Cannot use CONDITION for (ecbuild_)configure_file
if( HAVE_FORTRAN )
    configure_file( multio_config.f90.in multio_config.f90 )
endif()

list( APPEND multio_fapi_srcs
    multio_constants_mod.F90
    multio_error_handling_mod.F90
    multio_configuration_mod.F90
    multio_base_handle_mod.F90
    multio_handle_mod.F90
    multio_data_mod.F90
    multio_metadata_mod.F90
    multio_utils_mod.F90
    multio_api.F90
    multio_debug_fapi.h
)

ecbuild_add_library(

    TARGET multio-fapi

    CONDITION HAVE_FORTRAN

    SOURCES
        ${multio_fapi_srcs}
        ${CMAKE_CURRENT_BINARY_DIR}/multio_config.f90

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/module>
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>

    PUBLIC_LIBS
        multio-api
)
