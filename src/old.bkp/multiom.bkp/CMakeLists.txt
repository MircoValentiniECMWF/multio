if ( HAVE_OUTPUT_MANAGER )

# Update on new big features
set(OUTPUT_MANAGER_MAJOR 0)

# Interface changes
set(OUTPUT_MANAGER_MINOR 0)

# Implementation updates with constant interfaces
set(OUTPUT_MANAGER_PATCH 1)

# Use Fortran to get the compiler and compiler version
execute_process(
    COMMAND ${CMAKE_Fortran_COMPILER} --version
    OUTPUT_VARIABLE FORTRAN_COMPILER_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REGEX REPLACE "\n" ";" lines "${FORTRAN_COMPILER_VERSION}")
list(GET lines 0 FORTRAN_COMPILER_INFO)

# Linker flags (remove trailing spaces)
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_BUILD_FLAGS         "${IFS_Fortran_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_EXE_LINKER_FLAGS    "${ECBUILD_EXE_LINKER_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_SHARED_LINKER_FLAGS "${ECBUILD_SHARED_LINKER_FLAGS}")

# Get the git sha of the current commit
execute_process(
    COMMAND git rev-parse HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/ifs-source
    OUTPUT_VARIABLE GIT_SHA
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the current date and time
string(TIMESTAMP BUILD_DATE "%Y-%m-%d %H:%M:%S")

# Get the build type (e.g., Debug, Release)
if(CMAKE_BUILD_TYPE)
    set(BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    set(BUILD_TYPE "Unknown")
endif()

# Get the hostname
execute_process(
    COMMAND hostname
    OUTPUT_VARIABLE HOSTNAME
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

#
# + CONFIGURE HEADERS
# ======================================================================
# configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/output_manager_version_mod.F90.in                               ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_utils.h.in         ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_trace_utils.h.in   ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_trace_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_logging_utils.h.in ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_logging_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_errhdl_utils.h.in  ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_errhdl_utils.h  )


#
# Configure the include path (the variable is defiend and used in the parent scope)
list( APPEND OUTPUT_MANAGER_INCLUDE_PATH
    ${CMAKE_CURRENT_BINARY_DIR}/include/
)

#
#
# Generate generic mapping
add_subdirectory( containers )

#
# + BUILD OUTPUT MANAGER CORE (this organisation comes from dep. problems in CMake)
# ======================================================================
list( APPEND output_manager_src
  common/datakinds_def_mod.F90
  common/enumerators_mod.F90
  hooks/debug/debug_mod.F90
  hooks/hooks_mod.F90
  grib/metadata_base_mod.F90
  configuration/yaml_core_utils_mod.F90
  data-structures/fortran_message_mod.F90
  data-structures/parametrization_mod.F90
  data-structures/time_utils_mod.F90
  grib/grib_section_base_mod.F90
  grib/grib_encoder_options_mod.F90
  grib/grib2/grib2_section0/grib2_section0_000_mod.F90
  grib/grib2/grib2_section0/grib2_section0_factory_mod.F90
  grib/grib2/grib2_section1/grib2_section1_000_mod.F90
  grib/grib2/grib2_section1/grib2_section1_factory_mod.F90
  grib/grib2/grib2_section2/grib2_section2_001_mod.F90
  grib/grib2/grib2_section2/grib2_section2_007_mod.F90
  grib/grib2/grib2_section2/grib2_section2_009_mod.F90
  grib/grib2/grib2_section2/grib2_section2_015_mod.F90
  grib/grib2/grib2_section2/grib2_section2_018_mod.F90
  grib/grib2/grib2_section2/grib2_section2_023_mod.F90
  grib/grib2/grib2_section2/grib2_section2_024_mod.F90
  grib/grib2/grib2_section2/grib2_section2_026_mod.F90
  grib/grib2/grib2_section2/grib2_section2_027_mod.F90
  grib/grib2/grib2_section2/grib2_section2_030_mod.F90
  grib/grib2/grib2_section2/grib2_section2_036_mod.F90
  grib/grib2/grib2_section2/grib2_section2_192_mod.F90
  grib/grib2/grib2_section2/grib2_section2_factory_mod.F90
  grib/grib2/grib2_section3/grib2_section3_040_mod.F90
  grib/grib2/grib2_section3/grib2_section3_050_mod.F90
  grib/grib2/grib2_section3/grib2_section3_101_mod.F90
  grib/grib2/grib2_section3/grib2_section3_factory_mod.F90
  grib/grib2/grib2_section4/grib2_section4_000_mod.F90
  grib/grib2/grib2_section4/grib2_section4_001_mod.F90
  grib/grib2/grib2_section4/grib2_section4_008_mod.F90
  grib/grib2/grib2_section4/grib2_section4_011_mod.F90
  grib/grib2/grib2_section4/grib2_section4_032_mod.F90
  grib/grib2/grib2_section4/grib2_section4_040_mod.F90
  grib/grib2/grib2_section4/grib2_section4_041_mod.F90
  grib/grib2/grib2_section4/grib2_section4_042_mod.F90
  grib/grib2/grib2_section4/grib2_section4_043_mod.F90
  grib/grib2/grib2_section4/grib2_section4_044_mod.F90
  grib/grib2/grib2_section4/grib2_section4_045_mod.F90
  grib/grib2/grib2_section4/grib2_section4_046_mod.F90
  grib/grib2/grib2_section4/grib2_section4_047_mod.F90
  grib/grib2/grib2_section4/grib2_section4_099_mod.F90
  grib/grib2/grib2_section4/grib2_section4_103_mod.F90
  grib/grib2/grib2_section4/grib2_section4_factory_mod.F90
  grib/grib2/grib2_section5/grib2_section5_000_mod.F90
  grib/grib2/grib2_section5/grib2_section5_042_mod.F90
  grib/grib2/grib2_section5/grib2_section5_051_mod.F90
  grib/grib2/grib2_section5/grib2_section5_factory_mod.F90
  grib/grib2/grib2_section6/grib2_section6_000_mod.F90
  grib/grib2/grib2_section6/grib2_section6_factory_mod.F90
  grib/grib2/grib2_section4/grib2_time_configurator/grib2_time_configurator_000_mod.F90
  grib/grib2/grib2_section4/grib2_time_configurator/grib2_time_configurator_001_mod.F90
  grib/grib2/grib2_section4/grib2_time_configurator/grib2_time_configurator_002_mod.F90
  grib/grib2/grib2_section4/grib2_time_configurator/grib2_time_configurator_003_mod.F90
  grib/grib2/grib2_section4/grib2_time_configurator/grib2_time_configurator_factory_mod.F90
  grib/grib2/grib2_section4/grib2_param_configurator/grib2_param_configurator_000_mod.F90
  grib/grib2/grib2_section4/grib2_param_configurator/grib2_param_configurator_001_mod.F90
  grib/grib2/grib2_section4/grib2_param_configurator/grib2_param_configurator_002_mod.F90
  grib/grib2/grib2_section4/grib2_param_configurator/grib2_param_configurator_factory_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_000_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_001_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_002_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_003_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_004_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_005_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_006_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_007_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_008_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_009_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_010_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_011_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_012_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_013_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_014_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_015_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_016_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_017_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_018_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_019_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_020_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_021_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_022_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_023_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_024_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_025_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_026_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_027_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_028_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_029_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_030_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_031_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_032_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_033_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_034_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_035_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_036_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_037_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_038_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_039_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_040_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_041_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_042_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_043_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_044_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_045_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_046_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_047_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_048_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_049_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_050_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_051_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_052_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_053_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_054_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_055_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_056_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_057_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_058_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_059_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_060_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_061_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_062_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_063_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_064_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_065_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_066_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_067_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_068_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_069_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_070_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_071_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_072_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_073_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_074_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_075_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_076_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_077_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_078_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_079_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_080_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_081_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_082_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_083_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_084_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_085_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_086_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_087_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_999_mod.F90
  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_factory_mod.F90
  grib/grib2/grib2_encoder_mod.F90
  grib/grib_encoder_factory_mod.F90
  grib/grib_encoder_utils_mod.F90
  filters/filter_base_mod.F90
  filters/filter_composed_mod.F90
  filters/filter_param_mod.F90
  filters/filter_level_mod.F90
  filters/filter_direction_mod.F90
  filters/filter_frequency_mod.F90
  filters/filter_levtype_mod.F90
  filters/filter_repres_mod.F90
  filters/filter_model_mod.F90
  filters/filter_is_ensemble_mod.F90
  filters/filter_is_chemical_mod.F90
  filters/filter_is_aerosol_mod.F90
  filters/filter_is_chemical_optical_mod.F90
  filters/filter_factory_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_int8_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_int16_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_int32_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_int64_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_real32_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_real64_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/containers/keyset_string64_mod.F90
)


if ( HAVE_OUTPUT_MANAGER_ENCODER_REPORT )
list( APPEND OUTPUT_MANAGER_BUILD_FLAVOUR "WITH_OUTPUT_MANAGER_ENCODING_REPORT" )
endif()

#
# + BUILD OUTPUT MANAGER LIBRARY
# ======================================================================
ecbuild_add_library(
  TARGET  multiom
  SOURCES ${output_manager_src}

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    ${CMAKE_BINARY_DIR}/include
    ${OUTPUT_MANAGER_INCLUDE_PATH}

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
)

# Module directory
set_target_properties( multiom PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )



ecbuild_add_executable(

  TARGET multiom-test01

  SOURCES
    test/test_read01_prog.F90

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
    ${CMAKE_BINARY_DIR}/include
    ${OUTPUT_MANAGER_INCLUDE_PATH}

  LIBS
    fckit
    multiom
    eccodes
    eccodes_f90
    multio-fapi

  LINKER_LANGUAGE Fortran

)


ecbuild_add_executable(

  TARGET multiom-test02

  SOURCES
    test/test_read02_prog.F90


  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
    ${CMAKE_BINARY_DIR}/include
    ${OUTPUT_MANAGER_INCLUDE_PATH}

  LIBS
    fckit
    multiom
    eccodes
    eccodes_f90
    multio-fapi

  LINKER_LANGUAGE Fortran

)

endif()
