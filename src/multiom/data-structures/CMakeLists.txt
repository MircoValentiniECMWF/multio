set(MULTIOM_DATA_STRUCTURES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + data-structures sources: ${MULTIOM_DATA_STRUCTURES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_SOURCES
  ${MULTIOM_DATA_STRUCTURES_DIR}/fortran_message_enumerators_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/parametrization_enumerators_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/fortran_message_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/parametrization_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/time_utils_mod.F90
  CACHE INTERNAL "List of all sources in data-structures directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_DATA_STRUCTURES_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
