# Define the source and binary directories
set(TMPL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(TMPL_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Find all YAML files in the source directory
file(GLOB TMPL_FILES "${TMPL_SOURCE_DIR}/*.tmpl")

# Loop over the YAML files and copy them to the binary directory
foreach(TMPL_FILE ${TMPL_FILES})
    get_filename_component(TMPL_FILENAME ${TMPL_FILE} NAME)
    configure_file(${TMPL_FILE} ${TMPL_TARGET_DIR}/${TMPL_FILENAME} COPYONLY)
endforeach()