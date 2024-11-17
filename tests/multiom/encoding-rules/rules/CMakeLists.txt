# Define the source and binary directories
set(YAML_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(YAML_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Find all YAML files in the source directory
file(GLOB YAML_FILES "${YAML_SOURCE_DIR}/*.yaml")

# Loop over the YAML files and copy them to the binary directory
foreach(YAML_FILE ${YAML_FILES})
    get_filename_component(YAML_FILENAME ${YAML_FILE} NAME)
    configure_file(${YAML_FILE} ${YAML_TARGET_DIR}/${YAML_FILENAME} COPYONLY)
endforeach()