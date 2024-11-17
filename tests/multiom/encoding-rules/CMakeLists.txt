# Main file containing the rules
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/encoding-rules.yaml ${CMAKE_CURRENT_BINARY_DIR}/encoding-rules.yaml COPYONLY )

# Copy the elemental rules
add_subdirectory(rules)
