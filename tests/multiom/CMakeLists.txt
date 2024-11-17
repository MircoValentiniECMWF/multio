add_subdirectory(encoding-rules)
# add_subdirectory(mapping-rules)
add_subdirectory(samples)

#
# Run a test case for the multiom encoder
ecbuild_add_test(
    TARGET       multiom-encoder-01
    COMMAND      multiom_test_read21.x
    ENVIRONMENT  ENCODING_RULES_DIR=${CURRENT_BINARY_DIR}/encoding-rules MAPPING_RULES_DIR=${CURRENT_BINARY_DIR}/mapping-rules ECCODES_SAMPLES_PATH=${CURRENT_BINARY_DIR}/samples
    ARGS         ${CURRENT_BINARY_DIR}/encoding-rules/encoding-rules.yaml  ${CURRENT_BINARY_DIR}/mapping-rules/mapping-rules.yaml
)
