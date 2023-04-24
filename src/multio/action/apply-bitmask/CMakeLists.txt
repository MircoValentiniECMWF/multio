ecbuild_add_library(

	TARGET multio-action-apply-bitmask

    SOURCES
    	ApplyBitMask.cc
    	ApplyBitMask.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
