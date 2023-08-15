
list( APPEND _statistics_sources
    StatisticsConfiguration.cc
    StatisticsConfiguration.h
    StatisticsIO.cc
    StatisticsIO.h
    SynopticMatcher.cc
    SynopticMatcher.h
    SynopticCollection.cc
    SynopticCollection.h
    io/FstreamIO.cc
    io/FstreamIO.h
    TimeUtils.cc
    TimeUtils.h
    PeriodUpdater.cc
    PeriodUpdater.h
    StatisticsProfiler.h
    OperationWindow.cc
    OperationWindow.h
    Operations.cc
    Operations.h
    StatisticsComponentsActivation.h
    operations/Operation.h
    operations/OperationWithData.h
    operations/Accumulate.h
    operations/Average.h
    operations/FluxAverage.h
    operations/Instant.h
    operations/Minimum.h
    operations/Maximum.h
    operations/Difference.h
    operations/Variance.h
    operations/StdDev.h
    synoptic-matchers/AllTimesMatcher.h
    synoptic-matchers/DailyHoursMatcher.h
    TemporalStatistics.cc
    TemporalStatistics.h
    Statistics.cc
    Statistics.h
)

list( APPEND _statistics_libs
    multio
)

if(HAVE_ATLAS_IO)

ecbuild_find_package( NAME atlas_io    VERSION  0.33   REQUIRED )

list( APPEND _statistics_sources
    io/AtlasIO.cc
    io/AtlasIO.h
)

list( APPEND _statistics_libs
    atlas_io
)

endif()


if ( HAVE_EXECUTION_POLICY )
list( APPEND _tbb_lib )
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
# using Clang
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
list( APPEND _tbb_lib tbb )
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
list( APPEND _tbb_lib tbb )
endif()
endif()

ecbuild_add_library(

    TARGET multio-action-statistics

    SOURCES
        ${_statistics_sources}

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PRIVATE_LIBS
    ${_tbb_lib}
    PUBLIC_LIBS
    ${_statistics_libs}
)
