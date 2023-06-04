ecbuild_add_library(

    TARGET multio-action-statistics

    SOURCES
        Statistics.cc
        Statistics.h
        TemporalStatistics.cc
        TemporalStatistics.h
        Period.cc
        Period.h
        Operation.cc
        Operation.h
        StatisticsOptions.cc
        StatisticsOptions.h
        StatisticsIO.cc
        StatisticsIO.h
        operations/OperationBase.h
        operations/OperationWithData.h
        operations/Accumulate.h
        operations/Average.h
        operations/FluxAverage.h
        operations/Instant.h
        operations/Minimum.h
        operations/Maximum.h
        windows/HourlyStatistics.cc
        windows/HourlyStatistics.h
        windows/DailyStatistics.cc
        windows/DailyStatistics.h
        windows/MonthlyStatistics.cc
        windows/MonthlyStatistics.h


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_IO_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio atlas_io
)
