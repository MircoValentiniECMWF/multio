module multio_constants_mod
implicit none

    ! Default visibility
    private

    ! Error codes
    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3


    ! Constants
    integer, parameter :: int64 = selected_int_kind(15)


    ! Whitelist of public symbols
    public :: int64

    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

end module multio_constants_mod