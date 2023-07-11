!> @file
!! @brief Module that contains the constants used by multio-fapi
!!
!!
!! @author Mirco Valentini
!! @date   July 11, 2023
!! @version 1.0
!!
!! @note The behaviour of this module depends on the "multio_debug_fapi.h"
!!       that contains preprocessor macro.
!!


#include "multio_debug_fapi.h"

module multio_constants_mod
implicit none

    ! Default visibility
    private

    ! Error codes
    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3


    ! Whitelist of public symbols
    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

end module multio_constants_mod