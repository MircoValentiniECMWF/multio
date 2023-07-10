module multio_api

    ! Constants
    use :: multio_constants_mod, only: MULTIO_SUCCESS
    use :: multio_constants_mod, only: MULTIO_ERROR_ECKIT_EXCEPTION
    use :: multio_constants_mod, only: MULTIO_ERROR_GENERAL_EXCEPTION
    use :: multio_constants_mod, only: MULTIO_ERROR_UNKNOWN_EXCEPTION

    ! Datatypes
    use :: multio_configuration_mod, only: multio_configuration
    use :: multio_handle_mod,        only: multio_handle
    use :: multio_metadata_mod,      only: multio_metadata
    use :: multio_data_mod,          only: multio_data

    ! Utils
    use :: multio_utils_mod, only: multio_initialise
    use :: multio_utils_mod, only: multio_start_server
    use :: multio_utils_mod, only: multio_version
    use :: multio_utils_mod, only: multio_vcs_version
    use :: multio_utils_mod, only: multio_error_string

    ! Failure handling
    use :: multio_error_handling_mod, only: multio_failure_info

implicit none

    ! Default symbol visibility
    private

    ! White list of the visible symbols

    ! Public constants
    public :: MULTIO_SUCCESS
    public :: MULTIO_ERROR_ECKIT_EXCEPTION
    public :: MULTIO_ERROR_GENERAL_EXCEPTION
    public :: MULTIO_ERROR_UNKNOWN_EXCEPTION

    ! Public datatypes
    public :: multio_configuration
    public :: multio_handle
    public :: multio_metadata
    public :: multio_data

    ! Public utils
    public :: multio_initialise
    public :: multio_start_server
    public :: multio_version
    public :: multio_vcs_version
    public :: multio_error_string

    ! Public error handling
    public :: multio_failure_info

end module multio_api