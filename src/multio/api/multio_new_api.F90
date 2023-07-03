module multio_new_api_mod

    use :: multio_configuration_mod, only: multio_configuration
    use :: multio_handle_mod,        only: multio_handle
    use :: multio_metadata_mod,      only: multio_metadata
    use :: multio_data_mod,          only: multio_data

implicit none

    ! Default symbol visibility
    private

    ! White list of the visible symbols
    public :: multio_configuration
    public :: multio_handle
    public :: multio_metadata
    public :: multio_data

end module multio_new_api_mod