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

    public :: multio_start_server

contains

    !>
    !! @brief start a multio server
    !!
    !! @param [in] cc - configuration object witht the configuration of the server
    !!
    !! @return error code 
    !!
    function multio_start_server(cc) result(err)
        use :: multio_configuration_mod, only: multio_configuration
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        class(multio_configuration), intent(in) :: cc
        integer :: err
        integer(kind=c_int) :: c_err
        interface
            function c_multio_start_server(cc) result(err) &
                bind(c, name='multio_start_server')
                use, intrinsic :: iso_c_binding
            implicit none
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
            end function c_multio_start_server
        end interface
        c_err = c_multio_start_server(cc%impl)
        err = int(c_err,kind(err))
    end function multio_start_server

end module multio_new_api_mod