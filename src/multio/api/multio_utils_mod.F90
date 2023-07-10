module multio_utils_mod
implicit none

    ! Default visibility
    private

    ! Whitelist of public symbols
    public :: multio_initialise
    public :: multio_start_server
    public :: multio_version
    public :: multio_vcs_version
    public :: multio_error_string

contains

    !>
    !! @brief initialization of multio
    !!
    !! @return error code
    !!
    function multio_initialise() result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_initialise() result(err) &
                    bind(c, name='multio_initialise')
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                integer(c_int) :: err
            end function c_multio_initialise
        end interface
        ! Implementation
        c_err = c_multio_initialise()
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_initialise


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
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_start_server(cc) result(err) &
                bind(c, name='multio_start_server')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: cc
                integer(c_int) :: err
            end function c_multio_start_server
        end interface
        ! Implementation
        c_err = c_multio_start_server(cc%c_ptr())
        ! Cast output values
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_start_server

    !>
    !! @brief remove a failure handler to the list of failure handler list
    !!
    !! @param [in,out] ffi  - failure handlers list
    !! @param [in]     id   - failure handler to be removed
    !!
    !! @return error code
    !!
    function fortranise_cstr(cstr) result(fstr)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_f_pointer
    implicit none
        ! Dummy arguments
        type(c_ptr), intent(in) :: cstr
        ! Function result
        character(:), allocatable, target :: fstr
        ! Local variables
        character(c_char), dimension(:), pointer :: tmp
        integer :: length
        ! Private interfaces
        interface
            pure function strlen( str ) result(len) &
                    bind(c,name='strlen')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: str
                integer(kind=c_int) :: len
            end function strlen
        end interface
        ! Private inerfaces
        length = strlen(cstr)
        allocate(character(length) :: fstr)
        call c_f_pointer(cstr, tmp, [length])
        fstr = transfer(tmp(1:length), fstr)
        ! Exit point
        return
    end function fortranise_cstr


    !>
    !! @brief return the version of multio
    !!
    !! @param [out] version_str - multio version
    !!
    !! @return error code
    !!
    function multio_version(version_str) result(err)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use :: multio_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        character(:), allocatable, intent(out) :: version_str
        ! Function result
        integer :: err
        ! Local variables
        type(c_ptr) :: tmp_str
        ! Private interface
        interface
            function c_multio_version(pstr) result(err) &
                    bind(c, name='multio_version')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: pstr
                integer(c_int) :: err
            end function
        end interface
        ! Implementation
        err = c_multio_version(tmp_str)
        if (err == MULTIO_SUCCESS) version_str = fortranise_cstr(tmp_str)
        ! Exit point
        return
    end function multio_version


    !>
    !! @brief return the git sha of multio
    !!
    !! @param [out] git_sha1 - multio git sha
    !!
    !! @return error code
    !!
    function multio_vcs_version(git_sha1) result(err)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_constants_mod, only: MULTIO_SUCCESS
    implicit none
        ! Dummy arguments
        character(:), allocatable, intent(out) :: git_sha1
        ! Function result
        integer :: err
        ! Local variables
        type(c_ptr) :: tmp_str
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_vcs_version(pstr) result(err) &
                    bind(c, name='multio_vcs_version')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: pstr
                integer(c_int) :: err
            end function c_multio_vcs_version
        end interface
        ! Implementation
        c_err = c_multio_vcs_version(tmp_str)
        if (err == MULTIO_SUCCESS) git_sha1 = fortranise_cstr(tmp_str)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_vcs_version

    !>
    !! @brief return the version of multio
    !!
    !! @param [in] err  - error code
    !! @param [in] info - error info
    !!
    !! @return error string associated to err
    !!
    function multio_error_string(err, info) result(error_string)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_error_handling_mod, only: multio_failure_info
    implicit none
        ! Dummy arguments
        integer,                              intent(in) :: err
        class(multio_failure_info), optional, intent(in) :: info
        ! Function result
        character(:), allocatable, target :: error_string
        ! Local variablees
        integer(kind=c_int) :: c_err
        ! Private interfaces
        interface
            function c_multio_error_string_info(err, info) result(error_string) &
                bind(c, name='multio_error_string_info')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                integer(c_int), value, intent(in) :: err
                type(c_ptr), value,    intent(in) :: info
                type(c_ptr) :: error_string
            end function c_multio_error_string_info
            function c_multio_error_string(err) result(error_string) &
                    bind(c, name='multio_error_string')
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_ptr
            implicit none
                integer(c_int), value, intent(in) :: err
                type(c_ptr) :: error_string
            end function c_multio_error_string
        end interface
        ! Implementation
        c_err = int(err,c_int)
        if (present(info)) then
            error_string = fortranise_cstr(c_multio_error_string_info(c_err,info%impl))
        else
            error_string = fortranise_cstr(c_multio_error_string(c_err))
        end if
        ! Exit point
        return
    end function multio_error_string

end module multio_utils_mod