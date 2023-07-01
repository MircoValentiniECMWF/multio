module multio_utils_mod

implicit none

    ! Default visibility
    private

    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3

    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: double_size = 8 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: float_size = 4 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: int64 = selected_int_kind(15)

    ! Error handling definitions

    abstract interface
        subroutine failure_handler_t(context, error)
            implicit none
            integer, parameter :: int64 = selected_int_kind(15)
            integer(int64), intent(inout) :: context
            integer, intent(in) :: error
        end subroutine
    end interface

    integer(int64), save :: failure_handler_context
    procedure(failure_handler_t), pointer, save :: failure_handler_fn

    interface

        function c_multio_version(pstr) result(err) &
                bind(c, name='multio_version')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: pstr
            integer(c_int) :: err
        end function

        function c_multio_vcs_version(pstr) result(err) &
                bind(c, name='multio_vcs_version')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(out) :: pstr
            integer(c_int) :: err
        end function

        function multio_initialise() result(err) &
                bind(c, name='multio_initialise')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int) :: err
        end function

        function c_multio_start_server(cc) result(err) &
                bind(c, name='multio_start_server')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: cc
            integer(c_int) :: err
        end function

        function c_multio_set_failure_handler(handler, context) result(err) &
                bind(c, name='multio_set_failure_handler')
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_funptr), intent(in), value :: handler
            type(c_ptr), intent(in), value :: context
            integer(c_int) :: err
        end function

        function c_multio_error_string(err) result(error_string) &
                bind(c, name='multio_error_string')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: err
            type(c_ptr) :: error_string
        end function
    end  interface

contains

    function fortranise_cstr(cstr) result(fstr)
        type(c_ptr), intent(in) :: cstr
        character(:), allocatable, target :: fstr
        character(c_char), pointer :: tmp(:)
        integer :: length

        length = strlen(cstr)
        allocate(character(length) :: fstr)
        call c_f_pointer(cstr, tmp, [length])
        fstr = transfer(tmp(1:length), fstr)
    end function

    subroutine failure_handler_wrapper(unused_context, error) &
                bind(c)
        type(c_ptr), value :: unused_context
        integer(c_long), intent(in), value :: error
        call failure_handler_fn(failure_handler_context, int(error))
    end subroutine

    function multio_set_failure_handler(handler, context) result(err)
        integer(int64) :: context
        integer :: err

        interface
            subroutine handler (ctx, err)
                implicit none
                integer, parameter :: int64 = selected_int_kind(15)
                integer(int64), intent(inout) :: ctx
                integer, intent(in) :: err
            end subroutine
        end interface

        failure_handler_fn => handler
        failure_handler_context = context
        err = c_multio_set_failure_handler(c_funloc(failure_handler_wrapper), c_null_ptr)
    end function

    function multio_version(version_str) result(err)
        character(:), allocatable, intent(out) :: version_str
        type(c_ptr) :: tmp_str
        integer :: err
        err = c_multio_version(tmp_str)
        if (err == MULTIO_SUCCESS) version_str = fortranise_cstr(tmp_str)
    end function

    function multio_vcs_version(git_sha1) result(err)
        character(:), allocatable, intent(out) :: git_sha1
        type(c_ptr) :: tmp_str
        integer :: err
        err = c_multio_vcs_version(tmp_str)
        if (err == MULTIO_SUCCESS) git_sha1 = fortranise_cstr(tmp_str)
    end function

    function multio_error_string(err) result(error_string)
        integer, intent(in) :: err
        character(:), allocatable, target :: error_string
        error_string = fortranise_cstr(c_multio_error_string(err))
    end function

    function multio_start_server(cc) result(err)
        class(multio_configuration), intent(in) :: cc
        integer :: err
        err = c_multio_start_server(cc%impl)
    end function

end module multio_utils_mod