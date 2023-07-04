module multio_utils_mod
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr
implicit none

    ! Default visibility
    private

    ! Error codes
    integer, parameter :: MULTIO_SUCCESS = 0
    integer, parameter :: MULTIO_ERROR_ECKIT_EXCEPTION = 1
    integer, parameter :: MULTIO_ERROR_GENERAL_EXCEPTION = 2
    integer, parameter :: MULTIO_ERROR_UNKNOWN_EXCEPTION = 3

    ! Constants
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: double_size = 8 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: float_size = 4 !c_sizeof(1.0_dp) !intel compiler...
    integer, parameter :: int64 = selected_int_kind(15)

    ! Error handling definitions
    type multio_failure_info
        type(c_ptr) :: impl = c_null_ptr
    end type


    !>
    !! Interface of the failur handlers
    abstract interface
        subroutine failure_handler_t(context, error, info)
            import multio_failure_info
            implicit none
            integer, parameter :: int64 = selected_int_kind(15)
            integer(int64), intent(inout) :: context
            integer, intent(in) :: error
            class(multio_failure_info), intent(in) :: info
        end subroutine
    end interface


    !>
    !! @class not to te suse in the handler list
    type multio_fort_failure_info_node
        integer(c_int) :: id = 0
        integer(int64) :: context = 0
        procedure(failure_handler_t), nopass, pointer :: handler_fn => null()
        type(multio_fort_failure_info_node),  pointer :: next => null()
    end type


    !>
    !! @class list of failure handlers
    type multio_fort_failure_info_list
        integer(c_int) :: lastId = 0
        integer :: count = 0
        type(multio_fort_failure_info_node), pointer :: head => null()
        type(multio_fort_failure_info_node), pointer :: tail => null()
    contains
        procedure :: callHandler => multio_fort_failure_call
        procedure :: add         => multio_fort_failure_add
        procedure :: remove      => multio_fort_failure_remove
    end type


    !> Lis of handlers
    type(multio_fort_failure_info_list), save :: failure_info_list


    !> Failure handler
    integer(int64), save :: failure_handler_context
    procedure(failure_handler_t), pointer, save :: failure_handler_fn


    ! Whitelist of public symbols
    public :: multio_failure_info 
    public :: multio_fort_failure_info_node
    public :: multio_fort_failure_info_list

    public :: failure_info_list

    public :: failure_handler_t
    public :: failure_handler_context
    public :: failure_handler_fn

    public :: int64
    public :: multio_fort_failure_call
    public :: multio_fort_failure_add
    public :: failure_handler_wrapper
    public :: multio_fort_failure_remove

contains

    !>
    !! @brief call the failure handler associated with the given id
    !!
    !! @param [in,out] ffi   - failure handlers list
    !! @param [in]     id    - id of the failure handler to be called
    !! @param [in]     err   - error id during the call of the error handler
    !! @param [in]     info  - failure info
    !!
    !! @return error code 
    !!
    subroutine multio_fort_failure_call(ffi, id, err, info)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        integer(c_int),                       intent(in)    :: id
        integer,                              intent(out)   :: err
        class(multio_failure_info),           intent(in)    :: info
        ! Local variables
        type(multio_fort_failure_info_node), pointer :: node
        ! Implementation
        node => ffi%head
        do while(ASSOCIATED(node))
            if (node%id == id) then
                call node%handler_fn(node%context, err, info)
                node => null()
            else
                node => node%next
            endif
        enddo
        ! Exit point
        return
    end subroutine multio_fort_failure_call


    !>
    !! @brief add a failure handler to the list of failure handler list
    !!
    !! @param [in,out] ffi        - failure handlers list
    !! @param [in]     handler_fn - failure handler to be added to the list
    !! @param [in]     context    - context associated to the failure handler
    !!
    !! @return error code 
    !!
    function multio_fort_failure_add(ffi, handler_fn, context) result(new_id_loc)
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        procedure(failure_handler_t), pointer :: handler_fn
        integer(int64),                       intent(in)    :: context
        ! Local variables
        type(c_ptr) :: new_id_loc
        class(multio_fort_failure_info_node), pointer :: new_node
        ! Implementation
        ffi%lastId = ffi%lastId + 1
        ffi%count = ffi%count + 1

        allocate(new_node);
        new_node%id = ffi%lastId
        new_node%handler_fn => handler_fn
        new_node%context = context

        new_id_loc = c_loc(new_node%id)

        if(.not. associated(ffi%head)) then
            ffi%head => new_node
        end if

        if(associated(ffi%tail)) then
            ffi%tail%next => new_node
        endif
        ffi%tail => new_node
        ! Exit point
        return
    end function multio_fort_failure_add


    !>
    !! @brief remove a failure handler to the list of failure handler list
    !!
    !! @param [in,out] ffi  - failure handlers list
    !! @param [in]     id   - failure handler to be removed
    !!
    !! @return error code 
    !!
    subroutine multio_fort_failure_remove(ffi, id)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        integer(c_int),                       intent(in)    :: id
        ! Local variables
        type(multio_fort_failure_info_node), pointer :: node
        type(multio_fort_failure_info_node), pointer :: node_prev
        ! Initialization
        node_prev => null()
        node => ffi%head
        ! Implementation
        do while (associated(node))
            if (node%id == id) then
                if(associated(node_prev)) then
                    node_prev%next => node%next
                end if

                if(associated(ffi%head, node)) then
                    ffi%head => node%next
                endif

                if(associated(ffi%tail, node)) then
                    ffi%tail => node_prev
                endif

                ffi%count = ffi%count - 1

                deallocate(node)
                node => null()
            else
                node_prev => node
                node => node%next
            endif
        enddo
        ! Exit point
        return
    end subroutine multio_fort_failure_remove


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
            pure function strlen( str ) result(len)
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
    !! @brief wrap a failure handler
    !!
    !! @param [in,out] context_id - 
    !! @param [in]     c_error    - 
    !! @param [in]     info       - 
    !!
    !! @return error code 
    !!
    subroutine failure_handler_wrapper(context_id, c_error, info) &
                bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        type(c_ptr),    value, intent(in)    :: context_id
        integer(c_int),        intent(inout) :: c_error
        type(c_ptr),    value, intent(in)    :: info
        ! Local variables
        type(multio_failure_info) :: finfo
        integer(c_int), pointer :: id
        integer :: err
        ! Implementation
        call c_f_pointer( context_id, id )
        finfo%impl = info
        call failure_info_list%callHandler(id, err, finfo)
        ! Output cast and cleanup
        c_error = int(err,c_int)
        ! Exit point
        return
    end subroutine failure_handler_wrapper


    !>
    !! @brief return the version of multio
    !!
    !! @param [out] version_str - multio version
    !!
    !! @return error code 
    !!
    function multio_version(version_str) result(err)
        use, intrinsic :: iso_c_binding, only: c_ptr
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
    !! @param [in] err - error code
    !!
    !! @return error string associated to err
    !!
    function multio_error_string(err) result(error_string)
    implicit none
        ! Dummy arguments
        integer, intent(in) :: err
        ! Function result
        character(:), allocatable, target :: error_string
        ! Private interfaces
        interface
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
        error_string = fortranise_cstr(c_multio_error_string(err))
        ! Exit point
        return
    end function multio_error_string

end module multio_utils_mod