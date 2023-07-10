module multio_error_handling_mod
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr
   use, intrinsic :: iso_fortran_env, only: int64
implicit none

    ! Default visibility
    private


    ! Error handling definitions
    type :: multio_failure_info
        type(c_ptr) :: impl = c_null_ptr
    end type


    !>
    !! Interface of the failure handlers
    abstract interface
        subroutine failure_handler_t(context, error, info)
            use, intrinsic :: iso_fortran_env, only: int64
            import :: multio_failure_info
        implicit none
            integer(int64),             intent(inout) :: context
            integer,                    intent(in)    :: error
            class(multio_failure_info), intent(in)    :: info
        end subroutine failure_handler_t
    end interface


    !>
    !! @class
    type :: multio_fort_failure_info_node
        integer(c_int) :: id = 0
        integer(int64) :: context = 0
        procedure(failure_handler_t), nopass, pointer :: handler_fn => null()
        type(multio_fort_failure_info_node),  pointer :: next => null()
    end type


    !>
    !! @class list of failure handlers
    type :: multio_fort_failure_info_list
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
    ! integer(int64), save :: failure_handler_context
    ! procedure(failure_handler_t), pointer, save :: failure_handler_fn


    ! Whitelist of public symbols
    public :: multio_failure_info
    public :: multio_fort_failure_info_node
    public :: multio_fort_failure_info_list

    public :: failure_info_list

    public :: failure_handler_t
    ! public :: failure_handler_context
    ! public :: failure_handler_fn

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
        integer,                              intent(in)    :: err
        class(multio_failure_info),           intent(in)    :: info
        ! Local variables
        type(multio_fort_failure_info_node), pointer :: node
        ! Implementation
        node => ffi%head
        do while(associated(node))
            if (node%id .eq. id) then
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
    use :: multio_constants_mod, only: int64
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list),  intent(inout) :: ffi
        procedure(failure_handler_t), pointer, intent(in)    :: handler_fn
        integer(int64),                        intent(in)    :: context
        ! Function result
        type(c_ptr) :: new_id_loc
        ! Local variables
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
        use, intrinsic :: iso_c_binding, only: c_f_pointer
    implicit none
        ! Dummy arguments
        type(c_ptr),    value, intent(in) :: context_id
        integer(c_int), value, intent(in) :: c_error
        type(c_ptr),    value, intent(in) :: info
        ! Local variables
        type(multio_failure_info) :: finfo
        integer(c_int), pointer :: id
        integer :: err
        ! Implementation
        call c_f_pointer( context_id, id )
        finfo%impl = info
        err = int(c_error,kind(err))
        call failure_info_list%callHandler(id, err, finfo)
        ! Exit point
        return
    end subroutine failure_handler_wrapper

end module multio_error_handling_mod