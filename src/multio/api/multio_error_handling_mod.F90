!> @file
!!
!! @brief Definition of the failure handlers
!!
!! Failure handlers are Fortran functions that needs to be called from
!! the "c" lower level.
!!
!! This module has 2 main objectives:
!!
!! 1) avoid to force the users of the api to define handlers that are
!!    callable from "c". This is obtained by wrapping fortran failure
!!    handlers with a c-callable wrapper;
!! 2) allow different failure handlers for different handlers. This
!!    is obtained through a list of filure handlers. The specific failure
!!    handler to be called is then identified in the list using a unique idx;
!!
!! @todo: in order to force the users to use the provided interface, maybe it is
!!        useful to accept only procedure pointers, instead of procedures
!!
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
    !! All failure handlers set from fortran codes need to have this interface
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
    !! @class node used for storing the failure handlers
    type :: multio_fort_failure_info_node
        integer(c_int) :: id = 0
        integer(int64) :: context = 0
        procedure(failure_handler_t), nopass, pointer :: handler_fn => null()
        type(multio_fort_failure_info_node),  pointer :: next => null()
    end type


    !>
    !! @class list of failure handlers
    type :: multio_fort_failure_info_list

        ! Default visibility of the members
        private

        integer(c_int) :: lastId = 0
        integer :: count = 0
        type(multio_fort_failure_info_node), pointer :: head => null()
        type(multio_fort_failure_info_node), pointer :: tail => null()

    contains

        ! Lis management
        procedure, public, pass :: callHandler => multio_fort_failure_call
        procedure, public, pass :: add         => multio_fort_failure_add
        procedure, public, pass :: remove      => multio_fort_failure_remove

        ! Address of the procedure needed to call an error handler
        procedure, public, pass :: c_wrapper   => multio_fort_failure_wrapper_addr

    end type


    !> Lis of handlers
    type(multio_fort_failure_info_list), save :: failure_info_list


    ! Whitelist of public symbols
    public :: failure_info_list

    public :: failure_handler_t
    public :: multio_failure_info

contains

    !>
    !! @brief return the pointer to the c wrapper that need to be called form c
    !!
    !! @param [in] ffi - failure handlers list
    !!
    !! @return c address of the wrapper
    !!
    function multio_fort_failure_wrapper_addr( ffi ) result(c_addr)
        use, intrinsic :: iso_c_binding, only: c_funptr
        use, intrinsic :: iso_c_binding, only: c_funloc
    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list), intent(inout) :: ffi
        ! Function Result
        type(c_funptr) :: c_addr
        ! Implementation
        c_addr = c_funloc(failure_handler_wrapper)
        ! Exit point
        return
    end function multio_fort_failure_wrapper_addr


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
        SearchLoop: do while(associated(node))
            if (node%id .eq. id) then
                !! Call the failure handler
                call node%handler_fn(node%context, err, info)
                node => null()
            else
                !! Go to the next node\
                node => node%next
            endif
        enddo SearchLoop

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
        use, intrinsic :: iso_fortran_env, only: int64

    implicit none
        ! Dummy arguments
        class(multio_fort_failure_info_list),  intent(inout) :: ffi
        procedure(failure_handler_t)                         :: handler_fn
        integer(int64),                        intent(in)    :: context
        ! Function result
        type(c_ptr) :: new_id_loc
        ! Local variables
        class(multio_fort_failure_info_node), pointer :: new_node

        ! Update the counters in the list
        ffi%lastId = ffi%lastId + 1
        ffi%count = ffi%count + 1

        ! Allocate the new node
        allocate(new_node)
        new_node%id = ffi%lastId
        new_node%handler_fn => handler_fn
        new_node%context = context

        ! Get the c_pointer of the id of the current node
        new_id_loc = c_loc(new_node%id)

        ! Special case: List empty
        if(.not. associated(ffi%head)) then
            ffi%head => new_node
        end if

        ! Add the node to the end of the list
        if(associated(ffi%tail)) then
            ffi%tail%next => new_node
        endif
        ffi%tail => new_node

        ! Exit point
        return
    end function multio_fort_failure_add


    !>
    !! @brief remove a failure handler from the list of failure handlers.
    !!        The failure handler to be removed is identified by "id".
    !!
    !! @param [in,out] ffi  - failure handlers list
    !! @param [in]     id   - index of the failure handler to be removed
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
        SearchLoop: do while (associated(node))

            if (node%id == id) then

                !! Node found: remove node from the list

                ! Generic case: the node to be removed is a genric node inside the list
                if(associated(node_prev)) then
                    node_prev%next => node%next
                end if

                ! Special case: the node to be removed is the head
                if(associated(ffi%head, node)) then
                    ffi%head => node%next
                endif

                ! Special case: the node to be removed is the tail
                if(associated(ffi%tail, node)) then
                    ffi%tail => node_prev
                endif

                ! Update node counter
                ffi%count = ffi%count - 1

                ! Free node
                deallocate(node)
                node => null()
            else

                !! Node not found: go to the next node
                node_prev => node
                node => node%next

            endif

        enddo SearchLoop
        ! Exit point
        return
    end subroutine multio_fort_failure_remove


    !>
    !! @brief wrap a failure handler.
    !!        This is the function called from c. Then linker symbol
    !!        associate to this function is defined through the
    !!        bind keyword.
    !!
    !! @param [in] c_idx   - Identifier of the failure handler to be called
    !! @param [in] c_error - Identifier of the error
    !! @param [in] c_info  - Opaque informations about the error
    !!
    !! @return error code
    !!
    subroutine failure_handler_wrapper(c_pidx, c_error, c_info) &
                bind(c, name='failure_handler_wrapper')

        use, intrinsic :: iso_c_binding, only: c_ptr
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_f_pointer

    implicit none
        ! Dummy arguments
        type(c_ptr),    value, intent(in) :: c_pidx
        integer(c_int), value, intent(in) :: c_error
        type(c_ptr),    value, intent(in) :: c_info
        ! Local variables
        type(multio_failure_info) :: finfo
        integer(c_int), pointer :: c_idx
        integer :: err
        ! Convert c input data to fortran data
        call c_f_pointer( c_pidx, c_idx )
        finfo%impl = c_info
        err = int(c_error,kind(err))
        ! Call the fortran failure handler
        call failure_info_list%callHandler(c_idx, err, finfo)
        ! Exit point
        return
    end subroutine failure_handler_wrapper

end module multio_error_handling_mod