module multio_base_handle_mod
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_null_ptr
implicit none

    ! Default symbols visibility
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_base_handle object
    type :: multio_base_handle
        type(c_ptr) :: impl =  c_null_ptr
        integer(c_int), pointer :: failure_id => null()
    contains

        ! General management
        procedure, private, pass :: new_handle           => multio_base_handle_new
        procedure, private, pass :: new_handle_default   => multio_base_handle_new_default
        procedure, public,  pass :: delete               => multio_base_handle_delete
        procedure, public,  pass :: c_ptr                => multio_base_handle_c_ptr
        procedure, public,  pass :: set_failure_handler  => multio_base_handle_set_failure_handler
        generic,   public        :: new => new_handle,        &
                                        &  new_handle_default

        ! Connection handling
        procedure, public,  pass :: open_connections     => multio_base_handle_open_connections
        procedure, public,  pass :: close_connections    => multio_base_handle_close_connections

    end type ! multio_base_handle

    ! Public symbols whitelist
    public :: multio_base_handle

contains


    !>
    !! @brief extract the c pointer of the handle object
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return c pointer to the handle object
    !!
    function multio_base_handle_c_ptr( handle ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
        ! Dummy arguments
        class(multio_base_handle), target, intent(inout) :: handle
        ! Function result
        type(c_ptr) :: loc
        ! Implementation
        loc = handle%impl
        ! Exit point
        return
    end function multio_base_handle_c_ptr

    !>
    !! @brief create a new multio handle from  a multio configuration
    !!
    !! @param [in,out] handle - handle passed object pointer
    !! @param [in,out] cc     - pointer to the multio_configuration
    !!                          object used to configure the handle
    !!
    !! @return error code
    !!
    !! @see multio_configuration
    !! @see multio_base_handle_new_default
    !! @see multio_base_handle_delete
    !!
    function multio_base_handle_new(handle, cc) result(err)
        use :: multio_configuration_mod, only: multio_configuration
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_base_handle),        intent(inout) :: handle
        class(multio_configuration), intent(inout) :: cc
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_handle(handle, cc) result(err) &
                    bind(c, name='multio_new_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: handle
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int) :: err
            end function c_multio_new_handle
        end interface
        ! Implementation
        handle%failure_id => cc%failure_id
        cc%failure_id => null()
        c_err = c_multio_new_handle(handle%impl, cc%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_new


    !>
    !! @brief create a default multio handle
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_base_handle_new
    !! @see multio_base_handle_delete
    !!
    function multio_base_handle_new_default(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_handle_default(handle) result(err) &
                    bind(c, name='multio_new_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: handle
                integer(c_int) :: err
            end function c_multio_new_handle_default
        end interface
        ! implementation
        c_err = c_multio_new_handle_default(handle%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_new_default


    !>
    !! @brief delete a multio handle. This function remove all
    !!        the allocated memory and removes any installed error
    !!        handler
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_base_handle_new
    !! @see multio_base_handle_new_default
    !!
    function multio_base_handle_delete(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_error_handling_mod, only: failure_info_list
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
        ! Local variabels
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_handle(handle) result(err) &
                    bind(c, name='multio_delete_handle')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_delete_handle
        end interface
        ! Implementation
        c_err = c_multio_delete_handle(handle%c_ptr())
        handle%impl = c_null_ptr
        ! Remove failure handler
        if(associated(handle%failure_id)) then
           call failure_info_list%remove(handle%failure_id)
           handle%failure_id => null()
        end if
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_delete


    !>
    !! @brief set the failure handler to multio
    !!
    !! @param [in,out] handle - handler function to be called as failure handler
    !! @param [in,out] handle - configuration context
    !!
    !! @return error code
    !!
    function multio_base_handle_set_failure_handler( handle, handler, context) result(err)
        use :: multio_constants_mod, only: int64
        use :: iso_c_binding, only: c_int
        use :: iso_c_binding, only: c_null_ptr
        use :: iso_c_binding, only: c_funloc
        use :: iso_c_binding, only: c_f_pointer
        use :: multio_error_handling_mod, only: handler
        use :: multio_error_handling_mod, only: multio_failure_info
        use :: multio_error_handling_mod, only: failure_handler_t
        use :: multio_error_handling_mod, only: failure_info_list
        use :: multio_error_handling_mod, only: failure_handler_wrapper
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        integer(int64),            intent(inout) :: context
        ! Function result
        integer :: err
        ! Local variabels
        integer(kind=c_int) :: c_err
        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id => null()
        procedure(failure_handler_t), pointer :: handler_fn
        ! Private interface to the c API
        interface
            subroutine handler(ctx, err, info)
                import :: multio_failure_info
            implicit none
                integer, parameter :: int64 = selected_int_kind(15)
                integer(int64), intent(inout) :: ctx
                integer, intent(in) :: err
                class(multio_failure_info), intent(in) :: info
            end subroutine handler
            function c_multio_handle_set_failure_handler(mio, handler, context) result(err) &
                bind(c, name='multio_handle_set_failure_handler')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_funptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: mio
                type(c_funptr), value, intent(in) :: handler
                type(c_ptr),    value, intent(in) :: context
                integer(c_int) :: err
            end function c_multio_handle_set_failure_handler
        end interface
        ! Implementation
        handler_fn => handler
        ! Search for old error handlers
        if(associated(handle%failure_id)) then
            old_id => handle%failure_id
        endif
        ! Append the new error handler
        new_id_loc = failure_info_list%add(handler_fn, context)
        call c_f_pointer(new_id_loc, handle%failure_id)
        c_err = c_multio_handle_set_failure_handler(handle%c_ptr(), c_funloc(failure_handler_wrapper), new_id_loc)
        ! Revo the old error handler if exists
        if(associated(old_id)) then
            call failure_info_list%remove(old_id)
        endif
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_set_failure_handler


    !>
    !! @brief open a new connection
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_base_handle_close_connections
    !!
    function multio_base_handle_open_connections(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_open_connections(handle) result(err) &
                    bind(c, name='multio_open_connections')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_open_connections
        end interface
        ! implementation
        c_err = c_multio_open_connections(handle%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_open_connections


    !>
    !! @brief close any existent connection
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_base_handle_open_connections
    !!
    function multio_base_handle_close_connections(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_base_handle), intent(inout) :: handle
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_close_connections(handle) result(err) &
                    bind(c, name='multio_close_connections')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                integer(c_int) :: err
            end function c_multio_close_connections
        end interface
        ! implementation
        c_err = c_multio_close_connections(handle%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_base_handle_close_connections

end module multio_base_handle_mod