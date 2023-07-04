module multio_handle_mod
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_null_ptr 
implicit none

    ! Default symbols visibility
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_handle object
    type :: multio_handle
        private
        type(c_ptr)             :: impl       =  c_null_ptr
        integer(c_int), pointer :: failure_id => null()
    contains

        ! General management
        procedure, private, pass :: new_handle           => multio_new_handle
        procedure, private, pass :: new_handle_default   => multio_new_handle_default
        procedure, public,  pass :: delete               => multio_delete_handle
        procedure, public,  pass :: set_failure_handler  => multio_handle_set_failure_handler
        generic,   public        :: new => new_handle,        &
                                        &  new_handle_default

        ! Connection handling
        procedure, public,  pass :: open_connections     => multio_open_connections
        procedure, public,  pass :: close_connections    => multio_close_connections

        ! Signalling
        procedure, public,  pass :: flush                => multio_flush
        procedure, public,  pass :: notify               => multio_notify

        ! Domain handling
        procedure, public,  pass :: write_domain         => multio_write_domain

        ! Mask handling
        procedure, private, pass :: write_field_buffer   => multio_write_field_buffer
        procedure, private, pass :: write_mask_float_1d  => multio_write_mask_float_1d
        procedure, private, pass :: write_mask_double_1d => multio_write_mask_double_1d
        procedure, private, pass :: write_mask_float_2d  => multio_write_mask_float_2d
        procedure, private, pass :: write_mask_double_2d => multio_write_mask_double_2d
        generic,   public        :: write_mask => write_field_buffer,   &
                                               &  write_mask_float_1d,  &
                                               &  write_mask_float_2d,  &
                                               &  write_mask_double_1d, &
                                               &  write_mask_double_2d

        ! Field handling
        ! procedure, private, pass :: write_field_buffer    => multio_write_field_buffer
        procedure, private, pass :: write_field_float_1d  => multio_write_field_float_1d
        procedure, private, pass :: write_field_double_1d => multio_write_field_double_1d
        procedure, private, pass :: write_field_float_2d  => multio_write_field_float_2d
        procedure, private, pass :: write_field_double_2d => multio_write_field_double_2d
        generic,   public        :: write_field => write_field_float_1d,  &
                                                &  write_field_float_2d,  &
                                                &  write_field_double_1d, & 
                                                &  write_field_double_2d!, &
                                                !   write_field_buffer

        ! Utils
        procedure, public,  pass :: field_accepted => multio_field_accepted
        ! procedure :: field_is_active => multio_field_is_active
        ! procedure :: category_is_fully_active => multio_category_is_fully_active
    end type ! multio_handle

    ! Public symbols whitelist
    public :: multio_handle

contains

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
    !! @see multio_new_handle_default
    !!
    function multio_new_handle(handle, cc) result(err)
        use :: multio_configuration_mod, only: multio_configuration
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_handle),        intent(inout) :: handle
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
        c_err = c_multio_new_handle(handle%impl, cc%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_new_handle

    !>
    !! @brief create a default multio handle
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_new_handle
    !!
    function multio_new_handle_default(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_handle), intent(inout) :: handle
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
    end function multio_new_handle_default


    !>
    !! @brief delete a multio handle. This function remove all 
    !!        the allocated memory and removes any installed error
    !!        handler
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_new_handle
    !! @see multio_new_handle_default
    !!
    function multio_delete_handle(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_utils_mod, only: failure_info_list
    implicit none
        ! Dummy arguments
        class(multio_handle), intent(inout) :: handle
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
        c_err = c_multio_delete_handle(handle%impl)
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
    end function multio_delete_handle


    !>
    !! @brief set the failure handler to multio
    !!
    !! @param [in,out] handle - handler function to be called as failure handler
    !! @param [in,out] handle - configuration context
    !!
    !! @return error code 
    !!
    function multio_handle_set_failure_handler( handle, handler, context) result(err)
        use :: multio_utils_mod, only: int64
        use :: iso_c_binding, only: c_int
        use :: iso_c_binding, only: c_null_ptr
        use :: iso_c_binding, only: c_funloc
        use :: multio_utils_mod, only: handler
        use :: multio_utils_mod, only: multio_failure_info
        use :: multio_utils_mod, only: failure_handler_t
        use :: multio_utils_mod, only: failure_info_list, failure_handler_wrapper 
    implicit none
        ! Dummy arguments
        class(multio_handle), intent(inout) :: handle
        integer(int64),       intent(inout) :: context
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

        if(associated(handle%failure_id)) then
            old_id => handle%failure_id
        end if

        new_id_loc = failure_info_list%add(handler_fn, context)
        call c_f_pointer(new_id_loc, handle%failure_id)
        c_err = c_multio_handle_set_failure_handler(handle%impl, c_funloc(failure_handler_wrapper), new_id_loc)

        if(associated(old_id)) then
            call failure_info_list%remove(old_id)
        end if
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point        
        return
    end function multio_handle_set_failure_handler


    !>
    !! @brief open a new connection
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_close_connections
    !!
    function multio_open_connections(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_handle), intent(inout) :: handle
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
        c_err = c_multio_open_connections(handle%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_open_connections


    !>
    !! @brief close any existent connection
    !!
    !! @param [in,out] handle - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_open_connections
    !!
    function multio_close_connections(handle) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_handle), intent(inout) :: handle
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
        c_err = c_multio_close_connections(handle%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_close_connections


    !>
    !! @brief send a flush command through the multio planes
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the flush command
    !!
    !! @return error code 
    !!
    !! @see multio_notify
    !!
    function multio_flush(handle, metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(in)    :: metadata
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_flush(handle, metadata) result(err) &
                bind(c, name='multio_flush')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_flush
        end interface
        ! Implementation
        c_err = c_multio_flush(handle%impl, metadata%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_flush


    !>
    !! @brief notify some event through the multio planes
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the notification
    !!
    !! @return error code 
    !!
    !! @see multio_flush
    !!
    function multio_notify(handle, metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(in)    :: metadata
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_notify(handle, metadata) result(err) &
                    bind(c, name='multio_notify')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_notify
        end interface
        ! Implementation
        c_err = c_multio_notify(handle%impl, metadata%impl)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_notify


    !>
    !! @brief send a domain information to multio
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the domain
    !! @param [in]     data     - domain data
    !!
    !! @return error code 
    !!
    !! @see multio_notify
    !!
    !! @todo implement for different kinds
    function multio_write_domain(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),           intent(inout) :: handle
        class(multio_metadata),         intent(in)    :: metadata
        integer, dimension(:),  target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_domain(handle, metadata, data, size) result(err) &
                    bind(c, name='multio_write_domain')
                    use, intrinsic :: iso_c_binding, only: c_ptr
                    use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_domain
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err  = c_multio_write_domain(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_domain


    !>
    !! @brief send a float one dimension mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code 
    !!
    !! @see multio_write_mask_float_2d
    !! @see multio_write_mask_double_1d
    !! @see multio_write_mask_double_2d
    !!
    function multio_write_mask_float_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(in)    :: metadata
        real(kind=c_float), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: handle
                type(c_ptr), value, intent(in) :: metadata
                type(c_ptr), value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_float(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_mask_float_1d


    !>
    !! @brief send a float two dimensions mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code 
    !!
    !! @see multio_write_mask_float_1d
    !! @see multio_write_mask_double_1d
    !! @see multio_write_mask_double_2d
    !!
    function multio_write_mask_float_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(in)    :: metadata
        real(kind=c_float), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_float(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_mask_float_2d


    !>
    !! @brief send a double one dimension mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code 
    !!
    !! @see multio_write_mask_float_1d
    !! @see multio_write_mask_float_2d
    !! @see multio_write_mask_double_2d
    !!
    function multio_write_mask_double_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(in)    :: metadata
        real(kind=c_double), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_double(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_mask_double_1d


    !>
    !! @brief send a double two dimensions mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code 
    !!
    !! @see multio_write_mask_float_1d
    !! @see multio_write_mask_float_2d
    !! @see multio_write_mask_double_1d
    !!
    function multio_write_mask_double_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(in)    :: metadata
        real(kind=c_double), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_mask_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_mask_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_double(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_mask_double_2d


    !>
    !! @brief send a float one dimension field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code 
    !!
    !! @see multio_write_field_float_2d
    !! @see multio_write_field_double_1d
    !! @see multio_write_field_double_2d
    !! @see multio_write_field_buffer
    !!
    function multio_write_field_float_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(in)    :: metadata
        real(kind=c_float), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_float(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_field_float_1d


    !>
    !! @brief send a float two dimensions field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code 
    !!
    !! @see multio_write_field_float_1d
    !! @see multio_write_field_double_1d
    !! @see multio_write_field_double_2d
    !! @see multio_write_field_buffer
    !!
    function multio_write_field_float_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(in)    :: metadata
        real(kind=c_float), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_float(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_float(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_field_float_2d


    !>
    !! @brief send a double one dimension field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code 
    !!
    !! @see multio_write_field_float_1d
    !! @see multio_write_field_float_2d
    !! @see multio_write_field_double_2d
    !! @see multio_write_field_buffer
    !!
    function multio_write_field_double_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(in)    :: metadata
        real(kind=c_double), dimension(:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_double(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_field_double_1d


    !>
    !! @brief send a double two dimensions field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code 
    !!
    !! @see multio_write_field_float_1d
    !! @see multio_write_field_float_2d
    !! @see multio_write_field_double_1d
    !! @see multio_write_field_buffer
    !!
    function multio_write_field_double_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(in)    :: metadata
        real(kind=c_double), dimension(:,:), target, intent(in)    :: data
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: c_size
        ! Private interface to the c API
        interface
            function c_multio_write_field_double(handle, metadata, data, size) result(err) &
                bind(c, name='multio_write_field_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_field_double
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_field_double(handle%impl, metadata%impl, c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_field_double_2d


    !>
    !! @brief send a bufferend field (data are already packed in a eckit::buffer)
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code 
    !!
    !! @see multio_write_field_float_1d
    !! @see multio_write_field_float_2d
    !! @see multio_write_field_double_1d
    !! @see multio_write_field_double_1d
    !!
    function multio_write_field_buffer(handle, metadata, data) result(err)
        use :: multio_metadata_mod, only: multio_metadata
        use :: multio_data_mod,     only: multio_data
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_handle),       intent(inout) :: handle
        class(multio_metadata),     intent(in)    :: metadata
        class(multio_data), target, intent(in)    :: data
        ! Function rsult
        integer :: err
        ! Local variables
        integer(kind=c_int) c_err
        integer(kind=c_int) c_byte_size
        ! Private interface
        interface
            function c_multio_write_field_buffer(handle, metadata, data, byte_size) result(err) &
                bind(c, name='multio_write_field_buffer')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in) :: handle
                type(c_ptr),         value, intent(in) :: metadata
                type(c_ptr),         value, intent(in) :: data
                integer(kind=c_int), value, intent(in) :: byte_size
                integer(c_int) :: err
            end function c_multio_write_field_buffer
        end interface
        ! Implementation
        c_byte_size = int(data%byte_size(),c_int)
        c_err = c_multio_write_field_buffer(handle%impl, metadata%impl, c_loc(data), c_byte_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_write_field_buffer


    !>
    !! @brief set field accepted flag
    !!
    !! @param [in,out] handle    - handle passed object pointer
    !! @param [in]     metadata  - metadta to be sent with the field
    !! @param [in]     set_value - flag 
    !!
    !! @return error code 
    !!
    !!
    function multio_field_accepted(handle, metadata, set_value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(in)    :: metadata
        logical,                intent(out)   :: set_value
        ! Function result
        integer :: err
        ! Local variables
        character(:), allocatable, target :: nullified_field
        integer(kind=c_int) :: c_err
        logical(kind=c_bool) :: c_set_value
        ! Private interface to the c API
        interface
            function c_multio_field_accepted(handle, metadata, set_value) result(err) &
                bind(c, name='multio_field_accepted')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_bool
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: handle
                type(c_ptr), intent(in), value :: metadata
                logical(c_bool), intent(out) :: set_value
                integer(c_int) :: err
            end function c_multio_field_accepted
        end interface
        ! Implementation
        c_set_value = logical(set_value,c_bool)
        c_err = c_multio_field_accepted(handle%impl, metadata%impl, c_set_value)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_field_accepted

end module multio_handle_mod