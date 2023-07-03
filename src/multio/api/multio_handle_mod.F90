module multio_handle_mod
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int 
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
        procedure, private, pass :: write_mask_float_1d  => multio_write_mask_float_1d
        procedure, private, pass :: write_mask_double_1d => multio_write_mask_double_1d
        procedure, private, pass :: write_mask_float_2d  => multio_write_mask_float_2d
        procedure, private, pass :: write_mask_double_2d => multio_write_mask_double_2d
        generic,   public        :: write_mask => write_mask_float_1d,  &
                                               &  write_mask_float_2d,  &
                                               &  write_mask_double_1d, &
                                               &  write_mask_double_2d

        ! Field handling
        procedure, private, pass :: write_field_buffer    => multio_write_field_buffer
        procedure, private, pass :: write_field_float_1d  => multio_write_field_float_1d
        procedure, private, pass :: write_field_double_1d => multio_write_field_double_1d
        procedure, private, pass :: write_field_float_2d  => multio_write_field_float_2d
        procedure, private, pass :: write_field_double_2d => multio_write_field_double_2d
        generic,   public        :: write_field => write_field_buffer,    &
                                                &  write_field_float_1d,  &
                                                &  write_field_float_2d,  &
                                                &  write_field_double_1d, & 
                                                &  write_field_double_2d

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
    implicit none
        ! Dummy arguments
        class(multio_handle),                intent(inout) :: handle
        class(multio_metadata),              intent(in)    :: metadata
        real(kind=sp), dimension(:), target, intent(in)    :: data
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
    implicit none
        ! Dummy arguments
        class(multio_handle),                  intent(inout) :: handle
        class(multio_metadata),                intent(in)    :: metadata
        real(kind=sp), dimension(:,:), target, intent(in)    :: data
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
    implicit none
        ! Dummy arguments
        class(multio_handle),                intent(inout) :: handle
        class(multio_metadata),              intent(in)    :: metadata
        real(kind=dp), dimension(:), target, intent(in)    :: data
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
    implicit none
        ! Dummy arguments
        class(multio_handle),                  intent(inout) :: handle
        class(multio_metadata),                intent(in)    :: metadata
        real(kind=dp), dimension(:,:), target, intent(in)    :: data
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
    !!
    function multio_write_field_float_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_handle),                intent(inout) :: handle
        class(multio_metadata),              intent(in)    :: metadata
        real(kind=sp), dimension(:), target, intent(in)    :: data
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
    !!
    function multio_write_field_float_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_handle),                  intent(inout) :: handle
        class(multio_metadata),                intent(in)    :: metadata
        real(kind=sp), dimension(:,:), target, intent(in)    :: data
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
    !!
    function multio_write_field_double_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_handle),                intent(inout) :: handle
        class(multio_metadata),              intent(in)    :: metadata
        real(kind=dp), dimension(:), target, intent(in)    :: data
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
    !!
    function multio_write_field_double_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_handle),                  intent(inout) :: handle
        class(multio_metadata),                intent(in)    :: metadata
        real(kind=dp), dimension(:,:), target, intent(in)    :: data
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

end module multio_handle_mod