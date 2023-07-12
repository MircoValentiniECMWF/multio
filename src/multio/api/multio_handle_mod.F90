!> @file
!!
!! @brief Definition of the main functionalities of a multio_handle
!!
!! This module defines all the overloaded methods needed to write to
!! multio fields, masks and domains
!!
!! @note This module is separated from the "base" multio handle in
!!       order to avoid circular deps.
!!

module multio_handle_mod

    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use :: multio_base_handle_mod,   only: multio_base_handle

implicit none

    ! Default symbols visibility
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_handle object
    type, extends(multio_base_handle) :: multio_handle

        !! Deafult visibility of the members
        private

    contains

        ! Signalling
        procedure, public,  pass :: flush                => multio_handle_flush
        procedure, public,  pass :: notify               => multio_handle_notify

        ! Domain handling
        procedure, public,  pass :: write_domain         => multio_handle_write_domain

        ! Mask handling
        procedure, private, pass :: write_mask_float_1d  => multio_handle_write_mask_float_1d
        procedure, private, pass :: write_mask_double_1d => multio_handle_write_mask_double_1d
        procedure, private, pass :: write_mask_float_2d  => multio_handle_write_mask_float_2d
        procedure, private, pass :: write_mask_double_2d => multio_handle_write_mask_double_2d
        generic,   public        :: write_mask => write_mask_float_1d,  &
                                               &  write_mask_float_2d,  &
                                               &  write_mask_double_1d, &
                                               &  write_mask_double_2d

        ! Field handling
        procedure, private, pass :: write_field_buffer    => multio_handle_write_field_buffer
        procedure, private, pass :: write_field_float_1d  => multio_handle_write_field_float_1d
        procedure, private, pass :: write_field_double_1d => multio_handle_write_field_double_1d
        procedure, private, pass :: write_field_float_2d  => multio_handle_write_field_float_2d
        procedure, private, pass :: write_field_double_2d => multio_handle_write_field_double_2d
        generic,   public        :: write_field => write_field_float_1d,  &
                                                &  write_field_float_2d,  &
                                                &  write_field_double_1d, &
                                                &  write_field_double_2d, &
                                                   write_field_buffer

        ! Utils
        procedure, public,  pass :: field_accepted => multio_handle_field_accepted

    end type ! multio_handle

    ! Public symbols whitelist
    public :: multio_handle

contains

    !>
    !! @brief send a flush command through the multio planes
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the flush command
    !!
    !! @return error code
    !!
    !! @see multio_handle_notify
    !!
    function multio_handle_flush(handle, metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
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
        c_err = c_multio_flush(handle%c_ptr(), metadata%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_flush


    !>
    !! @brief notify some event through the multio planes
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the notification
    !!
    !! @return error code
    !!
    !! @see multio_handle_flush
    !!
    function multio_handle_notify(handle, metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
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
        c_err = c_multio_notify(handle%c_ptr(), metadata%c_ptr())
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_notify


    !>
    !! @brief send a domain information to multio
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the domain
    !! @param [in]     data     - domain data
    !!
    !! @return error code
    !!
    !! @todo implement for different kinds
    function multio_handle_write_domain(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),           intent(inout) :: handle
        class(multio_metadata),         intent(inout) :: metadata
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
        c_err  = c_multio_write_domain(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_domain


    !>
    !! @brief send a float one dimension mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_1d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_float_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(inout) :: metadata
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
                type(c_ptr),    value, intent(in) :: handle
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: data
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_write_mask_float
        end interface
        ! Implementation
        c_size = int(size(data),c_int)
        c_err = c_multio_write_mask_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_mask_float_1d


    !>
    !! @brief send a float two dimensions mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_double_1d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_float_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(inout) :: metadata
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
        c_err = c_multio_write_mask_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_mask_float_2d


    !>
    !! @brief send a double one dimension mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_2d
    !!
    function multio_handle_write_mask_double_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(inout) :: metadata
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
        c_err = c_multio_write_mask_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_mask_double_1d


    !>
    !! @brief send a double two dimensions mask
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the mask
    !! @param [in]     data     - mask data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_mask_float_1d
    !! @see multio_handle_write_mask_float_2d
    !! @see multio_handle_write_mask_double_1d
    !!
    function multio_handle_write_mask_double_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(inout) :: metadata
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
        c_err = c_multio_write_mask_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_mask_double_2d


    !>
    !! @brief send a float one dimension field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_float_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                     intent(inout) :: handle
        class(multio_metadata),                   intent(inout) :: metadata
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
        c_err = c_multio_write_field_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_field_float_1d


    !>
    !! @brief send a float two dimensions field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_float_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                       intent(inout) :: handle
        class(multio_metadata),                     intent(inout) :: metadata
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
        c_err = c_multio_write_field_float(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_field_float_2d


    !>
    !! @brief send a double one dimension field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_2d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_double_1d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                      intent(inout) :: handle
        class(multio_metadata),                    intent(inout) :: metadata
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
        c_err = c_multio_write_field_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_field_double_1d


    !>
    !! @brief send a double two dimensions field
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_buffer
    !!
    function multio_handle_write_field_double_2d(handle, metadata, data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_double
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),                        intent(inout) :: handle
        class(multio_metadata),                      intent(inout) :: metadata
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
        c_err = c_multio_write_field_double(handle%c_ptr(), metadata%c_ptr(), c_loc(data), c_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_field_double_2d


    !>
    !! @brief send a buffered field (data are already packed in a eckit::buffer)
    !!
    !! @param [in,out] handle   - handle passed object pointer
    !! @param [in]     metadata - metadta to be sent with the field
    !! @param [in]     data     - field data
    !!
    !! @return error code
    !!
    !! @see multio_handle_write_field_float_1d
    !! @see multio_handle_write_field_float_2d
    !! @see multio_handle_write_field_double_1d
    !! @see multio_handle_write_field_double_1d
    !!
    function multio_handle_write_field_buffer(handle, metadata, data) result(err)
        use :: multio_metadata_mod, only: multio_metadata
        use :: multio_data_mod,     only: multio_data
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_handle),       intent(inout) :: handle
        class(multio_metadata),     intent(inout) :: metadata
        class(multio_data), target, intent(inout) :: data
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
        c_err = c_multio_write_field_buffer(handle%c_ptr(), metadata%c_ptr(), data%c_ptr(), c_byte_size)
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_write_field_buffer


    !>
    !! @brief set field accepted flag
    !!
    !! @param [in,out] handle    - handle passed object pointer
    !! @param [in]     metadata  - metadta to be sent with the field
    !! @param [in]     set_value - flag
    !!
    !! @return error code
    !!
    function multio_handle_field_accepted(handle, metadata, set_value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        use :: multio_metadata_mod, only: multio_metadata
    implicit none
        ! Dummy arguments
        class(multio_handle),   intent(inout) :: handle
        class(multio_metadata), intent(inout) :: metadata
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
                type(c_ptr), value, intent(in)  :: handle
                type(c_ptr), value, intent(in)  :: metadata
                logical(c_bool),    intent(out) :: set_value
                integer(c_int) :: err
            end function c_multio_field_accepted
        end interface
        ! Implementation
        c_err = c_multio_field_accepted(handle%c_ptr(), metadata%c_ptr(), c_set_value)
        ! Setting return values
        set_value = logical(c_set_value,kind(set_value))
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_handle_field_accepted

end module multio_handle_mod