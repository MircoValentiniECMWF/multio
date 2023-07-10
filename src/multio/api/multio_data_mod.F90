module multio_data_mod

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_int

implicit none

    !> Default visibility of the module
    private

    !>
    !! @class datatype used to wrap eckit::buffer
    type :: multio_data

        !! Class visibility
        private

        type(c_ptr)          :: impl  = c_null_ptr
        integer(kind=c_int)  :: byte_size_ = 0_c_int

    contains

        procedure :: new    => multio_data_new
        procedure :: delete => multio_data_delete
        procedure :: c_ptr  => multio_data_c_ptr

        procedure :: resize    => multio_data_resize
        procedure :: size      => multio_data_size
        procedure :: zero      => multio_data_zero
        procedure :: byte_size => multio_data_byte_size

        procedure :: set_float_scalar  => multio_data_set_float_scalar
        procedure :: set_double_scalar => multio_data_set_double_scalar
        procedure :: set_float_chunk   => multio_data_set_float_chunk
        procedure :: set_double_chunk  => multio_data_set_double_chunk
        generic   :: set => set_float_scalar, set_double_scalar, set_float_chunk, set_double_chunk

    end type ! multio_data

    !> Public symbols whitelist
    public :: multio_data

contains

    !>
    !! @brief extract the c pointer of the data object
    !!
    !! @param [in,out] data - handle passed object pointer
    !!
    !! @return c pointer to the data object
    !!
    function multio_data_c_ptr( data ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
        ! Dummy arguments
        class(multio_data), target, intent(inout) :: data
        ! Function result
        type(c_ptr) :: loc
        ! Implementation
        loc = data%impl
        ! Exit point
        return
    end function multio_data_c_ptr


    !>
    !! @brief crate a new data object
    !!
    !! @param [in,out] data     - handle passed object pointer
    !! @param [in]     byte_sze - byte size of the datatype used
    !!
    !! @return error code
    !!
    !! @see multio_data_delete
    !!
    function multio_data_new(data, handle, byte_size) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_base_handle_mod, only: multio_base_handle
    implicit none
        ! Dummy arguments
        class(multio_data),        intent(inout) :: data
        class(multio_base_handle), intent(inout) :: handle
        integer,                   intent(in)    :: byte_size
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_new(data, handle) result(err) &
                bind(c, name='multio_data_new')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),        intent(out) :: data
                type(c_ptr), value, intent(in)  :: handle
                integer(c_int) :: err
            end function c_multio_data_new
        end interface
        ! Call the c API
        data%byte_size_ = int(byte_size,c_int)
        c_err = c_multio_data_new(data%impl, handle%c_ptr())
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_new


    !>
    !! @brief delete a data object
    !!
    !! @param [in,out] data - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_data_new
    !!
    function multio_data_delete(data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_delete(data) result(err) &
                bind(c, name='multio_data_delete')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                integer(c_int) :: err
            end function c_multio_data_delete
        end interface
        ! Call the c API
        c_err = c_multio_data_delete(data%c_ptr())
        data%impl = c_null_ptr
        data%byte_size_ = 0_c_int
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_delete


    !>
    !! @brief reset to zero the memory of the object
    !!
    !! @param [in,out] data - handle passed object pointer
    !!
    !! @return error code
    !!
    function multio_data_zero(data) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_zero(data) result(err) &
                bind(c, name='multio_data_zero')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                integer(c_int) :: err
            end function c_multio_data_zero
        end interface
        ! Call the c API
        c_err = c_multio_data_zero(data%c_ptr())
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_zero


    !>
    !! @brief resize a data object
    !!
    !! @param [in,out] data     - handle passed object pointer
    !! @param [in]     new_size - new size of the object to be set
    !!
    !! @return error code
    !!
    !!
    function multio_data_resize(data,new_size) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        integer,            intent(in)    :: new_size
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_resize(data,new_size) result(err) &
                bind(c, name='multio_data_resize')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in) :: data
                integer(kind=c_int), value, intent(in) :: new_size
                integer(c_int) :: err
            end function c_multio_data_resize
        end interface
        ! Call the c API
        c_size = int(new_size,c_int)
        c_err = c_multio_data_resize(data%c_ptr(), c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_resize


    !>
    !! @brief get the size of a data object
    !!
    !! @param [in,out] data - handle passed object pointer
    !! @param [out]    size - size of the object
    !!
    !! @return error code
    !!
    function multio_data_size(data,size) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_data), intent(inout) :: data
        integer,intent(out) :: size
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        integer(kind=c_int), target :: c_size
        ! Private interface to the c API
        interface
            function c_multio_data_size(data,size) result(err) &
                bind(c, name='multio_data_size')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: data
                type(c_ptr), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_size
        end interface
        ! Call the c API
        c_err = c_multio_data_size(data%c_ptr(), c_loc(c_size))
        ! Output cast and cleanup
        size = int(c_size,kind(size))
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_size


    !>
    !! @brief get the byte_size of the datatype used
    !!        i.e. 4 -> float, 8 -> double
    !!
    !! @param [in] data - handle passed object pointer
    !!
    !! @return byte_size of the datatype
    !!
    function multio_data_byte_size(data) result(byte_size)
    implicit none
        ! Dummy arguments
        class(multio_data), intent(in) :: data
        ! Function Result
        integer :: byte_size
        ! Implementation
        byte_size = int(data%byte_size_,kind(byte_size))
        ! Exit point
        return
    end function multio_data_byte_size


    !>
    !! @brief set a float element in a predefined position
    !!
    !! @param [in,out] data  - handle passed object pointer
    !! @param [in]     value - value to be set
    !! @param [in]     pos   - position where to set the value
    !!
    !! @return error code
    !!
    function multio_data_set_float_scalar(data, value, pos) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_float
    implicit none
        ! Dummy arguments
        class(multio_data),         intent(inout) :: data
        real(kind=c_float), target, intent(in)    :: value
        integer(c_int),             intent(in)    :: pos
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_float_scalar(data, value, pos) result(err) &
                bind(c, name='multio_data_set_float_scalar')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int) :: err
            end function c_multio_data_set_float_scalar
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_err = c_multio_data_set_float_scalar(data%c_ptr(), c_loc(value), pos)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_set_float_scalar


   !>
    !! @brief set a float chunk of elements in a predefined position
    !!
    !! @param [in,out] data  - handle passed object pointer
    !! @param [in]     value - value to be set
    !! @param [in]     pos   - start position where to set the value
    !! @param [in]     pos   - size of the chunk to be set
    !!
    !! @return error code
    !!
    function multio_data_set_float_chunk(data, value, pos, size) result(err)
        use, intrinsic :: iso_c_binding, only: c_float
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_data),                       intent(inout) :: data
        real(kind=c_float), dimension(:), target, intent(in)    :: value
        integer,                                  intent(in)    :: pos
        integer,                                  intent(in)    :: size
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_float_chunk(data, value, pos, size) result(err) &
                bind(c, name='multio_data_set_float_chunk')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_set_float_chunk
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_size = int(size,kind(c_size))
        c_err = c_multio_data_set_float_chunk(data%c_ptr(), c_loc(value), c_pos, c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_set_float_chunk


   !>
    !! @brief set a double element in a predefined position
    !!
    !! @param [in,out] data  - handle passed object pointer
    !! @param [in]     value - value to be set
    !! @param [in]     pos   - position where to set the value
    !!
    !! @return error code
    !!
    function multio_data_set_double_scalar(data, value, pos) result(err)
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_data),          intent(inout) :: data
        real(kind=c_double), target, intent(in)    :: value
        integer,                     intent(in)    :: pos
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_double_scalar(data, value, pos) result(err) &
                bind(c, name='multio_data_set_double_scalar')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int) :: err
            end function c_multio_data_set_double_scalar
        end interface
        ! Call the c API
        c_pos = int(pos,kind(c_pos))
        c_err = c_multio_data_set_double_scalar(data%c_ptr(), c_loc(value), c_pos)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_set_double_scalar


   !>
    !! @brief set a double chunk of elements in a predefined position
    !!
    !! @param [in,out] data  - handle passed object pointer
    !! @param [in]     value - value to be set
    !! @param [in]     pos   - start position where to set the value
    !! @param [in]     pos   - size of the chunk to be set
    !!
    !! @return error code
    !!
    function multio_data_set_double_chunk(data, value, pos, size) result(err)
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        ! Dummy arguments
        class(multio_data),                        intent(inout) :: data
        real(kind=c_double), dimension(:), target, intent(in)    :: value
        integer,                                   intent(in)    :: pos
        integer,                                   intent(in)    :: size
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_pos
        integer(kind=c_int) :: c_size
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_data_set_double_chunk(data, value, pos, size) result(err) &
                bind(c, name='multio_data_set_double_chunk')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: data
                type(c_ptr),    value, intent(in) :: value
                integer(c_int), value, intent(in) :: pos
                integer(c_int), value, intent(in) :: size
                integer(c_int) :: err
            end function c_multio_data_set_double_chunk
        end interface
        ! Call the c API
        c_pos  = int(pos,kind(c_pos))
        c_size = int(size,kind(c_size))
        c_err  = c_multio_data_set_double_chunk(data%c_ptr(), c_loc(value), c_pos, c_size)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_data_set_double_chunk

end module multio_data_mod