module multio_metadata_mod
    use, intrinsic :: iso_C_binding, only: c_ptr
implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_metadata object
    type :: multio_metadata
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure, public, pass :: new          => multio_new_metadata
        procedure, public, pass :: delete       => multio_delete_metadata
        procedure, public, pass :: set_int      => multio_metadata_set_int
        procedure, public, pass :: set_long     => multio_metadata_set_long
        procedure, public, pass :: set_longlong => multio_metadata_set_longlong
        procedure, public, pass :: set_string   => multio_metadata_set_string
        procedure, public, pass :: set_bool     => multio_metadata_set_bool
        procedure, public, pass :: set_float    => multio_metadata_set_float
        procedure, public, pass :: set_double   => multio_metadata_set_double
        ! Not possible to overload integers with the same dimensions.
        ! Depending on the architecture long can be 4 or 8 bytes. 
        ! int is usually 4 bytes and long_long is usually 8 bytes
        !> @todo probably a better approach would be to use c_int8_t, c_int16_t, ..., c_int64_t
        generic, public :: set => set_int, set_longlong, set_string, set_bool, set_float, set_double
    end type ! multio_metadata

    ! Public symbols whitelist
    public :: multio_metadata

contains 

    !>
    !! @brief crate a new metadata object
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_delete_metadata
    !!
    function multio_new_metadata(metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_metadata(metadata) result(err) &
                    bind(c, name='multio_new_metadata')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: metadata
                integer(c_int) :: err
            end function c_multio_new_metadata
        end interface
        ! Call the c API
        c_err = c_multio_new_metadata(metadata%impl)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_new_metadata


    !>
    !! @brief delete a metadata object
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !!
    !! @return error code 
    !!
    !! @see multio_new_metadata
    !!
    function multio_delete_metadata(metadata) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_metadata(metadata) result(err) &
                    bind(c, name='multio_delete_metadata')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: metadata
                integer(c_int) :: err
            end function c_multio_delete_metadata
        end interface
        ! Call the c API
        c_err = c_multio_delete_metadata(metadata%impl)
        ! Output cast and cleanup
        metadata%impl = c_null_ptr
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_delete_metadata


    !>
    !! @brief set a new k-v pair with integer value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_bool
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_int(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=c_int),    intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_int(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_int')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                integer(c_int), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_int
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_int(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_int


    !>
    !! @brief set a new k-v pair with long integer value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_bool
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_long(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        integer(kind=c_long),   intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_long(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_long')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_long
            implicit none
                type(c_ptr),     value, intent(in) :: metadata
                type(c_ptr),     value, intent(in) :: key
                integer(c_long), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_long
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_long(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_long


    !>
    !! @brief set a new k-v pair with long_long integer value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_bool
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_longlong(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata),    intent(inout) :: metadata
        character(len=*),          intent(in)    :: key
        integer(kind=c_long_long), intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_longlong(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_longlong')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_long_long
            implicit none
                type(c_ptr),          value, intent(in) :: metadata
                type(c_ptr),          value, intent(in) :: key
                integer(c_long_long), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_longlong
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_longlong(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_longlong


    !>
    !! @brief set a new k-v pair with float value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_bool
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_float(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_float
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        real(kind=c_float),     intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_float(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_float')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_float
            implicit none
                type(c_ptr),   value, intent(in) :: metadata
                type(c_ptr),   value, intent(in) :: key
                real(c_float), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_float
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_float(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_float


    !>
    !! @brief set a new k-v pair with double value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_bool
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_double(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        real(kind=c_double),    intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_double(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_double')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: metadata
                type(c_ptr),    value, intent(in) :: key
                real(c_double), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_double
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_double(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_double


    !>
    !! @brief set a new k-v pair with bool value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_string
    !!
    function multio_metadata_set_bool(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        logical,                intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        logical(c_bool) :: c_value
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_bool(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_bool')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_bool
            implicit none
                type(c_ptr),     value, intent(in) :: metadata
                type(c_ptr),     value, intent(in) :: key
                logical(c_bool), value, intent(in) :: value
                integer(c_int) :: err
            end function c_multio_metadata_set_bool
        end interface
        ! Initialization and allocation
        c_value = logical(value,c_value)
        nullified_key = trim(key) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_bool(metadata%impl, c_loc(nullified_key), value)
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_bool


    !>
    !! @brief set a new k-v pair with string value
    !!
    !! @param [in,out] metadata - handle passed object pointer
    !! @param [in]     key      - key to be set
    !! @param [in]     value    - value to be set
    !!
    !! @return error code 
    !!
    !! @see multio_metadata_set_int
    !! @see multio_metadata_set_long
    !! @see multio_metadata_set_longlong
    !! @see multio_metadata_set_float
    !! @see multio_metadata_set_double
    !! @see multio_metadata_set_bool
    !!
    function multio_metadata_set_string(metadata, key, value) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
    implicit none
        ! Dummy arguments
        class(multio_metadata), intent(inout) :: metadata
        character(len=*),       intent(in)    :: key
        character(len=*),       intent(in)    :: value
        ! Function result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_key
        character(:,kind=c_char), allocatable, target :: nullified_value
        ! Private interface to the c API
        interface
            function c_multio_metadata_set_string(metadata, key, value) result(err) &
                    bind(c, name='multio_metadata_set_string')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: metadata
                type(c_ptr), value, intent(in) :: key
                type(c_ptr), value, intent(in) :: value
                integer(c_int) :: err
            end function
        end interface
        ! Initialization and allocation
        nullified_key = trim(key) // c_null_char
        nullified_value = trim(value) // c_null_char
        ! Call the c API
        c_err = c_multio_metadata_set_string(metadata%impl, c_loc(nullified_key), c_loc(nullified_value))
        ! Output cast and cleanup
        if (allocated(nullified_key)) deallocate(nullified_key)
        if (allocated(nullified_value)) deallocate(nullified_value)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_metadata_set_string


module multio_metadata_mod