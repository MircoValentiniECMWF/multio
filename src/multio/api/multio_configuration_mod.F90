module multio_configuration_mod
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr
implicit none

    ! Default visibility of the module
    private

    !>
    !! @class datatype used to wrap the functionalities of a multio_configuration object
    type :: multio_configuration
        type(c_ptr) :: impl = c_null_ptr
        integer(c_int), pointer :: failure_id => null()
    contains
        procedure, public, pass :: new_default       => multio_new_configuration
        procedure, public, pass :: new_from_filename => multio_new_configuration_from_filename
        procedure, public, pass :: c_ptr             => multio_configuration_c_ptr
        generic,   public       :: new => new_default, new_from_filename

        procedure, public, pass :: delete                       => multio_delete_configuration
        procedure, public, pass :: set_failure_handler          => multio_conf_set_failure_handler

        procedure, public, pass :: set_path                     => multio_conf_set_path
        procedure, public, pass :: mpi_allow_world_default_comm => multio_conf_mpi_allow_world_default_comm
        procedure, public, pass :: mpi_parent_comm              => multio_conf_mpi_parent_comm
        procedure, public, pass :: mpi_return_client_comm       => multio_conf_mpi_return_client_comm
        procedure, public, pass :: mpi_return_server_comm       => multio_conf_mpi_return_server_comm
    end type ! multio_configuration

    ! Public symbols whitelist
    public :: multio_configuration

contains

    !>
    !! @brief extract the c pointer of the configuration object
    !!
    !! @param [in,out] cc - handle passed object pointer
    !!
    !! @return c pointer to the configuration object
    !!
    function multio_configuration_c_ptr( cc ) result(loc)
        use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
        ! Dummy arguments
        class(multio_configuration), target, intent(inout) :: cc
        ! Function result
        type(c_ptr) :: loc
        ! Implementation
        loc = cc%impl
        ! Exit point
        return
    end function multio_configuration_c_ptr

    !>
    !! @brief crate a new configuration object
    !!
    !! @param [in,out] cc - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_new_configuration_from_filename
    !! @see multio_delete_configuration
    !!
    function multio_new_configuration(cc) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(out) :: cc
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_new_configuration(cc) result(err) &
                bind(c, name='multio_new_configuration')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
            end function c_multio_new_configuration
        end interface
        ! Call the c API
        c_err = c_multio_new_configuration(cc%impl)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_new_configuration


    !>
    !! @brief crate a new configuration object from a file name
    !!
    !! @param [in,out] cc        - handle passed object pointer
    !! @param [in]     file_name - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_new_configuration
    !! @see multio_delete_configuration
    !!
    function multio_new_configuration_from_filename(cc, file_name) result(err)
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_null_char
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        character(len=*),            intent(in)    :: file_name
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_path
        ! Private interface to the c API
        interface
            function c_multio_new_configuration_from_filename(cc, file_name) result(err) &
                bind(c, name='multio_new_configuration_from_filename')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: file_name
                type(c_ptr), intent(out) :: cc
                integer(c_int) :: err
            end function c_multio_new_configuration_from_filename
        end interface
        ! Initialization and allocation
        nullified_path = trim(file_name) // c_null_char
        ! Call the c API
        c_err = c_multio_new_configuration_from_filename(cc%impl, c_loc(nullified_path))
        ! Output cast and cleanup
        if (allocated(nullified_path)) deallocate(nullified_path)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_new_configuration_from_filename


    !>
    !! @brief delete configuration object
    !!
    !! @param [in,out] cc - handle passed object pointer
    !!
    !! @return error code
    !!
    !! @see multio_new_configuration
    !! @see multio_new_configuration_from_filename
    !!
    function multio_delete_configuration(cc) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_delete_configuration(cc) result(err) &
                bind(c, name='multio_delete_configuration')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
            end function c_multio_delete_configuration
        end interface
        ! Call the c API
        c_err = c_multio_delete_configuration(cc%impl)
        cc%impl = c_null_ptr
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_delete_configuration


    !>
    !! @brief set the failure handler to multio
    !!
    !! @param [in,out] handle - handler function to be called as failure handler
    !! @param [in,out] handle - configuration context
    !!
    !! @return error code
    !!
    function multio_conf_set_failure_handler( cc, handler, context) result(err)
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        use, intrinsic :: iso_c_binding, only: c_funloc
        use, intrinsic :: iso_c_binding, only: c_f_pointer
        use :: multio_error_handling_mod, only: failure_handler_t
        use :: multio_error_handling_mod, only: failure_info_list
        use :: multio_error_handling_mod, only: failure_handler_wrapper
    implicit none
        ! Dummy arguments
        class(multio_configuration),           intent(inout) :: cc
        procedure(failure_handler_t), pointer, intent(in)    :: handler
        integer(int64),                        intent(inout) :: context
        ! Function result
        integer :: err
        ! Local variabels
        integer(kind=c_int) :: c_err
        type(c_ptr) :: new_id_loc
        integer(c_int), pointer :: old_id => null()
        ! Private interface to the c API
        interface
            function c_multio_config_set_failure_handler(mio, handler, context) result(err) &
                bind(c, name='multio_config_set_failure_handler')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_funptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: mio
                type(c_funptr), value, intent(in) :: handler
                type(c_ptr),    value, intent(in) :: context
                integer(c_int) :: err
            end function c_multio_config_set_failure_handler
        end interface
        ! Initialization
        new_id_loc = c_null_ptr
        old_id => null()
        ! Search for old error handlers
        if(associated(cc%failure_id)) then
            old_id => cc%failure_id
        endif
        ! Append the new error handler
        new_id_loc = failure_info_list%add(handler, context)
        call c_f_pointer(new_id_loc, cc%failure_id)
        c_err = c_multio_config_set_failure_handler(cc%c_ptr(), c_funloc(failure_handler_wrapper), new_id_loc)
        ! Revo the old error handler if exists
        if(associated(old_id)) then
            call failure_info_list%remove(old_id)
        endif
        ! Setting return value
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_set_failure_handler


    !>
    !! @brief set the path for the configuration
    !!
    !! @param [in,out] cc   - handle passed object pointer
    !! @param [in]     path - path for the configuration
    !!
    !! @return error code
    !!
    function multio_conf_set_path(cc, path) result(err)
        use, intrinsic :: iso_c_binding, only: c_loc
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_null_char
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        character(len=*),            intent(in)    :: path
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        character(:,kind=c_char), allocatable, target :: nullified_path
        ! Private interface to the c API
        interface
            function c_multio_conf_set_path(cc, path) result(err) &
                bind(c, name='multio_conf_set_path')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), intent(in), value :: path
                type(c_ptr), intent(in), value :: cc
                integer(c_int) :: err
            end function c_multio_conf_set_path
        end interface
        ! Initialization and allocation
        nullified_path = trim(path) // c_null_char
        ! Call the c API
        c_err = c_multio_conf_set_path(cc%impl, c_loc(nullified_path))
        ! Output cast and cleanup
        if (allocated(nullified_path)) deallocate(nullified_path)
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_set_path


    !>
    !! @brief allow mpicommworld as default communicator
    !!
    !! @param [in,out] cc    - handle passed object pointer
    !! @param [in]     allow - flag used to allow the use of mpi_comm_world
    !!
    !! @return error code
    !!
    !! @see multio_conf_mpi_return_server_comm
    !! @see multio_conf_mpi_return_client_comm
    !! @see multio_conf_mpi_parent_comm
    !! @see multio_conf_mpi_client_id
    !!
    function multio_conf_mpi_allow_world_default_comm(cc, allow) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_bool
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        logical,                     intent(in)    :: allow
        ! Function Result
        integer :: err
        ! Local variables
        logical(c_bool) :: c_allow
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_conf_mpi_allow_world_default_comm(cc, allow) result(err) &
                bind(c, name='multio_conf_mpi_allow_world_default_comm')
                use, intrinsic :: iso_c_binding, only: c_bool
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),     value, intent(in) :: cc
                logical(c_bool), value, intent(in) :: allow
                integer(c_int) :: err
            end function c_multio_conf_mpi_allow_world_default_comm
        end interface
        ! Initialization and allocation
        c_allow = logical(allow,c_bool)
        ! Call the c API
        c_err = c_multio_conf_mpi_allow_world_default_comm(cc%impl, c_allow)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_mpi_allow_world_default_comm


    !>
    !! @brief set the mpi parent comm
    !!
    !! @param [in,out] cc          - handle passed object pointer
    !! @param [in]     parent_comm - ???
    !!
    !! @return error code
    !!
    !! @see multio_conf_mpi_return_server_comm
    !! @see multio_conf_mpi_return_client_comm
    !! @see multio_conf_mpi_client_id
    !! @see multio_conf_mpi_allow_world_default_comm
    !!
    function multio_conf_mpi_parent_comm(cc, parent_comm) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int),              intent(in)    :: parent_comm
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_conf_mpi_parent_comm(cc, parent_comm) result(err) &
                bind(c, name='multio_conf_mpi_parent_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: cc
                integer(c_int), value, intent(in) :: parent_comm
                integer(c_int) :: err
            end function c_multio_conf_mpi_parent_comm
        end interface
        ! Call the c API
        c_err = c_multio_conf_mpi_parent_comm(cc%impl, parent_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_mpi_parent_comm


    !>
    !! @brief set the mpi client comm
    !!
    !! @param [in,out] cc          - handle passed object pointer
    !! @param [in]     return_comm - ???
    !!
    !! @return error code
    !!
    !! @see multio_conf_mpi_return_server_comm
    !! @see multio_conf_mpi_parent_comm
    !! @see multio_conf_mpi_client_id
    !! @see multio_conf_mpi_allow_world_default_comm
    !!
    function multio_conf_mpi_return_client_comm(cc, return_comm) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int),              intent(out)   :: return_comm ! can be c_null_ptr
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_conf_mpi_return_client_comm(cc, return_comm) result(err) &
                bind(c, name='multio_conf_mpi_return_client_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int),     intent(out) :: return_comm ! can be c_null_ptr
                integer(c_int) :: err
            end function c_multio_conf_mpi_return_client_comm
        end interface
        ! Call the c API
        c_err = c_multio_conf_mpi_return_client_comm(cc%impl, return_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_mpi_return_client_comm


    !>
    !! @brief set the mpi server comm
    !!
    !! @param [in,out] cc          - handle passed object pointer
    !! @param [in]     return_comm - ???
    !!
    !! @return error code
    !!
    !! @see multio_conf_mpi_return_client_comm
    !! @see multio_conf_mpi_parent_comm
    !! @see multio_conf_mpi_client_id
    !! @see multio_conf_mpi_allow_world_default_comm
    !!
    function multio_conf_mpi_return_server_comm(cc, return_comm) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
    implicit none
        ! Dummy arguments
        class(multio_configuration), intent(inout) :: cc
        integer(c_int), intent(out) :: return_comm ! can be c_null_ptr
        ! Function Result
        integer :: err
        ! Local variables
        integer(kind=c_int) :: c_err
        ! Private interface to the c API
        interface
            function c_multio_conf_mpi_return_server_comm(cc, return_comm) result(err) &
                bind(c, name='multio_conf_mpi_return_server_comm')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in)  :: cc
                integer(c_int),     intent(out) :: return_comm ! can be c_null_ptr
                integer(c_int) :: err
            end function c_multio_conf_mpi_return_server_comm
        end interface
        ! Call the c API
        c_err = c_multio_conf_mpi_return_server_comm(cc%impl, return_comm)
        ! Output cast and cleanup
        err = int(c_err,kind(err))
        ! Exit point
        return
    end function multio_conf_mpi_return_server_comm

end module multio_configuration_mod