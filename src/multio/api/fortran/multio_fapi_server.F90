!> @file
!!
!! @brief Server API: the server is made of runnables that do not create threads.
!!
!! The caller creates the shared state and the runnables with multio_server_create_runnables, then runs
!! each runnable on a thread of its own (e.g. one OpenMP thread per runnable): init, run and free are
!! called, in this order, from that thread. After all the runnables have been freed, the shared state
!! is freed.
!!
!! @code
!!   err = multio_server_create_runnables(cc, nthreads, server_comm, client_comms, shared, runnables)
!!   !$omp parallel num_threads(size(runnables))
!!   i = omp_get_thread_num() + 1
!!   err = runnables(i)%init(shared)
!!   err = runnables(i)%run(dr_hook)
!!   err = runnables(i)%free()
!!   !$omp end parallel
!!   err = shared%free()
!! @endcode
!!
!! Threads: multio does not create threads on this path; the threads must be OpenMP threads of the
!! caller. The tracing hook passed to run is only called from inside run, on the calling thread.
!!

#define MULTIO_SERVER_USAGE_SIZE 10

module multio_api_server_mod

    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !> @brief Shared state of a server
    type :: multio_server_shared
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure, public, pass :: free        => multio_server_shared_free
        procedure, public, pass :: usage_count => multio_server_shared_usage_count
        procedure, public, pass :: usage       => multio_server_shared_usage
    end type multio_server_shared

    !> @brief Part of a server executed by one thread of the caller
    type :: multio_server_runnable
        type(c_ptr) :: impl = c_null_ptr
    contains
        procedure, public, pass :: init => multio_server_runnable_init
        procedure, public, pass :: run  => multio_server_runnable_run
        procedure, public, pass :: free => multio_server_runnable_free
        procedure, public, pass :: kind => multio_server_runnable_kind
    end type multio_server_runnable

    !> Number of values returned by multio_server_shared%usage
    integer, parameter, public :: MULTIO_SERVER_USAGE_VALUES = MULTIO_SERVER_USAGE_SIZE

    ! Public symbols whitelist
    public :: multio_server_shared
    public :: multio_server_runnable
    public :: multio_server_create_runnables

contains

    !> @brief Create the server shared state and its runnables.
    !!
    !! The configuration is moved into the server: delete the configuration object afterwards.
    !!
    !! @param [inout] cc           Configuration (multio server configuration file)
    !! @param [in]    nthreads     Number of threads the caller can give to the server
    !! @param [in]    server_comm  Communicator of the server ranks
    !! @param [in]    client_comms Extended communicators, one per client group (server and client ranks)
    !! @param [out]   shared       Shared state
    !! @param [out]   runnables    Runnables, allocated here: one thread per runnable (at most nthreads)
    !!
    !! @return An error code indicating the operation's success.
    !!
    function multio_server_create_runnables(cc, nthreads, server_comm, client_comms, shared, runnables) result(err)
        use, intrinsic :: iso_c_binding,     only: c_int
        use :: multio_api_configuration_mod, only: multio_configuration
        use :: multio_api_constants_mod,     only: MULTIO_SUCCESS
    implicit none
        class(multio_configuration),                       intent(inout) :: cc
        integer,                                           intent(in)    :: nthreads
        integer,                                           intent(in)    :: server_comm
        integer, dimension(:),                             intent(in)    :: client_comms
        type(multio_server_shared),                        intent(out)   :: shared
        type(multio_server_runnable), dimension(:), allocatable, intent(out) :: runnables
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_err
        integer(kind=c_int) :: n_runnables
        integer(kind=c_int), dimension(size(client_comms)) :: c_client_comms
        integer :: i
        interface
            function c_multio_server_create_runnables(cc, nthreads, server_comm, client_comms, n_client_comms, &
                    shared, n_runnables) result(err) bind(c, name='multio_server_create_runnables')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in)  :: cc
                integer(kind=c_int), value, intent(in)  :: nthreads
                integer(kind=c_int), value, intent(in)  :: server_comm
                integer(kind=c_int), dimension(*), intent(in) :: client_comms
                integer(kind=c_int), value, intent(in)  :: n_client_comms
                type(c_ptr),                intent(out) :: shared
                integer(kind=c_int),        intent(out) :: n_runnables
                integer(kind=c_int) :: err
            end function c_multio_server_create_runnables
            function c_multio_server_get_runnable(shared, idx, runnable) result(err) &
                    bind(c, name='multio_server_get_runnable')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),         value, intent(in)  :: shared
                integer(kind=c_int), value, intent(in)  :: idx
                type(c_ptr),                intent(out) :: runnable
                integer(kind=c_int) :: err
            end function c_multio_server_get_runnable
        end interface

        c_client_comms = int(client_comms, kind=c_int)
        n_runnables = 0_c_int
        c_err = c_multio_server_create_runnables(cc%c_ptr(), int(nthreads, kind=c_int), &
            int(server_comm, kind=c_int), c_client_comms, int(size(client_comms), kind=c_int), &
            shared%impl, n_runnables)
        err = int(c_err, kind(err))
        if (err .ne. MULTIO_SUCCESS) then
            allocate(runnables(0))
            return
        end if

        allocate(runnables(n_runnables))
        do i = 1, int(n_runnables)
            c_err = c_multio_server_get_runnable(shared%impl, int(i-1, kind=c_int), runnables(i)%impl)
            err = int(c_err, kind(err))
            if (err .ne. MULTIO_SUCCESS) return
        end do
#else
        allocate(runnables(0))
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_create_runnables


    !> @brief Initialise the runnable, from the thread that will run it
    function multio_server_runnable_init(runnable, shared) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_server_runnable), intent(inout) :: runnable
        type(multio_server_shared),    intent(in)    :: shared
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        interface
            function c_multio_server_runnable_init(runnable, shared) result(err) &
                    bind(c, name='multio_server_runnable_init')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: runnable
                type(c_ptr), value, intent(in) :: shared
                integer(kind=c_int) :: err
            end function c_multio_server_runnable_init
        end interface
        err = int(c_multio_server_runnable_init(runnable%impl, shared%impl), kind(err))
#else
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_runnable_init


    !> @brief Run the runnable until it has finished its work
    !!
    !! @param [in] dr_hook Tracing hook of the caller (optional, stored by multio, not called yet)
    !!
    function multio_server_runnable_run(runnable, dr_hook) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_funptr
        use, intrinsic :: iso_c_binding, only: c_funloc
        use, intrinsic :: iso_c_binding, only: c_null_funptr
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
        use :: multio_api_dr_hook_mod,   only: multio_dr_hook_if
        use :: multio_api_dr_hook_mod,   only: multio_dr_hook_new
        use :: multio_api_dr_hook_mod,   only: multio_dr_hook_free
        use :: multio_api_dr_hook_mod,   only: multio_dr_hook_trampoline
    implicit none
        class(multio_server_runnable), intent(inout) :: runnable
        procedure(multio_dr_hook_if), optional      :: dr_hook
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        type(c_funptr) :: hook
        type(c_ptr) :: context
        interface
            function c_multio_server_runnable_run(runnable, hook, context) result(err) &
                    bind(c, name='multio_server_runnable_run')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_funptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),    value, intent(in) :: runnable
                type(c_funptr), value, intent(in) :: hook
                type(c_ptr),    value, intent(in) :: context
                integer(kind=c_int) :: err
            end function c_multio_server_runnable_run
        end interface

        hook = c_null_funptr
        context = c_null_ptr
        if (present(dr_hook)) then
            hook = c_funloc(multio_dr_hook_trampoline)
            context = multio_dr_hook_new(dr_hook)
        end if
        err = int(c_multio_server_runnable_run(runnable%impl, hook, context), kind(err))
        call multio_dr_hook_free(context)
#else
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_runnable_run


    !> @brief Free the runnable, from the thread that ran it
    function multio_server_runnable_free(runnable) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_server_runnable), intent(inout) :: runnable
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        interface
            function c_multio_server_runnable_free(runnable) result(err) &
                    bind(c, name='multio_server_runnable_free')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: runnable
                integer(kind=c_int) :: err
            end function c_multio_server_runnable_free
        end interface
        err = int(c_multio_server_runnable_free(runnable%impl), kind(err))
        runnable%impl = c_null_ptr
#else
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_runnable_free


    !> @brief Kind of the runnable (listener, receiver, dispatcher, coordinator, ...)
    function multio_server_runnable_kind(runnable, runnable_kind) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_f_pointer
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_server_runnable),  intent(in)  :: runnable
        character(len=:), allocatable,  intent(out) :: runnable_kind
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        type(c_ptr) :: c_kind
        integer(kind=c_int) :: length
        character(kind=c_char, len=1), dimension(:), pointer :: chars
        integer :: i
        interface
            function c_multio_server_runnable_kind(runnable, kind, length) result(err) &
                    bind(c, name='multio_server_runnable_kind')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),  value, intent(in)  :: runnable
                type(c_ptr),         intent(out) :: kind
                integer(kind=c_int), intent(out) :: length
                integer(kind=c_int) :: err
            end function c_multio_server_runnable_kind
        end interface
        err = int(c_multio_server_runnable_kind(runnable%impl, c_kind, length), kind(err))
        if (err .ne. MULTIO_SUCCESS) then
            runnable_kind = ''
            return
        end if
        call c_f_pointer(c_kind, chars, [length])
        allocate(character(len=length) :: runnable_kind)
        do i = 1, int(length)
            runnable_kind(i:i) = chars(i)
        end do
#else
        runnable_kind = ''
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_runnable_kind


    !> @brief Free the shared state, after all the runnables have been freed
    function multio_server_shared_free(shared) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_server_shared), intent(inout) :: shared
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        interface
            function c_multio_server_shared_free(shared) result(err) &
                    bind(c, name='multio_server_shared_free')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr), value, intent(in) :: shared
                integer(kind=c_int) :: err
            end function c_multio_server_shared_free
        end interface
        err = int(c_multio_server_shared_free(shared%impl), kind(err))
        shared%impl = c_null_ptr
#else
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_shared_free


    !> @brief Number of queues and pools whose memory usage is reported (takes a snapshot of the usage)
    function multio_server_shared_usage_count(shared, count) result(err)
        use, intrinsic :: iso_c_binding, only: c_int
        use :: multio_api_constants_mod, only: MULTIO_SUCCESS
    implicit none
        class(multio_server_shared), intent(inout) :: shared
        integer,                     intent(out)   :: count
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        integer(kind=c_int) :: c_count
        interface
            function c_multio_server_shared_usage_count(shared, count) result(err) &
                    bind(c, name='multio_server_shared_usage_count')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
            implicit none
                type(c_ptr),  value, intent(in)  :: shared
                integer(kind=c_int), intent(out) :: count
                integer(kind=c_int) :: err
            end function c_multio_server_shared_usage_count
        end interface
        err = int(c_multio_server_shared_usage_count(shared%impl, c_count), kind(err))
        count = int(c_count)
#else
        count = 0
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_shared_usage_count


    !> @brief Memory usage of a queue or pool of the snapshot taken by usage_count (index from 1 to count)
    !!
    !! values: count, bytes, max count, max bytes, capacity count, capacity bytes, total,
    !!         producer wait (microseconds), consumer wait (microseconds), reserved
    !!
    function multio_server_shared_usage(shared, idx, name, values) result(err)
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_c_binding,   only: c_int
        use, intrinsic :: iso_c_binding,   only: c_int64_t
        use, intrinsic :: iso_c_binding,   only: c_char
        use, intrinsic :: iso_c_binding,   only: c_f_pointer
        use :: multio_api_constants_mod,   only: MULTIO_SUCCESS
    implicit none
        class(multio_server_shared),   intent(inout) :: shared
        integer,                       intent(in)    :: idx
        character(len=:), allocatable, intent(out)   :: name
        integer(kind=int64), dimension(MULTIO_SERVER_USAGE_SIZE), intent(out) :: values
        integer :: err
#if !defined(MULTIO_DUMMY_API)
        type(c_ptr) :: c_name
        integer(kind=c_int) :: length
        integer(kind=c_int64_t), dimension(MULTIO_SERVER_USAGE_SIZE) :: c_values
        character(kind=c_char, len=1), dimension(:), pointer :: chars
        integer :: i
        interface
            function c_multio_server_shared_usage(shared, idx, name, length, values) result(err) &
                    bind(c, name='multio_server_shared_usage')
                use, intrinsic :: iso_c_binding, only: c_ptr
                use, intrinsic :: iso_c_binding, only: c_int
                use, intrinsic :: iso_c_binding, only: c_int64_t
            implicit none
                type(c_ptr),         value, intent(in)  :: shared
                integer(kind=c_int), value, intent(in)  :: idx
                type(c_ptr),                intent(out) :: name
                integer(kind=c_int),        intent(out) :: length
                integer(kind=c_int64_t), dimension(*), intent(out) :: values
                integer(kind=c_int) :: err
            end function c_multio_server_shared_usage
        end interface
        values = 0_int64
        err = int(c_multio_server_shared_usage(shared%impl, int(idx-1, kind=c_int), c_name, length, c_values), &
                  kind(err))
        if (err .ne. MULTIO_SUCCESS) then
            name = ''
            return
        end if
        call c_f_pointer(c_name, chars, [length])
        allocate(character(len=length) :: name)
        do i = 1, int(length)
            name(i:i) = chars(i)
        end do
        values = int(c_values, kind=int64)
#else
        name = ''
        values = 0_int64
        err = int(MULTIO_SUCCESS, kind(err))
#endif
        return
    end function multio_server_shared_usage

end module multio_api_server_mod
