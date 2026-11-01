!> @file
!!
!! @brief Tracing hook (DR_HOOK) of the caller.
!!
!! The caller passes its DR_HOOK routine to multio (client handle creation, server runnables).
!! A Fortran procedure with a character argument cannot be called from C directly: the procedure
!! is kept in a holder and multio receives the trampoline below and the address of the holder.
!! multio stores the hook; it is not called yet.
!!

module multio_api_dr_hook_mod

    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_c_binding, only: c_null_ptr

implicit none

    ! Default visibility of the module
    private

    !> @brief Interface of the tracing hook (as DR_HOOK_DEFAULT8 in fiat)
    abstract interface
        subroutine multio_dr_hook_if(cdname, kswitch, pkey)
            use, intrinsic :: iso_c_binding, only: c_int
            use, intrinsic :: iso_c_binding, only: c_double
        implicit none
            character(len=*),    intent(in)    :: cdname
            integer(kind=c_int), intent(in)    :: kswitch
            real(kind=c_double), intent(inout) :: pkey
        end subroutine multio_dr_hook_if
    end interface

    !> @brief Holder of the procedure, its address is the context passed to C
    type :: multio_dr_hook_holder
        procedure(multio_dr_hook_if), pointer, nopass :: fn => null()
    end type multio_dr_hook_holder

    ! Public symbols whitelist
    public :: multio_dr_hook_if
    public :: multio_dr_hook_new
    public :: multio_dr_hook_free
    public :: multio_dr_hook_trampoline

contains

    !> @brief Create a holder for the hook and return its address (context passed to C)
    function multio_dr_hook_new(dr_hook) result(context)
        use, intrinsic :: iso_c_binding, only: c_loc
    implicit none
        procedure(multio_dr_hook_if) :: dr_hook
        type(c_ptr) :: context
        type(multio_dr_hook_holder), pointer :: holder

        allocate(holder)
        holder%fn => dr_hook
        context = c_loc(holder)
    end function multio_dr_hook_new


    !> @brief Free a holder created by multio_dr_hook_new
    subroutine multio_dr_hook_free(context)
        use, intrinsic :: iso_c_binding, only: c_associated
        use, intrinsic :: iso_c_binding, only: c_f_pointer
    implicit none
        type(c_ptr), intent(inout) :: context
        type(multio_dr_hook_holder), pointer :: holder

        if (c_associated(context)) then
            call c_f_pointer(context, holder)
            deallocate(holder)
            context = c_null_ptr
        end if
    end subroutine multio_dr_hook_free


    !> @brief Called from C: forward the call to the Fortran hook of the holder
    subroutine multio_dr_hook_trampoline(context, name, name_length, kswitch, key) &
            bind(c, name='multio_fapi_dr_hook_trampoline')
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_double
        use, intrinsic :: iso_c_binding, only: c_char
        use, intrinsic :: iso_c_binding, only: c_associated
        use, intrinsic :: iso_c_binding, only: c_f_pointer
    implicit none
        type(c_ptr),         value, intent(in)    :: context
        type(c_ptr),         value, intent(in)    :: name
        integer(kind=c_int), value, intent(in)    :: name_length
        integer(kind=c_int), value, intent(in)    :: kswitch
        real(kind=c_double),        intent(inout) :: key
        type(multio_dr_hook_holder), pointer :: holder
        character(kind=c_char, len=1), dimension(:), pointer :: cname
        character(len=:), allocatable :: fname
        integer :: i

        if (.not. c_associated(context) .or. .not. c_associated(name)) return
        call c_f_pointer(context, holder)
        if (.not. associated(holder%fn)) return

        call c_f_pointer(name, cname, [name_length])
        allocate(character(len=name_length) :: fname)
        do i = 1, name_length
            fname(i:i) = cname(i)
        end do
        call holder%fn(fname, kswitch, key)
    end subroutine multio_dr_hook_trampoline

end module multio_api_dr_hook_mod
