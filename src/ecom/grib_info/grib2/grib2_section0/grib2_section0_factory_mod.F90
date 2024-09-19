!>
!> @file grib2_section0_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing GRIB2 Section 0 objects.
!>
!> The `GRIB2_SECTION0_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of GRIB2 Section 0 objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION0_000_MOD::GRIB2_SECTION0_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section0_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION6_000_MOD'
MODULE GRIB2_SECTION0_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION0_FACTORY

CONTAINS

!>
!> @brief Factory function for creating or initializing GRIB2 Section 0 objects.
!>
!> This function acts as a factory for creating or initializing a GRIB2 Section 0 object
!> based on the provided parameters. It assigns the proper type (`GRIB2_SECTION0_000_T`)
!> to the `GRIB_SECTION0` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] GRIB_SECTION0 The GRIB2 Section 0 object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in] PARAMS The model parameters structure of type `MODEL_PAR_T`.
!> @param [in] ID Integer identifier for the GRIB2 Section 0 object.
!> @param [in] CFG YAML configuration object used to configure the GRIB2 Section 0 object.
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `GRIB2_SECTION0_000_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION0_000_MOD::GRIB2_SECTION0_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION0_000_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION0_FACTORY'
FUNCTION GRIB2_SECTION0_FACTORY( GRIB_SECTION0, PARAMS, ID, CFG, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD,            ONLY: JPIB_K
  PP_USE_L('T') :: GRIB2_SECTION0_000_MOD, ONLY: GRIB2_SECTION0_000_T
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD,    ONLY: YAML_CONFIGURATION_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD,      ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB_SECTION0
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: PARAMS
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: ID
  TYPE(YAML_CONFIGURATION_T),          INTENT(IN)    :: CFG
  LOGICAL,                             INTENT(IN)    :: VERBOSE

  ! Function result
  INTEGER(KIND=ERR_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

   ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION_0=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_ERR_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialize the section
  SELECT CASE( ID )

  CASE( 0 )

    ALLOCATE( GRIB2_SECTION0_000_T::GRIB_SECTION0, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION_0 )

  END SELECT

  !> Initialization of the section
  PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  GRIB2_SECTION0%INIT( PARAMS, CFG, VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP

    TMP = REPEAT(' ', 32)
    WRITE(TMP,'(I32)')  ID

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_SECTION_0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown section0 number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating section0 number: '//TRIM(ADJUSTL(TMP)) )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating section0 number: '//TRIM(ADJUSTL(TMP))//' : '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error initializing section0 number: '//TRIM(ADJUSTL(TMP)) )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION0_FACTORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION0_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
