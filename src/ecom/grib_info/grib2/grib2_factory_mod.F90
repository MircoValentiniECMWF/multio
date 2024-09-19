!>
!> @file grib2_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing GRIB2 sections.
!>
!> The `GRIB2_FACTORY_MOD` provides a factory function that creates or initializes
!> GRIB2 sections from 0 to 6, using their respective section-specific factories.
!> The factory function initializes GRIB sections based on the provided parameters,
!> section ID, and configuration. This module is equipped with debugging, logging,
!> and tracing capabilities that can be enabled during runtime if needed.
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_FACTORY_MOD'
MODULE GRIB2_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!> Public symbols (dataTypes)
PUBLIC :: GRIB2_FACTORY

CONTAINS

!>
!> @brief Factory function for creating or initializing GRIB2 sections.
!>
!> This function serves as the main factory for creating or initializing GRIB2 sections,
!> selecting the appropriate factory function for each section based on the provided `SEC` value.
!> It initializes a GRIB section object (`GRIB_SECTION`) using model parameters,
!> section ID, and YAML configuration. Debugging, logging, and tracing functionalities
!> are available if verbose mode is enabled.
!>
!> @param [inout] GRIB_SECTION Pointer to the GRIB section object to be created or initialized.
!>                             Must be of type `GRIB_SECTION_BASE_A`.
!> @param [in] PARAMS Model parameters structure of type `MODEL_PAR_T`.
!> @param [in] SEC Integer indicating the section number (0 to 6) to be initialized.
!> @param [in] ID Integer identifier for the GRIB2 section object.
!> @param [in] CFG YAML configuration object used to configure the GRIB2 section.
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [PROCEDURE] GRIB2_SECTION0_FACTORY_MOD::GRIB2_SECTION0_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION1_FACTORY_MOD::GRIB2_SECTION1_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION2_FACTORY_MOD::GRIB2_SECTION2_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION3_FACTORY_MOD::GRIB2_SECTION3_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION4_FACTORY_MOD::GRIB2_SECTION4_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION5_FACTORY_MOD::GRIB2_SECTION5_FACTORY
!>   - @dependency [PROCEDURE] GRIB2_SECTION6_FACTORY_MOD::GRIB2_SECTION6_FACTORY
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION5_FACTORY'
FUNCTION GRIB2_FACTORY( GRIB_SECTION, PARAMS, SEC, ID, CFG, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD,                ONLY: JPIB_K
  PP_USE_L('P') :: GRIB2_SECTION0_FACTORY_MOD, ONLY: GRIB2_SECTION0_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION1_FACTORY_MOD, ONLY: GRIB2_SECTION1_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION2_FACTORY_MOD, ONLY: GRIB2_SECTION2_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION3_FACTORY_MOD, ONLY: GRIB2_SECTION3_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION4_FACTORY_MOD, ONLY: GRIB2_SECTION4_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION5_FACTORY_MOD, ONLY: GRIB2_SECTION5_FACTORY
  PP_USE_L('P') :: GRIB2_SECTION6_FACTORY_MOD, ONLY: GRIB2_SECTION6_FACTORY
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD,          ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB_SECTION
  TYPE(PARAMS_T),                      INTENT(IN)    :: PARAMS
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: SEC
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: ID
  TYPE(YAML_CONFIGURATION_T),          INTENT(IN)    :: CFG
  LOGICAL,                             INTENT(IN)    :: VERBOSE

  ! Function result
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION0=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION1=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION2=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION3=4_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION4=5_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION5=6_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_BUILD_SECTION6=7_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION=8_ERR_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for loging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialize the section
  SELECT CASE( SEC )
  CASE( 0 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION0) GRIB2_SECTION0_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 1 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION1) GRIB2_SECTION1_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 2 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION2) GRIB2_SECTION2_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 3 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION3) GRIB2_SECTION3_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 4 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION4) GRIB2_SECTION4_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 5 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION5) GRIB2_SECTION5_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE( 6 )
    PP_TRYCALL(ERRFLAG_BUILD_SECTION6) GRIB2_SECTION6_FACTORY( GRIB_SECTION, PARAMS, ID, CFG, VERBOSE )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    WRITE(TMP,'(I32)')  SEC

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_BUILD_SECTION0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 0' )
    CASE (ERRFLAG_BUILD_SECTION1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 1' )
    CASE (ERRFLAG_BUILD_SECTION2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 2' )
    CASE (ERRFLAG_BUILD_SECTION3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 3' )
    CASE (ERRFLAG_BUILD_SECTION4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 4' )
    CASE (ERRFLAG_BUILD_SECTION5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 5' )
    CASE (ERRFLAG_BUILD_SECTION6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error building section 6' )
    CASE (ERRFLAG_UNKNOWN_SECTION)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown section' )
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

END FUNCTION GRIB2_FACTORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME