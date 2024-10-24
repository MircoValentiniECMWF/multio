!>
!> @file grib2_level_configurator_@LEVEL_ID@_mod.F90
!>
!> @brief Module for managing GRIB2 level configurator operations.
!>
!> The `GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 level configurator objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 level configurator objects.
!>   - Allocation of resources.
!>   - Presetting internal leveleters.
!>   - Managing runlevel operations based on input leveleters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!>   - @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
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


#define PP_FILE_NAME 'grib2_level_configurator_@LEVEL_ID@_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_MOD'
MODULE GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 level configurator handler.
!>
!> The `GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runlevel,
!> encoding checks, and cleanup operations for GRIB2 level configurator objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T

CONTAINS

  !>
  !> @brief Initializes the GRIB2 level configurator object.
  !>
  !> This procedure sets up the necessary leveleters and prepares the
  !> object for use.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT

  !>
  !> @brief Allocates resources for the GRIB2 level configurator object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided leveleters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE

  !>
  !> @brief Presets the leveleters of the GRIB2 level configurator object.
  !>
  !> This procedure configures the internal leveleters of the object
  !> before runlevel execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET

  !>
  !> @brief Manages the runlevel execution of GRIB2 level configurator operations.
  !>
  !> This procedure handles operations and computations during runlevel,
  !> making use of level and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME

  !>
  !> @brief Determines if the GRIB2 level configurator object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided leveleters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 level configurator object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE

  !>
  !> @brief Print informations related to the section
  !>
  !> This procedure print informatin about the section and eventually call
  !> the print method of the nested sub-sections
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRINT

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T

CONTAINS

!>
!> @brief Initializes GRIB2 level configurator for a given object using the provided leveleters.
!>
!> This function initializes a GRIB2 level configurator object (`THIS`) using the provided model leveleters (`LEVELS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @level [inout] THIS    GRIB2 level configurator object to be initialized.
!> @level [in]    LEVELS  Model leveleters used during initialization.
!> @level [in]    CFG     YAML configuration data for initialization.
!> @level [in]    VERBOSE Logical flag for verbose output during initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT( THIS, PAR, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T),  INTENT(INOUT) :: THIS
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

  ! Initialise the section
  THIS%NUMBER_ = 'LEVEL_CONFIGURATOR'
  THIS%TYPE_   = '@LEVEL_NAME@'

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates resources for GRIB2 level configurator using the provided leveleters.
!>
!> This function allocates resources for a GRIB2 level configurator object (`THIS`) using the provided model leveleters (`LEVELS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>
!> @level [in]    THIS      GRIB2 level configurator object for which resources are allocated.
!> @level [in]    LEVELS    Model leveleters required for allocation.
!> @level [in]    MSG       Message structure providing necessary information.
!> @level [inout] METADATA  Pointer to metadata used during allocation.
!> @level [in]    VERBOSE   Logical flag for verbose output during allocation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE( THIS, PAR, &
&  MSG,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T),     INTENT(INOUT) :: THIS
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 level configurator using the provided leveleters and message data.
!>
!> This function presets a GRIB2 level configurator object (`THIS`) using the provided model leveleters (`LEVELS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>
!> @level [in]    THIS      GRIB2 level configurator object to be preset.
!> @level [in]    LEVELS    Model leveleters used during the preset process.
!> @level [in]    MSG       Message structure providing necessary information.
!> @level [inout] METADATA  Pointer to metadata involved in the preset process.
!> @level [in]    VERBOSE   Logical flag for verbose output during the preset operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET( THIS, PAR, &
&   MSG, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T),     INTENT(INOUT) :: THIS
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runlevel processing for GRIB2 level configurator using provided leveleters, message data, and level history.
!>
!> This function performs runlevel operations for a GRIB2 level configurator object (`THIS`) using the provided model leveleters (`LEVELS`),
!> message structure (`MSG`), current level (`CURR_TIME`), level history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runlevel operation.
!>
!> @section interface
!>
!> @level [in]    THIS          GRIB2 level configurator object for runlevel execution.
!> @level [in]    LEVELS        Model leveleters used during the runlevel process.
!> @level [in]    MSG           Message structure providing necessary information.
!> @level [in]    CURR_TIME     Current level used in the runlevel process.
!> @level [in]    TIME_HISTORY  Time history information for the runlevel process.
!> @level [inout] METADATA      Pointer to metadata involved in the runlevel process.
!> @level [in]    VERBOSE       Logical flag for verbose output during the runlevel operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME( THIS, PAR, &
&       MSG, CURR_TIME, TIME_HIST, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: TIME_UTILS_MOD,      ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,      ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T),     INTENT(INOUT) :: THIS
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 level configurator for encoding based on provided leveleters, message data, and level history.
!>
!> This function determines whether GRIB2 level configurator (`THIS`) is ready to be encoded. It processes the provided model leveleters
!> (`LEVELS`), message structure (`MSG`), current level (`CURR_TIME`), level history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>
!> @level [inout] THIS          GRIB2 level configurator object to be checked for encoding readiness.
!> @level [in]    LEVELS        Model leveleters used during the encoding preparation.
!> @level [in]    MSG           Message structure providing necessary information.
!> @level [in]    CURR_TIME     Current level used in the encoding process.
!> @level [in]    TIME_HISTORY  Time history information for the encoding process.
!> @level [inout] TO_BE_ENCODED Logical flag indicating whether the section is ready to be encoded.
!> @level [in]    VERBOSE       Logical flag for verbose output during the operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED( THIS, PAR, &
&    MSG, CURR_TIME, TIME_HIST, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: TIME_UTILS_MOD,      ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,      ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T),  INTENT(INOUT) :: THIS
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),         INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),            INTENT(IN)    :: CURR_TIME
  LOGICAL,                      INTENT(INOUT) :: TO_BE_ENCODED
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 level configurator object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 level configurator object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @level [inout] THIS    GRIB2 level configurator object to be deallocated and freed.
!> @level [in]    VERBOSE Logical flag for verbose output during resource cleanup.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_INIT
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_ALLOCATE
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRESET
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_RUNTIME
!> @see GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Print informations related to the grib section
!>
!> @section interface
!>   @level [inout] THIS An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @level [in]    UNIT The unit to which the information will be printed.
!>   @level [in]    OFFSET The offset to be used for indentation.
!>   @level [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRINT'
PP_THREAD_SAFE FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=1_JPIB_K

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

  ! Write the section information
  IF ( OFFSET .LE. 0 ) THEN
    WRITE(UNIT,'(A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T: ', TRIM(ADJUSTL(THIS%NUMBER_)), '::', TRIM(ADJUSTL(THIS%TYPE_))
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), 'GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_T: ', TRIM(ADJUSTL(THIS%NUMBER_)), '::', TRIM(ADJUSTL(THIS%TYPE_))
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error writing to given unit' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_LEVEL_CONFIGURATOR_@LEVEL_ID@_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
