!>
!> @file grib2_section3_050_mod.F90
!>
!> @brief Module for managing GRIB2 Section 3 operations.
!>
!> The `GRIB2_SECTION3_050_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Section 3 objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Section 3 objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_SECTION3_050_INIT
!>   - @see GRIB2_SECTION3_050_ALLOCATE
!>   - @see GRIB2_SECTION3_050_PRESET
!>   - @see GRIB2_SECTION3_050_RUNTIME
!>   - @see GRIB2_SECTION3_050_TO_BE_ENCODED
!>   - @see GRIB2_SECTION3_050_FREE
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


#define PP_FILE_NAME 'grib2_section3_050_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION3_050_MOD'
MODULE GRIB2_SECTION3_050_MOD

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: GRIB_SECTION_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 3 handler.
!>
!> The `GRIB2_SECTION3_050_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 3 objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION3_050_T

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 3 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: INIT => GRIB2_SECTION3_050_INIT

  !>
  !> @brief Allocates resources for the GRIB2 Section 3 object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION3_050_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 3 object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION3_050_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 3 operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION3_050_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 3 object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_SECTION3_050_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 3 object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION3_050_FREE

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION3_050_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 3 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @param [inout] THIS    GRIB2 Section 3 object to be initialized.
!> @param [in]    PARAMS  Model parameters used during initialization.
!> @param [in]    CFG     YAML configuration data for initialization.
!> @param [in]    VERBOSE Logical flag for verbose output during initialization.
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
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!> @see GRIB2_SECTION3_050_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_INIT'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_INIT( THIS, PARAMS, &
&          CFG, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD,   ONLY: MODEL_PAR_T
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T), INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),           INTENT(IN)    :: PARAMS
  TYPE(YAML_CONFIGURATION_T),  INTENT(IN)    :: CFG
  LOGICAL,                     INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates resources for GRIB2 Section 3 using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>
!> @param [in]    THIS      GRIB2 Section 3 object for which resources are allocated.
!> @param [in]    PARAMS    Model parameters required for allocation.
!> @param [in]    MSG       Message structure providing necessary information.
!> @param [inout] METADATA  Pointer to metadata used during allocation.
!> @param [in]    VERBOSE   Logical flag for verbose output during allocation.
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
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!> @see GRIB2_SECTION3_050_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_ALLOCATE'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_ALLOCATE( THIS, PARAMS, &
&  MSG,  METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T),     INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Section 3 using the provided parameters and message data.
!>
!> This function presets a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>
!> @param [in]    THIS      GRIB2 Section 3 object to be preset.
!> @param [in]    PARAMS    Model parameters used during the preset process.
!> @param [in]    MSG       Message structure providing necessary information.
!> @param [inout] METADATA  Pointer to metadata involved in the preset process.
!> @param [in]    VERBOSE   Logical flag for verbose output during the preset operation.
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
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!> @see GRIB2_SECTION3_050_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_PRESET'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_PRESET( THIS, PARAMS, &
&   MSG, METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T),     INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Section 3 using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 Section 3 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>
!> @param [in]    THIS          GRIB2 Section 3 object for runtime execution.
!> @param [in]    PARAMS        Model parameters used during the runtime process.
!> @param [in]    MSG           Message structure providing necessary information.
!> @param [in]    CURR_TIME     Current time used in the runtime process.
!> @param [in]    TIME_HISTORY  Time history information for the runtime process.
!> @param [inout] METADATA      Pointer to metadata involved in the runtime process.
!> @param [in]    VERBOSE       Logical flag for verbose output during the runtime operation.
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
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!> @see GRIB2_SECTION3_050_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_RUNTIME'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_RUNTIME( THIS, PARAMS, &
&       MSG, CURR_TIME, TIME_HISTORY, METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: CURR_TIME_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T),     INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 Section 3 for encoding based on provided parameters, message data, and time history.
!>
!> This function determines whether GRIB2 Section 3 (`THIS`) is ready to be encoded. It processes the provided model parameters
!> (`PARAMS`), message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>
!> @param [inout] THIS          GRIB2 Section 3 object to be checked for encoding readiness.
!> @param [in]    PARAMS        Model parameters used during the encoding preparation.
!> @param [in]    MSG           Message structure providing necessary information.
!> @param [in]    CURR_TIME     Current time used in the encoding process.
!> @param [in]    TIME_HISTORY  Time history information for the encoding process.
!> @param [inout] TO_BE_ENCODED Logical flag indicating whether the section is ready to be encoded.
!> @param [in]    VERBOSE       Logical flag for verbose output during the operation.
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
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_TO_BE_ENCODED'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_TO_BE_ENCODED( THIS, PARAMS, &
&    MSG, CURR_TIME, TIME_HISTORY, TO_BE_ENCODED, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: CURR_TIME_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T),  INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),            INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),              INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),         INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),            INTENT(IN)    :: CURR_TIME
  LOGICAL,                      INTENT(INOUT) :: TO_BE_ENCODED
  LOGICAL,                      INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Section 3 object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Section 3 object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @param [inout] THIS    GRIB2 Section 3 object to be deallocated and freed.
!> @param [in]    VERBOSE Logical flag for verbose output during resource cleanup.
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
!> @see GRIB2_SECTION3_050_INIT
!> @see GRIB2_SECTION3_050_ALLOCATE
!> @see GRIB2_SECTION3_050_PRESET
!> @see GRIB2_SECTION3_050_RUNTIME
!> @see GRIB2_SECTION3_050_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION3_050_FREE'
__THREAD_SAFE__ FUNCTION GRIB2_SECTION3_050_FREE( THIS, VERBOSE ) RESULT(RET)

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION3_050_T), INTENT(INOUT) :: THIS
  LOGICAL,                     INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

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
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION GRIB2_SECTION3_050_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION3_050_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
