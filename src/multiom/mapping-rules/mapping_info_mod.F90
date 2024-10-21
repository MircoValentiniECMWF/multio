! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'mapping_info_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAPPING_INFO_MOD'
MODULE MAPPING_INFO_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE

  !> Encoding info data structure
  TYPE :: MAPPING_INFO_T

    ! Default visibility of the type
    PRIVATE

    !> Wrapped encoder
    CLASS(ASSIGNMENT_BASE_A), POINTER :: ASSIGNMENT_ => NULL()

    !> Tag
    CHARACTER(LEN=256) :: TAG_ = REPEAT(' ',256)

    !> To decide what to do when free the encoder
    LOGICAL :: IS_LAZY_ = .FALSE.

  CONTAINS

    PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INIT    => MAPPING_INFO_INIT
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PREPARE => MAPPING_INFO_PREPARE
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: ENCODE  => MAPPING_INFO_ENCODE
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE    => MAPPING_INFO_FREE

  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: MAPPING_INFO_T

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAPPING_INFO_INIT'
PP_THREAD_SAFE FUNCTION MAPPING_INFO_INIT( THIS, ASSIGNMENT, TAG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MAPPING_INFO_T),             INTENT(INOUT) :: THIS
  CLASS(ASSIGNMENT_BASE_A), POINTER, INTENT(IN)    :: ASSIGNMENT
  CHARACTER(LEN=256),                INTENT(IN)    :: TAG
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

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

  ! Associate the fields
  THIS%ASSIGNMENT_ => ASSIGNMENT
  THIS%TAG_ = TAG

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION MAPPING_INFO_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MAPPING_INFO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
