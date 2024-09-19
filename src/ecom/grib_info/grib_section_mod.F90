! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_section_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_SECTION_MOD'
MODULE GRIB_SECTION_MOD

IMPLICIT NONE



TYPE, ABSTRACT :: GRIB_SECTION_BASE_A

CONTAINS

  PUBLIC(GRIB_SECTION_INIT_IF),          DEFERRED, PASS, PUBLIC :: INIT
  PUBLIC(GRIB_SECTION_ALLOCATE_IF),      DEFERRED, PASS, PUBLIC :: ALLOCATE
  PUBLIC(GRIB_SECTION_PRESET_IF),        DEFERRED, PASS, PUBLIC :: PRESET
  PUBLIC(GRIB_SECTION_RUNTIME_IF),       DEFERRED, PASS, PUBLIC :: RUNTIME
  PUBLIC(GRIB_SECTION_TO_BE_ENCODED_IF), DEFERRED, PASS, PUBLIC :: TO_BE_ENCODED
  PUBLIC(GRIB_SECTION_FREE_IF),          DEFERRED, PASS, PUBLIC :: FREE

END TYPE


ABSTACT INTERFACE
__THREAD_SAFE__ FUNCTION GRIB2_SECTION_INIT_IF( THIS, PARAMS, &
&          CFG, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD,   ONLY: MODEL_PAR_T
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),  INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),           INTENT(IN)    :: PARAMS
  TYPE(YAML_CONFIGURATION_T),  INTENT(IN)    :: CFG
  LOGICAL,                     INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB2_SECTION_INIT_IF


__THREAD_SAFE__ FUNCTION GRIB_SECTION_ALLOCATE_IF( THIS, PARAMS, &
&  MSG,  METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB_SECTION_ALLOCATE_IF


__THREAD_SAFE__ FUNCTION GRIB_SECTION_PRESET_IF( THIS, PARAMS, &
&   MSG, METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A
IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB_SECTION_PRESET_IF


__THREAD_SAFE__ FUNCTION GRIB_SECTION_RUNTIME_IF( THIS, PARAMS, &
&       MSG, CURR_TIME, TIME_HISTORY, METADATA, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: CURR_TIME_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(IN)    :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),                 INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  LOGICAL,                         INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB_SECTION_RUNTIME_IF


__THREAD_SAFE__ FUNCTION GRIB2_SECTION_TO_BE_ENCODED_IF( THIS, PARAMS, &
&    MSG, CURR_TIME, TIME_HISTORY, TO_BE_ENCODED, VERBOSE ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MODEL_PAR_T
  PP_USE_L('T') :: OM_DATA_TYPES_MOD, ONLY: MESSAGE_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  PP_USE_L('T') :: OM_CORE_MOD,       ONLY: CURR_TIME_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),  INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),           INTENT(IN)    :: PARAMS
  TYPE(MESSAGE_T),             INTENT(IN)    :: MSG
  TYPE(TIME_HISTORY_T),        INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),           INTENT(IN)    :: CURR_TIME
  LOGICAL,                     INTENT(INOUT) :: TO_BE_ENCODED
  LOGICAL,                     INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB2_SECTION_TO_BE_ENCODED_IF


__THREAD_SAFE__ FUNCTION GRIB_SECTION_FREE_IF( THIS, VERBOSE ) RESULT(RET)

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,                    INTENT(IN)    :: VERBOSE

  !> Function result
  INTEGER(KIND=ERR_K) :: RET

END FUNCTION GRIB_SECTION_FREE_IF



END INTERFACE

END MODULE GRIB_SECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
