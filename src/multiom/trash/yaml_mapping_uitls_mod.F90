! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'yaml_mapping_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'YAML_MAPPING_UTILS_MOD'
MODULE YAML_MAPPING_UTILS_MOD
IMPLICIT NONE

! Data types
TYPE :: MAPPING_RULE_T

  PRIVATE

  INTEGER(KIND=JPIB_K) :: FROM_PARAM_ID
  INTEGER(KIND=JPIB_K) :: FROM_LEVEL
  INTEGER(KIND=JPIB_K) :: FROM_DIRECTION
  INTEGER(KIND=JPIB_K) :: FROM_FREQUENCY
  INTEGER(KIND=JPIB_K) :: FROM_LEVTYPE

  INTEGER(KIND=JPIB_K) :: TO_PARAM_ID
  INTEGER(KIND=JPIB_K) :: TO_LEVEL
  INTEGER(KIND=JPIB_K) :: TO_DIRECTION
  INTEGER(KIND=JPIB_K) :: TO_FREQUENCY
  INTEGER(KIND=JPIB_K) :: TO_LEVTYPE
  REAL(KIND_JPRD_K)    :: SCALE_FACTOR

CONTAINS

  PROCEDURE, PUBLIC, PASS :: READ  => READ_RULE_MAPPING_FROM
  PROCEDURE, PUBLIC, PASS :: MATCH => MATCH_RULE_MAPPING_FROM
  PROCEDURE, PUBLIC, PASS :: APPLY => MATCH_RULE_MAPPING_FROM
  PROCEDURE, PUBLIC, PASS :: FREE  => FREE_RULE_MAPPING_FROM

END TYPE

TYPE :: MAPPING_RULES_T
  TYPE(MAPPING_RULE_T), DIMENSION(:), ALLOCATABLE :: MAPS
END TYPE



CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_MAPPINGS'
__THREAD_SAFE__ SUBROUTINE READ_RULE_MAPPINGS( CFG, MAPPINGS, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(MAPPING_RULES_T)       INTENT(OUT) :: MAPPINGS
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  TYPE(YAML_CONFIGURATIONS_T) :: MAPPING_RULES_CFG
  TYPE(YAML_CONFIGURATION_T) :: CURR_CFG
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  LOGICAL :: CFG_HAS_MAPPING_RULES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(MAPPINGS%MAPS), 0 )

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'mapping-rules', CFG_HAS_MAPPING_RULES, VERBOSE )

  ! Reading all the rules
  IF ( CFG_HAS_MAPPING_RULES ) THEN

    ! Read the mapping rules
    CALL YAML_GET_SUBCONFIGURATIONS( CFG, 'mapping-rules', MAPPING_RULES_CFG, VERBOSE )

    ! Get the number of rules
    CALL YAML_GET_CONFIGURATIONS_SIZE( MAPPING_RULES_CFG, N, VERBOSE )

    ! Check the allocation status of the subconfigurations
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MAPPING_RULES_CFG), 1 )

    ! Allocate output structure
    ALLOCATE( MAPPINGS%MAPS(N), STATUS=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, 2 )

    ! Read rules one-by-one
    DO I = 1, N

      ! Get the current configuration
      CALL YAML_GET_CONFIGURATION_BY_ID( MAPPING_RULES_CFG, I, CURR_CFG, VERBOSE )

      ! Read the mapping rule
      CALL READ_RULE_MAPPING( CURR_CFG, MAPPINGS%MAPPING(I), VERBOSE )

    ENDDO

    ! Free subconfigurations
    CALL YAML_DELETE_CONFIGURATIONS( MAPPING_RULES_CFG )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Mappings output structure already allocated' )
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Mappings configuration not allocated after reading' )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate mapping output structure: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate lookup_table' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_MAPPINGS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_MAPPING'
__THREAD_SAFE__ SUBROUTINE READ_RULE_MAPPING( CFG, MAPPING, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(MAPPING_RULE_T),       INTENT(OUT) :: MAPPING
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read the matching conditions
  CALL READ_RULE_MAPPING_FROM( CFG, &
&      MAPPING%FROM_PARAM_ID, MAPPING%FROM_LEVEL, MAPPING%FROM_DIRECTION, &
&      MAPPING%FROM_FREQUENCY, MAPPING%FROM_LEVTYPE, VERBOSE )

  ! Read the mapping values
  CALL READ_RULE_MAPPING_TO( CFG, &
&      MAPPING%TO_PARAM_ID,   MAPPING%TO_LEVEL,   MAPPING%TO_DIRECTION,   &
&      MAPPING%TO_FREQUENCY,   MAPPING%TO_LEVTYPE, MAPPING%SCALE_FACTOR, VERBOSE )

  ! Check the mapping consistency
  PP_DEBUG_CRITICAL_COND_THROW( MAPPING%FROM_LEVEL.NE.UNDEF_PARAM_E .AND. MAPPING%FROM_DIRECTION.NE.UNDEF_PARAM_E, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( MAPPING%TO_LEVEL.NE.UNDEF_PARAM_E   .AND. MAPPING%TO_DIRECTION.NE.UNDEF_PARAM_E,   2 )
  PP_DEBUG_CRITICAL_COND_THROW( MAPPING%FROM_LEVEL.NE.UNDEF_PARAM_E .AND. MAPPING%FROM_FREQUENCY.NE.UNDEF_PARAM_E, 3 )
  PP_DEBUG_CRITICAL_COND_THROW( MAPPING%TO_LEVEL.NE.UNDEF_PARAM_E   .AND. MAPPING%TO_FREQUENCY.NE.UNDEF_PARAM_E,   4 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Inconsistent mapping from-level and from-direction cannot be specified at the same time' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Inconsistent mapping to-level and to-direction cannot be specified at the same time' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Inconsistent mapping from-level and from-frequency cannot be specified at the same time' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Inconsistent mapping to-level and to-frequency cannot be specified at the same time' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_MAPPING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_MAPPING_FROM'
__THREAD_SAFE__ SUBROUTINE READ_RULE_MAPPING_FROM( CFG, PARAMID, LEVEL, &
&                               DIRECTION, FREQUENCY, LEVTYPE, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB),         INTENT(OUT) :: PARAMID
  INTEGER(KIND=JPIB),         INTENT(OUT) :: LEVEL
  INTEGER(KIND=JPIB),         INTENT(OUT) :: DIRECTION
  INTEGER(KIND=JPIB),         INTENT(OUT) :: FREQUENCY
  INTEGER(KIND=JPIB),         INTENT(OUT) :: LEVTYPE
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  TYPE(YAML_CONFIGURATION_T) :: FROM_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: LOC_LEVTYPE
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  LOGICAL :: CONFIGURATION_HAS_FROM
  LOGICAL :: CONFIGURATION_HAS_PARAMID
  LOGICAL :: CONFIGURATION_HAS_LEVEL
  LOGICAL :: CONFIGURATION_HAS_DIRECTION
  LOGICAL :: CONFIGURATION_HAS_FREQUENCY
  LOGICAL :: CONFIGURATION_HAS_LEVTYPE
  LOGICAL :: CONFIGURATION_HAS_TAG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'from', CONFIGURATION_HAS_FROM, VERBOSE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_FROM, 3 )

  ! Rules defined in-place
  CNT = 0
  IF ( CONFIGURATION_HAS_FROM ) THEN

    ! Read the "from" subconfiguration
    CALL YAML_GET_SUBCONFIGURATION( CFG, 'from', FROM_CFG, VERBOSE )

    ! Configuration substructure
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'paramId',   CONFIGURATION_HAS_PARAMID,   VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'level',     CONFIGURATION_HAS_LEVEL,     VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'direction', CONFIGURATION_HAS_DIRECTION, VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'frequency', CONFIGURATION_HAS_FREQUENCY, VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'levtype',   CONFIGURATION_HAS_LEVTYPE,   VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( FROM_CFG, 'tag',       CONFIGURATION_HAS_TAG,       VERBOSE )

    ! Read the paramId
    IF ( CONFIGURATION_HAS_PARAMID ) THEN
      CALL YAML_READ_INTEGER( FROM_CFG, 'paramId', PARAMID, VERBOSE )
      CNT = CNT + 1
    ELSE
      PARAMID = UNDEF_PARAM_E
    ENDIF

    ! Read the level
    IF ( CONFIGURATION_HAS_LEVEL ) THEN
      CALL YAML_INTEGER_STRING( FROM_CFG, 'level', LEVEL, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    ! Read the direction
    IF ( CONFIGURATION_HAS_DIRECTION ) THEN
      CALL YAML_INTEGER_STRING( FROM_CFG, 'direction', DIRECTION, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    ! Read the frequency
    IF ( CONFIGURATION_HAS_FREQUENCY ) THEN
      CALL YAML_INTEGER_STRING( FROM_CFG, 'frequency', FREQUENCY, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    IF ( CONFIGURATION_HAS_LEVTYPE ) THEN
      CALL YAML_READ_STRING( FROM_CFG, 'levtype', LOC_LEVTYPE, VERBOSE )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOC_LEVTYPE), 1 )
      IF ( STRING_IS_INTEGER( LOC_LEVTYPE ) ) THEN
        READ(LOC_LEVTYPE, *) LEVTYPE
      ELSE
        CALL CLEVTYPE2ILEVTYPE( LOC_LEVTYPE, LEVTYPE )
      ENDIF
      DEALLOCATE(LOC_LEVTYPE, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 2 )
      CNT = CNT + 1
    ELSE
      LEVTYPE = UNDEF_PARAM_E
    ENDIF

    ! Free subconfiguration
    CALL YAML_DELETE_CONFIGURATION( FROM_CFG )

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( CNT.LT.1, 4 )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'levtype is not allocated after read' )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levtype: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levtype' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find "from" subconfiguration' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'no rule found in "from" subconfiguration' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_MAPPING_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_MAPPING_TO'
__THREAD_SAFE__ SUBROUTINE READ_RULE_MAPPING_TO( CFG, PARAMID, LEVEL, &
&                DIRECTION, FREQUENCY, LEVTYPE, SCALE_FACTOR, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_FLOAT
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB),         INTENT(OUT) :: PARAMID
  INTEGER(KIND=JPIB),         INTENT(OUT) :: LEVEL
  INTEGER(KIND=JPIB),         INTENT(OUT) :: DIRECTION
  INTEGER(KIND=JPIB),         INTENT(OUT) :: FREQUENCY
  INTEGER(KIND=JPIB),         INTENT(OUT) :: LEVTYPE
  REAL(KIND=JPRD_K),          INTENT(OUT) :: SCALE_FACTOR
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  TYPE(YAML_CONFIGURATION_T) :: TO_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: LOC_LEVTYPE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  LOGICAL :: CONFIGURATION_HAS_TO
  LOGICAL :: CONFIGURATION_HAS_PARAMID
  LOGICAL :: CONFIGURATION_HAS_LEVEL
  LOGICAL :: CONFIGURATION_HAS_DIRECTION
  LOGICAL :: CONFIGURATION_HAS_FREQUENCY
  LOGICAL :: CONFIGURATION_HAS_LEVTYPE
  LOGICAL :: CONFIGURATION_HAS_SCALE_FACTOR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'to', CONFIGURATION_HAS_TO, VERBOSE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_TO, 3 )

  ! Rules defined in-place
  CNT = 0
  IF ( CONFIGURATION_HAS_TO ) THEN

    ! Read the "from" subconfiguration
    CALL YAML_GET_SUBCONFIGURATION( CFG, 'to', TO_CFG, VERBOSE )

    ! Configuration substructure
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'paramId',      CONFIGURATION_HAS_PARAMID,      VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'level',        CONFIGURATION_HAS_LEVEL,        VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'direction',    CONFIGURATION_HAS_DIRECTION,    VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'frequency',    CONFIGURATION_HAS_FREQUENCY,    VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'levtype',      CONFIGURATION_HAS_LEVTYPE,      VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( TO_CFG, 'scale-factor', CONFIGURATION_HAS_SCALE_FACTOR, VERBOSE )

    ! Read the paramId
    IF ( CONFIGURATION_HAS_PARAMID ) THEN
      CALL YAML_READ_INTEGER( TO_CFG, 'paramId', PARAMID, VERBOSE )
      CNT = CNT + 1
    ELSE
      PARAMID = UNDEF_PARAM_E
    ENDIF

    ! Read the level
    IF ( CONFIGURATION_HAS_LEVEL ) THEN
      CALL YAML_INTEGER_STRING( TO_CFG, 'level', LEVEL, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    ! Read the direction
    IF ( CONFIGURATION_HAS_DIRECTION ) THEN
      CALL YAML_INTEGER_STRING( TO_CFG, 'direction', DIRECTION, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    ! Read the frequency
    IF ( CONFIGURATION_HAS_FREQUENCY ) THEN
      CALL YAML_INTEGER_STRING( TO_CFG, 'frequency', FREQUENCY, VERBOSE )
      CNT = CNT + 1
    ELSE
      LEVEL = UNDEF_PARAM_E
    ENDIF

    ! Read the lectype
    IF ( CONFIGURATION_HAS_LEVTYPE ) THEN
      CALL YAML_READ_STRING( TO_CFG, 'levtype', LOC_LEVTYPE, VERBOSE )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOC_LEVTYPE), 1 )
      IF ( STRING_IS_INTEGER( LOC_LEVTYPE ) ) THEN
        READ(LOC_LEVTYPE, *) LEVTYPE
      ELSE
        CALL CLEVTYPE2ILEVTYPE( LOC_LEVTYPE, LEVTYPE )
      ENDIF
      DEALLOCATE(LOC_LEVTYPE, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 2 )
      CNT = CNT + 1
    ELSE
      LEVTYPE = UNDEF_PARAM_E
    ENDIF

    ! Read the scale factor
    IF ( CONFIGURATION_HAS_SCALE_FACTOR ) THEN
      CALL YAML_READ_FLOAT( TO_CFG, 'scale-factor', LOC_SCALE_FACTOR, VERBOSE )
    ELSE
      LEVTYPE = 1.0_JPRD_K
    ENDIF

    ! Free subconfiguration
    CALL YAML_DELETE_CONFIGURATION( FROM_CFG )

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( CNT.LT.1, 4 )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'levtype is not allocated after read' )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levtype: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate levtype' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find "from" subconfiguration' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'no rule found in "from" subconfiguration' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_MAPPING_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_MAPPING_ATM'
__THREAD_SAFE__ SUBROUTINE MATCH_MAPPING_ATM( MAP, &
&           IN_PARAMID, IN_LEVEL, IN_LEVTYPE, MATCH, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_T), INTENT(IN)  :: MAP
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVTYPE
  LOGICAL,              INTENT(OUT) :: MATCH
  LOGICAL,              INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  MATCH = .TRUE.

  IF ( MATCH .AND. MAP%FROM_PARAM_ID.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_PARAM_ID.EQ.IN_PARAMID )
  ENDIF

  IF ( MATCH .AND. MAP%FROM_LEVEL_ID.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_LEVEL.EQ.IN_LEVEL )
  ENDIF

  IF ( MATCH .AND. MAP%FROM_LEVTYPE_ID.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_LEVTYPE.EQ.IN_LEVTYPE )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MATCH_MAPPING_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'APPLY_MAPPING_ATM'
__THREAD_SAFE__ SUBROUTINE APPLY_MAPPING_ATM( MAP, &
&           IN_PARAMID, IN_LEVEL, IN_LEVTYPE, &
&           OUT_PARAMID, OUT_LEVEL, OUT_LEVTYPE, OUT_SCALE_FACTOR, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_T), INTENT(IN)  :: MAP
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVTYPE
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_PARAMID
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_LEVEL
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_LEVTYPE
  REAL(KIND=JPRD_K),    INTENT(OUT) :: OUT_SCALE_FACTOR
  LOGICAL,              INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: MATCH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  CALL MATCH_MAPPING_ATM( MAP, IN_PARAMID, IN_LEVEL, IN_LEVTYPE, MATCH, VERBOSE )

  ! Crete mapped values
  IF ( MATCH .AND. MAP%TO_PARAM_ID .NE. UNDEF_PARAM_E ) THEN
    OUT_PARAMID = MAP%TO_PARAM_ID
  ELSE
    OUT_PARAMID = IN_PARAMID
  ENDIF

  IF ( MATCH .AND. MAP%TO_LEVEL .NE. UNDEF_PARAM_E ) THEN
    OUT_LEVEL = MAP%TO_LEVEL
  ELSE
    OUT_LEVEL = IN_LEVEL
  ENDIF

  IF ( MATCH .AND. MAP%TO_LEVTYPE .NE. UNDEF_PARAM_E ) THEN
    OUT_LEVTYPE = MAP%TO_LEVTYPE
  ELSE
    OUT_LEVTYPE = IN_LEVTYPE
  ENDIF

  OUT_SCALE_FACTOR = MAP%SCALE_FACTOR


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE APPLY_MAPPING_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_MAPPING_WAM'
__THREAD_SAFE__ SUBROUTINE MATCH_MAPPING_WAM( MAP,&
&           IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_LEVTYPE, MATCH, VERBOSE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_T), INTENT(IN)  :: MAP
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVTYPE
  LOGICAL,              INTENT(OUT) :: MATCH
  LOGICAL,              INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  MATCH = .TRUE.

  IF ( MATCH .AND. MAP%FROM_PARAM_ID.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_PARAM_ID.EQ.IN_PARAMID )
  ENDIF

  IF ( MATCH .AND. MAP%FROM_DIRECTION.NE.UNDEF_PARAM_E ) THEN
    MATCH = ( MAP%FROM_DIRECTION.EQ.IN_DIRECTION )
  ENDIF

  IF ( MATCH .AND. MAP%FROM_FREQUENCY.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_FREQUENCY.EQ.IN_FREQUENCY )
  ENDIF

  IF ( MATCH .AND. MAP%FROM_LEVTYPE_ID.NE.UNDEF_PARAM_E ) THEN
    MATCH = MATCH .AND. ( MAP%FROM_LEVTYPE.EQ.IN_LEVTYPE )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MATCH_MAPPING_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'APPLY_MAPPING_WAM'
__THREAD_SAFE__ SUBROUTINE APPLY_MAPPING_WAM( MAP,&
&           IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_LEVTYPE, &
&           OUT_PARAMID, OUT_DIRECTION, OUT_FREQUENCY, OUT_LEVTYPE, &
&           OUT_SCALE_FACTOR, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_T), INTENT(IN)  :: MAP
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB),   INTENT(IN)  :: IN_LEVTYPE
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_PARAMID
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_LEVEL
  INTEGER(KIND=JPIB),   INTENT(OUT) :: OUT_LEVTYPE
  REAL(KIND=JPRD_K),    INTENT(OUT) :: OUT_SCALE_FACTOR
  LOGICAL,              INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: MATCH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the mapping matches the input values
  CALL MATCH_MAPPING_WAM( MAP, IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_LEVTYPE, MATCH, VERBOSE )

  ! Crete mapped values
  IF ( MATCH .AND. MAP%TO_PARAM_ID .NE. UNDEF_PARAM_E ) THEN
    OUT_PARAMID = MAP%TO_PARAM_ID
  ELSE
    OUT_PARAMID = IN_PARAMID
  ENDIF

  IF ( MATCH .AND. MAP%TO_DIRECTION .NE. UNDEF_PARAM_E ) THEN
    OUT_DIRECTION = MAP%TO_DIRECTION
  ELSE
    OUT_DIRECTION = IN_DIRECTION
  ENDIF

  IF ( MATCH .AND. MAP%TO_FREQUENCY .NE. UNDEF_PARAM_E ) THEN
    OUT_FREQUENCY = MAP%TO_FREQUENCY
  ELSE
    OUT_FREQUENCY = IN_FREQUENCY
  ENDIF

  IF ( MATCH .AND. MAP%TO_LEVTYPE .NE. UNDEF_PARAM_E ) THEN
    OUT_LEVTYPE = MAP%TO_LEVTYPE
  ELSE
    OUT_LEVTYPE = IN_LEVTYPE
  ENDIF

  OUT_SCALE_FACTOR = MAP%SCALE_FACTOR

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE APPLY_MAPPING_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE YAML_MAPPING_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME