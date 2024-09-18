! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define __THREAD_SAFE__ RECURSIVE

#define PP_FILE_NAME 'general_rules_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GENERAL_RULES_MOD'
MODULE GENERAL_RULES_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_FILTERS_UTILS_MOD, ONLY: FILTER_RULES_T
  USE :: YAML_FILTERS_UTILS_MOD, ONLY: TAG_LEN

IMPLICIT NONE

! Default visibility of the module
PRIVATE


! Parameters
LOGICAL, PARAMETER :: DEFAULT_USEPARAMIDECMF = .FALSE.
LOGICAL, PARAMETER :: DEFAULT_DIRECT_TO_FDB = .FALSE.

INTEGER(KIND=JPIM_K), PARAMETER :: UNDEF_PARAM_E = -9999_JPIM_K
INTEGER(KIND=JPIM_K), PARAMETER :: BITS_PER_VALUE_MIN = 1_JPIB_K
INTEGER(KIND=JPIM_K), PARAMETER :: BITS_PER_VALUE_MAX = 64_JPIB_K
INTEGER(KIND=JPIM_K), PARAMETER :: BITS_PER_VALUE_DEFAULT_TABLE = -10_JPIB_K
INTEGER(KIND=JPIM_K), PARAMETER :: BITS_PER_VALUE_COMPRESSED_TABLE = -20_JPIB_K


! Data types
TYPE :: MAPPING_RULE_T

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
END TYPE

TYPE :: GRIB_STRUCTURE_T
  INTEGER(KIND=JPIB_K) :: LOCAL_DEFINITION_TEMPLATE_NUMBER
  INTEGER(KIND=JPIB_K) :: GRID_DEFINITION_TEMPLATE_NUMBER
  INTEGER(KIND=JPIB_K) :: PRODUCT_DEFINITION_TEMPLATE_NUMBER
  INTEGER(KIND=JPIB_K) :: DATA_DEFINITION_TEMPLATE_NUMBER
END TYPE


TYPE :: MAPPING_RULES_T
  TYPE(MAPPING_RULE_T), DIMENSION(:), ALLOCATABLE :: MAPS
END TYPE

TYPE :: ENCODING_RULES_T
  LOGICAL :: USE_PARAMID_ECMF = DEFAULT_USEPARAMIDECMF
  INTEGER(KIND=JPIB_K) :: EDITION = UNDEF_PARAM_E
END TYPE

TYPE :: PACKING_RULES_T
  INTEGER(KIND=JPIB_K) :: PACKING_TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: BITS_PER_VALUE = UNDEF_PARAM_E
END TYPE

TYPE :: SINK_RULES_T
  LOGICAL :: DIRECT_TO_FDB = DEFAULT_DIRECT_TO_FDB
END TYPE


TYPE :: GENERAL_RULE_T
  CHARACTER(LEN=128)     :: NAME
  CHARACTER(LEN=TAG_LEN) :: TAG
  TYPE(FILTER_RULES_T)   :: FILTER
  TYPE(MAPPING_RULES_T)  :: MAPPING
  TYPE(ENCODING_RULES_T) :: ENCODING
  TYPE(PACKING_RULES_T)  :: PACKING
  TYPE(SINK_RULES_T)     :: SINK
END TYPE


TYPE :: GENERAL_RULES_T
  TYPE(GENERAL_RULE_T), DIMENSION(:), ALLOCATABLE :: DEFAULT_RULES
  TYPE(GENERAL_RULE_T), DIMENSION(:), ALLOCATABLE :: SPECIAL_RULES
END TYPE


!> Whitelist of public variables (datatypes)
PUBLIC :: GENERAL_RULES_T
PUBLIC :: GENERAL_RULE_T

!> Whitelist of public variables (procedures)
PUBLIC :: SUGENERAL_RULES
PUBLIC :: FREE_RULES

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_COUNT_ATM'
__THREAD_SAFE__ SUBROUTINE MATCH_COUNT_ATM( IN_RULES, &
&           IN_PARAMID, N_LEVEL, IN_REPRES, IN_LEVTYPE, &
&           IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, &
&           COUNT_RULES_DEFAULT, COUNT_RULES_SPECIAL )

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GENERAL_RULES_T),  INTENT(IN)  :: IN_RULES
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: IN_REPRES
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: IN_LEVTYPE
  CHARACTER(LEN=TAG_LEN), INTENT(IN)  :: IN_TAG
  LOGICAL,                INTENT(IN)  :: IN_ISANSAMBLE
  LOGICAL,                INTENT(IN)  :: IN_ISCHEMICAL
  INTEGER(KIND=JPIM_K),   INTENT(OUT) :: COUNT_RULES_DEFAULT
  INTEGER(KIND=JPIM_K),   INTENT(OUT) :: COUNT_RULES_SPECIAL

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Special rules
  COUNT_RULES_SPECIAL = 0
  IF ( ALLOCATED(IN_RULES%SPECIAL_RULES) ) THEN
    DO I = 1, SIZE(IN_RULES%SPECIAL_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%SPECIAL_RULES(I)%FILTER, &
&         IN_PARAMID, IN_LEVEL, IN_REPRES, IN_LEVTYPE, &
&         IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        COUNT_RULES_SPECIAL = COUNT_RULES_SPECIAL + 1
      ENDIF
    ENDDO
  ENDIF

  ! Default rules
  COUNT_RULES_DEFAULT = 0
  IF ( ALLOCATED(IN_RULES%DEFAULT_RULES) ) THEN
    DO I = 1, SIZE(IN_RULES%DEFAULT_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%DEFAULT_RULES(I)%FILTER, &
&          IN_PARAMID, IN_LEVEL, IN_REPRES, IN_LEVTYPE, &
&          IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        COUNT_RULES_DEFAULT = COUNT_RULES_DEFAULT + 1
      ENDIF
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MATCH_COUNT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_ATM'
__THREAD_SAFE__ SUBROUTINE MATCH_ATM( IN_RULES, &
&           IN_PARAMID, N_LEVEL, IN_REPRES, IN_LEVTYPE, &
&           IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, &
&           OUT_RULES_DEFAULT, OUT_RULES_SPECIAL )

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GENERAL_RULES_T),              INTENT(IN)  :: IN_RULES
  INTEGER(KIND=JPIB_K),               INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),                 INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB_K),               INTENT(IN)  :: IN_REPRES
  INTEGER(KIND=JPIB_K),               INTENT(IN)  :: IN_LEVTYPE
  CHARACTER(LEN=TAG_LEN),             INTENT(IN)  :: IN_TAG
  LOGICAL,                            INTENT(IN)  :: IN_ISANSAMBLE
  LOGICAL,                            INTENT(IN)  :: IN_ISCHEMICAL
  INTEGER(KIND=JPIM_K), DIMENSION(:), INTENT(OUT) :: OUT_RULES_DEFAULT
  INTEGER(KIND=JPIM_K), DIMENSION(:), INTENT(OUT) :: OUT_RULES_SPECIAL

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: CNT
  LOGICAL :: MATCH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Local variables
  N = SIZE(OUT_RULES_DEFAULT)
  M = SIZE(OUT_RULES_SPECIAL)

  ! Special rules
  IF ( M .GT. 0 ) THEN
    CNT = 0
    DO I = 1, SIZE(IN_RULES%SPECIAL_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%SPECIAL_RULES(I)%FILTER, &
&         IN_PARAMID, IN_LEVEL, IN_REPRES, IN_LEVTYPE, &
&         IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        CNT = CNT + 1
        PP_DEBUG_CRITICAL_COND_THROW( CNT .GT. M, 1 )
        OUT_RULES_SPECIAL(CNT) = I
      ENDIF
    ENDDO

  ENDIF

  ! Default rules
  IF ( N .GT. 0 ) THEN
    CNT = 0
    DO I = 1, SIZE(IN_RULES%DEFAULT_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%DEFAULT_RULES(I)%FILTER, &
&          IN_PARAMID, IN_LEVEL, IN_REPRES, IN_LEVTYPE, &
&          IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        CNT = CNT + 1
        PP_DEBUG_CRITICAL_COND_THROW( CNT .GT. M, 2 )
        OUT_RULES_DEFAULT(CNT) = I
      ENDIF
    ENDDO

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error out of bounds OUT_RULES_SPECIAL' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'error out of bounds OUT_RULES_DEFAULT' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MATCH_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MATCH_WAM'
__THREAD_SAFE__ SUBROUTINE MATCH_WAM( IN_RULES, &
&           IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_REPRES, IN_LEVTYPE, &
&           IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL,  &
&           OUT_RULES_DEFAULT, OUT_RULES_SPECIAL )

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GENERAL_RULES_T),                           INTENT(IN)  :: IN_RULES
  INTEGER(KIND=JPIB_K),                            INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),                              INTENT(IN)  :: IN_DIRECTION
  INTEGER(KIND=JPIB),                              INTENT(IN)  :: IN_FREQUENCY
  INTEGER(KIND=JPIB_K),                            INTENT(IN)  :: IN_REPRES
  INTEGER(KIND=JPIB_K),                            INTENT(IN)  :: IN_LEVTYPE
  CHARACTER(LEN=TAG_LEN),                          INTENT(IN)  :: IN_TAG
  LOGICAL,                                         INTENT(IN)  :: IN_ISANSAMBLE
  LOGICAL,                                         INTENT(IN)  :: IN_ISCHEMICAL
  INTEGER(KIND=JPIM_K), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: OUT_RULES_DEFAULT
  INTEGER(KIND=JPIM_K), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: OUT_RULES_SPECIAL

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Special rules
  CNT = 0
  DO I = 1, SIZE(IN_RULES%SPECIAL_RULES)
    CALL MATCH_FILTER_ATM( IN_RULES%SPECIAL_RULES(I)%FILTER, &
&       IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_REPRES, IN_LEVTYPE, &
&       IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
    IF ( MATCH ) THEN
      CNT = CNT + 1
    ENDIF
  ENDDO

  IF ( CNT .GT. 0 ) THEN
    ALLOCATE( OUT_RULES_SPECIAL(CNT), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 1 )
    CNT = 0
    DO I = 1, SIZE(IN_RULES%SPECIAL_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%SPECIAL_RULES(I)%FILTER, &
&         IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_REPRES, IN_LEVTYPE, &
&         IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        CNT = CNT + 1
        OUT_RULES_SPECIAL(CNT) = I
        ! CALL RULE_COPY( IN_RULES%SPECIAL_RULES(I), OUT_RULES(CNT) )
      ENDIF
    ENDDO

  ENDIF

  ! Default rules
  CNT = 0
  DO I = 1, SIZE(IN_RULES%DEFAULT_RULES)
    CALL MATCH_FILTER_ATM( IN_RULES%DEFAULT_RULES(I)%FILTER, &
&        IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_REPRES, IN_LEVTYPE, &
&        IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
    IF ( MATCH ) THEN
      CNT = CNT + 1
    ENDIF
  ENDDO

  IF ( CNT .GT. 0 ) THEN
    ALLOCATE( OUT_RULES_DEFAULT(CNT), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 2 )
    CNT = 0
    DO I = 1, SIZE(IN_RULES%DEFAULT_RULES)
      CALL MATCH_FILTER_ATM( IN_RULES%DEFAULT_RULES(I)%FILTER, &
&          IN_PARAMID, IN_DIRECTION, IN_FREQUENCY,  IN_REPRES, IN_LEVTYPE, &
&          IN_TAG, IN_ISANSAMBLE, IN_ISCHEMICAL, MATCH )
      IF ( MATCH ) THEN
        CNT = CNT + 1
        OUT_RULES_DEFAULT(CNT) = I
        ! CALL RULE_COPY( IN_RULES%DEFAULT_RULES(I), OUT_RULES(CNT) )
      ENDIF
    ENDDO

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
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating OUT_RULES_SPECIAL' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating OUT_RULES_SPECIAL: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (2)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating OUT_RULES_DEFAULT' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating OUT_RULES_DEFAULT: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MATCH_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUGENERAL_RULES'
__THREAD_SAFE__ SUBROUTINE SUGENERAL_RULES( CFG, RULES, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(GENERAL_RULES_T),      INTENT(OUT) :: RULES
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  TYPE(YAML_CONFIGURATION_T)  :: ENCODING_RULES_CFG
  TYPE(YAML_CONFIGURATIONS_T) :: DEFAULT_RULES_CFG
  TYPE(YAML_CONFIGURATIONS_T) :: SPECIAL_RULES_CFG
  LOGICAL :: CFG_HAS_ENCODING_RULES
  LOGICAL :: CFG_HAS_DEFAULT_RULES
  LOGICAL :: CFG_HAS_SPECIAL_RULES


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Confiuguration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'encoding-rules', CFG_HAS_ENCODING_RULES, VERBOSE )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CFG_HAS_ENCODING_RULES, 0 )

  ! Reading the encoding rules
  IF ( CFG_HAS_ENCODING_RULES ) THEN

    ! Read the encoding rules
    CALL YAML_GET_SUBCONFIGURATION( CFG, 'encoding-rules', ENCODING_RULES_CFG, VERBOSE )

    ! Configuration structure
    CALL YAML_CONFIGURATION_HAS_KEY( ENCODING_RULES_CFG,  'default-rules',  CFG_HAS_DEFAULT_RULES,  VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( ENCODING_RULES_CFG,  'special-rules',  CFG_HAS_SPECIAL_RULES,  VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.CFG_HAS_DEFAULT_RULES .AND. .NOT.CFG_HAS_SPECIAL_RULES, 1 )

    ! Read the default encoding rules (an array of rules that is supposed to be general)
    IF ( CFG_HAS_DEFAULT_RULES ) THEN

      ! Read default rules from configuration
      CALL YAML_GET_SUBCONFIGURATIONS( ENCODING_RULES_CFG, 'default-rules', DEFAULT_RULES_CFG, VERBOSE )

      ! Read default rules from configuration
      CALL READ_RULES( DEFAULT_RULES_CFG, RULES%DEFAULT_RULES, VERBOSE )

      ! Free rules array
      CALL YAML_DELETE_CONFIGURATIONS( DEFAULT_RULES_CFG, VERBOSE )

    ENDIF

    ! Read the special encoding rules (specific ancoding rules meant to cover some edge cases)
    IF ( CFG_HAS_SPECIAL_RULES ) THEN

      ! Read default rules from configuration
      CALL YAML_GET_SUBCONFIGURATIONS( ENCODING_RULES_CFG, 'special-rules', SPECIAL_RULES_CFG, VERBOSE )

      ! Read default rules from configuration
      CALL READ_RULES( SPECIAL_RULES_CFG, RULES%SPECIAL_RULES, VERBOSE )

      ! Free rules array
      CALL YAML_DELETE_CONFIGURATIONS( SPECIAL_RULES_CFG, VERBOSE )

    ENDIF

    ! Free rules array
    CALL YAML_DELETE_CONFIGURATION( ENCODING_RULES_CFG, VERBOSE )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Encoding rules not found in configuration file' )
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'empty encoding rules section found in configuration file' )
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

END SUBROUTINE SUGENERAL_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULES'
__THREAD_SAFE__ SUBROUTINE READ_RULES( CFG, RULES, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_REPLACE_ENVVAR_IN_STRING
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,  ONLY: YAML_GET_CONFIGURATION_BY_ID

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATIONS_T),                          INTENT(IN)  :: CFG
  TYPE(GENERAL_RULES_T),     DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: RULES
  LOGICAL,                                              INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: FILE_EXISTS
  LOGICAL :: RULE_HAS_FILE_KEY
  LOGICAL :: RULE_HAS_RULE_KEY
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N
  TYPE(YAML_CONFIGURATION_T) :: RULE_CFG
  TYPE(YAML_CONFIGURATION_T) :: CURR_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: YAMLFNAME
  CHARACTER(LEN=1024) :: YAMLFNAME_FULL
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization
  CURR_CFG => NULL()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(RULES), 0 )

  ! Get the rules size
  CALL YAML_GET_CONFIGURATIONS_SIZE( CFG, N, VERBOSE )

  ! Allocate memory for rules
  ALLOCATE( RULES(N), STATUS=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW(DEALLOC_STATUS .NE. 0, 1)

  ! Loop over the rules
  RulesLoop: DO I = 1, N

    ! Get the current configuration
    CALL YAML_GET_CONFIGURATION_BY_ID( CFG, I, CURR_CFG, VERBOSE )

    ! Check if the rule is defined in-place or in a separate file
    CALL YAML_CONFIGURATION_HAS_KEY( CURR_CFG, 'rule', RULE_HAS_RULE_KEY, VERBOSE )
    CALL YAML_CONFIGURATION_HAS_KEY( CURR_CFG, 'file', RULE_HAS_FILE_KEY, VERBOSE )

    PP_DEBUG_CRITICAL_COND_THROW(  RULE_HAS_RULE_KEY.AND.RULE_HAS_FILE_KEY, 6 )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.RULE_HAS_RULE_KEY .AND. .NOT.RULE_HAS_FILE_KEY, 5 )

    IF ( RULE_HAS_RULE_KEY ) THEN

      ! Read the rule from current file
      CALL READ_RULE( CURR_CFG, RULES(I), VERBOSE )

    ELSEIF ( RULE_HAS_FILE_KEY ) THEN

      ! Get the filename
      CALL YAML_READ_STRING( CURR_CFG, 'file', YAMLFNAME, VERBOSE )
      PP_DEBUG_COND_THROW( .NOT.ALLOCATED(YAMLFILENAME), 2 )

      ! Replace environment variables
      CALL OM_REPLACE_ENVVAR_IN_STRING( YAMLFNAME, YAMLFNAME_FULL )

      ! Free memory
      DEALLOCATE(YAMLFNAME, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW(DEALLOC_STATUS .NE. 0, 4)

      ! Check if the file exsts
      INQUIRE( FILE=TRIM(YAMLFNAME_FULL), EXIST=FILE_EXISTS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.FILE_EXISTS, 3 )

      ! Open the filename
      CALL YAML_NEW_CONFIGURATION_FROM_FILE( RULE_CFG, TRIM(YAMLFNAME_FULL), VERBOSE )

      ! Read the rule
      CALL READ_RULE( RULE_CFG, RULES(I), VERBOSE )

      ! Destroy the yaml configuration object
      CALL YAML_DELETE_CONFIGURATION( RULE_CFG, VERBOSE )
    ENDIF

  ENDDO RulesLoop

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rules array already allocated' )
    CASE (1)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating rules array' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error allocating rules array: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'yaml file name for rules not allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'yaml file name for rules doen not exist: '//TRIM(YAMLFNAME_FULL) )
    CASE (4)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating yaml file name for rules' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating yaml file name for rules: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unable to find valid keyword: expected one of: [rule|file]' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unexpeted both "rule" and "file" keys ' )
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

END SUBROUTINE READ_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE'
__THREAD_SAFE__ SUBROUTINE READ_RULE( CFG, RULE, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_FILTERS_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_FILTERS_UTILS_MOD, ONLY: READ_RULE_FILTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(GENERAL_RULE_T),       INTENT(OUT) :: RULE
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read Name
  CALL READ_RULE_NAME( CFG, RULE%NAME, VERBOSE )

  ! Read Tag
  CALL READ_RULE_TAG( CFG, RULE%TAG, VERBOSE )

  ! Read Filter
  CALL READ_RULE_FILTER( CFG, RULE%FILTER, VERBOSE )

  ! Read Mappings
  CALL READ_RULE_MAPPINGS( CFG, RULE%MAPPING, VERBOSE )

  ! Read Encoding Info
  CALL READ_RULE_ENCODING( CFG, RULE%ENCODING, VERBOSE )

  ! Read Packing Info
  CALL READ_RULE_PACKING( CFG, RULE%PACKING, VERBOSE )

  ! Read Sink Info
  CALL READ_RULE_SINK( CFG, RULE%SINK, VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FREE_RULES'
__THREAD_SAFE__ SUBROUTINE FREE_RULES( RULES )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GENERAL_RULES_T), INTENT(INOUT) :: RULES

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free default rules
  IF ( ALLOCATED(RULES%DEFAULT_RULES) ) THEN
    DO I = 1, SIZE(RULES%DEFAULT_RULES)
      CALL FREE_RULE( RULES%DEFAULT_RULES(I) )
    ENDDO
    DEALLOCATE( RULES%DEFAULT_RULES, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 0 )
  ENDIF

  ! Free special rules
  IF ( ALLOCATED(RULES%SPECIAL_RULES) ) THEN
    DO I = 1, SIZE(RULES%SPECIAL_RULES)
      CALL FREE_RULE( RULES%SPECIAL_RULES(I) )
    ENDDO
    DEALLOCATE( RULES%SPECIAL_RULES, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 1 )
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
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating DEFAULT_RULES' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating DEFAULT_RULES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (1)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating SPECIAL_RULES' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating SPECIAL_RULES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE FREE_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FREE_RULE'
__THREAD_SAFE__ SUBROUTINE FREE_RULE( RULE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GENERAL_RULE_T), INTENT(OUT) :: RULE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free default rules
  IF ( ALLOCATED(RULE%MAPPING%MAPS) ) THEN
    DEALLOCATE( RULE%MAPPING%MAPS, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 0 )
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
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating RULE%MAPPING%MAPS' )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'error deallocating RULE%MAPPING%MAPS: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE FREE_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_NAME'
__THREAD_SAFE__ SUBROUTINE READ_RULE_NAME( CFG, NAME, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  CHARACTER(LEN=128),         INTENT(OUT) :: NAME
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: RULE_HAS_RULE_KEY
  LOGICAL :: RULE_HAS_FILE_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: LOC_NAME
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check the configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'rule', RULE_HAS_RULE_KEY, VERBOSE )
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'file', RULE_HAS_FILE_KEY, VERBOSE )

  ! Check consistency
  PP_DEBUG_CRITICAL_COND_THROW( RULE_HAS_RULE_KEY .AND. RULE_HAS_FILE_KEY, 4 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.RULE_HAS_RULE_KEY .AND. .NOT.RULE_HAS_FILE_KEY, 3 )

  ! Read the name of the rule (usually the keyword "rule" is used for rules defined in-place,
  ! and the keyword "name" is used for rules defined in a separate file)
  IF ( RULE_HAS_RULE_KEY ) THEN
    ! Rules defined in-place
    CALL YAML_READ_STRING( CFG, '', LOC_NAME, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOC_NAME), 1 )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(LOC_NAME).GT.LEN(NAME), 2 )
    NAME = LOC_NAME
    DEALLOCATE(LOC_NAME, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, 5 )
  ELSEIF ( RULE_HAS_FILE_KEY ) THEN
    ! Rules defined in a separate file
    CALL YAML_READ_STRING( CFG, '', LOC_NAME, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOC_NAME), 1 )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(LOC_NAME).GT.LEN(NAME), 2 )
    NAME = LOC_NAME
    DEALLOCATE(LOC_NAME, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, 5 )
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rule name not allcoated' )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate NAME: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate NAME' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find the rule name: expected one of [rule|name]' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unexpected both "rule" and "name" set' )
    CASE (5)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate LOC_NAME: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate LOC_NAME' )
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

END SUBROUTINE READ_RULE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_TAG'
__THREAD_SAFE__ SUBROUTINE READ_RULE_TAG( CFG, TAG, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,    ONLY: YAML_CONFIGURATION_T
  USE :: YAML_FILTERS_UTILS_MOD, ONLY: TAG_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  CHARACTER(LEN=TAG_LEN),     INTENT(OUT) :: TAG
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: LOC_TAG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Rules defined in-place
  CALL YAML_READ_STRING( CFG, 'tag', LOC_TAG, VERBOSE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOC_TAG), 1 )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(LOC_TAG).GT.LEN(TAG), 2 )
  TAG = LOC_TAG
  DEALLOCATE(LOC_TAG, STATUS=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 3 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Rule name not allcoated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Tag name too long' )
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate LOC_TAG: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate LOC_TAG' )
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

END SUBROUTINE READ_RULE_TAG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


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



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_ENCODING'
__THREAD_SAFE__ SUBROUTINE READ_RULE_ENCODING( CFG, ENCODING, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(ENCODING_RULE_T),      INTENT(OUT) :: ENCODING
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration to allow using paramIDECMF for fields that are not already defined in eccodes
  CALL READ_RULE_ENCODING_USEPARAMIDECMF(  CFG, ENCODING%USEPARAMIDECMF,  VERBOSE )

  ! Select the grib edition for the field to be encoded
  CALL READ_RULE_ENCODING_EDITION(  CFG, ENCODING%EDITION,  VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_ENCODING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_PACKING'
__THREAD_SAFE__ SUBROUTINE READ_RULE_PACKING( CFG, PACKING, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(PACKING_RULE_T),       INTENT(OUT) :: PACKING
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read the packing type
  CALL READ_RULE_PACKING_PACKING_TYPE(  CFG, ENCODING%PACKING_TYPE,  VERBOSE )

  ! Read the bits per value
  CALL READ_RULE_PACKING_BITS_PER_VALUE(  CFG, ENCODING%BITS_PER_VALUE,  VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_PACKING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_SINK'
__THREAD_SAFE__ SUBROUTINE READ_RULE_SINK( CFG, PACKING, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(SINK_RULE_T),          INTENT(OUT) :: SINK
  LOGICAL,                    INTENT(IN)  :: VERBOSE


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read the packing type
  CALL READ_RULE_SINK_DIRECT_TO_FDB(  CFG, SINK%DIRECT_TO_FDB,  VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_SINK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_ENCODING_USEPARAMIDECMF'
__THREAD_SAFE__ SUBROUTINE READ_RULE_ENCODING_USEPARAMIDECMF( CFG, USEPARAMIDECMF, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  LOGICAL,                    INTENT(OUT) :: USEPARAMIDECMF
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: CONFIGURATION_HAS_USEPARAMIDECMF

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! REpository structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'use-paramId-ECMF', CONFIGURATION_HAS_USEPARAMIDECMF, VERBOSE )

  ! Read the local definition template number
  IF ( CONFIGURATION_HAS_USEPARAMIDECMF ) THEN
    CALL YAML_READ_INTEGER( CFG, 'use-paramId-ECMF', USEPARAMIDECMF, VERBOSE )
  ELSE
    USEPARAMIDECMF = DEFAULT_USEPARAMIDECMF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_ENCODING_USEPARAMIDECMF
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_SINK_DIRECT_TO_FDB'
__THREAD_SAFE__ SUBROUTINE READ_RULE_SINK_DIRECT_TO_FDB( CFG, DIRECT_TO_FDB, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  LOGICAL,                    INTENT(OUT) :: DIRECT_TO_FDB
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: CONFIGURATION_HAS_DIRECT_TO_FDB

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Repository structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'direct-to-fdb', CONFIGURATION_HAS_DIRECT_TO_FDB, VERBOSE )

  ! Read the local definition template number
  IF ( CONFIGURATION_HAS_DIRECT_TO_FDB ) THEN
    CALL YAML_READ_INTEGER( CFG, 'direct-to-fdb', DIRECT_TO_FDB, VERBOSE )
  ELSE
    DIRECT_TO_FDB = DEFAULT_DIRECT_TO_FDB
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_SINK_DIRECT_TO_FDB
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_PACKING_PACKING_TYPE'
__THREAD_SAFE__ SUBROUTINE READ_RULE_PACKING_PACKING_TYPE( CFG, PACKINGTYPE, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K),       INTENT(OUT) :: PACKINGTYPE
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ATMP
  LOGICAL :: CONFIGURATION_HAS_PACKINGTYPE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'packing-type', CONFIGURATION_HAS_PACKINGTYPE, VERBOSE )

  ! Read the local definition template number
  IF ( CONFIGURATION_HAS_PACKINGTYPE ) THEN

    ! Read teh configuration variable
    CALL YAML_READ_STRING( CFG, 'packing-type', ATMP, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), 0 )

    ! Handle configuration cases
    SELECT CASE (CLTMP)
    CASE ('grid_simple')
      PACKINGTYPE = PACKING_TYPE_GRIB_SIMPLE_E
    CASE ('spectral_complex')
      PACKINGTYPE = PACKING_TYPE_GRIB_COMPLEX_E
    CASE ('grid_ccsds')
      PACKINGTYPE = PACKING_TYPE_GRIB_CCSDE_E
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 2 )
    END SELECT
    DEALLOCATE(ATMP,STAT=DEALLOC_STATUS,ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 3 )
  ELSE
    PACKINGTYPE = UNDEF_PARAM_E
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
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'packing-type is not allocated after read' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown option for packing-type: '//TRIM(ADJUSTL(ATMP)) )
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate packing-type: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate packing-type' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Free memory
    IF ( ALLOCATED(ATMP) ) DEALLOCATE(ATMP)

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_PACKING_PACKING_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_PACKING_BITS_PER_VALUE'
__THREAD_SAFE__ SUBROUTINE READ_RULE_PACKING_BITS_PER_VALUE( CFG, BITS_PER_VALUE, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: STRING_TO_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K),       INTENT(OUT) :: ENCODINGEDITION
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ATMP
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  LOGICAL :: CONFIGURATION_HAS_BITS_PER_VALUE
  LOGICAL :: IS_INTEGER

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'bits-per-value', CONFIGURATION_HAS_BITS_PER_VALUE, VERBOSE )

  ! Read the bits per value to be used to encode the current field
  IF ( CONFIGURATION_HAS_BITS_PER_VALUE ) THEN

    ! Read the bits per value
    CALL YAML_READ_STRING( CFG, 'bits-per-value', ATMP, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), 0 )

    ! Check if the value is an integer
    CALL STRING_IS_INTEGER( ATMP, IS_INTEGER, VERBOSE )

    ! Depending on the value of the bits-per-value, set the number of bits per value
    IF ( IS_INTEGER ) THEN

      ! Read the integer value
      CALL STRING_TO_INTEGER( ATMP, BITS_PER_VALUE, VERBOSE )
      PP_DEBUG_CRITICAL_COND_THROW( BITS_PER_VALUE.LT.BITS_PER_VALUE_MIN, 1 )
      PP_DEBUG_CRITICAL_COND_THROW( BITS_PER_VALUE.GT.BITS_PER_VALUE_MAX, 2 )

    ELSE

      ! Handle more general configurations
      SELECT CASE( TRIM(ATMP) )
      CASE ( 'use-default-table' )
        BITS_PER_VALUE = BITS_PER_VALUE_DEFAULT_TABLE
      CASE ( 'use-compressed-table' )
        BITS_PER_VALUE = BITS_PER_VALUE_COMPRESSED_TABLE
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( 3 )
      END SELECT

    ENDIF

    ! Free meomry
    DEALLOCATE(ATMP,STAT=DEALLOC_STATUS,ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, 4 )

  ELSE
    BITS_PER_VALUE = UNDEF_PARAM_E
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
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (0)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'bits-per-value is not allocated after read' )
    CASE (1)
      TMP1=REPEAT(' ',32)
      TMP2=REPEAT(' ',32)
      WRITE(TMP1,'(I10)') BITS_PER_VALUE_MIN
      WRITE(TMP2,'(I10)') BITS_PER_VALUE
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'BitsPerValue lower than minimal possible value: minimal allowed='//TRIM(ADJUSTL(TMP1))//' - current='//TRIM(ADJUSTL(TMP2)) )
    CASE (2)
      TMP1=REPEAT(' ',32)
      TMP2=REPEAT(' ',32)
      WRITE(TMP1,'(I10)') BITS_PER_VALUE_MAX
      WRITE(TMP2,'(I10)') BITS_PER_VALUE
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'BitsPerValue higher than maximum possible value: maximum allowed='//TRIM(ADJUSTL(TMP1))//' - current='//TRIM(ADJUSTL(TMP2)) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown option for bits-per-value: '//TRIM(ADJUSTL(ATMP)) )
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate bits-per-value: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate bits-per-value' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Free memory
    IF ( ALLOCATED(ATMP) ) DEALLOCATE(ATMP)

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_PACKING_BITS_PER_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_ENCODING_ENCODINGEDITION'
__THREAD_SAFE__ SUBROUTINE READ_RULE_ENCODING_ENCODINGEDITION( CFG, ENCODINGEDITION, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K),       INTENT(OUT) :: ENCODINGEDITION
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: CONFIGURATION_HAS_ENCODINGEDITION


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'edition', CONFIGURATION_HAS_ENCODINGEDITION, VERBOSE )

  ! Read the local definition template number
  IF ( CONFIGURATION_HAS_ENCODINGEDITION ) THEN
    CALL YAML_READ_INTEGER( CFG, 'edition', ENCODINGEDITION, VERBOSE )
  ELSE
    ENCODINGEDITION = UNDEF_PARAM_E
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE READ_RULE_ENCODING_ENCODINGEDITION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_RULE_GRIB_STRUCTURE'
__THREAD_SAFE__ SUBROUTINE READ_RULE_GRIB_STRUCTURE( CFG, GRIB_STRUCTURE, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(GRIB_STRUCTURE_T),     INTENT(OUT) :: GRIB_STRUCTURE
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Local variables
  LOGICAL :: CONFIGURATION_HAS_LOCAL_DEFINITION_TEMPLATE_NUMBER
  LOGICAL :: CONFIGURATION_HAS_GRID_DEFINITION_TEMPLATE_NUMBER
  LOGICAL :: CONFIGURATION_HAS_PRODUCT_DEFINITION_TEMPLATE_NUMBER
  LOGICAL :: CONFIGURATION_HAS_DATA_DEFINITION_TEMPLATE_NUMBER

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configuration structure
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'local-definition-template-number',   CONFIGURATION_HAS_LOCAL_DEFINITION_TEMPLATE_NUMBER,   VERBOSE )
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'grid-definition-template-number',    CONFIGURATION_HAS_GRID_DEFINITION_TEMPLATE_NUMBER,    VERBOSE )
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'product-definition-template-number', CONFIGURATION_HAS_PRODUCT_DEFINITION_TEMPLATE_NUMBER, VERBOSE )
  CALL YAML_CONFIGURATION_HAS_KEY( CFG, 'data-definition-template-number',    CONFIGURATION_HAS_DATA_DEFINITION_TEMPLATE_NUMBER,    VERBOSE )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_LOCAL_DEFINITION_TEMPLATE_NUMBER,   1 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_GRID_DEFINITION_TEMPLATE_NUMBER,    2 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_PRODUCT_DEFINITION_TEMPLATE_NUMBER, 3 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.CONFIGURATION_HAS_DATA_DEFINITION_TEMPLATE_NUMBER,    4 )

  ! Read the local definition template number
  CALL YAML_READ_INTEGER( CFG, 'local-definition-template-number', GRIB_STRUCTURE%LOCAL_DEFINITION_TEMPLATE_NUMBER, VERBOSE )

  ! Read the local definition template number
  CALL YAML_READ_INTEGER( CFG, 'grid-definition-template-number', GRIB_STRUCTURE%GRID_DEFINITION_TEMPLATE_NUMBER, VERBOSE )

  ! Read the local definition template number
  CALL YAML_READ_INTEGER( CFG, 'product-definition-template-number', GRIB_STRUCTURE%PRODUCT_DEFINITION_TEMPLATE_NUMBER, VERBOSE )

  ! Read the local definition template number
  CALL YAML_READ_INTEGER( CFG, 'data-definition-template-number', GRIB_STRUCTURE%DATA_DEFINITION_TEMPLATE_NUMBER, VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

!$omp critical(error_handler)
  ErrorHandler: BLOCK
    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"local-definition-template-number" not found' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"grid-definition-template-number" not found' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"product-definition-template-number" not found' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"data-definition-template-number" not found' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler
!$omp end critical(error_handler)

  ! Exit point on error
  RETURN

END SUBROUTINE READ_RULE_GRIB_STRUCTURE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GENERAL_RULES_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME