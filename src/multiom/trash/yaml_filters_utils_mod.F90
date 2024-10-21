!>
!> @file yaml_filter_utils_mod.f90
!>
!> @brief Module containing procedures for reading and matching filter rules.
!>
!> This module provides various procedures to read and process filter rules
!> from a YAML configuration, and to match these rules against provided parameters.
!> Each procedure handles different aspects of the filter rules, including levels,
!> directions, frequencies, representations, level types, and other properties.
!>
!> @section Public DataTypes
!> - @ref FILTER_RULES_T
!>
!> @section Public Parameters
!> - @ref TAG_LEN
!>
!> @section Public Procedures
!> - @ref READ_RULE_FILTER
!> - @ref FREE_RULE_FILTER
!> - @ref MATCH_RULE_FILTER_ATM
!> - @ref MATCH_RULE_FILTER_WAM
!>
!>
!> @section Private Parameters
!> - @ref ISCHEMICAL_KEY
!> - @ref ISENSEMBLE_KEY
!> - @ref PARAMID_KEY
!> - @ref LEVEL_KEY
!> - @ref DIRECTION_KEY
!> - @ref FREQUENCY_KEY
!> - @ref REPRES_KEY
!> - @ref LEVTYPE_KEY
!> - @ref TAG_KEY
!>
!> @section Private Procedures
!>
!> - @ref READ_RULE_FILTER_PARAMID
!> - @ref READ_RULE_FILTER_LEVEL
!> - @ref READ_RULE_FILTER_DIRECTION
!> - @ref READ_RULE_FILTER_FREQUENCY
!> - @ref READ_RULE_FILTER_REPRES
!> - @ref READ_RULE_FILTER_LEVTYPE
!> - @ref READ_RULE_FILTER_TAG
!> - @ref READ_RULE_FILTER_ISCHEMICAL
!> - @ref READ_RULE_FILTER_ISENSEMBLE
!>
!> @section Dependencies
!>
!> @subsection local_dependencies
!> - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!> - @dependency [PROCEDURE] "OM_CORE_MOD::CLEVTYPE2ILEVTYPE"
!> - @dependency [TYPE]      "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_RANGES"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_FILTER"
!> - @dependency [INTERFACE] "YAML_CORE_UTILS_MOD::FUN_C2I_IF"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_LOGICAL"
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'yaml_filter_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MODULE YAML_FILTERS_UTILS_MOD'
MODULE YAML_FILTERS_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default symbols visibility
PRIVATE


!>
!> Parameters

!> Maximum length of a tag
INTEGER(KIND=JPIB_K), PARAMETER :: TAG_LEN=32_JPIB_K

!> Key used to read the filter rule for the `is-chemical` property
CHARACTER(LEN=*), PARAMETER :: ISCHEMICAL_KEY='is-chemical'

!> Key used to read the filter rule for the `is-ensemble` property
CHARACTER(LEN=*), PARAMETER :: ISENSEMBLE_KEY='is-ensemble'

!> Key used to read the filter rule for the `paramid` property
CHARACTER(LEN=*), PARAMETER :: PARAMID_KEY='paramId'

!> Key used to read the filter rule for the `level` property
CHARACTER(LEN=*), PARAMETER :: LEVEL_KEY='level'

!> Key used to read the filter rule for the `direction` property
CHARACTER(LEN=*), PARAMETER :: DIRECTION_KEY='direction'

!> Key used to read the filter rule for the `frequency` property
CHARACTER(LEN=*), PARAMETER :: FREQUENCY_KEY='frequency'

!> Key used to read the filter rule for the `repres` property
CHARACTER(LEN=*), PARAMETER :: REPRES_KEY='repres'

!> Key used to read the filter rule for the `levtype` property
CHARACTER(LEN=*), PARAMETER :: LEVTYPE_KEY='levtype'

!> Key used to read the filter rule for the `tag` property
CHARACTER(LEN=*), PARAMETER :: TAG_KEY='tag'


!>
!> @class Rules used to match fields
TYPE :: FILTER_RULES_T

  !> Default visibility of the members
  PRIVATE

  !> Logical flag indicating whether the `levtype` field is present in the filter
  LOGICAL :: HAS_LEVTYPE = .FALSE.

  !> Logical flag indicating whether the `level` field is present in the filter
  LOGICAL :: HAS_LEVEL = .FALSE.

  !> Logical flag indicating whether the `direction` field is present in the filter
  LOGICAL :: HAS_DIRECTION = .FALSE.

  !> Logical flag indicating whether the `frequency` field is present in the filter
  LOGICAL :: HAS_FREQUENCY = .FALSE.

  !> Logical flag indicating whether the `repres` field is present in the filter
  LOGICAL :: HAS_REPRES = .FALSE.

  !> Logical flag indicating whether the `paramId` field is present in the filter
  LOGICAL :: HAS_PARAMID = .FALSE.

  !> Logical flag indicating whether the `tag` field is present in the filter
  LOGICAL :: HAS_TAG = .FALSE.

  !> Logical flag indicating whether the `is-chemical` field is present in the filter
  LOGICAL :: HAS_IS_ENSEMBLE = .FALSE.

  !> Logical flag indicating whether the `is-ensemble` field is present in the filter
  LOGICAL :: HAS_IS_CHEMICAL = .FALSE.


  !> Array of `levtype` enumerators to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: LEVTYPE

  !> Array of `level` values to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: LEVEL

  !> Array of `direction` values to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: DIRECTION

  !> Array of `frequency` values to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: FREQUENCY

  !> Array of `repres` values to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: REPRES

  !> Array of `paramId` values to match
  INTEGER(KIND=JPIB_K),   DIMENSION(:), ALLOCATABLE :: PARAMID

  !> Array of `tag` values to match
  CHARACTER(LEN=TAG_LEN), DIMENSION(:), ALLOCATABLE :: TAG

  !> Value of `is-ensemble` to match
  INTEGER(KIND=JPIB_K) :: IS_ENSEMBLE = .FALSE.

  !> Value of `is-chemical` to match
  INTEGER(KIND=JPIB_K) :: IS_CHEMICAL = .FALSE.
END TYPE


!> Exposed symbols

!> Whitelist of public symbols (datatypes)
PUBLIC :: FILTER_RULES_T

!> Whitelist of public symbols (procedures)
PUBLIC :: READ_RULE_FILTER
PUBLIC :: MATCH_RULE_FILTER_WAM
PUBLIC :: MATCH_RULE_FILTER_ATM
PUBLIC :: FREE_RULEFILTER

!> Whitelist of public symbols (parameters)
PUBLIC :: TAG_LEN

CONTAINS


!>
!> @brief Reads filtering rules from a YAML configuration and populates a filter structure.
!>
!> This function reads the filtering rules from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER` structure with the parsed rules. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function also supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during the execution.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the filter rules are read.
!> @param [out] FILTER The structure that will be populated with the parsed filtering rules.
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_PARAMID"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_LEVEL"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_DIRECTION"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_FREQUENCY"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_REPRES"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_LEVTYPE"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_TAG"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_ISENSEMBLE"
!>   - @dependency [PROCEDURE] "READ_RULE_FILTER_ISCHEMICAL"
!>
!> @subsubsection local dependencies
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_PARAMID_ALLOCATED` (1): Error allocating `PARAMID`
!>   - `ERRFLAG_LEVEL_ALLOCATED` (2): Error allocating `LEVEL`
!>   - `ERRFLAG_DIRECTION_ALLOCATED` (3): Error allocating `DIRECTION`
!>   - `ERRFLAG_FREQUENCY_ALLOCATED` (4): Error allocating `FREQUENCY`
!>   - `ERRFLAG_REPRES_ALLOCATED` (5): Error allocating `REPRES`
!>   - `ERRFLAG_LEVTYPE_ALLOCATED` (6): Error allocating `LEVTYPE`
!>   - `ERRFLAG_TAG_ALLOCATED` (7): Error allocating `TAG`
!>   - `ERRFLAG_NESTED_READ_PARAMID` (100): Nested read error for `PARAMID`
!>   - `ERRFLAG_NESTED_READ_LEVEL` (101): Nested read error for `LEVEL`
!>   - `ERRFLAG_NESTED_READ_DIRECTION` (102): Nested read error for `DIRECTION`
!>   - `ERRFLAG_NESTED_READ_FREQUENCY` (103): Nested read error for `FREQUENCY`
!>   - `ERRFLAG_NESTED_READ_REPRES` (104): Nested read error for `REPRES`
!>   - `ERRFLAG_NESTED_READ_LEVTYPE` (105): Nested read error for `LEVTYPE`
!>   - `ERRFLAG_NESTED_READ_TAG` (106): Nested read error for `TAG`
!>   - `ERRFLAG_NESTED_READ_ISENSEMBLE` (107): Nested read error for `ISENSEMBLE`
!>   - `ERRFLAG_NESTED_READ_ISCHEMICAL` (108): Nested read error for `ISCHEMICAL`
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER( CFG, FILTER, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  TYPE(FILTER_RULES_T),       INTENT(OUT) :: FILTER
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_PARAMID_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVEL_ALLOCATED=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DIRECTION_ALLOCATED=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_FREQUENCY_ALLOCATED=4_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_REPRES_ALLOCATED=5_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVTYPE_ALLOCATED=6_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_TAG_ALLOCATED=7_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_PARAMID=100_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_LEVEL=101_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_DIRECTION=102_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_FREQUENCY=103_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_REPRES=104_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_LEVTYPE=105_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_TAG=106_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_ISENSEMBLE=107_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_NESTED_READ_ISCHEMICAL=108_ERR_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%PARAMID), ERRFLAG_PARAMID_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%LEVEL), ERRFLAG_LEVEL_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%DIRECTION), ERRFLAG_DIRECTION_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%FREQUENCY), ERRFLAG_FREQUENCY_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%REPRES), ERRFLAG_REPRES_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%LEVTYPE), ERRFLAG_LEVTYPE_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(FILTER%TAG), ERRFLAG_TAG_ALLOCATED )

  ! Read the filter rules
  PP_TRYCALL(ERRFLAG_NESTED_READ_PARAMID) READ_RULE_FILTER_PARAMID( CFG, FILTER%PARAMID, FILTER%HAS_PARAMID, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_LEVEL) READ_RULE_FILTER_LEVEL( CFG, FILTER%LEVEL, FILTER%HAS_LEVEL, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_DIRECTION) READ_RULE_FILTER_DIRECTION( CFG, FILTER%DIRECTION, FILTER%HAS_DIRECTION, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_FREQUENCY) READ_RULE_FILTER_FREQUENCY( CFG, FILTER%FREQUENCY, FILTER%HAS_FREQUENCY, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_REPRES) READ_RULE_FILTER_REPRES( CFG, FILTER%REPRES, FILTER%HAS_REPRES, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_LEVTYPE) READ_RULE_FILTER_LEVTYPE( CFG, FILTER%LEVTYPE, FILTER%HAS_LEVTYPE, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_TAG) READ_RULE_FILTER_TAG( CFG, FILTER%TAG, FILTER%HAS_TAG, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_ISENSEMBLE) READ_RULE_FILTER_ISENSEMBLE( CFG, FILTER%ISENSEMBLE, FILTER%HAS_ISENSEMBLE, VERBOSE )
  PP_TRYCALL(ERRFLAG_NESTED_READ_ISCHEMICAL) READ_RULE_FILTER_ISCHEMICAL( CFG, FILTER%ISCHEMICAL, FILTER%HAS_ISCHEMICAL, VERBOSE )

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARAMID_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%PARAMID already allocated allocated' )
    CASE (ERRFLAG_LEVEL_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%LEVEL already allocated allocated' )
    CASE (ERRFLAG_DIRECTION_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%DIRECTION already allocated allocated' )
    CASE (ERRFLAG_FREQUENCY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%FREQUENCY already allocated allocated' )
    CASE (ERRFLAG_REPRES_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%REPRES already allocated allocated' )
    CASE (ERRFLAG_LEVTYPE_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%LEVTYPE already allocated allocated' )
    CASE (ERRFLAG_TAG_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER%TAG already allocated allocated' )
    CASE (ERRFLAG_NESTED_READ_PARAMID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_PARAMID' )
    CASE (ERRFLAG_NESTED_READ_LEVEL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_LEVEL' )
    CASE (ERRFLAG_NESTED_READ_DIRECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_DIRECTION' )
    CASE (ERRFLAG_NESTED_READ_FREQUENCY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_FREQUENCY' )
    CASE (ERRFLAG_NESTED_READ_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_REPRES' )
    CASE (ERRFLAG_NESTED_READ_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_LEVTYPE' )
    CASE (ERRFLAG_NESTED_READ_TAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_TAG' )
    CASE (ERRFLAG_NESTED_READ_ISENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_ISENSEMBLE' )
    CASE (ERRFLAG_NESTED_READ_ISCHEMICAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: READ_RULE_FILTER_ISCHEMICAL' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees all resources associated with the filtering structure.
!>
!> This function deallocates memory for all fields within the provided `FILTER` structure.
!> It ensures that all dynamically allocated resources within `FILTER` are properly freed
!> and that no memory leaks occur. The function operates in a thread-safe manner and can
!> optionally run in verbose mode.
!>
!> @section interface
!> @param [inout] FILTER The filter structure whose resources are to be deallocated.
!>                       After completion, this structure will have no allocated resources.
!> @param [in]    VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                        for debugging purposes.
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE] "FILTER_RULES_T"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_DEALLOC_PARAMID` (1): Error deallocating `PARAMID`
!>   - `ERRFLAG_DEALLOC_LEVEL` (2): Error deallocating `LEVEL`
!>   - `ERRFLAG_DEALLOC_DIRECTION` (3): Error deallocating `DIRECTION`
!>   - `ERRFLAG_DEALLOC_FREQUENCY` (4): Error deallocating `FREQUENCY`
!>   - `ERRFLAG_DEALLOC_REPRES` (5): Error deallocating `REPRES`
!>   - `ERRFLAG_DEALLOC_LEVTYPE` (6): Error deallocating `LEVTYPE`
!>   - `ERRFLAG_DEALLOC_TAG` (7): Error deallocating `TAG`
!>
!> @see FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_RULE_FILTER'
__THREAD_SAFE__ FUNCTION FREE_RULE_FILTER( FILTER, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
   TYPE(FILTER_RULES_T), INTENT(INOUT) :: FILTER
  LOGICAL,               INTENT(IN)    :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_PARAMID=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_LEVEL=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_DIRECTION=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_FREQUENCY=4_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_REPRES=5_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_LEVTYPE=6_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DEALLOC_TAG=7_ERR_K

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

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

  ! Deallocate paramId
  IF ( FILTER%HAS_PARAMID ) THEN
    DEALLOCATE( FILTER%PARAMID, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_PARAMID )
    FILTER%HAS_PARAMID = .FALSE.
  ENDIF

  ! Deallocate level
  IF ( FILTER%HAS_LEVEL ) THEN
    DEALLOCATE( FILTER%LEVEL, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_LEVEL )
    FILTER%HAS_LEVEL = .FALSE.
  ENDIF

  ! Deallocate direction
  IF ( FILTER%HAS_DIRECTION ) THEN
    DEALLOCATE( FILTER%DIRECTION, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_DIRECTION )
    FILTER%HAS_DIRECTION = .FALSE.
  ENDIF

  ! Deallocate frequency
  IF ( FILTER%HAS_FREQUENCY ) THEN
    DEALLOCATE( FILTER%FREQUENCY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_FREQUENCY )
    FILTER%HAS_FREQUENCY = .FALSE.
  ENDIF

  ! Deallocate repres
  IF ( FILTER%HAS_REPRES ) THEN
    DEALLOCATE( FILTER%REPRES, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_REPRES )
    FILTER%HAS_REPRES = .FALSE.
  ENDIF

  ! Deallocate levtype
  IF ( FILTER%HAS_LEVTYPE ) THEN
    DEALLOCATE( FILTER%LEVTYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_LEVTYPE )
    FILTER%HAS_LEVTYPE = .FALSE.
  ENDIF

  ! Deallocate tag
  IF ( FILTER%HAS_TAG ) THEN
    DEALLOCATE( FILTER%TAG, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_TAG )
    FILTER%HAS_TAG = .FALSE.
  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DEALLOC_PARAMID)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating PARAMID' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating PARAMID: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_LEVEL)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating LEVEL' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating LEVEL: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_DIRECTION)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating DIRECTION' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating DIRECTION: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_FREQUENCY)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating FREQUENCY' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating FREQUENCY: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_REPRES)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating REPRES' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating REPRES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_LEVTYPE)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating LEVTYPE' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating LEVTYPE: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_DEALLOC_TAG)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating TAG' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating TAG: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
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

  ! Exit point on error
  RETURN

END FUNCTION FREE_RULE_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering parameter IDs from a YAML configuration and updates the parameter list.
!>
!> This function extracts parameter IDs from the provided YAML configuration object (`CFG`)
!> and populates the `FILTER_PARAMID` array with these IDs. It also sets the logical flag `HAS_PARAMID`
!> to indicate whether any parameter IDs were found. The function operates in a thread-safe manner and
!> can optionally run in verbose mode.
!>
!> @section interface
!> @param [in]  CFG            The YAML configuration object from which parameter IDs are read.
!> @param [out] FILTER_PARAMID The array that will be populated with the extracted parameter IDs.
!> @param [out] HAS_PARAMID    Logical flag that will be set to `.TRUE.` if parameter IDs are found;
!>                             `.FALSE.` otherwise.
!> @param [in]  VERBOSE        Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                             for debugging purposes.
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "PARAMID_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_RANGES"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_PARAMID_NOT_ALLOCATED` (1): Error if `FILTER_PARAMID` could not be allocated.
!>   - `ERRFLAG_PARAMID_EMPTY` (2): Error if `FILTER_PARAMID` is empty after reading.
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Error if the expected key is not present in the configuration.
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Error if reading the key from the configuration fails.
!>
!> @see YAML_CONFIGURATION_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_PARAMID'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_PARAMID( CFG, FILTER_PARAMID, HAS_PARAMID, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_RANGES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_PARAMID
  LOGICAL,                                         INTENT(OUT) :: HAS_PARAMID
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_PARAMID_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_PARAMID_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, PARAMID_KEY, HAS_PARAMID, VERBOSE )

  ! Read the paramId
  IF ( HAS_PARAMID ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_INTEGER_ARRAY_WITH_RANGES( CFG, PARAMID_KEY, FILTER_PARAMID, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_PARAMID), ERRFLAG_PARAMID_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_PARAMID).LT.1, ERRFLAG_PARAMID_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARAMID_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_PARAMID not allocated' )
    CASE (ERRFLAG_PARAMID_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_PARAMID not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_PARAMID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering levels from a YAML configuration and populates a level structure.
!>
!> This function reads filtering levels from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_LEVEL` array with the parsed levels. It also indicates whether
!> the levels were successfully populated through the `HAS_LEVEL` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter levels are read.
!> @param [out] FILTER_LEVEL The array that will be populated with the parsed filtering levels.
!> @param [out] HAS_LEVEL Logical flag indicating whether the levels were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "LEVEL_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_RANGES"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_LEVEL_NOT_ALLOCATED` (1): Error allocating `LEVEL`
!>   - `ERRFLAG_LEVEL_EMPTY` (2): Error when `LEVEL` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_LEVEL'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_LEVEL( CFG, FILTER_LEVEL, HAS_LEVEL, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_RANGES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_LEVEL
  LOGICAL,                                         INTENT(OUT) :: HAS_LEVEL
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVEL_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVEL_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, LEVEL_KEY, HAS_LEVEL, VERBOSE )

  ! Read the paramId
  IF ( HAS_LEVEL ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_INTEGER_ARRAY_WITH_RANGES( CFG, LEVEL_KEY, FILTER_LEVEL, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_LEVEL), ERRFLAG_LEVEL_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_LEVEL).LT.1, ERRFLAG_LEVEL_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_LEVEL_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_LEVEL not allocated' )
    CASE (ERRFLAG_LEVEL_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_LEVEL not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_LEVEL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering directions from a YAML configuration and populates a direction structure.
!>
!> This function reads filtering directions from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_DIRECTION` array with the parsed directions. It also indicates whether
!> the directions were successfully populated through the `HAS_DIRECTION` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter directions are read.
!> @param [out] FILTER_DIRECTION The array that will be populated with the parsed filtering directions.
!> @param [out] HAS_DIRECTION Logical flag indicating whether the directions were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "DIRECTION_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_RANGES"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_DIRECTION_NOT_ALLOCATED` (1): Error allocating `DIRECTION`
!>   - `ERRFLAG_DIRECTION_EMPTY` (2): Error when `DIRECTION` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_DIRECTION'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_DIRECTION( CFG, FILTER_DIRECTION, HAS_DIRECTION, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_RANGES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_DIRECTION
  LOGICAL,                                         INTENT(OUT) :: HAS_DIRECTION
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DIRECTION_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_DIRECTION_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, DIRECTION_KEY, HAS_DIRECTION, VERBOSE )

  ! Read the paramId
  IF ( HAS_DIRECTION ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED)   YAML_READ_INTEGER_ARRAY_WITH_RANGES( CFG, DIRECTION_KEY, FILTER_DIRECTION, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_DIRECTION), ERRFLAG_DIRECTION_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_DIRECTION).LT.1, ERRFLAG_DIRECTION_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DIRECTION_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_DIRECTION not allocated' )
    CASE (ERRFLAG_DIRECTION_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_DIRECTION not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_DIRECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering frequencies from a YAML configuration and populates a frequency structure.
!>
!> This function reads filtering frequencies from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_FREQUENCY` array with the parsed frequencies. It also indicates whether
!> the frequencies were successfully populated through the `HAS_FREQUENCY` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter frequencies are read.
!> @param [out] FILTER_FREQUENCY The array that will be populated with the parsed filtering frequencies.
!> @param [out] HAS_FREQUENCY Logical flag indicating whether the frequencies were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "FREQUENCY_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_RANGES"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_FREQUENCY_NOT_ALLOCATED` (1): Error allocating `FREQUENCY`
!>   - `ERRFLAG_FREQUENCY_EMPTY` (2): Error when `FREQUENCY` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_FREQUENCY'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_FREQUENCY( CFG, FILTER_FREQUENCY, HAS_FREQUENCY, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  P_USE('P') :: OM_CORE_MOD, ONLY: JPIB_K
  P_USE('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  P_USE('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  P_USE('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_RANGES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_FREQUENCY
  LOGICAL,                                         INTENT(OUT) :: HAS_FREQUENCY
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_FREQUENCY_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_FREQUENCY_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT)  YAML_CONFIGURATION_HAS_KEY( CFG, FREQUENCY_KEY, HAS_FREQUENCY, VERBOSE )

  ! Read the paramId
  IF ( HAS_FREQUENCY ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_INTEGER_ARRAY_WITH_RANGES( CFG, FREQUENCY_KEY, FILTER_FREQUENCY, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_FREQUENCY), ERRFLAG_FREQUENCY_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_FREQUENCY).LT.1, ERRFLAG_FREQUENCY_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_FREQUENCY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_FREQUENCY not allocated' )
    CASE (ERRFLAG_FREQUENCY_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_FREQUENCY not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_FREQUENCY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering representations from a YAML configuration and populates a representation structure.
!>
!> This function reads filtering representations from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_REPRES` array with the parsed representations. It also indicates whether
!> the representations were successfully populated through the `HAS_REPRES` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter representations are read.
!> @param [out] FILTER_REPRES The array that will be populated with the parsed filtering representations.
!> @param [out] HAS_REPRES Logical flag indicating whether the representations were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "REPRES_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_FILTER"
!>   - @dependency [INTERFACE] "YAML_CORE_UTILS_MOD::FUN_C2I_IF"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_REPRES_NOT_ALLOCATED` (1): Error allocating `REPRES`
!>   - `ERRFLAG_REPRES_EMPTY` (2): Error when `REPRES` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_REPRES'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_REPRES( CFG, FILTER_REPRES, HAS_REPRES, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_FILTER
  PP_USE_L('I') :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_REPRES
  LOGICAL,                                         INTENT(OUT) :: HAS_REPRES
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_REPRES_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_REPRES_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

  ! Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CREPRES2IREPRES

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, REPRES_KEY, HAS_REPRES, VERBOSE )

  ! Read the paramId
  IF ( HAS_REPRES ) THEN

    ! Associtate the conversion function
    P_CREPRES2IREPRES => CREPRES2IREPRES

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_INTEGER_ARRAY_WITH_FILTER( CFG, REPRES_KEY, FILTER_REPRES, P_CREPRES2IREPRES, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_REPRES), ERRFLAG_REPRES_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_REPRES).LT.1, ERRFLAG_REPRES_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_REPRES_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_REPRES not allocated' )
    CASE (ERRFLAG_REPRES_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_REPRES not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_REPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering level types from a YAML configuration and populates a level type structure.
!>
!> This function reads filtering level types from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_LEVTYPE` array with the parsed level types. It also indicates whether
!> the level types were successfully populated through the `HAS_LEVTYPE` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter level types are read.
!> @param [out] FILTER_LEVTYPE The array that will be populated with the parsed filtering level types.
!> @param [out] HAS_LEVTYPE Logical flag indicating whether the level types were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "LEVTYPE_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [PROCEDURE] "OM_CORE_MOD::CLEVTYPE2ILEVTYPE"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_FILTER"
!>   - @dependency [INTERFACE] "YAML_CORE_UTILS_MOD::FUN_C2I_IF"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_LEVTYPE_NOT_ALLOCATED` (1): Error allocating `LEVTYPE`
!>   - `ERRFLAG_LEVTYPE_EMPTY` (2): Error when `LEVTYPE` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T, FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_LEVTYPE'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_LEVTYPE( CFG, FILTER_LEVTYPE, HAS_LEVTYPE, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('S') :: OM_CORE_MOD, ONLY: CLEVTYPE2ILEVTYPE
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_ARRAY_WITH_FILTER
  PP_USE_L('I') :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                      INTENT(IN)  :: CFG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_LEVTYPE
  LOGICAL,                                         INTENT(OUT) :: HAS_LEVTYPE
  LOGICAL,                                         INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVTYPE_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_LEVTYPE_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

  ! Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CLEVTYPE2ILEVTYPE

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, LEVTYPE_KEY, HAS_LEVTYPE, VERBOSE )

  ! Read the paramId
  IF ( HAS_LEVTYPE ) THEN

    ! Associtate the conversion function
    P_CLEVTYPE2ILEVTYPE => CLEVTYPE2ILEVTYPE

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_INTEGER_ARRAY_WITH_FILTER( CFG, LEVTYPE_KEY, FILTER_LEVTYPE, P_CLEVTYPE2ILEVTYPE, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_LEVTYPE), ERRFLAG_LEVTYPE_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_LEVTYPE).LT.1, ERRFLAG_LEVTYPE_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_LEVTYPE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_LEVTYPE not allocated' )
    CASE (ERRFLAG_LEVTYPE_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_LEVTYPE not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_LEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering tags from a YAML configuration and populates a tag array.
!>
!> This function reads filtering tags from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_TAG` array with the parsed tags. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function also supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during the execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter tags are read.
!> @param [out] FILTER_TAG The array that will be populated with the parsed filtering tags.
!> @param [out] HAS_TAG Logical flag indicating whether tags were successfully read (`.TRUE.`)
!>                     or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "TAG_KEY"
!>
!> @subsection local_dependencies
!> - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!> - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!> - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_STRING_ARRAY_FIXED_SIZE"
!>
!> @section Error Codes
!> This function defines several error codes:
!>
!> - `ERRFLAG_TAG_NOT_ALLOCATED`: Error when the tag array is not allocated.
!> - `ERRFLAG_TAG_EMPTY`: Error when the tag array is empty.
!> - `ERRFLAG_KEY_NOT_PRESENT`: Error when a required key is not present in the configuration.
!> - `ERRFLAG_READ_KEY_FAILED`: Error when reading a key from the configuration fails.
!>
!> @see YAML_CONFIGURATION_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_TAG'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_TAG( CFG, FILTER_TAG, HAS_TAG, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING_ARRAY_FIXED_SIZE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                        INTENT(IN)  :: CFG
  CHARACTER(LEN=TAG_LEN), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: FILTER_TAG
  LOGICAL,                                           INTENT(OUT) :: HAS_TAG
  LOGICAL,                                           INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_TAG_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_TAG_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, TAG_KEY, HAS_TAG, VERBOSE )

  ! Read the paramId
  IF ( HAS_TAG ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_READ_STRING_ARRAY_FIXED_SIZE( CFG, TAG_KEY, FILTER_TAG, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_TAG), ERRFLAG_TAG_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_TAG).LT.1, ERRFLAG_TAG_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_TAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_TAG not allocated' )
    CASE (ERRFLAG_TAG_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_TAG not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_TAG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering is-ensemble flags from a YAML configuration and populates an is-ensemble flag.
!>
!> This function reads filtering is-ensemble flags from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_ISENSEMBLE` flag with the parsed values. It also indicates whether
!> the is-ensemble flags were successfully populated through the `HAS_IS_ENSEMBLE` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter is-ensemble flags are read.
!> @param [out] FILTER_ISENSEMBLE The flag that will be populated with the parsed is-ensemble values.
!> @param [out] HAS_IS_ENSEMBLE Logical flag indicating whether the is-ensemble flags were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "ISENSEMBLE_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_LOGICAL"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_ISENSEMBLE_NOT_ALLOCATED` (1): Error allocating `ISENSEMBLE`
!>   - `ERRFLAG_ISENSEMBLE_EMPTY` (2): Error when `ISENSEMBLE` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_ISENSEMBLE'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_ISENSEMBLE( CFG, FILTER_ISENSEMBLE, HAS_IS_ENSEMBLE, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  LOGICAL,                    INTENT(OUT) :: FILTER_ISENSEMBLE
  LOGICAL,                    INTENT(OUT) :: HAS_IS_ENSEMBLE
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_ISENSEMBLE_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_ISENSEMBLE_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, ISENSEMBLE_KEY, HAS_IS_ENSEMBLE, VERBOSE )

  ! Read the paramId
  IF ( HAS_IS_ENSEMBLE ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_LOGICAL( CFG, ISENSEMBLE_KEY, FILTER_ISENSEMBLE, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_ISENSEMBLE), ERRFLAG_ISENSEMBLE_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_ISENSEMBLE).LT.1, ERRFLAG_ISENSEMBLE_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ISENSEMBLE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_ISENSEMBLE not allocated' )
    CASE (ERRFLAG_ISENSEMBLE_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_ISENSEMBLE not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_ISENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads filtering is-chemical flags from a YAML configuration and populates an is-chemical flag.
!>
!> This function reads filtering is-chemical flags from a provided YAML configuration object (`CFG`)
!> and populates the `FILTER_ISCHEMICAL` flag with the parsed values. It also indicates whether
!> the is-chemical flags were successfully populated through the `HAS_ISCHEMICAL` flag. If an error occurs,
!> the function returns an error code to indicate the nature of the issue.
!>
!> The function supports a `VERBOSE` mode, which can be enabled for debugging purposes,
!> providing additional output during execution.
!>
!> @param [in] CFG The YAML configuration object from which the filter is-chemical flags are read.
!> @param [out] FILTER_ISCHEMICAL The flag that will be populated with the parsed is-chemical values.
!> @param [out] HAS_ISCHEMICAL Logical flag indicating whether the is-chemical flags were successfully populated (`.TRUE.`)
!>                        or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`)
!>                     for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE]      "FILTER_RULES_T"
!>   - @dependency [PARAMETER] "ISCHEMICAL_KEY"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>   - @dependency [TYPE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY"
!>   - @dependency [PROCEDURE] "YAML_CORE_UTILS_MOD::YAML_READ_LOGICAL"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_ISCHEMICAL_NOT_ALLOCATED` (1): Error allocating `ISCHEMICAL`
!>   - `ERRFLAG_ISCHEMICAL_EMPTY` (2): Error when `ISCHEMICAL` is empty
!>   - `ERRFLAG_KEY_NOT_PRESENT` (3): Key not present in the YAML configuration
!>   - `ERRFLAG_READ_KEY_FAILED` (4): Failure reading key from YAML configuration
!>
!> @see YAML_CONFIGURATION_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_RULE_FILTER_ISCHEMICAL'
__THREAD_SAFE__ FUNCTION READ_RULE_FILTER_ISCHEMICAL( CFG, FILTER_ISCHEMICAL, HAS_ISCHEMICAL, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K
  PP_USE_L('T') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  PP_USE_L('S') :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)  :: CFG
  LOGICAL,                    INTENT(OUT) :: FILTER_ISCHEMICAL
  LOGICAL,                    INTENT(OUT) :: HAS_ISCHEMICAL
  LOGICAL,                    INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_ISCHEMICAL_NOT_ALLOCATED=1_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_ISCHEMICAL_EMPTY=2_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_ERR_K
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_READ_KEY_FAILED=4_ERR_K

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

  ! Configuration structure
  PP_TRYCALL(ERRFLAG_KEY_NOT_PRESENT) YAML_CONFIGURATION_HAS_KEY( CFG, ISCHEMICAL_KEY, HAS_ISCHEMICAL, VERBOSE )

  ! Read the paramId
  IF ( HAS_ISCHEMICAL ) THEN

    ! Read the paramId as a string array
    PP_TRYCALL(ERRFLAG_READ_KEY_FAILED) YAML_READ_LOGICAL( CFG, ISCHEMICAL_KEY, FILTER_ISCHEMICAL, VERBOSE )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_ISCHEMICAL), ERRFLAG_ISCHEMICAL_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( SIZE(FILTER_ISCHEMICAL).LT.1, ERRFLAG_ISCHEMICAL_EMPTY )

  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ISCHEMICAL_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_ISCHEMICAL not allocated' )
    CASE (ERRFLAG_ISCHEMICAL_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'FILTER_ISCHEMICAL not allocated or empty' )
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_CONFIGURATION_HAS_KEY' )
    CASE (ERRFLAG_READ_KEY_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested error calling: YAML_READ_INTEGER_ARRAY_WITH_RANGES' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_RULE_FILTER_ISCHEMICAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a filter rule against provided parameters and determines if a match is found.
!>
!> This function checks whether the provided parameters match the filter rules defined in `FILTER`.
!> It returns a logical flag `MATCH` indicating if the parameters meet the filter criteria. The function
!> also supports `VERBOSE` mode to provide additional debugging output.
!>
!> @param [in] FILTER The filter rules to be applied for matching.
!> @param [in] IN_PARAMID Parameter ID to be matched.
!> @param [in] IN_LEVEL Level to be matched.
!> @param [in] IN_REPRES Representation to be matched.
!> @param [in] IN_LEVTYPE Level type to be matched.
!> @param [in] IN_TAG Tag to be matched.
!> @param [in] IN_ISENSEMBLE Logical flag indicating whether is-ensemble should be matched.
!> @param [in] IN_ISCHEMICAL Logical flag indicating whether is-chemical should be matched.
!> @param [out] MATCH Logical flag indicating if the filter criteria are met (`.TRUE.`) or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`) for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE] "FILTER_RULES_T"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_EMPTY_FILTER` (1): Error when `FILTER` is empty
!>
!> @see FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MATCH_RULE_FILTER_ATM'
__THREAD_SAFE__ FUNCTION MATCH_RULE_FILTER_ATM( FILTER, &
&           IN_PARAMID, IN_LEVEL, IN_REPRES, IN_LEVTYPE, &
&           IN_TAG, IN_ISENSEMBLE, IN_ISCHEMICAL, MATCH, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  TYPE(FILTER_RULES_T),   INTENT(IN)  :: FILTER
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_LEVEL
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_REPRES
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_LEVTYPE
  CHARACTER(LEN=TAG_LEN), INTENT(IN)  :: IN_TAG
  LOGICAL,                INTENT(IN)  :: IN_ISENSEMBLE
  LOGICAL,                INTENT(IN)  :: IN_ISCHEMICAL
  LOGICAL,                INTENT(OUT) :: MATCH
  LOGICAL,                INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_EMPTY_FILTER=1_ERR_K

  ! Local variables
  LOGICAL, DIMENSION(7) :: CONDITIONS

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

  ! Get the conditions
  CONDITIONS(1) = FILTER%HAS_PARAMID
  CONDITIONS(2) = FILTER%HAS_LEVEL
  CONDITIONS(3) = FILTER%HAS_REPRES
  CONDITIONS(4) = FILTER%HAS_LEVTYPE
  CONDITIONS(5) = FILTER%HAS_TAG
  CONDITIONS(6) = FILTER%HAS_ISENSEMBLE
  CONDITIONS(7) = FILTER%HAS_ISCHEMICAL

  ! Empty filter not allowed
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ANY(CONDITIONS), ERRFLAG_EMPTY_FILTER )

  ! Initialization
  MATCH = .TRUE.

  ! Try to match paramId
  IF ( MATCH .AND. FILTER%HAS_PARAMID ) THEN
    MATCH = MATCH .AND. ANY( FILTER%PARAMID.EQ.IN_PARAMID )
  ENDIF

  ! Try to match level
  IF ( MATCH .AND. FILTER%HAS_LEVEL ) THEN
    MATCH = MATCH .AND. ANY( FILTER%LEVEL.EQ.IN_LEVEL )
  ENDIF

  ! Try to match repres
  IF ( MATCH .AND. FILTER%HAS_REPRES ) THEN
    MATCH = MATCH .AND. ANY( FILTER%REPRES.EQ.IN_REPRES )
  ENDIF

  ! Try to match levtype
  IF ( MATCH .AND. FILTER%HAS_LEVTYPE ) THEN
    MATCH = MATCH .AND. ANY( FILTER%LEVTYPE.EQ.IN_LEVTYPE )
  ENDIF

  ! Try to match tag
  IF ( MATCH .AND. FILTER%HAS_TAG ) THEN
    MATCH = MATCH .AND. ANY( FILTER%TAG.EQ.IN_TAG )
  ENDIF

  ! Try to match is-ensemble
  IF ( MATCH .AND. FILTER%HAS_ISENSEMBLE ) THEN
    MATCH = MATCH .AND. (FILTER%ISENSEMBLE.EQ.IN_ISENSEMBLE)
  ENDIF

  ! Try to match is-chemical
  IF ( MATCH .AND. FILTER%HAS_ISCHEMICAL ) THEN
    MATCH = MATCH .AND. (FILTER%ISCHEMICAL.EQ.IN_ISCHEMICAL)
  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_EMPTY_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'EMPTY FILTERS are not allowed' )
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

  ! Exit point on error
  RETURN

END FUNCTION MATCH_RULE_FILTER_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a filter rule against provided parameters and determines if a match is found.
!>
!> This function checks whether the provided parameters match the filter rules defined in `FILTER`.
!> It returns a logical flag `MATCH` indicating if the parameters meet the filter criteria. The function
!> also supports `VERBOSE` mode to provide additional debugging output.
!>
!> @param [in] FILTER The filter rules to be applied for matching.
!> @param [in] IN_PARAMID Parameter ID to be matched.
!> @param [in] IN_DIRECTION Direction to be matched.
!> @param [in] IN_FREQUENCY Frequency to be matched.
!> @param [in] IN_REPRES Representation to be matched.
!> @param [in] IN_LEVTYPE Level type to be matched.
!> @param [in] IN_TAG Tag to be matched.
!> @param [in] IN_ISENSEMBLE Logical flag indicating whether is-ensemble should be matched.
!> @param [in] IN_ISCHEMICAL Logical flag indicating whether is-chemical should be matched.
!> @param [out] MATCH Logical flag indicating if the filter criteria are met (`.TRUE.`) or not (`.FALSE.`).
!> @param [in] VERBOSE Logical flag indicating whether verbose output is enabled (`.TRUE.`) for debugging purposes.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [TYPE] "FILTER_RULES_T"
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "OM_CORE_MOD::JPIB_K"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>
!> @section Error codes explicitly handled in this function:
!>   - `ERRFLAG_EMPTY_FILTER` (1): Error when `FILTER` is empty
!>
!> @see FILTER_RULES_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MATCH_RULE_FILTER_WAM'
__THREAD_SAFE__ FUNCTION MATCH_RULE_FILTER_WAM( FILTER, &
&           IN_PARAMID, IN_DIRECTION, IN_FREQUENCY, IN_REPRES, IN_LEVTYPE, &
&           IN_TAG, IN_ISENSEMBLE, IN_ISCHEMICAL, MATCH, VERBOSE ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  PP_USE_L('P') :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FILTER_RULES_T),   INTENT(IN)  :: FILTER
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_PARAMID
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_DIRECTION
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_FREQUENCY
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_REPRES
  INTEGER(KIND=JPIB),     INTENT(IN)  :: IN_LEVTYPE
  CHARACTER(LEN=TAG_LEN), INTENT(IN)  :: IN_TAG
  LOGICAL,                INTENT(IN)  :: IN_ISENSEMBLE
  LOGICAL,                INTENT(IN)  :: IN_ISCHEMICAL
  LOGICAL,                INTENT(OUT) :: MATCH
  LOGICAL,                INTENT(IN)  :: VERBOSE

  ! Function return value
  INTEGER(KIND=ERR_K) :: RET

  ! Local error codes
  INTEGER(KIND=ERR_K), PARAMETER :: ERRFLAG_EMPTY_FILTER=1_ERR_K

  ! Local variables
  LOGICAL, DIMENSION(8) :: CONDITIONS

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

  ! Get the conditions
  CONDITIONS(1) = FILTER%HAS_PARAMID
  CONDITIONS(2) = FILTER%HAS_DIRECTION
  CONDITIONS(3) = FILTER%HAS_FREQUENCY
  CONDITIONS(4) = FILTER%HAS_REPRES
  CONDITIONS(5) = FILTER%HAS_LEVTYPE
  CONDITIONS(6) = FILTER%HAS_TAG
  CONDITIONS(7) = FILTER%HAS_ISENSEMBLE
  CONDITIONS(8) = FILTER%HAS_ISCHEMICAL

  ! Empty filter not allowed
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ANY(CONDITIONS), ERRFLAG_EMPTY_FILTER )

  ! Initialization
  MATCH = .TRUE.

  ! Try to match paramId
  IF ( MATCH .AND. FILTER%HAS_PARAMID ) THEN
    MATCH = MATCH .AND. ANY( FILTER%PARAMID.EQ.IN_PARAMID )
  ENDIF

  ! Try to match direction
  IF ( MATCH .AND. FILTER%HAS_DIRECTION ) THEN
    MATCH = MATCH .AND. ANY( FILTER%DIRECTION.EQ.IN_DIRECTION )
  ENDIF

  ! Try to match frequency
  IF ( MATCH .AND. FILTER%HAS_FREQUENCY ) THEN
    MATCH = MATCH .AND. ANY( FILTER%FREQUENCY.EQ.IN_FREQUENCY )
  ENDIF

  ! Try to match repres
  IF ( MATCH .AND. FILTER%HAS_REPRES ) THEN
    MATCH = MATCH .AND. ANY( FILTER%REPRES.EQ.IN_REPRES )
  ENDIF

  ! Try to match levtype
  IF ( MATCH .AND. FILTER%HAS_LEVTYPE ) THEN
    MATCH = MATCH .AND. ANY( FILTER%LEVTYPE.EQ.IN_LEVTYPE )
  ENDIF

  ! Try to match tag
  IF ( MATCH .AND. FILTER%HAS_TAG ) THEN
    MATCH = MATCH .AND. ANY( FILTER%TAG.EQ.IN_TAG )
  ENDIF

  ! Try to match is-ensemble
  IF ( MATCH .AND. FILTER%HAS_ISENSEMBLE ) THEN
    MATCH = MATCH .AND. (FILTER%ISENSEMBLE.EQ.IN_ISENSEMBLE)
  ENDIF

  ! Try to match is-chemical
  IF ( MATCH .AND. FILTER%HAS_ISCHEMICAL ) THEN
    MATCH = MATCH .AND. (FILTER%ISCHEMICAL.EQ.IN_ISCHEMICAL)
  ENDIF

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
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_EMPTY_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'EMPTY FILTERS are not allowed' )
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

  ! Exit point on error
  RETURN

END FUNCTION MATCH_RULE_FILTER_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE YAML_FILTERS_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME