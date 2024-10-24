! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'fortran_message_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FORTRAN_MESSAGE_MOD'
MODULE FORTRAN_MESSAGE_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibiliity of the module
PRIVATE

!>
TYPE :: FORTRAN_MESSAGE_T

  !> General information
  INTEGER(KIND=JPIB_K) :: STREAM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: CLASS = UNDEF_PARAM_E
  CHARACTER(LEN=4)     :: EXPVER = REPEAT('*',4)
  INTEGER(KIND=JPIB_K) :: ORIGIN = UNDEF_PARAM_E  ! Centre
  INTEGER(KIND=JPIB_K) :: ANOFFSET = UNDEF_PARAM_E

  !> Ensemble information
  INTEGER(KIND=JPIB_K) :: NUMBER = UNDEF_PARAM_E  ! parturbation number

  !> Satellite information
  INTEGER(KIND=JPIB_K) :: IDENT = UNDEF_PARAM_E ! satellite identifier
  INTEGER(KIND=JPIB_K) :: INSTRUMENT = UNDEF_PARAM_E ! instrument identifier
  INTEGER(KIND=JPIB_K) :: CHANNEL = UNDEF_PARAM_E ! satellite channel number

  !> Field information
  INTEGER(KIND=JPIB_K) :: PARAM_TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: CHEM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: PARAM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: LEVTYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: LEVELIST = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: DIRECTION = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: FREQUENCY = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: MODEL = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: REPRES = UNDEF_PARAM_E

  INTEGER(KIND=JPIB_K) :: DATE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TIME = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: STEP = UNDEF_PARAM_E

  !> Grid information
  CHARACTER(LEN=8)     :: GRID = REPEAT('*',8)

CONTAINS

  !> Comparison operators
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM     => FORTRAN_MESSAGE_COPY_DATA_FROM
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SWAP_DATA     => FORTRAN_MESSAGE_SWAP_DATA
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: IS_EQUAL_TO   => FORTRAN_MESSAGE_EQUAL_TO
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: IS_LOWER_THAN => FORTRAN_MESSAGE_LOWER_THAN
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE          => FORTRAN_MESSAGE_FREE

  !> Set fields by field ID
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT    => FORTRAN_MESSAGE_SET_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => FORTRAN_MESSAGE_SET_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_FLOAT  => FORTRAN_MESSAGE_SET_FLOAT
  GENERIC :: SET => SET_INT
  GENERIC :: SET => SET_STRING
  GENERIC :: SET => SET_FLOAT

  !> Set fields by field ID
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_INT    => FORTRAN_MESSAGE_GET_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_STRING => FORTRAN_MESSAGE_GET_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_FLOAT  => FORTRAN_MESSAGE_GET_FLOAT
  GENERIC :: GET => GET_INT
  GENERIC :: GET => GET_STRING
  GENERIC :: GET => GET_FLOAT

END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: FORTRAN_MESSAGE_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_FREE'
FUNCTION FORTRAN_MESSAGE_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
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

  !> Reset to default values
  THIS%STREAM     = UNDEF_PARAM_E
  THIS%TYPE       = UNDEF_PARAM_E
  THIS%CLASS      = UNDEF_PARAM_E
  THIS%EXPVER     = REPEAT('*',4)
  THIS%ORIGIN     = UNDEF_PARAM_E
  THIS%ANOFFSET   = UNDEF_PARAM_E
  THIS%NUMBER     = UNDEF_PARAM_E
  THIS%IDENT      = UNDEF_PARAM_E
  THIS%INSTRUMENT = UNDEF_PARAM_E
  THIS%CHANNEL    = UNDEF_PARAM_E
  THIS%PARAM_TYPE = UNDEF_PARAM_E
  THIS%CHEM       = UNDEF_PARAM_E
  THIS%PARAM      = UNDEF_PARAM_E
  THIS%LEVTYPE    = UNDEF_PARAM_E
  THIS%LEVELIST   = UNDEF_PARAM_E
  THIS%DIRECTION  = UNDEF_PARAM_E
  THIS%FREQUENCY  = UNDEF_PARAM_E
  THIS%MODEL      = UNDEF_PARAM_E
  THIS%REPRES     = UNDEF_PARAM_E
  THIS%DATE       = UNDEF_PARAM_E
  THIS%TIME       = UNDEF_PARAM_E
  THIS%STEP       = UNDEF_PARAM_E
  THIS%GRID       = REPEAT('*',8)


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

END FUNCTION FORTRAN_MESSAGE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_COPY_DATA_FROM'
FUNCTION FORTRAN_MESSAGE_COPY_DATA_FROM( THIS, OTHER, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: OTHER
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
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

  !> Reset to default values
  THIS%STREAM     = OTHER%STREAM
  THIS%TYPE       = OTHER%TYPE
  THIS%CLASS      = OTHER%CLASS
  THIS%EXPVER     = OTHER%EXPVER
  THIS%ORIGIN     = OTHER%ORIGIN
  THIS%ANOFFSET   = OTHER%ANOFFSET
  THIS%NUMBER     = OTHER%NUMBER
  THIS%IDENT      = OTHER%IDENT
  THIS%INSTRUMENT = OTHER%INSTRUMENT
  THIS%CHANNEL    = OTHER%CHANNEL
  THIS%PARAM_TYPE = OTHER%PARAM_TYPE
  THIS%CHEM       = OTHER%CHEM
  THIS%PARAM      = OTHER%PARAM
  THIS%LEVTYPE    = OTHER%LEVTYPE
  THIS%LEVELIST   = OTHER%LEVELIST
  THIS%DIRECTION  = OTHER%DIRECTION
  THIS%FREQUENCY  = OTHER%FREQUENCY
  THIS%MODEL      = OTHER%MODEL
  THIS%REPRES     = OTHER%REPRES
  THIS%DATE       = OTHER%DATE
  THIS%TIME       = OTHER%TIME
  THIS%STEP       = OTHER%STEP
  THIS%GRID       = OTHER%GRID

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

END FUNCTION FORTRAN_MESSAGE_COPY_DATA_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SWAP_DATA'
FUNCTION FORTRAN_MESSAGE_SWAP_DATA( THIS, OTHER, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(INOUT) :: OTHER
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ITMP
  CHARACTER(LEN=4)     :: CTMP4
  CHARACTER(LEN=8)     :: CTMP8

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

  !> Swap data
  ITMP         = THIS%STREAM
  THIS%STREAM  = OTHER%STREAM
  OTHER%STREAM = ITMP

  ITMP       = THIS%TYPE
  THIS%TYPE  = OTHER%TYPE
  OTHER%TYPE = ITMP

  ITMP        = THIS%CLASS
  THIS%CLASS  = OTHER%CLASS
  OTHER%CLASS = ITMP

  CTMP4        = THIS%EXPVER
  THIS%EXPVER  = OTHER%EXPVER
  OTHER%EXPVER = CTMP4

  ITMP         = THIS%ORIGIN
  THIS%ORIGIN  = OTHER%ORIGIN
  OTHER%ORIGIN = ITMP

  ITMP           = THIS%ANOFFSET
  THIS%ANOFFSET  = OTHER%ANOFFSET
  OTHER%ANOFFSET = ITMP

  ITMP         = THIS%NUMBER
  THIS%NUMBER  = OTHER%NUMBER
  OTHER%NUMBER = ITMP

  ITMP        = THIS%IDENT
  THIS%IDENT  = OTHER%IDENT
  OTHER%IDENT = ITMP

  ITMP             = THIS%INSTRUMENT
  THIS%INSTRUMENT  = OTHER%INSTRUMENT
  OTHER%INSTRUMENT = ITMP

  ITMP          = THIS%CHANNEL
  THIS%CHANNEL  = OTHER%CHANNEL
  OTHER%CHANNEL = ITMP

  ITMP             = THIS%PARAM_TYPE
  THIS%PARAM_TYPE  = OTHER%PARAM_TYPE
  OTHER%PARAM_TYPE = ITMP

  ITMP       = THIS%CHEM
  THIS%CHEM  = OTHER%CHEM
  OTHER%CHEM = ITMP

  ITMP        = THIS%PARAM
  THIS%PARAM  = OTHER%PARAM
  OTHER%PARAM = ITMP

  ITMP          = THIS%LEVTYPE
  THIS%LEVTYPE  = OTHER%LEVTYPE
  OTHER%LEVTYPE = ITMP

  ITMP           = THIS%LEVELIST
  THIS%LEVELIST  = OTHER%LEVELIST
  OTHER%LEVELIST = ITMP

  ITMP            = THIS%DIRECTION
  THIS%DIRECTION  = OTHER%DIRECTION
  OTHER%DIRECTION = ITMP

  ITMP            = THIS%FREQUENCY
  THIS%FREQUENCY  = OTHER%FREQUENCY
  OTHER%FREQUENCY = ITMP

  ITMP        = THIS%MODEL
  THIS%MODEL  = OTHER%MODEL
  OTHER%MODEL =ITMP

  ITMP         = THIS%REPRES
  THIS%REPRES  = OTHER%REPRES
  OTHER%REPRES = ITMP

  ITMP       = THIS%DATE
  THIS%DATE  = OTHER%DATE
  OTHER%DATE = ITMP

  ITMP       = THIS%TIME
  THIS%TIME  = OTHER%TIME
  OTHER%TIME = ITMP

  ITMP       = THIS%STEP
  THIS%STEP  = OTHER%STEP
  OTHER%STEP = ITMP

  CTMP8      = THIS%GRID
  THIS%GRID  = OTHER%GRID
  OTHER%GRID = CTMP8

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

END FUNCTION FORTRAN_MESSAGE_SWAP_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_EQUAL_TO'
FUNCTION FORTRAN_MESSAGE_EQUAL_TO( THIS, OTHER, OPT, IS_EQUAL, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: OTHER
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  LOGICAL,                      INTENT(OUT)   :: IS_EQUAL
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

  !> Compare two messages
  IS_EQUAL = .TRUE.

  IF ( OPT%CACHE_LOCAL_USE_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%STREAM .EQ. OTHER%STREAM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%TYPE .EQ. OTHER%TYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CLASS .EQ. OTHER%CLASS )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%EXPVER .EQ. OTHER%EXPVER )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%ORIGIN .EQ. OTHER%ORIGIN )
  ENDIF

  ! Information related to time should never  be compared
  IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%ANOFFSET .EQ. OTHER%ANOFFSET )
  ENDIF

  IF ( OPT%CACHE_SATELLITES_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%NUMBER .EQ. OTHER%NUMBER )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%IDENT .EQ. OTHER%IDENT )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%INSTRUMENT .EQ. OTHER%INSTRUMENT )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CHANNEL .EQ. OTHER%CHANNEL )
  ENDIF

  IF ( OPT%CACHE_PRODUCT_DEFINITION_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%PARAM_TYPE .EQ. OTHER%PARAM_TYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CHEM .EQ. OTHER%CHEM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%PARAM .EQ. OTHER%PARAM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%LEVTYPE .EQ. OTHER%LEVTYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%MODEL .EQ. OTHER%MODEL )
  ENDIF

  IF ( OPT%CACHE_TYPE_OF_LEVELS ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%LEVELIST .EQ. OTHER%LEVELIST )
  ENDIF

  IF ( OPT%CACHE_DIRECTION_FREQUENCY ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%DIRECTION .EQ. OTHER%DIRECTION )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%FREQUENCY .EQ. OTHER%FREQUENCY )
  ENDIF


  ! Time information should never be cached
  IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%DATE .EQ. OTHER%DATE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%TIME .EQ. OTHER%TIME )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%STEP .EQ. OTHER%STEP )
  ENDIF

  IF ( OPT%CACHE_GRID_DEFINITION_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%REPRES .EQ. OTHER%REPRES )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%GRID .EQ. OTHER%GRID )
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

END FUNCTION FORTRAN_MESSAGE_EQUAL_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_LOWER_THAN'
FUNCTION FORTRAN_MESSAGE_LOWER_THAN( THIS, OTHER, OPT, IS_LOWER_THAN, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: OTHER
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  LOGICAL,                      INTENT(OUT)   :: IS_LOWER_THAN
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

  !> Compare two messages
  IS_LOWER_THAN = .TRUE.
  IF ( OPT%CACHE_LOCAL_USE_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%STREAM .LT. OTHER%STREAM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%TYPE .LT. OTHER%TYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CLASS .LT. OTHER%CLASS )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%EXPVER .LT. OTHER%EXPVER )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%ORIGIN .LT. OTHER%ORIGIN )
  ENDIF


  ! Information related to time should never  be compared
  IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%ANOFFSET .LT. OTHER%ANOFFSET )
  ENDIF

  IF ( OPT%CACHE_SATELLITES_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%NUMBER .LT. OTHER%NUMBER )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%IDENT .LT. OTHER%IDENT )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%INSTRUMENT .LT. OTHER%INSTRUMENT )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CHANNEL .LT. OTHER%CHANNEL )
  ENDIF

  IF ( OPT%CACHE_PRODUCT_DEFINITION_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%PARAM_TYPE .LT. OTHER%PARAM_TYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CHEM .LT. OTHER%CHEM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%PARAM .LT. OTHER%PARAM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%LEVTYPE .LT. OTHER%LEVTYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%MODEL .LT. OTHER%MODEL )
  ENDIF

  IF ( OPT%CACHE_TYPE_OF_LEVELS ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%LEVELIST .LT. OTHER%LEVELIST )
  ENDIF

  IF ( OPT%CACHE_DIRECTION_FREQUENCY ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%DIRECTION .LT. OTHER%DIRECTION )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%FREQUENCY .LT. OTHER%FREQUENCY )
  ENDIF

  ! Time information should never be cached!!!
  IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%DATE .LT. OTHER%DATE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%TIME .LT. OTHER%TIME )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%STEP .LT. OTHER%STEP )
  ENDIF

  IF ( OPT%CACHE_GRID_DEFINITION_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%REPRES .LT. OTHER%REPRES )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%GRID .LT. OTHER%GRID )
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

END FUNCTION FORTRAN_MESSAGE_LOWER_THAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_INT'
FUNCTION FORTRAN_MESSAGE_SET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ORIGIN_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ANOFFSET_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_NUMBER_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_IDENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_INSTRUMENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHANNEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHEM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVELIST_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DIRECTION_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_FREQUENCY_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_MODEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DATE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TIME_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STEP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  ! Set fields by field ID
  SELECT CASE (ID)
  CASE (MSGINTFLD_STREAM_E)
    THIS%STREAM  = VALUE
  CASE (MSGINTFLD_TYPE_E)
    THIS%TYPE = VALUE
  CASE (MSGINTFLD_CLASS_E)
    THIS%CLASS = VALUE
  CASE (MSGINTFLD_ORIGIN_E)
    THIS%ORIGIN = VALUE
  CASE (MSGINTFLD_ANOFFSET_E)
    THIS%ANOFFSET = VALUE
  CASE (MSGINTFLD_NUMBER_E)
    THIS%NUMBER = VALUE
  CASE (MSGINTFLD_IDENT_E)
    THIS%IDENT = VALUE
  CASE (MSGINTFLD_INSTRUMENT_E)
    THIS%INSTRUMENT = VALUE
  CASE (MSGINTFLD_CHANNEL_E)
    THIS%CHANNEL = VALUE
  CASE (MSGINTFLD_PARAM_TYPE_E)
    THIS%PARAM_TYPE = VALUE
  CASE (MSGINTFLD_CHEM_E)
    THIS%CHEM = VALUE
  CASE (MSGINTFLD_PARAM_E)
    THIS%PARAM = VALUE
  CASE (MSGINTFLD_LEVTYPE_E)
    THIS%LEVTYPE = VALUE
  CASE (MSGINTFLD_LEVELIST_E)
    THIS%LEVELIST = VALUE
  CASE (MSGINTFLD_DIRECTION_E)
    THIS%DIRECTION = VALUE
  CASE (MSGINTFLD_FREQUENCY_E)
    THIS%FREQUENCY = VALUE
  CASE (MSGINTFLD_MODEL_E)
    THIS%MODEL = VALUE
  CASE (MSGINTFLD_REPRES_E)
    THIS%REPRES = VALUE
  CASE (MSGINTFLD_DATE_E)
    THIS%DATE = VALUE
  CASE (MSGINTFLD_TIME_E)
    THIS%TIME = VALUE
  CASE (MSGINTFLD_STEP_E)
    THIS%STEP = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
  END SELECT

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_SET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_STRING'
FUNCTION FORTRAN_MESSAGE_SET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_GRID_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_EXPVER_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=*),         INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K


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

  !> Select the prefix
  SELECT CASE ( ID )
  CASE (MSGSTRFLD_GRID_E)
    THIS%GRID = VALUE
  CASE (MSGSTRFLD_EXPVER_E)
    THIS%EXPVER = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
  END SELECT

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_SET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_FLOAT'
FUNCTION FORTRAN_MESSAGE_SET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_SET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_INT'
FUNCTION FORTRAN_MESSAGE_GET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ORIGIN_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ANOFFSET_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_NUMBER_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_IDENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_INSTRUMENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHANNEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHEM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVELIST_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DIRECTION_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_FREQUENCY_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_MODEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DATE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TIME_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STEP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K


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

  ! Initialize the output value
  VALUE = 0_JPIB_K

  ! Set fields by field ID
  SELECT CASE (ID)
  CASE (MSGINTFLD_STREAM_E)
    VALUE = THIS%STREAM
  CASE (MSGINTFLD_TYPE_E)
    VALUE = THIS%TYPE
  CASE (MSGINTFLD_CLASS_E)
    VALUE = THIS%CLASS
  CASE (MSGINTFLD_ORIGIN_E)
    VALUE = THIS%ORIGIN
  CASE (MSGINTFLD_ANOFFSET_E)
    VALUE = THIS%ANOFFSET
  CASE (MSGINTFLD_NUMBER_E)
    VALUE = THIS%NUMBER
  CASE (MSGINTFLD_IDENT_E)
    VALUE = THIS%IDENT
  CASE (MSGINTFLD_INSTRUMENT_E)
    VALUE = THIS%INSTRUMENT
  CASE (MSGINTFLD_CHANNEL_E)
    VALUE = THIS%CHANNEL
  CASE (MSGINTFLD_PARAM_TYPE_E)
    VALUE = THIS%PARAM_TYPE
  CASE (MSGINTFLD_CHEM_E)
    VALUE = THIS%CHEM
  CASE (MSGINTFLD_PARAM_E)
    VALUE = THIS%PARAM
  CASE (MSGINTFLD_LEVTYPE_E)
    VALUE = THIS%LEVTYPE
  CASE (MSGINTFLD_LEVELIST_E)
    VALUE = THIS%LEVELIST
  CASE (MSGINTFLD_DIRECTION_E)
    VALUE = THIS%DIRECTION
  CASE (MSGINTFLD_FREQUENCY_E)
    VALUE = THIS%FREQUENCY
  CASE (MSGINTFLD_MODEL_E)
    VALUE = THIS%MODEL
  CASE (MSGINTFLD_REPRES_E)
    VALUE = THIS%REPRES
  CASE (MSGINTFLD_DATE_E)
    VALUE = THIS%DATE
  CASE (MSGINTFLD_TIME_E)
    VALUE = THIS%TIME
  CASE (MSGINTFLD_STEP_E)
    VALUE = THIS%STEP
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
  END SELECT

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_GET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_STRING'
FUNCTION FORTRAN_MESSAGE_GET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_GRID_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_EXPVER_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=4),         INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K


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

  !> Select the prefix
  SELECT CASE ( ID )
  CASE (MSGSTRFLD_GRID_E)
    VALUE = THIS%GRID
  CASE (MSGSTRFLD_EXPVER_E)
    VALUE = THIS%EXPVER
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
  END SELECT

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_GET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_FLOAT'
FUNCTION FORTRAN_MESSAGE_GET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  VALUE = 0.0_JPRD_K

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
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

END FUNCTION FORTRAN_MESSAGE_GET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FORTRAN_MESSAGE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
