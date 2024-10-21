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


  !> Fileds of the message
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_STREAM_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_TYPE_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_CLASS_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_PARAM_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_LEVTYPE_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_LEVELIST_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_DIRECTION_E=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_FREQUENCY_E=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_MODEL_E=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_REPRES_E=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGSTRINGFLD_EXPVER_E=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_MSGFLDS=11_JPIB_K




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
    CHARACTER(LEN=4)     :: GRID = REPEAT('*',8)

  CONTAINS

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

  !> Whitelist of public symbols (parameters)
  PUBLIC :: MSGINTFLD_STREAM_E
  PUBLIC :: MSGINTFLD_TYPE_E
  PUBLIC :: MSGINTFLD_CLASS_E
  PUBLIC :: MSGINTFLD_PARAM_E
  PUBLIC :: MSGINTFLD_LEVTYPE_E
  PUBLIC :: MSGINTFLD_LEVELIST_E
  PUBLIC :: MSGINTFLD_DIRECTION_E
  PUBLIC :: MSGINTFLD_FREQUENCY_E
  PUBLIC :: MSGINTFLD_MODEL_E
  PUBLIC :: MSGINTFLD_REPRES_E
  PUBLIC :: MSGSTRINGFLD_EXPVER_E
  PUBLIC :: N_MSGFLDS

  !> Whitelist of public symbols (procedures)
  PUBLIC :: IMSGINTFLDS2CMSGINTFLDS
  PUBLIC :: CMSGINTFLDS2IMSGINTFLDS
  PUBLIC :: IMSGFLOATFLDS2CMSGFLOATFLDS
  PUBLIC :: CMSGFLOATFLDS2IMSGFLOATFLDS
  PUBLIC :: IMSGSTRINGFLDS2CMSGSTRINGFLDS
  PUBLIC :: CMSGSTRINGFLDS2IMSGSTRINGFLDS

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_INT'
FUNCTION FORTRAN_MESSAGE_SET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
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
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

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

END FUNCTION FORTRAN_MESSAGE_SET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_STRING'
FUNCTION FORTRAN_MESSAGE_SET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
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
  CHARACTER(LEN=*),         INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

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

END FUNCTION FORTRAN_MESSAGE_SET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_INT'
FUNCTION FORTRAN_MESSAGE_GET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
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
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

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

  VALUE = 0_JPIB_K

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

END FUNCTION FORTRAN_MESSAGE_GET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_STRING'
FUNCTION FORTRAN_MESSAGE_GET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
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
  CHARACTER(LEN=4),         INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

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

  VALUE = 'XXXX'

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

END FUNCTION FORTRAN_MESSAGE_GET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGINTFLDS2CMSGINTFLDS'
PP_THREAD_SAFE FUNCTION IMSGINTFLDS2CMSGINTFLDS( IMSGINTFLDS, CMSGINTFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGINTFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGINTFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGINTFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGINTFLDS )
  CASE ( MSGINTFLD_STREAM_E )
    CMSGINTFLDS = 'stream'
  CASE ( MSGINTFLD_TYPE_E )
    CMSGINTFLDS = 'type'
  CASE ( MSGINTFLD_CLASS_E )
    CMSGINTFLDS = 'class'
  CASE ( MSGINTFLD_PARAM_E )
    CMSGINTFLDS = 'param'
  CASE ( MSGINTFLD_LEVTYPE_E )
    CMSGINTFLDS = 'levtype'
  CASE ( MSGINTFLD_LEVELIST_E )
    CMSGINTFLDS = 'levelist'
  CASE ( MSGINTFLD_DIRECTION_E )
    CMSGINTFLDS = 'direction'
  CASE ( MSGINTFLD_FREQUENCY_E )
    CMSGINTFLDS = 'frequency'
  CASE ( MSGINTFLD_MODEL_E )
    CMSGINTFLDS = 'model'
  CASE ( MSGINTFLD_REPRES_E )
    CMSGINTFLDS = 'repres'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGINTFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGINTFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGINTFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGINTFLDS2CMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGINTFLDS2IMSGINTFLDS'
PP_THREAD_SAFE FUNCTION CMSGINTFLDS2IMSGINTFLDS( CMSGINTFLDS, IMSGINTFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGINTFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGINTFLDS)) :: LOC_CMSGINTFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGINTFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGINTFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGINTFLDS, LOC_CMSGINTFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGINTFLDS)) )
  CASE ( 'stream' )
    IMSGINTFLDS = MSGINTFLD_STREAM_E
  CASE ( 'type' )
    IMSGINTFLDS = MSGINTFLD_TYPE_E
  CASE ( 'class' )
    IMSGINTFLDS = MSGINTFLD_CLASS_E
  CASE ( 'param' )
    IMSGINTFLDS = MSGINTFLD_PARAM_E
  CASE ( 'levtype' )
    IMSGINTFLDS = MSGINTFLD_LEVTYPE_E
  CASE ( 'levelist' )
    IMSGINTFLDS = MSGINTFLD_LEVELIST_E
  CASE ( 'direction' )
    IMSGINTFLDS = MSGINTFLD_DIRECTION_E
  CASE ( 'frequency' )
    IMSGINTFLDS = MSGINTFLD_FREQUENCY_E
  CASE ( 'model' )
    IMSGINTFLDS = MSGINTFLD_MODEL_E
  CASE ( 'repres' )
    IMSGINTFLDS = MSGINTFLD_REPRES_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGINTFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGINTFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cintop_unary: '//TRIM(ADJUSTL(CMSGINTFLDS)) )
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

END FUNCTION CMSGINTFLDS2IMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGFLOATFLDS2IMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION CMSGFLOATFLDS2IMSGFLOATFLDS( CMSGFLOATFLDS, IMSGFLOATFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGFLOATFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGFLOATFLDS)) :: LOC_CMSGFLOATFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGFLOATFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGFLOATFLDS, LOC_CMSGFLOATFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGFLOATFLDS)) )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cfloatop_unary: '//TRIM(ADJUSTL(CMSGFLOATFLDS)) )
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

END FUNCTION CMSGFLOATFLDS2IMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGFLOATFLDS2CMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION IMSGFLOATFLDS2CMSGFLOATFLDS( IMSGFLOATFLDS, CMSGFLOATFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGFLOATFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGFLOATFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGFLOATFLDS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGFLOATFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGFLOATFLDS2CMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGSTRINGFLDS2IMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION CMSGSTRINGFLDS2IMSGSTRINGFLDS( CMSGSTRINGFLDS, IMSGSTRINGFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGSTRINGFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGSTRINGFLDS)) :: LOC_CMSGSTRINGFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGSTRINGFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGSTRINGFLDS, LOC_CMSGSTRINGFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGSTRINGFLDS)) )
  CASE ('expver')
    IMSGSTRINGFLDS = MSGSTRINGFLD_EXPVER_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cstringop_unary: '//TRIM(ADJUSTL(CMSGSTRINGFLDS)) )
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

END FUNCTION CMSGSTRINGFLDS2IMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGSTRINGFLDS2CMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION IMSGSTRINGFLDS2CMSGSTRINGFLDS( IMSGSTRINGFLDS, CMSGSTRINGFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGSTRINGFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGSTRINGFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGSTRINGFLDS )
  CASE (MSGSTRINGFLD_EXPVER_E)
    CMSGSTRINGFLDS = 'expver'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGSTRINGFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGSTRINGFLDS2CMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FORTRAN_MESSAGE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
