! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'parametrization_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PARAMETRIZATION_MOD'
MODULE PARAMETRIZATION_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibility of the module
PRIVATE


  !> Fileds of the message
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PARFLDS=1_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_SCALEFACTOR_E=1_JPIB_K

  !> Whitelist of public symbols
  TYPE :: LEVEL_PAR_T
    REAL(KIND=JPRD_K), DIMENSION(:), POINTER :: PV => NULL()
  END TYPE

  !> Wave information I am not able to fit into mars message
  TYPE :: WAVE_PAR_T
    REAL(KIND=JPRD_K), POINTER, DIMENSION(:) :: DIRS_ => NULL()
    REAL(KIND=JPRD_K), POINTER, DIMENSION(:) :: FREQ_ => NULL()
  END TYPE

  !> Ensemble information I am not able to fit into mars message
  TYPE :: ENSEMBLE_PAR_T
    INTEGER(KIND=JPIB_K) :: TYPE_OF_ENSEMBLE_FORECAST_= UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: PERTURBATION_NUMBER_=-1_JPIB_K
    INTEGER(KIND=JPIB_K) :: NUMBER_OF_FORECAST_IN_ENSEMBLE_=-1_JPIB_K
  END TYPE

  !> Satellite infomration I am not able to fit into mars message
  TYPE :: SATELLITE_PAR_T
    INTEGER(KIND=JPIB_K) :: SATELLITE_SERIES = UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: SCALED_FACTOR_OF_CENTRAL_VAWENUMBER = UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: SCALED_VALUE_OF_CENTRAL_VAWENUMBER = UNDEF_PARAM_E
  END TYPE


  !> Collection of all information I am not able to fit into mars message
  TYPE :: PARAMETRIZATION_T

    ! Tables version
    INTEGER(KIND=JPIB_K) :: TABLES_VERSION=UNDEF_PARAM_E

    ! Scalar parameters
    INTEGER(KIND=JPIB_K) :: INITIAL_STEP=UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_STEP_IN_SECONDS=UNDEF_PARAM_E

    ! Configorations to be descussed and eventually to be integrated in the MARS keywords
    TYPE(LEVEL_PAR_T)     :: LEVELS
    TYPE(ENSEMBLE_PAR_T)  :: ENSEMBLE
    TYPE(WAVE_PAR_T)      :: WAVE
    TYPE(SATELLITE_PAR_T) :: SATELLITE

    !> Scal facto to be applie to the values
    REAL(KIND=JPRD_K) :: VALUES_SCALE_FACTOR=0.0_JPRD_K

  CONTAINS

    !> Set fields by field ID
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT    => PARAMETRIZATION_SET_INT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => PARAMETRIZATION_SET_STRING
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_FLOAT  => PARAMETRIZATION_SET_FLOAT
    GENERIC :: SET => SET_INT
    GENERIC :: SET => SET_STRING
    GENERIC :: SET => SET_FLOAT



    !> Set fields by field ID
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_INT    => PARAMETRIZATION_GET_INT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_STRING => PARAMETRIZATION_GET_STRING
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_FLOAT  => PARAMETRIZATION_GET_FLOAT
    GENERIC :: GET => GET_INT
    GENERIC :: GET => GET_STRING
    GENERIC :: GET => GET_FLOAT

  END TYPE


  !> Whitelist of public symbols (types)
  PUBLIC :: PARAMETRIZATION_T

  !> Whitelist of public symbols (procedures)
  PUBLIC :: IPARINTFLDS2CPARINTFLDS
  PUBLIC :: CPARINTFLDS2IPARINTFLDS

  PUBLIC :: IPARFLOATFLDS2CPARFLOATFLDS
  PUBLIC :: CPARFLOATFLDS2IPARFLOATFLDS

  PUBLIC :: IPARSTRINGFLDS2CPARSTRINGFLDS
  PUBLIC :: CPARSTRINGFLDS2IPARSTRINGFLDS

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_INT'
FUNCTION PARAMETRIZATION_SET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
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

END FUNCTION PARAMETRIZATION_SET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_STRING'
FUNCTION PARAMETRIZATION_SET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
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

END FUNCTION PARAMETRIZATION_SET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_FLOAT'
FUNCTION PARAMETRIZATION_SET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
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

END FUNCTION PARAMETRIZATION_SET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_INT'
FUNCTION PARAMETRIZATION_GET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
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

END FUNCTION PARAMETRIZATION_GET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_STRING'
FUNCTION PARAMETRIZATION_GET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
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

END FUNCTION PARAMETRIZATION_GET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_FLOAT'
FUNCTION PARAMETRIZATION_GET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
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

END FUNCTION PARAMETRIZATION_GET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARINTFLDS2CPARINTFLDS'
PP_THREAD_SAFE FUNCTION IPARINTFLDS2CPARINTFLDS( IPARINTFLDS, CPARINTFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARINTFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPARINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARINTFLD_UNARY=1_JPIB_K

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
  CPARINTFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IPARINTFLDS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARINTFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARINTFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARINTFLDS
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

END FUNCTION IPARINTFLDS2CPARINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARINTFLDS2IPARINTFLDS'
PP_THREAD_SAFE FUNCTION CPARINTFLDS2IPARINTFLDS( CPARINTFLDS, IPARINTFLDS, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARINTFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARINTFLDS)) :: LOC_CPARINTFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARINTFLD_UNARY=1_JPIB_K
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
  IPARINTFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARINTFLDS, LOC_CPARINTFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARINTFLDS)) )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARINTFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARINTFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cintop_unary: '//TRIM(ADJUSTL(CPARINTFLDS)) )
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

END FUNCTION CPARINTFLDS2IPARINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARFLOATFLDS2IPARFLOATFLDS'
PP_THREAD_SAFE FUNCTION CPARFLOATFLDS2IPARFLOATFLDS( CPARFLOATFLDS, IPARFLOATFLDS, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARFLOATFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARFLOATFLDS)) :: LOC_CPARFLOATFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY=1_JPIB_K
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
  IPARFLOATFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARFLOATFLDS, LOC_CPARFLOATFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARFLOATFLDS)) )
  CASE ( 'scale-factor' )
    IPARFLOATFLDS = MSGINTFLD_SCALEFACTOR_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cfloatop_unary: '//TRIM(ADJUSTL(CPARFLOATFLDS)) )
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

END FUNCTION CPARFLOATFLDS2IPARFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARFLOATFLDS2CPARFLOATFLDS'
PP_THREAD_SAFE FUNCTION IPARFLOATFLDS2CPARFLOATFLDS( IPARFLOATFLDS, CPARFLOATFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARFLOATFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPARFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY=1_JPIB_K

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
  CPARFLOATFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IPARFLOATFLDS )
  CASE (MSGINTFLD_SCALEFACTOR_E)
    CPARFLOATFLDS = 'scale-factor'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARFLOATFLDS
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

END FUNCTION IPARFLOATFLDS2CPARFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARSTRINGFLDS2IPARSTRINGFLDS'
PP_THREAD_SAFE FUNCTION CPARSTRINGFLDS2IPARSTRINGFLDS( CPARSTRINGFLDS, IPARSTRINGFLDS, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARSTRINGFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARSTRINGFLDS)) :: LOC_CPARSTRINGFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY=1_JPIB_K
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
  IPARSTRINGFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARSTRINGFLDS, LOC_CPARSTRINGFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARSTRINGFLDS)) )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cstringop_unary: '//TRIM(ADJUSTL(CPARSTRINGFLDS)) )
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

END FUNCTION CPARSTRINGFLDS2IPARSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARSTRINGFLDS2CPARSTRINGFLDS'
PP_THREAD_SAFE FUNCTION IPARSTRINGFLDS2CPARSTRINGFLDS( IPARSTRINGFLDS, CPARSTRINGFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARSTRINGFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPARSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY=1_JPIB_K

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
  CPARSTRINGFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IPARSTRINGFLDS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARSTRINGFLDS
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

END FUNCTION IPARSTRINGFLDS2CPARSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE PARAMETRIZATION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
