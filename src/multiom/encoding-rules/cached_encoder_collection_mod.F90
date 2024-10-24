
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'cached_encoder_collection_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CACHED_ENCODER_COLLECTION_MOD'
MODULE CACHED_ENCODER_COLLECTION_MOD

  ! Symbols imported from other modules within the project.
  USE :: CACHED_ENCODER_MOD,    ONLY: CACHED_ENCODER_T
  USE :: METADATA_BASE_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> @brief Datatype used to store a key-value pair
TYPE :: CACHED_ENCODER_COLLECTION_T

  !> Default visibility of the type
  PRIVATE

  !> Temporary encoder used for lazy initialization
  CLASS(GRIB_SECTION_BASE_A), POINTER :: TEMP_ENCODER => NULL()

  !> Encoders that satisfy the rules
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER :: ENCODERS => NULL()

CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT          => CACHED_ENCODER_COLLECTION_INIT
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: SIZE          => CACHED_ENCODER_COLLECTION_SIZE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ENCODE        => CACHED_ENCODER_COLLECTION_ENCODE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: SWAP_DATA     => CACHED_ENCODER_COLLECTION_SWAP_DATA
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE          => CACHED_ENCODER_COLLECTION_FREE
END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: CACHED_ENCODER_COLLECTION_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_INIT'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_INIT( THIS, MSG, PAR, METADATA, ENCODING_RULES, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,            ONLY: JPIB_K
  USE :: HOOKS_MOD,                    ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,            ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD,        ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB_ENCODER_OPTIONS_MOD,     ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: ENCODING_RULE_COLLECTION_MOD, ONLY: ENCODING_RULE_COLLECTION_T
  USE :: PARAMETRIZATION_MOD,          ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,          ONLY: FORTRAN_MESSAGE_T
  USE :: GRIB2_ENCODER_MOD,            ONLY: GRIB2_ENCODER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),            INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),            INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,    INTENT(IN)    :: METADATA
  TYPE(ENCODING_RULE_COLLECTION_T),   INTENT(IN)    :: ENCODING_RULES
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(:), POINTER :: MATCH_LIST
  CHARACTER(LEN=256) :: NAME
  CHARACTER(LEN=256) :: TAG
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_RULE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_ENCODER_FROM_RULE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_CACHED_ENCODER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_ENCODER=5_JPIB_K

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

  ! Get the encoders that match the rules
  MATCH_LIST => NULL()
  PP_TRYCALL(ERRFLAG_MATCH_RULE) ENCODING_RULES%MATCH( MSG, PAR, MATCH_LIST, HOOKS )

  IF ( ASSOCIATED(MATCH_LIST) ) THEN
    ALLOCATE( THIS%ENCODERS(SIZE(MATCH_LIST)), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS )

    DO I = 1, SIZE(MATCH_LIST)

      !> Initialize the pointer
      ENCODER => NULL()

      !> Get the encoder from the rule
      PP_TRYCALL(ERRFLAG_GET_ENCODER_FROM_RULE) ENCODING_RULES%GET_ENCODER( MATCH_LIST(I), NAME, TAG, ENCODER, HOOKS )

      !> Logging
      PP_LOG_INFO( 'Adding encoder "'//TRIM(ADJUSTL(NAME))//'" from rule: "'//TRIME(ADJUSTL(TAG))//'"' )

      !> Add the encoder to the cache
      PP_TRYCALL(ERRFLAG_INIT_CACHED_ENCODER) THIS%ENCODERS(I)%INIT( MSG, PAR, METADATA, ENCODER, OPT, HOOKS )
    ENDDO

  ELSE

    !> Allocate the temporary encoder
    ALLOCATE( GRIB2_ENCODER_T::THIS%TEMP_ENCODER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS )

    !> Initialize lazyly the temporary encoder
    PP_TRYCALL(ERRFLAG_INIT_ENCODER) THIS%TEMP_ENCODER%INIT( MSG, PAR, OPT, HOOKS )

    !> No encoders match the rules
    ALLOCATE( THIS%ENCODERS(1), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS )

    !> Add the encoder to the cache
    PP_TRYCALL(ERRFLAG_INIT_CACHED_ENCODER) THIS%ENCODERS(I)%INIT( MSG, PAR, METADATA, THIS%TEMP_ENCODER, OPT, HOOKS )

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
    CASE(ERRFLAG_MATCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match rule' )
    CASE(ERRFLAG_GET_ENCODER_FROM_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Get encoder from rule' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate encoders' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_INIT_CACHED_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Init cached encoder' )
    CASE(ERRFLAG_INIT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Init encoder' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_SIZE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_SIZE( THIS, SZ, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
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
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: SZ
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  ! Get the size
  SZ = SIZE(THIS%ENCODERS)

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_ENCODE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_ENCODE( THIS, ID, &
&       MSG, PAR, METADATA, ENCODING_DONE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENCODING_RULES_MOD,       ONLY: ENCODING_RULE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: ID
  TYPE(FORTRAN_MESSAGE_T),            INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),            INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,    INTENT(IN)    :: METADATA
  LOGICAL,                            INTENT(OUT)   :: ENCODING_DONE
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ID_LESS_THAN_ZERO=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ID_GREATER_THAN_SIZE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ENCODE=4_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS), ERRFLAG_ENCODERS_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ID .LE. 0, ERRFLAG_ID_LESS_THAN_ZERO )
  PP_DEBUG_CRITICAL_COND_THROW( ID .GT. SIZE(THIS%ENCODERS), ERRFLAG_ID_GREATER_THAN_SIZE )

  ! Encode the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ENCODE) THIS%ENCODERS(ID)%ENCODE( MSG, PAR, METADATA, ENCODING_DONE, OPT, HOOKS )

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
    CASE(ERRFLAG_ID_LESS_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ID less than zero' )
    CASE(ERRFLAG_ID_GREATER_THAN_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ID greater than size' )
    CASE(ERRFLAG_UNABLE_TO_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_ENCODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_SWAP_DATA'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_SWAP_DATA( THIS, OTHER, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
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
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: OTHER
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER :: TMP

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

  ! Swap the data
  TMP => THIS%ENCODERS
  THIS%ENCODERS => OTHER%ENCODERS
  OTHER%ENCODERS => TMP

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


END FUNCTION CACHED_ENCODER_COLLECTION_SWAP_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_FREE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
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
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_ENCODERS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS=2_JPIB_K

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

  ! Free all the encoders
  DO I = 1, SIZE(THIS%ENCODERS)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_ENCODERS) THIS%ENCODERS(I)%FREE( OPT, HOOKS )
  ENDDO

  ! Release memory
  DEALLOCATE( THIS%ENCODERS, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS )

  ! Deallocate temporary encoder
  IF ( ASSOCIATED(THIS%TEMP_ENCODER) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_ENCODERS) THIS%TEMP_ENCODER%FREE( OPT, HOOKS )
    DEALLOCATE( THIS%TEMP_ENCODER, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS )
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
    CASE(ERRFLAG_UNABLE_TO_FREE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free encoders' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate encoders' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
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

  ! Exit point (on error)
  RETURN


END FUNCTION CACHED_ENCODER_COLLECTION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CACHED_ENCODER_COLLECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME