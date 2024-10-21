! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'enumerators_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENUMERATORS_MOD'
MODULE ENUMERATORS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Enumerators for prefixes
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_MODEL_LEVEL_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_PRESSURE_LEVEL_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_VORTICITY_LEVEL_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_THETA_LEVEL_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_SURFACE_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_WAVE_INT_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PREFIX_WAVE_SPEC_E=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PREFIXES=7_JPIB_K

  ! Enumerators for levType
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_HHL_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_HPL_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_HL_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_ML_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_O2D_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_O3D_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_PL_E=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_PT_E=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_PV_E=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_SFC_E=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: LEVTYPE_SOL_E=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_LEVTYPES=11_JPIB_K

  ! Enumerators for representations
  INTEGER(KIND=JPIB_K), PARAMETER :: REPRES_LATLONG_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: REPRES_GAUSSIANGRID_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: REPRES_SPHERICALHARMONICS_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_REPRES=3_JPIB_K

  ! Enumerators for models
  INTEGER(KIND=JPIB_K), PARAMETER :: MODEL_ATMOSPHERE_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MODEL_OCEAN_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MODEL_WAVE_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_MODELS=3_JPIB_K

  ! Enumerators for precision
  INTEGER(KIND=JPIB_K), PARAMETER :: PRECISION_SP_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PRECISION_DP_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PRECISIONS=2_JPIB_K

  ! Grib editions
  INTEGER(KIND=JPIB_K), PARAMETER :: EDITION_GRIB1_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EDITION_GRIB2_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_EDITIONS=2_JPIB_K

  ! Enumerators for packing type
  INTEGER(KIND=JPIB_K), PARAMETER :: PACKING_GRIB_SIMPLE_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PACKING_GRIB_CCSDS_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PACKING_GRIB_COMPLEX_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PACKING=3_JPIB_K

  ! Enumerators for type of statisticsal process
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_MIN_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_MAX_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_STATISTICAL_PROCESS_MODE_E=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_TYPE_OF_STATISTICAL_PROCESS=7_JPIB_K

  ! Enumeators for type of time range
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_TIME_RANGE_INSTANT_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_TIME_RANGE_FROM_STEP0_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_TIME_RANGE_FROM_LASTPP_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_OF_TIME_RANGE_FIXED_SIZE_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_TYPE_OF_TIME_RANGE=4_JPIB_K

  !> Enumerators for the integer filters to be used in filtering operations
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_MATCH_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_IGNORE_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_GT_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_GE_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_LT_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_INT_LE_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_FLT_INT=6_JPIB_K

  !> Enumerators for the operations between filters defined in this module
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_COMPOSE_ALL_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_COMPOSE_ANY_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_COMPOSE_NONE_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: FLT_COMPOSE_ONE_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_FLT_COMPOSE=4_JPIB_K

  ! Undefined parameters
  INTEGER(KIND=JPIB_K), PARAMETER :: UNDEF_PARAM_E=-999999999_JPIB_K

  !>
  !>
  !>  Whitelist of public symbols (parameters)

  ! Enumerators for prefixes
  PUBLIC :: PREFIX_MODEL_LEVEL_E
  PUBLIC :: PREFIX_PRESSURE_LEVEL_E
  PUBLIC :: PREFIX_VORTICITY_LEVEL_E
  PUBLIC :: PREFIX_THETA_LEVEL_E
  PUBLIC :: PREFIX_SURFACE_E
  PUBLIC :: PREFIX_WAVE_INT_E
  PUBLIC :: PREFIX_WAVE_SPEC_E
  PUBLIC :: N_PREFIXES

  ! Enumerators for levType
  PUBLIC :: LEVTYPE_HHL_E
  PUBLIC :: LEVTYPE_HPL_E
  PUBLIC :: LEVTYPE_HL_E
  PUBLIC :: LEVTYPE_ML_E
  PUBLIC :: LEVTYPE_O2D_E
  PUBLIC :: LEVTYPE_O3D_E
  PUBLIC :: LEVTYPE_PL_E
  PUBLIC :: LEVTYPE_PT_E
  PUBLIC :: LEVTYPE_PV_E
  PUBLIC :: LEVTYPE_SFC_E
  PUBLIC :: LEVTYPE_SOL_E
  PUBLIC :: N_LEVTYPES

  ! Enumerators for representations
  PUBLIC :: REPRES_LATLONG_E
  PUBLIC :: REPRES_GAUSSIANGRID_E
  PUBLIC :: REPRES_SPHERICALHARMONICS_E
  PUBLIC :: N_REPRES

  ! Enumerators for models
  PUBLIC :: MODEL_ATMOSPHERE_E
  PUBLIC :: MODEL_OCEAN_E
  PUBLIC :: MODEL_WAVE_E
  PUBLIC :: N_MODELS

  ! Enumerators for precision
  PUBLIC :: PRECISION_SP_E
  PUBLIC :: PRECISION_DP_E
  PUBLIC :: N_PRECISIONS

  ! Grib editions
  PUBLIC :: EDITION_GRIB1_E
  PUBLIC :: EDITION_GRIB2_E
  PUBLIC :: N_EDITIONS

  ! Enumerators for packing type
  PUBLIC :: PACKING_GRIB_SIMPLE_E
  PUBLIC :: PACKING_GRIB_CCSDS_E
  PUBLIC :: PACKING_GRIB_COMPLEX_E
  PUBLIC :: N_PACKING


  ! Enumerators for type of statisticsal process
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  PUBLIC :: N_TYPE_OF_STATISTICAL_PROCESS

  ! Enumeators for type of time range
  PUBLIC :: TYPE_OF_TIME_RANGE_INSTANT_E
  PUBLIC :: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  PUBLIC :: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  PUBLIC :: TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  PUBLIC :: N_TYPE_OF_TIME_RANGE

  ! Enumerators for the integer filters to be used in filtering operations
  PUBLIC :: FLT_INT_MATCH_E
  PUBLIC :: FLT_INT_IGNORE_E
  PUBLIC :: FLT_INT_GT_E
  PUBLIC :: FLT_INT_GE_E
  PUBLIC :: FLT_INT_LT_E
  PUBLIC :: FLT_INT_LE_E
  PUBLIC :: N_FLT_INT

  ! Enumerators for the operations between filters defined in this module
  PUBLIC :: FLT_COMPOSE_ALL_E
  PUBLIC :: FLT_COMPOSE_ANY_E
  PUBLIC :: FLT_COMPOSE_NONE_E
  PUBLIC :: FLT_COMPOSE_ONE_E
  PUBLIC :: N_FLT_COMPOSE

  ! Undefined parameters
  PUBLIC :: UNDEF_PARAM_E

  !>  Whitelist of public symbols (procedures)
  PUBLIC :: IPREFIX2CPREFIX
  PUBLIC :: CPREFIX2IPREFIX
  PUBLIC :: ILEVTYPE2CLEVTYPE
  PUBLIC :: CLEVTYPE2ILEVTYPE
  PUBLIC :: IREPRES2CREPRES
  PUBLIC :: CREPRES2IREPRES
  PUBLIC :: IMODEL2CMODEL
  PUBLIC :: CMODEL2IMODEL
  PUBLIC :: IPRECISION2CPRECISION
  PUBLIC :: CPRECISION2IPRECISION
  PUBLIC :: IEDITION2CEDITION
  PUBLIC :: CEDITION2IEDITION
  PUBLIC :: IPACKING2CPACKING
  PUBLIC :: CPACKING2IPACKING
  PUBLIC :: ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS
  PUBLIC :: CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
  PUBLIC :: ITYPE_OF_TIME_RANGE2CTYPE_OF_TIME_RANGE
  PUBLIC :: CTYPE_OF_TIME_RANGE2ITYPE_OF_TIME_RANGE
  PUBLIC :: FLT_INT_IOP2COP
  PUBLIC :: FLT_INT_COP2IOP
  PUBLIC :: FLT_COMPOSE_COP2IOP
  PUBLIC :: FLT_COMPOSE_IOP2COP

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPREFIX2CPREFIX'
PP_THREAD_SAFE FUNCTION IPREFIX2CPREFIX( IPREFIX, CPREFIX, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPREFIX
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPREFIX
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PREFIX=1_JPIB_K

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
  CPREFIX = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IPREFIX )

  CASE ( PREFIX_MODEL_LEVEL_E )
    CPREFIX = 'm'
  CASE ( PREFIX_PRESSURE_LEVEL_E )
    CPREFIX = 'p'
  CASE ( PREFIX_VORTICITY_LEVEL_E )
    CPREFIX = 'v'
  CASE ( PREFIX_THETA_LEVEL_E )
    CPREFIX = 't'
  CASE ( PREFIX_SURFACE_E )
    CPREFIX = 's'
  CASE ( PREFIX_WAVE_INT_E )
    CPREFIX = 'wv_int'
  CASE ( PREFIX_WAVE_SPEC_E )
    CPREFIX = 'wv_spec'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PREFIX )
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
    CASE (ERRFLAG_UNKNOWN_PREFIX)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPREFIX
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iprefix: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPREFIX2CPREFIX
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPREFIX2IPREFIX'
PP_THREAD_SAFE FUNCTION CPREFIX2IPREFIX( CPREFIX, IPREFIX, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPREFIX
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPREFIX
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PREFIX=1_JPIB_K

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
  IPREFIX = UNDEF_PARAM_E

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(CPREFIX)) )

  CASE ( 'ML', 'm' )
    IPREFIX = PREFIX_MODEL_LEVEL_E
  CASE ( 'PL', 'p' )
    IPREFIX = PREFIX_PRESSURE_LEVEL_E
  CASE ( 'PV', 'v' )
    IPREFIX = PREFIX_VORTICITY_LEVEL_E
  CASE ( 'TH', 't' )
    IPREFIX = PREFIX_THETA_LEVEL_E
  CASE ( 'SF', 's', 'sfc' )
    IPREFIX = PREFIX_SURFACE_E
  CASE ( 'wv_int' )
    IPREFIX = PREFIX_WAVE_INT_E
  CASE ( 'wv_spec' )
    IPREFIX = PREFIX_WAVE_SPEC_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PREFIX )
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
    CASE (ERRFLAG_UNKNOWN_PREFIX)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cprefix: '//TRIM(ADJUSTL(CPREFIX)) )
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

END FUNCTION CPREFIX2IPREFIX
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ILEVTYPE2CLEVTYPE'
PP_THREAD_SAFE FUNCTION ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ILEVTYPE
  CHARACTER(LEN=16),    INTENT(OUT)   :: CLEVTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE=1_JPIB_K

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
  CLEVTYPE = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( ILEVTYPE )

  CASE ( LEVTYPE_HHL_E )
    CLEVTYPE = 'hhl'
  CASE ( LEVTYPE_HPL_E )
    CLEVTYPE = 'hpl'
  CASE ( LEVTYPE_HL_E )
    CLEVTYPE = 'hl'
  CASE ( LEVTYPE_ML_E )
    CLEVTYPE = 'ml'
  CASE ( LEVTYPE_O2D_E )
    CLEVTYPE = 'o2d'
  CASE ( LEVTYPE_O3D_E )
    CLEVTYPE = 'o3d'
  CASE ( LEVTYPE_PL_E )
    CLEVTYPE = 'pl'
  CASE ( LEVTYPE_PT_E )
    CLEVTYPE = 'pt'
  CASE ( LEVTYPE_PV_E )
    CLEVTYPE = 'pv'
  CASE ( LEVTYPE_SFC_E )
    CLEVTYPE = 'sfc'
  CASE ( LEVTYPE_SOL_E )
    CLEVTYPE = 'sol'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
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
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) ILEVTYPE
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown ilevtype: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION ILEVTYPE2CLEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CLEVTYPE2ILEVTYPE'
PP_THREAD_SAFE FUNCTION CLEVTYPE2ILEVTYPE( CLEVTYPE, ILEVTYPE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CLEVTYPE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ILEVTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE=1_JPIB_K

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
  ILEVTYPE = UNDEF_PARAM_E

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(CLEVTYPE)) )
  CASE ( 'hhl' )
    ILEVTYPE = LEVTYPE_HHL_E
  CASE ( 'hpl' )
    ILEVTYPE = LEVTYPE_HPL_E
  CASE ( 'hl' )
    ILEVTYPE = LEVTYPE_HL_E
  CASE ( 'ml' )
    ILEVTYPE = LEVTYPE_ML_E
  CASE ( 'o2d' )
    ILEVTYPE = LEVTYPE_O2D_E
  CASE ( 'o3d' )
    ILEVTYPE = LEVTYPE_O3D_E
  CASE ( 'pl' )
    ILEVTYPE = LEVTYPE_PL_E
  CASE ( 'pt' )
    ILEVTYPE = LEVTYPE_PT_E
  CASE ( 'pv' )
    ILEVTYPE = LEVTYPE_PV_E
  CASE ( 'sfc' )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE ( 'sol' )
    ILEVTYPE = LEVTYPE_SOL_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
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
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown clevtype: '//TRIM(ADJUSTL(CLEVTYPE)) )
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

END FUNCTION CLEVTYPE2ILEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IREPRES2CREPRES'
PP_THREAD_SAFE FUNCTION IREPRES2CREPRES( IREPRES, CREPRES, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IREPRES
  CHARACTER(LEN=16),    INTENT(OUT)   :: CREPRES
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_REPRES=1_JPIB_K

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
  CREPRES = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( IREPRES )

  CASE ( REPRES_LATLONG_E )
    CREPRES = 'll'
  CASE ( REPRES_GAUSSIANGRID_E )
    CREPRES = 'gg'
  CASE ( REPRES_SPHERICALHARMONICS_E )
    CREPRES = 'sh'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_REPRES )
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
    CASE (ERRFLAG_UNKNOWN_REPRES)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IREPRES
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown irepres: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IREPRES2CREPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CREPRES2IREPRES'
PP_THREAD_SAFE FUNCTION CREPRES2IREPRES( CREPRES, IREPRES, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CREPRES
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IREPRES
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_REPRES=1_JPIB_K

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
  IREPRES = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CREPRES)) )

  CASE ( 'll' )
    IREPRES = REPRES_LATLONG_E
  CASE ( 'gg' )
    IREPRES = REPRES_GAUSSIANGRID_E
  CASE ( 'sh' )
    IREPRES = REPRES_SPHERICALHARMONICS_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_REPRES )
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
    CASE (ERRFLAG_UNKNOWN_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown crepres: '//TRIM(ADJUSTL(CREPRES)) )
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

END FUNCTION CREPRES2IREPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMODEL2CMODEL'
PP_THREAD_SAFE FUNCTION IMODEL2CMODEL( IMODEL, CMODEL, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMODEL
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMODEL
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MODEL=1_JPIB_K

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
  CMODEL = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( IMODEL )

  CASE ( MODEL_ATMOSPHERE_E )
    CMODEL = 'atmosphere'
  CASE ( MODEL_OCEAN_E )
    CMODEL = 'ocean'
  CASE ( MODEL_WAVE_E )
    CMODEL = 'wave'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MODEL )
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
    CASE (ERRFLAG_UNKNOWN_MODEL)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMODEL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown imodel: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMODEL2CMODEL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMODEL2IMODEL'
PP_THREAD_SAFE FUNCTION CMODEL2IMODEL( CMODEL, IMODEL, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CMODEL
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMODEL
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MODEL=1_JPIB_K

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
  IMODEL = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CMODEL)) )

  CASE ( 'atmosphere' )
    IMODEL = MODEL_ATMOSPHERE_E
  CASE ( 'ocean' )
    IMODEL = MODEL_OCEAN_E
  CASE ( 'wave' )
    IMODEL = MODEL_WAVE_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MODEL )
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
    CASE (ERRFLAG_UNKNOWN_MODEL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cmodel: '//TRIM(ADJUSTL(CMODEL)) )
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

END FUNCTION CMODEL2IMODEL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPRECISION2CPRECISION'
PP_THREAD_SAFE FUNCTION IPRECISION2CPRECISION( IPRECISION, CPRECISION, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPRECISION
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPRECISION
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PRECISION=1_JPIB_K

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
  CPRECISION = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( IPRECISION )

  CASE ( PRECISION_DP_E )
    CPRECISION = 'dp'
  CASE ( PRECISION_SP_E )
    CPRECISION = 'sp'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PRECISION )
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
    CASE (ERRFLAG_UNKNOWN_PRECISION)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPRECISION
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iprecision: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPRECISION2CPRECISION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPRECISION2IPRECISION'
PP_THREAD_SAFE FUNCTION CPRECISION2IPRECISION( CPRECISION, IPRECISION, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPRECISION
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPRECISION
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PRECISION=1_JPIB_K

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
  IPRECISION = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CPRECISION)) )

  CASE ( 'sp' )
    IPRECISION = PRECISION_SP_E
  CASE ( 'dp' )
    IPRECISION = PRECISION_DP_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PRECISION )
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
    CASE (ERRFLAG_UNKNOWN_PRECISION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cprecision: '//TRIM(ADJUSTL(CPRECISION)) )
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

END FUNCTION CPRECISION2IPRECISION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IEDITION2CEDITION'
PP_THREAD_SAFE FUNCTION IEDITION2CEDITION( IEDITION, CEDITION, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IEDITION
  CHARACTER(LEN=16),    INTENT(OUT)   :: CEDITION
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_EDITION=1_JPIB_K

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
  CEDITION = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( IEDITION )

  CASE ( EDITION_GRIB1_E )
    CEDITION = 'grib1'
  CASE ( EDITION_GRIB2_E )
    CEDITION = 'grib2'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_EDITION )
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
    CASE (ERRFLAG_UNKNOWN_EDITION)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IEDITION
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iedition: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IEDITION2CEDITION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CEDITION2IEDITION'
PP_THREAD_SAFE FUNCTION CEDITION2IEDITION( CEDITION, IEDITION, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CEDITION
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IEDITION
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_EDITION=1_JPIB_K

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
  IEDITION = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CEDITION)) )

  CASE ( 'grib1' )
    IEDITION = EDITION_GRIB1_E
  CASE ( 'grib2' )
    IEDITION = EDITION_GRIB2_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_EDITION )
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
    CASE (ERRFLAG_UNKNOWN_EDITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cedition: '//TRIM(ADJUSTL(CEDITION)) )
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

END FUNCTION CEDITION2IEDITION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPACKING2CPACKING'
PP_THREAD_SAFE FUNCTION IPACKING2CPACKING( IPACKING, CPACKING, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPACKING
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPACKING
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PACKING=1_JPIB_K

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
  CPACKING = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( IPACKING )

  CASE ( PACKING_GRIB_SIMPLE_E )
    CPACKING = 'grib-simple'
  CASE ( PACKING_GRIB_CCSDS_E )
    CPACKING = 'grib-ccsds'
  CASE ( PACKING_GRIB_COMPLEX_E )
    CPACKING = 'grib-complex'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PACKING )
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
    CASE (ERRFLAG_UNKNOWN_PACKING)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPACKING
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown ipacking: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPACKING2CPACKING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPACKING2IPACKING'
PP_THREAD_SAFE FUNCTION CPACKING2IPACKING( CPACKING, IPACKING, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CPACKING
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPACKING
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PACKING=1_JPIB_K

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
  IPACKING = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CPACKING)) )

  CASE ( 'grib-simple' )
    IPACKING = PACKING_GRIB_SIMPLE_E
  CASE ( 'grib-ccsds' )
    IPACKING = PACKING_GRIB_CCSDS_E
  CASE ( 'grib-complex' )
    IPACKING = PACKING_GRIB_COMPLEX_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PACKING )
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
    CASE (ERRFLAG_UNKNOWN_PACKING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cpacking: '//TRIM(ADJUSTL(CPACKING)) )
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

END FUNCTION CPACKING2IPACKING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS'
PP_THREAD_SAFE FUNCTION ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS( ITYPE_OF_STATISTICAL_PROCESS, CTYPE_OF_STATISTICAL_PROCESS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITYPE_OF_STATISTICAL_PROCESS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CTYPE_OF_STATISTICAL_PROCESS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS=1_JPIB_K

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
  CTYPE_OF_STATISTICAL_PROCESS = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( ITYPE_OF_STATISTICAL_PROCESS )

  CASE ( TYPE_OF_STATISTICAL_PROCESS_INSTANT_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'instant'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'average'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'accumul'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_MIN_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'min'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_MAX_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'max'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'severity'
  CASE ( TYPE_OF_STATISTICAL_PROCESS_MODE_E )
    CTYPE_OF_STATISTICAL_PROCESS = 'mode'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS )
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
    CASE (ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) ITYPE_OF_STATISTICAL_PROCESS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown itypeOfStatisticaProcess: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS'
PP_THREAD_SAFE FUNCTION CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS( CTYPE_OF_STATISTICAL_PROCESS, ITYPE_OF_STATISTICAL_PROCESS, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CTYPE_OF_STATISTICAL_PROCESS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ITYPE_OF_STATISTICAL_PROCESS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS=1_JPIB_K

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
  ITYPE_OF_STATISTICAL_PROCESS = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CTYPE_OF_STATISTICAL_PROCESS)) )

  CASE ( 'instant' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  CASE ( 'average' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  CASE ( 'accumul' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  CASE ( 'min' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_MIN_E
  CASE ( 'max' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_MAX_E
  CASE ( 'severity' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  CASE ( 'mode' )
    ITYPE_OF_STATISTICAL_PROCESS = TYPE_OF_STATISTICAL_PROCESS_MODE_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS )
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
    CASE (ERRFLAG_UNKNOWN_TYPE_OF_STATISTICAL_PROCESS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown ctypeOfStatisticaProcess: '//TRIM(ADJUSTL(CTYPE_OF_STATISTICAL_PROCESS)) )
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

END FUNCTION CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ITYPE_OF_TIME_RANGE2CTYPE_OF_TIME_RANGE'
PP_THREAD_SAFE FUNCTION ITYPE_OF_TIME_RANGE2CTYPE_OF_TIME_RANGE( ITYPE_OF_TIME_RANGE, CTYPE_OF_TIME_RANGE, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITYPE_OF_TIME_RANGE
  CHARACTER(LEN=16),    INTENT(OUT)   :: CTYPE_OF_TIME_RANGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE=1_JPIB_K

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
  CTYPE_OF_TIME_RANGE = REPEAT(' ', 16)

  !> Select the repres
  SELECT CASE ( ITYPE_OF_TIME_RANGE )

  CASE ( TYPE_OF_TIME_RANGE_INSTANT_E )
    CTYPE_OF_TIME_RANGE = 'instant'
  CASE ( TYPE_OF_TIME_RANGE_FROM_STEP0_E )
    CTYPE_OF_TIME_RANGE = 'from-step0'
  CASE ( TYPE_OF_TIME_RANGE_FROM_LASTPP_E )
    CTYPE_OF_TIME_RANGE = 'from-last-pp'
  CASE ( TYPE_OF_TIME_RANGE_FIXED_SIZE_E )
    CTYPE_OF_TIME_RANGE = 'fixed-size'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE )
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
    CASE (ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) ITYPE_OF_TIME_RANGE
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown itypeOfTimeRange: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION ITYPE_OF_TIME_RANGE2CTYPE_OF_TIME_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CTYPE_OF_TIME_RANGE2ITYPE_OF_TIME_RANGE'
PP_THREAD_SAFE FUNCTION CTYPE_OF_TIME_RANGE2ITYPE_OF_TIME_RANGE( CTYPE_OF_TIME_RANGE, ITYPE_OF_TIME_RANGE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: CTYPE_OF_TIME_RANGE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ITYPE_OF_TIME_RANGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE=1_JPIB_K

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
  ITYPE_OF_TIME_RANGE = UNDEF_PARAM_E

  !> Select the repres
  SELECT CASE ( TRIM(ADJUSTL(CTYPE_OF_TIME_RANGE)) )

  CASE ( 'instant' )
    ITYPE_OF_TIME_RANGE = TYPE_OF_TIME_RANGE_INSTANT_E
  CASE ( 'from-step0' )
    ITYPE_OF_TIME_RANGE = TYPE_OF_TIME_RANGE_FROM_STEP0_E
  CASE ( 'from-last-pp' )
    ITYPE_OF_TIME_RANGE = TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  CASE ( 'fixed-size' )
    ITYPE_OF_TIME_RANGE = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE )
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
    CASE (ERRFLAG_UNKNOWN_TYPE_OF_TIME_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown ctypeOfTimeRange: '//TRIM(ADJUSTL(CTYPE_OF_TIME_RANGE)) )
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

END FUNCTION CTYPE_OF_TIME_RANGE2ITYPE_OF_TIME_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a character operation (COP) to an integer operation (IOP).
!>
!> This function takes a string representing a character operation (`COP`) and
!> converts it into a corresponding integer operation (`IOP`). It uses hooks during
!> the conversion process to allow additional processing if needed.
!>
!> @param [in] COP   The character operation (`CHARACTER(LEN=*)`) to be converted.
!> @param [out] IOP  The resulting integer operation (`INTEGER(KIND=JPIB_K)`).
!> @param [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the conversion process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the conversion operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLT_INT_COP2IOP'
FUNCTION FLT_INT_COP2IOP( COP, IOP, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: COP
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K

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

  ! Initialize the output variable
  IOP = UNDEF_PARAM_E

  ! Apply operation
  SELECT CASE( COP )

  CASE( 'match', 'eq' )

    IOP = FLT_INT_MATCH_E

  CASE( 'ignore', 'ne' )

    IOP = FLT_INT_IGNORE_E

  CASE( 'greater-than', 'gt' )

    IOP = FLT_INT_GT_E

  CASE( 'greater-equal', 'ge' )

    IOP = FLT_INT_GE_E

  CASE( 'lower-than', 'lt' )

    IOP = FLT_INT_LT_E

  CASE( 'lower-equal', 'le' )

    IOP = FLT_INT_LE_E

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
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

END FUNCTION FLT_INT_COP2IOP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLT_INT_IOP2COP'
FUNCTION FLT_INT_IOP2COP( IOP, COP, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IOP
  CHARACTER(LEN=16),    INTENT(OUT)   :: COP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K

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

  ! Initialize the output variable
  COP = REPEAT( ' ', 16 )

  ! Apply operation
  SELECT CASE( IOP )

  CASE( FLT_INT_MATCH_E )

    COP = 'match'

  CASE( FLT_INT_IGNORE_E )

    COP = 'ignore'

  CASE( FLT_INT_GT_E )

    COP = 'greater-than'

  CASE( FLT_INT_GE_E )

    COP = 'greater-equal'

  CASE( FLT_INT_LT_E )

    COP = 'lower-than'

  CASE( FLT_INT_LE_E )

    COP = 'lower-equal'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
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

END FUNCTION FLT_INT_IOP2COP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Converts an integer operation code (IOP) to a character operation code (COP).
!>
!> This thread-safe function takes an integer representation of an operation code
!> (`IOP`) and converts it into its corresponding character operation code (`COP`).
!> The conversion process utilizes hooks for debugging, logging, and tracing
!> purposes.
!>
!> @param [in] IOP The integer representation of the operation code to be converted.
!> @param [out] COP The resulting character representation of the operation code
!>                  after conversion. The length of the character string is fixed at
!>                  16 characters.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the conversion process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLT_COMPOSE_IOP2COP'
PP_THREAD_SAFE FUNCTION FLT_COMPOSE_IOP2COP( IOP, COP, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)   :: IOP
  CHARACTER(LEN=16),    INTENT(OUT)  :: COP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K

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

  ! Initialize output variable
  COP = REPEAT(' ', 16)

  ! Apply operation
  SELECT CASE( IOP )

  CASE( FLT_COMPOSE_ALL_E )

    COP = 'all'

  CASE( FLT_COMPOSE_ANY_E )

    COP = 'any'

  CASE( FLT_COMPOSE_NONE_E )

    COP = 'none'

  CASE( FLT_COMPOSE_ONE_E )

    COP = 'one'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
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

END FUNCTION FLT_COMPOSE_IOP2COP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLT_COMPOSE_COP2IOP'
PP_THREAD_SAFE FUNCTION FLT_COMPOSE_COP2IOP( COP, IOP, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: COP
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K

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

  ! Initialize output variable
  IOP = UNDEF_PARAM_E

  ! Apply operation
  SELECT CASE( TRIM(ADJUSTL(COP)) )

  CASE( 'all' )

    IOP = FLT_COMPOSE_ALL_E

  CASE( 'any' )

    IOP = FLT_COMPOSE_ANY_E

  CASE( 'none' )

    IOP = FLT_COMPOSE_NONE_E

  CASE( 'one' )

    IOP = FLT_COMPOSE_ONE_E

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
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

END FUNCTION FLT_COMPOSE_COP2IOP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE ENUMERATORS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME