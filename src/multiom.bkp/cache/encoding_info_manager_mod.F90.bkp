!> @file encoding_info_manager.F90
!>
!> @brief High-level management and organization of encoding information.
!>
!> This module provides a suite of procedures for managing and organizing encoding information
!> within the output manager. It includes functions and subroutines for creating, extracting, and manipulating
!> bitmasks used in encoding schemes, as well as handling linked lists of encoding data.
!>
!> The procedures within this module handle tasks such as generating bitmasks, extracting specific
!> fields from bitmasks, and maintaining linked lists of encoding information. The module is designed
!> for thread-safe operations to support concurrent execution.
!>
!> @author Mirco Valentini
!> @date   January 31, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define __THREAD_SAFE__ RECURSIVE

#define PP_FILE_NAME 'encoding_info_manager_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_INFO_MANAGER_MOD'
MODULE ENCODING_INFO_MANAGER_MOD

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: MAP_MOD, ONLY: MAP_T

IMPLICIT NONE

PRIVATE

  !> @brief Local parameters
  INTEGER(KIND=INT64), PARAMETER :: MAX_PARAMID   = (2_INT64**27_INT64) - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_LEVEL     = (2_INT64**26_INT64) - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_DIRECTION = (2_INT64**13_INT64) - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_FREQUENCY = (2_INT64**13_INT64) - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_LEVTYPE   = (2_INT64**4_INT64)  - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_REPRES    = (2_INT64**4_INT64)  - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_MODEL     = (2_INT64**2_INT64)  - 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: MAX_PRECISION = (2_INT64**2_INT64)  - 1_INT64

  !> @brief Encoding info structure, used to fastly recover the encdoing configuration
  TYPE :: ENCODING_INFO_T

    !> @brief A tag used to differentiate between different encoding information
    CHARACTER(LEN=32) :: TAG

    !> @brief In some cases the original param ID can be mapped to a different one to allow for different encodings
    INTEGER(KIND=JPIB_K) :: MAPPED_PARAM_ID

    !> @brief In some cases the original level can be mapped to a different one to allow for different encodings
    INTEGER(KIND=JPIB_K), DIMENSION(2) :: MAPPED_LEVEL

    !> @brief In some cases the original level type can be mapped to a different one to allow for different encodings
    INTEGER(KIND=JPIB_K) :: MAPPED_LEVTYPE

    !> @brief Some definitions still don't have a proper definition in eccodes, `paramIDECMF` is an hack to allow them to be encoded anyway
    LOGICAL :: USE_PARAMID_ECMF

    !> @brief Relevant information used for selecting the kind of encoding
    TYPE(GRIB_STRUCTURE_T) :: GRIB_STRUCTURE

    !> @brief Relevant information used for selecting the kind of encoding
    TYPE(GRIB_INFO_T) :: GRIB_INFO

    !> @brief Relevant information used to encode time inforamtion
    TYPE(TIME_ASSUMPTIONS_T) :: TIME_ASSUMPTIONS

    !> @brief Relevant information used to encode level inforamtion
    TYPE(LEVEL_ASSUMPTIONS_T) :: LEVEL_ASSUMPTIONS

    !> @brief Relevant information used to encode packing inforamtion
    TYPE(LEVEL_ASSUMPTIONS_T) :: PACKING_ASSUMPTIONS

    !> @brief Circular buffer to store the time history of the field
    TYPE(CIRCULARBUFFER_T) :: TIME_HISTORY
  END TYPE

  !> @brief Datatype used to contain a collection of encoding information
  TYPE :: ENCODING_INFO_COLLECTION_T
    !> @brief  Array of encoding information
    TYPE(ENCODING_INFO_T), DIMENSION(:), POINTER :: EI_ => NULL()
  END TYPE

  !> @brief node in a list that contains pointer to circular buffers.
  TYPE :: EI_LIST_NODE_T

    !> @brief Hash used to as key in the map when the information will
    !> be pushed the encoding information
    INTEGER(KIND=INT64) :: HASH

    !> @brief Pointer to the encoding information collection
    TYPE(ENCODING_INFO_COLLECTION_T), POINTER :: EIC_ => NULL()

    !> @brief Pointer to the next node in the list
    TYPE(EI_LIST_NODE_T),   POINTER :: NEXT_ => NULL()

    !> @brief Pointer to the previous node in the list
    TYPE(EI_LIST_NODE_T),   POINTER :: PREV_ => NULL()
  END TYPE

  !> @brief List of circular buffers
  TYPE :: EI_LIST_T
    !> @brief Pointer to the head of the list
    TYPE(EI_LIST_NODE_T), POINTER :: HEAD_ => NULL()

    !> @brief Pointer to the tail of the list
    TYPE(EI_LIST_NODE_T), POINTER :: TAIL_ => NULL()

    !> @brief Number of nodes in the list
    INTEGER(KIND=JPIB_K) :: SIZE = 0_JPIB_K
  END TYPE

  !> @brief Shared map between OMP threads that contains all the atmosphere encoding information
  TYPE(MAP_T), SAVE :: OMP_SHARED_ENCODING_INFO_ATM

  !> @brief Shared map between OMP threads that contains all the wave encoding information
  TYPE(MAP_T), SAVE :: OMP_SHARED_ENCODING_INFO_WAM


  !> Whitelist of public symbols
  ! PUBLIC :: ENCODING_INFO_INIT
  PUBLIC :: ENCODING_INFO_ACCESS_OR_CREATE_ATM
  PUBLIC :: ENCODING_INFO_ACCESS_OR_CREATE_WAM
  PUBLIC :: ENCODING_INFO_COMMIT_QTM
  PUBLIC :: ENCODING_INFO_COMMIT_WAM
  ! PUBLIC :: ENCODING_INFO_FREE

CONTAINS

!>
!> @brief Initializes the data structures and configurations needed for managing encoding information.
!>
!> This subroutine sets up the initial environment required for managing encoding information
!> within the system. It configures various components, initializes maps and rules, and prepares
!> the necessary data structures based on the provided configuration and processor topology.
!>
!> @param [in] CFG The configuration object containing settings and parameters needed for initialization.
!>                 It provides a centralized configuration interface for setting up the encoding management system.
!>
!> @param [in] PROCESSOR_TOPO The processor topology structure that describes the mapping of computational tasks
!>                            to the processor grid. This information is crucial for optimizing the setup according
!>                            to the available hardware.
!>
!> @param [in] MODEL_PARAMS A structure containing model parameters that influence how encoding information is managed.
!>                          These parameters include model-specific settings that tailor the initialization process
!>                          to the particular needs of the model in use.
!>
!> @param [in] VERBOSE A logical flag indicating whether the subroutine should provide detailed output during execution.
!>                     If `TRUE`, the subroutine will print additional information useful for debugging and monitoring
!>                     the initialization process.
!>
!> @see ENCODING_INFO_FREE
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SUENCODING_INFO'
SUBROUTINE SUENCODING_INFO( CFG, PROCESSOR_TOPO, MODEL_PARAMS, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,          ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,          ONLY: N_LEVTYPE_E
  USE :: OM_CORE_MOD,          ONLY: N_REPRES_E
  USE :: MAP_MOD,              ONLY: MAP_INIT
  USE :: YAML_RULES_MOD,       ONLY: INIT_RULES
  USE :: YAML_RULES_MOD,       ONLY: RULES_DIMS
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: INIT_TIME_ASSUMPTION_RULES

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(PROC_TOPO_T),         INTENT(IN) :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T),         INTENT(IN) :: MODEL_PARAMS
  LOGICAL,                   INTENT(IN) :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NUM_PARAM_ID
  INTEGER(KIND=JPIB_K) :: MAX_PARAM_ID
  INTEGER(KIND=JPIB_K) :: MIN_PARAM_ID
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read and initialise rules from YAML
  CALL INIT_RULES( CFG, VERBOSE )

  ! Read and initialise time encoding rules from YAML
  CALL INIT_TIME_ASSUMPTIONS_RULES( CFG, VERBOSE )
  CALL INIT_LEVEL_ASSUMPTIONS_RULES( CFG, VERBOSE )
  CALL INIT_PACKING_ASSUMPTIONS_RULES( CFG, VERBOSE )


  ! Initialize all the maps
  CALL MAP_INIT( OMP_SHARED_ENCODING_INFO_ATM )
  CALL MAP_INIT( OMP_SHARED_ENCODING_INFO_WAM )

  ! Initialise encoding tables
  CALL TIME_ENCODING_TABLE_HEADER( )
  CALL PACKING_ENCODING_TABLE_HEADER()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate encoding_info: "'//TRIM(ERRMSG)//'"' )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate encoding_info' )
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

END SUBROUTINE SUENCODING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Access or create an encoding information node in a thread-local list for atmosphere.
!>
!> This function attempts to find an existing node in the sharem map `OMP_SHARED_ENCODING_INFO_ATM`
!> that matches an hash costructed from parameter ID (`PARAMID`),
!> level (`LEVEL`), level type (`LEVTYPE`), representation type (`REPRES`), model (`MODEL`),
!> and precision (`PRECISION`). If such a node is found, a pointer to its encoding information
!> array is returned via `EI`. If no matching node exists, a new node is created and added
!> to the list, and a pointer to the new node's encoding information array is returned.
!>
!> @param [inout] LOCAL_LIST The thread-local list (`EI_LIST_T` type) where the function
!>                           searches for or adds a node. This list is passed by reference
!>                           and may be modified if a new node is created.
!>
!> @param [in] MODEL_PARAMS The model parameters (`MODEL_PAR_T` type) used as part of the
!>                          search criteria to identify or create a node.
!>
!> @param [in] PARAMID The parameter ID (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [in] LEVEL The level (`JPIB_K` type) associated with the node, used as part of
!>                   the search criteria.
!>
!> @param [in] LEVTYPE The level type (`JPIB_K` type) associated with the node, used as
!>                     part of the search criteria.
!>
!> @param [in] REPRES The representation type (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [in] MODEL The model identifier (`JPIB_K` type) associated with the node,
!>                   used as part of the search criteria.
!>
!> @param [in] PRECISION The precision (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [out] EI A pointer to the encoding information array (`ENCODING_INFO_T` type,
!>                 dimensioned as `(:)`) of the found or newly created node. This allows
!>                 for direct access and manipulation of the encoding information.
!>
!> @return The function returns a flag indicating if the encoding info was found or created.
!>
!> The operation is thread-safe (only read operations happens on shared data structures),
!> allowing it to be used in parallel regions without data corruption.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_ACCESS_OR_CREATE_ATM'
__THREAD_SAFE__ FUNCTION ENCODING_INFO_ACCESS_OR_CREATE_ATM( LOCAL_LIST, MODEL_PARAMS, PARAMID, &
&       LEVEL, LEVTYPE, REPRES, MODEL, PRECISION, EI ) RESULT(FOUND)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),                              INTENT(INOUT) :: LOCAL_LIST
  TYPE(MODEL_PAR_T),                            INTENT(IN)    :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: LEVEL
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: LEVTYPE
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: MODEL
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: PRECISION
  TYPE(ENCODING_INFO_T), POINTER, DIMENSION(:), INTENT(OUT)   :: EI

  ! Function Result
  LOGICAL :: FOUND

  ! Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP_LEVEL
  INTEGER(KIND=INT64) :: HASH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of function result
  NULLIFY(EI)

  ! Construct hash
  HASH = CREATE_FIELD_HASH_ATM(PARAMID, LEVEL, LEVTYPE, REPRES, MODEL, PRECISION )

  ! Get encoding info
  FOUND = MAP_GET( OMP_SHARED_ENCODING_INFO_ATM, HASH, VALUE )

  ! Check if the encoding info is already available
  IF ( FOUND ) THEN

    SELECT TYPE ( A => VALUE )

    CLASS IS ( ENCODING_INFO_T )

      ! Set output variables
      EI => A

    CLASS DEFAULT

      ! Error handling
      PP_DEBUG_DEVELOP_THROW( 4 )

    END SELECT

  ELSE

    ! Extract definitions from encding rules
    CALL GET_RULES_SIZE( PARAM_ID, LEV_TYPE, REPRES, LEVEL, NRULES )

    ! Create new encoding info locally
    CALL ENCODING_INFO_NEW( LOCAL_LIST, HASH, NRULES, EI )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODING_INFO_ACCESS_OR_CREATE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Access or create an encoding information node in a thread-local list for wave model.
!>
!> This function attempts to find an existing node in the sharem map `OMP_SHARED_ENCODING_INFO_WAM`
!> that matches an hash costructed from parameter ID (`PARAMID`),
!> level (`LEVEL`), level type (`LEVTYPE`), representation type (`REPRES`), model (`MODEL`),
!> and precision (`PRECISION`). If such a node is found, a pointer to its encoding information
!> array is returned via `EI`. If no matching node exists, a new node is created and added
!> to the list, and a pointer to the new node's encoding information array is returned.
!>
!> @param [inout] LOCAL_LIST The thread-local list (`EI_LIST_T` type) where the function
!>                           searches for or adds a node. This list is passed by reference
!>                           and may be modified if a new node is created.
!>
!> @param [in] MODEL_PARAMS The model parameters (`MODEL_PAR_T` type) used as part of the
!>                          search criteria to identify or create a node.
!>
!> @param [in] PARAMID The parameter ID (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [in] LEVEL The level (`JPIB_K` type) associated with the node, used as part of
!>                   the search criteria.
!>
!> @param [in] LEVTYPE The level type (`JPIB_K` type) associated with the node, used as
!>                     part of the search criteria.
!>
!> @param [in] REPRES The representation type (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [in] MODEL The model identifier (`JPIB_K` type) associated with the node,
!>                   used as part of the search criteria.
!>
!> @param [in] PRECISION The precision (`JPIB_K` type) used as part of the search criteria.
!>
!> @param [out] EI A pointer to the encoding information array (`ENCODING_INFO_T` type,
!>                 dimensioned as `(:)`) of the found or newly created node. This allows
!>                 for direct access and manipulation of the encoding information.
!>
!> @return The function returns a flag indicating if the encoding info was found or created.
!>
!> The operation is thread-safe (only read operations happens on shared data structures),
!> allowing it to be used in parallel regions without data corruption.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_ACCESS_OR_CREATE_WAM'
__THREAD_SAFE__ FUNCTION ENCODING_INFO_ACCESS_OR_CREATE_WAM( LOCAL_LIST, MODEL_PARAMS, PARAMID, &
&       DIRECTION, FREQUENCY, LEVTYPE, REPRES, MODEL, PRECISION, EI ) RESULT(FOUND)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),                              INTENT(INOUT) :: LOCAL_LIST
  TYPE(MODEL_PAR_T),                            INTENT(IN)    :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: DIRECTION
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: FREQUENCY
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: LEVTYPE
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: MODEL
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: PRECISION
  TYPE(ENCODING_INFO_T), POINTER, DIMENSION(:), INTENT(OUT)   :: EI

  ! Function Result
  LOGICAL :: FOUND

  ! Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP_LEVEL
  INTEGER(KIND=INT64) :: HASH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of function result
  NULLIFY(EI)

  ! Construct hash
  HASH = CREATE_FIELD_HASH_ATM(PARAMID, DIRECTION, FREQUENCY, LEVTYPE, REPRES, MODEL, PRECISION )

  ! Get encoding info
  FOUND = MAP_GET( OMP_SHARED_ENCODING_INFO_WAM, HASH, VALUE )

  ! Check if the encoding info is already available
  IF ( FOUND ) THEN

    SELECT TYPE ( A => VALUE )

    CLASS IS ( ENCODING_INFO_T )

      ! Set output variables
      EI => A

    CLASS DEFAULT

      ! Error handling
      PP_DEBUG_DEVELOP_THROW( 4 )

    END SELECT

  ELSE

    ! Extract definitions from encding rules
    CALL GET_RULES_SIZE( PARAM_ID, LEV_TYPE, REPRES, LEVEL, NRULES )

    ! Create new encoding info locally
    CALL ENCODING_INFO_NEW( LOCAL_LIST, NRULES, EI )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODING_INFO_ACCESS_OR_CREATE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_POPULATE_ATM'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_POPULATE_ATM( LOCAL_LIST, MODEL_PARAMS, PARAMID, &
&       LEVEL, LEVTYPE, REPRES, MODEL, PRECISION, EI )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),                     INTENT(INOUT) :: LOCAL_LIST
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: LEVEL
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: LEVTYPE
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: MODEL
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: PRECISION
  TYPE(ENCODING_INFO_T), DIMENSION(:), INTENT(OUT)   :: EI

  ! Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP_LEVEL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of local variables
  TMP_LEVEL(1) = LEVEL
  TMP_LEVEL(2) = 0_JPIB_K

  ! Get all the definitions we need to encode
  SearchTheEncodingRules: DO RULE_ID = 1, SIZE(EI)

    ! Extract definitions from encding rules
    CALL MATCH_RULES( RULE_ID, PARAM_ID, LEV_TYPE, REPRES, TMP_LEVEL, EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEVEL, EI(RULE_ID)%MAPPED_LEV_TYPE, EI(RULE_ID)%GRIB_INFO )

    ! Time assumptions for the specified field
    CALL MATCH_TIME_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%TIME_ASSUMPTIONS )

    ! Time assumptions for the specified field
    CALL MATCH_LEVEL_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%LEVEL_ASSUMPTIONS )

    ! Time assumptions for the specified field
    CALL MATCH_PACKING_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%PACKING_ASSUMPTIONS )

    ! Initialize circular buffer
    EI(RULE_ID)%TIME_HISTORY%INIT( CAPACITY )

  ENDDO SearchTheEncodingRules

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ENCODING_INFO_POPULATE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_POPULATE_WAM'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_POPULATE_WAM( LOCAL_LIST, MODEL_PARAMS, PARAMID, &
&       DIRECTION, FREQUENCY, LEVTYPE, REPRES, MODEL, PRECISION, EI )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),                     INTENT(INOUT) :: LOCAL_LIST
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: DIRECTION
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: FREQUENCY
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: LEVTYPE
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: MODEL
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: PRECISION
  TYPE(ENCODING_INFO_T), DIMENSION(:), INTENT(OUT)   :: EI

  ! Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP_LEVEL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of local variables
  TMP_LEVEL(1) = DIRECTION
  TMP_LEVEL(2) = FREQUENCY

  ! Get all the definitions we need to encode
  SearchTheEncodingRules: DO RULE_ID = 1, SIZE(EI)

    ! Extract definitions from encding rules
    CALL MATCH_RULES( RULE_ID, PARAM_ID, LEV_TYPE, REPRES, TMP_LEVEL, EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEVEL, EI(RULE_ID)%MAPPED_LEV_TYPE, EI(RULE_ID)%GRIB_INFO )

    ! Time assumptions for the specified field
    CALL MATCH_TIME_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%TIME_ASSUMPTIONS )

    ! Time assumptions for the specified field
    CALL MATCH_LEVEL_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%LEVEL_ASSUMPTIONS )

    ! Time assumptions for the specified field
    CALL MATCH_PACKING_ASSUMPTIONS_RULES( EI(RULE_ID)%MAPPED_PARAM_ID, EI(RULE_ID)%MAPPED_LEV_TYPE, REPRES, EI(RULE_ID)%MAPPED_LEVEL, IS_ENSAMBLE_SIMULATION( MODEL_PARAMS ), EI(I)%PACKING_ASSUMPTIONS )

    ! Initialize circular buffer
    EI(RULE_ID)%TIME_HISTORY%INIT( CAPACITY )

  ENDDO SearchTheEncodingRules

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ENCODING_INFO_POPULATE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Generate a unique bitmask (hash) for atmospheric field parameters.
!>
!> This function generates a unique 64-bit bitmask (hash) based on the provided atmospheric
!> field parameters. The parameters include `PARAMID`, `LEVEL`, `LEVTYPE`, `REPRES`,
!> `MODEL`, and `PRECISION`. The bitmask is constructed using the following bit allocation:
!>
!> - **1  bit**  for the sign bit (`sid`), derived from the sign of the `LEVEL` parameter.
!> - **27 bits** for the parameter ID (`PARAMID`), which uniquely identifies the field.
!> - **26 bits** for the level (`LEVEL`), allowing for a wide range of level values.
!> - **4  bits** for the level type (`LEVTYPE`), specifying the type of level.
!> - **2  bits** for the representation type (`REPRES`), indicating how the data is represented.
!> - **2  bits** for the model identifier (`MODEL`), distinguishing between different models.
!> - **2  bits** for the precision (`PRECISION`), defining the precision of the data.
!>
!> The resulting 64-bit bitmask uniquely identifies a specific combination of these parameters,
!> allowing for efficient storage and retrieval of associated data within the system. The
!> function is thread-safe, ensuring consistent results in a concurrent environment.
!>
!> @param [in] PARAMID The parameter ID (`INT64` type) that forms part of the hash, using 27 bits.
!>
!> @param [in] LEVEL The level (`INT64` type) associated with the atmospheric field,
!>                   using 26 bits and 1 sign bit.
!>
!> @param [in] LEVTYPE The level type (`INT64` type) used as part of the hash generation, using 4 bits.
!>
!> @param [in] REPRES The representation type (`INT64` type) used as part of the hash generation, using 2 bits.
!>
!> @param [in] MODEL The model identifier (`INT64` type) associated with the atmospheric
!>                   field, used in the hash generation, using 2 bits.
!>
!> @param [in] PRECISION The precision (`INT64` type) used as part of the hash generation, using 2 bits.
!>
!> @return BITMASK The resulting 64-bit bitmask (`INT64` type) that uniquely identifies
!>                 the combination of input parameters, structured as described above.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CREATE_FIELD_HASH_ATM'
__THREAD_SAFE__ FUNCTION CREATE_FIELD_HASH_ATM(PARAMID, LEVEL, LEVTYPE, REPRES, MODEL, PRECISION ) RESULT(BITMASK)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=INT64), INTENT(IN) :: PARAMID
  INTEGER(KIND=INT64), INTENT(IN) :: LEVEL
  INTEGER(KIND=INT64), INTENT(IN) :: LEVTYPE
  INTEGER(KIND=INT64), INTENT(IN) :: REPRES
  INTEGER(KIND=INT64), INTENT(IN) :: MODEL
  INTEGER(KIND=INT64), INTENT(IN) :: PRECISION

  ! Function Result
  INTEGER(KIND=INT64) :: BITMASK

  ! Local variables
  INTEGER(KIND=INT64) :: SID
  INTEGER(KIND=INT64) :: LID
  INTEGER(KIND=INT64) :: TMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of the output variable
  BITMASK = 0_INT64

  ! Initialization of other local variables
  IF ( LEVEL .LT. 0 ) THEN
     SID = 0
     LID = -LEVEL
  ELSE
     SID = 1
     LID = LEVEL
  END IF

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( PARAMID.LT.0, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( PARAMID.GT.MAX_PARAMID, 2 )

  PP_DEBUG_CRITICAL_COND_THROW( LID.LT.0, 3 )
  PP_DEBUG_CRITICAL_COND_THROW( LID.GT.MAX_LEVEL, 4 )

  PP_DEBUG_CRITICAL_COND_THROW( LEVTYPE.LT.0, 5 )
  PP_DEBUG_CRITICAL_COND_THROW( LEVTYPE.GT.MAX_LEVTYPE, 6 )

  PP_DEBUG_CRITICAL_COND_THROW( REPRES.LT.0, 7 )
  PP_DEBUG_CRITICAL_COND_THROW( REPRES.GT.MAX_REPRES, 8 )

  PP_DEBUG_CRITICAL_COND_THROW( MODEL.LT.0, 9 )
  PP_DEBUG_CRITICAL_COND_THROW( MODEL.GT.MAX_MODEL, 10 )

  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.LT.0, 11 )
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.GT.MAX_PRECISION, 12 )

  ! Compose the bitmask
  BITMASK = IOR(BITMASK, ISHFT(SID,       INT(0 ,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(PARAMID,   INT(1 ,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(LID,       INT(28,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(LEVTYPE,   INT(54,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(REPRES,    INT(58,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(MODEL,     INT(60,KIND=INT64)))
  BITMASK = IOR(BITMASK, ISHFT(PRECISION, INT(62,KIND=INT64)))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: STR
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PARAMID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (paramId:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PARAMID.LT.0'//TRIM(ADJUSTL(STR)) )
    CASE (2)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PARAMID
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_PARAMID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (paramId:=', TRIM(ADJUSTL(TMP1)), ' - max_paramId:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PARAMID.GT.MAX_PARAMID'//TRIM(ADJUSTL(STR)) )

    CASE (3)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (level:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVEL.LT.0' )
    CASE (4)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_LEVEL
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (level:=', TRIM(ADJUSTL(TMP1)), ' - max_level:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVEL.GT.MAX_LID' )

    CASE (5)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LEVTYPE
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (levtype:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVTYPE.LT.0' )
    CASE (6)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LEVTYPE
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_LEVTYPE
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (levtype:=', TRIM(ADJUSTL(TMP1)), ' - max_levtype:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVTYPE.GT.MAX_LEVTYPE' )

    CASE (7)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') REPRES
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (repres:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: REPRES.LT.0' )
    CASE (8)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') REPRES
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_REPRES
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (repres:=', TRIM(ADJUSTL(TMP1)), ' - max_repres:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: REPRES.GT.MAX_REPRES' )

    CASE (9)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') MODEL
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (model:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: MODEL.LT.0' )
    CASE (10)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') MODEL
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_MODEL
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (model:=', TRIM(ADJUSTL(TMP1)), ' - max_model:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: MODEL.GT.MAX_MODEL' )

    CASE (11)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PRECISION
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (precision:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PRECISION.LT.0' )
    CASE (12)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PRECISION
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_PRECISION
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (precision:=', TRIM(ADJUSTL(TMP1)), ' - max_precision:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PRECISION.GT.MAX_PRECISION' )

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

END FUNCTION CREATE_FIELD_HASH_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Generate a unique bitmask (hash) for WAM field parameters.
!>
!> This function generates a unique 64-bit bitmask (hash) based on the provided WAM (Wave
!> and Atmosphere Model) field parameters. The parameters include `PARAMID`, `DIRECTION`,
!> `FREQUENCY`, `LEVTYPE`, `REPRES`, `MODEL`, and `PRECISION`. The bitmask is constructed
!> using the following bit allocation:
!>
!> - **1  bit** for the sign bit (`sid`), typically used to indicate the sign of the `DIRECTION`.
!> - **27 bits** for the parameter ID (`PARAMID`), which uniquely identifies the field.
!> - **13 bits** for the direction (`DIRECTION`), specifying the wave or wind direction.
!> - **13 bits** for the frequency (`FREQUENCY`), representing the wave or signal frequency.
!> - **4  bits** for the level type (`LEVTYPE`), specifying the type of level.
!> - **2  bits** for the representation type (`REPRES`), indicating how the data is represented.
!> - **2  bits** for the model identifier (`MODEL`), distinguishing between different models.
!> - **2  bits** for the precision (`PRECISION`), defining the precision of the data.
!>
!> The resulting 64-bit bitmask uniquely identifies a specific combination of these parameters,
!> allowing for efficient storage and retrieval of associated data within the system. The
!> function is thread-safe, ensuring consistent results in a concurrent environment.
!>
!> @param [in] PARAMID The parameter ID (`INT64` type) that forms part of the hash, using 27 bits.
!>
!> @param [in] DIRECTION The direction (`INT64` type) associated with the WAM field,
!>                       using 13 bits.
!>
!> @param [in] FREQUENCY The frequency (`INT64` type) associated with the WAM field,
!>                       using 13 bits.
!>
!> @param [in] LEVTYPE The level type (`INT64` type) used as part of the hash generation, using 4 bits.
!>
!> @param [in] REPRES The representation type (`INT64` type) used as part of the hash generation, using 2 bits.
!>
!> @param [in] MODEL The model identifier (`INT64` type) associated with the WAM field,
!>                   used in the hash generation, using 2 bits.
!>
!> @param [in] PRECISION The precision (`INT64` type) used as part of the hash generation, using 2 bits.
!>
!> @return BITMASK The resulting 64-bit bitmask (`INT64` type) that uniquely identifies
!>                 the combination of input parameters, structured as described above.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CREATE_FIELD_HASH_WAM'
__THREAD_SAFE__ FUNCTION CREATE_FIELD_HASH_WAM(PARAMID, DIRECTION, FREQUENCY, LEVTYPE, REPRES, MODEL, PRECISION ) RESULT(BITMASK)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=INT64), INTENT(IN) :: PARAMID
  INTEGER(KIND=INT64), INTENT(IN) :: DIRECTION
  INTEGER(KIND=INT64), INTENT(IN) :: FREQUENCY
  INTEGER(KIND=INT64), INTENT(IN) :: LEVTYPE
  INTEGER(KIND=INT64), INTENT(IN) :: REPRES
  INTEGER(KIND=INT64), INTENT(IN) :: MODEL
  INTEGER(KIND=INT64), INTENT(IN) :: PRECISION

  ! Function Result
  INTEGER(KIND=INT64) :: BITMASK

  ! Local variables
  INTEGER(KIND=INT64) :: SID
  INTEGER(KIND=INT64) :: LID
  INTEGER(KIND=INT64) :: TMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of the output variable
  BITMASK = 0_INT64

  ! Initialization of other local variables
  IF ( DIRECTION .LT. 0 ) THEN
     SID = 0
     LID = -DIRECTION
  ELSE
     SID = 1
     LID = DIRECTION
  END IF

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( PARAMID.LT.0, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( PARAMID.GT.MAX_PARAMID, 2 )

  PP_DEBUG_CRITICAL_COND_THROW( LID.LT.0, 3 )
  PP_DEBUG_CRITICAL_COND_THROW( LID.GT.MAX_DIRECTION, 4 )

  PP_DEBUG_CRITICAL_COND_THROW( FREQUENCY.LT.0, 5 )
  PP_DEBUG_CRITICAL_COND_THROW( FREQUENCY.GT.MAX_FREQUENCY, 6 )

  PP_DEBUG_CRITICAL_COND_THROW( LEVTYPE.LT.0, 7 )
  PP_DEBUG_CRITICAL_COND_THROW( LEVTYPE.GT.MAX_LEVTYPE, 8 )

  PP_DEBUG_CRITICAL_COND_THROW( REPRES.LT.0, 9 )
  PP_DEBUG_CRITICAL_COND_THROW( REPRES.GT.MAX_REPRES, 10 )

  PP_DEBUG_CRITICAL_COND_THROW( MODEL.LT.0, 11 )
  PP_DEBUG_CRITICAL_COND_THROW( MODEL.GT.MAX_MODEL, 12 )

  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.LT.0, 13 )
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.GT.MAX_PRECISION, 14 )

  ! Compose the bitmask
  BITMASK = IOR(BITMASK, ISHFT(SID,       INT(0, KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(PARAMID,   INT(1, KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(LID,       INT(28,KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(FREQUENCY, INT(41,KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(LEVTYPE,   INT(54,KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(REPRES,    INT(58,KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(MODEL,     INT(60,KIND=INT64) ) )
  BITMASK = IOR(BITMASK, ISHFT(PRECISION, INT(62,KIND=INT64) ) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: STR
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PARAMID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (paramId:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PARAMID.LT.0'//TRIM(ADJUSTL(STR)) )
    CASE (2)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PARAMID
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_PARAMID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (paramId:=', TRIM(ADJUSTL(TMP1)), ' - max_paramId:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PARAMID.GT.MAX_PARAMID'//TRIM(ADJUSTL(STR)) )

    CASE (3)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (direction:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: DIRECTION.LT.0' )
    CASE (4)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_LID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (direction:=', TRIM(ADJUSTL(TMP1)), ' - max_direction:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: DIRECTION.GT.MAX_LID' )

    CASE (5)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (frequency:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: FREQUENCY.LT.0' )
    CASE (6)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LID
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_LID
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (frequency:=', TRIM(ADJUSTL(TMP1)), ' - max_frequency:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: FREQUENCY.GT.MAX_LID' )







    CASE (7)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LEVTYPE
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (levtype:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVTYPE.LT.0' )
    CASE (8)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') LEVTYPE
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_LEVTYPE
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (levtype:=', TRIM(ADJUSTL(TMP1)), ' - max_levtype:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: LEVTYPE.GT.MAX_LEVTYPE' )

    CASE (9)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') REPRES
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (repres:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: REPRES.LT.0' )
    CASE (10)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') REPRES
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_REPRES
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (repres:=', TRIM(ADJUSTL(TMP1)), ' - max_repres:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: REPRES.GT.MAX_REPRES' )

    CASE (11)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') MODEL
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (model:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: MODEL.LT.0' )
    CASE (12)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') MODEL
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_MODEL
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (model:=', TRIM(ADJUSTL(TMP1)), ' - max_model:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: MODEL.GT.MAX_MODEL' )

    CASE (13)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PRECISION
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A)') ' -> (precision:=', TRIM(ADJUSTL(TMP1)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PRECISION.LT.0' )
    CASE (14)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I32)') PRECISION
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I32)') MAX_PRECISION
      STR = REPEAT(' ',4096)
      WRITE(STR, '(A,A,A,A,A)') ' -> (precision:=', TRIM(ADJUSTL(TMP1)), ' - max_precision:=', TRIM(ADJUSTL(TMP2)), ')'
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input out of bounds: PRECISION.GT.MAX_PRECISION' )

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

END FUNCTION CREATE_FIELD_HASH_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Extracts individual field parameters from a 64-bit bitmask.
!>
!> This subroutine extracts the individual field parameters from a given 64-bit bitmask
!> (`BITMASK`) generated by the `CREATE_FIELD_HASH_ATM` function. The bitmask contains encoded
!> values for the `PARAMID`, `ID`, `LEVTYPE`, `REPRES`, `MODEL`, and `PRECISION` parameters.
!> The extraction is based on the following bit allocation within the bitmask:
!>
!> - **1  bit**  for the sign bit (`sid`), extracted as part of `ID`.
!> - **27 bits** for the parameter ID (`PARAMID`), which uniquely identifies the field.
!> - **26 bits** for the ID (`ID`), representing a potentially signed identifier (including `sid`).
!> - **4  bits** for the level type (`LEVTYPE`), specifying the type of level.
!> - **2  bits** for the representation type (`REPRES`), indicating how the data is represented.
!> - **2  bits** for the model identifier (`MODEL`), distinguishing between different models.
!> - **2  bits** for the precision (`PRECISION`), defining the precision of the data.
!>
!> The subroutine is thread-safe, ensuring that multiple threads can extract parameters
!> from bitmasks concurrently without data races or inconsistencies.
!>
!> @param [in] BITMASK The 64-bit bitmask (`INT64` type) from which the parameters will be extracted.
!>
!> @param [out] PARAMID The extracted parameter ID (`INT64` type) using 27 bits.
!>
!> @param [out] ID The extracted identifier (`INT64` type) using 26 bits, including the sign bit.
!>
!> @param [out] LEVTYPE The extracted level type (`INT64` type) using 4 bits.
!>
!> @param [out] REPRES The extracted representation type (`INT64` type) using 2 bits.
!>
!> @param [out] MODEL The extracted model identifier (`INT64` type) using 2 bits.
!>
!> @param [out] PRECISION The extracted precision (`INT64` type) using 2 bits.
!>
!> The subroutine interprets the bitmask according to the predefined structure and extracts
!> each of these components, returning them via the corresponding output parameters.
!>
!> @see CREATE_FIELD_HASH_ATM
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'EXTRACT_BITMASK_ATM'
__THREAD_SAFE__ SUBROUTINE EXTRACT_BITMASK_ATM( BITMASK, PARAMID, ID, LEVTYPE, REPRES, MODEL, PRECISION )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(INT64), INTENT(IN)  :: BITMASK
  INTEGER(INT64), INTENT(OUT) :: PARAMID
  INTEGER(INT64), INTENT(OUT) :: ID
  INTEGER(INT64), INTENT(OUT) :: LEVTYPE
  INTEGER(INT64), INTENT(OUT) :: REPRES
  INTEGER(INT64), INTENT(OUT) :: MODEL
  INTEGER(INT64), INTENT(OUT) :: PRECISION

  ! Local variables
  INTEGER(INT64) :: SIGN

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Extract the values bit by bit
  SIGN      = (2*EXTRACT_FIELD(BITMASK, 0, 1)-1)
  PARAMID   = EXTRACT_FIELD(BITMASK, 1, 27)
  ID        = SIGN*EXTRACT_FIELD(BITMASK, 28, 26)
  LEVTYPE   = EXTRACT_FIELD(BITMASK, 54, 4)
  REPRES    = EXTRACT_FIELD(BITMASK, 58, 2)
  MODEL     = EXTRACT_FIELD(BITMASK, 60, 2)
  PRECISION = EXTRACT_FIELD(BITMASK, 62, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE EXTRACT_BITMASK_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Extracts individual field parameters from a 64-bit bitmask specific to WAM (Wave Model).
!>
!> This subroutine extracts the individual field parameters from a given 64-bit bitmask
!> (`BITMASK`) generated by the `CREATE_FIELD_HASH_WAM` function. The bitmask contains encoded
!> values for the `PARAMID`, `DIRECTION`, `FREQUENCY`, `LEVTYPE`, `REPRES`, `MODEL`, and
!> `PRECISION` parameters. The extraction is based on the following bit allocation within the bitmask:
!>
!> - **27 bits** for the parameter ID (`PARAMID`), which uniquely identifies the field.
!> - **13 bits** for the direction (`DIRECTION`), representing wave direction data.
!> - **13 bits** for the frequency (`FREQUENCY`), representing wave frequency data.
!> - **4 bits** for the level type (`LEVTYPE`), specifying the type of level.
!> - **2 bits** for the representation type (`REPRES`), indicating how the data is represented.
!> - **2 bits** for the model identifier (`MODEL`), distinguishing between different models.
!> - **2 bits** for the precision (`PRECISION`), defining the precision of the data.
!>
!> The subroutine is thread-safe, ensuring that multiple threads can extract parameters
!> from bitmasks concurrently without data races or inconsistencies.
!>
!> @param [in] BITMASK The 64-bit bitmask (`INT64` type) from which the parameters will be extracted.
!>
!> @param [out] PARAMID The extracted parameter ID (`INT64` type) using 27 bits.
!>
!> @param [out] DIRECTION The extracted direction (`INT64` type) using 13 bits.
!>
!> @param [out] FREQUENCY The extracted frequency (`INT64` type) using 13 bits.
!>
!> @param [out] LEVTYPE The extracted level type (`INT64` type) using 4 bits.
!>
!> @param [out] REPRES The extracted representation type (`INT64` type) using 2 bits.
!>
!> @param [out] MODEL The extracted model identifier (`INT64` type) using 2 bits.
!>
!> @param [out] PRECISION The extracted precision (`INT64` type) using 2 bits.
!>
!> The subroutine interprets the bitmask according to the predefined structure and extracts
!> each of these components, returning them via the corresponding output parameters.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'EXTRACT_BITMASK_ATM'
__THREAD_SAFE__ SUBROUTINE EXTRACT_BITMASK_WAM( BITMASK, PARAMID, DIRECTION, FREQUENCY, LEVTYPE, REPRES, MODEL, PRECISION )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(INT64), INTENT(IN)  :: BITMASK
  INTEGER(INT64), INTENT(OUT) :: PARAMID
  INTEGER(INT64), INTENT(OUT) :: DIRECTION
  INTEGER(INT64), INTENT(OUT) :: FREQUENCY
  INTEGER(INT64), INTENT(OUT) :: LEVTYPE
  INTEGER(INT64), INTENT(OUT) :: REPRES
  INTEGER(INT64), INTENT(OUT) :: MODEL
  INTEGER(INT64), INTENT(OUT) :: PRECISION

  ! Local variables
  INTEGER(INT64) :: SIGN

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Extract the values bit by bit
  SIGN      = (2*EXTRACT_FIELD(BITMASK, 0, 1)-1)
  PARAMID   = EXTRACT_FIELD(BITMASK, 1, 27)
  DIRECTION = SIGN*EXTRACT_FIELD(BITMASK, 28, 13)
  FREQUENCY = SIGN*EXTRACT_FIELD(BITMASK, 41, 13)
  LEVTYPE   = EXTRACT_FIELD(BITMASK, 54, 4)
  REPRES    = EXTRACT_FIELD(BITMASK, 58, 2)
  MODEL     = EXTRACT_FIELD(BITMASK, 60, 2)
  PRECISION = EXTRACT_FIELD(BITMASK, 62, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE EXTRACT_BITMASK_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Extracts a specific field from a 64-bit bitmask based on bit position and length.
!>
!> This function extracts a contiguous field of bits from a given 64-bit bitmask (`BITMASK`).
!> The extraction is performed according to the specified bit position (`POS_`) and length (`LENGTH_`).
!> The extracted field is returned as an integer value.
!>
!> @param [in] BITMASK The 64-bit bitmask (`INT64` type) from which the field will be extracted.
!>
!> @param [in] POS_ The starting bit position (0-based) in the bitmask where the extraction begins.
!>
!> @param [in] LENGTH_ The number of bits to be extracted starting from `POS_`.
!>
!> @return FIELD The extracted field (`INTEGER` type) from the bitmask. The field value is
!> represented as an integer and is extracted based on the specified `POS_` and `LENGTH_`.
!>
!> The function is thread-safe, ensuring that multiple threads can perform field extraction concurrently 
!> without data races or inconsistencies.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_FIELD'
__THREAD_SAFE__ FUNCTION EXTRACT_FIELD( BITMASK, POS_, LENGTH_ ) RESULT(FIELD)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(INT64), INTENT(IN) :: BITMASK
  INTEGER,        INTENT(IN) :: POS_
  INTEGER,        INTENT(IN) :: LENGTH_

  ! Function Result
  INTEGER(INT64) :: FIELD

  ! Local variables
  INTEGER(INT64) :: MASK
  INTEGER(INT64) :: POS
  INTEGER(INT64) :: LENGTH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( POS_.LT.0, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( LENGTH_.LE.0, 2 )
  PP_DEBUG_CRITICAL_COND_THROW( LENGTH_+LENGTH.GT.63, 3 )

  POS    = INT(POS_,KIND=INT64)
  LENGTH = INT(LENGTH_,KIND=INT64)
  MASK   = ISHFT(2_INT64**LENGTH - 1, POS)
  FIELD  = IAND(BITMASK, MASK)
  FIELD  = ISHFT(FIELD, -POS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Pos is supposed to be greater or equal to 0' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Length is supposed to be greater than 0' )

    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Pos+Length is supposed to be lower than 64 (legnth of the bitmask)' )

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

END FUNCTION EXTRACT_FIELD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocate and add a new node to a doubly linked list.
!>
!> This subroutine allocates a new node and adds it to the specified doubly linked list (`EI_LIST`).
!> The node's payload consists of a hash value (`HASH`) and an array of encoding information
!> with length `N`. After the node is successfully added to the list, a pointer to the payload
!> is returned via the `PEI` argument. This allows for further manipulation of the node's data
!> after it has been inserted into the list.
!>
!> @param [inout] EI_LIST The doubly linked list (`EI_LIST_T` type) to which the new node
!>                        will be added. This list is passed by reference and will be modified
!>                        to include the newly allocated node.
!>
!> @param [in] HASH The hash value (`INT64` type) that forms part of the payload for the
!>                  new node. This value is used to identify or categorize the encoding
!>                  information within the node.
!>
!> @param [in] N The length (`JPIB_K` type) of the encoding information array that will
!>               be stored in the node's payload. This array is allocated as part of the
!>               node creation process.
!>
!> @param [out] PEI(N) A pointer (`ENCODING_INFO_T` type) to the newly created payload
!>                 within the node. This pointer allows the caller to directly access
!>                 and manipulate the encoding information after the node has been added
!>                 to the list.
!>
!> @note This operation is not thread-safe since the list is supposed to be thread private
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_NEW'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_NEW( EI_LIST, HASH, N, PEI )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),                              INTENT(INOUT) :: EI_LIST
  INTEGER(KIND=INT64),                          INTENT(IN)    :: HASH
  INTEGER(KIND=JPIB_K),                         INTENT(IN)    :: N
  TYPE(ENCODING_INFO_T), POINTER, DIMENSION(:), INTENT(OUT)   :: PEI

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  NULLIFY( PEI )

  ! Allocation
  IF ( .NOT. ASSOCIATED(EI_LIST%HEAD_) ) THEN
    ALLOCATE( EI_LIST%HEAD_ )
    EI_LIST%TAIL_ => EI_LIST%HEAD_
    ALLOCATE( EI_LIST%HEAD_%EIC_ )
    ALLOCATE( EI_LIST%HEAD_%EIC_%EI_(N) )
    PEI => EI_LIST%TAIL_%EIC_%EI_
    EI_LIST%TAIL_%HASH = HASH
    EI_LIST%SIZE = 1
    NULLIFY(EI_LIST%HEAD_%PREV_)
    NULLIFY(EI_LIST%HEAD_%NEXT_)
  ELSE
    ALLOCATE( EI_LIST%TAIL_%NEXT_ )
    EI_LIST%TAIL_%NEXT_%PREV_ => EI_LIST%TAIL_
    EI_LIST%TAIL_ => EI_LIST%TAIL_%NEXT_
    NULLIFY(EI_LIST%TAIL_%NEXT_)
    ALLOCATE( EI_LIST%TAIL_%EIC_ )
    ALLOCATE( EI_LIST%TAIL_%EIC_%EI_(N) )
    EI_LIST%TAIL_%HASH = HASH
    PEI => EI_LIST%TAIL_%EIC_%EI_
    EI_LIST%SIZE = EI_LIST%SIZE + 1
  ENDIF

  ! Allocate the payload
  ALLOCATE( PEI%GRIB_INFO(N) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_NEW
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Remove a node from a doubly linked list.
!>
!> This subroutine is responsible for safely removing a node, referenced by `CURR`, from a
!> doubly linked list (`EI_LIST`). The list is updated to maintain its integrity after the
!> node's removal, ensuring that both the previous and next nodes in the list are correctly
!> linked.
!>
!> @param [inout] EI_LIST The doubly linked list (`EI_LIST_T` type) from which the node
!>                        will be removed. The list is passed by reference and will be
!>                        modified to reflect the removal of the node.
!>
!> @param [inout] CURR The node (`EI_LIST_NODE_T` type) to be removed from the list.
!>                    This pointer is passed by reference and will be invalidated upon
!>                    successful removal. The memory associated with `CURR` may need to
!>                    be deallocated separately, depending on the implementation.
!>
!>
!> @note This operation is not thread-safe since the list is supposed to be thread private
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_DELETE'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_DELETE( EI_LIST, CURR )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T),               INTENT(INOUT) :: EI_LIST
  TYPE(EI_LIST_NODE_T), POINTER, INTENT(INOUT) :: CURR

  ! Local variables
  TYPE(EI_LIST_NODE_T), POINTER :: TMP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! ASSERT( EI_LIST%SIZE .GT. 0 )

  IF ( ASSOCIATED(CURR%PREV_) ) THEN
    CURR%PREV_%NEXT_ => CURR%NEXT_
  ENDIF

  IF ( ASSOCIATED(CURR%NEXT_) ) THEN
    CURR%NEXT_%PREV_ => CURR%PREV_
  ENDIF

  IF ( ASSOCIATED(CURR,EI_LIST%HEAD_) ) THEN
    EI_LIST%HEAD_ => CURR%NEXT_
  ENDIF
  IF ( ASSOCIATED(CURR,EI_LIST%TAIL_) ) THEN
    EI_LIST%TAIL_ => CURR%PREV_
  ENDIF

  EI_LIST%SIZE = EI_LIST%SIZE - 1

  IF ( EI_LIST%SIZE .EQ. 0 ) THEN
    NULLIFY( EI_LIST%HEAD_ )
    NULLIFY( EI_LIST%TAIL_ )
  ENDIF

  DEALLOCATE(CURR)
  NULLIFY(CURR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_DELETE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Commit local thread-specific nodes to a shared map.
!>
!> This subroutine is used to synchronize and commit nodes stored in a thread-local list (`EI_LIST`)
!> to a shared map. The thread-local lists are used to accumulate nodes when the code is busy,
!> minimizing the need for critical sections and reducing contention among threads.
!> When the code is less busy, this subroutine is called to push all nodes from the local lists
!> into the shared map in a thread-safe manner.
!>
!> This routine is used in case of the ATM encoding info.
!>
!> @param [inout] EI_LIST The thread-local list (`EI_LIST_T` type) containing the nodes
!>                        to be committed to the shared map. This list is passed by reference
!>                        and will be emptied upon successful completion of the commit operation.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_COMMIT_ATM'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_COMMIT_ATM( EI_LIST )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: MAP_MOD,     ONLY: MAP_INSERT
  USE :: MAP_MOD,     ONLY: VALUE_DESTRUCTOR_IF

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T), INTENT(INOUT) :: EI_LIST

  ! Local variables
  INTEGER(KIND=INT64) :: KEY
  CLASS(*), POINTER :: VALUE
  PROCEDURE(VALUE_DESTRUCTOR_IF), POINTER :: DESTRUCTOR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Push local encoding info to the shared map
  DESTRUCTOR => DEALLOCATE_ENCODING_INFO
  CURR => EI_LIST%HEAD_
  DO WHILE( ASSOCIATED(CURR) )
    KEY   = CURR%EI_%HASH
    VALUE => CURR%EI_
!$OMP CRITICAL (ENCODING_INFO_COMMIT_ATM)
    CALL MAP_INSERT( OMP_SHARED_ENCODING_INFO_ATM, KEY, VALUE, VALUE_DESTRUCTOR=DESTRUCTOR )
!$OMP END CRITICAL (ENCODING_INFO_COMMIT_ATM)
    KEY=0_INT64
    NULLIFY(VALUE)
    NULLIFY(CURR%EI_)
    CALL ENCODING_INFO_DELETE( EI_LIST, CURR )
    CURR => CURR%NEXT_
  ENDDO

  ! Reset the entry list
  NULLIFY( EI_LIST%HEAD_ )
  NULLIFY( EI_LIST%TAIL_ )
  EI_LIST%SIZE = 0

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_COMMIT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Commit local thread-specific nodes to a shared map.
!>
!> This subroutine is used to synchronize and commit nodes stored in a thread-local list (`EI_LIST`)
!> to a shared map. The thread-local lists are used to accumulate nodes when the code is busy,
!> minimizing the need for critical sections and reducing contention among threads.
!> When the code is less busy, this subroutine is called to push all nodes from the local lists
!> into the shared map in a thread-safe manner.
!>
!> This routine is used in case of the WAM encoding info.
!>
!> @param [inout] EI_LIST The thread-local list (`EI_LIST_T` type) containing the nodes
!>                        to be committed to the shared map. This list is passed by reference
!>                        and will be emptied upon successful completion of the commit operation.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_COMMIT_WAM'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_COMMIT_WAM( EI_LIST )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: MAP_MOD,     ONLY: MAP_INSERT
  USE :: MAP_MOD,     ONLY: VALUE_DESTRUCTOR_IF

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T), INTENT(INOUT) :: EI_LIST

  ! Local variables
  INTEGER(KIND=INT64) :: KEY
  CLASS(*), POINTER :: VALUE
  PROCEDURE(VALUE_DESTRUCTOR_IF), POINTER :: DESTRUCTOR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Push local encoding info to the shared map
  DESTRUCTOR => DEALLOCATE_ENCODING_INFO
  CURR => EI_LIST%HEAD_
  DO WHILE( ASSOCIATED(CURR) )
    KEY   = CURR%EI_%HASH
    VALUE => CURR%EI_
!$OMP CRITICAL (ENCODING_INFO_COMMIT_WAM)
    CALL MAP_INSERT( OMP_SHARED_ENCODING_INFO_WAM, KEY, VALUE, VALUE_DESTRUCTOR=DESTRUCTOR )
!$OMP END CRITICAL (ENCODING_INFO_COMMIT_WAM)
    KEY=0_INT64
    NULLIFY(VALUE)
    NULLIFY(CURR%EI_)
    CALL ENCODING_INFO_DELETE( EI_LIST, CURR )
    CURR => CURR%NEXT_
  ENDDO

  ! Reset the entry list
  NULLIFY( EI_LIST%HEAD_ )
  NULLIFY( EI_LIST%TAIL_ )
  EI_LIST%SIZE = 0

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_COMMIT_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Deallocate an `encoding_info_collection_t` object.
!>
!> This subroutine is intended to be called as a destructor for `encoding_info_collection_t`
!> objects when they are removed from a map or when the map itself is deallocated.
!> It ensures that the resources associated with the object are properly freed, preventing memory leaks.
!>
!> @param [inout] VALUE The opaque value representing the `encoding_info_collection_t` object
!>                      that needs to be deallocated. The value is passed by reference and will
!>                      be invalidated upon successful deallocation.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_COMMIT_WAM'
SUBROUTINE DEALLOCATE_ENCODING_INFO( VALUE )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ASSOCIATED(VALUE) ) THEN
    SELECT TYPE( A => VALUE)
    TYPE IS (ENCODING_INFO_COLLECTION_T)
      IF ( ASSOCIATED(A%EI_) ) THEN
        DO I = 1, SIZE(A%EI_)
          CALL A%EI_(I)%TIME_HISTORY%FREE()
        ENDDO
        DEALLOCATE(A%EI_)
      ENDIF
      DEALLOCATE(A)
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 1 )
    END SELECT
  ENDIF
  NULLIFY(VALUE)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong type of the value. Unable to deallocate')
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

END SUBROUTINE DEALLOCATE_ENCODING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Deallocates all nodes in a linked list of encoding information.
!>
!> This subroutine is responsible for freeing all memory associated with a linked list
!> of encoding information nodes. It traverses the list, deallocates each node, and
!> ensures that the list is properly cleared.
!>
!> @param [inout] EI_LIST The linked list of encoding information nodes to be deallocated.
!>              Upon completion, this list will be empty and all associated memory
!>              will have been freed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_FREE_LIST'
__THREAD_SAFE__ SUBROUTINE ENCODING_INFO_FREE_LIST( EI_LIST )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(EI_LIST_T), INTENT(INOUT) :: EI_LIST

  ! Local variables
  TYPE(EI_LIST_NODE_T), POINTER :: CURR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ASSOCIATED(EI_LIST%HEAD_) ) THEN

    DO

      IF ( .NOT.ASSOCIATED(EI_LIST%TAIL_) ) THEN
        EXIT
      ENDIF

      CURR => EI_LIST%TAIL_
      EI_LIST%TAIL_ => EI_LIST%TAIL_%PREV_
      IF ( ASSOCIATED(EI_LIST%TAIL_) ) THEN
        NULLIFY(EI_LIST%TAIL_%NEXT_)
      ENDIF

      ! Deallocate the payload
      CALL ENCODING_INFO_DELETE( EI_LIST, CURR )

      ! Deallocate the node itself
      DEALLOCATE( CURR )
      NULLIFY(CURR)

      EI_LIST%SIZE = EI_LIST%SIZE - 1

    ENDDO

    NULLIFY(EI_LIST%TAIL_)
    NULLIFY(EI_LIST%HEAD_)
    EI_LIST%SIZE = 0

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ENCODING_INFO_FREE_ALL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees all allocated memory associated with encoding information at the end of the simulation.
!>
!> This subroutine is responsible for releasing all resources and deallocating memory used for managing
!> encoding information. It should be called at the end of the simulation or data processing workflow to
!> ensure that no memory leaks occur and all dynamically allocated structures are properly freed.
!>
!> @see SUENCODING_INFO
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_FREE'
SUBROUTINE ENCODING_INFO_FREE(  )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,               ONLY: JPIB_K
  USE :: MAP_MOD,                   ONLY: MAP_FREE
  USE :: YAML_RULES_MOD,            ONLY: FREE_RULES
  USE :: YAML_TIME_ASSUMPTIONS_MOD, ONLY: FREE_TIME_ASSUMPTION_RULES
  ! USE :: TIME_ASSUMPTIONS_MOD,      ONLY: TIME_ASSUMPTIONS_FREE
  USE :: PACKAGING_ASSUMPTIONS_MOD, ONLY: PACKAGING_ASSUMPTIONS_FREE
  USE :: LEVEL_ASSUMPTIONS_MOD,     ONLY: LEVEL_ASSUMPTIONS_FREE
  USE :: GENERAL_ASSUMPTIONS_MOD,   ONLY: GENERAL_ASSUMPTIONS_FREE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Closing encoding tables
  CALL TIME_ENCODING_TABLE_FOOTER()
  CALL PACKING_ENCODING_TABLE_FOOTER()

  ! Free the maps that contain the encoding info
  CALL MAP_FREE( OMP_SHARED_ENCODING_INFO_ATM )
  CALL MAP_FREE( OMP_SHARED_ENCODING_INFO_WAM )

  ! Free assumptions configuration
  CALL FREE_RULES()
  CALL TIME_ASSUMPTIONS_FREE()
  CALL LEVEL_ASSUMPTIONS_FREE()
  CALL PACKAGING_ASSUMPTIONS_FREE()
  CALL GENERAL_ASSUMPTIONS_FREE()

  ! Exit point on error
  RETURN

END SUBROUTINE ENCODING_INFO_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE ENCODING_INFO_MANAGER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
