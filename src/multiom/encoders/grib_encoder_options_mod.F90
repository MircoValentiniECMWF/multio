! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'fortran_message_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_OPTIONS_MOD'
MODULE GRIB_ENCODER_OPTIONS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: OPT_CACHE_FULL_E

IMPLICIT NONE

PRIVATE

TYPE :: GRIB_ENCODER_OPTIONS_T

  ! Variables used to control the size of the cache
  LOGICAL :: ENABLE_CACHE              = .FALSE.
  LOGICAL :: CACHE_SATELLITES_INFO     = .FALSE.
  LOGICAL :: CACHE_TYPE_OF_LEVELS      = .FALSE.
  LOGICAL :: CACHE_DIRECTION_FREQUENCY = .FALSE.

  LOGICAL :: USE_TYPE_OF_LEVEL = .FALSE.
  INTEGER(KIND=JPIB_K) :: CACHE_STRATEGY = OPT_CACHE_FULL_E

END TYPE



PUBLIC :: GRIB_ENCODER_OPTIONS_T

END MODULE GRIB_ENCODER_OPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
