! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'time_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_UTILS_MOD'
MODULE TIME_UTILS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

PRIVATE

TYPE :: CURR_TIME_T

  CHARACTER(LEN=256) :: PAR_
  INTEGER(KIND=JPIB_K) :: ISEC_

END TYPE

TYPE :: TIME_HISTORY_T

  CHARACTER(LEN=256) :: PAR_
  INTEGER(KIND=JPIB_K) :: LAST_

END TYPE



PUBLIC :: CURR_TIME_T
PUBLIC :: TIME_HISTORY_T

END MODULE TIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME