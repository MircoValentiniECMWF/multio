! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'metadata_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_BASE_MOD'
MODULE METADATA_BASE_MOD

IMPLICIT NONE

PRIVATE

TYPE, ABSTRACT :: METADATA_BASE_A

  CHARACTER(LEN=256) :: MSG_

END TYPE



PUBLIC :: METADATA_BASE_A

END MODULE METADATA_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME