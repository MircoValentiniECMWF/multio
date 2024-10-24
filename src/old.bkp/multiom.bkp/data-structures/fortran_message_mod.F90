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
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K

IMPLICIT NONE

PRIVATE

TYPE :: FORTRAN_MESSAGE_T

  INTEGER(KIND=JPIB_K) :: PARAM_ID
  INTEGER(KIND=JPIB_K) :: LEVTYPE
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: LEVEL
  INTEGER(KIND=JPIB_K) :: MODEL
  INTEGER(KIND=JPIB_K) :: REPRES

END TYPE



PUBLIC :: FORTRAN_MESSAGE_T

END MODULE FORTRAN_MESSAGE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
