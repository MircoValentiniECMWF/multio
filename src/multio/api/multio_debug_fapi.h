#if defined(MULTIO_DEBUG_API)
#define __use_debug__ use ::multio_debug_
#else
#endif


#if defined(MULTIO_DEBUG_API)
#define __debug_unit__ integer ::debug_unit = -1
#else
#endif


#if defined(MULTIO_DEBUG_API)
#define __multio_fapi_enter__(fcn_name)
#else
#define __multio_fapi_enter__(fcn_name)
#endif


#if defined(MULTIO_DEBUG_API)
#define __multio_fapi_exit__(fcn_name)
#else
#define __multio_fapi_exit__(fcn_name)
#endif


#if defined(MULTIO_DEBUG_API)
#define __multio_log__(msg)
write(*,*) msg
#else
#define __multio_log__(msg)
#endif


#if defined(MULTIO_DEBUG_API)
#define __multio_cond_log__(fcn_name)
#else
#define __multio_cond_log__(fcn_name)
#endif