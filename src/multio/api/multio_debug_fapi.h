#ifndef multio_debug_fapi_h
#define multio_debug_fapi_h


#define __xstr__(x) #x
#define __str__(x) __xstr__(x)

#define __MULTIO_DEBUG_API__

#if defined(__MULTIO_DEBUG_API__)
#define __use_debug__ use ::multio_debug_
#else
#endif


#if defined(__MULTIO_DEBUG_API__)
#define __debug_unit__ integer ::debug_unit = -1
#else
#endif


#if defined(__MULTIO_DEBUG_API__)
#define __multio_fapi_enter__()                                                                                      \
    write(*, '(A,A,A,A,A,A,A,A,A,I5)') 'multio_api enter :: file=', __file__, ', module=', __str__(__module_name__), \
        ', procType=', __str__(__proc_type__), ', procName=', __str__(__proc_name__), ', line=', __LINE__
#else
#define __multio_fapi_enter__()
#endif


#if defined(__MULTIO_DEBUG_API__)
#define __multio_fapi_exit__()                                                                                      \
    write(*, '(A,A,A,A,A,A,A,A,A,I5)') 'multio_api exit :: file=', __file__, ', module=', __str__(__module_name__), \
        ', procType=', __str__(__proc_type__), ', procName=', __str__(__proc_name__), ', line=', __LINE__
#else
#define __multio_fapi_exit__(fcn_name)
#endif


#if defined(__MULTIO_DEBUG_API__)
#define __multio_fapi_log_kv__(k, t, v) write(*, *) 'key=', k, ', type=', t, ', value=', v
#else
#define __multio_fapi_log_kv__(k, t, v)
#endif

#endif