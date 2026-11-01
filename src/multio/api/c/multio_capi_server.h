/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/** @file multio_capi_server.h
 *
 * Server API: the server is made of runnables that do not create threads.
 *
 * The caller creates the shared state and the runnables, then runs each runnable on a thread of its own
 * (e.g. one OpenMP thread per runnable): init, run and free are called, in this order, from that thread.
 * After all the runnables have been freed, the shared state is freed.
 *
 * Threads: multio does not create threads on this path. The tracing hook is only called from inside
 * multio_server_runnable_run, on the thread of the caller (e.g. an OpenMP thread for Fortran callers).
 */

#pragma once

#include <stdint.h>

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

struct multio_server_shared_t;
typedef struct multio_server_shared_t multio_server_shared_t;

struct multio_server_runnable_t;
typedef struct multio_server_runnable_t multio_server_runnable_t;

/** Tracing hook of the caller (DR_HOOK). multio stores it, it is not called yet.
 * \param context Context passed when the hook was set
 * \param name Routine name (not NUL terminated)
 * \param name_length Length of the name
 * \param kswitch 0 on entry, 1 on exit
 * \param key Handle kept between entry and exit
 */
typedef void (*multio_dr_hook_function_t)(void* context, const char* name, int name_length, int kswitch,
                                          double* key);

/** Number of values returned by multio_server_shared_usage */
#define MULTIO_SERVER_USAGE_SIZE 10

/** Set the tracing hook of a (client) handle
 * \param mio Handle
 * \param hook Hook function (NULL: no hook)
 * \param context Context passed to the hook
 * \returns Return code (#MultioErrorValues)
 */
int multio_handle_set_dr_hook(multio_handle_t* mio, multio_dr_hook_function_t hook, void* context);

/** Create the server shared state and its runnables
 * \note The configuration is moved into the server: delete the configuration object afterwards
 * \param cc Configuration (multio server configuration file)
 * \param nthreads Number of threads the caller can give to the server
 * \param server_comm Communicator of the server ranks (Fortran handle)
 * \param client_comms Extended communicators, one per client group: server ranks and client ranks (Fortran handles)
 * \param n_client_comms Number of client groups
 * \param shared Created shared state
 * \param n_runnables Number of runnables (threads) needed, at most nthreads
 * \returns Return code (#MultioErrorValues)
 */
int multio_server_create_runnables(multio_configuration_t* cc, int nthreads, int server_comm, const int* client_comms,
                                   int n_client_comms, multio_server_shared_t** shared, int* n_runnables);

/** Get a runnable (index from 0 to n_runnables-1) */
int multio_server_get_runnable(multio_server_shared_t* shared, int index, multio_server_runnable_t** runnable);

/** Kind of a runnable (listener, receiver, dispatcher, coordinator, ...): not NUL terminated */
int multio_server_runnable_kind(multio_server_runnable_t* runnable, const char** kind, int* length);

/** Initialise a runnable, from the thread that will run it */
int multio_server_runnable_init(multio_server_runnable_t* runnable, multio_server_shared_t* shared);

/** Run a runnable until it has finished its work
 * \param hook Tracing hook of the caller (NULL: no hook)
 * \param context Context passed to the hook
 */
int multio_server_runnable_run(multio_server_runnable_t* runnable, multio_dr_hook_function_t hook, void* context);

/** Free a runnable, from the thread that ran it */
int multio_server_runnable_free(multio_server_runnable_t* runnable);

/** Free the shared state and the runnables (after all the runnables have been freed) */
int multio_server_shared_free(multio_server_shared_t* shared);

/** Number of queues and pools reported by multio_server_shared_usage */
int multio_server_shared_usage_count(multio_server_shared_t* shared, int* count);

/** Memory usage of a queue or pool (index from 0 to count-1)
 * \param name Name of the queue or pool (not NUL terminated)
 * \param name_length Length of the name
 * \param values MULTIO_SERVER_USAGE_SIZE values: count, bytes, max count, max bytes, capacity count,
 *               capacity bytes, total, producer wait (microseconds), consumer wait (microseconds), reserved
 */
int multio_server_shared_usage(multio_server_shared_t* shared, int index, const char** name, int* name_length,
                               int64_t* values);

#ifdef __cplusplus
} /* extern "C" */
#endif
