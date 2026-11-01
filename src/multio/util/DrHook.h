/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file DrHook.h
/// Tracing hook (DR_HOOK) passed by the calling model.
///
/// The model passes its DR_HOOK routine through the API (client handle creation, server runnables).
/// multio stores it; it is not called yet. When it is, it is only called on the thread of the caller
/// (inside the runnable run, or the client API call): never from a thread created by multio, since
/// Fortran callers require OpenMP threads.

#pragma once

namespace multio::util {

struct DrHook {
    /// name: routine name (not NUL terminated), nameLength: its length,
    /// kswitch: 0 on entry, 1 on exit, key: handle kept between entry and exit
    using Function = void (*)(void* context, const char* name, int nameLength, int kswitch, double* key);

    Function function = nullptr;
    void* context = nullptr;

    explicit operator bool() const { return function != nullptr; }
};

}  // namespace multio::util
