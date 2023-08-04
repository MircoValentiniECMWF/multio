#pragma once

#include <chrono>
#include <cstdint>


class Profiler {
private:
    int64_t dt_;
    int64_t cnt_;
    std::chrono::time_point<std::chrono::high_resolution_clock> t1_;
    bool state_;

public:
    Profiler() : dt_{0}, cnt_{0}, state_{false} {};
    Profiler(int64_t time, int64_t count) : dt_{time}, cnt_{count}, state_{false} {};

    void reset() {
        dt_ = 0;
        cnt_ = 0;
        state_ = false;
        return;
    };

    void tic() {
        if (!state_) {
            t1_ = std::chrono::high_resolution_clock::now();
            state_ = true;
        }
        return;
    };

    void toc() {
        if (state_) {
            std::chrono::time_point<std::chrono::high_resolution_clock> t2 = std::chrono::high_resolution_clock::now();
            auto int_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t2 - t1_);
            dt_ += int_ns.count();
            cnt_++;
            state_ = false;
        }
        return;
    }

    int64_t getTotalTimeNsec() const { return dt_; };
    int64_t getNumberOfCalls() const { return cnt_; };
};