#pragma once

#include <chrono>
#include <array>
#include <cstdint>

#include "eckit/exception/Exceptions.h"


template<size_t N=1>
class Profiler {
private:

    std::array<int64_t,N> dt_;
    std::array<int64_t,N> cnt_;
    std::chrono::time_point<std::chrono::high_resolution_clock> t1_;
    bool state_;

public:


    Profiler() : dt_{0}, cnt_{0}, state_{false} {};


    Profiler(int64_t time, int64_t count) : dt_{time}, cnt_{count}, state_{false} {};


    void reset() {
        for ( int i=0; i<N; ++i){
            dt_[i] = 0;
            cnt_[i] = 0;
        }
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


    void toc( int i ) {
        if ( i<0 || i>= N ){
            throw eckit::SeriousBug("ERROR: Index out of range", Here());
        }
        if (state_) {
            std::chrono::time_point<std::chrono::high_resolution_clock> t2 = std::chrono::high_resolution_clock::now();
            auto int_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t2 - t1_);
            dt_[i] += int_ns.count();
            cnt_[i]++;
            state_ = false;
        }
        return;
    }



    void toc(  ) {
        if (state_) {
            std::chrono::time_point<std::chrono::high_resolution_clock> t2 = std::chrono::high_resolution_clock::now();
            auto int_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t2 - t1_);
            dt_[0] += int_ns.count();
            cnt_[0]++;
            state_ = false;
        }
        return;
    };


    int64_t getTotalTimeNsec() const { return dt_[0]; };


    int64_t getNumberOfCalls() const { return cnt_[0]; };


    int64_t getTotalTimeNsec( int64_t i ) const { 
        if ( i<0 || i>= N ){
            throw eckit::SeriousBug("ERROR: Index out of range", Here());
        }        
        return dt_[i];
    };


    int64_t getNumberOfCalls( int64_t i ) const {
        if ( i<0 || i>= N ){
            throw eckit::SeriousBug("ERROR: Index out of range", Here());
        }
        return cnt_[i];
    };

};


class StatisticsProfilingInfo{
    private:
        int64_t cnt_;

        int64_t TotInitTime_    = 0;
        int64_t TotWindowTime_  = 0;
        int64_t TotDataTime_    = 0;
        int64_t TotComputeTime_ = 0;
        int64_t TotDumpTime_    = 0;
        int64_t TotLoadTime_    = 0;
        int64_t TotalMemory_ = 0;

    public:

        StatisticsProfilingInfo():
        cnt_{0},
        TotInitTime_{0},
        TotWindowTime_{0},
        TotDataTime_{0},
        TotComputeTime_{0},
        TotDumpTime_{0},
        TotLoadTime_{0},
        TotalMemory_{0}        
        {};

    void updateCounter(){
        cnt_++;
        return;
    }

    void InitTime    ( int64_t val ){ 
        TotInitTime_   = val;
        return;
    };

    void WindowTime  ( int64_t val ){ 
        TotWindowTime_ = val;
        return;
    };

    void DataTime    ( int64_t val ){ 
        TotDataTime_ = val;
        return;
    };

    void ComputeTime ( int64_t val ){ 
        TotComputeTime_ = val;
        return;
    };

    void DumpTime    ( int64_t val ){ 
        TotDumpTime_ = val;
        return;
    };

    void LoadTime    ( int64_t val ){ 
        TotLoadTime_ = val;
        return;
    };
 
    void TotalMemory ( int64_t val ){ 
        TotalMemory_ = val;
        return;
    };


    int64_t cnt(){ return cnt_; };
    int64_t TotInitTime(){ return TotInitTime_; };
    int64_t TotWindowTime(){ return TotWindowTime_;};
    int64_t TotDataTime(){ return TotDataTime_;};
    int64_t TotComputeTime(){ return TotComputeTime_; };
    int64_t TotDumpTime(){ return TotDumpTime_; };
    int64_t TotLoadTime(){ return TotLoadTime_; };
    int64_t TotalMemory(){ return TotalMemory_; };

};