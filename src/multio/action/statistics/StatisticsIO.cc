#include "StatisticsIO.h"

#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <array>

namespace multio::action {

// Informations for the current 
StatisticsIO::StatisticsIO():
    step_{0},
    seed_{123456789L},
    fileLengthInBytes_{0},
    wf_{nullptr},
    stepStr_{""},
    stepStrOld_{""},
    fname_{""},
    oldFile_{""},
    tmpFile_{""},
    defFile_{""}{
    setStep( step_ );
    return;
};

void StatisticsIO::setStep( long step )
{
    step_ = step;
    std::stringstream tmp;
    tmp << std::setfill ('0') << std::setw (6) << step;
    stepStr_ = tmp.str();
    tmp.str("");
    tmp.clear();
    tmp << std::setfill ('0') << std::setw (6) << step-2;
    stepStrOld_ = tmp.str();
    return;
};

void StatisticsIO::fileLength(){
    wf_->seekg (0, wf_->end);
    fileLengthInBytes_ = wf_->tellg();
    wf_->seekg (0, wf_->beg);
    return;
};

bool StatisticsIO::check( ) const{
    if (!wf_) {
      throw eckit::SeriousBug("Cannot open file!" + defFile_, Here());
    }
    if (!wf_->good()) {
        throw eckit::SeriousBug("Error opening file!" + defFile_, Here());
    }
    if (!wf_->is_open()) {
        throw eckit::SeriousBug("Error file not opened!" + defFile_, Here());
    }
    return wf_ && wf_->is_open() ? true : false;
};

void StatisticsIO::startPeriod(const std::string& partialPath, const std::string periodKind, const std::string& mode ){
    // -------------------------------------------------
    std::ostringstream tmpOs;
    std::ostringstream defOs;
    std::ostringstream oldOs;
    tmpOs << partialPath << "-" << periodKind << "-" << stepStr_ << "-period.tmp.bin";
    defOs << partialPath << "-" << periodKind << "-" << stepStr_ << "-period.bin";
    oldOs << partialPath << "-" << periodKind << "-" << stepStrOld_ << "-period.bin";
    oldFile_ = oldOs.str();
    tmpFile_ = tmpOs.str();
    defFile_ = defOs.str();
    fname_ = tmpOs.str();
    // create the fstream object
    if ( mode == "r" && !defFile_.exists() ){
        throw eckit::SeriousBug("Restart file " + defFile_ + "not exist!", Here());
    }
    wf_ = (mode=="r") ? new std::fstream(defOs.str(), std::fstream::in | std::fstream::binary) : 
          (mode=="w") ? new std::fstream(fname_, std::fstream::out | std::fstream::trunc | std::fstream::binary) : 
          nullptr;
    check();
    // Exit point
    return;
}

void StatisticsIO::startOperation(const std::string& partialPath, const std::string& operationKind, const std::string& mode ){
    // -------------------------------------------------
    std::ostringstream tmpOs;
    std::ostringstream defOs;
    std::ostringstream oldOs;
    tmpOs << partialPath << "-" << operationKind << "-" << stepStr_ << "-operation.tmp.bin";
    defOs << partialPath << "-" << operationKind << "-" << stepStr_ << "-operation.bin";
    oldOs << partialPath << "-" << operationKind << "-" << stepStrOld_ << "-operation.bin";
    oldFile_ = oldOs.str();
    tmpFile_ = tmpOs.str();
    defFile_ = defOs.str();
    fname_ = tmpOs.str();
    wf_ = (mode=="r") ? new std::fstream(defOs.str(), std::fstream::in | std::fstream::binary) : 
          (mode=="w") ? new std::fstream(fname_, std::fstream::out | std::fstream::trunc | std::fstream::binary) : 
          nullptr;
    check();
    // Exit point
    return;
}

void StatisticsIO::writePeriod( const eckit::DateTime& startTime, const eckit::DateTime& endTime,const eckit::DateTime& creationTime ){
    std::array<int64_t,8> data;
    data[0] = 8L;
    data[1] = startTime.date().yyyymmdd();
    data[2] = startTime.time().hhmmss();
    data[3] = endTime.date().yyyymmdd();
    data[4] = endTime.time().hhmmss();
    data[5] = creationTime.date().yyyymmdd();
    data[6] = creationTime.time().hhmmss();
    data[7] = seed_;
    for (  int i = 0; i<7; ++i){
        data[7] ^= data[i];
    }
    check();
    wf_->write( (char*)&data[0], 8*sizeof(int64_t) );
    check();
    wf_->flush();
    check();
    // Exit point
    return;
};


void StatisticsIO::readPeriod( eckit::DateTime& startTime, eckit::DateTime& endTime, eckit::DateTime& creationTime ){
    fileLength();
    if ( fileLengthInBytes_ != 8*sizeof(int64_t) ) {
        throw eckit::SeriousBug("Error file dimension mismatch!", Here());
    }
    std::array<int64_t,8> data{{0L,0L,0L,0L,0L,0L,0L,0L}};
    // Read array
    wf_->read( reinterpret_cast<char*>(&data[0]), 8*sizeof(long) );
    check();
    wf_->flush();
    check();
    int64_t checksum=seed_;
    // Compute checksum
    for (  int i = 0; i<7; ++i){
        checksum ^= data[i];
    }
    // Verify
    if (data[0]!=8){
        throw eckit::SeriousBug("Error wrong dimension of the readed file", Here());
    }
    if (checksum!=data[7]){
        throw eckit::SeriousBug("Error checksum mismatch", Here());    
    }
    startTime    = eckit::DateTime{eckit::Date{data[1]},eckit::Time{data[2]}};
    endTime      = eckit::DateTime{eckit::Date{data[3]},eckit::Time{data[4]}};
    creationTime = eckit::DateTime{eckit::Date{data[5]},eckit::Time{data[6]}};
    // Exit point
    return;
};


void StatisticsIO::endPeriod( ){
    if (wf_){
        if ( wf_->is_open() ){
            wf_->close();
            if (!wf_->good()) {
                throw eckit::SeriousBug("Error occurred at file closing time!", Here());
            }
            if (tmpFile_.exists()){
                eckit::PathName::rename(tmpFile_, defFile_);
            }
            if (oldFile_.exists()) {
                oldFile_.unlink();
            }
        }
        delete wf_;
        wf_ = nullptr;
        fname_ = "";
        oldFile_ = "";
        tmpFile_ = "";
        defFile_ = "";
        fileLengthInBytes_ = 0;
    }
    // Exit point
    return;
}

void StatisticsIO::endOperation( ){
    if (wf_){
        if ( wf_->is_open() ){
            wf_->close();
            if (!wf_->good()) {
                throw eckit::SeriousBug("Error occurred at file closing time!", Here());
            }
            if (tmpFile_.exists()){
                eckit::PathName::rename(tmpFile_, defFile_);
            }
            if (oldFile_.exists()) {
                oldFile_.unlink();
            }
        }
        delete wf_;
        wf_ = nullptr;
        fname_ = "";
        oldFile_ = "";
        tmpFile_ = "";
        defFile_ = "";
        fileLengthInBytes_ = 0;
    }
    // Exit point
    return;
}

}

