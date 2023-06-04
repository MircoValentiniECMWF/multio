#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <string>

#include "eckit/filesystem/PathName.h"
#include "eckit/types/DateTime.h"

namespace multio::action {

class FstreamIO : public StatisticsIO {
private:
    //
    long step_;
    long seed_;
    size_t fileLengthInBytes_;
    std::fstream* wf_;
    std::string stepStr_;
    std::string stepStrOld_;
    std::string fname_;
    eckit::PathName oldFile_;
    eckit::PathName tmpFile_;
    eckit::PathName defFile_;

    void fileLength();

    bool check() const;

public:
    StatisticsIO();
    void setStep(long step);

    int open(const std::string& path, const std::string name, const std::string& mode);
    int close();

    void startPeriod(const std::string& partialPath, const std::string periodKind, const std::string& mode);
    void writePeriod(const eckit::DateTime& startTime, const eckit::DateTime& endTime,
                     const eckit::DateTime& creationTime);
    void readPeriod(eckit::DateTime& startTime, eckit::DateTime& endTime, eckit::DateTime& creationTime);
    void endPeriod();

    void startOperation(const std::string& partialPath, const std::string& operationKind, const std::string& mode);
    template <typename T>
    void writeOperation(size_t count, const std::vector<T>& values) {
        std::array<std::int64_t, 3> data;
        data[0] = values.size();
        data[1] = count;
        data[2] = seed_;
        data[2] ^= data[0];
        data[2] ^= data[1];
        std::vector<double> tmp(values.size(), 0.0);
        std::transform(values.cbegin(), values.cend(), tmp.begin(), [&data](T v) {
            double tmp = static_cast<double>(v);
            data[2] ^= *reinterpret_cast<std::int64_t*>(&tmp);
            return tmp;
        });
        wf_->write(reinterpret_cast<char*>(&data[0]), data.size() * sizeof(std::int64_t));
        check();
        wf_->write(reinterpret_cast<char*>(&tmp[0]), tmp.size() * sizeof(double));
        check();
        wf_->flush();
        check();
        fileLength();
        // Exit point
        return;
    };

    template <typename T>
    void readOperation(size_t& count, std::vector<T>& values) {
        check();
        fileLength();
        std::array<std::int64_t, 3> data;
        wf_->read(reinterpret_cast<char*>(&data[0]), data.size() * sizeof(std::int64_t));
        check();
        if (data[0] != values.size()) {
            throw eckit::SeriousBug("Error dimension mismatch!", Here());
        }
        if (fileLengthInBytes_ != data.size() * sizeof(std::int64_t) + data[0] * sizeof(double)) {
            std::string d1(std::to_string(data.size() * sizeof(std::int64_t) + data[0] * sizeof(double)));
            std::string d2(std::to_string(fileLengthInBytes_));
            throw eckit::SeriousBug("Error file dimension mismatch :: " + d1 + ", " + d2, Here());
        }
        count = data[1];
        std::vector<double> tmp(values.size(), 0.0);
        wf_->read(reinterpret_cast<char*>(&tmp[0]), values.size() * sizeof(double));
        check();
        std::int64_t checksum = seed_;
        checksum = seed_;
        checksum ^= data[0];
        checksum ^= data[1];
        std::transform(tmp.cbegin(), tmp.cend(), values.begin(), [&checksum](double v) {
            T tmp = static_cast<T>(v);
            checksum ^= *reinterpret_cast<std::int64_t*>(&v);
            return tmp;
        });
        if (checksum != data[2]) {
            throw eckit::SeriousBug("Error checksum mismatch!", Here());
        }
        // Exit point
        return;
    };
    void endOperation();
};

}  // namespace multio::action