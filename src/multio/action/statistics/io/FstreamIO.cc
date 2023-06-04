#pragma once

#include <array>
#include <cinttypes>

#include "StatisticsIO.h"

namespace multio::action {

FstreamIO::FstreamIO(){};

void FstreamIO::setPath(const std::string& path) {
    return;
};

void FstreamIO::setName(const std::string& baseName) {
    return;
};

void FstreamIO::setDetail(const std::string& suffix, long step) {
    return;
};

void FstreamIO::writePeriod(const std::string& name, const std::array<unit64_t, 15>& data) {
    return;
};

void FstreamIO::readPeriod(const std::string& name, std::array<unit64_t, 15>& data) {
    return;
};

void FstreamIO::writeOperation(const std::string& name, const std::vector<double>& data) {
    return;
};

void FstreamIO::readOperation(const std::string& name, std::vector<double>& data) {
    return;
};

void FstreamIO::flush() {
    return;
};
};


StatisticsIOBuilder<FstreamIO> FstreamBuilder("fstream_io");

}  // namespace multio::action