#pragma once

#include <array>
#include <cinttypes>

#include "StatisticsIO.h"

namespace multio::action {

class FstreamIO : StatisticsIO {
public:
    FstreamIO();
    void setPath(const std::string& path);
    void setName(const std::string& baseName, long step);
    void setDetail(const std::string& suffix, long step);
    void writePeriod(const std::string& name, const std::array<unit64_t, 15>& data);
    void readPeriod(const std::string& name, std::array<unit64_t, 15>& data);
    void writeOperation(const std::string& name, const std::vector<double>& data);
    void readOperation(const std::string& name, std::vector<double>& data);
    void flush();
};


StatisticsIOBuilder<FstreamIO> FstreamBuilder("fstream_io");

}  // namespace multio::action