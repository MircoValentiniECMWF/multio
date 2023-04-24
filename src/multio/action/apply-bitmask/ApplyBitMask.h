/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <cstdint>
#include <map>
#include <vector>

#include "multio/action/ChainedAction.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {

class Bitmask {

private:
    std::vector<bool> isMissing_;
    long numberOfMissing_;

public:
    Bitmask(size_t size) : isMissing_(size, false), numberOfMissing_{0} { return; };

    void updateBitMask(const void* mask, size_t size) {
        const uint8_t* loc_mask = reinterpret_cast<const uint8_t*>(mask);
        ASSERT(size == isMissing_.size());
        for (int i = 0; i < size; ++i) {
            isMissing_[i] = loc_mask[i] == 0 ? true : false;
            numberOfMissing_ += loc_mask[i] == 0 ? 1 : 0;
        }
        return;
    };

    void ApplyMask(std::vector<double>& val, const double& referenceValue) const {
        // std::cout << "APPLY bit mask :: (nom=" << numberOfMissing_ << ")" << isMissing_.size() << ", " << val.size()
        //          << std::endl;
        ASSERT(isMissing_.size() == val.size());
        for (int i = 0; i < isMissing_.size(); ++i) {
            val[i] = isMissing_[i] ? referenceValue : val[i];
        }
        return;
    };
};


class ApplyBitMask : public ChainedAction {
public:
    explicit ApplyBitMask(const ConfigurationContext& config);

    void executeImpl(message::Message msg) override;

private:
    template <typename T>
    message::Message maskField(const std::string& key, message::Message&& msg) const {
        const T* val = static_cast<const T*>(msg.payload().data());
        long sz = msg.size() / sizeof(T);
        std::vector<double> outData(sz);
        for (int i = 0; i < sz; ++i) {
            outData[i] = static_cast<double>(val[i]);
        }
        mask_.at(key)->ApplyMask(outData, reference_value_);
        eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()), outData.size() * sizeof(double));
        message::Metadata md{msg.metadata()};
        md.set("missingValue", reference_value_);
        md.set("precision", "double");
        return {message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
                std::move(buffer)};
    }
    std::string getKey(const message::Message& msg) const;
    std::map<std::string, std::unique_ptr<Bitmask>> mask_;
    double reference_value_;
    void print(std::ostream& os) const override;
};

}  // namespace action
}  // namespace multio
