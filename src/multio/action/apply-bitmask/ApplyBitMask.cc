/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "ApplyBitMask.h"

#include "multio/util/PrecisionTag.h"

namespace multio {
namespace action {


ApplyBitMask::ApplyBitMask(const ConfigurationContext& confCtx) :
    ChainedAction(confCtx), reference_value_{confCtx.config().getDouble("value", double(-6.0E10))} {}

std::string ApplyBitMask::getKey(const message::Message& msg) const {
    std::ostringstream os;
    os << std::to_string(
        std::hash<std::string>{}(msg.metadata().getString("param", "") + msg.metadata().getString("paramId", "")))
       << std::to_string(std::hash<long>{}(msg.metadata().getLong("level", 0) | msg.metadata().getLong("levelist", 0)))
       << std::to_string(std::hash<std::string>{}(msg.metadata().getString("levtype", "unknown")))
       << std::to_string(std::hash<std::string>{}(msg.metadata().getString("gridType", "unknown")))
       << std::to_string(std::hash<std::string>{}(msg.metadata().getString("globalSize", "unknown")))
       << std::to_string(std::hash<std::string>{}(msg.source()));
    return os.str();
}

void ApplyBitMask::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field && msg.tag() != message::Message::Tag::Mask) {
        executeNext(std::move(msg));
        return;
    }

    std::string key = getKey(msg);

    if (msg.tag() == message::Message::Tag::Mask) {
        if (mask_.find(key) == mask_.end()) {
            mask_[key] = std::make_unique<Bitmask>(msg.size());
        }
        mask_.at(key)->updateBitMask(msg.payload().data(), msg.size());
        return;
    }

    if (mask_.find(key) != mask_.end() && msg.metadata().getBool("bitmapPresent")) {
        executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
            using Precision = typename decltype(pt)::type;
            return maskField<Precision>(key, std::move(msg));
        }));
    }
    else {
        executeNext(std::move(msg));
    }
}

void ApplyBitMask::print(std::ostream& os) const {
    os << "ApplyBitMask";
}


static ActionBuilder<ApplyBitMask> ApplyBitMaskBuilder("apply-bitmask");

}  // namespace action
}  // namespace multio
