/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#include <fstream>
#include <regex>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/PeekHandle.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/message/Decoder.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/value/Value.h"
#include "metkit/codes/CodesSplitter.h"
#include "multio/ifsio/ifsio.h"
#include "multio/tools/MultioTool.h"

namespace multio {
namespace test {

namespace {
class TempFile {
  const std::string path_;

 public:
  TempFile(std::string&& path) : path_{std::move(path)} {}
  ~TempFile() { std::remove(path_.c_str()); }

  const std::string& path() const { return path_; }
};

class MetadataSetter : public eckit::LocalConfiguration {
  // void print(std::ostream& os) const {
  //     os << *root_ << std::endl;
  // }
  // friend std::ostream& operator<<(std::ostream& os, const MetadataSetter& md)
  // {
  //     md.print(os);
  //     return os;
  // }

 public:
  using eckit::LocalConfiguration::getDouble;
  using eckit::LocalConfiguration::getLong;
  using eckit::LocalConfiguration::getString;
  using eckit::LocalConfiguration::has;

  template <typename T>
  void setValue(const std::string& key, const T& value) {
    set(key, value);
  }

  template <typename T>
  T get(const std::string& key) {
    T value;
    eckit::LocalConfiguration::get(key, value);
    return value;
  }

  std::vector<std::string> keys() { return eckit::LocalConfiguration::keys(); }
};
}  // namespace

class MultioFeed final : public multio::MultioTool {
 public:  // methods
  MultioFeed(int argc, char** argv);

 private:
  void usage(const std::string& tool) const override {
    eckit::Log::info() << std::endl
                       << "Usage: " << tool << " [options]" << std::endl;
  }

  void init(const eckit::option::CmdArgs& args) override;

  void finish(const eckit::option::CmdArgs&) override;

  void execute(const eckit::option::CmdArgs& args) override;

  bool subtocExists() const;

  eckit::PathName fdbRootPath_;

  bool testSubtoc_ = false;
  bool decodeDoubleData_ = false;
  bool decodeSingleData_ = false;
  std::string configPath_ = "";
};

MultioFeed::MultioFeed(int argc, char** argv)
    : multio::MultioTool{argc, argv},
      fdbRootPath_{"~multio/multio/tests/fdb/root"} {
  options_.push_back(new eckit::option::SimpleOption<bool>(
      "test-subtoc", "Test if subtoc has been created"));
  options_.push_back(new eckit::option::SimpleOption<bool>(
      "decode",
      "Decode messages and pass raw data with metadata through the pipeline "
      "(with data in double precision)"));
  options_.push_back(new eckit::option::SimpleOption<bool>(
      "decodeSingle",
      "Decode messages and pass raw data with metadata through the pipeline "
      "(with data in single precision)"));
  options_.push_back(new eckit::option::SimpleOption<std::string>(
      "plans", "Path to YAML/JSON file containing plans and actions."));
}

void MultioFeed::init(const eckit::option::CmdArgs& args) {
  args.get("test-subtoc", testSubtoc_);
  if (testSubtoc_) {
    std::system(
        std::string{"rm -rf " + fdbRootPath_.asString() + "/*"}.c_str());
    fdbRootPath_.mkdir();
  }
  args.get("decode", decodeDoubleData_);
  args.get("decodeSingle", decodeSingleData_);

  if (decodeDoubleData_ && decodeSingleData_) {
    throw eckit::BadValue{"Both double and single precision requested"};
  }

  args.get("plans", configPath_);

  if (!configPath_.empty()) {
    ::setenv("MULTIO_PLANS_FILE", configPath_.c_str(), 1);
  }
}

void MultioFeed::finish(const eckit::option::CmdArgs&) {}

void MultioFeed::execute(const eckit::option::CmdArgs& args) {
  using eckit::message::ValueRepresentation;
  eckit::message::Reader reader{args(0)};

  eckit::message::Message msg;

  while ((msg = reader.next())) {
    if (decodeDoubleData_ || decodeSingleData_) {
      MetadataSetter metadata;
      eckit::message::TypedSetter<MetadataSetter> gatherer{metadata};
      eckit::message::GetMetadataOptions mdOpts{};
      mdOpts.valueRepresentation = ValueRepresentation::Native;
      mdOpts.nameSpace = "mars";
      msg.getMetadata(gatherer, mdOpts);

      MetadataSetter metadataDetailed;
      eckit::message::TypedSetter<MetadataSetter> gathererDetailed{
          metadataDetailed};
      mdOpts.valueRepresentation = ValueRepresentation::Native;
      mdOpts.nameSpace = "";
      msg.getMetadata(gathererDetailed, mdOpts);

      if (metadataDetailed.has("gridType"))
        metadata.set("gridType", metadataDetailed.getString("gridType"));

      // Maybe use gridType?
      if (metadataDetailed.getBool("sphericalHarmonics", false)) {
        metadata.set("sphericalHarmonics", true);

        if (metadataDetailed.has("complexPacking"))
          metadata.set("complexPacking",
                       metadataDetailed.getLong("complexPacking"));
        if (metadataDetailed.has("generatingProcessIdentifier"))
          metadata.set("generatingProcessIdentifier",
                       metadataDetailed.getLong("generatingProcessIdentifier"));
        if (metadataDetailed.has("J"))
          metadata.set("pentagonalResolutionParameterJ",
                       metadataDetailed.getLong("J"));
        if (metadataDetailed.has("K"))
          metadata.set("pentagonalResolutionParameterK",
                       metadataDetailed.getLong("K"));
        if (metadataDetailed.has("M"))
          metadata.set("pentagonalResolutionParameterM",
                       metadataDetailed.getLong("M"));
        if (metadataDetailed.has("JS"))
          metadata.set("subSetJ", metadataDetailed.getLong("JS"));
        if (metadataDetailed.has("KS"))
          metadata.set("subSetK", metadataDetailed.getLong("KS"));
        if (metadataDetailed.has("MS"))
          metadata.set("subSetM", metadataDetailed.getLong("MS"));

        // Seems not to be settable in codes
        metadata.set("unpackedSubsetPrecision", 1);
      }

      // Name is not required but convenient to print...
      if (metadataDetailed.has("name")) {
        metadata.set("name", metadataDetailed.getString("name"));
      }
      if (metadataDetailed.has("paramId")) {
        metadata.set("paramId", metadataDetailed.getLong("paramId"));
      }
      if (metadataDetailed.has("param")) {
        metadata.set("param", metadataDetailed.getLong("param"));
        if (!metadata.has("paramId")) {
          metadata.set("paramId", metadataDetailed.getLong("param"));
        }
      }
      if (metadataDetailed.has("GRIBEditionNumber")) {
        metadata.set("gribEdition",
                     metadataDetailed.getString("GRIBEditionNumber"));
      }
      if (!metadata.has("level") && metadataDetailed.has("level")) {
        metadata.set("level", metadataDetailed.getString("level"));
      }

      eckit::Buffer data = msg.decode();

      metadata.set("globalSize", data.size() / sizeof(double));

      if (decodeDoubleData_) {
        metadata.set("precision", "double");
        size_t words =
            eckit::round(data.size(), sizeof(fortint)) / sizeof(fortint);
        fortint iwords = static_cast<fortint>(words);

        if (imultio_write_raw_(&metadata,
                               reinterpret_cast<const void*>(data.data()),
                               &iwords)) {
          ASSERT(false);
        }
      } else {
        metadata.set("precision", "single");
        size_t words =
            eckit::round(data.size() / sizeof(double) * sizeof(float),
                         sizeof(fortint)) /
            sizeof(fortint);
        fortint iwords = static_cast<fortint>(words);

        std::vector<float> tmp(data.size() / sizeof(double), 0.0);
        const double* srcData = reinterpret_cast<const double*>(data.data());
        for (int i = 0; i < tmp.size(); ++i) {
          tmp[i] = float(srcData[i]);
        }
        if (imultio_write_raw_(&metadata,
                               reinterpret_cast<const void*>(tmp.data()),
                               &iwords)) {
          ASSERT(false);
        }
      }
    } else {
      size_t words =
          eckit::round(msg.length(), sizeof(fortint)) / sizeof(fortint);

      fortint iwords = static_cast<fortint>(words);

      if (imultio_write_(msg.data(), &iwords)) {
        ASSERT(false);
      }
    }
  }

  if (imultio_flush_()) {
    ASSERT(false);
  }

  if (testSubtoc_) {
    ASSERT(subtocExists());
  }
}

bool MultioFeed::subtocExists() const {
  TempFile file{"tmp.out"};

  std::string cmd{"find " + fdbRootPath_.asString() + " -name toc* > " +
                  file.path()};
  std::system(cmd.c_str());

  const std::regex subtoc{"^toc\\.[0-9]{8}\\.[0-9]{6}\\..*",
                          std::regex_constants::egrep};

  std::ifstream ifstrm{file.path().c_str()};
  std::string line;
  while (std::getline(ifstrm, line)) {
    auto fname = line.substr(line.rfind("/") + 1);
    if (std::regex_match(fname, subtoc)) {
      return true;
    }
  }
  return false;
}

}  // namespace test
}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
  multio::test::MultioFeed tool(argc, argv);
  return tool.start();
}
