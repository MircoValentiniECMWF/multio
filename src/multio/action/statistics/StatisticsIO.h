#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "eckit/filesystem/PathName.h"

namespace multio::action {

uint64_t computeChecksum(const std::vector<std::uint64_t>& state, std::size_t size);

class StatisticsIO {
public:
    StatisticsIO(const std::string& path, const std::string& prefix, const std::string& ext);
    virtual ~StatisticsIO() = default;

    void setKey(const std::string& key);
    void setCurrStep(long step);
    void setPrevStep(long step);
    void setSuffix(const std::string& suffix);
    void reset();
    std::vector<std::uint64_t>& getBuffer(std::size_t size);

    virtual void write(const std::string& name, std::size_t writeSize) = 0;
    virtual void read(const std::string& name, std::size_t readSize) = 0;
    virtual void flush() = 0;


protected:
    std::string generatePathName() const;
    std::string generateCurrFileName(const std::string& name) const;
    std::string generatePrevFileName(const std::string& name) const;
    void removeCurrFile(const std::string& name) const;
    void removePrevFile(const std::string& name) const;

    const std::string path_;
    const std::string prefix_;
    long prevStep_;
    long currStep_;

    std::string key_;
    std::string suffix_;
    std::string name_;
    const std::string ext_;

    std::vector<std::uint64_t> buffer_;
};

class StatisticsIOBuilderBase;

class StatisticsIOFactory : private eckit::NonCopyable {
private:  // methods
    StatisticsIOFactory() {}

public:  // methods
    static StatisticsIOFactory& instance();

    void enregister(const std::string& name, const StatisticsIOBuilderBase* builder);
    void deregister(const std::string& name);

    void list(std::ostream&);

    std::shared_ptr<StatisticsIO> build(const std::string& name, const std::string& path, const std::string& prefix);

private:  // members
    std::map<std::string, const StatisticsIOBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class StatisticsIOBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix) const = 0;

protected:  // methods
    StatisticsIOBuilderBase(const std::string&);

    virtual ~StatisticsIOBuilderBase();

    std::string name_;
};

template <class T>
class StatisticsIOBuilder final : public StatisticsIOBuilderBase {
    std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix) const override {
        return std::make_shared<T>(path, prefix);
    }

public:
    StatisticsIOBuilder(const std::string& name) : StatisticsIOBuilderBase(name) {}
};

}  // namespace multio::action
