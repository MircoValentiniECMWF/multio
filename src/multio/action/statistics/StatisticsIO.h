#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "eckit/filesystem/PathName.h"
#include "eckit/types/DateTime.h"

namespace multio::action {

class StatisticsIO {
public:
    StatisticsIO();
    virtual void setPath(const std::string& path) = 0;
    virtual void setName(const std::string& baseName) = 0;
    virtual void setDetail(const std::string& suffix, long step) = 0;
    virtual void writePeriod(const std::string& name, const std::array<std::uint64_t, 15>& data) = 0;
    virtual void readPeriod(const std::string& name, std::array<std::uint64_t, 15>& data) = 0;
    virtual void writeOperation(const std::string& name, const std::vector<double>& data) = 0;
    virtual void readOperation(const std::string& name, std::vector<double>& data) = 0;
    virtual void flush() = 0;

private:
    std::string path_;
    std::string name_;
    std::string suffix_;
    long step_;
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

    std::shared_ptr<StatisticsIO> build(const std::string&);

private:  // members
    std::map<std::string, const StatisticsIOBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class StatisticsIOBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::shared_ptr<StatisticsIO> make() const = 0;

protected:  // methods
    StatisticsIOBuilderBase(const std::string&);

    virtual ~StatisticsIOBuilderBase();

    std::string name_;
};

template <class T>
class StatisticsIOBuilder final : public StatisticsIOBuilderBase {
    std::shared_ptr<StatisticsIO> make() const override { return std::make_shared<T>(); }

public:
    StatisticsIOBuilder(const std::string& name) : StatisticsIOBuilderBase(name) {}
};

}  // namespace multio::action