
#include <string>

#include "multio/action/statistics/MovingWindow.h"
#include "multio/action/statistics/StatisticsConfiguration.h"
#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

class OperationBase {
public:
    OperationBase(const std::string& name, const std::string& operation, const MovingWindow& win,
                  const StatisticsConfiguration& cfg) :
        name_{name},
        operation_{operation},
        logHeader_{"operation(" + operation_ + "::" + name_ + ")"},
        cfg_{cfg},
        win_{win} {};

    virtual ~OperationBase() = default;

    const std::string& name() const { return name_; };
    const std::string& operation() const { return operation_; };
    void print(std::ostream& os) const {
        os << logHeader_;
        return;
    }

protected:
    const std::string name_;
    const std::string operation_;
    const std::string logHeader_;
    const StatisticsConfiguration& cfg_;
    const MovingWindow& win_;


    friend std::ostream& operator<<(std::ostream& os, const OperationBase& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
