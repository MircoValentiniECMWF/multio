namespace multio {
namespace action {

class StatisticsIO {

    StatisticsIO();

    bool isOpened() const;
    int open(const std::string& path, const std::string name);
    int close();

    bool check() const;

    int writePeriod(unsigned long kind, const eckit::DateTime& StartPoint, const eckit::DateTime& CreationPoint,
                    const eckit::DateTime& EndPoint);
    int readPeriod(unsigned long& kind, eckit::DateTime& StartPoint, eckit::DateTime& CreationPoint,
                   eckit::DateTime& EndPoint);


    int writeOperation();
    int readOperation();
};

}  // namespace action
}  // namespace multio