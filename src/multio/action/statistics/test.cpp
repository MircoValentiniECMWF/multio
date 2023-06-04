#include <algorithm>
#include <array>
#include <iostream>
#include <memory>
#include <vector>


class A {
public:
    A(int as) : as_{as} {};
    virtual int ret() = 0;

private:
    int as_;
};


class B : public A {
public:
    B(int b) : A{b}, bs_{b + 1} {};
    int ret() { return 94385672; };

private:
    int bs_;
};


std::unique_ptr<A> make_a(int a) {
    return std::make_unique<B>(a);
};


class test {
public:
    test(int x) : xxx{make_a(x)}, b{xxx->ret()} { std::cout << "Valore di b : " << b << std::endl; };

private:
    const std::unique_ptr<A> xxx;
    int b;
};

int main() {

    std::array<long, 10> a{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    std::for_each(a.cbegin(), a.cend(), [](const long& v) { std::cout << v << std::endl; });
    auto last = a.cend();
    std::for_each(a.cbegin(), --last, [](const long& v) { std::cout << v << std::endl; });

    std::vector<double> xx{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
    std::vector<float> yy(9);

    std::transform(xx.cbegin(), xx.cend(), yy.begin(), [](double v) { return float(v); });
    std::for_each(yy.cbegin(), yy.cend(), [](float v) {
        std::cout << "Porcaccia :: " << v << std::endl;
        return;
    });

    test p(34);

    return 0;
}