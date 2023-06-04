#include <iostream>
#include <string>
#include <vector>
const std::vector<std::string> m{"jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"};
int main() {
    for (int y = 1993; y < 2024; ++y) {
        for (int i = 0; i < 12; ++i) {
            std::cout << m[i] << " = " << y << " - "
                      <<  // std::max(0,2-(i%2)*i)%2 << " - " << 31-std::max( 0, i%6-i/6)%2  <<
                " - "
                      << 31 - std::max(0, i % 6 - i / 6) % 2
                             - std::max(0, 2 - i * (i % 2)) % 2
                                   * (y % 4 == 0 ? y % 100 == 0 ? y % 400 == 0 ? 1 : 2 : 1 : 2)
                      << std::endl;
        }
        std::cout << std::endl << std::endl << std::endl;
    }
    return 0;
}