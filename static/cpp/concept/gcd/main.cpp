#include "gcd.hpp"
#include "rational.hpp"
#include <iostream>
#include <sstream>

int main()
{
    // signed integer
    std::cout << "GCD of -48 and 18: " << gcd(-48, 18) << std::endl;

    // unsigned integer
    std::cout << "GCD of 48 and 18: " << gcd(48u, 18u) << " " << gcd(18u, 48u) << std::endl;

    // long integer
    std::cout << "GCD of 123456789 and 987654321: "
              << gcd(123456789L, 987654321L) << std::endl;

    // long long integer
    std::cout << "GCD of 9223372036854775807 and 4611686018427387903: "
              << gcd(9223372036854775807LL, 4611686018427387903LL) << std::endl;

    // rational number tests
    Rational<int> r1(48, 18);
    Rational<int> r2(1, 3);
    std::cout << "r1 = " << r1 << std::endl;
    std::cout << "r2 = " << r2 << std::endl;
    std::cout << "r1 + r2 = " << r1 + r2 << std::endl;
    std::cout << "r1 - r2 = " << r1 - r2 << std::endl;
    std::cout << "r1 * r2 = " << r1 * r2 << std::endl;
    std::cout << "r1 / r2 = " << r1 / r2 << std::endl;
    std::cout << "r1 == r2: " << (r1 == r2) << std::endl;
    std::cout << "r1 < r2: " << (r1 < r2) << std::endl;

    // Test input stream operator with string streams
    std::cout << "\nTesting input stream operator:" << std::endl;

    // Test valid input
    {
        std::istringstream iss("3/4");
        Rational<int> r;
        iss >> r;
        std::cout << "Valid input '3/4': " << r << std::endl;
    }

    // Test negative numbers
    {
        std::istringstream iss("-5/2");
        Rational<int> r;
        iss >> r;
        std::cout << "Valid input '-5/2': " << r << std::endl;
    }

    // Test invalid format
    {
        std::istringstream iss("3.4");
        Rational<int> r;
        if (!(iss >> r))
        {
            std::cout << "Invalid format '3.4': Input failed as expected" << std::endl;
        }
    }

    // Test division by zero
    {
        std::istringstream iss("1/0");
        Rational<int> r;
        if (!(iss >> r))
        {
            std::cout << "Division by zero '1/0': Input failed as expected" << std::endl;
        }
    }

    // Test non-numeric input
    {
        std::istringstream iss("abc");
        Rational<int> r;
        if (!(iss >> r))
        {
            std::cout << "Non-numeric input 'abc': Input failed as expected" << std::endl;
        }
    }

    return 0;
}