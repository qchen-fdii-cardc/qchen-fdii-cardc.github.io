#include "gcd.hpp"
#include "rational.hpp"
#include <iostream>
#include <sstream>
#include <boost/multiprecision/cpp_int.hpp>

template <typename T>
T gcd_no_concept(T a, T b)
{
    if (a < b)
    {
        std::swap(a, b);
    }
    while (b != 0)
    {
        T temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int main()
{
    char result = gcd_no_concept('a', 'b');
    std::cout << "GCD of 'a' and 'b': " << int(result) << std::endl;
    char *p = gcd_no_concept("a", "b");
    std::cout << "GCD of 'a' and 'b': " << int(*p) << std::endl;
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

    using namespace boost::multiprecision;
    cpp_int a("1234567890123456789012345678901234567890");
    cpp_int b("9876543210987654321098765432109876543210");

    std::cout << "a = " << a << "\n";
    std::cout << "b = " << b << "\n";
    std::cout << "a % b = " << a % b << "\n";
    std::cout << "b % a = " << b % a << "\n";
    std::cout << "a / b = " << a / b << "\n";
    std::cout << "b / a = " << b / a << "\n";
    std::cout << "GCD of " << a << " and " << b << ": " << gcd(a, b) << std::endl;
    std::cout << "GCD of " << a << " and " << b << ": " << gcd_no_concept(a, b) << std::endl;

    return 0;
}