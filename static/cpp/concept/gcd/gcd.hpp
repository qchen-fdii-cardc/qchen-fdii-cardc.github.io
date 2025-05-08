#pragma once

#include <concepts>
#include <type_traits>
#include <algorithm>

// define a concept for GCD type
template <typename T>
concept GCDType = requires(T a, T b) {
    // require type supports comparison operation (only < and !=)
    { a < b } -> std::convertible_to<bool>;
    { a != b } -> std::convertible_to<bool>;

    // require type supports modulo operation
    { a % b } -> std::same_as<T>;

    // require type supports assignment
    { a = b } -> std::same_as<T &>;

    // require type supports zero value
    { T(0) } -> std::same_as<T>;

    // require type supports abs for signed types
    requires(!std::is_signed_v<T> || requires { { std::abs(a) } -> std::same_as<T>; });

    // require type supports swap
    { std::swap(a, b) } -> std::same_as<void>;
};

/**
 * @brief Calculate the greatest common divisor of two numbers.
 *
 * @tparam T The type of the numbers, must support comparison and modulo operations.
 * @param a First number
 * @param b Second number
 * @return The greatest common divisor of a and b
 */
template <GCDType T>
constexpr T gcd(T a, T b)
{
    // Handle negative numbers
    if constexpr (std::is_signed_v<T>)
    {
        a = std::abs(a);
        b = std::abs(b);
    }

    // Ensure a >= b
    if (a < b)
    {
        std::swap(a, b);
    }

    // Euclidean algorithm
    while (b != T(0))
    {
        T temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}