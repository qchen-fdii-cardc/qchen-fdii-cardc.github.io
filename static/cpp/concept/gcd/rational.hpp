#pragma once

#include <concepts>
#include <iostream>
#include <stdexcept>
#include "gcd.hpp"

// define a concept for integral type
template <typename T>
concept Integral = std::is_integral_v<T>;

/**
 * @brief A template class representing a rational number.
 *
 * This class implements a rational number with a numerator and denominator.
 * The template parameter T must be an integral type.
 *
 * @tparam T The type of the numerator and denominator, must be an integral type.
 *
 * @exception std::invalid_argument Thrown in the following cases:
 *   - When the denominator is zero in the constructor
 *   - When attempting to divide by zero in operator/=
 *
 * @note The class automatically simplifies fractions and normalizes signs.
 *       The denominator is always positive after construction.
 */
template <Integral T>
struct Rational
{
    T numerator;
    T denominator;

    /**
     * @brief Constructs a rational number.
     *
     * @param num The numerator (default: 0)
     * @param den The denominator (default: 1)
     *
     * @exception std::invalid_argument If den is zero
     *
     * @note The constructor will:
     *   - Throw if denominator is zero
     *   - Normalize the sign (denominator will be positive)
     *   - Simplify the fraction
     */
    constexpr Rational(T num = 0, T den = 1)
        : numerator(num), denominator(den)
    {
        if (denominator == 0)
        {
            throw std::invalid_argument("Denominator cannot be zero");
        }
        // normalize sign
        if (denominator < 0)
        {
            numerator = -numerator;
            denominator = -denominator;
        }
        simplify();
    }

    // copy constructor
    constexpr Rational(const Rational &other) = default;

    // move constructor
    constexpr Rational(Rational &&other) noexcept = default;

    // copy assignment operator
    constexpr Rational &operator=(const Rational &other) = default;

    // move assignment operator
    constexpr Rational &operator=(Rational &&other) noexcept = default;

    // destructor
    ~Rational() = default;

    // simplify fraction
    Rational &simplify()
    {
        T g = gcd(numerator, denominator);
        numerator /= g;
        denominator /= g;
        return *this;
    }

    // unary operators
    Rational operator+() const { return *this; }
    Rational operator-() const { return Rational(-numerator, denominator); }

    // arithmetic operators
    Rational &operator+=(const Rational &r)
    {
        numerator = numerator * r.denominator + denominator * r.numerator;
        denominator *= r.denominator;
        return simplify();
    }

    Rational &operator-=(const Rational &r)
    {
        numerator = numerator * r.denominator - denominator * r.numerator;
        denominator *= r.denominator;
        return simplify();
    }

    Rational &operator*=(const Rational &r)
    {
        numerator *= r.numerator;
        denominator *= r.denominator;
        return simplify();
    }

    Rational &operator/=(const Rational &r)
    {
        if (r.numerator == 0)
        {
            throw std::invalid_argument("Division by zero");
        }
        numerator *= r.denominator;
        denominator *= r.numerator;
        return simplify();
    }

    // binary operators
    friend Rational operator+(const Rational &lhs, const Rational &rhs)
    {
        Rational result = lhs;
        return result += rhs;
    }

    friend Rational operator-(const Rational &lhs, const Rational &rhs)
    {
        Rational result = lhs;
        return result -= rhs;
    }

    friend Rational operator*(const Rational &lhs, const Rational &rhs)
    {
        Rational result = lhs;
        return result *= rhs;
    }

    friend Rational operator/(const Rational &lhs, const Rational &rhs)
    {
        Rational result = lhs;
        return result /= rhs;
    }

    // comparison operators
    bool operator==(const Rational &r) const
    {
        return numerator * r.denominator == denominator * r.numerator;
    }

    bool operator<(const Rational &r) const
    {
        return numerator * r.denominator < denominator * r.numerator;
    }

    bool operator!=(const Rational &r) const { return !(*this == r); }
    bool operator>(const Rational &r) const { return r < *this; }
    bool operator<=(const Rational &r) const { return !(r < *this); }
    bool operator>=(const Rational &r) const { return !(*this < r); }

    // conversion operators
    template <Integral U>
    explicit operator U() const
    {
        return static_cast<U>(numerator) / static_cast<U>(denominator);
    }

    // stream operators
    friend std::ostream &operator<<(std::ostream &os, const Rational &r)
    {
        if (r.denominator == 1)
        {
            return os << r.numerator;
        }
        return os << r.numerator << "/" << r.denominator;
    }

    friend std::istream &operator>>(std::istream &is, Rational &r)
    {
        T num, den;
        char slash;
        if (is >> num >> slash >> den)
        {
            if (slash != '/')
            {
                is.setstate(std::ios::failbit);
            }
            else
            {
                try
                {
                    r = Rational(num, den);
                }
                catch (...)
                {
                    is.setstate(std::ios::failbit);
                }
            }
        }
        return is;
    }
};