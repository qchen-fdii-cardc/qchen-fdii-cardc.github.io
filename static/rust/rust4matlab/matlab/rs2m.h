#ifndef RS2M_H
#define RS2M_H

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    // Basic addition function
    uint64_t add(uint64_t left, uint64_t right);

    // Generate evenly spaced array
    int32_t linspace(double start, double end, int32_t n, double *out_ptr);

    // Square function
    double square(double x);

#ifdef __cplusplus
}
#endif

#endif // RS2M_H