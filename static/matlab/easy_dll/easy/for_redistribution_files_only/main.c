#include <stdio.h>
#include "easy.h"

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#endif

// Correct function signatures with proper calling convention
static int errorHandler(const char *msg)
{
    printf("MATLAB Error: %s\n", msg);
    fflush(stdout);
    return 0;
}

static int printHandler(const char *msg)
{
    printf("MATLAB Message: %s\n", msg);
    fflush(stdout);
    return 0;
}

int main()
{
    printf("Starting MATLAB runtime initialization...\n");
    fflush(stdout);

    const char *args[] = {"-singleCompThread"};
    if (!mclInitializeApplication(args, 1))
    {
        printf("Failed to initialize MATLAB runtime\n");
        fflush(stdout);
        return -1;
    }

    // Initialize MATLAB runtime with handlers
    if (!easyInitializeWithHandlers(errorHandler, printHandler))
    {
        printf("Failed to initialize MATLAB runtime\n");
        fflush(stdout);
        return -1;
    }
    printf("MATLAB runtime initialized successfully\n");
    fflush(stdout);

    // Create input parameters
    mxArray *a = mxCreateDoubleScalar(1.0);
    mxArray *b = mxCreateDoubleScalar(2.0);
    mxArray *result = NULL;

    // Call MATLAB function
    if (mlfEasy(1, &result, a, b))
    {
        // Get result
        double *resultData = mxGetPr(result);
        printf("Calculation result: %f\n", resultData[0]);
        fflush(stdout);

        // Free result array
        mxDestroyArray(result);
    }
    else
    {
        printf("Function call failed\n");
        fflush(stdout);
    }

    // Clean up input parameters
    mxDestroyArray(a);
    mxDestroyArray(b);

    printf("Terminating MATLAB runtime...\n");
    fflush(stdout);

    // Terminate MATLAB runtime
    easyTerminate();

    printf("easy library runtime terminated\n");
    fflush(stdout);

    printf("Attempting graceful exit...\n");
    fflush(stdout);

    if (!mclTerminateApplication())
    {
        printf("Failed to terminate MATLAB runtime\n");
        fflush(stdout);

        // Try multiple termination methods
        printf("Using TerminateProcess...\n");
        fflush(stdout);

#ifdef _WIN32
        // Use TerminateProcess instead of ExitProcess
        HANDLE hProcess = GetCurrentProcess();
        TerminateProcess(hProcess, -1);
#endif
        return -1;
    }
    // If we reach here, TerminateProcess failed
    printf("mclTerminateApplication succeeded, exit...\n");
    fflush(stdout);
    return 0;
}