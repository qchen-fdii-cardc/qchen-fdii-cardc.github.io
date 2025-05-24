#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#include <tlhelp32.h>
#include <process.h>
#else
#include <unistd.h>
#endif
#include "easy.h"

// 错误处理函数
void errorHandler(const char* msg) {
    printf("MATLAB Error: %s\n", msg);
}

// 打印处理函数
void printHandler(const char* msg) {
    printf("MATLAB Message: %s\n", msg);
}

#ifdef _WIN32
// 终止所有MATLAB相关进程
void terminateMatlabProcesses() {
    // 使用taskkill命令强制终止所有MATLAB相关进程
    system("taskkill /F /IM matlab.exe /T");
    system("taskkill /F /IM mcr.exe /T");
    system("taskkill /F /IM MATLAB.exe /T");
    system("taskkill /F /IM MCR.exe /T");
    
    // 等待一小段时间确保进程被终止
    Sleep(1000);
}
#endif

int main() {
    printf("Starting MATLAB runtime initialization...\n");
    
    // Initialize MATLAB runtime with handlers
    if (!easyInitializeWithHandlers(errorHandler, printHandler)) {
        printf("Failed to initialize MATLAB runtime\n");
        return -1;
    }
    printf("MATLAB runtime initialized successfully\n");

    // Create input parameters
    mxArray* a = mxCreateDoubleScalar(1.0);
    mxArray* b = mxCreateDoubleScalar(2.0);
    mxArray* result = NULL;

    // Call MATLAB function
    if (mlfEasy(1, &result, a, b)) {
        // Get result
        double* resultData = mxGetPr(result);
        printf("Calculation result: %f\n", resultData[0]);
        
        // Free result array
        mxDestroyArray(result);
    } else {
        printf("Function call failed\n");
        easyPrintStackTrace();
    }

    // Clean up input parameters
    mxDestroyArray(a);
    mxDestroyArray(b);

    printf("Terminating MATLAB runtime...\n");
    
    // Print stack trace before termination
    easyPrintStackTrace();
    
    // Terminate MATLAB runtime
    easyTerminate();
    printf("MATLAB runtime terminated\n");

    // Force flush stdout
    fflush(stdout);
    
    // Add a small delay
    #ifdef _WIN32
    Sleep(1000);
    #else
    sleep(1);
    #endif

    // Force terminate any remaining MATLAB processes
    #ifdef _WIN32
    printf("Forcing termination of MATLAB processes...\n");
    terminateMatlabProcesses();
    printf("MATLAB processes terminated\n");
    #endif

    // 最后强制退出程序
    #ifdef _WIN32
    ExitProcess(0);
    #else
    _exit(0);
    #endif

    return 0;
} 