#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main()
{
    pid_t pid;
    printf("Parent process starting, PID: %d\n", getpid());

    pid = fork();
    if (pid < 0)
    {
        perror("fork");
        return 1;
    }

    if (pid == 0)
    {
        printf("Child process, PID: %d, Parent PID: %d\n", getpid(), getppid());
        char *args[] = {"ls", "-l", NULL};
        execv("/bin/ls", args);
        perror("execv /bin/ls failed.");
        return 127;
    }
    else
    {
        printf("Parent process continuing, PID: %d, Child PID: %d\n", getpid(), pid);
        // wait for child process to exit
        int status;
        wait(&status);
        printf("Child process exited with status %d\n", status);
    }
    return 0;
}
