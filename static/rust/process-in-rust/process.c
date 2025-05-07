#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main()
{
    pid_t pid;
    pid = fork();
    if (pid < 0)
    {
        perror("fork");
        return 1;
    }

    if (pid == 0)
    {
        printf("child process\n");
        char *args[] = {"ls", "-l", NULL};
        execv("/bin/ls", args);
        perror("execv /bin/ls failed.");
        return 127;
    }
    else
    {
        printf("parent process\n");
        // wait for child process to exit
        int status;
        wait(&status);
        printf("child process exited with status %d\n", status);
    }
    return 0;
}
