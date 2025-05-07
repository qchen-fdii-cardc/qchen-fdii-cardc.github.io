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
    }
    else
    {
        printf("parent process\n");
    }
    return 0;
}
