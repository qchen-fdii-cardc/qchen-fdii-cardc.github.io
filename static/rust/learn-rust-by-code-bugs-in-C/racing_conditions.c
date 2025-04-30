#include <stdio.h>
#include <stdlib.h>
#include <threads.h>

int running = 1;
int run_print = 1;

int increment_thread(void *arg)
{
    srand(42);
    int *counter = (int *)arg;
    while (running)
    {
        int delta = rand() % 10;
        printf("increment: %d\n", delta);
        (*counter) += delta;
        thrd_sleep(&(struct timespec){.tv_sec = 0, .tv_nsec = 200000000}, NULL);
    }
    return 0;
}

int decrement_thread(void *arg)
{
    srand(43);
    int *counter = (int *)arg;
    while (running)
    {
        int delta = rand() % 10;
        printf("decrement: %d\n", delta);
        (*counter) -= delta;
        thrd_sleep(&(struct timespec){.tv_sec = 0, .tv_nsec = 200000000}, NULL);
    }
    return 0;
}

int print_counter(void *arg)
{
    int *counter = (int *)arg;
    while (run_print)
    {
        printf("counter: %d\n", *counter);
        thrd_sleep(&(struct timespec){.tv_sec = 0, .tv_nsec = 500000000}, NULL);
    }
    return 0;
}

int main()
{
    int counter = 0;
    thrd_t tid1, tid2, tid3;
    thrd_create(&tid3, print_counter, &counter);
    thrd_create(&tid1, increment_thread, &counter);
    thrd_create(&tid2, decrement_thread, &counter);

    thrd_sleep(&(struct timespec){.tv_sec = 3, .tv_nsec = 0}, NULL);
    running = 0;
    thrd_join(tid1, NULL);
    thrd_join(tid2, NULL);
    run_print = 0;
    thrd_join(tid3, NULL);
    printf("counter: %d\n", counter);
    return 0;
}
