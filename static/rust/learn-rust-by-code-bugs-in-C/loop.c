#include <stdio.h>
#include <threads.h>

typedef struct Vec
{
    int data;
    struct Vec *next;
} Vec;

Vec *create_node(int data)
{
    Vec *new_node = (Vec *)malloc(sizeof(Vec));
    new_node->data = data;
    new_node->next = NULL;
    return new_node;
}

void push_end(Vec *head, int data)
{
    Vec *current = head;
    while (current->next != NULL)
    {
        current = current->next;
    }
    current->next = create_node(data);
}

Vec *next(Vec *head)
{
    return head->next;
}

int main()
{
    Vec *head = create_node(0);
    for (Vec *current = head; current != NULL; current = next(current))
    {
        printf("%d\n", current->data);
        push_end(current, current->data + 1);
        thrd_sleep(&(struct timespec){.tv_sec = 0, .tv_nsec = 100000000}, NULL);
    }
    free(head);
    return 0;
}