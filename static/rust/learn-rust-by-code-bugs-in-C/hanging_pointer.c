#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *get_arr()
{
    int x = 42;
    return &x;
}

int main()
{
    printf("case 1: dangling pointer\n");
    int *arr = (int *)malloc(sizeof(int) * 10);
    for (int i = 0; i < 10; i++)
    {
        arr[i] = i;
    }
    free(arr);
    for (int i = 0; i < 10; i++)
    {

        printf("%d ", arr[i]);
    }
    printf("\n");

    printf("case 2: memory leak\n");
    int *arr2 = NULL;
    {
        int y = 42;
        arr2 = &y;
    }
    printf("%d\n", *arr2);

    printf("case 3: return dangling pointer\n");
    int *arr3 = get_arr();
    printf("%d\n", *arr3);

    return 0;
}