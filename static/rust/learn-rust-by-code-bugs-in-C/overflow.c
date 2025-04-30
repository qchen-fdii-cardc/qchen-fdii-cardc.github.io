#include <stdio.h>

int main() {
    int* a = (int*)malloc(sizeof(int) * 3);
    int* b = (int*)malloc(sizeof(int) * 3);
    *a = 1;
    *b = 2;
    *(a + 1) = 3;
    *(b + 1) = 4;
    *(a + 2) = 5;
    *(b + 2) = 6;
    *(a + 3) = 7;
    *(b + 3) = 8;
    
    for (int i = 0; i < 4; i++) {
        printf("a[%d] = %d\n", i, *(a + i));
        printf("b[%d] = %d\n", i, *(b + i));
    }
    free(a);
    free(b);
    return 0;
}
