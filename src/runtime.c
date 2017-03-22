#include <stdio.h>

int fnread()
{
    int n = 0;
    scanf("%d", &n);
    printf("> %d\n", n);
    return n;
}

void fnwrite(int n)
{
    printf("< %d\n", n);
}