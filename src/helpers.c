#include <stdio.h>

extern int fnread()
{
    int n = 0;
    scanf("%d", &n);
    return n;
}

extern void fnwrite(int n)
{
    printf("%d\n", n);
}