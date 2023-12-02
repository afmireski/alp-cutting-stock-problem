#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int *generateRandomV(int len, int seed, int max)
{
    int *v = (int *)malloc(sizeof(int) * len);

    srand(seed);
    for (int i = 0; i < len; i++)
    {
        int element = rand() % max;
        v[i] = element;
    }

    return v;
}

void printV(int *v, int len)
{
    printf("[");
    for (int i = 0; i < len; i++)
    {
        printf("%d, ", v[i]);
    }
    printf("\b\b]\n");
}

int main(int argc, char **argv)
{
    if (argc > 2)
    {
        int len = atoi(argv[1]);
        int seed = 3;
        int max = len > 10000 ? 10000 : len;
        const char *filename = argv[2];

        char path1[100] = "./haskell/";
        char path2[100] = "./javascript/";
        
        strcat(path2, filename);
        strcat(path1, filename);

        int *v = generateRandomV(len, seed, max);
        // printV(v, len);

        // OUTPUT
        FILE *output1 = fopen(path1, "w"); // OPEN A DATA FILE
        FILE *output2 = fopen(path2, "w"); // OPEN A DATA FILE

        if (output1 == NULL || output2 == NULL)
        {
            printf("File not found!\n");

            return 0;
        }

        fprintf(output2, "[");
        for (int i = 0; i < len; i++)
        {

            fprintf(output2, i < len-1 ? "%d," : "%d]", v[i]);
            fprintf(output1, i < len-1 ? "%d " : "%d", v[i]);
        }

        fclose(output1); // CLOSE FILE
        fclose(output2); // CLOSE FILE

        free(v);
    }

    return 0;
}