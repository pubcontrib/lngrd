#define LNGRD_IMPLEMENTATION
#include "lngrd.h"
#include <stdio.h>
#include <stdlib.h>

static void run_help(void);
static void run_version(void);

int main(int argc, char *argv[])
{
    int i;
    char mode;

    mode = ' ';

    for (i = 1; i < argc; i++)
    {
        char *arg;

        arg = argv[i];

        if (arg[0] == '\0' || arg[0] != '-')
        {
            break;
        }

        arg++;

        if (arg[0] == '-')
        {
            i++;
            break;
        }

        for (; arg[0] != '\0'; arg++)
        {
            switch (arg[0])
            {
                case 'h':
                case 'v':
                    mode = arg[0];
                    break;

                default:
                    fprintf(stderr, "alien option -%c\n", arg[0]);
                    exit(1);
                    break;
            }
        }
    }

    switch (mode)
    {
        case 'h':
            run_help();
            break;

        case 'v':
            run_version();
            break;

        default:
            run_help();
            exit(1);
            break;
    }

    return 0;
}

static void run_help(void)
{
    printf("Usage:\n");
    printf("  lngrd\n");
    printf("\n");
    printf("Options:\n");
    printf("  -h  Show help.\n");
    printf("  -v  Show version.\n");
}

static void run_version(void)
{
    printf("%s\n", LNGRD_VERSION);
}
