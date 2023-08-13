#define LNGRD_IMPLEMENTATION
#include "lngrd.h"
#include <stdio.h>
#include <stdlib.h>

static void run_text(const lngrd_String *code);
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
                case 't':
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
        case 't':
        {
            if (i < argc)
            {
                lngrd_String code;

                code.bytes = argv[i];
                code.length = strlen(argv[i]);

                run_text(&code);
            }
            else
            {
                fprintf(stderr, "missing required script argument\n");
                exit(1);
            }

            break;
        }

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

static void run_text(const lngrd_String *code)
{
    lngrd_Executer executer;
    lngrd_Parser parser;
    lngrd_Lexer lexer;

    if (!lngrd_check_support())
    {
        fprintf(stderr, "unsupported environment\n");
        exit(1);
    }

    lngrd_start_executer(&executer);

    lngrd_start_lexer(&lexer, code);
    lngrd_start_parser(&parser, &lexer);
    lngrd_progress_executer(&executer, &parser);
    lngrd_stop_parser(&parser);

    lngrd_stop_executer(&executer);

    if (executer.errored)
    {
        exit(1);
    }
}

static void run_help(void)
{
    printf("Usage:\n");
    printf("  lngrd -t script\n");
    printf("\n");
    printf("Options:\n");
    printf("  -t  Set program to text mode.\n");
    printf("  -h  Show help.\n");
    printf("  -v  Show version.\n");
}

static void run_version(void)
{
    printf("%s\n", LNGRD_VERSION);
}
