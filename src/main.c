#define LNGRD_IMPLEMENTATION
#include "lngrd.h"
#include <stdio.h>
#include <stdlib.h>

static void run_file(const char *file);
static void run_text(const lngrd_String *code);
static void run_help(void);
static void run_version(void);

int main(int argc, char *argv[])
{
    int i;
    char mode;

    mode = 'f';

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
                case 'f':
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
        case 'f':
        {
            if (i < argc)
            {
                char *file;

                file = argv[i];

                run_file(file);
            }
            else
            {
                fprintf(stderr, "missing required script argument\n");
                exit(1);
            }

            break;
        }

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

static void run_file(const char *file)
{
    lngrd_String code;
    FILE *handle;
    char *bytes;
    size_t length;
    long end;

    if (!lngrd_check_support())
    {
        fprintf(stderr, "unsupported environment\n");
        exit(1);
    }

    handle = fopen(file, "rb");

    if (!handle)
    {
        fprintf(stderr, "missing script file\n");
        exit(1);
    }

    fseek(handle, 0, SEEK_END);
    end = ftell(handle);
    fseek(handle, 0, SEEK_SET);

    if (end < 0)
    {
        fclose(handle);

        fprintf(stderr, "script file read failed\n");
        exit(1);
    }

    length = (size_t) end;

    if (length > 0)
    {
        size_t read;

        bytes = (char *) allocate(length, sizeof(char));
        read = fread(bytes, 1, length, handle);

        if (ferror(handle) || read != length)
        {
            free(bytes);
            fclose(handle);

            fprintf(stderr, "script file read failed\n");
            exit(1);
        }
    }
    else
    {
        bytes = NULL;
    }

    fclose(handle);
    code.bytes = bytes;
    code.length = length;

    run_text(&code);

    if (bytes)
    {
        free(bytes);
    }
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
    printf("  lngrd script\n");
    printf("  lngrd -f script\n");
    printf("  lngrd -t script\n");
    printf("\n");
    printf("Options:\n");
    printf("  -f  Set program to file mode. Default mode.\n");
    printf("  -t  Set program to text mode.\n");
    printf("  -h  Show help.\n");
    printf("  -v  Show version.\n");
}

static void run_version(void)
{
    printf("%s\n", LNGRD_VERSION);
}
