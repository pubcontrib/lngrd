#ifndef LNGRD_H
#define LNGRD_H

/*
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#ifndef LNGRD_API
#define LNGRD_API
#endif

/*
 * PUBLIC API
 */

/*semantic version of the library API and implementation*/
#define LNGRD_VERSION "v0.12.0"

#define LNGRD_INT_LIMIT 2147483647L
#define LNGRD_SIZE_LIMIT ((size_t) -1)
#if INT_MIN > -LNGRD_INT_LIMIT || INT_MAX < LNGRD_INT_LIMIT
typedef signed long lngrd_SInt; /*32-bit signed integer*/
typedef unsigned long lngrd_UInt; /*32-bit unsigned integer*/
#else
typedef signed int lngrd_SInt; /*32-bit signed integer*/
typedef unsigned int lngrd_UInt; /*32-bit unsigned integer*/
#endif

/*classifier for a piece of generic data*/
typedef enum
{
    LNGRD_BLOCK_TYPE_NUMBER = 0x01,
    LNGRD_BLOCK_TYPE_STRING = 0x02,
    LNGRD_BLOCK_TYPE_LIST = 0x04,
    LNGRD_BLOCK_TYPE_MAP = 0x08,
    LNGRD_BLOCK_TYPE_FUNCTION = 0x10,
    LNGRD_BLOCK_TYPE_EXPRESSION = 0x20
} lngrd_BlockType;

/*reference counted generic data container*/
typedef struct
{
    lngrd_BlockType type;
    void *data;
    size_t references;
} lngrd_Block;

/*bit layout for a signed fixed-point number*/
typedef enum
{
    LNGRD_NUMBER_LAYOUT_32_0 /*32.0*/
} lngrd_NumberLayout;

/*signed fixed-point number*/
typedef struct
{
    lngrd_NumberLayout layout;
    lngrd_SInt value;
} lngrd_Number;

/*length terminated ASCII string*/
typedef struct
{
    char *bytes;
    size_t length;
} lngrd_String;

/*block dynamic array*/
typedef struct
{
    lngrd_Block **items;
    size_t length;
    size_t capacity;
} lngrd_List;

/*lifecycle phase of block key-value pair*/
typedef enum
{
    LNGRD_PAIR_PHASE_UNCLAIMED,
    LNGRD_PAIR_PHASE_OCCUPIED,
    LNGRD_PAIR_PHASE_VACANT
} lngrd_PairPhase;

/*block key-value pair*/
typedef struct
{
    lngrd_Block *key;
    lngrd_Block *value;
    lngrd_PairPhase phase;
} lngrd_Pair;

/*block associated array*/
typedef struct
{
    lngrd_Pair *items;
    size_t length;
    size_t capacity;
} lngrd_Map;

/*expression sequence*/
typedef struct
{
    lngrd_List *expressions;
    lngrd_String *source;
    lngrd_SInt inlined;
} lngrd_Function;

/*classifier of abstract syntax tree node*/
typedef enum
{
    LNGRD_EXPRESSION_TYPE_LITERAL,
    LNGRD_EXPRESSION_TYPE_LOOKUP,
    LNGRD_EXPRESSION_TYPE_ASSIGN,
    LNGRD_EXPRESSION_TYPE_UNASSIGN,
    LNGRD_EXPRESSION_TYPE_INVOKE,
    LNGRD_EXPRESSION_TYPE_BRANCH,
    LNGRD_EXPRESSION_TYPE_LOOP,
    LNGRD_EXPRESSION_TYPE_CATCH,
    LNGRD_EXPRESSION_TYPE_THROW,
    LNGRD_EXPRESSION_TYPE_ARGUMENT,
    LNGRD_EXPRESSION_TYPE_GROUP,
    LNGRD_EXPRESSION_TYPE_NATIVE
} lngrd_ExpressionType;

/*abstract syntax tree node*/
typedef struct
{
    lngrd_ExpressionType type;
    void *form;
} lngrd_Expression;

/*classifier for a lex token*/
typedef enum
{
    LNGRD_TOKEN_TYPE_UNKNOWN,
    LNGRD_TOKEN_TYPE_WHITESPACE,
    LNGRD_TOKEN_TYPE_COMMENT,
    LNGRD_TOKEN_TYPE_NUMBER,
    LNGRD_TOKEN_TYPE_STRING,
    LNGRD_TOKEN_TYPE_IDENTIFIABLE,
    LNGRD_TOKEN_TYPE_KEYSYMBOL,
    LNGRD_TOKEN_TYPE_KEYWORD
} lngrd_TokenType;

/*lex token*/
typedef struct
{
    lngrd_TokenType type;
    size_t start;
    size_t end;
} lngrd_Token;

/*lexer state machine*/
typedef struct
{
    const lngrd_String *code;
    lngrd_SInt closed;
    lngrd_SInt errored;
    lngrd_Token token;
} lngrd_Lexer;

/*parser state machine*/
typedef struct
{
    lngrd_Lexer *lexer;
    lngrd_List *pyre;
    lngrd_SInt closed;
    lngrd_SInt errored;
    lngrd_Block *expression;
    lngrd_List *stack;
} lngrd_Parser;

/*memory claim of expression execution iterator internals*/
typedef enum
{
    LNGRD_ACTION_OWN_ITEMS = 0x01,
    LNGRD_ACTION_OWN_LIST = 0x02
} lngrd_ActionOwn;

/*expression execution iterator*/
typedef struct
{
    lngrd_Block *single;
    lngrd_List *expressions;
    size_t index;
    lngrd_UInt ownership;
    lngrd_UInt phase;
    lngrd_UInt checkpoint;
    lngrd_UInt capacity;
    lngrd_SInt provisioned;
} lngrd_Action;

/*action dynamic array*/
typedef struct
{
    lngrd_Action *actions;
    size_t length;
    size_t capacity;
} lngrd_Plan;

/*executer state machine*/
typedef struct
{
    lngrd_List *pyre;
    lngrd_SInt errored;
    lngrd_Block *result;
    lngrd_Map *globals;
    lngrd_List *locals;
    lngrd_Plan *plan;
    lngrd_List *arguments;
    lngrd_List *sketches;
    lngrd_List *doodles;
} lngrd_Executer;

/*literal expression form*/
typedef struct
{
    lngrd_Block *block;
} lngrd_LiteralForm;

/*classifier of identifier domain*/
typedef enum
{
    LNGRD_SCOPE_TYPE_LOCAL,
    LNGRD_SCOPE_TYPE_GLOBAL
} lngrd_ScopeType;

/*assign expression form*/
typedef struct
{
    lngrd_ScopeType scope;
    lngrd_Block *name;
} lngrd_AssignForm;

/*unassign expression form*/
typedef struct
{
    lngrd_ScopeType scope;
    lngrd_Block *name;
} lngrd_UnassignForm;

/*lookup expression form*/
typedef struct
{
    lngrd_ScopeType scope;
    lngrd_Block *name;
} lngrd_LookupForm;

/*invoke expression form*/
typedef struct
{
    lngrd_List *arguments;
} lngrd_InvokeForm;

/*branch expression form*/
typedef struct
{
    lngrd_Block *test;
    lngrd_Block *pass;
    lngrd_Block *fail;
} lngrd_BranchForm;

/*loop expression form*/
typedef struct
{
    lngrd_Block *test;
    lngrd_Block *body;
} lngrd_LoopForm;

/*catch expression form*/
typedef struct
{
    lngrd_Block *failable;
} lngrd_CatchForm;

/*throw expression form*/
typedef struct
{
    lngrd_Block *error;
} lngrd_ThrowForm;

/*argument expression form*/
typedef struct
{
    lngrd_Block *index;
} lngrd_ArgumentForm;

/*group expression form*/
typedef struct
{
    lngrd_List *expressions;
} lngrd_GroupForm;

/*native expression form*/
typedef struct
{
    void (*work)(lngrd_Executer *);
} lngrd_NativeForm;

/*dynamic length terminated ASCII string*/
typedef struct
{
    char *bytes;
    size_t length;
    size_t capacity;
} lngrd_Buffer;

/*checks if the run-time environment meets minimum requirements*/
LNGRD_API int lngrd_check_support(void);
/*initializes a lexer*/
LNGRD_API void lngrd_start_lexer(lngrd_Lexer *lexer, const lngrd_String *code);
/*moves a lexer one step forward on its path*/
LNGRD_API void lngrd_progress_lexer(lngrd_Lexer *lexer);
/*initializes a parser*/
LNGRD_API void lngrd_start_parser(lngrd_Parser *parser, lngrd_Lexer *lexer, lngrd_List *pyre);
/*moves a parser one step forward on its path*/
LNGRD_API void lngrd_progress_parser(lngrd_Parser *parser);
/*cleans up resources owned by a parser*/
LNGRD_API void lngrd_stop_parser(lngrd_Parser *parser);
/*initializes an executer*/
LNGRD_API void lngrd_start_executer(lngrd_Executer *executer, lngrd_List *pyre);
/*moves an executer forward with a given plan*/
LNGRD_API void lngrd_progress_executer(lngrd_Executer *executer, lngrd_Parser *parser);
/*cleans up resources owned by an executer*/
LNGRD_API void lngrd_stop_executer(lngrd_Executer *executer);

#ifdef LNGRD_IMPLEMENTATION

/*
 * IMPLEMENTATION
 */

static void read_whitespace_token(lngrd_Lexer *lexer);
static void read_comment_token(lngrd_Lexer *lexer);
static void read_number_token(lngrd_Lexer *lexer);
static void read_string_token(lngrd_Lexer *lexer);
static void read_identifiable_token(lngrd_Lexer *lexer);
static void read_keyword_token(lngrd_Lexer *lexer);
static int has_another_symbol(const lngrd_Lexer *lexer);
static char read_next_symbol(lngrd_Lexer *lexer);
static char peek_next_symbol(const lngrd_Lexer *lexer);
static int is_whitespace_symbol(char symbol);
static int is_number_symbol(char symbol);
static int is_string_symbol(char symbol);
static int is_scope_symbol(char symbol);
static int is_letter_symbol(char symbol);
static int is_shorthand_symbol(char symbol);
static int is_keyword_symbol(char symbol);
static int parse_identifier(const lngrd_String *string, lngrd_String **result);
static int unescape_string(const lngrd_String *string, lngrd_String **result);
static int escape_string(const lngrd_String *string, lngrd_String **result);
static void do_add_work(lngrd_Executer *executer);
static void do_subtract_work(lngrd_Executer *executer);
static void do_multiply_work(lngrd_Executer *executer);
static void do_divide_work(lngrd_Executer *executer);
static void do_modulo_work(lngrd_Executer *executer);
static void do_increment_work(lngrd_Executer *executer);
static void do_decrement_work(lngrd_Executer *executer);
static void do_and_work(lngrd_Executer *executer);
static void do_or_work(lngrd_Executer *executer);
static void do_not_work(lngrd_Executer *executer);
static void do_precedes_work(lngrd_Executer *executer);
static void do_succeeds_work(lngrd_Executer *executer);
static void do_equals_work(lngrd_Executer *executer);
static void do_get_work(lngrd_Executer *executer);
static void do_set_work(lngrd_Executer *executer);
static void do_unset_work(lngrd_Executer *executer);
static void do_measure_work(lngrd_Executer *executer);
static void do_slice_work(lngrd_Executer *executer);
static void do_merge_work(lngrd_Executer *executer);
static void do_sort_work(lngrd_Executer *executer);
static void do_read_work(lngrd_Executer *executer);
static void do_write_work(lngrd_Executer *executer);
static void do_delete_work(lngrd_Executer *executer);
static void do_query_work(lngrd_Executer *executer);
static void do_exit_work(lngrd_Executer *executer);
static void do_serialize_work(lngrd_Executer *executer);
static void do_deserialize_work(lngrd_Executer *executer);
static void do_classify_work(lngrd_Executer *executer);
static void do_evaluate_work(lngrd_Executer *executer);
static void set_global_function(const char *name, const char *source, void (*work)(lngrd_Executer *), lngrd_Executer *executer);
static void set_executer_error(const char *message, lngrd_Executer *executer);
static void set_executor_result(lngrd_Block *result, lngrd_Executer *executer);
static int require_argument(lngrd_UInt index, lngrd_UInt types, lngrd_Executer *executer, lngrd_Block **result);
static lngrd_Block *create_block(lngrd_BlockType type, void *data, size_t references);
static lngrd_UInt hash_block(const lngrd_Block *block);
static lngrd_SInt compare_blocks(const lngrd_Block *x, const lngrd_Block *y, lngrd_List *sketches, lngrd_List *doodles);
static int is_block_truthy(const lngrd_Block *block);
static void burn_pyre(lngrd_List *pyre);
static lngrd_Number *create_number(lngrd_NumberLayout layout, lngrd_SInt value);
static int string_to_number(const lngrd_String *string, lngrd_Number **result);
static int number_to_string(const lngrd_Number *number, lngrd_String **result);
static lngrd_SInt compare_numbers(const lngrd_Number *left, const lngrd_Number *right);
static lngrd_UInt hash_number(const lngrd_Number *number);
static void destroy_number(lngrd_Number *number);
static lngrd_String *create_string(char *bytes, size_t length);
static lngrd_String *cstring_to_string(const char *cstring);
static lngrd_String *bytes_to_string(const char *bytes, size_t length);
static char *string_to_cstring(const lngrd_String *string);
static lngrd_SInt compare_strings(const lngrd_String *left, const lngrd_String *right);
static lngrd_UInt hash_string(const lngrd_String *string);
static int is_keyword_match(const lngrd_String *string, const char *keyword);
static void destroy_string(lngrd_String *string);
static lngrd_List *create_list(void);
static void push_list_item(lngrd_List *list, lngrd_Block *item);
static lngrd_Block *pop_list_item(lngrd_List *list);
static lngrd_Block *peek_list_item(lngrd_List *list);
static lngrd_List *copy_list(lngrd_List *list);
static void sort_list(lngrd_List *list, lngrd_List *sketches, lngrd_List *doodles);
static void burn_list(lngrd_List *list, lngrd_List *pyre);
static lngrd_Map *create_map(void);
static lngrd_Block *get_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_List *sketches, lngrd_List *doodles);
static void set_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_Block *block, lngrd_List *pyre, lngrd_List *sketches, lngrd_List *doodles);
static void unset_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_List *pyre, lngrd_List *sketches, lngrd_List *doodles);
static void burn_map(lngrd_Map *map, lngrd_List *pyre);
static lngrd_Function *create_function(void);
static lngrd_SInt compare_functions(const lngrd_Function *left, const lngrd_Function *right);
static void burn_function(lngrd_Function *function, lngrd_List *pyre);
static lngrd_Expression *create_expression(lngrd_ExpressionType type, void *form);
static void burn_expression(lngrd_Expression *expression, lngrd_List *pyre);
static lngrd_Plan *create_plan(void);
static void push_plan_action(lngrd_Plan *plan, lngrd_Action action);
static lngrd_Action pop_plan_action(lngrd_Plan *plan);
static lngrd_Action peek_plan_action(lngrd_Plan *plan);
static void destroy_plan(lngrd_Plan *plan);
static lngrd_Action prepare_simple_action(lngrd_Block *single, lngrd_UInt checkpoint, lngrd_UInt capacity);
static lngrd_Action prepare_general_action(lngrd_List *expressions, lngrd_UInt ownership, lngrd_UInt checkpoint, lngrd_UInt capacity);
static void start_buffer(lngrd_Buffer *buffer);
static int append_buffer_bytes(lngrd_Buffer *buffer, const lngrd_String *string);
static lngrd_String *buffer_to_string(lngrd_Buffer *buffer);
static void abandon_buffer(lngrd_Buffer *buffer);
static int can_fit_both(size_t left, size_t right);
static void *allocate(size_t number, size_t size);
static void *reallocate(void *memory, size_t number, size_t size);
static void crash_with_message(const char *message);

LNGRD_API int lngrd_check_support(void)
{
    /*ensure a maximum object size of at least 32-bits*/
    if (LNGRD_SIZE_LIMIT < LNGRD_INT_LIMIT)
    {
        return 0;
    }

    /*ensure two's complement*/
    if ((-1 & 3) != 3)
    {
        return 0;
    }

    return 1;
}

LNGRD_API void lngrd_start_lexer(lngrd_Lexer *lexer, const lngrd_String *code)
{
    lexer->code = code;
    lexer->closed = 0;
    lexer->errored = 0;
    lexer->token.type = LNGRD_TOKEN_TYPE_UNKNOWN;
    lexer->token.start = 0;
    lexer->token.end = 0;
}

LNGRD_API void lngrd_progress_lexer(lngrd_Lexer *lexer)
{
    char symbol;

    if (lexer->closed || lexer->errored)
    {
        return;
    }

    if (lexer->code->length > LNGRD_INT_LIMIT)
    {
        lexer->errored = 1;
        return;
    }

    lexer->token.type = LNGRD_TOKEN_TYPE_UNKNOWN;
    lexer->token.start = lexer->token.end;

    if (!has_another_symbol(lexer))
    {
        lexer->closed = 1;
        return;
    }

    symbol = read_next_symbol(lexer);

    if (is_whitespace_symbol(symbol))
    {
        read_whitespace_token(lexer);
    }
    else if (symbol == '(' || symbol == ')' || symbol == '\\' || symbol == '/' || symbol == ',' || symbol == '<' || symbol == '>' || symbol == '[' || symbol == ']')
    {
        lexer->token.type = LNGRD_TOKEN_TYPE_KEYSYMBOL;
    }
    else if (is_keyword_symbol(symbol))
    {
        read_keyword_token(lexer);
    }
    else if (is_scope_symbol(symbol))
    {
        read_identifiable_token(lexer);
    }
    else if ((symbol == '|' || symbol == '%') && has_another_symbol(lexer) && is_scope_symbol(peek_next_symbol(lexer)))
    {
        read_next_symbol(lexer);
        read_identifiable_token(lexer);
    }
    else if (is_number_symbol(symbol))
    {
        read_number_token(lexer);
    }
    else if ((symbol == '-' || symbol == '+') && has_another_symbol(lexer) && is_number_symbol(peek_next_symbol(lexer)))
    {
        read_next_symbol(lexer);
        read_number_token(lexer);
    }
    else if (symbol == '"')
    {
        read_string_token(lexer);
    }
    else if (symbol == '#')
    {
        read_comment_token(lexer);
    }
    else
    {
        lexer->errored = 1;
    }
}

LNGRD_API void lngrd_start_parser(lngrd_Parser *parser, lngrd_Lexer *lexer, lngrd_List *pyre)
{
    parser->lexer = lexer;
    parser->pyre = pyre;
    parser->closed = 0;
    parser->errored = 0;
    parser->expression = NULL;
    parser->stack = create_list();
}

LNGRD_API void lngrd_progress_parser(lngrd_Parser *parser)
{
    lngrd_Lexer *lexer;
    lngrd_List *stack;
    lngrd_Block *offer;
    int terminate;

    if (parser->closed || parser->errored)
    {
        return;
    }

    parser->expression = NULL;
    lexer = parser->lexer;
    stack = parser->stack;
    offer = NULL;
    terminate = 0;

    while (!lexer->closed && !lexer->errored)
    {
        if (terminate)
        {
            offer = pop_list_item(stack);
            terminate = 0;
        }
        else if (stack->length > 0 && offer)
        {
            lngrd_Expression *pending;
            int completed;

            pending = (lngrd_Expression *) peek_list_item(stack)->data;
            completed = 0;

            if (pending->type == LNGRD_EXPRESSION_TYPE_LITERAL)
            {
                lngrd_LiteralForm *form;
                lngrd_Block *block;
                lngrd_BlockType type;

                form = (lngrd_LiteralForm *) pending->form;
                block = form->block;
                type = block->type;

                if (type == LNGRD_BLOCK_TYPE_NUMBER || type == LNGRD_BLOCK_TYPE_STRING)
                {
                    completed = 1;
                }
                else if (type == LNGRD_BLOCK_TYPE_LIST)
                {
                    lngrd_Expression *expression;
                    lngrd_LiteralForm *form;
                    lngrd_List *list;

                    expression = (lngrd_Expression *) offer->data;

                    if (expression->type != LNGRD_EXPRESSION_TYPE_LITERAL)
                    {
                        break;
                    }

                    form = (lngrd_LiteralForm *) expression->form;
                    list = (lngrd_List *) block->data;
                    push_list_item(list, form->block);

                    offer->data = NULL;
                    push_list_item(parser->pyre, offer);
                    offer = NULL;
                    free(expression->form);
                    free(expression);
                }
                else if (type == LNGRD_BLOCK_TYPE_FUNCTION)
                {
                    lngrd_Function *function;

                    function = (lngrd_Function *) block->data;
                    push_list_item(function->expressions, offer);
                }
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_LOOKUP || pending->type == LNGRD_EXPRESSION_TYPE_ASSIGN || pending->type == LNGRD_EXPRESSION_TYPE_UNASSIGN)
            {
                completed = 1;
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_INVOKE)
            {
                lngrd_InvokeForm *form;

                form = (lngrd_InvokeForm *) pending->form;
                push_list_item(form->arguments, offer);
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_BRANCH)
            {
                lngrd_BranchForm *form;

                form = (lngrd_BranchForm *) pending->form;

                if (!form->test)
                {
                    form->test = offer;
                }
                else if (!form->pass)
                {
                    lngrd_Token checkpoint;
                    int marked;

                    form->pass = offer;
                    checkpoint.type = lexer->token.type;
                    checkpoint.start = lexer->token.start;
                    checkpoint.end = lexer->token.end;
                    marked = 0;

                    while (!lexer->closed && !lexer->errored)
                    {
                        lngrd_progress_lexer(lexer);

                        if (!lexer->closed && !lexer->errored)
                        {
                            lngrd_Token token;
                            lngrd_TokenType tokenType;

                            token = lexer->token;
                            tokenType = token.type;

                            if (tokenType == LNGRD_TOKEN_TYPE_UNKNOWN)
                            {
                                parser->errored = 1;
                                return;
                            }

                            if (tokenType == LNGRD_TOKEN_TYPE_WHITESPACE || tokenType == LNGRD_TOKEN_TYPE_COMMENT)
                            {
                                continue;
                            }

                            if (tokenType == LNGRD_TOKEN_TYPE_KEYWORD)
                            {
                                lngrd_String tokenValue;

                                tokenValue.bytes = lexer->code->bytes + token.start;
                                tokenValue.length = token.end - token.start;

                                if (is_keyword_match(&tokenValue, "else"))
                                {
                                    if (marked)
                                    {
                                        parser->errored = 1;
                                        return;
                                    }

                                    checkpoint.type = lexer->token.type;
                                    checkpoint.start = lexer->token.start;
                                    checkpoint.end = lexer->token.end;
                                    marked = 1;
                                    continue;
                                }
                            }

                            lexer->token.type = checkpoint.type;
                            lexer->token.start = checkpoint.start;
                            lexer->token.end = checkpoint.end;
                            break;
                        }
                    }

                    if (marked)
                    {
                        if (lexer->closed)
                        {
                            parser->errored = 1;
                            return;
                        }
                    }
                    else
                    {
                        lexer->closed = 0;
                        completed = 1;
                    }
                }
                else if (!form->fail)
                {
                    form->fail = offer;
                    completed = 1;
                }
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_LOOP)
            {
                lngrd_LoopForm *form;

                form = (lngrd_LoopForm *) pending->form;

                if (!form->test)
                {
                    form->test = offer;
                }
                else
                {
                    form->body = offer;
                    completed = 1;
                }
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_CATCH)
            {
                lngrd_CatchForm *form;

                form = (lngrd_CatchForm *) pending->form;
                form->failable = offer;
                completed = 1;
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_THROW)
            {
                lngrd_ThrowForm *form;

                form = (lngrd_ThrowForm *) pending->form;
                form->error = offer;
                completed = 1;
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_ARGUMENT)
            {
                lngrd_ArgumentForm *form;

                form = (lngrd_ArgumentForm *) pending->form;
                form->index = offer;
                completed = 1;
            }
            else if (pending->type == LNGRD_EXPRESSION_TYPE_GROUP)
            {
                lngrd_GroupForm *form;

                form = (lngrd_GroupForm *) pending->form;
                push_list_item(form->expressions, offer);
            }

            if (completed)
            {
                offer = pop_list_item(stack);
            }
            else
            {
                offer = NULL;
            }
        }
        else if (offer)
        {
            parser->expression = offer;
            offer = NULL;
            return;
        }
        else
        {
            lngrd_progress_lexer(lexer);

            if (!lexer->closed && !lexer->errored)
            {
                lngrd_Token token;
                lngrd_TokenType tokenType;
                lngrd_String tokenValue;

                token = lexer->token;
                tokenType = token.type;
                tokenValue.bytes = lexer->code->bytes + token.start;
                tokenValue.length = token.end - token.start;

                if (tokenType == LNGRD_TOKEN_TYPE_UNKNOWN)
                {
                    parser->errored = 1;
                    return;
                }

                if (tokenType == LNGRD_TOKEN_TYPE_WHITESPACE || tokenType == LNGRD_TOKEN_TYPE_COMMENT)
                {
                    continue;
                }

                if (tokenType == LNGRD_TOKEN_TYPE_NUMBER)
                {
                    lngrd_Expression *expression;
                    lngrd_LiteralForm *form;
                    lngrd_Block *block;
                    lngrd_Number *number;

                    if (!string_to_number(&tokenValue, &number))
                    {
                        parser->errored = 1;
                        return;
                    }

                    block = create_block(LNGRD_BLOCK_TYPE_NUMBER, number, 1);

                    form = (lngrd_LiteralForm *) allocate(1, sizeof(lngrd_LiteralForm));
                    form->block = block;

                    expression = create_expression(LNGRD_EXPRESSION_TYPE_LITERAL, form);

                    offer = create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1);
                }
                else if (tokenType == LNGRD_TOKEN_TYPE_STRING)
                {
                    lngrd_Expression *expression;
                    lngrd_LiteralForm *form;
                    lngrd_Block *block;
                    lngrd_String view, *string;

                    view.bytes = lexer->code->bytes + token.start + 1;
                    view.length = token.end - token.start - 2;

                    if (!unescape_string(&view, &string))
                    {
                        parser->errored = 1;
                        return;
                    }

                    block = create_block(LNGRD_BLOCK_TYPE_STRING, string, 1);

                    form = (lngrd_LiteralForm *) allocate(1, sizeof(lngrd_LiteralForm));
                    form->block = block;

                    expression = create_expression(LNGRD_EXPRESSION_TYPE_LITERAL, form);

                    offer = create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1);
                }
                else if (tokenType == LNGRD_TOKEN_TYPE_IDENTIFIABLE)
                {
                    lngrd_Expression *expression;

                    if (tokenValue.bytes[0] == '|')
                    {
                        lngrd_AssignForm *form;
                        lngrd_String *name;
                        lngrd_String view;

                        if (tokenValue.bytes[1] == '$' || tokenValue.bytes[1] == '@')
                        {
                            view.bytes = lexer->code->bytes + token.start + 1;
                            view.length = token.end - token.start - 1;

                            if (!parse_identifier(&view, &name))
                            {
                                parser->errored = 1;
                                return;
                            }

                            form = (lngrd_AssignForm *) allocate(1, sizeof(lngrd_AssignForm));
                            form->scope = tokenValue.bytes[1] == '$' ? LNGRD_SCOPE_TYPE_LOCAL : LNGRD_SCOPE_TYPE_GLOBAL;
                            form->name = create_block(LNGRD_BLOCK_TYPE_STRING, name, 1);

                            expression = create_expression(LNGRD_EXPRESSION_TYPE_ASSIGN, form);
                        }
                        else
                        {
                            parser->errored = 1;
                            return;
                        }
                    }
                    else if (tokenValue.bytes[0] == '%')
                    {
                        lngrd_UnassignForm *form;
                        lngrd_String *name;
                        lngrd_String view;

                        if (tokenValue.bytes[1] == '$' || tokenValue.bytes[1] == '@')
                        {
                            view.bytes = lexer->code->bytes + token.start + 1;
                            view.length = token.end - token.start - 1;

                            if (!parse_identifier(&view, &name))
                            {
                                parser->errored = 1;
                                return;
                            }

                            form = (lngrd_UnassignForm *) allocate(1, sizeof(lngrd_UnassignForm));
                            form->scope = tokenValue.bytes[1] == '$' ? LNGRD_SCOPE_TYPE_LOCAL : LNGRD_SCOPE_TYPE_GLOBAL;
                            form->name = create_block(LNGRD_BLOCK_TYPE_STRING, name, 1);

                            expression = create_expression(LNGRD_EXPRESSION_TYPE_UNASSIGN, form);
                        }
                        else
                        {
                            parser->errored = 1;
                            return;
                        }
                    }
                    else
                    {
                        lngrd_LookupForm *form;
                        lngrd_String *name;
                        lngrd_String view;

                        if (tokenValue.bytes[0] == '$' || tokenValue.bytes[0] == '@')
                        {
                            view.bytes = lexer->code->bytes + token.start;
                            view.length = token.end - token.start;

                            if (!parse_identifier(&view, &name))
                            {
                                parser->errored = 1;
                                return;
                            }

                            form = (lngrd_LookupForm *) allocate(1, sizeof(lngrd_LookupForm));
                            form->scope = tokenValue.bytes[0] == '$' ? LNGRD_SCOPE_TYPE_LOCAL : LNGRD_SCOPE_TYPE_GLOBAL;
                            form->name = create_block(LNGRD_BLOCK_TYPE_STRING, name, 1);

                            expression = create_expression(LNGRD_EXPRESSION_TYPE_LOOKUP, form);
                        }
                        else
                        {
                            parser->errored = 1;
                            return;
                        }
                    }

                    offer = create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1);
                }
                else if (tokenType == LNGRD_TOKEN_TYPE_KEYWORD)
                {
                    lngrd_Expression *expression;

                    if (is_keyword_match(&tokenValue, "if"))
                    {
                        lngrd_BranchForm *form;

                        form = (lngrd_BranchForm *) allocate(1, sizeof(lngrd_BranchForm));
                        form->test = NULL;
                        form->pass = NULL;
                        form->fail = NULL;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_BRANCH, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));

                        continue;
                    }
                    else if (is_keyword_match(&tokenValue, "while"))
                    {
                        lngrd_LoopForm *form;

                        form = (lngrd_LoopForm *) allocate(1, sizeof(lngrd_LookupForm));
                        form->test = NULL;
                        form->body = NULL;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_LOOP, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));

                        continue;
                    }
                    else if (is_keyword_match(&tokenValue, "catch"))
                    {
                        lngrd_CatchForm *form;

                        form = (lngrd_CatchForm *) allocate(1, sizeof(lngrd_CatchForm));
                        form->failable = NULL;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_CATCH, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));

                        continue;
                    }
                    else if (is_keyword_match(&tokenValue, "throw"))
                    {
                        lngrd_ThrowForm *form;

                        form = (lngrd_ThrowForm *) allocate(1, sizeof(lngrd_ThrowForm));
                        form->error = NULL;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_THROW, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));

                        continue;
                    }
                    else if (is_keyword_match(&tokenValue, "argument"))
                    {
                        lngrd_ArgumentForm *form;

                        form = (lngrd_ArgumentForm *) allocate(1, sizeof(lngrd_ArgumentForm));
                        form->index = NULL;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_ARGUMENT, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));

                        continue;
                    }
                    else
                    {
                        parser->errored = 1;
                        return;
                    }
                }
                else if (tokenType == LNGRD_TOKEN_TYPE_KEYSYMBOL)
                {
                    char symbol;

                    symbol = tokenValue.bytes[0];

                    if (symbol == '(')
                    {
                        lngrd_Expression *expression;
                        lngrd_InvokeForm *form;

                        form = (lngrd_InvokeForm *) allocate(1, sizeof(lngrd_InvokeForm));
                        form->arguments = create_list();

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_INVOKE, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));
                    }
                    else if (symbol == ')')
                    {
                        if (stack->length == 0 || ((lngrd_Expression *) peek_list_item(stack)->data)->type != LNGRD_EXPRESSION_TYPE_INVOKE)
                        {
                            parser->errored = 1;
                            return;
                        }

                        terminate = 1;
                    }
                    else if (symbol == '\\')
                    {
                        lngrd_Expression *expression;
                        lngrd_GroupForm *form;

                        form = (lngrd_GroupForm *) allocate(1, sizeof(lngrd_GroupForm));
                        form->expressions = create_list();

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_GROUP, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));
                    }
                    else if (symbol == '/')
                    {
                        if (stack->length == 0 || ((lngrd_Expression *) peek_list_item(stack)->data)->type != LNGRD_EXPRESSION_TYPE_GROUP)
                        {
                            parser->errored = 1;
                            return;
                        }

                        terminate = 1;
                    }
                    else if (symbol == '<')
                    {
                        lngrd_Expression *expression;
                        lngrd_LiteralForm *form;
                        lngrd_Block *block;
                        lngrd_Function *function;

                        function = create_function();
                        function->source = create_string(NULL, token.start);
                        block = create_block(LNGRD_BLOCK_TYPE_FUNCTION, function, 1);

                        form = (lngrd_LiteralForm *) allocate(1, sizeof(lngrd_LiteralForm));
                        form->block = block;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_LITERAL, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));
                    }
                    else if (symbol == '>')
                    {
                        lngrd_Expression *expression;
                        lngrd_LiteralForm *form;
                        lngrd_Function *function;
                        size_t start, end, length;

                        if (stack->length == 0)
                        {
                            parser->errored = 1;
                            return;
                        }

                        expression = (lngrd_Expression *) peek_list_item(stack)->data;

                        if (expression->type != LNGRD_EXPRESSION_TYPE_LITERAL)
                        {
                            parser->errored = 1;
                            return;
                        }

                        form = (lngrd_LiteralForm *) expression->form;
                        function = (lngrd_Function *) form->block->data;
                        start = function->source->length;
                        end = token.start + 1;
                        length = end - start;
                        function->source->length = length;

                        if (length > 0)
                        {
                            function->source->bytes = (char *) allocate(length, sizeof(char));
                            memcpy(function->source->bytes, lexer->code->bytes + start, length);
                        }

                        terminate = 1;
                    }
                    else if (symbol == '[')
                    {
                        lngrd_Expression *expression;
                        lngrd_LiteralForm *form;
                        lngrd_Block *block;
                        lngrd_List *list;

                        list = create_list();

                        block = create_block(LNGRD_BLOCK_TYPE_LIST, list, 1);

                        form = (lngrd_LiteralForm *) allocate(1, sizeof(lngrd_LiteralForm));
                        form->block = block;

                        expression = create_expression(LNGRD_EXPRESSION_TYPE_LITERAL, form);

                        push_list_item(stack, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));
                    }
                    else if (symbol == ']')
                    {
                        if (stack->length == 0 || ((lngrd_Expression *) peek_list_item(stack)->data)->type != LNGRD_EXPRESSION_TYPE_LITERAL)
                        {
                            parser->errored = 1;
                            return;
                        }

                        terminate = 1;
                    }
                    else if (symbol == ',')
                    {
                        continue;
                    }
                }
            }
        }
    }

    parser->closed = lexer->closed;
    parser->errored = lexer->errored;

    if (stack->length > 0)
    {
        parser->errored = 1;
    }

    if (offer)
    {
        parser->errored = 1;
        push_list_item(parser->pyre, offer);
    }
}

LNGRD_API void lngrd_stop_parser(lngrd_Parser *parser)
{
    burn_list(parser->stack, parser->pyre);

    if (parser->expression)
    {
        push_list_item(parser->pyre, parser->expression);
    }

    burn_pyre(parser->pyre);
}

LNGRD_API void lngrd_start_executer(lngrd_Executer *executer, lngrd_List *pyre)
{
    executer->pyre = pyre;
    executer->errored = 0;
    executer->result = create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 1);
    executer->globals = create_map();
    executer->locals = create_list();
    executer->plan = create_plan();
    executer->arguments = create_list();
    executer->sketches = create_list();
    executer->doodles = create_list();

    set_global_function("add", "<(@add argument 1 argument 2)>", do_add_work, executer);
    set_global_function("subtract", "<(@subtract argument 1 argument 2)>", do_subtract_work, executer);
    set_global_function("multiply", "<(@multiply argument 1 argument 2)>", do_multiply_work, executer);
    set_global_function("divide", "<(@divide argument 1 argument 2)>", do_divide_work, executer);
    set_global_function("modulo", "<(@modulo argument 1 argument 2)>", do_modulo_work, executer);
    set_global_function("increment", "<(@increment argument 1)>", do_increment_work, executer);
    set_global_function("decrement", "<(@decrement argument 1)>", do_decrement_work, executer);
    set_global_function("and", "<(@and argument 1 argument 2)>", do_and_work, executer);
    set_global_function("or", "<(@or argument 1 argument 2)>", do_or_work, executer);
    set_global_function("not", "<(@not argument 1)>", do_not_work, executer);
    set_global_function("precedes", "<(@precedes argument 1 argument 2)>", do_precedes_work, executer);
    set_global_function("succeeds", "<(@succeeds argument 1 argument 2)>", do_succeeds_work, executer);
    set_global_function("equals", "<(@equals argument 1 argument 2)>", do_equals_work, executer);
    set_global_function("get", "<(@get argument 1 argument 2)>", do_get_work, executer);
    set_global_function("set", "<(@set argument 1 argument 2 argument 3)>", do_set_work, executer);
    set_global_function("unset", "<(@unset argument 1 argument 2)>", do_unset_work, executer);
    set_global_function("measure", "<(@measure argument 1)>", do_measure_work, executer);
    set_global_function("slice", "<(@slice argument 1 argument 2 argument 3)>", do_slice_work, executer);
    set_global_function("merge", "<(@merge argument 1 argument 2)>", do_merge_work, executer);
    set_global_function("sort", "<(@sort argument 1)>", do_sort_work, executer);
    set_global_function("read", "<(@read argument 1 argument 2)>", do_read_work, executer);
    set_global_function("write", "<(@write argument 1 argument 2)>", do_write_work, executer);
    set_global_function("delete", "<(@delete argument 1)>", do_delete_work, executer);
    set_global_function("query", "<(@query argument 1)>", do_query_work, executer);
    set_global_function("exit", "<(@exit argument 1)>", do_exit_work, executer);
    set_global_function("serialize", "<(@serialize argument 1)>", do_serialize_work, executer);
    set_global_function("deserialize", "<(@deserialize argument 1)>", do_deserialize_work, executer);
    set_global_function("classify", "<(@classify argument 1)>", do_classify_work, executer);
    set_global_function("evaluate", "<(@evaluate argument 1)>", do_evaluate_work, executer);

    push_list_item(executer->locals, create_block(LNGRD_BLOCK_TYPE_MAP, create_map(), 1));
}

LNGRD_API void lngrd_progress_executer(lngrd_Executer *executer, lngrd_Parser *parser)
{
    lngrd_Plan *plan;
    lngrd_List *arguments;
    lngrd_List *locals;
    lngrd_List *pyre;
    lngrd_List *expressions;
    lngrd_List single;
    lngrd_Block *reserve[1];

    if (executer->errored)
    {
        return;
    }

    plan = executer->plan;
    arguments = executer->arguments;
    locals = executer->locals;
    pyre = executer->pyre;
    expressions = create_list();
    single.items = reserve;
    single.length = 1;
    single.capacity = 1;

    while (!parser->closed && !parser->errored)
    {
        lngrd_progress_parser(parser);

        if (!parser->closed && !parser->errored)
        {
            push_list_item(expressions, parser->expression);
            parser->expression = NULL;
        }
    }

    executer->errored = parser->errored;

    if (executer->errored)
    {
        burn_list(expressions, pyre);
        return;
    }

    push_plan_action(plan, prepare_general_action(expressions, LNGRD_ACTION_OWN_ITEMS | LNGRD_ACTION_OWN_LIST, 0, 0));

    while (plan->length > 0)
    {
        lngrd_Action action;
        size_t index;
        int sustain;

        index = plan->length - 1;
        action = plan->actions[index];

        if (action.single)
        {
            single.items[0] = action.single;
            expressions = &single;
        }
        else
        {
            expressions = action.expressions;
        }

        sustain = 0;

        for (; action.index < expressions->length; action.index++, action.phase = 0)
        {
            lngrd_Expression *expression;

            expression = (lngrd_Expression *) expressions->items[action.index]->data;

            if (executer->errored && expression->type != LNGRD_EXPRESSION_TYPE_INVOKE && expression->type != LNGRD_EXPRESSION_TYPE_CATCH)
            {
                break;
            }

            if (expression->type == LNGRD_EXPRESSION_TYPE_LITERAL)
            {
                lngrd_LiteralForm *form;

                form = (lngrd_LiteralForm *) expression->form;
                set_executor_result(form->block, executer);
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_LOOKUP)
            {
                lngrd_LookupForm *form;
                lngrd_Block *block;

                form = (lngrd_LookupForm *) expression->form;

                if (form->scope == LNGRD_SCOPE_TYPE_LOCAL)
                {
                    if (!peek_list_item(locals))
                    {
                        pop_list_item(locals);
                        push_list_item(locals, create_block(LNGRD_BLOCK_TYPE_MAP, create_map(), 1));
                    }

                    block = get_map_item((lngrd_Map *) peek_list_item(locals)->data, form->name, executer->sketches, executer->doodles);
                }
                else if (form->scope == LNGRD_SCOPE_TYPE_GLOBAL)
                {
                    block = get_map_item(executer->globals, form->name, executer->sketches, executer->doodles);
                }
                else
                {
                    block = NULL;
                }

                if (block)
                {
                    set_executor_result(block, executer);
                }
                else
                {
                    set_executer_error("absent variable", executer);
                    break;
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_ASSIGN)
            {
                lngrd_AssignForm *form;

                form = (lngrd_AssignForm *) expression->form;
                form->name->references++;
                executer->result->references++;

                if (form->scope == LNGRD_SCOPE_TYPE_LOCAL)
                {
                    if (!peek_list_item(locals))
                    {
                        pop_list_item(locals);
                        push_list_item(locals, create_block(LNGRD_BLOCK_TYPE_MAP, create_map(), 1));
                    }

                    set_map_item((lngrd_Map *) peek_list_item(locals)->data, form->name, executer->result, executer->pyre, executer->sketches, executer->doodles);
                }
                else if (form->scope == LNGRD_SCOPE_TYPE_GLOBAL)
                {
                    set_map_item(executer->globals, form->name, executer->result, executer->pyre, executer->sketches, executer->doodles);
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_UNASSIGN)
            {
                lngrd_UnassignForm *form;

                form = (lngrd_UnassignForm *) expression->form;

                if (form->scope == LNGRD_SCOPE_TYPE_LOCAL)
                {
                    if (!peek_list_item(locals))
                    {
                        pop_list_item(locals);
                        push_list_item(locals, create_block(LNGRD_BLOCK_TYPE_MAP, create_map(), 1));
                    }

                    unset_map_item((lngrd_Map *) peek_list_item(locals)->data, form->name, executer->pyre, executer->sketches, executer->doodles);
                }
                else if (form->scope == LNGRD_SCOPE_TYPE_GLOBAL)
                {
                    unset_map_item(executer->globals, form->name, executer->pyre, executer->sketches, executer->doodles);
                }

                set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_INVOKE)
            {
                lngrd_InvokeForm *form;

                form = (lngrd_InvokeForm *) expression->form;

                if (action.phase == 0 && form->arguments->length == 0)
                {
                    set_executer_error("absent argument", executer);
                    break;
                }

                if (executer->errored || action.phase > form->arguments->length)
                {
                    size_t index;

                    for (index = 0; index < action.phase - 1; index++)
                    {
                        push_list_item(pyre, pop_list_item(arguments));
                    }

                    if (action.phase > form->arguments->length && action.provisioned)
                    {
                        if (peek_list_item(locals))
                        {
                            push_list_item(pyre, pop_list_item(locals));
                        }
                        else
                        {
                            pop_list_item(locals);
                        }
                    }

                    if (!executer->errored)
                    {
                        action.index += 1;
                        action.phase = 0;
                        sustain = 1;
                    }

                    break;
                }

                if (action.phase < form->arguments->length)
                {
                    if (action.phase > 0)
                    {
                        push_list_item(arguments, executer->result);
                        executer->result = NULL;
                    }

                    push_plan_action(plan, prepare_simple_action(form->arguments->items[action.phase], action.checkpoint, action.capacity));
                    action.phase += 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == form->arguments->length)
                {
                    lngrd_Block *first;
                    lngrd_Function *function;
                    lngrd_UInt checkpoint;

                    push_list_item(arguments, executer->result);
                    executer->result = NULL;
                    first = arguments->items[arguments->length - action.phase];

                    if (first->type != LNGRD_BLOCK_TYPE_FUNCTION)
                    {
                        size_t index;

                        for (index = 0; index < action.phase; index++)
                        {
                            push_list_item(pyre, pop_list_item(arguments));
                        }

                        set_executer_error("alien argument", executer);
                        break;
                    }

                    function = (lngrd_Function *) first->data;

                    if (function->inlined)
                    {
                        checkpoint = action.checkpoint;
                        action.provisioned = 0;
                    }
                    else
                    {
                        checkpoint = executer->arguments->length;
                        action.provisioned = 1;
                        push_list_item(locals, NULL);
                    }

                    push_plan_action(plan, prepare_general_action(function->expressions, 0, checkpoint, action.phase));
                    action.phase += 1;
                    sustain = 1;

                    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);

                    break;
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_BRANCH)
            {
                lngrd_BranchForm *form;

                form = (lngrd_BranchForm *) expression->form;

                if (action.phase == 0)
                {
                    push_plan_action(plan, prepare_simple_action(form->test, action.checkpoint, action.capacity));
                    action.phase = 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == 1)
                {
                    if (is_block_truthy(executer->result))
                    {
                        push_plan_action(plan, prepare_simple_action(form->pass, action.checkpoint, action.capacity));
                        action.phase = 2;
                        sustain = 1;

                        break;
                    }
                    else
                    {
                        if (!form->fail)
                        {
                            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
                        }
                        else
                        {
                            push_plan_action(plan, prepare_simple_action(form->fail, action.checkpoint, action.capacity));
                            action.phase = 2;
                            sustain = 1;

                            break;
                        }
                    }
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_LOOP)
            {
                lngrd_LoopForm *form;

                form = (lngrd_LoopForm *) expression->form;

                if (action.phase == 0)
                {
                    push_plan_action(plan, prepare_simple_action(form->test, action.checkpoint, action.capacity));
                    action.phase = 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == 1)
                {
                    if (is_block_truthy(executer->result))
                    {
                        push_plan_action(plan, prepare_simple_action(form->body, action.checkpoint, action.capacity));
                        action.phase = 0;
                        sustain = 1;

                        break;
                    }
                    else
                    {
                        action.phase = 2;
                        sustain = 1;

                        break;
                    }
                }
                else if (action.phase == 2)
                {
                    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_CATCH)
            {
                lngrd_CatchForm *form;

                form = (lngrd_CatchForm *) expression->form;

                if (action.phase == 0)
                {
                    push_plan_action(plan, prepare_simple_action(form->failable, action.checkpoint, action.capacity));
                    action.phase = 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == 1)
                {
                    if (executer->errored)
                    {
                        executer->errored = 0;
                    }
                    else
                    {
                        set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
                    }
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_THROW)
            {
                lngrd_ThrowForm *form;

                form = (lngrd_ThrowForm *) expression->form;

                if (action.phase == 0)
                {
                    push_plan_action(plan, prepare_simple_action(form->error, action.checkpoint, action.capacity));
                    action.phase = 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == 1)
                {
                    executer->errored = 1;
                }

                break;
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_ARGUMENT)
            {
                lngrd_ArgumentForm *form;

                form = (lngrd_ArgumentForm *) expression->form;

                if (action.phase == 0)
                {
                    push_plan_action(plan, prepare_simple_action(form->index, action.checkpoint, action.capacity));
                    action.phase = 1;
                    sustain = 1;

                    break;
                }
                else if (action.phase == 1)
                {
                    lngrd_Number *index;

                    if (executer->result->type != LNGRD_BLOCK_TYPE_NUMBER)
                    {
                        set_executer_error("alien argument", executer);

                        break;
                    }

                    index = (lngrd_Number *) executer->result->data;

                    if (index->layout != LNGRD_NUMBER_LAYOUT_32_0)
                    {
                        set_executer_error("damaged argument", executer);

                        break;
                    }

                    if (index->value < 1 || (lngrd_UInt) index->value >= action.capacity)
                    {
                        set_executer_error("absent argument", executer);

                        break;
                    }

                    set_executor_result(executer->arguments->items[action.checkpoint - action.capacity + index->value], executer);
                    action.phase = 2;
                    sustain = 1;

                    break;
                }
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_GROUP)
            {
                lngrd_GroupForm *form;

                form = (lngrd_GroupForm *) expression->form;
                push_plan_action(plan, prepare_general_action(form->expressions, 0, action.checkpoint, action.capacity));
                action.index += 1;
                action.phase = 0;
                sustain = 1;

                set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);

                break;
            }
            else if (expression->type == LNGRD_EXPRESSION_TYPE_NATIVE)
            {
                lngrd_NativeForm *form;

                form = (lngrd_NativeForm *) expression->form;
                form->work(executer);

                break;
            }
        }

        if (sustain)
        {
            lngrd_Action *direct;

            direct = &plan->actions[index];
            direct->index = action.index;
            direct->phase = action.phase;
            direct->provisioned = action.provisioned;
        }
        else
        {
            if (action.expressions)
            {
                if (action.ownership & LNGRD_ACTION_OWN_ITEMS)
                {
                    while (action.expressions->length > 0)
                    {
                        push_list_item(pyre, pop_list_item(action.expressions));
                    }
                }

                if (action.ownership & LNGRD_ACTION_OWN_LIST)
                {
                    free(action.expressions->items);
                    free(action.expressions);
                }
            }

            pop_plan_action(plan);
            burn_pyre(pyre);
        }
    }

    if (plan->length > 0 || arguments->length > 0 || locals->length > 1)
    {
        crash_with_message("stack leaked");
    }
}

LNGRD_API void lngrd_stop_executer(lngrd_Executer *executer)
{
    if (executer->result)
    {
        push_list_item(executer->pyre, executer->result);
    }

    burn_map(executer->globals, executer->pyre);
    burn_list(executer->locals, executer->pyre);
    destroy_plan(executer->plan);
    burn_list(executer->arguments, executer->pyre);
    free(executer->sketches->items);
    free(executer->sketches);
    free(executer->doodles->items);
    free(executer->doodles);

    burn_pyre(executer->pyre);
}

static void read_whitespace_token(lngrd_Lexer *lexer)
{
    lexer->token.type = LNGRD_TOKEN_TYPE_WHITESPACE;

    while (has_another_symbol(lexer))
    {
        char symbol;

        symbol = peek_next_symbol(lexer);

        if (!is_whitespace_symbol(symbol))
        {
            return;
        }
        else
        {
            lexer->token.end++;
        }
    }
}

static void read_comment_token(lngrd_Lexer *lexer)
{
    lexer->token.type = LNGRD_TOKEN_TYPE_COMMENT;

    while (has_another_symbol(lexer))
    {
        char symbol;

        symbol = peek_next_symbol(lexer);

        if (symbol == '\n')
        {
            return;
        }
        else
        {
            lexer->token.end++;
        }
    }
}

static void read_number_token(lngrd_Lexer *lexer)
{
    lexer->token.type = LNGRD_TOKEN_TYPE_NUMBER;

    while (has_another_symbol(lexer))
    {
        char symbol;

        symbol = peek_next_symbol(lexer);

        if (!is_number_symbol(symbol))
        {
            return;
        }
        else
        {
            lexer->token.end++;
        }
    }
}

static void read_string_token(lngrd_Lexer *lexer)
{
    int escaping, completed;

    lexer->token.type = LNGRD_TOKEN_TYPE_STRING;
    escaping = 0;
    completed = 0;

    while (has_another_symbol(lexer))
    {
        char symbol;

        symbol = peek_next_symbol(lexer);

        if (symbol == '"')
        {
            read_next_symbol(lexer);

            if (escaping)
            {
                escaping = 0;
            }
            else
            {
                completed = 1;
                break;
            }
        }
        else if (symbol == '\\')
        {
            read_next_symbol(lexer);
            escaping = !escaping;
        }
        else if (is_string_symbol(symbol))
        {
            read_next_symbol(lexer);

            if (escaping)
            {
                escaping = 0;
            }
        }
        else
        {
            break;
        }
    }

    if (!completed)
    {
        lexer->errored = 1;
        return;
    }
}

static void read_identifiable_token(lngrd_Lexer *lexer)
{
    char symbol;

    lexer->token.type = LNGRD_TOKEN_TYPE_IDENTIFIABLE;

    if (!has_another_symbol(lexer))
    {
        lexer->errored = 1;
        return;
    }

    symbol = peek_next_symbol(lexer);

    if (is_shorthand_symbol(symbol))
    {
        while (has_another_symbol(lexer))
        {
            symbol = peek_next_symbol(lexer);

            if (is_shorthand_symbol(symbol))
            {
                lexer->token.end++;
            }
            else
            {
                break;
            }
        }
    }
    else if (symbol == '"')
    {
        read_next_symbol(lexer);
        read_string_token(lexer);
        lexer->token.type = LNGRD_TOKEN_TYPE_IDENTIFIABLE;
    }
    else
    {
        lexer->errored = 1;
        return;
    }
}

static void read_keyword_token(lngrd_Lexer *lexer)
{
    static const char *keywords[] = {"if", "else", "while", "catch", "throw", "argument", NULL};
    const char **keyword;

    lexer->token.type = LNGRD_TOKEN_TYPE_KEYWORD;

    for (keyword = keywords; *keyword; keyword++)
    {
        int match;
        size_t index, length;

        match = 1;
        length = strlen(*keyword);

        for (index = 0; index < length; index++)
        {
            size_t offset;

            offset = lexer->token.start + index;

            if (offset >= lexer->code->length)
            {
                lexer->errored = 1;
                return;
            }

            if (lexer->code->bytes[offset] != (*keyword)[index])
            {
                match = 0;
                break;
            }
        }

        if (match)
        {
            lexer->token.end = lexer->token.start + length;
            return;
        }
    }

    lexer->errored = 1;
}

static int has_another_symbol(const lngrd_Lexer *lexer)
{
    return lexer->token.end < lexer->code->length;
}

static char read_next_symbol(lngrd_Lexer *lexer)
{
    return lexer->code->bytes[lexer->token.end++];
}

static char peek_next_symbol(const lngrd_Lexer *lexer)
{
    return lexer->code->bytes[lexer->token.end];
}

static int is_whitespace_symbol(char symbol)
{
    return symbol == ' ' || symbol == '\t' || symbol == '\n' || symbol == '\r';
}

static int is_number_symbol(char symbol)
{
    return symbol >= '0' && symbol <= '9';
}

static int is_string_symbol(char symbol)
{
    return symbol >= ' ' && symbol <= '~';
}

static int is_scope_symbol(char symbol)
{
    return symbol == '$' || symbol == '@';
}

static int is_letter_symbol(char symbol)
{
    return symbol >= 'A' && symbol <= 'z';
}

static int is_shorthand_symbol(char symbol)
{
    return is_letter_symbol(symbol)
        || is_number_symbol(symbol)
        || symbol == '_';
}

static int is_keyword_symbol(char symbol)
{
    return symbol >= 'a' && symbol <= 'z';
}

static int parse_identifier(const lngrd_String *string, lngrd_String **result)
{
    lngrd_String view;

    if (string->bytes[1] != '"')
    {
        view.bytes = string->bytes + 1;
        view.length = string->length - 1;
    }
    else
    {
        view.bytes = string->bytes + 2;
        view.length = string->length - 3;
    }

    return unescape_string(&view, result);
}

static int unescape_string(const lngrd_String *string, lngrd_String **result)
{
    char *bytes;
    size_t index, length;

    for (index = 0, length = 0; index < string->length; index++, length++)
    {
        if (string->bytes[index] == '\\')
        {
            index += 1;

            if (string->bytes[index] == 'x' && index + 2 < string->length)
            {
                index += 2;
            }
        }
    }

    if (length == 0)
    {
        (*result) = create_string(NULL, 0);

        return 1;
    }

    bytes = (char *) allocate(length, sizeof(char));

    if (length == string->length)
    {
        memcpy(bytes, string->bytes, length);
    }
    else
    {
        size_t fill;

        for (index = 0, fill = 0; index < string->length; index++)
        {
            if (string->bytes[index] != '\\')
            {
                bytes[fill++] = string->bytes[index];
            }
            else
            {
                switch (string->bytes[++index])
                {
                    case '\\':
                        bytes[fill++] = '\\';
                        break;

                    case '"':
                        bytes[fill++] = '"';
                        break;

                    case 'n':
                        bytes[fill++] = '\n';
                        break;

                    case 'r':
                        bytes[fill++] = '\r';
                        break;

                    case 't':
                        bytes[fill++] = '\t';
                        break;

                    case 'x':
                    {
                        unsigned char byte;
                        char symbol;

                        byte = 0;

                        switch (symbol = string->bytes[++index])
                        {
                            case '0':
                                break;

                            case '1':
                                byte |= 0x10;
                                break;

                            case '2':
                                byte |= 0x20;
                                break;

                            case '3':
                                byte |= 0x30;
                                break;

                            case '4':
                                byte |= 0x40;
                                break;

                            case '5':
                                byte |= 0x50;
                                break;

                            case '6':
                                byte |= 0x60;
                                break;

                            case '7':
                                byte |= 0x70;
                                break;

                            case '8':
                                byte |= 0x80;
                                break;

                            case '9':
                                byte |= 0x90;
                                break;

                            case 'a':
                                byte |= 0xa0;
                                break;

                            case 'b':
                                byte |= 0xb0;
                                break;

                            case 'c':
                                byte |= 0xc0;
                                break;

                            case 'd':
                                byte |= 0xd0;
                                break;

                            case 'e':
                                byte |= 0xe0;
                                break;

                            case 'f':
                                byte |= 0xf0;
                                break;

                            default:
                                free(bytes);
                                return 0;
                        }

                        switch (symbol = string->bytes[++index])
                        {
                            case '0':
                                break;

                            case '1':
                                byte |= 0x01;
                                break;

                            case '2':
                                byte |= 0x02;
                                break;

                            case '3':
                                byte |= 0x03;
                                break;

                            case '4':
                                byte |= 0x04;
                                break;

                            case '5':
                                byte |= 0x05;
                                break;

                            case '6':
                                byte |= 0x06;
                                break;

                            case '7':
                                byte |= 0x07;
                                break;

                            case '8':
                                byte |= 0x08;
                                break;

                            case '9':
                                byte |= 0x09;
                                break;

                            case 'a':
                                byte |= 0x0a;
                                break;

                            case 'b':
                                byte |= 0x0b;
                                break;

                            case 'c':
                                byte |= 0x0c;
                                break;

                            case 'd':
                                byte |= 0x0d;
                                break;

                            case 'e':
                                byte |= 0x0e;
                                break;

                            case 'f':
                                byte |= 0x0f;
                                break;

                            default:
                                free(bytes);
                                return 0;
                        }

                        bytes[fill++] = byte;

                        break;
                    }

                    default:
                        free(bytes);
                        return 0;
                }
            }
        }
    }

    (*result) = create_string(bytes, length);

    return 1;
}

static int escape_string(const lngrd_String *string, lngrd_String **result)
{
    char *bytes;
    size_t index, length, escapes;

    for (index = 0, escapes = 0; index < string->length; index++)
    {
        unsigned char symbol;

        symbol = string->bytes[index];

        if (symbol >= 32 && symbol <= 126 && symbol != '\\' && symbol != '"')
        {
            continue;
        }
        else if (symbol == '\\' || symbol == '"' || symbol == '\t' || symbol == '\r' || symbol == '\n')
        {
            escapes += 1;
        }
        else
        {
            escapes += 3;
        }
    }

    if (escapes == 0)
    {
        if (!can_fit_both(string->length, 2))
        {
            return 0;
        }

        length = string->length + 2;

        if (length > LNGRD_INT_LIMIT)
        {
            return 0;
        }

        bytes = (char *) allocate(length, sizeof(char));

        if (string->length > 0)
        {
            memcpy(bytes + 1, string->bytes, string->length);
        }
    }
    else
    {
        size_t offset;

        if (!can_fit_both(string->length, 2) || !can_fit_both(string->length + 2, escapes))
        {
            return 0;
        }

        length = string->length + 2 + escapes;

        if (length > LNGRD_INT_LIMIT)
        {
            return 0;
        }

        bytes = (char *) allocate(length, sizeof(char));

        for (index = 0, offset = 1; index < string->length; index++)
        {
            unsigned char symbol;

            symbol = string->bytes[index];

            if (symbol >= 32 && symbol <= 126 && symbol != '\\' && symbol != '"')
            {
                bytes[offset++] = symbol;
            }
            else
            {
                switch (symbol)
                {
                    case '\\':
                        bytes[offset++] = '\\';
                        bytes[offset++] = '\\';
                        break;

                    case '"':
                        bytes[offset++] = '\\';
                        bytes[offset++] = '"';
                        break;

                    case '\t':
                        bytes[offset++] = '\\';
                        bytes[offset++] = 't';
                        break;

                    case '\r':
                        bytes[offset++] = '\\';
                        bytes[offset++] = 'r';
                        break;

                    case '\n':
                        bytes[offset++] = '\\';
                        bytes[offset++] = 'n';
                        break;

                    default:
                        bytes[offset++] = '\\';
                        bytes[offset++] = 'x';

                        switch (symbol & 0xf0)
                        {
                            case 0x00:
                                bytes[offset++] = '0';
                                break;

                            case 0x10:
                                bytes[offset++] = '1';
                                break;

                            case 0x20:
                                bytes[offset++] = '2';
                                break;

                            case 0x30:
                                bytes[offset++] = '3';
                                break;

                            case 0x40:
                                bytes[offset++] = '4';
                                break;

                            case 0x50:
                                bytes[offset++] = '5';
                                break;

                            case 0x60:
                                bytes[offset++] = '6';
                                break;

                            case 0x70:
                                bytes[offset++] = '7';
                                break;

                            case 0x80:
                                bytes[offset++] = '8';
                                break;

                            case 0x90:
                                bytes[offset++] = '9';
                                break;

                            case 0xa0:
                                bytes[offset++] = 'a';
                                break;

                            case 0xb0:
                                bytes[offset++] = 'b';
                                break;

                            case 0xc0:
                                bytes[offset++] = 'c';
                                break;

                            case 0xd0:
                                bytes[offset++] = 'd';
                                break;

                            case 0xe0:
                                bytes[offset++] = 'e';
                                break;

                            case 0xf0:
                                bytes[offset++] = 'f';
                                break;

                            default:
                                break;
                        }

                        switch (symbol & 0x0f)
                        {
                            case 0x00:
                                bytes[offset++] = '0';
                                break;

                            case 0x01:
                                bytes[offset++] = '1';
                                break;

                            case 0x02:
                                bytes[offset++] = '2';
                                break;

                            case 0x03:
                                bytes[offset++] = '3';
                                break;

                            case 0x04:
                                bytes[offset++] = '4';
                                break;

                            case 0x05:
                                bytes[offset++] = '5';
                                break;

                            case 0x06:
                                bytes[offset++] = '6';
                                break;

                            case 0x07:
                                bytes[offset++] = '7';
                                break;

                            case 0x08:
                                bytes[offset++] = '8';
                                break;

                            case 0x09:
                                bytes[offset++] = '9';
                                break;

                            case 0x0a:
                                bytes[offset++] = 'a';
                                break;

                            case 0x0b:
                                bytes[offset++] = 'b';
                                break;

                            case 0x0c:
                                bytes[offset++] = 'c';
                                break;

                            case 0x0d:
                                bytes[offset++] = 'd';
                                break;

                            case 0x0e:
                                bytes[offset++] = 'e';
                                break;

                            case 0x0f:
                                bytes[offset++] = 'f';
                                break;

                            default:
                                break;
                        }

                        break;
                }
            }
        }
    }

    bytes[0] = '"';
    bytes[length - 1] = '"';
    (*result) = create_string(bytes, length);

    return 1;
}

static void do_add_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;
    lngrd_Number *l, *r;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &right))
    {
        return;
    }

    l = (lngrd_Number *) left->data;
    r = (lngrd_Number *) right->data;

    if ((r->value > 0 && (l->value > (LNGRD_INT_LIMIT - r->value)))
            || (r->value < 0 && (l->value < (-LNGRD_INT_LIMIT - r->value))))
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, l->value + r->value), 0), executer);
}

static void do_subtract_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;
    lngrd_Number *l, *r;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &right))
    {
        return;
    }

    l = (lngrd_Number *) left->data;
    r = (lngrd_Number *) right->data;

    if ((r->value > 0 && (l->value < (-LNGRD_INT_LIMIT + r->value)))
            || (r->value < 0 && (l->value > (LNGRD_INT_LIMIT + r->value))))
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, l->value - r->value), 0), executer);
}

static void do_multiply_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;
    lngrd_Number *l, *r;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &right))
    {
        return;
    }

    l = (lngrd_Number *) left->data;
    r = (lngrd_Number *) right->data;

    if ((r->value > 0 && (l->value > (LNGRD_INT_LIMIT / r->value)))
            || (r->value > 0 && (l->value < (-LNGRD_INT_LIMIT / r->value)))
            || (r->value < 0 && (l->value < (LNGRD_INT_LIMIT / r->value)))
            || (r->value < 0 && (l->value > (-LNGRD_INT_LIMIT / r->value))))
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, l->value * r->value), 0), executer);
}

static void do_divide_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;
    lngrd_Number *l, *r;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &right))
    {
        return;
    }

    l = (lngrd_Number *) left->data;
    r = (lngrd_Number *) right->data;

    if (r->value == 0)
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, l->value / r->value), 0), executer);
}

static void do_modulo_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;
    lngrd_Number *l, *r;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &right))
    {
        return;
    }

    l = (lngrd_Number *) left->data;
    r = (lngrd_Number *) right->data;

    if (r->value == 0)
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, l->value % r->value), 0), executer);
}

static void do_increment_work(lngrd_Executer *executer)
{
    lngrd_Block *value;
    lngrd_Number *v;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &value))
    {
        return;
    }

    v = (lngrd_Number *) value->data;

    if (v->value > (LNGRD_INT_LIMIT - 1))
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, v->value + 1), 0), executer);
}

static void do_decrement_work(lngrd_Executer *executer)
{
    lngrd_Block *value;
    lngrd_Number *v;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &value))
    {
        return;
    }

    v = (lngrd_Number *) value->data;

    if (v->value < (-LNGRD_INT_LIMIT + 1))
    {
        set_executer_error("arithmetic error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, v->value - 1), 0), executer);
}

static void do_and_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &right))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, is_block_truthy(left) && is_block_truthy(right)), 0), executer);
}

static void do_or_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &right))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, is_block_truthy(left) || is_block_truthy(right)), 0), executer);
}

static void do_not_work(lngrd_Executer *executer)
{
    lngrd_Block *value;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &value))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, !is_block_truthy(value)), 0), executer);
}

static void do_precedes_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &right))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, compare_blocks(left, right, executer->sketches, executer->doodles) < 0), 0), executer);
}

static void do_succeeds_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &right))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, compare_blocks(left, right, executer->sketches, executer->doodles) > 0), 0), executer);
}

static void do_equals_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &right))
    {
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, compare_blocks(left, right, executer->sketches, executer->doodles) == 0), 0), executer);
}

static void do_get_work(lngrd_Executer *executer)
{
    lngrd_Block *collection, *key;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &collection)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &key))
    {
        return;
    }

    switch (collection->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_Number *number;
            lngrd_String *string;
            char *bytes;

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            string = (lngrd_String *) collection->data;

            if (number->value < 1 || (size_t) number->value > string->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            bytes = (char *) allocate(1, sizeof(char));
            bytes[0] = string->bytes[number->value - 1];

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, create_string(bytes, 1), 0), executer);

            break;
        }

        case LNGRD_BLOCK_TYPE_LIST:
        {
            lngrd_Number *number;
            lngrd_List *list;

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            list = (lngrd_List *) collection->data;

            if (number->value < 1 || (size_t) number->value > list->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            set_executor_result(list->items[number->value - 1], executer);

            break;
        }

        default:
            break;
    }
}

static void do_set_work(lngrd_Executer *executer)
{
    lngrd_Block *collection, *key, *item;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &collection)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &key))
    {
        return;
    }

    switch (collection->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_Number *number;
            lngrd_String *source, *addition, *destination;
            char *bytes;
            size_t length, before, after;

            if (!require_argument(3, LNGRD_BLOCK_TYPE_STRING, executer, &item))
            {
                return;
            }

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            source = (lngrd_String *) collection->data;

            if (number->value < 1 || (size_t) number->value > source->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            addition = (lngrd_String *) item->data;

            if (!can_fit_both(source->length - 1, addition->length))
            {
                set_executer_error("boundary error", executer);
                return;
            }

            length = source->length - 1 + addition->length;

            if (length > LNGRD_INT_LIMIT)
            {
                set_executer_error("boundary error", executer);
                return;
            }

            before = number->value - 1;
            after = source->length - before - 1;
            bytes = (char *) allocate(length, sizeof(char));
            destination = create_string(bytes, length);

            if (before > 0)
            {
                memcpy(bytes, source->bytes, before);
            }

            if (addition->length > 0)
            {
                memcpy(bytes + before, addition->bytes, addition->length);
            }

            if (after > 0)
            {
                memcpy(bytes + before + addition->length, source->bytes + before + 1, after);
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, destination, 0), executer);

            break;
        }

        case LNGRD_BLOCK_TYPE_LIST:
        {
            lngrd_Number *number;
            lngrd_List *source, *destination;
            size_t index;

            if (!require_argument(3, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &item))
            {
                return;
            }

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            source = (lngrd_List *) collection->data;

            if (number->value < 1 || (size_t) number->value > source->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            destination = create_list();

            for (index = 0; index < source->length; index++)
            {
                if (index != (size_t) number->value - 1)
                {
                    source->items[index]->references += 1;
                    push_list_item(destination, source->items[index]);
                }
                else
                {
                    item->references += 1;
                    push_list_item(destination, item);
                }
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_LIST, destination, 0), executer);

            break;
        }

        default:
            break;
    }
}

static void do_unset_work(lngrd_Executer *executer)
{
    lngrd_Block *collection, *key;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &collection)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &key))
    {
        return;
    }

    switch (collection->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_Number *number;
            lngrd_String *source, *destination;
            char *bytes;
            size_t length;

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            source = (lngrd_String *) collection->data;

            if (number->value < 1 || (size_t) number->value > source->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            length = source->length - 1;

            if (length > 1)
            {
                bytes = (char *) allocate(length, sizeof(char));
                memcpy(bytes, source->bytes, number->value - 1);
                memcpy(bytes + number->value - 1, source->bytes + number->value, source->length - number->value);
            }
            else
            {
                bytes = NULL;
            }

            destination = create_string(bytes, length);

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, destination, 0), executer);

            break;
        }

        case LNGRD_BLOCK_TYPE_LIST:
        {
            lngrd_Number *number;
            lngrd_List *source, *destination;
            size_t index;

            number = (lngrd_Number *) key->data;

            if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
            {
                set_executer_error("damaged argument", executer);
                return;
            }

            source = (lngrd_List *) collection->data;

            if (number->value < 1 || (size_t) number->value > source->length)
            {
                set_executer_error("absent key", executer);
                return;
            }

            destination = create_list();

            for (index = 0; index < source->length; index++)
            {
                if (index != (size_t) number->value - 1)
                {
                    source->items[index]->references += 1;
                    push_list_item(destination, source->items[index]);
                }
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_LIST, destination, 0), executer);

            break;
        }

        default:
            break;
    }
}

static void do_measure_work(lngrd_Executer *executer)
{
    lngrd_Block *value;
    size_t length;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &value))
    {
        return;
    }

    length = 0;

    switch (value->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
            length = ((lngrd_String *) value->data)->length;
            break;

        case LNGRD_BLOCK_TYPE_LIST:
            length = ((lngrd_List *) value->data)->length;
            break;

        default:
            break;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_NUMBER, create_number(LNGRD_NUMBER_LAYOUT_32_0, (lngrd_SInt) length), 0), executer);
}

static void do_slice_work(lngrd_Executer *executer)
{
    lngrd_Block *value, *start, *end;
    lngrd_Number *s, *e;
    lngrd_SInt left, right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &value)
            || !require_argument(2, LNGRD_BLOCK_TYPE_NUMBER, executer, &start)
            || !require_argument(3, LNGRD_BLOCK_TYPE_NUMBER, executer, &end))
    {
        return;
    }

    s = (lngrd_Number *) start->data;
    e = (lngrd_Number *) end->data;

    left = s->value - 1;
    right = e->value - 1;

    if (left > right)
    {
        lngrd_SInt swap;

        swap = left;
        left = right;
        right = swap;
    }

    if (left < 0)
    {
        left = 0;
    }

    switch (value->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_String *v;
            char *bytes;
            size_t length;

            v = (lngrd_String *) value->data;

            if (right < 0 || (size_t) right >= v->length)
            {
                right = v->length - 1;
            }

            right += 1;
            length = right - left;

            if (length > 0)
            {
                bytes = (char *) allocate(length, sizeof(char));
                memcpy(bytes, v->bytes + left, length);
            }
            else
            {
                bytes = NULL;
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, create_string(bytes, length), 0), executer);

            break;
        }

        case LNGRD_BLOCK_TYPE_LIST:
        {
            lngrd_List *v, *result;
            size_t index, length;

            v = (lngrd_List *) value->data;

            if (right < 0 || (size_t) right >= v->length)
            {
                right = v->length - 1;
            }

            right += 1;
            length = right - left;

            result = create_list();

            for (index = 0; index < length; index++)
            {
                push_list_item(result, v->items[left + index]);
                v->items[left + index]->references += 1;
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_LIST, result, 0), executer);

            break;
        }

        default:
            break;
    }
}

static void do_merge_work(lngrd_Executer *executer)
{
    lngrd_Block *left, *right;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &left)
            || !require_argument(2, LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST, executer, &right))
    {
        return;
    }

    if (left->type != right->type)
    {
        set_executer_error("alien argument", executer);
        return;
    }

    switch (left->type)
    {
        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_String *l, *r;
            char *bytes;
            size_t length;

            l = (lngrd_String *) left->data;
            r = (lngrd_String *) right->data;

            if (!can_fit_both(l->length, r->length))
            {
                set_executer_error("boundary error", executer);
                return;
            }

            length = l->length + r->length;

            if (length > LNGRD_INT_LIMIT)
            {
                set_executer_error("boundary error", executer);
                return;
            }

            if (length > 0)
            {
                bytes = (char *) allocate(length, sizeof(char));
            }
            else
            {
                bytes = NULL;
            }

            if (l->length > 0)
            {
                memcpy(bytes, l->bytes, l->length);
            }

            if (r->length > 0)
            {
                memcpy(bytes + l->length, r->bytes, r->length);
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, create_string(bytes, length), 0), executer);

            break;
        }

        case LNGRD_BLOCK_TYPE_LIST:
        {
            lngrd_List *l, *r, *result;
            size_t index, length;

            l = (lngrd_List *) left->data;
            r = (lngrd_List *) right->data;

            if (!can_fit_both(l->length, r->length))
            {
                set_executer_error("boundary error", executer);
                return;
            }

            length = l->length + r->length;

            if (length > LNGRD_INT_LIMIT)
            {
                set_executer_error("boundary error", executer);
                return;
            }

            result = create_list();

            for (index = 0; index < l->length; index++)
            {
                push_list_item(result, l->items[index]);
                l->items[index]->references += 1;
            }

            for (index = 0; index < r->length; index++)
            {
                push_list_item(result, r->items[index]);
                r->items[index]->references += 1;
            }

            set_executor_result(create_block(LNGRD_BLOCK_TYPE_LIST, result, 0), executer);

            break;
        }

        default:
            break;
    }
}

static void do_sort_work(lngrd_Executer *executer)
{
    lngrd_Block *list;
    lngrd_List *sorted;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_LIST, executer, &list))
    {
        return;
    }

    sorted = copy_list((lngrd_List *) list->data);
    sort_list(sorted, executer->sketches, executer->doodles);

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_LIST, sorted, 0), executer);
}

static void do_read_work(lngrd_Executer *executer)
{
    lngrd_Block *file, *until;
    lngrd_String *u;
    FILE *handle;
    int terminated, closable;
    char terminator;
    lngrd_Buffer buffer;
    lngrd_String expand;
    char slot[1];

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING, executer, &file)
            || !require_argument(2, LNGRD_BLOCK_TYPE_STRING, executer, &until))
    {
        return;
    }

    u = (lngrd_String *) until->data;

    terminated = u->length > 0;
    terminator = terminated ? u->bytes[0] : '\0';

    if (u->length > 1)
    {
        set_executer_error("damaged argument", executer);
        return;
    }

    handle = NULL;
    closable = 0;

    switch (file->type)
    {
        case LNGRD_BLOCK_TYPE_NUMBER:
        {
            lngrd_Number *f;
            lngrd_Number in, out, err;

            f = (lngrd_Number *) file->data;
            in.value = 0;
            in.layout = LNGRD_NUMBER_LAYOUT_32_0;
            out.value = 1;
            out.layout = LNGRD_NUMBER_LAYOUT_32_0;
            err.value = 2;
            err.layout = LNGRD_NUMBER_LAYOUT_32_0;

            if (compare_numbers(f, &in) == 0)
            {
                handle = stdin;
            }
            else if (compare_numbers(f, &out) == 0)
            {
                handle = stdout;
            }
            else if (compare_numbers(f, &err) == 0)
            {
                handle = stderr;
            }

            break;
        }

        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_String *f;
            char *cstring;

            f = (lngrd_String *) file->data;

            cstring = string_to_cstring(f);
            handle = fopen(cstring, "rb");
            free(cstring);
            closable = 1;

            break;
        }

        default:
            break;
    }

    if (!handle)
    {
        set_executer_error("absent file", executer);
        return;
    }

    start_buffer(&buffer);
    expand.bytes = slot;
    expand.length = 1;

    while (1)
    {
        int symbol;

        symbol = getc(handle);

        if (ferror(handle))
        {
            if (closable)
            {
                fclose(handle);
            }

            abandon_buffer(&buffer);

            set_executer_error("io error", executer);
            return;
        }

        if (symbol == EOF || (terminated && terminator == symbol))
        {
            break;
        }

        slot[0] = symbol;

        if (!append_buffer_bytes(&buffer, &expand))
        {
            if (closable)
            {
                fclose(handle);
            }

            abandon_buffer(&buffer);

            set_executer_error("boundary error", executer);
            return;
        }
    }

    if (closable)
    {
        fclose(handle);
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, buffer_to_string(&buffer), 0), executer);
}

static void do_write_work(lngrd_Executer *executer)
{
    lngrd_Block *file, *text;
    lngrd_String *t;
    FILE *handle;
    int closable;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING, executer, &file)
            || !require_argument(2, LNGRD_BLOCK_TYPE_STRING, executer, &text))
    {
        return;
    }

    handle = NULL;
    closable = 0;

    switch (file->type)
    {
        case LNGRD_BLOCK_TYPE_NUMBER:
        {
            lngrd_Number *f;
            lngrd_Number in, out, err;

            f = (lngrd_Number *) file->data;
            in.value = 0;
            in.layout = LNGRD_NUMBER_LAYOUT_32_0;
            out.value = 1;
            out.layout = LNGRD_NUMBER_LAYOUT_32_0;
            err.value = 2;
            err.layout = LNGRD_NUMBER_LAYOUT_32_0;

            if (compare_numbers(f, &in) == 0)
            {
                handle = stdin;
            }
            else if (compare_numbers(f, &out) == 0)
            {
                handle = stdout;
            }
            else if (compare_numbers(f, &err) == 0)
            {
                handle = stderr;
            }

            break;
        }

        case LNGRD_BLOCK_TYPE_STRING:
        {
            lngrd_String *f;
            char *cstring;

            f = (lngrd_String *) file->data;

            cstring = string_to_cstring(f);
            handle = fopen(cstring, "wb");
            free(cstring);
            closable = 1;

            break;
        }

        default:
            break;
    }

    if (!handle)
    {
        set_executer_error("absent file", executer);
        return;
    }

    t = (lngrd_String *) text->data;

    if (t->length > 0)
    {
        fwrite(t->bytes, sizeof(char), t->length, handle);
    }

    if (ferror(handle))
    {
        if (closable)
        {
            fclose(handle);
        }

        set_executer_error("io error", executer);
        return;
    }

    if (closable)
    {
        fclose(handle);
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
}

static void do_delete_work(lngrd_Executer *executer)
{
    lngrd_Block *file;
    char *cstring;
    int status;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING, executer, &file))
    {
        return;
    }

    cstring = NULL;

    if (file->type == LNGRD_BLOCK_TYPE_STRING)
    {
        lngrd_String *f;

        f = (lngrd_String *) file->data;

        cstring = string_to_cstring(f);
    }

    if (!cstring)
    {
        set_executer_error("io error", executer);
        return;
    }

    status = remove(cstring);
    free(cstring);

    if (status == -1)
    {
        set_executer_error("io error", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
}

static void do_query_work(lngrd_Executer *executer)
{
    lngrd_Block *variable;
    lngrd_String *v;
    char *cstring, *text;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING, executer, &variable))
    {
        return;
    }

    v = (lngrd_String *) variable->data;

    cstring = string_to_cstring(v);
    text = getenv(cstring);
    free(cstring);

    if (!text)
    {
        set_executer_error("absent environment variable", executer);
        return;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(text), 0), executer);
}

static void do_exit_work(lngrd_Executer *executer)
{
    lngrd_Block *code;
    lngrd_Number *c;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER, executer, &code))
    {
        return;
    }

    c = (lngrd_Number *) code->data;

    if (c->layout != LNGRD_NUMBER_LAYOUT_32_0)
    {
        set_executer_error("damaged argument", executer);
        return;
    }

    if (c->value < 0 || c->value > 255)
    {
        set_executer_error("damaged argument", executer);
        return;
    }

    exit(c->value);

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
}

static void do_serialize_work(lngrd_Executer *executer)
{
    lngrd_Block *value;
    lngrd_Buffer buffer;
    lngrd_List *sketches;
    static const lngrd_String listStart = {(char *) "[", 1};
    static const lngrd_String listEnd = {(char *) "]", 1};
    static const lngrd_String delimiter = {(char *) " ", 1};

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_FUNCTION, executer, &value))
    {
        return;
    }

    start_buffer(&buffer);
    sketches = executer->sketches;

    sketches->length = 0;
    push_list_item(sketches, value);

    while (sketches->length > 0)
    {
        value = pop_list_item(sketches);

        if (value)
        {
            switch (value->type)
            {
                case LNGRD_BLOCK_TYPE_NUMBER:
                {
                    lngrd_String *string;

                    if (!number_to_string((lngrd_Number *) value->data, &string))
                    {
                        abandon_buffer(&buffer);
                        set_executer_error("codec error", executer);
                        return;
                    }

                    if (!append_buffer_bytes(&buffer, string))
                    {
                        abandon_buffer(&buffer);
                        set_executer_error("codec error", executer);
                        return;
                    }

                    destroy_string(string);

                    if (sketches->length > 0 && peek_list_item(sketches))
                    {
                        append_buffer_bytes(&buffer, &delimiter);
                    }

                    break;
                }

                case LNGRD_BLOCK_TYPE_STRING:
                {
                    lngrd_String *string;

                    if (!escape_string((lngrd_String *) value->data, &string))
                    {
                        abandon_buffer(&buffer);
                        set_executer_error("codec error", executer);
                        return;
                    }

                    if (!append_buffer_bytes(&buffer, string))
                    {
                        abandon_buffer(&buffer);
                        set_executer_error("codec error", executer);
                        return;
                    }

                    destroy_string(string);

                    if (sketches->length > 0 && peek_list_item(sketches))
                    {
                        append_buffer_bytes(&buffer, &delimiter);
                    }

                    break;
                }

                case LNGRD_BLOCK_TYPE_LIST:
                {
                    lngrd_List *list;
                    size_t index;

                    append_buffer_bytes(&buffer, &listStart);
                    push_list_item(sketches, NULL);

                    list = (lngrd_List *) value->data;

                    if (list->length == 0)
                    {
                        break;
                    }

                    index = list->length - 1;

                    while (1)
                    {
                        push_list_item(sketches, list->items[index]);

                        if (index == 0)
                        {
                            break;
                        }

                        index -= 1;
                    }

                    break;
                }

                case LNGRD_BLOCK_TYPE_FUNCTION:
                {
                    if (!append_buffer_bytes(&buffer, ((lngrd_Function *) value->data)->source))
                    {
                        abandon_buffer(&buffer);
                        set_executer_error("codec error", executer);
                        return;
                    }

                    if (sketches->length > 0 && peek_list_item(sketches))
                    {
                        append_buffer_bytes(&buffer, &delimiter);
                    }

                    break;
                }

                default:
                    break;
            }
        }
        else
        {
            append_buffer_bytes(&buffer, &listEnd);

            if (sketches->length > 0 && peek_list_item(sketches))
            {
                append_buffer_bytes(&buffer, &delimiter);
            }
        }
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, buffer_to_string(&buffer), 0), executer);
}

static void do_deserialize_work(lngrd_Executer *executer)
{
    lngrd_Block *code, *value;
    lngrd_String *c;
    lngrd_Lexer lexer;
    lngrd_Parser parser;
    lngrd_Expression *literal;
    lngrd_LiteralForm *form;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING, executer, &code))
    {
        return;
    }

    c = (lngrd_String *) code->data;

    lngrd_start_lexer(&lexer, c);
    lngrd_start_parser(&parser, &lexer, executer->pyre);
    lngrd_progress_parser(&parser);

    if (parser.errored || parser.closed)
    {
        lngrd_stop_parser(&parser);
        set_executer_error("codec error", executer);
        return;
    }

    literal = (lngrd_Expression *) parser.expression->data;

    if (literal->type != LNGRD_EXPRESSION_TYPE_LITERAL)
    {
        lngrd_stop_parser(&parser);
        set_executer_error("codec error", executer);
        return;
    }

    form = (lngrd_LiteralForm *) literal->form;
    value = form->block;
    value->references += 1;
    lngrd_stop_parser(&parser);
    value->references -= 1;

    set_executor_result(value, executer);
}

static void do_classify_work(lngrd_Executer *executer)
{
    lngrd_Block *value;
    char *type;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_NUMBER | LNGRD_BLOCK_TYPE_STRING | LNGRD_BLOCK_TYPE_LIST | LNGRD_BLOCK_TYPE_MAP | LNGRD_BLOCK_TYPE_FUNCTION, executer, &value))
    {
        return;
    }

    type = (char *) "";

    switch (value->type)
    {
        case LNGRD_BLOCK_TYPE_NUMBER:
            type = (char *) "number";
            break;

        case LNGRD_BLOCK_TYPE_STRING:
            type = (char *) "string";
            break;

        case LNGRD_BLOCK_TYPE_LIST:
            type = (char *) "list";
            break;

        case LNGRD_BLOCK_TYPE_MAP:
            type = (char *) "map";
            break;

        case LNGRD_BLOCK_TYPE_FUNCTION:
            type = (char *) "function";
            break;

        default:
            break;
    }

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(type), 0), executer);
}

static void do_evaluate_work(lngrd_Executer *executer)
{
    lngrd_Block *code;
    lngrd_String *c;
    lngrd_Parser parser;
    lngrd_Lexer lexer;
    lngrd_List *expressions;
    lngrd_Action native, invoke;
    char *error;

    if (!require_argument(1, LNGRD_BLOCK_TYPE_STRING, executer, &code))
    {
        return;
    }

    c = (lngrd_String *) code->data;

    lngrd_start_lexer(&lexer, c);
    lngrd_start_parser(&parser, &lexer, executer->pyre);

    expressions = create_list();

    while (!parser.closed && !parser.errored)
    {
        lngrd_progress_parser(&parser);

        if (!parser.closed && !parser.errored)
        {
            push_list_item(expressions, parser.expression);
            parser.expression = NULL;
        }
    }

    error = NULL;

    if (lexer.errored)
    {
        error = (char *) "lex error";
    }
    else if (parser.errored)
    {
        error = (char *) "parse error";
    }

    lngrd_stop_parser(&parser);

    if (error)
    {
        burn_list(expressions, executer->pyre);
        set_executer_error(error, executer);
        return;
    }

    native = pop_plan_action(executer->plan);
    invoke = peek_plan_action(executer->plan);
    push_plan_action(executer->plan, prepare_general_action(expressions, LNGRD_ACTION_OWN_ITEMS | LNGRD_ACTION_OWN_LIST, invoke.checkpoint, invoke.capacity));
    push_plan_action(executer->plan, native);

    set_executor_result(create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(""), 0), executer);
}

static void set_global_function(const char *name, const char *source, void (*work)(lngrd_Executer *), lngrd_Executer *executer)
{
    lngrd_NativeForm *form;
    lngrd_Block *key, *value;
    lngrd_Function *function;
    lngrd_Expression *expression;

    form = (lngrd_NativeForm *) allocate(1, sizeof(lngrd_NativeForm));
    form->work = work;
    key = create_block(LNGRD_BLOCK_TYPE_STRING, cstring_to_string(name), 1);
    function = create_function();
    function->source = cstring_to_string(source);
    function->inlined = 1;
    expression = create_expression(LNGRD_EXPRESSION_TYPE_NATIVE, form);
    push_list_item(function->expressions, create_block(LNGRD_BLOCK_TYPE_EXPRESSION, expression, 1));
    value = create_block(LNGRD_BLOCK_TYPE_FUNCTION, function, 1);
    set_map_item(executer->globals, key, value, executer->pyre, executer->sketches, executer->doodles);
}

static void set_executer_error(const char *message, lngrd_Executer *executer)
{
    lngrd_Block *error;
    lngrd_String *data;

    data = cstring_to_string(message);
    error = create_block(LNGRD_BLOCK_TYPE_STRING, data, 0);
    set_executor_result(error, executer);
    executer->errored = 1;
}

static void set_executor_result(lngrd_Block *result, lngrd_Executer *executer)
{
    if (executer->result)
    {
        push_list_item(executer->pyre, executer->result);
    }

    if (result)
    {
        executer->result = result;
        result->references += 1;
    }
}

static int require_argument(lngrd_UInt index, lngrd_UInt types, lngrd_Executer *executer, lngrd_Block **result)
{
    lngrd_Block *argument;
    lngrd_UInt capacity, length;

    capacity = peek_plan_action(executer->plan).capacity;
    length = executer->arguments->length;

    if (index < 1 || index >= capacity)
    {
        set_executer_error("absent argument", executer);
        return 0;
    }

    argument = executer->arguments->items[length - capacity + index];

    if (!(argument->type & types))
    {
        set_executer_error("alien argument", executer);
        return 0;
    }

    (*result) = argument;

    return 1;
}

static lngrd_Block *create_block(lngrd_BlockType type, void *data, size_t references)
{
    lngrd_Block *block;

    block = (lngrd_Block *) allocate(1, sizeof(lngrd_Block));
    block->type = type;
    block->data = data;
    block->references = references;

    return block;
}

static lngrd_UInt hash_block(const lngrd_Block *block)
{
    switch (block->type)
    {
        case LNGRD_BLOCK_TYPE_NUMBER:
            return hash_number((lngrd_Number *) block->data);

        case LNGRD_BLOCK_TYPE_STRING:
            return hash_string((lngrd_String *) block->data);

        default:
            crash_with_message("unsupported branch");
    }

    return 0;
}

static lngrd_SInt compare_blocks(const lngrd_Block *x, const lngrd_Block *y, lngrd_List *sketches, lngrd_List *doodles)
{
    sketches->length = 0;
    push_list_item(sketches, (lngrd_Block *) x);
    doodles->length = 0;
    push_list_item(doodles, (lngrd_Block *) y);

    while (sketches->length > 0)
    {
        lngrd_Block *left, *right;
        lngrd_SInt test;

        left = pop_list_item(sketches);
        right = pop_list_item(doodles);

        if (left->type < right->type)
        {
            return -1;
        }
        else if (x->type > y->type)
        {
            return 1;
        }

        test = 0;

        switch (left->type)
        {
            case LNGRD_BLOCK_TYPE_NUMBER:
                test = compare_numbers((lngrd_Number *) left->data, (lngrd_Number *) right->data);
                break;

            case LNGRD_BLOCK_TYPE_STRING:
                test = compare_strings((lngrd_String *) left->data, (lngrd_String *) right->data);
                break;

            case LNGRD_BLOCK_TYPE_LIST:
            {
                lngrd_List *l, *r;
                size_t index;

                l = (lngrd_List *) left->data;
                r = (lngrd_List *) right->data;

                if (l->length < r->length)
                {
                    test = -1;
                    break;
                }
                else if (l->length > r->length)
                {
                    test = 1;
                    break;
                }

                if (l->length == 0)
                {
                    break;
                }

                index = l->length - 1;

                while (1)
                {
                    push_list_item(sketches, l->items[index]);
                    push_list_item(doodles, r->items[index]);

                    if (index == 0)
                    {
                        break;
                    }

                    index -= 1;
                }

                break;
            }

            case LNGRD_BLOCK_TYPE_FUNCTION:
                test = compare_functions((lngrd_Function *) left->data, (lngrd_Function *) right->data);
                break;

            default:
                crash_with_message("unsupported branch");
        }

        if (test != 0)
        {
            return test;
        }
    }

    return 0;
}

static int is_block_truthy(const lngrd_Block *block)
{
    switch (block->type)
    {
        case LNGRD_BLOCK_TYPE_NUMBER:
            return ((lngrd_Number *) block->data)->value != 0;

        case LNGRD_BLOCK_TYPE_STRING:
            return ((lngrd_String *) block->data)->length > 0;

        case LNGRD_BLOCK_TYPE_LIST:
            return ((lngrd_List *) block->data)->length > 0;

        case LNGRD_BLOCK_TYPE_MAP:
            return ((lngrd_Map *) block->data)->length > 0;

        case LNGRD_BLOCK_TYPE_FUNCTION:
            return ((lngrd_Function *) block->data)->expressions->length > 0;

        default:
            crash_with_message("unsupported branch");
    }

    return 0;
}

static void burn_pyre(lngrd_List *pyre)
{
    while (pyre->length > 0)
    {
        lngrd_Block *fuel;

        fuel = pop_list_item(pyre);

        if (fuel->references == 0)
        {
            crash_with_message("double burn");
        }

        if (--fuel->references == 0)
        {
            if (fuel->data)
            {
                switch (fuel->type)
                {
                    case LNGRD_BLOCK_TYPE_NUMBER:
                        destroy_number((lngrd_Number *) fuel->data);
                        break;

                    case LNGRD_BLOCK_TYPE_STRING:
                        destroy_string((lngrd_String *) fuel->data);
                        break;

                    case LNGRD_BLOCK_TYPE_LIST:
                        burn_list((lngrd_List *) fuel->data, pyre);
                        break;

                    case LNGRD_BLOCK_TYPE_MAP:
                        burn_map((lngrd_Map *) fuel->data, pyre);
                        break;

                    case LNGRD_BLOCK_TYPE_FUNCTION:
                        burn_function((lngrd_Function *) fuel->data, pyre);
                        break;

                    case LNGRD_BLOCK_TYPE_EXPRESSION:
                        burn_expression((lngrd_Expression *) fuel->data, pyre);
                        break;

                    default:
                        crash_with_message("unsupported branch");
                }
            }

            free(fuel);
        }
    }
}

static lngrd_Number *create_number(lngrd_NumberLayout layout, lngrd_SInt value)
{
    lngrd_Number *number;

    number = (lngrd_Number *) allocate(1, sizeof(lngrd_Number));
    number->layout = layout;
    number->value = value;

    return number;
}

static int string_to_number(const lngrd_String *string, lngrd_Number **result)
{
    static lngrd_UInt ten_to[] = {1UL, 10UL, 100UL, 1000UL, 10000UL, 100000UL, 1000000UL, 10000000UL, 100000000UL, 1000000000UL};
    lngrd_SInt value;
    lngrd_UInt numeric;
    int negative;
    size_t index, place;

    numeric = 0;
    negative = 0;
    index = 0;

    if (string->bytes[0] == '-')
    {
        negative = 1;
        index += 1;
    }
    else if (string->bytes[0] == '+')
    {
        index += 1;
    }

    for (; index < string->length; index++)
    {
        unsigned char digit;

        digit = string->bytes[index];

        if (digit != '0')
        {
            break;
        }
    }

    for (place = string->length - index - 1; index < string->length; place--, index++)
    {
        unsigned char digit;

        digit = string->bytes[index];
        numeric += (digit - '0') * ten_to[place];

        if (numeric > LNGRD_INT_LIMIT)
        {
            return 0;
        }
    }

    value = numeric;

    if (negative)
    {
        value *= -1;
    }

    (*result) = create_number(LNGRD_NUMBER_LAYOUT_32_0, value);

    return 1;
}

static int number_to_string(const lngrd_Number *number, lngrd_String **result)
{
    lngrd_SInt whole;
    lngrd_String *string;
    size_t length, index;
    char *bytes;

    if (number->layout != LNGRD_NUMBER_LAYOUT_32_0)
    {
        return 0;
    }

    whole = number->value;

    if (whole < 0)
    {
        whole *= -1;
    }

    for (length = 0; whole > 0 || length == 0; length++)
    {
        whole /= 10;
    }

    whole = number->value;

    if (whole < 0)
    {
        length += 1;
    }

    bytes = (char *) allocate(length, sizeof(char));
    string = create_string(bytes, length);
    index = length - 1;

    if (whole < 0)
    {
        bytes[0] = '-';
        whole *= -1;
    }

    if (whole == 0)
    {
        bytes[index] = '0';
    }

    while (whole > 0)
    {
        lngrd_SInt next, digit;

        next = whole / 10;
        digit = whole - (next * 10);
        bytes[index--] = '0' + digit;
        whole = next;
    }

    (*result) = string;

    return 1;
}

static lngrd_SInt compare_numbers(const lngrd_Number *left, const lngrd_Number *right)
{
    if (left->value < right->value)
    {
        return -1;
    }
    else if (left->value > right->value)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

static lngrd_UInt hash_number(const lngrd_Number *number)
{
    return (lngrd_UInt) number->value;
}

static void destroy_number(lngrd_Number *number)
{
    free(number);
}

static lngrd_String *create_string(char *bytes, size_t length)
{
    lngrd_String *string;

    string = (lngrd_String *) allocate(1, sizeof(lngrd_String));
    string->bytes = bytes;
    string->length = length;

    return string;
}

static lngrd_String *cstring_to_string(const char *cstring)
{
    return bytes_to_string(cstring, strlen(cstring));
}

static lngrd_String *bytes_to_string(const char *bytes, size_t length)
{
    char *buffer;

    if (length > 0)
    {
        buffer = (char *) allocate(length, sizeof(char));
        memcpy(buffer, bytes, length);
    }
    else
    {
        buffer = NULL;
    }

    return create_string(buffer, length);
}

static char *string_to_cstring(const lngrd_String *string)
{
    char *cstring;

    cstring = (char *) allocate(string->length + 1, sizeof(char));

    if (string->length > 0)
    {
        memcpy(cstring, string->bytes, string->length);
    }

    cstring[string->length] = '\0';

    return cstring;
}

static lngrd_SInt compare_strings(const lngrd_String *left, const lngrd_String *right)
{
    size_t index;

    for (index = 0; index < left->length; index++)
    {
        unsigned char x, y;

        if (index == right->length)
        {
            return 1;
        }

        x = left->bytes[index];
        y = right->bytes[index];

        if (x != y)
        {
            return x < y ? -1 : 1;
        }
    }

    if (index < right->length)
    {
        return -1;
    }

    return 0;
}

static lngrd_UInt hash_string(const lngrd_String *string)
{
    lngrd_UInt hash;
    size_t index;

    hash = 0;

    for (index = 0; index < string->length; index++)
    {
        hash += string->bytes[index++];
    }

    return hash;
}

static int is_keyword_match(const lngrd_String *string, const char *keyword)
{
    size_t index;

    for (index = 0; keyword[index]; index++)
    {
        if (index >= string->length || string->bytes[index] != keyword[index])
        {
            return 0;
        }
    }

    return index == string->length;
}

static void destroy_string(lngrd_String *string)
{
    if (string->bytes)
    {
        free(string->bytes);
    }

    free(string);
}

static lngrd_List *create_list(void)
{
    lngrd_List *list;

    list = (lngrd_List *) allocate(1, sizeof(lngrd_List));
    list->items = (lngrd_Block **) allocate(8, sizeof(lngrd_Block *));
    list->length = 0;
    list->capacity = 8;

    return list;
}

static void push_list_item(lngrd_List *list, lngrd_Block *item)
{
    if (list->length == list->capacity)
    {
        if (list->capacity < 1073741824L)
        {
            list->capacity *= 2;
        }
        else if (list->capacity == 1073741824L)
        {
            list->capacity = LNGRD_INT_LIMIT;
        }
        else
        {
            crash_with_message("oversized memory requested");
        }

        list->items = (lngrd_Block **) reallocate(list->items, list->capacity, sizeof(lngrd_Block *));
    }

    list->items[list->length++] = item;
}

static lngrd_Block *pop_list_item(lngrd_List *list)
{
    return list->items[--list->length];
}

static lngrd_Block *peek_list_item(lngrd_List *list)
{
    return list->items[list->length - 1];
}

static lngrd_List *copy_list(lngrd_List *list)
{
    lngrd_List *sorted;
    size_t index;

    sorted = (lngrd_List *) allocate(1, sizeof(lngrd_List));
    sorted->items = (lngrd_Block **) allocate(list->capacity, sizeof(lngrd_Block *));
    sorted->length = list->length;
    sorted->capacity = list->capacity;

    for (index = 0; index < sorted->length; index++)
    {
        sorted->items[index] = list->items[index];
        sorted->items[index]->references += 1;
    }

    return sorted;
}

static void sort_list(lngrd_List *list, lngrd_List *sketches, lngrd_List *doodles)
{
    size_t cursor, swap;

    for (cursor = 1; cursor < list->length; cursor++)
    {
        for (swap = cursor - 1;; swap--)
        {
            lngrd_Block *left, *right;

            left = list->items[swap];
            right = list->items[swap + 1];

            if (compare_blocks(left, right, sketches, doodles) <= 0)
            {
                break;
            }

            list->items[swap] = right;
            list->items[swap + 1] = left;

            if (swap == 0)
            {
                break;
            }
        }
    }
}

static void burn_list(lngrd_List *list, lngrd_List *pyre)
{
    while (list->length > 0)
    {
        push_list_item(pyre, pop_list_item(list));
    }

    free(list->items);
    free(list);
}

static lngrd_Map *create_map(void)
{
    lngrd_Map *map;
    size_t index;

    map = (lngrd_Map *) allocate(1, sizeof(lngrd_Map));
    map->items = (lngrd_Pair *) allocate(8, sizeof(lngrd_Pair));
    map->length = 0;
    map->capacity = 8;

    for (index = 0; index < map->capacity; index++)
    {
        map->items[index].phase = LNGRD_PAIR_PHASE_UNCLAIMED;
    }

    return map;
}

static lngrd_Block *get_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_List *sketches, lngrd_List *doodles)
{
    size_t index, tally;

    index = hash_block(key) % map->capacity;

    for (tally = 0; tally < map->capacity; tally++, index++)
    {
        lngrd_Pair *pair;

        if (index == map->capacity)
        {
            index = 0;
        }

        pair = &map->items[index];

        if (pair->phase == LNGRD_PAIR_PHASE_OCCUPIED)
        {
            if (compare_blocks(key, pair->key, sketches, doodles) == 0)
            {
                return pair->value;
            }
        }
        else if (pair->phase == LNGRD_PAIR_PHASE_VACANT)
        {
            continue;
        }
        else
        {
            break;
        }
    }

    return NULL;
}

static void set_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_Block *block, lngrd_List *pyre, lngrd_List *sketches, lngrd_List *doodles)
{
    size_t index, tally;

    if (map->length == map->capacity)
    {
        lngrd_Pair *oldItems;
        size_t oldLength, index;

        oldItems = map->items;
        oldLength = map->length;

        if (map->capacity < 1073741824L)
        {
            map->capacity *= 2;
        }
        else if (map->capacity == 1073741824L)
        {
            map->capacity = LNGRD_INT_LIMIT;
        }
        else
        {
            crash_with_message("oversized memory requested");
        }

        map->length = 0;
        map->items = (lngrd_Pair *) allocate(map->capacity, sizeof(lngrd_Pair));

        for (index = 0; index < map->capacity; index++)
        {
            map->items[index].phase = LNGRD_PAIR_PHASE_UNCLAIMED;
        }

        for (index = 0; index < oldLength; index++)
        {
            lngrd_Pair *item;

            item = &oldItems[index];

            if (item->phase == LNGRD_PAIR_PHASE_OCCUPIED)
            {
                set_map_item(map, item->key, item->value, pyre, sketches, doodles);
            }
        }

        free(oldItems);
    }

    index = hash_block(key) % map->capacity;

    for (tally = 0; tally < map->capacity; tally++, index++)
    {
        lngrd_Pair *pair;

        if (index == map->capacity)
        {
            index = 0;
        }

        pair = &map->items[index];

        if (pair->phase == LNGRD_PAIR_PHASE_OCCUPIED)
        {
            if (compare_blocks(key, pair->key, sketches, doodles) == 0)
            {
                push_list_item(pyre, pair->key);
                push_list_item(pyre, pair->value);
                pair->key = key;
                pair->value = block;

                break;
            }
        }
        else
        {
            map->length += 1;
            pair->key = key;
            pair->value = block;
            pair->phase = LNGRD_PAIR_PHASE_OCCUPIED;

            break;
        }
    }
}

static void unset_map_item(lngrd_Map *map, lngrd_Block *key, lngrd_List *pyre, lngrd_List *sketches, lngrd_List *doodles)
{
    size_t index, tally;

    index = hash_block(key) % map->capacity;

    for (tally = 0; tally < map->capacity; tally++, index++)
    {
        lngrd_Pair *pair;

        if (index == map->capacity)
        {
            index = 0;
        }

        pair = &map->items[index];

        if (pair->phase == LNGRD_PAIR_PHASE_OCCUPIED)
        {
            if (compare_blocks(key, pair->key, sketches, doodles) == 0)
            {
                push_list_item(pyre, pair->key);
                push_list_item(pyre, pair->value);
                pair->phase = LNGRD_PAIR_PHASE_VACANT;

                break;
            }
        }
        else if (pair->phase == LNGRD_PAIR_PHASE_VACANT)
        {
            continue;
        }
        else
        {
            return;
        }
    }
}

static void burn_map(lngrd_Map *map, lngrd_List *pyre)
{
    size_t index;

    for (index = 0; index < map->capacity; index++)
    {
        lngrd_Pair item;

        item = map->items[index];

        if (item.phase == LNGRD_PAIR_PHASE_OCCUPIED)
        {
            push_list_item(pyre, item.key);
            push_list_item(pyre, item.value);
        }
    }

    free(map->items);
    free(map);
}

static lngrd_Function *create_function(void)
{
    lngrd_Function *function;

    function = (lngrd_Function *) allocate(1, sizeof(lngrd_Function));
    function->expressions = create_list();
    function->source = NULL;
    function->inlined = 0;

    return function;
}

static lngrd_SInt compare_functions(const lngrd_Function *left, const lngrd_Function *right)
{
    return compare_strings(left->source, right->source);
}

static void burn_function(lngrd_Function *function, lngrd_List *pyre)
{
    burn_list(function->expressions, pyre);
    destroy_string(function->source);

    free(function);
}

static lngrd_Expression *create_expression(lngrd_ExpressionType type, void *form)
{
    lngrd_Expression *expression;

    expression = (lngrd_Expression *) allocate(1, sizeof(lngrd_Expression));
    expression->type = type;
    expression->form = form;

    return expression;
}

static void burn_expression(lngrd_Expression *expression, lngrd_List *pyre)
{
    switch (expression->type)
    {
        case LNGRD_EXPRESSION_TYPE_LITERAL:
        {
            lngrd_LiteralForm *form;

            form = (lngrd_LiteralForm *) expression->form;
            push_list_item(pyre, form->block);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_LOOKUP:
        {
            lngrd_LookupForm *form;

            form = (lngrd_LookupForm *) expression->form;
            push_list_item(pyre, form->name);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_ASSIGN:
        {
            lngrd_AssignForm *form;

            form = (lngrd_AssignForm *) expression->form;
            push_list_item(pyre, form->name);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_UNASSIGN:
        {
            lngrd_UnassignForm *form;

            form = (lngrd_UnassignForm *) expression->form;
            push_list_item(pyre, form->name);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_INVOKE:
        {
            lngrd_InvokeForm *form;
            lngrd_List *arguments;

            form = (lngrd_InvokeForm *) expression->form;
            arguments = form->arguments;

            while (arguments->length > 0)
            {
                push_list_item(pyre, pop_list_item(arguments));
            }

            free(arguments->items);
            free(arguments);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_BRANCH:
        {
            lngrd_BranchForm *form;

            form = (lngrd_BranchForm *) expression->form;

            if (form->test)
            {
                push_list_item(pyre, form->test);
            }

            if (form->pass)
            {
                push_list_item(pyre, form->pass);
            }

            if (form->fail)
            {
                push_list_item(pyre, form->fail);
            }

            break;
        }

        case LNGRD_EXPRESSION_TYPE_LOOP:
        {
            lngrd_LoopForm *form;

            form = (lngrd_LoopForm *) expression->form;

            if (form->test)
            {
                push_list_item(pyre, form->test);
            }

            if (form->body)
            {
                push_list_item(pyre, form->body);
            }

            break;
        }

        case LNGRD_EXPRESSION_TYPE_CATCH:
        {
            lngrd_CatchForm *form;

            form = (lngrd_CatchForm *) expression->form;

            if (form->failable)
            {
                push_list_item(pyre, form->failable);
            }

            break;
        }

        case LNGRD_EXPRESSION_TYPE_THROW:
        {
            lngrd_ThrowForm *form;

            form = (lngrd_ThrowForm *) expression->form;

            if (form->error)
            {
                push_list_item(pyre, form->error);
            }

            break;
        }

        case LNGRD_EXPRESSION_TYPE_ARGUMENT:
        {
            lngrd_ArgumentForm *form;

            form = (lngrd_ArgumentForm *) expression->form;

            if (form->index)
            {
                push_list_item(pyre, form->index);
            }

            break;
        }

        case LNGRD_EXPRESSION_TYPE_GROUP:
        {
            lngrd_GroupForm *form;
            lngrd_List *expressions;

            form = (lngrd_GroupForm *) expression->form;
            expressions = form->expressions;

            while (expressions->length > 0)
            {
                push_list_item(pyre, pop_list_item(expressions));
            }

            free(expressions->items);
            free(expressions);

            break;
        }

        case LNGRD_EXPRESSION_TYPE_NATIVE:
            break;

        default:
            crash_with_message("unsupported branch");
    }

    free(expression->form);
    free(expression);
}

static lngrd_Plan *create_plan(void)
{
    lngrd_Plan *plan;

    plan = (lngrd_Plan *) allocate(1, sizeof(lngrd_Plan));
    plan->actions = (lngrd_Action *) allocate(8, sizeof(lngrd_Action));
    plan->length = 0;
    plan->capacity = 8;

    return plan;
}

static void push_plan_action(lngrd_Plan *plan, lngrd_Action action)
{
    if (plan->length == plan->capacity)
    {
        if (plan->capacity < 1073741824L)
        {
            plan->capacity *= 2;
        }
        else if (plan->capacity == 1073741824L)
        {
            plan->capacity = LNGRD_INT_LIMIT;
        }
        else
        {
            crash_with_message("oversized memory requested");
        }

        plan->actions = (lngrd_Action *) reallocate(plan->actions, plan->capacity, sizeof(lngrd_Action));
    }

    plan->actions[plan->length++] = action;
}

static lngrd_Action pop_plan_action(lngrd_Plan *plan)
{
    return plan->actions[--plan->length];
}

static lngrd_Action peek_plan_action(lngrd_Plan *plan)
{
    return plan->actions[plan->length - 1];
}

static void destroy_plan(lngrd_Plan *plan)
{
    free(plan->actions);
    free(plan);
}

static lngrd_Action prepare_simple_action(lngrd_Block *single, lngrd_UInt checkpoint, lngrd_UInt capacity)
{
    lngrd_Action action;

    action.single = single;
    action.expressions = NULL;
    action.index = 0;
    action.ownership = 0;
    action.phase = 0;
    action.checkpoint = checkpoint;
    action.capacity = capacity;
    action.provisioned = 0;

    return action;
}

static lngrd_Action prepare_general_action(lngrd_List *expressions, lngrd_UInt ownership, lngrd_UInt checkpoint, lngrd_UInt capacity)
{
    lngrd_Action action;

    action.single = NULL;
    action.expressions = expressions;
    action.index = 0;
    action.ownership = ownership;
    action.phase = 0;
    action.checkpoint = checkpoint;
    action.capacity = capacity;
    action.provisioned = 0;

    return action;
}

static void start_buffer(lngrd_Buffer *buffer)
{
    size_t capacity;

    capacity = 8;
    buffer->bytes = (char *) allocate(capacity, sizeof(char));
    buffer->length = 0;
    buffer->capacity = capacity;
}

static int append_buffer_bytes(lngrd_Buffer *buffer, const lngrd_String *string)
{
    int resize;

    if (string->length == 0)
    {
        return 1;
    }

    if (!can_fit_both(buffer->length, string->length))
    {
        return 0;
    }

    resize = 0;

    while (buffer->length + string->length > buffer->capacity)
    {
        if (buffer->capacity < 1073741824L)
        {
            buffer->capacity *= 2;
        }
        else if (buffer->capacity == 1073741824L)
        {
            buffer->capacity = LNGRD_INT_LIMIT;
        }
        else
        {
            return 0;
        }

        resize = 1;
    }

    if (resize)
    {
        buffer->bytes = (char *) reallocate(buffer->bytes, buffer->capacity, sizeof(char));
    }

    memcpy(buffer->bytes + buffer->length, string->bytes, string->length);
    buffer->length += string->length;

    return 1;
}

static lngrd_String *buffer_to_string(lngrd_Buffer *buffer)
{
    lngrd_String *string;

    string = (lngrd_String *) allocate(1, sizeof(lngrd_String));
    string->length = buffer->length;

    if (buffer->length > 0)
    {
        string->bytes = (char *) reallocate(buffer->bytes, buffer->length, sizeof(char));
    }
    else
    {
        string->bytes = NULL;
        free(buffer->bytes);
    }

    return string;
}

static void abandon_buffer(lngrd_Buffer *buffer)
{
    free(buffer->bytes);
}

static int can_fit_both(size_t left, size_t right)
{
    return (left + right) >= left;
}

static void *allocate(size_t number, size_t size)
{
    void *memory;

    if (number == 0 || size == 0)
    {
        crash_with_message("zero memory requested");
    }

    if (number > LNGRD_SIZE_LIMIT / size)
    {
        crash_with_message("oversized memory requested");
    }

    memory = malloc(number * size);

    if (!memory)
    {
        crash_with_message("memory allocation failed");
    }

    return memory;
}

static void *reallocate(void *memory, size_t number, size_t size)
{
    if (number == 0 || size == 0)
    {
        crash_with_message("zero memory requested");
    }

    if (number > LNGRD_SIZE_LIMIT / size)
    {
        crash_with_message("oversized memory requested");
    }

    memory = realloc(memory, number * size);

    if (!memory)
    {
        crash_with_message("memory allocation failed");
    }

    return memory;
}

static void crash_with_message(const char *message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}

#endif

#ifdef __cplusplus
}
#endif

#endif
