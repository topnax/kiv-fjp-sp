
%{
#include <cstdio>
#include "ast.h"

extern int yylex();
int yyerror(const char *s);

program* ast_root = NULL;

extern FILE *yyin, *yyout;
bool verbose_out = false;

#define YY_NO_UNISTD_H

%}

%union{
    char str_literal[200];
    int type;
    int int_literal;
    char identifier[30];
    char l_op[5];
    char a_op[5];
    char b_op[5];
    char comparsion[5];
    char parenthesis;
    char bracket;
    char b_not;

    program* prog;
    std::list<global_statement*>* global_statement;
    function_declaration* function_declaration;
    variable_declaration* variable_declaration;
    declaration* my_declaration;
    value* my_value;
    arithmetic* arithmetic;
    function_call* function_call;
    std::list<value*>* expressions;
    expression* expression;
    command* my_command;
    std::list<command*>* commands;
    block* block;
    std::list<declaration*>* parameters;
    boolean_expression* boolean_expression;
    condition* condition;
    loop* loop;
    std::list<declaration*>* multi_declaration;
    struct_definition* struct_def;
}

%define parse.error verbose

%token STR_LITERAL INT_LITERAL OTHER SEMICOLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP COMPARSION
%token ASSIGN NOT
%token B_L_CURLY B_R_CURLY B_L_SQUARE B_R_SQUARE PAREN_L PAREN_R

// reserved keywords
%token STRUCT WHILE FOR IF ELSE CONST RETURN COMMA DOT TRUE FALSE

%token END 0 "end of file"

%type <prog> prog
%type <global_statement> global
%type <function_declaration> function
%type <variable_declaration> variable
%type <my_declaration> declaration
%type <my_value> value
%type <arithmetic> arithmetic
%type <function_call> function_call
%type <expressions> expressions
%type <expression> expression
%type <my_command> command
%type <commands> commands
%type <block> block
%type <parameters> parameters
%type <boolean_expression> boolean_expression
%type <loop> loop
%type <condition> condition
%type <multi_declaration> multi_declaration
%type <struct_def> struct_def
%type <str_literal> STR_LITERAL
%type <int_literal> INT_LITERAL
%type <identifier> IDENTIFIER
%type <type> TYPE
%type <l_op> L_OP
%type <a_op> A_OP
%type <b_op> B_OP
%type <comparsion> COMPARSION;
%type <b_not> NOT;

%type <bracket> B_L_CURLY;
%type <bracket> B_R_CURLY;
%type <bracket> B_L_SQUARE;
%type <bracket> B_R_SQUARE;

%type <parenthesis> PAREN_L;
%type <parenthesis> PAREN_R;

%start prog

%%

prog:
    global {
        $$ = new program($1); ast_root = $$;
    }
;

global:
    global function {
        $$ = $1;
        $1->push_back($2);
    }
    | global variable {
        $$ = $1;
        $1->push_back($2);
    }
    | global struct_def {
        $$ = $1;
        $1->push_back($2);
    }
    | {
        $$ = new std::list<global_statement*>();
    }
;

declaration:
    TYPE IDENTIFIER {
        printf("variable declaration: type=%d identifier=%s\n", $1, $2);
        $$ = new declaration($1, $2);
    }
    | TYPE IDENTIFIER B_L_SQUARE INT_LITERAL B_R_SQUARE {
        printf("variable array declaration: type=%d identifier=%s size=%d\n", $1, $2, $4);
        $$ = new declaration($1, $2, $4);
    }
    | STRUCT IDENTIFIER IDENTIFIER {
        printf("variable structure %s: identifier=%s\n", $2, $3);
        $$ = new declaration(TYPE_META_STRUCT, $2, $3);
    }
;

multi_declaration:
    multi_declaration declaration SEMICOLON {
        printf("struct non-terminal declaration\n");
        $$ = $1;
        $1->push_back($2);
    }
    | {
        printf("struct terminal declaration\n");
        $$ = new std::list<declaration*>();
    }
;

variable:
    declaration SEMICOLON {
        printf("got variable declaration\n");
        $$ = new variable_declaration($1);
    } 
    | declaration ASSIGN value SEMICOLON {
        printf("got variable delcaration with value assignment\n");
        $$ = new variable_declaration($1, false, $3);
    }
    | CONST declaration ASSIGN value SEMICOLON {
        printf("got constant delcaration with value assignment\n");
        $$ = new variable_declaration($2, true, $4);
    }
;

struct_def:
    STRUCT IDENTIFIER B_L_CURLY multi_declaration B_R_CURLY SEMICOLON {
        printf("got structure definition\n");
        $$ = new struct_definition($2, $4);
    }
;

condition:
    IF PAREN_L expression PAREN_R block {
        printf("got if condition (without else)\n");
        $$ = new condition($3, $5);
    }
    | IF PAREN_L expression PAREN_R block ELSE block {
        printf("got if condition (with else)\n");
        $$ = new condition($3, $5, $7);
    }
;

loop:
    WHILE PAREN_L expression PAREN_R block {
        printf("got while loop\n");
        $$ = new while_loop($3, $5);
    }
    | FOR PAREN_L expression SEMICOLON expression SEMICOLON expression PAREN_R block {
        printf("got for loop\n");
        $$ = new for_loop($3, $5, $7, $9);
    }
;

arithmetic:
    value A_OP value {
        printf("operator %s\n", $2);
        $$ = new arithmetic($1, arithmetic::str_to_op($2), $3);
    }
;

expressions:
    expressions COMMA value {
        printf("expression list as a function call parameter (recursive)\n");
        $$ = $1;
        $1->push_back($3);
    }
    | value {
        printf("expression list as a function call parameter (single)\n");
        $$ = new std::list<value*>{$1};
    }
;

function_call:
    IDENTIFIER PAREN_L PAREN_R {
        printf("non-parametric function call\n");
        $$ = new function_call($1);
    }
    | IDENTIFIER PAREN_L expressions PAREN_R {
        printf("parametric function call\n");
        $$ = new function_call($1, $3);
    }
;

value:
    INT_LITERAL {
        printf("got int literal as value\n");
        $$ = new value($1);
    }
    | STR_LITERAL {
        printf("got str literal as value\n");
        $$ = new value($1);
    }
    | IDENTIFIER {
        printf("got scalar identifier %s as value\n", $1);
        $$ = new value(new variable_ref($1));
    }
    | arithmetic {
        printf("got arithmetic as value\n");
        $$ = new value($1);
    }
    | function_call {
        printf("got function call as value\n");
        $$ = new value($1);
    }
    | IDENTIFIER DOT IDENTIFIER {
        printf("got struct %s member %s as value\n", $1, $3);
        $$ = new value($1, $3);
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE {
        printf("got array identifier as value\n");
        $$ = new value($1, $3);
    }
;

expression:
    IDENTIFIER ASSIGN value {
        printf("assigning value to %s\n", $1);
        $$ = new assign_expression($1, (value*)nullptr, $3);
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE ASSIGN value {
        printf("assigning value to array element %s\n", $1);
        $$ = new assign_expression($1, $3, $6);
    }
    | IDENTIFIER DOT IDENTIFIER ASSIGN value {
        printf("assigning to struct %s member %s\n", $1, $3);
        $$ = new assign_expression($1, $3, $5);
    }
    | value {
        printf("got value (expression)\n");
        $$ = new expression($1);
    }
    | boolean_expression {
        printf("got boolean expression\n");
        $$ = new expression($1);
    }
;

command:
    variable {
        printf("got var decl command\n");
        $$ = new command($1);
    }
    | expression SEMICOLON {
        printf("got expression decl command\n");
        $$ = new command($1, false);
    }
    | RETURN expression SEMICOLON {
        printf("got return statement\n");
        $$ = new command($2, true);
    }
    | loop {
        printf("got loop command\n");
        $$ = new command($1);
    }
    | condition {
        printf("got condition command\n");
        $$ = new command($1);
    }
;

commands:
    commands command {
        printf("got a non-terminal command definition\n");
        $$ = $1;
        $1->push_back($2);
    }
    | {
        $$ = new std::list<command*>();
    }
;

block:
    B_L_CURLY B_R_CURLY {
        printf("got an empty block\n");
        $$ = new block(nullptr);
    }
    | B_L_CURLY commands B_R_CURLY {
        printf("got a multi-line block\n");
        $$ = new block($2);
    }
    | command {
        printf("got a single-line block\n");
        std::list<command*>* lst = new std::list<command*>();
        lst->push_back($1);
        $$ = new block(lst);
    }
;

parameters:
    declaration {
        printf("got a terminal function parameter definition\n");
        $$ = new std::list<declaration*>{ $1 };
    }
    | parameters COMMA declaration {
        printf("got a non-terminal function parameter definition\n");
        $$ = $1;
        $1->push_back($3);
    }
;

function:
    declaration PAREN_L PAREN_R block {
        printf("got a function definition (no params)\n");
        $$ = new function_declaration($1, $4);
    }
    | declaration PAREN_L parameters PAREN_R block {
        printf("got a function definition (with params)\n");
        $$ = new function_declaration($1, $5, $3);
    }
    | declaration PAREN_L PAREN_R SEMICOLON {
        printf("got a function forward declaration (no params)\n");
        $$ = new function_declaration($1, nullptr);
    }
    | declaration PAREN_L parameters PAREN_R SEMICOLON {
        printf("got a function forward declaration (with params)\n");
        $$ = new function_declaration($1, nullptr, $3);
    }
;

boolean_expression:
    value {
        printf("got value (boolean expression)\n");
        $$ = new boolean_expression($1);
    }
    | value COMPARSION value {
        printf("got compare (boolean expression)\n");
        $$ = new boolean_expression($1, $3, boolean_expression::str_to_bool_op($2));
    }
    | NOT boolean_expression {
        printf("got negation (boolean expression)\n");
        $$ = new boolean_expression($2, nullptr, boolean_expression::str_to_bool_op($1));
    }
    | PAREN_L boolean_expression PAREN_R {
        printf("got enclosed expression (boolean expression)\n");
        $$ = $2;
    }
    | boolean_expression L_OP boolean_expression {
        printf("got boolean expression (boolean expression)\n");
        $$ = new boolean_expression($1, $3, boolean_expression::str_to_bool_op($2));
    }
    | TRUE {
        printf("got true (boolean expression)\n");
        $$ = new boolean_expression(true);
    }
    | FALSE {
        printf("got false (boolean expression)\n");
        $$ = new boolean_expression(false);
    }
;

%%

int yyerror(const char *s)
{
	printf("Error: %s\n", s);
	return 0;
}

int parse(FILE* input, FILE* output)
{
    yyin = input;
    yyout = output;

    return yyparse();
}

