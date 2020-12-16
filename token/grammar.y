
%{
#include <stdio.h>

int yylex();
int yyerror(const char *s);

extern FILE *yyin, *yyout;

%}

%define parse.error verbose

%token STR_LITERAL INT_LITERAL OTHER SEMICOLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP COMPARSION
%token ASSIGN NOT
%token B_L_CURLY B_R_CURLY B_L_SQUARE B_R_SQUARE PAREN_L PAREN_R

// reserved keywords
%token STRUCT WHILE FOR IF ELSE CONST RETURN COMMA DOT TRUE FALSE

%token END 0 "end of file"


%type <str_literal> STR_LITERAL
%type <int_literal> INT_LITERAL
%type <identifier> IDENTIFIER
%type <type> TYPE
%type <l_op> L_OP
%type <a_op> A_OP
%type <b_op> B_OP
%type <comparsion> COMPARSION;

%type <bracket> B_L_CURLY;
%type <bracket> B_R_CURLY;
%type <bracket> B_L_SQUARE;
%type <bracket> B_R_SQUARE;

%type <parenthesis> PAREN_L;
%type <parenthesis> PAREN_R;


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
}

%%

prog:
    | prog function
    | prog variable
    | END
;

declaration:
    | TYPE IDENTIFIER {
        printf("variable declaration: type=%d identifier=%s\n", $1, $2);
    }
    | TYPE IDENTIFIER B_L_SQUARE INT_LITERAL B_R_SQUARE {
        printf("variable array declaration: type=%d identifier=%s size=%d\n", $1, $2, $4);
    }
    | STRUCT IDENTIFIER IDENTIFIER {
        printf("variable structure %s: identifier=%s\n", $2, $3);
    }
;

value:
    | INT_LITERAL {
        printf("got int literal as value\n");
    }
    | STR_LITERAL {
        printf("got str literal as value\n");
    }
    | IDENTIFIER {
        printf("got scalar identifier as value\n");
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE {
        printf("got array identifier as value\n");
    }
;

multi_declaration:
    | declaration SEMICOLON {
        printf("struct terminal declaration\n");
    }
    | multi_declaration declaration SEMICOLON {
        printf("struct non-terminal declaration\n");
    }
;

variable:
    | declaration SEMICOLON {
        printf("got variable declaration\n");
    } 
    | declaration ASSIGN value SEMICOLON {
        printf("got variable delcaration with value assignment\n");
    }
    | CONST declaration ASSIGN value SEMICOLON {
        printf("got constant delcaration with value assignment\n");
    }
    | STRUCT IDENTIFIER B_L_CURLY multi_declaration B_R_CURLY SEMICOLON {
        printf("got structure definition\n");
    }
;

condition:
    | IF PAREN_L expression PAREN_R block {
        printf("got if condition (without else)\n");
    }
    | IF PAREN_L expression PAREN_R block ELSE block {
        printf("got if condition (with else)\n");
    }
;

loop:
    | WHILE PAREN_L expression PAREN_R block {
        printf("got while loop\n");
    }
    | FOR PAREN_L expression SEMICOLON expression SEMICOLON expression PAREN_R block {
        printf("got for loop\n");
    }
;

arithmetic:
    | value A_OP value {
        printf("operator %s\n", $2);
    }
;

expressions:
    | expression {
        printf("expression as a function call parameter\n");
    }
    | expressions COMMA expression {
        printf("expression list as a function call parameter\n");
    }
;

function_call:
    | IDENTIFIER PAREN_L PAREN_R {
        printf("non-parametric function call\n");
    }
    | IDENTIFIER PAREN_L expressions PAREN_R {
        printf("parametric function call\n");
    }
;

value:
    | INT_LITERAL {
        printf("got int literal as value\n");
    }
    | STR_LITERAL {
        printf("got str literal as value\n");
    }
    | IDENTIFIER {
        printf("got identifier as value\n");
    }
    | arithmetic {
        printf("got arithmetic as value\n");
    }
    | function_call {
        printf("got function call as value\n");
    }
    | IDENTIFIER DOT IDENTIFIER {
        printf("got struct %s member %s as value\n", $1, $3);
    }
    // todo: array
;

expression:
    | IDENTIFIER ASSIGN value {
        printf("assigning value to %s\n", $1);
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE ASSIGN value {
        printf("assigning value to array element %s\n", $1);
    }
    | IDENTIFIER DOT IDENTIFIER ASSIGN value {
        printf("assigning to struct %s member %s\n", $1, $3);
    }
    | value {
        printf("got value (expression)\n");
    }
    | function_call {
        printf("got function call (expression)\n");
    }
    | boolean_expression {
        printf("got boolean expression\n");
    }
;

command:
    | variable {
        printf("got var decl command\n");
    }
    | expression SEMICOLON {
        printf("got expression decl command\n");
    }
    | RETURN expression SEMICOLON {
        printf("got return statement\n");
    }
    | loop {
        printf("got loop command\n");
    }
    | condition {
        printf("got condition command\n");
    }
;

commands:
    | command {
        printf("got a terminal command definition\n");
    }
    | commands command {
        printf("got a non-terminal command definition\n");
    }
    | END
;

block:
    | B_L_CURLY commands B_R_CURLY {
        printf("got a multi-line block\n");
    }
    | command {
        printf("got a single-line block\n");
    }
    | END
;

parameters:
    | declaration {
        printf("got a terminal function parameter definition\n");
    }
    | parameters COMMA declaration {
        printf("got a non-terminal function parameter definition\n");
    }
    | END
;

function:
    | declaration PAREN_L PAREN_R block {
        printf("got a function definition (no params)\n");
    }
    | declaration PAREN_L parameters PAREN_R block {
        printf("got a function definition (with params)\n");
    }
;

boolean_expression:
    | value {
        printf("got value (boolean expression)\n");
    }
    | value COMPARSION value {
        printf("got compare (boolean expression)\n");
    }
    | NOT boolean_expression {
        printf("got negation (boolean expression)\n");
    }
    | PAREN_L boolean_expression PAREN_R {
        printf("got enclosed expression (boolean expression)\n");
    }
    | boolean_expression L_OP boolean_expression {
        printf("got boolean expression (boolean expression)\n");
    }
;

reserved:
        STRUCT
        | WHILE
        | FOR
        | IF
        | ELSE
        | CONST
        | RETURN
        | TRUE
        | FALSE
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

    yyparse();
}

