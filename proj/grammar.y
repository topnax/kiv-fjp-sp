
%{
#include <cstdio>
#include <iostream>
#include "ast.h"

extern "C" int yylex();
int yyerror(const char *s);

bool print_symbol_match = false;
program* ast_root = NULL;

extern FILE *yyin, *yyout;
bool verbose_out = false;

static void _print_symbol_match(const std::string& symbol, const std::string& str) {
    if (print_symbol_match) {
        std::cout << "[" << symbol << "]: " << str << std::endl;
    }
}

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
    assign_expression* assign_expression;
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

%token STR_LITERAL INT_LITERAL OTHER SEMICOLON QUESTION COLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP_PM A_OP_TD COMPARSION
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
%type <assign_expression> assign_expression
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
%type <a_op> A_OP_PM
%type <a_op> A_OP_TD
%type <b_op> B_OP
%type <comparsion> COMPARSION;
%type <b_not> NOT;

%type <bracket> B_L_CURLY;
%type <bracket> B_R_CURLY;
%type <bracket> B_L_SQUARE;
%type <bracket> B_R_SQUARE;

%type <parenthesis> PAREN_L;
%type <parenthesis> PAREN_R;

%left L_OP
%left COMPARSION
%left A_OP_PM
%left A_OP_TD
%left NOT

%start prog

%%

prog:
    global {
        $$ = new program($1);
        ast_root = $$;
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
        _print_symbol_match("declaration", "variable declaration: type=" + std::to_string($1) + " identifier=" + std::string($2));
        $$ = new declaration($1, $2);
    }
    | TYPE IDENTIFIER B_L_SQUARE INT_LITERAL B_R_SQUARE {
        _print_symbol_match("declaration", "array declaration: type=" + std::to_string($1) + " identifier=" + std::string($2) + " size=" + std::to_string($4));
        $$ = new declaration($1, $2, $4);
    }
    | STRUCT IDENTIFIER IDENTIFIER {
        _print_symbol_match("declaration", "variable structure: struct name=" + std::string($2) + " identifier=" + std::string($3));
        $$ = new declaration(TYPE_META_STRUCT, $2, $3);
    }
;

multi_declaration:
    multi_declaration declaration SEMICOLON {
        _print_symbol_match("multi_declaration", "non-terminal declaration");
        $$ = $1;
        $1->push_back($2);
    }
    | {
        _print_symbol_match("multi_declaration", "terminal declaration");
        $$ = new std::list<declaration*>();
    }
;

variable:
    declaration SEMICOLON {
        _print_symbol_match("variable", "variable declaration");
        $$ = new variable_declaration($1);
    } 
    | declaration ASSIGN value SEMICOLON {
        _print_symbol_match("variable", "variable declaration with initialization");
        $$ = new variable_declaration($1, false, $3);
    }
    | CONST declaration ASSIGN value SEMICOLON {
        _print_symbol_match("variable", "constant declaration with initialization");
        $$ = new variable_declaration($2, true, $4);
    }
;

struct_def:
    STRUCT IDENTIFIER B_L_CURLY multi_declaration B_R_CURLY SEMICOLON {
        _print_symbol_match("struct_def", "structure definition");
        $$ = new struct_definition($2, $4);
    }
;

condition:
    IF PAREN_L value PAREN_R block {
        _print_symbol_match("condition", "if condition (without else)");
        $$ = new condition($3, $5);
    }
    | IF PAREN_L value PAREN_R block ELSE block {
        _print_symbol_match("condition", "if condition (with else)");
        $$ = new condition($3, $5, $7);
    }
;

loop:
    WHILE PAREN_L value PAREN_R block {
        _print_symbol_match("loop", "while loop");
        $$ = new while_loop($3, $5);
    }
    | FOR PAREN_L expression SEMICOLON value SEMICOLON expression PAREN_R block {
        _print_symbol_match("loop", "for loop");
        $$ = new for_loop($3, $5, $7, $9);
    }
;

arithmetic:
    value A_OP_PM value {
        _print_symbol_match("arithmetic", "arithmetic expression (+, -), A " + std::string($2) + " B");
        $$ = new arithmetic($1, arithmetic::str_to_op($2), $3);
    }
    | value A_OP_TD value {
        _print_symbol_match("arithmetic", "arithmetic expression (*, /), A " + std::string($2) + " B");
        $$ = new arithmetic($1, arithmetic::str_to_op($2), $3);
    }
;

expressions:
    expressions COMMA value {
        _print_symbol_match("expressions", "expression list as a function call parameter (multiple parameters)");
        $$ = $1;
        $1->push_back($3);
    }
    | value {
        _print_symbol_match("expressions", "expression list as a function call parameter (single parameter)");
        $$ = new std::list<value*>{$1};
    }
;

function_call:
    IDENTIFIER PAREN_L PAREN_R {
        _print_symbol_match("function_call", "non-parametric function call");
        $$ = new function_call($1);
    }
    | IDENTIFIER PAREN_L expressions PAREN_R {
        _print_symbol_match("function_call", "parametric function call");
        $$ = new function_call($1, $3);
    }
;

value:
    INT_LITERAL {
        _print_symbol_match("value", "integer literal '" + std::to_string($1) + "'");
        $$ = new value($1);
    }
    | STR_LITERAL {
        _print_symbol_match("value", "string literal '" + std::string($1) + "'");
        $$ = new value($1);
    }
    | IDENTIFIER {
        _print_symbol_match("value", "scalar identifier '" + std::string($1) + "'");
        $$ = new value(new variable_ref($1));
    }
    | arithmetic {
        _print_symbol_match("value", "arithmetic expression");
        $$ = new value($1);
    }
    | function_call {
        _print_symbol_match("value", "function call");
        $$ = new value($1);
    }
    | boolean_expression {
        _print_symbol_match("value", "boolean expression");
        $$ = new value($1);
    }
    | boolean_expression QUESTION value COLON value {
        _print_symbol_match("value", "ternary operator");
        $$ = new value($1, $3, $5);
    }
    | assign_expression {
        _print_symbol_match("value", "assign expression");
        $$ = new value($1);
    }
    | IDENTIFIER DOT IDENTIFIER {
        _print_symbol_match("value", "struct '" + std::string($1) + "' member '" + std::string($3) + "'");
        $$ = new value($1, $3);
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE {
        _print_symbol_match("value", "array '" + std::string($1) + "' (indexed)");
        $$ = new value(new variable_ref($1), $3);
    }
    | PAREN_L value PAREN_R {
        _print_symbol_match("value", "parenthesis enclosure");
        $$ = $2;
    }
;

assign_expression:
    IDENTIFIER ASSIGN value {
        _print_symbol_match("assign_expression", "scalar left-hand side '" + std::string($1) + "'");
        $$ = new assign_expression($1, (value*)nullptr, $3);
    }
    | IDENTIFIER B_L_SQUARE value B_R_SQUARE ASSIGN value {
        _print_symbol_match("assign_expression", "array element of left-hand side '" + std::string($1) + "'");
        $$ = new assign_expression($1, $3, $6);
    }
    | IDENTIFIER DOT IDENTIFIER ASSIGN value {
        _print_symbol_match("assign_expression", "struct '" + std::string($1) + "' member '" + std::string($3) + "' as left-hand side");
        $$ = new assign_expression($1, $3, $5);
    }
;

expression:
    value {
        _print_symbol_match("expression", "value expression (discarding return value)");
        $$ = new expression($1);
    }
;

command:
    variable {
        _print_symbol_match("command", "variable declaration");
        $$ = new command($1);
    }
    | expression SEMICOLON {
        _print_symbol_match("command", "expression");
        $$ = new command($1);
    }
    | RETURN value SEMICOLON {
        _print_symbol_match("command", "return statement");
        $$ = new command($2);
    }
    | loop {
        _print_symbol_match("command", "loop");
        $$ = new command($1);
    }
    | condition {
        _print_symbol_match("command", "condition");
        $$ = new command($1);
    }
;

commands:
    commands command {
        _print_symbol_match("commands", "non-terminal command list match");
        $$ = $1;
        $1->push_back($2);
    }
    | {
        _print_symbol_match("commands", "terminal command list match");
        $$ = new std::list<command*>();
    }
;

block:
    B_L_CURLY B_R_CURLY {
        _print_symbol_match("block", "empty block");
        $$ = new block(nullptr);
    }
    | B_L_CURLY commands B_R_CURLY {
        _print_symbol_match("block", "multi-line block");
        $$ = new block($2);
    }
    | command {
        _print_symbol_match("block", "single-line block");
        std::list<command*>* lst = new std::list<command*>();
        lst->push_back($1);
        $$ = new block(lst);
    }
;

parameters:
    declaration {
        _print_symbol_match("parameters", "terminal function parameters definition");
        $$ = new std::list<declaration*>{ $1 };
    }
    | parameters COMMA declaration {
        _print_symbol_match("parameters", "non-terminal function parameters definition");
        $$ = $1;
        $1->push_back($3);
    }
;

function:
    declaration PAREN_L PAREN_R block {
        _print_symbol_match("function", "non-parametric function definition");
        $$ = new function_declaration($1, $4);
    }
    | declaration PAREN_L parameters PAREN_R block {
        _print_symbol_match("function", "parametric function definition");
        $$ = new function_declaration($1, $5, $3);
    }
    | declaration PAREN_L PAREN_R SEMICOLON {
        _print_symbol_match("function", "non-parametric function forward declaration");
        $$ = new function_declaration($1, nullptr);
    }
    | declaration PAREN_L parameters PAREN_R SEMICOLON {
        _print_symbol_match("function", "parametric function forward declaration");
        $$ = new function_declaration($1, nullptr, $3);
    }
;

boolean_expression:
    value COMPARSION value {
        _print_symbol_match("boolean_expression", "comparison, A " + std::string($2) + " B");
        $$ = new boolean_expression($1, $3, boolean_expression::str_to_bool_op($2));
    }
    | NOT value {
        _print_symbol_match("boolean_expression", "negation of value");
        $$ = new boolean_expression($2, nullptr, boolean_expression::operation::negate);
    }
    | PAREN_L boolean_expression PAREN_R {
        _print_symbol_match("boolean_expression", "parenthesis enclosure");
        $$ = $2;
    }
    | value L_OP value {
        _print_symbol_match("boolean_expression", "boolean operation on values, A " + std::string($2) + " B");
        $$ = new boolean_expression($1, $3, boolean_expression::str_to_bool_op($2));
    }
    | TRUE {
        _print_symbol_match("boolean_expression", "TRUE token");
        $$ = new boolean_expression(true);
    }
    | FALSE {
        _print_symbol_match("boolean_expression", "FALSE token");
        $$ = new boolean_expression(false);
    }
;

%%

int yyerror(const char *s)
{
	std::cerr << "Error: " << s << std::endl;
	return 0;
}

int parse(FILE* input, FILE* output)
{
    yyin = input;
    yyout = output;

    return yyparse();
}

