%option noyywrap
%{

#include <cstdio>
#include <cstring>
#include "ast.h"
#include "grammar.tab.h"
#include "types.h"
#define yyterminate() return END

#ifdef WIN32
#define YY_NO_UNISTD_H
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#endif

#define YY_DECL extern "C" int yylex (void)

void showError(const char* token);
%}

type        (int|string|char|bool)
identifier  ([a-z_][a-zA-Z0-9_]*)
int_literal      ([0-9])+
str_literal \"(\\.|[^"\\])*\"
l_op        (\|\||\&\&)
b_op        (\||\&|\~)
a_op_pm     (\+|\-)
a_op_td     (\*|\/)
comparsion  (>|<|==|>=|<=|!=)

%%
struct              {return STRUCT;}
while               {return WHILE;}
for                 {return FOR;}
if                  {return IF;}
else                {return ELSE;}
const               {return CONST;}
return              {return RETURN;}
true                {return TRUE;}
false               {return FALSE;}
","                 {return COMMA;}
"."                 {return DOT;}
"="                 {return ASSIGN;}
"!"                 {return NOT;}
"?"                 {return QUESTION;}

{type}		        {yylval.type = convert_to_type_enum(yytext); return TYPE;}
{identifier}		{sscanf(yytext, "%s", yylval.identifier); return IDENTIFIER;}
{str_literal}       {
                        int length = yyleng;
                        if (length > sizeof(yylval.str_literal)) {
                            length = sizeof(yylval.str_literal) - 1;
                        }
                        memcpy(yylval.str_literal, yytext, length);
                        yylval.str_literal[length] = 0;
                        return STR_LITERAL;
                    }
{int_literal}            {yylval.int_literal = atoi(yytext); return INT_LITERAL;}
{l_op}              {sscanf(yytext, "%s", yylval.l_op); return L_OP;}
{b_op}              {sscanf(yytext, "%s", yylval.b_op); return B_OP;}
{a_op_pm}           {sscanf(yytext, "%s", yylval.a_op); return A_OP_PM;}
{a_op_td}           {sscanf(yytext, "%s", yylval.a_op); return A_OP_TD;}
{comparsion}        {sscanf(yytext, "%s", yylval.comparsion); return COMPARSION;}

"{"      		    {yylval.bracket = yytext[0]; return B_L_CURLY;}
"}"      		    {yylval.bracket = yytext[0]; return B_R_CURLY;}
"["      		    {yylval.bracket = yytext[0]; return B_L_SQUARE;}
"]"      		    {yylval.bracket = yytext[0]; return B_R_SQUARE;}
"("      		    {yylval.parenthesis = yytext[0]; return PAREN_L;}
")"      		    {yylval.parenthesis = yytext[0]; return PAREN_R;}
";"      		    {return SEMICOLON;}
":"                 {return COLON;}
" "      		    ;
"\t"                ;
"\r"                ;
"\n"                ;
.      		        {showError(yytext); return OTHER;}

%%

void showError(const char* token){
    printf("Unexpected character sequence: %s\n", token);
}
