%{

#include <stdio.h>
#include <string.h>
#include "grammar.tab.h"
#include "types.h"

void showError();
%}

type        (int|string|char|bool)
identifier  ([a-z][a-zA-Z0-9]*)
number      ([0-9])+
str_literal \"(\\.|[^"\\])*\"
l_op        (\|\||\&\&)
b_op        (\||\&|\~)
a_op        (\+|\*|\/|\-)
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
"="                 {return ASSIGN;}

{type}		        {yylval.type = convert_to_type_enum(yytext); return TYPE;}
{identifier}		{sscanf(yytext, "%s", yylval.identifier); return IDENTIFIER;}
{str_literal}       {sscanf(yytext, "%s", yylval.str_literal); return STR_LITERAL;}
{number}            {yylval.number = atoi(yytext); return NUM;}
{l_op}              {sscanf(yytext, "%s", yylval.l_op); return L_OP;}
{b_op}              {sscanf(yytext, "%s", yylval.b_op); return B_OP;}
{a_op}              {sscanf(yytext, "%s", yylval.a_op); return A_OP;}
{comparsion}        {sscanf(yytext, "%s", yylval.comparsion); return COMPARSION;}


"{"      		    {yylval.bracket = yytext[0]; return B_L_CURLY;}
"}"      		    {yylval.bracket = yytext[0]; return B_R_CURLY;}
"["      		    {yylval.bracket = yytext[0]; return B_L_SQUARE;}
"]"      		    {yylval.bracket = yytext[0]; return B_R_SQUARE;}
"("      		    {yylval.parenthesis = yytext[0]; return PAREN_L;}
")"      		    {yylval.parenthesis = yytext[0]; return PAREN_R;}
";"      		    {return SEMICOLON;}
" "      		    ;
.      		        {showError(); return OTHER;}

%%


void showError(){
    printf("Other input");
}
