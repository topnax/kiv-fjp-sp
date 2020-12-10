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
brackets  ()

%%

{type}		        {yylval.type = convert_to_type_enum(yytext); return (TYPE);}
{identifier}		{sscanf(yytext, "%s", yylval.identifier); return (IDENTIFIER);}
{str_literal}       {sscanf(yytext, "%s", yylval.str_literal); return (STR_LITERAL);}
{number}            {yylval.number = atoi(yytext); return (NUM);}
{l_op}              {sscanf(yytext, "%s", yylval.l_op); return (L_OP);}
{b_op}              {sscanf(yytext, "%s", yylval.b_op); return (B_OP);}
{a_op}              {sscanf(yytext, "%s", yylval.a_op); return (A_OP);}
{comparsion}         {sscanf(yytext, "%s", yylval.comparsion); return (COMPARSION);}
";"      		    {return (SEMICOLON);}
" "      		    ;
.      		        {showError(); return(OTHER);}

%%


void showError(){
    printf("Other input");
}
