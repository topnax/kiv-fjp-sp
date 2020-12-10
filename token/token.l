%{

#include <stdio.h>
#include <string.h>
#include "grammar.tab.h"
#include "types.h"

void showError();
%}

type  (int|string|char|bool)
identifier  ([a-z][a-zA-Z0-9]*)
number     ([0-9])+
alpha       ([a-zA-Z])+
l_op       (\|\||\&\&)+
b_op       (\||\&)+

%%

{type}		        {yylval.type = convert_to_type_enum(yytext); return (TYPE);}
{identifier}		{sscanf(yytext, "%s", yylval.identifier); return (IDENTIFIER);}
{alpha}             {sscanf(yytext, "%s", yylval.name); return (STRING);}
{number}            {yylval.number = atoi(yytext); return (NUM);}
{l_op}              {sscanf(yytext, "%s", yylval.l_op); return (L_OP);}
{b_op}              {sscanf(yytext, "%s", yylval.b_op); return (B_OP);}
";"      		    {return (SEMICOLON);}
" "      		    ;
.      		        {showError(); return(OTHER);}

%%


void showError(){
    printf("Other input");
}
