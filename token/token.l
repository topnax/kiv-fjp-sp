%{

#include <stdio.h>
#include <string.h>
#include "grammar.tab.h"
#include "types.h"

void showError();
%}

type  (int|string|char|bool)
identifier  ([a-z][a-zA-Z0-9]*)
numbers     ([0-9])+
alpha       ([a-zA-Z])+

%%

{type}		        {yylval.type = convert_to_type_enum(yytext); return (TYPE);}
{identifier}		{sscanf(yytext, "%s", yylval.identifier); return (IDENTIFIER);}
{alpha}             {sscanf(yytext, "%s", yylval.name); return (STRING);}
{numbers}           {yylval.number = atoi(yytext); return (NUM);}
";"      		    {return (SEMICOLON);}
" "      		    ;
.      		        {showError(); return(OTHER);}

%%


void showError(){
    printf("Other input");
}
