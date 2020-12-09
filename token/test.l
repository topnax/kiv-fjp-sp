%{

#include <stdio.h>
#include <string.h>
#include "test.tab.h"
void showError();
%}

identifier  ([a-z][a-zA-Z0-9]*)
numbers     ([0-9])+
alpha       ([a-zA-Z])+

%%

{identifier}		{sscanf(yytext, "%s", yylval.identifier); return (IDENTIFIER);}
{alpha}             {sscanf(yytext, "%s", yylval.name); return (STRING);}
{numbers}           {yylval.number = atoi(yytext); return (NUM);}
";"      		    {return (SEMICOLON);}
.      		        {showError(); return(OTHER);}

%%


void showError(){
    printf("Other input");
}
