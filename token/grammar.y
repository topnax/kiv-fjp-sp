
%{
#include <stdio.h>

int yylex();
int yyerror(char *s);

%}

%token STRING NUM OTHER SEMICOLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP COMPARSION

%type <name> STRING
%type <number> NUM
%type <identifier> IDENTIFIER
%type <type> TYPE
%type <l_op> L_OP
%type <a_op> A_OP
%type <b_op> B_OP
%type <comparsion> COMPARSION;

%union{
    char name[20];
    int type;
    int number;
    char identifier[30];
    char l_op[5];
    char a_op[5];
    char b_op[5];
    char comparsion[5];
}

%%

prog:
  stmts
;

stmts:
		| stmt SEMICOLON stmts
;

stmt:
        | TYPE IDENTIFIER {
            printf("variable declaration: type=%d identifier=%s\n", $1, $2);
        } 
        | TYPE {
				printf("Your entered a type: - %d\n", $1);
		}
        | STRING {
				printf("Your entered a string - %s\n", $1);
		}
		| NUM {
				printf("The number you entered is - %d\n", $1);
		}
		| IDENTIFIER {
				printf("The identifier you entered is - %s\n", $1);
		}
		| L_OP {
				printf("The logic operator you entered is - %s\n", $1);
		}
		| A_OP {
				printf("The arithmetic operator you entered is - %s\n", $1);
		}
		| B_OP {
				printf("The binary operator you entered is - %s\n", $1);
		}
		| COMPARSION {
				printf("The comparsion you entered is - %s\n", $1);
		}
		| OTHER
;

%%

int yyerror(char *s)
{
	printf("Syntax Error on line %s\n", s);
	return 0;
}

