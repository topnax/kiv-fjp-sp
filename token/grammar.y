
%{
#include <stdio.h>

int yylex();
int yyerror(char *s);

%}

%token STR_LITERAL NUM OTHER SEMICOLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP COMPARSION
%token ASSIGN
%token B_L_CURLY B_R_CURLY B_L_SQUARE B_R_SQUARE PAREN_L PAREN_R

// reserved keywords
%token STRUCT WHILE FOR IF ELSE CONST RETURN COMMA TRUE FALSE


%type <str_literal> STR_LITERAL
%type <number> NUM
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


%type <parenthesis> paren;
%type <bracket> bracket;


%union{
    char str_literal[200];
    int type;
    int number;
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
  stmts
;

stmts:
		| stmt SEMICOLON stmts
;

stmt:
        | reserved {
            printf("A reserved keyword encountered\n");
        }
        | TYPE IDENTIFIER {
            printf("variable declaration: type=%d identifier=%s\n", $1, $2);
        } 
        | TYPE {
				printf("Your entered a type: - %d\n", $1);
		}
        | STR_LITERAL {
				printf("Your entered a string literal - %s\n", $1);
		}
		| NUM {
				printf("The number you entered is - %d\n", $1);
		}
		| IDENTIFIER {
				printf("The identifier you entered is - %s\n", $1);
		}
    	| ASSIGN {
				printf("Encountered assign operator\n");
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
        | bracket {
				printf("The bracket you entered is - %c\n", $1);
        }
		| paren {
				printf("The parenthesis encountered %c\n", $1);
		}
        | COMMA {
            printf("A comma encountered\n");
        }
		| OTHER
;

paren :
        PAREN_R
        | PAREN_L
;

reserved :
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

bracket :
    B_L_SQUARE
    | B_R_SQUARE
    | B_L_CURLY
    | B_R_CURLY
;

%%

int yyerror(char *s)
{
	printf("Syntax Error on line %s\n", s);
	return 0;
}

