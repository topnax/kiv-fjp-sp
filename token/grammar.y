
%{
#include <stdio.h>

int yylex();
int yyerror(char *s);

%}

%token STR_LITERAL INT_LITERAL OTHER SEMICOLON IDENTIFIER TYPE WHITESPACE L_OP B_OP A_OP COMPARSION
%token ASSIGN
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


%type <parenthesis> paren;
%type <bracket> bracket;


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
    | variable prog
    | END
;

stmts:
		| stmt SEMICOLON stmts
;

stmt:
        | reserved {
            printf("A reserved keyword encountered\n");
        }

        | TYPE {
				printf("Your entered a type: - %d\n", $1);
		}
        | STR_LITERAL {
				printf("Your entered a string literal - %s\n", $1);
		}
		| INT_LITERAL {
				printf("The int literal you entered is - %d\n", $1);
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
        | DOT {
            printf("A dot encountered\n");
        }
		| OTHER
;

declaration:
        | TYPE IDENTIFIER {
            printf("variable declaration: type=%d identifier=%s\n", $1, $2);
        }
;

value:
    | INT_LITERAL
    | STR_LITERAL
    | IDENTIFIER
;

variable:
    | declaration SEMICOLON {
        printf("got variable declaration\n");
    } 
    | declaration ASSIGN value SEMICOLON {
        printf("got variable delcaration with value assignment\n");
    }
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

