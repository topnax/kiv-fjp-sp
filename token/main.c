#include <stdio.h>

extern int yyparse(void);

int main()
{
    yyparse();
    return 0;
}
