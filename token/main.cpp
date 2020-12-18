#include <iostream>
#include <stdio.h>
#include "ast.h"

extern int parse(FILE* input, FILE* output);
extern program* ast_root;

int main(int argc, char* argv[])
{
    FILE* fp;
    if (argc == 2) {
        // file name provided
        std::cout << "Opening file " << argv[1] << std::endl;
        fp = fopen(argv[1], "r");

        if (!fp) {
            // could not open the the file
            std::cerr << "Cannot open file " << std::endl;
            return -1;
        }

        // parse the provided file
        parse(fp, stdout);

    } else {
        // no file provided, parse stdin
        parse(stdin, stdout);
    }

    return 0;
}
