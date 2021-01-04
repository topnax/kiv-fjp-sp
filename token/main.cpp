#include <iostream>
#include <stdio.h>
#include "ast.h"

extern int parse(FILE* input, FILE* output);
extern program* ast_root;

int main(int argc, char* argv[])
{
    int parseresult = 0;

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
        parseresult = parse(fp, stdout);

    } else {
        // no file provided, parse stdin
        parseresult = parse(stdin, stdout);
    }

    if (parseresult != 0) {
        std::cerr << "Compilation ended with errors" << std::endl;
        return -1;
    }

    std::cout << "AST complete, generating code" << std::endl;

    evaluate_context ctx;
    evaluate_error result = ast_root->evaluate(ctx);

    switch (result) {
        case evaluate_error::ok:
            std::cout << "OK" << std::endl;
            break;
        case evaluate_error::undeclared_identifier:
            std::cerr << "Undeclared identifier" << std::endl;
            break;
        case evaluate_error::unknown_typename:
            std::cerr << "Unknown typename" << std::endl;
            break;
        case evaluate_error::invalid_state:
            std::cerr << "Invalid state" << std::endl;
            break;
    }

    if (result == evaluate_error::ok) {
        std::cout << std::endl << "Transcribed program: " << std::endl;
        std::cout << ctx.transcribe() << std::endl;
    }

    return 0;
}
