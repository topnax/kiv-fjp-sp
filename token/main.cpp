#include <iostream>
#include <cstdio>
#include <fstream>
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
        case evaluate_error::unresolved_reference:
            std::cerr << "Unresolved reference" << std::endl;
            break;
    }

    //if (result == evaluate_error::ok)
    {
        std::cout << std::endl << "Generated program: " << std::endl;
        std::cout << ctx.text_out() << std::endl;
    }

    if (result == evaluate_error::ok) {

        std::vector<binary_instruction> b_out;
        auto res = ctx.binary_out(b_out);
        if (res) {

            std::ofstream out("out.pl0", std::ios::out | std::ios::binary);

            for (auto& bi : b_out) {
                out.write(reinterpret_cast<const char*>(&bi), sizeof(binary_instruction));
            }

            out.close();

            std::cout << std::endl << "Output stored to: out.pl0" << std::endl;
        }
    }

    return 0;
}
