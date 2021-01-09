#include <iostream>
#include <cstdio>
#include <fstream>
#include "ast.h"

extern int parse(FILE* input, FILE* output);
extern program* ast_root;
extern bool print_symbol_match;

int main(int argc, char* argv[])
{
    int parseresult = 0;

    print_symbol_match = false;

    std::string out_name = "out.pl0";

    FILE* fp;
    if (argc >= 2) {

        if (argc > 2) {
            out_name = argv[2];
        }

        // file name provided
        std::cout << "Opening file " << argv[1] << std::endl;
        fp = fopen(argv[1], "r");

        if (!fp) {
            // could not open the the file
            std::cerr << "Cannot open file " << std::endl;
            return -1;
        }

        std::cout << "Compiling..." << std::endl;

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

    std::cout << "AST built successfully, generating code..." << std::endl;

    evaluate_context ctx;
    evaluate_error result = ast_root->evaluate(ctx);

    if (result == evaluate_error::ok)
    {
        auto instr_out_name = out_name + ".instr";
        std::cout << "Compilation successful! Instructions written to: " << instr_out_name << std::endl;
        std::ofstream out(instr_out_name, std::ios::out);
        out << ctx.text_out();
        out.close();
    }
    else
    {
        std::cerr << "Compilation FAILED: " << ctx.error_message << std::endl;
    }

    if (result == evaluate_error::ok) {

        std::vector<binary_instruction> b_out;
        auto res = ctx.binary_out(b_out);
        if (res) {

            std::ofstream out(out_name, std::ios::out | std::ios::binary);

            for (auto& bi : b_out) {
                out.write(reinterpret_cast<const char*>(&bi), sizeof(binary_instruction));
            }

            out.close();

            std::cout << std::endl << "Binary (executable) output written to: " << out_name << std::endl;
        }
    }

    return 0;
}
