#include <iostream>
#include <stdio.h>

extern "C" int parse(FILE* input, FILE* output);

int main(int argc, char* argv[])
{
    FILE* fp = fopen("../samples/struct.cmm", "r");
    if (!fp) {
        std::cerr << "Cannot open file" << std::endl;
        return -1;
    }

    parse(fp, stdout);
    return 0;
}
