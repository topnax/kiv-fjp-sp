#include <iostream>
#include <cstdint>
#include <fstream>
#include <vector>

enum class pcode_fct : uint8_t
{
	LIT,
	OPR,
	LOD,
	STO,
	CAL,
	INT,
	JMP,
	JPC,
    STA,
    LDA,
};

enum class pcode_opr : int {
	RETURN = 0,

	// arithmetic operations
	NEGATE = 1, // arithmetic
	ADD = 2,
	SUBTRACT = 3,
	MULTIPLY = 4,
	DIVIDE = 5,

	// compare operations
	ODD = 6,
	// 7 is undefined
	EQUAL = 8,
	NOTEQUAL = 9,
	LESS = 10,
	GREATER_OR_EQUAL = 11,
	GREATER = 12,
	LESS_OR_EQUAL = 13,
};

constexpr int BuiltinBase = 0x00FFFFFF;

#pragma pack(push, 1)

struct instruction {
	pcode_fct f;
	uint8_t l;		// [0..254] / 255 = global scope
	int a;
};

#pragma pack(pop)

constexpr size_t StackSize = 2048;

int p; // program register (program counter)
int b; // base register (current frame base)
int t; // top of the stack register (stack register)
int i; // instruction register
int s[StackSize]; // virtual p-machine allocated stack

int base(uint8_t l) {

	// global scope
	if (l == 255) {
		return 0;
	}

	int b1 = b;
	while (l > 0) {
		b1 = s[b1];
		l--;
	}

	return b1;
}

int main(int argc, char** argv)
{
	if (argc != 2) {
		std::cout << "Usage: " << argv[0] << " <path to program>" << std::endl;
		return 1;
	}

	std::ifstream codefile(argv[1], std::ios::binary | std::ios::in);

	codefile.seekg(0, std::ios::end);
	auto codesize = codefile.tellg();
	codefile.seekg(0, std::ios::beg);

	std::vector<instruction> code(codesize / sizeof(instruction));
	codefile.read(reinterpret_cast<char*>(code.data()), codesize);

	std::cout << "=== P-machine start ===" << std::endl;
	p = 0;	// program register = 0 (start execution at offset 0)
	t = 0;	// top of the stack register = 0 (stack is empty)
	b = 0;	// base register = 0 (current base references address 0)
	s[1] = s[2] = s[3] = 0;	// clear stack frame, just in case

	int last_p = 0;
	bool error = false;

	const bool DebugOutput = false;

	do {
		instruction i = code[p];
		last_p = p;

		if (DebugOutput) {
			std::cout << "=== [" << p << "] ";
		}

		p = p + 1;

		switch(i.f) {
			case pcode_fct::LIT:
			{
				if (DebugOutput) {
					std::cout << "=== LIT: " << i.a << std::endl;
				}
				t++;
				s[t] = i.a;
				break;
			}
			case pcode_fct::OPR:
			{
				if (DebugOutput) {
					std::cout << "=== OPR: " << i.a << std::endl;
				}
				switch (static_cast<pcode_opr>(i.a)) {
					case pcode_opr::RETURN:
						t = b - 1;
						p = s[t + 3];
						b = s[t + 2];
						break;
					case pcode_opr::NEGATE:
						s[t] = -s[t];
						break;
					case pcode_opr::ADD:
						t--;
						if (DebugOutput) {
							std::cout << "===    " << s[t] << " + " << s[t + 1] << std::endl;
						}
						s[t] = s[t] + s[t + 1];
						break;
					case pcode_opr::SUBTRACT:
						t--;
						if (DebugOutput) {
							std::cout << "===    " << s[t] << " - " << s[t + 1] << std::endl;
						}
						s[t] = s[t] - s[t + 1];
						break;
					case pcode_opr::MULTIPLY:
						t--;
						if (DebugOutput) {
							std::cout << "===    " << s[t] << " * " << s[t + 1] << std::endl;
						}
						s[t] = s[t] * s[t + 1];
						break;
					case pcode_opr::DIVIDE:
						t--;
						if (DebugOutput) {
							std::cout << "===    " << s[t] << " / " << s[t + 1] << std::endl;
						}
						s[t] = s[t] / s[t + 1];
						break;
					case pcode_opr::ODD:
						s[t] = (s[t] & 1);
						break;
					case pcode_opr::EQUAL:
						t--;
						s[t] = (s[t] == s[t + 1]);
						break;
					case pcode_opr::NOTEQUAL:
						t--;
						s[t] = (s[t] != s[t + 1]);
						break;
					case pcode_opr::LESS:
						t--;
						s[t] = (s[t] < s[t + 1]);
						break;
					case pcode_opr::GREATER_OR_EQUAL:
						t--;
						s[t] = (s[t] >= s[t + 1]);
						break;
					case pcode_opr::GREATER:
						t--;
						s[t] = (s[t] > s[t + 1]);
						break;
					case pcode_opr::LESS_OR_EQUAL:
						t--;
						s[t] = (s[t] <= s[t + 1]);
						break;
					default:
						break;
				}
				break;
			}
            case pcode_fct::LDA: {
                auto addr = s[t];
                t--;
                auto ba = base(s[t]);

                if (DebugOutput) {
                    std::cout << "=== LDA: addr:" << static_cast<int>(addr) << ", b: " << ba  << std::endl;
                }

                s[t] = s[b + addr];

                break;
            }
            case pcode_fct::STA : {

                auto addr = s[t];
                t--;
                auto b = base(s[t]);
                t--;
                auto value = s[t];
                t--;

                if (DebugOutput) {
                    std::cout << "=== STA: addr:" << static_cast<int>(addr) << ", b: " << b  << ", value:" << value << std::endl;
                }

                s[b + addr] = value;
                break;
            }
			case pcode_fct::LOD:
				if (DebugOutput) {
					std::cout << "=== LOD: " << static_cast<int>(i.l) << " - " << i.a << std::endl;
				}
				t++;
				s[t] = s[base(i.l) + i.a];
				break;
			case pcode_fct::STO:
				if (DebugOutput) {
					std::cout << "=== STO: " << static_cast<int>(i.l) << " - " << i.a << " (value = " << s[t] << ")" << std::endl;
				}
				s[base(i.l) + i.a] = s[t];
				t--;
				break;
			case pcode_fct::CAL:
			{
				if (DebugOutput) {
					std::cout << "=== CAL: " << i.a << std::endl;
				}
				if (i.a < BuiltinBase) {
					s[t + 1] = base(i.l);
					s[t + 2] = b;
					s[t + 3] = p;
					b = t + 1;
					p = i.a;
				}
				else {
					int ptr;
					switch (i.a) {
						case BuiltinBase + 0:	// consolePrintNum
							std::cout << s[t];
							break;
						case BuiltinBase + 1:	// consolePrintLnNum
							std::cout << s[t] << std::endl;
							break;
						case BuiltinBase + 2:	// consolePrintStr
							ptr = s[t];
							while (s[ptr] != '\0')
								std::cout << static_cast<char>(s[ptr++]);
							break;
						case BuiltinBase + 3:	// consolePrintLnStr
							ptr = s[t];
							while (s[ptr] != '\0')
								std::cout << static_cast<char>(s[ptr++]);
							std::cout << std::endl;
							break;
						case BuiltinBase + 4:	// consoleScanNum
							std::cin >> ptr;
							s[b + 3] = ptr;
							break;
					}
				}
				break;
			}
			case pcode_fct::INT:
				if (DebugOutput) {
					std::cout << "=== INT: " << i.a << std::endl;
				}
				t = t + i.a;
				break;
			case pcode_fct::JMP:
				if (DebugOutput) {
					std::cout << "=== JMP: " << i.a << std::endl;
				}
				p = i.a;
				break;
			case pcode_fct::JPC:
				if (DebugOutput) {
					std::cout << "=== JPC: " << i.a << std::endl;
				}
				if (s[t] == 0) {
					p = i.a;
					t = t - 1;
				}
				break;
			default:
				error = true;
				std::cout << "=== Invalid opcode - " << static_cast<int>(i.f) << std::endl;
				break;
		}

		if (p >= code.size()) {
			error = true;
			std::cout << "=== Invalid state - p > code size (" << p << " > " << code.size() << ")" << std::endl;
			break;
		}
		if (t >= StackSize) {
			error = true;
			std::cout << "=== Invalid state - t > stack size (" << t << " > " << StackSize << ")" << std::endl;
			break;
		}

	} while (!error && p != last_p); // we reached an endless loop

	std::cout << "=== P-machine stop ===" << std::endl;

	if (!error) {
		// return code of main is stored in stack at index 3 (0-2 is virtual call block)
		std::cout << "=== main() returned " << s[3] << std::endl;
	}

	return error ? 2 : 0;
}
