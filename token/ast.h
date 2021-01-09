#pragma once

#include <list>
#include <string>
#include <vector>
#include <map>
#include <stack>
#include <sstream>
#include <iostream>
#include <limits>

#include "types.h"

enum class evaluate_error {
    ok,                             // no error, OK
    undeclared_identifier,          // identifier was not declared (not even forward-declared)
    unknown_typename,               // the type name (struct only in our case) was not found
    invalid_state,                  // compiler is in invalid state (implementation error)
    unresolved_reference,           // could not find symbol
    undeclared_struct_member,       // structure has no such member
    redeclaration,                  // identifier redeclaration
    redeclaration_different_types,  // identifier (function) redeclaration with different types (return type, parameters)
    invalid_call,                   // invalid function call (we try to call identifier that is a variable, ...)
    cannot_assign_type,             // cannot assign to lhs due to type
    cannot_assign_const,            // cannot perform assignment to constant left-hand side (one exception: initilization)
    nonvoid_func_no_return,         // non-void function does not return a value
};

// following Wirth's original p-code machine design
enum class pcode_fct : uint8_t {
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

// the stack frame mechanism is built in a way that this number would eventually "converge" to global scope
// to speed things up, the interpreter immediatelly jumps to global scope base when this level value is seen
constexpr uint8_t LevelGlobal = 255;

// this is where builtin functions start
constexpr int BuiltinBase = 0x00FFFFFF;

// call block size (each function call has this as a part of its stack frame)
// - it consists of outer context base, current base, return address and return value of callees
// this is retained from virtual p-machine definition and enhanced for return values
constexpr int CallBlockBaseSize = 4;

// cell of a return value (where the callee stores it in parent scope (level = 1), and caller takes it from there in his scope (level = 0))
constexpr int ReturnValueCell = 3;

#pragma pack(push, 1)

struct binary_instruction {
    pcode_fct f;
    uint8_t l;		// [0..254] / 255 = global scope
    int a;
};

#pragma pack(pop)

struct pcode_arg {

    pcode_arg(int val) : isref(false), value(val), symbolref("") {
    }
    pcode_arg(pcode_opr val) : isref(false), value(static_cast<int>(val)), symbolref("") {
    }
    pcode_arg(const std::string& sym, bool function = false) : isref(true), isfunc(function), value(0), symbolref(sym) {
    }

    // is this argument a reference? (needs to be resolved?)
    bool isref;
    // is this argument a function reference?
    bool isfunc = false;
    // if resolved, this holds the actual value
    int value;
    // symbol reference (for what symbol to look when resolving)
    std::string symbolref;
    // offset to be used when resolving to an actual address
    int offset = 0;

    // shortcut for resolving address of a symbol
    void resolve(int addr_value) {
        value = addr_value + offset;
        isref = false;
        symbolref = "";
    }
};

struct pcode_instruction {

    pcode_instruction() : instruction(pcode_fct::LIT), lvl(0), arg(0) {
    }

    pcode_instruction(pcode_fct fct, int level, pcode_arg argument) : instruction(fct), lvl(level), arg(argument) {
    }

    pcode_instruction(pcode_fct fct, pcode_arg argument) : instruction(fct), lvl(0), arg(argument) {
    }

    pcode_fct instruction;
    int lvl;
    pcode_arg arg;

    std::string to_string() const {
        std::string ret;

        switch (instruction) {
            case pcode_fct::LIT: ret = "LIT"; break;
            case pcode_fct::OPR: ret = "OPR"; break;
            case pcode_fct::LOD: ret = "LOD"; break;
            case pcode_fct::STO: ret = "STO"; break;
            case pcode_fct::CAL: ret = "CAL"; break;
            case pcode_fct::INT: ret = "INT"; break;
            case pcode_fct::JMP: ret = "JMP"; break;
            case pcode_fct::JPC: ret = "JPC"; break;
            case pcode_fct::STA: ret = "STA"; break;
            case pcode_fct::LDA: ret = "LDA"; break;
            default: ret = "???"; break;
        }

        ret += " " + std::to_string(lvl) + " " + (arg.isref ? "<"+arg.symbolref+">" : std::to_string(arg.value));

        return ret;
    }
};

struct block;
struct declaration;
struct value;

struct ptype_info {
    // major type, see types.h
    int major_type = TYPE_VOID;
    // minor type, if this is a struct, this tells the current structure
    int minor_type = 0;

    // is this a function type identifier?
    bool is_function = false;
    // is this a constant?
    bool is_const = false;

    bool operator==(const ptype_info& second) const {
        // technically "const" and "non-const" variants are different types, but in order to be able to assign const to non-const, we check for it separately
        return major_type == second.major_type && minor_type == second.minor_type /*&& is_function == second.is_function && is_const == second.is_const*/;
    }

    bool operator!=(const ptype_info& second) const {
        return !(*this == second);
    }
};

// struct of declared identifier
struct declared_identifier {
    // scope of identifier
    block* scope = nullptr;
    // identifier type (or meta-type in case of struct)
    ptype_info type = { TYPE_VOID, 0 };
    // structure name
    std::string struct_name{};
    // is forward declared? (does not necessarily have a body, in case of function)
    bool forward_decl = false;
    // if it's a function, what parameters does it take?
    std::vector<ptype_info> func_parameters = {};
};

// this structure holds the compilation context
struct evaluate_context {
    // all currently declared identifiers
    std::map<std::string, declared_identifier> declared_identifiers;
    // scope stack
    std::stack<block*> current_scope;
    // defined structures
    std::map<std::string, std::list<declaration*>*> struct_defs;
    // defined structures type indices
    std::map<std::string, int> struct_type_indices;

    int struct_max_index = 0;

    // error message if compilation fails
    std::string error_message;

    // size of global variables
    int global_frame_size = 0;
    std::map<std::string, int> global_identifier_cell;
    // global initializers table
    std::map<std::string, value*> global_initializers;
    // string literals to be defined (key = cell where string starts)
    std::map<int, std::string> string_literals;

    // declare identifier in current scope
    evaluate_error declare_identifier(const std::string& identifier, const ptype_info type, const std::string& struct_name = "", bool forward_decl = false, const std::vector<ptype_info>& types = {}) {

        auto itr = declared_identifiers.find(identifier);

        // identifier must not exist (at all - do not allow overlapping)
        if (itr == declared_identifiers.end()) {
            declared_identifiers[identifier] = {
                    get_current_scope(),
                    type,
                    struct_name,
                    forward_decl,
                    types
            };
            return evaluate_error::ok;
        }
        // forward declaration does not trigger redeclaration error
        else if (itr->second.forward_decl || forward_decl) {

            auto& decl = itr->second;

            // always check signature (return type and parameters)
            if (decl.type.major_type != type.major_type || decl.type.minor_type != type.minor_type
                || decl.func_parameters.size() != types.size()) {
                return evaluate_error::redeclaration_different_types;
            }

            for (size_t i = 0; i < types.size(); i++) {
                if (decl.func_parameters[i].major_type != types[i].major_type || decl.func_parameters[i].minor_type != types[i].minor_type) {
                    return evaluate_error::redeclaration_different_types;
                }
            }

            // if current declaration attempt is not a forward declaration, mark it in declared record
            if (!forward_decl) {
                decl.forward_decl = false;
            }
            return evaluate_error::ok;
        }

        return evaluate_error::redeclaration;
    }

    // undeclare existing identifier
    bool undeclare_identifier(const std::string& identifier) {
        if (declared_identifiers.find(identifier) != declared_identifiers.end()) {
            declared_identifiers.erase(identifier);
            return true;
        }

        return false;
    }

    // is given identifier declared?
    bool is_identifier_declared(const std::string& identifier) const {
        return (declared_identifiers.find(identifier) != declared_identifiers.end());
    }

    // get declared identifier; prior call is_identifier_declared is needed
    const declared_identifier& get_declared_identifier(const std::string& identifier) const {
        return declared_identifiers.find(identifier)->second; // we assume the identifier is there
    }

    // define new structure
    bool define_struct(const std::string& name, std::list<declaration*>* decl) {
        if (struct_defs.find(name) == struct_defs.end()) {
            struct_defs[name] = decl;
            struct_type_indices[name] = ++struct_max_index;
            return true;
        }

        return false;
    }

    // is given structure defined?
    bool is_struct_defined(const std::string& name) const {
        return (struct_defs.find(name) != struct_defs.end());
    }

    // retrieves structure definition (assumes that existence was verified by is_struct_defined)
    const auto& get_struct_definition(const std::string& name) const {
        return struct_defs.find(name)->second;
    }

    // stores string literal so it could be created at program start
    int store_global_string_literal(const std::string& lit) {
        int ret = global_frame_size;
        global_frame_size += static_cast<int>(lit.length()) + 1;
        string_literals[ret] = lit;

        return ret;
    }

    // push new scope defined by block
    bool push_scope(block* blk) {
        current_scope.push(blk);
        return true;
    }

    // pop current scope and undeclare all identifiers
    bool pop_scope(block* blk) {

        if (current_scope.empty()) {
            return false;
        }

        block* scope = current_scope.top();
        // verify the current scope matches
        if (scope != blk) {
            return false;
        }

        current_scope.pop();

        std::vector<std::string> toerase;

        // undeclare identifiers from this scope
        for (auto& decl : declared_identifiers) {
            if (decl.second.scope == scope) {
                toerase.push_back(decl.first);
            }
        }

        for (auto decl : toerase) {
            undeclare_identifier(decl);
        }

        return true;
    }

    // generated instructions
    std::vector<pcode_instruction> generated_program;

    // generate instruction to code
    int gen_instruction(pcode_fct instr, pcode_arg argument, int level = 0) {
        generated_program.emplace_back(instr, level, argument);

        return static_cast<int>(generated_program.size() - 1);
    }

    // transcribes generated program to mnemonics and instruction operand values
    std::string text_out() const {
        std::ostringstream out;

        for (auto& instr : generated_program) {
            out << instr.to_string() << std::endl;
        }

        return out.str();
    }

    // transcribes generated program to a vector of instructions in binary format
    bool binary_out(std::vector<binary_instruction>& out) const {
        binary_instruction bi;

        for (auto& instr : generated_program) {
            bi.f = instr.instruction;
            bi.l = instr.lvl;
            bi.a = instr.arg.value;

            out.push_back(bi);
        }

        return true;
    }

    // retrieves current scope; nullptr for global scope (no enclosing block)
    block* get_current_scope() const {
        return current_scope.empty() ? nullptr : current_scope.top();
    }

    // find identifier and retrieve its level and offset
    bool find_identifier(const std::string& identifier, int& level, int& offset);

    // this flag is dynamically set in process of code generation; indicates that return statement is present in the function
    bool return_statement = false;
};

// every AST node inherits this struct
struct ast_node {
    virtual ~ast_node() {
    };

    // evaluate node - for example to generate PL/0
    virtual evaluate_error evaluate(evaluate_context& context) = 0;
};

// statement on global scope
struct global_statement : public ast_node {
    virtual evaluate_error evaluate(evaluate_context& context) override {
        return evaluate_error::ok;
    }
};

struct value;

// boolean expression or literal
struct boolean_expression : public ast_node {

    enum class operation {
        none,
        negate, // !
        b_and,  // &&
        b_or,   // ||
        c_gt,   // >
        c_lt,   // <
        c_ge,   // >=
        c_le,   // <=
        c_eq,   // ==
        c_neq,  // !=
    };

    // converts string to operator enum
    static operation str_to_bool_op(const std::string& op) {
        if (op == "||") {
            return operation::b_or;
        }
        else if (op == "&&") {
            return operation::b_and;
        }
        else if (op == ">") {
            return operation::c_gt;
        }
        else if (op == "<") {
            return operation::c_lt;
        }
        else if (op == ">=") {
            return operation::c_ge;
        }
        else if (op == "<=") {
            return operation::c_le;
        }
        else if (op == "==") {
            return operation::c_eq;
        }
        else if (op == "!=") {
            return operation::c_neq;
        }
        return operation::none;
    }

    // converts operation to p-code OPR argument
    static pcode_opr operation_to_pcode_opr(const operation oper) {
        switch (oper) {
            case operation::c_eq: return pcode_opr::EQUAL;
            case operation::c_neq: return pcode_opr::NOTEQUAL;
            case operation::c_gt: return pcode_opr::GREATER;
            case operation::c_lt: return pcode_opr::LESS;
            case operation::c_ge: return pcode_opr::GREATER_OR_EQUAL;
            case operation::c_le: return pcode_opr::LESS_OR_EQUAL;
        }

        return pcode_opr::NEGATE; // dummy, should not happen
    }

    boolean_expression(value* val, value* val2, operation comp_op = operation::none)
        : cmpval1(val), cmpval2(val2), op(comp_op) {
        //
    }

    boolean_expression(boolean_expression* bval, boolean_expression* bval2, operation bool_op = operation::none)
        : boolexp1(bval), boolexp2(bval2), op(bool_op) {
        //
    }

    boolean_expression(bool value)
        : preset_value(value) {
        //
    }

    virtual ~boolean_expression();

    // values to be compared (for comparisons)
    value* cmpval1 = nullptr, *cmpval2 = nullptr;
    // boolean expression(s) to be considered (for operations, ...)
    boolean_expression* boolexp1 = nullptr, *boolexp2 = nullptr;
    // operation to be performed
    operation op = operation::none;
    // if this is a constant literal, this stores its value
    bool preset_value = false;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// terminal expression - wraps value to discard its stack record later
struct expression : public ast_node {

    expression() {}

    expression(value* val)
        : evalvalue(val) {
        //
    }

    virtual ~expression();

    value* evalvalue = nullptr;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// assignment expression
struct assign_expression : public expression {

    assign_expression(char* assidentifier, value* arrayidx, value* val)
        : identifier(assidentifier), arrayindex(arrayidx), assignvalue(val) {
        //
    }

    assign_expression(char* structidentifier, char* memberidentifier, value* val)
        : identifier(structidentifier), structmemberidentifier(memberidentifier), assignvalue(val), structmember(true) {

    }

    virtual ~assign_expression();

    std::string identifier;
    std::string structmemberidentifier;
    bool structmember = false;
    value* arrayindex = nullptr;
    value* assignvalue;

    // if this is a right-hand side of another assignment, push the resulting left-hand side to stack
    bool push_result_to_stack = false;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// arithmetic expression
struct arithmetic : public ast_node {

    enum class operation {
        none,
        add,    // +
        sub,    // -
        mul,    // *
        div,    // /
    };

    // converts string to operation enum
    static operation str_to_op(const std::string& str) {
        if (str == "+") {
            return operation::add;
        }
        else if (str == "-") {
            return operation::sub;
        }
        else if (str == "*") {
            return operation::mul;
        }
        else if (str == "/") {
            return operation::div;
        }

        return operation::none;
    }

    // converts operation to p-code OPR argument
    static pcode_opr operation_to_pcode_opr(operation oper) {
        switch (oper) {
            case operation::add: return pcode_opr::ADD;
            case operation::sub: return pcode_opr::SUBTRACT;
            case operation::mul: return pcode_opr::MULTIPLY;
            case operation::div: return pcode_opr::DIVIDE;
        }

        return pcode_opr::NEGATE;
    }

    arithmetic(value* lhs, operation valop, value* rhs)
        : lhs_val(lhs), rhs_val(rhs), op(valop) {
        //
    }
    virtual ~arithmetic();

    // left- and right-hand side of arithmetic operation
    value* lhs_val, *rhs_val;
    // operation to be performed
    operation op;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// function call
struct function_call : public ast_node {
    function_call(char* identifier, std::list<value*>* param_list = nullptr)
        : function_identifier(identifier), parameters(param_list) {
    }
    virtual ~function_call();

    // identifier of function to be called
    std::string function_identifier;
    // parameters definition
    std::list<value*>* parameters;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// wrapper for variable reference in value token
struct variable_ref : public ast_node {
    variable_ref(const char* varidentifier) : identifier(varidentifier) {
        //
    }

    // wrapped variable identifier
    std::string identifier;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        // only check if identifier exists
        if (!context.is_identifier_declared(identifier)) {
            context.error_message = "Undeclared identifier '" + identifier + "'";
            return evaluate_error::undeclared_identifier;
        }

        return evaluate_error::ok;
    }
};

// generic value structure (everything that 'has' a value eventually ends up here)
struct value : public ast_node {
    enum class value_type {
        const_int_literal,
        const_str_literal,
        variable,
        return_value,
        member,
        array_element,
        arithmetic,
        ternary,
        assign_expression,
        boolean_expression,
    };

    value_type val_type;
    union {
        int int_value;
        variable_ref* variable;
        function_call* call;
        struct {
            variable_ref* array;
            value* index;
        } array_element;
        arithmetic* arithmetic_expression;
        struct {
            boolean_expression* boolexpr;
            value* positive;
            value* negative;
        } ternary;
        assign_expression* assign_expr;
        boolean_expression* boolexpr;
    } content;

    std::string str_base; // str_value, struct_name
    std::string member_spec;

    bool return_value_ignored = false;

    value(int val) : val_type(value_type::const_int_literal) {
        content.int_value = val;
    }

    value(const std::string& val) : val_type(value_type::const_str_literal) {

        // remove quote marks (parser passes it with them)
        if (val.size() >= 2 && val[0] == '\"' && val[val.size() - 1] == '\"') {
            str_base = val.substr(1, val.size() - 2);
        }
        else {
            str_base = val;
        }
    }

    value(boolean_expression* exp, value* positive, value* negative) : val_type(value_type::ternary) {
        content.ternary.boolexpr = exp;
        content.ternary.positive = positive;
        content.ternary.negative = negative;
    }

    value(variable_ref* varref) : val_type(value_type::variable) {
        content.variable = varref;
    }

    value(function_call* func_call) : val_type(value_type::return_value) {
        content.call = func_call;
    }

    value(const std::string& structname, const std::string& member) : val_type(value_type::member) {
        str_base = structname;
        member_spec = member;
    }

    value(variable_ref* array, value* index) : val_type(value_type::array_element) {
        content.array_element.array = array;
        content.array_element.index = index;
    }

    value(arithmetic* arithmetic_expr) : val_type(value_type::arithmetic) {
        content.arithmetic_expression = arithmetic_expr;
    }

    value(assign_expression* assign_expr) : val_type(value_type::assign_expression) {
        content.assign_expr = assign_expr;
    }

    value(boolean_expression* bool_expr) : val_type(value_type::boolean_expression) {
        content.boolexpr = bool_expr;
    }

    virtual ~value();

    virtual evaluate_error evaluate(evaluate_context& context) override;

    ptype_info get_type_info(const evaluate_context& context) const;
};

struct variable_declaration;
struct expression;
struct loop;
struct condition;

// any command
struct command : public ast_node {
    command(variable_declaration* decl)
        : vardecl(decl) {
        //
    }

    command(expression* decl)
        : expressiondecl(decl) {
        //
    }

    command(value* val)
        : retvalue(val) {
        //
    }

    command(loop* loop)
        : loopdecl(loop) {
        //
    }

    command(condition* cond)
        : conddecl(cond) {
        //
    }

    virtual ~command();

    variable_declaration* vardecl = nullptr;
    expression* expressiondecl = nullptr;
    loop* loopdecl = nullptr;
    condition* conddecl = nullptr;
    value* retvalue = nullptr;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// a block of commands (single or multi line)
struct block : public ast_node {
    block(std::list<command*>* commandlist) : commands(commandlist) {
    }
    virtual ~block();

    // list of block commands
    std::list<command*>* commands;

    // size of function frame
    int frame_size = CallBlockBaseSize; // call block and return value is implicitly present

    // maps identifiers to a specific cell (address)
    std::map<std::string, int> identifier_cell;

    // is this block a function scope? if yes, a frame will be allocated
    bool is_func_scope = false;

    std::list<variable_declaration>* injected_declarations = nullptr; // for function parameters

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// loop abstract parent
struct loop : public ast_node {
    loop(block* commands)
        : loop_commands(commands) {
        //
    };
    virtual ~loop();

    // every loop has a block of commands
    block* loop_commands;
};

struct while_loop : public loop {
    while_loop(value* exp, block* commands)
            : loop(commands), cond_expr(exp) {
        //
    }
    virtual ~while_loop();

    // condition to be evaluated BEFORE each iteration
    value* cond_expr;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        evaluate_error ret = evaluate_error::ok;

        // evaluate condition
        int condpos = static_cast<int>(context.generated_program.size());
        ret = cond_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // jump below loop block if false (previous evaluation leaves result on stack, JPC performs jump based on this value)
        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);

        // evaluate loop commands
        if (loop_commands) {
            ret = loop_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // jump at condition and repeat, if needed
            context.gen_instruction(pcode_fct::JMP, condpos);
        }

        // store current "address" to JPC instruction, so it knows where to jump
        context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

        return evaluate_error::ok;
    }
};

struct for_loop : public loop {
    for_loop(expression* exp1, value* exp2, expression* exp3, block* commands)
            : loop(commands), init_expr(exp1), cond_val(exp2), mod_expr(exp3) {
        //
    }
    virtual ~for_loop();

    expression* init_expr, *mod_expr;
    value* cond_val;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        evaluate_error ret = evaluate_error::ok;

        // evaluate initialization expression
        ret = init_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // store repeat position (we will jump here after each iteration)
        int repeatpos = static_cast<int>(context.generated_program.size());

        // evaluate condition value
        ret = cond_val->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // jump below loop block if false
        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);

        // evaluate loop commands
        if (loop_commands) {
            ret = loop_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        // evaluate modifying expression
        ret = mod_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // jump to condition
        context.gen_instruction(pcode_fct::JMP, repeatpos);

        // update JPC target position to jump here when the condition is not met
        context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

        return evaluate_error::ok;
    }
};

// condition structure
struct condition : public ast_node {
    condition(value* exp, block* commands, block* elsecommands = nullptr)
        : cond_expr(exp), true_commands(commands), false_commands(elsecommands) {
        //
    }
    virtual ~condition();

    // condition to be evaluated
    value* cond_expr;
    // true and false command blocks
    block* true_commands, *false_commands;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        evaluate_error ret = evaluate_error::ok;

        // evaluate condition
        ret = cond_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // prepare JPC, the later code will generate target address
        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);
        int truejmp = -1;

        // true commands (if condition is true)
        if (true_commands) {
            ret = true_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // jump below else block if any (do not execute "else" block)
            if (false_commands) {
                truejmp = context.gen_instruction(pcode_fct::JMP, 0);
            }
        }

        // JPC will always jump here if the condition is false (it will either execute the false block or continue)
        context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

        if (false_commands) {

            ret = false_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // if there is a false command block, true command block must jump after this
            context.generated_program[truejmp].arg.value = static_cast<int>(context.generated_program.size());
        }

        return evaluate_error::ok;
    }
};

// any declaration
struct declaration : public ast_node {
    declaration(int entity_major_type, const std::string& entity_identifier, int arraySize = 0)
        : type{ entity_major_type, 0 }, identifier(entity_identifier), array_size(arraySize) {
        //
    }

    declaration(int entity_major_type, std::string struct_name_identifier, std::string entity_identifier)
        : type{ entity_major_type, 0 }, identifier(entity_identifier), array_size(0), struct_name(struct_name_identifier) {
        //
    }

    std::string identifier;
    ptype_info type;
    int array_size;
    std::string struct_name;

    int override_frame_pos = std::numeric_limits<int>::max();

    // determines size (so the stack frame could be properly allocated)
    int determine_size(evaluate_context& context) const {
        int size = 0;
        // bool, char and int both has size of 1 (1 cell)
        if (type.major_type == TYPE_BOOL || type.major_type == TYPE_CHAR || type.major_type == TYPE_INT) {
            size = 1;
            // arrays occupy N-times more space, as one would expect
            if (array_size > 0) {
                size *= array_size;
            }
        }
        // string in this context occupies just one cell (address of the string literal)
        else if (type.major_type == TYPE_STRING) {
            size = 1;
        }
        // size of structure is determined as a sum of sizes of each of its members
        else if (type.major_type == TYPE_META_STRUCT) {
            auto decls = context.struct_defs.find(struct_name);
            // prior is_struct_defined call ensures existence
            for (auto decl : *decls->second) {
                size += decl->determine_size(context);
            }
        }

        return size;
    }

    virtual evaluate_error evaluate(evaluate_context& context) override {

        // if it's a struct declaration, verify its existence
        if (type.major_type == TYPE_META_STRUCT) {
            if (!context.is_struct_defined(struct_name)) {
                context.error_message = "Unknown type name '" + struct_name + "'";
                return evaluate_error::unknown_typename;
            }

            type.minor_type = context.struct_type_indices[struct_name];
        }

        block* scope = context.get_current_scope();

        // determine size of this type
        int size = determine_size(context);
        // scope is not global - resize a specific function frame
        if (scope) {
            // we did not override position
            if (override_frame_pos == std::numeric_limits<int>::max()) {
                scope->identifier_cell[identifier] = scope->frame_size;
                scope->frame_size += size;
            }
            else { // position override is done for function parameters (the variable resides in cell with negative index, see function calls)
                scope->identifier_cell[identifier] = override_frame_pos;
            }
        }
        else { // global - resize global "frame"
            context.global_identifier_cell[identifier] = context.global_frame_size;
            context.global_frame_size += size;
        }

        return evaluate_error::ok;
    }
};

// any variable declaration
struct variable_declaration : public global_statement {
    variable_declaration(declaration* vardecl, bool constant = false, value* initializer = nullptr)
        : decl(vardecl), is_constant(constant), initialized_by(initializer) {
        if (is_constant) {
            decl->type.is_const = true;
        }
    }
    virtual ~variable_declaration();

    declaration* decl = nullptr;
    bool is_constant;
    value* initialized_by;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        evaluate_error ret = decl->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // declare identifier if not previously declared
        ret = context.declare_identifier(decl->identifier, decl->type, decl->struct_name);

        if (ret != evaluate_error::ok) {
            if (ret == evaluate_error::redeclaration) {
                context.error_message = "Identifier '" + decl->identifier + "' redeclaration";
            }
            else if (ret == evaluate_error::redeclaration_different_types) {
                context.error_message = "Identifier '" + decl->identifier + "' redeclaration with different types (return value and/or parameters)";
            }
            else {
                context.error_message = "Unhandled error";
            }

            return ret;
        }

        // is this declaration initialized?
        if (initialized_by) {
            if (context.get_current_scope()) {
                ret = initialized_by->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }

                if (initialized_by->get_type_info(context) != decl->type) {
                    context.error_message = "Initializer type of '" + decl->identifier + "' does not match the variable type";
                    return evaluate_error::cannot_assign_type;
                }

                // store evaluation result (stack top) to a given variable
                context.gen_instruction(pcode_fct::STO, decl->identifier);
            }
            else {
                context.global_initializers[decl->identifier] = initialized_by;
            }
        }
        return evaluate_error::ok;

    }
};

// definition of a structure
struct struct_definition : public global_statement {
    struct_definition(std::string struct_name_identifier, std::list<declaration*>* multi_decl)
        : struct_name(struct_name_identifier), contents(multi_decl){
        //
    }
    virtual ~struct_definition();

    std::string struct_name;
    std::list<declaration*>* contents;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        // define struct with given name and contents
        context.define_struct(struct_name, contents);

        return evaluate_error::ok;
    }
};

// function declaration structure
struct function_declaration : public global_statement {
    function_declaration(declaration* fdecl, block* cmds, std::list<declaration*>* param_list = nullptr)
        : decl(fdecl), commands(cmds), parameters_list(param_list) {

        // block must behave differently, if it's a function scope (allocate frame)
        if (commands) {
            commands->is_func_scope = true;
        }

        decl->type.is_function = true;
    }
    virtual ~function_declaration();

    declaration* decl;
    block* commands;
    std::list<declaration*>* parameters_list;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        evaluate_error ret = evaluate_error::ok;

        // forward declarations does not have a block of commands
        bool is_forward_decl = (commands == nullptr);

        std::vector<ptype_info> param_types;
        if (parameters_list && parameters_list->size() > 0) {
            for (auto* decl : *parameters_list) {
                param_types.push_back(decl->type);
            }
        }

        // declare identifier if not previously declared
        ret = context.declare_identifier(decl->identifier, decl->type, decl->struct_name, is_forward_decl, param_types);

        if (ret != evaluate_error::ok) {
            if (ret == evaluate_error::redeclaration) {
                context.error_message = "Identifier '" + decl->identifier + "' redeclaration";
            }
            else if (ret == evaluate_error::redeclaration_different_types) {
                context.error_message = "Identifier '" + decl->identifier + "' redeclaration with different types (return value and/or parameters)";
            }
            else {
                context.error_message = "Unhandled error";
            }

            return ret;
        }

        // has commands (may be forward decl)
        if (!is_forward_decl) {

            // store the address this symbol is defined on
            context.global_identifier_cell[decl->identifier] = static_cast<int>(context.generated_program.size());

            std::list<variable_declaration> param_decl;

            // we expect parameters to be pushed to stack before function call
            // parameters are loaded into cells directly before called function stack frame
            // the callee then refers to parameters with negative cell index (-1 refers to last parameter, -2 to second to last, etc.)
            if (parameters_list) {
                int pos = -static_cast<int>(parameters_list->size());
                // transform "declaration" to "variable_declaration" in order to inject it to callee scope
                for (declaration* decl : *parameters_list) {
                    decl->override_frame_pos = pos++;
                    param_decl.emplace_back(decl, false, nullptr);
                }
            }

            // inject parameters to callee scope - it will reserve place for them and declare their names in it
            commands->injected_declarations = &param_decl;

            context.return_statement = false;
            ret = commands->evaluate(context);

            for (auto& decl : param_decl) {
                decl.decl = nullptr; // so the destructor will not delete it
            }

            if (ret != evaluate_error::ok) {
                return ret;
            }

            // check if a non-void function returns a value
            if (!context.return_statement) {
                if (decl->type.major_type != TYPE_VOID) {
                    context.error_message = "Function '" + decl->identifier + "' does not return any value";
                    return evaluate_error::nonvoid_func_no_return;
                }
            }

            context.gen_instruction(pcode_fct::OPR, pcode_opr::RETURN);
        }

        return evaluate_error::ok;

    }
};

// main program structure
struct program : public ast_node {
    program(std::list<global_statement*>* stmt_list)
        : statements(stmt_list) {
        //
    }
    virtual ~program();

    std::list<global_statement*>* statements;

    // declare builtin function (must be defined also by the interpreter)
    void declare_builtin(evaluate_context& context, const std::string& identifier, int slot, ptype_info type, const std::vector<ptype_info>& parameters) {
        context.declare_identifier(identifier, type, "", false, parameters);
        context.global_identifier_cell[identifier] = BuiltinBase + slot;
    }

    virtual evaluate_error evaluate(evaluate_context& context) override {

        declare_builtin(context, "consolePrintNum", 0, { TYPE_VOID, 0, true }, { { TYPE_INT } });
        declare_builtin(context, "consolePrintLnNum", 1, { TYPE_VOID, 0, true }, { { TYPE_INT } });
        declare_builtin(context, "consolePrintStr", 2, { TYPE_VOID, 0, true }, { { TYPE_STRING } });
        declare_builtin(context, "consolePrintLnStr", 3, { TYPE_VOID, 0, true }, { { TYPE_STRING } });
        declare_builtin(context, "consoleScanNum", 4, { TYPE_INT, 0, true }, {});

        int stackrespos = context.gen_instruction(pcode_fct::INT, 0);

        context.global_frame_size += CallBlockBaseSize; // reserve space for virtual call block (for genericity) and "main" return value

        int jmptoinitglobalspos = context.gen_instruction(pcode_fct::JMP, 0); // jump to code that initializes global variables

        // call "main" function
        context.gen_instruction(pcode_fct::CAL, pcode_arg("main", true));
        int looppos = context.gen_instruction(pcode_fct::JMP, 0);
        context.generated_program[looppos].arg.value = looppos;

        for (auto* stmt : *statements) {
            evaluate_error ret = stmt->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        int lvl, val;

        context.generated_program[stackrespos].arg.value = context.global_frame_size;

        context.generated_program[jmptoinitglobalspos].arg.value = static_cast<int>(context.generated_program.size());
        for (auto& inits : context.global_initializers) {
            inits.second->evaluate(context);
            context.find_identifier(inits.first, lvl, val);
            context.gen_instruction(pcode_fct::STO, val, lvl);
        }
        for (auto& strlit : context.string_literals) {

            for (int i = 0; i <= /* include null-terminator */ strlit.second.size(); i++) {
                context.gen_instruction(pcode_fct::LIT, strlit.second[i]);
                context.gen_instruction(pcode_fct::STO, strlit.first + i);
            }

        }
        context.gen_instruction(pcode_fct::JMP, jmptoinitglobalspos + 1); // jump back

        // resolve functions in all program commands ("link")
        for (int pos = 0; pos < context.generated_program.size(); pos++) {
            auto& instr = context.generated_program[pos];
            if (instr.instruction == pcode_fct::CAL && instr.arg.isref && instr.arg.isfunc) {
                if (context.find_identifier(instr.arg.symbolref, lvl, val)) {
                    instr.arg.resolve(val);
                    instr.lvl = 0;
                }
            }
        }

        for (int pos = 0; pos < context.generated_program.size(); pos++) {
            auto& instr = context.generated_program[pos];
            if (instr.arg.isref) {
                context.error_message = "Unresolved symbol '" + instr.arg.symbolref +"'";
                return evaluate_error::unresolved_reference;
            }
        }

        return evaluate_error::ok;
    }
};

