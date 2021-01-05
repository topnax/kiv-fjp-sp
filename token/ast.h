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
    ok,
    undeclared_identifier,
    unknown_typename,
    invalid_state,
    unresolved_reference,
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
// to speed things up, the interpreter immediatelly jumps to global scope base when this level valus is seen
constexpr uint8_t LevelGlobal = 255;

// this is where builtin functions start
constexpr int BuiltinBase = 0x00FFFFFF;

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

    bool isref;
    bool isfunc = false;
    int value;
    std::string symbolref;

    void resolve(int addr_value) {
        value = addr_value;
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
            default: ret = "???"; break;
        }

        ret += " " + std::to_string(lvl) + " " + (arg.isref ? "<"+arg.symbolref+">" : std::to_string(arg.value));

        return ret;
    }
};

struct block;
struct declaration;
struct value;

// struct of declared identifier
struct declared_identifier {
    block* scope;
};

struct evaluate_context {
    // all currently declared identifiers
    std::map<std::string, declared_identifier> declared_identifiers;
    // scope stack
    std::stack<block*> current_scope;
    // defined structures
    std::map<std::string, std::list<declaration*>*> struct_defs;

    // size of global variables
    int global_frame_size = 0;
    std::map<std::string, int> global_identifier_cell;
    // global initializers table
    std::map<std::string, value*> global_initializers;
    // string literals to be defined (key = cell where string starts)
    std::map<int, std::string> string_literals;

    // declare identifier in current scope
    bool declare_identifier(const std::string& identifier) {
        if (declared_identifiers.find(identifier) == declared_identifiers.end()) {
            declared_identifiers[identifier] = {
                get_current_scope()
            };
            return true;
        }

        return false;
    }

    // undeclare existing identifier
    bool undeclare_identifier(const std::string& identifier) {
        if (declared_identifiers.find(identifier) != declared_identifiers.end()) {
            declared_identifiers.erase(identifier);
            return true;
        }

        return false;
    }

    bool is_identifier_declared(const std::string& identifier) {
        return (declared_identifiers.find(identifier) != declared_identifiers.end());
    }

    // define new structure
    bool define_struct(const std::string& name, std::list<declaration*>* decl) {
        if (struct_defs.find(name) == struct_defs.end()) {
            struct_defs[name] = decl;
            return true;
        }

        return false;
    }

    bool is_struct_defined(const std::string& name) {
        return (struct_defs.find(name) != struct_defs.end());
    }

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
        if (scope != blk) {
            return false;
        }

        current_scope.pop();

        std::vector<std::string> toerase;

        // undeclare identifiers from this scope
        for (auto decl : declared_identifiers) {
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

    int gen_instruction(pcode_fct instr, pcode_arg argument, int level = 0) {
        generated_program.emplace_back(instr, level, argument);

        return static_cast<int>(generated_program.size() - 1);
    }

    std::string text_out() const {
        std::ostringstream out;

        for (auto& instr : generated_program) {
            out << instr.to_string() << std::endl;
        }

        return out.str();
    }

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

    block* get_current_scope() const {
        return current_scope.empty() ? nullptr : current_scope.top();
    }

    bool find_identifier(const std::string& identifier, int& level, int& offset);
};

// used by block to start and end scope
struct scope_guard {
    scope_guard(evaluate_context& ctx, block* blk)
        : context(ctx), guarded_block(blk) {
        context.push_scope(guarded_block);
    }
    ~scope_guard() {
        context.pop_scope(guarded_block);
    }

    evaluate_context& context;
    block* guarded_block;
};

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

    static operation str_to_bool_op(char op) {
        if (op == '!') {
            return operation::negate;
        }
        return operation::none;
    }
    static operation str_to_bool_op(const std::string op) {
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

    value* cmpval1 = nullptr, *cmpval2 = nullptr;
    boolean_expression* boolexp1 = nullptr, *boolexp2 = nullptr;
    operation op = operation::none;
    bool preset_value = false;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// any expression (assign, boolean or value)
struct expression : public ast_node {

    expression() {}

    expression(value* val)
        : evalvalue(val) {
        //
    }

    expression(boolean_expression* boolexpr)
        : bool_expression(boolexpr) {
        //
    }

    virtual ~expression();

    value* evalvalue = nullptr;
    boolean_expression* bool_expression = nullptr;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// assignment expression
struct assign_expression : public expression {

    assign_expression(char* assidentifier, value* arrayidx, value* val)
        : identifier(assidentifier), arrayindex(arrayidx), assignvalue(val) {
        //
    }

    assign_expression(char* structidentifier, char* memberidentifier, value* val)
        : identifier(structidentifier), structmemberidentifier(memberidentifier), assignvalue(val) {

    }

    virtual ~assign_expression();

    std::string identifier;
    std::string structmemberidentifier;
    value* arrayindex = nullptr;
    value* assignvalue;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// arithmetic expression
struct arithmetic : public ast_node {

    enum class operation {
        none,
        add,
        sub,
        mul,
        div,
    };

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

    value* lhs_val, *rhs_val;
    operation op;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// function call
struct function_call : public ast_node {
    function_call(char* identifier, std::list<value*>* param_list = nullptr)
        : function_identifier(identifier), parameters(param_list) {
    }
    virtual ~function_call();

    std::string function_identifier;
    std::list<value*>* parameters;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

// wrapper for variable reference in value token
struct variable_ref : public ast_node {
    variable_ref(char* varidentifier) : identifier(varidentifier) {
        //
    }

    std::string identifier;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        // only check if identifier exists
        if (!context.is_identifier_declared(identifier)) {
            return evaluate_error::undeclared_identifier;
        }

        return evaluate_error::ok;
    }
};

struct value : public ast_node {
    enum class value_type {
        const_int_literal,
        const_str_literal,
        variable,
        return_value,
        member,
        array_element,
        arithmetic,
        ternary
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
    } content;

    std::string str_base; // str_value, struct_name
    std::string member_spec;

    value(int val) : val_type(value_type::const_int_literal) {
        content.int_value = val;
    }

    value(const std::string& val) : val_type(value_type::const_str_literal) {

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

    virtual ~value();

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

struct variable_declaration;
struct expression;
struct loop;
struct condition;

struct command : public ast_node {
    command(variable_declaration* decl)
        : vardecl(decl) {
        //
    }

    command(expression* decl, bool return_stmt)
        : expressiondecl(decl), is_return_stmt(return_stmt) {
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
    bool is_return_stmt = false;
    loop* loopdecl = nullptr;
    condition* conddecl = nullptr;

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

struct block : public ast_node {
    block(std::list<command*>* commandlist) : commands(commandlist) {
    }
    virtual ~block();

    std::list<command*>* commands;

    int frame_size = 4; // call block and return value is implicitly present
    std::map<std::string, int> identifier_cell;

    bool is_func_scope = false;

    std::list<variable_declaration>* injected_declarations = nullptr; // for function parameters

    virtual evaluate_error evaluate(evaluate_context& context) override;
};

struct loop : public ast_node {
    loop(block* commands)
        : loop_commands(commands) {
        //
    };
    virtual ~loop();

    block* loop_commands;
};

struct while_loop : public loop {
    while_loop(expression* exp, block* commands)
            : loop(commands), cond_expr(exp) {
        //
    }
    virtual ~while_loop();

    expression* cond_expr;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Evaluating while loop" << std::endl;

        evaluate_error ret = evaluate_error::ok;

        // evaluate condition
        int condpos = static_cast<int>(context.generated_program.size());
        ret = cond_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // jump below loop block if false
        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);

        if (loop_commands) {
            ret = loop_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            context.gen_instruction(pcode_fct::JMP, condpos);
        }

        context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

        return evaluate_error::ok;
    }
};

struct for_loop : public loop {
    for_loop(expression* exp1, expression* exp2, expression* exp3, block* commands)
            : loop(commands), init_expr(exp1), cond_expr(exp2), mod_expr(exp3) {
        //
    }
    virtual ~for_loop();

    expression* init_expr, *mod_expr;
    expression *cond_expr;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Evaluating for loop" << std::endl;

        evaluate_error ret = evaluate_error::ok;

        ret = init_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        int repeatpos = static_cast<int>(context.generated_program.size());

        ret = cond_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);

        if (loop_commands) {
            ret = loop_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        ret = mod_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        context.gen_instruction(pcode_fct::JMP, repeatpos);

        context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

        return evaluate_error::ok;
    }
};

struct condition : public ast_node {
    condition(expression* exp, block* commands, block* elsecommands = nullptr)
        : cond_expr(exp), true_commands(commands), false_commands(elsecommands) {
        //
    }
    virtual ~condition();

    expression* cond_expr;
    block* true_commands, *false_commands;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Evaluating condition" << std::endl;

        evaluate_error ret = evaluate_error::ok;

        ret = cond_expr->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);
        int truejmp = -1;

        if (true_commands) {
            ret = true_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // jump below else block if any
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

struct declaration : public ast_node {
    declaration(int entity_type, const std::string& entity_identifier, int arraySize = 0)
        : type(entity_type), identifier(entity_identifier), array_size(arraySize) {
        //
    }

    declaration(int entity_type, std::string struct_name_identifier, std::string entity_identifier)
        : type(entity_type), identifier(entity_identifier), array_size(0), struct_name(struct_name_identifier) {
        //
    }

    std::string identifier;
    int type;
    int array_size;
    std::string struct_name;

    int override_frame_pos = std::numeric_limits<int>::max();

    int determine_size(evaluate_context& context) const {
        int size = 0;
        if (type == TYPE_BOOL || type == TYPE_CHAR || type == TYPE_INT) {
            size = 1;
            if (array_size > 0) {
                size *= array_size;
            }
        }
        else if (type == TYPE_STRING) {
            size = 1;
            // TODO: somehow determine string size
        }
        else if (type == TYPE_META_STRUCT) {
            auto decls = context.struct_defs.find(struct_name);
            // prior is_struct_defined call ensures existence
            for (auto decl : *decls->second) {
                size += decl->determine_size(context);
            }
        }

        return size;
    }

    virtual evaluate_error evaluate(evaluate_context& context) override {

        if (type == TYPE_META_STRUCT) {
            if (!context.is_struct_defined(struct_name)) {
                return evaluate_error::unknown_typename;
            }
        }

        block* scope = context.get_current_scope();

        int size = determine_size(context);
        if (scope) {
            if (override_frame_pos == std::numeric_limits<int>::max()) {
                scope->identifier_cell[identifier] = scope->frame_size;
                scope->frame_size += size;
            }
            else {
                scope->identifier_cell[identifier] = override_frame_pos;
            }
        }
        else {
            context.global_identifier_cell[identifier] = context.global_frame_size;
            context.global_frame_size += size;
        }

        return evaluate_error::ok;
    }
};

struct variable_declaration : public global_statement {
    variable_declaration(declaration* vardecl, bool constant = false, value* initializer = nullptr)
        : decl(vardecl), is_constant(constant), initialized_by(initializer) {
        //
    }
    virtual ~variable_declaration();

    declaration* decl = nullptr;
    bool is_constant;
    value* initialized_by;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Declaring variable: " << decl->identifier << std::endl;

        evaluate_error ret = decl->evaluate(context);
        if (ret != evaluate_error::ok) {
            return ret;
        }

        // TODO: type
        context.declare_identifier(decl->identifier);
        if (initialized_by) {
            if (context.get_current_scope()) {
                ret = initialized_by->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }

                context.gen_instruction(pcode_fct::STO, decl->identifier);
            }
            else {
                context.global_initializers[decl->identifier] = initialized_by;
            }
        }

        return evaluate_error::ok;

    }
};

struct struct_definition : public global_statement {
    struct_definition(std::string struct_name_identifier, std::list<declaration*>* multi_decl)
        : struct_name(struct_name_identifier), contents(multi_decl){
        //
    }
    virtual ~struct_definition();

    std::string struct_name;
    std::list<declaration*>* contents;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Defining structure: " << struct_name << std::endl;

        context.define_struct(struct_name, contents);

        return evaluate_error::ok;
    }
};

struct function_declaration : public global_statement {
    function_declaration(declaration* fdecl, block* cmds, std::list<declaration*>* param_list = nullptr)
        : decl(fdecl), commands(cmds), parameters_list(param_list) {

        if (commands) {
            commands->is_func_scope = true;
        }
    }
    virtual ~function_declaration();

    declaration* decl;
    block* commands;
    std::list<declaration*>* parameters_list;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Declaring function: " << decl->identifier << std::endl;

        context.declare_identifier(decl->identifier);

        if (commands) {

            context.global_identifier_cell[decl->identifier] = static_cast<int>(context.generated_program.size());

            std::list<variable_declaration> param_decl;

            // we expect parameters to be pushed to stack before function call
            if (parameters_list) {
                int pos = -static_cast<int>(parameters_list->size());
                for (declaration* decl : *parameters_list) {
                    decl->override_frame_pos = pos++;
                    param_decl.emplace_back(decl, false, nullptr);
                }
            }

            commands->injected_declarations = &param_decl;

            evaluate_error ret = commands->evaluate(context);

            for (auto& decl : param_decl) {
                decl.decl = nullptr; // so the destructor will not delete it
            }

            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        context.gen_instruction(pcode_fct::OPR, pcode_opr::RETURN);

        return evaluate_error::ok;

    }
};

struct program : public ast_node {
    program(std::list<global_statement*>* stmt_list)
        : statements(stmt_list) {
        //
    }
    virtual ~program();

    std::list<global_statement*>* statements;

    void declare_builtin(evaluate_context& context, const std::string& identifier, int slot) {
        context.declare_identifier(identifier);
        context.global_identifier_cell[identifier] = BuiltinBase + slot;
    }

    virtual evaluate_error evaluate(evaluate_context& context) override {

        declare_builtin(context, "consolePrintNum", 0);
        declare_builtin(context, "consolePrintLnNum", 1);
        declare_builtin(context, "consolePrintStr", 2);
        declare_builtin(context, "consolePrintLnStr", 3);
        declare_builtin(context, "consoleScanNum", 4);

        int stackrespos = context.gen_instruction(pcode_fct::INT, 0);

        context.global_frame_size += 4; // reserve space for virtual call block (for genericity) and "main" return value

        int jmptoinitglobalspos = context.gen_instruction(pcode_fct::JMP, 0); // jump to code that initializes global variables

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
                return evaluate_error::unresolved_reference;
            }
        }

        return evaluate_error::ok;
    }
};

