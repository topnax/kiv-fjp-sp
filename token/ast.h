#pragma once

#include <list>
#include <variant>
#include <string>
#include <map>
#include <stack>
#include <iostream>

#include "types.h"

enum class evaluate_error
{
    ok,
    undeclared_identifier,
    unknown_typename,
    invalid_state,
};

struct block;
struct declaration;

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

    // declare identifier in current scope
    bool declare_identifier(const std::string& identifier) {
        if (declared_identifiers.find(identifier) == declared_identifiers.end()) {
            declared_identifiers[identifier] = {
                current_scope.empty() ? nullptr : current_scope.top()
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

    // push new scope defined by block
    bool push_scope(block* blk) {
        current_scope.push(blk);
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

        // undeclare identifiers from this scope
        for (auto decl : declared_identifiers) {
            if (decl.second.scope == scope) {
                undeclare_identifier(decl.first);
            }
        }
    }
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
    operation op;
    bool preset_value;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        if (cmpval1) {
            if (cmpval2) {
                // compare cmpval1 and 2
            }
            else {
                // is cmpval1 non zero?
            }
        }
        else if (boolexp1) {
            if (boolexp2) {
                // bool operation on boolexp1 and 2
            }
            else {
                // is boolexp1 true?
            }
        }
        else {
            // use "preset_value"
        }

        return evaluate_error::ok;
    }
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

    virtual evaluate_error evaluate(evaluate_context& context) override {

        if (!context.is_identifier_declared(identifier)) {
            return evaluate_error::undeclared_identifier;
        }

        //

        return evaluate_error::ok;

    }
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

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Function call: " << function_identifier << std::endl;

        // TODO: check if identifier is function and if parameters match function declaration
        if (!context.is_identifier_declared(function_identifier)) {
            return evaluate_error::undeclared_identifier;
        }

        // this should push return value to stack maybe

        return evaluate_error::ok;
    }
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
        const_literal,
        variable,
        return_value,
        member,
        array_element,
        arithmetic
    };

    value_type val_type;
    union {
        int int_value;
        char* str_value;
        variable_ref* variable;
        function_call* call;
        struct {
            char* struct_name;
            char* member;
        } struct_path;
        struct {
            char* array_name;
            value* index;
        } array_element;
        arithmetic* arithmetic_expression;
    } content;

    value(int val) : val_type(value_type::const_literal) {
        content.int_value = val;
    }

    value(char* val) : val_type(value_type::const_literal) {
        content.str_value = val;
    }

    value(variable_ref* varref) : val_type(value_type::variable) {
        content.variable = varref;
    }

    value(function_call* func_call) : val_type(value_type::return_value) {
        content.call = func_call;
    }

    value(char* structname, char* member) : val_type(value_type::member) {
        content.struct_path.struct_name = structname;
        content.struct_path.member = member;
    }

    value(char* arrayname, value* index) : val_type(value_type::array_element) {
        content.array_element.array_name = arrayname;
        content.array_element.index = index;
    }

    value(arithmetic* arithmetic_expr) : val_type(value_type::arithmetic) {
        content.arithmetic_expression = arithmetic_expr;
    }

    virtual ~value();

    virtual evaluate_error evaluate(evaluate_context& context) override {

        // evaluate will always push value to stack

        switch (val_type) {
            case value_type::const_literal:
                //
                break;
            case value_type::variable:
                return content.variable->evaluate(context);
            case value_type::return_value:
                return content.call->evaluate(context);
            case value_type::arithmetic:
                return content.arithmetic_expression->evaluate(context);
            case value_type::array_element:
                //
                break;
            case value_type::member:
                //
                break;
        }

        return evaluate_error::ok;
    }
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

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Evaluating block" << std::endl;
        scope_guard guard(context, this);

        if (commands) {
            for (command* cmd : *commands) {
                evaluate_error ret = cmd->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }
            }
        }

        return evaluate_error::ok;

    }
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

        //

        if (loop_commands) {
            return loop_commands->evaluate(context);
        }

        return evaluate_error::ok;
    }
};

struct for_loop : public loop {
    for_loop(expression* exp1, expression* exp2, expression* exp3, block* commands)
            : loop(commands), init_expr(exp1), cond_expr(exp2), mod_expr(exp3) {
        //
    }
    virtual ~for_loop();

    expression* init_expr, *cond_expr, *mod_expr;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Evaluating for loop" << std::endl;

        //

        if (loop_commands) {
            return loop_commands->evaluate(context);
        }

        //

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

        //

        if (true_commands) {
            ret = true_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        if (false_commands) {
            ret = false_commands->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        return evaluate_error::ok;
    }
};

struct declaration : public ast_node {
    declaration(int entity_type, char entity_identifier[30], int arraySize = 0)
        : type(entity_type), identifier(entity_identifier), array_size(arraySize) {
        //
    }

    declaration(int entity_type, std::string struct_name_identifier, std::string entity_identifier)
        : type(entity_type), identifier(entity_identifier), struct_name(struct_name_identifier) {
        //
    }

    std::string identifier;
    int type;
    int array_size;
    std::string struct_name;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        if (type == TYPE_META_STRUCT) {
            if (!context.is_struct_defined(struct_name)) {
                return evaluate_error::unknown_typename;
            }
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

        // TODO: type and assigned address and memory
        context.declare_identifier(decl->identifier);

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
        //
    }
    virtual ~function_declaration();

    declaration* decl;
    block* commands;
    std::list<declaration*>* parameters_list;

    virtual evaluate_error evaluate(evaluate_context& context) override {

        std::cout << "Declaring function: " << decl->identifier << std::endl;

        // TODO: parameters and assigned address and memory
        context.declare_identifier(decl->identifier);

        if (commands) {
            return commands->evaluate(context);
        }

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

    virtual evaluate_error evaluate(evaluate_context& context) override {

        for (auto* stmt : *statements) {
            evaluate_error ret = stmt->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }

        return evaluate_error::ok;

    }
};

