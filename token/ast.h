#pragma once

#include <list>
#include <variant>
#include <string>

#include "types.h"

struct evaluate_context {
    //
};

struct ast_node {
    virtual int evaluate(evaluate_context& context) = 0;
};

struct global_statement : public ast_node {
    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct value;

struct expression : public ast_node {
    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct arithmetic : public ast_node {
    arithmetic(value* lhs, char* op, value* rhs) {
        //
    }

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct function_call : public ast_node {
    function_call(char* identifier, std::list<value*>* param_list = nullptr)
        : function_identifier(identifier), parameters(param_list) {

    }

    char* function_identifier;
    std::list<value*>* parameters;

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct variable_ref : public ast_node {
    variable_ref(char* varidentifier) : identifier(varidentifier) {
        //
    }

    char* identifier;

    virtual int evaluate(evaluate_context& context) override {
        //
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

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct variable_declaration;

struct command : public ast_node {
    command(variable_declaration* vardecl) {
        //
    }

    command(expression* expressiondecl, bool return_stmt) {
        //
    }

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct block : public ast_node {
    block(std::list<command*>* commandlist) : commands(commandlist) {

    }

    std::list<command*>* commands;

    virtual int evaluate(evaluate_context& context) override {

        if (commands) {
            for (command* cmd : *commands) {
                cmd->evaluate(context);
            }
        }

    }
};

struct declaration;

struct variable_declaration : public global_statement {
    variable_declaration(declaration* decl, bool constant = false, value* initializer = nullptr) {

    }

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct function_declaration : public global_statement {
    function_declaration(declaration* decl, block* cmds, std::list<declaration*>* param_list = nullptr) {
        //
    }

    function_declaration(declaration* decl, bool forwarddecl, std::list<declaration*>* param_list = nullptr) {
        //
    }

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct declaration : public ast_node {
    declaration(int type, char identifier[30], int arraySize = 0) {
        //
    }

    virtual int evaluate(evaluate_context& context) override {
        //
    }
};

struct program : public ast_node {
    program(std::list<global_statement*>* stmt_list)
        : statements(stmt_list) {
        //
    }

    std::list<global_statement*>* statements;

    virtual int evaluate(evaluate_context& context) override {

        for (auto* stmt : *statements) {
            stmt->evaluate(context);
        }

    }
};
