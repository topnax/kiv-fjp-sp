#include "ast.h"

evaluate_error command::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::invalid_state;

    if (vardecl) {
        ret = vardecl->evaluate(context);
    }
    else if (expressiondecl) {
        // TODO: retain return value if is_return_value and return it
        ret = expressiondecl->evaluate(context);
    }
    else if (loopdecl) {
        ret = loopdecl->evaluate(context);
    }
    else if (conddecl) {
        ret = conddecl->evaluate(context);
    }

    return ret;
}

evaluate_error expression::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::invalid_state;

    if (evalvalue) {
        ret = evalvalue->evaluate(context);
    }
    else if (bool_expression) {
        ret = bool_expression->evaluate(context);
    }

    return ret;
}

evaluate_error arithmetic::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::ok;

    ret = lhs_val->evaluate(context);
    if (ret != evaluate_error::ok) {
        return ret;
    }

    ret = rhs_val->evaluate(context);
    if (ret != evaluate_error::ok) {
        return ret;
    }

    //

    return evaluate_error::ok;
}

evaluate_error boolean_expression::evaluate(evaluate_context& context) {

    if (cmpval1) {
        if (cmpval2) {
            // compare cmpval1 and 2
            cmpval1->evaluate(context);
            cmpval2->evaluate(context);
            context.gen_instruction(pcode_fct::OPR, static_cast<int>(operation_to_pcode_opr(op)));
        }
        else {
            // is cmpval1 nonzero?
            cmpval1->evaluate(context);
            context.gen_instruction(pcode_fct::LIT, 0);
            context.gen_instruction(pcode_fct::OPR, static_cast<int>(pcode_opr::NOTEQUAL));
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
        context.gen_instruction(pcode_fct::LIT, preset_value ? 1 : 0);
    }

    return evaluate_error::ok;
}

boolean_expression::~boolean_expression() {
    if (cmpval1) {
        delete cmpval1;
    }
    if (cmpval2) {
        delete cmpval2;
    }
    if (boolexp1) {
        delete boolexp1;
    }
    if (boolexp2) {
        delete boolexp2;
    }
}

expression::~expression() {
    if (evalvalue) {
        delete evalvalue;
    }
    if (bool_expression) {
        delete bool_expression;
    }
}

assign_expression::~assign_expression() {
    if (arrayindex) {
        delete arrayindex;
    }
    delete assignvalue;
}

arithmetic::~arithmetic() {
    delete lhs_val;
    delete rhs_val;
}

function_call::~function_call() {
    if (parameters) {
        delete parameters;
    }
}

value::~value() {
    // TODO
}

command::~command() {
    if (vardecl) {
        delete vardecl;
    }
    if (expressiondecl) {
        delete expressiondecl;
    }
    if (loopdecl) {
        delete loopdecl;
    }
    if (conddecl) {
        delete conddecl;
    }
}

block::~block() {
    if (commands) {
        delete commands;
    }
}

loop::~loop() {
    delete loop_commands;
}

while_loop::~while_loop() {
    delete cond_expr;
}

for_loop::~for_loop() {
    delete init_expr;
    delete cond_expr;
    delete mod_expr;
}

condition::~condition() {
    delete cond_expr;
    delete true_commands;
    if (false_commands) {
        delete false_commands;
    }
}

variable_declaration::~variable_declaration() {
    if (decl) {
        delete decl;
    }
    if (initialized_by) {
        delete initialized_by;
    }
}

struct_definition::~struct_definition() {
    delete contents;
}

function_declaration::~function_declaration() {
    delete decl;
    delete commands;
    delete parameters_list;
}

program::~program() {
    for (auto* stmt : *statements) {
        delete stmt;
    }
    delete statements;
}