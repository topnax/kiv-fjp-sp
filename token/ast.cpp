#include "ast.h"

bool evaluate_context::find_identifier(const std::string& identifier, int& level, int& offset) {

    block* curscope = get_current_scope();
    if (curscope) {
        auto res = curscope->identifier_cell.find(identifier);
        if (res != curscope->identifier_cell.end()) {
            level = 0;
            offset = res->second;
            return true;
        }
    }

    // if the identifier is not found in current scope, look to global scope

    // TODO: global scope
    auto res = global_identifier_cell.find(identifier);
    if (res != global_identifier_cell.end()) {
        level = LevelGlobal;
        offset = res->second;
        return true;
    }

    return false;
}

evaluate_error command::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::invalid_state;

    if (vardecl) {
        ret = vardecl->evaluate(context);
    }
    else if (expressiondecl) {
        ret = expressiondecl->evaluate(context);

        // return value is on top of the stack

        if (is_return_stmt) {
            context.gen_instruction(pcode_fct::STO, 3, 1); // return value is stored as first cell (index = 3) of caller scope
            context.gen_instruction(pcode_fct::OPR, pcode_opr::RETURN);
        }
    }
    else if (loopdecl) {
        ret = loopdecl->evaluate(context);
    }
    else if (conddecl) {
        ret = conddecl->evaluate(context);
    }

    return ret;
}

evaluate_error assign_expression::evaluate(evaluate_context& context) {

    if (!context.is_identifier_declared(identifier)) {
        return evaluate_error::undeclared_identifier;
    }

    evaluate_error res = evaluate_error::ok;

    // evaluate the expression to be assigned to the given identifier
    res = assignvalue->evaluate(context);
    if (res != evaluate_error::ok) {
        return res;
    }

    if (arrayindex == nullptr) {
        context.gen_instruction(pcode_fct::STO, identifier);
    } else {
        // array index is not null, an array is being accessed an array

        // specify the base
        // TODO other than 0 base?
        context.gen_instruction(pcode_fct::LIT, 0);

        // evaluate the array index
        arrayindex->evaluate(context);

        // put the address of the start of the array to the top of the stack
        context.gen_instruction(pcode_fct::LIT, pcode_arg(identifier));

        // add the index to the address representing the start of the array
        context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);

        // put the value to the memory specified by the address (first two values on the stack)
        context.gen_instruction(pcode_fct::STA, 0, 0);
    }

    return evaluate_error::ok;

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

    context.gen_instruction(pcode_fct::OPR, operation_to_pcode_opr(op));

    return evaluate_error::ok;
}

evaluate_error boolean_expression::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::ok;

    if (cmpval1) {
        if (cmpval2) {
            // compare cmpval1 and 2
            ret = cmpval1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            ret = cmpval2->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            context.gen_instruction(pcode_fct::OPR, operation_to_pcode_opr(op));
        }
        else {
            // is cmpval1 nonzero?
            ret = cmpval1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            context.gen_instruction(pcode_fct::LIT, 0);
            context.gen_instruction(pcode_fct::OPR, pcode_opr::NOTEQUAL);
        }
    }
    else if (boolexp1) {
        if (boolexp2) {
            // bool operation on boolexp1 and 2

            ret = boolexp1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            ret = boolexp2->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // 0/1 and 0/1 on stack

            if (op == operation::b_and) {
                // sum of last 2 values must be 2 (both are 1)
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::LIT, 2);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::EQUAL);
            }
            else if (op == operation::b_or) {
                // sum of last 2 values must be non-0 (at least one is 1)
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::LIT, 0);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::NOTEQUAL);
            }
            else {
                return evaluate_error::invalid_state;
            }

        }
        else {

            if (op == operation::negate) {
                ret = boolexp1->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }
                // 0 or 1 on stack; negated = (value + 1) & 1

                context.gen_instruction(pcode_fct::LIT, 1);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ODD);
            }
            else {
                // is boolexp1 true?

                ret = boolexp1->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }
            }
        }
    }
    else {
        // use "preset_value"
        context.gen_instruction(pcode_fct::LIT, preset_value ? 1 : 0);
    }

    return evaluate_error::ok;
}

evaluate_error value::evaluate(evaluate_context& context) {

    // evaluate will always push value to stack

    evaluate_error ret = evaluate_error::ok;
    int addr;

    switch (val_type) {
        case value_type::const_int_literal:
            context.gen_instruction(pcode_fct::LIT, content.int_value);
            break;
        case value_type::const_str_literal:
            addr = context.store_global_string_literal(str_base);
            context.gen_instruction(pcode_fct::LIT, addr);
            break;
        case value_type::variable:
            context.gen_instruction(pcode_fct::LOD, content.variable->identifier);
            return content.variable->evaluate(context);
        case value_type::return_value:
            ret = content.call->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            // return value is at 0;3
            context.gen_instruction(pcode_fct::LOD, 3, 0);
            return ret;
        case value_type::arithmetic:
            return content.arithmetic_expression->evaluate(context);
        case value_type::array_element:
            // specify the base
            // TODO other than base 0?
            context.gen_instruction(pcode_fct::LIT, 0);

            // evaluate index value
            content.array_element.index->evaluate(context);

            // store the address of the start of the array
            context.gen_instruction(pcode_fct::LIT, pcode_arg(content.array_element.array->identifier));

            // add the index to the address of the start
            context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);

            // put the value given by the address present at the top of the stack to the top of the stack
            context.gen_instruction(pcode_fct::LDA,0,0);
            break;
        case value_type::member:
            // TODO
            break;
        case value_type::ternary:
        {
            ret = content.ternary.boolexpr->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);
            ret = content.ternary.positive->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            int jmppos = context.gen_instruction(pcode_fct::JMP, 0);
            context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

            ret = content.ternary.negative->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            context.generated_program[jmppos].arg.value = static_cast<int>(context.generated_program.size());

            break;
        }
    }

    return ret;
}

evaluate_error block::evaluate(evaluate_context& context) {

    std::cout << "Evaluating block" << std::endl;

    int stackrespos = -1;

    if (is_func_scope) {
        context.push_scope(this);
        stackrespos = context.gen_instruction(pcode_fct::INT, 0);
    }

    if (injected_declarations) {
        for (variable_declaration decl : *injected_declarations) {
            decl.evaluate(context);
        }
    }

    if (commands) {
        for (command* cmd : *commands) {
            evaluate_error ret = cmd->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }
    }

    if (is_func_scope) {
        context.generated_program[stackrespos].arg.value = frame_size;

        int lvl, val;

        for (int pos = stackrespos; pos < context.generated_program.size(); pos++) {
            auto& instr = context.generated_program[pos];

            if (instr.arg.isref) {
                bool found = context.find_identifier(instr.arg.symbolref, lvl, val);
                if (found) {
                    instr.arg.resolve(val);
                    instr.lvl = (instr.arg.isfunc) ? 0 : lvl;
                }
            }
        }

        context.pop_scope(this);
    }

    return evaluate_error::ok;
}

evaluate_error function_call::evaluate(evaluate_context& context) {

    std::cout << "Function call: " << function_identifier << std::endl;

    evaluate_error ret = evaluate_error::ok;

    // TODO: check if identifier is function and if parameters match function declaration
    if (!context.is_identifier_declared(function_identifier)) {
        return evaluate_error::undeclared_identifier;
    }

    // push parameters to stack
    if (parameters) {
        for (value* param : *parameters) {
            ret = param->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }
    }

    context.gen_instruction(pcode_fct::CAL, pcode_arg(function_identifier, true));

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