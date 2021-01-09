#include "ast.h"

bool evaluate_context::find_identifier(const std::string& identifier, int& level, int& offset) {

    // look for identifier in current scope
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
    }
    else if (loopdecl) {
        ret = loopdecl->evaluate(context);
    }
    else if (conddecl) {
        ret = conddecl->evaluate(context);
    }
    else if (retvalue) {

        ret = retvalue->evaluate(context);

        // return value is on top of the stack

        context.gen_instruction(pcode_fct::STO, ReturnValueCell, 1); // return value is stored as first cell (index = 3) of caller scope (level = 1)
        context.gen_instruction(pcode_fct::OPR, pcode_opr::RETURN);

        context.return_statement = true;
    }
    else {
        context.error_message = "Invalid state - symbol 'command' could not be evaluated in given context";
    }

    return ret;
}

evaluate_error assign_expression::evaluate(evaluate_context& context) {

    if (!context.is_identifier_declared(identifier)) {
        context.error_message = "Undeclared identifier '" + identifier + "'";
        return evaluate_error::undeclared_identifier;
    }

    evaluate_error res = evaluate_error::ok;

    // evaluate the expression to be assigned to the given identifier
    res = assignvalue->evaluate(context);
    if (res != evaluate_error::ok) {
        return res;
    }

    if (context.is_identifier_declared(identifier) && context.declared_identifiers[identifier].type.major_type == TYPE_META_STRUCT) {
        // assigning to a struct number
        int size = 0;

        // compute the offset
        for (auto def :*context.struct_defs[context.declared_identifiers[identifier].struct_name]){
            // check whether member identifier matches the definition identifier
            if (structmemberidentifier == def->identifier) {

                if (def->type != assignvalue->get_type_info(context)) {
                    context.error_message = "Cannot assign value to member '" + structmemberidentifier + "' of '" + identifier + "' due to different data types";
                    return evaluate_error::cannot_assign_type;
                }

                break;
            }

            size += def->determine_size(context);
        }
        // new pcode_arg with the given identifier and summed size as offset
        auto arg = pcode_arg(identifier);
        arg.offset = size;
        context.gen_instruction(pcode_fct::STO, arg);

        // is the assignment used as a right-hand side of another assignment?
        if (push_result_to_stack) {
            value tmp(identifier, structmemberidentifier);
            tmp.evaluate(context);
        }

    } else if (arrayindex == nullptr) {
        context.gen_instruction(pcode_fct::STO, identifier);

        const auto& tp = context.get_declared_identifier(identifier).type;

        // if left-hand side is const, we cannot assign
        if (tp.is_const) {
            context.error_message = "Cannot assign value to constant '" + identifier + "'";
            return evaluate_error::cannot_assign_const;
        }

        // check if LHS type is the same as RHS type
        if (context.get_declared_identifier(identifier).type != assignvalue->get_type_info(context)) {
            context.error_message = "Cannot assign value to variable '" + identifier + "' due to different data types";
            return evaluate_error::cannot_assign_type;
        }

        // is the assignment used as a right-hand side of another assignment?
        if (push_result_to_stack) {
            value tmp(new variable_ref(identifier.c_str()));
            tmp.evaluate(context);
        }
    } else {
        // array index is not null, an array is being accessed an array

        if (context.get_declared_identifier(identifier).type != assignvalue->get_type_info(context)) {
            context.error_message = "Cannot assign value to variable '" + identifier + "' due to different data types";
            return evaluate_error::cannot_assign_type;
        }

        // specify the base
        // TODO other than 0 base - resolver should replace "level" in there instead of following LIT (address) instruction
        context.gen_instruction(pcode_fct::LIT, 0);

        // evaluate the array index
        arrayindex->evaluate(context);

        // put the address of the start of the array to the top of the stack
        context.gen_instruction(pcode_fct::LIT, pcode_arg(identifier));

        // add the index to the address representing the start of the array
        context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);

        // put the value to the memory specified by the address (first two values on the stack)
        context.gen_instruction(pcode_fct::STA, 0, 0);

        // is the assignment used as a right-hand side of another assignment?
        if (push_result_to_stack) {
            value tmp(new variable_ref(identifier.c_str()), arrayindex);
            tmp.evaluate(context);
        }
    }

    return evaluate_error::ok;

}

evaluate_error expression::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::invalid_state;

    // wrap "value" evaluation, but ignore return value (discard stack value by calling INT 0 -1)
    if (evalvalue) {
        evalvalue->return_value_ignored = true;
        ret = evalvalue->evaluate(context);
    }
    else {
        context.error_message = "Invalid state - symbol 'expression' could not be evaluated in given context";
    }

    return ret;
}

evaluate_error arithmetic::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::ok;

    // all arithmetic expressions are binary in grammar

    // evaluate left-hand side
    ret = lhs_val->evaluate(context);
    if (ret != evaluate_error::ok) {
        return ret;
    }

    // evaluate right-hand side
    ret = rhs_val->evaluate(context);
    if (ret != evaluate_error::ok) {
        return ret;
    }

    // perform operation on two top values from stack
    context.gen_instruction(pcode_fct::OPR, operation_to_pcode_opr(op));

    return evaluate_error::ok;
}

evaluate_error boolean_expression::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::ok;

    // if cmpval1 is defined, it's comparation
    if (cmpval1) {
        // cmpval2 defined = perform comparison of two values
        if (cmpval2) {
            // evaluate first value
            ret = cmpval1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            // evaluate second value
            ret = cmpval2->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            // compare them - result is 0 or 1 on stack
            context.gen_instruction(pcode_fct::OPR, operation_to_pcode_opr(op));
        }
        else { // is cmpval1 true = is its value non-zero?
            ret = cmpval1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            context.gen_instruction(pcode_fct::LIT, 0);
            context.gen_instruction(pcode_fct::OPR, pcode_opr::NOTEQUAL);
        }
    }
    // no cmpval defined, but boolexp1 is defined - perform boolean operations
    else if (boolexp1) {
        // second boolean expression is defined, perform binary boolean operation
        if (boolexp2) {

            // evaluate first expression
            ret = boolexp1->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
            // evaluate second expression
            ret = boolexp2->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }

            // p-code does not support boolean expressions, so we must use arithmetic operations
            // to compensate for their absence

            // 0/1 and 0/1 on stack - perform some operations according to given operation

            // boolean AND - sum of last 2 values must be 2 (both are 1)
            if (op == operation::b_and) {
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::LIT, 2);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::EQUAL);
            }
            // boolean OR - sum of last 2 values must be non-0 (at least one is 1)
            else if (op == operation::b_or) {
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::LIT, 0);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::NOTEQUAL);
            }
            // no other binary boolean operator is present
            else {
                context.error_message = "Invalid state - symbol 'boolean_expression' could not be evaluated in given context";
                return evaluate_error::invalid_state;
            }

        }
        // just one bool expression is present - this means we either evaluate assertion or negation of it
        else {

            // negation - "!" token present before
            if (op == operation::negate) {

                // evaluate it, result is on stack (0 or 1)
                ret = boolexp1->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }

                // no standard p-code operation for negation, use boolean expressions to compensate for it
                // 0 or 1 on stack; negated = (value + 1) & 1 (p-code ODD operation)

                context.gen_instruction(pcode_fct::LIT, 1);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);
                context.gen_instruction(pcode_fct::OPR, pcode_opr::ODD);
            }
            // evaluate assertion (no operation)
            else {

                // evaluate it and leave result on stack as is
                ret = boolexp1->evaluate(context);
                if (ret != evaluate_error::ok) {
                    return ret;
                }
            }
        }
    }
    // no value or expression node is defined, use "preset value" - this means the boolean expression consists solely of a bool literal (true or false)
    else {
        // use "preset_value"
        context.gen_instruction(pcode_fct::LIT, preset_value ? 1 : 0);
    }

    return evaluate_error::ok;
}

evaluate_error value::evaluate(evaluate_context& context) {

    // evaluate will always push value to stack, unless "return_value_ignored" is defined

    evaluate_error ret = evaluate_error::ok;
    int addr;

    switch (val_type) {
        // constant integer literal - push constant to stack
        case value_type::const_int_literal:
            context.gen_instruction(pcode_fct::LIT, content.int_value);
            break;
        // constant string literal - push literal address to stack
        case value_type::const_str_literal:
            addr = context.store_global_string_literal(str_base);
            context.gen_instruction(pcode_fct::LIT, addr);
            break;
        // variable - load by address (identifier will be resolved later)
        case value_type::variable:
            context.gen_instruction(pcode_fct::LOD, content.variable->identifier);
            ret = content.variable->evaluate(context);
            break;
        // return value - evaluate the call, caller will store return value to a specific location
        case value_type::return_value:
            ret = content.call->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            // return value is at 0;ReturnValueCell (callee put it there)
            context.gen_instruction(pcode_fct::LOD, ReturnValueCell, 0);
            break;
        // arithmetic expression - just evaluate, arithmetic_expression evaluate method will leave result on stack
        case value_type::arithmetic:
            ret = content.arithmetic_expression->evaluate(context);
            break;
        // array element - load base, move by offset and load using extended p-code instruction set
        case value_type::array_element:
            // specify the base
            // TODO other than 0 base - resolver should replace "level" in there instead of following LIT (address) instruction
            context.gen_instruction(pcode_fct::LIT, 0);

            // evaluate index value
            ret = content.array_element.index->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }

            // store the address of the start of the array
            context.gen_instruction(pcode_fct::LIT, pcode_arg(content.array_element.array->identifier));

            // add the index to the address of the start
            context.gen_instruction(pcode_fct::OPR, pcode_opr::ADD);

            // put the value given by the address present at the top of the stack to the top of the stack
            context.gen_instruction(pcode_fct::LDA,0,0);
            break;
        // struct member - determine struct base and member offset and load it
        case value_type::member: {
            // check whether identifier is declared
            if (!context.is_identifier_declared(str_base)) {
                context.error_message = "Undeclared identifier '" + str_base + "'";
                ret = evaluate_error::undeclared_identifier;
                break;
            }

            // get the struct name based on the identifier
            auto struct_name = context.declared_identifiers[str_base].struct_name;

            // struct must be defined
            if (!context.is_struct_defined(struct_name)) {
                ret = evaluate_error::unknown_typename;
                break;
            }

            // load the struct definition
            auto struct_def = context.struct_defs[struct_name];

            // TODO: support nested struct members - determine_size is recursive
            // compute the offset by summing sizes of struct members till we find the required member
            int size = 0;
            bool found = false;
            for (auto member: *struct_def) {
                if (member->identifier == member_spec) {
                    found = true;
                    break;
                }
                // TODO: call is_struct_defined if it's a struct inside a struct
                size += member->determine_size(context);
            }

            // the member was not found
            if (!found) {
                context.error_message = "Undeclared struct member '" + member_spec + "'";
                ret = evaluate_error::undeclared_struct_member;
                break;
            }

            // new pcode_arg with the size as a offset
            auto p_arg = pcode_arg(str_base);
            p_arg.offset = size;

            context.gen_instruction(pcode_fct::LOD, p_arg, 0);

            break;
        }
        // ternary operator - evaluate condition and push value to stack according to boolean_expression result
        case value_type::ternary:
        {
            // evaluate conditional
            ret = content.ternary.boolexpr->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            // jump to false branch if false (address will be stored later, once known)
            int jpcpos = context.gen_instruction(pcode_fct::JPC, 0);
            // evaluate "true" branch
            ret = content.ternary.positive->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            // jump below "false" branch
            int jmppos = context.gen_instruction(pcode_fct::JMP, 0);
            // fill in the "false" JPC jump address
            context.generated_program[jpcpos].arg.value = static_cast<int>(context.generated_program.size());

            // evaluate "false" branch
            ret = content.ternary.negative->evaluate(context);
            if (ret != evaluate_error::ok) {
                break;
            }
            // fill the "true" branch jump destination
            context.generated_program[jmppos].arg.value = static_cast<int>(context.generated_program.size());

            break;
        }
        // assign expression - set "push to stack" flag and evaluate
        case value_type::assign_expression:
            // this will cause assign expression to push left-hand side of assignment to stack after assignment
            content.assign_expr->push_result_to_stack = true;
            ret = content.assign_expr->evaluate(context);
            break;
        // boolean expression - evaluate, result is stored to stack (0 or 1)
        case value_type::boolean_expression:
            ret = content.boolexpr->evaluate(context);
            break;
    }

    // if evaluated ok, but outer context decided to ignore return value, move stack pointer
    // this is a subject for optimalizations
    if (ret == evaluate_error::ok && return_value_ignored) {
        context.gen_instruction(pcode_fct::INT, -1);
    }

    return ret;
}

ptype_info value::get_type_info(const evaluate_context& context) const
{
    auto get_identifier_type_info = [&context](const std::string& identifier) -> ptype_info {
        if (!context.is_identifier_declared(identifier)) {
            return { TYPE_VOID, 0, false };
        }

        return context.get_declared_identifier(identifier).type;
    };

    switch (val_type) {
        case value_type::const_int_literal:
            return { TYPE_INT, 0, false };
        case value_type::const_str_literal:
            return { TYPE_STRING, 0, false };
        case value_type::variable:
            return get_identifier_type_info(content.variable->identifier);
        case value_type::return_value:
            return get_identifier_type_info(content.call->function_identifier);
        case value_type::array_element:
            return get_identifier_type_info(content.array_element.array->identifier);
        case value_type::member:
        {
            // get the struct name based on the identifier
            if (!context.is_identifier_declared(str_base)) {
                return { TYPE_VOID, 0, false };
            }
            const auto& def = context.get_declared_identifier(str_base);

            // load the struct definition
            auto struct_def = context.get_struct_definition(def.struct_name);

            for (auto member : *struct_def) {
                if (member->identifier == member_spec) {
                    return member->type;
                }
            }

            return { TYPE_VOID, 0, false };
        }
        case value_type::arithmetic:
            return { TYPE_INT, 0, false };
        case value_type::ternary:
        {
            ptype_info postype = content.ternary.positive->get_type_info(context);
            ptype_info negtype = content.ternary.negative->get_type_info(context);

            if (postype != negtype) {
                return { TYPE_VOID, 0, false };
            }

            return postype;
        }
        case value_type::assign_expression:
            return content.assign_expr->assignvalue->get_type_info(context);
            break;
        case value_type::boolean_expression:
            return { TYPE_BOOL, 0, false };
    }

    return { TYPE_VOID, 0, false };
}

evaluate_error block::evaluate(evaluate_context& context) {

    int stackrespos = -1;

    // if this is a function scope, prepare "frame allocation mark"
    // this will be filled later with "CallBlockBaseSize + size of local variables"
    // non-function scopes (like a block in condition/loop/...) do not allocate stack frames
    if (is_func_scope) {
        context.push_scope(this);
        stackrespos = context.gen_instruction(pcode_fct::INT, 0);
    }

    // if outer scope decided to inject some declarations, evaluate them here
    // this is typically just a case of function call parameters (must be declared in callee context)
    if (injected_declarations) {
        for (variable_declaration decl : *injected_declarations) {
            decl.evaluate(context);
        }
    }

    // evaluate commands if any (empty block does not have commands)
    if (commands) {
        for (command* cmd : *commands) {
            evaluate_error ret = cmd->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }
    }

    // if this is a function scope, perform some more operations
    if (is_func_scope) {
        // fill stack frame allocation instruction with actual frame size
        context.generated_program[stackrespos].arg.value = frame_size;

        int lvl, val;

        // resolve symbols used in this block
        // this must resolve variable symbols, but does not have to resolve functions
        // functions may have been forward-declared, in this case, "program::evaluate" will resolve them later
        for (int pos = stackrespos; pos < context.generated_program.size(); pos++) {
            auto& instr = context.generated_program[pos];

            // if the argument is a reference, try to resolve
            if (instr.arg.isref) {
                // find identifier
                bool found = context.find_identifier(instr.arg.symbolref, lvl, val);
                if (found) {
                    // resolve if found
                    instr.arg.resolve(val);
                    instr.lvl = (instr.arg.isfunc) ? 0 : lvl;
                }
            }
        }

        // pop scope from context
        context.pop_scope(this);
    }

    return evaluate_error::ok;
}

evaluate_error function_call::evaluate(evaluate_context& context) {

    evaluate_error ret = evaluate_error::ok;

    // check if the identifier exist
    if (!context.is_identifier_declared(function_identifier)) {
        context.error_message = "Undeclared identifier '" + function_identifier + "'";
        return evaluate_error::undeclared_identifier;
    }

    // retrieve declaration info
    const auto& declinfo = context.get_declared_identifier(function_identifier);

    // identifier must represent a function
    if (!declinfo.type.is_function) {
        context.error_message = "Identifier '" + function_identifier + "' is not a function";
        return evaluate_error::invalid_call;
    }

    if (!parameters && declinfo.func_parameters.size() > 0) {
        context.error_message = "Wrong number of parameters for '" + function_identifier + "' function call";
        return evaluate_error::invalid_call;
    }

    // push parameters to stack, if there should be some
    // the caller then refers to every parameter as to a cell with negative position (-1 in his base is the last parameter pushed by caller, etc.)
    if (parameters) {

        if (parameters->size() != declinfo.func_parameters.size()) {
            context.error_message = "Wrong number of parameters for '" + function_identifier + "' function call";
            return evaluate_error::invalid_call;
        }

        size_t parampos = 0;

        for (value* param : *parameters) {

            auto pi = param->get_type_info(context);

            if (pi != declinfo.func_parameters[parampos]) {
                context.error_message = "Type of parameter " + std::to_string(parampos) + " for '" + function_identifier + "' function call does not match";
                return evaluate_error::invalid_call;
            }
            parampos++;

            ret = param->evaluate(context);
            if (ret != evaluate_error::ok) {
                return ret;
            }
        }
    }

    // call the function
    context.gen_instruction(pcode_fct::CAL, pcode_arg(function_identifier, true));

    // clean up stack after function call
    if (parameters && parameters->size() > 0) {
        context.gen_instruction(pcode_fct::INT, -static_cast<int>(parameters->size()));
    }

    return evaluate_error::ok;
}

// destructors for cleaning up the AST

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
    switch (val_type) {
        case value_type::const_int_literal:
        case value_type::const_str_literal:
        default:
            break;
        case value_type::variable:
            delete content.variable;
            break;
        case value_type::return_value:
            delete content.call;
            break;
        case value_type::array_element:
            delete content.array_element.array;
            delete content.array_element.index;
            break;
        case value_type::arithmetic:
            delete content.arithmetic_expression;
            break;
        case value_type::ternary:
            delete content.ternary.boolexpr;
            delete content.ternary.positive;
            delete content.ternary.negative;
            break;
        case value_type::assign_expression:
            delete content.assign_expr;
            break;
        case value_type::boolean_expression:
            delete content.boolexpr;
            break;
    }
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
    delete cond_val;
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