#include "Def.hpp"
#include "value.hpp"
#include "expr.hpp"
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
// #include <bits/fs_dir.h>

extern std :: map<std :: string, ExprType> primitives;
extern std :: map<std :: string, ExprType> reserved_words;

Value Let::eval(Assoc &env) {
    Assoc new_env = env;
    for (auto elem:bind) {
        new_env = extend(elem.first, elem.second->eval(env), new_env);
    }
    return body->eval(new_env);
} // let expression

Value Lambda::eval(Assoc &env) {
    return Value(new Closure(x, e, env));
} // lambda expression

Value Apply::eval(Assoc &e) {
    Closure* temp_clo = dynamic_cast<Closure*>(rator->eval(e).get());
    if(temp_clo == nullptr) {
        throw(RuntimeError("Not a Closure"));
    }
    Assoc new_env = temp_clo->env;
    std::vector< std::pair<std::string, Value> > tobind;
    for(int i = 0; i < temp_clo->parameters.size(); i++) {
        new_env = extend(temp_clo->parameters[i], rand[i]->eval(e), new_env);
    }
    return (temp_clo->e)->eval(new_env);
} // for function calling

Value Letrec::eval(Assoc &env) {
    std::vector <Value> values;
    Assoc new_env = env;
    for(int i = 0; i < bind.size(); i++)
        new_env = extend(bind[i].first, Value(NULL),new_env);
    for(int i = 0; i < bind.size(); i++)
        values.push_back(bind[i].second->eval(new_env));
    for(int i = 0; i < bind.size(); i++)
        modify(bind[i].first, values[i],new_env);
    return body->eval(new_env);
} // letrec expression



//初始化，加减乘加入var
Value Var::eval(Assoc &e) {
    Value target_value = find(x, e);
    // std::cout << x << std::endl;
    if(target_value.get() != nullptr)
        return target_value;
    // if(x == "+" || x == "-" || x == "*") {
    //     Value temp = new Symbol(x);
    //     return temp;
    // }
    if(target_value.get() == nullptr)
        throw(RuntimeError("undefined variable"));
} // evaluation of variable

Value Fixnum::eval(Assoc &e) {
    return Value(new Integer(n));
} // evaluation of a fixnum

Value If::eval(Assoc &e) {
    Expr temp = new Not(cond);
    if(dynamic_cast<Boolean*> (temp->eval(e).get())->b)
        return alter->eval(e);
    // if(dynamic_cast<Boolean*>(cond->eval(e).get()) == nullptr)
    //     throw(RuntimeError("Not a Boolean"));
    // if(dynamic_cast<Boolean*>(cond->eval(e).get())->b)
        return conseq->eval(e);
} // if expression

Value True::eval(Assoc &e) {
    return BooleanV(true);
} // evaluation of #t

Value False::eval(Assoc &e) {
    return BooleanV(false);
} // evaluation of #f

Value Begin::eval(Assoc &e) {
    for(int i = 0; i < es.size() - 1; i++) {
        es[i]->eval(e);
    }
    return es[es.size() - 1]->eval( e);
} // begin expression



Value Quote::eval(Assoc &e) {
    if(dynamic_cast<TrueSyntax *>(s.get()) != nullptr)
        return BooleanV(true);
    if(dynamic_cast<FalseSyntax *>(s.get()) != nullptr)
        return BooleanV(false);
    if(dynamic_cast<Number *>(s.get()) != nullptr)
        return IntegerV(dynamic_cast<Number *>(s.get())->n);
    if(dynamic_cast<Identifier *>(s.get()) != nullptr)
        return SymbolV(dynamic_cast<Identifier*>(s.get())->s);
    if(dynamic_cast<List *>(s.get()) != nullptr) {
        std::vector<Syntax> copy_stxs = dynamic_cast<List*>(s.get())->stxs;
        if(copy_stxs.size() == 0)
            return NullV();
        // Value temp_pair = Value(new Pair(copy_stxs[copy_stxs.size() - 1]->parse(e)->eval(e), NullV()));
        Expr temp_expr(new Quote(copy_stxs[copy_stxs.size() - 1]));
        Value temp_pair = Value(new Pair(temp_expr->eval(e), NullV()));
        for(int i = copy_stxs.size() - 2; i >= 0; i--) {
            if(i == copy_stxs.size() - 2 && dynamic_cast<Identifier*>(copy_stxs[i].get()) != nullptr) {
                if(dynamic_cast<Identifier*>(copy_stxs[i].get())->s == ".") {
                    temp_expr.ptr = new Quote(copy_stxs[i - 1]);
                    temp_pair = Value(new Pair(temp_expr->eval(e), Quote(copy_stxs[copy_stxs.size() - 1]).eval(e), true));
                    i --;
                    continue;
                }
            }
            temp_expr.ptr = new Quote(copy_stxs[i]);
            temp_pair = Value(new Pair(temp_expr->eval(e), temp_pair));
        }
        return temp_pair;
    }
} // quote expression

Value MakeVoid::eval(Assoc &e) {
    return Value(new Void());
} // (void)

Value Exit::eval(Assoc &e) {
    return Value(new Terminate());
} // (exit)

Value Binary::eval(Assoc &e) {
    return evalRator(rand1->eval(e), rand2->eval(e));
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
    return evalRator(rand->eval(e));
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) * (dynamic_cast<Integer*>(rand2.get())->n)));
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) + (dynamic_cast<Integer*>(rand2.get())->n)));
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) - (dynamic_cast<Integer*>(rand2.get())->n)));
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) < (dynamic_cast<Integer*>(rand2.get())->n));
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) <= (dynamic_cast<Integer*>(rand2.get())->n));
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) >= (dynamic_cast<Integer*>(rand2.get())->n));
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) > (dynamic_cast<Integer*>(rand2.get())->n));
} // >

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1 -> v_type == V_INT && rand2 -> v_type == V_INT)
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    if(rand1->v_type==V_BOOL and rand2->v_type==V_BOOL)
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    if(rand1->v_type == V_BOOL && rand2->v_type == V_BOOL)
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    if(rand1->v_type == V_NULL && rand2->v_type == V_NULL)
        return BooleanV(true);
    if(rand1->v_type == V_VOID && rand2->v_type == V_VOID)
        return BooleanV(true);
    return BooleanV(rand1.get() == rand2.get());
} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
    return Value(new Pair(rand1, rand2));
} // cons

Value IsBoolean::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_BOOL));
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_INT));
} // fixnum?

Value IsSymbol::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_SYM));
} // symbol?

Value IsNull::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_NULL));
} // null?

Value IsPair::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_PAIR));
} // pair?

Value IsProcedure::evalRator(const Value &rand) {
    return Value(new Boolean((*rand.ptr).v_type == V_PROC));
} // procedure?

Value Not::evalRator(const Value &rand) {
    if(rand.get()->v_type == V_BOOL && (dynamic_cast<Boolean*>(rand.get())->b) == false)
        return BooleanV(true);
    return BooleanV(false);
} // not

Value Car::evalRator(const Value &rand) {
    if(rand.get()->v_type != V_PAIR)
        throw RuntimeError("wrong type");
    return (dynamic_cast<Pair*>(rand.get())->car);
} // car

Value Cdr::evalRator(const Value &rand) {
    if(rand.get()->v_type != V_PAIR)
        throw RuntimeError("wrong type");
    return (dynamic_cast<Pair*>(rand.get())->cdr);
} // cdr

//cd C:\Users\ROG\Desktop\Scheme-Interpreter-Main
//cd mnt/c/Users/ROG/Desktop/Scheme-Interpreter-Main/score