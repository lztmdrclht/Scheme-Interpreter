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
    return ClosureV(x, e, env);
} // lambda expression

Value Apply::eval(Assoc &e) {
    Value _rator = rator.get()->eval(e);
    auto temp_clos = dynamic_cast<Closure *>(_rator.get());
    if(temp_clos == nullptr || temp_clos->parameters.size() != rand.size())
        throw RuntimeError("Wrong Apply");

    Assoc new_env = temp_clos->env;
    for (size_t i = 0; i < temp_clos->parameters.size(); ++i) {
        new_env =extend(temp_clos->parameters[i], rand[i].get()->eval(e), new_env);
    }
    return temp_clos->e.get()->eval(new_env);

} // for function calling

Value Letrec::eval(Assoc &env) {
    std::vector <Value> values;
    Assoc new_env = env;
    for(int i = 0; i < bind.size(); i++)
        new_env = extend(bind[i].first, NullV(),new_env);
    for(int i = 0; i < bind.size(); i++) {
        if(bind[i].second->eval(new_env)->v_type == V_NULL)
            throw(RuntimeError("Null Variable"));
        values.push_back(bind[i].second->eval(new_env));
    }
    for(int i = 0; i < bind.size(); i++)
        modify(bind[i].first, values[i],new_env);
    return body->eval(new_env);
} // letrec expression


Value Var::eval(Assoc &e) {
    Value target_value = find(x, e);
    // std::cout << x << std::endl;
    if(target_value.get() != nullptr)
        return target_value;
    throw(RuntimeError("undefined variable"));
} // evaluation of variable

Value Fixnum::eval(Assoc &e) {
    return IntegerV(n);
} // evaluation of a fixnum

Value If::eval(Assoc &e) {
    Value temp = cond.get()->eval(e);
    auto temp_bool = dynamic_cast<Boolean *>(temp.get());
    if (temp_bool && temp_bool->b == false)
        return alter.get()->eval(e);
    return conseq.get()->eval(e);
} // if expression

Value True::eval(Assoc &e) {
    return BooleanV(true);
} // evaluation of #t

Value False::eval(Assoc &e) {
    return BooleanV(false);
} // evaluation of #f

Value Begin::eval(Assoc &e) {
    if(es.size() == 0)
        return NullV();
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
        if (copy_stxs.size() >= 3) {
            auto partition_dot = dynamic_cast<Identifier *>(copy_stxs[copy_stxs.size() - 2].get());
            if (partition_dot && partition_dot->s == ".") {
                Value temp_pair = Expr(new Quote(copy_stxs[copy_stxs.size() - 1])).get()->eval(e);
                for (int i = copy_stxs.size() - 3; i >= 0; --i)
                    temp_pair = PairV((Expr(new Quote(copy_stxs[i]))).get()->eval(e), temp_pair);
                return temp_pair;
            }
        }
        Value temp_pair = NullV();
        for (int i = copy_stxs.size() - 1; i >= 0; --i)
            temp_pair = PairV((Expr(new Quote(copy_stxs[i]))).get()->eval(e), temp_pair);
        return temp_pair;
    }
    return NullV();
} // quote expression

Value MakeVoid::eval(Assoc &e) {
    return VoidV();
} // (void)

Value Exit::eval(Assoc &e) {
    return TerminateV();
} // (exit)

Value Binary::eval(Assoc &e) {
    return evalRator(rand1->eval(e), rand2->eval(e));
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
    return evalRator(rand->eval(e));
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type1");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) * (dynamic_cast<Integer*>(rand2.get())->n)));
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type2");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) + (dynamic_cast<Integer*>(rand2.get())->n)));
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type3");
    return Value(new Integer((dynamic_cast<Integer*>(rand1.get())->n) - (dynamic_cast<Integer*>(rand2.get())->n)));
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type4");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) < (dynamic_cast<Integer*>(rand2.get())->n));
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type5");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) <= (dynamic_cast<Integer*>(rand2.get())->n));
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type6");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type7");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) >= (dynamic_cast<Integer*>(rand2.get())->n));
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type != V_INT || rand2.get()->v_type != V_INT)
        throw RuntimeError("wrong type8");
    return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) > (dynamic_cast<Integer*>(rand2.get())->n));
} // >
//
Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1 -> v_type == V_INT && rand2 -> v_type == V_INT)
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    if(rand1->v_type==V_BOOL and rand2->v_type==V_BOOL)
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    if(rand1->v_type == V_SYM && rand2->v_type == V_SYM)
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    if(rand1->v_type == V_NULL && rand2->v_type == V_NULL)
        return BooleanV(true);
    if(rand1->v_type == V_VOID && rand2->v_type == V_VOID)
        return BooleanV(true);
    return BooleanV(rand1.get() == rand2.get());
} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
    return PairV(rand1, rand2);
} // cons

Value IsBoolean::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Boolean *>(rand.get()) != nullptr);
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Integer *>(rand.get()) != nullptr);
} // fixnum?

Value IsSymbol::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Symbol *>(rand.get()) != nullptr);
} // symbol?

Value IsNull::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Null *>(rand.get()) != nullptr);
} // null?

Value IsPair::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Pair *>(rand.get()) != nullptr);
} // pair?

Value IsProcedure::evalRator(const Value &rand) {
    return BooleanV(dynamic_cast<Closure *>(rand.get()) != nullptr);
} // procedure?

Value Not::evalRator(const Value &rand) {
    if(rand.get()->v_type == V_BOOL && (dynamic_cast<Boolean*>(rand.get())->b) == false)
        return BooleanV(true);
    return BooleanV(false);
} // not

Value Car::evalRator(const Value &rand) {
    // if(rand.get()->v_type == V_NULL)
    //     return NullV();
    // std::cout << rand.get()->v_type << std::endl;
    // rand.get()->show(std::cout);
    if(rand.get()->v_type != V_PAIR)
        throw RuntimeError("wrong type9");
    return (dynamic_cast<Pair*>(rand.get())->car);
} // car

Value Cdr::evalRator(const Value &rand) {
    if(rand.get()->v_type != V_PAIR)
        throw RuntimeError("wrong type0");
    return (dynamic_cast<Pair*>(rand.get())->cdr);
} // cdr

// // //cd C:\Users\ROG\Desktop\Scheme-Interpreter-Main
// // //cd mnt/c/Users/ROG/Desktop/Scheme-Interpreter-Main/score