#ifndef PARSER
#define PARSER

// parser of myscheme

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "expr.hpp"
#include "value.hpp"
#include <map>
#include <cstring>
#include <iostream>
#define mp make_pair
using std :: string;
using std :: vector;
using std :: pair;

extern std :: map<std :: string, ExprType> primitives;
extern std :: map<std :: string, ExprType> reserved_words;

Expr Syntax :: parse(Assoc &env) {
    return (*ptr).parse(env);
}

Expr Number :: parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr Identifier::parse(Assoc &env) {
    if (find(s, env).get())
        return Expr(new Var(s));
    switch (primitives[s]) {
        case E_VOID:
        case E_EXIT: {
            List *st = new List();
            st->stxs.push_back(Syntax(new Identifier("lambda")));
            st->stxs.push_back(new List());
            List *stx = new List();
            stx->stxs.push_back(Syntax(new Identifier(s)));
            st->stxs.push_back(stx);
            return st->parse(env);
        }
        case E_MUL:
        case E_MINUS:
        case E_PLUS:
        case E_LT:
        case E_LE:
        case E_EQ:
        case E_GE:
        case E_GT:
        case E_EQQ:
        case E_CONS: {
            List *args = new List();
            args->stxs.push_back(Syntax(new Identifier("x")));
            args->stxs.push_back(Syntax(new Identifier("y")));
            List *stx = new List();
            stx->stxs.push_back(Syntax(new Identifier(s)));
            stx->stxs.push_back(Syntax(new Identifier("x")));
            stx->stxs.push_back(Syntax(new Identifier("y")));
            List *st = new List();
            st->stxs.push_back(Syntax(new Identifier("lambda")));
            st->stxs.push_back(args);
            st->stxs.push_back(stx);

            return st->parse(env);
        }
        case E_BOOLQ:
        case E_INTQ:
        case E_NULLQ:
        case E_PAIRQ:
        case E_PROCQ:
        case E_SYMBOLQ:
        case E_NOT:
        case E_CAR:
        case E_CDR: {
            List *args = new List();
            args->stxs.push_back(Syntax(new Identifier("x")));
            List *stx = new List();
            stx->stxs.push_back(Syntax(new Identifier(s)));
            stx->stxs.push_back(Syntax(new Identifier("x")));
            List *st = new List();
            st->stxs.push_back(Syntax(new Identifier("lambda")));
            st->stxs.push_back(args);
            st->stxs.push_back(stx);
            return st->parse(env);
        }
    }
    return Expr(new Var(s));
}

Expr TrueSyntax :: parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax :: parse(Assoc &env) {
    return Expr(new False());
}

Expr List :: parse(Assoc &env) {
    if (stxs.empty())
        return Expr(new MakeVoid());
//    if(dynamic_cast<Identifier*>(stxs[0].get()) == nullptr && dynamic_cast<List*>(stxs[0].get()) == nullptr)
//        throw(RuntimeError("syntax error"));

    if(dynamic_cast<List*>(stxs[0].get()) != nullptr) {
        Expr result = stxs[0]->parse(env);
        vector<Expr> expr_para;
        for(int i = 1; i < stxs.size(); i++)
            expr_para.push_back(stxs[i]->parse(env));
        return Expr(new Apply(result, expr_para));
    }

    if(dynamic_cast<Identifier *>(stxs[0].get()) != nullptr) {
        std::string temp_s = dynamic_cast<Identifier *>(stxs[0].get())->s;
        if(find(temp_s, env).get() != nullptr) {
            vector<Expr> expr_para;
            for(int i = 1; i < stxs.size(); i++)
                expr_para.push_back(stxs[i]->parse(env));
            return Expr(new Apply(stxs[0]->parse(env), expr_para));
            // return Expr(new Apply(new Var(temp_s), expr_para));
        }

        if(primitives.count(temp_s) != 0) {
            if(primitives[temp_s] == E_MUL) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Mult(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_MINUS) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Minus(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_PLUS) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Plus(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_LT) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Less(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_LE) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new LessEq(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_EQ) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Equal(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_GE) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new GreaterEq(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_GT) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Greater(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_VOID) {
                if(stxs.size() != 1) throw(RuntimeError("wrong parameter number"));
                else return Expr(new MakeVoid());
            }
            if(primitives[temp_s] == E_EQQ) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsEq(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_BOOLQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsBoolean(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_INTQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsFixnum(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_NULLQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsNull(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_PAIRQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsPair(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_PROCQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsProcedure(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_SYMBOLQ) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new IsSymbol(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_CONS) {
                if(stxs.size() != 3) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Cons(stxs[1]->parse(env), stxs[2]->parse(env)));
            }
            if(primitives[temp_s] == E_NOT) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Not(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_CAR) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Car(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_CDR) {
                if(stxs.size() != 2) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Cdr(stxs[1]->parse(env)));
            }
            if(primitives[temp_s] == E_EXIT) {
                if(stxs.size() != 1) throw(RuntimeError("wrong parameter number"));
                else return Expr(new Exit());
            }
        }

        if(reserved_words.count(temp_s) != 0) {
            if(reserved_words[temp_s] == E_LET) {
                if(stxs.size() != 3)
                    throw(RuntimeError("wrong parameter number"));

                vector<std::pair<std::string, Expr>> binded_var;
                List* var_list = dynamic_cast<List*>(stxs[1].get());
                if(var_list == nullptr)
                    throw(RuntimeError("wrong parameter type"));

                Assoc new_env = env;
                for(auto &var_expr : var_list->stxs) {
                    List* stx_tobind = dynamic_cast<List*>(var_expr.get());
                    if(stx_tobind->stxs.size() != 2)
                        throw(RuntimeError("wrong parameter number"));
                    Identifier* temp_s = dynamic_cast<Identifier*>(stx_tobind->stxs[0].get());
                    if(temp_s == nullptr)
                        throw(RuntimeError("wrong parameter type"));
                    std::string var_name = temp_s->s;
                    Expr temp_expr = (stx_tobind->stxs[1])->parse(env);
                    //                env = extend(var_name, NullV(), env);
                    new_env = extend(var_name, ExpressionV(temp_expr), new_env);

                    binded_var.push_back(std::make_pair(var_name, temp_expr));
                }
                return Expr(new Let(binded_var, stxs[2]->parse(new_env)));
            }
            if(reserved_words[temp_s] == E_IF) {
                if(stxs.size() != 4)
                    throw(RuntimeError("wrong parameter number"));
                return Expr(new If(stxs[1]->parse(env), stxs[2]->parse(env), stxs[3]->parse(env)));
            }
            if(reserved_words[temp_s] == E_BEGIN) {
                vector<Expr> exprs;
                for(int i = 1; i < stxs.size(); i++)
                    exprs.push_back(stxs[i]->parse(env));
                return Expr(new Begin(exprs));
            }
            if(reserved_words[temp_s] == E_QUOTE) {
                if(stxs.size() != 2)
                    throw(RuntimeError("wrong parameter number"));
                return Expr(new Quote(stxs[1]));
            }

            if(reserved_words[temp_s] == E_LAMBDA) {
                if(stxs.size() != 3)
                    throw(RuntimeError("wrong parameter number"));

                vector<std::string> paras;
                List* paras_ptr = dynamic_cast<List*>(stxs[1].get());
                Assoc new_env = env;

                if(paras_ptr == nullptr)
                    throw(RuntimeError("wrong parameter type"));
                for(auto &_para : paras_ptr->stxs) {
                    paras.push_back(dynamic_cast<Identifier *>(_para.get())->s);
                    if(find(dynamic_cast<Identifier *>(_para.get())->s, env).get() == nullptr)
                        new_env = extend(dynamic_cast<Identifier*>(_para.get())->s, VoidV(), new_env);
                }
                return Expr(new Lambda(paras, stxs[2]->parse(new_env)));
            }

            if(reserved_words[temp_s] == E_LETREC) {
                if(stxs.size() != 3)
                    throw(RuntimeError("wrong parameter number"));

                Assoc new_env = env;
                vector<std::pair<std::string, Expr>> binded_var;
                List* var_list = dynamic_cast<List*>(stxs[1].get());

                for(auto &var_expr : var_list->stxs) {
                    List* stx_tobind = dynamic_cast<List*>(var_expr.get());
                    if(stx_tobind == nullptr)
                        throw(RuntimeError("wrong parameter type"));
                    if(stx_tobind->stxs.size() != 2)
                        throw(RuntimeError("wrong parameter number"));
                    // Identifier* temp_s = dynamic_cast<Identifier*>(stx_tobind->stxs[0].get());
                    if(dynamic_cast<Identifier*>(stx_tobind->stxs[0].get()) == nullptr)
                        throw(RuntimeError("wrong parameter type"));
                    std::string var_name = dynamic_cast<Identifier*>(stx_tobind->stxs[0].get())->s;
                    //                env = extend(var_name, NullV(), env);
                    Expr temp_expr = (stx_tobind->stxs[1])->parse(env);
                    binded_var.push_back(std::make_pair(var_name, temp_expr));
                    new_env = extend(var_name, ExpressionV(temp_expr), new_env);
                }
                return Expr(new Letrec(binded_var, stxs[2]->parse(new_env)));
            }
        }
    }
    // stxs[0].get()->show(std::cout);
    throw(RuntimeError("Wrong2"));
}

#endif