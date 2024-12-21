#include "Def.hpp"
#include "syntax.hpp"
#include "expr.hpp"
#include "value.hpp"
#include "RE.hpp"
#include <sstream>
#include <iostream>
#include <map>


extern std :: map<std :: string, ExprType> primitives;
extern std :: map<std :: string, ExprType> reserved_words;

void REPL()
{
    // read - evaluation - print loop

    while (1)
    {
        Assoc global_env = empty();
        std::vector<std::string> x;
        x.push_back("x");
        x.push_back("y");
        global_env = extend("+", new Closure(x, new Plus(new Var("x"), new Var("y")), global_env), global_env);
        global_env = extend("-", new Closure(x, new Minus(new Var("x"), new Var("y")), global_env), global_env);
        global_env = extend("*", new Closure(x, new Mult(new Var("x"), new Var("y")), global_env), global_env);
        #ifndef ONLINE_JUDGE
            std::cout << "scm> ";
        #endif
        Syntax stx = readSyntax(std :: cin); // read
        try
        {
            Expr expr = stx -> parse(global_env); // parse
            // stx -> show(std :: cout); // syntax print
            Value val = expr -> eval(global_env);
            if (val -> v_type == V_TERMINATE)
                break;
            val -> show(std :: cout); // value print
        }
        catch (const RuntimeError &RE)
        {
            // std :: cout << RE.message();
            std :: cout << "RuntimeError";
        }
        puts("");
    }
}


int main(int argc, char *argv[]) {
    initPrimitives();
    initReservedWords();
    REPL();
    return 0;
}
