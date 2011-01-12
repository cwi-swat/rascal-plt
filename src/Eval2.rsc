module Eval2

// local side effects, returning env

import AST;

import List;
import IO;

alias Env = map[str, int];
alias PEnv = map[str, Func];

alias Result = tuple[Env, int];

public Result eval(str main, list[int] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  env = ( f.formals[i] : args[i] | i <- domain(f.formals) ); 
  return eval(f.body, env, penv);
}

public Result eval(Exp exp, Env env, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return <env, nat>;
 
    case var(str name):
       return <env, env[name]>;       
       
    case mul(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, x * y>;
    } 
      
    case div(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, x / y>;
    } 
      
    case add(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, x + y>;
    } 
      
    case min(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, x - y>;
    } 
      
    case gt(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, (x > y) ? 1 : 0>;
    } 
      
    case lt(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, (x < y) ? 1 : 0>;
    } 
      
    case geq(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, (x >= y) ? 1 : 0>;
    } 
      
    case leq(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, (x <= y) ? 1 : 0>;
    } 
  
    case cond(Exp cond, Exp then, Exp otherwise): {
      <env, c> = eval(cond, env, penv);
      return (c != 0) ? eval(then, env, penv) : eval(otherwise, env, penv);
    }
      
    case call(str name, list[Exp] args): {
       f = penv[name];
       for (i <- domain(f.formals)) {
         <env, v> = eval(args[i], env, penv);
         env[f.formals[i]] = v;
       }
       return eval(f.body, env, penv);
    }
         
    case let(list[Binding] bindings, Exp exp): {
       for (b <- bindings) {
         <env, x> = eval(b.exp, env, penv);
         env[b.var] = x;
       }
       return eval(exp, env, penv);
    } 
    
    case seq(Exp lhs, Exp rhs): {
      <env, _> = eval(lhs, env, penv);
      return eval(rhs, env, penv);
    }
    
    case assign(var(str name), Exp exp): {
      <env, v> = eval(exp, env, penv);
      env[name] = v;
      return <env, v>;
    }

    default: throw "Unsupported expression: <exp>";

  }
}


