module Eval3

// pointers into the stack

import AST;


import List;
import IO;


alias Address = int;
alias Mem = list[value];
alias Env = map[str, Address];
alias PEnv = map[str, Func];

alias Result = tuple[Mem, value];

public Address push(Mem mem) {
  return size(mem);
}

public tuple[Mem, Address] alloc(Mem mem, value v) {
  mem += [v];
  return <mem, size(mem) - 1>;
}

public Mem pop(Mem mem, Address scope) {
  return slice(mem, 0, scope);
}

public Result eval(str main, list[value] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  mem = [];
  env = ();
  for (i <- domain(f.formals)) {
    <mem, a> = alloc(mem, args[i]);
    env[f.formals[i]] = a;
  }
  println("env = <env>");
  return eval(f.body, env, penv, mem);
}

public Result eval(Exp exp, Env env, PEnv penv, Mem mem) {
  switch (exp) {
    case nat(int nat):
       return <mem, nat>;
 
    case var(str name): 
       return <mem, mem[env[name]]>;
       
       
    case mul(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) * asInt(y)>;
    } 
      
    case div(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) / asInt(y)>;
    } 
      
    case add(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) + asInt(y)>;
    } 
      
    case min(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) - asInt(y)>;
    } 
      
    case gt(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) > asInt(y)>;
    } 
      
    case lt(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) < asInt(y)>;
    } 
      
    case geq(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) >= asInt(y)>;
    } 
      
    case leq(Exp lhs, Exp rhs): {
      <mem, x> = eval(lhs, env, penv, mem);
      <mem, y> = eval(rhs, env, penv, mem);
      return <mem, asInt(x) <= asInt(y)>;
    } 
  
    case cond(Exp cond, Exp then, Exp otherwise): {
      <mem, c> = eval(cond, env, penv, mem);
      return asBool(c) ? eval(then, env, penv, mem) : eval(otherwise, env, penv, mem);
    }
      
    case call(str name, list[Exp] args): {
       f = penv[name];
       scope = push(mem);
       newEnv = ();
       for (i <- domain(f.formals)) {
         // assume no side-effects in formals
         <_, v>  = eval(args[i], env, penv, mem);
         <mem, a> = alloc(mem, v);
         newEnv[f.formals[i]] = a;
       }
       <mem, v> = eval(f.body, newEnv, penv, mem);
       return <pop(mem, scope), v>; 
    }
        
    case address(str var):
      return <mem, env[var]>;
      
    case deref(Exp exp): {
      <mem, v> = eval(exp, env, penv, mem);
      return <mem, mem[asInt(v)]>; 
    }
         
    case let(list[Binding] bindings, Exp exp): {
       scope = push(mem);
       for (b <- bindings) {
         <mem, v> = eval(b.exp, env, penv, mem);
         <mem, a> = alloc(mem, v);
         env[b.var] = a;
       }
       <mem, v> = eval(exp, env, penv, mem);
       return <pop(mem, scope), v>;
    } 
    
    case seq(Exp lhs, Exp rhs): {
      <mem, _> = eval(lhs, env, penv, mem);
      return eval(rhs, env, penv, mem);
    }
    
    // x := 1 is the same as *&x := 1
    case assign(var(str name), Exp e): {
      <mem, v> = eval(e, env, penv, mem);
      mem[env[name]] = v;
      return <mem, v>;
    }

    case assign(deref(Exp lvalue), Exp e): {
      <mem, addr> = eval(lvalue, env, penv, mem);
      <mem, v> = eval(e, env, penv, mem);
      mem[asInt(addr)] = v;
      return <mem, v>;
    }

    default: throw "Unsupported expression: <exp>";

  }
}


public int asInt(value x) {
  if (int n := x) return n;
  throw "Not an int: <x>";
}

public bool asBool(value x) {
  if (bool b := x) return b;
  throw "Not a bool: <x>";
}
