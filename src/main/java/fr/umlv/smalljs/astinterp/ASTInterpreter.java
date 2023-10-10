package fr.umlv.smalljs.astinterp;

import fr.umlv.smalljs.ast.Expr;
import fr.umlv.smalljs.ast.Expr.Block;
import fr.umlv.smalljs.ast.Expr.FieldAccess;
import fr.umlv.smalljs.ast.Expr.FieldAssignment;
import fr.umlv.smalljs.ast.Expr.Fun;
import fr.umlv.smalljs.ast.Expr.FunCall;
import fr.umlv.smalljs.ast.Expr.If;
import fr.umlv.smalljs.ast.Expr.Literal;
import fr.umlv.smalljs.ast.Expr.LocalVarAccess;
import fr.umlv.smalljs.ast.Expr.LocalVarAssignment;
import fr.umlv.smalljs.ast.Expr.MethodCall;
import fr.umlv.smalljs.ast.Expr.New;
import fr.umlv.smalljs.ast.Expr.Return;
import fr.umlv.smalljs.ast.Script;
import fr.umlv.smalljs.rt.Failure;
import fr.umlv.smalljs.rt.JSObject;
import fr.umlv.smalljs.rt.JSObject.Invoker;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static fr.umlv.smalljs.rt.JSObject.UNDEFINED;
import static java.util.stream.Collectors.joining;

public class ASTInterpreter {
  private static JSObject asJSObject(Object value, int lineNumber) {
    if (!(value instanceof JSObject jsObject)) {
      throw new Failure("at line " + lineNumber + ", type error " + value + " is not a JSObject");
    }
    return jsObject;
  }

  static Object visit(Expr expression, JSObject env) {
    return switch (expression) {
      case Block(List<Expr> instrs, int lineNumber) -> {
        for (var instr : instrs) {
          visit(instr, env);
        }
        yield UNDEFINED;
      }
      case Literal<?>(Object value, int lineNumber) -> value;
      case FunCall(Expr qualifier, List<Expr> args, int lineNumber) -> {
        var functionMaybe = visit(qualifier, env);
        if (!(functionMaybe instanceof JSObject jsObject)) {
          throw new Failure("not a function " + functionMaybe);
        }
        var values = args.stream()
            .map(a -> visit(a, env))
            .toArray();
        yield jsObject.invoke(UNDEFINED, values);
      }
      case LocalVarAccess(String name, int lineNumber) -> env.lookup(name);
      case LocalVarAssignment(String name, Expr expr, boolean declaration, int lineNumber) -> {
        if (declaration && env.lookup(name) != UNDEFINED) {
          throw new Failure(name + " already defined at " + lineNumber);
        }
        var value = visit(expr, env);
        env.register(name, value);
        yield UNDEFINED;
      }
      case Fun(Optional<String> optName, List<String> parameters, Block body, int lineNumber) -> {
        var functionName = optName.orElse("lambda");
        Invoker invoker = (self, receiver, args) -> {
          if (args.length != parameters.size()) {
            throw new Failure("Invalid number of arguments");
          }
          var localEnv = JSObject.newEnv(env);
          localEnv.register("this", receiver);
          for (var i = 0; i < args.length; i++) {
            localEnv.register(parameters.get(i), args[i]);
          }
          try {
            return visit(body, localEnv);
          } catch (ReturnError returnError) {
            return returnError.getValue();
          }
        };
        var function = JSObject.newFunction(functionName, invoker);
        optName.ifPresent(name -> {
          env.register(name, function);
        });
         yield function;
      }
      case Return(Expr expr, int lineNumber) -> {
        var value = visit(expr, env);
        throw new ReturnError(value);
      }
      case If(Expr condition, Block trueBlock, Block falseBlock, int lineNumber) -> {
        var booleanValue = visit(condition, env);
        if (!(booleanValue instanceof Integer value)) {
          throw new Failure("Invalid boolean value");
        }
        if (value == 1) {
          yield visit(trueBlock, env);
        }
        yield visit(falseBlock, env);
      }
      case New(Map<String, Expr> initMap, int lineNumber) -> {
        var object = JSObject.newObject(null);
        for (var entry : initMap.entrySet()) {
          var value = visit(entry.getValue(), env);
          object.register(entry.getKey(), value);
        }
        object.register("this", object);
        yield object;
      }
      case FieldAccess(Expr receiver, String name, int lineNumber) -> {
        var value = visit(receiver, env);
        if (!(value instanceof JSObject jsObject)) {
          yield value;
        }
        yield jsObject.lookup(name);
      }
      case FieldAssignment(Expr receiver, String name, Expr expr, int lineNumber) -> {
        var value = visit(receiver, env);
        if (!(value instanceof JSObject jsObject)) {
          throw new Failure(receiver + " is not an object");
        }
        jsObject.register(name, visit(expr, env));
        yield receiver;

      }
      case MethodCall(Expr receiver, String name, List<Expr> args, int lineNumber) -> {
        var value = visit(receiver, env);
        if (!(value instanceof JSObject jsObject)) {
          throw new Failure(receiver + " is not an object");
        }
        var function = jsObject.lookup(name);
        if (function == UNDEFINED) {
          throw new Failure(receiver + " doesn't have a method called " + name);
        }
        if (!(function instanceof JSObject functionObject)) {
          throw new Failure(name + " is not a method");
        }
        var values = args.stream()
                .map(a -> visit(a, env))
                .toArray();
        yield functionObject.invoke(jsObject, values);
      }
    };
  }

  @SuppressWarnings("unchecked")
  public static void interpret(Script script, PrintStream outStream) {
    JSObject globalEnv = JSObject.newEnv(null);
    Block body = script.body();
    globalEnv.register("global", globalEnv);
    globalEnv.register("print", JSObject.newFunction("print", (self, receiver, args) -> {
      System.err.println("print called with " + Arrays.toString(args));
      outStream.println(Arrays.stream(args).map(Object::toString).collect(joining(" ")));
      return UNDEFINED;
    }));
    globalEnv.register("+", JSObject.newFunction("+", (self, receiver, args) -> (Integer) args[0] + (Integer) args[1]));
    globalEnv.register("-", JSObject.newFunction("-", (self, receiver, args) -> (Integer) args[0] - (Integer) args[1]));
    globalEnv.register("/", JSObject.newFunction("/", (self, receiver, args) -> (Integer) args[0] / (Integer) args[1]));
    globalEnv.register("*", JSObject.newFunction("*", (self, receiver, args) -> (Integer) args[0] * (Integer) args[1]));
    globalEnv.register("%", JSObject.newFunction("%", (self, receiver, args) -> (Integer) args[0] % (Integer) args[1]));

    globalEnv.register("==", JSObject.newFunction("==", (self, receiver, args) -> args[0].equals(args[1]) ? 1 : 0));
    globalEnv.register("!=", JSObject.newFunction("!=", (self, receiver, args) -> !args[0].equals(args[1]) ? 1 : 0));
    globalEnv.register("<", JSObject.newFunction("<", (self, receiver, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) < 0) ? 1 : 0));
    globalEnv.register("<=", JSObject.newFunction("<=", (self, receiver, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) <= 0) ? 1 : 0));
    globalEnv.register(">", JSObject.newFunction(">", (self, receiver, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) > 0) ? 1 : 0));
    globalEnv.register(">=", JSObject.newFunction(">=", (self, receiver, args) -> (((Comparable<Object>) args[0]).compareTo(args[1]) >= 0) ? 1 : 0));
    visit(body, globalEnv);
  }
}

