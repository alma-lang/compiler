// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Ast from "./Ast.js";
import * as Type from "./Type.js";
import * as Infer from "./Infer.js";
import * as Caml_js_exceptions from "../../../node_modules/rescript/lib/es6/caml_js_exceptions.js";

function test(name, ast, expected) {
  try {
    var typ = Infer.infer(ast);
    var correct = expected === Type.toString(typ);
    console.log("  " + (
          correct ? "" : name
        ) + " " + (
          correct ? "" : ":"
        ) + " " + (
          correct ? "" : Type.toString(typ)
        ));
    console.log((
          correct ? " " : "x"
        ) + " " + name + " : " + Type.toString(typ));
    console.log("  " + name + " : " + Ast.exprToString(ast.value));
    return ;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Infer.$$TypeError) {
      console.log("type error");
      return ;
    }
    throw exn;
  }
}

function node(value) {
  return {
          value: value,
          start: 0,
          end: 0
        };
}

function lambda(a, b) {
  return {
          value: {
            TAG: 6,
            _0: a,
            _1: b,
            [Symbol.for("name")]: "Lambda"
          },
          start: 0,
          end: 0
        };
}

function fncall(a, b) {
  return {
          value: {
            TAG: 7,
            _0: a,
            _1: b,
            [Symbol.for("name")]: "FnCall"
          },
          start: 0,
          end: 0
        };
}

function identifier(a) {
  return {
          value: {
            TAG: 3,
            _0: a,
            [Symbol.for("name")]: "Identifier"
          },
          start: 0,
          end: 0
        };
}

function let_(a, b, c) {
  return {
          value: {
            TAG: 8,
            _0: a,
            _1: b,
            _2: c,
            [Symbol.for("name")]: "Let"
          },
          start: 0,
          end: 0
        };
}

function run(param) {
  var b_value = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b = {
    value: b_value,
    start: 0,
    end: 0
  };
  var a_value = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var a = {
    value: a_value,
    start: 0,
    end: 0
  };
  var b_value$1 = {
    TAG: 7,
    _0: a,
    _1: b,
    [Symbol.for("name")]: "FnCall"
  };
  var b$1 = {
    value: b_value$1,
    start: 0,
    end: 0
  };
  var b_value$2 = {
    TAG: 6,
    _0: "x",
    _1: b$1,
    [Symbol.for("name")]: "Lambda"
  };
  var b$2 = {
    value: b_value$2,
    start: 0,
    end: 0
  };
  test("1", {
        value: {
          TAG: 6,
          _0: "f",
          _1: b$2,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "(a -> b) -> a -> b");
  var b_value$3 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$3 = {
    value: b_value$3,
    start: 0,
    end: 0
  };
  var a_value$1 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var a$1 = {
    value: a_value$1,
    start: 0,
    end: 0
  };
  var b_value$4 = {
    TAG: 7,
    _0: a$1,
    _1: b$3,
    [Symbol.for("name")]: "FnCall"
  };
  var b$4 = {
    value: b_value$4,
    start: 0,
    end: 0
  };
  var a_value$2 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var a$2 = {
    value: a_value$2,
    start: 0,
    end: 0
  };
  var b_value$5 = {
    TAG: 7,
    _0: a$2,
    _1: b$4,
    [Symbol.for("name")]: "FnCall"
  };
  var b$5 = {
    value: b_value$5,
    start: 0,
    end: 0
  };
  var b_value$6 = {
    TAG: 6,
    _0: "x",
    _1: b$5,
    [Symbol.for("name")]: "Lambda"
  };
  var b$6 = {
    value: b_value$6,
    start: 0,
    end: 0
  };
  test("2", {
        value: {
          TAG: 6,
          _0: "f",
          _1: b$6,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "(a -> a) -> a -> a");
  var b_value$7 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$7 = {
    value: b_value$7,
    start: 0,
    end: 0
  };
  var b_value$8 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var b$8 = {
    value: b_value$8,
    start: 0,
    end: 0
  };
  var a_value$3 = {
    TAG: 3,
    _0: "n",
    [Symbol.for("name")]: "Identifier"
  };
  var a$3 = {
    value: a_value$3,
    start: 0,
    end: 0
  };
  var a_value$4 = {
    TAG: 7,
    _0: a$3,
    _1: b$8,
    [Symbol.for("name")]: "FnCall"
  };
  var a$4 = {
    value: a_value$4,
    start: 0,
    end: 0
  };
  var b_value$9 = {
    TAG: 7,
    _0: a$4,
    _1: b$7,
    [Symbol.for("name")]: "FnCall"
  };
  var b$9 = {
    value: b_value$9,
    start: 0,
    end: 0
  };
  var b_value$10 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var b$10 = {
    value: b_value$10,
    start: 0,
    end: 0
  };
  var a_value$5 = {
    TAG: 3,
    _0: "m",
    [Symbol.for("name")]: "Identifier"
  };
  var a$5 = {
    value: a_value$5,
    start: 0,
    end: 0
  };
  var a_value$6 = {
    TAG: 7,
    _0: a$5,
    _1: b$10,
    [Symbol.for("name")]: "FnCall"
  };
  var a$6 = {
    value: a_value$6,
    start: 0,
    end: 0
  };
  var b_value$11 = {
    TAG: 7,
    _0: a$6,
    _1: b$9,
    [Symbol.for("name")]: "FnCall"
  };
  var b$11 = {
    value: b_value$11,
    start: 0,
    end: 0
  };
  var b_value$12 = {
    TAG: 6,
    _0: "x",
    _1: b$11,
    [Symbol.for("name")]: "Lambda"
  };
  var b$12 = {
    value: b_value$12,
    start: 0,
    end: 0
  };
  var b_value$13 = {
    TAG: 6,
    _0: "f",
    _1: b$12,
    [Symbol.for("name")]: "Lambda"
  };
  var b$13 = {
    value: b_value$13,
    start: 0,
    end: 0
  };
  var b_value$14 = {
    TAG: 6,
    _0: "n",
    _1: b$13,
    [Symbol.for("name")]: "Lambda"
  };
  var b$14 = {
    value: b_value$14,
    start: 0,
    end: 0
  };
  test("(+)", {
        value: {
          TAG: 6,
          _0: "m",
          _1: b$14,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c");
  var b_value$15 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$15 = {
    value: b_value$15,
    start: 0,
    end: 0
  };
  var b_value$16 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var b$16 = {
    value: b_value$16,
    start: 0,
    end: 0
  };
  var a_value$7 = {
    TAG: 3,
    _0: "n",
    [Symbol.for("name")]: "Identifier"
  };
  var a$7 = {
    value: a_value$7,
    start: 0,
    end: 0
  };
  var a_value$8 = {
    TAG: 7,
    _0: a$7,
    _1: b$16,
    [Symbol.for("name")]: "FnCall"
  };
  var a$8 = {
    value: a_value$8,
    start: 0,
    end: 0
  };
  var b_value$17 = {
    TAG: 7,
    _0: a$8,
    _1: b$15,
    [Symbol.for("name")]: "FnCall"
  };
  var b$17 = {
    value: b_value$17,
    start: 0,
    end: 0
  };
  var a_value$9 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var a$9 = {
    value: a_value$9,
    start: 0,
    end: 0
  };
  var b_value$18 = {
    TAG: 7,
    _0: a$9,
    _1: b$17,
    [Symbol.for("name")]: "FnCall"
  };
  var b$18 = {
    value: b_value$18,
    start: 0,
    end: 0
  };
  var b_value$19 = {
    TAG: 6,
    _0: "x",
    _1: b$18,
    [Symbol.for("name")]: "Lambda"
  };
  var b$19 = {
    value: b_value$19,
    start: 0,
    end: 0
  };
  var b_value$20 = {
    TAG: 6,
    _0: "f",
    _1: b$19,
    [Symbol.for("name")]: "Lambda"
  };
  var b$20 = {
    value: b_value$20,
    start: 0,
    end: 0
  };
  test("succ", {
        value: {
          TAG: 6,
          _0: "n",
          _1: b$20,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "((a -> b) -> c -> a) -> (a -> b) -> c -> b");
  var b_value$21 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$21 = {
    value: b_value$21,
    start: 0,
    end: 0
  };
  var b_value$22 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var b$22 = {
    value: b_value$22,
    start: 0,
    end: 0
  };
  var a_value$10 = {
    TAG: 3,
    _0: "n",
    [Symbol.for("name")]: "Identifier"
  };
  var a$10 = {
    value: a_value$10,
    start: 0,
    end: 0
  };
  var b_value$23 = {
    TAG: 7,
    _0: a$10,
    _1: b$22,
    [Symbol.for("name")]: "FnCall"
  };
  var b$23 = {
    value: b_value$23,
    start: 0,
    end: 0
  };
  var a_value$11 = {
    TAG: 3,
    _0: "m",
    [Symbol.for("name")]: "Identifier"
  };
  var a$11 = {
    value: a_value$11,
    start: 0,
    end: 0
  };
  var a_value$12 = {
    TAG: 7,
    _0: a$11,
    _1: b$23,
    [Symbol.for("name")]: "FnCall"
  };
  var a$12 = {
    value: a_value$12,
    start: 0,
    end: 0
  };
  var b_value$24 = {
    TAG: 7,
    _0: a$12,
    _1: b$21,
    [Symbol.for("name")]: "FnCall"
  };
  var b$24 = {
    value: b_value$24,
    start: 0,
    end: 0
  };
  var b_value$25 = {
    TAG: 6,
    _0: "x",
    _1: b$24,
    [Symbol.for("name")]: "Lambda"
  };
  var b$25 = {
    value: b_value$25,
    start: 0,
    end: 0
  };
  var b_value$26 = {
    TAG: 6,
    _0: "f",
    _1: b$25,
    [Symbol.for("name")]: "Lambda"
  };
  var b$26 = {
    value: b_value$26,
    start: 0,
    end: 0
  };
  var b_value$27 = {
    TAG: 6,
    _0: "n",
    _1: b$26,
    [Symbol.for("name")]: "Lambda"
  };
  var b$27 = {
    value: b_value$27,
    start: 0,
    end: 0
  };
  test("mult", {
        value: {
          TAG: 6,
          _0: "m",
          _1: b$27,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "(a -> b -> c) -> (d -> a) -> d -> b -> c");
  var b_value$28 = {
    TAG: 3,
    _0: "u",
    [Symbol.for("name")]: "Identifier"
  };
  var b$28 = {
    value: b_value$28,
    start: 0,
    end: 0
  };
  var b_value$29 = {
    TAG: 6,
    _0: "u",
    _1: b$28,
    [Symbol.for("name")]: "Lambda"
  };
  var b$29 = {
    value: b_value$29,
    start: 0,
    end: 0
  };
  var b_value$30 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$30 = {
    value: b_value$30,
    start: 0,
    end: 0
  };
  var b_value$31 = {
    TAG: 6,
    _0: "u",
    _1: b$30,
    [Symbol.for("name")]: "Lambda"
  };
  var b$31 = {
    value: b_value$31,
    start: 0,
    end: 0
  };
  var b_value$32 = {
    TAG: 3,
    _0: "f",
    [Symbol.for("name")]: "Identifier"
  };
  var b$32 = {
    value: b_value$32,
    start: 0,
    end: 0
  };
  var a_value$13 = {
    TAG: 3,
    _0: "g",
    [Symbol.for("name")]: "Identifier"
  };
  var a$13 = {
    value: a_value$13,
    start: 0,
    end: 0
  };
  var b_value$33 = {
    TAG: 7,
    _0: a$13,
    _1: b$32,
    [Symbol.for("name")]: "FnCall"
  };
  var b$33 = {
    value: b_value$33,
    start: 0,
    end: 0
  };
  var a_value$14 = {
    TAG: 3,
    _0: "h",
    [Symbol.for("name")]: "Identifier"
  };
  var a$14 = {
    value: a_value$14,
    start: 0,
    end: 0
  };
  var b_value$34 = {
    TAG: 7,
    _0: a$14,
    _1: b$33,
    [Symbol.for("name")]: "FnCall"
  };
  var b$34 = {
    value: b_value$34,
    start: 0,
    end: 0
  };
  var b_value$35 = {
    TAG: 6,
    _0: "h",
    _1: b$34,
    [Symbol.for("name")]: "Lambda"
  };
  var b$35 = {
    value: b_value$35,
    start: 0,
    end: 0
  };
  var b_value$36 = {
    TAG: 6,
    _0: "g",
    _1: b$35,
    [Symbol.for("name")]: "Lambda"
  };
  var b$36 = {
    value: b_value$36,
    start: 0,
    end: 0
  };
  var a_value$15 = {
    TAG: 3,
    _0: "n",
    [Symbol.for("name")]: "Identifier"
  };
  var a$15 = {
    value: a_value$15,
    start: 0,
    end: 0
  };
  var a_value$16 = {
    TAG: 7,
    _0: a$15,
    _1: b$36,
    [Symbol.for("name")]: "FnCall"
  };
  var a$16 = {
    value: a_value$16,
    start: 0,
    end: 0
  };
  var a_value$17 = {
    TAG: 7,
    _0: a$16,
    _1: b$31,
    [Symbol.for("name")]: "FnCall"
  };
  var a$17 = {
    value: a_value$17,
    start: 0,
    end: 0
  };
  var b_value$37 = {
    TAG: 7,
    _0: a$17,
    _1: b$29,
    [Symbol.for("name")]: "FnCall"
  };
  var b$37 = {
    value: b_value$37,
    start: 0,
    end: 0
  };
  var b_value$38 = {
    TAG: 6,
    _0: "x",
    _1: b$37,
    [Symbol.for("name")]: "Lambda"
  };
  var b$38 = {
    value: b_value$38,
    start: 0,
    end: 0
  };
  var b_value$39 = {
    TAG: 6,
    _0: "f",
    _1: b$38,
    [Symbol.for("name")]: "Lambda"
  };
  var b$39 = {
    value: b_value$39,
    start: 0,
    end: 0
  };
  test("pred", {
        value: {
          TAG: 6,
          _0: "n",
          _1: b$39,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g");
  var b$40 = let_("y", {
        value: {
          TAG: 3,
          _0: "x",
          [Symbol.for("name")]: "Identifier"
        },
        start: 0,
        end: 0
      }, {
        value: {
          TAG: 3,
          _0: "y",
          [Symbol.for("name")]: "Identifier"
        },
        start: 0,
        end: 0
      });
  test("let1", {
        value: {
          TAG: 6,
          _0: "x",
          _1: b$40,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, "a -> a");
  var b_value$40 = {
    TAG: 3,
    _0: "x",
    [Symbol.for("name")]: "Identifier"
  };
  var b$41 = {
    value: b_value$40,
    start: 0,
    end: 0
  };
  var b$42 = let_("y", {
        value: {
          TAG: 6,
          _0: "z",
          _1: b$41,
          [Symbol.for("name")]: "Lambda"
        },
        start: 0,
        end: 0
      }, {
        value: {
          TAG: 3,
          _0: "y",
          [Symbol.for("name")]: "Identifier"
        },
        start: 0,
        end: 0
      });
  return test("let2", {
              value: {
                TAG: 6,
                _0: "x",
                _1: b$42,
                [Symbol.for("name")]: "Lambda"
              },
              start: 0,
              end: 0
            }, "a -> b -> a");
}

export {
  test ,
  node ,
  lambda ,
  fncall ,
  identifier ,
  let_ ,
  run ,
  
}
/* No side effect */