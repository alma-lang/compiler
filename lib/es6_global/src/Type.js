// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "../../../node_modules/rescript/lib/es6/js_int.js";
import * as Belt_List from "../../../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../../../node_modules/rescript/lib/es6/belt_Array.js";
import * as Belt_HashMapInt from "../../../node_modules/rescript/lib/es6/belt_HashMapInt.js";

function shouldParenthesize(_x) {
  while(true) {
    var x = _x;
    if (typeof x === "number") {
      return false;
    }
    switch (x.TAG | 0) {
      case /* Named */0 :
          return false;
      case /* Var */1 :
          var t$p = x._0.contents;
          if (t$p.TAG !== /* Bound */0) {
            return false;
          }
          _x = t$p._0;
          continue ;
      case /* Fn */2 :
      case /* PolyType */3 :
          return true;
      
    }
  };
}

function nextLetter(s) {
  var c = s.contents.charCodeAt(0);
  s.contents = String.fromCharCode((c | 0) + 1 | 0);
  
}

function toString(t) {
  var toStringRec = function (curTypeVarName, typeVarNames, _x) {
    while(true) {
      var x = _x;
      if (typeof x === "number") {
        return "unit";
      }
      switch (x.TAG | 0) {
        case /* Named */0 :
            var params = x._1;
            var name = x._0;
            if (Belt_List.length(params) === 0) {
              return name;
            }
            var paramsString = Belt_Array.joinWith(Belt_List.toArray(params), " ", (function (param) {
                    return toStringRec(curTypeVarName, typeVarNames, param);
                  }));
            return name + " " + paramsString;
        case /* Var */1 :
            var t_ = x._0.contents;
            if (t_.TAG === /* Bound */0) {
              _x = t_._0;
              continue ;
            }
            var n = t_._0;
            var s = Belt_HashMapInt.get(typeVarNames, n);
            if (s !== undefined) {
              return s;
            }
            var s$1 = curTypeVarName.contents;
            Belt_HashMapInt.set(typeVarNames, n, s$1);
            nextLetter(curTypeVarName);
            return s$1;
        case /* Fn */2 :
            var a = x._0;
            var aStr = toStringRec(curTypeVarName, typeVarNames, a);
            var bStr = toStringRec(curTypeVarName, typeVarNames, x._1);
            if (shouldParenthesize(a)) {
              return "(" + (aStr + (") -> " + bStr));
            } else {
              return aStr + (" -> " + bStr);
            }
        case /* PolyType */3 :
            var typeVarToString = function (t) {
              return toStringRec(curTypeVarName, typeVarNames, {
                          TAG: 1,
                          _0: {
                            contents: {
                              TAG: 1,
                              _0: t,
                              _1: Js_int.max,
                              [Symbol.for("name")]: "Unbound"
                            }
                          },
                          [Symbol.for("name")]: "Var"
                        });
            };
            var tvsStr = Belt_List.reduce(x._0, "", (function (s, tv) {
                    return s + (" '" + typeVarToString(tv));
                  }));
            return "\xe2\x88\x80" + (tvsStr + (" . " + toStringRec(curTypeVarName, typeVarNames, x._1)));
        
      }
    };
  };
  return toStringRec({
              contents: "a"
            }, Belt_HashMapInt.make(1), t);
}

function print(t) {
  console.log(toString(t));
  
}

var number = {
  TAG: 0,
  _0: "Number",
  _1: /* [] */0,
  [Symbol.for("name")]: "Named"
};

var bool_ = {
  TAG: 0,
  _0: "Bool",
  _1: /* [] */0,
  [Symbol.for("name")]: "Named"
};

var string_ = {
  TAG: 0,
  _0: "String",
  _1: /* [] */0,
  [Symbol.for("name")]: "Named"
};

export {
  shouldParenthesize ,
  nextLetter ,
  toString ,
  print ,
  number ,
  bool_ ,
  string_ ,
  
}
/* No side effect */