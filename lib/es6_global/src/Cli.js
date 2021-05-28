// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Repl from "./Repl.js";
import * as InferTest from "./InferTest.js";

var args = Deno.args;

function help(param) {
  console.log("\n  repl            Start the REPL\n  compiler-tests  Run the compiler tests\n");
  
}

if (args.length !== 1) {
  console.log("\n  repl            Start the REPL\n  compiler-tests  Run the compiler tests\n");
} else {
  var match = args[0];
  switch (match) {
    case "compiler-tests" :
        InferTest.run(undefined);
        break;
    case "repl" :
        Repl.run(undefined);
        break;
    default:
      console.log("\n  repl            Start the REPL\n  compiler-tests  Run the compiler tests\n");
  }
}

export {
  args ,
  help ,
  
}
/* args Not a pure module */