// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function customDouble(foo) {
  return (foo << 1);
}

var Dep = {
  customDouble: customDouble
};

var MyOtherException = /* @__PURE__ */Caml_exceptions.create("ModuleExceptionBug.MyOtherException");

console.log(34);

var ddjdj = 34;

export {
  Dep ,
  MyOtherException ,
  ddjdj ,
  
}
/*  Not a pure module */
