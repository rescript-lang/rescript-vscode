// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as InnerModules from "./InnerModules.js";

var testTop = InnerModules.wrapExitTop;

function testM1(x) {
  return InnerModules.M1.wrapExitM1(x);
}

function testM2(x) {
  return Curry._1(InnerModules.M1.M2.wrapExitM2, x);
}

export {
  testTop ,
  testM1 ,
  testM2 ,
}
/* No side effect */
