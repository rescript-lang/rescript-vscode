// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int64 from "rescript/lib/es6/int64.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";

var Int_base = {};

var Int = {};

function of_int_exn(i) {
  return i;
}

function to_int(i) {
  return i;
}

var Int8 = {
  zero: 0,
  one: 1,
  of_int_exn: of_int_exn,
  to_int: to_int
};

function of_int_exn$1(i) {
  return i;
}

var lower_int64 = Caml_int64.neg(Caml_int64.lsl_(Int64.one, 15));

var upper_int64 = Caml_int64.sub(Caml_int64.lsl_(Int64.one, 15), Int64.one);

var of_int64_exn = Caml_int64.to_int32;

function to_int$1(t) {
  return t;
}

var Int16 = {
  of_int_exn: of_int_exn$1,
  lower_int64: lower_int64,
  upper_int64: upper_int64,
  of_int64_exn: of_int64_exn,
  to_int: to_int$1
};

var Float = {};

export {
  Int_base ,
  Int ,
  Int8 ,
  Int16 ,
  Float ,
  
}
/* No side effect */
