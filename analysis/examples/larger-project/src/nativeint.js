// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function to_string(n) {
  return Caml_format.caml_nativeint_format("%d", n);
}

function of_string_opt(s) {
  try {
    return Caml_option.some(Caml_format.caml_nativeint_of_string(s));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

export {
  to_string ,
  of_string_opt ,
  compare ,
  equal ,
  
}
/* No side effect */
