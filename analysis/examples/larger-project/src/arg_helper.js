// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Printf from "./printf.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Printexc from "rescript/lib/es6/printexc.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function fatal(err) {
  console.error(err);
  return Pervasives.exit(2);
}

function Make(S) {
  var $$default = function (v) {
    return {
            base_default: v,
            base_override: S.Key.$$Map.empty,
            user_default: undefined,
            user_override: S.Key.$$Map.empty
          };
  };
  var set_base_default = function (value, t) {
    return {
            base_default: value,
            base_override: t.base_override,
            user_default: t.user_default,
            user_override: t.user_override
          };
  };
  var add_base_override = function (key, value, t) {
    return {
            base_default: t.base_default,
            base_override: Curry._3(S.Key.$$Map.add, key, value, t.base_override),
            user_default: t.user_default,
            user_override: t.user_override
          };
  };
  var reset_base_overrides = function (t) {
    return {
            base_default: t.base_default,
            base_override: S.Key.$$Map.empty,
            user_default: t.user_default,
            user_override: t.user_override
          };
  };
  var set_user_default = function (value, t) {
    return {
            base_default: t.base_default,
            base_override: t.base_override,
            user_default: Caml_option.some(value),
            user_override: t.user_override
          };
  };
  var add_user_override = function (key, value, t) {
    return {
            base_default: t.base_default,
            base_override: t.base_override,
            user_default: t.user_default,
            user_override: Curry._3(S.Key.$$Map.add, key, value, t.user_override)
          };
  };
  var Parse_failure = /* @__PURE__ */Caml_exceptions.create("Arg_helper.Make(S).Parse_failure");
  var parse_exn = function (str, update) {
    var values = List.filter(function (param) {
            return "" !== param;
          })($$String.split_on_char(/* ',' */44, str));
    var parsed = List.fold_left((function (acc, value) {
            var equals;
            try {
              equals = $$String.index(value, /* '=' */61);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === "Not_found") {
                var exit = 0;
                var value$1;
                try {
                  value$1 = Curry._1(S.Value.of_string, value);
                  exit = 2;
                }
                catch (raw_exn$1){
                  var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                  throw {
                        RE_EXN_ID: Parse_failure,
                        _1: exn$1,
                        Error: new Error()
                      };
                }
                if (exit === 2) {
                  return set_user_default(value$1, acc);
                }
                
              } else {
                throw exn;
              }
            }
            var length = value.length;
            if (!(equals >= 0 && equals < length)) {
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "arg_helper.res",
                      84,
                      8
                    ],
                    Error: new Error()
                  };
            }
            if (equals === 0) {
              throw {
                    RE_EXN_ID: Parse_failure,
                    _1: {
                      RE_EXN_ID: "Failure",
                      _1: "Missing key in argument specification"
                    },
                    Error: new Error()
                  };
            }
            var key = $$String.sub(value, 0, equals);
            var key$1;
            try {
              key$1 = Curry._1(S.Key.of_string, key);
            }
            catch (raw_exn$2){
              var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
              throw {
                    RE_EXN_ID: Parse_failure,
                    _1: exn$2,
                    Error: new Error()
                  };
            }
            var value$2 = $$String.sub(value, equals + 1 | 0, (length - equals | 0) - 1 | 0);
            var value$3;
            try {
              value$3 = Curry._1(S.Value.of_string, value$2);
            }
            catch (raw_exn$3){
              var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
              throw {
                    RE_EXN_ID: Parse_failure,
                    _1: exn$3,
                    Error: new Error()
                  };
            }
            return add_user_override(key$1, value$3, acc);
          }), update.contents, values);
    update.contents = parsed;
    
  };
  var parse = function (str, help_text, update) {
    try {
      parse_exn(str, update);
      return ;
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === Parse_failure) {
        return fatal(Curry._2(Printf.sprintf("%s: %s"), Printexc.to_string(exn._1), help_text));
      }
      throw exn;
    }
  };
  var parse_no_error = function (str, update) {
    try {
      parse_exn(str, update);
      return /* Ok */0;
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === Parse_failure) {
        return /* Parse_failed */{
                _0: exn._1
              };
      }
      throw exn;
    }
  };
  var get = function (key, parsed) {
    try {
      return Curry._2(S.Key.$$Map.find, key, parsed.user_override);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var value = parsed.user_default;
        if (value !== undefined) {
          return Caml_option.valFromOption(value);
        }
        try {
          return Curry._2(S.Key.$$Map.find, key, parsed.base_override);
        }
        catch (raw_exn$1){
          var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
          if (exn$1.RE_EXN_ID === "Not_found") {
            return parsed.base_default;
          }
          throw exn$1;
        }
      } else {
        throw exn;
      }
    }
  };
  return {
          $$default: $$default,
          set_base_default: set_base_default,
          add_base_override: add_base_override,
          reset_base_overrides: reset_base_overrides,
          set_user_default: set_user_default,
          add_user_override: add_user_override,
          Parse_failure: Parse_failure,
          parse_exn: parse_exn,
          parse: parse,
          parse_no_error: parse_no_error,
          get: get
        };
}

export {
  fatal ,
  Make ,
  
}
/* No side effect */
