// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Map from "rescript/lib/es6/map.js";
import * as $$Set from "rescript/lib/es6/set.js";
import * as Sys from "rescript/lib/es6/sys.js";
import * as Caml from "rescript/lib/es6/caml.js";
import * as List from "rescript/lib/es6/list.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Bytes from "rescript/lib/es6/bytes.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as $$Buffer from "rescript/lib/es6/buffer.js";
import * as Format from "rescript/lib/es6/format.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Caml_sys from "rescript/lib/es6/caml_sys.js";
import * as Filename from "rescript/lib/es6/filename.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_bytes from "rescript/lib/es6/caml_bytes.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_string from "rescript/lib/es6/caml_string.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";
import * as Caml_external_polyfill from "rescript/lib/es6/caml_external_polyfill.js";

var Fatal_error = /* @__PURE__ */Caml_exceptions.create("Misc.Fatal_error");

function fatal_error(msg) {
  Pervasives.prerr_string(">> Fatal error: ");
  console.error(msg);
  throw {
        RE_EXN_ID: Fatal_error,
        Error: new Error()
      };
}

function fatal_errorf(fmt) {
  return Format.kasprintf(fatal_error, fmt);
}

function try_finally(work, cleanup) {
  var result;
  try {
    result = Curry._1(work, undefined);
  }
  catch (e){
    Curry._1(cleanup, undefined);
    throw e;
  }
  Curry._1(cleanup, undefined);
  return result;
}

function set_refs(l) {
  return List.iter((function (param) {
                param._0.contents = param._1;
                
              }), l);
}

function protect_refs(refs, f) {
  var backup = List.map((function (param) {
          var r = param._0;
          return /* R */{
                  _0: r,
                  _1: r.contents
                };
        }), refs);
  set_refs(refs);
  var x;
  try {
    x = Curry._1(f, undefined);
  }
  catch (e){
    set_refs(backup);
    throw e;
  }
  set_refs(backup);
  return x;
}

function map_end(f, l1, l2) {
  if (l1) {
    return {
            hd: Curry._1(f, l1.hd),
            tl: map_end(f, l1.tl, l2)
          };
  } else {
    return l2;
  }
}

function map_left_right(f, x) {
  if (!x) {
    return /* [] */0;
  }
  var res = Curry._1(f, x.hd);
  return {
          hd: res,
          tl: map_left_right(f, x.tl)
        };
}

function for_all2(pred, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return false;
      } else {
        return true;
      }
    }
    if (!l2) {
      return false;
    }
    if (!Curry._2(pred, l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue ;
  };
}

function replicate_list(elem, n) {
  if (n <= 0) {
    return /* [] */0;
  } else {
    return {
            hd: elem,
            tl: replicate_list(elem, n - 1 | 0)
          };
  }
}

function list_remove(x, y) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "misc.res",
          92,
          32
        ],
        Error: new Error()
      };
}

function split_last(x) {
  if (x) {
    var tl = x.tl;
    var x$1 = x.hd;
    if (!tl) {
      return [
              /* [] */0,
              x$1
            ];
    }
    var match = split_last(tl);
    return [
            {
              hd: x$1,
              tl: match[0]
            },
            match[1]
          ];
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "misc.res",
          96,
          14
        ],
        Error: new Error()
      };
}

function compare(cmp, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!l2) {
      return 1;
    }
    var c = Curry._2(cmp, l1.hd, l2.hd);
    if (c !== 0) {
      return c;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue ;
  };
}

function equal(eq, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return false;
      } else {
        return true;
      }
    }
    if (!l2) {
      return false;
    }
    if (!Curry._2(eq, l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue ;
  };
}

function filter_map(f, l) {
  var _acc = /* [] */0;
  var _l = l;
  while(true) {
    var l$1 = _l;
    var acc = _acc;
    if (!l$1) {
      return List.rev(acc);
    }
    var t = l$1.tl;
    var v = Curry._1(f, l$1.hd);
    if (v !== undefined) {
      _l = t;
      _acc = {
        hd: Caml_option.valFromOption(v),
        tl: acc
      };
      continue ;
    }
    _l = t;
    continue ;
  };
}

function map2_prefix(f, l1, l2) {
  var _acc = /* [] */0;
  var _l1 = l1;
  var _l2 = l2;
  while(true) {
    var l2$1 = _l2;
    var l1$1 = _l1;
    var acc = _acc;
    if (!l1$1) {
      return [
              List.rev(acc),
              l2$1
            ];
    }
    if (l2$1) {
      var h = Curry._2(f, l1$1.hd, l2$1.hd);
      _l2 = l2$1.tl;
      _l1 = l1$1.tl;
      _acc = {
        hd: h,
        tl: acc
      };
      continue ;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "map2_prefix",
          Error: new Error()
        };
  };
}

function some_if_all_elements_are_some(l) {
  var _acc = /* [] */0;
  var _l = l;
  while(true) {
    var l$1 = _l;
    var acc = _acc;
    if (!l$1) {
      return List.rev(acc);
    }
    var h = l$1.hd;
    if (h === undefined) {
      return ;
    }
    _l = l$1.tl;
    _acc = {
      hd: Caml_option.valFromOption(h),
      tl: acc
    };
    continue ;
  };
}

function split_at(n, l) {
  var _n = n;
  var _acc = /* [] */0;
  var _l = l;
  while(true) {
    var l$1 = _l;
    var acc = _acc;
    var n$1 = _n;
    if (n$1 === 0) {
      return [
              List.rev(acc),
              l$1
            ];
    }
    if (l$1) {
      _l = l$1.tl;
      _acc = {
        hd: l$1.hd,
        tl: acc
      };
      _n = n$1 - 1 | 0;
      continue ;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "split_at",
          Error: new Error()
        };
  };
}

var List$1 = {
  compare: compare,
  equal: equal,
  filter_map: filter_map,
  map2_prefix: map2_prefix,
  some_if_all_elements_are_some: some_if_all_elements_are_some,
  split_at: split_at
};

function equal$1(eq, o1, o2) {
  if (o1 !== undefined) {
    if (o2 !== undefined) {
      return Curry._2(eq, Caml_option.valFromOption(o1), Caml_option.valFromOption(o2));
    } else {
      return false;
    }
  } else {
    return o2 === undefined;
  }
}

function iter(f, x) {
  if (x !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(x));
  }
  
}

function map(f, x) {
  if (x !== undefined) {
    return Caml_option.some(Curry._1(f, Caml_option.valFromOption(x)));
  }
  
}

function fold(f, a, b) {
  if (a !== undefined) {
    return Curry._2(f, Caml_option.valFromOption(a), b);
  } else {
    return b;
  }
}

function value_default(f, $$default, a) {
  if (a !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(a));
  } else {
    return $$default;
  }
}

var $$Option = {
  equal: equal$1,
  iter: iter,
  map: map,
  fold: fold,
  value_default: value_default
};

function exists2(p, a1, a2) {
  var n = a1.length;
  if (a2.length !== n) {
    Pervasives.invalid_arg("Misc.Stdlib.Array.exists2");
  }
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return false;
    }
    if (Curry._2(p, a1[i], a2[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var $$Array$1 = {
  exists2: exists2
};

var Stdlib = {
  List: List$1,
  $$Option: $$Option,
  $$Array: $$Array$1
};

function find_in_path(path, name) {
  if (Curry._1(Filename.is_implicit, name)) {
    var _x = path;
    while(true) {
      var x = _x;
      if (x) {
        var fullname = Filename.concat(x.hd, name);
        if (Caml_external_polyfill.resolve("caml_sys_file_exists")(fullname)) {
          return fullname;
        }
        _x = x.tl;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  }
  if (Caml_external_polyfill.resolve("caml_sys_file_exists")(name)) {
    return name;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function find_in_path_rel(path, name) {
  var simplify = function (_s) {
    while(true) {
      var s = _s;
      var base = Curry._1(Filename.basename, s);
      var dir = Curry._1(Filename.dirname, s);
      if (dir === s) {
        return dir;
      }
      if (base !== Filename.current_dir_name) {
        return Filename.concat(simplify(dir), base);
      }
      _s = dir;
      continue ;
    };
  };
  var _x = path;
  while(true) {
    var x = _x;
    if (x) {
      var fullname = simplify(Filename.concat(x.hd, name));
      if (Caml_external_polyfill.resolve("caml_sys_file_exists")(fullname)) {
        return fullname;
      }
      _x = x.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_in_path_uncap(path, name) {
  var uname = $$String.uncapitalize_ascii(name);
  var _x = path;
  while(true) {
    var x = _x;
    if (x) {
      var dir = x.hd;
      var fullname = Filename.concat(dir, name);
      var ufullname = Filename.concat(dir, uname);
      if (Caml_external_polyfill.resolve("caml_sys_file_exists")(ufullname)) {
        return ufullname;
      }
      if (Caml_external_polyfill.resolve("caml_sys_file_exists")(fullname)) {
        return fullname;
      }
      _x = x.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function remove_file(filename) {
  try {
    if (Caml_external_polyfill.resolve("caml_sys_file_exists")(filename)) {
      return Caml_external_polyfill.resolve("caml_sys_remove")(filename);
    } else {
      return ;
    }
  }
  catch (raw__msg){
    var _msg = Caml_js_exceptions.internalToOCamlException(raw__msg);
    if (_msg.RE_EXN_ID === "Sys_error") {
      return ;
    }
    throw _msg;
  }
}

function expand_directory(alt, s) {
  if (s.length !== 0 && Caml_string.get(s, 0) === /* '+' */43) {
    return Filename.concat(alt, $$String.sub(s, 1, s.length - 1 | 0));
  } else {
    return s;
  }
}

function create_hashtable(size, init) {
  var tbl = Hashtbl.create(undefined, size);
  List.iter((function (param) {
          return Hashtbl.add(tbl, param[0], param[1]);
        }), init);
  return tbl;
}

function copy_file(ic, oc) {
  var buff = Caml_bytes.caml_create_bytes(4096);
  var _param;
  while(true) {
    var n = Pervasives.input(ic, buff, 0, 4096);
    if (n === 0) {
      return ;
    }
    Pervasives.output(oc, buff, 0, n);
    _param = undefined;
    continue ;
  };
}

function copy_file_chunk(ic, oc, len) {
  var buff = Caml_bytes.caml_create_bytes(4096);
  var _n = len;
  while(true) {
    var n = _n;
    if (n <= 0) {
      return ;
    }
    var r = Pervasives.input(ic, buff, 0, n < 4096 ? n : 4096);
    if (r === 0) {
      throw {
            RE_EXN_ID: "End_of_file",
            Error: new Error()
          };
    }
    Pervasives.output(oc, buff, 0, r);
    _n = n - r | 0;
    continue ;
  };
}

function string_of_file(ic) {
  var b = $$Buffer.create(65536);
  var buff = Caml_bytes.caml_create_bytes(4096);
  var _param;
  while(true) {
    var n = Pervasives.input(ic, buff, 0, 4096);
    if (n === 0) {
      return $$Buffer.contents(b);
    }
    $$Buffer.add_subbytes(b, buff, 0, n);
    _param = undefined;
    continue ;
  };
}

function output_to_file_via_temporary(modeOpt, filename, fn) {
  var mode = modeOpt !== undefined ? modeOpt : ({
        hd: /* Open_text */7,
        tl: /* [] */0
      });
  var match = Filename.open_temp_file(mode, 438, Curry._1(Filename.dirname, filename), Curry._1(Filename.basename, filename), ".tmp");
  var oc = match[1];
  var temp_filename = match[0];
  var res;
  try {
    res = Curry._2(fn, temp_filename, oc);
  }
  catch (exn){
    Pervasives.close_out(oc);
    remove_file(temp_filename);
    throw exn;
  }
  Pervasives.close_out(oc);
  try {
    Caml_external_polyfill.resolve("caml_sys_rename")(temp_filename, filename);
    return res;
  }
  catch (exn$1){
    remove_file(temp_filename);
    throw exn$1;
  }
}

function log2(n) {
  if (n <= 1) {
    return 0;
  } else {
    return 1 + log2((n >> 1)) | 0;
  }
}

function align(n, a) {
  if (n >= 0) {
    return (n + a | 0) - 1 & (-a | 0);
  } else {
    return n & (-a | 0);
  }
}

function no_overflow_add(a, b) {
  return (a ^ b | a ^ Pervasives.lnot(a + b | 0)) < 0;
}

function no_overflow_sub(a, b) {
  return (a ^ Pervasives.lnot(b) | b ^ (a - b | 0)) < 0;
}

function no_overflow_mul(a, b) {
  if (b !== 0) {
    return Caml_int32.div(Math.imul(a, b), b) === a;
  } else {
    return false;
  }
}

function no_overflow_lsl(a, k) {
  if (0 <= k && k < Sys.word_size && (Pervasives.min_int >> k) <= a) {
    return a <= (Pervasives.max_int >> k);
  } else {
    return false;
  }
}

function cvt_int_aux(str, neg, of_string) {
  if (str.length === 0 || Caml_string.get(str, 0) === /* '-' */45) {
    return Curry._1(of_string, str);
  } else {
    return Curry._1(neg, Curry._1(of_string, "-" + str));
  }
}

function $$int(s) {
  return cvt_int_aux(s, (function (prim) {
                return -prim | 0;
              }), Caml_format.caml_int_of_string);
}

function int32(s) {
  return cvt_int_aux(s, (function (prim) {
                return -prim | 0;
              }), Caml_format.caml_int32_of_string);
}

function int64(s) {
  return cvt_int_aux(s, Caml_int64.neg, Caml_format.caml_int64_of_string);
}

function nativeint(s) {
  return cvt_int_aux(s, (function (prim) {
                return -prim | 0;
              }), Caml_format.caml_nativeint_of_string);
}

var Int_literal_converter = {
  cvt_int_aux: cvt_int_aux,
  $$int: $$int,
  int32: int32,
  int64: int64,
  nativeint: nativeint
};

function chop_extensions(file) {
  var dirname = Curry._1(Filename.dirname, file);
  var basename = Curry._1(Filename.basename, file);
  try {
    var pos = $$String.index(basename, /* '.' */46);
    var basename$1 = $$String.sub(basename, 0, pos);
    if (Curry._1(Filename.is_implicit, file) && dirname === Filename.current_dir_name) {
      return basename$1;
    } else {
      return Filename.concat(dirname, basename$1);
    }
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return file;
    }
    throw exn;
  }
}

function search_substring(pat, str, start) {
  var _i = start;
  var _j = 0;
  while(true) {
    var j = _j;
    var i = _i;
    if (j >= pat.length) {
      return i;
    }
    if ((i + j | 0) >= str.length) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    if (Caml_string.get(str, i + j | 0) === Caml_string.get(pat, j)) {
      _j = j + 1 | 0;
      continue ;
    }
    _j = 0;
    _i = i + 1 | 0;
    continue ;
  };
}

function replace_substring(before, after, str) {
  var search = function (_acc, _curr) {
    while(true) {
      var curr = _curr;
      var acc = _acc;
      var next;
      try {
        next = search_substring(before, str, curr);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          var suffix = $$String.sub(str, curr, str.length - curr | 0);
          return List.rev({
                      hd: suffix,
                      tl: acc
                    });
        }
        throw exn;
      }
      var prefix = $$String.sub(str, curr, next - curr | 0);
      _curr = next + before.length | 0;
      _acc = {
        hd: prefix,
        tl: acc
      };
      continue ;
    };
  };
  return $$String.concat(after, search(/* [] */0, 0));
}

function rev_split_words(s) {
  var split1 = function (res, _i) {
    while(true) {
      var i = _i;
      if (i >= s.length) {
        return res;
      }
      var match = Caml_string.get(s, i);
      if (match > 13 || match < 9) {
        if (match !== 32) {
          return split2(res, i, i + 1 | 0);
        }
        _i = i + 1 | 0;
        continue ;
      }
      if (match === 12 || match === 11) {
        return split2(res, i, i + 1 | 0);
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  var split2 = function (res, i, _j) {
    while(true) {
      var j = _j;
      if (j >= s.length) {
        return {
                hd: $$String.sub(s, i, j - i | 0),
                tl: res
              };
      }
      var match = Caml_string.get(s, j);
      if (match > 13 || match < 9) {
        if (match !== 32) {
          _j = j + 1 | 0;
          continue ;
        }
        
      } else if (match === 12 || match === 11) {
        _j = j + 1 | 0;
        continue ;
      }
      return split1({
                  hd: $$String.sub(s, i, j - i | 0),
                  tl: res
                }, j + 1 | 0);
    };
  };
  return split1(/* [] */0, 0);
}

function get_ref(r) {
  var v = r.contents;
  r.contents = /* [] */0;
  return v;
}

function fst3(param) {
  return param[0];
}

function snd3(param) {
  return param[1];
}

function thd3(param) {
  return param[2];
}

function fst4(param) {
  return param[0];
}

function snd4(param) {
  return param[1];
}

function thd4(param) {
  return param[2];
}

function for4(param) {
  return param[3];
}

function create(str_size) {
  var tbl_size = Caml_int32.div(str_size, Sys.max_string_length) + 1 | 0;
  var tbl = Caml_array.make(tbl_size, Bytes.empty);
  for(var i = 0 ,i_finish = tbl_size - 2 | 0; i <= i_finish; ++i){
    Caml_array.set(tbl, i, Caml_bytes.caml_create_bytes(Sys.max_string_length));
  }
  Caml_array.set(tbl, tbl_size - 1 | 0, Caml_bytes.caml_create_bytes(Caml_int32.mod_(str_size, Sys.max_string_length)));
  return tbl;
}

function length(tbl) {
  var tbl_size = tbl.length;
  return Math.imul(Sys.max_string_length, tbl_size - 1 | 0) + Caml_array.get(tbl, tbl_size - 1 | 0).length | 0;
}

function get(tbl, ind) {
  return Caml_bytes.get(Caml_array.get(tbl, Caml_int32.div(ind, Sys.max_string_length)), Caml_int32.mod_(ind, Sys.max_string_length));
}

function set(tbl, ind, c) {
  return Caml_bytes.set(Caml_array.get(tbl, Caml_int32.div(ind, Sys.max_string_length)), Caml_int32.mod_(ind, Sys.max_string_length), c);
}

function blit(src, srcoff, dst, dstoff, len) {
  for(var i = 0; i < len; ++i){
    set(dst, dstoff + i | 0, get(src, srcoff + i | 0));
  }
  
}

function output(oc, tbl, pos, len) {
  for(var i = pos ,i_finish = pos + len | 0; i < i_finish; ++i){
    Pervasives.output_char(oc, get(tbl, i));
  }
  
}

function unsafe_blit_to_bytes(src, srcoff, dst, dstoff, len) {
  for(var i = 0; i < len; ++i){
    dst[dstoff + i | 0] = get(src, srcoff + i | 0);
  }
  
}

function input_bytes(ic, len) {
  var tbl = create(len);
  $$Array.iter((function (str) {
          return Pervasives.really_input(ic, str, 0, str.length);
        }), tbl);
  return tbl;
}

var LongString = {
  create: create,
  length: length,
  get: get,
  set: set,
  blit: blit,
  output: output,
  unsafe_blit_to_bytes: unsafe_blit_to_bytes,
  input_bytes: input_bytes
};

function edit_distance(a, b, cutoff) {
  var la = a.length;
  var lb = b.length;
  var cutoff$1 = Caml.caml_int_min(la > lb ? la : lb, cutoff);
  if (Pervasives.abs(la - lb | 0) > cutoff$1) {
    return ;
  }
  var m = $$Array.make_matrix(la + 1 | 0, lb + 1 | 0, cutoff$1 + 1 | 0);
  Caml_array.set(Caml_array.get(m, 0), 0, 0);
  for(var i = 1; i <= la; ++i){
    Caml_array.set(Caml_array.get(m, i), 0, i);
  }
  for(var j = 1; j <= lb; ++j){
    Caml_array.set(Caml_array.get(m, 0), j, j);
  }
  for(var i$1 = 1; i$1 <= la; ++i$1){
    for(var j$1 = Caml.caml_int_max(1, (i$1 - cutoff$1 | 0) - 1 | 0) ,j_finish = Caml.caml_int_min(lb, (i$1 + cutoff$1 | 0) + 1 | 0); j$1 <= j_finish; ++j$1){
      var cost = Caml_string.get(a, i$1 - 1 | 0) === Caml_string.get(b, j$1 - 1 | 0) ? 0 : 1;
      var best = Caml.caml_int_min(1 + Caml.caml_int_min(Caml_array.get(Caml_array.get(m, i$1 - 1 | 0), j$1), Caml_array.get(Caml_array.get(m, i$1), j$1 - 1 | 0)) | 0, Caml_array.get(Caml_array.get(m, i$1 - 1 | 0), j$1 - 1 | 0) + cost | 0);
      var best$1 = i$1 > 1 && j$1 > 1 && Caml_string.get(a, i$1 - 1 | 0) === Caml_string.get(b, j$1 - 2 | 0) && Caml_string.get(a, i$1 - 2 | 0) === Caml_string.get(b, j$1 - 1 | 0) ? Caml.caml_int_min(best, Caml_array.get(Caml_array.get(m, i$1 - 2 | 0), j$1 - 2 | 0) + cost | 0) : best;
      Caml_array.set(Caml_array.get(m, i$1), j$1, best$1);
    }
  }
  var result = Caml_array.get(Caml_array.get(m, la), lb);
  if (result > cutoff$1) {
    return ;
  } else {
    return result;
  }
}

function spellcheck(env, name) {
  var match = name.length;
  var cutoff = match > 4 || match < 1 ? (
      match === 6 || match === 5 ? 2 : 3
    ) : (
      match >= 3 ? 1 : 0
    );
  return List.fold_left((function (param, param$1) {
                  var dist = edit_distance(name, param$1, cutoff);
                  if (dist === undefined) {
                    return param;
                  }
                  var best_dist = param[1];
                  if (dist < best_dist) {
                    return [
                            {
                              hd: param$1,
                              tl: /* [] */0
                            },
                            dist
                          ];
                  } else if (dist === best_dist) {
                    return [
                            {
                              hd: param$1,
                              tl: param[0]
                            },
                            dist
                          ];
                  } else {
                    return param;
                  }
                }), [
                /* [] */0,
                Pervasives.max_int
              ], env)[0];
}

function did_you_mean(ppf, get_choices) {
  Format.fprintf(ppf, /* Format */{
        _0: {
          TAG: /* Formatting_lit */17,
          _0: /* FFlush */2,
          _1: /* End_of_format */0
        },
        _1: "@?"
      });
  var choices = Curry._1(get_choices, undefined);
  if (!choices) {
    return ;
  }
  var match = split_last(choices);
  var rest = match[0];
  return Curry._3(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* Formatting_lit */17,
                    _0: /* Force_newline */3,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: "Hint: Did you mean ",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* '?' */63,
                              _1: {
                                TAG: /* Formatting_lit */17,
                                _0: /* FFlush */2,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "@\nHint: Did you mean %s%s%s?@?"
                }), $$String.concat(", ", rest), rest === /* [] */0 ? "" : " or ", match[1]);
}

function cut_at(s, c) {
  var pos = $$String.index(s, c);
  return [
          $$String.sub(s, 0, pos),
          $$String.sub(s, pos + 1 | 0, (s.length - pos | 0) - 1 | 0)
        ];
}

var compare$1 = Caml_obj.caml_compare;

var StringSet = $$Set.Make({
      compare: compare$1
    });

var compare$2 = Caml_obj.caml_compare;

var StringMap = $$Map.Make({
      compare: compare$2
    });

function ansi_of_color(x) {
  switch (x) {
    case /* Black */0 :
        return "0";
    case /* Red */1 :
        return "1";
    case /* Green */2 :
        return "2";
    case /* Yellow */3 :
        return "3";
    case /* Blue */4 :
        return "4";
    case /* Magenta */5 :
        return "5";
    case /* Cyan */6 :
        return "6";
    case /* White */7 :
        return "7";
    
  }
}

function code_of_style(x) {
  if (typeof x === "number") {
    if (x === /* Bold */0) {
      return "1";
    } else {
      return "0";
    }
  } else if (x.TAG === /* FG */0) {
    return "3" + ansi_of_color(x._0);
  } else {
    return "4" + ansi_of_color(x._0);
  }
}

function ansi_of_style_l(l) {
  var s = l ? (
      l.tl ? $$String.concat(";", List.map(code_of_style, l)) : code_of_style(l.hd)
    ) : "0";
  return "\x11[" + (s + "m");
}

var default_styles = {
  error: {
    hd: /* Bold */0,
    tl: {
      hd: {
        TAG: /* FG */0,
        _0: /* Red */1
      },
      tl: /* [] */0
    }
  },
  warning: {
    hd: /* Bold */0,
    tl: {
      hd: {
        TAG: /* FG */0,
        _0: /* Magenta */5
      },
      tl: /* [] */0
    }
  },
  loc: {
    hd: /* Bold */0,
    tl: /* [] */0
  }
};

var cur_styles = {
  contents: default_styles
};

function get_styles(param) {
  return cur_styles.contents;
}

function set_styles(s) {
  cur_styles.contents = s;
  
}

function style_of_tag(s) {
  switch (s) {
    case "error" :
        return cur_styles.contents.error;
    case "loc" :
        return cur_styles.contents.loc;
    case "warning" :
        return cur_styles.contents.warning;
    default:
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
  }
}

var color_enabled = {
  contents: true
};

function mark_open_tag(or_else, s) {
  try {
    var style = style_of_tag(s);
    if (color_enabled.contents) {
      return ansi_of_style_l(style);
    } else {
      return "";
    }
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return Curry._1(or_else, s);
    }
    throw exn;
  }
}

function mark_close_tag(or_else, s) {
  try {
    style_of_tag(s);
    if (color_enabled.contents) {
      return ansi_of_style_l({
                  hd: /* Reset */1,
                  tl: /* [] */0
                });
    } else {
      return "";
    }
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return Curry._1(or_else, s);
    }
    throw exn;
  }
}

function set_color_tag_handling(ppf) {
  var functions = Format.pp_get_formatter_tag_functions(ppf, undefined);
  var partial_arg = functions.mark_open_tag;
  var partial_arg$1 = functions.mark_close_tag;
  var functions$p_mark_open_tag = function (param) {
    return mark_open_tag(partial_arg, param);
  };
  var functions$p_mark_close_tag = function (param) {
    return mark_close_tag(partial_arg$1, param);
  };
  var functions$p_print_open_tag = functions.print_open_tag;
  var functions$p_print_close_tag = functions.print_close_tag;
  var functions$p = {
    mark_open_tag: functions$p_mark_open_tag,
    mark_close_tag: functions$p_mark_close_tag,
    print_open_tag: functions$p_print_open_tag,
    print_close_tag: functions$p_print_close_tag
  };
  Format.pp_set_mark_tags(ppf, true);
  Format.pp_set_formatter_tag_functions(ppf, functions$p);
  Format.pp_set_margin(ppf, Format.pp_get_margin(Format.std_formatter, undefined));
  
}

function should_enable_color(param) {
  var term;
  try {
    term = Caml_sys.caml_sys_getenv("TERM");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      term = "";
    } else {
      throw exn;
    }
  }
  if (term !== "dumb" && term !== "") {
    return Caml_external_polyfill.resolve("caml_sys_isatty")(Pervasives.stderr);
  } else {
    return false;
  }
}

var first = {
  contents: true
};

var formatter_l_1 = {
  hd: Format.err_formatter,
  tl: {
    hd: Format.str_formatter,
    tl: /* [] */0
  }
};

var formatter_l = {
  hd: Format.std_formatter,
  tl: formatter_l_1
};

function setup(o) {
  if (first.contents) {
    first.contents = false;
    Format.set_mark_tags(true);
    List.iter(set_color_tag_handling, formatter_l);
    var tmp;
    if (o !== undefined) {
      switch (o) {
        case /* Auto */0 :
            tmp = should_enable_color(undefined);
            break;
        case /* Always */1 :
            tmp = true;
            break;
        case /* Never */2 :
            tmp = false;
            break;
        
      }
    } else {
      tmp = should_enable_color(undefined);
    }
    color_enabled.contents = tmp;
  }
  
}

var Color = {
  ansi_of_color: ansi_of_color,
  code_of_style: code_of_style,
  ansi_of_style_l: ansi_of_style_l,
  default_styles: default_styles,
  cur_styles: cur_styles,
  get_styles: get_styles,
  set_styles: set_styles,
  style_of_tag: style_of_tag,
  color_enabled: color_enabled,
  mark_open_tag: mark_open_tag,
  mark_close_tag: mark_close_tag,
  set_color_tag_handling: set_color_tag_handling,
  should_enable_color: should_enable_color,
  setup: setup
};

function normalise_eol(s) {
  var b = $$Buffer.create(80);
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    if (Caml_string.get(s, i) !== /* '\r' */13) {
      $$Buffer.add_char(b, Caml_string.get(s, i));
    }
    
  }
  return $$Buffer.contents(b);
}

function delete_eol_spaces(src) {
  var len_src = src.length;
  var dst = Caml_bytes.caml_create_bytes(len_src);
  var loop = function (_i_src, _i_dst) {
    while(true) {
      var i_dst = _i_dst;
      var i_src = _i_src;
      if (i_src === len_src) {
        return i_dst;
      }
      var c = Caml_string.get(src, i_src);
      if (c === 9) {
        return loop_spaces(1, i_src + 1 | 0, i_dst);
      }
      if (c === 32) {
        return loop_spaces(1, i_src + 1 | 0, i_dst);
      }
      Caml_bytes.set(dst, i_dst, c);
      _i_dst = i_dst + 1 | 0;
      _i_src = i_src + 1 | 0;
      continue ;
    };
  };
  var loop_spaces = function (_spaces, _i_src, i_dst) {
    while(true) {
      var i_src = _i_src;
      var spaces = _spaces;
      if (i_src === len_src) {
        return i_dst;
      }
      var match = Caml_string.get(src, i_src);
      if (match === 10 || match === 9) {
        if (match >= 10) {
          Caml_bytes.set(dst, i_dst, /* '\n' */10);
          return loop(i_src + 1 | 0, i_dst + 1 | 0);
        }
        
      } else if (match !== 32) {
        for(var n = 0; n <= spaces; ++n){
          Caml_bytes.set(dst, i_dst + n | 0, Caml_string.get(src, (i_src - spaces | 0) + n | 0));
        }
        return loop(i_src + 1 | 0, (i_dst + spaces | 0) + 1 | 0);
      }
      _i_src = i_src + 1 | 0;
      _spaces = spaces + 1 | 0;
      continue ;
    };
  };
  var stop = loop(0, 0);
  return Bytes.sub_string(dst, 0, stop);
}

var HookExnWrapper = /* @__PURE__ */Caml_exceptions.create("Misc.HookExnWrapper");

var HookExn = /* @__PURE__ */Caml_exceptions.create("Misc.HookExn");

function raise_direct_hook_exn(e) {
  throw {
        RE_EXN_ID: HookExn,
        _1: e,
        Error: new Error()
      };
}

function fold_hooks(list, hook_info, ast) {
  return List.fold_left((function (ast, param) {
                try {
                  return Curry._2(param[1], hook_info, ast);
                }
                catch (raw_e){
                  var e = Caml_js_exceptions.internalToOCamlException(raw_e);
                  if (e.RE_EXN_ID === HookExn) {
                    throw e._1;
                  }
                  throw {
                        RE_EXN_ID: HookExnWrapper,
                        error: e,
                        hook_name: param[0],
                        hook_info: hook_info,
                        Error: new Error()
                      };
                }
              }), ast, List.sort(Caml_obj.caml_compare, list));
}

function MakeHooks(M) {
  var hooks = {
    contents: /* [] */0
  };
  var add_hook = function (name, f) {
    hooks.contents = {
      hd: [
        name,
        f
      ],
      tl: hooks.contents
    };
    
  };
  var apply_hooks = function (sourcefile, intf) {
    return fold_hooks(hooks.contents, sourcefile, intf);
  };
  return {
          add_hook: add_hook,
          apply_hooks: apply_hooks
        };
}

var may = iter;

var may_map = map;

export {
  Fatal_error ,
  fatal_error ,
  fatal_errorf ,
  try_finally ,
  protect_refs ,
  map_end ,
  map_left_right ,
  for_all2 ,
  replicate_list ,
  list_remove ,
  split_last ,
  Stdlib ,
  may ,
  may_map ,
  find_in_path ,
  find_in_path_rel ,
  find_in_path_uncap ,
  remove_file ,
  expand_directory ,
  create_hashtable ,
  copy_file ,
  copy_file_chunk ,
  string_of_file ,
  output_to_file_via_temporary ,
  log2 ,
  align ,
  no_overflow_add ,
  no_overflow_sub ,
  no_overflow_mul ,
  no_overflow_lsl ,
  Int_literal_converter ,
  chop_extensions ,
  search_substring ,
  replace_substring ,
  rev_split_words ,
  get_ref ,
  fst3 ,
  snd3 ,
  thd3 ,
  fst4 ,
  snd4 ,
  thd4 ,
  for4 ,
  LongString ,
  edit_distance ,
  spellcheck ,
  did_you_mean ,
  cut_at ,
  StringSet ,
  StringMap ,
  Color ,
  normalise_eol ,
  delete_eol_spaces ,
  HookExnWrapper ,
  HookExn ,
  raise_direct_hook_exn ,
  fold_hooks ,
  MakeHooks ,
  
}
/* StringSet Not a pure module */
