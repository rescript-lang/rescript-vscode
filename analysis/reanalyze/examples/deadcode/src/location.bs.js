// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Misc from "./misc.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as $$Buffer from "rescript/lib/es6/buffer.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Parsing from "rescript/lib/es6/parsing.js";
import * as Caml_sys from "rescript/lib/es6/caml_sys.js";
import * as Filename from "rescript/lib/es6/filename.js";
import * as Printexc from "rescript/lib/es6/printexc.js";
import * as Warnings from "./warnings.bs.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var absname = {
  contents: false
};

function in_file(name) {
  var loc = {
    pos_fname: name,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: -1
  };
  return {
          loc_start: loc,
          loc_end: loc,
          loc_ghost: true
        };
}

var none = in_file("_none_");

function curr(lexbuf) {
  return {
          loc_start: lexbuf.lex_start_p,
          loc_end: lexbuf.lex_curr_p,
          loc_ghost: false
        };
}

function init(lexbuf, fname) {
  lexbuf.lex_curr_p = {
    pos_fname: fname,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: 0
  };
  
}

function symbol_rloc(param) {
  return {
          loc_start: Parsing.symbol_start_pos(undefined),
          loc_end: Parsing.symbol_end_pos(undefined),
          loc_ghost: false
        };
}

function symbol_gloc(param) {
  return {
          loc_start: Parsing.symbol_start_pos(undefined),
          loc_end: Parsing.symbol_end_pos(undefined),
          loc_ghost: true
        };
}

function rhs_loc(n) {
  return {
          loc_start: Parsing.rhs_start_pos(n),
          loc_end: Parsing.rhs_end_pos(n),
          loc_ghost: false
        };
}

var input_name = {
  contents: "_none_"
};

var input_lexbuf = {
  contents: undefined
};

function set_input_name(name) {
  if (name !== "") {
    input_name.contents = name;
    return ;
  }
  
}

var num_loc_lines = {
  contents: 0
};

function absolute_path(s) {
  var s$1 = Curry._1(Filename.is_relative, s) ? Filename.concat(Caml_sys.caml_sys_getcwd(undefined), s) : s;
  var aux = function (_s) {
    while(true) {
      var s = _s;
      var base = Curry._1(Filename.basename, s);
      var dir = Curry._1(Filename.dirname, s);
      if (dir === s) {
        return dir;
      }
      if (base !== Filename.current_dir_name) {
        if (base === Filename.parent_dir_name) {
          return Curry._1(Filename.dirname, aux(dir));
        } else {
          return Filename.concat(aux(dir), base);
        }
      }
      _s = dir;
      continue ;
    };
  };
  return aux(s$1);
}

function show_filename(file) {
  var file$1 = file === "_none_" ? input_name.contents : file;
  if (absname.contents) {
    return absolute_path(file$1);
  } else {
    return file$1;
  }
}

function print_filename(ppf, file) {
  return Curry._1(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%s"
                }), show_filename(file));
}

function reset(param) {
  num_loc_lines.contents = 0;
  
}

function get_pos_info(pos) {
  return [
          pos.pos_fname,
          pos.pos_lnum,
          pos.pos_cnum - pos.pos_bol | 0
        ];
}

var error_prefix = "Error";

function print_compact(ppf, loc) {
  var match = get_pos_info(loc.loc_start);
  var startchar = match[2];
  var endchar = (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum | 0) + startchar | 0;
  Curry._3(Format.fprintf(ppf, /* Format */{
            _0: {
              TAG: /* Alpha */15,
              _0: {
                TAG: /* Char_literal */12,
                _0: /* ':' */58,
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_i */3,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: /* End_of_format */0
                }
              }
            },
            _1: "%a:%i"
          }), print_filename, match[0], match[1]);
  if (startchar >= 0) {
    return Curry._2(Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* ',' */44,
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_i */3,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* String_literal */11,
                          _0: "--",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_i */3,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: /* End_of_format */0
                          }
                        }
                      }
                    },
                    _1: ",%i--%i"
                  }), startchar, endchar);
  }
  
}

function echo_eof(param) {
  Format.print_newline(undefined);
  num_loc_lines.contents = num_loc_lines.contents + 1 | 0;
  
}

function mkloc(txt, loc) {
  return {
          txt: txt,
          loc: loc
        };
}

function mknoloc(txt) {
  return {
          txt: txt,
          loc: none
        };
}

function pp_ksprintf(before, k, fmt) {
  var buf = $$Buffer.create(64);
  var ppf = Format.formatter_of_buffer(buf);
  Misc.Color.set_color_tag_handling(ppf);
  if (before !== undefined) {
    Curry._1(before, ppf);
  }
  return Format.kfprintf((function (param) {
                Format.pp_print_flush(ppf, undefined);
                return Curry._1(k, $$Buffer.contents(buf));
              }), ppf, fmt);
}

function print_phanton_error_prefix(ppf) {
  return Format.pp_print_as(ppf, error_prefix.length + 2 | 0, "");
}

function errorf(locOpt, subOpt, if_highlightOpt, fmt) {
  var loc = locOpt !== undefined ? locOpt : none;
  var sub = subOpt !== undefined ? subOpt : /* [] */0;
  var if_highlight = if_highlightOpt !== undefined ? if_highlightOpt : "";
  return pp_ksprintf(print_phanton_error_prefix, (function (msg) {
                return {
                        loc: loc,
                        msg: msg,
                        sub: sub,
                        if_highlight: if_highlight
                      };
              }), fmt);
}

function error(locOpt, subOpt, if_highlightOpt, msg) {
  var loc = locOpt !== undefined ? locOpt : none;
  var sub = subOpt !== undefined ? subOpt : /* [] */0;
  var if_highlight = if_highlightOpt !== undefined ? if_highlightOpt : "";
  return {
          loc: loc,
          msg: msg,
          sub: sub,
          if_highlight: if_highlight
        };
}

var error_of_exn = {
  contents: /* [] */0
};

function register_error_of_exn(f) {
  error_of_exn.contents = {
    hd: f,
    tl: error_of_exn.contents
  };
  
}

function error_of_exn$1(exn) {
  if (exn.RE_EXN_ID === Warnings.Errors) {
    return "Already_displayed";
  }
  var _x = error_of_exn.contents;
  while(true) {
    var x = _x;
    if (!x) {
      return ;
    }
    var error = Curry._1(x.hd, exn);
    if (error !== undefined) {
      return {
              NAME: "Ok",
              VAL: Caml_option.valFromOption(error)
            };
    }
    _x = x.tl;
    continue ;
  };
}

function error_of_printer(loc, print, x) {
  return Curry._2(errorf(loc, undefined, undefined, /* Format */{
                  _0: {
                    TAG: /* Alpha */15,
                    _0: {
                      TAG: /* Formatting_lit */17,
                      _0: /* FFlush */2,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%a@?"
                }), print, x);
}

function error_of_printer_file(print, x) {
  return error_of_printer(in_file(input_name.contents), print, x);
}

register_error_of_exn(function (x) {
      if (x.RE_EXN_ID === "Sys_error") {
        return Curry._1(errorf(in_file(input_name.contents), undefined, undefined, /* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "I/O error: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "I/O error: %s"
                      }), x._1);
      }
      if (x.RE_EXN_ID !== Misc.HookExnWrapper) {
        return ;
      }
      var e = x.error;
      var match = error_of_exn$1(e);
      var sub = match !== undefined && typeof match === "object" ? match.VAL : error(undefined, undefined, undefined, Printexc.to_string(e));
      return Curry._1(errorf(in_file(x.hook_info.sourcefile), {
                      hd: sub,
                      tl: /* [] */0
                    }, undefined, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "In hook ",
                        _1: {
                          TAG: /* Caml_string */3,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* ':' */58,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "In hook %S:"
                    }), x.hook_name);
    });

var $$Error = /* @__PURE__ */Caml_exceptions.create("Location.Error");

register_error_of_exn(function (x) {
      if (x.RE_EXN_ID === $$Error) {
        return x._1;
      }
      
    });

function raise_errorf(locOpt, subOpt, if_highlightOpt) {
  var loc = locOpt !== undefined ? locOpt : none;
  var sub = subOpt !== undefined ? subOpt : /* [] */0;
  var if_highlight = if_highlightOpt !== undefined ? if_highlightOpt : "";
  var partial_arg = print_phanton_error_prefix;
  return function (param) {
    return pp_ksprintf(partial_arg, (function (msg) {
                  throw {
                        RE_EXN_ID: $$Error,
                        _1: {
                          loc: loc,
                          msg: msg,
                          sub: sub,
                          if_highlight: if_highlight
                        },
                        Error: new Error()
                      };
                }), param);
  };
}

var msg_file = "File \"";

var msg_line = "\", line ";

var msg_chars = ", characters ";

var msg_to = "-";

var msg_colon = ":";

var warning_prefix = "Warning";

var Already_displayed_error = Warnings.Errors;

export {
  absname ,
  in_file ,
  none ,
  curr ,
  init ,
  symbol_rloc ,
  symbol_gloc ,
  rhs_loc ,
  input_name ,
  input_lexbuf ,
  set_input_name ,
  num_loc_lines ,
  absolute_path ,
  show_filename ,
  print_filename ,
  reset ,
  msg_file ,
  msg_line ,
  msg_chars ,
  msg_to ,
  msg_colon ,
  get_pos_info ,
  error_prefix ,
  warning_prefix ,
  print_compact ,
  echo_eof ,
  mkloc ,
  mknoloc ,
  pp_ksprintf ,
  print_phanton_error_prefix ,
  errorf ,
  error ,
  register_error_of_exn ,
  Already_displayed_error ,
  error_of_exn$1 as error_of_exn,
  error_of_printer ,
  error_of_printer_file ,
  $$Error ,
  raise_errorf ,
  
}
/* none Not a pure module */
