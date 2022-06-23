// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "./format.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Res_token from "./res_token.js";
import * as Res_grammar from "./res_grammar.js";
import * as Res_diagnostics_printing_utils from "./res_diagnostics_printing_utils.js";

function getStartPos(t) {
  return t.startPos;
}

function getEndPos(t) {
  return t.endPos;
}

function defaultUnexpected(token) {
  return "I'm not sure what to parse here when looking at \"" + (Res_token.toString(token) + "\".");
}

function reservedKeyword(token) {
  var tokenTxt = Res_token.toString(token);
  return "`" + (tokenTxt + ("` is a reserved keyword. Keywords need to be escaped: \\\"" + (tokenTxt + "\"")));
}

function explain(t) {
  var currentToken = t.category;
  if (typeof currentToken === "number") {
    switch (currentToken) {
      case /* UnclosedString */0 :
          return "This string is missing a double quote at the end";
      case /* UnclosedTemplate */1 :
          return "Did you forget to close this template expression with a backtick?";
      case /* UnclosedComment */2 :
          return "This comment seems to be missing a closing `*/`";
      
    }
  } else {
    switch (currentToken.TAG | 0) {
      case /* Unexpected */0 :
          var breadcrumbs = currentToken.context;
          var t$1 = currentToken.token;
          var name = Res_token.toString(t$1);
          if (breadcrumbs) {
            var match = breadcrumbs.hd[0];
            if (typeof match === "number") {
              if (match >= 32) {
                if (match !== 52) {
                  if (match === 55) {
                    var breadcrumbs$1 = breadcrumbs.tl;
                    var exit = 0;
                    if (typeof t$1 === "number") {
                      if (t$1 !== 14) {
                        if (t$1 !== 53) {
                          if (t$1 !== 57 || !breadcrumbs$1) {
                            exit = 2;
                          } else {
                            if (breadcrumbs$1.hd[0] === 23) {
                              return "I was expecting a pattern to match on before the `=>`";
                            }
                            exit = 2;
                          }
                        } else if (breadcrumbs$1) {
                          if (breadcrumbs$1.hd[0] === 16) {
                            return "A for-loop has the following form: `for i in 0 to 10`. Did you forget to supply a name before `in`?";
                          }
                          exit = 2;
                        } else {
                          exit = 2;
                        }
                      } else if (breadcrumbs$1) {
                        if (breadcrumbs$1.hd[0] === 24) {
                          return "I was expecting a name for this let-binding. Example: `let message = \"hello\"`";
                        }
                        exit = 2;
                      } else {
                        exit = 2;
                      }
                    } else {
                      exit = 2;
                    }
                    if (exit === 2) {
                      if (Res_token.isKeyword(t$1)) {
                        return reservedKeyword(t$1);
                      } else {
                        return defaultUnexpected(t$1);
                      }
                    }
                    
                  }
                  
                } else {
                  var breadcrumbs$2 = breadcrumbs.tl;
                  var exit$1 = 0;
                  if (breadcrumbs$2) {
                    var match$1 = breadcrumbs$2.hd[0];
                    if (typeof match$1 === "number" && (match$1 === 38 || match$1 === 37)) {
                      if (typeof t$1 === "number") {
                        switch (t$1) {
                          case /* Rbrace */23 :
                          case /* Comma */25 :
                          case /* Eof */26 :
                          case /* At */75 :
                              return "I'm missing a type here";
                          default:
                            exit$1 = 2;
                        }
                      } else {
                        if (t$1.TAG === /* String */3) {
                          return "I'm missing a type here";
                        }
                        exit$1 = 2;
                      }
                    } else {
                      exit$1 = 2;
                    }
                  } else {
                    exit$1 = 2;
                  }
                  if (exit$1 === 2) {
                    if (Res_grammar.isStructureItemStart(t$1) || t$1 === /* Eof */26) {
                      return "Missing a type here";
                    } else {
                      return defaultUnexpected(t$1);
                    }
                  }
                  
                }
              } else if (match !== 7) {
                if (match >= 31) {
                  if (typeof t$1 === "number") {
                    return "I'm not sure what to parse here when looking at \"" + (name + "\".");
                  } else if (t$1.TAG === /* Lident */4) {
                    return "Did you mean '" + (t$1._0 + "? A Type parameter starts with a quote.");
                  } else {
                    return "I'm not sure what to parse here when looking at \"" + (name + "\".");
                  }
                }
                
              } else {
                var breadcrumbs$3 = breadcrumbs.tl;
                var exit$2 = 0;
                if (breadcrumbs$3) {
                  var match$2 = breadcrumbs$3.hd[0];
                  var exit$3 = 0;
                  if (typeof match$2 !== "number") {
                    return "Did you forget to write an expression here?";
                  }
                  switch (match$2) {
                    case /* ExprUnary */8 :
                        return "Did you forget to write an expression here?";
                    case /* ExprSetField */9 :
                        return "It seems that this record field mutation misses an expression";
                    case /* ExprBlock */10 :
                        if (typeof t$1 === "number") {
                          if (t$1 === 17) {
                            return "Looks like there might be an expression missing here";
                          }
                          if (t$1 === 23) {
                            return "It seems that this expression block is empty";
                          }
                          exit$3 = 3;
                        } else {
                          exit$3 = 3;
                        }
                        break;
                    case /* ExprArrayMutation */14 :
                        return "Seems that an expression is missing, with what do I mutate the array?";
                    case /* ExprCall */11 :
                    case /* ExprList */12 :
                    case /* ExprArrayAccess */13 :
                    case /* ExprIf */15 :
                    case /* ExprFor */16 :
                    case /* IfCondition */17 :
                    case /* IfBranch */18 :
                    case /* ElseBranch */19 :
                    case /* TypeExpression */20 :
                    case /* External */21 :
                    case /* PatternMatching */22 :
                    case /* PatternMatchCase */23 :
                        exit$3 = 3;
                        break;
                    case /* LetBinding */24 :
                        return "This let-binding misses an expression";
                    default:
                      exit$3 = 3;
                  }
                  if (exit$3 === 3) {
                    if (typeof t$1 === "number") {
                      switch (t$1) {
                        case /* Lbrace */22 :
                        case /* Colon */24 :
                        case /* Comma */25 :
                            exit$2 = 2;
                            break;
                        case /* Rbracket */21 :
                        case /* Rbrace */23 :
                        case /* Eof */26 :
                            return "Missing expression";
                        default:
                          exit$2 = 2;
                      }
                    } else {
                      exit$2 = 2;
                    }
                  }
                  
                } else {
                  exit$2 = 2;
                }
                if (exit$2 === 2) {
                  return "I'm not sure what to parse here when looking at \"" + (name + "\".");
                }
                
              }
            }
            
          }
          if (Res_token.isKeyword(t$1)) {
            return "`" + (name + ("` is a reserved keyword. Keywords need to be escaped: \\\"" + (Res_token.toString(t$1) + "\"")));
          } else {
            return "I'm not sure what to parse here when looking at \"" + (name + "\".");
          }
      case /* Expected */1 :
          var context = currentToken.context;
          var hint = context !== undefined ? " It signals the start of " + Res_grammar.toString(context) : "";
          return "Did you forget a `" + (Res_token.toString(currentToken.token) + ("` here?" + hint));
      case /* Message */2 :
          return currentToken._0;
      case /* Uident */3 :
          var currentToken$1 = currentToken._0;
          if (typeof currentToken$1 !== "number" && currentToken$1.TAG === /* Lident */4) {
            var lident = currentToken$1._0;
            var guess = $$String.capitalize_ascii(lident);
            return "Did you mean `" + (guess + ("` instead of `" + (lident + "`?")));
          }
          if (!Res_token.isKeyword(currentToken$1)) {
            return "At this point, I'm looking for an uppercased name like `Belt` or `Array`";
          }
          var token = Res_token.toString(currentToken$1);
          return "`" + (token + "` is a reserved keyword.");
          break;
      case /* Lident */4 :
          var currentToken$2 = currentToken._0;
          if (typeof currentToken$2 !== "number" && currentToken$2.TAG === /* Uident */5) {
            var uident = currentToken$2._0;
            var guess$1 = $$String.uncapitalize_ascii(uident);
            return "Did you mean `" + (guess$1 + ("` instead of `" + (uident + "`?")));
          }
          if (!Res_token.isKeyword(currentToken$2)) {
            if (currentToken$2 === 12) {
              return "`_` isn't a valid name.";
            } else {
              return "I'm expecting a lowercase name like `user or `age`";
            }
          }
          var token$1 = Res_token.toString(currentToken$2);
          return "`" + (token$1 + ("` is a reserved keyword. Keywords need to be escaped: \\\"" + (token$1 + "\"")));
          break;
      case /* UnknownUchar */5 :
          if (currentToken._0 !== 94) {
            return "Not sure what to do with this character.";
          } else {
            return "Not sure what to do with this character.\n" + ("  If you're trying to dereference a mutable value, use `myValue.contents` instead.\n" + "  To concatenate strings, use `\"a\" ++ \"b\"` instead.");
          }
      
    }
  }
}

function make(startPos, endPos, category) {
  return {
          startPos: startPos,
          endPos: endPos,
          category: category
        };
}

function printReport(diagnostics, src) {
  var print = function (_diagnostics, src) {
    while(true) {
      var diagnostics = _diagnostics;
      if (!diagnostics) {
        return ;
      }
      var rest = diagnostics.tl;
      var d = diagnostics.hd;
      Res_diagnostics_printing_utils.Super_location.super_error_reporter(Format.err_formatter, src, {
            loc: {
              loc_start: d.startPos,
              loc_end: d.endPos,
              loc_ghost: false
            },
            msg: explain(d),
            sub: /* [] */0,
            if_highlight: ""
          });
      if (rest) {
        Curry._1(Format.fprintf(Format.err_formatter), "@.");
      }
      _diagnostics = rest;
      continue ;
    };
  };
  Curry._1(Format.fprintf(Format.err_formatter), "@[<v>");
  print(List.rev(diagnostics), src);
  return Curry._1(Format.fprintf(Format.err_formatter), "@]@.");
}

function unexpected(token, context) {
  return {
          TAG: /* Unexpected */0,
          token: token,
          context: context
        };
}

function expected(grammar, pos, token) {
  return {
          TAG: /* Expected */1,
          context: grammar,
          pos: pos,
          token: token
        };
}

function uident(currentToken) {
  return {
          TAG: /* Uident */3,
          _0: currentToken
        };
}

function lident(currentToken) {
  return {
          TAG: /* Lident */4,
          _0: currentToken
        };
}

function unknownUchar(code) {
  return {
          TAG: /* UnknownUchar */5,
          _0: code
        };
}

function message(txt) {
  return {
          TAG: /* Message */2,
          _0: txt
        };
}

var Grammar;

var Token;

var unclosedString = /* UnclosedString */0;

var unclosedComment = /* UnclosedComment */2;

var unclosedTemplate = /* UnclosedTemplate */1;

export {
  Grammar ,
  Token ,
  getStartPos ,
  getEndPos ,
  defaultUnexpected ,
  reservedKeyword ,
  explain ,
  make ,
  printReport ,
  unexpected ,
  expected ,
  uident ,
  lident ,
  unclosedString ,
  unclosedComment ,
  unclosedTemplate ,
  unknownUchar ,
  message ,
}
/* Format Not a pure module */
