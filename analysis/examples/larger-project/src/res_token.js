// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_string from "rescript/lib/es6/caml_string.js";
import * as Res_comment from "./res_comment.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function precedence(x) {
  if (typeof x !== "number") {
    return 0;
  }
  if (x < 17) {
    if (x !== 4) {
      if (x >= 14) {
        return 4;
      } else {
        return 0;
      }
    } else {
      return 9;
    }
  }
  if (x >= 46) {
    if (x < 58) {
      return 0;
    }
    switch (x) {
      case /* MinusGreater */58 :
          return 8;
      case /* Land */67 :
          return 3;
      case /* Lor */68 :
          return 2;
      case /* ColonEqual */74 :
          return 1;
      case /* BangEqual */70 :
      case /* BangEqualEqual */71 :
      case /* LessEqual */72 :
      case /* GreaterEqual */73 :
      case /* BarGreater */81 :
          return 4;
      case /* External */59 :
      case /* Typ */60 :
      case /* Private */61 :
      case /* Mutable */62 :
      case /* Constraint */63 :
      case /* Include */64 :
      case /* Module */65 :
      case /* Of */66 :
      case /* Band */69 :
      case /* At */75 :
      case /* AtAt */76 :
      case /* Percent */77 :
      case /* PercentPercent */78 :
      case /* List */79 :
      case /* Backtick */80 :
      case /* Try */82 :
      case /* Import */83 :
      case /* Export */84 :
          return 0;
      
    }
  } else {
    if (x < 29) {
      return 0;
    }
    switch (x) {
      case /* Forwardslash */29 :
      case /* ForwardslashDot */30 :
      case /* Asterisk */31 :
      case /* AsteriskDot */32 :
          return 6;
      case /* Exponentiation */33 :
          return 7;
      case /* Minus */34 :
      case /* MinusDot */35 :
      case /* Plus */36 :
      case /* PlusDot */37 :
      case /* PlusPlus */38 :
          return 5;
      case /* GreaterThan */41 :
      case /* LessThan */42 :
          return 4;
      case /* PlusEqual */39 :
      case /* ColonGreaterThan */40 :
      case /* LessThanSlash */43 :
      case /* Hash */44 :
          return 0;
      case /* HashEqual */45 :
          return 1;
      
    }
  }
}

function toString(x) {
  if (typeof x === "number") {
    switch (x) {
      case /* Open */0 :
          return "open";
      case /* True */1 :
          return "true";
      case /* False */2 :
          return "false";
      case /* As */3 :
          return "as";
      case /* Dot */4 :
          return ".";
      case /* DotDot */5 :
          return "..";
      case /* DotDotDot */6 :
          return "...";
      case /* Bang */7 :
          return "!";
      case /* Semicolon */8 :
          return ";";
      case /* Let */9 :
          return "let";
      case /* And */10 :
          return "and";
      case /* Rec */11 :
          return "rec";
      case /* Underscore */12 :
          return "_";
      case /* SingleQuote */13 :
          return "'";
      case /* Equal */14 :
          return "=";
      case /* EqualEqual */15 :
          return "==";
      case /* EqualEqualEqual */16 :
          return "===";
      case /* Bar */17 :
          return "|";
      case /* Lparen */18 :
          return "(";
      case /* Rparen */19 :
          return ")";
      case /* Lbracket */20 :
          return "[";
      case /* Rbracket */21 :
          return "]";
      case /* Lbrace */22 :
          return "{";
      case /* Rbrace */23 :
          return "}";
      case /* Colon */24 :
          return ":";
      case /* Comma */25 :
          return ",";
      case /* Eof */26 :
          return "eof";
      case /* Exception */27 :
          return "exception";
      case /* Backslash */28 :
          return "\\";
      case /* Forwardslash */29 :
          return "/";
      case /* ForwardslashDot */30 :
          return "/.";
      case /* Asterisk */31 :
          return "*";
      case /* AsteriskDot */32 :
          return "*.";
      case /* Exponentiation */33 :
          return "**";
      case /* Minus */34 :
          return "-";
      case /* MinusDot */35 :
          return "-.";
      case /* Plus */36 :
          return "+";
      case /* PlusDot */37 :
          return "+.";
      case /* PlusPlus */38 :
          return "++";
      case /* PlusEqual */39 :
          return "+=";
      case /* ColonGreaterThan */40 :
          return ":>";
      case /* GreaterThan */41 :
          return ">";
      case /* LessThan */42 :
          return "<";
      case /* LessThanSlash */43 :
          return "</";
      case /* Hash */44 :
          return "#";
      case /* HashEqual */45 :
          return "#=";
      case /* Assert */46 :
          return "assert";
      case /* Lazy */47 :
          return "lazy";
      case /* Tilde */48 :
          return "tilde";
      case /* Question */49 :
          return "?";
      case /* If */50 :
          return "if";
      case /* Else */51 :
          return "else";
      case /* For */52 :
          return "for";
      case /* In */53 :
          return "in";
      case /* While */54 :
          return "while";
      case /* Switch */55 :
          return "switch";
      case /* When */56 :
          return "when";
      case /* EqualGreater */57 :
          return "=>";
      case /* MinusGreater */58 :
          return "->";
      case /* External */59 :
          return "external";
      case /* Typ */60 :
          return "type";
      case /* Private */61 :
          return "private";
      case /* Mutable */62 :
          return "mutable";
      case /* Constraint */63 :
          return "constraint";
      case /* Include */64 :
          return "include";
      case /* Module */65 :
          return "module";
      case /* Of */66 :
          return "of";
      case /* Land */67 :
          return "&&";
      case /* Lor */68 :
          return "||";
      case /* Band */69 :
          return "&";
      case /* BangEqual */70 :
          return "!=";
      case /* BangEqualEqual */71 :
          return "!==";
      case /* LessEqual */72 :
          return "<=";
      case /* GreaterEqual */73 :
          return ">=";
      case /* ColonEqual */74 :
          return ":=";
      case /* At */75 :
          return "@";
      case /* AtAt */76 :
          return "@@";
      case /* Percent */77 :
          return "%";
      case /* PercentPercent */78 :
          return "%%";
      case /* List */79 :
          return "list{";
      case /* Backtick */80 :
          return "`";
      case /* BarGreater */81 :
          return "|>";
      case /* Try */82 :
          return "try";
      case /* Import */83 :
          return "import";
      case /* Export */84 :
          return "export";
      
    }
  } else {
    switch (x.TAG | 0) {
      case /* Codepoint */0 :
          return "codepoint '" + (x.original + "'");
      case /* Int */1 :
          return "int " + x.i;
      case /* Float */2 :
          return "Float: " + x.f;
      case /* String */3 :
          return "string \"" + (x._0 + "\"");
      case /* Lident */4 :
      case /* Uident */5 :
          return x._0;
      case /* Comment */6 :
          return "Comment" + Res_comment.toString(x._0);
      case /* TemplateTail */7 :
          return "TemplateTail(" + (x._0 + ")");
      case /* TemplatePart */8 :
          return x._0 + "${";
      
    }
  }
}

function keywordTable(x) {
  switch (x) {
    case "and" :
        return /* And */10;
    case "as" :
        return /* As */3;
    case "assert" :
        return /* Assert */46;
    case "constraint" :
        return /* Constraint */63;
    case "else" :
        return /* Else */51;
    case "exception" :
        return /* Exception */27;
    case "export" :
        return /* Export */84;
    case "external" :
        return /* External */59;
    case "false" :
        return /* False */2;
    case "for" :
        return /* For */52;
    case "if" :
        return /* If */50;
    case "import" :
        return /* Import */83;
    case "in" :
        return /* In */53;
    case "include" :
        return /* Include */64;
    case "lazy" :
        return /* Lazy */47;
    case "let" :
        return /* Let */9;
    case "list{" :
        return /* List */79;
    case "module" :
        return /* Module */65;
    case "mutable" :
        return /* Mutable */62;
    case "of" :
        return /* Of */66;
    case "open" :
        return /* Open */0;
    case "private" :
        return /* Private */61;
    case "rec" :
        return /* Rec */11;
    case "switch" :
        return /* Switch */55;
    case "true" :
        return /* True */1;
    case "try" :
        return /* Try */82;
    case "type" :
        return /* Typ */60;
    case "when" :
        return /* When */56;
    case "while" :
        return /* While */54;
    default:
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
  }
}

function isKeyword(x) {
  if (typeof x === "number") {
    if (x >= 48) {
      if (x >= 69) {
        if (x !== 79) {
          return x >= 82;
        } else {
          return true;
        }
      } else if (x >= 57) {
        return x >= 59;
      } else {
        return x >= 50;
      }
    } else if (x > 45 || x < 12) {
      return x > 8 || x < 4;
    } else {
      return x === 27;
    }
  } else {
    return false;
  }
}

function lookupKeyword(str) {
  try {
    return keywordTable(str);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      var match = Caml_string.get(str, 0);
      if (match > 90 || match < 65) {
        return {
                TAG: /* Lident */4,
                _0: str
              };
      } else {
        return {
                TAG: /* Uident */5,
                _0: str
              };
      }
    }
    throw exn;
  }
}

function isKeywordTxt(str) {
  try {
    keywordTable(str);
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

var $$Comment;

var $$catch = {
  TAG: /* Lident */4,
  _0: "catch"
};

export {
  $$Comment ,
  precedence ,
  toString ,
  keywordTable ,
  isKeyword ,
  lookupKeyword ,
  isKeywordTxt ,
  $$catch ,
}
/* Res_comment Not a pure module */
