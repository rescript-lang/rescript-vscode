// Generated by ReScript, PLEASE EDIT WITH CARE

import * as P from "./P.js";
import * as Caml from "rescript/lib/es6/caml.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as $$Buffer from "rescript/lib/es6/buffer.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Res_utf8 from "./res_utf8.js";
import * as Res_token from "./res_token.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Res_comment from "./res_comment.js";
import * as Res_diagnostics from "./res_diagnostics.js";

function setDiamondMode(scanner) {
  scanner.mode = {
    hd: /* Diamond */1,
    tl: scanner.mode
  };
}

function setJsxMode(scanner) {
  scanner.mode = {
    hd: /* Jsx */0,
    tl: scanner.mode
  };
}

function popMode(scanner, mode) {
  var match = scanner.mode;
  if (match && match.hd === mode) {
    scanner.mode = match.tl;
    return ;
  }
  
}

function inDiamondMode(scanner) {
  var match = scanner.mode;
  if (match && match.hd) {
    return true;
  } else {
    return false;
  }
}

function inJsxMode(scanner) {
  var match = scanner.mode;
  if (match && !match.hd) {
    return true;
  } else {
    return false;
  }
}

function position(scanner) {
  return {
          pos_fname: scanner.filename,
          pos_lnum: scanner.lnum,
          pos_bol: scanner.lineOffset,
          pos_cnum: scanner.offset
        };
}

function _printDebug(startPos, endPos, scanner, token) {
  Pervasives.print_string(scanner.src);
  Pervasives.print_string($$String.make(startPos.pos_cnum, /* ' ' */32));
  P.print_char(/* '^' */94);
  var n = endPos.pos_cnum - startPos.pos_cnum | 0;
  if (n !== 0) {
    if (n !== 1) {
      Pervasives.print_string($$String.make(n - 2 | 0, /* '-' */45));
      P.print_char(/* '^' */94);
    }
    
  } else if (token !== /* Eof */26) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "res_scanner.res",
            89,
            6
          ],
          Error: new Error()
        };
  }
  P.print_char(/* ' ' */32);
  Pervasives.print_string(Res_token.toString(token));
  P.print_char(/* ' ' */32);
  Pervasives.print_int(startPos.pos_cnum);
  P.print_char(/* '-' */45);
  Pervasives.print_int(endPos.pos_cnum);
  console.log("");
}

function next(scanner) {
  var nextOffset = scanner.offset + 1 | 0;
  var match = scanner.ch;
  if (match !== 10) {
    
  } else {
    scanner.lineOffset = nextOffset;
    scanner.lnum = scanner.lnum + 1 | 0;
  }
  if (nextOffset < scanner.src.length) {
    scanner.offset = nextOffset;
    scanner.ch = scanner.src.codePointAt(scanner.offset);
  } else {
    scanner.offset = scanner.src.length;
    scanner.ch = -1;
  }
}

function next2(scanner) {
  next(scanner);
  next(scanner);
}

function next3(scanner) {
  next(scanner);
  next(scanner);
  next(scanner);
}

function peek(scanner) {
  if ((scanner.offset + 1 | 0) < scanner.src.length) {
    return scanner.src.codePointAt(scanner.offset + 1 | 0);
  } else {
    return -1;
  }
}

function peek2(scanner) {
  if ((scanner.offset + 2 | 0) < scanner.src.length) {
    return scanner.src.codePointAt(scanner.offset + 2 | 0);
  } else {
    return -1;
  }
}

function make(filename, src) {
  return {
          filename: filename,
          src: src,
          err: (function (param, param$1, param$2) {
              
            }),
          ch: src === "" ? -1 : src.codePointAt(0),
          offset: 0,
          lineOffset: 0,
          lnum: 1,
          mode: /* [] */0
        };
}

function isWhitespace(ch) {
  if (ch > 13 || ch < 9) {
    return ch === 32;
  } else {
    return !(ch === 12 || ch === 11);
  }
}

function skipWhitespace(scanner) {
  while(true) {
    if (!isWhitespace(scanner.ch)) {
      return ;
    }
    next(scanner);
    continue ;
  };
}

function digitValue(ch) {
  if (ch >= 65) {
    if (ch >= 97) {
      if (ch >= 103) {
        return 16;
      } else {
        return (ch - /* 'a' */97 | 0) + 10 | 0;
      }
    } else if (ch >= 71) {
      return 16;
    } else {
      return ((ch + 32 | 0) - /* 'a' */97 | 0) + 10 | 0;
    }
  } else if (ch > 57 || ch < 48) {
    return 16;
  } else {
    return ch - 48 | 0;
  }
}

function skipLowerCaseChars(scanner) {
  while(true) {
    var match = scanner.ch;
    if (match > 122 || match < 97) {
      return ;
    }
    next(scanner);
    continue ;
  };
}

function scanIdentifier(scanner) {
  var startOff = scanner.offset;
  var skipGoodChars = function (scanner) {
    while(true) {
      var match = scanner.ch;
      if (match >= 65) {
        if (match > 96 || match < 91) {
          if (match >= 123) {
            return ;
          }
          next(scanner);
          continue ;
        }
        if (match !== 95) {
          return ;
        }
        next(scanner);
        continue ;
      }
      if (match >= 48) {
        if (match >= 58) {
          return ;
        }
        next(scanner);
        continue ;
      }
      if (match !== 39) {
        return ;
      }
      next(scanner);
      continue ;
    };
  };
  skipGoodChars(scanner);
  var str = $$String.sub(scanner.src, startOff, scanner.offset - startOff | 0);
  if (/* '{' */123 === scanner.ch && str === "list") {
    next(scanner);
    return Res_token.lookupKeyword("list{");
  } else {
    return Res_token.lookupKeyword(str);
  }
}

function scanDigits(scanner, base) {
  if (base <= 10) {
    while(true) {
      var match = scanner.ch;
      if (match >= 58) {
        if (match !== 95) {
          return ;
        }
        next(scanner);
        continue ;
      }
      if (match < 48) {
        return ;
      }
      next(scanner);
      continue ;
    };
  }
  while(true) {
    var match$1 = scanner.ch;
    if (match$1 >= 71) {
      if (match$1 >= 97) {
        if (match$1 >= 103) {
          return ;
        }
        next(scanner);
        continue ;
      }
      if (match$1 !== 95) {
        return ;
      }
      next(scanner);
      continue ;
    }
    if (match$1 >= 58) {
      if (match$1 < 65) {
        return ;
      }
      next(scanner);
      continue ;
    }
    if (match$1 < 48) {
      return ;
    }
    next(scanner);
    continue ;
  };
}

function scanNumber(scanner) {
  var startOff = scanner.offset;
  var match = scanner.ch;
  var base;
  if (match !== 48) {
    base = 10;
  } else {
    var match$1 = peek(scanner);
    if (match$1 >= 89) {
      if (match$1 !== 98) {
        if (match$1 !== 111) {
          if (match$1 !== 120) {
            next(scanner);
            base = 8;
          } else {
            next(scanner);
            next(scanner);
            base = 16;
          }
        } else {
          next(scanner);
          next(scanner);
          base = 8;
        }
      } else {
        next(scanner);
        next(scanner);
        base = 2;
      }
    } else if (match$1 !== 66) {
      if (match$1 !== 79) {
        if (match$1 >= 88) {
          next(scanner);
          next(scanner);
          base = 16;
        } else {
          next(scanner);
          base = 8;
        }
      } else {
        next(scanner);
        next(scanner);
        base = 8;
      }
    } else {
      next(scanner);
      next(scanner);
      base = 2;
    }
  }
  scanDigits(scanner, base);
  var isFloat = /* '.' */46 === scanner.ch ? (next(scanner), scanDigits(scanner, base), true) : false;
  var match$2 = scanner.ch;
  var isFloat$1;
  var exit = 0;
  if (match$2 >= 81) {
    if (match$2 !== 101 && match$2 !== 112) {
      isFloat$1 = isFloat;
    } else {
      exit = 1;
    }
  } else if (match$2 !== 69 && match$2 < 80) {
    isFloat$1 = isFloat;
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var match$3 = peek(scanner);
    if (match$3 !== 43 && match$3 !== 45) {
      next(scanner);
    } else {
      next(scanner);
      next(scanner);
    }
    scanDigits(scanner, base);
    isFloat$1 = true;
  }
  var literal = $$String.sub(scanner.src, startOff, scanner.offset - startOff | 0);
  var ch = scanner.ch;
  var suffix;
  if (ch > 122 || ch < 103) {
    if (ch > 90 || ch < 71) {
      suffix = undefined;
    } else {
      next(scanner);
      suffix = ch;
    }
  } else if (ch !== 110) {
    next(scanner);
    suffix = ch;
  } else {
    var msg = "Unsupported number type (nativeint). Did you mean `" + (literal + "`?");
    var pos = position(scanner);
    Curry._3(scanner.err, pos, pos, Res_diagnostics.message(msg));
    next(scanner);
    suffix = /* 'n' */110;
  }
  if (isFloat$1) {
    return {
            TAG: /* Float */2,
            f: literal,
            suffix: suffix
          };
  } else {
    return {
            TAG: /* Int */1,
            i: literal,
            suffix: suffix
          };
  }
}

function scanExoticIdentifier(scanner) {
  next(scanner);
  var buffer = $$Buffer.create(20);
  var startPos = position(scanner);
  var scan = function (_param) {
    while(true) {
      var ch = scanner.ch;
      if (ch > 13 || ch < 10) {
        if (ch === 34) {
          return next(scanner);
        }
        
      } else if (!(ch === 12 || ch === 11)) {
        var endPos = position(scanner);
        Curry._3(scanner.err, startPos, endPos, Res_diagnostics.message("A quoted identifier can't contain line breaks."));
        return next(scanner);
      }
      if (ch === -1) {
        var endPos$1 = position(scanner);
        return Curry._3(scanner.err, startPos, endPos$1, Res_diagnostics.message("Did you forget a \" here?"));
      }
      $$Buffer.add_char(buffer, ch);
      next(scanner);
      _param = undefined;
      continue ;
    };
  };
  scan(undefined);
  return {
          TAG: /* Lident */4,
          _0: $$Buffer.contents(buffer)
        };
}

function scanStringEscapeSequence(startPos, scanner) {
  var scan = function (n, base, max) {
    var loop = function (_n, _x) {
      while(true) {
        var x = _x;
        var n = _n;
        if (n === 0) {
          return x;
        }
        var d = digitValue(scanner.ch);
        if (d >= base) {
          var pos = position(scanner);
          var msg = scanner.ch === -1 ? "unclosed escape sequence" : "unknown escape sequence";
          Curry._3(scanner.err, startPos, pos, Res_diagnostics.message(msg));
          return -1;
        }
        next(scanner);
        _x = Math.imul(x, base) + d | 0;
        _n = n - 1 | 0;
        continue ;
      };
    };
    var x = loop(n, 0);
    if (!(x > max || 55296 <= x && x < 57344)) {
      return ;
    }
    var pos = position(scanner);
    Curry._3(scanner.err, startPos, pos, Res_diagnostics.message("escape sequence is invalid unicode code point"));
  };
  var match = scanner.ch;
  if (match >= 48) {
    if (match < 92) {
      if (match >= 58) {
        return ;
      } else {
        return scan(3, 10, 255);
      }
    }
    if (match >= 121) {
      return ;
    }
    switch (match) {
      case 111 :
          next(scanner);
          return scan(3, 8, 255);
      case 92 :
      case 98 :
      case 110 :
      case 114 :
      case 116 :
          return next(scanner);
      case 117 :
          next(scanner);
          var match$1 = scanner.ch;
          if (match$1 !== 123) {
            return scan(4, 16, Res_utf8.max);
          }
          next(scanner);
          var x = 0;
          while((function () {
                  var match = scanner.ch;
                  if (match > 70 || match < 48) {
                    return !(match > 102 || match < 97);
                  } else {
                    return match > 64 || match < 58;
                  }
                })()) {
            x = (x << 4) + digitValue(scanner.ch) | 0;
            next(scanner);
          };
          var match$2 = scanner.ch;
          if (match$2 !== 125) {
            return ;
          } else {
            return next(scanner);
          }
      case 93 :
      case 94 :
      case 95 :
      case 96 :
      case 97 :
      case 99 :
      case 100 :
      case 101 :
      case 102 :
      case 103 :
      case 104 :
      case 105 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 112 :
      case 113 :
      case 115 :
      case 118 :
      case 119 :
          return ;
      case 120 :
          next(scanner);
          return scan(2, 16, 255);
      
    }
  } else {
    switch (match) {
      case 33 :
      case 35 :
      case 36 :
      case 37 :
      case 38 :
          return ;
      case 32 :
      case 34 :
      case 39 :
          return next(scanner);
      default:
        return ;
    }
  }
}

function scanString(scanner) {
  var startPosWithQuote = position(scanner);
  next(scanner);
  var firstCharOffset = scanner.offset;
  var scan = function (_param) {
    while(true) {
      var ch = scanner.ch;
      if (ch !== 34) {
        if (ch !== 92) {
          if (ch === -1) {
            var endPos = position(scanner);
            Curry._3(scanner.err, startPosWithQuote, endPos, Res_diagnostics.unclosedString);
            return $$String.sub(scanner.src, firstCharOffset, scanner.offset - firstCharOffset | 0);
          }
          next(scanner);
          _param = undefined;
          continue ;
        }
        var startPos = position(scanner);
        next(scanner);
        scanStringEscapeSequence(startPos, scanner);
        _param = undefined;
        continue ;
      }
      var lastCharOffset = scanner.offset;
      next(scanner);
      return $$String.sub(scanner.src, firstCharOffset, lastCharOffset - firstCharOffset | 0);
    };
  };
  return {
          TAG: /* String */3,
          _0: scan(undefined)
        };
}

function scanEscape(scanner) {
  var offset = scanner.offset - 1 | 0;
  var convertNumber = function (scanner, n, base) {
    var x = 0;
    for(var _for = n; _for >= 1; --_for){
      var d = digitValue(scanner.ch);
      x = Math.imul(x, base) + d | 0;
      next(scanner);
    }
    var c = x;
    if (Res_utf8.isValidCodePoint(c)) {
      return c;
    } else {
      return Res_utf8.repl;
    }
  };
  var ch = scanner.ch;
  var codepoint;
  if (ch >= 58) {
    switch (ch) {
      case 98 :
          next(scanner);
          codepoint = /* '\b' */8;
          break;
      case 110 :
          next(scanner);
          codepoint = /* '\n' */10;
          break;
      case 111 :
          next(scanner);
          codepoint = convertNumber(scanner, 3, 8);
          break;
      case 114 :
          next(scanner);
          codepoint = /* '\r' */13;
          break;
      case 116 :
          next(scanner);
          codepoint = /* '\t' */9;
          break;
      case 117 :
          next(scanner);
          var match = scanner.ch;
          if (match !== 123) {
            codepoint = convertNumber(scanner, 4, 16);
          } else {
            next(scanner);
            var x = 0;
            while((function () {
                    var match = scanner.ch;
                    if (match > 70 || match < 48) {
                      return !(match > 102 || match < 97);
                    } else {
                      return match > 64 || match < 58;
                    }
                  })()) {
              x = (x << 4) + digitValue(scanner.ch) | 0;
              next(scanner);
            };
            var match$1 = scanner.ch;
            if (match$1 !== 125) {
              
            } else {
              next(scanner);
            }
            var c = x;
            codepoint = Res_utf8.isValidCodePoint(c) ? c : Res_utf8.repl;
          }
          break;
      case 99 :
      case 100 :
      case 101 :
      case 102 :
      case 103 :
      case 104 :
      case 105 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 112 :
      case 113 :
      case 115 :
      case 118 :
      case 119 :
          next(scanner);
          codepoint = ch;
          break;
      case 120 :
          next(scanner);
          codepoint = convertNumber(scanner, 2, 16);
          break;
      default:
        next(scanner);
        codepoint = ch;
    }
  } else if (ch >= 48) {
    codepoint = convertNumber(scanner, 3, 10);
  } else {
    next(scanner);
    codepoint = ch;
  }
  var contents = $$String.sub(scanner.src, offset, scanner.offset - offset | 0);
  next(scanner);
  return {
          TAG: /* Codepoint */0,
          c: codepoint,
          original: contents
        };
}

function scanSingleLineComment(scanner) {
  var startOff = scanner.offset;
  var startPos = position(scanner);
  var skip = function (scanner) {
    while(true) {
      var ch = scanner.ch;
      if (ch === 10) {
        return ;
      }
      if (ch === 13) {
        return ;
      }
      if (ch === -1) {
        return ;
      }
      next(scanner);
      continue ;
    };
  };
  skip(scanner);
  var endPos = position(scanner);
  return {
          TAG: /* Comment */6,
          _0: Res_comment.makeSingleLineComment({
                loc_start: startPos,
                loc_end: endPos,
                loc_ghost: false
              }, $$String.sub(scanner.src, startOff, scanner.offset - startOff | 0))
        };
}

function scanMultiLineComment(scanner) {
  var contentStartOff = scanner.offset + 2 | 0;
  var startPos = position(scanner);
  var scan = function (_depth) {
    while(true) {
      var depth = _depth;
      var match = scanner.ch;
      var match$1 = peek(scanner);
      if (match !== 42) {
        if (match === 47 && match$1 === 42) {
          next(scanner);
          next(scanner);
          _depth = depth + 1 | 0;
          continue ;
        }
        
      } else if (match$1 === 47) {
        next(scanner);
        next(scanner);
        if (depth <= 1) {
          return ;
        }
        _depth = depth - 1 | 0;
        continue ;
      }
      if (match === -1) {
        var endPos = position(scanner);
        return Curry._3(scanner.err, startPos, endPos, Res_diagnostics.unclosedComment);
      }
      next(scanner);
      continue ;
    };
  };
  scan(0);
  var length = (scanner.offset - 2 | 0) - contentStartOff | 0;
  var length$1 = length < 0 ? 0 : length;
  return {
          TAG: /* Comment */6,
          _0: Res_comment.makeMultiLineComment({
                loc_start: startPos,
                loc_end: position(scanner),
                loc_ghost: false
              }, $$String.sub(scanner.src, contentStartOff, length$1))
        };
}

function scanTemplateLiteralToken(scanner) {
  var startOff = scanner.offset;
  if (scanner.ch === /* '}' */125) {
    next(scanner);
  }
  var startPos = position(scanner);
  var scan = function (_param) {
    while(true) {
      var ch = scanner.ch;
      if (ch !== 36) {
        if (ch !== 92) {
          if (ch !== 96) {
            if (ch === -1) {
              var endPos = position(scanner);
              Curry._3(scanner.err, startPos, endPos, Res_diagnostics.unclosedTemplate);
              return {
                      TAG: /* TemplateTail */7,
                      _0: $$String.sub(scanner.src, startOff, Caml.int_max((scanner.offset - 1 | 0) - startOff | 0, 0))
                    };
            }
            next(scanner);
            _param = undefined;
            continue ;
          }
          next(scanner);
          return {
                  TAG: /* TemplateTail */7,
                  _0: $$String.sub(scanner.src, startOff, (scanner.offset - 1 | 0) - startOff | 0)
                };
        }
        var match = peek(scanner);
        if (match >= 36) {
          if (match > 95 || match < 37) {
            if (match >= 97) {
              next(scanner);
              _param = undefined;
              continue ;
            }
            next(scanner);
            next(scanner);
            _param = undefined;
            continue ;
          }
          if (match !== 92) {
            next(scanner);
            _param = undefined;
            continue ;
          }
          next(scanner);
          next(scanner);
          _param = undefined;
          continue ;
        }
        if (match !== 10) {
          if (match !== 13) {
            next(scanner);
            _param = undefined;
            continue ;
          }
          next(scanner);
          next(scanner);
          _param = undefined;
          continue ;
        }
        next(scanner);
        next(scanner);
        _param = undefined;
        continue ;
      }
      var match$1 = peek(scanner);
      if (match$1 !== 123) {
        next(scanner);
        _param = undefined;
        continue ;
      }
      next(scanner);
      next(scanner);
      var contents = $$String.sub(scanner.src, startOff, (scanner.offset - 2 | 0) - startOff | 0);
      return {
              TAG: /* TemplatePart */8,
              _0: contents
            };
    };
  };
  var token = scan(undefined);
  var endPos = position(scanner);
  return [
          startPos,
          endPos,
          token
        ];
}

function scan(scanner) {
  skipWhitespace(scanner);
  var startPos = position(scanner);
  var ch = scanner.ch;
  var token;
  var exit = 0;
  switch (ch) {
    case 33 :
        var match = peek(scanner);
        var match$1 = peek2(scanner);
        if (match !== 61) {
          next(scanner);
          token = /* Bang */7;
        } else if (match$1 !== 61) {
          next(scanner);
          next(scanner);
          token = /* BangEqual */70;
        } else {
          next3(scanner);
          token = /* BangEqualEqual */71;
        }
        break;
    case 34 :
        token = scanString(scanner);
        break;
    case 35 :
        var match$2 = peek(scanner);
        if (match$2 !== 61) {
          next(scanner);
          token = /* Hash */44;
        } else {
          next(scanner);
          next(scanner);
          token = /* HashEqual */45;
        }
        break;
    case 37 :
        var match$3 = peek(scanner);
        if (match$3 !== 37) {
          next(scanner);
          token = /* Percent */77;
        } else {
          next(scanner);
          next(scanner);
          token = /* PercentPercent */78;
        }
        break;
    case 38 :
        var match$4 = peek(scanner);
        if (match$4 !== 38) {
          next(scanner);
          token = /* Band */69;
        } else {
          next(scanner);
          next(scanner);
          token = /* Land */67;
        }
        break;
    case 39 :
        var match$5 = peek(scanner);
        var match$6 = peek2(scanner);
        if (match$5 !== 92) {
          if (match$6 !== 39) {
            next(scanner);
            var offset = scanner.offset;
            var match$7 = Res_utf8.decodeCodePoint(scanner.offset, scanner.src, scanner.src.length);
            var length = match$7[1];
            for(var _for = 0; _for < length; ++_for){
              next(scanner);
            }
            if (scanner.ch === /* '\'' */39) {
              var contents = $$String.sub(scanner.src, offset, length);
              next(scanner);
              token = {
                TAG: /* Codepoint */0,
                c: match$7[0],
                original: contents
              };
            } else {
              scanner.ch = match$5;
              scanner.offset = offset;
              token = /* SingleQuote */13;
            }
          } else {
            var offset$1 = scanner.offset + 1 | 0;
            next3(scanner);
            token = {
              TAG: /* Codepoint */0,
              c: match$5,
              original: $$String.sub(scanner.src, offset$1, 1)
            };
          }
        } else if (match$6 !== 34) {
          next(scanner);
          next(scanner);
          token = scanEscape(scanner);
        } else {
          next(scanner);
          token = /* SingleQuote */13;
        }
        break;
    case 40 :
        next(scanner);
        token = /* Lparen */18;
        break;
    case 41 :
        next(scanner);
        token = /* Rparen */19;
        break;
    case 42 :
        var match$8 = peek(scanner);
        if (match$8 !== 42) {
          if (match$8 !== 46) {
            next(scanner);
            token = /* Asterisk */31;
          } else {
            next(scanner);
            next(scanner);
            token = /* AsteriskDot */32;
          }
        } else {
          next(scanner);
          next(scanner);
          token = /* Exponentiation */33;
        }
        break;
    case 43 :
        var match$9 = peek(scanner);
        if (match$9 !== 43) {
          if (match$9 !== 46) {
            if (match$9 !== 61) {
              next(scanner);
              token = /* Plus */36;
            } else {
              next(scanner);
              next(scanner);
              token = /* PlusEqual */39;
            }
          } else {
            next(scanner);
            next(scanner);
            token = /* PlusDot */37;
          }
        } else {
          next(scanner);
          next(scanner);
          token = /* PlusPlus */38;
        }
        break;
    case 44 :
        next(scanner);
        token = /* Comma */25;
        break;
    case 45 :
        var match$10 = peek(scanner);
        if (match$10 !== 46) {
          if (match$10 !== 62) {
            next(scanner);
            token = /* Minus */34;
          } else {
            next(scanner);
            next(scanner);
            token = /* MinusGreater */58;
          }
        } else {
          next(scanner);
          next(scanner);
          token = /* MinusDot */35;
        }
        break;
    case 46 :
        var match$11 = peek(scanner);
        var match$12 = peek2(scanner);
        if (match$11 !== 46) {
          next(scanner);
          token = /* Dot */4;
        } else if (match$12 !== 46) {
          next(scanner);
          next(scanner);
          token = /* DotDot */5;
        } else {
          next3(scanner);
          token = /* DotDotDot */6;
        }
        break;
    case 47 :
        var match$13 = peek(scanner);
        switch (match$13) {
          case 42 :
              token = scanMultiLineComment(scanner);
              break;
          case 43 :
          case 44 :
          case 45 :
              next(scanner);
              token = /* Forwardslash */29;
              break;
          case 46 :
              next(scanner);
              next(scanner);
              token = /* ForwardslashDot */30;
              break;
          case 47 :
              next(scanner);
              next(scanner);
              token = scanSingleLineComment(scanner);
              break;
          default:
            next(scanner);
            token = /* Forwardslash */29;
        }
        break;
    case 48 :
    case 49 :
    case 50 :
    case 51 :
    case 52 :
    case 53 :
    case 54 :
    case 55 :
    case 56 :
    case 57 :
        token = scanNumber(scanner);
        break;
    case 58 :
        var match$14 = peek(scanner);
        if (match$14 !== 61) {
          if (match$14 !== 62) {
            next(scanner);
            token = /* Colon */24;
          } else {
            next(scanner);
            next(scanner);
            token = /* ColonGreaterThan */40;
          }
        } else {
          next(scanner);
          next(scanner);
          token = /* ColonEqual */74;
        }
        break;
    case 59 :
        next(scanner);
        token = /* Semicolon */8;
        break;
    case 60 :
        if (inJsxMode(scanner)) {
          next(scanner);
          skipWhitespace(scanner);
          var match$15 = scanner.ch;
          if (match$15 !== 47) {
            if (match$15 !== 61) {
              token = /* LessThan */42;
            } else {
              next(scanner);
              token = /* LessEqual */72;
            }
          } else {
            next(scanner);
            token = /* LessThanSlash */43;
          }
        } else {
          var match$16 = peek(scanner);
          if (match$16 !== 61) {
            next(scanner);
            token = /* LessThan */42;
          } else {
            next(scanner);
            next(scanner);
            token = /* LessEqual */72;
          }
        }
        break;
    case 61 :
        var match$17 = peek(scanner);
        var match$18 = peek2(scanner);
        if (match$17 !== 61) {
          if (match$17 !== 62) {
            next(scanner);
            token = /* Equal */14;
          } else {
            next(scanner);
            next(scanner);
            token = /* EqualGreater */57;
          }
        } else if (match$18 !== 61) {
          next(scanner);
          next(scanner);
          token = /* EqualEqual */15;
        } else {
          next3(scanner);
          token = /* EqualEqualEqual */16;
        }
        break;
    case 62 :
        var match$19 = peek(scanner);
        if (match$19 !== 61 || inDiamondMode(scanner)) {
          next(scanner);
          token = /* GreaterThan */41;
        } else {
          next(scanner);
          next(scanner);
          token = /* GreaterEqual */73;
        }
        break;
    case 63 :
        next(scanner);
        token = /* Question */49;
        break;
    case 64 :
        var match$20 = peek(scanner);
        if (match$20 !== 64) {
          next(scanner);
          token = /* At */75;
        } else {
          next(scanner);
          next(scanner);
          token = /* AtAt */76;
        }
        break;
    case 91 :
        next(scanner);
        token = /* Lbracket */20;
        break;
    case 92 :
        next(scanner);
        token = scanExoticIdentifier(scanner);
        break;
    case 93 :
        next(scanner);
        token = /* Rbracket */21;
        break;
    case 36 :
    case 94 :
        exit = 1;
        break;
    case 95 :
        var match$21 = peek(scanner);
        if (match$21 >= 91) {
          if (match$21 >= 97) {
            if (match$21 >= 123) {
              next(scanner);
              token = /* Underscore */12;
            } else {
              token = scanIdentifier(scanner);
            }
          } else if (match$21 !== 95) {
            next(scanner);
            token = /* Underscore */12;
          } else {
            token = scanIdentifier(scanner);
          }
        } else if (match$21 >= 58) {
          if (match$21 >= 65) {
            token = scanIdentifier(scanner);
          } else {
            next(scanner);
            token = /* Underscore */12;
          }
        } else if (match$21 >= 48) {
          token = scanIdentifier(scanner);
        } else {
          next(scanner);
          token = /* Underscore */12;
        }
        break;
    case 96 :
        next(scanner);
        token = /* Backtick */80;
        break;
    case 65 :
    case 66 :
    case 67 :
    case 68 :
    case 69 :
    case 70 :
    case 71 :
    case 72 :
    case 73 :
    case 74 :
    case 75 :
    case 76 :
    case 77 :
    case 78 :
    case 79 :
    case 80 :
    case 81 :
    case 82 :
    case 83 :
    case 84 :
    case 85 :
    case 86 :
    case 87 :
    case 88 :
    case 89 :
    case 90 :
    case 97 :
    case 98 :
    case 99 :
    case 100 :
    case 101 :
    case 102 :
    case 103 :
    case 104 :
    case 105 :
    case 106 :
    case 107 :
    case 108 :
    case 109 :
    case 110 :
    case 111 :
    case 112 :
    case 113 :
    case 114 :
    case 115 :
    case 116 :
    case 117 :
    case 118 :
    case 119 :
    case 120 :
    case 121 :
    case 122 :
        token = scanIdentifier(scanner);
        break;
    case 123 :
        next(scanner);
        token = /* Lbrace */22;
        break;
    case 124 :
        var match$22 = peek(scanner);
        if (match$22 !== 62) {
          if (match$22 !== 124) {
            next(scanner);
            token = /* Bar */17;
          } else {
            next(scanner);
            next(scanner);
            token = /* Lor */68;
          }
        } else {
          next(scanner);
          next(scanner);
          token = /* BarGreater */81;
        }
        break;
    case 125 :
        next(scanner);
        token = /* Rbrace */23;
        break;
    case 126 :
        next(scanner);
        token = /* Tilde */48;
        break;
    default:
      exit = 1;
  }
  if (exit === 1) {
    next(scanner);
    if (ch === -1) {
      token = /* Eof */26;
    } else {
      var endPos = position(scanner);
      Curry._3(scanner.err, startPos, endPos, Res_diagnostics.unknownUchar(ch));
      token = scan(scanner)[2];
    }
  }
  var endPos$1 = position(scanner);
  return [
          startPos,
          endPos$1,
          token
        ];
}

function reconsiderLessThan(scanner) {
  skipWhitespace(scanner);
  if (scanner.ch === /* '/' */47) {
    next(scanner);
    return /* LessThanSlash */43;
  } else {
    return /* LessThan */42;
  }
}

function isBinaryOp(src, startCnum, endCnum) {
  if (startCnum === 0) {
    return false;
  }
  if (endCnum < 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "res_scanner.res",
            989,
            4
          ],
          Error: new Error()
        };
  }
  if (!(startCnum > 0 && startCnum < src.length)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "res_scanner.res",
            990,
            4
          ],
          Error: new Error()
        };
  }
  var leftOk = isWhitespace(src.codePointAt(startCnum - 1 | 0));
  var rightOk = endCnum >= src.length || isWhitespace(src.codePointAt(endCnum));
  if (leftOk) {
    return rightOk;
  } else {
    return false;
  }
}

function tryAdvanceQuotedString(scanner) {
  var scanContents = function (tag) {
    while(true) {
      var ch = scanner.ch;
      if (ch !== 124) {
        if (ch === -1) {
          return ;
        }
        next(scanner);
        continue ;
      }
      next(scanner);
      var match = scanner.ch;
      if (match >= 123) {
        if (match === 125) {
          return next(scanner);
        }
        continue ;
      }
      if (match >= 97) {
        var startOff = scanner.offset;
        skipLowerCaseChars(scanner);
        var suffix = $$String.sub(scanner.src, startOff, scanner.offset - startOff | 0);
        if (tag === suffix) {
          if (scanner.ch === /* '}' */125) {
            return next(scanner);
          }
          continue ;
        }
        continue ;
      }
      continue ;
    };
  };
  var match = scanner.ch;
  if (match >= 123) {
    if (match !== 124) {
      return ;
    } else {
      return scanContents("");
    }
  }
  if (match < 97) {
    return ;
  }
  var startOff = scanner.offset;
  skipLowerCaseChars(scanner);
  var tag = $$String.sub(scanner.src, startOff, scanner.offset - startOff | 0);
  if (scanner.ch === /* '|' */124) {
    return scanContents(tag);
  }
  
}

var Diagnostics;

var Token;

var $$Comment;

var hackyEOFChar = -1;

export {
  Diagnostics ,
  Token ,
  $$Comment ,
  hackyEOFChar ,
  setDiamondMode ,
  setJsxMode ,
  popMode ,
  inDiamondMode ,
  inJsxMode ,
  position ,
  _printDebug ,
  next ,
  next2 ,
  next3 ,
  peek ,
  peek2 ,
  make ,
  isWhitespace ,
  skipWhitespace ,
  digitValue ,
  skipLowerCaseChars ,
  scanIdentifier ,
  scanDigits ,
  scanNumber ,
  scanExoticIdentifier ,
  scanStringEscapeSequence ,
  scanString ,
  scanEscape ,
  scanSingleLineComment ,
  scanMultiLineComment ,
  scanTemplateLiteralToken ,
  scan ,
  reconsiderLessThan ,
  isBinaryOp ,
  tryAdvanceQuotedString ,
}
/* P Not a pure module */
