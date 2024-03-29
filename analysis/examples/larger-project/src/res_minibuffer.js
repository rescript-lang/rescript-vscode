// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Sys from "rescript/lib/es6/sys.js";
import * as Bytes from "rescript/lib/es6/bytes.js";
import * as Caml_bytes from "rescript/lib/es6/caml_bytes.js";

function create(n) {
  var n$1 = n < 1 ? 1 : n;
  var s = Caml_bytes.caml_create_bytes(n$1);
  return {
          buffer: s,
          position: 0,
          length: n$1
        };
}

function contents(b) {
  return Bytes.sub_string(b.buffer, 0, b.position);
}

function resize_internal(b, more) {
  var len = b.length;
  var new_len = len;
  while((b.position + more | 0) > new_len) {
    new_len = (new_len << 1);
  };
  if (new_len > Sys.max_string_length && (b.position + more | 0) <= Sys.max_string_length) {
    new_len = Sys.max_string_length;
  }
  var new_buffer = Caml_bytes.caml_create_bytes(new_len);
  Bytes.blit(b.buffer, 0, new_buffer, 0, b.position);
  b.buffer = new_buffer;
  b.length = new_len;
  
}

function add_char(b, c) {
  var pos = b.position;
  if (pos >= b.length) {
    resize_internal(b, 1);
  }
  b.buffer[pos] = c;
  b.position = pos + 1 | 0;
  
}

function add_string(b, s) {
  var len = s.length;
  var new_position = b.position + len | 0;
  if (new_position > b.length) {
    resize_internal(b, len);
  }
  Bytes.blit_string(s, 0, b.buffer, b.position, len);
  b.position = new_position;
  
}

function flush_newline(b) {
  var position = b.position;
  while(b.buffer[position - 1 | 0] === /* ' ' */32 && position >= 0) {
    position = position - 1 | 0;
  };
  b.position = position;
  return add_char(b, /* '\n' */10);
}

export {
  create ,
  contents ,
  resize_internal ,
  add_char ,
  add_string ,
  flush_newline ,
  
}
/* No side effect */
