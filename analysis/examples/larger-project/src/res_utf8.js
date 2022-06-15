// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Bytes from "rescript/lib/es6/bytes.js";

var categoryTable = [
  {
    low: -1,
    high: -1,
    size: 1
  },
  {
    low: 1,
    high: -1,
    size: 1
  },
  {
    low: 128,
    high: 191,
    size: 2
  },
  {
    low: 160,
    high: 191,
    size: 3
  },
  {
    low: 128,
    high: 191,
    size: 3
  },
  {
    low: 128,
    high: 159,
    size: 3
  },
  {
    low: 144,
    high: 191,
    size: 4
  },
  {
    low: 128,
    high: 191,
    size: 4
  },
  {
    low: 128,
    high: 143,
    size: 4
  }
];

var categories = [
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  3,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  5,
  4,
  4,
  6,
  7,
  7,
  7,
  8,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
];

function decodeCodePoint(i, s, len) {
  if (len < 1) {
    return [
            65533,
            1
          ];
  }
  var first = s.charCodeAt(i);
  if (first < 128) {
    return [
            first,
            1
          ];
  }
  var index = categories[first];
  if (index === 0) {
    return [
            65533,
            1
          ];
  }
  var cat = categoryTable[index];
  if (len < (i + cat.size | 0)) {
    return [
            65533,
            1
          ];
  }
  if (cat.size === 2) {
    var c1 = s.charCodeAt(i + 1 | 0);
    if (c1 < cat.low || cat.high < c1) {
      return [
              65533,
              1
            ];
    }
    var i1 = c1 & 63;
    var i0 = ((first & 31) << 6);
    var uc = i0 | i1;
    return [
            uc,
            2
          ];
  }
  if (cat.size === 3) {
    var c1$1 = s.charCodeAt(i + 1 | 0);
    var c2 = s.charCodeAt(i + 2 | 0);
    if (c1$1 < cat.low || cat.high < c1$1 || c2 < 128 || 191 < c2) {
      return [
              65533,
              1
            ];
    }
    var i0$1 = ((first & 15) << 12);
    var i1$1 = ((c1$1 & 63) << 6);
    var i2 = c2 & 63;
    var uc$1 = i0$1 | i1$1 | i2;
    return [
            uc$1,
            3
          ];
  }
  var c1$2 = s.charCodeAt(i + 1 | 0);
  var c2$1 = s.charCodeAt(i + 2 | 0);
  var c3 = s.charCodeAt(i + 3 | 0);
  if (c1$2 < cat.low || cat.high < c1$2 || c2$1 < 128 || 191 < c2$1 || c3 < 128 || 191 < c3) {
    return [
            65533,
            1
          ];
  }
  var i1$2 = ((c1$2 & 63) << 12);
  var i2$1 = ((c2$1 & 63) << 6);
  var i3 = c3 & 63;
  var i0$2 = ((first & 7) << 18);
  var uc$2 = i0$2 | i3 | i2$1 | i1$2;
  return [
          uc$2,
          4
        ];
}

function encodeCodePoint(c) {
  if (c <= 127) {
    var bytes = [0];
    bytes[0] = c;
    return Bytes.unsafe_to_string(bytes);
  }
  if (c <= 2047) {
    var bytes$1 = [
      0,
      0
    ];
    bytes$1[0] = 192 | (c >>> 6);
    bytes$1[1] = 128 | c & 63;
    return Bytes.unsafe_to_string(bytes$1);
  }
  if (c <= 65535) {
    var bytes$2 = [
      0,
      0,
      0
    ];
    bytes$2[0] = 224 | (c >>> 12);
    bytes$2[1] = 128 | (c >>> 6) & 63;
    bytes$2[2] = 128 | c & 63;
    return Bytes.unsafe_to_string(bytes$2);
  }
  var bytes$3 = [
    0,
    0,
    0,
    0
  ];
  bytes$3[0] = 240 | (c >>> 18);
  bytes$3[1] = 128 | (c >>> 12) & 63;
  bytes$3[2] = 128 | (c >>> 6) & 63;
  bytes$3[3] = 128 | c & 63;
  return Bytes.unsafe_to_string(bytes$3);
}

function isValidCodePoint(c) {
  if (0 <= c && c < 55296) {
    return true;
  } else if (57343 < c) {
    return c <= 1114111;
  } else {
    return false;
  }
}

var repl = 65533;

var max = 1114111;

var surrogateMin = 55296;

var surrogateMax = 57343;

var h2 = 192;

var h3 = 224;

var h4 = 240;

var cont_mask = 63;

var locb = 128;

var hicb = 191;

export {
  repl ,
  max ,
  surrogateMin ,
  surrogateMax ,
  h2 ,
  h3 ,
  h4 ,
  cont_mask ,
  locb ,
  hicb ,
  categoryTable ,
  categories ,
  decodeCodePoint ,
  encodeCodePoint ,
  isValidCodePoint ,
  
}
/* No side effect */
