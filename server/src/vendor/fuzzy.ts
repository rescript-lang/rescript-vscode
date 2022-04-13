// @ts-nocheck
// NOTE: Taken and inlined from https://github.com/mattyork/fuzzy/

/*
 * Fuzzy
 * https://github.com/myork/fuzzy
 *
 * Copyright (c) 2012 Matt York
 * Licensed under the MIT license.
 */

/**
 * Return all elements of `array` that have a fuzzy match against `pattern`.
 */
export declare function simpleFilter(
  pattern: string,
  array: string[]
): string[];

/**
 * Does `pattern` fuzzy match `inputString`?
 */
export declare function test(pattern: string, inputString: string): boolean;

export interface MatchOptions {
  pre?: string;
  post?: string;
  caseSensitive?: boolean;
}

export interface MatchResult {
  rendered: string;
  score: number;
}

/**
 * If `pattern` matches `inputString`, wrap each matching character in `opts.pre`
 * and `opts.post`. If no match, return null.
 */
export declare function match(
  pattern: string,
  inputString: string,
  opts?: MatchOptions
): MatchResult;

export interface FilterOptions<T> {
  pre?: string;
  post?: string;
  extract?(input: T): string;
}

export interface FilterResult<T> {
  string: string;
  score: number;
  index: number;
  original: T;
}

/**
 * The normal entry point. Filters `arr` for matches against `pattern`.
 */
export declare function filter<T>(
  pattern: string,
  arr: T[],
  opts?: FilterOptions<T>
): FilterResult<T>[];

// Return all elements of `array` that have a fuzzy
// match against `pattern`.
export const simpleFilter = function (pattern, array) {
  return array.filter(function (str) {
    return test(pattern, str);
  });
};

// Does `pattern` fuzzy match `str`?
export const test = function (pattern, str) {
  return match(pattern, str) !== null;
};

// If `pattern` matches `str`, wrap each matching character
// in `opts.pre` and `opts.post`. If no match, return null
export const match = function (pattern, str, opts) {
  opts = opts || {};
  var patternIdx = 0,
    result = [],
    len = str.length,
    totalScore = 0,
    currScore = 0,
    // prefix
    pre = opts.pre || "",
    // suffix
    post = opts.post || "",
    // String to compare against. This might be a lowercase version of the
    // raw string
    compareString = (opts.caseSensitive && str) || str.toLowerCase(),
    ch;

  pattern = (opts.caseSensitive && pattern) || pattern.toLowerCase();

  // For each character in the string, either add it to the result
  // or wrap in template if it's the next string in the pattern
  for (var idx = 0; idx < len; idx++) {
    ch = str[idx];
    if (compareString[idx] === pattern[patternIdx]) {
      ch = pre + ch + post;
      patternIdx += 1;

      // consecutive characters should increase the score more than linearly
      currScore += 1 + currScore;
    } else {
      currScore = 0;
    }
    totalScore += currScore;
    result[result.length] = ch;
  }

  // return rendered string if we have a match for every char
  if (patternIdx === pattern.length) {
    // if the string is an exact match with pattern, totalScore should be maxed
    totalScore = compareString === pattern ? Infinity : totalScore;
    return { rendered: result.join(""), score: totalScore };
  }

  return null;
};

// The normal entry point. Filters `arr` for matches against `pattern`.
// It returns an array with matching values of the type:
//
//     [{
//         string:   '<b>lah' // The rendered string
//       , index:    2        // The index of the element in `arr`
//       , original: 'blah'   // The original element in `arr`
//     }]
//
// `opts` is an optional argument bag. Details:
//
//    opts = {
//        // string to put before a matching character
//        pre:     '<b>'
//
//        // string to put after matching character
//      , post:    '</b>'
//
//        // Optional function. Input is an entry in the given arr`,
//        // output should be the string to test `pattern` against.
//        // In this example, if `arr = [{crying: 'koala'}]` we would return
//        // 'koala'.
//      , extract: function(arg) { return arg.crying; }
//    }
export const filter = function (pattern, arr, opts) {
  if (!arr || arr.length === 0) {
    return [];
  }
  if (typeof pattern !== "string") {
    return arr;
  }
  opts = opts || {};
  return (
    arr
      .reduce(function (prev, element, idx, arr) {
        var str = element;
        if (opts.extract) {
          str = opts.extract(element);
        }
        var rendered = match(pattern, str, opts);
        if (rendered != null) {
          prev[prev.length] = {
            string: rendered.rendered,
            score: rendered.score,
            index: idx,
            original: element,
          };
        }
        return prev;
      }, [])

      // Sort by score. Browsers are inconsistent wrt stable/unstable
      // sorting, so force stable by using the index in the case of tie.
      // See http://ofb.net/~sethml/is-sort-stable.html
      .sort(function (a, b) {
        var compare = b.score - a.score;
        if (compare) return compare;
        return a.index - b.index;
      })
  );
};
