// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Trace from "./trace.bs.js";
import * as React from "react";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as JSResource from "JSResource";
import * as RequireCond from "requireCond";
import * as DeadValueTest from "./DeadValueTest.bs.js";
import * as ImmutableArray from "./ImmutableArray.bs.js";
import * as BootloaderResource from "BootloaderResource";
import * as DynamicallyLoadedComponent from "./DynamicallyLoadedComponent.bs.js";

var $ExportWithRename$OR$DynamicallyLoadedComponent$RequireCond = RequireCond("gk", "chat", {
      true: "ExportWithRename.bs",
      false: "DynamicallyLoadedComponent.bs"
    });

console.log(ImmutableArray.fromArray);

var Inner = {};

var M = {};

var VariantUsedOnlyInImplementation = {
  a: /* A */0
};

var UnderscoreInside = {};

var MM = {
  x: 55,
  y: 55
};

console.log(55);

console.log(DeadValueTest.valueAlive);

function unusedRec(_param) {
  while(true) {
    _param = undefined;
    continue ;
  };
}

function split_map(l) {
  split_map(l);
  return /* [] */0;
}

function rec1(_param) {
  while(true) {
    _param = undefined;
    continue ;
  };
}

function rec2(_param) {
  while(true) {
    _param = undefined;
    continue ;
  };
}

function recWithCallback(_param) {
  while(true) {
    _param = undefined;
    continue ;
  };
}

function foo(_param) {
  while(true) {
    _param = undefined;
    continue ;
  };
}

function bar(param) {
  return foo(undefined);
}

function withDefaultValue(paramWithDefaultOpt, y) {
  var paramWithDefault = paramWithDefaultOpt !== undefined ? paramWithDefaultOpt : 3;
  return paramWithDefault + y | 0;
}

var Ext_buffer = {};

console.log(/* Root */{
      _0: "xzz"
    });

var reasonResource = JSResource("DynamicallyLoadedComponent.bs");

function makeProps(prim0, prim1, prim2) {
  var tmp = {
    s: prim0
  };
  if (prim1 !== undefined) {
    tmp.key = prim1;
  }
  return tmp;
}

function make(props) {
  return React.createElement(BootloaderResource.read(reasonResource).make, props);
}

var LazyDynamicallyLoadedComponent = {
  reasonResource: reasonResource,
  makeProps: makeProps,
  make: make
};

var reasonResource$1 = JSResource("DynamicallyLoadedComponent.bs");

function makeProps$1(prim0, prim1, prim2) {
  var tmp = {
    s: prim0
  };
  if (prim1 !== undefined) {
    tmp.key = prim1;
  }
  return tmp;
}

function make$1(props) {
  return React.createElement(BootloaderResource.read(reasonResource$1).make, props);
}

var LazyDynamicallyLoadedComponent2 = {
  reasonResource: reasonResource$1,
  makeProps: makeProps$1,
  make: make$1
};

var cmp = React.createElement(make, {
      s: "hello"
    });

function cmp2(param) {
  return React.createElement(make$1, {
              s: "hello"
            });
}

console.log(cmp);

var Chat = {};

console.log(React.createElement(DynamicallyLoadedComponent.make, {
          s: ""
        }));

var second = Caml_int64.one;

var minute = Caml_int64.mul([
      0,
      60
    ], second);

var deadRef = {
  contents: 12
};

function DeadTest(Props) {
  return Props.s;
}

console.log(DeadTest);

console.log(123);

Trace.infok("", "", (function (param) {
        return Curry._2(param.pf, /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    },
                    _1: "%s"
                  }, "");
      }));

var GloobLive = {
  globallyLive1: 1,
  globallyLive2: 2,
  globallyLive3: 3
};

var WithInclude = {};

console.log(/* A */0);

var fortytwo = 42;

var fortyTwoButExported = 42;

var thisIsUsedOnce = 34;

var thisIsUsedTwice = 34;

var thisIsKeptAlive = 42;

var thisIsMarkedLive = 42;

var ComponentSwitch = $ExportWithRename$OR$DynamicallyLoadedComponent$RequireCond;

var zzz;

var makeSwitch = $ExportWithRename$OR$DynamicallyLoadedComponent$RequireCond.make;

var make$2 = DeadTest;

var theSideEffectIsLogging;

var stringLengthNoSideEffects = 5;

export {
  $ExportWithRename$OR$DynamicallyLoadedComponent$RequireCond ,
  fortytwo ,
  fortyTwoButExported ,
  thisIsUsedOnce ,
  thisIsUsedTwice ,
  thisIsKeptAlive ,
  thisIsMarkedLive ,
  Inner ,
  M ,
  VariantUsedOnlyInImplementation ,
  UnderscoreInside ,
  MM ,
  unusedRec ,
  split_map ,
  rec1 ,
  rec2 ,
  recWithCallback ,
  foo ,
  bar ,
  withDefaultValue ,
  Ext_buffer ,
  LazyDynamicallyLoadedComponent ,
  LazyDynamicallyLoadedComponent2 ,
  cmp ,
  cmp2 ,
  Chat ,
  ComponentSwitch ,
  zzz ,
  second ,
  minute ,
  deadRef ,
  makeSwitch ,
  make$2 as make,
  theSideEffectIsLogging ,
  stringLengthNoSideEffects ,
  GloobLive ,
  WithInclude ,
  
}
/* $ExportWithRename$OR$DynamicallyLoadedComponent$RequireCond Not a pure module */