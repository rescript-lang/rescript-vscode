// This file has been generated from https://raw.githubusercontent.com/rescript-lang/rescript-compiler/master/docs/docson/build-schema.json
// with https://app.quicktype.io/ and stripped down to what we actually need for the extension.

export interface BuildSchema {
  name: string;
  namespace?: boolean | string;
  "package-specs"?:
    | Array<ModuleFormat | ModuleFormatObject>
    | ModuleFormat
    | ModuleFormatObject;
  suffix?: SuffixSpec;
}

export enum ModuleFormat {
  Commonjs = "commonjs",
  Es6 = "es6",
  Es6Global = "es6-global",
  Esmodule = "esmodule",
}

export interface ModuleFormatObject {
  "in-source"?: boolean;
  module: ModuleFormat;
  suffix?: SuffixSpec;
}

export enum SuffixSpec {
  BsCjs = ".bs.cjs",
  BsJS = ".bs.js",
  BsMjs = ".bs.mjs",
  Cjs = ".cjs",
  JS = ".js",
  Mjs = ".mjs",
}
