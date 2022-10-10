// This file has been generated from https://raw.githubusercontent.com/rescript-lang/rescript-compiler/master/docs/docson/build-schema.json
// with https://app.quicktype.io/

// To parse this data:
//
//   import { Convert, BuildSchema } from "./file";
//
//   const buildSchema = Convert.toBuildSchema(json);

/**
 * All paths are required to be in **Unix format** (foo/bar), the build system normalizes
 * them for other platforms internally
 */
export interface BuildSchema {
  /**
   * ReScript dependencies of the library, like in package.json. Currently searches in
   * `node_modules`
   */
  "bs-dependencies"?: string[];
  /**
   * ReScript dev dependencies of the library, like in package.json. Currently searches in
   * `node_modules`
   */
  "bs-dev-dependencies"?: string[];
  /**
   * (Not needed usually) external include directories, which will be applied `-I` to all
   * compilation units
   */
  "bs-external-includes"?: string[];
  /**
   * Flags passed to bsc.exe
   */
  "bsc-flags"?: string[] | BscFlagsObject;
  /**
   * Ignore generators, cut the dependency on generator tools
   */
  "cut-generators"?: boolean;
  /**
   * (internal) Used by bsb to build to different targets: native (ocamlopt), bytecode
   * (ocamlc) or JS (bsc)
   */
  entries?: TargetItems[];
  /**
   * Use the external stdlib library instead of the one shipped with the compiler package
   */
  "external-stdlib"?: string;
  /**
   * Whether to generate the `.merlin` file for [Merlin](https://github.com/ocaml/merlin).
   * Default: true
   */
  "generate-merlin"?: boolean;
  /**
   * (WIP) Pre defined rules
   */
  generators?: RuleGenerator[];
  /**
   * gentype config, see cristianoc/genType for more details
   */
  gentypeconfig?: GentypeSpecs;
  /**
   * a list of directories that bsb will not look into
   */
  "ignored-dirs"?: string[];
  /**
   * (Experimental) post-processing hook. bsb will invoke `cmd ${file}` whenever a `${file}`
   * is changed
   */
  "js-post-build"?: JSPostBuild;
  /**
   * Configuration for the JSX transformation.
   */
  jsx?: JsxSpecs;
  /**
   * Package name
   */
  name: string;
  /**
   * can be true/false or a customized name
   */
  namespace?: boolean | string;
  /**
   * ReScript can currently output to [Commonjs](https://en.wikipedia.org/wiki/CommonJS), and
   * [ES6
   * modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import)
   */
  "package-specs"?:
    | Array<ModuleFormat | ModuleFormatObject>
    | ModuleFormat
    | ModuleFormatObject;
  /**
   * Those dependencies are pinned (since version 8.4)
   */
  "pinned-dependencies"?: string[];
  /**
   * preprocessors to pass to compiler. The syntax is package_name/binary, for example:
   * `pp/syntax.exe`. Currenly searches in `node_modules`
   */
  "pp-flags"?: string;
  /**
   * PPX macros to pass to compiler. The syntax is package_name/binary, for example:
   * `reason/reactjs_jsx_ppx_3.native`. Currenly searches in `node_modules`
   */
  "ppx-flags"?: Array<string[] | string>;
  /**
   * Configure reanalyze, a static code analysis tool for ReScript.
   */
  reanalyze?: Reanalyze;
  /**
   * ReScript comes with [Reason](http://reasonml.github.io/) by default. Specific
   * configurations here.
   */
  reason?: ReasonSpecs;
  /**
   * Source code location
   */
  sources: Array<SourcesObject | string> | SourcesObject | string;
  suffix?: SuffixSpec;
  /**
   * (Experimental) whether to use the OCaml standard library. Default: true
   */
  "use-stdlib"?: boolean;
  /**
   * The semantic version of the ReScript library
   */
  version?: string;
  /**
   * warning numbers and whether to turn it into error or not
   */
  warnings?: Warnings;
}

/**
 * (Not implemented yet)
 */
export interface BscFlagsObject {
  flags?: string[];
  kind?: BscFlagsKind;
}

export enum BscFlagsKind {
  Append = "append",
  Prefix = "prefix",
  Reset = "reset",
}

/**
 * (internal) Used by bsb to build to different targets: native (ocamlopt), bytecode
 * (ocamlc) or JS (bsc)
 *
 * A list of buildable targets
 */
export interface TargetItems {
  /**
   * The compiler to use for the target
   */
  kind?: EntryKind;
  /**
   * Name of the main module used as entry point for this target. 'entry-point' isn't used
   * when this project is built as a dependency.
   */
  main?: string;
}

/**
 * The compiler to use for the target
 */
export enum EntryKind {
  Bytecode = "bytecode",
  JS = "js",
  Native = "native",
}

/**
 * The shell command is running in *dev* time, and you generated could should be checked in,
 * the depedency is tracked properly during dev time,example: `{ "name" : "ocamllex",
 * "command" : "ocamllex.opt $in -o $out"}`
 */
export interface RuleGenerator {
  command?: string;
  name?: string;
}

/**
 * gentype config, see cristianoc/genType for more details
 *
 * path to gentype, path resolution is similar to ReScript
 */
export interface GentypeSpecs {
  path?: string;
}

/**
 * (Experimental) post-processing hook. bsb will invoke `cmd ${file}` whenever a `${file}`
 * is changed
 */
export interface JSPostBuild {
  cmd?: string;
}

/**
 * Configuration for the JSX transformation.
 */
export interface JsxSpecs {
  /**
   * JSX transformation mode
   */
  mode?: Mode;
  /**
   * JSX module, currently only support the React.
   */
  module?: Module;
  /**
   * Build the given dependencies in JSX V3 compatibility mode.
   */
  "v3-dependencies"?: string[];
  /**
   * Whether to apply the specific version of JSX PPX transformation
   */
  version: number;
}

/**
 * JSX transformation mode
 */
export enum Mode {
  Automatic = "automatic",
  Classic = "classic",
}

/**
 * JSX module, currently only support the React.
 */
export enum Module {
  React = "react",
}

/**
 * es6-global generate relative `require` paths instead of relying on NodeJS' module
 * resolution. Default: commonjs.
 */
export enum ModuleFormat {
  Commonjs = "commonjs",
  Es6 = "es6",
  Es6Global = "es6-global",
}

export interface ModuleFormatObject {
  /**
   * Default: false.
   */
  "in-source"?: boolean;
  module: ModuleFormat;
  suffix?: SuffixSpec;
}

/**
 * suffix of generated js files, default to [.js]
 */
export enum SuffixSpec {
  BsCjs = ".bs.cjs",
  BsJS = ".bs.js",
  BsMjs = ".bs.mjs",
  Cjs = ".cjs",
  JS = ".js",
  Mjs = ".mjs",
}

/**
 * Configure reanalyze, a static code analysis tool for ReScript.
 */
export interface Reanalyze {
  /**
   * The types of analysis to activate. `dce` means dead code analysis, `exception` means
   * exception analysis, and `termination` is to check for infinite loops.
   */
  analysis?: Analysis[];
  /**
   * Paths for any folders you'd like to exclude from analysis. Useful for bindings and
   * similar. Example: `["src/bindings"]`.
   */
  suppress?: string[];
  /**
   * Any specific paths inside suppressed folders that you want to unsuppress. Example:
   * ["src/bindings/SomeBinding.res"].
   */
  unsuppress?: string[];
}

export enum Analysis {
  Dce = "dce",
  Exception = "exception",
  Termination = "termination",
}

/**
 * ReScript comes with [Reason](http://reasonml.github.io/) by default. Specific
 * configurations here.
 */
export interface ReasonSpecs {
  /**
   * Whether to apply the
   * [RescriptReact](https://github.com/rescript-lang/rescript-react)-specific JSX PPX
   * transformation.
   */
  "react-jsx"?: number;
}

export interface SourcesObject {
  /**
   * name of the directory
   */
  dir: string;
  files?: string[] | FilesObject;
  /**
   * (WIP) Files generated in dev time
   */
  generators?: BuildGenerator[];
  /**
   * Not implemented yet
   */
  group?: GroupObject | string;
  "internal-depends"?: string[];
  /**
   * Default: export all modules. It is recommended for library developers to hide some
   * files/interfaces
   */
  public?: string[] | PublicEnum;
  resources?: string[];
  subdirs?: Array<SourcesObject | string> | boolean | SourcesObject | string;
  type?: Type;
}

export interface FilesObject {
  /**
   * Files to be excluded
   */
  excludes?: string[];
  /**
   * Regex to glob the patterns, syntax is documented
   * [here](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html), for better
   * incremental build performance, we'd suggest listing files explicitly
   */
  "slow-re"?: string;
}

/**
 * Note that we will add the directory path accordingly
 */
export interface BuildGenerator {
  edge?: string[];
  name?: string;
}

export interface GroupObject {
  /**
   * When true, all subdirs are considered as a whole as dependency
   */
  hierachy?: boolean;
  name?: string;
}

export enum PublicEnum {
  All = "all",
}

export enum Type {
  Dev = "dev",
}

/**
 * warning numbers and whether to turn it into error or not
 */
export interface Warnings {
  error?: boolean | string;
  /**
   * Default: -40+6+7+27+32..39+44+45
   * [Here](https://caml.inria.fr/pub/docs/manual-ocaml/comp.html#sec270) for the meanings of
   * the warning flags
   */
  number?: string;
}

// Converts JSON strings to/from your types
export class Convert {
  public static toBuildSchema(json: string): BuildSchema {
    return JSON.parse(json);
  }

  public static buildSchemaToJson(value: BuildSchema): string {
    return JSON.stringify(value);
  }
}
