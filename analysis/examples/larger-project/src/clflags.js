// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Arg from "rescript/lib/es6/arg.js";
import * as Sys from "rescript/lib/es6/sys.js";
import * as List from "rescript/lib/es6/list.js";
import * as Misc from "./misc.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Config from "./config.js";
import * as Printf from "./printf.js";
import * as Filename from "rescript/lib/es6/filename.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

var Int_arg_helper = {};

var Float_arg_helper = {};

var objfiles = {
  contents: /* [] */0
};

var ccobjs = {
  contents: /* [] */0
};

var dllibs = {
  contents: /* [] */0
};

var compile_only = {
  contents: false
};

var output_name = {
  contents: undefined
};

var include_dirs = {
  contents: /* [] */0
};

var no_std_include = {
  contents: false
};

var print_types = {
  contents: false
};

var make_archive = {
  contents: false
};

var debug = {
  contents: false
};

var fast = {
  contents: false
};

var use_linscan = {
  contents: false
};

var link_everything = {
  contents: false
};

var custom_runtime = {
  contents: false
};

var no_check_prims = {
  contents: false
};

var bytecode_compatible_32 = {
  contents: false
};

var output_c_object = {
  contents: false
};

var output_complete_object = {
  contents: false
};

var all_ccopts = {
  contents: /* [] */0
};

var classic = {
  contents: false
};

var nopervasives = {
  contents: false
};

var preprocessor = {
  contents: undefined
};

var all_ppx = {
  contents: /* [] */0
};

var annotations = {
  contents: false
};

var binary_annotations = {
  contents: false
};

var use_threads = {
  contents: false
};

var use_vmthreads = {
  contents: false
};

var noassert = {
  contents: false
};

var verbose = {
  contents: false
};

var noversion = {
  contents: false
};

var noprompt = {
  contents: false
};

var nopromptcont = {
  contents: false
};

var init_file = {
  contents: undefined
};

var noinit = {
  contents: false
};

var open_modules = {
  contents: /* [] */0
};

var use_prims = {
  contents: ""
};

var use_runtime = {
  contents: ""
};

var principal = {
  contents: false
};

var real_paths = {
  contents: true
};

var recursive_types = {
  contents: false
};

var strict_sequence = {
  contents: false
};

var strict_formats = {
  contents: false
};

var applicative_functors = {
  contents: true
};

var make_runtime = {
  contents: false
};

var gprofile = {
  contents: false
};

var c_compiler = {
  contents: undefined
};

var no_auto_link = {
  contents: false
};

var dllpaths = {
  contents: /* [] */0
};

var make_package = {
  contents: false
};

var for_package = {
  contents: undefined
};

var error_size = {
  contents: 500
};

var float_const_prop = {
  contents: true
};

var transparent_modules = {
  contents: false
};

var dump_source = {
  contents: false
};

var dump_parsetree = {
  contents: false
};

var dump_typedtree = {
  contents: false
};

var dump_rawlambda = {
  contents: false
};

var dump_lambda = {
  contents: false
};

var dump_rawclambda = {
  contents: false
};

var dump_clambda = {
  contents: false
};

var dump_rawflambda = {
  contents: false
};

var dump_flambda = {
  contents: false
};

var dump_flambda_let = {
  contents: undefined
};

var dump_flambda_verbose = {
  contents: false
};

var dump_instr = {
  contents: false
};

var keep_asm_file = {
  contents: false
};

var optimize_for_speed = {
  contents: true
};

var opaque = {
  contents: false
};

var dump_cmm = {
  contents: false
};

var dump_selection = {
  contents: false
};

var dump_cse = {
  contents: false
};

var dump_live = {
  contents: false
};

var dump_avail = {
  contents: false
};

var dump_spill = {
  contents: false
};

var dump_split = {
  contents: false
};

var dump_interf = {
  contents: false
};

var dump_prefer = {
  contents: false
};

var dump_regalloc = {
  contents: false
};

var dump_reload = {
  contents: false
};

var dump_scheduling = {
  contents: false
};

var dump_linear = {
  contents: false
};

var dump_interval = {
  contents: false
};

var keep_startup_file = {
  contents: false
};

var dump_combine = {
  contents: false
};

var debug_runavail = {
  contents: false
};

var native_code = {
  contents: false
};

var force_slash = {
  contents: false
};

var clambda_checks = {
  contents: false
};

var flambda_invariant_checks = {
  contents: true
};

var dont_write_files = {
  contents: false
};

function std_include_flag(prefix) {
  if (no_std_include.contents) {
    return "";
  } else {
    return prefix + Curry._1(Filename.quote, Config.standard_library);
  }
}

function std_include_dir(param) {
  if (no_std_include.contents) {
    return /* [] */0;
  } else {
    return {
            hd: Config.standard_library,
            tl: /* [] */0
          };
  }
}

var shared = {
  contents: false
};

var dlcode = {
  contents: true
};

var tmp = Config.architecture === "amd64" ? true : false;

var pic_code = {
  contents: tmp
};

var runtime_variant = {
  contents: ""
};

var keep_docs = {
  contents: false
};

var keep_locs = {
  contents: true
};

var unsafe_string = {
  contents: false
};

var classic_inlining = {
  contents: false
};

var inlining_report = {
  contents: false
};

var afl_instrument = {
  contents: false
};

var afl_inst_ratio = {
  contents: 100
};

var simplify_rounds = {
  contents: undefined
};

var default_simplify_rounds = {
  contents: 1
};

function rounds(param) {
  var r = simplify_rounds.contents;
  if (r !== undefined) {
    return r;
  } else {
    return default_simplify_rounds.contents;
  }
}

var default_inline_threshold = 10 / 8;

var default_inline_toplevel_threshold = 16 * default_inline_threshold | 0;

var unbox_specialised_args = {
  contents: true
};

var unbox_free_vars_of_closures = {
  contents: true
};

var unbox_closures = {
  contents: false
};

var unbox_closures_factor = {
  contents: 10
};

var remove_unused_arguments = {
  contents: false
};

var classic_arguments_inline_threshold = 10 / 8;

var classic_arguments_inline_toplevel_threshold = 1;

var classic_arguments = {
  inline_call_cost: undefined,
  inline_alloc_cost: undefined,
  inline_prim_cost: undefined,
  inline_branch_cost: undefined,
  inline_indirect_cost: undefined,
  inline_lifting_benefit: undefined,
  inline_branch_factor: undefined,
  inline_max_depth: undefined,
  inline_max_unroll: undefined,
  inline_threshold: classic_arguments_inline_threshold,
  inline_toplevel_threshold: classic_arguments_inline_toplevel_threshold
};

var o2_arguments_inline_call_cost = 10;

var o2_arguments_inline_alloc_cost = 14;

var o2_arguments_inline_prim_cost = 6;

var o2_arguments_inline_branch_cost = 10;

var o2_arguments_inline_indirect_cost = 8;

var o2_arguments_inline_max_depth = 2;

var o2_arguments_inline_threshold = 25;

var o2_arguments_inline_toplevel_threshold = 400;

var o2_arguments = {
  inline_call_cost: o2_arguments_inline_call_cost,
  inline_alloc_cost: o2_arguments_inline_alloc_cost,
  inline_prim_cost: o2_arguments_inline_prim_cost,
  inline_branch_cost: o2_arguments_inline_branch_cost,
  inline_indirect_cost: o2_arguments_inline_indirect_cost,
  inline_lifting_benefit: undefined,
  inline_branch_factor: undefined,
  inline_max_depth: o2_arguments_inline_max_depth,
  inline_max_unroll: undefined,
  inline_threshold: o2_arguments_inline_threshold,
  inline_toplevel_threshold: o2_arguments_inline_toplevel_threshold
};

var o3_arguments_inline_call_cost = 15;

var o3_arguments_inline_alloc_cost = 21;

var o3_arguments_inline_prim_cost = 9;

var o3_arguments_inline_branch_cost = 15;

var o3_arguments_inline_indirect_cost = 12;

var o3_arguments_inline_branch_factor = 0;

var o3_arguments_inline_max_depth = 3;

var o3_arguments_inline_max_unroll = 1;

var o3_arguments_inline_threshold = 50;

var o3_arguments_inline_toplevel_threshold = 800;

var o3_arguments = {
  inline_call_cost: o3_arguments_inline_call_cost,
  inline_alloc_cost: o3_arguments_inline_alloc_cost,
  inline_prim_cost: o3_arguments_inline_prim_cost,
  inline_branch_cost: o3_arguments_inline_branch_cost,
  inline_indirect_cost: o3_arguments_inline_indirect_cost,
  inline_lifting_benefit: undefined,
  inline_branch_factor: o3_arguments_inline_branch_factor,
  inline_max_depth: o3_arguments_inline_max_depth,
  inline_max_unroll: o3_arguments_inline_max_unroll,
  inline_threshold: o3_arguments_inline_threshold,
  inline_toplevel_threshold: o3_arguments_inline_toplevel_threshold
};

var all_passes = {
  contents: /* [] */0
};

var dumped_passes_list = {
  contents: /* [] */0
};

function dumped_pass(s) {
  if (!List.mem(s, all_passes.contents)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "clflags.res",
            276,
            2
          ],
          Error: new Error()
        };
  }
  return List.mem(s, dumped_passes_list.contents);
}

function set_dumped_pass(s, enabled) {
  if (!List.mem(s, all_passes.contents)) {
    return ;
  }
  var passes_without_s = List.filter(function (param) {
          return s !== param;
        })(dumped_passes_list.contents);
  var dumped_passes = enabled ? ({
        hd: s,
        tl: passes_without_s
      }) : passes_without_s;
  dumped_passes_list.contents = dumped_passes;
  
}

function parse_color_setting(x) {
  switch (x) {
    case "always" :
        return /* Always */1;
    case "auto" :
        return /* Auto */0;
    case "never" :
        return /* Never */2;
    default:
      return ;
  }
}

var color = {
  contents: undefined
};

var unboxed_types = {
  contents: false
};

var arg_spec = {
  contents: /* [] */0
};

var arg_names = {
  contents: Misc.StringMap.empty
};

function reset_arguments(param) {
  arg_spec.contents = /* [] */0;
  arg_names.contents = Misc.StringMap.empty;
  
}

function add_arguments(loc, args) {
  return List.iter((function (x) {
                var arg_name = x[0];
                try {
                  var loc2 = Curry._2(Misc.StringMap.find, arg_name, arg_names.contents);
                  Curry._1(Printf.eprintf("Warning: plugin argument %s is already defined:\n"), arg_name);
                  Curry._1(Printf.eprintf("   First definition: %s\n"), loc2);
                  return Curry._1(Printf.eprintf("   New definition: %s\n"), loc);
                }
                catch (raw_exn){
                  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                  if (exn.RE_EXN_ID === "Not_found") {
                    arg_spec.contents = Pervasives.$at(arg_spec.contents, {
                          hd: x,
                          tl: /* [] */0
                        });
                    arg_names.contents = Curry._3(Misc.StringMap.add, arg_name, loc, arg_names.contents);
                    return ;
                  }
                  throw exn;
                }
              }), args);
}

function print_arguments(usage) {
  return Arg.usage(arg_spec.contents, usage);
}

function parse_arguments(f, msg) {
  try {
    var argv = {
      contents: Sys.argv
    };
    var current = {
      contents: Arg.current.contents
    };
    return Arg.parse_and_expand_argv_dynamic(current, argv, arg_spec, f, msg);
  }
  catch (raw_msg){
    var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg);
    if (msg$1.RE_EXN_ID === Arg.Bad) {
      Curry._1(Printf.eprintf("%s"), msg$1._1);
      return Pervasives.exit(2);
    }
    if (msg$1.RE_EXN_ID === Arg.Help) {
      Curry._1(Printf.printf("%s"), msg$1._1);
      return Pervasives.exit(0);
    }
    throw msg$1;
  }
}

var inline_toplevel_multiplier = 16;

var default_inline_call_cost = 5;

var default_inline_alloc_cost = 7;

var default_inline_prim_cost = 3;

var default_inline_branch_cost = 5;

var default_inline_indirect_cost = 4;

var default_inline_branch_factor = 0.1;

var default_inline_lifting_benefit = 1300;

var default_inline_max_unroll = 0;

var default_inline_max_depth = 1;

var default_unbox_closures_factor = 10;

var o1_arguments = {
  inline_call_cost: undefined,
  inline_alloc_cost: undefined,
  inline_prim_cost: undefined,
  inline_branch_cost: undefined,
  inline_indirect_cost: undefined,
  inline_lifting_benefit: undefined,
  inline_branch_factor: undefined,
  inline_max_depth: undefined,
  inline_max_unroll: undefined,
  inline_threshold: undefined,
  inline_toplevel_threshold: undefined
};

export {
  Int_arg_helper ,
  Float_arg_helper ,
  objfiles ,
  ccobjs ,
  dllibs ,
  compile_only ,
  output_name ,
  include_dirs ,
  no_std_include ,
  print_types ,
  make_archive ,
  debug ,
  fast ,
  use_linscan ,
  link_everything ,
  custom_runtime ,
  no_check_prims ,
  bytecode_compatible_32 ,
  output_c_object ,
  output_complete_object ,
  all_ccopts ,
  classic ,
  nopervasives ,
  preprocessor ,
  all_ppx ,
  annotations ,
  binary_annotations ,
  use_threads ,
  use_vmthreads ,
  noassert ,
  verbose ,
  noversion ,
  noprompt ,
  nopromptcont ,
  init_file ,
  noinit ,
  open_modules ,
  use_prims ,
  use_runtime ,
  principal ,
  real_paths ,
  recursive_types ,
  strict_sequence ,
  strict_formats ,
  applicative_functors ,
  make_runtime ,
  gprofile ,
  c_compiler ,
  no_auto_link ,
  dllpaths ,
  make_package ,
  for_package ,
  error_size ,
  float_const_prop ,
  transparent_modules ,
  dump_source ,
  dump_parsetree ,
  dump_typedtree ,
  dump_rawlambda ,
  dump_lambda ,
  dump_rawclambda ,
  dump_clambda ,
  dump_rawflambda ,
  dump_flambda ,
  dump_flambda_let ,
  dump_flambda_verbose ,
  dump_instr ,
  keep_asm_file ,
  optimize_for_speed ,
  opaque ,
  dump_cmm ,
  dump_selection ,
  dump_cse ,
  dump_live ,
  dump_avail ,
  dump_spill ,
  dump_split ,
  dump_interf ,
  dump_prefer ,
  dump_regalloc ,
  dump_reload ,
  dump_scheduling ,
  dump_linear ,
  dump_interval ,
  keep_startup_file ,
  dump_combine ,
  debug_runavail ,
  native_code ,
  force_slash ,
  clambda_checks ,
  flambda_invariant_checks ,
  dont_write_files ,
  std_include_flag ,
  std_include_dir ,
  shared ,
  dlcode ,
  pic_code ,
  runtime_variant ,
  keep_docs ,
  keep_locs ,
  unsafe_string ,
  classic_inlining ,
  inlining_report ,
  afl_instrument ,
  afl_inst_ratio ,
  simplify_rounds ,
  default_simplify_rounds ,
  rounds ,
  default_inline_threshold ,
  inline_toplevel_multiplier ,
  default_inline_toplevel_threshold ,
  default_inline_call_cost ,
  default_inline_alloc_cost ,
  default_inline_prim_cost ,
  default_inline_branch_cost ,
  default_inline_indirect_cost ,
  default_inline_branch_factor ,
  default_inline_lifting_benefit ,
  default_inline_max_unroll ,
  default_inline_max_depth ,
  unbox_specialised_args ,
  unbox_free_vars_of_closures ,
  unbox_closures ,
  default_unbox_closures_factor ,
  unbox_closures_factor ,
  remove_unused_arguments ,
  o1_arguments ,
  classic_arguments ,
  o2_arguments ,
  o3_arguments ,
  all_passes ,
  dumped_passes_list ,
  dumped_pass ,
  set_dumped_pass ,
  parse_color_setting ,
  color ,
  unboxed_types ,
  arg_spec ,
  arg_names ,
  reset_arguments ,
  add_arguments ,
  print_arguments ,
  parse_arguments ,
  
}
/* pic_code Not a pure module */
