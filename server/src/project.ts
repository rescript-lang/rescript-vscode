import * as LSP from "vscode-languageserver-protocol";
import * as FsHelpers from "./fs_helpers";
import * as Const from "./constants";
import * as Path from "path";

// TODO: races here?
// TODO: this doesn't handle file:/// scheme
export let rootByPath = (
	source: LSP.DocumentUri
): null | LSP.DocumentUri => {
	return FsHelpers.findPathRec(source, Const.bscPartialPath);
};

export let bscPath = (source: LSP.DocumentUri): LSP.DocumentUri => Path.join(source, Const.bscPartialPath);

export let findBscPath = (
	source: LSP.DocumentUri
): null | LSP.DocumentUri => {
	let rootPath = rootByPath(source);
	return rootPath == null ? null : bscPath(rootPath);
}

// the "build root" represents the nearest directory containing a "bsconfig.json" file.
// "bsconfig.json" can be used to locate the nearest build artifacts
export let findBuildRoot = (
	source: LSP.DocumentUri
): null | LSP.DocumentUri => {
	return FsHelpers.findPathRec(source, Const.bsconfigPartialPath);
};