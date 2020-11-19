import { Range } from "vscode-languageserver-textdocument";
import * as c from "./constants";
import * as childProcess from "child_process";
import * as p from "vscode-languageserver-protocol";
import * as path from "path";
import * as t from "vscode-languageserver-types";
import fs from "fs";
import * as os from "os";

let tempFilePrefix = "rescript_format_file_" + process.pid + "_";
let tempFileId = 0;

export let createFileInTempDir = (extension = "") => {
	let tempFileName = tempFilePrefix + tempFileId + extension;
	tempFileId = tempFileId + 1;
	return path.join(os.tmpdir(), tempFileName);
};

// TODO: races here?
// TODO: this doesn't handle file:/// scheme
export let findProjectRootOfFile = (
	source: p.DocumentUri
): null | p.DocumentUri => {
	let dir = path.dirname(source);
	if (fs.existsSync(path.join(dir, c.bscPartialPath))) {
		return dir;
	} else {
		if (dir === source) {
			// reached top
			return null;
		} else {
			return findProjectRootOfFile(dir);
		}
	}
};

// the "build root" represents the nearest directory containing a "bsconfig.json" file.
// "bsconfig.json" can be used to locate the nearest build artefacts
export let findBuildRootOfFile = (
	source: p.DocumentUri
): null | p.DocumentUri => {
	let dir = path.dirname(source);
	if (fs.existsSync(path.join(dir, c.bsconfigPartialPath))) {
		return dir;
	} else {
		if (dir === source) {
			// reached top
			return null;
		} else {
			return findBuildRootOfFile(dir);
		}
	}
};

type execResult =
	| {
		kind: "success";
		result: string;
	}
	| {
		kind: "error";
		error: string;
	};
export let formatUsingValidBscPath = (
	code: string,
	bscPath: p.DocumentUri,
	isInterface: boolean
): execResult => {
	let extension = isInterface ? c.resiExt : c.resExt;
	let formatTempFileFullPath = createFileInTempDir(extension);
	fs.writeFileSync(formatTempFileFullPath, code, {
		encoding: "utf-8",
	});
	try {
		let result = childProcess.execFileSync(
			bscPath,
			["-color", "never", "-format", formatTempFileFullPath],
			{ stdio: "pipe" }
		);
		return {
			kind: "success",
			result: result.toString(),
		};
	} catch (e) {
		return {
			kind: "error",
			error: e.message,
		};
	} finally {
		// async close is fine. We don't use this file name again
		fs.unlink(formatTempFileFullPath, () => null);
	}
};

export let runBsbWatcherUsingValidBsbPath = (
	bsbPath: p.DocumentUri,
	projectRootPath: p.DocumentUri
) => {
	let process = childProcess.execFile(bsbPath, ["-w"], {
		cwd: projectRootPath,
	});
	return process;
	// try {
	// 	let result = childProcess.execFileSync(bsbPath, [], { stdio: 'pipe', cwd: projectRootPath })
	// 	return {
	// 		kind: 'success',
	// 		result: result.toString(),
	// 	}
	// } catch (e) {
	// 	return {
	// 		kind: 'error',
	// 		error: e.message,
	// 	}
	// }
};

export let parseDiagnosticLocation = (location: string): Range => {
	// example output location:
	// 3:9
	// 3:5-8
	// 3:9-6:1

	// language-server position is 0-based. Ours is 1-based. Don't forget to convert
	// also, our end character is inclusive. Language-server's is exclusive
	let isRange = location.indexOf("-") >= 0;
	if (isRange) {
		let [from, to] = location.split("-");
		let [fromLine, fromChar] = from.split(":");
		let isSingleLine = to.indexOf(":") >= 0;
		let [toLine, toChar] = isSingleLine ? to.split(":") : [fromLine, to];
		return {
			start: {
				line: parseInt(fromLine) - 1,
				character: parseInt(fromChar) - 1,
			},
			end: { line: parseInt(toLine) - 1, character: parseInt(toChar) },
		};
	} else {
		let [line, char] = location.split(":");
		let start = { line: parseInt(line) - 1, character: parseInt(char) };
		return {
			start: start,
			end: start,
		};
	}
};

type filesDiagnostics = {
	[key: string]: p.Diagnostic[];
};
type parsedCompilerLogResult = {
	done: boolean;
	result: filesDiagnostics;
};
export let parseCompilerLogOutput = (
	content: string
): parsedCompilerLogResult => {
	/* example .compiler.log file content that we're gonna parse:

#Start(1600519680823)

	Syntax error!
	/Users/chenglou/github/reason-react/src/test.res:1:8-2:3

	1 │ let a =
	2 │ let b =
	3 │

	This let-binding misses an expression


	Warning number 8
	/Users/chenglou/github/reason-react/src/test.res:3:5-8

	1 │ let a = j`😀`
	2 │ let b = `😀`
	3 │ let None = None
	4 │ let bla: int = "
	5 │   hi

	You forgot to handle a possible case here, for example:
	Some _


	We've found a bug for you!
	/Users/chenglou/github/reason-react/src/test.res:3:9

	1 │ let a = 1
	2 │ let b = "hi"
	3 │ let a = b + 1

	This has type: string
	Somewhere wanted: int

#Done(1600519680836)
	*/

	type parsedDiagnostic = {
		code: number | undefined;
		severity: t.DiagnosticSeverity;
		tag: t.DiagnosticTag | undefined;
		content: string[];
	};
	let parsedDiagnostics: parsedDiagnostic[] = [];
	let lines = content.split("\n");
	let done = false;

	for (let i = 0; i < lines.length; i++) {
		let line = lines[i];
		if (line.startsWith("  We've found a bug for you!")) {
			parsedDiagnostics.push({
				code: undefined,
				severity: t.DiagnosticSeverity.Error,
				tag: undefined,
				content: [],
			});
		} else if (line.startsWith("  Warning number ")) {
			let warningNumber = parseInt(line.slice("  Warning number ".length));
			let tag: t.DiagnosticTag | undefined = undefined;
			switch (warningNumber) {
				case 11:
				case 20:
				case 26:
				case 27:
				case 32:
				case 33:
				case 34:
				case 35:
				case 36:
				case 37:
				case 38:
				case 39:
				case 60:
				case 66:
				case 67:
				case 101:
					tag = t.DiagnosticTag.Unnecessary;
					break;
				case 3:
					tag = t.DiagnosticTag.Deprecated;
					break;
			}
			parsedDiagnostics.push({
				code: Number.isNaN(warningNumber) ? undefined : warningNumber,
				severity: t.DiagnosticSeverity.Warning,
				tag: tag,
				content: [],
			});
		} else if (line.startsWith("  Syntax error!")) {
			parsedDiagnostics.push({
				code: undefined,
				severity: t.DiagnosticSeverity.Error,
				tag: undefined,
				content: [],
			});
		} else if (line.startsWith("#Done(")) {
			done = true;
		} else if (/^  +[0-9]+ /.test(line)) {
			// code display. Swallow
		} else if (line.startsWith("  ")) {
			parsedDiagnostics[parsedDiagnostics.length - 1].content.push(line);
		}
	}

	let result: filesDiagnostics = {};
	parsedDiagnostics.forEach((parsedDiagnostic) => {
		let [fileAndLocation, ...diagnosticMessage] = parsedDiagnostic.content;
		let locationSeparator = fileAndLocation.indexOf(":");
		let file = fileAndLocation.substring(2, locationSeparator);
		let location = fileAndLocation.substring(locationSeparator + 1);
		if (result[file] == null) {
			result[file] = [];
		}
		let cleanedUpDiagnostic =
			diagnosticMessage
				.map((line) => {
					// remove the spaces in front
					return line.slice(2);
				})
				.join("\n")
				// remove start and end whitespaces/newlines
				.trim() + "\n";
		result[file].push({
			severity: parsedDiagnostic.severity,
			tags: parsedDiagnostic.tag === undefined ? [] : [parsedDiagnostic.tag],
			code: parsedDiagnostic.code,
			range: parseDiagnosticLocation(location),
			source: "ReScript",
			message: cleanedUpDiagnostic,
		});
	});

	return { done, result };
};
