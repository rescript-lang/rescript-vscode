import * as Path from "path";
import Fs from "fs";

export let findPathRec = (
	currentPath: string,
	targetPath: string
): null | string => {
	let baseDir = Path.dirname(currentPath);
	if (Fs.existsSync(Path.join(baseDir, targetPath))) {
		return baseDir;
	} else {
		if (baseDir === currentPath) {
			// reached top
			return null;
		} else {
			return findPathRec(baseDir, targetPath);
		}
	}
};