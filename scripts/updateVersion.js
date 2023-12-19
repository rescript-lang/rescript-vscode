//@ts-check
// This file is used only in dev time
// Bump version in package.json and this script will update version in ml file
// and rescript.json

const fs = require("fs");
const path = require("path");

const toolsPkgDir = path.join(__dirname, "..", "tools");

const { version } = JSON.parse(
  fs.readFileSync(path.join(toolsPkgDir, "package.json"), "utf8"),
);

const rescriptJsonPath = path.join(toolsPkgDir, "rescript.json");

const rescriptJson = JSON.parse(fs.readFileSync(rescriptJsonPath, "utf8"));
rescriptJson.version = version;
fs.writeFileSync(rescriptJsonPath, JSON.stringify(rescriptJson, null, 2));

fs.writeFileSync(
  path.join(toolsPkgDir, "bin", "version.ml"),
  `let version = "${version}"`,
);
