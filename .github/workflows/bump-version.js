const fs = require("fs");
const path = require("path");
const semver = require("semver");

const latestPublish = process.argv[2];

const packageJson = fs.readFileSync(path.join("./", "package.json"), {
  encoding: "utf-8",
});

let release = JSON.parse(packageJson).version;

let newVersion = latestPublish;

// A prepublished version must be one minor higher than a regular published version.
// E.g. if package.json has version 1.3.0 and there is no prepublished version yet,
// increment minor by one -> 1.4.0.
if (semver.minor(latestPublish) === semver.minor(release)) {
  newVersion = semver.inc(newVersion, "minor", semver.rel);
}
// Increment the version patch. E.g. if we fetch version 1.4.0 as the latest pre-release,
// increment patch by one -> 1.4.1.
else if (semver.minor(latestPublish) > semver.minor(release)) {
  newVersion = semver.inc(newVersion, "patch", semver.rel);
}
// Otherwise throw an error, because the pre-release version should always be just one
// minor higher than the release version.
else {
  throw new Error(
    "Version number minors are more than off by one, check package.json and (pre-)published versions manually."
  );
}

if (!semver.valid(newVersion)) {
  throw new Error("Invalid version string: ", newVersion);
}

console.log(`::set-output name=new_version::${newVersion}`);
