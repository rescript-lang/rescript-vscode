import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { after, test } from "node:test";
import { TextDocument } from "vscode-languageserver-textdocument";
import { getConfigCompletions } from "../src/jsonConfig";

const root = fs.mkdtempSync(
  path.join(os.tmpdir(), "rescript-json-completions-"),
);
after(() => fs.rmSync(root, { recursive: true, force: true }));
fs.writeFileSync(path.join(root, "rescript.json"), "{}");
const schemaDir = path.join(root, "node_modules/rescript/docs/docson");
fs.mkdirSync(schemaDir, { recursive: true });
fs.writeFileSync(
  path.join(schemaDir, "build-schema.json"),
  JSON.stringify({
    properties: {
      name: { type: "string" },
      jsx: { $ref: "#/definitions/jsx" },
    },
    definitions: {
      jsx: {
        type: "object",
        properties: {
          version: { type: "number" },
          preserve: { type: "boolean", default: false },
        },
      },
    },
  }),
);

function completions(source: string) {
  const offset = source.indexOf("|");
  const document = TextDocument.create(
    pathToFileURL(path.join(root, "rescript.json")).href,
    "json",
    1,
    source.replace("|", ""),
  );
  return {
    document,
    items: getConfigCompletions(document, document.positionAt(offset)),
  };
}

test("offers only unused properties in the containing nested object", () => {
  const { items } = completions('{"jsx": {"version": 4, |}}');
  assert.deepEqual(
    items.map((item) => item.label),
    ["preserve"],
  );
});

test("does not fall back to top-level properties when nested options are exhausted", () => {
  assert.deepEqual(
    completions('{"jsx": {"version": 4, "preserve": true, |}}').items,
    [],
  );
});

test("does not offer property completions inside values or unknown objects", () => {
  assert.deepEqual(completions('{"name": "hel|lo"}').items, []);
  assert.deepEqual(completions('{"unknown": {|}}').items, []);
});

test("replaces empty property quotes instead of inserting another pair", () => {
  const { document, items } = completions('{"jsx": {"|"}}');
  const item = items.find((item) => item.label === "preserve")!;
  assert.ok(item.textEdit && "range" in item.textEdit);
  assert.equal(
    TextDocument.applyEdits(document, [item.textEdit]),
    '{"jsx": {"preserve": false}}',
  );
});

test("replaces partial keys and preserves existing values", () => {
  const { document, items } = completions('{"jsx": {"pre|": true}}');
  const item = items.find((item) => item.label === "preserve")!;
  assert.ok(item.textEdit && "range" in item.textEdit);
  assert.equal(
    TextDocument.applyEdits(document, [item.textEdit]),
    '{"jsx": {"preserve": true}}',
  );
});
