import { parseSync, type Node } from "oxc-parser";
import { walk } from "oxc-walker";
import MagicString from "magic-string";

const integerRegex = /^-?\d+$/;
const floatRegex = /^-?\d+(\.\d+)?$/;

const rescriptKeywords = new Set(["type", "open", "as", "in"]);

type Rule<T extends Node = Node> = {
  match: (node: T, parent: Node | null) => boolean;
  transform: (node: T, parent: Node | null, magicString: MagicString) => void;
  stopAfterMatch?: boolean; // If true, stop applying further rules after this one matches
};

// Single quotes to double quotes
const singleQuotesToDouble: Rule<Node> = {
  match: (node) =>
    node.type === "JSXAttribute" &&
    node.value?.type === "Literal" &&
    typeof node.value.raw === "string" &&
    node.value.raw.startsWith("'"),
  transform: (node, _, magicString) => {
    const attr = node as Extract<Node, { type: "JSXAttribute" }>;
    const value = attr.value as Extract<typeof attr.value, { type: "Literal" }>;
    magicString.update(value.start, value.end, `"${value.raw!.slice(1, -1)}"`);
  },
};

// SVG width/height numeric to string
const svgWidthHeightToString: Rule<Node> = {
  match: (node, parent) =>
    node.type === "JSXAttribute" &&
    parent?.type === "JSXOpeningElement" &&
    parent.name.type === "JSXIdentifier" &&
    parent.name.name.toLowerCase() === "svg" &&
    node.name.type === "JSXIdentifier" &&
    (node.name.name === "width" || node.name.name === "height") &&
    node.value?.type === "JSXExpressionContainer" &&
    node.value.expression?.type === "Literal" &&
    typeof node.value.expression.value === "number",
  transform: (node, _, magicString) => {
    const attr = node as Extract<Node, { type: "JSXAttribute" }>;
    const value = attr.value as Extract<
      typeof attr.value,
      { type: "JSXExpressionContainer" }
    >;
    const expression = value.expression as Extract<
      typeof value.expression,
      { type: "Literal" }
    >;
    const numericValue = String(expression.value);
    magicString.update(value.start, value.end, `"${numericValue}"`);
  },
};

// Rescript keywords get underscore suffix
const rescriptKeywordUnderscore: Rule<Node> = {
  match: (node) =>
    node.type === "JSXAttribute" &&
    node.name.type === "JSXIdentifier" &&
    rescriptKeywords.has(node.name.name),
  transform: (node, _, magicString) => {
    const attr = node as Extract<Node, { type: "JSXAttribute" }>;
    magicString.appendRight(attr.name.end, "_");
  },
};

// aria- attributes to camelCase
const ariaToCamelCase: Rule<Node> = {
  match: (node) =>
    node.type === "JSXAttribute" &&
    node.name.type === "JSXIdentifier" &&
    typeof node.name.name === "string" &&
    node.name.name.startsWith("aria-"),
  transform: (node, _, magicString) => {
    const attr = node as Extract<Node, { type: "JSXAttribute" }>;
    const name = attr.name.name as string;
    magicString.update(
      attr.name.start + 4,
      attr.name.start + 6,
      name[5]?.toUpperCase() || "",
    );
  },
};

// data-testid to dataTestId
const dataTestIdToCamelCase: Rule<Node> = {
  match: (node) =>
    node.type === "JSXAttribute" &&
    node.name.type === "JSXIdentifier" &&
    node.name.name === "data-testid",
  transform: (node, _, magicString) => {
    const attr = node as Extract<Node, { type: "JSXAttribute" }>;
    magicString.update(attr.name.start, attr.name.end, "dataTestId");
  },
};

// Null values become =true
const nullValueToTrue: Rule<Node> = {
  match: (node) => node.type === "JSXAttribute" && node.value === null,
  transform: (node, _, magicString) => {
    magicString.appendRight(node.end, "=true");
  },
};

// Integer text nodes
const integerTextNode: Rule<Node> = {
  match: (node) =>
    node.type === "JSXText" &&
    typeof node.raw === "string" &&
    integerRegex.test(node.raw.trim()),
  transform: (node, _, magicString) => {
    magicString.prependLeft(node.start, "{React.int(");
    magicString.appendRight(node.end, ")}");
  },
  stopAfterMatch: true,
};

// Float text nodes
const floatTextNode: Rule<Node> = {
  match: (node) =>
    node.type === "JSXText" &&
    typeof node.raw === "string" &&
    floatRegex.test(node.raw.trim()),
  transform: (node, _, magicString) => {
    magicString.prependLeft(node.start, "{React.float(");
    magicString.appendRight(node.end, ")}");
  },
  stopAfterMatch: true,
};

// String text nodes
const stringTextNode: Rule<Node> = {
  match: (node) =>
    node.type === "JSXText" &&
    typeof node.value === "string" &&
    node.value.trim() !== "",
  transform: (node, _, magicString) => {
    magicString.prependLeft(node.start, '{React.string("');
    magicString.appendRight(node.end, '")}');
  },
  stopAfterMatch: true,
};

const rules: Rule<Node>[] = [
  singleQuotesToDouble,
  svgWidthHeightToString,
  rescriptKeywordUnderscore,
  ariaToCamelCase,
  dataTestIdToCamelCase,
  nullValueToTrue,
  integerTextNode,
  floatTextNode,
  stringTextNode,
];

function applyRules(
  node: Node,
  parent: Node | null,
  rules: Rule<Node>[],
  magicString: MagicString,
): void {
  for (const rule of rules) {
    if (rule.match(node, parent)) {
      rule.transform(node, parent, magicString);
      if (rule.stopAfterMatch) {
        break;
      }
    }
  }
}

export function transformJsx(input: string): string {
  const magicString = new MagicString(input);
  const parseResult = parseSync("clipboard-input.tsx", input, {
    astType: "ts",
    lang: "tsx",
  });

  walk(parseResult.program, {
    enter: (node: Node, parent: Node | null) => {
      applyRules(node, parent, rules, magicString);
    },
  });

  return magicString.toString();
}

export default transformJsx;
