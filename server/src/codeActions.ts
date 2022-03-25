// This file holds code actions derived from diagnostics. There are more code
// actions available in the extension, but they are derived via the analysis
// OCaml binary.
import * as p from "vscode-languageserver-protocol";

export type filesCodeActions = {
  [key: string]: { range: p.Range; codeAction: p.CodeAction }[];
};

interface findCodeActionsConfig {
  diagnostic: p.Diagnostic;
  diagnosticMessage: string[];
  file: string;
  range: p.Range;
  addFoundActionsHere: filesCodeActions;
}

export let findCodeActionsInDiagnosticsMessage = ({
  diagnostic,
  diagnosticMessage,
  file,
  range,
  addFoundActionsHere: codeActions,
}: findCodeActionsConfig) => {
  diagnosticMessage.forEach((line, index, array) => {
    // Because of how actions work, there can only be one per diagnostic. So,
    // halt whenever a code action has been found.
    let actions = [
      didYouMeanAction,
      addUndefinedRecordFields,
      simpleConversion,
      topLevelUnitType,
    ];

    for (let action of actions) {
      if (
        action({
          array,
          codeActions,
          diagnostic,
          file,
          index,
          line,
          range,
        })
      ) {
        break;
      }
    }
  });
};

interface codeActionExtractorConfig {
  line: string;
  index: number;
  array: string[];
  file: string;
  range: p.Range;
  diagnostic: p.Diagnostic;
  codeActions: filesCodeActions;
}

type codeActionExtractor = (config: codeActionExtractorConfig) => boolean;

let didYouMeanAction: codeActionExtractor = ({
  codeActions,
  diagnostic,
  file,
  line,
  range,
}) => {
  if (line.startsWith("Hint: Did you mean")) {
    let regex = /Did you mean ([A-Za-z0-9_]*)?/;
    let match = line.match(regex);

    if (match === null) {
      return false;
    }

    let [_, suggestion] = match;

    if (suggestion != null) {
      codeActions[file] = codeActions[file] || [];
      let codeAction: p.CodeAction = {
        title: `Replace with '${suggestion}'`,
        edit: {
          changes: {
            [file]: [{ range, newText: suggestion }],
          },
        },
        diagnostics: [diagnostic],
      };

      codeActions[file].push({
        range,
        codeAction,
      });

      return true;
    }
  }

  return false;
};

let addUndefinedRecordFields: codeActionExtractor = ({
  array,
  codeActions,
  diagnostic,
  file,
  index,
  line,
  range,
}) => {
  if (line.startsWith("Some record fields are undefined:")) {
    let recordFieldNames = line
      .trim()
      .split("Some record fields are undefined: ")[1]
      ?.split(" ");

    // This collects the rest of the fields if fields are printed on
    // multiple lines.
    array.slice(index + 1).forEach((line) => {
      recordFieldNames.push(...line.trim().split(" "));
    });

    if (recordFieldNames != null) {
      codeActions[file] = codeActions[file] || [];

      // The formatter outputs trailing commas automatically if the record
      // definition is on multiple lines, and no trailing comma if it's on a
      // single line. We need to adapt to this so we don't accidentally
      // insert an invalid comma.
      let multilineRecordDefinitionBody = range.start.line !== range.end.line;

      // Let's build up the text we're going to insert.
      let newText = "";

      if (multilineRecordDefinitionBody) {
        // If it's a multiline body, we know it looks like this:
        // ```
        // let someRecord = {
        //   atLeastOneExistingField: string,
        // }
        // ```
        // We can figure out the formatting from the range the code action
        // gives us. We'll insert to the direct left of the ending brace.

        // The end char is the closing brace, and it's always going to be 2
        // characters back from the record fields.
        let paddingCharacters = multilineRecordDefinitionBody
          ? range.end.character + 2
          : 0;
        let paddingContentRecordField = Array.from({
          length: paddingCharacters,
        }).join(" ");
        let paddingContentEndBrace = Array.from({
          length: range.end.character,
        }).join(" ");

        recordFieldNames.forEach((fieldName, index) => {
          if (index === 0) {
            // This adds spacing from the ending brace up to the equivalent
            // of the last record field name, needed for the first inserted
            // record field name.
            newText += "  ";
          } else {
            // The rest of the new record field names will start from a new
            // line, so they need left padding all the way to the same level
            // as the rest of the record fields.
            newText += paddingContentRecordField;
          }

          newText += `${fieldName}: assert false,\n`;
        });

        // Let's put the end brace back where it was (we still have it to the direct right of us).
        newText += `${paddingContentEndBrace}`;
      } else {
        // A single line record definition body is a bit easier - we'll just add the new fields on the same line.
        newText += ", ";
        newText += recordFieldNames
          .map((fieldName) => `${fieldName}: assert false`)
          .join(", ");
      }

      let codeAction: p.CodeAction = {
        title: `Add missing record fields`,
        edit: {
          changes: {
            [file]: [
              {
                range: {
                  start: {
                    line: range.end.line,
                    character: range.end.character - 1,
                  },
                  end: {
                    line: range.end.line,
                    character: range.end.character - 1,
                  },
                },
                newText,
              },
            ],
          },
        },
        diagnostics: [diagnostic],
      };

      codeActions[file].push({
        range,
        codeAction,
      });

      return true;
    }
  }

  return false;
};

let simpleConversion: codeActionExtractor = ({
  line,
  codeActions,
  file,
  range,
  diagnostic,
}) => {
  if (line.startsWith("You can convert ")) {
    let regex = /You can convert (\w*) to (\w*) with ([\w.]*).$/;
    let match = line.match(regex);

    if (match === null) {
      return false;
    }

    let [_, from, to, fn] = match;

    if (from != null && to != null && fn != null) {
      codeActions[file] = codeActions[file] || [];
      let codeAction: p.CodeAction = {
        title: `Convert ${from} to ${to} with ${fn}`,
        edit: {
          changes: {
            [file]: [
              {
                range: {
                  start: {
                    line: range.start.line,
                    character: range.start.character,
                  },
                  end: {
                    line: range.start.line,
                    character: range.start.character,
                  },
                },
                newText: `${fn}(`,
              },
              {
                range: {
                  start: {
                    line: range.end.line,
                    character: range.end.character,
                  },
                  end: {
                    line: range.end.line,
                    character: range.end.character,
                  },
                },
                newText: `)`,
              },
            ],
          },
        },
        diagnostics: [diagnostic],
      };

      codeActions[file].push({
        range,
        codeAction,
      });

      return true;
    }
  }

  return false;
};

let topLevelUnitType: codeActionExtractor = ({
  line,
  codeActions,
  file,
  range,
  diagnostic,
}) => {
  if (line.startsWith("Toplevel expression is expected to have unit type.")) {
    codeActions[file] = codeActions[file] || [];
    let codeAction: p.CodeAction = {
      title: `Wrap expression in ignore`,
      edit: {
        changes: {
          [file]: [
            {
              range: {
                start: {
                  line: range.start.line,
                  character: range.start.character,
                },
                end: {
                  line: range.start.line,
                  character: range.start.character,
                },
              },
              newText: `ignore(`,
            },
            {
              range: {
                start: {
                  line: range.end.line,
                  character: range.end.character,
                },
                end: {
                  line: range.end.line,
                  character: range.end.character,
                },
              },
              newText: `)`,
            },
          ],
        },
      },
      diagnostics: [diagnostic],
    };

    codeActions[file].push({
      range,
      codeAction,
    });

    return true;
  }

  return false;
};
