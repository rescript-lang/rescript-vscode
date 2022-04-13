import { filter } from "./vendor/fuzzy";

export let processDiagnosticMessage = (diagnosticMessage: string[]) => {
  // TODO: This should be upstreamed to the compiler at some point soon.
  // Heuristic for detecting whether this is a JSX call.
  if (
    diagnosticMessage[0]?.trim() ===
      "The function applied to this argument has type" &&
    // We assume that we can detect JSX calls by checking if they have an
    // optional key label, and that it returns `ReactDOM.Props.domProps`.
    diagnosticMessage.some((line) => line.trim().includes("~?key: string,")) &&
    diagnosticMessage.some((line) =>
      line.trim().endsWith(") => ReactDOM.Props.domProps")
    )
  ) {
    let targetLabel = diagnosticMessage[diagnosticMessage.length - 1]
      .split("This argument cannot be applied with label ~")[1]
      ?.trim();

    if (targetLabel != null) {
      type jsxProp = {
        name: string;
        type: string;
        optional: boolean;
      };

      let allLabels: jsxProp[] = [];

      diagnosticMessage.forEach((line) => {
        let trimmed = line.trim();
        if (trimmed.startsWith("~")) {
          let regex = /~(.+?): (.+),/;
          let matched = trimmed.match(regex);

          if (matched === null) {
            return;
          }

          let [_, key, type] = matched;
          let optional = key.startsWith("?");

          allLabels.push({
            name: optional ? key.slice(1) : key,
            type,
            optional,
          });
        }
      });

      if (allLabels.length > 0) {
        let message = `This JSX element does not take the prop \`${targetLabel}\`.`;

        let didYouMean = filter(targetLabel, allLabels, {
          extract: (prop) => prop.name,
        })[0];

        if (didYouMean != null) {
          let didYouMeanJsxProp = allLabels.find(
            (label) => label.name === didYouMean.string
          );
          if (didYouMeanJsxProp != null) {
            message += `\nDid you mean \`${didYouMeanJsxProp.name}\`?\n\n${
              didYouMeanJsxProp.name
            }: ${didYouMeanJsxProp.type}${
              didYouMeanJsxProp.optional ? "=?" : ""
            }`;
          }
        }

        return message + "\n";
      }
    }
  }

  // remove start and end whitespaces/newlines
  return diagnosticMessage.join("\n").trim() + "\n";
};
