const fs = require("fs");

for (const file of process.argv.slice(2)) {
  const text = fs.readFileSync(file, "utf8");
  fs.writeFileSync(file, text.replace(/\r\n/g, "\n").replace(/\r/g, "\n"));
}
