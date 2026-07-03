const fs = require("fs");

const rawCsv = fs.readFileSync("AC Tasks.csv", "utf8");

const lines = rawCsv.split(/\r?\n/);
const filteredLines = lines.filter((line, index) => {
  if (index === 0) return true; // keep header row
  const firstColumn = line.split(",", 1)[0];
  return firstColumn.trim() !== "";
});
const csv = filteredLines.join("\n");

const elmEscaped = csv
  .replace(/\\/g, "\\\\")
  .replace(/"""/g, '\\"\\"\\"');

const elmModule = `module Data exposing (csvData)


csvData : String
csvData =
    """${elmEscaped}"""
`;

fs.writeFileSync("src/Data.elm", elmModule);
