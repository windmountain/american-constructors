const fs = require("fs");

const csv = fs.readFileSync("AC Tasks.csv", "utf8");

const elmEscaped = csv
  .replace(/\\/g, "\\\\")
  .replace(/"""/g, '\\"\\"\\"');

const elmModule = `module Data exposing (csvData)


csvData : String
csvData =
    """${elmEscaped}"""
`;

fs.writeFileSync("src/Data.elm", elmModule);
