open Node.Child_process;
open Belt;

let convertToCsv = (name): unit => {
  let file = name ++ ".ods";
  execSync(
    "libreoffice --headless --convert-to csv " ++ file,
    option(~cwd="levels", ()),
  )
  ->ignore;
};

let toReasonFile = (name): unit => {
  let csv = Node.Fs.readFileAsUtf8Sync("levels/" ++ name ++ ".csv");
  Node.Fs.writeFileAsUtf8Sync(
    "src/Levels_" ++ name ++ ".re",
    "let csv = \"" ++ csv ++ "\";",
  );
};

let generate = name => {
  convertToCsv(name);
  toReasonFile(name);
};

let levelNames: list(string) =
  Node.Fs.readdirSync("levels")
  ->List.fromArray
  ->List.keep(file => Js.String.endsWith(".ods", file))
  ->List.map(file => Js.String.split(".", file)->Array.getExn(0));

List.forEach(
  levelNames,
  levelName => {
    print_endline("generating level " ++ levelName ++ "...");
    generate(levelName);
  },
);

Node.Fs.writeFileAsUtf8Sync(
  "src/Levels_All.re",
  "let csvs = ["
  ++ Js.String.concatMany(
       List.toArray(
         levelNames->List.map(name => "Levels_" ++ name ++ ".csv, "),
       ),
       "",
     )
  ++ "];",
);
