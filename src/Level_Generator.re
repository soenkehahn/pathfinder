open Node.Child_process;

let convertToCsv = (name): unit => {
  let file = name ++ ".ods";
  execSync(
    "libreoffice --headless --convert-to csv " ++ file,
    option(~cwd="levels", ()),
  )
  |> ignore;
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

let levelNames =
  Node.Fs.readdirSync("levels")
  |> Array.to_list
  |> List.filter(file => Js.String.endsWith(".ods", file), _)
  |> List.map(file => Js.String.split(".", file)[0], _);

Belt.List.forEach(
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
       Array.of_list(
         List.map(name => "Levels_" ++ name ++ ".csv, ", levelNames),
       ),
       "",
     )
  ++ "];",
);
