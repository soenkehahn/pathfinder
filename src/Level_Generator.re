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

convertToCsv("Nine");
toReasonFile("Nine");
