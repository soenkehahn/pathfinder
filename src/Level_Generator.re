open Node.Child_process;
open Belt;

let generateCsvs = (): unit => {
  execSync(
    "libreoffice --convert-to csv levels/*.ods --outdir dist",
    option(),
  )
  ->ignore;
};

let concatWith = (chunks, separator): string => {
  chunks
  ->List.mapWithIndex((i, chunk) =>
      if (i == 0) {
        chunk;
      } else {
        separator ++ chunk;
      }
    )
  ->List.reduce("", (++));
};

let main = () => {
  print_endline("generating csvs...");
  generateCsvs();

  let levelNames: list(string) =
    Node.Fs.readdirSync("levels")
    ->List.fromArray
    ->List.keep(file => Js.String.endsWith(".ods", file))
    ->List.map(file => "."->Js.String.split(file)->Array.getExn(0));

  Node.Fs.writeFileAsUtf8Sync(
    "dist/all_levels.json",
    "["
    ++ levelNames
       ->List.map(name => "\"" ++ name ++ ".csv\"")
       ->concatWith(", ")
    ++ "]",
  );
};
