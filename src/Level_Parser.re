open Scene_Core;
open Belt;
open List;
open Key;

module ParseResult = {
  let let_ = Result.flatMap;

  let rec mapM =
          (list: list('a), f: 'a => Result.t('b, 'error))
          : Result.t(list('b), 'error) =>
    switch (list) {
    | [a, ...r] =>
      switch (f(a)) {
      | Ok(b) =>
        switch (mapM(r, f)) {
        | Ok(r) => Ok([b, ...r])
        | Error(message) => Error(message)
        }
      | Error(message) => Error(message)
      }
    | [] => Ok([])
    };

  let localizeError =
      (result: Result.t('a, string), location: string): Result.t('a, string) =>
    switch (result) {
    | Ok(a) => Ok(a)
    | Error(message) => Error(location ++ ": " ++ message)
    };
};

let map2d = (grid: list(list('a)), f: 'a => 'b): list(list('b)) => {
  grid->map(row => row->map(f));
};

let filter2d =
    (grid: list(list((int, int, 'a))), f: 'a => bool)
    : list((int, int, 'a)) =>
  grid->map(row => row->keep(((_x, _y, cell)) => f(cell)))->flatten;

let parseNumber = (cell): option(int) => {
  Js.String.split(" ", cell)->Array.get(1)->Option.map(int_of_string);
};

let parseSimple =
    (grid: list(list((int, int, string))), name: string): list(position) =>
  grid->filter2d(cell => cell == name)->map(((x, y, _cell)) => {x, y});

let parseWithNumber = (grid, name): list((position, option(int))) =>
  grid
  ->filter2d(cell => Js.String.startsWith(name, cell))
  ->map(((x, y, cell)) => ({x, y}, parseNumber(cell)));

type grid = {
  initialMoves: int,
  grid: list(list((int, int, string))),
};

let parseGrid = (csv: string): Result.t(grid, string) => {
  let cells: list(list(string)) =
    Js.String.(
      split("\n", csv)->fromArray->map(row => split(",", row)->fromArray)
    );
  let withXs: list(list((int, string))) =
    cells->map(row => row->mapWithIndex((x, cell) => (x, cell)));
  let withCoordinates: list(list((int, int, string))) =
    withXs->mapWithIndex((rowIndex, row) =>
      row->map(((x, cell)) => (x, length(withXs) - 1 - rowIndex, cell))
    );
  let players = withCoordinates->parseWithNumber("Player");
  let%ParseResult (player, initialMoves) =
    switch (players) {
    | [result] => Ok(result)
    | [] => Error("no player found")
    | [_, ..._] => Error("multiple players found")
    };
  let grid =
    withCoordinates->map2d(((x, y, cell)) =>
      (x - player.x, y - player.y, cell)
    );
  Ok({grid, initialMoves: initialMoves->Option.getWithDefault(3)});
};

let parseGoal = grid =>
  switch (grid->parseSimple("Goal")) {
  | [position] => Ok(position)
  | [] => Error("no goal found")
  | [_, ..._] => Error("multiple goals found")
  };

let parseMovesExtras = (grid): list(MovesExtra.t) =>
  grid
  ->parseWithNumber("Moves")
  ->List.map(((position, moves)) =>
      MovesExtra.{extraMoves: moves->Option.getExn, position}
    );

let parse = (csv: string): Result.t(scene, string) => {
  let%ParseResult {grid, initialMoves} = parseGrid(csv);
  let%ParseResult goal = parseGoal(grid);
  Ok({
    revertible: {
      player: Player.initial,
      rocks: grid->parseSimple("Rock")->map(Rock.initial),
    },
    history: [],
    movesLeft: initialMoves,
    hasHammer: false,
    goal,
    movesExtras: parseMovesExtras(grid),
    walls: grid->parseSimple("Wall"),
    hammers: grid->parseSimple("Hammer"),
    boulders: grid->parseSimple("Boulder"),
  });
};
