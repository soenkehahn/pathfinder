open Scene_Core;
open Belt;
open List;

exception ParseError(string);

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

type parseResult = {
  initialMoves: int,
  grid: list(list((int, int, string))),
};

let parseGrid = (csv: string): parseResult => {
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
  let players =
    withCoordinates->filter2d(cell => Js.String.startsWith("Player", cell));
  let (player, initialMoves) =
    switch (players) {
    | [(x, y, cell)] => ({x, y}, parseNumber(cell))
    | [] => raise(ParseError("no player found"))
    | [_, ..._] => raise(ParseError("multiple players found"))
    };
  let grid =
    withCoordinates->map2d(((x, y, cell)) =>
      (x - player.x, y - player.y, cell)
    );
  {grid, initialMoves: initialMoves->Option.getWithDefault(3)};
};

let parseGoal = grid =>
  switch (grid->filter2d(cell => cell == "Goal")) {
  | [(x, y, _)] => {x, y}
  | [] => raise(ParseError("no goal found"))
  | [_, ..._] => raise(ParseError("multiple goals found"))
  };

let parseMovesExtras = (grid): list(MovesExtra.t) =>
  MovesExtra.(
    grid
    ->filter2d(cell => Js.String.startsWith("Moves", cell))
    ->map(((x, y, cell)) =>
        {
          extraMoves: parseNumber(cell)->Option.getExn,
          position: {
            x,
            y,
          },
        }
      )
  );

let parseWalls = (grid: list(list((int, int, string)))): list(position) =>
  grid->filter2d(cell => cell == "Wall")->map(((x, y, _cell)) => {x, y});

let parseRocks = grid =>
  grid
  ->filter2d(cell => cell == "Rock")
  ->map(((x, y, _cell)) => Rock.initial({x, y}));

let parseHammers = grid =>
  grid->filter2d(cell => cell == "Hammer")->map(((x, y, _cell)) => {x, y});

let parse = (csv: string): scene => {
  let {grid, initialMoves} = parseGrid(csv);
  {
    revertible: {
      player: Player.initial,
      rocks: parseRocks(grid),
    },
    history: [],
    movesLeft: initialMoves,
    hasHammer: false,
    goal: parseGoal(grid),
    movesExtras: parseMovesExtras(grid),
    walls: parseWalls(grid),
    hammers: parseHammers(grid),
  };
};
