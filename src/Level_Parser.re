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

let parse_number = (cell): option(int) => {
  Js.String.split(" ", cell)->Array.get(1)->Option.map(int_of_string);
};

type parseResult = {
  initialMoves: int,
  grid: list(list((int, int, string))),
};

let parse_grid = (csv: string): parseResult => {
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
    | [(x, y, cell)] => ({x, y}, parse_number(cell))
    | [] => raise(ParseError("no player found"))
    | [_, ..._] => raise(ParseError("multiple players found"))
    };
  let grid =
    withCoordinates->map2d(((x, y, cell)) =>
      (x - player.x, y - player.y, cell)
    );
  {grid, initialMoves: initialMoves->Option.getWithDefault(3)};
};

let parse_goal = grid =>
  switch (grid->filter2d(cell => cell == "Goal")) {
  | [(x, y, _)] => {x, y}
  | [] => raise(ParseError("no goal found"))
  | [_, ..._] => raise(ParseError("multiple goals found"))
  };

let parse_moves_extras = (grid): list(MovesExtra.t) =>
  MovesExtra.(
    grid
    ->filter2d(cell => Js.String.startsWith("Moves", cell))
    ->map(((x, y, cell)) =>
        {
          extraMoves: parse_number(cell)->Option.getExn,
          position: {
            x,
            y,
          },
        }
      )
  );

let parse_walls = (grid: list(list((int, int, string)))): list(position) =>
  grid->filter2d(cell => cell == "Wall")->map(((x, y, _cell)) => {x, y});

let parse_rocks = grid =>
  grid
  ->filter2d(cell => cell == "Rock")
  ->map(((x, y, _cell)) => Rock.initial({x, y}));

let parse_hammers = grid =>
  grid->filter2d(cell => cell == "Hammer")->map(((x, y, _cell)) => {x, y});

let parse = (csv: string): scene => {
  let {grid, initialMoves} = parse_grid(csv);
  {
    revertible: {
      player: Player.initial,
      rocks: parse_rocks(grid),
    },
    history: [],
    movesLeft: initialMoves,
    hasHammer: false,
    goal: parse_goal(grid),
    movesExtras: parse_moves_extras(grid),
    walls: parse_walls(grid),
    hammers: parse_hammers(grid),
  };
};
