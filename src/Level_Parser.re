open Scene_Core;

exception ParseError(string);

let map2d = (f: 'a => 'b, grid: list(list('a))): list(list('b)) => {
  List.(map(map(f, _), grid));
};

let filter2d =
    (f: 'a => bool, grid: list(list((int, int, 'a))))
    : list((int, int, 'a)) =>
  List.(flatten(map(filter(((_x, _y, cell)) => f(cell), _), grid)));

let get_grid =
    (cells: list(list(string))): list(list((int, int, string))) => {
  open List;
  let withXs: list(list((int, string))) =
    map(row => mapi((x, cell) => (x, cell), row), cells);
  let withCoordinates: list(list((int, int, string))) =
    mapi(
      (rowIndex, row) =>
        map(((x, cell)) => (x, length(withXs) - 1 - rowIndex, cell), row),
      withXs,
    );
  let players = filter2d(cell => cell == "Player", withCoordinates);
  let player =
    switch (players) {
    | [(x, y, _)] => {x, y}
    | [] => raise(ParseError("no player found"))
    | [_, ..._] => raise(ParseError("multiple players found"))
    };
  map2d(
    ((x, y, cell)) => (x - player.x, y - player.y, cell),
    withCoordinates,
  );
};

let parse_moves = (cell): int => {
  int_of_string(Js.String.split(" ", cell)[1]);
};

let parse = (csv: string): scene => {
  let cells =
    Js.String.(
      split("\n", csv)
      |> Array.to_list
      |> List.map(row => split(",", row) |> Array.to_list, _)
    );
  let grid = get_grid(cells);
  let goal =
    switch (filter2d(cell => cell == "Goal", grid)) {
    | [(x, y, _)] => {x, y}
    | [] => raise(ParseError("no goal found"))
    | [_, ..._] => raise(ParseError("multiple goals found"))
    };
  let extras =
    filter2d(cell => Js.String.startsWith("Moves", cell), grid)
    |> List.map(
         ((x, y, cell)) =>
           {
             extraMoves: parse_moves(cell),
             position: {
               x,
               y,
             },
           },
         _,
       );
  let result = {
    moves: 3,
    player: {
      x: 0,
      y: 0,
    },
    goal,
    path: [],
    extras,
  };
  result;
};
