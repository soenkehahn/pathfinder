let cellSize = 50;

type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

type scene = {
  movesLeft: int,
  position,
  path: list(position),
};

let initial = {
  movesLeft: 5,
  position: {
    x: 0,
    y: 0,
  },
  path: [],
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let modifyPosition = (scene, f) => {...scene, position: f(scene.position)};

let appendToPath = (scene, position) => {
  ...scene,
  path: [position, ...scene.path],
};

let undo = scene =>
  switch (scene.path) {
  | [last, ...path] => {position: last, path, movesLeft: scene.movesLeft + 1}
  | [] => scene
  };

let draw = (context: Webapi.Canvas.Canvas2d.t, scene: scene): unit => {
  open Webapi.Canvas.Canvas2d;
  setFillStyle(context, String, "#ff0000");
  fillRect(
    ~x=float_of_int(scene.position.x * cellSize),
    ~y=float_of_int(scene.position.y * cellSize),
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
    context,
  );
  ();
};

type key =
  | Up
  | Down
  | Left
  | Right
  | Undo;

let key_of_js_key = (key: string): option(key) =>
  switch (key) {
  | "ArrowUp" => Some(Up)
  | "ArrowDown" => Some(Down)
  | "ArrowLeft" => Some(Left)
  | "ArrowRight" => Some(Right)
  | " " => Some(Undo)
  | _ => None
  };

let move = (scene, f) =>
  if (scene.movesLeft > 0) {
    scene
    |> modifyPosition(_, f)
    |> modifyMovesLeft(_, x => x - 1)
    |> appendToPath(_, scene.position);
  } else {
    scene;
  };

let step = (scene: scene, key: key): scene =>
  switch (key) {
  | Up => scene |> move(_, modifyY(_, y => y + 1))
  | Down => scene |> move(_, modifyY(_, y => y - 1))
  | Left => scene |> move(_, modifyX(_, x => x - 1))
  | Right => scene |> move(_, modifyX(_, x => x + 1))
  | Undo => undo(scene)
  };
