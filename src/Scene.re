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
};

let initial = {
  movesLeft: 5,
  position: {
    x: 0,
    y: 0,
  },
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let modifyPosition = (scene, f) => {...scene, position: f(scene.position)};

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
  | Right;

let key_of_js_key = (key: string): option(key) =>
  switch (key) {
  | "ArrowUp" => Some(Up)
  | "ArrowDown" => Some(Down)
  | "ArrowLeft" => Some(Left)
  | "ArrowRight" => Some(Right)
  | _ => None
  };

let handleKeyPress = (scene: scene, key: key): scene =>
  if (scene.movesLeft > 0) {
    (
      switch (key) {
      | Up => scene |> modifyPosition(_, modifyY(_, y => y + 1))
      | Down => scene |> modifyPosition(_, modifyY(_, y => y - 1))
      | Left => scene |> modifyPosition(_, modifyX(_, x => x - 1))
      | Right => scene |> modifyPosition(_, modifyX(_, x => x + 1))
      }
    )
    |> modifyMovesLeft(_, x => x - 1);
  } else {
    scene;
  };
