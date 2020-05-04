let cellSize = 50;

type scene = {
  x: int,
  y: int,
};

let draw = (context: Webapi.Canvas.Canvas2d.t, scene: scene): unit => {
  open Webapi.Canvas.Canvas2d;
  setFillStyle(context, String, "#ff0000");
  fillRect(
    ~x=float_of_int(scene.x * cellSize),
    ~y=float_of_int(scene.y * cellSize),
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
    context,
  );
  ();
};

type key =
  | Up;

let key_of_js_key = (key: string): option(key) =>
  switch (key) {
  | "ArrowUp" => Some(Up)
  | _ => None
  };

let handleKeyPress = (scene: scene, key: key): option(scene) =>
  switch (key) {
  | Up => Some({x: scene.x, y: scene.y + 1})
  };
