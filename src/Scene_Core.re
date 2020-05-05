type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

type extra = {
  position,
  extraMoves: int,
};

type scene = {
  moves: int,
  player: position,
  path: list(position),
  goal: position,
  extras: list(extra),
};

let modifyMoves = (scene, f) => {...scene, moves: f(scene.moves)};

let modifyPlayer = (scene, f) => {...scene, player: f(scene.player)};

let appendToPath = (scene, position) => {
  ...scene,
  path: [position, ...scene.path],
};
