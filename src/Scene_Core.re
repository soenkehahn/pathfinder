type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

type movesExtra = {
  position,
  extraMoves: int,
};

type scene = {
  movesLeft: int,
  player: position,
  path: list(position),
  goal: position,
  movesExtras: list(movesExtra),
  walls: list(position),
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let modifyPlayer = (scene, f) => {...scene, player: f(scene.player)};

let appendToPath = (scene, position) => {
  ...scene,
  path: [position, ...scene.path],
};
