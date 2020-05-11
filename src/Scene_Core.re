type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

module MovesExtra = {
  type t = {
    position,
    extraMoves: int,
  };
};

module Rock = {
  type t = {
    position,
    structuralIntegrity: int,
  };

  let initial = (position: position): t => {
    position,
    structuralIntegrity: 3,
  };
};

type scene = {
  movesLeft: int,
  player: position,
  previous: option(scene),
  goal: position,
  movesExtras: list(MovesExtra.t),
  walls: list(position),
  rocks: list(Rock.t),
};

let rec mapAllScenes = (scene: scene, f: scene => scene): scene =>
  f({
    ...scene,
    previous:
      Belt.Option.map(scene.previous, scene => mapAllScenes(scene, f)),
  });

let rec getPath = (scene): list(position) => {
  let head = scene.player;
  let rest = Belt.Option.mapWithDefault(scene.previous, [], getPath);
  [head, ...rest];
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let setPlayer = (scene, player) => {...scene, player};

let setPrevious = (scene, previous) => {...scene, previous: Some(previous)};

let modifyRocks = (scene: scene, f: Rock.t => option(Rock.t)): scene => {
  {...scene, rocks: Belt.List.keepMap(scene.rocks, f)};
};
