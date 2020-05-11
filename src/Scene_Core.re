type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

module Player = {
  type t = {
    position,
    hasHammer: bool,
  };

  let initial: t = {
    position: {
      x: 0,
      y: 0,
    },
    hasHammer: false,
  };
};

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
  player: Player.t,
  previous: option(scene),
  goal: position,
  movesExtras: list(MovesExtra.t),
  walls: list(position),
  rocks: list(Rock.t),
  hammers: list(position),
};

let rec mapAllScenes = (scene: scene, f: scene => scene): scene =>
  f({
    ...scene,
    previous:
      Belt.Option.map(scene.previous, scene => mapAllScenes(scene, f)),
  });

let rec getPath = (scene): list(position) => {
  let head = scene.player.position;
  let rest = Belt.Option.mapWithDefault(scene.previous, [], getPath);
  [head, ...rest];
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let modifyPlayer = (scene, f) => {...scene, player: f(scene.player)};

let setPrevious = (scene, previous) => {...scene, previous: Some(previous)};

let modifyRocks = (scene: scene, f: Rock.t => option(Rock.t)): scene => {
  {...scene, rocks: Belt.List.keepMap(scene.rocks, f)};
};
