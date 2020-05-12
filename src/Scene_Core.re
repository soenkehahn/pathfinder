type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

module Player = {
  type t = {position};

  let initial: t = {
    position: {
      x: 0,
      y: 0,
    },
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

// fixme: where to put previous?

type revertible = {
  movesLeft: int,
  player: Player.t,
  previous: option(revertible),
  rocks: list(Rock.t),
};

let rec mapAllScenes =
        (revertable: revertible, f: revertible => revertible): revertible =>
  f({
    ...revertable,
    previous:
      Belt.Option.map(revertable.previous, scene => mapAllScenes(scene, f)),
  });

let modifyMovesLeft = (revertable, f) => {
  ...revertable,
  movesLeft: f(revertable.movesLeft),
};

let modifyPlayer = (revertable, f) => {
  ...revertable,
  player: f(revertable.player),
};

let modifyRocks =
    (revertable: revertible, f: Rock.t => option(Rock.t)): revertible => {
  {...revertable, rocks: Belt.List.keepMap(revertable.rocks, f)};
};

type scene = {
  revertible,
  hasHammer: bool,
  goal: position,
  movesExtras: list(MovesExtra.t),
  walls: list(position),
  hammers: list(position),
};

let getPath = (scene: scene): list(position) => {
  let rec inner = (revertible: revertible): list(position) => {
    let head = revertible.player.position;
    let rest = Belt.Option.mapWithDefault(revertible.previous, [], inner);
    [head, ...rest];
  };
  inner(scene.revertible);
};

let setPrevious = (scene, previous) => {...scene, previous: Some(previous)};
