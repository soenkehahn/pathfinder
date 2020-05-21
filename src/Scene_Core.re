open Belt;
open Key;

module Player = {
  type t = position;

  let initial: t = {x: 0, y: 0};
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

type revertible = {
  player: Player.t,
  rocks: list(Rock.t),
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
  history: list((Key.direction, revertible)),
  movesLeft: int,
  hasHammer: bool,
  goal: position,
  movesExtras: list(MovesExtra.t),
  walls: list(position),
  hammers: list(position),
  boulders: list(position),
};

let modifyMovesLeft = (scene, f) => {
  ...scene,
  movesLeft: f(scene.movesLeft),
};

let replaceBoulder = (scene: scene, old: position, new_: position) => {
  ...scene,
  boulders:
    scene.boulders
    ->List.map(p =>
        if (p == old) {
          new_;
        } else {
          p;
        }
      ),
};

let pushHistory = (scene: scene, previous) => {
  ...scene,
  history: [previous, ...scene.history],
};

let getPath = (scene: scene): list(position) => {
  let head = scene.revertible.player;
  let rest =
    Belt.List.map(scene.history, ((_, revertable)) => revertable.player);
  [head, ...rest];
};
