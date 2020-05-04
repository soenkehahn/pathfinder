open Key;

type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

type scene = {
  movesLeft: int,
  player: position,
  path: list(position),
  goal: position,
};

let initial = {
  movesLeft: 5,
  player: {
    x: 0,
    y: 0,
  },
  path: [],
  goal: {
    x: 3,
    y: 0,
  },
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

let undo = scene =>
  switch (scene.path) {
  | [last, ...path] => {
      ...scene,
      player: last,
      path,
      movesLeft: scene.movesLeft + 1,
    }
  | [] => scene
  };

let move = (scene, f) =>
  if (scene.movesLeft > 0) {
    scene
    |> modifyPlayer(_, f)
    |> modifyMovesLeft(_, x => x - 1)
    |> appendToPath(_, scene.player);
  } else {
    scene;
  };

let is_game_over = scene => scene.player == scene.goal;

let step = (scene: scene, key: key): scene =>
  if (!is_game_over(scene)) {
    switch (key) {
    | Up => scene |> move(_, modifyY(_, y => y + 1))
    | Down => scene |> move(_, modifyY(_, y => y - 1))
    | Left => scene |> move(_, modifyX(_, x => x - 1))
    | Right => scene |> move(_, modifyX(_, x => x + 1))
    | Undo => undo(scene)
    };
  } else {
    scene;
  };
