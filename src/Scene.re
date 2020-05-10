open Key;
open Scene_Core;

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

let move_player = (scene, f: position => position) => {
  let new_position = f(scene.player);
  if (scene.movesLeft > 0 && !List.mem(new_position, scene.walls)) {
    scene
    |> modifyPlayer(_, _ => new_position)
    |> modifyMovesLeft(_, moves => moves - 1)
    |> appendToPath(_, scene.player);
  } else {
    scene;
  };
};

let is_game_over = scene => scene.player == scene.goal;

let processMovesExtras = (scene): scene => {
  let (activeExtras: list(movesExtra), remainingExtras) =
    List.partition(
      extra => extra.position == scene.player,
      scene.movesExtras,
    );
  List.fold_left(
    (scene, extra) =>
      {...scene, movesLeft: scene.movesLeft + extra.extraMoves},
    {...scene, movesExtras: remainingExtras},
    activeExtras,
  );
};

let step = (scene: scene, key: key): scene =>
  if (is_game_over(scene)) {
    scene;
  } else {
    (
      switch (key) {
      | Up => scene |> move_player(_, modifyY(_, y => y + 1))
      | Down => scene |> move_player(_, modifyY(_, y => y - 1))
      | Left => scene |> move_player(_, modifyX(_, x => x - 1))
      | Right => scene |> move_player(_, modifyX(_, x => x + 1))
      | Space => undo(scene)
      }
    )
    |> processMovesExtras(_);
  };
