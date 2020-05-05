open Key;
open Scene_Core;

let undo = scene =>
  switch (scene.path) {
  | [last, ...path] => {...scene, player: last, path, moves: scene.moves + 1}
  | [] => scene
  };

let move = (scene, f) =>
  if (scene.moves > 0) {
    scene
    |> modifyPlayer(_, f)
    |> modifyMoves(_, x => x - 1)
    |> appendToPath(_, scene.player);
  } else {
    scene;
  };

let is_game_over = scene => scene.player == scene.goal;

let processExtras = (scene): scene => {
  let (activeExtras: list(extra), remainingExtras) =
    List.partition(extra => extra.position == scene.player, scene.extras);
  List.fold_left(
    (scene, extra) => {...scene, moves: scene.moves + extra.extraMoves},
    {...scene, extras: remainingExtras},
    activeExtras,
  );
};

let step = (scene: scene, key: key): scene =>
  if (is_game_over(scene)) {
    scene;
  } else {
    (
      switch (key) {
      | Up => scene |> move(_, modifyY(_, y => y + 1))
      | Down => scene |> move(_, modifyY(_, y => y - 1))
      | Left => scene |> move(_, modifyX(_, x => x - 1))
      | Right => scene |> move(_, modifyX(_, x => x + 1))
      | Undo => undo(scene)
      }
    )
    |> processExtras(_);
  };
