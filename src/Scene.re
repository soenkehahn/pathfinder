open Key;
open Scene_Core;

let undo = scene =>
  switch (scene.path) {
  | [last, ...path] => {...scene, player: last, path, moves: scene.moves + 1}
  | [] => scene
  };

let move_player = (scene, f: position => position) => {
  let new_position = f(scene.player);
  if (scene.moves > 0 && !List.mem(new_position, scene.walls)) {
    scene
    |> modifyPlayer(_, _ => new_position)
    |> modifyMoves(_, moves => moves - 1)
    |> appendToPath(_, scene.player);
  } else {
    scene;
  };
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
      | Up => scene |> move_player(_, modifyY(_, y => y + 1))
      | Down => scene |> move_player(_, modifyY(_, y => y - 1))
      | Left => scene |> move_player(_, modifyX(_, x => x - 1))
      | Right => scene |> move_player(_, modifyX(_, x => x + 1))
      | Space => undo(scene)
      }
    )
    |> processExtras(_);
  };
