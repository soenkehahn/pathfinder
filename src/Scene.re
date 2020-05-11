open Key;
open Scene_Core;

let undo = (scene): scene =>
  switch (scene.previous) {
  | Some(previous) => previous
  | None => scene
  };

let move_player = (scene, f: position => position) => {
  let newPlayer = f(scene.player);
  if (scene.movesLeft <= 0) {
    scene;
  } else if (List.mem(newPlayer, scene.walls)) {
    scene;
  } else if (List.mem(
               newPlayer,
               scene.rocks |> List.map(rock => Rock.(rock.position), _),
             )) {
    Rock.(
      modifyRocks(scene, rock =>
        if (rock.position == newPlayer) {
          if (rock.structuralIntegrity == 1) {
            None;
          } else {
            Some({
              ...rock,
              structuralIntegrity: rock.structuralIntegrity - 1,
            });
          };
        } else {
          Some(rock);
        }
      )
    )
    |> modifyMovesLeft(_, moves => moves - 1)
    |> setPrevious(_, scene);
  } else {
    scene
    |> setPlayer(_, newPlayer)
    |> modifyMovesLeft(_, moves => moves - 1)
    |> setPrevious(_, scene);
  };
};

let is_game_over = scene => scene.player == scene.goal;

let processMovesExtras = (scene): scene => {
  open MovesExtra;
  let (activeExtras: list(t), remainingExtras) =
    List.partition(
      extra => extra.position == scene.player,
      scene.movesExtras,
    );
  let extraMoves =
    activeExtras
    |> List.map(extra => extra.extraMoves, _)
    |> List.fold_left((+), 0, _);
  mapAllScenes(scene, scene =>
    {
      ...scene,
      movesLeft: scene.movesLeft + extraMoves,
      movesExtras: remainingExtras,
    }
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
