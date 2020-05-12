open Key;
open Scene_Core;

let revert = (scene): scene =>
  switch (scene.history) {
  | [previous, ...rest] => {
      ...scene,
      movesLeft: scene.movesLeft + 1,
      revertible: previous,
      history: rest,
    }
  | [] => scene
  };

let move_player = (scene: scene, f: position => position) => {
  let newPlayerPosition = f(scene.revertible.player);
  if (scene.movesLeft <= 0) {
    scene;
  } else if (List.mem(newPlayerPosition, scene.walls)) {
    scene;
  } else if (List.mem(
               newPlayerPosition,
               scene.revertible.rocks
               |> List.map(rock => Rock.(rock.position), _),
             )) {
    {
      ...scene,
      revertible:
        Rock.(
          modifyRocks(scene.revertible, rock =>
            if (rock.position == newPlayerPosition) {
              if (rock.structuralIntegrity == 1 || scene.hasHammer) {
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
        ),
    }
    |> pushHistory(_, scene.revertible)
    |> modifyMovesLeft(_, moves => moves - 1);
  } else {
    {
      ...scene,
      revertible:
        scene.revertible |> modifyPlayer(_, _player => newPlayerPosition),
    }
    |> pushHistory(_, scene.revertible)
    |> modifyMovesLeft(_, moves => moves - 1);
  };
};

let is_game_over = scene => scene.revertible.player == scene.goal;

let processMovesExtras = (scene: scene): scene => {
  open MovesExtra;
  let (activeExtras: list(t), remainingExtras) =
    List.partition(
      extra => extra.position == scene.revertible.player,
      scene.movesExtras,
    );
  let extraMoves =
    activeExtras
    |> List.map(extra => extra.extraMoves, _)
    |> List.fold_left((+), 0, _);
  {
    ...scene,
    movesLeft: scene.movesLeft + extraMoves,
    movesExtras: remainingExtras,
  };
};

let processHammers = (scene: scene): scene => {
  let (activeHammers, remainingHammers) =
    List.partition(
      hammer => hammer == scene.revertible.player,
      scene.hammers,
    );
  if (activeHammers->List.length > 0) {
    {...scene, hasHammer: true, hammers: remainingHammers};
  } else {
    scene;
  };
};

let processExtras = (scene: scene): scene =>
  scene->processMovesExtras->processHammers;

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
      | Space => scene->revert
      }
    )
    ->processExtras;
  };
