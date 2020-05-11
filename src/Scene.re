open Key;
open Scene_Core;

let undo = (scene): scene =>
  switch (scene.previous) {
  | Some(previous) => previous
  | None => scene
  };

let move_player = (scene, f: position => position) => {
  let newPlayerPosition = f(scene.player.position);
  if (scene.movesLeft <= 0) {
    scene;
  } else if (List.mem(newPlayerPosition, scene.walls)) {
    scene;
  } else if (List.mem(
               newPlayerPosition,
               scene.rocks |> List.map(rock => Rock.(rock.position), _),
             )) {
    Rock.(
      modifyRocks(scene, rock =>
        if (rock.position == newPlayerPosition) {
          if (rock.structuralIntegrity == 1 || scene.player.hasHammer) {
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
    |> modifyPlayer(_, player => {...player, position: newPlayerPosition})
    |> modifyMovesLeft(_, moves => moves - 1)
    |> setPrevious(_, scene);
  };
};

let is_game_over = scene => scene.player.position == scene.goal;

let processMovesExtras = (scene): scene => {
  open MovesExtra;
  let (activeExtras: list(t), remainingExtras) =
    List.partition(
      extra => extra.position == scene.player.position,
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

let processHammers = scene => {
  let (activeHammers, remainingHammers) =
    List.partition(hammer => hammer == scene.player.position, scene.hammers);
  let hasHammer = activeHammers->List.length > 0;
  let scene = {...scene, hammers: remainingHammers};
  if (hasHammer) {
    scene->mapAllScenes(scene =>
      {...scene, hammers: remainingHammers}
      ->modifyPlayer(player => {...player, hasHammer: true})
    );
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
      | Space => undo(scene)
      }
    )
    |> processExtras(_);
  };
