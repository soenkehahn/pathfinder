open Belt;
open List;
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

let movePlayer = (scene: scene, f: position => position) => {
  let newPlayerPosition = f(scene.revertible.player);
  if (scene.movesLeft <= 0) {
    scene;
  } else if (scene.walls->has(newPlayerPosition, (==))) {
    scene;
  } else if (scene.revertible.rocks
             ->map(rock => rock.position)
             ->has(newPlayerPosition, (==))) {
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
    ->pushHistory(scene.revertible)
    ->modifyMovesLeft(moves => moves - 1);
  } else {
    {
      ...scene,
      revertible: scene.revertible->modifyPlayer(_player => newPlayerPosition),
    }
    ->pushHistory(scene.revertible)
    ->modifyMovesLeft(moves => moves - 1);
  };
};

let isGameOver = scene => scene.revertible.player == scene.goal;

let processMovesExtras = (scene: scene): scene => {
  let (activeExtras: list(MovesExtra.t), remainingExtras) =
    scene.movesExtras
    ->partition(extra => extra.position == scene.revertible.player);
  let extraMoves =
    activeExtras->map(extra => extra.extraMoves)->reduce(0, (+));
  {
    ...scene,
    movesLeft: scene.movesLeft + extraMoves,
    movesExtras: remainingExtras,
  };
};

let processHammers = (scene: scene): scene => {
  let (activeHammers, remainingHammers) =
    scene.hammers->partition(hammer => hammer == scene.revertible.player);
  if (activeHammers->length > 0) {
    {...scene, hasHammer: true, hammers: remainingHammers};
  } else {
    scene;
  };
};

let processExtras = (scene: scene): scene =>
  scene->processMovesExtras->processHammers;

let step = (scene: scene, key: key): scene =>
  if (isGameOver(scene)) {
    scene;
  } else {
    (
      switch (key) {
      | Up => scene->movePlayer(modifyY(_, y => y + 1))
      | Down => scene->movePlayer(modifyY(_, y => y - 1))
      | Left => scene->movePlayer(modifyX(_, x => x - 1))
      | Right => scene->movePlayer(modifyX(_, x => x + 1))
      | Space => scene->revert
      }
    )
    ->processExtras;
  };
