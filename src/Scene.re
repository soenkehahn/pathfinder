open Belt;
open List;
open Key;
open Scene_Core;
open Utils;

let collidesWithImmovable = (position, scene): bool => {
  let immovables =
    concatMany([|
      scene.revertible.rocks->map(rock => rock.position),
      scene.hammers,
      [scene.goal],
      scene.movesExtras->map(extra => extra.position),
      scene.walls,
    |]);
  immovables->has(position, (==));
};

let collidingBoulder = (scene, position): option(position) => {
  let colliding = scene.boulders->keep(boulder => boulder == position);
  switch (colliding) {
  | [boulder] => Some(boulder)
  | [] => None
  | _ => throw("invariant violated: multiple boulders in one position")
  };
};

type boulderCollisionResult =
  | BouldersShifted(scene)
  | Blocked;

let rec handleBoulderCollisions =
        (scene: scene, direction: direction, position: position)
        : boulderCollisionResult => {
  switch (scene->collidingBoulder(position)) {
  | None => BouldersShifted(scene)
  | Some(collidingBoulder) =>
    let newBoulderPosition = direction->move(collidingBoulder);
    if (newBoulderPosition->collidesWithImmovable(scene)) {
      Blocked;
    } else {
      switch (scene->handleBoulderCollisions(direction, newBoulderPosition)) {
      | Blocked => Blocked
      | BouldersShifted(modifiedScene) =>
        BouldersShifted(
          modifiedScene->replaceBoulder(collidingBoulder, newBoulderPosition),
        )
      };
    };
  };
};

let revert = (scene: scene): scene =>
  switch (scene.history) {
  | [(direction, previous), ...rest] =>
    switch (scene->handleBoulderCollisions(direction, previous.player)) {
    | BouldersShifted(modifiedScene) => {
        ...modifiedScene,
        movesLeft: scene.movesLeft + 1,
        revertible: previous,
        history: rest,
      }
    | Blocked => scene
    }
  | [] => scene
  };

let movePlayer = (scene: scene, direction: direction) => {
  let newPlayerPosition = direction->move(scene.revertible.player);
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
    ->pushHistory((direction->Key.revert, scene.revertible))
    ->modifyMovesLeft(moves => moves - 1);
  } else {
    switch (scene->handleBoulderCollisions(direction, newPlayerPosition)) {
    | BouldersShifted(modifiedScene) =>
      {
        ...modifiedScene,
        revertible:
          modifiedScene.revertible->modifyPlayer(_player => newPlayerPosition),
      }
      ->pushHistory((direction->Key.revert, scene.revertible))
      ->modifyMovesLeft(moves => moves - 1)
    | Blocked => scene
    };
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
      | Direction(direction) => scene->movePlayer(direction)
      | Space => scene->revert
      }
    )
    ->processExtras;
  };
