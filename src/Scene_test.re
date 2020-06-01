open Belt;
open List;
open Jest;
open Scene_Core;
open Scene;
open Expect;
open! Expect.Operators;
open Key;
open Test_Utils;

let rec steps = (scene, keys) =>
  switch (keys) {
  | [key, ...rest] => scene->step(key)->steps(rest)
  | [] => scene
  };

describe("moving", () => {
  let table = [
    (Up, {x: 0, y: 1}),
    (Down, {x: 0, y: (-1)}),
    (Left, {x: (-1), y: 0}),
    (Right, {x: 1, y: 0}),
  ];

  testAll(
    "moves the player in the given direction", table, ((key, expected)) =>
    expect(step(testScene(), direction(key)).revertible.player) == expected
  );

  test("moves the player multiple times", () => {
    let scene = testScene()->steps([Up, Right]->map(direction));
    expect(scene.revertible.player) == {x: 1, y: 1};
  });

  test("counts down the moves", () => {
    let initial = testScene(~movesLeft=3, ()).movesLeft;
    let scene = testScene()->step(direction(Up));
    expect((initial, scene.movesLeft)) == (3, 2);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = testScene(~movesLeft=0, ());
    expect(scene->step(direction(Up))) == scene;
  });

  test("tracks path of player", () => {
    let scene = testScene()->steps([Up, Right]->map(direction));
    expect(getPath(scene)) == [{x: 1, y: 1}, {x: 0, y: 1}, {x: 0, y: 0}];
  });
});

describe("revert", () => {
  test("moves to the last position", () => {
    let scene =
      testScene(
        ~history=[
          (Up, testScene(~playerPosition={x: 23, y: 42}, ()).revertible),
        ],
        (),
      )
      ->step(Space);
    expect(scene.revertible.player) == {x: 23, y: 42};
  });

  test("increases movesLeft", () => {
    let scene = testScene(~movesLeft=3, ())->steps([direction(Up), Space]);
    expect(scene.movesLeft) == 3;
  });

  test("resets the history", () => {
    let scene = testScene()->steps([direction(Up), Space]);
    expect(scene.history) == [];
  });

  test("on the initial scene doesn't do anything", () => {
    let scene = testScene()->step(Space);
    expect(scene) == scene;
  });

  test("works when movesLeft is 0", () => {
    let scene =
      testScene(
        ~movesLeft=0,
        ~history=[
          (Up, testScene(~playerPosition={x: 23, y: 42}, ()).revertible),
        ],
        (),
      )
      ->step(Space);
    expect(scene.revertible.player) == {x: 23, y: 42};
  });
});

describe("isGameOver", () => {
  test("is false during the game", () =>
    expect(isGameOver(testScene())) == false
  );

  let endScene =
    testScene(~playerPosition={x: 1, y: 2}, ~goal={x: 1, y: 2}, ());

  test("when reaching the goal the game is won", () => {
    expect(isGameOver(endScene)) == true
  });

  test("when reaching the goal, no other keyboard input is accepted", () => {
    expect(steps(endScene, [direction(Up), direction(Right), Space]))
    == endScene
  });
});

describe("moves extras", () => {
  let scene =
    testScene(
      ~movesLeft=3,
      ~movesExtras=[{
                      position: {
                        x: 1,
                        y: 0,
                      },
                      extraMoves: 3,
                    }],
      (),
    );

  test("gives the player extra moves", () => {
    expect(step(scene, direction(Right)).movesLeft) == scene.movesLeft
    - 1
    + 3
  });

  test("removes the extra from the scene", () => {
    expect(step(scene, direction(Right)).movesExtras) == []
  });

  test("revert doesn't bring consumed extras back", () => {
    expect(steps(scene, [direction(Right), Space]).movesExtras) == []
  });

  test("revert doesn't remove added moves", () => {
    expect(steps(scene, [direction(Right), Space]).movesLeft) == 6
  });

  describe("reverting twice", () => {
    let scene =
      testScene(
        ~movesLeft=3,
        ~movesExtras=[{
                        position: {
                          x: 2,
                          y: 0,
                        },
                        extraMoves: 3,
                      }],
        (),
      );
    test("reverting twice doesn't bring consumed extras back", () => {
      expect(
        steps(scene, [direction(Right), direction(Right), Space, Space]).
          movesExtras,
      )
      == []
    });

    test("reverting twice doesn't remove added moves", () => {
      expect(
        steps(scene, [direction(Right), direction(Right), Space, Space]).
          movesLeft,
      )
      == 6
    });
  });
});

describe("walls", () => {
  test("walls can't be passed through", () => {
    let scene =
      testScene(
        ~playerPosition={x: 0, y: 0},
        ~walls=[{x: (-1), y: 0}],
        (),
      );
    expect(step(scene, direction(Left))) == scene;
  })
});

describe("rocks", () => {
  open Rock;

  test("rocks can't be passed through on the first attempt", () => {
    let scene = testScene(~rocks=[initial({x: 1, y: 0})], ());
    expect(step(scene, direction(Right)).revertible.player) == {x: 0, y: 0};
  });

  test("rocks can be destroyed with three moves", () => {
    let scene = testScene(~movesLeft=4, ~rocks=[initial({x: 1, y: 0})], ());
    expect(
      steps(scene, [Right, Right, Right]->map(direction)).revertible.rocks,
    )
    == [];
  });

  test("destroying rocks costs moves", () => {
    let scene = testScene(~movesLeft=5, ~rocks=[initial({x: 1, y: 0})], ());
    expect(steps(scene, [Right, Right, Right]->map(direction)).movesLeft)
    == 2;
  });

  test("destroying rocks can happen in non-consecutive moves", () => {
    let scene = testScene(~movesLeft=6, ~rocks=[initial({x: 1, y: 0})], ());
    expect(
      steps(scene, [Right, Up, Down, Right, Right, Right]->map(direction)).
        revertible.
        player,
    )
    == {x: 1, y: 0};
  });

  test("revert will revert damage to rocks", () => {
    let scene = testScene(~rocks=[initial({x: 1, y: 0})], ());
    expect(
      steps(scene, [direction(Right), Space]).revertible.rocks->headExn.
        structuralIntegrity,
    )
    == 3;
  });
});

describe("hammer extras", () => {
  test("get removed from the scene when passed through", () => {
    let scene = testScene(~hammers=[{x: 1, y: 0}], ());
    expect(step(scene, direction(Right)).hammers) == [];
  });

  test("add the hammer extra to the player", () => {
    let scene = testScene(~hammers=[{x: 1, y: 0}], ());
    expect((scene.hasHammer, step(scene, direction(Right)).hasHammer))
    == (false, true);
  });

  test("allow to destroy walls with one move", () => {
    let scene =
      testScene(~hasHammer=true, ~rocks=[Rock.initial({x: 1, y: 0})], ());
    expect(step(scene, direction(Right)).revertible.rocks) == [];
  });

  test("reverting doesn't put hammers back into the scene", () => {
    let scene = testScene(~hammers=[{x: 1, y: 0}], ());
    expect(steps(scene, [direction(Right), Space]).hammers) == [];
  });

  test("reverting doesn't remove the hammer extra from the player", () => {
    let scene = testScene(~hammers=[{x: 1, y: 0}], ());
    expect(steps(scene, [direction(Right), Space]).hasHammer) == true;
  });
});

describe("irrevertable boulders", () => {
  test("boulders can be pushed around", () => {
    let scene =
      testScene(~goal={x: 0, y: 1}, ~boulders=[{x: 1, y: 0}], ())
      ->step(direction(Right));
    expect((scene.revertible.player, scene.boulders))
    == ({x: 1, y: 0}, [{x: 2, y: 0}]);
  });

  test("boulders can push other boulders", () => {
    let scene =
      testScene(
        ~goal={x: 0, y: 1},
        ~boulders=[{x: 1, y: 0}, {x: 2, y: 0}],
        (),
      )
      ->step(direction(Right));
    expect((scene.revertible.player, scene.boulders))
    == ({x: 1, y: 0}, [{x: 2, y: 0}, {x: 3, y: 0}]);
  });

  test("boulders cannot push other boulders when those are blocked", () => {
    let scene =
      testScene(
        ~goal={x: 0, y: 1},
        ~boulders=[{x: 1, y: 0}, {x: 2, y: 0}],
        ~walls=[{x: 3, y: 0}],
        (),
      )
      ->step(direction(Right));
    expect((scene.revertible.player, scene.boulders))
    == ({x: 0, y: 0}, [{x: 1, y: 0}, {x: 2, y: 0}]);
  });

  describe("when reverting", () => {
    test("does not revert boulders", () => {
      let scene =
        testScene(~goal={x: 0, y: 1}, ~boulders=[{x: 1, y: 0}], ())
        ->steps([direction(Right), Space]);
      expect(scene.boulders) == [{x: 2, y: 0}];
    });

    test("pushes boulders", () => {
      let scene =
        testScene(
          ~movesLeft=10,
          ~goal={x: (-10), y: 0},
          ~boulders=[{x: 1, y: 1}],
          (),
        )
        ->steps([
            direction(Up),
            direction(Up),
            direction(Right),
            direction(Right),
            direction(Down),
            direction(Left),
            Space,
            Space,
            Space,
            Space,
            Space,
          ]);
      expect((scene.revertible.player, scene.boulders))
      == ({x: 0, y: 1}, [{x: 0, y: 0}]);
    });

    test("blocked boulders block reverting", () => {
      let scene =
        testScene(
          ~movesLeft=10,
          ~goal={x: (-10), y: 0},
          ~boulders=[{x: 1, y: 1}],
          ~walls=[{x: 0, y: (-1)}],
          (),
        )
        ->steps([
            direction(Up),
            direction(Up),
            direction(Right),
            direction(Right),
            direction(Down),
            direction(Left),
            Space,
            Space,
            Space,
            Space,
            Space,
            Space,
          ]);
      expect((scene.revertible.player, scene.boulders))
      == ({x: 0, y: 1}, [{x: 0, y: 0}]);
    });
  });

  describe("immovable obstacles", () => {
    let table = [
      (
        "rocks",
        testScene(
          ~goal={x: 0, y: 1},
          ~boulders=[{x: 1, y: 0}],
          ~rocks=[Rock.initial({x: 2, y: 0})],
          (),
        ),
      ),
      (
        "goal",
        testScene(~goal={x: 2, y: 0}, ~boulders=[{x: 1, y: 0}], ()),
      ),
      (
        "movesExtras",
        testScene(
          ~goal={x: 0, y: 1},
          ~boulders=[{x: 1, y: 0}],
          ~movesExtras=[MovesExtra.{
                          position: {
                            x: 2,
                            y: 0,
                          },
                          extraMoves: 3,
                        }],
          (),
        ),
      ),
      (
        "walls",
        testScene(
          ~goal={x: 0, y: 1},
          ~boulders=[{x: 1, y: 0}],
          ~walls=[{x: 2, y: 0}],
          (),
        ),
      ),
      (
        "hammers",
        testScene(
          ~goal={x: 0, y: 1},
          ~boulders=[{x: 1, y: 0}],
          ~hammers=[{x: 2, y: 0}],
          (),
        ),
      ),
    ];

    testAll(
      "boulders can't be pushed over other objects",
      table,
      ((_, testScene)) => {
        let scene = testScene->step(direction(Right));
        expect((scene.revertible.player, scene.boulders))
        == ({x: 0, y: 0}, [{x: 1, y: 0}]);
      },
    );
  });
});

describe("level restarting", () => {
  test("allows to restart the level", () => {
    let scene = testScene();
    expect(scene->steps([direction(Up), Escape])) == scene;
  });

  test("restarting resets multiple steps", () => {
    let scene = testScene();
    expect(scene->steps([direction(Up), direction(Up), Escape])) == scene;
  });
});
