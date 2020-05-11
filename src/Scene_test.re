open Jest;
open Scene_Core;
open Scene;
open Expect;
open! Expect.Operators;
open Key;

let rec steps = (scene, keys) =>
  switch (keys) {
  | [key, ...rest] => scene |> step(_, key) |> steps(_, rest)
  | [] => scene
  };

let test_scene =
    (
      ~movesLeft: int=3,
      ~player: position={x: 0, y: 0},
      ~previous: option(scene)=None,
      ~goal={x: 3, y: 0},
      ~movesExtras=[],
      ~walls=[],
      ~rocks=[],
      (),
    ) => {
  movesLeft,
  player,
  previous,
  goal,
  movesExtras,
  walls,
  rocks,
};

describe("moving", () => {
  let table = [
    (Up, {x: 0, y: 1}),
    (Down, {x: 0, y: (-1)}),
    (Left, {x: (-1), y: 0}),
    (Right, {x: 1, y: 0}),
  ];
  testAll(
    "moves the player in the given direction", table, ((direction, expected)) =>
    expect(step(test_scene(), direction).player) == expected
  );

  test("moves the player multiple times", () => {
    let scene = test_scene() |> steps(_, [Up, Right]);
    expect(scene.player) == {x: 1, y: 1};
  });

  test("counts down the moves", () => {
    let initial = test_scene(~movesLeft=3, ()).movesLeft;
    let scene = test_scene() |> step(_, Up);
    expect((initial, scene.movesLeft)) == (3, 2);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = test_scene(~movesLeft=0, ());
    expect(step(scene, Up)) == scene;
  });

  test("tracks path of player", () => {
    let scene = test_scene() |> steps(_, [Up, Right]);
    expect(getPath(scene)) == [{x: 1, y: 1}, {x: 0, y: 1}, {x: 0, y: 0}];
  });
});

describe("undo", () => {
  test("moves to the last position", () => {
    let scene =
      test_scene(
        ~previous=Some(test_scene(~player={x: 23, y: 42}, ())),
        (),
      )
      |> step(_, Space);
    expect(scene.player) == {x: 23, y: 42};
  });

  test("increases movesLeft", () => {
    let scene = test_scene(~movesLeft=3, ()) |> steps(_, [Up, Space]);
    expect(scene.movesLeft) == 3;
  });

  test("resets the history", () => {
    let scene = test_scene() |> steps(_, [Up, Space]);
    expect(scene.previous) == None;
  });

  test("on the initial scene doesn't do anything", () => {
    let scene = test_scene() |> step(_, Space);
    expect(scene) == scene;
  });

  test("works when movesLeft is 0", () => {
    let scene =
      test_scene(
        ~movesLeft=0,
        ~previous=Some(test_scene(~player={x: 23, y: 42}, ())),
        (),
      )
      |> step(_, Space);
    expect(scene.player) == {x: 23, y: 42};
  });
});

describe("is_game_over", () => {
  test("is false during the game", () =>
    expect(is_game_over(test_scene())) == false
  );

  let end_scene = test_scene(~player={x: 1, y: 2}, ~goal={x: 1, y: 2}, ());

  test("when reaching the goal the game is won", () => {
    expect(is_game_over(end_scene)) == true
  });

  test("when reaching the goal, no other keyboard input is accepted", () => {
    expect(steps(end_scene, [Up, Right, Space])) == end_scene
  });
});

describe("moves extra", () => {
  let scene =
    test_scene(
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
    expect(step(scene, Right).movesLeft) == scene.movesLeft - 1 + 3
  });

  test("removes the extra from the scene", () => {
    expect(step(scene, Right).movesExtras) == []
  });

  test("undo doesn't bring consumed extras back", () => {
    expect(steps(scene, [Right, Space]).movesExtras) == []
  });

  test("undo doesn't remove added moves", () => {
    expect(steps(scene, [Right, Space]).movesLeft) == 6
  });

  describe("undoing twice", () => {
    let scene =
      test_scene(
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
    test("undoing twice doesn't bring consumed extras back", () => {
      expect(steps(scene, [Right, Right, Space, Space]).movesExtras) == []
    });

    test("undoing twice doesn't remove added moves", () => {
      expect(steps(scene, [Right, Right, Space, Space]).movesLeft) == 6
    });
  });
});

describe("walls", () => {
  test("walls can't be passed through", () => {
    let scene =
      test_scene(~player={x: 0, y: 0}, ~walls=[{x: (-1), y: 0}], ());
    expect(step(scene, Left)) == scene;
  })
});

describe("rocks", () => {
  open Rock;

  test("rocks can't be passed through on the first attempt", () => {
    let scene = test_scene(~rocks=[initial({x: 1, y: 0})], ());
    expect(step(scene, Right).player) == {x: 0, y: 0};
  });

  test("rocks can be destroyed with three moves", () => {
    let scene =
      test_scene(~movesLeft=4, ~rocks=[initial({x: 1, y: 0})], ());
    expect(steps(scene, [Right, Right, Right]).rocks) == [];
  });

  test("destroying rocks costs moves", () => {
    let scene =
      test_scene(~movesLeft=5, ~rocks=[initial({x: 1, y: 0})], ());
    expect(steps(scene, [Right, Right, Right]).movesLeft) == 2;
  });

  test("destroying rocks can happen in non-consecutive moves", () => {
    let scene =
      test_scene(~movesLeft=6, ~rocks=[initial({x: 1, y: 0})], ());
    expect(steps(scene, [Right, Up, Down, Right, Right, Right]).player)
    == {x: 1, y: 0};
  });

  test("undo will undo damage to rocks", () => {
    let scene = test_scene(~rocks=[initial({x: 1, y: 0})], ());
    expect(
      steps(scene, [Right, Space]).rocks
      |> Belt.List.headExn
      |> (rock => rock.structuralIntegrity),
    )
    == 3;
  });
});
