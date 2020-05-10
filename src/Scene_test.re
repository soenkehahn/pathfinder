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
      ~moves: int=3,
      ~player: position={x: 0, y: 0},
      ~path: list(position)=[],
      ~goal={x: 3, y: 0},
      ~extras=[],
      ~walls=[],
      (),
    ) => {
  moves,
  player,
  path,
  goal,
  extras,
  walls,
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
    let initial = test_scene(~moves=3, ()).moves;
    let scene = test_scene() |> step(_, Up);
    expect((initial, scene.moves)) == (3, 2);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = test_scene(~moves=0, ());
    expect(step(scene, Up)) == scene;
  });

  test("tracks path of player", () => {
    let scene = test_scene() |> steps(_, [Up, Right]);
    expect(scene.path) == [{x: 0, y: 1}, {x: 0, y: 0}];
  });
});

describe("undo", () => {
  test("moves to the last position", () => {
    let scene = test_scene(~path=[{x: 23, y: 42}], ()) |> step(_, Space);
    expect(scene.player) == {x: 23, y: 42};
  });

  test("increases movesLeft", () => {
    let scene = test_scene(~moves=3, ()) |> steps(_, [Up, Space]);
    expect(scene.moves) == 3;
  });

  test("removes the position from the path", () => {
    let scene = test_scene() |> steps(_, [Up, Space]);
    expect(scene.path) == [];
  });

  test("on the initial scene doesn't do anything", () => {
    let scene = test_scene() |> step(_, Space);
    expect(scene) == scene;
  });

  test("works when movesLeft is 0", () => {
    let scene =
      test_scene(~moves=0, ~path=[{x: 23, y: 42}], ()) |> step(_, Space);
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

describe("extra moves extra", () => {
  let scene =
    test_scene(~extras=[{
                          position: {
                            x: 1,
                            y: 0,
                          },
                          extraMoves: 3,
                        }], ());

  test("gives the player extra moves", () => {
    expect(step(scene, Right).moves) == scene.moves - 1 + 3
  });

  test("removes the extra from the scene", () => {
    expect(step(scene, Right).extras) == []
  });
});

describe("walls", () => {
  test("walls can't be passed through", () => {
    let scene =
      test_scene(~player={x: 0, y: 0}, ~walls=[{x: (-1), y: 0}], ());
    expect(step(scene, Left)) == scene;
  })
});
