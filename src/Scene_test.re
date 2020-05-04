open Jest;
open Scene;
open Expect;
open! Expect.Operators;

let rec steps = (scene, keys) =>
  switch (keys) {
  | [key, ...rest] => scene |> step(_, key) |> steps(_, rest)
  | [] => scene
  };

describe("key_of_js_key", () => {
  let table = [
    ("ArrowUp", Up),
    ("ArrowDown", Down),
    ("ArrowLeft", Left),
    ("ArrowRight", Right),
  ];
  testAll("converts to key", table, ((js_key, key)) =>
    expect(key_of_js_key(js_key)) === Some(key)
  );

  test("returns None for unknown keys", () =>
    expect(key_of_js_key("unknown")) === None
  );
});

describe("moving", () => {
  let table = [
    (Up, {x: 0, y: 1}),
    (Down, {x: 0, y: (-1)}),
    (Left, {x: (-1), y: 0}),
    (Right, {x: 1, y: 0}),
  ];
  testAll(
    "moves the player in the given direction", table, ((direction, expected)) =>
    expect(step(Scene.initial, direction).position) == expected
  );

  test("moves the player multiple times", () => {
    let scene = Scene.initial |> steps(_, [Up, Right]);
    expect(scene.position) == {x: 1, y: 1};
  });

  test("counts down the moves", () => {
    let initial = Scene.initial.movesLeft;
    let scene = Scene.initial |> step(_, Up);
    expect((initial, scene.movesLeft)) == (5, 4);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = {...Scene.initial, movesLeft: 0};
    expect(step(scene, Up)) == scene;
  });

  test("tracks path of player", () => {
    let scene = Scene.initial |> steps(_, [Up, Right]);
    expect(scene.path) == [{x: 0, y: 1}, {x: 0, y: 0}];
  });
});

describe("undo", () => {
  test("moves to the last position", () => {
    let scene =
      {
        position: {
          x: 0,
          y: 0,
        },
        path: [{x: 23, y: 42}],
        movesLeft: 4,
      }
      |> step(_, Undo);
    expect(scene.position) == {x: 23, y: 42};
  });

  test("increases movesLeft", () => {
    let scene = Scene.initial |> steps(_, [Up, Undo]);
    expect(scene.movesLeft) == 5;
  });

  test("removes the position from the path", () => {
    let scene = Scene.initial |> steps(_, [Up, Undo]);
    expect(scene.path) == [];
  });

  test("on the initial scene doesn't do anything", () => {
    let scene = Scene.initial |> step(_, Undo);
    expect(scene) == scene;
  });

  test("works when movesLeft is 0", () => {
    let scene =
      {...Scene.initial, movesLeft: 0, path: [{x: 23, y: 42}]}
      |> step(_, Undo);
    expect(scene.position) == {x: 23, y: 42};
  });
});
