open Jest;
open Scene;
open Expect;
open! Expect.Operators;

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

describe("handleKeyPress", () => {
  let table = [
    (Up, {x: 0, y: 1}),
    (Down, {x: 0, y: (-1)}),
    (Left, {x: (-1), y: 0}),
    (Right, {x: 1, y: 0}),
  ];
  testAll(
    "moves the player in the given direction", table, ((direction, expected)) =>
    expect(handleKeyPress(Scene.initial, direction).position) == expected
  );

  test("moves the player multiple times", () => {
    let scene =
      Scene.initial |> handleKeyPress(_, Up) |> handleKeyPress(_, Right);
    expect(scene.position) == {x: 1, y: 1};
  });

  test("counts down the moves", () => {
    let initial = Scene.initial.movesLeft;
    let scene = Scene.initial |> handleKeyPress(_, Up);
    expect((initial, scene.movesLeft)) == (5, 4);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = {...Scene.initial, movesLeft: 0};
    expect(handleKeyPress(scene, Up)) == scene;
  });
});
