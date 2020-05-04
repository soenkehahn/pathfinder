open Jest;
open Scene;
open Expect;
open! Expect.Operators;
let (>>=) = Belt.Option.flatMap;

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
    expect(handleKeyPress(Scene.initial, direction)) == Some(expected)
  );

  test("moves the player multiple times", () => {
    let scene =
      Scene.initial |> handleKeyPress(_, Up) >>= handleKeyPress(_, Right);
    expect(scene) == Some({x: 1, y: 1});
  });
});
