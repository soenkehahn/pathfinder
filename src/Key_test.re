open Jest;
open Expect;
open! Expect.Operators;
open Key;

describe("revert", () => {
  let table = [(Up, Down), (Down, Up), (Left, Right), (Right, Left)];
  testAll("reverts directions", table, ((direction, reverted)) =>
    expect(direction->revert) == reverted
  );
});

describe("fromString", () => {
  let table = [
    ("ArrowUp", Direction(Up)),
    ("ArrowDown", Direction(Down)),
    ("ArrowLeft", Direction(Left)),
    ("ArrowRight", Direction(Right)),
    (" ", Space),
    ("Escape", Escape),
  ];
  testAll("converts to key", table, ((js_key, key)) =>
    expect(fromString(js_key)) == Some(key)
  );

  test("returns None for unknown keys", () =>
    expect(fromString("unknown")) == None
  );
});
