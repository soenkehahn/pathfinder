open Jest;
open Expect;
open! Expect.Operators;
open Key;

describe("fromString", () => {
  let table = [
    ("ArrowUp", Up),
    ("ArrowDown", Down),
    ("ArrowLeft", Left),
    ("ArrowRight", Right),
  ];
  testAll("converts to key", table, ((js_key, key)) =>
    expect(fromString(js_key)) === Some(key)
  );

  test("returns None for unknown keys", () =>
    expect(fromString("unknown")) === None
  );
});
