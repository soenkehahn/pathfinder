open Jest;
open Expect;
open! Expect.Operators;
open Key;

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
