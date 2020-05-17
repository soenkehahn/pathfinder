open Jest;
open Expect;
open! Expect.Operators;
open Level_Generator;

describe("concatWith", () => {
  test("concatenates the given strings", () =>
    expect(["a", "b", "c"]->concatWith("")) == "abc"
  );

  test("puts separators between the chunks", () =>
    expect(["a", "b", "c"]->concatWith("#")) == "a#b#c"
  );

  test("works for a single chunk", () =>
    expect(["a"]->concatWith("#")) == "a"
  );
});
