open Jest;

describe("test", () => {
  open Expect;
  open! Expect.Operators;

  test("==", () =>
    expect(1) === 1
  );
});
