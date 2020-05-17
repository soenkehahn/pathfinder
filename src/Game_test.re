open Jest;
open Expect;
open! Expect.Operators;
open Key;
open Scene_Core;
open Test_Utils;
open Belt;
open List;

let testLevels: list(scene) =
  [1, 2, 3]->List.map(i => testScene(~playerPosition={x: i, y: 0}, ()));

describe("dropLevels", () => {
  test("starts with the given level", () => {
    expect(Game.dropLevels("2", testLevels)->List.headExn)
    == testLevels->List.getExn(1)
  });

  test("when no level given, starts at 1", () => {
    expect(Game.dropLevels("", testLevels)->List.headExn)
    == testLevels->List.headExn
  });
});

describe("levels", () => {
  describe("when the game is over", () => {
    open Game;
    let wonGame = {
      scene:
        testScene(~playerPosition={x: 3, y: 0}, ~goal={x: 3, y: 0}, ()),
      levels: testLevels,
    };

    test("it switches to the next level", () => {
      expect(Game.step(wonGame, Space).scene) == testLevels->List.headExn
    });

    test("pops the level from the level stack", () => {
      expect(length(Game.step(wonGame, Space).levels))
      == length(wonGame.levels)
      - 1
    });
  })
});
