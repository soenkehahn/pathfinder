open Jest;
open Expect;
open! Expect.Operators;
open Key;
open List;
open Game;
open Scene_Core;
open Test_Utils;

describe("initial", () => {
  open Level_Parser;
  open Levels_All;

  test("starts with the given level", () => {
    expect(initial(~level=2, ()).scene) == parse(List.nth(csvs, 1))
  });

  test("when no level given, starts at 1", () => {
    expect(initial().scene) == parse(Belt.List.headExn(csvs))
  });
});

describe("levels", () => {
  describe("when the game is over", () => {
    let won_game = {
      ...Game.initial(),
      scene:
        test_scene(~playerPosition={x: 3, y: 0}, ~goal={x: 3, y: 0}, ()),
    };

    test("it switches to the next level", () => {
      expect(Game.step(won_game, Space).scene) == List.nth(Game.levels, 1)
    });

    test("pops the level from the level stack", () => {
      expect(length(Game.step(won_game, Space).levels))
      == length(won_game.levels)
      - 1
    });
  })
});
