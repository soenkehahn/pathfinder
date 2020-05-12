open Jest;
open Expect;
open! Expect.Operators;
open Key;
open List;
open Game;
open Scene_Core;

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
    // fixme: make simpler
    let won_game = {
      ...Game.initial(),
      scene: {
        ...Game.initial().scene,
        revertible: {
          ...Game.initial().scene.revertible,
          player: {
            position: Game.initial().scene.goal,
          },
        },
      },
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
