open Jest;
open Expect;
open! Expect.Operators;
open Key;
open List;
open Game;

describe("levels", () => {
  describe("when the game is over", () => {
    test("it switches to the next level", () => {
      let won_game = {
        ...Game.initial,
        scene: {
          ...Game.initial.scene,
          player: Game.initial.scene.goal,
        },
      };
      expect(Game.step(won_game, Space).scene) == List.nth(Game.levels, 1);
    });

    test("pops the level from the level stack", () => {
      let won_game = {
        ...Game.initial,
        scene: {
          ...Game.initial.scene,
          player: Game.initial.scene.goal,
        },
      };
      expect(length(Game.step(won_game, Space).levels))
      == length(won_game.levels)
      - 1;
    });
  })
});
