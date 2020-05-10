open Jest;
open Expect;
open! Expect.Operators;
open Level_Parser;
open Scene_Core;

describe("parse", () => {
  describe("player parsing", () => {
    test("it parses the player's position", () => {
      let csv = "Player,Goal";
      expect(parse(csv).player) == {x: 0, y: 0};
    });

    testAll(
      "it always parses the player's position as the coordinate origin",
      [
        "Empty,Player,Goal",
        "Player,Empty,Goal",
        "Player\nEmpty\nGoal",
        "Goal\nEmpty\nPlayer",
      ],
      csv =>
      expect(parse(csv).player) == {x: 0, y: 0}
    );
  });

  describe("goal parsing", () => {
    test("it parses the goal's position", () => {
      let csv = "Player,Goal";
      expect(parse(csv).goal) == {x: 1, y: 0};
    });

    test("it parses goal x coordinates relative to the player", () => {
      let csv = "Empty,Player,Goal";
      expect(parse(csv).goal) == {x: 1, y: 0};
    });

    test("it parses goal y coordinates relative to the player", () => {
      let csv = "Goal\nPlayer";
      expect(parse(csv).goal) == {x: 0, y: 1};
    });

    test("it works for negative coordinates", () => {
      let csv = "Empty,Player\nGoal,Empty";
      expect(parse(csv).goal) == {x: (-1), y: (-1)};
    });
  });

  describe("moves extras parsing", () => {
    test("it adds moves extras to the level at the right position", () => {
      let csv = "Player,Goal,Moves 4";
      expect(parse(csv).movesExtras |> List.map(extra => extra.position, _))
      == [{x: 2, y: 0}];
    });

    test("it parses the number of moves", () => {
      let csv = "Player,Goal,Moves 4";
      expect(
        parse(csv).movesExtras |> List.map(extra => extra.extraMoves, _),
      )
      == [4];
    });
  });

  describe("walls parsing", () => {
    test("it adds walls to the level at the right position", () => {
      let csv = "Player,Goal,Wall";
      expect(parse(csv).walls) == [{x: 2, y: 0}];
    })
  });
});
