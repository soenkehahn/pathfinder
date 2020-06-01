open Jest;
open Expect;
open! Expect.Operators;
open Level_Parser;
open Scene_Core;
open Belt;

let testFailure = (message: string) => {
  exception TestFailure(string);
  raise(TestFailure(message));
};

let get = (input: Result.t('a, string)): 'a =>
  switch (input) {
  | Ok(output) => output
  | Error(message) => testFailure(message)
  };

describe("parse", () => {
  describe("player parsing", () => {
    test("it parses the player's position", () => {
      let csv = "Player,Goal";
      expect(parse(csv)->get.revertible.player) == {x: 0, y: 0};
    });

    testAll(
      "always parses the player's position as the coordinate origin",
      [
        "Empty,Player,Goal",
        "Player,Empty,Goal",
        "Player\nEmpty\nGoal",
        "Goal\nEmpty\nPlayer",
      ],
      csv =>
      expect(parse(csv)->get.revertible.player) == {x: 0, y: 0}
    );

    test(
      "parses numbers after 'Player' in the csv as the number of initial moves",
      () =>
      expect(parse("Player 4,Goal")->get.movesLeft) == 4
    );

    test("returns 3 as the default number of moves", () =>
      expect(parse("Player,Goal")->get.movesLeft) == 3
    );
  });

  describe("goal parsing", () => {
    test("it parses the goal's position", () => {
      let csv = "Player,Goal";
      expect(parse(csv)->get.goal) == {x: 1, y: 0};
    });

    test("it parses goal x coordinates relative to the player", () => {
      let csv = "Empty,Player,Goal";
      expect(parse(csv)->get.goal) == {x: 1, y: 0};
    });

    test("it parses goal y coordinates relative to the player", () => {
      let csv = "Goal\nPlayer";
      expect(parse(csv)->get.goal) == {x: 0, y: 1};
    });

    test("it works for negative coordinates", () => {
      let csv = "Empty,Player\nGoal,Empty";
      expect(parse(csv)->get.goal) == {x: (-1), y: (-1)};
    });
  });

  describe("moves extras parsing", () => {
    open MovesExtra;

    test("it adds moves extras to the level at the right position", () => {
      let csv = "Player,Goal,Moves 4";
      expect(parse(csv)->get.movesExtras->List.map(extra => extra.position))
      == [{x: 2, y: 0}];
    });

    test("it parses the number of moves", () => {
      let csv = "Player,Goal,Moves 4";
      expect(
        parse(csv)->get.movesExtras->List.map(extra => extra.extraMoves),
      )
      == [4];
    });
  });

  describe("walls parsing", () => {
    test("it adds walls to the level at the right position", () => {
      let csv = "Player,Goal,Wall";
      expect(parse(csv)->get.walls) == [{x: 2, y: 0}];
    })
  });

  describe("rocks parsing", () => {
    test("it adds rocks to the level at the right position", () => {
      let csv = "Player,Goal,Rock";
      expect(parse(csv)->get.revertible.rocks)
      == [{
            position: {
              x: 2,
              y: 0,
            },
            structuralIntegrity: 3,
          }];
    })
  });

  describe("hammer parsing", () => {
    test("it adds hammers to the level at the right position", () => {
      let csv = "Player,Goal,Hammer";
      expect(parse(csv)->get.hammers) == [{x: 2, y: 0}];
    })
  });

  describe("boulder parsing", () => {
    test("it adds boulders to the level at the right position", () => {
      let csv = "Player,Goal,Boulder";
      expect(parse(csv)->get.boulders) == [{x: 2, y: 0}];
    })
  });
});
