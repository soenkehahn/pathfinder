open Jest;
open Scene_Core;
open Scene;
open Expect;
open! Expect.Operators;
open Key;

let rec steps = (scene, keys) =>
  switch (keys) {
  | [key, ...rest] => scene |> step(_, key) |> steps(_, rest)
  | [] => scene
  };

let scene = {
  moves: 5,
  player: {
    x: 0,
    y: 0,
  },
  path: [],
  goal: {
    x: 3,
    y: 0,
  },
  extras: [],
};

describe("moving", () => {
  let table = [
    (Up, {x: 0, y: 1}),
    (Down, {x: 0, y: (-1)}),
    (Left, {x: (-1), y: 0}),
    (Right, {x: 1, y: 0}),
  ];
  testAll(
    "moves the player in the given direction", table, ((direction, expected)) =>
    expect(step(scene, direction).player) == expected
  );

  test("moves the player multiple times", () => {
    let scene = scene |> steps(_, [Up, Right]);
    expect(scene.player) == {x: 1, y: 1};
  });

  test("counts down the moves", () => {
    let initial = scene.moves;
    let scene = scene |> step(_, Up);
    expect((initial, scene.moves)) == (5, 4);
  });

  test("disallows movements when movesLeft is 0 foo", () => {
    let scene = {...scene, moves: 0};
    expect(step(scene, Up)) == scene;
  });

  test("tracks path of player", () => {
    let scene = scene |> steps(_, [Up, Right]);
    expect(scene.path) == [{x: 0, y: 1}, {x: 0, y: 0}];
  });
});

describe("undo", () => {
  test("moves to the last position", () => {
    let scene =
      {
        ...scene,
        player: {
          x: 0,
          y: 0,
        },
        path: [{x: 23, y: 42}],
        moves: 4,
      }
      |> step(_, Undo);
    expect(scene.player) == {x: 23, y: 42};
  });

  test("increases movesLeft", () => {
    let scene = scene |> steps(_, [Up, Undo]);
    expect(scene.moves) == 5;
  });

  test("removes the position from the path", () => {
    let scene = scene |> steps(_, [Up, Undo]);
    expect(scene.path) == [];
  });

  test("on the initial scene doesn't do anything", () => {
    let scene = scene |> step(_, Undo);
    expect(scene) == scene;
  });

  test("works when movesLeft is 0", () => {
    let scene =
      {...scene, moves: 0, path: [{x: 23, y: 42}]} |> step(_, Undo);
    expect(scene.player) == {x: 23, y: 42};
  });
});

describe("is_game_over", () => {
  test("is false during the game", () =>
    expect(is_game_over(scene)) == false
  );

  let end_scene = {
    ...scene,
    goal: {
      x: 0,
      y: 0,
    },
  };

  test("when reaching the goal the game is won", () => {
    expect(is_game_over(end_scene)) == true
  });

  test("when reaching the goal, no other keyboard input is accepted", () => {
    expect(steps(end_scene, [Up, Right, Undo])) == end_scene
  });
});

describe("extra moves extra", () => {
  let scene = {
    ...scene,
    extras: [{
               position: {
                 x: 1,
                 y: 0,
               },
               extraMoves: 3,
             }],
  };

  test("gives the player extra moves", () => {
    expect(step(scene, Right).moves) == scene.moves - 1 + 3
  });

  test("removes the extra from the scene", () => {
    expect(step(scene, Right).extras) == []
  });
});
