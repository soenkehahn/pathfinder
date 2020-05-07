open Scene_Core;

open Level_Parser;

let levels = [
  parse(Levels_Nine.csv),
  {
    moves: 3,
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
  },
  {
    player: {
      x: 0,
      y: 0,
    },
    moves: 3,
    goal: {
      x: 3,
      y: 3,
    },
    path: [],
    extras: [{
               position: {
                 y: 3,
                 x: 0,
               },
               extraMoves: 3,
             }],
  },
  {
    player: {
      x: 0,
      y: 0,
    },
    moves: 4,
    goal: {
      x: 4,
      y: 4,
    },
    path: [],
    extras: [{
               position: {
                 y: (-2),
                 x: (-2),
               },
               extraMoves: 4,
             }],
  },
];
