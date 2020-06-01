open Scene_Core;
open Key;

let testScene =
    (
      ~movesLeft: int=3,
      ~playerPosition: position=Player.initial,
      ~history: list((direction, revertible))=[],
      ~rocks: list(Rock.t)=[],
      ~hasHammer: bool=false,
      ~goal={x: 3, y: 0},
      ~movesExtras=[],
      ~walls=[],
      ~hammers=[],
      ~boulders=[],
      (),
    )
    : scene => {
  let rec result = {
    initialScene: () => result,
    revertible: {
      player: playerPosition,
      rocks,
    },
    history,
    movesLeft,
    hasHammer,
    goal,
    movesExtras,
    walls,
    hammers,
    boulders,
  };
  result;
};
