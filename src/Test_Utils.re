open Scene_Core;

let testScene =
    (
      ~movesLeft: int=3,
      ~playerPosition: position=Player.initial,
      ~history: list(revertible)=[],
      ~rocks=[],
      ~hasHammer: bool=false,
      ~goal={x: 3, y: 0},
      ~movesExtras=[],
      ~walls=[],
      ~hammers=[],
      (),
    )
    : scene => {
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
};
