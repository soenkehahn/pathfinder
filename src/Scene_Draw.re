open Belt;
open List;
open Scene_Core;
open Scene;
open Key;
open Webapi.Canvas.Canvas2d;
type context = Webapi.Canvas.Canvas2d.t;

let cellSize = 50;

let drawPath = (context, path: list(position)): unit => {
  switch (path) {
  | [] => ()
  | [_, ...rest] =>
    context->setStrokeStyle(String, "#ffffff");
    context->lineWidth(5.);
    context->lineCap(LineCap.round);
    context->beginPath;
    zip(path, rest)
    ->map(((a, b)) => {
        context->moveTo(
          ~x=float_of_int(a.x * cellSize),
          ~y=float_of_int(a.y * cellSize),
        );
        context->lineTo(
          ~x=float_of_int(b.x * cellSize),
          ~y=float_of_int(b.y * cellSize),
        );
      })
    ->ignore;
    context->stroke;
  };
};

let drawRect = (context, color, position) => {
  context->setFillStyle(String, color);
  context->fillRect(
    ~x=float_of_int(position.x * cellSize) -. float_of_int(cellSize) /. 2.,
    ~y=float_of_int(position.y * cellSize) -. float_of_int(cellSize) /. 2.,
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
  );
};

let drawPlayer = (context: context, player: position): unit => {
  context->drawRect("#ff0000", player);
};

let drawGoal = (context: context, goal: position): unit => {
  context->drawRect("#0000ff", goal);
};

let drawMovesExtra = (context: context, movesExtra: MovesExtra.t): unit => {
  context->drawRect("#00ffff", movesExtra.position);
};

let drawWalls = (context, walls): unit =>
  walls->forEach(wall => context->drawRect("#be480a", wall));

let drawRocks = (context, rocks: list(Rock.t)): unit =>
  forEach(
    rocks,
    rock => {
      context->setFillStyle(String, "#444444");
      let position = rock.position;
      context->fillRect(
        ~x=
          float_of_int(position.x * cellSize) -. float_of_int(cellSize) /. 2.,
        ~y=
          float_of_int(position.y * cellSize) -. float_of_int(cellSize) /. 2.,
        ~w=float_of_int(cellSize),
        ~h=
          float_of_int(cellSize)
          *. (float_of_int(rock.structuralIntegrity) /. 3.),
      );
    },
  );

let drawHammers = (context, hammers): unit =>
  forEach(hammers, hammer => context->drawRect("#ffbf00", hammer));

let drawBoulders = (context, boulders): unit =>
  forEach(boulders, boulder => context->drawRect("#ec9ba4", boulder));

let draw = (context: context, scene: scene): unit => {
  context->drawGoal(scene.goal);
  scene.movesExtras->map(context->drawMovesExtra(_))->ignore;
  context->drawPath(getPath(scene));
  context->drawPlayer(scene.revertible.player);
  context->drawWalls(scene.walls);
  context->drawRocks(scene.revertible.rocks);
  context->drawHammers(scene.hammers);
  context->drawBoulders(scene.boulders);
};

let ui = (scene: scene) =>
  <>
    {React.string("moves left: " ++ string_of_int(scene.movesLeft))}
    {if (isGameOver(scene)) {
       <> <br /> {React.string("You won!")} </>;
     } else {
       <>
         <br />
         {React.string(
            "Use the arrow keys to move and space to revert moves. Restart the level with Escape.",
          )}
       </>;
     }}
  </>;
