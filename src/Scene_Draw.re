open Belt;
open List;
open Scene_Core;
open Scene;
open Webapi.Canvas.Canvas2d;
type context = Webapi.Canvas.Canvas2d.t;

let cellSize = 50;

let draw_path = (context, path: list(position)): unit => {
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

let draw_rect = (context, color, position) => {
  context->setFillStyle(String, color);
  context->fillRect(
    ~x=float_of_int(position.x * cellSize) -. float_of_int(cellSize) /. 2.,
    ~y=float_of_int(position.y * cellSize) -. float_of_int(cellSize) /. 2.,
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
  );
};

let draw_player = (context: context, player: position): unit => {
  context->draw_rect("#ff0000", player);
};

let draw_goal = (context: context, goal: position): unit => {
  context->draw_rect("#0000ff", goal);
};

let draw_moves_extra = (context: context, movesExtra: MovesExtra.t): unit => {
  context->draw_rect("#00ffff", movesExtra.position);
};

let draw_walls = (context, walls): unit =>
  walls->forEach(wall => context->draw_rect("#be480a", wall));

let draw_rocks = (context, rocks: list(Rock.t)): unit =>
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

let draw_hammers = (context, hammers): unit =>
  forEach(hammers, hammer => context->draw_rect("#ffbf00", hammer));

let draw = (context: context, scene: scene): unit => {
  context->draw_goal(scene.goal);
  scene.movesExtras->map(context->draw_moves_extra(_))->ignore;
  context->draw_path(getPath(scene));
  context->draw_player(scene.revertible.player);
  context->draw_walls(scene.walls);
  context->draw_rocks(scene.revertible.rocks);
  context->draw_hammers(scene.hammers);
};

let ui = (scene: scene) =>
  <>
    {React.string("moves left: " ++ string_of_int(scene.movesLeft))}
    {if (is_game_over(scene)) {
       <> <br /> {React.string("You won!")} </>;
     } else {
       <>
         <br />
         {React.string(
            "Use the arrow keys to move and space to revert moves.",
          )}
       </>;
     }}
  </>;
