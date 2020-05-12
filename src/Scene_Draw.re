open Scene_Core;
open Scene;
open Webapi.Canvas.Canvas2d;
type context = t;

let cellSize = 50;

let draw_path = (context, path: list(position)): unit => {
  switch (path) {
  | [] => ()
  | [_, ...rest] =>
    setStrokeStyle(context, String, "#ffffff");
    lineWidth(context, 5.);
    lineCap(context, LineCap.round);
    beginPath(context);
    Belt.List.map(Belt.List.zip(path, rest), ((a, b)) =>
      (
        {
          moveTo(
            context,
            ~x=float_of_int(a.x * cellSize),
            ~y=float_of_int(a.y * cellSize),
          );
          lineTo(
            context,
            ~x=float_of_int(b.x * cellSize),
            ~y=float_of_int(b.y * cellSize),
          );
        }: unit
      )
    )
    |> ignore;
    stroke(context);
  };
};

let draw_rect = (context, color, position) => {
  setFillStyle(context, String, color);
  fillRect(
    ~x=float_of_int(position.x * cellSize) -. float_of_int(cellSize) /. 2.,
    ~y=float_of_int(position.y * cellSize) -. float_of_int(cellSize) /. 2.,
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
    context,
  );
};

let draw_player = (context: context, player: position): unit => {
  draw_rect(context, "#ff0000", player);
};

let draw_goal = (context: context, goal: position): unit => {
  draw_rect(context, "#0000ff", goal);
};

let draw_moves_extra = (context: context, movesExtra: MovesExtra.t): unit => {
  draw_rect(context, "#00ffff", movesExtra.position);
};

let draw_walls = (context, walls): unit =>
  Belt.List.forEach(walls, wall => draw_rect(context, "#be480a", wall));

let draw_rocks = (context, rocks: list(Rock.t)): unit =>
  Belt.List.forEach(
    rocks,
    rock => {
      setFillStyle(context, String, "#444444");
      let position = rock.position;
      fillRect(
        ~x=
          float_of_int(position.x * cellSize) -. float_of_int(cellSize) /. 2.,
        ~y=
          float_of_int(position.y * cellSize) -. float_of_int(cellSize) /. 2.,
        ~w=float_of_int(cellSize),
        ~h=
          float_of_int(cellSize)
          *. (float_of_int(rock.structuralIntegrity) /. 3.),
        context,
      );
    },
  );

let draw_hammers = (context, hammers): unit =>
  Belt.List.forEach(hammers, hammer => draw_rect(context, "#ffbf00", hammer));

let draw = (context: context, scene: scene): unit => {
  draw_goal(context, scene.goal);
  List.map(draw_moves_extra(context, _), scene.movesExtras) |> ignore;
  draw_path(context, getPath(scene));
  draw_player(context, scene.revertible.player);
  draw_walls(context, scene.walls);
  draw_rocks(context, scene.revertible.rocks);
  draw_hammers(context, scene.hammers);
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
