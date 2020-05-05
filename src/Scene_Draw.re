open Scene_Core;
open Scene;
open Webapi.Canvas.Canvas2d;
type context = t;

let cellSize = 50;

let draw_rect = (context, color, position) => {
  setFillStyle(context, String, color);
  fillRect(
    ~x=float_of_int(position.x * cellSize),
    ~y=float_of_int(position.y * cellSize),
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

let draw_extra = (context: context, extra: extra): unit => {
  draw_rect(context, "#00ffff", extra.position);
};

let draw = (context: context, scene: scene): unit => {
  draw_goal(context, scene.goal);
  List.map(draw_extra(context, _), scene.extras) |> ignore;
  draw_player(context, scene.player);
};

let ui = scene =>
  <>
    {React.string("moves left: " ++ string_of_int(scene.moves))}
    {if (is_game_over(scene)) {
       <> <br /> {React.string("You won!")} </>;
     } else {
       React.null;
     }}
  </>;
