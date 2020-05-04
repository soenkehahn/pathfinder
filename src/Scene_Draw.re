open Scene;
open Webapi.Canvas.Canvas2d;
type context = t;

let cellSize = 50;

let draw_player = (context: context, player: position): unit => {
  setFillStyle(context, String, "#ff0000");
  fillRect(
    ~x=float_of_int(player.x * cellSize),
    ~y=float_of_int(player.y * cellSize),
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
    context,
  );
  ();
};

let draw_goal = (context: context, goal: position): unit => {
  setFillStyle(context, String, "#0000ff");
  fillRect(
    ~x=float_of_int(goal.x * cellSize),
    ~y=float_of_int(goal.y * cellSize),
    ~w=float_of_int(cellSize),
    ~h=float_of_int(cellSize),
    context,
  );
  ();
};

let draw = (context: context, scene: scene): unit => {
  draw_goal(context, scene.goal);
  draw_player(context, scene.player);
};

let ui = scene =>
  <>
    {React.string("moves left: " ++ string_of_int(scene.movesLeft))}
    {if (is_game_over(scene)) {
       <> <br /> {React.string("You won!")} </>;
     } else {
       React.null;
     }}
  </>;
