open Scene_Core;
open Scene;
open Key;
open Belt;
open List;

type t = {
  scene,
  levels: list(scene),
};

let make = (levels: list(scene)) =>
  switch (levels) {
  | [current, ...rest] => {scene: current, levels: rest}
  | [] =>
    exception NoLevels;
    raise(NoLevels);
  };

let dropLevels = (level: string, levels: list(scene)): list(scene) =>
  switch (int_of_string_opt(level)) {
  | Some(level) => levels->drop(level - 1)->Option.getWithDefault([])
  | None => levels
  };

let step = (game, key) =>
  switch (is_game_over(game.scene), key, game.levels) {
  | (true, Space, [next, ...rest]) => {scene: next, levels: rest}
  | _ => {...game, scene: step(game.scene, key)}
  };

let draw = (context, game) => Scene_Draw.draw(context, game.scene);

let ui = game =>
  <>
    {Scene_Draw.ui(game.scene)}
    {switch (is_game_over(game.scene), game.levels) {
     | (true, [_, ..._]) =>
       <> <br /> {React.string("Use space to get to the next level.")} </>
     | (true, []) =>
       <> <br /> {React.string("Congratulations, you beat all levels!")} </>
     | _ => React.null
     }}
  </>;
