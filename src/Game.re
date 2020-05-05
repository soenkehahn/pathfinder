open Scene_Core;
open Scene;
open Key;

type game = {
  scene,
  levels: list(scene),
};

let initial =
  switch (Scene_Levels.levels) {
  | [current, ...rest] => {scene: current, levels: rest}
  | [] =>
    exception NoLevels;
    raise(NoLevels);
  };

let draw = (context, game) => Scene_Draw.draw(context, game.scene);

let ui = game =>
  <>
    {Scene_Draw.ui(game.scene)}
    {switch (is_game_over(game.scene), game.levels) {
     | (true, []) =>
       <> <br /> {React.string("Congratulations, you beat all levels!")} </>
     | _ => React.null
     }}
  </>;

let step = (game, key) =>
  switch (is_game_over(game.scene), key, game.levels) {
  | (true, Undo, [next, ...rest]) => {scene: next, levels: rest}
  | _ => {...game, scene: step(game.scene, key)}
  };
