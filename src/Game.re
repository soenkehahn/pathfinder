open Scene_Core;
open Scene;
open Key;
open Level_Parser;
open Belt;

type game = {
  scene,
  levels: list(scene),
};

let levels = Levels_All.csvs->List.map(parse);

let initial = (~level: int=1, ()) =>
  switch (levels->List.drop(level - 1)) {
  | Some([current, ...rest]) => {scene: current, levels: rest}
  | _ =>
    exception NoLevels;
    raise(NoLevels);
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

let step = (game, key) =>
  switch (is_game_over(game.scene), key, game.levels) {
  | (true, Space, [next, ...rest]) => {scene: next, levels: rest}
  | _ => {...game, scene: step(game.scene, key)}
  };
