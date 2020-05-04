open Scene;

type game = {scene};

let initial = {scene: Scene.initial};

let draw = (context, game) => Scene_Draw.draw(context, game.scene);

let ui = game => Scene_Draw.ui(game.scene);

let step = (game, key) => {scene: step(game.scene, key)};
