open Game;
open Key;

let drawGame = (canvas: Dom.element, game: game): unit => {
  open Webapi.Canvas;
  open Canvas2d;
  let width = float_of_int(CanvasElement.width(canvas));
  let height = float_of_int(CanvasElement.height(canvas));
  let context = CanvasElement.getContext2d(canvas);
  setTransform(context, ~m11=1., ~m12=0., ~m21=0., ~m22=1., ~dx=0., ~dy=0.);
  setFillStyle(context, String, "#000000");
  fillRect(~x=0.0, ~y=0.0, ~w=width, ~h=height, context);
  translate(context, ~x=width /. 2., ~y=height /. 2.);
  scale(context, ~x=1., ~y=-1.);
  Game.draw(context, game);
};

let centerStyle =
  ReactDOMRe.Style.make(
    ~padding="0",
    ~margin="auto",
    ~display="block",
    ~width="800px",
    ~height="600px",
    ~position="absolute",
    ~top="0",
    ~bottom="0",
    ~left="0",
    ~right="0",
    (),
  );

module DrawGame = {
  [@react.component]
  let make = (~game: game) => {
    open React;
    let canvasElementRef: Ref.t(option(Dom.element)) = useRef(None);
    useLayoutEffect1(
      () => {
        Ref.current(canvasElementRef)
        |> Belt.Option.map(_, canvas => drawGame(canvas, game))
        |> ignore;
        None;
      },
      [|game|],
    );
    <>
      {Game.ui(game)}
      <canvas
        width="800"
        height="600"
        style=centerStyle
        ref={ReactDOMRe.Ref.callbackDomRef(elem =>
          React.Ref.setCurrent(canvasElementRef, Js.Nullable.toOption(elem))
        )}
      />
    </>;
  };
};

module App = {
  [@react.component]
  let make = () => {
    let (game, setGame) = React.useState(() => Game.initial);

    let handleKeyboardEvents = (event): unit => {
      Webapi.Dom.KeyboardEvent.(
        if (!repeat(event)) {
          switch (key_of_js_key(key(event))) {
          | Some(key) => setGame(game => step(game, key))
          | None => ()
          };
        }
      );
    };

    React.useEffect0(() => {
      Webapi.Dom.EventTarget.addKeyDownEventListener(
        handleKeyboardEvents,
        Webapi.Dom.Document.asEventTarget(Webapi.Dom.document),
      );

      Some(
        () => {
          Webapi.Dom.EventTarget.removeKeyDownEventListener(
            handleKeyboardEvents,
            Webapi.Dom.Document.asEventTarget(Webapi.Dom.document),
          )
        },
      );
    });
    <DrawGame game />;
  };
};

ReactDOMRe.renderToElementWithId(<App />, "main");
