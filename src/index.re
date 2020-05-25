open Belt;
open FetchLevels;
open Js.Promise;

let drawGame = (canvas: Dom.element, game: Game.t): unit => {
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

module RenderGame = {
  [@react.component]
  let make = (~game: Game.t) => {
    open React;
    let canvasElementRef: React.ref(option(Dom.element)) = useRef(None);
    useLayoutEffect1(
      () => {
        canvasElementRef.current
        ->Option.map(canvas => drawGame(canvas, game))
        ->ignore;
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
        ref={ReactDOMRe.Ref.callbackDomRef(elem => {
          canvasElementRef.current = Js.Nullable.toOption(elem)
        })}
      />
    </>;
  };
};

module ManageGame = {
  [@react.component]
  let make = (~game: Game.t) => {
    let (game, setGame) = React.useState(() => game);
    let handleKeyboardEvents = (event): unit => {
      Webapi.Dom.KeyboardEvent.(
        if (!repeat(event)) {
          switch (Key.fromString(key(event))) {
          | Some(key) => setGame(game => Game.step(game, key))
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
    <RenderGame game />;
  };
};

module App = {
  [@react.component]
  let make = () => {
    let url = ReasonReactRouter.useUrl();
    let (game, setGame) = React.useState(() => None);
    React.useEffect0(() => {
      {
        let%P levels = getLevels();
        switch (levels) {
        | Ok(levels) =>
          setGame(_ => Some(Game.make(Game.dropLevels(url.hash, levels))))
        | Error(message) => setGame(_ => Some(Error(message)))
        };
        resolve();
      }
      ->ignore;
      None;
    });
    switch (game) {
    | None => React.null
    | Some(Ok(game)) => <ManageGame game />
    | Some(Error(message)) => React.string(message)
    };
  };
};

let main = () => ReactDOMRe.renderToElementWithId(<App />, "main");

try(main()) {
| e => Js.log(Printexc.to_string(e))
};
