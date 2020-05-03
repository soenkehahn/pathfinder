
let draw = (canvas: Dom.element): unit => {
  open Webapi.Canvas.CanvasElement;
  open Webapi.Canvas.Canvas2d;
  let width = float_of_int(Webapi.Canvas.CanvasElement.width(canvas));
  let height = float_of_int(Webapi.Canvas.CanvasElement.height(canvas));
  let context = getContext2d(canvas);
  fillRect(~x=0.0, ~y=0.0, ~w=width, ~h=height, context);
  setFillStyle(context, String, "#ff0000");
  fillRect(~x=190.0, ~y=190.0, ~w=10.0, ~h=10.0, context);
  ();
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

module App = {
  [@react.component]
  let make = () => {
    open React;
    let canvasElementRef: Ref.t(option(Dom.element)) = useRef(None);
    useLayoutEffect0(() => {
      Ref.current(canvasElementRef) |> Belt.Option.map(_, draw) |> ignore;
      None;
    });
    <>
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

ReactDOMRe.renderToElementWithId(<App />, "main");
