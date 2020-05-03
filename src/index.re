module App = {
  [@react.component]
  let make = () => {
    <div> {React.string("huhu")} </div>;
  };
};

ReactDOMRe.renderToElementWithId(<App />, "main");
