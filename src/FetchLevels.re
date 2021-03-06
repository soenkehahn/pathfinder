open Fetch;
open Js.Promise;
open Scene_Core;
open Belt;
open List;
open Level_Parser;

module P = {
  let let_ = (promise, callback) => then_(callback, promise);
};

[@bs.scope "JSON"] [@bs.val]
external jsonToStringArray: string => array(string) = "parse";

let getLevelNames = (): Js.Promise.t(array(string)) => {
  let%P response = fetch("all_levels.json");
  let%P json = Response.text(response);
  resolve(jsonToStringArray(json));
};

let getLevels = (): Js.Promise.t(ParseResult.t(list(scene))) => {
  let%P levelNames = getLevelNames();
  let%P csvs =
    levelNames
    ->Array.map(name => {
        let%P response = fetch(name);
        let%P text = response->Response.text;
        resolve((name, text));
      })
    ->all;
  resolve(
    csvs
    ->fromArray
    ->ParseResult.mapM(((name, csv)) =>
        parse(csv)->Level_Parser.ParseResult.localizeError(name)
      ),
  );
};
